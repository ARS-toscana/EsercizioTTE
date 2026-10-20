############################################################
#                                                          #
####     CREATE D4_CENSORING_WEIGHTS_TRUNCATED          ####
#                                                          #
############################################################

# authors: Olga Paoletti
# v 0.2 30 Jul 2026
#
# Main changes:
# - checks uniqueness of person_id + t0 before joins
# - prevents accidental Cartesian joins
# - expands follow-up by a unique episode identifier
# - attaches time-dependent covariates using a row-level update join
# - removes constant model covariates before fitting the pooled logistic models
# - avoids modifying covariate-name vectors across subpopulation iterations

#########################################
# assign input and output directories
#########################################

if (TEST) {
  
  testname <- "test_32_D4_CENSORING_WEIGHTS_TRUNCATED"
  
  thisdirinput   <- file.path(dirtest, testname)
  thisdiroutput  <- file.path(dirtest, testname, "g_output")
  thisdirsettings <- thisdirinput
  thisdirarchive <- dirarchive
  
  dir.create(
    thisdiroutput,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
} else {
  
  thisdirinput    <- dirtemp
  thisdiroutput   <- dirtemp
  thisdirsettings <- dirpargen
  thisdirarchive  <- dirarchive
}

print("CREATE D4_CENSORING_WEIGHTS_TRUNCATED")

#########################################
# helper functions
#########################################

check_required_columns <- function(DT, required_cols, object_name) {
  
  missing_cols <- setdiff(required_cols, names(DT))
  
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Missing column(s) in %s: %s",
        object_name,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


check_unique_key <- function(DT, key_cols, object_name) {
  
  duplicated_keys <- DT[
    ,
    .N,
    by = key_cols
  ][N > 1L]
  
  if (nrow(duplicated_keys) > 0L) {
    
    print(
      duplicated_keys[
        1:min(.N, 20L)
      ]
    )
    
    stop(
      sprintf(
        "%s contains %s duplicated combination(s) of: %s.",
        object_name,
        nrow(duplicated_keys),
        paste(key_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


get_nonconstant_variables <- function(DT, variables) {
  
  variables <- intersect(
    unique(variables),
    names(DT)
  )
  
  variables[
    vapply(
      variables,
      function(v) {
        
        x <- DT[[v]]
        x <- x[!is.na(x)]
        
        length(unique(x)) >= 2L
      },
      logical(1)
    )
  ]
}


build_model_formula <- function(
    outcome,
    time_variable,
    covariates = character()
) {
  
  rhs_terms <- sprintf(
    "splines::ns(%s, df = 4)",
    time_variable
  )
  
  if (length(covariates) > 0L) {
    rhs_terms <- c(
      rhs_terms,
      covariates
    )
  }
  
  stats::as.formula(
    paste(
      outcome,
      "~",
      paste(rhs_terms, collapse = " + ")
    )
  )
}


attach_td_covariate <- function(long_dt, td_dt) {
  
  long_dt <- data.table::copy(
    data.table::as.data.table(long_dt)
  )
  
  td_dt <- data.table::copy(
    data.table::as.data.table(td_dt)
  )
  
  value_cols <- grep(
    "^value_of_",
    names(td_dt),
    value = TRUE
  )
  
  if (length(value_cols) != 1L) {
    stop(
      sprintf(
        paste0(
          "Expected exactly one value_of_ column in a TD dataset; ",
          "found: %s"
        ),
        paste(value_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  value_col <- value_cols[1L]
  cov_name  <- sub("^value_of_", "", value_col)
  
  check_required_columns(
    td_dt,
    c(
      "person_id",
      "start_d",
      "end_d",
      value_col
    ),
    paste0("TD dataset for ", cov_name)
  )
  
  data.table::setnames(
    td_dt,
    value_col,
    "td_value_internal"
  )
  
  td_dt <- td_dt[
    !is.na(person_id) &
      !is.na(start_d) &
      !is.na(end_d) &
      start_d <= end_d
  ]
  
  # For overlapping records, mult = "last" selects the last record
  # according to this ordering.
  data.table::setorder(
    td_dt,
    person_id,
    start_d,
    end_d
  )
  
  long_dt[
    ,
    row_id_internal := .I
  ]
  
  matched_values <- td_dt[
    long_dt,
    on = .(
      person_id,
      start_d <= interval_start_date,
      end_d >= interval_start_date
    ),
    mult = "last",
    .(
      row_id_internal = i.row_id_internal,
      td_value_internal
    )
  ]
  
  check_unique_key(
    matched_values,
    "row_id_internal",
    paste0("matched TD values for ", cov_name)
  )
  
  # Initialise the new covariate before the update join.
  long_dt[
    ,
    (cov_name) := NA
  ]
  
  long_dt[
    matched_values,
    on = "row_id_internal",
    (cov_name) := i.td_value_internal
  ]
  
  long_dt[
    ,
    row_id_internal := NULL
  ]
  
  # Missing numeric TD values are interpreted as zero.
  if (
    is.numeric(long_dt[[cov_name]]) ||
    is.integer(long_dt[[cov_name]])
  ) {
    
    data.table::set(
      long_dt,
      i = which(is.na(long_dt[[cov_name]])),
      j = cov_name,
      value = 0
    )
  }
  
  long_dt[]
}

#########################################
# load settings and parameters
#########################################

settings <- readRDS(
  file.path(
    thisdirsettings,
    "SETTINGS.rds"
  )
)

settings <- data.table::as.data.table(settings)

thissubpopulations <- unique(
  settings[
    ,
    subpop
  ]
)

thissubpopulations <- unlist(
  strsplit(
    trimws(thissubpopulations),
    " +"
  )
)

parameters_this_step <- data.table::as.data.table(
  unique(
    readxl::read_excel(
      file.path(
        thisdirarchive,
        "codebooks",
        "D4_CENSORING_WEIGHTS_TRUNCATED.xlsx"
      ),
      sheet = 1,
      .name_repair = "unique_quiet"
    )
  )
)

covariate_all <- unname(
  unlist(
    parameters_this_step[
      parameter == "covariate",
      values
    ]
  )
)

# Keep immutable base names outside the subpopulation loop.
covariate_no_TD_base <- covariate_all[1:2]
covariate_TD_base    <- covariate_all[3:22]

covariate_TD_base <- paste0(
  covariate_TD_base,
  "_TD"
)

covariate_TD_base <- tolower(
  covariate_TD_base
)

covariate_after_intervals_base <- sub(
  "_td$",
  "",
  covariate_TD_base
)

# Reduce covariate list during tests.
if (isTRUE(TEST)) {
  
  covariate_TD_base <- tolower(
    c(
      "number_of_hospital_days_TD",
      "asthma_exacerbations_TD",
      "rheumatological_diseases_TD"
    )
  )
  
  covariate_after_intervals_base <- sub(
    "_td$",
    "",
    covariate_TD_base
  )
}

#########################################
# loop over subpopulations
#########################################

for (subpop in thissubpopulations) {
  
  suffix <- data.table::fifelse(
    subpop == "all",
    "",
    paste0("_", subpop)
  )
  
  print(suffix)
  
  # Recreate these vectors at every iteration.
  covariate_no_TD <- paste0(
    covariate_no_TD_base,
    "_at_t0"
  )
  
  covariate_TD <- covariate_TD_base
  
  covariate_after_intervals <- covariate_after_intervals_base
  
  # Import only the two non-TD datasets here.
  # TD datasets are loaded later using their actual filenames, so mixed
  # upper/lower-case names such as D3_Cancer_GI_tract_TD.rds work on Linux.
  listofinput <- c(
    "D4_EXP_ACT_COV_WEIGHT",
    "D4_FUP"
  )
  
  #########################################
  # import input datasets
  #########################################
  
  for (file in listofinput) {
    
    input_path <- file.path(
      thisdirinput,
      paste0(
        file,
        suffix,
        ".rds"
      )
    )
    
    if (!file.exists(input_path)) {
      stop(
        sprintf(
          "Input file not found: %s",
          input_path
        ),
        call. = FALSE
      )
    }
    
    temp <- data.table::as.data.table(
      readRDS(input_path)
    )
    
    assign(
      file,
      temp
    )
    
    rm(temp)
  }
  
  #########################################
  # create baseline dataset
  #########################################
  
  cols <- c(
    "person_id",
    "t0",
    "exposure",
    covariate_no_TD,
    "weight"
  )
  
  check_required_columns(
    D4_EXP_ACT_COV_WEIGHT,
    cols,
    "D4_EXP_ACT_COV_WEIGHT"
  )
  
  data <- data.table::copy(
    D4_EXP_ACT_COV_WEIGHT[
      ,
      ..cols
    ]
  )
  
  check_unique_key(
    data,
    c("person_id", "t0"),
    "D4_EXP_ACT_COV_WEIGHT"
  )
  
  #########################################
  # merge follow-up data
  #########################################
  
  fup_cols <- c(
    "person_id",
    "t0",
    "censoring_date_E1",
    "cause_of_censoring_E1",
    "fup_days_E1",
    "date_intercurring_events_E1",
    "intercurrent_event_E1"
  )
  
  check_required_columns(
    D4_FUP,
    fup_cols,
    "D4_FUP"
  )
  
  fup_data <- data.table::copy(
    D4_FUP[
      ,
      ..fup_cols
    ]
  )
  
  check_unique_key(
    fup_data,
    c("person_id", "t0"),
    "D4_FUP"
  )
  
  dt <- merge(
    data,
    fup_data,
    by = c(
      "person_id",
      "t0"
    ),
    all = FALSE,
    sort = FALSE
  )
  
  check_unique_key(
    dt,
    c("person_id", "t0"),
    "Merged baseline and follow-up dataset"
  )
  
  # The end date used to expand person-time must be present and valid.
  invalid_follow_up <- dt[
    is.na(t0) |
      is.na(date_intercurring_events_E1) |
      date_intercurring_events_E1 < t0
  ]
  
  if (nrow(invalid_follow_up) > 0L) {
    
    print(
      invalid_follow_up[
        1:min(.N, 20L),
        .(
          person_id,
          t0,
          date_intercurring_events_E1,
          intercurrent_event_E1
        )
      ]
    )
    
    stop(
      sprintf(
        paste0(
          "%s episode(s) have a missing or invalid ",
          "date_intercurring_events_E1."
        ),
        nrow(invalid_follow_up)
      ),
      call. = FALSE
    )
  }
  
  #########################################
  # build 30-day person-interval dataset
  #########################################
  
  interval_days <- 30L
  
  dt[
    ,
    episode_id := .I
  ]
  
  dt[
    ,
    total_days := as.integer(
      date_intercurring_events_E1 - t0
    )
  ]
  
  dt[
    ,
    n_intervals :=
      total_days %/% interval_days + 1L
  ]
  
  if (
    dt[
      ,
      any(
        is.na(n_intervals) |
        n_intervals < 1L
      )
    ]
  ) {
    stop(
      "Invalid number of intervals detected.",
      call. = FALSE
    )
  }
  
  expected_long_rows <- dt[
    ,
    sum(
      as.double(n_intervals)
    )
  ]
  
  long_dt <- dt[
    ,
    {
      
      n_this <- n_intervals[1L]
      total_this <- total_days[1L]
      
      idx <- seq.int(
        from = 0L,
        to = n_this - 1L
      )
      
      .(
        interval = idx + 1L,
        start_day = idx * interval_days,
        stop_day = pmin(
          (idx + 1L) * interval_days,
          total_this + 1L
        )
      )
    },
    by = episode_id
  ]
  
  if (nrow(long_dt) != expected_long_rows) {
    stop(
      sprintf(
        paste0(
          "Incorrect interval expansion: expected %s rows, ",
          "but generated %s rows."
        ),
        expected_long_rows,
        nrow(long_dt)
      ),
      call. = FALSE
    )
  }
  
  # Attach episode-level variables using the unique episode_id.
  long_dt <- dt[
    long_dt,
    on = "episode_id"
  ]
  
  check_unique_key(
    long_dt,
    c("episode_id", "interval"),
    "Person-interval dataset"
  )
  
  long_dt[
    ,
    interval_start_date :=
      t0 + start_day
  ]
  
  long_dt[
    ,
    interval_end_date := pmin(
      t0 + stop_day - 1L,
      date_intercurring_events_E1
    )
  ]
  
  data.table::setorder(
    long_dt,
    episode_id,
    interval
  )
  
  long_dt[
    ,
    last_interval :=
      as.integer(
        interval == max(interval)
      ),
    by = episode_id
  ]
  
  long_dt[
    ,
    censored_interval :=
      as.integer(
        last_interval == 1L &
          intercurrent_event_E1 == 1L
      )
  ]
  
  long_dt[
    ,
    time_since_index_30d :=
      interval - 1L
  ]
  
  #########################################
  # attach time-dependent covariates
  #########################################
  
  covariate_TD_normalized <- tolower(
    covariate_TD
  )
  
  td_files <- list.files(
    thisdirinput,
    pattern = "^D3_.*_TD.*\\.rds$",
    full.names = TRUE
  )
  
  td_names_normalized <- tolower(
    sub(
      "\\.rds$",
      "",
      sub(
        "^D3_",
        "",
        basename(td_files)
      )
    )
  )
  
  # Remove the subpopulation suffix before matching file names.
  if (nzchar(suffix)) {
    td_names_normalized <- sub(
      paste0(suffix, "$"),
      "",
      td_names_normalized
    )
  }
  
  keep_td <- td_names_normalized %in%
    covariate_TD_normalized
  
  td_files <- td_files[keep_td]
  td_names_normalized <- td_names_normalized[keep_td]
  
  missing_td_inputs <- setdiff(
    covariate_TD_normalized,
    td_names_normalized
  )
  
  if (length(missing_td_inputs) > 0L) {
    stop(
      sprintf(
        "Missing TD input file(s) for: %s",
        paste(
          missing_td_inputs,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  TD_list <- lapply(
    td_files,
    function(f) {
      data.table::as.data.table(
        readRDS(f)
      )
    }
  )
  
  names(TD_list) <- td_names_normalized
  
  # Ensure the requested order is retained.
  TD_list <- TD_list[
    covariate_TD_normalized
  ]
  
  for (nm in names(TD_list)) {
    
    message(
      "Attaching TD covariate: ",
      nm
    )
    
    n_before <- nrow(long_dt)
    
    long_dt <- attach_td_covariate(
      long_dt,
      TD_list[[nm]]
    )
    
    if (nrow(long_dt) != n_before) {
      stop(
        sprintf(
          paste0(
            "The number of rows changed while attaching %s: ",
            "%s before, %s after."
          ),
          nm,
          n_before,
          nrow(long_dt)
        ),
        call. = FALSE
      )
    }
  }
  
  #########################################
  # standardise selected variable names
  #########################################
  
  if (
    "gender_at_t0" %in%
    names(long_dt)
  ) {
    long_dt[
      ,
      gender_at_t0 :=
        as.factor(gender_at_t0)
    ]
  }
  
  # Convert names from column 20 onward to lower case,
  # only when at least 20 columns exist.
  if (ncol(long_dt) >= 20L) {
    
    cols_to_lower <- 20:ncol(long_dt)
    
    old_names <- names(long_dt)[
      cols_to_lower
    ]
    
    new_names <- tolower(
      old_names
    )
    
    duplicate_new_names <- new_names[
      duplicated(new_names)
    ]
    
    if (
      length(duplicate_new_names) > 0L
    ) {
      stop(
        sprintf(
          paste0(
            "Lower-casing columns would create duplicate names: %s"
          ),
          paste(
            unique(duplicate_new_names),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }
    
    data.table::setnames(
      long_dt,
      old = old_names,
      new = new_names
    )
  }
  
  # Re-normalise formula variable names after lower-casing.
  covariate_after_intervals <- tolower(
    covariate_after_intervals
  )
  
  #########################################
  # pooled logistic censoring models
  #########################################
  
  denominator_candidates <- c(
    covariate_no_TD,
    covariate_after_intervals
  )
  
  numerator_candidates <- covariate_no_TD
  
  denominator_covariates <- get_nonconstant_variables(
    long_dt,
    denominator_candidates
  )
  
  numerator_covariates <- get_nonconstant_variables(
    long_dt,
    numerator_candidates
  )
  
  removed_denominator <- setdiff(
    denominator_candidates,
    denominator_covariates
  )
  
  removed_numerator <- setdiff(
    numerator_candidates,
    numerator_covariates
  )
  
  if (
    length(removed_denominator) > 0L
  ) {
    message(
      paste0(
        "Removed absent or constant denominator covariates: ",
        paste(
          removed_denominator,
          collapse = ", "
        )
      )
    )
  }
  
  if (
    length(removed_numerator) > 0L
  ) {
    message(
      paste0(
        "Removed absent or constant numerator covariates: ",
        paste(
          removed_numerator,
          collapse = ", "
        )
      )
    )
  }
  
  if (
    length(
      unique(
        long_dt$censored_interval[
          !is.na(
            long_dt$censored_interval
          )
        ]
      )
    ) < 2L
  ) {
    stop(
      paste0(
        "censored_interval has fewer than two observed levels. ",
        "The pooled logistic censoring model cannot be fitted."
      ),
      call. = FALSE
    )
  }
  
  denom_formula <- build_model_formula(
    outcome = "censored_interval",
    time_variable = "time_since_index_30d",
    covariates = denominator_covariates
  )
  
  numer_formula <- build_model_formula(
    outcome = "censored_interval",
    time_variable = "time_since_index_30d",
    covariates = numerator_covariates
  )
  
  print(denom_formula)
  print(numer_formula)
  
  fit_denom <- stats::glm(
    formula = denom_formula,
    data = long_dt,
    family = stats::binomial(),
    na.action = stats::na.exclude
  )
  
  fit_numer <- stats::glm(
    formula = numer_formula,
    data = long_dt,
    family = stats::binomial(),
    na.action = stats::na.exclude
  )
  
  long_dt[
    ,
    p_cens_denom :=
      stats::predict(
        fit_denom,
        newdata = long_dt,
        type = "response"
      )
  ]
  
  long_dt[
    ,
    p_cens_numer :=
      stats::predict(
        fit_numer,
        newdata = long_dt,
        type = "response"
      )
  ]
  
  if (
    long_dt[
      ,
      any(
        is.na(p_cens_denom) |
        is.na(p_cens_numer)
      )
    ]
  ) {
    stop(
      paste0(
        "Missing predicted censoring probabilities were generated. ",
        "Check missing values in the model covariates."
      ),
      call. = FALSE
    )
  }
  
  # Bound both probabilities away from 0 and 1.
  eps <- 1e-6
  
  long_dt[
    ,
    p_cens_denom :=
      pmin(
        pmax(
          p_cens_denom,
          eps
        ),
        1 - eps
      )
  ]
  
  long_dt[
    ,
    p_cens_numer :=
      pmin(
        pmax(
          p_cens_numer,
          eps
        ),
        1 - eps
      )
  ]
  
  long_dt[
    ,
    p_uncens_denom :=
      1 - p_cens_denom
  ]
  
  long_dt[
    ,
    p_uncens_numer :=
      1 - p_cens_numer
  ]
  
  #########################################
  # cumulative stabilised IPCW
  #########################################
  
  data.table::setorder(
    long_dt,
    episode_id,
    interval
  )
  
  long_dt[
    ,
    cum_uncens_denom :=
      cumprod(
        p_uncens_denom
      ),
    by = episode_id
  ]
  
  long_dt[
    ,
    cum_uncens_numer :=
      cumprod(
        p_uncens_numer
      ),
    by = episode_id
  ]
  
  long_dt[
    ,
    weight_censoring :=
      cum_uncens_numer /
      cum_uncens_denom
  ]
  
  if (
    long_dt[
      ,
      any(
        !is.finite(weight_censoring) |
        weight_censoring <= 0
      )
    ]
  ) {
    stop(
      paste0(
        "Non-finite or non-positive censoring weights were generated."
      ),
      call. = FALSE
    )
  }
  
  q <- stats::quantile(
    long_dt$weight_censoring,
    probs = c(
      0.01,
      0.99
    ),
    na.rm = TRUE,
    names = FALSE
  )
  
  long_dt[
    ,
    truncated_weight_censoring :=
      pmin(
        pmax(
          weight_censoring,
          q[1L]
        ),
        q[2L]
      )
  ]
  
  #########################################
  # create episode-level output
  #########################################
  
  D4_CENSORING_WEIGHTS_TRUNCATED <- long_dt[
    ,
    .SD[.N],
    by = episode_id
  ]
  
  if (
    "weight" %in%
    names(
      D4_CENSORING_WEIGHTS_TRUNCATED
    )
  ) {
    
    D4_CENSORING_WEIGHTS_TRUNCATED[
      ,
      truncated_product_weight :=
        weight *
        truncated_weight_censoring
    ]
  }
  
  if (
    D4_CENSORING_WEIGHTS_TRUNCATED[
      ,
      any(
        !is.finite(
          truncated_product_weight
        ) |
        truncated_product_weight <= 0
      )
    ]
  ) {
    stop(
      paste0(
        "Non-finite or non-positive truncated_product_weight ",
        "values were generated."
      ),
      call. = FALSE
    )
  }
  
  keep_cols <- c(
    "person_id",
    "t0",
    "exposure",
    "fup_days_E1",
    "date_intercurring_events_E1",
    "intercurrent_event_E1",
    "truncated_product_weight"
  )
  
  check_required_columns(
    D4_CENSORING_WEIGHTS_TRUNCATED,
    keep_cols,
    "D4_CENSORING_WEIGHTS_TRUNCATED"
  )
  
  D4_CENSORING_WEIGHTS_TRUNCATED <-
    D4_CENSORING_WEIGHTS_TRUNCATED[
      ,
      ..keep_cols
    ]
  
  check_unique_key(
    D4_CENSORING_WEIGHTS_TRUNCATED,
    c(
      "person_id",
      "t0"
    ),
    "Final D4_CENSORING_WEIGHTS_TRUNCATED"
  )
  
  #########################################
  # save output
  #########################################
  
  outputfile <- D4_CENSORING_WEIGHTS_TRUNCATED
  
  nameoutput <- "D4_CENSORING_WEIGHTS_TRUNCATED"
  
  nameoutputext <- paste0(
    nameoutput,
    suffix,
    ".rds"
  )
  
  assign(
    nameoutput,
    outputfile
  )
  
  saveRDS(
    outputfile,
    file = file.path(
      thisdiroutput,
      nameoutputext
    )
  )
  
  message(
    "Saved: ",
    file.path(
      thisdiroutput,
      nameoutputext
    )
  )
  
  #########################################
  # remove dynamically imported TD objects
  #########################################
  
  objects_to_remove <- intersect(
    listofinput,
    ls()
  )
  
  rm(
    list = objects_to_remove
  )
  
  invisible(
    gc()
  )
}

#########################################
# during check, save output to QC repository
#########################################

# if (TEST & CHECK) {
#
#   thisdircheck <- file.path(
#     thisdirinput,
#     "g_output_QC"
#   )
#
#   dir.create(
#     thisdircheck,
#     recursive = TRUE,
#     showWarnings = FALSE
#   )
#
#   saveRDS(
#     outputfile,
#     file = file.path(
#       thisdircheck,
#       nameoutputext
#     )
#   )
# }
#
# if (!TEST & CHECKSEQ) {
#
#   saveRDS(
#     outputfile,
#     file = file.path(
#       dirtestseq,
#       nameoutput,
#       "g_output_QC",
#       nameoutputext
#     )
#   )
# }
