############################################################
#                                                          #
#                 CREATE D5_ResultsRMST                    #
#                                                          #
############################################################
# authors: Azzurra Bongianni, Rosa Gini, Olga Paoletti

# v 1.1: adapted to FinReg version
#
# E1:
#   N patients
#   N events
#   Person-years
#   Incidence rate with 95% CI
#
# E2_year1:
#   E2b_outcome and E2b_fup
#   RMST restricted at 1 year
#
# E2_year2:
#   E2a_outcome and E2a_fup
#   RMST restricted at 2 years
#
# E3:
#   N patients
#   N events
#   Person-years
#   Incidence rate with 95% CI
#
# RMST model:
#   Weibull accelerated failure-time model
#
# RMST confidence intervals:
#   1000 stratified non-parametric bootstrap samples
#   Percentile-based 95% confidence intervals
#
# Stability:
#   - bootstrap samples run sequentially
#   - crude and IPTW models use the same bootstrap sample
#   - existing truncated IPTW weights are retained


print("CREATE D5_ResultsRMST")

############################################################

if (TEST) {
  testname <- "test_33_D5_ResultsRMST"
  thisdirinput <- file.path(
    dirtest,
    testname
  )
  thisdiroutput <- file.path(
    thisdirinput,
    "g_output"
  )
  thisdirsettings <- thisdirinput
  dir.create(
    thisdiroutput,
    recursive = TRUE,
    showWarnings = FALSE
  )
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}

############################################################
# 3. SETTINGS
############################################################

settings <- readRDS(
  file.path(
    thisdirsettings,
    "SETTINGS.rds"
  )
)
settings <- as.data.table(settings)
thissubpopulations <- unique(
  settings[, subpop]
)
thissubpopulations <- unlist(
  strsplit(
    trimws(thissubpopulations),
    " +"
  )
)

############################################################
# 4. ANALYSIS DEFINITIONS
############################################################

analysis_definitions <- list(
  E1 = list(
    outcome_col = "E1_outcome",
    fup_col = "E1_fup",
    calculate_rmst = FALSE,
    tau = NA_real_,
    label = "E1"
  ),
  E2_year1 = list(
    outcome_col = "E2b_outcome",
    fup_col = "E2b_fup",
    calculate_rmst = TRUE,
    tau = 1,
    label = "E2_year1"
  ),
  E2_year2 = list(
    outcome_col = "E2a_outcome",
    fup_col = "E2a_fup",
    calculate_rmst = TRUE,
    tau = 2,
    label = "E2_year2"
  ),
  E3 = list(
    outcome_col = "E3_outcome",
    fup_col = "E3_fup",
    calculate_rmst = FALSE,
    tau = NA_real_,
    label = "E3"
  )
)

############################################################
# 5. BOOTSTRAP PARAMETERS
############################################################

dataset_values <- unique(as.character(settings$ds))
if (length(dataset_values) != 1L) {
  stop("settings$ds must contain exactly one dataset name.")
}

# Diagnostic mode is activated automatically and exclusively for SIDIAP.
debug_rmst <- identical(dataset_values, "SIDIAP")
B <- if (debug_rmst) 50L else 1000L
bootstrap_seed <- 1234L

# Print warnings immediately, close to the analysis that generated them.
if (debug_rmst) {
  options(warn = 1)
}
if (debug_rmst) {
  # Sequential processing avoids Windows PSOCK worker failures and
  # non-exportable external-pointer errors.
  future::plan(
    future::sequential
  )
}else{
  available_cores <- future::availableCores()
  maximum_workers <- 2L
  n_workers <- max(
    1L,
    min(
      maximum_workers,
      available_cores - 1L
    )
  )
  
  if (n_workers > 1L) {
    
    
    # DG-CHANGES: the following part was modified by DG on 31-07-2026 in connection with Github issue #53
    
    future::plan(future::sequential)
    gc()
    
    # end of DG-CHANGES
    
    
    future::plan(
      future::multisession,
      workers = n_workers
    )
    
  } else {
    
    future::plan(
      future::sequential
    )
  }
}


############################################################
# 7. EMPTY RMST RESULT
############################################################
empty_rmst_result <- function() {
  c(
    rmst_0 = NA_real_,
    rmst_1 = NA_real_,
    difference = NA_real_
  )
}

############################################################
# 8. PERCENTILE CONFIDENCE INTERVAL
############################################################
percentile_ci <- function(x) {
  x <- x[
    is.finite(x)
  ]
  if (length(x) < 2L) {
    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }
  stats::quantile(
    x,
    probs = c(
      0.025,
      0.975
    ),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
}

############################################################
# 9. STABLE WEIBULL AFT RMST FUNCTION
############################################################
estimate_rmst_fast <- function(
  dt,
  tau,
  weight_col = NULL,
  calculate_hessian = FALSE,
  print_errors = FALSE
) {
  dt <- as.data.table(dt)
  fail_rmst <- function(stage, detail = NULL) {
    if (print_errors) {
      message(
        "RMST failed [", stage, "]",
        if (!is.null(detail)) paste0(": ", detail) else ""
      )
    }
    empty_rmst_result()
  }
  required_variables <- c(
    "FUP_years",
    "outcome",
    "exposure"
  )
  missing_variables <- setdiff(
    required_variables,
    names(dt)
  )
  if (length(missing_variables) > 0L) {
    if (print_errors) {
      message(
        "Missing variables: ",
        paste(missing_variables, collapse = ", ")
      )
    }
    return(fail_rmst("missing variables", paste(missing_variables, collapse = ", ")))
  }
  valid_rows <- (
    !is.na(dt$FUP_years) &
      is.finite(dt$FUP_years) &
      dt$FUP_years > 0 &
      !is.na(dt$outcome) &
      dt$outcome %in% c(0L, 1L) &
      !is.na(dt$exposure) &
      dt$exposure %in% c(0L, 1L)
  )
  dt <- dt[valid_rows]
  if (!is.null(weight_col)) {
    if (!weight_col %in% names(dt)) {
      if (print_errors) {
        message("Weight variable not found: ", weight_col)
      }
      return(fail_rmst("weight variable not found", weight_col))
    }
    valid_weight <- (
      !is.na(dt[[weight_col]]) &
        is.finite(dt[[weight_col]]) &
        dt[[weight_col]] > 0
    )
    dt <- dt[valid_weight]
  }
  if (nrow(dt) == 0L) {
    return(fail_rmst("no valid observations"))
  }
  if (!all(c(0L, 1L) %in% dt$exposure)) {
    return(fail_rmst("one exposure group is absent"))
  }
  events_by_group <- dt[
    ,
    .(
      events = sum(outcome == 1L, na.rm = TRUE)
    ),
    by = exposure
  ]
  if (
    nrow(events_by_group) < 2L ||
    any(events_by_group$events == 0L)
  ) {
    return(fail_rmst(
      "no events in one exposure group",
      paste0(
        "events by group: ",
        paste(events_by_group$exposure, events_by_group$events, sep = "=", collapse = ", ")
      )
    ))
  }
  # Fit the Weibull AFT model with survival::survreg.
  # This avoids the flexsurvreg initial-value failure.
  fit <- tryCatch(
    {
      if (is.null(weight_col)) {
        survival::survreg(
          survival::Surv(FUP_years, outcome) ~ exposure,
          data = dt,
          dist = "weibull",
          x = FALSE,
          y = FALSE,
          model = FALSE
        )
      } else {
        survival::survreg(
          survival::Surv(FUP_years, outcome) ~ exposure,
          data = dt,
          dist = "weibull",
          weights = dt[[weight_col]],
          x = FALSE,
          y = FALSE,
          model = FALSE
        )
      }
    },
    error = function(e) {
      if (print_errors) {
        message(
          "survreg failed: ",
          conditionMessage(e)
        )
      }
      NULL
    }
  )
  if (is.null(fit)) {
    return(fail_rmst("survreg returned NULL"))
  }
  sigma <- as.numeric(fit$scale)
  if (
    length(sigma) != 1L ||
    !is.finite(sigma) ||
    sigma <= 0
  ) {
    return(fail_rmst("invalid Weibull scale", paste(sigma, collapse = ", ")))
  }
  weibull_shape <- 1 / sigma
  predict_one_rmst <- function(exposure_value) {
    linear_predictor <- tryCatch(
      as.numeric(
        stats::predict(
          fit,
          newdata = data.frame(
            exposure = exposure_value
          ),
          type = "lp"
        )
      )[1L],
      error = function(e) {
        if (print_errors) {
          message(
            "predict() failed for exposure ", exposure_value,
            ": ", conditionMessage(e)
          )
        }
        NA_real_
      }
    )
    if (!is.finite(linear_predictor)) {
      return(NA_real_)
    }
    weibull_scale <- exp(linear_predictor)
    if (
      !is.finite(weibull_scale) ||
      weibull_scale <= 0
    ) {
      return(NA_real_)
    }
    tryCatch(
      as.numeric(
        stats::integrate(
          f = function(time) {
            exp(
              -(time / weibull_scale) ^ weibull_shape
            )
          },
          lower = 0,
          upper = tau,
          subdivisions = 100L,
          rel.tol = 1e-7,
          stop.on.error = FALSE
        )$value
      ),
      error = function(e) {
        if (print_errors) {
          message(
            "integrate() failed for exposure ", exposure_value,
            ": ", conditionMessage(e)
          )
        }
        NA_real_
      }
    )
  }
  rmst_0 <- predict_one_rmst(0L)
  rmst_1 <- predict_one_rmst(1L)
  if (
    !is.finite(rmst_0) ||
    !is.finite(rmst_1)
  ) {
    return(fail_rmst(
      "non-finite RMST",
      paste0("rmst_0=", rmst_0, ", rmst_1=", rmst_1)
    ))
  }
  c(
    rmst_0 = rmst_0,
    rmst_1 = rmst_1,
    difference = rmst_1 - rmst_0
  )
}

############################################################
# 9B. DIAGNOSTIC WEIBULL MODEL
############################################################
diagnose_survreg <- function(
  dt,
  tau,
  analysis_label,
  model_label,
  weight_col = NULL
) {
  diagnostic_data <- as.data.table(copy(dt))
  diagnostic_data <- diagnostic_data[
    is.finite(FUP_years) &
      FUP_years > 0 &
      outcome %in% c(0L, 1L) &
      exposure %in% c(0L, 1L)
  ]
  if (!is.null(weight_col)) {
    diagnostic_data <- diagnostic_data[
      is.finite(get(weight_col)) & get(weight_col) > 0
    ]
  }
  cat(
    "\n--- DIAGNOSTIC MODEL ", model_label,
    ": ", analysis_label, " ---\n",
    sep = ""
  )
  model_data_summary <- diagnostic_data[
    ,
    .(
      rows = .N,
      patients = uniqueN(person_id),
      events = sum(outcome == 1L),
      events_before_tau = sum(outcome == 1L & FUP_years <= tau),
      min_fup = min(FUP_years),
      median_fup = median(FUP_years),
      max_fup = max(FUP_years),
      min_weight = if (!is.null(weight_col)) min(get(weight_col)) else NA_real_,
      max_weight = if (!is.null(weight_col)) max(get(weight_col)) else NA_real_
    ),
    by = exposure
  ]
  cat("Model", model_label," \n")
  print(model_data_summary)
  captured_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      {
        if (is.null(weight_col)) {
          survival::survreg(
            survival::Surv(FUP_years, outcome) ~ exposure,
            data = diagnostic_data,
            dist = "weibull",
            x = TRUE,
            y = TRUE,
            model = TRUE
          )
        } else {
          survival::survreg(
            survival::Surv(FUP_years, outcome) ~ exposure,
            data = diagnostic_data,
            dist = "weibull",
            weights = diagnostic_data[[weight_col]],
            x = TRUE,
            y = TRUE,
            model = TRUE
          )
        }
      },
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message("DIAGNOSTIC survreg error [", model_label, "]: ", conditionMessage(e))
      NULL
    }
  )
  if (length(captured_warnings) > 0L) {
    message(
      "DIAGNOSTIC survreg warnings [", model_label, "]: ",
      paste(unique(captured_warnings), collapse = " | ")
    )
  }
  if (is.null(fit)) {
    return(list(fit = NULL, summary = model_data_summary))
  }
  cat("Coefficients:\n")
  print(stats::coef(fit), digits = 12)
  cat("survreg scale:", fit$scale, "\n")
  predictions <- tryCatch(
    stats::predict(
      fit,
      newdata = data.frame(exposure = c(0L, 1L)),
      type = "lp"
    ),
    error = function(e) {
      message("DIAGNOSTIC predict error [", model_label, "]: ", conditionMessage(e))
      c(NA_real_, NA_real_)
    }
  )
  cat("Linear predictions for exposure 0 and 1:\n")
  print(predictions, digits = 12)
  diagnostic_rmst <- c(rmst_0 = NA_real_, rmst_1 = NA_real_, difference = NA_real_)
  if (
    length(predictions) == 2L &&
    all(is.finite(predictions)) &&
    is.finite(fit$scale) &&
    fit$scale > 0
  ) {
    shape <- 1 / fit$scale
    scales <- exp(predictions)
    rmst_values <- vapply(
      scales,
      FUN.VALUE = numeric(1L),
      FUN = function(scale_value) {
        tryCatch(
          stats::integrate(
            function(time) exp(-(time / scale_value)^shape),
            lower = 0,
            upper = tau
          )$value,
          error = function(e) {
            message("DIAGNOSTIC integrate error [", model_label, "]: ", conditionMessage(e))
            NA_real_
          }
        )
      }
    )
    diagnostic_rmst <- c(
      rmst_0 = rmst_values[1L],
      rmst_1 = rmst_values[2L],
      difference = rmst_values[2L] - rmst_values[1L]
    )
  }
  cat("Diagnostic RMST (not rounded):\n")
  print(diagnostic_rmst, digits = 12)
  list(
    fit = fit,
    summary = model_data_summary,
    predictions = predictions,
    rmst = diagnostic_rmst,
    warnings = captured_warnings
  )
}

############################################################
# 10. SEQUENTIAL BOOTSTRAP FUNCTION
############################################################
bootstrap_rmst_sequential <- function(
  dt,
  tau,
  B = 1000L,
  weight_col = "weight",
  seed = 1234L
) {
  dt <- as.data.table(dt)
  ##########################################################
  # Exposure-specific row indices
  ##########################################################
  control_rows <- which(
    dt$exposure == 0L
  )
  exposed_rows <- which(
    dt$exposure == 1L
  )
  n_control <- length(
    control_rows
  )
  n_exposed <- length(
    exposed_rows
  )
  if (
    n_control == 0L ||
    n_exposed == 0L
  ) {
    warning(
      "The bootstrap was skipped because one exposure group is empty."
    )
    return(
      list(
        estimates = data.table(),
        ci = data.table(
          crude_rmst_0_lb = NA_real_,
          crude_rmst_0_ub = NA_real_,
          crude_rmst_1_lb = NA_real_,
          crude_rmst_1_ub = NA_real_,
          crude_difference_lb = NA_real_,
          crude_difference_ub = NA_real_,
          iptw_rmst_0_lb = NA_real_,
          iptw_rmst_0_ub = NA_real_,
          iptw_rmst_1_lb = NA_real_,
          iptw_rmst_1_ub = NA_real_,
          iptw_difference_lb = NA_real_,
          iptw_difference_ub = NA_real_,
          requested_bootstraps = B,
          successful_bootstraps_crude = 0L,
          successful_bootstraps_iptw = 0L
        )
      )
    )
  }
  ##########################################################
  # Check whether adjusted analysis is available
  ##########################################################
  calculate_iptw <- (
    !is.null(weight_col) &&
      weight_col %in% names(dt)
  )
  ##########################################################
  # Run bootstrap replicates
  ##########################################################
  set.seed(seed)
  bootstrap_list <- lapply(
    X = seq_len(B),
    FUN = function(b) {
      sampled_control_rows <- sample(
        control_rows,
        size = n_control,
        replace = TRUE
      )
      sampled_exposed_rows <- sample(
        exposed_rows,
        size = n_exposed,
        replace = TRUE
      )
      sampled_rows <- c(
        sampled_control_rows,
        sampled_exposed_rows
      )
      bootstrap_data <- dt[
        sampled_rows
      ]
      ######################################################
      # Crude model
      ######################################################
      crude_estimate <- estimate_rmst_fast(
        dt = bootstrap_data,
        tau = tau,
        weight_col = NULL,
        calculate_hessian = FALSE,
        print_errors = FALSE
      )
      ######################################################
      # IPTW-adjusted model
      ######################################################
      if (calculate_iptw) {
        iptw_estimate <- estimate_rmst_fast(
          dt = bootstrap_data,
          tau = tau,
          weight_col = weight_col,
          calculate_hessian = FALSE,
          print_errors = FALSE
        )
      } else {
        iptw_estimate <- empty_rmst_result()
      }
      ######################################################
      # Return one bootstrap row
      ######################################################
      c(
        crude_rmst_0 =
          unname(
            crude_estimate["rmst_0"]
          ),
        crude_rmst_1 =
          unname(
            crude_estimate["rmst_1"]
          ),
        crude_difference =
          unname(
            crude_estimate["difference"]
          ),
        iptw_rmst_0 =
          unname(
            iptw_estimate["rmst_0"]
          ),
        iptw_rmst_1 =
          unname(
            iptw_estimate["rmst_1"]
          ),
        iptw_difference =
          unname(
            iptw_estimate["difference"]
          )
      )
    }
  )
  ##########################################################
  # Combine bootstrap estimates
  ##########################################################
  bootstrap_matrix <- do.call(
    rbind,
    bootstrap_list
  )
  bootstrap_results <- as.data.table(
    bootstrap_matrix
  )
  expected_bootstrap_names <- c(
    "crude_rmst_0",
    "crude_rmst_1",
    "crude_difference",
    "iptw_rmst_0",
    "iptw_rmst_1",
    "iptw_difference"
  )
  setnames(
    bootstrap_results,
    expected_bootstrap_names
  )
  ##########################################################
  # Calculate percentile confidence intervals
  ##########################################################
  crude_rmst_0_ci <- percentile_ci(
    bootstrap_results$crude_rmst_0
  )
  crude_rmst_1_ci <- percentile_ci(
    bootstrap_results$crude_rmst_1
  )
  crude_difference_ci <- percentile_ci(
    bootstrap_results$crude_difference
  )
  iptw_rmst_0_ci <- percentile_ci(
    bootstrap_results$iptw_rmst_0
  )
  iptw_rmst_1_ci <- percentile_ci(
    bootstrap_results$iptw_rmst_1
  )
  iptw_difference_ci <- percentile_ci(
    bootstrap_results$iptw_difference
  )
  ##########################################################
  # Number of successful bootstrap estimates
  ##########################################################
  successful_crude <- sum(
    is.finite(
      bootstrap_results$crude_difference
    )
  )
  successful_iptw <- sum(
    is.finite(
      bootstrap_results$iptw_difference
    )
  )
  ##########################################################
  # Bootstrap CI output
  ##########################################################
  bootstrap_ci <- data.table(
    crude_rmst_0_lb =
      crude_rmst_0_ci[1L],
    crude_rmst_0_ub =
      crude_rmst_0_ci[2L],
    crude_rmst_1_lb =
      crude_rmst_1_ci[1L],
    crude_rmst_1_ub =
      crude_rmst_1_ci[2L],
    crude_difference_lb =
      crude_difference_ci[1L],
    crude_difference_ub =
      crude_difference_ci[2L],
    iptw_rmst_0_lb =
      iptw_rmst_0_ci[1L],
    iptw_rmst_0_ub =
      iptw_rmst_0_ci[2L],
    iptw_rmst_1_lb =
      iptw_rmst_1_ci[1L],
    iptw_rmst_1_ub =
      iptw_rmst_1_ci[2L],
    iptw_difference_lb =
      iptw_difference_ci[1L],
    iptw_difference_ub =
      iptw_difference_ci[2L],
    requested_bootstraps = B,
    successful_bootstraps_crude =
      successful_crude,
    successful_bootstraps_iptw =
      successful_iptw
  )
  list(
    estimates = bootstrap_results,
    ci = bootstrap_ci
  )
}

############################################################
# 11. SUBPOPULATION LOOP
############################################################
subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(
    subpop == "all",
    "",
    paste0(
      "_",
      subpop
    )
  )
  cat(
    "\nSubpopulation:",
    subpop,
    "\n"
  )
  ##########################################################
  # Load analytical dataset
  ##########################################################
  analytical_dataset <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_ANALYTICAL_DATASET_HR_RMST_TP",
        suffix,
        ".rds"
      )
    )
  )
  analytical_dataset <- as.data.table(
    analytical_dataset
  )
  D4_EXP_ACT_COV_WEIGHT <-  readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_EXP_ACT_COV_WEIGHT",
        suffix,
        ".rds"
      )
    )
  )
  if (debug_rmst) {
    ##########################################################
    # Diagnostic checks before merging weights
    ##########################################################
    n_rows_before_merge <- nrow(analytical_dataset)
    n_patients_before_merge <- uniqueN(analytical_dataset$person_id)
    duplicate_weights <- D4_EXP_ACT_COV_WEIGHT[
      , .N, by = person_id
    ][N > 1L]
    conflicting_weights <- D4_EXP_ACT_COV_WEIGHT[
      , .(n_distinct_weights = uniqueN(weight)), by = person_id
    ][n_distinct_weights > 1L]
    cat(
      "Rows before weight merge:", n_rows_before_merge, "\n",
      "Unique patients before weight merge:", n_patients_before_merge, "\n",
      "person_id duplicated in weight data:", nrow(duplicate_weights), "\n",
      "person_id with conflicting weights:", nrow(conflicting_weights), "\n"
    )
    if (nrow(duplicate_weights) > 0L) {
      warning(
        nrow(duplicate_weights),
        " person_id values occur more than once in the weight data."
      )
      print(head(duplicate_weights, 20L))
    }
    if (nrow(conflicting_weights) > 0L) {
      print(head(conflicting_weights, 20L))
      stop(
        "The weight data contain different weights for the same person_id. ",
        "Resolve this before estimating RMST."
      )
    }
    weights_for_merge <- unique(
      D4_EXP_ACT_COV_WEIGHT[, .(person_id, weight)],
      by = "person_id"
    )
    analytical_dataset <- merge(
      analytical_dataset,
      weights_for_merge,
      by = "person_id",
      all.x = TRUE
    )
    cat(
      "Rows after weight merge:", nrow(analytical_dataset), "\n",
      "Unique patients after weight merge:", uniqueN(analytical_dataset$person_id), "\n",
      "Patients without weight:", uniqueN(analytical_dataset[is.na(weight), person_id]), "\n"
    )
    if (nrow(analytical_dataset) != n_rows_before_merge) {
      warning("The number of analytical rows changed after the weight merge.")
    }
  } else {
    analytical_dataset <- merge(
      analytical_dataset,
      D4_EXP_ACT_COV_WEIGHT[, .(person_id, weight)],
      by = "person_id"
    )
  }
  ##########################################################
  # Validate required dataset columns
  ##########################################################
  required_dataset_columns <- c(
    "person_id",
    "exposure",
    "E1_outcome",
    "E1_fup",
    "E2a_outcome",
    "E2a_fup",
    "E2b_outcome",
    "E2b_fup",
    "E3_outcome",
    "E3_fup"
  )
  missing_dataset_columns <- setdiff(
    required_dataset_columns,
    names(analytical_dataset)
  )
  if (length(missing_dataset_columns) > 0L) {
    stop(
      "Missing variables in D4_ANALYTICAL_DATASET_HR_RMST_TP",
      suffix,
      ": ",
      paste(
        missing_dataset_columns,
        collapse = ", "
      )
    )
  }
  ##########################################################
  # Initialize analysis results list
  ##########################################################
  results_list <- vector(
    mode = "list",
    length = length(
      analysis_definitions
    )
  )
  names(results_list) <- names(
    analysis_definitions
  )
  ##########################################################
  # 12. ANALYSIS LOOP
  ##########################################################
  for (
    analysis_name in names(
      analysis_definitions
    )
  ) {
    definition <- analysis_definitions[[analysis_name]]
    outcome_col <- definition$outcome_col
    fup_col <- definition$fup_col
    calculate_rmst <- definition$calculate_rmst
    tau <- definition$tau
    analysis_label <- definition$label
    # cat(
    #   "\nRunning analysis:",
    #   analysis_label,
    #   "\n"
    # )
    ########################################################
    # Create analysis input
    ########################################################
    input <- analytical_dataset[
      ,
      .(
        person_id,
        exposure,
        FUP = as.numeric(
          get(fup_col)
        ),
        outcome = as.integer(
          get(outcome_col)
        ),
        weight = if (
          "weight" %in%
          names(analytical_dataset)
        ) {
          as.numeric(
            weight
          )
        } else {
          NA_real_
        }
      )
    ]
    input[
      ,
      FUP_years := FUP / 365.25
    ]
    input[
      ,
      exposure := as.integer(
        exposure
      )
    ]
    ########################################################
    # Keep valid observations
    ########################################################
    input <- input[
      !is.na(person_id) &
        !is.na(exposure) &
        exposure %in% c(0L, 1L) &
        !is.na(FUP) &
        is.finite(FUP) &
        FUP >= 0 &
        !is.na(outcome) &
        outcome %in% c(0L, 1L)
    ]
    ########################################################
    # Check IPTW weight availability
    ########################################################
    has_weight <- (
      "weight" %in%
        names(analytical_dataset) &&
        any(
          is.finite(
            input$weight
          ) &
            input$weight > 0,
          na.rm = TRUE
        )
    )
    ########################################################
    # Descriptive results by exposure group
    ########################################################
    processing <- input[
      ,
      .(
        N_ = uniqueN(
          person_id
        ),
        N_outcome_ = sum(
          outcome == 1L,
          na.rm = TRUE
        ),
        PY_ = sum(
          FUP_years,
          na.rm = TRUE
        )
      ),
      by = .(
        exposure_stratum = exposure
      )
    ]
    ########################################################
    # Ensure exposure 0 and exposure 1 rows
    ########################################################
    exposure_template <- data.table(
      exposure_stratum = c(
        0L,
        1L
      )
    )
    processing <- merge(
      exposure_template,
      processing,
      by = "exposure_stratum",
      all.x = TRUE,
      sort = TRUE
    )
    ########################################################
    # Incidence rate per 1,000 person-years
    ########################################################
    processing[
      ,
      IR_ := fifelse(
        !is.na(PY_) &
          PY_ > 0,
        1000 *
          N_outcome_ /
          PY_,
        NA_real_
      )
    ]
    ########################################################
    # Incidence-rate 95% CI
    ########################################################
    if (exists("exactPoiCI")) {
      incidence_ci <- tryCatch(
        {
          exactPoiCI(
            processing,
            "N_outcome_",
            "PY_",
            conversion_factor = 1,
            OoM = 3
          )
        },
        error = function(e) {
          cat(
            "exactPoiCI failed for ",
            analysis_label,
            ": ",
            conditionMessage(e),
            "\n",
            sep = ""
          )
          NULL
        }
      )
      if (!is.null(incidence_ci)) {
        processing[
          ,
          `:=`(
            IR_lb_ =
              incidence_ci[[2]],
            IR_ub_ =
              incidence_ci[[3]]
          )
        ]
      } else {
        processing[
          ,
          `:=`(
            IR_lb_ = NA_real_,
            IR_ub_ = NA_real_
          )
        ]
      }
    } else {
      warning(
        "Function exactPoiCI() was not found. ",
        "Incidence-rate confidence intervals were set to NA."
      )
      processing[
        ,
        `:=`(
          IR_lb_ = NA_real_,
          IR_ub_ = NA_real_
        )
      ]
    }
    ########################################################
    # 13. RMST FOR E2 ONLY
    ########################################################
    if (calculate_rmst) {
      cat(
        "Calculating RMST at",
        tau,
        "year(s)\n"
      )
      ######################################################
      # Input diagnostics, saved for inspection
      ######################################################
      if (debug_rmst) {
        rmst_input_diagnostics <- input[
          ,
          .(
            rows = .N,
            patients = uniqueN(person_id),
            events = sum(outcome == 1L, na.rm = TRUE),
            non_events = sum(outcome == 0L, na.rm = TRUE),
            missing_outcome = sum(is.na(outcome)),
            zero_fup = sum(FUP_years == 0, na.rm = TRUE),
            negative_fup = sum(FUP_years < 0, na.rm = TRUE),
            missing_fup = sum(is.na(FUP_years)),
            non_finite_fup = sum(!is.finite(FUP_years)),
            min_fup = suppressWarnings(min(FUP_years, na.rm = TRUE)),
            median_fup = suppressWarnings(median(FUP_years, na.rm = TRUE)),
            max_fup = suppressWarnings(max(FUP_years, na.rm = TRUE)),
            events_before_tau = sum(
              outcome == 1L & FUP_years <= tau,
              na.rm = TRUE
            ),
            subjects_at_or_beyond_tau = sum(FUP_years >= tau, na.rm = TRUE),
            valid_weight = sum(is.finite(weight) & weight > 0, na.rm = TRUE),
            invalid_weight = sum(
              is.na(weight) | !is.finite(weight) | weight <= 0
            )
          ),
          by = exposure
        ]
        cat("RMST input diagnostics:\n")
        print(rmst_input_diagnostics)
        diagnostic_prefix <- paste0(
          "RMST_diagnostics_", analysis_label, suffix
        )
        data.table::fwrite(
          rmst_input_diagnostics,
          file.path(thisdiroutput, paste0(diagnostic_prefix, "_input.csv"))
        )
        diagnostic_crude <- diagnose_survreg(
          dt = input,
          tau = tau,
          analysis_label = analysis_label,
          model_label = "CRUDE",
          weight_col = NULL
        )
        diagnostic_iptw <- if (has_weight) {
          diagnose_survreg(
            dt = input,
            tau = tau,
            analysis_label = analysis_label,
            model_label = "IPTW",
            weight_col = "weight"
          )
        } else {
          NULL
        }
        saveRDS(
          list(
            input_summary = rmst_input_diagnostics,
            crude = diagnostic_crude,
            iptw = diagnostic_iptw
          ),
          file.path(thisdiroutput, paste0(diagnostic_prefix, "_models.rds"))
        )
      }
      ######################################################
      # Crude point estimate
      ######################################################
      crude_point <- estimate_rmst_fast(
        dt = input,
        tau = tau,
        weight_col = NULL,
        calculate_hessian = TRUE,
        print_errors = debug_rmst
      )
      if (debug_rmst) {
        cat("Crude point estimate (not rounded):\n")
        print(crude_point, digits = 12)
      }
      ######################################################
      # IPTW-adjusted point estimate
      ######################################################
      if (has_weight) {
        iptw_point <- estimate_rmst_fast(
          dt = input,
          tau = tau,
          weight_col = "weight",
          calculate_hessian = TRUE,
          print_errors = debug_rmst
        )
        if (debug_rmst) {
          cat("IPTW point estimate (not rounded):\n")
          print(iptw_point, digits = 12)
        }
      } else {
        warning(
          "No valid weight values were found for ",
          analysis_label,
          ". Adjusted estimates were set to NA."
        )
        iptw_point <- empty_rmst_result()
      }
      ######################################################
      # Sequential bootstrap
      ######################################################
      bootstrap_result <- bootstrap_rmst_sequential(
        dt = input,
        tau = tau,
        B = B,
        weight_col = if (
          has_weight
        ) {
          "weight"
        } else {
          NULL
        },
        seed = bootstrap_seed +
          match(
            analysis_name,
            names(
              analysis_definitions
            )
          )
      )
      bootstrap_ci <- bootstrap_result$ci
      if (debug_rmst) {
        data.table::fwrite(
          bootstrap_result$estimates,
          file.path(
            thisdiroutput,
            paste0(diagnostic_prefix, "_bootstrap.csv")
          )
        )
        cat("Bootstrap diagnostics:\n")
        print(bootstrap_ci)
        if (
          bootstrap_ci$successful_bootstraps_crude <
          0.8 * bootstrap_ci$requested_bootstraps
        ) {
          warning(
            "Fewer than 80% of crude bootstrap models succeeded for ",
            analysis_label, "."
          )
        }
        if (
          has_weight &&
          bootstrap_ci$successful_bootstraps_iptw <
          0.8 * bootstrap_ci$requested_bootstraps
        ) {
          warning(
            "Fewer than 80% of IPTW bootstrap models succeeded for ",
            analysis_label, "."
          )
        }
      }
      ######################################################
      # Crude RMST by exposure group
      ######################################################
      processing[
        ,
        RMST_ := fifelse(
          exposure_stratum == 0L,
          unname(
            crude_point["rmst_0"]
          ),
          unname(
            crude_point["rmst_1"]
          )
        )
      ]
      processing[
        ,
        RMST_lb_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            crude_rmst_0_lb,
          bootstrap_ci$
            crude_rmst_1_lb
        )
      ]
      processing[
        ,
        RMST_ub_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            crude_rmst_0_ub,
          bootstrap_ci$
            crude_rmst_1_ub
        )
      ]
      ######################################################
      # Crude RMST difference
      #
      # Exposed minus unexposed
      ######################################################
      processing[
        ,
        Diff_RMST_crude_ := fifelse(
          exposure_stratum == 1L,
          unname(
            crude_point["difference"]
          ),
          NA_real_
        )
      ]
      processing[
        ,
        Diff_RMST_crude_lb_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            crude_difference_lb,
          NA_real_
        )
      ]
      processing[
        ,
        Diff_RMST_crude_ub_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            crude_difference_ub,
          NA_real_
        )
      ]
      ######################################################
      # IPTW-adjusted RMST by exposure group
      ######################################################
      processing[
        ,
        RMST_adj_ := fifelse(
          exposure_stratum == 0L,
          unname(
            iptw_point["rmst_0"]
          ),
          unname(
            iptw_point["rmst_1"]
          )
        )
      ]
      processing[
        ,
        RMST_adj_lb_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            iptw_rmst_0_lb,
          bootstrap_ci$
            iptw_rmst_1_lb
        )
      ]
      processing[
        ,
        RMST_adj_ub_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            iptw_rmst_0_ub,
          bootstrap_ci$
            iptw_rmst_1_ub
        )
      ]
      ######################################################
      # IPTW-adjusted RMST difference
      ######################################################
      processing[
        ,
        Diff_RMST_adj_ := fifelse(
          exposure_stratum == 1L,
          unname(
            iptw_point["difference"]
          ),
          NA_real_
        )
      ]
      processing[
        ,
        Diff_RMST_adj_lb_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            iptw_difference_lb,
          NA_real_
        )
      ]
      processing[
        ,
        Diff_RMST_adj_ub_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            iptw_difference_ub,
          NA_real_
        )
      ]
      ######################################################
      # Bootstrap diagnostics
      ######################################################
      processing[
        ,
        `:=`(
          bootstrap_samples =
            bootstrap_ci$
            requested_bootstraps,
          successful_bootstraps_crude =
            bootstrap_ci$
            successful_bootstraps_crude,
          successful_bootstraps_iptw =
            bootstrap_ci$
            successful_bootstraps_iptw
        )
      ]
    } else {
      ######################################################
      # No RMST for E1 and E3
      ######################################################
      processing[
        ,
        `:=`(
          RMST_ = NA_real_,
          RMST_lb_ = NA_real_,
          RMST_ub_ = NA_real_,
          Diff_RMST_crude_ =
            NA_real_,
          Diff_RMST_crude_lb_ =
            NA_real_,
          Diff_RMST_crude_ub_ =
            NA_real_,
          RMST_adj_ = NA_real_,
          RMST_adj_lb_ = NA_real_,
          RMST_adj_ub_ = NA_real_,
          Diff_RMST_adj_ =
            NA_real_,
          Diff_RMST_adj_lb_ =
            NA_real_,
          Diff_RMST_adj_ub_ =
            NA_real_,
          bootstrap_samples =
            NA_integer_,
          successful_bootstraps_crude =
            NA_integer_,
          successful_bootstraps_iptw =
            NA_integer_
        )
      ]
    }
    ########################################################
    # Analysis metadata
    ########################################################
    processing[
      ,
      `:=`(
        analysisRMST =
          analysis_label,
        outcome_variable =
          outcome_col,
        followup_variable =
          fup_col,
        RMST_year =
          tau,
        ds = paste0(
          settings$ds,
          suffix
        )
      )
    ]
    ########################################################
    # Store analysis output
    ########################################################
    results_list[[analysis_name]] <- processing
  }
  ##########################################################
  # 14. COMBINE ALL ANALYSES
  ##########################################################
  results_list <- Filter(
    Negate(
      is.null
    ),
    results_list
  )
  if (length(results_list) == 0L) {
    stop(
      "No analysis results were produced."
    )
  }
  D5_ResultsRMST <- rbindlist(
    results_list,
    use.names = TRUE,
    fill = TRUE
  )
  ##########################################################
  # 15. ROUND OUTPUT VARIABLES
  ##########################################################
  columns_to_round <- c(
    "PY_",
    "IR_",
    "IR_lb_",
    "IR_ub_",
    "RMST_",
    "RMST_lb_",
    "RMST_ub_",
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    "Diff_RMST_crude_",
    "Diff_RMST_crude_lb_",
    "Diff_RMST_crude_ub_",
    "Diff_RMST_adj_",
    "Diff_RMST_adj_lb_",
    "Diff_RMST_adj_ub_"
  )
  columns_to_round <- intersect(
    columns_to_round,
    names(
      D5_ResultsRMST
    )
  )
  D5_ResultsRMST[
    ,
    (columns_to_round) := lapply(
      .SD,
      round,
      digits = 2
    ),
    .SDcols = columns_to_round
  ]
  ##########################################################
  # 16. RENAME FINAL VARIABLES
  ##########################################################
  old_names <- c(
    "IR_lb_",
    "IR_ub_",
    "Diff_RMST_crude_",
    "Diff_RMST_crude_lb_",
    "Diff_RMST_crude_ub_",
    "Diff_RMST_adj_",
    "Diff_RMST_adj_lb_",
    "Diff_RMST_adj_ub_"
  )
  new_names <- c(
    "IRll_",
    "IRul_",
    "RMSTdiff_",
    "RMSTdiffll_",
    "RMSTdifful_",
    "RMSTdiff_adj_",
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_"
  )
  variables_present <- old_names %in%
    names(
      D5_ResultsRMST
    )
  setnames(
    D5_ResultsRMST,
    old = old_names[
      variables_present
    ],
    new = new_names[
      variables_present
    ]
  )
  ##########################################################
  # 17. SELECT FINAL OUTPUT COLUMNS
  ##########################################################
  variables_to_keep <- c(
    "ds",
    "analysisRMST",
    "outcome_variable",
    "followup_variable",
    "exposure_stratum",
    "RMST_year",
    "N_",
    "N_outcome_",
    "PY_",
    "IR_",
    "IRll_",
    "IRul_",
    "RMST_",
    "RMST_lb_",
    "RMST_ub_",
    "RMSTdiff_",
    "RMSTdiffll_",
    "RMSTdifful_",
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    "RMSTdiff_adj_",
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_",
    "bootstrap_samples",
    "successful_bootstraps_crude",
    "successful_bootstraps_iptw"
  )
  variables_to_keep <- intersect(
    variables_to_keep,
    names(
      D5_ResultsRMST
    )
  )
  D5_ResultsRMST <- D5_ResultsRMST[
    ,
    ..variables_to_keep
  ]
  ##########################################################
  # 18. SET FINAL ANALYSIS ORDER
  ##########################################################
  D5_ResultsRMST[
    ,
    analysis_order := match(
      analysisRMST,
      c(
        "E1",
        "E2_year1",
        "E2_year2",
        "E3"
      )
    )
  ]
  setorder(
    D5_ResultsRMST,
    analysis_order,
    exposure_stratum
  )
  D5_ResultsRMST[
    ,
    analysis_order := NULL
  ]
  #########################################
  # cleaning
  keep_cols <- c(
    "ds",
    "analysisRMST",
    "exposure_stratum",
    "N_",
    "N_outcome_",
    "PY_",
    "IR_" ,
    "IRll_",
    "IRul_" ,
    "RMST_" ,
    "RMST_lb_",
    "RMST_ub_",
    "RMSTdiff_" ,
    "RMSTdiffll_",
    "RMSTdifful_",
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    "RMSTdiff_adj_" ,
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_"
  )   #, covariate_after_intervals, do not keep since we have different values in time
  D5_ResultsRMST <- D5_ResultsRMST[, ..keep_cols]
  ##########################################################
  # 19. SAVE OUTPUT
  ##########################################################
  outputfile <- D5_ResultsRMST
  nameoutput <- paste0(
    "D5_ResultsRMST",
    suffix
  )
  assign(
    nameoutput,
    outputfile
  )
  saveRDS(
    outputfile,
    file = file.path(
      thisdiroutput,
      paste0(
        nameoutput,
        ".rds"
      )
    )
  )
}

############################################################
# 20. CLOSE PARALLEL WORKERS
############################################################
future::plan(
  future::sequential
)
