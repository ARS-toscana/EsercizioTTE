############################################################
#                                                          #
#                 CREATE D5_RisultatiRMST                  #
#                                                          #
############################################################
# authors: Rosa Gini, Sabrina Giometto, Elena Ferrati


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


print("CREATE D5_RisultatiRMST")

############################################################

if (TEST) {
  testname <- "test_D5_RisultatiRMST"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, recursive = TRUE, showWarnings = FALSE)
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}



############################################################
# 5. BOOTSTRAP PARAMETERS
############################################################

B <- 1000L
bootstrap_seed <- 1234L

n_workers <- max(1L, min(2L, future::availableCores() - 1L))
if (n_workers > 1L) {
    
    
    
  future::plan(future::sequential)
  gc()

    
    
  future::plan(
    future::multisession,
    workers = n_workers
  )
    
  } else {
    
    future::plan(
      future::sequential
    )
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
# estimate_rmst_fast <- function(
#     dt,
#     tau,
#     weight_col = NULL
# ) {
#   dt <- as.data.table(dt)
#   if (is.null(weight_col)) {
#     fit <- survival::survreg(
#         survival::Surv(tempo_evento, evento) ~ exposure,
#         data = dt,
#         dist = "weibull")
#   } else {
#     w  <- dt[[weight_col]]
#     dt <- dt[!is.na(w) & is.finite(w) & w > 0]
#         fit <- survival::survreg(
#           survival::Surv(tempo_evento, evento) ~ exposure,
#           data = dt,
#           dist = "weibull",
#           weights = dt[[weight_col]]
#         )
#   }
#   
#   sigma <- as.numeric(fit$scale)
#   weibull_shape <- 1 / sigma
#   
#   predict_one_rmst <- function(exposure_value) {
#     linear_predictor <- as.numeric(
#       stats::predict(
#           fit,
#           newdata = data.frame(
#             exposure = exposure_value),
#           type = "lp"
#           )
#       )[1L]
#     weibull_scale <- exp(linear_predictor)
#     as.numeric(
#       stats::integrate(
#         f = function(time) {
#           exp(-(time / weibull_scale) ^ weibull_shape)
#           },
#           lower = 0,
#           upper = tau,
#           subdivisions = 100L,
#           rel.tol = 1e-7
#         )$value
#     )
#   }
#   rmst_0 <- predict_one_rmst(0L)
#   rmst_1 <- predict_one_rmst(1L)
#   c(
#     rmst_0 = rmst_0,
#     rmst_1 = rmst_1,
#     difference = rmst_1 - rmst_0
#   )
# }
# 


############################################################
# 9. KAPLAN-MEIER RMST FUNCTION 
############################################################

estimate_rmst_fast <- function(
    dt,
    tau,
    weight_col = NULL) 
  {
  dt <- as.data.table(dt)
  if (!is.null(weight_col)) {
    dt <- dt[is.finite(get(weight_col)) & get(weight_col) > 0]
    w <- dt[[weight_col]]
    } else {
      w <- NULL
      }
  
  fit <- survival::survfit(
        survival::Surv(tempo_evento, evento) ~ exposure,
        data = dt,
        weights = w
      )
  
  rm <- summary(fit, rmean = tau)$table[, "rmean"]
  c(rmst_0 = unname(rm[1]), rmst_1 = unname(rm[2]),
  difference = unname(rm[2] - rm[1]))
}
############################################################
# # 9B. DIAGNOSTIC WEIBULL MODEL
# ############################################################
# diagnose_survreg <- function(
#   dt,
#   tau,
#   analysis_label,
#   model_label,
#   weight_col = NULL
# ) {
#   diagnostic_data <- as.data.table(copy(dt))
#   # diagnostic_data <- diagnostic_data[
#   #   is.finite(FUP_years) &
#   #     FUP_years > 0 &
#   #     outcome %in% c(0L, 1L) &
#   #     exposure %in% c(0L, 1L)
#   # ]
#   # if (!is.null(weight_col)) {
#   #   diagnostic_data <- diagnostic_data[
#   #     is.finite(get(weight_col)) & get(weight_col) > 0
#   #   ]
#   # }
#   cat(
#     "\n--- DIAGNOSTIC MODEL ", model_label,
#     ": ", analysis_label, " ---\n",
#     sep = ""
#   )
#   model_data_summary <- diagnostic_data[
#     ,
#     .(
#       rows = .N,
#       patients = uniqueN(person_id),
#       events = sum(evento == 1L),
#       events_before_tau = sum(evento == 1L & tempo_evento <= tau),
#       min_fup = min(tempo_evento),
#       median_fup = median(tempo_evento),
#       max_fup = max(tempo_evento),
#       min_weight = if (!is.null(weight_col)) min(get(weight_col)) else NA_real_,
#       max_weight = if (!is.null(weight_col)) max(get(weight_col)) else NA_real_
#     ),
#     by = exposure
#   ]
#   cat("Model", model_label," \n")
#   print(model_data_summary)
#   captured_warnings <- character()
#   fit <- #tryCatch(
#     withCallingHandlers(
#       {
#         if (is.null(weight_col)) {
#           survival::survreg(
#             survival::Surv(tempo_evento, evento) ~ exposure,
#             data = diagnostic_data,
#             dist = "weibull",
#             x = TRUE,
#             y = TRUE,
#             model = TRUE
#           )
#         } else {
#           survival::survreg(
#             survival::Surv(tempo_evento, evento) ~ exposure,
#             data = diagnostic_data,
#             dist = "weibull",
#             weights = diagnostic_data[[weight_col]],
#             x = TRUE,
#             y = TRUE,
#             model = TRUE
#           )
#         }
#       },
#       warning = function(w) {
#         captured_warnings <<- c(captured_warnings, conditionMessage(w))
#         invokeRestart("muffleWarning")
#       }
#     )
#   #   error = function(e) {
#   #     message("DIAGNOSTIC survreg error [", model_label, "]: ", conditionMessage(e))
#   #     NULL
#   #   }
#   # )
#   if (length(captured_warnings) > 0L) {
#     message(
#       "DIAGNOSTIC survreg warnings [", model_label, "]: ",
#       paste(unique(captured_warnings), collapse = " | ")
#     )
#   }
#   # if (is.null(fit)) {
#   #   return(list(fit = NULL, summary = model_data_summary))
#   # }
#   cat("Coefficients:\n")
#   print(stats::coef(fit), digits = 12)
#   cat("survreg scale:", fit$scale, "\n")
#   predictions <- #tryCatch(
#     stats::predict(
#       fit,
#       newdata = data.frame(exposure = c(0L, 1L)),
#       type = "lp"
#     )
#   #   error = function(e) {
#   #     message("DIAGNOSTIC predict error [", model_label, "]: ", conditionMessage(e))
#   #     c(NA_real_, NA_real_)
#   #   }
#   # )
#   cat("Linear predictions for exposure 0 and 1:\n")
#   print(predictions, digits = 12)
#   # diagnostic_rmst <- c(rmst_0 = NA_real_, rmst_1 = NA_real_, difference = NA_real_)
#   # if (
#   #   length(predictions) == 2L &&
#   #   all(is.finite(predictions)) &&
#   #   is.finite(fit$scale) &&
#   #   fit$scale > 0
#   # ) {
#     shape <- 1 / fit$scale
#     scales <- exp(predictions)
#     rmst_values <- vapply(
#       scales,
#       FUN.VALUE = numeric(1L),
#       FUN = function(scale_value) {
#         #tryCatch(
#         stats::integrate(
#           function(time) exp(-(time / scale_value)^shape),
#           lower = 0,
#           upper = tau
#           )$value
#           # error = function(e) {
#           #   message("DIAGNOSTIC integrate error [", model_label, "]: ", conditionMessage(e))
#           #   NA_real_
#           # }
#         #)
#       }
#     )
#     diagnostic_rmst <- c(
#       rmst_0 = rmst_values[1L],
#       rmst_1 = rmst_values[2L],
#       difference = rmst_values[2L] - rmst_values[1L]
#     )
#   
#   cat("Diagnostic RMST (not rounded):\n")
#   print(diagnostic_rmst, digits = 12)
#   list(
#     fit = fit,
#     summary = model_data_summary,
#     predictions = predictions,
#     rmst = diagnostic_rmst,
#     warnings = captured_warnings
#   )
# }

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


##########################################################
# Check whether adjusted analysis is available
##########################################################
calculate_iptw <- (
  !is.null(weight_col) &&
    weight_col %in% names(dt))
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
      weight_col = NULL
      # calculate_hessian = FALSE,
      # print_errors = FALSE
    )
    ######################################################
    # IPTW-adjusted model
    ######################################################
    if (calculate_iptw) {
      iptw_estimate <- estimate_rmst_fast(
        dt = bootstrap_data,
        tau = tau,
        weight_col = weight_col
        # calculate_hessian = FALSE,
        # print_errors = FALSE
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

##########################################################
# Load analytical dataset
##########################################################
analytical_dataset <- readRDS(
  file.path(
    thisdirinput,
    paste0(
      "D4_dataset_analitico_IR_HR_RMST.rds"
      # suffix,
      # ".rds"
    )
  )
)

# analytical_dataset <- analytical_dataset[!is.na(truncated_weight) & truncated_weight > 0]

analytical_dataset <- as.data.table(analytical_dataset)


########################################################
# Descriptive results by exposure group
########################################################
# processing <- analytical_dataset[
#   ,
#   .(
#     N_ = uniqueN(
#       person_id
#     ),
#     N_outcome_ = sum(
#       evento == 1L,
#       na.rm = TRUE
#     ),
#     PY_ = sum(
#       tempo_evento,
#       na.rm = TRUE  
#     ) / 365.25   #in anni
#   ),
#   by = .(
#     exposure_stratum = exposure
#   )
# ]
########################################################
# Ensure exposure 0 and exposure 1 rows
########################################################
processing <- data.table(exposure_stratum = c(0L,1L))
# processing <- merge(
#   exposure_template,
#   processing,
#   by = "exposure_stratum",
#   all.x = TRUE,
#   sort = TRUE
# )
########################################################
# Incidence rate per 1,000 person-years
########################################################
# processing[
#   ,
#   IR_ := fifelse(
#     !is.na(PY_) &
#       PY_ > 0,
#     1000 *
#       N_outcome_ /
#       PY_,
#     NA_real_
#   )
# ]
########################################################
# Incidence-rate 95% CI
########################################################

#   incidence_ci <- 
#     
#       exactPoiCI(
#         processing,
#         "N_outcome_",
#         "PY_",
#         conversion_factor = 1, 
#         per = 1000 #OoM = 3
#       )
# 
# processing[
#   ,
#   `:=`(
#     IR_lb_ = incidence_ci[[2]],
#     IR_ub_ = incidence_ci[[3]]
#   )
# ]
########################################################
# 13. RMST #FOR E2 ONLY
########################################################

tau <- 365
cat(
  "Calculating RMST at",
  tau,
  "days(s)\n"
  )
  
  ######################################################
  # Crude point estimate
  ######################################################
  crude_point <- estimate_rmst_fast(
    dt = analytical_dataset,
    tau = tau,
    weight_col = NULL
    # calculate_hessian = TRUE,
    # print_errors = debug_rmst
  )
  
  ######################################################
  # IPTW-adjusted point estimate
  ######################################################

    iptw_point <- estimate_rmst_fast(
      dt = analytical_dataset,
      tau = tau,
      weight_col = "truncated_weight" #"weight",
      # calculate_hessian = TRUE,
      # print_errors = debug_rmst
    )
  
  ######################################################
  # Sequential bootstrap
  ######################################################
  bootstrap_result <- bootstrap_rmst_sequential(
    dt = analytical_dataset,
    tau = tau,
    B = B,
    weight_col = "truncated_weight",
    # weight_col = if (
    #   has_weight
    # ) {
    #   "weight"
    # } else {
    #   NULL
    # },
    seed = bootstrap_seed #+
      # match(
      #   analysis_name,
      #   names(
      #     analysis_definitions
      #   )
      # )
  )
  bootstrap_ci <- bootstrap_result$ci
  
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

########################################################
# Analysis metadata
########################################################
processing[
  ,
  `:=`(
    analysisRMST =
      "E1", #analysis_label,
    outcome_variable =
      "evento",
    followup_variable =
      "tempo_evento",
    RMST_days =
      tau)]
  
  
D5_RisultatiRMST <- copy(processing)    
##########################################################
# 15. ROUND OUTPUT VARIABLES
##########################################################
columns_to_round <- c(
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
# columns_to_round <- intersect(
#   columns_to_round,
#   names(
#     D5_ResultsRMST
#   )
# )
D5_RisultatiRMST[
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
  "Diff_RMST_crude_",
  "Diff_RMST_crude_lb_",
  "Diff_RMST_crude_ub_",
  "Diff_RMST_adj_",
  "Diff_RMST_adj_lb_",
  "Diff_RMST_adj_ub_"
)
new_names <- c(
  "RMSTdiff_",
  "RMSTdiffll_",
  "RMSTdifful_",
  "RMSTdiff_adj_",
  "RMSTdiff_adj_ll_",
  "RMSTdiff_adj_ul_"
)
variables_present <- old_names %in%
  names(
    D5_RisultatiRMST
  )
setnames(
  D5_RisultatiRMST,
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
  #"ds",
  "analysisRMST",
  "outcome_variable",
  "followup_variable",
  "exposure_stratum",
  "RMST_year",
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
  "bootstrap_samples"
  # "successful_bootstraps_crude",
  # "successful_bootstraps_iptw"
)
variables_to_keep <- intersect(
  variables_to_keep,
  names(
    D5_RisultatiRMST
  )
)
D5_RisultatiRMST <- D5_RisultatiRMST[
  ,
  ..variables_to_keep
]

#########################################
# cleaning
keep_cols <- c(
  #"ds",
  "analysisRMST",
  "exposure_stratum",
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
D5_RisultatiRMST <- D5_RisultatiRMST[, ..keep_cols]
##########################################################
# 19. SAVE OUTPUT
##########################################################
  
  
saveRDS(D5_RisultatiRMST, file = file.path(
  thisdiroutput, "D5_RisultatiRMST.rds"))
############################################################
# 20. CLOSE PARALLEL WORKERS
############################################################
future::plan(
  future::sequential
)
