############################################################
#                                                          #
#            Create D5_RisultatiIR_HR                      #
#                                                          #
############################################################

# v1.0 
# authors: Rosa Gini, Sabrina Giometto, Elena Ferrati
# corrected version:
# - robust validation of exposure, follow-up and outcome
# - always retains exposure strata 0 and 1
# - safe Cox, unweighted log-rank, cloglog and Schoenfeld diagnostics
# - avoids ggsurvplot error: "'from' must be a finite number"
# - DG-CHANGES: smoothed curves and adapted schoenfeld plot

print("CREATE D5_RisultatiIR_HR")

#########################################

# assegnazione cartelle

if (TEST) {
  testname <- "test_D5_RisultatiIR_HR"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(thisdirinput, "g_output")
  dir.create(thisdiroutput,showWarnings = FALSE, recursive = TRUE)
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# parametri
analysisIR_HR <- "E1"
expected_exposure_strata <- c(0, 1)

make_diagnostic_plots <- TRUE

#########################################
# helper functions

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}
 
  
#########################################
# load input dataset
  
analytical_dataset <- readRDS(file.path(thisdirinput,
                              "D4_dataset_analitico_IR_HR_RMST.rds"))
  
setDT(analytical_dataset)
output <- data.table()
plot_analyses <- "E1"
  

required_variables <- c("exposure", "evento", "tempo_evento", "truncated_weight")
missing_variables <- setdiff(required_variables,names(analytical_dataset))
  

#########################################
# prepare input

input <- analytical_dataset[,.(exposure = to_numeric(exposure),
                               FUP = to_numeric(tempo_evento),
                               outcome = to_numeric(evento),
                               truncated_weight)]
    

input <- input[
  !is.na(exposure) &
    exposure %in% c(0, 1) &
    !is.na(FUP) &
    is.finite(FUP) &
    FUP > 0 &
    !is.na(outcome) &
    outcome %in% c(0, 1)
]
    
#########################################
# initialise both exposure strata
    
processing <- data.table(
  exposure_stratum = expected_exposure_strata)
    
#########################################
# N, events and person-years
    
#if (nrow(input) > 0L) {
counts <- input[,.(
  N_ = .N,
  N_outcome_ = sum(outcome == 1, na.rm = TRUE),
  PY_ = sum(FUP, na.rm = TRUE) / 365.25 #convertito in anni fup
  ),
  by = .(exposure_stratum = exposure)]
  
processing <- merge( processing, counts,
                     by = "exposure_stratum",
                     all.x = TRUE,
                     sort = FALSE)

processing[is.na(N_),
           `:=`(
             N_ = 0L,
             N_outcome_ = 0L,
             PY_ = 0)]
    
processing[,`:=`(
  N_ = as.integer(N_),
  N_outcome_ = as.integer(N_outcome_))]
  #PY_ = round(PY_, digits = 1))]
    
#########################################
# incidence rates
    
processing[,
           IR_ := fifelse(
             PY_ > 0,
             1000 * N_outcome_ / PY_,  #IR per 1000 persone-anno
             NA_real_)]
    
CI <- epitools::pois.exact(
  x = processing$N_outcome_, 
  pt = processing$PY_, conf.level = 0.95)

processing[, `:=`(
  IR_lb_ = 1000 * CI$lower,
  IR_ub_ = 1000 * CI$upper
)]

processing[, PY_ := round(PY_, 1)]

    
processing[, c("IR_", "IR_lb_", "IR_ub_") := lapply(
  .SD, round, digits = 3),
  .SDcols = c("IR_", "IR_lb_", "IR_ub_")]
    
#########################################
# check whether Cox/log-rank are estimable
    
events_by_group <- input[,.(
  N = .N,
  events = sum(outcome == 1, na.rm = TRUE),
  min_FUP = min(FUP),
  max_FUP = max(FUP)
  ), by = exposure
]
    
can_fit_cox <-
  nrow(input) > 0L &&
  all(c(0, 1) %in% events_by_group$exposure) &&
  all(
    events_by_group[
      match(c(0, 1), exposure),
      N
    ] > 0L
  ) &&
  all(
    events_by_group[
      match(c(0, 1), exposure),
      events
    ] > 0L
  )
    
#########################################
# crude Cox model
    
cox_model <- NULL
    
if (can_fit_cox) {
  cox_model <- 
    survival::coxph(
      survival::Surv(FUP, outcome) ~ exposure,
      data = input,
      robust = TRUE
    )
}

#########################################
# HR and confidence interval
    
hr_ci <- NULL
    
hr_ci <- 
  list(
    hr = as.numeric(exp(stats::coef(cox_model))),
    ci = exp(stats::confint(cox_model))
    )

processing[,`:=`(
  HR_crude_ = fifelse(
    exposure_stratum == 1,
    round(hr_ci$hr, 3),
    1
  ),
  HR_crude_lb_ = fifelse(
    exposure_stratum == 1,
    round(as.numeric(hr_ci$ci[1L, 1L]), 3),
    NA_real_
  ),
    
  HR_crude_ub_ = fifelse(
    exposure_stratum == 1,
    round(as.numeric(hr_ci$ci[1L, 2L]), 3),
    NA_real_
  )
  )
]
    
input_iptw <- input [
  !is.na(truncated_weight) &
    is.finite(truncated_weight) &
    truncated_weight > 0
] 

#HR_adj, HR_adj_lb_ and HR_adj_ub_
cox_iptw <- survival::coxph(
  survival::Surv(FUP, outcome) ~ exposure,
  data = input_iptw,
  weights = truncated_weight,
  robust = TRUE
  )

processing[, `:=`(
  HR_adj_ = ifelse(exposure_stratum == 1,
                   round(exp(coef(cox_iptw)), digits = 3),
                   1),
  HR_adj_ll_ = ifelse(exposure_stratum == 1,
                      round(exp(confint(cox_iptw))[,1], digits = 3),
                      NA),
  HR_adj_ul_ = ifelse(exposure_stratum == 1,
                      round(exp(confint(cox_iptw))[,2], digits = 3),
                      NA)
  )]
    
#########################################
# unadjusted log-rank test
#
# survdiff() is used instead of svydesign()/svylogrank() because this
# is the unweighted analysis. This avoids the warning:
# "No weights or probabilities supplied, assuming equal probability".
    
logrank_p <- NA_real_
    
if (can_fit_cox) {
  
    lr <- survival::survdiff(
      survival::Surv(FUP, outcome) ~ exposure,
      data = input
      )
        
logrank_p <- stats::pchisq(
          q = lr$chisq,
          df = length(lr$n) - 1L,
          lower.tail = FALSE
          )
}


processing[, logrank_unadj := round(logrank_p, 3)]
    
#########################################
# diagnostic: log(-log) survival plot

log_unadj <- NULL

plot_input <- input[
  !is.na(FUP) &
    is.finite(FUP) &
    FUP > 0 &
    !is.na(outcome) &
    outcome %in% c(0, 1) &
    exposure %in% c(0, 1)
  ]
    

  
if (make_diagnostic_plots) { 
  
  surv_fit_plot <- 
    survival::survfit(
      survival::Surv(FUP, outcome) ~ exposure,
      data = plot_input
    )
  }
 
  
log_unadj <- 
  survminer::ggsurvplot(
    surv_fit_plot,
    data = plot_input,
    fun = "cloglog",
    pval = if (is.finite(logrank_p)) logrank_p else FALSE,
    conf.int = TRUE,
    palette = c("skyblue", "gold"),
    title = paste0(
      analysisIR_HR,
      ": unadjusted log(-log) survival curves"
      ),
    legend.labs = c(
      "Comparators", 
      "Exposed"
      ),
    risk.table = FALSE
    )

    
#########################################
# cumulative incidence plots: unadjusted and IPTW-adjusted
#
# Kaplan-Meier estimates are transformed from survival probability
# S(t) to cumulative incidence 1 - S(t).
    
cumulative_incidence_unadj <- NULL
cumulative_incidence_adj <- NULL
    
    
cumulative_incidence_unadj_fit <- 
  survival::survfit(
    survival::Surv(FUP, outcome) ~ exposure,
    data = plot_input,
    conf.type = "log"
    )
    

  
cumulative_incidence_unadj <-
  survminer::ggsurvplot(
    cumulative_incidence_unadj_fit,
    data = plot_input,
    fun = "event",
    conf.int = TRUE,
    censor = TRUE,
    censor.shape = 3,
    censor.size = 2.5,
    palette = c("#F8766D", "#00BFC4"),
    legend.labs = c("Comparators", "Exposed"),
    xlab = "Day since start of risk period",
    ylab = "Cumulative incidence",
    title = paste0(
      analysisIR_HR,
      ": unadjusted cumulative incidence"
      ),
    risk.table = FALSE,
    ggtheme = ggplot2::theme_minimal()
    )

y_min_surv <- floor(
  min(cumulative_incidence_unadj_fit$lower, na.rm = TRUE) * 100
) / 100

survival_unadj <-
  survminer::ggsurvplot(
    cumulative_incidence_unadj_fit,
    data = plot_input,
    conf.int = TRUE,
    censor = TRUE,
    censor.shape = 3,
    censor.size = 2.5,
    palette = c("#F8766D", "#00BFC4"),
    legend.labs = c("Comparators", "Exposed"),
    xlab = "Day since start of risk period",
    ylab = "Survival probability",
    ylim = c(y_min_surv, 1),
    title = paste0(
      analysisIR_HR,
      ": unadjusted cumulative incidence"
    ),
    risk.table = FALSE,
    ggtheme = ggplot2::theme_minimal()
  )

# End of DG-CHANGES
    
    
cumulative_incidence_input <- input[
  !is.na(FUP) &
    is.finite(FUP) &
    FUP > 0 &
    !is.na(outcome) &
    outcome %in% c(0, 1) &
    exposure %in% c(0, 1) &
    !is.na(truncated_weight) &
    is.finite(truncated_weight) &
    truncated_weight > 0
]
    
    
if (make_diagnostic_plots) { 
  
  cumulative_incidence_fit <-
    survival::survfit(
      survival::Surv(FUP, outcome) ~ exposure,
      data = cumulative_incidence_input,
      weights = truncated_weight,
      robust = TRUE,
      conf.type = "log"
      )
}
  

  
cumulative_incidence_adj <- #tryCatch({

#cumulative_incidence_plot <-
survminer::ggsurvplot(
  cumulative_incidence_fit,
  data = cumulative_incidence_input,
  fun = "event",
  conf.int = TRUE,
  censor = TRUE,
  censor.shape = 3,
  censor.size = 2.5,
  palette = c("#F8766D", "#00BFC4"),
  legend.labs = c("Comparators", "Exposed"),
  xlab = "Day since start of risk period",
  ylab = "Cumulative incidence",
  title = paste0(
    analysisIR_HR,
    ": IPTW-adjusted cumulative incidence"
    ),
  risk.table = FALSE,
  ggtheme = ggplot2::theme_minimal()
  )

cumulative_incidence_adj$plot <-
  cumulative_incidence_adj$plot +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    legend.title = ggplot2::element_blank()
  )
    

#########################################
# diagnostic: Schoenfeld residuals
    
# schonefeld_unadj <- NULL
# 
# if (make_diagnostic_plots && !is.null(cox_model)) {
#   
#   schonefeld_unadj <- tryCatch({
#     
zph <- survival::cox.zph(cox_model)
    
n_event_times <- length( unique( input$FUP[
  input$outcome == 1]))
    
df_use <- max( 1L, min( 4L, n_event_times - 1L))

# DG-CHANGES: modifications made on 03-08-2026 to prevent the graph from having individual data points - Schoenfeld residuals plot
        
schonefeld_unadj <- survminer::ggcoxzph(
  zph,
  main = paste0(
    analysisIR_HR,
    ": unadjusted Schoenfeld residuals"),
  df = df_use
  )
        
       
#########################################
# add metadata and clean
    
# processing[ ,ds := paste0(settings$ds[1L], suffix)]
processing[ , analysisIR_HR := "E1"]
   
listvartokeep <- c(
  # "ds",
  "exposure_stratum",
  "analysisIR_HR",
  "N_",
  "N_outcome_",
  "PY_",
  "IR_",
  "IR_lb_",
  "IR_ub_",
  "HR_crude_",
  "HR_crude_lb_",
  "HR_crude_ub_",
  "HR_adj_",
  "HR_adj_ll_",
  "HR_adj_ul_",
  "logrank_unadj"
)
    
processing <- processing[ , ..listvartokeep]
    
output <- rbind(output, processing, fill = TRUE)
    
#########################################
# save diagnostic plots
    
#if (make_diagnostic_plots && !is.null(log_unadj)) {
  
  ggsave(
    filename = paste0(
      "D5_",
      analysisIR_HR,
      "_Figure_log_unadjusted.pdf"
      # suffix,
      # ".pdf"
      ),
    plot = log_unadj$plot,
    path = thisdiroutput, #thisdiresxp
    width = 10,
    height = 6
  )
#}
    
if (make_diagnostic_plots && !is.null(cumulative_incidence_adj)) {
  
  ggsave(
    filename = paste0(
      "D5_",
      analysisIR_HR,
      "_Figure_cumulative_incidence_adjusted.pdf"
      # suffix,
      # ".pdf"
      ),
    plot = cumulative_incidence_adj$plot,
    path = thisdiroutput, #thisdirexp
    width = 10,
    height = 6
  )
}
    
if (make_diagnostic_plots && !is.null(cumulative_incidence_unadj)) {
  
  ggsave(
    filename = paste0(
      "D5_",
      analysisIR_HR,
      "_Figure_cumulative_incidence_unadjusted.pdf"
      # suffix,
      # ".pdf"
      ),
    plot = cumulative_incidence_unadj$plot,
    path = thisdiroutput,
    width = 10,
    height = 6
  )
  
  ggsave(
    filename = paste0(
      "D5_",
      analysisIR_HR,
      "_Figure_survival_unadjusted.pdf"
      # suffix,
      # ".pdf"
    ),
    plot = survival_unadj$plot,
    path = thisdiroutput,
    width = 10,
    height = 6
  )
}


    
if (make_diagnostic_plots && !is.null(schonefeld_unadj)) {
  
  schonefeld_plot <- if (
    inherits(schonefeld_unadj, "ggplot")
    ) {
    schonefeld_unadj
    } else if (
      is.list(schonefeld_unadj) &&
      length(schonefeld_unadj) >= 1L
      ) {
      schonefeld_unadj[[1L]]
      } else {
        NULL
        }
  
  if (!is.null(schonefeld_plot)) {
    
    ggsave(
      filename = paste0(
        "D5_",
        analysisIR_HR,
        "_Figure_Schoenfeld_residuals_unadjusted.pdf"
        # suffix,
        # ".pdf"
        ),
      plot = schonefeld_plot,
      path = thisdiroutput, #thisdirexp
      width = 10,
      height = 6
    )
  }
}
  
  
#########################################
# final naming and validation
  
setnames(
  output,
  old = c(
    "IR_lb_",
    "IR_ub_",
    "HR_crude_",
    "HR_crude_lb_",
    "HR_crude_ub_"
    ),
  new = c(
    "IRll_",
    "IRul_",
    "HR_",
    "HRll_",
    "HRul_"
    )
  )
  
setorder(
  output,
  analysisIR_HR,
  exposure_stratum
  )
  
if (
  anyDuplicated(
    output[
      ,
      .(
        analysisIR_HR,
        exposure_stratum
        )
      ]
    )
  ) {
    
    stop(
      sprintf(
        paste0(
          "[D5_RisultatiIR_HR]: ",
          "duplicated analysis/exposure key"
        )
      )
    )
  }
  
#########################################
# save output
  
outputfile <- output
  
nameoutput <- paste0(
  "D5_RisultatiIR_HR"
  #suffix
  )
  
assign(
  nameoutput,
  outputfile
  )
  
  
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))

