############################################################
#                                                          #
####         Create_D4_esp_comp_pesi         #
#                                                          #
############################################################


# authors: Sofia Miniati, Paoletti Olga, Rosa Gini

# v 1.1 4 Sep 2026


print('CREATE D4_esp_comp_pesi')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_01_D4_esp_comp_pesi"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdiroutputplots <- thisdiroutput
  thisdirsettings <- thisdirinput
  thisdirarchive <-dirarchive
  
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdiroutputplots <- direxp
  thisdirsettings <- dirpargen
  thisdirarchive <-dirarchive
}

#########################################

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D4_coorte_baseline_pesi.xlsx")),1)))

covariate <- unname(unlist(parameters_this_step[parameter == "covar_bin",.(value)]))
covariate_at_t0 <- paste0(covariate,"_at_t0")


#########################################
# load input datasets

input <- readRDS(file.path(thisdirinput,paste0("D3_coorte_baseline.rds")))

#########################################
# start_processing

to_keep<-c("exposure", "age_at_t0", "gender_at_t0", covariate_at_t0)
# handle missing


# if (ds == "DANREG") {
#      to_keep <- setdiff(to_keep, "Smoking_imputed_at_t0")
# }else{
# input[, Smoking_imputed_at_t0 := as.factor(Smoking_imputed_at_t0)]
# 
#    }
variables<-input[,..to_keep]

variables[, gender_at_t0 := as.factor(gender_at_t0)]
#variables[, CKD_at_t0 := as.factor(CKD_at_t0)]


# remove from the model variables that are constant

const_cols <- sapply(variables, function(x) {
  if (is.factor(x)) nlevels(droplevels(x)) <= 1
  else length(unique(x)) <= 1
})

variables_clean <- variables[, !const_cols, with = FALSE]

#logistic model
model <- glm(exposure ~ ., 
             data = variables_clean,  
             family = binomial(link="logit"))

#propensity score
input$ps <- model$fitted.values

summary_file <- file.path(thisdiroutput, paste0("D5_PS_model_summary.txt"))

capture.output(
  summary(model),
  file = summary_file
)

################################
# save model parameters in a dataset

coefs <- coef(model)
ci <- confint(model)
coef_summary <- summary(model)$coefficients

se <- rep(NA_real_, length(coefs))
names(se) <- names(coefs)
se[rownames(coef_summary)] <- coef_summary[, "Std. Error"]

pval <- rep(NA_real_, length(coefs))
names(pval) <- names(coefs)
pval[rownames(coef_summary)] <- coef_summary[, "Pr(>|z|)"]

coef_names <- names(coefs)

# recover variable names from model (excluding the intercept)
model_vars <- attr(model$terms, "term.labels")

# assign each coefficient to its origin variable
match_var <- function(cn) {
  if (cn == "(Intercept)") return("(Intercept)")
  matches <- model_vars[sapply(model_vars, function(v) startsWith(cn, v))]
  if (length(matches) == 0) return(NA_character_)
  matches[which.max(nchar(matches))]
}

variable <- sapply(coef_names, match_var)
level <- substring(coef_names, nchar(variable) + 1)
level[level == ""] <- NA_character_  # continuous variables: no level

or_table <- data.table(
  variable = variable,
  level = level,
  OR = exp(coefs),
  CI_lower = exp(ci[, 1]),
  CI_upper = exp(ci[, 2]),
  p_value = pval
)

or_file <- file.path(thisdiroutputplots, paste0("D5_PS_model_OR.rds"))
saveRDS(or_table, file = or_file)

  #stabilized weights

# marginal probability of treatment
p_treated <- mean(input$exposure == 1, na.rm = TRUE)

# stabilized weights
input[, weight := ifelse(
  exposure == 1,
  p_treated / ps,
  (1 - p_treated) / (1 - ps)
)]


# #########################################
# clean

listvartokeep <- c("person_id",
                   "t0",
                   "exposure",
                   "age_at_t0",
                   "gender_at_t0",
                   covariate_at_t0,
                   "ps",
                   "weight"
)
input <- input[,..listvartokeep]



#########################################
# save

outputfile <- input

nameoutput <- paste0("D4_esp_comp_pesi")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
