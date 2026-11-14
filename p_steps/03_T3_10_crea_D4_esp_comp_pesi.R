#######################################
#                                     #
#        Create_D4_esp_comp_pesi      #
#                                     #          
#######################################

# authors: Sofia Miniati, Paoletti Olga, Rosa Gini, Sabrina Giometto, Elena Ferrati

# v 1.1 22 Oct 2026

print('CREATE D4_esp_comp_pesi')

#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_D4_esp_comp_pesi"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

#########################################

thisdirarchive <- file.path(thisdir, "p_parameters", "archive_parameters")

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D4_esp_comp_baseline_pesi.xlsx")),1)))

covariate <- unname(unlist(parameters_this_step[parameter == "covar_bin",.(value)]))
# covariate_at_t0 <- paste0(covariate,"_at_t0")
covariate_at_t0 <- covariate

#########################################
# load input datasets

input <- readRDS(file.path(thisdirinput,paste0("D3_esp_comp_con_baseline.rds")))

#########################################
# start_processing

# creazione esposizione e t0
input[, `:=`(exposure = ifelse(drug=="A", 1, 0),
             t0 = date_first)]

to_keep <- c("exposure", "age", covariate_at_t0)

variables <- input[,..to_keep]

var <- setdiff(names(variables), "age")

variables[, (var) := lapply(.SD, as.factor), .SDcols = var]

# str(variables)

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
  # browser()
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

or_file <- file.path(thisdiroutput, paste0("D5_PS_model_OR.rds"))
saveRDS(or_table, file = or_file)

#stabilized weights

# marginal probability of treatment
p_treated <- mean(input$exposure == 1, na.rm = TRUE)

# stabilized weights
input[, weight := ifelse(
  exposure == 1,
  p_treated/ps,
  (1 - p_treated) / (1 - ps)
)]


# #########################################
# clean

listvartokeep <- c("person_id",
                   "t0",
                   "exposure",
                   "age",
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
