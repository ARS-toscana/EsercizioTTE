##############################################
# simulate input for 03_T3_10_crea_D4_esp_comp_pesi

# author: Rosa Gini, Olga Paoletti, Claudia Bartolini, Sabrina Giometto, Elena Ferrati

# v 1.0 02 Nov 2026

##################################################
# HEADER

# rm(list = ls(all.names = TRUE))

# set the directory where the script is saved as the working directory

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)



#######################################
# PARAMETERS

# set # of persons
Npersons <- 5000

# input metadata
thisdirarchive <- file.path(thisdir, "..", "..", "p_parameters", "archive_parameters")

# list covariates
parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D4_esp_comp_baseline_pesi.xlsx")),1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(value)])

# covariates_binary <- paste0(thiscovariates_bin,"_at_t0")
covariates_binary <- thiscovariates_bin


# set probability of exposure
# probexposure = 0.55


#######################################
# DATA GENERATION

# all variables
list_variables <- c("person_id", "date_first", "age", "drug", covariates_binary,
                    "interruzione", "switch", "morte")

# create base 
data <- data.table::data.table(person_id = 1:Npersons)

# person_id 
data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

# date_first
set.seed(1351)
data[, date_first := sample(seq(study_start_date, study_end_date, by = "day"),
                            .N, replace = TRUE)]
# op start and end
set.seed(523634)
birth_year_list <- c(1915,1925,1935,1945,1955)
probyear = c(.01,.1,.2,.5)
totprob = sum(probyear)
probyear = c(probyear, 1 - totprob)
data[, birth_year := sample(birth_year_list, Npersons, replace = TRUE, prob = probyear)]

# set.seed(54632)
# data[, random := round(runif(.N, min = 1, max = date_first - 365 - dmy(paste0("0101",as.character(birth_year)))))]
# data[, op_start  := as.Date( date_first - random)]
# data[, date_first := pmax(date_first, dmy(paste0("0101",as.character(birth_year))))]
# set.seed(14632)
# data[, random := round(rpois(.N, lambda = 900))]
# data[, op_end := pmin(date_first + random, study_end_date)]
# data[,random := NULL]

# year_first
# data[, year_first:=year(date_first)]

# exposure
# set.seed(1252)
# # data[, drug := rbinom(.N, size = 1, prob = probexposure)]
# # data[, drug:=ifelse(drug==1, "A", "B")]
# 
# alpha0<--0.5
# alpha1<-1.2
# p_drug<-1/ (1+exp(-(alpha0+alpha1*data$metastatic)))
# data[, drug := rbinom(.N, size=1, prob=p_drug)]

# # ASL
# data[, ASL:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
#                    prob = c(rep(0.33, 3)))]

# age at date_first
data[, age := year(date_first) - birth_year]
data[,birth_year := NULL]

# covariates at date_first

j <- 1
for (cb in covariates_binary) {
  seedchange <- 1111 + j
  set.seed(seedchange)
  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  seedchange <- 112 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  # data[drug == "A" & random > .95, cov := 1]
  setnames(data,"cov",cb)
  j <- j + 1
}
data[,random := NULL]

# exposure
set.seed(1252)
# data[, drug := rbinom(.N, size = 1, prob = probexposure)]
# data[, drug:=ifelse(drug==1, "A", "B")]

#drug dipendente da metastatic (modello logistico)
alpha0<--0.5
alpha1<-1.2
p_drug<-1/ (1+exp(-(alpha0+alpha1*data$metastatic)))
data[, drug := rbinom(.N, size=1, prob=p_drug)]

# interruzione
set.seed(444)
data[, interruzione:= 1]

# switch
set.seed(148)
data[, switch:= rbinom(.N, size = 1, prob = 0.3)]

# morte
set.seed(898)
data[, morte:= rbinom(.N, size = 1, prob = 0.35)]

###

to_keep <- c("drug", "age", covariates_binary, "interruzione", "switch", "morte")

variables <- data[,..to_keep]


var <- setdiff(names(variables), "age")

variables[, (var) := lapply(.SD, as.factor), .SDcols = var]

const_cols <- sapply(variables, function(x) {
  if (is.factor(x)) nlevels(droplevels(x)) <= 1
  else length(unique(x)) <= 1
})

variables_clean <- variables[, !const_cols, with = FALSE]

###
library(data.table)
library(survival)

table(data$metastatic, data$drug)

#parametri per tempo_causa_1 (morte)
b0_out<-1.5
b1_out<-0
b2_out<--1.2 #effetto di metastatic su outcome
b3_age<--0.04 #effetto di age su outcome
sig_out<-0.7

b_scompcard<--0.45
b_infart<--0.35
b_ictus<--0.40

#tempo_causa_2 (fine_osservazione_persona)
#data[, tempo_causa_2 := as.numeric(op_end-date_first)]
b0_c2<-1.8; b1_c2<-0; sig_c2<-0.8

#tempo_causa_3 (fine_finestra_studio)
#data[, tempo_causa_3 := as.numeric(data_fine_finestra_studio-date_first)]
b0_c3<-2.8; b1_c3<-0; sig_c3<-0.4

#tempo_causa_4 (fine_studio)
# data[,tempo_causa_4 := as.numeric(data_fine_studio-date_first)]
b0_c4<-1.3; b1_c4<-0; sig_c4<-0.65

#parametri per tempo_interruzione
b0_int<-1.8
b1_int<--0
b2_int<-0
sig_int<-0.9

#parametri per tempo_switch
b0_sw<-1.9
b1_sw<-0.8
b2_sw<-0
sig_sw<-0.85

#modello AFT Log_Normale
# simulazione <- function(b0, b1, trt, sigma) {
#   epsilon <- rnorm(length(trt), mean = 0, sd = 1)
#   exp(b0 + (b1 * trt) + (sigma * epsilon))
# }
# 
# df_simulato <- data.frame(
#   id = 1:N,
#   trattamento = drug ,
#   tempo_censura      = simulazione(b0_cens, b1_cens, drug, sig_cens),
#   tempo_outcome      = simulazione(b0_out,  b1_out,  drug, sig_out)
# )

#modello Weibull
simulazione <- function(b0, b1, trt, b2, meta, b3, age, sigma) {
  u <- runif(length(trt), min = 0, max = 1)
  exp(b0 + (b1 * trt) + (b2*meta) + (b3*age)) * (-log(u))^(sigma)
}

# df_simulato <- data.frame(
#   id = 1:N,
#   metastatic = metastatic,
#   drug = drug,
#   tempo_censura = simulazione(b0_cens, b1_cens, drug, 0, rep(0,N), sig_cens),
#   tempo_outcome_0 = simulazione(b0_out_0, b1_out_0, drug, sig_out_0),
#   tempo_outcome = simulazione(b0_out, b1_out, drug, b2_out, metastatic, sig_out)
# )

data[, tempo_causa_1 := simulazione(b0_out, b1_out, drug, 1, (b2_out * metastatic) + 
                                      (b3_age * (age - mean(age, na.rm = TRUE))) + 
                                      (b_scompcard * scompcard) + 
                                      (b_infart * infart) + 
                                      (b_ictus * ictus),0, 0, sig_out)]
data[, tempo_causa_2 := simulazione(b0_c2, b1_c2, drug, 0, 0, 0, 0, sig_c2)]
data[, tempo_causa_3 := simulazione(b0_c3, b1_c3, drug, 0, 0, 0, 0, sig_c3)]
data[, tempo_causa_4 := simulazione(b0_c4, b1_c4, drug, 0, 0, 0, 0, sig_c4)]
# data[, tempo_causa_2 := as.numeric(op_end-date_first)]
# data[, tempo_causa_3 := as.numeric(data_fine_finestra_studio-date_first)]
# data[,tempo_causa_4 := as.numeric(data_fine_studio-date_first)]

data[, tempo_interruzione := simulazione(b0_int, b1_int, drug, 0, 0, 0, 0, sig_int)]
data[, tempo_switch := simulazione(b0_sw, b1_sw, drug, b2_sw, 0, 0, 0, sig_sw)]

data[, tempo_causa_1:= ifelse(morte ==1, tempo_causa_1, NA)]

#date di outcome, interruzione e switch
# data[, data_morte := ifelse(morte==1, date_first+round(tempo_outcome), NA)]
# data[, data_morte := as.Date(data_morte, origin = "1970-01-01")]

# data[, data_interruzione := ifelse(interruzione==1, date_first + round(tempo_interruzione), NA)]
# data[, data_interruzione := as.Date(data_interruzione, origin = "1970-01-01")]

# data[, data_switch := ifelse(switch==1, date_first+round(tempo_switch), NA)]
# data[, data_switch := as.Date(data_switch, origin = "1970-01-01")]

#creo tempo censura
# data[, data_censura := pmin(data_morte, op_end, data_fine_finestra_studio, data_fine_studio)]
data[, tempo_censura := pmin(tempo_causa_1, tempo_causa_2, tempo_causa_3, tempo_causa_4, na.rm = TRUE)]
data[, causa_censura := fcase(tempo_censura == tempo_causa_1, 1, 
                              tempo_censura == tempo_causa_2, 2,
                              tempo_censura == tempo_causa_3, 3,
                              tempo_censura == tempo_causa_4, 4, default = NA_integer_
     )]

data[, outcome := ifelse(causa_censura == 1, 1, 0)]

# data[, tempo_causa_1:= ifelse(morte ==1 & tempo_causa_1 <= tempo_censura, tempo_causa_1, NA)]
data[, tempo_interruzione := ifelse(interruzione ==1 & tempo_interruzione <= tempo_censura, tempo_interruzione, NA)]
data[, interruzione_pre_censura := ifelse(interruzione == 1 & !is.na(tempo_interruzione) & tempo_interruzione <= tempo_censura, 1, 0)]

data[, tempo_switch := ifelse(switch==1 & tempo_switch <= tempo_censura, tempo_switch, NA)]
data[, switch_pre_censura := ifelse(switch == 1 & !is.na(tempo_switch) & tempo_switch <= tempo_censura, 1, 0)]

#verifica parametri su ogni variabile
#tratto il tempo simulato come evento di interesse

# fit_cens<-coxph(Surv(tempo_censura) ~ drug, data = df_simulato)
# fit_cens
# fit_out_0<-coxph(Surv(tempo_outcome_0) ~ drug, data = df_simulato)
# fit_out_0
# fit_out<-coxph(Surv(tempo_outcome) ~ drug, data = df_simulato)
# fit_out

table(data$causa_censura)
fit_grezzo<-coxph(Surv(tempo_censura, outcome) ~ drug, data = data)
fit_grezzo
fit_aggiustato <- coxph(Surv(tempo_censura, outcome) ~ drug + metastatic + age, data = data)
fit_aggiustato
hr_grezzo <- exp(coef(fit_grezzo))
hr_grezzo
hr_aggiustato <- exp(coef(fit_aggiustato))
hr_aggiustato

prop.table(table(data$metastatic, data$drug), margin=2)*100


##############

ps_model <- glm(
  drug ~ ., 
  data = variables_clean, 
  family = binomial(link = "logit")
)

data[, ps := ps_model$fitted.values]

p_treated <- mean(data$drug == 1, na.rm = TRUE)

data[, weight := ifelse(
  drug == 1,
  p_treated / ps,
  (1 - p_treated) / (1 - ps)
)]

fit_cox_ipw <- coxph(
  Surv(tempo_censura, outcome) ~ drug, 
  data = data, 
  weights = weight,
  robust = TRUE
)

fit_cox_ipw

hr_ipw <- exp(coef(fit_cox_ipw))
hr_ipw

weighted_table <- data[, .(
  pct_metastatic_0 = sum(weight[metastatic == 0]) / sum(weight),
  pct_metastatic_1 = sum(weight[metastatic == 1]) / sum(weight)
), by = drug]

weighted_table

summary(data$weight)

smd <- function(x, g, w = rep(1, length(x))) {
  m1 <- weighted.mean(x[g == 1], w[g == 1])
  m0 <- weighted.mean(x[g == 0], w[g == 0])
  (m1 - m0) / sqrt((m1 * (1 - m1) + m0 * (1 - m0)) / 2)
}
smd(data$metastatic, data$drug) # prima
smd(data$metastatic, data$drug, data$weight) #dopo

###pesi troncati
cols_to_keep <- colnames(data)
required_cols <- c("person_id", "date_first", "drug", "ps")

missing_cols <- setdiff(required_cols, names(data))

if (length(missing_cols) > 0) {
  stop(sprintf(
    "The following required columns are missing from input: %s",
    paste(missing_cols, collapse = ", ")
  ))
}

input_troncatura <- fread(file=file.path(thisdirarchive, "input_troncatura.csv"))


trunc_par_exposed <- input_troncatura[drug == "A"]

if (nrow(trunc_par_exposed) == 0) {
  stop("No row found for drug == A in input_troncatura")
}

if (nrow(trunc_par_exposed) > 1) {
  stop("More than one row found for drug == A in input_troncatura")
}

ps_lower_exposed <- NA_real_

if (!is.na(trunc_par_exposed$lower_limit)) {
  
  ps_lower_exposed <- quantile(
    data[drug == "A", ps],
    probs = trunc_par_exposed$lower_limit / 100,
    na.rm = TRUE
  )
}

ps_upper_exposed <- NA_real_

if (!is.na(trunc_par_exposed$upper_limit)) {
  
  ps_upper_exposed <- quantile(
    data[drug == "A", ps],
    probs = trunc_par_exposed$upper_limit / 100,
    na.rm = TRUE
  )
}

#########################################
# apply equipoise trimming

data[, in_overlap_tmp := 1L]

if (!is.na(ps_lower_exposed)) {
  data[ps < ps_lower_exposed, in_overlap_tmp := 0L]
}

if (!is.na(ps_upper_exposed)) {
  data[ps > ps_upper_exposed, in_overlap_tmp := 0L]
}

#########################################
# create truncated_ps
# outside overlap: 0
# inside overlap: original ps

data[, truncated_ps := fifelse(
  in_overlap_tmp == 1L,
  ps,
  0
)]

#########################################
# Calculate IPTW from the remaining subjects' PS
# Existing weight is overwritten.

data[, weight := 0]

p_treated_trimmed <- mean(
  data[in_overlap_tmp == 1L, drug == "A"],
  na.rm = TRUE
)

data[
  in_overlap_tmp == 1L,
  weight := fifelse(
    drug == "A",
    p_treated_trimmed / ps,
    (1 - p_treated_trimmed) / (1 - ps)
  )
]

#########################################
# Truncate resulting IPTW weights at 1st/99th percentile
# among included subjects only

data[, truncated_weight := 0]

q_weight <- quantile(
  data[in_overlap_tmp == 1L, weight],
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

data[
  in_overlap_tmp == 1L,
  truncated_weight := pmin(
    pmax(weight, q_weight[1]),
    q_weight[2]
  )
]

#########################################
# cleaning
# keep original columns plus:
# truncated_ps
# weight
# truncated_weight
#
# weight is the newly calculated IPTW among remaining subjects.

keep_cols <- unique(c(
  cols_to_keep,
  "truncated_ps",
  "weight",
  "truncated_weight"
))

input <- data[, ..keep_cols]


## salvo D4_follow_up
listvar <- c("person_id", "date_first", "drug", "interruzione",
             "switch", "morte", "causa_censura", "outcome", "tempo_censura", 
             "tempo_causa_1", "tempo_causa_2", "tempo_causa_3", "tempo_causa_4",
             "interruzione_pre_censura", "switch_pre_censura",
             "tempo_interruzione","tempo_switch"
)

data<-data[,..listvar]

namedataset <- "D4_follow_up"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))

## salvo D4_esp_comp_pesi_troncati_trimmed
cols_to_keep <- c("person_id", "date_first", "drug", "age", covariates_binary, 
                  "ps", "weight", "truncated_ps", "truncated_weight")

input <- input[,..cols_to_keep]

nameoutput <- "D4_esp_comp_pesi_troncati_trimmed"
saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))

