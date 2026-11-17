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
if (!require("survival")) install.packages("survival")
library(survival)


#######################################
# PARAMETERS

# set # of persons
Npersons <- 5000

# input metadata
thisdirarchive <- thisdir

# list covariates
parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D4_esp_comp_baseline_pesi.xlsx")),1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(value)])

# covariates_binary <- paste0(thiscovariates_bin,"_at_t0")
covariates_binary <- thiscovariates_bin


# set probability of esposizione
# probesposizione = 0.55


#######################################
# DATA GENERATION

# all variables
list_variables <- c("person_id", "t0", "age", "esposizione", covariates_binary,
                    "interruzione", "switch", "morte")

# create base 
data <- data.table::data.table(person_id = 1:Npersons)

# person_id 
data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

# t0
set.seed(1351)
data[, t0 := sample(seq(study_start_date, study_end_date, by = "day"),
                            .N, replace = TRUE)]
# anno di nascita
set.seed(523634)
birth_year_list <- c(1915,1925,1935,1945,1955)
probyear = c(.01,.1,.2,.5)
totprob = sum(probyear)
probyear = c(probyear, 1 - totprob)
data[, birth_year := sample(birth_year_list, Npersons, replace = TRUE, prob = probyear)]

# età a t0
data[, age := year(t0) - birth_year]
data[,birth_year := NULL]

# metastatic

cb <- "metastatic"
seedchange <- 11453
set.seed(seedchange)
data[, random := runif(.N)]
data[, cov := fifelse(random > .8, 1,0)]
setnames(data,"cov",cb)
data[,random := NULL]

# esposizione dipendente da metastatic (modello logistico)
set.seed(1252)
alpha0 <- -0.5
alpha1 <- 3
p_drug <- 1/ (1+exp(-(alpha0+alpha1*data$metastatic)))
data[, esposizione := rbinom(.N, size=1, prob=p_drug)]


# covariate al tempo t0

j <- 1
for (cb in setdiff(covariates_binary, "metastatic")) {
  seedchange <- 1111 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[ esposizione == 1, cov := fifelse(random > .9 , 1,0)]
  data[ esposizione == 0, cov := fifelse(random > .92 , 1,0)]
  setnames(data,"cov",cb)
  j <- j + 1
}
data[,random := NULL]


# interruzione
data[, interruzione:= 1]

# switch
set.seed(148)
alpha0 <- -0.5
alpha1 <- -1.4
p_switch <- 1/ (1+exp(-(alpha0+alpha1*data$esposizione)))
data[, switch := rbinom(.N, size=1, prob=p_switch)]

# morte
# set.seed(898)
# alpha0 <- -0.5
# alpha1 <- 1.8
# p_morte <- 1/ (1+exp(-(alpha0+alpha1*data$metastatic)))
# data[, morte := rbinom(.N, size=1, prob=p_morte)]


# filtro
to_keep <- c("esposizione", "age", covariates_binary, "interruzione", "switch")
variables <- data[,..to_keep]

# rimozione variabili costanti
var <- setdiff(names(variables), "age")

variables[, (var) := lapply(.SD, as.factor), .SDcols = var]

const_cols <- sapply(variables, function(x) {
  if (is.factor(x)) nlevels(droplevels(x)) <= 1
  else length(unique(x)) <= 1
})

variables_clean <- variables[, !const_cols, with = FALSE]

##################
# simulazione tempi

# controllo distribuzione
data[, .N, .(metastatic, esposizione)]
# data[, .N, .(metastatic, morte)]
data[, .N, .(switch, esposizione)]


#############################################

# parametri per tempo_causa_1 (morte)
b0_out  <- 9.5
b1_out  <- 0
b2_out  <- -2.0
b3_age  <- -0.02
sig_out <- 0.7
H <- 3650   # orizzonte di accertamento della morte (giorni)

b_scompcard <- -0.45
b_infart <- -0.35
b_ictus <- -0.40

# parametri per tempo_causa_2 (fine_studio)
b0_c2 <- 7.2
b1_c2 <- 0
sig_c2 <- 0.45

# parametri per tempo_causa_3 (fine_finestra_studio)
# b0_c3 <- 7.1
# b1_c3 <- 0
# sig_c3 <- 0.3

# parametri per tempo_causa_4 (fine_osservazione_persona)
b0_c4 <- 7
b1_c4 <- 0
sig_c4 <- 0.4

# parametri per tempo_interruzione
b0_int <- 6.5
b1_int <- 0
b2_int <- 0
sig_int <- 0.65

# parametri per tempo_switch
b0_sw <- 5.8
b1_sw <- -0.8
b2_sw <- 0
sig_sw <- 0.65

# generazione tempi con modello Weibull
simulazione <- function(b0, b1, trt, b2, meta, b3, age, sigma) {
  u <- runif(length(trt), min = 0, max = 1)
  exp(b0 + (b1 * trt) + (b2*meta) + (b3*age)) * (-log(u))^(sigma)
}

# tempi causa_censura_j
# data[, tempo_causa_1 := simulazione(b0_out, b1_out, esposizione, 1, (b2_out * metastatic) + 
#                                       (b3_age * (age - mean(age, na.rm = TRUE))) + 
#                                       (b_scompcard * scompcard) + 
#                                       (b_infart * infart) + 
#                                       (b_ictus * ictus),0, 0, sig_out)]
data[, lp_out := b0_out + b1_out * esposizione + b2_out * metastatic +
       b3_age * (age - mean(age)) +
       b_scompcard * scompcard + b_infart * infart + b_ictus * ictus]
set.seed(898)
data[, tempo_causa_1 := exp(lp_out) * (-log(runif(.N)))^sig_out]

data[, morte := as.integer(tempo_causa_1 <= H)]
data[, tempo_causa_2 := simulazione(b0_c2, b1_c2, esposizione, 0, 0, 0, 0, sig_c2)]
data[, tempo_causa_3 := 365]
data[, tempo_causa_4 := simulazione(b0_c4, b1_c4, esposizione, 0, 0, 0, 0, sig_c4)]

# tempi interruzione e switch
data[, tempo_interruzione := simulazione(b0_int, b1_int, esposizione, 0, 0, 0, 0, sig_int)]
data[, tempo_switch := simulazione(b0_sw, b1_sw, esposizione, b2_sw, 0, 0, 0, sig_sw)]

# data[, tempo_causa_1:= ifelse(morte ==1, tempo_causa_1, NA)]

# tempo censura
data[, tempo_censura := pmin(tempo_causa_1, tempo_causa_2, tempo_causa_3, tempo_causa_4, na.rm = TRUE)]
data[, causa_censura := fcase(tempo_censura == tempo_causa_1, 1, 
                              tempo_censura == tempo_causa_2, 2,
                              tempo_censura == tempo_causa_3, 3,
                              tempo_censura == tempo_causa_4, 4, default = NA_integer_
     )]

# outcome
data[, outcome := ifelse(causa_censura == 1, 1, 0)]

# tempo_causa_j che coincide con tempo_censura
data[, `:=`(
  tempo_causa_1 = ifelse(causa_censura == 1, tempo_causa_1, NA),
  tempo_causa_2 = ifelse(causa_censura == 2, tempo_causa_2, NA),
  tempo_causa_3 = ifelse(causa_censura == 3, tempo_causa_3, NA),
  tempo_causa_4 = ifelse(causa_censura == 4, tempo_causa_4, NA)
)]

# controllo che tempi interruzione e switch siano precedenti alla censura
data[, interruzione_pre_censura := ifelse(interruzione == 1 & !is.na(tempo_interruzione) & tempo_interruzione <= tempo_censura, 1, 0)]
data[, tempo_interruzione := ifelse(interruzione_pre_censura==1, tempo_interruzione, NA)]

data[, switch_pre_censura := ifelse(switch == 1 & !is.na(tempo_switch) & tempo_switch <= tempo_censura, 1, 0)]
data[, tempo_switch := ifelse(switch_pre_censura==1, tempo_switch, NA)]

# stima PS
ps_model <- glm(esposizione ~ ., 
                data = variables_clean, 
                family = binomial(link = "logit"))

data[, ps := ps_model$fitted.values]

p_treated <- mean(data$esposizione == 1, na.rm = TRUE)

# calcolo pesi
data[, weight := ifelse(esposizione == 1,
                        p_treated / ps,
                        (1 - p_treated) / (1 - ps))]

# trimming
cols_to_keep <- colnames(data)
required_cols <- c("person_id", "t0", "esposizione", "ps")

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
    data[esposizione == 1, ps],
    probs = trunc_par_exposed$lower_limit / 100,
    na.rm = TRUE
  )
}

ps_upper_exposed <- NA_real_

if (!is.na(trunc_par_exposed$upper_limit)) {
  
  ps_upper_exposed <- quantile(
    data[esposizione == 1, ps],
    probs = trunc_par_exposed$upper_limit / 100,
    na.rm = TRUE
  )
}

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
  data[in_overlap_tmp == 1L, esposizione == 1],
  na.rm = TRUE
)

data[
  in_overlap_tmp == 1L,
  weight := fifelse(
    esposizione == 1,
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

# verifica parametri su ogni variabile
#tratto il tempo simulato come evento di interesse

data[, .N, causa_censura]

fit_grezzo <- coxph(Surv(tempo_censura, outcome) ~ esposizione,
                    data = data)
fit_grezzo

prop.table(table(data$metastatic, data$esposizione), margin=2)*100

fit_cox_ipw <- coxph(Surv(tempo_censura, outcome) ~ esposizione,
                     data = data[in_overlap_tmp == 1],
                     weights = truncated_weight,
                     robust = TRUE)

fit_cox_ipw

hr_grezzo <- exp(coef(fit_grezzo))
hr_grezzo

hr_ipw <- exp(coef(fit_cox_ipw))
hr_ipw

weighted_table <- data[, .(
  pct_metastatic_0 = sum(weight[metastatic == 0]) / sum(weight),
  pct_metastatic_1 = sum(weight[metastatic == 1]) / sum(weight)
), by = esposizione]

weighted_table

summary(data$weight)

smd <- function(x, g, w = rep(1, length(x))) {
  m1 <- weighted.mean(x[g == 1], w[g == 1])
  m0 <- weighted.mean(x[g == 0], w[g == 0])
  (m1 - m0) / sqrt((m1 * (1 - m1) + m0 * (1 - m0)) / 2)
}
smd(data$metastatic, data$esposizione) # prima
smd(data$metastatic, data$esposizione, data$weight) #dopo



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
listvar <- c("person_id", "t0", "esposizione", "interruzione",
             "switch", "morte", "causa_censura", "outcome", "tempo_censura", 
             "tempo_causa_1", "tempo_causa_2", "tempo_causa_3", "tempo_causa_4",
             "interruzione_pre_censura", "switch_pre_censura",
             "tempo_interruzione","tempo_switch"
)

data<-data[,..listvar]

namedataset <- "D4_follow_up"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))

## salvo D4_esp_comp_pesi_troncati_trimmed
cols_to_keep <- c("person_id", "t0", "esposizione", "age", covariates_binary, 
                  "ps", "weight", "truncated_ps", "truncated_weight")

input <- input[,..cols_to_keep]

nameoutput <- "D4_esp_comp_pesi_troncati_trimmed"
saveRDS(input, file = paste0(thisdir, "/", nameoutput,".rds"))

