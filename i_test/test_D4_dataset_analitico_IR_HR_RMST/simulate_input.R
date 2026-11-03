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

###
library(data.table)
library(survival)
set.seed(123)
N<-5000

metastatic <- rbinom(N, size=1, prob=0.3)

drug <- sample(c("A", "B"), size=N, replace = TRUE, prob = c(0.5, 0.5))
drug <- ifelse(drug == "A", 1, 0)

#drug dipendente da metastatic (modello logistico)
alpha0<--0.5
alpha1<-1.2
p_drug<-1/ (1+exp(-(alpha0+alpha1*metastatic)))
drug<-rbinom(N, size=1, prob=p_drug)

table(metastatic, drug)

#parametri per tempo_censura
b0_cens<-2.8
b1_cens<-0.1
sig_cens<-0.5

#parametri per tempo_outcome
b0_out<-5.5
b1_out<-0
b2_out<--1.2 #effetto di metastatic su outcome
sig_out<-0.7

# #parametri per tempo_outcome_0
# b0_out_0<-2.5
# b1_out_0<-1.2
# sig_out_0<-0.3


# #parametri per tempo_causa_<j> (es due cause j=1 e j=2)
# b0_c1<-2.5; b1_c1<-0.0; sig_c1<-0.6
# b0_c2<-2.6; b1_c2<-0.2; sig_c2<-0.8
# 
# #parametri per tempo_interruzione
# b0_int<-1.8
# b1_int<--0.6
# sig_int<-0.9
# 
# #parametri per tempo_switch
# b0_sw<-1.9
# b1_sw<-0.3
# sig_sw<-0.85

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
simulazione <- function(b0, b1, trt, b2, cov, sigma) {
  u <- runif(length(trt), min = 0, max = 1)
  exp(b0 + (b1 * trt) + (b2*cov)) * (-log(u))^(sigma)
}

df_simulato <- data.frame(
  id = 1:N,
  metastatic = metastatic,
  drug = drug,
  tempo_censura = simulazione(b0_cens, b1_cens, drug, 0, rep(0,N), sig_cens),
  # tempo_outcome_0 = simulazione(b0_out_0, b1_out_0, drug, sig_out_0),
  tempo_outcome = simulazione(b0_out, b1_out, drug, b2_out, metastatic, sig_out)
)


#verifica parametri su ogni variabile
#tratto il tempo simulato come evento di interesse

fit_cens<-coxph(Surv(tempo_censura) ~ drug, data = df_simulato)
fit_cens
# fit_out_0<-coxph(Surv(tempo_outcome_0) ~ drug, data = df_simulato)
# fit_out_0
fit_out<-coxph(Surv(tempo_outcome) ~ drug, data = df_simulato)
fit_out

tempi <- as.matrix(df_simulato[, c("tempo_outcome", "tempo_censura")])
df_simulato$tempo_fu <- do.call(pmin, as.data.frame(tempi))
df_simulato$causa_fine  <- ifelse(df_simulato$tempo_outcome <= df_simulato$tempo_censura, "outcome", "censura")
df_simulato$status_evento <- ifelse(df_simulato$causa_fine == "outcome", 1, 0)

# fit_oss <- function(causa) {
#   ev <- as.integer(df_simulato$causa_fine == causa)
#   survreg(Surv(tempo_fu, ev) ~ drug, data = df_simulato, dist = "lognormal")
# }
# fit_out_oss <- fit_oss("outcome")
# c(coef(fit_out_oss), sigma = fit_out_oss$scale)

table(df_simulato$causa_fine)
fit_grezzo<-coxph(Surv(tempo_fu, status_evento) ~ drug, data = df_simulato)
fit_grezzo
fit_aggiustato <- coxph(Surv(tempo_fu, status_evento) ~ drug + metastatic, data = df_simulato)
fit_aggiustato
hr_grezzo <- exp(coef(fit_grezzo))
hr_grezzo
hr_aggiustato <- exp(coef(fit_aggiustato))
hr_aggiustato

prop.table(table(df_simulato$metastatic, df_simulato$drug), margin=2)*100


##############
df_simulato <- as.data.table(df_simulato)

ps_model <- glm(
  drug ~ metastatic, 
  data = df_simulato, 
  family = binomial(link = "logit")
)

df_simulato[, ps := ps_model$fitted.values]

p_treated <- mean(df_simulato$drug == 1, na.rm = TRUE)

df_simulato[, weight := ifelse(
  drug == 1,
  p_treated / ps,
  (1 - p_treated) / (1 - ps)
)]

fit_cox_ipw <- coxph(
  Surv(tempo_fu, status_evento) ~ drug, 
  data = df_simulato, 
  weights = weight,
  robust = TRUE
)

fit_cox_ipw

hr_ipw <- exp(coef(fit_cox_ipw))
hr_ipw

weighted_table <- df_simulato[, .(
  pct_metastatic_0 = sum(weight[metastatic == 0]) / sum(weight),
  pct_metastatic_1 = sum(weight[metastatic == 1]) / sum(weight)
), by = drug]

weighted_table

summary(df_simulato$weight)

smd <- function(x, g, w = rep(1, length(x))) {
  m1 <- weighted.mean(x[g == 1], w[g == 1])
  m0 <- weighted.mean(x[g == 0], w[g == 0])
  (m1 - m0) / sqrt((m1 * (1 - m1) + m0 * (1 - m0)) / 2)
}
smd(df_simulato$metastatic, df_simulato$drug) # prima
smd(df_simulato$metastatic, df_simulato$drug, df_simulato$weight) #dopo


