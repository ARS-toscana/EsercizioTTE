##############################################
# simulate input for 03_T3_10_crea_D4_esp_comp_pesi

# author: Rosa Gini, Olga Paoletti, Claudia Bartolini, Sabrina Giometto, Elena Ferrati

# v 1.0 22 Oct 2026

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

library(survival)
set.seed(123)
N<-5000

drug <- sample(c("A", "B"), size=N, replace = TRUE, prob = c(0.5, 0.5))
drug <- ifelse(drug == "A", 1, 0)

#parametri per tempo_censura
b0_cens<-2.8
b1_cens<-0.1
sig_cens<-0.5

#parametri per tempo_outcome_1
b0_out_1<-5.5
b1_out_1<--0.6
sig_out_1<-0.7

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
simulazione <- function(b0, b1, trt, sigma) {
  u <- runif(length(trt), min = 0, max = 1)
  exp(b0 + (b1 * trt)) * (-log(u))^(sigma)
}

df_simulato <- data.frame(
  id = 1:N,
  drug = drug,
  tempo_censura = simulazione(b0_cens, b1_cens, drug, sig_cens),
  # tempo_outcome_0 = simulazione(b0_out_0, b1_out_0, drug, sig_out_0),
  tempo_outcome_1 = simulazione(b0_out_1, b1_out_1, drug, sig_out_1)
)


#verifica parametri su ogni variabile
#tratto il tempo simulato come evento di interesse

# fit_cens <- survreg(Surv(tempo_censura) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_cens))
# 
# fit_out <- survreg(Surv(tempo_outcome) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_out))

# fit_c1 <- survreg(Surv(tempo_causa_1) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_c1))
# 
# fit_c2 <- survreg(Surv(tempo_causa_2) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_c2))
# 
# fit_int <- survreg(Surv(tempo_interruzione) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_int))
# 
# fit_sw <- survreg(Surv(tempo_switch) ~ drug, data = df_simulato, dist = "lognormal")
# print(summary(fit_sw))

fit_cens<-coxph(Surv(tempo_censura) ~ drug, data = df_simulato)
fit_cens
# fit_out_0<-coxph(Surv(tempo_outcome_0) ~ drug, data = df_simulato)
# fit_out_0
fit_out_1<-coxph(Surv(tempo_outcome_1) ~ drug, data = df_simulato)
fit_out_1


tempi <- as.matrix(df_simulato[, c("tempo_outcome_1", "tempo_censura")])
df_simulato$tempo_fu <- do.call(pmin, as.data.frame(tempi))
df_simulato$causa_fine  <- ifelse(df_simulato$tempo_outcome_1 <= df_simulato$tempo_censura, "outcome", "censura")

df_simulato$status_evento <- ifelse(df_simulato$causa_fine == "outcome", 1, 0)

# fit_oss <- function(causa) {
#   ev <- as.integer(df_simulato$causa_fine == causa)
#   survreg(Surv(tempo_fu, ev) ~ drug, data = df_simulato, dist = "lognormal")
# }
# fit_out_oss <- fit_oss("outcome")
# c(coef(fit_out_oss), sigma = fit_out_oss$scale)

table(df_simulato$causa_fine)
fit_cox_outcome<-coxph(Surv(tempo_fu, status_evento) ~ drug, data = df_simulato)
fit_cox_outcome
hr <- exp(coef(fit_cox_outcome))
