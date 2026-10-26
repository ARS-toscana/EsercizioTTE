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
probexposure = 0.55


#######################################
# DATA GENERATION

# all variables
list_variables <- c("person_id", "date_first", "op_start", "op_end", "year_first", "ASL", "age", "drug", covariates_binary)


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

set.seed(54632)
data[, random := round(runif(.N, min = 1, max = date_first - 365 - dmy(paste0("0101",as.character(birth_year)))))]
data[, op_start  := as.Date( date_first - random)]
data[, date_first := pmax(date_first, dmy(paste0("0101",as.character(birth_year))))]
set.seed(14632)
data[, random := round(rpois(.N, lambda = 5000))]
data[, op_end := pmin(date_first + random, study_end_date)]
data[,random := NULL]

# year_first
data[, year_first:=year(date_first)]

# exposure
set.seed(1252)
data[, drug := rbinom(.N, size = 1, prob = probexposure)]
data[, drug:=ifelse(drug==1, "A", "B")]

# ASL
data[, ASL:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                   prob = c(rep(0.33, 3)))]

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
  data[drug == "A" & random > .95, cov := 1]
  setnames(data,"cov",cb)
  j <- j + 1
}

data[,random := NULL]

# keep variables
data <- data[, ..list_variables]

# save
namedataset <- "D3_esp_comp_con_baseline"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
