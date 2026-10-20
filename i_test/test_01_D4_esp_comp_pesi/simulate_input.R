##############################################
# simulate input for D4_coorte_baseline_pesi

# author: Rosa Gini, Olga Paoletti, Claudia Bartolini

# v 1.0 12 Oct 2026

##################################################
# HEADER

rm(list = ls(all.names = TRUE))

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


# set prob of exposed

probexposure <- 0.3

# input metadata

thisdirarchive <- file.path(thisdir, "..", "..", "run", "p_parameters", "archive_parameters")


# list covariates per type

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D3_coorte_baseline.xlsx")),1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(values)])
thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)])
thiscovariates_cont <- unlist(parameters_this_step[parameter == "covar_cont",.(values)])

covariates_binary <- paste0(thiscovariates_bin,"_at_t0")


covariates_cat <- paste0(thiscovariates_cat,"_at_t0")

covariates_cont <- paste0(thiscovariates_cont, "_at_t0")

# probability of exposure

probexposure = c(.34)


#######################################
# DATA GENERATION

# all variables


list_variables <- c("person_id", "t0", "gender_at_t0", "birth_year", "age_at_t0", "entry_spell_category", "exit_spell_category", "exposure", covariates_binary, covariates_cat, covariates_cont) # person_id 

#############
# create base 
data <- data.table::data.table(person_id = 1:Npersons)
# person_id 

data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

# gender

set.seed(12499)
data[, sex := sample(1:3, Npersons, replace = TRUE, prob = c(.5,.49,.01))]
# data[,sex := fifelse(sex_at_instance_creation == 1,"M","F") ]
data[, gender_at_t0 := c("M", "F", "O")[sex]]
data[, sex := NULL]

# birth_year

set.seed(523634)
birth_year_list <- c(1915,1925,1935,1945,1955)
probyear = c(.01,.1,.2,.5)
totprob = sum(probyear)
probyear = c(probyear, 1 - totprob)
data[, birth_year := sample(birth_year_list, Npersons, replace = TRUE, prob = probyear)]


##################
# t0

set.seed(1351)
data[, t0 := as.Date(floor(runif(.N, min = study_start_date, max = study_end_date+1)))]

##################
# age at t0

data[, age_at_t0 := year(t0) - birth_year]



# entry and exit_spell

set.seed(54632)
data[, random := round(runif(.N, min = 1, max = t0 - 365 - dmy(paste0("0101",as.character(birth_year)))))]
data[, entry_spell_category  := as.Date( t0 - random)]
data[, t0 := pmax(t0, dmy(paste0("0101",as.character(birth_year))))]
set.seed(14632)
data[, random := round(rpois(.N, lambda = 5000))]
data[, exit_spell_category := pmin(t0 + random, study_end_date)]
data[,random := NULL]


##################
# exposure

set.seed(1252)
data[, exposure := rbinom(.N, size = 1, prob = probexposure)]


##################
# covariates at t0

#binary

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
  data[exposure == 1 & random > .95, cov := 1]
  setnames(data,"cov",cb)
  j <- j + 1
}

for (cb in covariates_cont) {
  seedchange <- 112231 + j
  set.seed(seedchange)
  labname <- gsub("_at_t0", "", cb)
  print(labname)
  Minlab <- 0
  Maxlab <- 5000
  data[, cov := runif(.N, min = Minlab, max = Maxlab)]
  seedchange <- 112302 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[random > .5, cov := NA_real_]
  setnames(data,"cov",cb)
  j <- j + 1
}


for (cb in setdiff(covariates_cat, "gender_at_t0")) {
  seedchange <- 1131 + j
  set.seed(seedchange)
  labname <- gsub("_at_t0", "", cb)
  print(labname)
  varname <- paste0(labname,"value")
  record <- parameters_this_step[parameter == varname,]
  thisvalues <- record$values
  data[, cov := sample(thisvalues, .N, replace = T)]
  seedchange <- 1135 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[random > .5, cov := NA_real_]
  record <- parameters_this_step[values == labname,]
  has_missing <- record$has_missing[1]
  print(has_missing)
  if (is.null(has_missing) | has_missing == 0) {
    data[is.na(cov), cov := "0"]
  }
  setnames(data,"cov",cb)
}


# keep variables

data <- data[, ..list_variables]



# save

namedataset <- "D3_coorte_baseline"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))


