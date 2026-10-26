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

#######################################
# DATA GENERATION

# all variables
list_variables <- c("person_id", "date_first", "episode_start", "episode_end")


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
data[, random := round(runif(.N, min = 0, max = 365*10))]
data[, episode_start  := as.Date(date_first)+random]
# data[, date_first := pmax(date_first, dmy(paste0("0101",as.character(birth_year))))]
set.seed(14632)
data[, random := round(runif(.N, min = 1, max = 365*10-1))]
data[, episode_end := as.Date(episode_start)+random]
data[,random := NULL]



# keep variables
data <- data[, ..list_variables]

# save
namedataset <- "D3_episodi_trattamento"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
