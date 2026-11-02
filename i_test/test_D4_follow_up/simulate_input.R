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

# set probability of exposure
probexposure = 0.55


#######################################
# DATA GENERATION for D3_esp_comp

# all variables
list_variables <- c("person_id", "date_first", "op_start", "op_end", "year_first", "ASL", "drug")


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
data[, random := round(rpois(.N, lambda = 1000))]
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

# keep variables
data <- data[, ..list_variables]

# save
namedataset <- "D3_esp_comp"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))

#######################################
# DATA GENERATION for D3_outcome

# set # of persons
Npersons <- 5000

# all variables
list_variables <- c("person_id", "date_first", "outcome", "data_outcome")


# create base 
data <- data.table::data.table(person_id = 1:Npersons)

# person_id 
data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

# date_first
set.seed(1351)
data[, date_first := sample(seq(study_start_date, study_end_date, by = "day"),
                            .N, replace = TRUE)]

# outcome
set.seed(123)
data[, outcome := rbinom(.N, size = 1, prob = 0.12)]

# data outcome
set.seed(14632)
data[, data_outcome := date_first + round(rexp(.N, rate = 1/3000))]
data[data_outcome > study_end_date, data_outcome := NA]


# keep variables
data <- data[, ..list_variables]

# save
namedataset <- "D3_outcome"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))


#######################################
# DATA GENERATION for D3_episodi_trattamento

# Più righe per soggetto, una per episodio; il farmaco può cambiare tra episodi.
# Il primo episodio inizia a date_first con il farmaco indice di D3_esp_comp.

# all variables
list_variables <- c("person_id", "date_first", "episode_start", "episode_end", "drug")

# farmaci possibili e parametri della simulazione
farmaci       <- c("A", "B")
max_episodi   <- 6L     # numero massimo di episodi per persona
lambda_ep     <- 1.5    # media degli episodi aggiuntivi oltre al primo
durata_media  <- 300    # durata media di un episodio (giorni, oltre ai 30 minimi)
gap_medio     <- 120    # intervallo medio tra un episodio e il successivo (giorni)
prob_cambio   <- 0.3    # probabilità che un episodio usi un farmaco diverso dal precedente

# base: persone, data indice e farmaco indice da D3_esp_comp
base <- readRDS(file = paste0(thisdir, "/D3_esp_comp.rds"))
base <- base[, .(person_id, date_first, op_end, drug_indice = drug)]

# numero di episodi per persona
set.seed(7841)
base[, n_episodi := pmin(1L + rpois(.N, lambda = lambda_ep), max_episodi)]

# una riga per episodio
data <- base[rep(seq_len(.N), n_episodi)]
data[, n := rowid(person_id)]

# durata degli episodi e intervallo dal precedente (0 per il primo episodio)
set.seed(3329)
data[, durata := 30L + as.integer(round(rexp(.N, rate = 1/durata_media)))]
set.seed(9152)
data[, gap := fifelse(n == 1L, 0L, 1L + as.integer(round(rexp(.N, rate = 1/gap_medio))))]

# date degli episodi: il primo inizia a date_first, i successivi dopo la fine del precedente + gap
data[, episode_start := date_first + cumsum(shift(durata, fill = 0L) + gap), by = person_id]
data[, episode_end := episode_start + durata]

# farmaco: primo episodio = farmaco indice; poi con probabilità prob_cambio si passa
# a un farmaco diverso dal precedente, altrimenti si continua con lo stesso
set.seed(6610)
data[, cambia := rbinom(.N, size = 1, prob = prob_cambio)]
data[, offset := sample(1:(length(farmaci) - 1), .N, replace = TRUE)]
data[, drug := NA_character_]
data[n == 1L, drug := drug_indice]
if (max(data$n) > 1) {
  for (k in 2:max(data$n)) {
    data[, drug_prec := shift(drug), by = person_id]
    data[n == k, drug := fifelse(cambia == 1L,
                                 farmaci[(match(drug_prec, farmaci) + offset - 1L) %% length(farmaci) + 1L],
                                 drug_prec)]
  }
}

# episodi entro l'osservazione e lo studio: si eliminano quelli che iniziano dopo
# e si tronca la fine degli altri
data <- data[episode_start <= pmin(op_end, study_end_date)]
data[, episode_end := pmin(episode_end, op_end, study_end_date)]

# keep variables
data <- data[, ..list_variables]
setorder(data, person_id, episode_start)

# save
namedataset <- "D3_episodi_trattamento"

saveRDS(data, file = paste0(thisdir, "/", namedataset, ".rds"))

