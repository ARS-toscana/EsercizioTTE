############################################################
#                                                          #
####   CREA CDM_INSTANCE ####
# 
#                                                          #
############################################################

# author: Rosa Gini,

# v 1.0 12 Nov 2026

# genera CDM sintentico (da https://github.com/target-roc19/case-study-03-dapagliflozin-public/blob/main/transformations/991_Create_CDM_instance.R)
# autore: rosa gini

##############################

# data baseline da cui calcolare il numero di giorni come intero (è il giorno prima di study_start_date, così '+ 1' è il giorno di inizio studio)

baselinedate <- study_start_date - 1

# input data model

dirinputCDM <- file.path(dirarchive,"i_CDM_data_model")

# versione

instance_version <- 0.1
version <- instance_version
dirversionCDM <- dirinput # paste0(dirCDM,"/v_",version)
dir.create(file.path(dirversionCDM), showWarnings = F)



# lista di tutti i concept_id che sono utili per lo studio (list completa in p_parameetrs/3_concept_sets.R)

concept_id_list <- c(concept_sets_of_our_study_drugs, concept_sets_of_our_study_diagnosis)

# type of concept_id

type_of_concept_id <- list()

for (conc in concept_id_list) {
  if (conc %in% concept_sets_of_our_study_diagnosis) {
    type_of_concept_id[[conc]] <- "Diagnosis"
  }
  if (conc %in% concept_sets_of_our_study_drugs) {
    type_of_concept_id[[conc]] <- "Medicines"
  }
}


# crea un dataset code_concept_id_codsys_DT con un codice per ogni concept_id

code_concept_id_codsys_DT <- data.table(concept_id = concept_id_list)
code_concept_id_codsys_DT[,  tc := unlist(type_of_concept_id[concept_id], use.names = FALSE)]
code_concept_id_codsys_DT[,  coding_system := fifelse(tc == "Diagnosis", "ICD9", "ATC")]
code_concept_id_codsys_DT[, code := mapply(function(c, s) {
  v <- concept_set_codes_our_study[[c]][[s]]
  if (is.null(v)) NA_character_ else as.character(v[1])
}, concept_id, coding_system)]

code_concept_id_codsys_DT <- unique(code_concept_id_codsys_DT)

##############################
# crea file 'seed'
##############################

# lista dei file 'seed'
tablesCDM <- c("ANAGRAFE_ASSISTITI",
               "sdo",
               "sdoproc",
               "fed",
               "spf",
               "exe",
               "ps",
               "MISURE",
               "SURVEY")


# variabili data di ogni seed file
listdatesCDM <- list()
listdatesCDM[["ANAGRAFE_ASSISTITI"]] <- c("data_inizioass","data_fineass", "datanas", "datadec")
listdatesCDM[["sdo"]] <- c("data_a","data_d")
listdatesCDM[["sdoproc"]] <- c("data_int")
listdatesCDM[["fed"]] <- c("datasped")
listdatesCDM[["spf"]] <- c("datasped")
listdatesCDM[["exe"]] <- c("datai","dataf")
listdatesCDM[["MISURE"]] <- c("ini_record","fine_record")
listdatesCDM[["ps"]] <- c("data_a")
listdatesCDM[["SURVEY"]] <- c("data")

listcharCDM <- list()

for (tab in tablesCDM){ 
  data <- data.table()
  assign(paste0(tab,"data"),data)
  data <-  as.data.table(readxl::read_excel(file.path(dirinputCDM, paste0(tab, ".xlsx"))))
  colvars <- listdatesCDM[[tab]]
  if (!is.null(colvars)) {
    data[, (colvars) := lapply(.SD, as.IDate), .SDcols = colvars]  
  }
  charvars <- listcharCDM[[tab]]
  if (!is.null(charvars)) {
    data[, (charvars) := lapply(.SD, as.character), .SDcols = charvars]  
  }
  assign(paste0(tab,"data_model"),data)
}

#############
# helper
#############

clean_and_save <- function(
    data,
    tablename,
    suffix = ""){
  data <- rbind(get(paste0(tablename,"data_model")),data,fill =T)
  tokeep <- names(get(paste0(tablename,"data_model")))
  data <- data[,..tokeep]
  date_cols <- sapply(data, is.Date)
  date_cols <- names(date_cols)[date_cols]
  data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
  fwrite(data, file.path(dirversionCDM, paste0(paste0(tablename,suffix),".csv") ))
  return(data)
}

#############################################################################
#############################################################################
# ELEVA AL CDM I DATI DI UN TEST
  
testfolder <- "test_D4_dataset_analitico_IR_HR_RMST"
dataset_to_be_regressed <- readRDS(file.path(dirtest,testfolder,"D4_esp_comp_pesi_troncati_trimmed.rds"))
data_fup <- readRDS(file.path(dirtest,testfolder,"D4_follow_up.rds"))
dataset_to_be_regressed <- merge(dataset_to_be_regressed, data_fup[,.(person_id, tempo_causa_1, tempo_interruzione, tempo_switch)], by = "person_id")

testfolder <- "test_D4_esp_comp_pesi"
dataset_op <- readRDS(file.path(dirtest,testfolder,"D3_esp_comp_con_baseline.rds"))
dataset_op <- dataset_op[,.(person_id, op_start, op_end)]
dataset_to_be_regressed <- merge(dataset_to_be_regressed, dataset_op, by = "person_id")


############da rimuovere una volta che i data model è sistemato

if (!("metastatic" %in% names(dataset_to_be_regressed))) {
  dataset_to_be_regressed[, metastatic := 0]
}
if (!("t0" %in% names(dataset_to_be_regressed))) {
  dataset_to_be_regressed[, t0 := date_first]
}
if (!("exposure" %in% names(dataset_to_be_regressed))) {
  dataset_to_be_regressed[, exposure := fifelse(drug == "A", 1, 0)]
}

########################### fine da rimuovere


#######################
# caratteristiche individui

tab <- "ANAGRAFE_ASSISTITI"

data <- unique(copy(dataset_to_be_regressed))

# datadec

setnames(data, "person_id", "id")

# sesso

data[, sesso := 1]

# datanas

set.seed(5234)
data[,year_first := year(date_first) ]
data[,year_of_birth := year_first - age]
data[,days_birth := date_first - ymd(paste0(as.character(year_first),"0101"))]
data[, datanas := ymd(paste0(as.character(year_of_birth),"0101")) + round(days_birth * runif(.N))]

# data_inizioass data_fineass

setnames(data, "op_start", "data_inizioass")
setnames(data, "op_end", "data_fineass")

# datadec

data[, datadec := date_first + round(365.25 * tempo_causa_1)]

# aggiungi qualche periodo di osservazione in più nel passato

set.seed(13334)
toadd <- copy(data)
toadd[, random := runif(.N)]
toadd <- toadd[ random < .05 & datanas < data_inizioass,]
set.seed(33443)
toadd[, data_fineass := data_inizioass - round(5000 * runif(.N))]
set.seed(3322)
toadd[, data_inizioass := data_fineass - round(5000 * runif(.N))]
data <- rbind(data, toadd, fill = T)
setorder(data, id, data_inizioass)

# aggiungi qualche persona in più

set.seed(71333)
toadd <- copy(data)
toadd[, numid := as.numeric(substr(id, 2, nchar(id)))]
maxid <- max(toadd$numid)
toadd[, random := runif(.N)]
toadd <- toadd[ random < .5,] 
set.seed(1234)
data[, sesso := as.character(sample(1:2, .N, replace = TRUE, prob = c(.5,.5)))]
set.seed(81323)
toadd[, random := runif(.N)]
toadd[ random > .1, datadec := NA_Date_] 
toadd[,id := paste0("000000",as.character(seq_len(.N) + maxid))]
toadd[,id := paste0("P",substr(id, nchar(id) - 6, nchar(id)))]
data <- rbind(data, toadd, fill = T)

# pulisci e salva

setorder(data, id, data_inizioass)
data <- clean_and_save( data, tab)


##############################
# COVARIATES


# crea un dataset to_CDM in cui i concept_id di tutte le covariate sono assegnati a una data prima di t0 t0, poi aggiungi e rimuovi qualcosa

ingredients_covariates <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_esp_comp_baseline.xlsx")),1)))

thiscovariates <- unlist(unique(ingredients_covariates[parameter == "component" ,.(value)]))


ingredients_covariates <- ingredients_covariates[parameter != "component",.(parameter, value, howmany, start, end  )]

setnames(ingredients_covariates, c("parameter", "value"), c("varname", "concept_id"))

data <- copy(dataset_to_be_regressed)


for (varname in thiscovariates) {
  data[, (varname) := as.character(get(varname))]
}


data <- melt(
  data,
  id.vars = c("person_id", "t0", "op_start","op_end"),
  variable.name = "varname",
   measure.vars = thiscovariates,
  value.name = "value"
)

data <- data[value != "0",]


to_CDM <- merge(data, ingredients_covariates, by = "varname", allow.cartesian = T)
to_CDM <- to_CDM[rep(seq_len(.N), howmany)]

# genera la data

set.seed(452783)
to_CDM[, random := runif(.N)]
to_CDM[, date := as.Date(t0 - round(random*(end-start)))]

# aggungi qualche riga degli stessi soggetti

set.seed(49333)
toadd <- copy(to_CDM)
toadd[, random := runif(.N)]
toadd <- toadd[ random < .1,] 
set.seed(77933)
to_CDM[, date := date + round(500*rnorm(.N))]
to_CDM <- rbind(to_CDM, toadd, fill = T)


# aggiungi qualche riga di altri soggetti

set.seed(89333)
toadd <- copy(to_CDM)
toadd[, numid := as.numeric(substr(person_id, 2, nchar(person_id)))]
toadd[, random := runif(.N)]
toadd <- toadd[ random < .5,] 
toadd[,person_id := paste0("000000",as.character(seq_len(.N) + maxid))]
toadd[,person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]
to_CDM <- rbind(to_CDM, toadd, fill = T)

# elimina qualche riga qui e lì

set.seed(52783)
to_CDM[, random := runif(.N)]
to_CDM <- to_CDM[(random > .05) , ]

# aggiungi l'esposizione

data <- copy(dataset_to_be_regressed)
data <- unique(data[, .(person_id, t0, exposure)])
data[exposure == 1,concept_id := "A"]
data[exposure == 0,concept_id := "B"]
set.seed(452783)
data[, random := runif(.N)]
data[, date := as.Date(t0)]
to_CDM <- rbind(to_CDM, data, fill = T)

# record di switch

data <- copy(dataset_to_be_regressed)
data <- data[,.(person_id, t0, tempo_switch, exposure)]
data <- data[!is.na(tempo_switch),]
data[, date := t0 + round(365.25 * tempo_switch)]
data[exposure == 1,concept_id := "B"]
data[exposure == 0,concept_id := "A"]
to_CDM <- rbind(to_CDM, data, fill = T)


# altri record di esposizione

data <- copy(dataset_to_be_regressed)
set.seed(34627)
data <- data[,.(person_id, t0, tempo_interruzione, exposure)]
data <- data[!is.na(tempo_interruzione),]
data[, howmany := floor(365.25 * tempo_interruzione/45) ]
data <- data[rep(seq_len(.N), howmany)]
setorder(data, person_id)
data[, ord := seq_len(.N), by = person_id]
data[, incr := fifelse(ord == 1, 0, 25 + round(15 * runif(.N)))]
data[, date := t0[1] + cumsum(incr), by = person_id]
data[, incr := NULL]
data[exposure == 1,concept_id := "A"]
data[exposure == 0,concept_id := "B"]
to_CDM <- rbind(to_CDM, data, fill = T)


#########################################
# converti to_CDM in tabelle di CDM

tab <- "sdo"

data <- copy(to_CDM)[concept_id %in% concept_sets_of_our_study_diagnosis,]
limit <- .8*data[,.N]
data <- data[1: limit,]
setnames(data, "person_id","id")
setnames(data, "date","data_a")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[,dia := code]
data[, dia_col := 1]
data <- clean_and_save( data, tab)

tab <- "exe"

data <- copy(to_CDM)[concept_id %in% concept_sets_of_our_study_diagnosis,]
limit1 <- .8*data[,.N]
limit2 <- .9*data[,.N]
data <- data[limit1:limit2,]
setnames(data, "person_id","id")
setnames(data, "date","datai")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[,dia := code]
data <- clean_and_save( data, tab)

tab <- "ps"

data <- copy(to_CDM)[concept_id %in% concept_sets_of_our_study_diagnosis,]
limit1 <- .9*data[,.N]
limit2 <- data[,.N]
data <- data[limit1:limit2,]
setnames(data, "person_id","id")
setnames(data, "date","data_a")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[,dia := code]
data <- clean_and_save( data, tab)



# fed

tab <- "fed"

data <- copy(to_CDM)[concept_id %in% c("A", "B"),]
setnames(data, "person_id","id")
setnames(data, "date","datasped")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[, atc := code]
data[, pezzi := 1]
data <- clean_and_save( data, tab)

# spf

tab <- "spf"

data <- copy(to_CDM)[concept_id %in% setdiff(concept_sets_of_our_study_drugs, c("A", "B")),]
setnames(data, "person_id","id")
setnames(data, "date","datasped")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[, atc := code]
data[, pezzi := 1]
data <- clean_and_save( data, tab)
