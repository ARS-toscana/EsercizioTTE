# author: Rosa Gini

# v 1.0 13 Nov 2026

#########################################

if (TEST){
  testname <- "test_D3_selezione_esp_comp"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}


# carica dati

for (i in c("A","B")) {
  medicine <- as.data.table(get(load(file.path(thisdirinput, paste0(i,".RData")))[[1]]))
  setnames(medicine, "id", "person_id")
  
  medicine <- medicine[,.(person_id, DATE)]
  medicine[, drug := i]
  assign(i, medicine)
}


persons <- readRDS(file = file.path(thisdirinput, "D3_individui.rds"))
obsperiods <- readRDS(file = file.path(thisdirinput, "D3_periodi_osservazione.rds"))
obsperiods <- obsperiods[periodo_in_studio_individuale == 1,]

################################
# criteri di selezione

processing <- persons


############################
# selection variables

processing[, sel := 0]

# sel_dati_incompleti

thissel <- "sel_dati_incompleti" 

processing[ , (thissel) := fifelse( sel == 1 | dati_incompleti == 1, 1, 0) ]

processing[ , sel := fifelse(get(thissel) == 1 , 1, 0) ]

#####
# sel_no_periodi_osservazione 	sel_obs_period_non_toccano_periodo_studio	

obsenriched <- merge(processing, obsperiods, by = "person_id", all.x = T)

obsenriched[, date_18th_birthday := birth_date %m+% years(18)]

thissel <- "sel_no_periodi_osservazione" 

temp <- unique(obsenriched[is.na(start_op),.(person_id)])

temp[, (thissel) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | get(thissel) == 1, 1, 0, 0) ]

processing[ , sel := fifelse(sel == 1 |get(thissel) == 1 , 1, 0) ]

thissel <- "sel_obs_period_non_toccano_periodo_studio"

temp <- unique(obsenriched[start_op <= study_end_date & end_op >= study_start_date,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  

thissel <- "sel_no_maschi"

processing[ , (thissel) := fifelse( sel == 1 | is.na(gender) | gender != 1, 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  


thissel <- "sel_mai18durante_periodo_studio"

temp <- obsenriched[start_op <= study_end_date & end_op >= study_start_date & date_18th_birthday <= end_op & date_18th_birthday <= study_end_date ,]

temp <- unique(temp[,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  


# sel_no_farmaco

thissel <- "sel_no_farmaco"

medenriched <- merge(processing[sel == 0,], rbind(A,B), by = "person_id", all = F)

temp <- unique(medenriched[,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]


# date_first	sel_no_farmaco_durante_periodo_studio_con_eta	

temp <- medenriched[,.(date_first = min(DATE)), by = "person_id"]

medenriched <- merge(medenriched, temp, by = "person_id", all.x = T)

processing <- merge(processing, temp, by = "person_id", all.x = T)

thissel <- "sel_no_farmaco_durante_periodo_studio_con_eta"

obsenriched <- merge(obsenriched, temp, by = "person_id", all.x = T)

temp <- obsenriched[date_first >= date_18th_birthday & date_first >= start_op & date_first <= end_op,]

temp <- unique(temp[,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]

# sel_no_lookback 	

thissel <- "sel_no_lookback"

temp <- merge(obsenriched, processing[sel == 0,.(person_id)], all = F)

temp <- temp[ date_first >= start_op + 730 & date_first <= end_op ,]

temp <- unique(temp[,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]

# sel_farmaco_ambiguo	drug

thissel <- "sel_farmaco_ambiguo"

temp <- medenriched[DATE == date_first,]

temp <- unique(temp[,.(person_id,drug)])

temp[, N := .N, by = "person_id"]

temp <- temp[N == 1,.(person_id, drug)]

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 1) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]

####################
# final variables

# in_studio

processing[, in_studio := fifelse(sel == 0, 1, 0)]

# age

processing[, age := age_fast(birth_date, date_first)]


####################
# clean and save

tokeep <- c("person_id", "sel_dati_incompleti", "sel_no_periodi_osservazione", "sel_obs_period_non_toccano_periodo_studio", "sel_no_maschi", "sel_mai18durante_periodo_studio", "sel_no_farmaco", "date_first", "sel_no_farmaco_durante_periodo_studio_con_eta", "sel_no_lookback", "sel_farmaco_ambiguo", "drug", "in_studio", "birth_date", "age")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_selezione_esp_comp.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

# study population

processing <- processing[in_studio == 1,]

tokeep <- c("person_id",  "date_first",  "drug", "birth_date", "age")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_esp_comp.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

