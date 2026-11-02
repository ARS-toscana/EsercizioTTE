############################################################
#                                                          #
#     CREATE D4_follow_up                                  #
#                                                          #
#                                                          #
############################################################

# author: Rosa Gini, Sabrina Giometto, Elena Ferrati

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_follow_up"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

#########################################
# load input datasets

print('CREATE D4_follow_up')


date_censura_l <- c("data_outcome",                    # 1 = outcome (decesso)
                    "data_fine_osservazione_persona",  # 2 = fine osservazione della persona
                    "data_fine_studio")                # 3 = fine studio

# POPOLAZIONE

# load study population
persons <- readRDS(file.path(thisdirinput,paste0("D3_esp_comp.rds")))

processing <- persons[, .(person_id,
                          drug,
                          t0 = as.Date(date_first),
                          data_fine_osservazione_persona = as.Date(op_end))]

processing[, data_fine_studio := study_end_date]

rm(persons)

# OUTCOME
outcomes <- readRDS(file.path(thisdirinput, "D3_outcome.rds"))

outcomes <- outcomes[, .(person_id,
                         outcome = as.integer(outcome),
                         data_outcome = as.Date(data_outcome))]

processing <- merge(processing, outcomes, by = "person_id", all.x = T)

processing[is.na(outcome) | outcome != 1L |
             is.na(data_outcome) ,
           `:=`(outcome = 0L, data_outcome = as.Date(NA))]

rm(outcomes)

# CENSURA

# data_censura = prima data tra outcome, fine osservazione e fine studio

processing[, data_censura := pmin(data_outcome,
                                  data_fine_osservazione_persona,
                                  data_fine_studio,
                                  na.rm = TRUE)]

processing[, causa_censura := NA_integer_]

for (j in seq_along(date_censura_l)) {
  col <- date_censura_l[j]
  processing[is.na(causa_censura) & !is.na(get(col)) & get(col) == data_censura,
             causa_censura := j]
}

processing[, tempo_censura := as.integer(data_censura - t0)]

if (processing[tempo_censura <= 0, .N] > 0) {
  warning(paste0(processing[tempo_censura <= 0, .N], " persone con tempo_censura <= 0"))
}


# INTERRUZIONE
episodi <- readRDS(file.path(thisdirinput,paste0("D3_episodi_trattamento.rds")))

processing[, episode_start := t0]
episodi <- merge(episodi[,.(person_id, episode_start, episode_end)], processing, by = c("person_id","episode_start"), all.x = T )

setorder(episodi, person_id, episode_end)

episodi[, n := rowid(person_id) ]

episodi <- episodi[n == 1, ]
episodi <- episodi[, .(person_id, data_interruzione = episode_end)]

processing <- merge(processing, episodi, by = "person_id", all.x = T)

processing[, interruzione := as.integer(!is.na(data_interruzione))]

processing[ , episode_start := NULL]

rm(episodi)


# SWITCH
episodi <- readRDS(file.path(thisdirinput,paste0("D3_episodi_trattamento.rds")))

setnames(processing, "drug", "esposizione")
episodi <- merge(episodi, processing[,.(person_id, t0, esposizione, data_censura)], by = "person_id", all = F )
episodi <- episodi[episode_start > t0 & episode_start <= data_censura,]
episodi <- episodi[drug != esposizione,]

setorder(episodi, person_id, episode_start)
episodi[, n := rowid(person_id) ]

episodi <- episodi[n == 1, ]
episodi <- episodi[, .(person_id, data_switch = episode_start)]

processing <- merge(processing, episodi, by = "person_id", all.x = T)

processing[, switch := as.integer(!is.na(data_switch))]

rm(episodi)


# TEMPI
processing[, tempo_outcome := fifelse(outcome == 1L, as.integer(data_outcome - t0), NA_integer_)]

for (j in seq_along(date_censura_l)) {
  processing[, (paste0("tempo_causa_", j)) := fifelse(causa_censura == j, tempo_censura, NA_integer_)]
}

processing[, tempo_interruzione := fifelse(interruzione == 1L, as.integer(data_interruzione - t0), NA_integer_)]
processing[, tempo_switch := fifelse(switch == 1L, as.integer(data_switch - t0), NA_integer_)]
  
 #########################################
 # cleaning
 
tokeep <- c("person_id", "t0", "esposizione",
            "interruzione", "data_interruzione",
            "switch", "data_switch",
            "outcome", "data_outcome",
            "data_fine_osservazione_persona", "data_fine_studio",
            "data_censura", "causa_censura", "tempo_censura",
            "tempo_outcome",
            paste0("tempo_causa_", seq_along(cause_censura)),
            "tempo_interruzione", "tempo_switch")

 processing <- processing[,..tokeep]
 
 
 #########################################
 # save
 
 outputfile <- processing
 
 nameoutput <- "D4_follow_up"
 assign(nameoutput, outputfile)
 saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
   
