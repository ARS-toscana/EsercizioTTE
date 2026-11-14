############################################################
#                                                          #
#     CREA D4_follow_up                                    #
#                                                          #
#                                                          #
############################################################

# autori: Rosa Gini, Sabrina Giometto, Elena Ferrati

#########################################
# assegna cartelle di input e output

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
# caricamento datasets di input

print('CREATE D4_follow_up')


date_censura_l <- c("data_morte",                      # 1 = morte
                    "data_fine_osservazione_persona",  # 2 = fine osservazione persona
                    "data_fine_finestra_studio",
                    "data_fine_studio")                # 3 = fine finestra studio
                                                       # 4 = fine studio

# POPOLAZIONE

# caricamento popolazione in studio
persons <- readRDS(file.path(thisdirinput,paste0("D3_esp_comp.rds")))

# caricamento periodi osservazione
oss <- readRDS(file.path(thisdirinput,paste0("D3_periodi_osservazione.rds")))

processing <- merge(persons, oss[, .(person_id, end_op)], by = "person_id", all.x = T)
processing <- processing[end_op >= date_first ,]

processing <- processing[, .(person_id,
                          drug,
                          t0 = as.Date(date_first),
                          data_fine_osservazione_persona = as.Date(end_op))]

# FINE STUDIO
processing[, data_fine_studio := study_end_date]

# FINE FINESTRA STUDIO
processing[, data_fine_finestra_studio:=t0+364]

rm(persons)

# OUTCOME
outcomes <- readRDS(file.path(thisdirinput, "D3_outcome.rds"))

outcomes <- outcomes[, .(person_id,
                         morte = as.integer(outcome),
                         data_morte = as.Date(data_outcome))]

processing <- merge(processing, outcomes, by = "person_id", all.x = T)

processing[is.na(morte) | morte != 1L |
             is.na(data_morte) ,
           `:=`(morte = 0L, data_morte = as.Date(NA))]

rm(outcomes)

# CENSURA

# data_censura = prima data tra outcome, fine osservazione e fine studio
processing[, data_censura := pmin(data_morte,
                                  data_fine_osservazione_persona,
                                  data_fine_finestra_studio,
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

# OUTCOME

# creazione outcome se la causa di censura coincide con la morte
processing[, outcome:=as.integer(causa_censura==1)]


# INTERRUZIONE
episodi <- readRDS(file.path(thisdirinput,paste0("D3_episodi_trattamento.rds")))

processing[, episode_start := t0]
# episodi[, episode_start:=as.Date(episode_start)]
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
episodi <- episodi[episode_start >= t0 ,]
episodi <- episodi[drug != esposizione,]

setorder(episodi, person_id, episode_start)
episodi[, n := rowid(person_id) ]

episodi <- episodi[n == 1, ]
episodi <- episodi[, .(person_id, data_switch = episode_start)]

processing <- merge(processing, episodi, by = "person_id", all.x = T)

processing[, switch := as.integer(!is.na(data_switch))]

rm(episodi)

# INTERRUZIONE = minimo tra fine episodio indice e inizio episodio con farmaco diverso da quello indice
processing[, data_interruzione := pmin(data_interruzione, data_switch, na.rm = TRUE)]
           
# INTERRUZIONE E SWITCH PRE-CENSURA
processing[, interruzione_pre_censura:=ifelse(interruzione==1 & data_interruzione < data_censura, 1, 0)]
processing[, switch_pre_censura:=ifelse(switch==1 & data_switch < data_censura, 1, 0)]

# TEMPI
processing[, tempo_censura := as.integer(data_censura - t0 +1)]

for (j in seq_along(date_censura_l)) {
  processing[, (paste0("tempo_causa_", j)) := fifelse(causa_censura == j, tempo_censura, NA_integer_)]
}

# processing[, data_interruzione:=as.Date(data_interruzione)]
# processing[, data_switch:=as.Date(data_switch)]

processing[, tempo_interruzione := fifelse(interruzione_pre_censura == 1L, as.integer(data_interruzione - t0 + 1), NA_integer_)]
processing[, tempo_switch := fifelse(switch_pre_censura == 1L, as.integer(data_switch - t0 + 1), NA_integer_)]
  
#########################################
# pulizia

tokeep <- c("person_id", "t0", "esposizione",
          "interruzione", "data_interruzione",
          "switch", "data_switch",
          "morte", "data_morte",
          "data_fine_osservazione_persona", "data_fine_finestra_studio",
          "data_fine_studio",
          "data_censura", "causa_censura", "outcome", "tempo_censura",
          paste0("tempo_causa_", seq_along(date_censura_l)),
          "interruzione_pre_censura", "switch_pre_censura",
          "tempo_interruzione", "tempo_switch")

processing <- processing[,..tokeep]


#########################################
# salvataggio

outputfile <- processing

nameoutput <- "D4_follow_up"
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
 
