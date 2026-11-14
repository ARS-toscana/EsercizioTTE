############################################################
#                                                          #
#      CREATE D4_dataset_analitico_IR_HR_RMST              #
#                                                          #
#                                                          #
############################################################

# author: Rosa Gini, Sabrina Giometto, Elena Ferrati

# v 0.1 02 Nov 2026

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_dataset_analitico_IR_HR_RMST"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

#########################################
# load input datasets

print('CREATE D4_dataset_analitico_IR_HR_RMST')

D4_esp_comp_pesi_troncati_trimmed <- readRDS(file.path(thisdirinput,paste0("D4_esp_comp_pesi_troncati_trimmed.rds")))

#################################
# start

processing <- readRDS(file.path(thisdirinput, "D4_follow_up.rds"))


# evento
#processing[, evento := fifelse(causa_censura==1 & data_outcome <= data_censura, 1, 0)]
processing[, evento:= fifelse(causa_censura==1, 1, 0)]

# fup
processing[, tempo_evento := as.numeric(tempo_censura)]

# merge con coorte in studio con pesi
analytical_dataset <- merge(processing[, esposizione:=NULL], D4_esp_comp_pesi_troncati_trimmed[, .(person_id, exposure, weight, truncated_weight)], by = "person_id")

#########################################
# cleaning
 
tokeep <-  c("person_id", "exposure", "evento", "tempo_evento", "weight", "truncated_weight") 
 
analytical_dataset <- analytical_dataset[,..tokeep]
 
 
#########################################
# save

outputfile <- analytical_dataset

nameoutput <- paste0("D4_dataset_analitico_IR_HR_RMST")

assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
   