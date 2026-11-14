############################################################
#                                                          #
####   CREATE D3_outcome                                   #
#                                                          #
#                                                          #
############################################################

# author: Rosa Gini, Sabrina Giometto

# 1.0 14 Nov 2026

#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_30_D3_OUTCOMES"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirexp <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # thisconcept_ids <- c("D_MAJORGIBLEED_AESI")
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  
}

#########################################
# caricamento dati input

print('CREA D3_outcome')

persons <- readRDS(file.path(thisdirinput, "D3_individui.rds"))

processing <- readRDS(file.path(thisdirinput, "D3_esp_comp.rds"))


# data <- data.table(
#   person_id = character(),
#   date = as.Date(character()),
#   meaning_renamed = character(),
#   concept_id = character()
# )
# 
# for (conc in thisconcept_ids) {
#   fileparquet <- file.path(thisdirinput,paste0(conc,"_dataset",".parquet"))
#   if (file.exists(fileparquet)) {
#     # dt <- as.data.table(as.data.frame(polars::pl$read_parquet(fileparquet)))
#     if (thisdatasource != "SIDIAP") {
#       dt <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0(conc,"_dataset",".parquet")))))
#     } else {
#       dt <- as.data.table(arrow::read_parquet(file.path(thisdirinput,paste0(conc,"_dataset",".parquet"))))
#     }
#     dt[, concept_id := conc]
#     dt <- dt[,.(person_id,date,meaning_renamed,concept_id)]
#     data <- rbind(data,dt, fill = T)
#   }
#   
# } 

data <- merge(persons, processing[, .(person_id, date_first)], by = "person_id", all = F)
data <- data[, .(data_outcome = min(death_date)), by = c("person_id", "date_first")]
data[, outcome:=ifelse(!is.na(data_outcome), 1, 0)]


processing <- data

#########################################
# salvataggio

outputfile <- processing

nameoutput <- "D3_outcome.rds"

assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutput))

