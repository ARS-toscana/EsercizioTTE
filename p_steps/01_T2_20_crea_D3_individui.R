# author: Rosa Gini

# v 1.0 26 Aug 2026

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_individui"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# carica dati

processing <- fread(file.path(thisdirinput,"ANAGRAFE_ASSISTITI.csv"))

# assegna formato data

date_cols <- c("data_inizioass", "data_fineass", "datadec", "datanas")

for (datevar in date_cols) {
  print(datevar)
  processing[, (datevar) := ymd(get(datevar))]
}

# tiene info più recente

setorderv(processing, c("id", "data_inizioass"))

processing <- processing[, is_the_most_recent_info := as.integer(seq(.N) == .N), by = id]

processing <- processing[is_the_most_recent_info == 1, ]

# dati mancanti

processing[, dati_incompleti := 0]
processing[is.na(sesso) | (sesso != 1 & sesso != 2), dati_incompleti := 1]
processing[is.na(datanas) | (datanas < ymd("18900101")), dati_incompleti := 1]
processing[!is.na(datadec) & dati_incompleti == 0 & datadec < datanas, dati_incompleti := 1]

# person_id

setnames(processing,old = c("id"),new = c("person_id"))

# gender

processing[dati_incompleti == 0, gender := sesso]

# birth_date

processing[dati_incompleti == 0, birth_date := datanas]

processing[dati_incompleti == 0, death_date := datadec]


################################
# clean

tokeep <- c("person_id", "dati_incompleti","gender", "birth_date","death_date")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_individui"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
