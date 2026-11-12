# author: Rosa Gini

# v 1.0 12 Nov 2026

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_periodi_di_osservazione"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# carica input

ana <- fread(file.path(thisdirinput,"ANAGRAFE_ASSISTITI.csv"))

date_cols <- c("data_inizioass", "data_fineass", "datadec", "datanas")

for (datevar in date_cols) {
  print(datevar)
  ana[, (datevar) := ymd(get(datevar))]
}

# rimuovi periodi assurdi

ana <- ana[data_inizioass <= data_fineass,]

# unisci periodi contigui

processing <- CreateSpells(
  dataset = ana,
  id = "id" ,
  start_date = "data_inizioass",
  end_date = "data_fineass",
  gap_allowed = 365
)

rm(ana)

# sovrapposto_a_periodo_studio

processing[ , sovrapposto_a_periodo_studio := fifelse(study_start_date <= exit_spell_category & study_end_date >= entry_spell_category,1,0)]


# periodo_in_studio_individuale

setorderv(processing, c("id", "entry_spell_category"))

processing[, ord := seq_len(.N), by = "id"]
processing[ sovrapposto_a_periodo_studio == 1, minord := min(ord), by = "id"]


processing[ord == minord, periodo_in_studio_individuale := 1]
processing[is.na(periodo_in_studio_individuale), periodo_in_studio_individuale := 0]

# rinomina

setnames(processing, old = c("id","entry_spell_category","exit_spell_category"),new = c("person_id","start_op", "end_op"))


################################
# clean

tokeep <- c("person_id", "start_op", "end_op", "sovrapposto_a_periodo_studio", "periodo_in_studio_individuale")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_OBSPERIODS"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
