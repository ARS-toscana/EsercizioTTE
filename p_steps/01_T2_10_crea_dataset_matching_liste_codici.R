
# author: Rosa Gini

# v 1.0 12 Nov 2026
# included test

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_datasets_individuati_da_liste_di_codici"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisconcept_sets_of_our_study_drugs <- c("med_iperten", "med_dislip")
  thisconcept_sets_of_our_study_diagnosis <- c("dia_cardioisc","dia_angi")
  thisTheShinISS_CDM_tables <- list()
  thisTheShinISS_CDM_tables[["Diagnosis"]] = c("sdo","exe")
  thisTheShinISS_CDM_tables[["Medicines"]] = c("spf")
  thisrenamecol <- list()
  # TheShinISS_CDM_codvar, TheShinISS_CDM_codvar and TheShinISS_CDM_datevar sono fissati nel file 02_parameters_CDM
  # concept_set_domains, concept_set_codes_our_study, concept_set_codes_our_study_excl sono fissati nel file 03_concept_sets
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
  thisconcept_sets_of_our_study_drugs <- concept_sets_of_our_study_drugs
  thisconcept_sets_of_our_study_diagnosis <- concept_sets_of_our_study_diagnosis
  thisTheShinISS_CDM_tables <- TheShinISS_CDM_tables
  thisrenamecol <- list(DATE=DATE)
}

#APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY THE CODES OF INTEREST


CreateConceptSetDatasets(dataset = thisTheShinISS_CDM_tables,
                         codvar = TheShinISS_CDM_codvar,
                         datevar = TheShinISS_CDM_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = thisrenamecol,
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = thisconcept_sets_of_our_study_drugs,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = thisdirinput,
                         diroutput = thisdiroutput,
                         extension = c("csv")
)


CreateConceptSetDatasets(dataset = thisTheShinISS_CDM_tables,
                         codvar = TheShinISS_CDM_codvar,
                         datevar = TheShinISS_CDM_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = thisrenamecol,
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = thisconcept_sets_of_our_study_diagnosis,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = thisdirinput,
                         diroutput = thisdiroutput,
                         extension = c("csv"))

