# Esempio Target Trial Emulation

# authors: Rosa Gini, Sabrina Giometto, Elena Ferrati

# v 0.1 22 Oct 2026

# skeleton 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TEST <- F

dirinput <- paste0(thisdir,"/i_input/")

####################
# load parameters
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/5_variable_lists.R"))
source(paste0(thisdir,"/p_parameters/6_parameters_study.R"))
# source(paste0(thisdir,"/p_parameters/7_parameters_postprocessing.R"))


#

# ######################################
# # run scripts
source(paste0(thisdir,"/p_steps/01_T2_10_crea_dataset_matching_liste_codici.R"))
source(paste0(thisdir,"/p_steps/01_T2_20_crea_D3_individui.R"))
source(paste0(thisdir,"/p_steps/01_T2_30_crea_D3_periodi_osservazione.R"))
source(paste0(thisdir,"/p_steps/02_T2_10_crea_D3_selezione_esp_comp.R"))
source(paste0(thisdir,"/p_steps/02_T2_20_crea_D3_episodi_trattamento.R"))
source(paste0(thisdir,"/p_steps/02_T2_30_crea_D3_esp_comp_con_baseline.R"))
source(paste0(thisdir,"/p_steps/02_T2_50_crea_D3_outcomes.R"))
source(paste0(thisdir,"/p_steps/03_T3_10_crea_D4_esp_comp_pesi.R"))
source(paste0(thisdir,"/p_steps/04_T4_10_crea_D5_Tab_1_attrition.R"))
source(paste0(thisdir,"/p_steps/04_T4_20_crea_D5_Tab_2_caratteristiche_baseline.R"))
source(paste0(thisdir,"/p_steps/04_T4_30_crea_D5_Fig_S1_distribuzione_PS.R"))
source(paste0(thisdir,"/p_steps/05_T5_10_crea_D6_Tab_1_attrition.R"))
source(paste0(thisdir,"/p_steps/05_T5_20_crea_D6_Tab_2_caratteristiche_baseline.R"))
source(paste0(thisdir,"/p_steps/05_T5_30_crea_D6_Fig_S1_distribuzione_PS.R"))

################### FERMARSI QUI E RISPONDERE AL PRIMO GRUPPO DI DOMANDE
################### COMPILARE IL FILE i_input_decision/input_troncatura.csv E POI PROSEGUIRE

source(paste0(thisdir,"/p_steps/03_T3_20_crea_D4_esp_comp_pesi_troncati.R"))
source(paste0(thisdir,"/p_steps/03_T3_30_create_D4_follow_up.R"))
source(paste0(thisdir,"/p_steps/03_T3_40_crea_D4_dataset_analitico_IT_HR_RMST.R"))
source(paste0(thisdir,"/p_steps/04_T4_40_crea_D5_Tab_3_asd_prima_dopo_pesi.R"))
source(paste0(thisdir,"/p_steps/04_T4_50_crea_D5_Tab_4_follow_up_time_e_cause_uscita.R"))
source(paste0(thisdir,"/p_steps/04_T4_60_crea_D5_RisultatiIR_HR.R"))
source(paste0(thisdir,"/p_steps/04_T4_70_crea_D5_RisultatiRMST.R"))
source(paste0(thisdir,"/p_steps/05_T5_40_crea_D6_Tab_3_asd_prima_dopo_pesi.R"))
source(paste0(thisdir,"/p_steps/05_T5_50_crea_D6_Tab_4_follow_up_time_e_cause_uscita.R"))
source(paste0(thisdir,"/p_steps/05_T5_60_crea_D6_Tab_5_risultati.R"))

