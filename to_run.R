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

# 
# 
# source(paste0(thisdir,"/p_steps/05_T4_10_Create_D5_attrition.R"))
# source(paste0(thisdir,"/p_steps/06_T5_10_Create_D6_Table_S1.R"))

# source(paste0(thisdir,"/p_steps/05_T4_20_Create_D5_cohort_characteristics.R"))
# source(paste0(thisdir,"/p_steps/06_T5_20_Create_D6_Table_1.R"))

# source(paste0(thisdir,"/p_steps/05_T4_30_Create_D5_adverse_events.R"))
# source(paste0(thisdir,"/p_steps/06_T5_30_Create_D6_Table_2.R"))

# source(paste0(thisdir,"/p_steps/05_T4_40_Create_D5_distribution_type_user.R"))
# source(paste0(thisdir,"/p_steps/06_T5_40_Create_D6_Figure_1.R"))

# source(paste0(thisdir,"/p_steps/05_T4_50_Create_D5_distribution_drug_within_first_line_new_users.R"))
# source(paste0(thisdir,"/p_steps/06_T5_40_Create_D6_Figure_2.R"))

