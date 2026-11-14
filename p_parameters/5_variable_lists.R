###################################################################
# DESCRIBE THE VARIABLES
# ###################################################################
# 
var_selection <- c("sel_dati_incompleti",
                   "sel_no_periodi_osservazione",
                   "sel_obs_period_non_toccano_periodo_studio",
                   "sel_no_maschi",
                   "sel_mai18durante_periodo_studio",
                   "sel_no_farmaco",
                   "sel_no_farmaco_durante_periodo_studio_con_eta",
                   "sel_no_lookback",
                   "sel_farmaco_ambiguo",
                   "in_studio")
# 
# # names of variables
# 
# codelists_variable_condition <- c("iperten", "cardioisc", "infart", "arit",
#                                   "angi", "ictus", "tia", "scompcard", "dislip",
#                                   "diab", "renal")
# variables_condition <- paste0("VAR_",codelists_variable_condition)
# 
# codelists_variable_medication <- c("cortic", "antiang", "antitromb", "ipolip", 
#                                    "antidiab", "bifosf")
# variables_medication <- paste0("VAR_",codelists_variable_medication)
# 
# variables <- c(variables_condition, variables_medication)
# 
# # labels of variables
# 
# name_variable <- list()
# for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
#   name_variable[[paste0("VAR_",concept)]] <- name_codelist[[concept]]
# }
# 
# 
# # assign the codelists and time spans to those covariates that are computed via codelists
# 
# codelists_variable <- list()
# for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
#   codelists_variable[[paste0("VAR_",concept)]] <- concept
# }
# timespan <- list()
# for (concept in c(codelists_variable_medication)) {
#   timespan[[paste0("VAR_",concept)]] <- 730
# }