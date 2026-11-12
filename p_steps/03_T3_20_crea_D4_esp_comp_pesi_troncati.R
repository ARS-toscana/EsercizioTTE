############################################################
#                                                          #
#         Create_D4_esp_comp_pesi_troncati                 #
#                                                          #
############################################################

# authors: Sofia Miniati, Paoletti Olga, Rosa Gini, Azzurra Bongianni

# v 1.0 23 Oct 2026

# from CS4

print("CREATE D4_esp_comp_pesi_troncati")

library(data.table)

#########################################
# assign directories and other parameters used in this step

if (TEST) {
  
  testname <- "test_D4_esp_comp_pesi_troncati"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(thisdirinput, "g_output")
  dir.create(thisdiroutput, showWarnings = FALSE)
  
} else {
  
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  
}

#########################################
# load parameters

# settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
# 
# thissubpopulations <- unique(settings[, .(subpop)])
# thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
# 
# deap <- unique(settings$ds)[1]
# 
# subpop <- "all"
# 
# for (subpop in thissubpopulations) {
#   
#   suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
#   print(suffix)
  
#########################################
# load input datasets
  
input <- readRDS(
  file.path(
    thisdirinput,
    paste0("D4_esp_comp_pesi.rds"))
)
  
input <- as.data.table(input)
  
# keep original columns
cols_to_keep <- colnames(input)
  
  # input_troncatura <- fread(
  #   file = file.path(thisdirchoice, "input_troncatura.csv")
  # )
  # 
  # input_troncatura <- input_troncatura[ds == deap, ]
  
thisdirarchive <- file.path(thisdir, "p_parameters", "archive_parameters")
input_troncatura <- fread(file=file.path(thisdirarchive, "input_troncatura.csv"))

#########################################
# check required columns
  
required_cols <- c("person_id", "t0", "exposure", "ps")
  
missing_cols <- setdiff(required_cols, names(input))
  
if (length(missing_cols) > 0) {
  stop(sprintf(
    "The following required columns are missing from input: %s",
    paste(missing_cols, collapse = ", ")
  ))
}
  
#########################################
# Equipoise trimming based on PS overlap region
#
# Subjects outside the PS overlap region:
# truncated_ps = 0
# weight = 0
# truncated_weight = 0
#
# Subjects inside the PS overlap region:
# truncated_ps = original ps
# weight = IPTW recalculated from original ps among remaining subjects
# truncated_weight = recalculated weight truncated at 1st/99th percentile
  
trunc_par_exposed <- input_troncatura[drug == "A"]
  
if (nrow(trunc_par_exposed) == 0) {
  stop("No row found for drug == A in input_troncatura")
}
  
if (nrow(trunc_par_exposed) > 1) {
  stop("More than one row found for drug == A in input_troncatura")
}
  
#########################################
# lower PS trimming threshold, if specified
  
ps_lower_exposed <- NA_real_
  
if (!is.na(trunc_par_exposed$lower_limit)) {
    
  ps_lower_exposed <- quantile(
    input[exposure == 1, ps],
    probs = trunc_par_exposed$lower_limit / 100,
    na.rm = TRUE
  )
}
  
#########################################
# upper PS trimming threshold, if specified
  
ps_upper_exposed <- NA_real_
  
if (!is.na(trunc_par_exposed$upper_limit)) {
    
  ps_upper_exposed <- quantile(
    input[exposure == 1, ps],
    probs = trunc_par_exposed$upper_limit / 100,
    na.rm = TRUE
  )
}
  
#########################################
# apply equipoise trimming
  
input[, in_overlap_tmp := 1L]
  
if (!is.na(ps_lower_exposed)) {
  input[ps < ps_lower_exposed, in_overlap_tmp := 0L]
}
  
if (!is.na(ps_upper_exposed)) {
  input[ps > ps_upper_exposed, in_overlap_tmp := 0L]
}
  
#########################################
# create truncated_ps
# outside overlap: 0
# inside overlap: original ps
  
input[, truncated_ps := fifelse(
  in_overlap_tmp == 1L,
  ps,
  0
)]
  
#########################################
# Calculate IPTW from the remaining subjects' PS
# Existing weight is overwritten.
  
input[, weight := 0]
  
p_treated_trimmed <- mean(
  input[in_overlap_tmp == 1L, exposure == 1],
  na.rm = TRUE
)
  
input[
  in_overlap_tmp == 1L,
  weight := fifelse(
    exposure == 1,
    p_treated_trimmed / ps,
    (1 - p_treated_trimmed) / (1 - ps)
  )
]
  
#########################################
# Truncate resulting IPTW weights at 1st/99th percentile
# among included subjects only
  
input[, truncated_weight := 0]
  
q_weight <- quantile(
  input[in_overlap_tmp == 1L, weight],
  probs = c(0.01, 0.99),
  na.rm = TRUE
)
  
input[
  in_overlap_tmp == 1L,
  truncated_weight := pmin(
    pmax(weight, q_weight[1]),
    q_weight[2]
  )
]
  
#########################################
# cleaning
# keep original columns plus:
# truncated_ps
# weight
# truncated_weight
#
# weight is the newly calculated IPTW among remaining subjects.
  
keep_cols <- unique(c(
  cols_to_keep,
  "truncated_ps",
  "weight",
  "truncated_weight"
))
  
input <- input[, ..keep_cols]
  
#########################################
# save
  
outputfile <- input
  
nameoutput <- "D4_esp_comp_pesi_troncati_trimmed"
  
assign(nameoutput, outputfile)
  
saveRDS(
  outputfile,
  file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
)
