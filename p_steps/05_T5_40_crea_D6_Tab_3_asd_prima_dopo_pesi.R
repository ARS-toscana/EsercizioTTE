############################################################
#                                                          #
####        Crea D6_Tab_3_asd_prima_dopo_pesi              #
#                                                          #
############################################################

# authors: Rosa Gini, Sabrina Giometto

# v 1.0 12 Nov 2026


print('CREA D6_Tab_3_asd_prima_dopo_pesi')

#########################################
# assegnazione cartelle

if (TEST){ 
  testname <- "test_D6_Tab_3"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
    thisdirinput <- direxp
    thisdiroutput <- direxp
}

#########################################
# caricamento dati
thisdirarchive <- file.path(thisdir, "p_parameters", "archive_parameters")

# lista covariate
parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive, "codebooks", "D4_esp_comp_baseline_pesi.xlsx"), 1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(value)])

# parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))
# 
# for (par in parameter_list) {
#   temp <- unname(unlist(unique(parameters_this_step[parameter == par,.(get("values"))])))
#   assign(par, temp)
# }
# 
# thiscovariates_bin <- covar_bin
# thiscovariates_cat <- covar_cat
# thiscovariates_cont <- covar_cont


#########################################
# caricamento dati

input <- readRDS(file.path(thisdirinput, "D5_Tab_3_asd_prima_dopo_pesi.rds"))
tab_nice <- input
      
##################
# helpers

add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar,"N_")
  varP <- paste0(covar,"perc_")
  tab_nice[stratum != "asd", cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_continuous <- function(j, covar){
  j <- j + 1
  varM <- paste0(covar,"_mean_")
  varSD <- paste0(covar,"_sd_")
  tab_nice[stratum != "asd", cell := paste0(get(varM), " (",get(varSD),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[stratum != "asd", cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  
  return(j)
}

descriptive_median_q1q3 <- function(j, covar) {
  
  j <- j + 1
  varM <- paste0(covar,"_median")
  varQ1 <- paste0(covar,"_q1")
  varQ3 <- paste0(covar,"_q3")
  tab_nice[, cell := paste0(get(varM), " (", get(varQ1), " - ",
                            get(varQ3), ")")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  return(j)
}

add_asd <- function(j, covar){
  varname <- paste0("cell_",j)
  varinput <- paste0(covar,"_asd_")
  tab_nice[stratum == "asd", (varname) := get(varinput)]
}

##########################################
# creazione tabelle

#########################################
# si popolano le righe

#########################################
# add rows for ASD

asd_cols <- grep("_asd_", names(tab_nice), value = TRUE)

asd <- copy(tab_nice)[1, ..asd_cols]
asd[, stratum := "asd"]
asd[, exposure := 98]

tab_nice <- rbind(tab_nice, asd, fill = T)

#########################################
# put in long format

# tab_nice <- melt(
#   tab_nice,
#   id.vars = c("name_ds_subpop","ds_subpop","stratum","exposure"),
#   variable.name = "metric",
#   value.name = "value"
# )
# 
# # extract BF/AF
# 
# tab_nice[, W := sub(".*_(BF|AF)$", "\\1", metric)]
# tab_nice[, metric := sub("_(BF|AF)$", "_", metric)]
# 
# #########################################
# # reshape
# 
# tab_nice <- dcast(
#   tab_nice,
#   name_ds_subpop + ds_subpop + stratum + exposure + W ~ metric,
#   value.var = "value"
# )
# 
# tab_nice[, W := factor(W, levels = c("BF", "AF"))]
# tab_nice[, stratum := factor(stratum, levels = c("exposed", "comparators", "asd"))]
# 
# setorder(tab_nice, name_ds_subpop, W, stratum)

id_vars <- c("stratum", "exposure")

# list columns ending in _BF or _AF
bf_af_cols <- grep("_(BF|AF)$", names(tab_nice), value = TRUE)

# leave out of this reshape the other columns
other_cols <- setdiff(names(tab_nice), c(id_vars, bf_af_cols))

print(other_cols)  # check

# make long columns BF/AF
long <- melt(
  tab_nice,
  id.vars = id_vars,
  measure.vars = bf_af_cols,
  variable.name = "metric",
  value.name = "value"
)

# split BF/AF
long[, W := sub(".*_(BF|AF)$", "\\1", metric)]
long[, metric := sub("_(BF|AF)$", "_", metric)]

# make wide on metrics
tab_long <- dcast(long, stratum + exposure + W ~ metric, value.var = "value")

tab_long[, W := factor(W, levels = c("BF", "AF"))]

tab_nice <- tab_long

setorder(tab_nice, W, stratum)


############################################
# row 2 - stratum

j <- 1

row_header_1 <- c("")

j <- j + 1

tab_nice[stratum == "exposed", cell := "Esposti"]
tab_nice[stratum == "comparators", cell := "Comparatori"]
tab_nice[stratum == "asd", cell := "Differenza assoluta standardizzata"]

setnames(tab_nice, "cell", paste0("cell_",j))

############################################
# row 3 - N

row_header_1 <- c(row_header_1, "Totale nuovi utilizzatori")

j <- j + 1

tab_nice[, cell := N_]
setnames(tab_nice, "cell", paste0("cell_",j))

############################################
# AGE - categorical

# row_header_1 <- c(row_header_1, "Età alla data indice,", "  mediana (Q1 - Q3)")
# j <- add_empty_row(j)
# j <- descriptive_median_q1q3(j, "age")
# 
# add_asd(j, "age")

# cancro metastatico
row_header_1 <- c(row_header_1, "Cancro metastatico")
j <- descriptive_N_perc(j, "metastatic_")

add_asd(j - 1, "metastatic")


############################################
# Patologie concomitanti

row_header_1 <- c(row_header_1, "Patologie concomitanti")
j <- add_empty_row(j)

############################################
# Ipertensione
row_header_1 <- c(row_header_1, "Ipertensione")
j <- descriptive_N_perc(j, "iperten_")

add_asd(j, "iperten")

############################################
# Cardiopatia ischemica
row_header_1 <- c(row_header_1, "Cardiopatia ischemica")
j <- descriptive_N_perc(j, "cardioisc_")

add_asd(j, "iperten")

############################################
# Infarto miocardico
row_header_1 <- c(row_header_1, "Infarto miocardico")
j <- descriptive_N_perc(j, "infart_")

add_asd(j, "infart")

############################################
# Aritmie
row_header_1 <- c(row_header_1, "Aritmie")
j <- descriptive_N_perc(j, "arit_")

add_asd(j, "arit")

############################################
# Aritmie
row_header_1 <- c(row_header_1, "Angina")
j <- descriptive_N_perc(j, "angi_")

add_asd(j, "angi")

############################################
# Ictus ischemico o emorragico
row_header_1 <- c(row_header_1, "Ictus ischemico o emorragico")
j <- descriptive_N_perc(j, "ictus_")

add_asd(j, "ictus")

############################################
# Attacco ischemico transitorio (TIA)
row_header_1 <- c(row_header_1, "Attacco ischemico transitorio (TIA)")
j <- descriptive_N_perc(j, "tia_")

add_asd(j, "tia")

############################################
# Scompenso cardiaco
row_header_1 <- c(row_header_1, "Scompenso cardiaco")
j <- descriptive_N_perc(j, "scompcard_")

add_asd(j, "scompcard")

############################################
# Dislipidemia
row_header_1 <- c(row_header_1, "Dislipidemia")
j <- descriptive_N_perc(j, "dislip_")

add_asd(j, "dislip")

############################################
# Diabete mellito
row_header_1 <- c(row_header_1, "Diabete mellito")
j <- descriptive_N_perc(j, "diab_")

add_asd(j, "diab")

############################################
# Insufficienza renale cronica
row_header_1 <- c(row_header_1, "Insufficienza renale cronica")
j <- descriptive_N_perc(j, "renal_") 

add_asd(j, "renal")

############################################
# Farmaci concomitanti
row_header_1 <- c(row_header_1, "Farmaci concomitanti")
j <- add_empty_row(j)

############################################
# Corticosteroidi
row_header_1 <- c(row_header_1, "Corticosteroidi")
j <- descriptive_N_perc(j, "cortic_")

add_asd(j, "cortic")

############################################
# Antianginosi
row_header_1 <- c(row_header_1, "Antianginosi")
j <- descriptive_N_perc(j, "antiang_") 

add_asd(j, "antiang")

############################################
# Antitrombotici
row_header_1 <- c(row_header_1, "Antitrombotici")
j <- descriptive_N_perc(j, "antitromb_") 

add_asd(j, "antitromb")

############################################
# Ipolipemizzanti
row_header_1 <- c(row_header_1, "Ipolipemizzanti")
j <- descriptive_N_perc(j, "ipolip_")

add_asd(j, "ipolip")

############################################
# Antidiabetici
row_header_1 <- c(row_header_1, "Antidiabetici")
j <- descriptive_N_perc(j, "antidiab_")

add_asd(j, "antidiab")

############################################
# Bifosfonati
row_header_1 <- c(row_header_1, "Bifosfonati")
j <- descriptive_N_perc(j, "bifosf_")

add_asd(j, "bifosf")

############################################
# Trattamenti concomitanti
row_header_1 <- c(row_header_1, "Trattamenti concomitanti")
j <- descriptive_N_perc(j, "conc_treat_")

add_asd(j, "conc_treat")



#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)

tokeep <- c("stratum","W",cell_cols)

tab_nice <- tab_nice[, ..tokeep]

#########################################
# RESHAPE CELLS

tab_nice <- melt(
  tab_nice,
  id.vars = c("stratum","W"),
  measure.vars = patterns(cell = "^cell_[0-9]+$"),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, stratum, W)

tab_nice <- dcast(
  tab_nice,
  rownum ~ stratum + W,
  value.var = "value"
)

setorder(tab_nice, rownum)

#########################################
# ADD ROW HEADER

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# ORDER COLUMNS

ordered_cols <- c("row_header",
                  "exposed_BF",
                  "comparators_BF",
                  "asd_BF",
                  "exposed_AF",
                  "comparators_AF",
                  "asd_AF")

setcolorder(tab_nice, ordered_cols)

#########################################
# SAVE
outputfile <- tab_nice

nameoutput <- "D6_Tab_3_asd_prima_dopo_pesi"

assign(nameoutput, outputfile)

saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")), col.names = FALSE)
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")), col_names = FALSE)

# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))

doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = FALSE) %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
