
#### D6_Table_1 ----


# authors: Sabrina Giometto

# v 0.1 28 Jul 2026 Creation of D6 started
# v 0.2 10 Set 2026 Creation of D6 completed
# v 0.3 15 Set 2026 Changes according to the latest version of the protocol 
#                   (discont e switch 12 e 24 + restarter)


print('CREA D6_Tab_2')


# assign directories

if (TEST){ 
  testname <- "test_D6_Tab_2"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


# caricamento dati
D5 <- read.csv(paste0(thisdirinput, "D5_Tab_2_caratteristiche_baseline.csv"))
D5 <- as.data.table(D5)


# helpers

add_empty_row <- function(j){

  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {

  j <- j + 1
  varN <- paste0(covar,"N")
  varP <- paste0(covar,"p")
  tab_nice[, cell := paste0(
    formatC(get(varN), format = "f", digits = 0, big.mark = ".",
            decimal.mark = ","), " (",
    formatC(get(varP), format = "f", digits = 1, big.mark = ".",
            decimal.mark = ","),"%)"
  )]
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



#########################################
# si popolano le righe
  
tab_nice <- copy(D5)

# row 0
row_header_1 <- c()
j            <- -1

# row 4
row_header_1 <- c(row_header_1, "Totale nuovi utilizzatori")
j <- j + 1
tab_nice[, cell := as.character(N)]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 5-6
row_header_1 <- c(row_header_1, "Età alla data indice,", "  mediana (Q1 - Q3)")
j <- add_empty_row(j)
j <- descriptive_median_q1q3(j, "age")

# row 9
row_header_1 <- c(row_header_1, "Cancro metastatico")
j <- descriptive_N_perc(j, "metastatic_")

# row 8
row_header_1 <- c(row_header_1, "Patologie concomitanti")
j <- add_empty_row(j)

# row 9
row_header_1 <- c(row_header_1, "Ipertensione")
j <- descriptive_N_perc(j, "iperten_")

# row 10
row_header_1 <- c(row_header_1, "Cardiopatia ischemica")
j <- descriptive_N_perc(j, "cardioisc_")

# row 11
row_header_1 <- c(row_header_1, "Infarto miocardico")
j <- descriptive_N_perc(j, "infart_")

# row 12
row_header_1 <- c(row_header_1, "Aritmie")
j <- descriptive_N_perc(j, "arit_")

# row 13
row_header_1 <- c(row_header_1, "Angina")
j <- descriptive_N_perc(j, "angi_")

# row 14
row_header_1 <- c(row_header_1, "Ictus ischemico o emorragico")
j <- descriptive_N_perc(j, "ictus_")

# row 15
row_header_1 <- c(row_header_1, "Attacco ischemico transitorio (TIA)")
j <- descriptive_N_perc(j, "tia_")

# row 16
row_header_1 <- c(row_header_1, "Scompenso cardiaco")
j <- descriptive_N_perc(j, "scompcard_")

# row 17
row_header_1 <- c(row_header_1, "Dislipidemia")
j <- descriptive_N_perc(j, "dislip_")

# row 18
row_header_1 <- c(row_header_1, "Diabete mellito")
j <- descriptive_N_perc(j, "diab_")

# row 19
row_header_1 <- c(row_header_1, "Insufficienza renale cronica")
j <- descriptive_N_perc(j, "renal_") 

# row 21
row_header_1 <- c(row_header_1, "Farmaci concomitanti")
j <- add_empty_row(j)

# row 22
row_header_1 <- c(row_header_1, "Corticosteroidi")
j <- descriptive_N_perc(j, "cortic_")

# row 20
row_header_1 <- c(row_header_1, "Antianginosi")
j <- descriptive_N_perc(j, "antiang_") 

# row 23
row_header_1 <- c(row_header_1, "Antitrombotici")
j <- descriptive_N_perc(j, "antitromb_") 

# row 24
row_header_1 <- c(row_header_1, "Ipolipemizzanti")
j <- descriptive_N_perc(j, "ipolip_")

# row 25
row_header_1 <- c(row_header_1, "Antidiabetici")
j <- descriptive_N_perc(j, "antidiab_")

# row 26
row_header_1 <- c(row_header_1, "Bifosfonati")
j <- descriptive_N_perc(j, "bifosf_")

# row 20
row_header_1 <- c(row_header_1, "Trattamenti concomitanti")
j <- descriptive_N_perc(j, "conc_treat_")

#########################################
# filtro celle

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c(cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# trasposizione

# da wide a long

tab_nice <- melt(
  tab_nice,
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]


# ordinamento
setorder(tab_nice, rownum)


#########################################
# aggiunta intestazioni

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# ordine colonne

setcolorder(tab_nice, c("row_header", "value"))


#########################################
# salvataggio

outputfile <- tab_nice
nameoutput <- "D6_Tab_2_caratteristiche_baseline"
assign(nameoutput, outputfile)

# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput, ".xlsx")))
# html
# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,"_", k,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = F) %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput, ".docx")))

