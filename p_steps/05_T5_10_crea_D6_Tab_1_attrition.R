# authors: Sabrina Giometto

# v 0.1 11 Nov 2026


print('CREA D6_Tab_1_attrition')

# assign directories

if (TEST){ 
  testname <- "test_D6_Tab_1"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


# caricamento dati
D5_Tab_1_attrition <- read.csv(paste0(thisdirinput, "D5_Tab_1_attrition.csv"))
D5_Tab_1_attrition <- as.data.table(D5_Tab_1_attrition)


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
# Si popolano le righe

tab_nice <- copy(D5_Tab_1_attrition)

# row 0
row_header_1 <- c()
j            <- -1

# row 1
row_header_1 <- c(row_header_1, "Soggetti con almeno un farmaco nella storia e un periodo di osservazione che tocca il periodo di studio")
j <- j + 1
tab_nice[, cell := as.character(N)]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 2
row_header_1 <- c(row_header_1, "Soggetti registrati in anagrafe sanitaria regionale per almeno un giorno tra il 01/01/2016 e il 31/12/2025
                               con data di nascita e sesso compilati e validi")
j <- descriptive_N_perc(j, "sel_dati_incompleti_")

# row 3
row_header_1 <- c(row_header_1, "Soggetti con un periodo di osservazione")
j <- descriptive_N_perc(j, "sel_no_periodi_osservazione_")

# row 4
row_header_1 <- c(row_header_1, "Soggetti con un periodo di osservazione coincidente con il periodo di studio")
j <- descriptive_N_perc(j, "sel_obs_period_non_toccano_periodo_studio_")

# row 4
row_header_1 <- c(row_header_1, "Soggetti di genere femminile")
j <- descriptive_N_perc(j, "sel_no_maschi_")

# row 5
row_header_1 <- c(row_header_1, "Soggetti di età ≥18 anni alla prima data di dispensazione di uno dei farmaci di interesse durante il periodo di studio (data indice)")
j <- descriptive_N_perc(j, "sel_mai18durante_periodo_studio_")

# row 6
row_header_1 <- c(row_header_1, "Soggetti con almeno una dispensazione del farmaco di interesse nell'istanza")
j <- descriptive_N_perc(j, "sel_no_farmaco_")

# row 7
row_header_1 <- c(row_header_1, "Soggetti con almeno una dispensazione del farmaco di interesse nel periodo di studio")
j <- descriptive_N_perc(j, "sel_no_farmaco_durante_periodo_studio_con_eta_")

# row 8
row_header_1 <- c(row_header_1, "Soggetti con almeno 24 mesi di osservazione disponibili prima della data indice")
j <- descriptive_N_perc(j, "sel_no_lookback_")

# row 8
row_header_1 <- c(row_header_1, "Soggetti con un farmaco non ambiguo")
j <- descriptive_N_perc(j, "sel_farmaco_ambiguo_")

# row 10
row_header_1 <- c(row_header_1, "Totale soggetti inclusi nello studio")
j <- descriptive_N_perc(j, "in_studio_")


#########################################
# filtro celle

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("N", cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# trasposizione

# Prima da wide a long

tab_nice <- melt(tab_nice,
                 id.vars = "N",
                 measure.vars = patterns(
                 cell = "^cell_[0-9]+$"),
                 variable.name = "rownum")

tab_nice[, rownum := as.integer(rownum)]

# setorder(tab_nice, rownum, period, ASL)


# poi da long a wide tenendo rownum come unità di osservazione

tab_nice <- dcast(tab_nice,
                  rownum ~ N,
                  value.var = "value")

# ordinamento
setorder(tab_nice, rownum)


#########################################
# aggiunta intestazione

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# colonne finali

data_cols <- setdiff(names(tab_nice), "row_header")
setcolorder(tab_nice, c("row_header", data_cols))

#########################################
# NAMES

# newnames <- c(
#   pre = "Pre- Nota 100 AIFA (1ge2016-25gen2022)",
#   nota = "Nota 100 AIFA (26gen2022-31lug2025)",
#   modifica = "Modifica Nota 100 AIFA(1ago2025-31dic2025)"
# )
# 
# 
# tab_nice[1] <- lapply(tab_nice[1], function(x) {
#   
#   idx <- x %in% names(newnames)
#   x[idx] <- unname(newnames[x[idx]])
#   x
#   
# })



#########################################
# salvataggio

outputfile <- tab_nice
nameoutput <- "D6_Tab_1_attrition"
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

  