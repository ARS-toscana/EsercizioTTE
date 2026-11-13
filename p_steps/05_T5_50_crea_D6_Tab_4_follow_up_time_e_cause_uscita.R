############################################################
#                                                          #
####         D6_Tab_4_follow_up_time_e_cause_uscita        #
#                                                          #
############################################################

# autori: Rosa Gini, Sabrina Giometto

# v 1.0 12 Nov 2026


print('CREA D6_Tab_4_follow_up_time_e_cause_uscita')

#########################################
# assegnazione cartelle

if (TEST){ 
  testname <- "test_D6_Tab_4"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


#########################################
# caricamento dati input
    
temp <- readRDS(file.path(thisdirinput, "D5_Tab_4_follow_up_time_e_cause_uscita.rds"))


# helper

descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar,"N_")
  varP <- paste0(covar,"perc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_continuous <- function(j, covar, minmax = 1){
  # j <- j + 1
  # varM <- paste0(covar,"_mean_")
  # varSD <- paste0(covar,"_sd_")
  # tab_nice[, cell := paste0(get(varM), " (",get(varSD),")")]
  # setnames(tab_nice, "cell", paste0("cell_",j))
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  # j <- j + 1
  # varN <- paste0(covar,"_missingN_")
  # varP <- paste0(covar,"_missingperc_")
  # tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  # setnames(tab_nice, "cell", paste0("cell_",j))
  if (minmax == 1) {
    j <- j + 1
    varMin <- paste0(covar,"_min_")
    varMax <- paste0(covar,"_max_")
    tab_nice[, cell := paste0(get(varMin),"-",get(varMax))]
    setnames(tab_nice, "cell", paste0("cell_",j)) 
  }
  return(j)
}


add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

# etichette cause di censura

causelab <- list()

  causelab[["1"]] <- "Morte"
  causelab[["2"]]  <- "Fine osservazione persona"
  causelab[["3"]]  <- "Fine finestra studio"
  causelab[["4"]]  <- "Fine studio"

causes_of_end_fup <- c(1:4)

#########################################
# celle

tab_nice <- copy(temp)

row_header_1 <- c()
j <- 0

# row 0

row_header_1 = c(row_header_1,
                 ""
)

tab_nice[ stratum == 1, cell := "A"]
tab_nice[ stratum == 0, cell := "B"]
tab_nice[ stratum == 99, cell := "Totale"]
setnames(tab_nice, "cell", paste0("cell_",j))

j <- j + 1


# row 1

row_header_1 = c(row_header_1,                  
                 "N" 
)

tab_nice[, cell := N_]
setnames(tab_nice, "cell", paste0("cell_",j))

  
  # rows 2-4
  
  row_header_1 <- c(row_header_1,
                    "Giorni-persona di follow-up",
                    "Mediana (Q1 - Q3)",
                    "Min - Max")
  
  j <- add_empty_row(j)
  j <- descriptive_continuous(j, "tempo_censura")
  
  # Cause di fine folluw-up
  
  row_header_1 <- c(row_header_1, "Cause di fine folluw-up")
  
  j <- add_empty_row(j)
  
  for (cause in causes_of_end_fup) {
    row_header_1 <- c(row_header_1,
                      paste0(causelab[[cause]], ": N (%)"),
                      paste0("         ", " Tempo all'evento, Mediana (Q1 - Q3)")
    )
    
    j <- descriptive_N_perc(j, paste0("causa_censura_",cause,"_"))
    j <- descriptive_continuous(j, paste0("tempo_causa_",cause,""), minmax = 0)
  }  
  
  # Eventi intercorrenti che non causano fine del follow-up
  row_header_1 <- c(row_header_1,
                    "Eventi intercorrenti che non causano fine del follow-up")
  
  j <- add_empty_row(j)
  
  # interruzione
      row_header_1 <- c(row_header_1,
                        "Interruzione: N (%)",
                        "Tempo all'evento, Mediana (Q1 - Q3)")
      
      j <- descriptive_N_perc(j, "interruzione_pre_censura_")
      j <- descriptive_continuous(j, "tempo_interruzione", minmax = 0)
    
      # switch
      row_header_1 <- c(row_header_1,
                        "Switch: N (%)",
                        "Tempo all'evento, Mediana (Q1 - Q3)")
      
      j <- descriptive_N_perc(j, "switch_pre_censura_")
      j <- descriptive_continuous(j, "tempo_switch", minmax = 0)
  

  # View(tab_nice[, .SD, .SDcols = patterns("^cell_")])

#########################################
# filtro


cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("stratum", cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# trasposizioni

# trasposizione tab_nice da wide a long 

tab_nice <- melt(
  tab_nice,
  id.vars = c("stratum"),
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, stratum)


# trasposizione da long a wide tenendo rownum come UoO

tab_nice <- dcast(
  tab_nice,
  rownum ~ stratum,
  value.var = "value"
)


# ordinamento
setorder(tab_nice, rownum)

#########################################
# aggiunta riga di intestazione
#

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# colonne finali

colorder <- c("row_header",setdiff(names(tab_nice),"row_header"))

tab_nice <- tab_nice[, ..colorder]


# #########################################
# salvataggio

outputfile <- tab_nice
nameoutput <- "D6_Tab_4_follow_up_time_e_cause_uscita"
assign(nameoutput, outputfile)
# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# html
# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))


