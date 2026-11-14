############################################################
#                                                          #
####        D6_Tab_5_risultati                             #
#                                                          #
############################################################

# authors: Claudia Bartolini, Sabrina Giometto

# v 1.0 13 Nov 2026


print('CREA D6_Tab_5_risultati')


#########################################
# assign directories

if (TEST){ 
  testname <- "test_D6_Tab_5"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


#########################################
# caricamento dati input

temp <- fread(file=file.path(thisdirinput, "D5_RisultatiIR_HR.csv"))

rmst <- readRDS(file=file.path(thisdirinput, "D5_RisultatiRMST.rds"))
    

# helper

clean_chr <- function(x) {
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  x
}

format_ci <- function(est, lower, upper) {
  
  est   <- clean_chr(est)
  lower <- clean_chr(lower)
  upper <- clean_chr(upper)
  
  result <- rep(NA_character_, length(est))
  
  # Stima e limiti tutti disponibili
  complete <- (
    !is.na(est) &
      !is.na(lower) &
      !is.na(upper)
  )
  
  result[complete] <- paste0(
    est[complete],
    " (",
    lower[complete],
    "–",
    upper[complete],
    ")"
  )
  
  # Se i limiti non sono disponibili, mantiene solo la stima
  estimate_only <- (
    !is.na(est) &
      (is.na(lower) | is.na(upper))
  )
  
  result[estimate_only] <- est[estimate_only]
  
  result
}

first_present <- function(x) {
  
  x <- clean_chr(x)
  x <- x[!is.na(x)]
  
  if (length(x) > 0L) {
    x[1L]
  } else {
    NA_character_
  }
}

extract_value <- function(x, analysis) {
  
  value <- x[analysisRMST == analysis]
  value <- clean_chr(value)
  value <- value[!is.na(value)]
  
  if (length(value) > 0L) {
    value[1L]
  } else {
    NA_character_
  }
}

# creazione tab_nice
tab_nice <- copy(temp)

DT <-tab_nice[,
              treatment := fcase(
                exposure_stratum == "0", "B",
                exposure_stratum == "1", "A",
                default = NA_character_ )]


general_info <- DT[, .(treatment,
                      `N pazienti` = clean_chr(N_),
                      `N eventi` = clean_chr(N_outcome_),
                      `Anni-Persona` = clean_chr(PY_),
                      `Incidence rate (95% CI)` = format_ci(
                        est   = IR_,
                        lower = IRll_,
                        upper = IRul_))]

# HR
hr <- DT[,.(treatment,
            `Hazard ratio grezzo (95% CI)` =
             format_ci(
             est   = HR_,
             lower = HRll_,
             upper = HRul_),
            `Hazard ratio aggiustato (95% CI)` =
             format_ci(
             est   = HR_adj_,
             lower = HR_adj_ll_,
             upper = HR_adj_ul_)
    )]

# RMST
rmst[, treatment:=ifelse(exposure_stratum==1, "A", "B")]

rmst[, analysisRMST:=NULL]
rmst[, exposure_stratum:=NULL]

rmst_year1 <- rmst[,.(treatment,
                    `Restricted mean survival time 1 anno (95% CI)` =
                     format_ci(
                     est   = RMST_,
                     lower = RMST_lb_,
                     upper = RMST_ub_) ) ]

#all together
table1_long <- merge(general_info, hr,
                      by = "treatment",
                      all.x = TRUE, sort = FALSE)

table1_long <- merge(table1_long, rmst_year1,
                     by = "treatment",
                     all.x = TRUE, sort = FALSE)

# table1_long <- merge(table1_long,rmst_year5,
#                      by = c("estimand", "treatment"),
#                      all.x = TRUE, sort = FALSE)

# rmst_columns <- c(
#   "Restricted mean survival time year 3 (95% CI)",
#   "Restricted mean survival time year 5 (95% CI)"
# )

# table1_long[,
#             (rmst_columns) := lapply(.SD,
#                                      function(x) {
#                                        x <- clean_chr(x)
#                                        fifelse(is.na(x), "N/A", x)
#                                      }
#             ),.SDcols = rmst_columns]

#dcast
# table1 <- dcast(
#   table1_long,
#   . ~ treatment,
#   value.var = c(
#     "N patients",
#     "N events",
#     "Person-Years",
#     "Incidence rate (95% CI)",
#     "Hazard ratio grezzo (95% CI)",
#     "Hazard ratio aggiustato (95% CI)"
#   ),sep = " | " )

# setorder(table1, estimand_order)
# table1[, estimand_order := NULL]
# setnames( table1,  old = "estimand",new = "Comparison")

# metrics <- c(
#   "N patients",
#   "N events",
#   "Person-Years",
#   "Incidence rate (95% CI)",
#   "Hazard ratio grezzo (95% CI)",
#   "Hazard ratio aggiustato (95% CI)"
# )
# 
# A_columns <- paste0(
#   metrics," | A")
# 
# B_columns <- paste0(
#   metrics,  " | B" )
# 
# 
# setcolorder(
#   table1,  c(
#     "Comparison",
#     dapagliflozin_columns,
#     dpp4i_columns
#   )
# )

# #########################################
# salvataggio

outputfile <- table1_long
nameoutput <- "D6_Tab_5_risultati"
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
  

  
 