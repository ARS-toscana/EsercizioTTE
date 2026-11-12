# autori: Sabrina Giometto

# v 1.0 11 Nov 2026 

#########################################

if (TEST){
  testname <- "test_D5_Tab_2"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}


# caricamento dati
data <- readRDS(file = paste0(thisdirinput, "/D3_esp_comp_con_baseline.rds"))
data <- as.data.table(data)


# creazione D5 con covariate continue
D5_nocov <- data[, .(
              N          = .N,
              age_median = median(age),
              age_q1 = quantile(age, probs = 0.25),
              age_q3 = quantile(age, probs = 0.75))]

# creazione D5 con covariate binarie
covariates_binary <- c("iperten", "cardioisc", "infart", "arit", "angi",
                       "ictus", "tia", "scompcard", "dislip", "diab", "renal",
                       "metastatic", "cortic", "antiang", "antitromb", "ipolip", 
                       "antidiab", "bifosf", "conc_treat")

D5_cov <- NULL

for (i in covariates_binary) {

  tmp <- data[, .(
              N = .N,
              tmp_N = sum(get(i)==1),
              tmp_p = round(sum(get(i)==1)/.N,3)*100)]

  setnames(tmp,"tmp_N",paste0(i, "_N"))
  setnames(tmp,"tmp_p",paste0(i, "_p"))

  if (is.null(D5_cov)) {

    D5_cov <- tmp

  } else {

    D5_cov <- merge(D5_cov, tmp, by = "N")
  }

}

# creazione D5 finale
D5 <- merge(D5_nocov, D5_cov, by = "N", all = F)


# salvataggio
saveRDS(D5, file = paste0(thisdiroutput, "/D5_Tab_2_caratteristiche_baseline.rds"))
write.csv(D5, file = paste0(thisdiroutput, "/D5_Tab_2_caratteristiche_baseline.csv"))

