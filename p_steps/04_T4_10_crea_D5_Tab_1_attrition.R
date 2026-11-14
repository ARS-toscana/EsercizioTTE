# autori: Sabrina Giometto

# v 0.1 11 Nov 2026

#########################################

if (TEST){
  testname <- "test_D5_Tab_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}


# caricamento dati
D3_selezione_esp_comp <- readRDS(file = paste0(thisdirinput, "/D3_selezione_esp_comp.rds"))
D3_selezione_esp_comp <- as.data.table(D3_selezione_esp_comp)


# creazione D5 con variabili di selezione binarie

D5 <- NULL

for (i in var_selection) {
  
  if (i!="in_studio") {
    
    tmp <- D3_selezione_esp_comp[, .(
      N = .N,
      tmp_N = sum(get(i)==0, na.rm = T),
      tmp_p = round(sum(get(i)==0, na.rm = T)/.N,3)*100)]
    
    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  } else if (i == "in_studio") {
    
    tmp <- D3_selezione_esp_comp[, .(
      N = .N,
      tmp_N = sum(get(i)==1, na.rm = T),
      tmp_p = round(sum(get(i)==1, na.rm = T)/.N,3)*100)]
    
    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  } 
  
  if (is.null(D5)) {
    
    D5 <- tmp
    
  } else {
    
    D5 <- merge(D5, tmp)
  }
  
  assign("D5_attrition", D5)
  
}
  

# salvataggio
saveRDS(D5_attrition, file = paste0(thisdiroutput, "/D5_Tab_1_attrition.rds"))
write.csv(D5_attrition, file = paste0(thisdiroutput, "/D5_Tab_1_attrition.csv"))
  
