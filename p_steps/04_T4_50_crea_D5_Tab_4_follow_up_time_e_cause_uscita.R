############################################################
#                                                          #
###     Crea D5_Tab_4_follow_up_time_e_cause_uscita        #
#                                                          #
############################################################


# autori: Rosa Gini, Sabrina Giometto

# v 1.0 12 Nov 2026


print('CREA D5_Tab_4_follow_up_time_e_cause_uscita')


#########################################
# assegnazione cartelle

if (TEST){
  testname <- "test_D5_Tab_4"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

#########################################
# load parameters

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D4_follow_up.xlsx")),1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(values)])
thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)]) #[-1]
thiscovariates_cont <- unlist(parameters_this_step[parameter == "covar_cont",.(values)])

parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))
for (par in parameter_list) {
  temp <- unname(unlist(unique(parameters_this_step[parameter  == par,.(get("values"))])))
  assign(par, temp)
}


#########################################
# funzioni asd

# binaria
asd_bin <- function(x1, x0){
  p1 <- mean(x1 == 1, na.rm=TRUE)
  p0 <- mean(x0 == 1, na.rm=TRUE)
  val <- abs(p1 - p0) / sqrt((p1*(1-p1) + p0*(1-p0))/2)
  round(val,1)
}

# continua
asd_cont <- function(x1, x0){
  m1 <- mean(x1, na.rm=TRUE)
  m0 <- mean(x0, na.rm=TRUE)
  sd1 <- sd(x1, na.rm=TRUE)
  sd0 <- sd(x0, na.rm=TRUE)
  val <- abs(m1 - m0) / sqrt((sd1^2+sd0^2)/2)
  round(val,1)
}

# categorica
asd_cat <- function(x1, x0) {
  cats <- union(unique(x1), unique(x0))
  p1 <- sapply(cats, function(c) mean(x1 == c, na.rm = TRUE))
  p0 <- sapply(cats, function(c) mean(x0 == c, na.rm = TRUE))
  num <- sum((p1 - p0)^2)
  denom <- sum((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  val <- sqrt(num / denom)
  round(val, 1)
}


# funzioni per descrivere le covariate

# binarie
desc_binary <- function(dataset, inputvar, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = F) {
  temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1, .(var_N = .N), by = "stratum" ]
  processing <- merge(processing,temp, by =  "stratum", all.x = T)
  processing[is.na(var_N), var_N := 0]
  thislist <- c("N")
  if (nchar(vartot) > 0) {
    processing[, var_perc := round(100* var_N/get(vartot), digits = 1)]
    thislist <- c(thislist,"perc")
  }
  setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}

# categoriche
desc_cat <- function(dataset, inputvar, listofvalues, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = F) {
  for (valuevar in listofvalues) {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar, .(var_N = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    processing[is.na(var_N), var_N := 0]
    thislist <- c("N")
    if (nchar(vartot) > 0) {
      processing[, var_perc := round(100* var_N/get(vartot), digits = 1)]
      thislist <- c(thislist,"perc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",valuevar,"_",thislist,"_"))
  }
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}

# continue
desc_continuous <- function(dataset, inputvar, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = F) {
  temp <- dataset[eval(parse(text = condition)) , .(var_mean = round(mean(get(inputvar), na.rm = T),1),
                                                    var_sd = round(sd(get(inputvar), na.rm = T),1),
                                                    var_median = round(median(get(inputvar), na.rm = T),1),
                                                    var_q1 = round(quantile(get(inputvar), probs = 0.25, na.rm = T),1),
                                                    var_q3 = round(quantile(get(inputvar), probs = 0.75, na.rm = T),1),
                                                    var_min = round(min(get(inputvar), na.rm = T),1),
                                                    var_max = round(max(get(inputvar), na.rm = T),1)
  ), by = "stratum" ]
  processing <- merge(processing,temp, by =  "stratum", all.x = T)
  # processing[is.na(var1), var1 := 0]
  # processing[, var2 := round(100* var1/N_, digits = 1)]
  thislist <- c("mean", "sd", "median", "q1", "q3", "min", "max")
  setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
      
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}


  
#########################################
# caricamento dati input
  
allstrata <- readRDS(file.path(thisdirinput, "D4_follow_up.rds"))

# DA RIMUOVERE DOPO AVER CORRETTO

setnames(allstrata, "drug", "esposizione")

##################################

allstrata[,stratum:= esposizione]

# aggiunta strato "totale"

toadd <- copy(allstrata)
toadd[, stratum := 99]

allstrata <- rbind(allstrata, toadd)

#  numero di soggetti
processing <- allstrata[,.(N_ = .N), by = "stratum"]

# descrittiva

for (varname in thiscovariates_cat) {
  listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter  == paste0(varname,"value"),.(values)])))
  processing <- desc_cat(dataset = allstrata, inputvar = paste0(varname), listofvalues = listofvaluesthisvar, outputvar = varname, vartot = "N_")  
}

for (covar in c(thiscovariates_cont)) {
  namevar <- paste0(covar)
  allstrata[, (namevar) := as.numeric(get(namevar))]
  suppressWarnings( processing <- desc_continuous(dataset = allstrata, inputvar =  paste0(covar), outputvar = covar, vartot = "N_", compute_Nmissing = F))
}

for (covar in thiscovariates_bin) { 
  processing <- desc_binary(dataset = allstrata, inputvar = paste0(covar), outputvar = covar, vartot = "N_", compute_Nmissing = F)
  
}

  
#########################################
# salvataggio

outputfile <- processing

nameoutput <- "D5_Tab_4_follow_up_time_e_cause_uscita"
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))

