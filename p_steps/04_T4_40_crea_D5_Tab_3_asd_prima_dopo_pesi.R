############################################################
#                                                          #
####         Crea D5_Tab_3_asd_prima_dopo_pesi             #
#                                                          #
############################################################

# autori: Sofia Miniati, Rosa Gini, Sabrina Giometto

# v 1.0 12 Nov 2026

print('CREA D5_Tab_3_asd_prima_dopo_pesi')

#########################################
# assegnazione cartelle

if (TEST){
  testname <- "test_D5_Tab_3"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

#########################################
# caricamento dati
thisdirarchive <- file.path(thisdir, "p_parameters", "archive_parameters")

# lista covariate
parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive, "codebooks", "D4_esp_comp_baseline_pesi.xlsx"), 1)))

thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(value)])


# helpers
wtd_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

wtd_var <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  m <- sum(w * x) / sum(w)
  sum(w * (x - m)^2) / sum(w)
}

wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

asd_bin <- function(x1, x0, w1 = NULL, w0 = NULL){
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  p1 <- wtd_mean(as.numeric(x1 == 1), w1)
  p0 <- wtd_mean(as.numeric(x0 == 1), w0)
  val <- abs(p1 - p0) / sqrt((p1*(1-p1) + p0*(1-p0))/2)
  round(val,1)
}

asd_cont <- function(x1, x0, w1 = NULL, w0 = NULL){
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  m1 <- wtd_mean(x1, w1); m0 <- wtd_mean(x0, w0)
  v1 <- wtd_var(x1, w1);  v0 <- wtd_var(x0, w0)
  val <- abs(m1 - m0) / sqrt((v1 + v0)/2)
  round(val,1)
}

asd_cat <- function(x1, x0, w1 = NULL, w0 = NULL) {
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  cats <- union(unique(x1), unique(x0))
  p1 <- sapply(cats, function(c) wtd_mean(as.numeric(x1 == c), w1))
  p0 <- sapply(cats, function(c) wtd_mean(as.numeric(x0 == c), w0))
  num <- sum((p1 - p0)^2)
  denom <- sum((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  val <- sqrt(num / denom)
  round(val, 1)
}

desc_binary <- function(processing, dataset, inputvar, outputvar, vartot = "",
                        condition = "!is.na(person_id)", compute_Nmissing = FALSE,
                        weightvar = NULL) {
  if (is.null(weightvar)) {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1, .(var_N = .N), by = "stratum"]
  } else {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1,
                    .(var_N = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
  }
  processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
  processing[is.na(var_N), var_N := 0]
  thislist <- c("N")
  if (nchar(vartot) > 0) {
    processing[, var_perc := round(100 * var_N / get(vartot), digits = 1)]
    thislist <- c(thislist, "perc")
  }
  setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}

desc_cat <- function(processing, dataset, inputvar, listofvalues, outputvar, vartot = "",
                     condition = "!is.na(person_id)", compute_Nmissing = TRUE,
                     weightvar = NULL) {
  for (valuevar in listofvalues) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar, .(var_N = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar,
                      .(var_N = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    processing[is.na(var_N), var_N := 0]
    thislist <- c("N")
    if (nchar(vartot) > 0) {
      processing[, var_perc := round(100 * var_N / get(vartot), digits = 1)]
      thislist <- c(thislist, "perc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", valuevar, "_", thislist, "_"))
  }
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}

desc_continuous <- function(processing, dataset, inputvar, outputvar, vartot = "",
                            condition = "!is.na(person_id)", compute_Nmissing = FALSE,
                            weightvar = NULL) {
  if (is.null(weightvar)) {
    temp <- dataset[eval(parse(text = condition)), .(
      var_mean   = round(mean(get(inputvar), na.rm = TRUE), 1),
      var_sd     = round(sd(get(inputvar), na.rm = TRUE), 1),
      var_median = round(median(get(inputvar), na.rm = TRUE), 1),
      var_q1     = round(quantile(get(inputvar), probs = 0.25, na.rm = TRUE), 1),
      var_q3     = round(quantile(get(inputvar), probs = 0.75, na.rm = TRUE), 1),
      var_min    = round(min(get(inputvar), na.rm = TRUE), 1),
      var_max    = round(max(get(inputvar), na.rm = TRUE), 1)
    ), by = "stratum"]
  } else {
    temp <- dataset[eval(parse(text = condition)), .(
      var_mean   = round(wtd_mean(get(inputvar), get(weightvar)), 1),
      var_sd     = round(sqrt(wtd_var(get(inputvar), get(weightvar))), 1),
      var_median = round(wtd_quantile(get(inputvar), get(weightvar), 0.5), 1),
      var_q1     = round(wtd_quantile(get(inputvar), get(weightvar), 0.25), 1),
      var_q3     = round(wtd_quantile(get(inputvar), get(weightvar), 0.75), 1),
      var_min    = round(min(get(inputvar), na.rm = TRUE), 1),
      var_max    = round(max(get(inputvar), na.rm = TRUE), 1)
    ), by = "stratum"]
  }
  processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
  thislist <- c("mean", "sd", "median", "q1", "q3", "min", "max")
  setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}


  
#########################################
# caricamento dati input

allstrata <- readRDS(file.path(thisdirinput, "D4_esp_comp_pesi.rds"))
weights <-  readRDS(file.path(thisdirinput, "D4_esp_comp_pesi_troncati_trimmed.rds"))
weights <- weights[,.(person_id, t0, exposure, truncated_weight)]
or <- readRDS(file.path(thisdirinput, "D5_PS_model_OR.rds"))

allstrata <- merge(allstrata, weights , by = c("person_id", "t0", "exposure"), all.x = T)
allstrata[is.na(truncated_weight), truncated_weight := 0]
allstrata[, stratum := fifelse(exposure == 1, "exposed", "comparators")]

#########################################
# tabelle descrittive non pesate

processing <- allstrata[, .(N_ = .N), by = "stratum"]

for (covar in thiscovariates_bin) {
  namevar <- covar
  processing <- desc_binary(processing, dataset = allstrata,  inputvar = namevar, outputvar = covar,
                            vartot = "N_", compute_Nmissing = FALSE)
}

#########################################
# tabelle descrittive pesate

processing_w <- allstrata[, .(Nw_ = round(sum(truncated_weight, na.rm = TRUE), 1)), by = "stratum"]

for (covar in thiscovariates_bin) {
  namevar <- covar
  processing_w <- desc_binary(processing_w, dataset = allstrata, inputvar = namevar, outputvar = covar,
                              vartot = "Nw_", compute_Nmissing = FALSE,
                              weightvar = "truncated_weight")
}


setnames(processing, setdiff(names(processing), "stratum"),
         paste0(setdiff(names(processing), "stratum"), "BF"))

setnames(processing_w, "Nw_", "N_")
setnames(processing_w, setdiff(names(processing_w), "stratum"),
         paste0(setdiff(names(processing_w), "stratum"), "AF"))

#########################################
# unione tabelle descrittive pesate e non pesate

processing <- merge(processing, processing_w, by = "stratum")

#########################################
# ASD, non pesati e pesati

for (covar in thiscovariates_bin) {
  varname <- covar
  x1 <- allstrata[stratum == "exposed", get(varname)]
  x0 <- allstrata[stratum == "comparators", get(varname)]
  w1 <- allstrata[stratum == "exposed", truncated_weight]
  w0 <- allstrata[stratum == "comparators", truncated_weight]
  processing[, asd   := asd_bin(x1, x0)]
  processing[, asd_w := asd_bin(x1, x0, w1, w0)]
  setnames(processing, c("asd","asd_w"), c(paste0(covar,"_asd_BF"), paste0(covar,"_asd_AF")))
}

processing[, exposure := fifelse(stratum == "exposed", 1, 0) ]


#############################
# OR

for (covar in thiscovariates_bin) {
  orcovar <- copy(or)[variable == covar,]
  nrowcovar <- nrow(orcovar)
  if (nrowcovar > 0) {
    for (j in 1) {
      # thisvalue <- orcovar[j]$level
      thisor <- orcovar[j]$OR
      thisll <- orcovar[j]$CI_lower
      thisul <- orcovar[j]$CI_upper
      # print(covar)
      # print(thisvalue)
      for (par in c("or_","ll_","ul_")) {
        namevar <- paste0(covar,"_",par)
        thisval <- as.character(get(paste0("this",substr(par, 1, nchar(par) - 1))))
        processing[, (namevar) := fifelse(str_length(thisval) > 0, round(as.numeric(thisval),3),NA_real_)]
        
      }
    }
  }
}



#########################################
# salvataggio

ordercols <- c("stratum", "exposure", setdiff(names(processing), c("stratum", "exposure")))
processing <- processing[, ..ordercols]

nameoutput <- "D5_Tab_3_asd_prima_dopo_pesi"
nameoutputext <- paste0(nameoutput, ".rds")
assign(nameoutput, processing)
saveRDS(processing, file = file.path(thisdiroutput, nameoutputext))
