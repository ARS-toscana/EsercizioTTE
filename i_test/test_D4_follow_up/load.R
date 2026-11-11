rm(list=ls(all.names=TRUE))


baselinedate <- 20151231


#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

# list of datasets
listdatasets <- c("D3_outcome", "D3_esp_comp", "D3_episodi_trattamento")

# dates variables 
listdates <- list()


listdates[["D3_outcome"]] <- c("date_first", "data_outcome")

listdates[["D3_esp_comp"]] <- c("date_first", "op_start", "op_end")

listdates[["D3_episodi_trattamento"]] <- c("date_first", "episode_start", "episode_end")


# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    # baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
    baseline[[namedataset]][[datevar]] <- NA_Date_
  }
}


# load datasets
for (namedataset in listdatasets){
  
  data <- as.data.table(fread(paste0(thisdir, "/", namedataset, ".csv")))
  # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
  # data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  
  for (datevar in listdates[[namedataset]]) {
    
    if (!is.na(baseline[[namedataset]][[datevar]])){
      
    #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
      
    }else{
      
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset, data)
  
  saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
    
}

