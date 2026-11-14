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


# list of csv datasets

listdatasetscsv <- c()

# list of RData datasets

listdatasetsRData <- c("A", "B")

# list of datasets
listdatasets <- c(listdatasetscsv, listdatasetsRData, "D3_individui", "D3_periodi_osservazione")

# dates variables 
listdates <- list()


listdates[["D3_individui"]] <- c("birth_date", "death_date")
listdates[["D3_periodi_osservazione"]] <- c("start_op",	"end_op"	)

for (dataset in listdatasetsRData) {
  listdates[[dataset]] <- c("DATE")
}


# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
    baseline[[namedataset]][[datevar]] <- NA_Date_
  }
}


# load datasets
for (namedataset in listdatasets){
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  
  for (datevar in listdates[[namedataset]]) {
    
    if (!is.na(baseline[[namedataset]][[datevar]])){
      
    #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
      
    }else{
      
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset, data)
  
  if (namedataset %in% listdatasetscsv) {
    date_cols <- listdates[[namedataset]]
    data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
    fwrite(data, file = file.path(thisdir, paste0(namedataset,".csv")))
  }else if (namedataset %in% listdatasetsRData) {
    save(data, file = file.path(thisdir, paste0(namedataset,".RData")), list = namedataset)
  }else{
  saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
  }  
}

