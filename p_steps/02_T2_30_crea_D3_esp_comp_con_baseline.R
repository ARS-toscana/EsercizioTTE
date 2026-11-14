# author: Rosa Gini

# v 1.0 14 Nov 2026

#########################################

if (TEST){
  # testname <- "test_D3_esp_comp_con_baseline"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output")
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_esp_comp_baseline.xlsx")),1)))

component_variables <- unlist(unique(parameters_this_step[parameter == "component",.(value)]))



persons <- readRDS(file.path(thisdirinput, paste0("D3_individui.rds")))

# load data

processing <- readRDS(file.path(thisdirinput, "D3_esp_comp.rds"))

# year_first

processing[, year_first := year(date_first)]

# variabili
  
component <- "angi"
for (component in component_variables) {
  processing[, (component) := 0]
  ingredients <- unlist(unique(parameters_this_step[parameter == component,.(value)]))
  for (ingredient in ingredients) {
    temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
    num <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(howmany)]))
    winstart <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(start)]))
    winend <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(end)]))
    position <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(position)]))
    setnames(temp, "id", "person_id")
    temp[, person_id := as.character(person_id)]
    temp <- temp[DATE >= study_start_date - 730,]
    temp <- merge(processing[,.(person_id, date_first)],temp, by = "person_id", all = F)
    temp <- temp[DATE >= date_first + winstart & DATE <= date_first + winend,]
    if (!is.na(position) & position == 1) {
      temp <- temp[ord == 0 | Table_cdm == "ps",]
    }
    temp <- unique(temp[,.(person_id, DATE)])
    temp[, n := rowid(person_id)]
    temp <- temp[n == num,]
    temp[, temp := 1]
    tokeep <- c("person_id", "temp")
    temp <- temp[,..tokeep]
    processing <- merge(processing, temp, by = "person_id", all.x = T)
    processing[is.na(temp), temp := 0]
    processing[, (component) := pmax(get(component), temp)]
    processing[, temp := NULL]
  }
  
}


# clean and save

# tokeep <- c("person_id", "date_first", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")
# 
# processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_esp_comp_con_baseline.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))



