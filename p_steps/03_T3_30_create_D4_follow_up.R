############################################################
#                                                          #
####   CREATE D4_FUP                              ####
# 
#                                                          #
############################################################

# author: Rosa Gini

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_22_D4_FUP"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirexp <- thisdiroutput
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirexp <- direxp
}

#########################################
# load input datasets

print('CREATE D4_FUP')


thisconcept_ids <- c("A", "B" )

# helper function: causes of censoring have always the same labels

compute_cause_censoring <- function(namecause,namedate,vector){
  processing[, (namecause) := NA_integer_]
  j <- 1
  for (cause in c("outcome_date",
                  "death_date",
                  "recommended_end_date",
                  "end_time_window_fup",
                  "study_exit_date",
                  "discontinuation_date",
                  "new_medicine_date",
                  "exit_spell_category")) {
    if (j %in% vector) {
      processing[is.na(get(namecause)) & !is.na(get(cause)) & get(cause) == get(namedate), (namecause) := j]
    }
    j <- j + 1
  }
}

compute_cause_censoring_shorter <- function(namecause,namedate,vector){
  processing[, (namecause) := NA_integer_]
  j <- 1
  for (cause in c("outcome_date",
                  "death_date",
                  "recommended_end_date",
                  "end_time_window_fup_shorter",
                  "study_exit_date",
                  "discontinuation_date",
                  "new_medicine_date",
                  "exit_spell_category")) {
    if (j %in% vector) {
      processing[is.na(get(namecause)) & !is.na(get(cause)) & get(cause) == get(namedate), (namecause) := j]
    }
    j <- j + 1
  }
}



suffix <- ""

persons <- readRDS(file.path(thisdirinput,paste0("D3_STUDY_POPULATION.rds")))

processing <- persons

######################################
# discontinuation_date

eligibility <- readRDS(file.path(thisdirinput,paste0("D3_EPISODES_STUDY_DRUGS.rds")))
# eligibility <- eligibility[is_study_episode == 1,]
processing[, episode_start := t0]
processing <-   merge(processing,eligibility[,.(person_id, episode_start,episode_end)], by = c("person_id","episode_start"), all.x = T )

setnames(processing, "episode_end", "discontinuation_date")
processing[ , episode_start := NULL]

rm(eligibility)


######################################
# new_medicine_date

# another medicine (not the exposure nor the comparator)

data <- data.table(
  person_id = character(),
  date = as.Date(character()),
  meaning_renamed = character(),
  concept_id = character()
)
for (conc in thisconcept_ids) {
  fileparquet <- file.path(thisdirinput,paste0(conc,"_dataset",".parquet"))
    dt <- as.data.table(as.data.frame(polars::pl$read_parquet(fileparquet)))
    dt[, concept_id := conc]
    dt <- dt[,.(person_id,date,concept_id)]
    data <- rbind(data,dt, fill = T)
} 

data <- merge(data, persons[,.(person_id, exit_spell_category, t0)], by = "person_id", all = F )
data <- data[ date > t0 & date <= exit_spell_category,]

# calculate first date per person 
data <- unique(data[, .(person_id, date, concept_id)])

setorder(data, person_id, date)

data[, n := rowid(person_id) ]

first_date <- copy(data)[n == 1,]
rm(data)

# calculate minimum of other medicine (exposure for comparators, and comparator for exposed)

episodes <- readRDS(file.path(thisdirinput,paste0("D3_EPISODES_STUDY_DRUGS",suffix,".rds")))
episodes <-   merge(episodes, persons[,.(person_id, exit_spell_category, t0, exposure)], by = "person_id", all = F )
episodes <- episodes[ episode_start > t0 & episode_start <= exit_spell_category,]
episodes <- episodes[ (exposure == 0 & exposure_episode == 1) |(exposure == 1 & exposure_episode == 0),]

setorder(episodes, person_id, episode_start)
episodes[, n := rowid(person_id) ]

episodes <- episodes[ n == 1, ]
episodes <- episodes[, .(person_id, episode_start, episode_end)]
episodes[, concept_id := "other_study_drug"]


# use -first_date- and -episodes- to define start of new medicine

processing <- merge(processing, first_date, by = "person_id", all.x = T)

processing[, new_medicine_date := date]
processing[, concept_new_medicine := concept_id]

processing[, date := NULL]

processing <- merge(processing, episodes, by = "person_id", all.x = T)

conc <- "other_study_drug"
concdate <- "episode_start"
processing[(is.na(new_medicine_date) & !is.na(get(concdate))) | (!is.na(new_medicine_date) & !is.na(get(concdate)) & get(concdate) < new_medicine_date), concept_new_medicine := conc]
processing[concept_new_medicine == conc, new_medicine_date := get(concdate)]

###########################
# end_time_window_fup

processing[, end_time_window_fup := as.Date(t0 + 730)]

###########################
# end_time_window_fup_shorter

processing[, end_time_window_fup_shorter := as.Date(t0 + 365)]

#################################
# outcome_date
  
outcomes <- readRDS(file.path(thisdirinput,paste0("D3_OUTCOMES",suffix,".rds")))
outcomes <- outcomes[, .(outcome_date = min(outcome_date)), by = "person_id"]
processing <- merge(processing, outcomes, by = "person_id", all.x = T)

#################################
# death_date

persons <- readRDS(file.path(thisdirinput,paste0("D3_PERSONS.rds")))
processing <- merge(processing, persons[,.(person_id, death_date)], by = "person_id", all.x = T)


##################################
# censoring_date
# cause_of_censoring

processing[, censoring_date := pmin(outcome_date,
                                    death_date,
                                    recommended_end_date,
                                    end_time_window_fup,
                                    study_exit_date,
                                    discontinuation_date,
                                    new_medicine_date,
                                    exit_spell_category, 
                                    na.rm = T)]


compute_cause_censoring("cause_of_censoring", "censoring_date", 1:8)



##########################################
# ESTIMAND 1
##########################################

##########################################
# censoring_date_E1
# cause_of_censoring_E1
# fup_days_E1
# date_intercurring_events_E1

processing[, censoring_date_E1 := censoring_date]

processing[, cause_of_censoring_E1 := cause_of_censoring]

processing[, fup_days_E1 := as.integer(censoring_date_E1 - t0)]


# this is the date that enters IPCW and extreme scenarios: now it will be set to 'exit_spell_category' only, and the corresponding cause will be '8' only
processing[, date_intercurring_events_E1 := pmin(exit_spell_category, 
                                                na.rm = T)]

processing[ , intercurrent_event_E1 := as.integer(!is.na(date_intercurring_events_E1) & censoring_date_E1 == date_intercurring_events_E1 & cause_of_censoring_E1 %in% c(8))]

# this is to be used in the extreme scenarios analysis: the non-administrative cause of censoring no longer censors - it is only cause 8 now
processing[, censoring_date_excluding_intercurring_E1 := pmin(outcome_date,
                                                              death_date,
                                                              recommended_end_date,
                                                              end_time_window_fup,
                                                              discontinuation_date,
                                                              new_medicine_date,
                                                              study_exit_date,
                                                              na.rm = T)]


###################
# E1: fup time separately per cause of end


for (cause in 1:8) {
  processing[cause_of_censoring_E1 == cause, fup_days := fup_days_E1]
  setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E1_",cause))
}  

##########################################
# ESTIMAND 2b
##########################################

###########################################
# censoring_date_E2b
# cause_of_censoring_E2b
# fup_days_E2b

processing[, censoring_date_E2b :=  pmin(outcome_date,
                                         death_date,
                                         recommended_end_date,
                                         end_time_window_fup_shorter,
                                         study_exit_date,
                                         discontinuation_date,
                                         new_medicine_date,
                                         exit_spell_category, 
                                         na.rm = T)]  

compute_cause_censoring_shorter("cause_of_censoring_E2b", "censoring_date_E2b", 1:8) 
processing[, fup_days_E2b := as.integer(censoring_date_E2b - t0)]  



###################
# E2b: fup time separately per cause of end

for (cause in 1:8) {
  processing[cause_of_censoring_E2b == cause, fup_days := fup_days_E2b]
  setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E2b_",cause))
}  

##########################################
# ESTIMAND 3
##########################################

###########################################
# censoring_date_E3
# cause_of_censoring_E3
# fup_days_E3

processing[, censoring_date_E3 := pmin(outcome_date,
                                       death_date,
                                       recommended_end_date,
                                       end_time_window_fup,
                                       study_exit_date,
                                       exit_spell_category, 
                                       na.rm = T)]  

compute_cause_censoring("cause_of_censoring_E3", "censoring_date_E3", c(1,2,3,4,5,8)) 
processing[, fup_days_E3 := as.integer(censoring_date_E3 - t0)]  

###################
# E3: fup time separately per cause of end

for (cause in c(1,2,3,4,5,8)) {
  processing[cause_of_censoring_E3 == cause, fup_days := fup_days_E3]
  setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E3_",cause))
}  

###################
# E3: fup time until intercurring events that do not cause end of follow up (events 6 and 7)

processing[, intercurring_not_ending_fup_E3_6 := fifelse(!is.na(discontinuation_date) & discontinuation_date < censoring_date_E3, 1, 0)]

processing[intercurring_not_ending_fup_E3_6 == 1, fup_days_to_intercurring_not_ending_fup_E3_6 := as.integer(discontinuation_date - t0)]

processing[, intercurring_not_ending_fup_E3_7 := fifelse(!is.na(new_medicine_date) & new_medicine_date < censoring_date_E3, 1, 0)]

processing[intercurring_not_ending_fup_E3_7 == 1, fup_days_to_intercurring_not_ending_fup_E3_7 := as.integer(new_medicine_date - t0)]
  
  
 #########################################
 # cleaning
 
 tokeep <-  c("person_id", "t0", "exposure", "birth_year", "exit_spell_category", "death_date", "discontinuation_date", "new_medicine_date",  "end_time_window_fup", "outcome_date", "censoring_date", "cause_of_censoring", "censoring_date_E1", "cause_of_censoring_E1", "fup_days_E1","intercurrent_event_E1", "date_intercurring_events_E1", "censoring_date_excluding_intercurring_E1", paste0("fup_days_cause_of_censoring_E1_",1:8), "censoring_date_E2b", "cause_of_censoring_E2b", "fup_days_E2b", paste0("fup_days_cause_of_censoring_E2b_",1:8), "censoring_date_E3", "cause_of_censoring_E3", "fup_days_E3", paste0("fup_days_cause_of_censoring_E3_",c(1,2,3,4,5,8)), "intercurring_not_ending_fup_E3_6", "fup_days_to_intercurring_not_ending_fup_E3_6", "intercurring_not_ending_fup_E3_7", "fup_days_to_intercurring_not_ending_fup_E3_7")
 
 processing <- processing[,..tokeep]
 
 
 #########################################
 # save
 
 outputfile <- processing
 
 nameoutput <- paste0("D4_FUP")
 nameoutputext <- paste0(nameoutput,suffix,".rds")
 assign(nameoutput, outputfile)
 saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
   
