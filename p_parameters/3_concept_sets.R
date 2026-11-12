###################################################################
# DESCRIBE THE CONCEPT SET IDs AND RULES
###################################################################

concept_sets_of_our_study_drugs <- c("A","B", "med_iperten", "med_cardioisc_angi", "med_dislip", "med_diab", "med_cortic",  "med_antitromb",  "med_bifos", "med_altri_onco")

concept_sets_of_our_study_diagnosis <- c("dia_iperten","dia_cardioisc","dia_angi","dia_dislip","dia_diab", "dia_ictus", "dia_infart", "dia_arit", "dia_tia", "dia_scompcard","dia_metastatic")

drug_names <- c("A", "B")


# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

for (concept_id in concept_sets_of_our_study_drugs) {
  concept_set_domains[[concept_id]] = "Medicines"
}

for (concept_id in concept_sets_of_our_study_diagnosis) {
  concept_set_domains[[concept_id]]="Diagnosis"
}


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["A"]][["ATC"]] = c("L02BAAA")
concept_set_codes_our_study[["B"]][["ATC"]] = c("L02BBBB")

concept_set_codes_our_study[["med_iperten"]][["ATC"]] <- c("C09", "C02", "C07", "C08C")
concept_set_codes_our_study[["med_cardioisc_angi"]][["ATC"]] <- "C01DA"
concept_set_codes_our_study[["med_dislip"]][["ATC"]] <- "C10"
concept_set_codes_our_study[["med_diab"]][["ATC"]] <- "A10"
concept_set_codes_our_study[["med_RENDIS_Alg1_1"]][["ATC"]] <- c("C09C")
concept_set_codes_our_study[["med_RENDIS_Alg1_2"]][["ATC"]] <- c("C09B")
concept_set_codes_our_study[["med_RENDIS_Alg1_3"]][["ATC"]] <- c("M04AA01")
concept_set_codes_our_study[["med_RENDIS_Alg2"]][["ATC"]] <- c("B03XA01", "V03AE03", "V03AE02", "V03AE01", "H05BX02", "B03XA02", "H05BX01")
concept_set_codes_our_study[["med_bifos"]][["ATC"]] <- c("M05BA", "M05BB")
concept_set_codes_our_study[["med_cortic"]][["ATC"]] <- c("H02AB","H02B")
concept_set_codes_our_study[["med_antitromb"]][["ATC"]] <- c("B01A")
concept_set_codes_our_study[["med_altri_onco"]][["ATC"]] <- c("L01")
concept_set_codes_our_study_excl[["med_altri_onco"]][["ATC"]] <- c(concept_set_codes_our_study[["A"]][["ATC"]], concept_set_codes_our_study[["B"]][["ATC"]])


concept_set_codes_our_study[["dia_iperten"]][["ICD9"]] <- c("401", "402", "403", "404", "405", "36211")
concept_set_codes_our_study[["dia_cardioisc"]][["ICD9"]] <- c("410", "414")
concept_set_codes_our_study[["dia_angi"]][["ICD9"]] <- c("413")
concept_set_codes_our_study[["dia_dislip"]][["ICD9"]] <- c("2720", "2721", "2723", "2724")
concept_set_codes_our_study[["dia_diab"]][["ICD9"]] <- c("250")
concept_set_codes_our_study[["dia_ictus"]][["ICD9"]] <- c("430", "431", "432", "434", "436")
concept_set_codes_our_study[["dia_infart"]][["ICD9"]] <- c("410")
concept_set_codes_our_study[["dia_arit"]][["ICD9"]] <- c("427")
concept_set_codes_our_study[["dia_tia"]][["ICD9"]] <- c("435")
concept_set_codes_our_study[["dia_scompcard"]][["ICD9"]] <- c("428", "3981", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493")
concept_set_codes_our_study[["dia_metastatic"]][["ICD9"]] <- c("1985", "196", "1970", "1977")



