############################################################
#                                                          #
###         Crea D5_Fig_S1_distribuzione_PS                #
#                                                          #
############################################################


# autori: Sofia Miniati, Sabrina Giometto

# v 1.0 12 Nov 2026

print('CREA D5_Fig_S1_distribuzione_PS')


#########################################
# assegnazione cartelle

if (TEST){
  testname <- "test_D5_Fig_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}


#########################################
# caricamento dati input
  
input <- readRDS(file.path(thisdirinput, "D4_esp_comp_pesi.rds"))
  
#########################################
# start_processing

input <- input[,.(exposure,ps)]

#bin
width <- 0.05
input[, bin := pmin(floor(ps / width) * width, 1 - width)]

all_bin <- seq(0, 1 - width, by = width)

##Frequency
processing <- CJ(exposure_stratum = unique(input$exposure), bin = all_bin)

freq_obs <- input[, .(Frequency = .N), by = .(exposure, bin)]
setnames(freq_obs, "exposure", "exposure_stratum")

processing <- freq_obs[processing, on = .(exposure_stratum, bin)]
processing[is.na(Frequency), Frequency := 0]


#pc_1
processing[input[, .(pc_1 = quantile(ps, 0.01)), by = .(exposure_stratum = exposure)], 
           pc_1 := i.pc_1, 
           on = .(exposure_stratum)]
#pc_99
processing[input[, .(pc_99 = quantile(ps, 0.99)), by = .(exposure_stratum = exposure)], 
           pc_99 := i.pc_99, 
           on = .(exposure_stratum)]

##Diagnostics
#First quantile
processing[input[, .(pc_25 = quantile(ps, 0.25)), by = .(exposure_stratum = exposure)], 
           pc_25 := i.pc_25, 
           on = .(exposure_stratum)]
#Median
processing[input[, .(pc_50 = quantile(ps, 0.5)), by = .(exposure_stratum = exposure)], 
           pc_50 := i.pc_50, 
           on = .(exposure_stratum)]
#Third quantile
processing[input[, .(pc_75 = quantile(ps, 0.75)), by = .(exposure_stratum = exposure)], 
           pc_75 := i.pc_75, 
           on = .(exposure_stratum)]
#Mean
processing[input[, .(ps_mean = mean(ps)), by = .(exposure_stratum = exposure)], 
           ps_mean := i.ps_mean, 
           on = .(exposure_stratum)]

#########################################
# clean

listvartokeep <- c("exposure_stratum",
                   "bin",
                   "Frequency",
                   "pc_1",
                   "pc_99",
                   "pc_25",
                   "pc_50",
                   "pc_75",
                   "ps_mean"
                   )
processing <- processing[,..listvartokeep]

#########################################
# save

outputfile <- processing

nameoutput <- "D5_Fig_S1_distribuzione_PS"
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))


