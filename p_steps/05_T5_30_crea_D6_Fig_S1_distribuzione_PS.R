############################################################
#                                                          #
###        Crea D6_Fig_S1_distribuzione_PS                 #
#                                                          #
############################################################

# autori: Sofia Miniati, Rosa Gini, Sabrina Giometto


# v 1.0 12 Nov 2026


print('CREA D6_Fig_S1_distribuzione_PS')

#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_D6_Fig_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
    thisdirinput <- direxp
    thisdiroutput <- direxp
}

#########################################
# load


#########################################

# this header works both locally and on DRE
# create the list list_ds_analysed of all datasources including their subpopulations (ds_subpop)
# store all the data in a list of datasets indexed by ds_subpop

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()


input <- readRDS(file = file.path(thisdirinput, "D5_Fig_S1_distribuzione_PS.rds"))
    
#########################################
# load input datasets

input <- tab_nice_all[[suffix]]
#    fread(file.path(thisdirinput, paste0("D5_Supplementary_Figure_1_masked",suffix,".csv")),colClasses = "character")

#########################################
# start_processing

#Numeric Frequency and others
input[, Frequency := as.numeric(fifelse(Frequency == "<5", 2.5, Frequency))]
input[,bin := as.numeric(bin)]
input[,pc_1 := as.numeric(pc_1)]
input[,pc_99 := as.numeric(pc_99)]

#exposure_stratum as factor
input[, exposure_fac := as.factor(exposure_stratum)]
levels(input$exposure_fac) <- c("Comparator", "Exposure")


#vector of percentiles and colors
percentiles <- data.table(
  xintercept = c(
    input$pc_1[input$exposure_stratum == 1][1],
    input$pc_99[input$exposure_stratum == 1][1],
    input$pc_1[input$exposure_stratum == 0][1],
    input$pc_99[input$exposure_stratum == 0][1]
  ),
  group = c(
    "1st percentile for exposure",
    "99th percentile for exposure",
    "1st percentile for comparator", 
    "99th percentile for comparator"
  ),
  linetype = c("dotted", "dotted", "dashed", "dashed"),
  color = c("gold", "gold", "skyblue", "skyblue")
)

width <- 0.05


##Propensity Score Overlap

plot <- ggplot(input, aes(x = bin, y = Frequency, fill = exposure_fac)) +
  geom_col(position = "identity", width = width) |> blend("multiply") +
  scale_fill_manual(values = c("skyblue", "gold")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = "Propensity Score Overlap by Treatment Group",
       x = "Propensity Score",
       y = "Frequency",
       fill = NULL) +
  theme_minimal() +
  guides(fill = guide_legend(
    reverse = TRUE,
    override.aes = list(linetype = 0, color = NA),
    order = 2
  )) +
  theme(legend.position = c(0.85, 0.85), 
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.background = element_rect(fill = "white", 
                                         color = "grey",
                                         linewidth = 0.3)) +
  geom_vline(data = percentiles, 
             aes(xintercept = xintercept, color = group, linetype = group),
            linewidth=1.4, show.legend = TRUE) +
  scale_color_manual(name = NULL,
                     values = setNames(percentiles$color, percentiles$group)) +
  scale_linetype_manual(name = NULL,
                        values = setNames(percentiles$linetype, percentiles$group))



#########################################
# clean

# listvartokeep <- c()
# processing <- processing[,..listvartokeep]

#########################################
# save

#figure
ggsave(paste0("D6_Fig_S1_distribuzione_PS.pdf"), 
       plot = plot, 
       path = thisdiroutput, 
       width = 10, 
       height = 8)
  
