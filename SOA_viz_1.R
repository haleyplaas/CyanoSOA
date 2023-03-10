<<<<<<< HEAD
rm(list=ls()) #clear environment
setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS") #set working directory 
#load necessary packages
library(dplyr); library(tidyr); library(ggplot2);library(cowplot);library(tidyverse);library(patchwork);library(tidyverse);library(scales); library(rstatix);library(plyr)

MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB")
GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM")

for(i in 1:length(MIB_files)) {                              
  assign(paste0("MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB/", MIB_files[i])))}
for(i in 1:length(GSM_files)) {                              
  assign(paste0("GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM/", GSM_files[i])))}

#Cleaning the data -----------------------------------------------------------------------------------------------------------------------------
#2-METHYLISOBORNEOL TRIALS
MIB1 <- MIB1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB1$timepoint <- 1:nrow(MIB1)
MIB1 <- MIB1 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB1 <- MIB1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB1$timepoint < 62 ~ "O3",
  MIB1$timepoint >= 62 & MIB1$timepoint < 86 ~ "OH",
  MIB1$timepoint >= 86 & MIB1$timepoint < 203 ~ "OH+VOC",
  MIB1$timepoint >= 203 & MIB1$timepoint <= 236 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

MIB2 <- MIB2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB2$timepoint <- 1:nrow(MIB2)
MIB2 <- MIB2 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB2 <- MIB2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB2$timepoint < 80 ~ "O3",
  MIB2$timepoint >= 80 & MIB2$timepoint < 168 ~ "OH",
  MIB2$timepoint >= 168 & MIB2$timepoint < 256 ~ "OH+VOC",
  MIB2$timepoint >= 256 & MIB2$timepoint <= 279 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB3 <- MIB3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB3$timepoint <- 1:nrow(MIB3)
MIB3 <- MIB3 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB3 <- MIB3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB3$timepoint < 38 ~ "O3",
  MIB3$timepoint >= 38 & MIB3$timepoint < 71 ~ "OH",
  MIB3$timepoint >= 71 & MIB3$timepoint < 156 ~ "OH+VOC",
  MIB3$timepoint >= 156 & MIB3$timepoint <= 180 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB4 <- MIB4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB4$timepoint <- 1:nrow(MIB4)
MIB4 <- MIB4 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB4 <- MIB4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB4$timepoint < 32 ~ "O3",
  MIB4$timepoint >= 32 & MIB4$timepoint < 56 ~ "OH",
  MIB4$timepoint >= 56 & MIB4$timepoint < 147 ~ "OH+VOC",
  MIB4$timepoint >= 147 & MIB4$timepoint <= 183 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

#GEOSMIN TRIALS
GSM1 <- GSM1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM1$timepoint <- 1:nrow(GSM1)
GSM1 <- GSM1 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM1 <- GSM1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM1$timepoint < 74 ~ "O3",
  GSM1$timepoint >= 74 & GSM1$timepoint < 162 ~ "OH",
  GSM1$timepoint >= 162 & GSM1$timepoint < 245 ~ "OH+VOC",
  GSM1$timepoint >= 245 & GSM1$timepoint <= 282 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

GSM2 <- GSM2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM2$timepoint <- 1:nrow(GSM2)
GSM2 <- GSM2 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM2 <- GSM2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM2$timepoint < 43 ~ "O3",
  GSM2$timepoint >= 43 & GSM2$timepoint < 60 ~ "OH",
  GSM2$timepoint >= 60 & GSM2$timepoint < 173 ~ "OH+VOC",
  GSM2$timepoint >= 173 & GSM2$timepoint <= 197 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM3 <- GSM3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM3$timepoint <- 1:nrow(GSM3)
GSM3 <- GSM3 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM3 <- GSM3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM3$timepoint < 34 ~ "O3",
  GSM3$timepoint >= 34 & GSM3$timepoint < 70 ~ "OH",
  GSM3$timepoint >= 70 & GSM3$timepoint < 152 ~ "OH+VOC",
  GSM3$timepoint >= 152 & GSM3$timepoint <= 173 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM4 <- GSM4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM4$timepoint <- 1:nrow(GSM4)
GSM4 <- GSM4 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM4 <- GSM4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM4$timepoint < 62 ~ "O3",
  GSM4$timepoint >= 62 & GSM4$timepoint < 84 ~ "OH",
  GSM4$timepoint >= 84 & GSM4$timepoint < 174 ~ "OH+VOC",
  GSM4$timepoint >= 174 & GSM4$timepoint <= 216 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

#Calculating average number concentration and mass concentrations ------------------------------------------------------------------------------------
#MIB Trial 1
MIB1.mass <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB1.mass.conc <- MIB1.mass-MIB1.bg
MIB1.num <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB1.num.conc <- MIB1.num-MIB1.bg

#MIB Trial 2
MIB2.mass <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB2.mass.conc <- MIB2.mass-MIB2.bg
MIB2.num <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB2.num.conc <- MIB2.num-MIB2.bg

#MIB Trial 3
MIB3.mass <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB3.mass.conc <- MIB3.mass-MIB3.bg
MIB3.num <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB3.num.conc <- MIB3.num-MIB3.bg

#MIB Trial 4
MIB4.mass <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB4.mass.conc <- MIB4.mass-MIB4.bg
MIB4.num <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB4.num.conc <- MIB4.num-MIB4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
MIB.trials.num <- cbind(MIB1.num.conc,MIB2.num.conc,MIB3.num.conc)
MIB.trials.num.t <- as.data.frame(t(MIB.trials.num))
colnames(MIB.trials.num.t) <- c("Number Concentration")
MIB.Number.Concentration <- get_summary_stats(MIB.trials.num.t, type = "mean_sd")

# # concentration background
MIB.bg.num <- cbind(MIB1.bg,MIB2.bg,MIB3.bg)
MIB.bg.num.t <- as.data.frame(t(MIB.bg.num))
colnames(MIB.bg.num.t) <- c("Number Concentration")
MIB.bg.Concentration <- get_summary_stats(MIB.bg.num.t, type = "mean_sd")

MIB.trials.mass <- cbind(MIB1.mass.conc,MIB2.mass.conc,MIB3.mass.conc)
MIB.trials.mass.t <- as.data.frame(t(MIB.trials.mass))
colnames(MIB.trials.mass.t) <- c("Mass Concentration")
MIB.Mass.Concentration <- get_summary_stats(MIB.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
MIB1.1 <- MIB1 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 65) %>% filter(timepoint < 86 | timepoint > 96) %>% filter(timepoint < 177 | timepoint > 209) %>% filter(timepoint < 230) #filtering by timepoint here is to remove transitional measurements and also try and normalize # of measurements per treatment between trial
MIB1.1 <- MIB1.1 %>%mutate(timepoint = 1:nrow(MIB1.1))

MIB2.1 <- MIB2 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 90) %>% filter(timepoint < 111 | timepoint > 173) %>% filter(timepoint < 254 | timepoint > 258) %>% filter(timepoint < 279) 
MIB2.1 <- MIB2.1 %>% mutate(timepoint = 1:nrow(MIB2.1))

MIB3.1 <- MIB3 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint < 18 | timepoint > 33) %>% filter(timepoint < 37 | timepoint > 42) %>% filter(timepoint < 63 | timepoint > 73) %>% filter(timepoint < 154 | timepoint > 159) %>% filter(timepoint < 180) 
MIB3.1 <- MIB3.1 %>% mutate(timepoint = 1:nrow(MIB3.1))

MIB4.1 <- MIB4 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 10) %>% filter(timepoint < 31 | timepoint > 34) %>% filter(timepoint < 55 | timepoint > 61) %>% filter(timepoint < 142 | timepoint > 156) %>% filter(timepoint < 177) 
MIB4.1 <- MIB4.1 %>% mutate(timepoint = 1:nrow(MIB4.1))

# --------------------------------------------------------------------------------------------------------------------------------
#GSM Trial 1
GSM1.mass <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM1.mass.conc <- GSM1.mass-GSM1.bg
GSM1.num <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM1.num.conc <- GSM1.num-GSM1.bg

#GSM Trial 2
GSM2.mass <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM2.mass.conc <- GSM2.mass-GSM2.bg
GSM2.num <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM2.num.conc <- GSM2.num-GSM2.bg

#GSM Trial 3
GSM3.mass <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM3.mass.conc <- GSM3.mass-GSM3.bg
GSM3.num <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM3.num.conc <- GSM3.num-GSM3.bg

#GSM Trial 4
GSM4.mass <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM4.mass.conc <- GSM4.mass-GSM4.bg
GSM4.num <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM4.num.conc <- GSM4.num-GSM4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
GSM.trials.num <- cbind(GSM2.num.conc,GSM3.num.conc,GSM4.num.conc)
GSM.trials.num.t <- as.data.frame(t(GSM.trials.num))
colnames(GSM.trials.num.t) <- c("Number Concentration")
GSM.Number.Concentration <- get_summary_stats(GSM.trials.num.t, type = "mean_sd")
# # concentration background
GSM.bg.num <- cbind(GSM2.bg,GSM3.bg,GSM4.bg)
GSM.bg.num.t <- as.data.frame(t(GSM.bg.num))
colnames(GSM.bg.num.t) <- c("Number Concentration")
GSM.bg.Concentration <- get_summary_stats(GSM.bg.num.t, type = "mean_sd")

GSM.trials.mass <- cbind(GSM2.mass.conc,GSM3.mass.conc,GSM4.mass.conc)
GSM.trials.mass.t <- as.data.frame(t(GSM.trials.mass))
colnames(GSM.trials.mass.t) <- c("Mass Concentration")
GSM.Mass.Concentration <- get_summary_stats(GSM.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
GSM1.1 <- GSM1 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 21 | timepoint > 75) %>% filter(timepoint < 94 | timepoint > 163) %>% filter(timepoint < 243 | timepoint > 262) %>% filter(timepoint < 284) 
GSM1.1 <- GSM1.1 %>% mutate(timepoint = 1:nrow(GSM1.1))

GSM2.1 <- GSM2 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 12 | timepoint > 45) %>% filter(timepoint < 59 | timepoint > 75) %>% filter(timepoint < 155 | timepoint > 177) %>% filter(timepoint < 197) 
timepoint <- c(12,13,14,15,16,17,18)
tp.df <- as.data.frame(timepoint)
GSM2.x <- rbind.fill(GSM2.1, tp.df) %>% arrange(timepoint)
GSM2.1 <- GSM2.x %>% mutate(timepoint = 1:nrow(GSM2.x))

GSM3.1 <- GSM3 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 13) %>% filter(timepoint < 34 | timepoint > 45) %>% filter(timepoint < 66 | timepoint > 71) %>% filter(timepoint < 152| timepoint > 153) %>% filter(timepoint < 174) 
GSM3.1 <- GSM3.1 %>% mutate(timepoint = 1:nrow(GSM3.1))

GSM4.1 <- GSM4 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 40) %>% filter(timepoint < 61 | timepoint > 62) %>% filter(timepoint < 82| timepoint > 89) %>% filter(timepoint < 169 | timepoint > 197) %>% filter(timepoint < 218) 
GSM4.1 <- GSM4.1 %>% mutate(timepoint = 1:nrow(GSM4.1))

# AEROSOL NUMBER AND MASS CONCENTRATIONS AND TIME SERIES PLOT ------------------------------------------------------------
#MIB
MIB.trials <- right_join(MIB1.1, MIB2.1, by =c('timepoint')) %>% left_join(MIB3.1, by =c('timepoint')) %>% left_join(MIB4.1, by =c('timepoint'))
MIB.Trials.x <- rbind(MIB1.1, MIB2.1, MIB3.1)
MIB.aerosol.summary.stats <- MIB.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "2MIB")

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

#PLOTS 
#MIB
MIB.ts <- ggplot(MIB.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() + scale_color_manual(values=c('gray', 'gray35', 'black')) + xlab("Experiment Time (minutes)") + ylab("Mass Concentration") + ggtitle("2-Methylisoborneol") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold"))

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

GSM.TS <- ggplot(GSM.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  scale_color_manual(values=c('gray', 'gray35', 'black')) + theme_bw() +
  xlab("Experiment Time (minutes)") + ylab("Mass Concentration") +
  ggtitle("Geosmin") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold")) +
  theme(legend.position="none") 

GSM.TS + MIB.ts

#-----------------------------------------------------------------------------------------------------------------------------------
#Size Distributions
#manual ggplot specs 
sd.plot.colors <- c("Average" = "#000000",
                    "Trial1" = "#1b9e77",
                    "Trial2" = "#d95f02",
                    "Trial3" = "#7570b3",
                    "Trial4" = "#e7298a")
sd.plot.linetype <- c("Calculated" = "dashed",
                      "Measured" = "solid") 

# 2-METHYLISOBORNEOL
MIB1.size <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.MIB1.size <- MIB1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
MIB2.size <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.MIB2.size <- MIB2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
MIB3.size <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.MIB3.size <- MIB3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
MIB4.size <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))
avg.MIB4.size <- MIB4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))

MIB.size.x <- rbind(MIB1.size,MIB2.size,MIB3.size,MIB4.size)
MIB.size.dist <- MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
MIB.size.dist <- MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
MIB.size.dist.tg <- MIB.size.dist %>% filter(Trial != "Trial4") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.MIB.size.x <- rbind(avg.MIB1.size, avg.MIB2.size, avg.MIB3.size)
avg.MIB.size.dist <- avg.MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.MIB.size.dist <- avg.MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.MIB.size.dist <- rbind(avg.MIB.size.dist, MIB.size.dist.tg)

MIB.size.dist <- ggplot(final.MIB.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 2) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("2-MIB SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

# GEOSMIN
GSM1.size <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
avg.GSM1.size <- GSM1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
GSM2.size <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.GSM2.size <- GSM2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
GSM3.size <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.GSM3.size <- GSM3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
GSM4.size <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.GSM4.size <- GSM4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))

GSM.size.x <- rbind(GSM1.size,GSM2.size,GSM3.size,GSM4.size)
GSM.size.dist <- GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
GSM.size.dist <- GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
GSM.size.dist.tg <- GSM.size.dist %>% filter(Trial != "Trialx") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.GSM.size.x <- rbind(avg.GSM2.size, avg.GSM3.size,avg.GSM4.size)
avg.GSM.size.dist <- avg.GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.GSM.size.dist <- avg.GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.GSM.size.dist <- rbind(avg.GSM.size.dist, GSM.size.dist.tg) %>% arrange(desc(Trial))
CMD <- final.GSM.size.dist %>% group_by(Trial) %>%  summarise_if(is.numeric, ~median(na.exclude(.))) 
GSM.Trial.1 <- final.GSM.size.dist %>% filter(Trial == "Trial1")
GSM.Trial.2 <- final.GSM.size.dist %>% filter(Trial == "Trial2")
GSM.Trial.3 <- final.GSM.size.dist %>% filter(Trial == "Trial3")
GSM.Trial.a <- final.GSM.size.dist %>% filter(Trial == "Average")

GSM.size.dist <- ggplot(final.GSM.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 1.25) + 
  #geom_col(GSM.size.dist.tg, aes(x = Bin, y = Count, color = Trial), alpha = 0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("Geosmin SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

GSM.size.dist + MIB.size.dist

ggplot(final.GSM.size.dist) + geom_col(aes(x = Bin, y = Count, fill = Trial), alpha = 0.5) + 
  geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), size = 1.25, alpha = 0.75) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors)  +
  scale_fill_manual(values=sd.plot.colors) +
  ggtitle("Geosmin SOA Size Distribution") + 
  facet_wrap(~ Trial)+ 
  theme_bw() +
  geom_vline(linetype = 5,aes(xintercept=30)) + #here is where to plot x-intercept as Mean Median Diameter
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))


# IMPORTING PAM DATA TO DETERMINE AVERAGE CONDITIONS IN THE OFR -------------------------------------------------------------------------
PAM_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB")
PAM_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM")

for(i in 1:length(PAM_MIB_files)) {                              
  assign(paste0("PAM_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB/", PAM_MIB_files[i])))}
for(i in 1:length(PAM_GSM_files)) {                              
  assign(paste0("PAM_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM/", PAM_GSM_files[i])))}

#MIB PAM Conditions 
PAM1 <- PAM_MIB1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM1a <- PAM1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  PAM1$DateTime >= "2022-11-04 10:00:00" & PAM1$DateTime < "2022-11-04 11:00:00" ~ "O3",
  PAM1$DateTime >= "2022-11-04 11:00:00" & PAM1$DateTime < "2022-11-04 11:030:00" ~ "OH",
  PAM1$DateTime >= "2022-11-04 11:30:00" & PAM1$DateTime < "2022-11-04 12:38:00" ~ "OH+VOC",
  PAM1$DateTime >= "2022-11-04 12:38:00" & PAM1$DateTime < "2022-11-04 14:08:00" ~ "Filter",
  PAM1$DateTime >= "2022-11-04 14:08:00" & PAM1$DateTime <= "2022-11-04 14:38:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM1b <- PAM1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM2 <- PAM_MIB2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM2a <- PAM2 %>% mutate(Treatment = case_when(
  PAM2$DateTime >= "2022-11-07 08:26:00" & PAM2$DateTime < "2022-11-07 09:48:00" ~ "O3",
  PAM2$DateTime >= "2022-11-07 09:48:00" & PAM2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  PAM2$DateTime >= "2022-11-07 11:50:00" & PAM2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  PAM2$DateTime >= "2022-11-07 12:20:00" & PAM2$DateTime < "2022-11-07 13:50:00" ~ "Filter",
  PAM2$DateTime >= "2022-11-07 13:50:00" & PAM2$DateTime <= "2022-11-07 14:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM2b <- PAM2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM3 <- PAM_MIB3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM3a <- PAM3 %>% mutate(Treatment = case_when(
  PAM3$DateTime >= "2022-11-10 9:50:00" & PAM3$DateTime < "2022-11-10 10:22:00" ~ "O3",
  PAM3$DateTime >= "2022-11-10 10:22:00" & PAM3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  PAM3$DateTime >= "2022-11-10 11:03:00" & PAM3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  PAM3$DateTime >= "2022-11-10 11:30:00" & PAM3$DateTime < "2022-11-10 13:00:00" ~ "Filter",
  PAM3$DateTime >= "2022-11-10 13:00:00" & PAM3$DateTime <= "2022-11-10 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM3b <- PAM3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin PAM conditions 
PAM4 <- PAM_GSM1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM4a <- PAM4 %>% mutate(Treatment = case_when(
  PAM4$DateTime >= "2022-11-08 8:56:00" & PAM4$DateTime < "2022-11-08 10:11:00" ~ "O3",
  PAM4$DateTime >= "2022-11-08 10:11:00" & PAM4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  PAM4$DateTime >= "2022-11-08 10:33:00" & PAM4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  PAM4$DateTime >= "2022-11-08 11:05:00" & PAM4$DateTime < "2022-11-08 13:05:00" ~ "Filter",
  PAM4$DateTime >= "2022-11-08 13:05:00" & PAM4$DateTime <= "2022-11-08 13:25:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM4b <- PAM4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM5 <- PAM_GSM2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM5a <- PAM5 %>% mutate(Treatment = case_when(
  PAM5$DateTime >= "2022-11-29 14:22:00" & PAM5$DateTime < "2022-11-29 14:42:00" ~ "O3",
  PAM5$DateTime >= "2022-11-29 14:42:00" & PAM5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  PAM5$DateTime >= "2022-11-29 15:25:00" & PAM5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  PAM5$DateTime >= "2022-11-29 15:50:00" & PAM5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  PAM5$DateTime >= "2022-11-29 17:20:00" & PAM5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM5b <- PAM5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM6 <- PAM_GSM3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM6a <- PAM6 %>% mutate(Treatment = case_when(
  PAM6$DateTime >= "2023-01-30 11:10:00" & PAM6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  PAM6$DateTime >= "2023-01-30 11:40:00" & PAM6$DateTime < "2023-01-30 12:10:00" ~ "OH",
  PAM6$DateTime >= "2023-01-30 12:10:00" & PAM6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  PAM6$DateTime >= "2023-01-30 12:40:00" & PAM6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  PAM6$DateTime >= "2023-01-30 14:10:00" & PAM6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM6b <- PAM6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#ozone data from the PAM -----------------------------------------------------------------------------------------------------------------
OZONE_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB")
OZONE_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM")

for(i in 1:length(OZONE_MIB_files)) {                              
  assign(paste0("OZONE_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB/", OZONE_MIB_files[i])))}
for(i in 1:length(OZONE_GSM_files)) {                              
  assign(paste0("OZONE_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM/", OZONE_GSM_files[i])))}

#2-MIB OZONE CONDITIONS 
OZONE1 <- OZONE_MIB1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE1a <- OZONE1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  OZONE1$DateTime >= "2022-11-04 09:30:00" & OZONE1$DateTime < "2022-11-04 09:54:00" ~ "O3",
  OZONE1$DateTime >= "2022-11-04 10:00:00" & OZONE1$DateTime < "2022-11-04 11:00:00" ~ "OH",
  OZONE1$DateTime >= "2022-11-04 11:00:00" & OZONE1$DateTime < "2022-11-04 12:00:00" ~ "OH+VOC",
  OZONE1$DateTime >= "2022-11-04 12:00:00" & OZONE1$DateTime < "2022-11-04 13:00:00" ~ "Filter",
  OZONE1$DateTime >= "2022-11-04 13:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE1b <- OZONE1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE2 <- OZONE_MIB2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE2a <- OZONE2 %>% mutate(Treatment = case_when(
  OZONE2$DateTime >= "2022-11-07 09:42:00" & OZONE2$DateTime < "2022-11-07 09:49:00" ~ "O3",
  OZONE2$DateTime >= "2022-11-07 09:49:00" & OZONE2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  OZONE2$DateTime >= "2022-11-07 11:50:00" & OZONE2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  OZONE2$DateTime >= "2022-11-07 12:20:00" & OZONE2$DateTime < "2022-11-07 13:45:00" ~ "Filter",
  OZONE2$DateTime >= "2022-11-07 13:45:00" & OZONE2$DateTime <= "2022-11-07 14:09:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE2b <- OZONE2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE3 <- OZONE_MIB3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE3a <- OZONE3 %>% mutate(Treatment = case_when(
  OZONE3$DateTime >= "2022-11-10 10:10:00" & OZONE3$DateTime < "2022-11-10 10:16:00" ~ "O3",
  OZONE3$DateTime >= "2022-11-10 10:22:00" & OZONE3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  OZONE3$DateTime >= "2022-11-10 11:03:00" & OZONE3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  OZONE3$DateTime >= "2022-11-10 11:30:00" & OZONE3$DateTime < "2022-11-10 12:53:00" ~ "Filter",
  OZONE3$DateTime >= "2022-11-10 13:00:00" & OZONE3$DateTime <= "2022-11-10 13:13:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE3b <- OZONE3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin OZONE conditions 
OZONE4 <- OZONE_GSM1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE4a <- OZONE4 %>% mutate(Treatment = case_when(
  OZONE4$DateTime >= "2022-11-08 10:00:00" & OZONE4$DateTime < "2022-11-08 10:04:00" ~ "O3",
  OZONE4$DateTime >= "2022-11-08 10:11:00" & OZONE4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  OZONE4$DateTime >= "2022-11-08 10:33:00" & OZONE4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  OZONE4$DateTime >= "2022-11-08 11:05:00" & OZONE4$DateTime < "2022-11-08 13:00:00" ~ "Filter",
  OZONE4$DateTime >= "2022-11-08 13:05:00" & OZONE4$DateTime <= "2022-11-08 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE4b <- OZONE4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE5 <- OZONE_GSM2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE5a <- OZONE5 %>% mutate(Treatment = case_when(
  OZONE5$DateTime >= "2022-11-29 14:26:00" & OZONE5$DateTime < "2022-11-29 14:37:00" ~ "O3",
  OZONE5$DateTime >= "2022-11-29 14:42:00" & OZONE5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  OZONE5$DateTime >= "2022-11-29 15:25:00" & OZONE5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  OZONE5$DateTime >= "2022-11-29 15:50:00" & OZONE5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  OZONE5$DateTime >= "2022-11-29 17:20:00" & OZONE5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE5b <- OZONE5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE6 <- OZONE_GSM3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE6a <- OZONE6 %>% mutate(Treatment = case_when(
  OZONE6$DateTime >= "2023-01-30 11:10:00" & OZONE6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  OZONE6$DateTime >= "2023-01-30 11:40:00" & OZONE6$DateTime < "2023-01-30 12:09:00" ~ "OH",
  OZONE6$DateTime >= "2023-01-30 12:09:00" & OZONE6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  OZONE6$DateTime >= "2023-01-30 12:40:00" & OZONE6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  OZONE6$DateTime >= "2023-01-30 14:10:00" & OZONE6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE6b <- OZONE6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#combining Ozone and PAM data for easy access 
OFR1 <- left_join(OZONE1b, PAM1b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR2 <- left_join(OZONE2b, PAM2b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR3 <- left_join(OZONE3b, PAM3b, by = "Treatment") %>% mutate(Trial = "Trial3")
OFR4 <- left_join(OZONE4b, PAM4b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR5 <- left_join(OZONE5b, PAM5b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR6 <- left_join(OZONE6b, PAM6b, by = "Treatment") %>% mutate(Trial = "Trial3")

PAM.MIB.summary <- rbind(OFR1,OFR2,OFR3) %>% filter(Treatment == "OH+VOC")
PAM.MIB.summary.1 <- get_summary_stats(PAM.MIB.summary, type = "mean_sd")
PAM.GSM.summary <- rbind(OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.GSM.summary.1 <- get_summary_stats(PAM.GSM.summary, type = "mean_sd")
PAM.all.summary <- rbind(OFR1,OFR2,OFR3,OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.all.summary.1 <- get_summary_stats(PAM.all.summary, type = "mean_sd")





#______________________________________________________________________________________________________________________________________________________
# OLD STUFF 

#finding the geometric standard deviation of GSM and 2-MIB Trial 2
GSM.N <- GSM.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(sum = sum(`Number Concentration`))
MIB.N <- MIB.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(mean = mean(`Number Concentration`))

GSM.2.3 <- GSM.2 %>% dplyr::select(-Date, -Time,-vol, -timepoint) %>% dplyr::select(num, everything()) %>% rename(N = num) %>% pivot_longer(!N, names_to = "di", values_to = "ni") %>% mutate(di = gsub("X","",as.character(di))) %>% mutate(di = as.numeric(di)) %>% mutate(geom = (ni*log(di))) 

numerator<- GSM.2.3 %>% select(geom) %>% summarise(sum(geom))
denominator<- GSM.2.3 %>% select(ni) %>% summarise(sum(ni))

exp(numerator/denominator)

GSM.2.2 <- GSM.2.2 

MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin))

#calculating Count Median Diameter via TSI worksheet
# Dg = geometric mean diameter OR Count Median Diameter (CMD)
# Di = midpoint particle size (each bin's value in the first row)
# ni = number of particles in each bin (value in each bin)
# N = sum of ni (total number of particles)

#first thing is see if num (number concentration is equal to if I add the values for each bin)
GSM.2.x <- GSM.2.trial %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Di", values_to = "ni") %>% group_by(Di)  
bin.sums <- GSM.2.x %>% mutate(Di = gsub("X","",as.character(Di))) %>% mutate(Di = as.numeric(Di)) %>% group_by(timepoint) %>% summarise_at(vars(ni), sum) %>% rename(N = ni)
num.conc <- GSM.2.trial %>% dplyr::select(timepoint, num)
bin.sums.num <- left_join(num.conc, bin.sums, by = "timepoint") #ok for some reason the number concentration, which I assumed would be the same as the sum of each bin, is much lower than the sum of each bin. So I am just going to use the sum of each bin as the actual number concentration.
CMD.df <- left_join(GSM.2.x, bin.sums, by = "timepoint") %>% mutate(ln.ni = log10(ni)) # i also think I need to take the log of ni for the lognormal distribution
CMD.timepoint.1 <- CMD.df %>% filter(timepoint == 1) %>% mutate(Dini = Di^ln.ni)






MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin)) 








=======
rm(list=ls()) #clear environment
setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS") #set working directory 
#load necessary packages
library(dplyr); library(tidyr); library(ggplot2);library(cowplot);library(tidyverse);library(patchwork);library(tidyverse);library(scales); library(rstatix);library(plyr)

MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB")
GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM")

for(i in 1:length(MIB_files)) {                              
  assign(paste0("MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB/", MIB_files[i])))}
for(i in 1:length(GSM_files)) {                              
  assign(paste0("GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM/", GSM_files[i])))}

#Cleaning the data -----------------------------------------------------------------------------------------------------------------------------
#2-METHYLISOBORNEOL TRIALS
MIB1 <- MIB1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB1$timepoint <- 1:nrow(MIB1)
MIB1 <- MIB1 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB1 <- MIB1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB1$timepoint < 62 ~ "O3",
  MIB1$timepoint >= 62 & MIB1$timepoint < 86 ~ "OH",
  MIB1$timepoint >= 86 & MIB1$timepoint < 203 ~ "OH+VOC",
  MIB1$timepoint >= 203 & MIB1$timepoint <= 236 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

MIB2 <- MIB2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB2$timepoint <- 1:nrow(MIB2)
MIB2 <- MIB2 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB2 <- MIB2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB2$timepoint < 80 ~ "O3",
  MIB2$timepoint >= 80 & MIB2$timepoint < 168 ~ "OH",
  MIB2$timepoint >= 168 & MIB2$timepoint < 256 ~ "OH+VOC",
  MIB2$timepoint >= 256 & MIB2$timepoint <= 279 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB3 <- MIB3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB3$timepoint <- 1:nrow(MIB3)
MIB3 <- MIB3 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB3 <- MIB3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB3$timepoint < 38 ~ "O3",
  MIB3$timepoint >= 38 & MIB3$timepoint < 71 ~ "OH",
  MIB3$timepoint >= 71 & MIB3$timepoint < 156 ~ "OH+VOC",
  MIB3$timepoint >= 156 & MIB3$timepoint <= 180 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB4 <- MIB4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB4$timepoint <- 1:nrow(MIB4)
MIB4 <- MIB4 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB4 <- MIB4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB4$timepoint < 32 ~ "O3",
  MIB4$timepoint >= 32 & MIB4$timepoint < 56 ~ "OH",
  MIB4$timepoint >= 56 & MIB4$timepoint < 147 ~ "OH+VOC",
  MIB4$timepoint >= 147 & MIB4$timepoint <= 183 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

#GEOSMIN TRIALS
GSM1 <- GSM1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM1$timepoint <- 1:nrow(GSM1)
GSM1 <- GSM1 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM1 <- GSM1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM1$timepoint < 74 ~ "O3",
  GSM1$timepoint >= 74 & GSM1$timepoint < 162 ~ "OH",
  GSM1$timepoint >= 162 & GSM1$timepoint < 245 ~ "OH+VOC",
  GSM1$timepoint >= 245 & GSM1$timepoint <= 282 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

GSM2 <- GSM2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM2$timepoint <- 1:nrow(GSM2)
GSM2 <- GSM2 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM2 <- GSM2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM2$timepoint < 43 ~ "O3",
  GSM2$timepoint >= 43 & GSM2$timepoint < 60 ~ "OH",
  GSM2$timepoint >= 60 & GSM2$timepoint < 173 ~ "OH+VOC",
  GSM2$timepoint >= 173 & GSM2$timepoint <= 197 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM3 <- GSM3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM3$timepoint <- 1:nrow(GSM3)
GSM3 <- GSM3 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM3 <- GSM3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM3$timepoint < 34 ~ "O3",
  GSM3$timepoint >= 34 & GSM3$timepoint < 70 ~ "OH",
  GSM3$timepoint >= 70 & GSM3$timepoint < 152 ~ "OH+VOC",
  GSM3$timepoint >= 152 & GSM3$timepoint <= 173 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM4 <- GSM4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM4$timepoint <- 1:nrow(GSM4)
GSM4 <- GSM4 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM4 <- GSM4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM4$timepoint < 62 ~ "O3",
  GSM4$timepoint >= 62 & GSM4$timepoint < 84 ~ "OH",
  GSM4$timepoint >= 84 & GSM4$timepoint < 174 ~ "OH+VOC",
  GSM4$timepoint >= 174 & GSM4$timepoint <= 216 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

#Calculating average number concentration and mass concentrations ------------------------------------------------------------------------------------
#MIB Trial 1
MIB1.mass <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB1.mass.conc <- MIB1.mass-MIB1.bg
MIB1.num <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB1.num.conc <- MIB1.num-MIB1.bg

#MIB Trial 2
MIB2.mass <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB2.mass.conc <- MIB2.mass-MIB2.bg
MIB2.num <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB2.num.conc <- MIB2.num-MIB2.bg

#MIB Trial 3
MIB3.mass <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB3.mass.conc <- MIB3.mass-MIB3.bg
MIB3.num <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB3.num.conc <- MIB3.num-MIB3.bg

#MIB Trial 4
MIB4.mass <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB4.mass.conc <- MIB4.mass-MIB4.bg
MIB4.num <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB4.num.conc <- MIB4.num-MIB4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
MIB.trials.num <- cbind(MIB1.num.conc,MIB2.num.conc,MIB3.num.conc)
MIB.trials.num.t <- as.data.frame(t(MIB.trials.num))
colnames(MIB.trials.num.t) <- c("Number Concentration")
MIB.Number.Concentration <- get_summary_stats(MIB.trials.num.t, type = "mean_sd")

# # concentration background
MIB.bg.num <- cbind(MIB1.bg,MIB2.bg,MIB3.bg)
MIB.bg.num.t <- as.data.frame(t(MIB.bg.num))
colnames(MIB.bg.num.t) <- c("Number Concentration")
MIB.bg.Concentration <- get_summary_stats(MIB.bg.num.t, type = "mean_sd")

MIB.trials.mass <- cbind(MIB1.mass.conc,MIB2.mass.conc,MIB3.mass.conc)
MIB.trials.mass.t <- as.data.frame(t(MIB.trials.mass))
colnames(MIB.trials.mass.t) <- c("Mass Concentration")
MIB.Mass.Concentration <- get_summary_stats(MIB.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
MIB1.1 <- MIB1 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 65) %>% filter(timepoint < 86 | timepoint > 96) %>% filter(timepoint < 177 | timepoint > 209) %>% filter(timepoint < 230) #filtering by timepoint here is to remove transitional measurements and also try and normalize # of measurements per treatment between trial
MIB1.1 <- MIB1.1 %>%mutate(timepoint = 1:nrow(MIB1.1))

MIB2.1 <- MIB2 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 90) %>% filter(timepoint < 111 | timepoint > 173) %>% filter(timepoint < 254 | timepoint > 258) %>% filter(timepoint < 279) 
MIB2.1 <- MIB2.1 %>% mutate(timepoint = 1:nrow(MIB2.1))

MIB3.1 <- MIB3 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint < 18 | timepoint > 33) %>% filter(timepoint < 37 | timepoint > 42) %>% filter(timepoint < 63 | timepoint > 73) %>% filter(timepoint < 154 | timepoint > 159) %>% filter(timepoint < 180) 
MIB3.1 <- MIB3.1 %>% mutate(timepoint = 1:nrow(MIB3.1))

MIB4.1 <- MIB4 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 10) %>% filter(timepoint < 31 | timepoint > 34) %>% filter(timepoint < 55 | timepoint > 61) %>% filter(timepoint < 142 | timepoint > 156) %>% filter(timepoint < 177) 
MIB4.1 <- MIB4.1 %>% mutate(timepoint = 1:nrow(MIB4.1))

# --------------------------------------------------------------------------------------------------------------------------------
#GSM Trial 1
GSM1.mass <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM1.mass.conc <- GSM1.mass-GSM1.bg
GSM1.num <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM1.num.conc <- GSM1.num-GSM1.bg

#GSM Trial 2
GSM2.mass <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM2.mass.conc <- GSM2.mass-GSM2.bg
GSM2.num <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM2.num.conc <- GSM2.num-GSM2.bg

#GSM Trial 3
GSM3.mass <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM3.mass.conc <- GSM3.mass-GSM3.bg
GSM3.num <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM3.num.conc <- GSM3.num-GSM3.bg

#GSM Trial 4
GSM4.mass <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM4.mass.conc <- GSM4.mass-GSM4.bg
GSM4.num <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM4.num.conc <- GSM4.num-GSM4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
GSM.trials.num <- cbind(GSM2.num.conc,GSM3.num.conc,GSM4.num.conc)
GSM.trials.num.t <- as.data.frame(t(GSM.trials.num))
colnames(GSM.trials.num.t) <- c("Number Concentration")
GSM.Number.Concentration <- get_summary_stats(GSM.trials.num.t, type = "mean_sd")
# # concentration background
GSM.bg.num <- cbind(GSM2.bg,GSM3.bg,GSM4.bg)
GSM.bg.num.t <- as.data.frame(t(GSM.bg.num))
colnames(GSM.bg.num.t) <- c("Number Concentration")
GSM.bg.Concentration <- get_summary_stats(GSM.bg.num.t, type = "mean_sd")

GSM.trials.mass <- cbind(GSM2.mass.conc,GSM3.mass.conc,GSM4.mass.conc)
GSM.trials.mass.t <- as.data.frame(t(GSM.trials.mass))
colnames(GSM.trials.mass.t) <- c("Mass Concentration")
GSM.Mass.Concentration <- get_summary_stats(GSM.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
GSM1.1 <- GSM1 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 21 | timepoint > 75) %>% filter(timepoint < 94 | timepoint > 163) %>% filter(timepoint < 243 | timepoint > 262) %>% filter(timepoint < 284) 
GSM1.1 <- GSM1.1 %>% mutate(timepoint = 1:nrow(GSM1.1))

GSM2.1 <- GSM2 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 12 | timepoint > 45) %>% filter(timepoint < 59 | timepoint > 75) %>% filter(timepoint < 155 | timepoint > 177) %>% filter(timepoint < 197) 
timepoint <- c(12,13,14,15,16,17,18)
tp.df <- as.data.frame(timepoint)
GSM2.x <- rbind.fill(GSM2.1, tp.df) %>% arrange(timepoint)
GSM2.1 <- GSM2.x %>% mutate(timepoint = 1:nrow(GSM2.x))

GSM3.1 <- GSM3 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 13) %>% filter(timepoint < 34 | timepoint > 45) %>% filter(timepoint < 66 | timepoint > 71) %>% filter(timepoint < 152| timepoint > 153) %>% filter(timepoint < 174) 
GSM3.1 <- GSM3.1 %>% mutate(timepoint = 1:nrow(GSM3.1))

GSM4.1 <- GSM4 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 40) %>% filter(timepoint < 61 | timepoint > 62) %>% filter(timepoint < 82| timepoint > 89) %>% filter(timepoint < 169 | timepoint > 197) %>% filter(timepoint < 218) 
GSM4.1 <- GSM4.1 %>% mutate(timepoint = 1:nrow(GSM4.1))

# AEROSOL NUMBER AND MASS CONCENTRATIONS AND TIME SERIES PLOT ------------------------------------------------------------
#MIB
MIB.trials <- right_join(MIB1.1, MIB2.1, by =c('timepoint')) %>% left_join(MIB3.1, by =c('timepoint')) %>% left_join(MIB4.1, by =c('timepoint'))
MIB.Trials.x <- rbind(MIB1.1, MIB2.1, MIB3.1)
MIB.aerosol.summary.stats <- MIB.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "2MIB")

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

#PLOTS 
#MIB
MIB.ts <- ggplot(MIB.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() + scale_color_manual(values=c('gray', 'gray35', 'black')) + xlab("Experiment Time (minutes)") + ylab("Mass Concentration") + ggtitle("2-Methylisoborneol") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold"))

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

GSM.TS <- ggplot(GSM.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  scale_color_manual(values=c('gray', 'gray35', 'black')) + theme_bw() +
  xlab("Experiment Time (minutes)") + ylab("Mass Concentration") +
  ggtitle("Geosmin") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold")) +
  theme(legend.position="none") 

GSM.TS + MIB.ts

#-----------------------------------------------------------------------------------------------------------------------------------
#Size Distributions
#manual ggplot specs 
sd.plot.colors <- c("Average" = "#000000",
                    "Trial1" = "#1b9e77",
                    "Trial2" = "#d95f02",
                    "Trial3" = "#7570b3",
                    "Trial4" = "#e7298a")
sd.plot.linetype <- c("Calculated" = "dashed",
                      "Measured" = "solid") 

# 2-METHYLISOBORNEOL
MIB1.size <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.MIB1.size <- MIB1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
MIB2.size <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.MIB2.size <- MIB2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
MIB3.size <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.MIB3.size <- MIB3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
MIB4.size <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))
avg.MIB4.size <- MIB4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))

MIB.size.x <- rbind(MIB1.size,MIB2.size,MIB3.size,MIB4.size)
MIB.size.dist <- MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
MIB.size.dist <- MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
MIB.size.dist.tg <- MIB.size.dist %>% filter(Trial != "Trial4") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.MIB.size.x <- rbind(avg.MIB1.size, avg.MIB2.size, avg.MIB3.size)
avg.MIB.size.dist <- avg.MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.MIB.size.dist <- avg.MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.MIB.size.dist <- rbind(avg.MIB.size.dist, MIB.size.dist.tg)

MIB.size.dist <- ggplot(final.MIB.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 2) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("2-MIB SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

# GEOSMIN
GSM1.size <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
avg.GSM1.size <- GSM1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
GSM2.size <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.GSM2.size <- GSM2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
GSM3.size <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.GSM3.size <- GSM3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
GSM4.size <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.GSM4.size <- GSM4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))

GSM.size.x <- rbind(GSM1.size,GSM2.size,GSM3.size,GSM4.size)
GSM.size.dist <- GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
GSM.size.dist <- GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
GSM.size.dist.tg <- GSM.size.dist %>% filter(Trial != "Trialx") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.GSM.size.x <- rbind(avg.GSM2.size, avg.GSM3.size,avg.GSM4.size)
avg.GSM.size.dist <- avg.GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.GSM.size.dist <- avg.GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.GSM.size.dist <- rbind(avg.GSM.size.dist, GSM.size.dist.tg) %>% arrange(desc(Trial))
CMD <- final.GSM.size.dist %>% group_by(Trial) %>%  summarise_if(is.numeric, ~median(na.exclude(.))) 
GSM.Trial.1 <- final.GSM.size.dist %>% filter(Trial == "Trial1")
GSM.Trial.2 <- final.GSM.size.dist %>% filter(Trial == "Trial2")
GSM.Trial.3 <- final.GSM.size.dist %>% filter(Trial == "Trial3")
GSM.Trial.a <- final.GSM.size.dist %>% filter(Trial == "Average")

GSM.size.dist <- ggplot(final.GSM.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 1.25) + 
  #geom_col(GSM.size.dist.tg, aes(x = Bin, y = Count, color = Trial), alpha = 0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("Geosmin SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

GSM.size.dist + MIB.size.dist

ggplot(final.GSM.size.dist) + geom_col(aes(x = Bin, y = Count, fill = Trial), alpha = 0.5) + 
  geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), size = 1.25, alpha = 0.75) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors)  +
  scale_fill_manual(values=sd.plot.colors) +
  ggtitle("Geosmin SOA Size Distribution") + 
  facet_wrap(~ Trial)+ 
  theme_bw() +
  geom_vline(linetype = 5,aes(xintercept=30)) + #here is where to plot x-intercept as Mean Median Diameter
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))


# IMPORTING PAM DATA TO DETERMINE AVERAGE CONDITIONS IN THE OFR -------------------------------------------------------------------------
PAM_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB")
PAM_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM")

for(i in 1:length(PAM_MIB_files)) {                              
  assign(paste0("PAM_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB/", PAM_MIB_files[i])))}
for(i in 1:length(PAM_GSM_files)) {                              
  assign(paste0("PAM_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM/", PAM_GSM_files[i])))}

#MIB PAM Conditions 
PAM1 <- PAM_MIB1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM1a <- PAM1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  PAM1$DateTime >= "2022-11-04 10:00:00" & PAM1$DateTime < "2022-11-04 11:00:00" ~ "O3",
  PAM1$DateTime >= "2022-11-04 11:00:00" & PAM1$DateTime < "2022-11-04 11:030:00" ~ "OH",
  PAM1$DateTime >= "2022-11-04 11:30:00" & PAM1$DateTime < "2022-11-04 12:38:00" ~ "OH+VOC",
  PAM1$DateTime >= "2022-11-04 12:38:00" & PAM1$DateTime < "2022-11-04 14:08:00" ~ "Filter",
  PAM1$DateTime >= "2022-11-04 14:08:00" & PAM1$DateTime <= "2022-11-04 14:38:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM1b <- PAM1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM2 <- PAM_MIB2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM2a <- PAM2 %>% mutate(Treatment = case_when(
  PAM2$DateTime >= "2022-11-07 08:26:00" & PAM2$DateTime < "2022-11-07 09:48:00" ~ "O3",
  PAM2$DateTime >= "2022-11-07 09:48:00" & PAM2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  PAM2$DateTime >= "2022-11-07 11:50:00" & PAM2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  PAM2$DateTime >= "2022-11-07 12:20:00" & PAM2$DateTime < "2022-11-07 13:50:00" ~ "Filter",
  PAM2$DateTime >= "2022-11-07 13:50:00" & PAM2$DateTime <= "2022-11-07 14:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM2b <- PAM2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM3 <- PAM_MIB3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM3a <- PAM3 %>% mutate(Treatment = case_when(
  PAM3$DateTime >= "2022-11-10 9:50:00" & PAM3$DateTime < "2022-11-10 10:22:00" ~ "O3",
  PAM3$DateTime >= "2022-11-10 10:22:00" & PAM3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  PAM3$DateTime >= "2022-11-10 11:03:00" & PAM3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  PAM3$DateTime >= "2022-11-10 11:30:00" & PAM3$DateTime < "2022-11-10 13:00:00" ~ "Filter",
  PAM3$DateTime >= "2022-11-10 13:00:00" & PAM3$DateTime <= "2022-11-10 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM3b <- PAM3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin PAM conditions 
PAM4 <- PAM_GSM1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM4a <- PAM4 %>% mutate(Treatment = case_when(
  PAM4$DateTime >= "2022-11-08 8:56:00" & PAM4$DateTime < "2022-11-08 10:11:00" ~ "O3",
  PAM4$DateTime >= "2022-11-08 10:11:00" & PAM4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  PAM4$DateTime >= "2022-11-08 10:33:00" & PAM4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  PAM4$DateTime >= "2022-11-08 11:05:00" & PAM4$DateTime < "2022-11-08 13:05:00" ~ "Filter",
  PAM4$DateTime >= "2022-11-08 13:05:00" & PAM4$DateTime <= "2022-11-08 13:25:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM4b <- PAM4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM5 <- PAM_GSM2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM5a <- PAM5 %>% mutate(Treatment = case_when(
  PAM5$DateTime >= "2022-11-29 14:22:00" & PAM5$DateTime < "2022-11-29 14:42:00" ~ "O3",
  PAM5$DateTime >= "2022-11-29 14:42:00" & PAM5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  PAM5$DateTime >= "2022-11-29 15:25:00" & PAM5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  PAM5$DateTime >= "2022-11-29 15:50:00" & PAM5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  PAM5$DateTime >= "2022-11-29 17:20:00" & PAM5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM5b <- PAM5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM6 <- PAM_GSM3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM6a <- PAM6 %>% mutate(Treatment = case_when(
  PAM6$DateTime >= "2023-01-30 11:10:00" & PAM6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  PAM6$DateTime >= "2023-01-30 11:40:00" & PAM6$DateTime < "2023-01-30 12:10:00" ~ "OH",
  PAM6$DateTime >= "2023-01-30 12:10:00" & PAM6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  PAM6$DateTime >= "2023-01-30 12:40:00" & PAM6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  PAM6$DateTime >= "2023-01-30 14:10:00" & PAM6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM6b <- PAM6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#ozone data from the PAM -----------------------------------------------------------------------------------------------------------------
OZONE_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB")
OZONE_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM")

for(i in 1:length(OZONE_MIB_files)) {                              
  assign(paste0("OZONE_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB/", OZONE_MIB_files[i])))}
for(i in 1:length(OZONE_GSM_files)) {                              
  assign(paste0("OZONE_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM/", OZONE_GSM_files[i])))}

#2-MIB OZONE CONDITIONS 
OZONE1 <- OZONE_MIB1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE1a <- OZONE1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  OZONE1$DateTime >= "2022-11-04 09:30:00" & OZONE1$DateTime < "2022-11-04 09:54:00" ~ "O3",
  OZONE1$DateTime >= "2022-11-04 10:00:00" & OZONE1$DateTime < "2022-11-04 11:00:00" ~ "OH",
  OZONE1$DateTime >= "2022-11-04 11:00:00" & OZONE1$DateTime < "2022-11-04 12:00:00" ~ "OH+VOC",
  OZONE1$DateTime >= "2022-11-04 12:00:00" & OZONE1$DateTime < "2022-11-04 13:00:00" ~ "Filter",
  OZONE1$DateTime >= "2022-11-04 13:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE1b <- OZONE1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE2 <- OZONE_MIB2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE2a <- OZONE2 %>% mutate(Treatment = case_when(
  OZONE2$DateTime >= "2022-11-07 09:42:00" & OZONE2$DateTime < "2022-11-07 09:49:00" ~ "O3",
  OZONE2$DateTime >= "2022-11-07 09:49:00" & OZONE2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  OZONE2$DateTime >= "2022-11-07 11:50:00" & OZONE2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  OZONE2$DateTime >= "2022-11-07 12:20:00" & OZONE2$DateTime < "2022-11-07 13:45:00" ~ "Filter",
  OZONE2$DateTime >= "2022-11-07 13:45:00" & OZONE2$DateTime <= "2022-11-07 14:09:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE2b <- OZONE2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE3 <- OZONE_MIB3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE3a <- OZONE3 %>% mutate(Treatment = case_when(
  OZONE3$DateTime >= "2022-11-10 10:10:00" & OZONE3$DateTime < "2022-11-10 10:16:00" ~ "O3",
  OZONE3$DateTime >= "2022-11-10 10:22:00" & OZONE3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  OZONE3$DateTime >= "2022-11-10 11:03:00" & OZONE3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  OZONE3$DateTime >= "2022-11-10 11:30:00" & OZONE3$DateTime < "2022-11-10 12:53:00" ~ "Filter",
  OZONE3$DateTime >= "2022-11-10 13:00:00" & OZONE3$DateTime <= "2022-11-10 13:13:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE3b <- OZONE3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin OZONE conditions 
OZONE4 <- OZONE_GSM1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE4a <- OZONE4 %>% mutate(Treatment = case_when(
  OZONE4$DateTime >= "2022-11-08 10:00:00" & OZONE4$DateTime < "2022-11-08 10:04:00" ~ "O3",
  OZONE4$DateTime >= "2022-11-08 10:11:00" & OZONE4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  OZONE4$DateTime >= "2022-11-08 10:33:00" & OZONE4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  OZONE4$DateTime >= "2022-11-08 11:05:00" & OZONE4$DateTime < "2022-11-08 13:00:00" ~ "Filter",
  OZONE4$DateTime >= "2022-11-08 13:05:00" & OZONE4$DateTime <= "2022-11-08 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE4b <- OZONE4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE5 <- OZONE_GSM2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE5a <- OZONE5 %>% mutate(Treatment = case_when(
  OZONE5$DateTime >= "2022-11-29 14:26:00" & OZONE5$DateTime < "2022-11-29 14:37:00" ~ "O3",
  OZONE5$DateTime >= "2022-11-29 14:42:00" & OZONE5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  OZONE5$DateTime >= "2022-11-29 15:25:00" & OZONE5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  OZONE5$DateTime >= "2022-11-29 15:50:00" & OZONE5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  OZONE5$DateTime >= "2022-11-29 17:20:00" & OZONE5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE5b <- OZONE5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE6 <- OZONE_GSM3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE6a <- OZONE6 %>% mutate(Treatment = case_when(
  OZONE6$DateTime >= "2023-01-30 11:10:00" & OZONE6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  OZONE6$DateTime >= "2023-01-30 11:40:00" & OZONE6$DateTime < "2023-01-30 12:09:00" ~ "OH",
  OZONE6$DateTime >= "2023-01-30 12:09:00" & OZONE6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  OZONE6$DateTime >= "2023-01-30 12:40:00" & OZONE6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  OZONE6$DateTime >= "2023-01-30 14:10:00" & OZONE6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE6b <- OZONE6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#combining Ozone and PAM data for easy access 
OFR1 <- left_join(OZONE1b, PAM1b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR2 <- left_join(OZONE2b, PAM2b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR3 <- left_join(OZONE3b, PAM3b, by = "Treatment") %>% mutate(Trial = "Trial3")
OFR4 <- left_join(OZONE4b, PAM4b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR5 <- left_join(OZONE5b, PAM5b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR6 <- left_join(OZONE6b, PAM6b, by = "Treatment") %>% mutate(Trial = "Trial3")

PAM.MIB.summary <- rbind(OFR1,OFR2,OFR3) %>% filter(Treatment == "OH+VOC")
PAM.MIB.summary.1 <- get_summary_stats(PAM.MIB.summary, type = "mean_sd")
PAM.GSM.summary <- rbind(OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.GSM.summary.1 <- get_summary_stats(PAM.GSM.summary, type = "mean_sd")
PAM.all.summary <- rbind(OFR1,OFR2,OFR3,OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.all.summary.1 <- get_summary_stats(PAM.all.summary, type = "mean_sd")





#______________________________________________________________________________________________________________________________________________________
# OLD STUFF 

#finding the geometric standard deviation of GSM and 2-MIB Trial 2
GSM.N <- GSM.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(sum = sum(`Number Concentration`))
MIB.N <- MIB.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(mean = mean(`Number Concentration`))

GSM.2.3 <- GSM.2 %>% dplyr::select(-Date, -Time,-vol, -timepoint) %>% dplyr::select(num, everything()) %>% rename(N = num) %>% pivot_longer(!N, names_to = "di", values_to = "ni") %>% mutate(di = gsub("X","",as.character(di))) %>% mutate(di = as.numeric(di)) %>% mutate(geom = (ni*log(di))) 

numerator<- GSM.2.3 %>% select(geom) %>% summarise(sum(geom))
denominator<- GSM.2.3 %>% select(ni) %>% summarise(sum(ni))

exp(numerator/denominator)

GSM.2.2 <- GSM.2.2 

MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin))

#calculating Count Median Diameter via TSI worksheet
# Dg = geometric mean diameter OR Count Median Diameter (CMD)
# Di = midpoint particle size (each bin's value in the first row)
# ni = number of particles in each bin (value in each bin)
# N = sum of ni (total number of particles)

#first thing is see if num (number concentration is equal to if I add the values for each bin)
GSM.2.x <- GSM.2.trial %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Di", values_to = "ni") %>% group_by(Di)  
bin.sums <- GSM.2.x %>% mutate(Di = gsub("X","",as.character(Di))) %>% mutate(Di = as.numeric(Di)) %>% group_by(timepoint) %>% summarise_at(vars(ni), sum) %>% rename(N = ni)
num.conc <- GSM.2.trial %>% dplyr::select(timepoint, num)
bin.sums.num <- left_join(num.conc, bin.sums, by = "timepoint") #ok for some reason the number concentration, which I assumed would be the same as the sum of each bin, is much lower than the sum of each bin. So I am just going to use the sum of each bin as the actual number concentration.
CMD.df <- left_join(GSM.2.x, bin.sums, by = "timepoint") %>% mutate(ln.ni = log10(ni)) # i also think I need to take the log of ni for the lognormal distribution
CMD.timepoint.1 <- CMD.df %>% filter(timepoint == 1) %>% mutate(Dini = Di^ln.ni)






MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin)) 








>>>>>>> 8ef23a88212bc703a7aa13ecd159d6aaf9a6fa76
=======
rm(list=ls()) #clear environment
setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS") #set working directory 
#load necessary packages
library(dplyr); library(tidyr); library(ggplot2);library(cowplot);library(tidyverse);library(patchwork);library(tidyverse);library(scales); library(rstatix);library(plyr)

MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB")
GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM")

for(i in 1:length(MIB_files)) {                              
  assign(paste0("MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/MIB/", MIB_files[i])))}
for(i in 1:length(GSM_files)) {                              
  assign(paste0("GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/SEMS/GSM/", GSM_files[i])))}
# DID THIS SHOW UP IN GITHUB??
#Cleaning the data -----------------------------------------------------------------------------------------------------------------------------
#2-METHYLISOBORNEOL TRIALS
MIB1 <- MIB1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB1$timepoint <- 1:nrow(MIB1)
MIB1 <- MIB1 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB1 <- MIB1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB1$timepoint < 62 ~ "O3",
  MIB1$timepoint >= 62 & MIB1$timepoint < 86 ~ "OH",
  MIB1$timepoint >= 86 & MIB1$timepoint < 203 ~ "OH+VOC",
  MIB1$timepoint >= 203 & MIB1$timepoint <= 236 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

MIB2 <- MIB2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB2$timepoint <- 1:nrow(MIB2)
MIB2 <- MIB2 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB2 <- MIB2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB2$timepoint < 80 ~ "O3",
  MIB2$timepoint >= 80 & MIB2$timepoint < 168 ~ "OH",
  MIB2$timepoint >= 168 & MIB2$timepoint < 256 ~ "OH+VOC",
  MIB2$timepoint >= 256 & MIB2$timepoint <= 279 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB3 <- MIB3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB3$timepoint <- 1:nrow(MIB3)
MIB3 <- MIB3 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB3 <- MIB3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB3$timepoint < 38 ~ "O3",
  MIB3$timepoint >= 38 & MIB3$timepoint < 71 ~ "OH",
  MIB3$timepoint >= 71 & MIB3$timepoint < 156 ~ "OH+VOC",
  MIB3$timepoint >= 156 & MIB3$timepoint <= 180 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

MIB4 <- MIB4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
MIB4$timepoint <- 1:nrow(MIB4)
MIB4 <- MIB4 %>% dplyr::select(StartDate, Time, timepoint, everything())
MIB4 <- MIB4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  MIB4$timepoint < 32 ~ "O3",
  MIB4$timepoint >= 32 & MIB4$timepoint < 56 ~ "OH",
  MIB4$timepoint >= 56 & MIB4$timepoint < 147 ~ "OH+VOC",
  MIB4$timepoint >= 147 & MIB4$timepoint <= 183 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

#GEOSMIN TRIALS
GSM1 <- GSM1 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM1$timepoint <- 1:nrow(GSM1)
GSM1 <- GSM1 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM1 <- GSM1 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM1$timepoint < 74 ~ "O3",
  GSM1$timepoint >= 74 & GSM1$timepoint < 162 ~ "OH",
  GSM1$timepoint >= 162 & GSM1$timepoint < 245 ~ "OH+VOC",
  GSM1$timepoint >= 245 & GSM1$timepoint <= 282 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything())

GSM2 <- GSM2 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM2$timepoint <- 1:nrow(GSM2)
GSM2 <- GSM2 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM2 <- GSM2 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM2$timepoint < 43 ~ "O3",
  GSM2$timepoint >= 43 & GSM2$timepoint < 60 ~ "OH",
  GSM2$timepoint >= 60 & GSM2$timepoint < 173 ~ "OH+VOC",
  GSM2$timepoint >= 173 & GSM2$timepoint <= 197 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM3 <- GSM3 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM3$timepoint <- 1:nrow(GSM3)
GSM3 <- GSM3 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM3 <- GSM3 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM3$timepoint < 34 ~ "O3",
  GSM3$timepoint >= 34 & GSM3$timepoint < 70 ~ "OH",
  GSM3$timepoint >= 70 & GSM3$timepoint < 152 ~ "OH+VOC",
  GSM3$timepoint >= 152 & GSM3$timepoint <= 173 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

GSM4 <- GSM4 %>% mutate(StartTime = as.POSIXct(StartTime, format = "%H:%M:%S")) %>% tidyr::separate(col = StartTime, into = c("Date", "Time"), sep = " ", convert = TRUE) %>% dplyr::select(-Date)
GSM4$timepoint <- 1:nrow(GSM4)
GSM4 <- GSM4 %>% dplyr::select(StartDate, Time, timepoint, everything())
GSM4 <- GSM4 %>% mutate(Treatment = case_when( #change the following timepoints specific to the run 
  GSM4$timepoint < 62 ~ "O3",
  GSM4$timepoint >= 62 & GSM4$timepoint < 84 ~ "OH",
  GSM4$timepoint >= 84 & GSM4$timepoint < 174 ~ "OH+VOC",
  GSM4$timepoint >= 174 & GSM4$timepoint <= 216 ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, StartDate, Time, timepoint, everything()) 

#Calculating average number concentration and mass concentrations ------------------------------------------------------------------------------------
#MIB Trial 1
MIB1.mass <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB1.mass.conc <- MIB1.mass-MIB1.bg
MIB1.num <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB1.bg <- MIB1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB1.num.conc <- MIB1.num-MIB1.bg

#MIB Trial 2
MIB2.mass <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB2.mass.conc <- MIB2.mass-MIB2.bg
MIB2.num <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB2.bg <- MIB2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB2.num.conc <- MIB2.num-MIB2.bg

#MIB Trial 3
MIB3.mass <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB3.mass.conc <- MIB3.mass-MIB3.bg
MIB3.num <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB3.bg <- MIB3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB3.num.conc <- MIB3.num-MIB3.bg

#MIB Trial 4
MIB4.mass <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
MIB4.mass.conc <- MIB4.mass-MIB4.bg
MIB4.num <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
MIB4.bg <- MIB4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
MIB4.num.conc <- MIB4.num-MIB4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
MIB.trials.num <- cbind(MIB1.num.conc,MIB2.num.conc,MIB3.num.conc)
MIB.trials.num.t <- as.data.frame(t(MIB.trials.num))
colnames(MIB.trials.num.t) <- c("Number Concentration")
MIB.Number.Concentration <- get_summary_stats(MIB.trials.num.t, type = "mean_sd")

# # concentration background
MIB.bg.num <- cbind(MIB1.bg,MIB2.bg,MIB3.bg)
MIB.bg.num.t <- as.data.frame(t(MIB.bg.num))
colnames(MIB.bg.num.t) <- c("Number Concentration")
MIB.bg.Concentration <- get_summary_stats(MIB.bg.num.t, type = "mean_sd")

MIB.trials.mass <- cbind(MIB1.mass.conc,MIB2.mass.conc,MIB3.mass.conc)
MIB.trials.mass.t <- as.data.frame(t(MIB.trials.mass))
colnames(MIB.trials.mass.t) <- c("Mass Concentration")
MIB.Mass.Concentration <- get_summary_stats(MIB.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
MIB1.1 <- MIB1 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 65) %>% filter(timepoint < 86 | timepoint > 96) %>% filter(timepoint < 177 | timepoint > 209) %>% filter(timepoint < 230) #filtering by timepoint here is to remove transitional measurements and also try and normalize # of measurements per treatment between trial
MIB1.1 <- MIB1.1 %>%mutate(timepoint = 1:nrow(MIB1.1))

MIB2.1 <- MIB2 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 19) %>% filter(timepoint < 40 | timepoint > 90) %>% filter(timepoint < 111 | timepoint > 173) %>% filter(timepoint < 254 | timepoint > 258) %>% filter(timepoint < 279) 
MIB2.1 <- MIB2.1 %>% mutate(timepoint = 1:nrow(MIB2.1))

MIB3.1 <- MIB3 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint < 18 | timepoint > 33) %>% filter(timepoint < 37 | timepoint > 42) %>% filter(timepoint < 63 | timepoint > 73) %>% filter(timepoint < 154 | timepoint > 159) %>% filter(timepoint < 180) 
MIB3.1 <- MIB3.1 %>% mutate(timepoint = 1:nrow(MIB3.1))

MIB4.1 <- MIB4 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 10) %>% filter(timepoint < 31 | timepoint > 34) %>% filter(timepoint < 55 | timepoint > 61) %>% filter(timepoint < 142 | timepoint > 156) %>% filter(timepoint < 177) 
MIB4.1 <- MIB4.1 %>% mutate(timepoint = 1:nrow(MIB4.1))

# --------------------------------------------------------------------------------------------------------------------------------
#GSM Trial 1
GSM1.mass <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM1.mass.conc <- GSM1.mass-GSM1.bg
GSM1.num <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM1.bg <- GSM1 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM1.num.conc <- GSM1.num-GSM1.bg

#GSM Trial 2
GSM2.mass <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM2.mass.conc <- GSM2.mass-GSM2.bg
GSM2.num <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM2.bg <- GSM2 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM2.num.conc <- GSM2.num-GSM2.bg

#GSM Trial 3
GSM3.mass <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM3.mass.conc <- GSM3.mass-GSM3.bg
GSM3.num <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM3.bg <- GSM3 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM3.num.conc <- GSM3.num-GSM3.bg

#GSM Trial 4
GSM4.mass <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanMass), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanMass), mean)
GSM4.mass.conc <- GSM4.mass-GSM4.bg
GSM4.num <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% summarise_at(vars(TotScanConc), mean)
GSM4.bg <- GSM4 %>% dplyr::filter(Treatment == 'OH') %>% summarise_at(vars(TotScanConc), mean)
GSM4.num.conc <- GSM4.num-GSM4.bg

# Number and Mass concentrations ----------------------------------------------------------------------------------------------------
GSM.trials.num <- cbind(GSM2.num.conc,GSM3.num.conc,GSM4.num.conc)
GSM.trials.num.t <- as.data.frame(t(GSM.trials.num))
colnames(GSM.trials.num.t) <- c("Number Concentration")
GSM.Number.Concentration <- get_summary_stats(GSM.trials.num.t, type = "mean_sd")
# # concentration background
GSM.bg.num <- cbind(GSM2.bg,GSM3.bg,GSM4.bg)
GSM.bg.num.t <- as.data.frame(t(GSM.bg.num))
colnames(GSM.bg.num.t) <- c("Number Concentration")
GSM.bg.Concentration <- get_summary_stats(GSM.bg.num.t, type = "mean_sd")

GSM.trials.mass <- cbind(GSM2.mass.conc,GSM3.mass.conc,GSM4.mass.conc)
GSM.trials.mass.t <- as.data.frame(t(GSM.trials.mass))
colnames(GSM.trials.mass.t) <- c("Mass Concentration")
GSM.Mass.Concentration <- get_summary_stats(GSM.trials.mass.t, type = "mean_sd")

# Time Series Plot of changes in Mass Concentration ----------------------------------------------------------------------------------
GSM1.1 <- GSM1 %>% dplyr::mutate(Trial = rep("TrialX", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 21 | timepoint > 75) %>% filter(timepoint < 94 | timepoint > 163) %>% filter(timepoint < 243 | timepoint > 262) %>% filter(timepoint < 284) 
GSM1.1 <- GSM1.1 %>% mutate(timepoint = 1:nrow(GSM1.1))

GSM2.1 <- GSM2 %>% dplyr::mutate(Trial = rep("Trial1", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 0) %>% filter(timepoint < 12 | timepoint > 45) %>% filter(timepoint < 59 | timepoint > 75) %>% filter(timepoint < 155 | timepoint > 177) %>% filter(timepoint < 197) 
timepoint <- c(12,13,14,15,16,17,18)
tp.df <- as.data.frame(timepoint)
GSM2.x <- rbind.fill(GSM2.1, tp.df) %>% arrange(timepoint)
GSM2.1 <- GSM2.x %>% mutate(timepoint = 1:nrow(GSM2.x))

GSM3.1 <- GSM3 %>% dplyr::mutate(Trial = rep("Trial2", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 13) %>% filter(timepoint < 34 | timepoint > 45) %>% filter(timepoint < 66 | timepoint > 71) %>% filter(timepoint < 152| timepoint > 153) %>% filter(timepoint < 174) 
GSM3.1 <- GSM3.1 %>% mutate(timepoint = 1:nrow(GSM3.1))

GSM4.1 <- GSM4 %>% dplyr::mutate(Trial = rep("Trial3", times=n())) %>% select(Trial, Treatment, timepoint, TotScanConc, TotScanMass) %>%  filter(timepoint > 40) %>% filter(timepoint < 61 | timepoint > 62) %>% filter(timepoint < 82| timepoint > 89) %>% filter(timepoint < 169 | timepoint > 197) %>% filter(timepoint < 218) 
GSM4.1 <- GSM4.1 %>% mutate(timepoint = 1:nrow(GSM4.1))

# AEROSOL NUMBER AND MASS CONCENTRATIONS AND TIME SERIES PLOT ------------------------------------------------------------
#MIB
MIB.trials <- right_join(MIB1.1, MIB2.1, by =c('timepoint')) %>% left_join(MIB3.1, by =c('timepoint')) %>% left_join(MIB4.1, by =c('timepoint'))
MIB.Trials.x <- rbind(MIB1.1, MIB2.1, MIB3.1)
MIB.aerosol.summary.stats <- MIB.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "2MIB")

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

#PLOTS 
#MIB
MIB.ts <- ggplot(MIB.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() + scale_color_manual(values=c('gray', 'gray35', 'black')) + xlab("Experiment Time (minutes)") + ylab("Mass Concentration") + ggtitle("2-Methylisoborneol") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold"))

#GSM
GSM.trials <- right_join(GSM1.1, GSM2.1, by =c('timepoint')) %>% left_join(GSM3.1, by =c('timepoint')) %>% left_join(GSM4.1, by =c('timepoint'))
GSM.Trials.x <- rbind(GSM2.1, GSM3.1, GSM4.1)
GSM.aerosol.summary.stats <- GSM.Trials.x %>% group_by(Treatment) %>% get_summary_stats(type = "mean_sd") %>% filter(variable != "timepoint") %>% mutate(VOC = "GSM")

summary.stats <- rbind(GSM.aerosol.summary.stats, MIB.aerosol.summary.stats)

GSM.TS <- ggplot(GSM.Trials.x) + geom_point(aes(x = timepoint, y = TotScanMass, color = Trial)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  scale_color_manual(values=c('gray', 'gray35', 'black')) + theme_bw() +
  xlab("Experiment Time (minutes)") + ylab("Mass Concentration") +
  ggtitle("Geosmin") + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), plot.title = element_text(size=14,face="bold")) +
  theme(legend.position="none") 

GSM.TS + MIB.ts

#-----------------------------------------------------------------------------------------------------------------------------------
#Size Distributions
#manual ggplot specs 
sd.plot.colors <- c("Average" = "#000000",
                    "Trial1" = "#1b9e77",
                    "Trial2" = "#d95f02",
                    "Trial3" = "#7570b3",
                    "Trial4" = "#e7298a")
sd.plot.linetype <- c("Calculated" = "dashed",
                      "Measured" = "solid") 

# 2-METHYLISOBORNEOL
MIB1.size <- MIB1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.MIB1.size <- MIB1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
MIB2.size <- MIB2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.MIB2.size <- MIB2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
MIB3.size <- MIB3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.MIB3.size <- MIB3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
MIB4.size <- MIB4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))
avg.MIB4.size <- MIB4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial4", times=n()))

MIB.size.x <- rbind(MIB1.size,MIB2.size,MIB3.size,MIB4.size)
MIB.size.dist <- MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
MIB.size.dist <- MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
MIB.size.dist.tg <- MIB.size.dist %>% filter(Trial != "Trial4") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.MIB.size.x <- rbind(avg.MIB1.size, avg.MIB2.size, avg.MIB3.size)
avg.MIB.size.dist <- avg.MIB.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.MIB.size.dist <- avg.MIB.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.MIB.size.dist <- rbind(avg.MIB.size.dist, MIB.size.dist.tg)

MIB.size.dist <- ggplot(final.MIB.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 2) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("2-MIB SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

# GEOSMIN
GSM1.size <- GSM1 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
avg.GSM1.size <- GSM1.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trialx", times=n()))
GSM2.size <- GSM2 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
avg.GSM2.size <- GSM2.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial1", times=n()))
GSM3.size <- GSM3 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
avg.GSM3.size <- GSM3.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial2", times=n()))
GSM4.size <- GSM4 %>% dplyr::filter(Treatment == 'OH+VOC') %>% select(-timepoint, -Treatment, -StartDate, -Time, -TotScanConc, -TotScanArea, -TotScanVol, -TotScanMass) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))
avg.GSM4.size <- GSM4.size %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Trial3", times=n()))

GSM.size.x <- rbind(GSM1.size,GSM2.size,GSM3.size,GSM4.size)
GSM.size.dist <- GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
GSM.size.dist <- GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) 
GSM.size.dist.tg <- GSM.size.dist %>% filter(Trial != "Trialx") %>% group_by(Bin) %>% summarise_if(is.numeric, ~mean(na.exclude(.))) %>% dplyr::mutate(Trial = rep("Average", times=n()), Type = rep("Calculated", times=n()))

avg.GSM.size.x <- rbind(avg.GSM2.size, avg.GSM3.size,avg.GSM4.size)
avg.GSM.size.dist <- avg.GSM.size.x %>% pivot_longer(!Trial, names_to = "Bin", values_to = "Count")
avg.GSM.size.dist <- avg.GSM.size.dist %>% mutate(Bin = gsub("X","",as.character(Bin))) %>% mutate(Bin = as.numeric(Bin)) %>% dplyr::mutate(Type = rep("Measured", times=n()))
final.GSM.size.dist <- rbind(avg.GSM.size.dist, GSM.size.dist.tg) %>% arrange(desc(Trial))
CMD <- final.GSM.size.dist %>% group_by(Trial) %>%  summarise_if(is.numeric, ~median(na.exclude(.))) 
GSM.Trial.1 <- final.GSM.size.dist %>% filter(Trial == "Trial1")
GSM.Trial.2 <- final.GSM.size.dist %>% filter(Trial == "Trial2")
GSM.Trial.3 <- final.GSM.size.dist %>% filter(Trial == "Trial3")
GSM.Trial.a <- final.GSM.size.dist %>% filter(Trial == "Average")

GSM.size.dist <- ggplot(final.GSM.size.dist) + geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), linewidth = 1.25) + 
  #geom_col(GSM.size.dist.tg, aes(x = Bin, y = Count, color = Trial), alpha = 0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors) +
  scale_linetype_manual(values=sd.plot.linetype) + 
  ggtitle("Geosmin SOA Size Distribution") + 
  theme_bw() +
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))

GSM.size.dist + MIB.size.dist

ggplot(final.GSM.size.dist) + geom_col(aes(x = Bin, y = Count, fill = Trial), alpha = 0.5) + 
  geom_line(aes(x = Bin, y = Count, color = Trial, linetype = Type), size = 1.25, alpha = 0.75) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values=sd.plot.colors)  +
  scale_fill_manual(values=sd.plot.colors) +
  ggtitle("Geosmin SOA Size Distribution") + 
  facet_wrap(~ Trial)+ 
  theme_bw() +
  geom_vline(linetype = 5,aes(xintercept=30)) + #here is where to plot x-intercept as Mean Median Diameter
  xlab("Particle Diameter (nm)") +
  ylab(bquote('dN/dlogDP '(cm^-3))) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(0,3500000), breaks = seq(0, 3000000, by = 1000000))


# IMPORTING PAM DATA TO DETERMINE AVERAGE CONDITIONS IN THE OFR -------------------------------------------------------------------------
PAM_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB")
PAM_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM")

for(i in 1:length(PAM_MIB_files)) {                              
  assign(paste0("PAM_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/MIB/", PAM_MIB_files[i])))}
for(i in 1:length(PAM_GSM_files)) {                              
  assign(paste0("PAM_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/PAM/GSM/", PAM_GSM_files[i])))}

#MIB PAM Conditions 
PAM1 <- PAM_MIB1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM1a <- PAM1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  PAM1$DateTime >= "2022-11-04 10:00:00" & PAM1$DateTime < "2022-11-04 11:00:00" ~ "O3",
  PAM1$DateTime >= "2022-11-04 11:00:00" & PAM1$DateTime < "2022-11-04 11:030:00" ~ "OH",
  PAM1$DateTime >= "2022-11-04 11:30:00" & PAM1$DateTime < "2022-11-04 12:38:00" ~ "OH+VOC",
  PAM1$DateTime >= "2022-11-04 12:38:00" & PAM1$DateTime < "2022-11-04 14:08:00" ~ "Filter",
  PAM1$DateTime >= "2022-11-04 14:08:00" & PAM1$DateTime <= "2022-11-04 14:38:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM1b <- PAM1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM2 <- PAM_MIB2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM2a <- PAM2 %>% mutate(Treatment = case_when(
  PAM2$DateTime >= "2022-11-07 08:26:00" & PAM2$DateTime < "2022-11-07 09:48:00" ~ "O3",
  PAM2$DateTime >= "2022-11-07 09:48:00" & PAM2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  PAM2$DateTime >= "2022-11-07 11:50:00" & PAM2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  PAM2$DateTime >= "2022-11-07 12:20:00" & PAM2$DateTime < "2022-11-07 13:50:00" ~ "Filter",
  PAM2$DateTime >= "2022-11-07 13:50:00" & PAM2$DateTime <= "2022-11-07 14:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM2b <- PAM2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM3 <- PAM_MIB3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM3a <- PAM3 %>% mutate(Treatment = case_when(
  PAM3$DateTime >= "2022-11-10 9:50:00" & PAM3$DateTime < "2022-11-10 10:22:00" ~ "O3",
  PAM3$DateTime >= "2022-11-10 10:22:00" & PAM3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  PAM3$DateTime >= "2022-11-10 11:03:00" & PAM3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  PAM3$DateTime >= "2022-11-10 11:30:00" & PAM3$DateTime < "2022-11-10 13:00:00" ~ "Filter",
  PAM3$DateTime >= "2022-11-10 13:00:00" & PAM3$DateTime <= "2022-11-10 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM3b <- PAM3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin PAM conditions 
PAM4 <- PAM_GSM1 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM4a <- PAM4 %>% mutate(Treatment = case_when(
  PAM4$DateTime >= "2022-11-08 8:56:00" & PAM4$DateTime < "2022-11-08 10:11:00" ~ "O3",
  PAM4$DateTime >= "2022-11-08 10:11:00" & PAM4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  PAM4$DateTime >= "2022-11-08 10:33:00" & PAM4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  PAM4$DateTime >= "2022-11-08 11:05:00" & PAM4$DateTime < "2022-11-08 13:05:00" ~ "Filter",
  PAM4$DateTime >= "2022-11-08 13:05:00" & PAM4$DateTime <= "2022-11-08 13:25:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM4b <- PAM4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM5 <- PAM_GSM2 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM5a <- PAM5 %>% mutate(Treatment = case_when(
  PAM5$DateTime >= "2022-11-29 14:22:00" & PAM5$DateTime < "2022-11-29 14:42:00" ~ "O3",
  PAM5$DateTime >= "2022-11-29 14:42:00" & PAM5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  PAM5$DateTime >= "2022-11-29 15:25:00" & PAM5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  PAM5$DateTime >= "2022-11-29 15:50:00" & PAM5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  PAM5$DateTime >= "2022-11-29 17:20:00" & PAM5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM5b <- PAM5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

PAM6 <- PAM_GSM3 %>% mutate(DateTime = as.POSIXct(dat, format = "%m/%d/%Y %H:%M")) %>% dplyr::select(-dat, -OzoneLamp) %>% dplyr::select(DateTime, everything()) 
PAM6a <- PAM6 %>% mutate(Treatment = case_when(
  PAM6$DateTime >= "2023-01-30 11:10:00" & PAM6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  PAM6$DateTime >= "2023-01-30 11:40:00" & PAM6$DateTime < "2023-01-30 12:10:00" ~ "OH",
  PAM6$DateTime >= "2023-01-30 12:10:00" & PAM6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  PAM6$DateTime >= "2023-01-30 12:40:00" & PAM6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  PAM6$DateTime >= "2023-01-30 14:10:00" & PAM6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
PAM6b <- PAM6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#ozone data from the PAM -----------------------------------------------------------------------------------------------------------------
OZONE_MIB_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB")
OZONE_GSM_files <- list.files("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM")

for(i in 1:length(OZONE_MIB_files)) {                              
  assign(paste0("OZONE_MIB", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/MIB/", OZONE_MIB_files[i])))}
for(i in 1:length(OZONE_GSM_files)) {                              
  assign(paste0("OZONE_GSM", i), 
         read.csv(paste0("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/SOA/CyanoSOA/OZONE/GSM/", OZONE_GSM_files[i])))}

#2-MIB OZONE CONDITIONS 
OZONE1 <- OZONE_MIB1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE1a <- OZONE1 %>% mutate(Treatment = case_when( #change times to reflect PAM experiment log
  OZONE1$DateTime >= "2022-11-04 09:30:00" & OZONE1$DateTime < "2022-11-04 09:54:00" ~ "O3",
  OZONE1$DateTime >= "2022-11-04 10:00:00" & OZONE1$DateTime < "2022-11-04 11:00:00" ~ "OH",
  OZONE1$DateTime >= "2022-11-04 11:00:00" & OZONE1$DateTime < "2022-11-04 12:00:00" ~ "OH+VOC",
  OZONE1$DateTime >= "2022-11-04 12:00:00" & OZONE1$DateTime < "2022-11-04 13:00:00" ~ "Filter",
  OZONE1$DateTime >= "2022-11-04 13:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE1b <- OZONE1a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE2 <- OZONE_MIB2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE2a <- OZONE2 %>% mutate(Treatment = case_when(
  OZONE2$DateTime >= "2022-11-07 09:42:00" & OZONE2$DateTime < "2022-11-07 09:49:00" ~ "O3",
  OZONE2$DateTime >= "2022-11-07 09:49:00" & OZONE2$DateTime < "2022-11-07 11:50:00" ~ "OH",
  OZONE2$DateTime >= "2022-11-07 11:50:00" & OZONE2$DateTime < "2022-11-07 12:20:00" ~ "OH+VOC",
  OZONE2$DateTime >= "2022-11-07 12:20:00" & OZONE2$DateTime < "2022-11-07 13:45:00" ~ "Filter",
  OZONE2$DateTime >= "2022-11-07 13:45:00" & OZONE2$DateTime <= "2022-11-07 14:09:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE2b <- OZONE2a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE3 <- OZONE_MIB3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE3a <- OZONE3 %>% mutate(Treatment = case_when(
  OZONE3$DateTime >= "2022-11-10 10:10:00" & OZONE3$DateTime < "2022-11-10 10:16:00" ~ "O3",
  OZONE3$DateTime >= "2022-11-10 10:22:00" & OZONE3$DateTime < "2022-11-10 11:03:00" ~ "OH",
  OZONE3$DateTime >= "2022-11-10 11:03:00" & OZONE3$DateTime < "2022-11-10 11:30:00" ~ "OH+VOC",
  OZONE3$DateTime >= "2022-11-10 11:30:00" & OZONE3$DateTime < "2022-11-10 12:53:00" ~ "Filter",
  OZONE3$DateTime >= "2022-11-10 13:00:00" & OZONE3$DateTime <= "2022-11-10 13:13:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE3b <- OZONE3a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#Geosmin OZONE conditions 
OZONE4 <- OZONE_GSM1 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE4a <- OZONE4 %>% mutate(Treatment = case_when(
  OZONE4$DateTime >= "2022-11-08 10:00:00" & OZONE4$DateTime < "2022-11-08 10:04:00" ~ "O3",
  OZONE4$DateTime >= "2022-11-08 10:11:00" & OZONE4$DateTime < "2022-11-08 10:33:00" ~ "OH",
  OZONE4$DateTime >= "2022-11-08 10:33:00" & OZONE4$DateTime < "2022-11-08 11:05:00" ~ "OH+VOC",
  OZONE4$DateTime >= "2022-11-08 11:05:00" & OZONE4$DateTime < "2022-11-08 13:00:00" ~ "Filter",
  OZONE4$DateTime >= "2022-11-08 13:05:00" & OZONE4$DateTime <= "2022-11-08 13:20:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE4b <- OZONE4a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE5 <- OZONE_GSM2 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE5a <- OZONE5 %>% mutate(Treatment = case_when(
  OZONE5$DateTime >= "2022-11-29 14:26:00" & OZONE5$DateTime < "2022-11-29 14:37:00" ~ "O3",
  OZONE5$DateTime >= "2022-11-29 14:42:00" & OZONE5$DateTime < "2022-11-29 15:25:00" ~ "OH",
  OZONE5$DateTime >= "2022-11-29 15:25:00" & OZONE5$DateTime < "2022-11-29 15:50:00" ~ "OH+VOC",
  OZONE5$DateTime >= "2022-11-29 15:50:00" & OZONE5$DateTime < "2022-11-29 17:20:00" ~ "Filter",
  OZONE5$DateTime >= "2022-11-29 17:20:00" & OZONE5$DateTime <= "2022-11-29 17:40:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE5b <- OZONE5a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

OZONE6 <- OZONE_GSM3 %>% unite("DateTime", c(Date,Time), sep = " ") %>% mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M:%S")) %>% dplyr::select(DateTime, Ozone) 
OZONE6a <- OZONE6 %>% mutate(Treatment = case_when(
  OZONE6$DateTime >= "2023-01-30 11:10:00" & OZONE6$DateTime < "2023-01-30 11:40:00" ~ "O3",
  OZONE6$DateTime >= "2023-01-30 11:40:00" & OZONE6$DateTime < "2023-01-30 12:09:00" ~ "OH",
  OZONE6$DateTime >= "2023-01-30 12:09:00" & OZONE6$DateTime < "2023-01-30 12:40:00" ~ "OH+VOC",
  OZONE6$DateTime >= "2023-01-30 12:40:00" & OZONE6$DateTime < "2023-01-30 14:10:00" ~ "Filter",
  OZONE6$DateTime >= "2023-01-30 14:10:00" & OZONE6$DateTime <= "2023-01-30 15:00:00" ~ "O3+VOC")) %>% 
  dplyr::select(Treatment, everything()) %>% 
  drop_na()
OZONE6b <- OZONE6a %>% group_by(Treatment) %>% summarise_if(is.numeric, ~mean(na.exclude(.)))

#combining Ozone and PAM data for easy access 
OFR1 <- left_join(OZONE1b, PAM1b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR2 <- left_join(OZONE2b, PAM2b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR3 <- left_join(OZONE3b, PAM3b, by = "Treatment") %>% mutate(Trial = "Trial3")
OFR4 <- left_join(OZONE4b, PAM4b, by = "Treatment") %>% mutate(Trial = "Trial1")
OFR5 <- left_join(OZONE5b, PAM5b, by = "Treatment") %>% mutate(Trial = "Trial2")
OFR6 <- left_join(OZONE6b, PAM6b, by = "Treatment") %>% mutate(Trial = "Trial3")

PAM.MIB.summary <- rbind(OFR1,OFR2,OFR3) %>% filter(Treatment == "OH+VOC")
PAM.MIB.summary.1 <- get_summary_stats(PAM.MIB.summary, type = "mean_sd")
PAM.GSM.summary <- rbind(OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.GSM.summary.1 <- get_summary_stats(PAM.GSM.summary, type = "mean_sd")
PAM.all.summary <- rbind(OFR1,OFR2,OFR3,OFR4,OFR5,OFR6) %>% filter(Treatment == "OH+VOC")
PAM.all.summary.1 <- get_summary_stats(PAM.all.summary, type = "mean_sd")





#______________________________________________________________________________________________________________________________________________________
# OLD STUFF 

#finding the geometric standard deviation of GSM and 2-MIB Trial 2
GSM.N <- GSM.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(sum = sum(`Number Concentration`))
MIB.N <- MIB.2.1 %>% dplyr::select(`Number Concentration`) %>% summarise(mean = mean(`Number Concentration`))

GSM.2.3 <- GSM.2 %>% dplyr::select(-Date, -Time,-vol, -timepoint) %>% dplyr::select(num, everything()) %>% rename(N = num) %>% pivot_longer(!N, names_to = "di", values_to = "ni") %>% mutate(di = gsub("X","",as.character(di))) %>% mutate(di = as.numeric(di)) %>% mutate(geom = (ni*log(di))) 

numerator<- GSM.2.3 %>% select(geom) %>% summarise(sum(geom))
denominator<- GSM.2.3 %>% select(ni) %>% summarise(sum(ni))

exp(numerator/denominator)

GSM.2.2 <- GSM.2.2 

MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin))

#calculating Count Median Diameter via TSI worksheet
# Dg = geometric mean diameter OR Count Median Diameter (CMD)
# Di = midpoint particle size (each bin's value in the first row)
# ni = number of particles in each bin (value in each bin)
# N = sum of ni (total number of particles)

#first thing is see if num (number concentration is equal to if I add the values for each bin)
GSM.2.x <- GSM.2.trial %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Di", values_to = "ni") %>% group_by(Di)  
bin.sums <- GSM.2.x %>% mutate(Di = gsub("X","",as.character(Di))) %>% mutate(Di = as.numeric(Di)) %>% group_by(timepoint) %>% summarise_at(vars(ni), sum) %>% rename(N = ni)
num.conc <- GSM.2.trial %>% dplyr::select(timepoint, num)
bin.sums.num <- left_join(num.conc, bin.sums, by = "timepoint") #ok for some reason the number concentration, which I assumed would be the same as the sum of each bin, is much lower than the sum of each bin. So I am just going to use the sum of each bin as the actual number concentration.
CMD.df <- left_join(GSM.2.x, bin.sums, by = "timepoint") %>% mutate(ln.ni = log10(ni)) # i also think I need to take the log of ni for the lognormal distribution
CMD.timepoint.1 <- CMD.df %>% filter(timepoint == 1) %>% mutate(Dini = Di^ln.ni)






MIB.2.2 <- MIB.2 %>% dplyr::select(-Date, -Time, -num, -vol) %>% pivot_longer(!timepoint, names_to = "Bin", values_to = "Count") %>% group_by(Bin) %>% summarise(mean = mean(Count), sd = sd(Count)) 
MIB.2.2 <- MIB.2.2 %>% mutate(Bin = gsub("X","",as.character(MIB.2.2$Bin))) %>% mutate(Bin = as.numeric(Bin)) 








>>>>>>> 6f6fe27daaf6d46936518ec2616d362599fad2e4
