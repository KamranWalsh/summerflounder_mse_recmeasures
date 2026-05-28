library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)

#code for running RDM to form data to fit RDM emulator HGAM to, and code for HGAM fitting, diagnostics, plots, etc. 
#length distributions and accompanying SSB values are pulled from a directory of MSE runs where the length distributions and SSBs from each simulation were saved

#RDM_input_allOM_nostratify with updated sinatra_split2_savelengths for start BBMSY = 1

setwd("~/Desktop/FlounderMSE")

#use blank input table for RDM 
blankinput <- readRDS("blankinputtables.rds")

#FORM PLAUSIBLE DISTRIBUTION OF REGULATIONS 

sample_seas <- c(60, 
                 75,
                 90,
                 120,
                 135,
                 150, 
                 165,
                 180, 
                 195,
                 210,
                 225,
                 240,
                 255,
                 270, 
                 285,
                 300)
sample_bag <- c(4,5,6,7,8)
sample_minlen <- c(14,15,16,17,18,19,20)
state <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")

combinations_sample <- expand.grid(SeasonLen = unique(sample_seas), Bag = unique(sample_bag),
                                   MinLen = unique(sample_minlen))

#df_trimmed <- combinations_sample[1:(nrow(combinations_sample) - 24),]

rbc4 <- combinations_sample

#View(rbc4)
#random not necessary anymore if sampling from pool of length distributions
#rbc4 <- rbc4[sample(nrow(rbc4)), ]
#row.names(rbc4) <- NULL  # reset row names
#View(filltable)
#nrow(rbc4)

rbcNC <- rbc4 %>% mutate(State = "NC")
rbcNJ <- rbc4 %>% mutate(State = "NJ")
rbcNY <- rbc4 %>% mutate(State = "NY")
rbcMD <- rbc4 %>% mutate(State = "MD")
rbcMA <- rbc4 %>% mutate(State = "MA")
rbcRI <- rbc4 %>% mutate(State = "RI")
rbcVA <- rbc4 %>% mutate(State = "VA")
rbcDE <- rbc4 %>% mutate(State = "DE")
rbcCT <- rbc4 %>% mutate(State = "CT")

rbc4 <- rbind(rbcCT, rbcDE, rbcVA, rbcRI, rbcMA, rbcMD, rbcNY,
              rbcNJ, rbcNC)

#try and have more of each regulation set to link SSB to
#rbc5 <- rbc4 %>% slice(rep(1:n(), each = 10))

#View(rbc4)
#for filling in RDM output table below 

# sequential                      
filltable <- dplyr::select(rbc4[1:560,], SeasonLen, Bag, MinLen) 
#nrow(filltable)

#SET DIRECTORY TO ACCESS RDM SOURCE FILES <- CAN BE ANY MSE RUN DIRECTORY WITH MOST RECENT SINATRA
Alternative = "scen1"
#extract regulations for each time step 
if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3_seedRDM/scen11_20/sim1_10")}
#if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3/scen11_20/sim1_10")}
#if(Alternative == "scen1"){scendir = "~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3/scen11_20/sim1_10/"}

# CONVERT STARTING REGULATIONS INTO RDM INPUT FORMAT    
n = 560

## -----------------------------------------------------------------------------

NJtableHCR <- rbc4 %>% filter(State == "NJ")
#View(NYtableHCR)
NJdatalist = list()
NJdatalist = vector("list", length = n)

for (x in 1:length(NJtableHCR$State)){
  NJinput2 <- blankinput$NJinput2
  if(NJtableHCR$SeasonLen[x]==60){NJinput2$fluke_bag[13:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[13:16] = NJtableHCR$MinLen[x]} else 
    if(NJtableHCR$SeasonLen[x]==75){NJinput2$fluke_bag[12:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[12:16] = NJtableHCR$MinLen[x]} else
      if (NJtableHCR$SeasonLen[x]==90){NJinput2$fluke_bag[11:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[11:16] = NJtableHCR$MinLen[x]} else 
        if (NJtableHCR$SeasonLen[x]==105){NJinput2$fluke_bag[10:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[10:16] = NJtableHCR$MinLen[x]} else
          if (NJtableHCR$SeasonLen[x]==120){NJinput2$fluke_bag[9:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:16] = NJtableHCR$MinLen[x]} else 
            if (NJtableHCR$SeasonLen[x]==135){NJinput2$fluke_bag[9:17] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:17] = NJtableHCR$MinLen[x]} else 
              if (NJtableHCR$SeasonLen[x]==150){NJinput2$fluke_bag[9:18] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:18] = NJtableHCR$MinLen[x]} else 
                if (NJtableHCR$SeasonLen[x]==165){NJinput2$fluke_bag[9:19] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:19] = NJtableHCR$MinLen[x]} else 
                  if (NJtableHCR$SeasonLen[x]==180){NJinput2$fluke_bag[9:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:20] = NJtableHCR$MinLen[x]} else 
                    if (NJtableHCR$SeasonLen[x]==195){NJinput2$fluke_bag[8:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[8:20] = NJtableHCR$MinLen[x]} else 
                      if (NJtableHCR$SeasonLen[x]==210){NJinput2$fluke_bag[7:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[7:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 2} else #for when season length change results in increase of bounds from regulations_option 2019 spreadsheet
                        if (NJtableHCR$SeasonLen[x]==225){NJinput2$fluke_bag[6:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[6:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 3} else 
                          if (NJtableHCR$SeasonLen[x]==240){NJinput2$fluke_bag[5:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                            if (NJtableHCR$SeasonLen[x]==255){NJinput2$fluke_bag[5:21] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:21] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                              if (NJtableHCR$SeasonLen[x]==270){NJinput2$fluke_bag[5:22] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:22] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                                if (NJtableHCR$SeasonLen[x]==285){NJinput2$fluke_bag[5:23] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:23] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4; NJinput2$pd_multiplier[22] <- 2} else 
                                  if (NJtableHCR$SeasonLen[x]==300){NJinput2$fluke_bag[5:24] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:24] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4; NJinput2$pd_multiplier[22] <- 3} 
  NJinput2$fluke_bag <- ifelse(is.na(NJinput2$fluke_bag), 0, NJtableHCR$Bag[x])
  NJinput2$fluke_min <- ifelse(is.na(NJinput2$fluke_min), 100, NJtableHCR$MinLen[x])
  if (NJinput2$fluke_bag[10]!=0){NJinput2$pd_multiplier[10] <- 2} # I've been keeping pd_multiplier as 1 if that period isn't open for a scenario
  if (NJinput2$fluke_bag[18]!=0){NJinput2$pd_multiplier[18] <- 2.5}
  NJinputmiddleman <- na.omit(NJinput2)
  NJdatalist[[x]] <- NJinputmiddleman %>% mutate(Nsim = x)
}
NJbig_data = do.call(rbind, NJdatalist)

## -----------------------------------------------------------------------------

NYtableHCR <- rbc4 %>% filter(State == "NY")

NYdatalist = list()
NYdatalist = vector("list", length = n)

for (x in 1:length(NYtableHCR$State)){
  NYinput2 <- blankinput$NYinput2
  if(NYtableHCR$SeasonLen[x]==60){NYinput2$fluke_bag[13:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[13:16] = NYtableHCR$MinLen[x]} else 
    if(NYtableHCR$SeasonLen[x]==75){NYinput2$fluke_bag[12:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[12:16] = NYtableHCR$MinLen[x]} else
      if (NYtableHCR$SeasonLen[x]==90){NYinput2$fluke_bag[11:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[11:16] = NYtableHCR$MinLen[x]} else 
        if (NYtableHCR$SeasonLen[x]==105){NYinput2$fluke_bag[10:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[10:16] = NYtableHCR$MinLen[x]} else
          if (NYtableHCR$SeasonLen[x]==120){NYinput2$fluke_bag[9:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:16] = NYtableHCR$MinLen[x]} else 
            if (NYtableHCR$SeasonLen[x]==135){NYinput2$fluke_bag[9:17] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:17] = NYtableHCR$MinLen[x]} else 
              if (NYtableHCR$SeasonLen[x]==150){NYinput2$fluke_bag[9:18] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:18] = NYtableHCR$MinLen[x]} else 
                if (NYtableHCR$SeasonLen[x]==165){NYinput2$fluke_bag[9:19] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:19] = NYtableHCR$MinLen[x]} else 
                  if (NYtableHCR$SeasonLen[x]==180){NYinput2$fluke_bag[9:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:20] = NYtableHCR$MinLen[x]} else 
                    if (NYtableHCR$SeasonLen[x]==195){NYinput2$fluke_bag[8:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[8:20] = NYtableHCR$MinLen[x]} else 
                      if (NYtableHCR$SeasonLen[x]==210){NYinput2$fluke_bag[7:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[7:20] = NYtableHCR$MinLen[x]} else 
                        if (NYtableHCR$SeasonLen[x]==225){NYinput2$fluke_bag[6:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[6:20] = NYtableHCR$MinLen[x]} else 
                          if (NYtableHCR$SeasonLen[x]==240){NYinput2$fluke_bag[5:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:20] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2} else 
                            if (NYtableHCR$SeasonLen[x]==255){NYinput2$fluke_bag[5:21] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:21] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 2} else 
                              if (NYtableHCR$SeasonLen[x]==270){NYinput2$fluke_bag[5:22] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:22] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 3} else 
                                if (NYtableHCR$SeasonLen[x]==285){NYinput2$fluke_bag[5:23] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:23] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 4} else 
                                  if (NYtableHCR$SeasonLen[x]==300){NYinput2$fluke_bag[5:24] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:24] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 5} 
  NYinput2$fluke_bag <- ifelse(is.na(NYinput2$fluke_bag), 0, NYtableHCR$Bag[x])
  NYinput2$fluke_min <- ifelse(is.na(NYinput2$fluke_min), 100, NYtableHCR$MinLen[x])
  if (NYinput2$fluke_bag[9]!=0){NYinput2$pd_multiplier[9] <- 1.25}
  NYinputmiddleman <- na.omit(NYinput2)
  NYdatalist[[x]] <- NYinputmiddleman %>% mutate(Nsim = x)
}
NYbig_data = do.call(rbind, NYdatalist)


## -----------------------------------------------------------------------------

NCtableHCR <- rbc4 %>% filter(State == "NC")

NCdatalist = list()
NCdatalist = vector("list", length = n)

for (x in 1:length(NCtableHCR$State)){
  NCinput2 <- blankinput$NCinput2
  if(NCtableHCR$SeasonLen[x]==60){NCinput2$fluke_bag[13:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[13:16] = NCtableHCR$MinLen[x]} else 
    if(NCtableHCR$SeasonLen[x]==75){NCinput2$fluke_bag[12:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[12:16] = NCtableHCR$MinLen[x]} else
      if (NCtableHCR$SeasonLen[x]==90){NCinput2$fluke_bag[11:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[11:16] = NCtableHCR$MinLen[x]} else 
        if (NCtableHCR$SeasonLen[x]==105){NCinput2$fluke_bag[10:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[10:16] = NCtableHCR$MinLen[x]} else
          if (NCtableHCR$SeasonLen[x]==120){NCinput2$fluke_bag[9:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:16] = NCtableHCR$MinLen[x]} else 
            if (NCtableHCR$SeasonLen[x]==135){NCinput2$fluke_bag[9:17] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:17] = NCtableHCR$MinLen[x]} else 
              if (NCtableHCR$SeasonLen[x]==150){NCinput2$fluke_bag[9:18] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:18] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2} else 
                if (NCtableHCR$SeasonLen[x]==165){NCinput2$fluke_bag[9:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2} else 
                  if (NCtableHCR$SeasonLen[x]==180){NCinput2$fluke_bag[9:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[20] <- 2} else 
                    if (NCtableHCR$SeasonLen[x]==195){NCinput2$fluke_bag[8:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[8:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[20] <- 2} else 
                      if (NCtableHCR$SeasonLen[x]==210){NCinput2$fluke_bag[7:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[7:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 2; NCinput2$pd_multiplier[20] <- 2} else 
                        if (NCtableHCR$SeasonLen[x]==225){NCinput2$fluke_bag[6:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[6:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 3; NCinput2$pd_multiplier[20] <- 2} else 
                          if (NCtableHCR$SeasonLen[x]==240){NCinput2$fluke_bag[5:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 2} else 
                            if (NCtableHCR$SeasonLen[x]==255){NCinput2$fluke_bag[5:21] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:21] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 3} else 
                              if (NCtableHCR$SeasonLen[x]==270){NCinput2$fluke_bag[5:22] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:22] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 4} else 
                                if (NCtableHCR$SeasonLen[x]==285){NCinput2$fluke_bag[5:23] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:23] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 5} else 
                                  if (NCtableHCR$SeasonLen[x]==300){NCinput2$fluke_bag[5:24] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:24] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[17] <- 2; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 6} 
  NCinput2$fluke_bag <- ifelse(is.na(NCinput2$fluke_bag), 0, NCtableHCR$Bag[x])
  NCinput2$fluke_min <- ifelse(is.na(NCinput2$fluke_min), 100, NCtableHCR$MinLen[x])
  if (NCinput2$fluke_bag[16]!=0){NCinput2$pd_multiplier[16] <- 2}
  NCinputmiddleman <- na.omit(NCinput2)
  NCdatalist[[x]] <- NCinputmiddleman %>% mutate(Nsim = x)
}
NCbig_data = do.call(rbind, NCdatalist)

## -----------------------------------------------------------------------------

RItableHCR <- rbc4 %>% filter(State == "RI")

RIdatalist = list()
RIdatalist = vector("list", length = n)

for (x in 1:length(RItableHCR$State)){
  RIinput2 <- blankinput$RIinput2
  if(RItableHCR$SeasonLen[x]==60){RIinput2$fluke_bag[13:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[13:16] = RItableHCR$MinLen[x]} else 
    if(RItableHCR$SeasonLen[x]==75){RIinput2$fluke_bag[12:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[12:16] = RItableHCR$MinLen[x]} else
      if (RItableHCR$SeasonLen[x]==90){RIinput2$fluke_bag[11:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[11:16] = RItableHCR$MinLen[x]} else 
        if (RItableHCR$SeasonLen[x]==105){RIinput2$fluke_bag[10:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[10:16] = RItableHCR$MinLen[x]} else
          if (RItableHCR$SeasonLen[x]==120){RIinput2$fluke_bag[9:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:16] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
            if (RItableHCR$SeasonLen[x]==135){RIinput2$fluke_bag[9:17] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:17] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
              if (RItableHCR$SeasonLen[x]==150){RIinput2$fluke_bag[9:18] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:18] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                if (RItableHCR$SeasonLen[x]==165){RIinput2$fluke_bag[9:19] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:19] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                  if (RItableHCR$SeasonLen[x]==180){RIinput2$fluke_bag[9:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867); RIinput2$pd_multiplier[19] <- 2} else 
                    if (RItableHCR$SeasonLen[x]==195){RIinput2$fluke_bag[8:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[8:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1+(1/0.867)); RIinput2$pd_multiplier[19] <- 2} else 
                      if (RItableHCR$SeasonLen[x]==210){RIinput2$fluke_bag[7:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[7:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (2+(1/0.867)); RIinput2$pd_multiplier[19] <- 2} else 
                        if (RItableHCR$SeasonLen[x]==225){RIinput2$fluke_bag[6:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[6:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (3+(1/0.867)); RIinput2$pd_multiplier[19] <- 2} else 
                          if (RItableHCR$SeasonLen[x]==240){RIinput2$fluke_bag[5:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[19] <- 2} else 
                            if (RItableHCR$SeasonLen[x]==255){RIinput2$fluke_bag[5:21] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:21] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[19] <- 2} else 
                              if (RItableHCR$SeasonLen[x]==270){RIinput2$fluke_bag[5:22] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:22] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 2; RIinput2$pd_multiplier[19] <- 2} else 
                                if (RItableHCR$SeasonLen[x]==285){RIinput2$fluke_bag[5:23] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:23] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 3; RIinput2$pd_multiplier[19] <- 2} else 
                                  if (RItableHCR$SeasonLen[x]==300){RIinput2$fluke_bag[5:24] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:24] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 4; RIinput2$pd_multiplier[19] <- 2} 
  RIinput2$fluke_bag <- ifelse(is.na(RIinput2$fluke_bag), 0, RItableHCR$Bag[x])
  RIinput2$fluke_min <- ifelse(is.na(RIinput2$fluke_min), 100, RItableHCR$MinLen[x])
  RIinputmiddleman <- na.omit(RIinput2)
  RIdatalist[[x]] <- RIinputmiddleman %>% mutate(Nsim = x)
}
RIbig_data = do.call(rbind, RIdatalist)

## -----------------------------------------------------------------------------

MDtableHCR <- rbc4 %>% filter(State == "MD")

MDdatalist = list()
MDdatalist = vector("list", length = n)

for (x in 1:length(MDtableHCR$State)){
  MDinput2 <- blankinput$MDinput2
  if(MDtableHCR$SeasonLen[x]==60){MDinput2$fluke_bag[13:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[13:16] = MDtableHCR$MinLen[x]} else 
    if(MDtableHCR$SeasonLen[x]==75){MDinput2$fluke_bag[12:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[12:16] = MDtableHCR$MinLen[x]} else
      if (MDtableHCR$SeasonLen[x]==90){MDinput2$fluke_bag[11:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[11:16] = MDtableHCR$MinLen[x]} else 
        if (MDtableHCR$SeasonLen[x]==105){MDinput2$fluke_bag[10:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[10:16] = MDtableHCR$MinLen[x]} else
          if (MDtableHCR$SeasonLen[x]==120){MDinput2$fluke_bag[9:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:16] = MDtableHCR$MinLen[x]} else 
            if (MDtableHCR$SeasonLen[x]==135){MDinput2$fluke_bag[9:17] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:17] = MDtableHCR$MinLen[x]} else 
              if (MDtableHCR$SeasonLen[x]==150){MDinput2$fluke_bag[9:18] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:18] = MDtableHCR$MinLen[x]} else 
                if (MDtableHCR$SeasonLen[x]==165){MDinput2$fluke_bag[9:19] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:19] = MDtableHCR$MinLen[x]} else 
                  if (MDtableHCR$SeasonLen[x]==180){MDinput2$fluke_bag[9:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:20] = MDtableHCR$MinLen[x]} else 
                    if (MDtableHCR$SeasonLen[x]==195){MDinput2$fluke_bag[8:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[8:20] = MDtableHCR$MinLen[x]} else 
                      if (MDtableHCR$SeasonLen[x]==210){MDinput2$fluke_bag[7:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[7:20] = MDtableHCR$MinLen[x]} else 
                        if (MDtableHCR$SeasonLen[x]==225){MDinput2$fluke_bag[6:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[6:20] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 2} else 
                          if (MDtableHCR$SeasonLen[x]==240){MDinput2$fluke_bag[5:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:20] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3} else 
                            if (MDtableHCR$SeasonLen[x]==255){MDinput2$fluke_bag[5:21] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:21] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3} else 
                              if (MDtableHCR$SeasonLen[x]==270){MDinput2$fluke_bag[5:22] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:22] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 2} else 
                                if (MDtableHCR$SeasonLen[x]==285){MDinput2$fluke_bag[5:23] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:23] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 3} else 
                                  if (MDtableHCR$SeasonLen[x]==300){MDinput2$fluke_bag[5:24] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:24] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 4} 
  MDinput2$fluke_bag <- ifelse(is.na(MDinput2$fluke_bag), 0, MDtableHCR$Bag[x])
  MDinput2$fluke_min <- ifelse(is.na(MDinput2$fluke_min), 100, MDtableHCR$MinLen[x])
  MDinputmiddleman <- na.omit(MDinput2)
  MDdatalist[[x]] <- MDinputmiddleman %>% mutate(Nsim = x)
}
MDbig_data = do.call(rbind, MDdatalist)


## -----------------------------------------------------------------------------

VAtableHCR <- rbc4 %>% filter(State == "VA")

VAdatalist = list()
VAdatalist = vector("list", length = n)

for (x in 1:length(VAtableHCR$State)){
  VAinput2 <- blankinput$VAinput2
  if(VAtableHCR$SeasonLen[x]==60){VAinput2$fluke_bag[13:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[13:16] = VAtableHCR$MinLen[x]} else 
    if(VAtableHCR$SeasonLen[x]==75){VAinput2$fluke_bag[12:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[12:16] = VAtableHCR$MinLen[x]} else
      if (VAtableHCR$SeasonLen[x]==90){VAinput2$fluke_bag[11:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[11:16] = VAtableHCR$MinLen[x]} else 
        if (VAtableHCR$SeasonLen[x]==105){VAinput2$fluke_bag[10:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[10:16] = VAtableHCR$MinLen[x]} else
          if (VAtableHCR$SeasonLen[x]==120){VAinput2$fluke_bag[9:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:16] = VAtableHCR$MinLen[x]} else 
            if (VAtableHCR$SeasonLen[x]==135){VAinput2$fluke_bag[9:17] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:17] = VAtableHCR$MinLen[x]} else 
              if (VAtableHCR$SeasonLen[x]==150){VAinput2$fluke_bag[9:18] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:18] = VAtableHCR$MinLen[x]} else 
                if (VAtableHCR$SeasonLen[x]==165){VAinput2$fluke_bag[9:19] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:19] = VAtableHCR$MinLen[x]} else 
                  if (VAtableHCR$SeasonLen[x]==180){VAinput2$fluke_bag[9:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:20] = VAtableHCR$MinLen[x]} else 
                    if (VAtableHCR$SeasonLen[x]==195){VAinput2$fluke_bag[8:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[8:20] = VAtableHCR$MinLen[x]} else 
                      if (VAtableHCR$SeasonLen[x]==210){VAinput2$fluke_bag[7:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[7:20] = VAtableHCR$MinLen[x]} else 
                        if (VAtableHCR$SeasonLen[x]==225){VAinput2$fluke_bag[6:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[6:20] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 2} else 
                          if (VAtableHCR$SeasonLen[x]==240){VAinput2$fluke_bag[5:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:20] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                            if (VAtableHCR$SeasonLen[x]==255){VAinput2$fluke_bag[5:21] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:21] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                              if (VAtableHCR$SeasonLen[x]==270){VAinput2$fluke_bag[5:22] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:22] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                                if (VAtableHCR$SeasonLen[x]==285){VAinput2$fluke_bag[5:23] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:23] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3; VAinput2$pd_multiplier[22] <- 2} else 
                                  if (VAtableHCR$SeasonLen[x]==300){VAinput2$fluke_bag[5:24] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:24] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3; VAinput2$pd_multiplier[22] <- 3} 
  VAinput2$fluke_bag <- ifelse(is.na(VAinput2$fluke_bag), 0, VAtableHCR$Bag[x])
  VAinput2$fluke_min <- ifelse(is.na(VAinput2$fluke_min), 100, VAtableHCR$MinLen[x])
  VAinputmiddleman <- na.omit(VAinput2)
  VAdatalist[[x]] <- VAinputmiddleman %>% mutate(Nsim = x)
}
VAbig_data = do.call(rbind, VAdatalist)


## -----------------------------------------------------------------------------

CTtableHCR <- rbc4 %>% filter(State == "CT")

CTdatalist = list()
CTdatalist = vector("list", length = n)

for (x in 1:length(CTtableHCR$State)){
  CTinput2 <- blankinput$CTinput2
  if(CTtableHCR$SeasonLen[x]==60){CTinput2$fluke_bag[13:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[13:16] = CTtableHCR$MinLen[x]} else 
    if(CTtableHCR$SeasonLen[x]==75){CTinput2$fluke_bag[12:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[12:16] = CTtableHCR$MinLen[x]} else
      if (CTtableHCR$SeasonLen[x]==90){CTinput2$fluke_bag[11:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[11:16] = CTtableHCR$MinLen[x]} else 
        if (CTtableHCR$SeasonLen[x]==105){CTinput2$fluke_bag[10:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[10:16] = CTtableHCR$MinLen[x]} else
          if (CTtableHCR$SeasonLen[x]==120){CTinput2$fluke_bag[9:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:16] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
            if (CTtableHCR$SeasonLen[x]==135){CTinput2$fluke_bag[9:17] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:17] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
              if (CTtableHCR$SeasonLen[x]==150){CTinput2$fluke_bag[9:18] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:18] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                if (CTtableHCR$SeasonLen[x]==165){CTinput2$fluke_bag[9:19] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:19] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                  if (CTtableHCR$SeasonLen[x]==180){CTinput2$fluke_bag[9:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                    if (CTtableHCR$SeasonLen[x]==195){CTinput2$fluke_bag[8:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[8:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 2.25} else 
                      if (CTtableHCR$SeasonLen[x]==210){CTinput2$fluke_bag[7:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[7:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 3.25} else 
                        if (CTtableHCR$SeasonLen[x]==225){CTinput2$fluke_bag[6:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[6:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 4.25} else 
                          if (CTtableHCR$SeasonLen[x]==240){CTinput2$fluke_bag[5:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25} else 
                            if (CTtableHCR$SeasonLen[x]==255){CTinput2$fluke_bag[5:21] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:21] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 2} else 
                              if (CTtableHCR$SeasonLen[x]==270){CTinput2$fluke_bag[5:22] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:22] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 3} else 
                                if (CTtableHCR$SeasonLen[x]==285){CTinput2$fluke_bag[5:23] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:23] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 4} else 
                                  if (CTtableHCR$SeasonLen[x]==300){CTinput2$fluke_bag[5:24] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:24] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 5} 
  CTinput2$fluke_bag <- ifelse(is.na(CTinput2$fluke_bag), 0, CTtableHCR$Bag[x])
  CTinput2$fluke_min <- ifelse(is.na(CTinput2$fluke_min), 100, CTtableHCR$MinLen[x])
  CTinputmiddleman <- na.omit(CTinput2)
  CTdatalist[[x]] <- CTinputmiddleman %>% mutate(Nsim = x)
}
CTbig_data = do.call(rbind, CTdatalist)


## -----------------------------------------------------------------------------

DEtableHCR <- rbc4 %>% filter(State == "DE")

DEdatalist = list()
DEdatalist = vector("list", length = n)

for (x in 1:length(DEtableHCR$State)){
  DEinput2 <- blankinput$DEinput2
  if(DEtableHCR$SeasonLen[x]==60){DEinput2$fluke_bag[13:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[13:16] = DEtableHCR$MinLen[x]} else 
    if(DEtableHCR$SeasonLen[x]==75){DEinput2$fluke_bag[12:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[12:16] = DEtableHCR$MinLen[x]} else
      if (DEtableHCR$SeasonLen[x]==90){DEinput2$fluke_bag[11:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[11:16] = DEtableHCR$MinLen[x]} else 
        if (DEtableHCR$SeasonLen[x]==105){DEinput2$fluke_bag[10:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[10:16] = DEtableHCR$MinLen[x]} else
          if (DEtableHCR$SeasonLen[x]==120){DEinput2$fluke_bag[9:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:16] = DEtableHCR$MinLen[x]} else 
            if (DEtableHCR$SeasonLen[x]==135){DEinput2$fluke_bag[9:17] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:17] = DEtableHCR$MinLen[x]} else 
              if (DEtableHCR$SeasonLen[x]==150){DEinput2$fluke_bag[9:18] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:18] = DEtableHCR$MinLen[x]} else 
                if (DEtableHCR$SeasonLen[x]==165){DEinput2$fluke_bag[9:19] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:19] = DEtableHCR$MinLen[x]} else 
                  if (DEtableHCR$SeasonLen[x]==180){DEinput2$fluke_bag[9:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:20] = DEtableHCR$MinLen[x]} else 
                    if (DEtableHCR$SeasonLen[x]==195){DEinput2$fluke_bag[8:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[8:20] = DEtableHCR$MinLen[x]} else 
                      if (DEtableHCR$SeasonLen[x]==210){DEinput2$fluke_bag[7:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[7:20] = DEtableHCR$MinLen[x]} else 
                        if (DEtableHCR$SeasonLen[x]==225){DEinput2$fluke_bag[6:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[6:20] = DEtableHCR$MinLen[x]} else 
                          if (DEtableHCR$SeasonLen[x]==240){DEinput2$fluke_bag[5:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:20] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2} else 
                            if (DEtableHCR$SeasonLen[x]==255){DEinput2$fluke_bag[5:21] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:21] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2} else 
                              if (DEtableHCR$SeasonLen[x]==270){DEinput2$fluke_bag[5:22] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:22] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 2} else 
                                if (DEtableHCR$SeasonLen[x]==285){DEinput2$fluke_bag[5:23] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:23] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 3} else 
                                  if (DEtableHCR$SeasonLen[x]==300){DEinput2$fluke_bag[5:24] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:24] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 4} 
  DEinput2$fluke_bag <- ifelse(is.na(DEinput2$fluke_bag), 0, DEtableHCR$Bag[x])
  DEinput2$fluke_min <- ifelse(is.na(DEinput2$fluke_min), 100, DEtableHCR$MinLen[x])
  DEinputmiddleman <- na.omit(DEinput2)
  DEdatalist[[x]] <- DEinputmiddleman %>% mutate(Nsim = x)
}
DEbig_data = do.call(rbind, DEdatalist)


## -----------------------------------------------------------------------------

MAtableHCR <- rbc4 %>% filter(State == "MA")

MAdatalist = list()
MAdatalist = vector("list", length = n)

for (x in 1:length(MAtableHCR$State)){
  MAinput2 <- blankinput$MAinput2
  if(MAtableHCR$SeasonLen[x]==60){MAinput2$fluke_bag[13:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[13:16] = MAtableHCR$MinLen[x]} else 
    if(MAtableHCR$SeasonLen[x]==75){MAinput2$fluke_bag[12:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[12:16] = MAtableHCR$MinLen[x]} else
      if (MAtableHCR$SeasonLen[x]==90){MAinput2$fluke_bag[11:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[11:16] = MAtableHCR$MinLen[x]} else 
        if (MAtableHCR$SeasonLen[x]==105){MAinput2$fluke_bag[10:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[10:16] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- (1/0.563)} else
          if (MAtableHCR$SeasonLen[x]==120){MAinput2$fluke_bag[9:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:16] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
            if (MAtableHCR$SeasonLen[x]==135){MAinput2$fluke_bag[9:17] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:17] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
              if (MAtableHCR$SeasonLen[x]==150){MAinput2$fluke_bag[9:18] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:18] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                if (MAtableHCR$SeasonLen[x]==165){MAinput2$fluke_bag[9:19] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:19] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                  if (MAtableHCR$SeasonLen[x]==180){MAinput2$fluke_bag[9:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                    if (MAtableHCR$SeasonLen[x]==195){MAinput2$fluke_bag[8:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[8:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 2+(1/0.563)} else 
                      if (MAtableHCR$SeasonLen[x]==210){MAinput2$fluke_bag[7:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[7:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 3+(1/0.563)} else 
                        if (MAtableHCR$SeasonLen[x]==225){MAinput2$fluke_bag[6:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[6:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 4+(1/0.563)} else 
                          if (MAtableHCR$SeasonLen[x]==240){MAinput2$fluke_bag[5:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563)} else 
                            if (MAtableHCR$SeasonLen[x]==255){MAinput2$fluke_bag[5:21] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:21] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 2} else 
                              if (MAtableHCR$SeasonLen[x]==270){MAinput2$fluke_bag[5:22] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:22] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 3} else 
                                if (MAtableHCR$SeasonLen[x]==285){MAinput2$fluke_bag[5:23] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:23] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 4} else 
                                  if (MAtableHCR$SeasonLen[x]==300){MAinput2$fluke_bag[5:24] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:24] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563);  MAinput2$pd_multiplier[20] <- 5} 
  MAinput2$fluke_bag <- ifelse(is.na(MAinput2$fluke_bag), 0, MAtableHCR$Bag[x])
  MAinput2$fluke_min <- ifelse(is.na(MAinput2$fluke_min), 100, MAtableHCR$MinLen[x])
  if (MAinput2$fluke_bag[19]!=0){MAinput2$pd_multiplier[19] <- (1/0.6)}
  MAinputmiddleman <- na.omit(MAinput2)
  MAdatalist[[x]] <- MAinputmiddleman %>% mutate(Nsim = x)
}
MAbig_data = do.call(rbind, MAdatalist)
#View(MAbig_data)

#order doesn't matter
#Big_Data <- rbind(CTbig_data, DEbig_data, MAbig_data, MDbig_data, NCbig_data, NJbig_data, NYbig_data, 
                  #RIbig_data, VAbig_data)

Big_Data <- rbind(MAbig_data, RIbig_data, CTbig_data, NYbig_data, NJbig_data, DEbig_data, MDbig_data, 
                  VAbig_data, NCbig_data)

Big_Data <- Big_Data %>% arrange(Big_Data$Nsim)
#View(Big_Data)

n = 560 

bigdatalist = list()
bigdatalist = vector("list", length = n)
for (x in 1:560){
  Big_Data2 <- Big_Data %>% 
    filter(Nsim == x )
  # Big_Data2$Nsim <- NULL
  bigdatalist[[x]] <- Big_Data2
}  

#View(bigdatalist[[315]])
#View(directed_trips_table)

#RDM FOR EACH SET OF REGULATIONS AND EACH LENGTH DISTRIBUTION 

pkgs_to_use <- c("tidyr",
                 "magrittr",
                 #"reshape2",
                 #"splitstackshape",
                 #"doBy",
                 #"WriteXLS",
                 #'Rcpp',
                 #"ggplot2",
                 "dplyr",
                 "rlist",
                 #"fitdistrplus",
                 #"MASS",
                 #"psych",
                 #"rgl",
                 "copula",
                 #"VineCopula",
                 #"scales",
                 #"univariateML",
                 #"logspline",
                 #"readr",
                 "data.table",
                 "conflicted")
#install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# library(readxl)
# library(writexl)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
#conflicts_prefer(copula::`%in%`) #why do we need this all of a sudden?
### 

#year of the prediction
#iyr <- 2040
#iyr <- as.integer(args[1])
#hcr_output <- args[2]

n = 560

RDMoutput_edit = list()
RDMoutput_edit = vector("list", length = n)
#x<-1

#set seed 
set.seed(444)

for (x in 1:560){
  
  mgmt_scen <- 1
  directed_trips_table <- bigdatalist[[x]] %>% 
    tibble() %>% 
    rename(dtrip_2019 = dtrip2019) %>% 
    mutate(state = factor(state, levels = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"))) 
  directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  
  #directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))
  #directed_trips_table <- readRDS("directed_trips_regions_bimonthly.rds")
  #directed_trips_table <- readxl::read_xlsx("directed_trips_regions_bimonthly_test.xlsx")
  #  for (i in c(3:8)) {
  # #    xx <- readxl::read_xlsx(paste0("regulations_option",i,".xlsx"))
  #    directed_trips_table <- readRDS(paste0("regulations_option",i,".rds"))
  #     directed_trips_table$state <- factor(directed_trips_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #     directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  #    saveRDS(directed_trips_table,file=paste0("regulations_option",i,".rds"))
  #   }
  #directed_trips_table <- readRDS("coastwide_regulations_scenario.rds")
  # Input the calibration output which contains the number of choice occasions needed to simulate
  #calibration_data = data.frame(readxl::read_excel("calibration_output_by_period.xlsx"))
  calibration_data_table <- readRDS("calibration_output_by_period.rds")
  #utility parameter draws
  #param_draws_all <- readRDS("param_draws_all.rds")
  source("gen_params.R")
  #costs
  #costs_new <- readRDS( "costs_all.rds")
  #costs_new <- readRDS( "costs_all_1000.rds")
  costs_new <- readRDS( "costs_alt.rds")
  # for (i in 1:9) costs_new[[i]] <- costs_new[[i]] %>% filter(tripid<=1000)
  # saveRDS(costs_new, file = "costs_all_1000.rds")
  
  #selectivity (read in moved from the otehr file)
  selectivity <- readRDS("rec_selectivity_20210422.rds")
  
  ## OM 2 ##
  # MRIP bias
  #calibration_data_table <- readRDS("calibration_output_by_period_lb.rds")
  #calibration_data_table$state <- factor(calibration_data_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #calibration_data_table <- split(calibration_data_table, calibration_data_table$state)
  #saveRDS(calibration_data_table, file = "calibration_output_by_period_lb.rds")
  #selectivity <- readRDS("rec_selectivity_by_state_cdf_star_raw_18_19_lb.rds")
  #xx <- readxl::read_excel("rec_selectivity_by_state_cdf_star_raw_18_19_lb.xlsx")
  #saveRDS(xx,file="rec_selectivity_by_state_cdf_star_raw_18_19_lb.rds")
  
  
  ## OM 3 ##
  ## read in biomass shift params
  # the adjustment gets made in predicted catch per trip.
  #prop_rel <- readRDS('region_availability.rds') %>% 
  #  #ungroup() %>% 
  #  #mutate(region = fct_relevel(region,c("NO","NJ","SO"))) %>% 
  #  #arrange(region) %>% 
  #  filter(year == iyr) %>% 
  #  select(prop_rel) %>% 
  #  t() %>% as.numeric()
  #print("proportions!")
  #print(prop_rel)  
  #comment out below if OM 3
  prop_rel <- c(1,1,1)
  
  Nlen <- 42 
  Nlengthbin <- bigdatalist[[x]]$Nsim[1]
  
  #cycle each set of regulations through each OM length structure
  #560/5
  
  #first trying 112 samples out of each parent directory to keep unique regulations
  
  # SCEN 1_10 1:112
    
    #pull out everything in parent and sub-directory
  
    #fitting to base scenario OM SSB 
    dat_directory <- "~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3_seedRDM"

      dat_files <- list.files(path = dat_directory, pattern = "\\.dat$", 
                            full.names = TRUE, recursive = TRUE)
    
    #keep only length structure files 
    subset_files <- dat_files[basename(dat_files) %in% c("om-length2020.dat", "om-length2021.dat","om-length2022.dat","om-length2023.dat","om-length2024.dat",
                                                         "om-length2025.dat", "om-length2026.dat","om-length2027.dat","om-length2028.dat","om-length2029.dat",
                                                         "om-length2030.dat", "om-length2031.dat","om-length2032.dat","om-length2033.dat","om-length2034.dat",
                                                         "om-length2035.dat", "om-length2036.dat","om-length2037.dat","om-length2038.dat","om-length2039.dat",
                                                         "om-length2040.dat", "om-length2041.dat","om-length2042.dat","om-length2043.dat","om-length2044.dat",
                                                         "om-length2045.dat")] 
    
    #randomly sample a length structure, but re-sample if it breaks RDM 
    repeat{
      tryCatch({ 
       
    selected_file <- sample(subset_files, 1)
    om_length_cm <- {scan(selected_file,n=Nlen+1)} 
    
    #extract the subdirectory name
    subdir <- dirname(selected_file) #sub-sub-directory 
    subdir2 <- dirname(subdir) #sub-directory 
    subdir_name <- basename(subdir) #sub-sub-directory
    subdir_name2 <- basename(subdir2) #sub-directory
    
    #pull out the spawbio from that same subdirectory 
    subdir_files <- list.files(path = subdir, pattern = paste0("^", "spawbio.out", "$"), 
                               full.names = TRUE, recursive = FALSE)
    spawbio_character <- {scan(subdir_files, what = "spawbio.out")} 
    spawbio <- as.numeric(spawbio_character)
    
    #match the subdir spawbio to the same timestep of the length structure file <- skips 2019 start values 
    if (subdir_name2 == "scen1_10" & subdir_name == "sim1_10"){spawbio_storelength <- spawbio[29:54]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim11_20"){spawbio_storelength <- spawbio[56:81]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim21_30"){spawbio_storelength <- spawbio[83:108]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim31_40"){spawbio_storelength <- spawbio[110:135]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim41_50"){spawbio_storelength <- spawbio[137:162]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim51_60"){spawbio_storelength <- spawbio[164:189]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim61_70"){spawbio_storelength <- spawbio[191:216]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim71_80"){spawbio_storelength <- spawbio[218:243]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim81_90"){spawbio_storelength <- spawbio[245:270]}
    if (subdir_name2 == "scen1_10" & subdir_name == "sim91_100"){spawbio_storelength <- spawbio[272:297]}
    
    if (subdir_name2 == "scen11_20" & subdir_name == "sim1_10"){spawbio_storelength <- spawbio[299:324]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim11_20"){spawbio_storelength <- spawbio[326:351]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim21_30"){spawbio_storelength <- spawbio[353:378]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim31_40"){spawbio_storelength <- spawbio[380:405]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim41_50"){spawbio_storelength <- spawbio[407:432]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim51_60"){spawbio_storelength <- spawbio[434:459]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim61_70"){spawbio_storelength <- spawbio[461:486]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim71_80"){spawbio_storelength <- spawbio[488:513]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim81_90"){spawbio_storelength <- spawbio[515:540]}
    if (subdir_name2 == "scen11_20" & subdir_name == "sim91_100"){spawbio_storelength <- spawbio[542:567]}
    
    if (subdir_name2 == "scen21_30" & subdir_name == "sim1_10"){spawbio_storelength <- spawbio[569:594]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim11_20"){spawbio_storelength <- spawbio[596:621]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim21_30"){spawbio_storelength <- spawbio[623:648]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim31_40"){spawbio_storelength <- spawbio[650:675]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim41_50"){spawbio_storelength <- spawbio[677:702]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim51_60"){spawbio_storelength <- spawbio[704:729]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim61_70"){spawbio_storelength <- spawbio[731:756]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim71_80"){spawbio_storelength <- spawbio[758:783]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim81_90"){spawbio_storelength <- spawbio[785:810]}
    if (subdir_name2 == "scen21_30" & subdir_name == "sim91_100"){spawbio_storelength <- spawbio[812:837]}
    
    if (subdir_name2 == "scen31_40" & subdir_name == "sim1_10"){spawbio_storelength <- spawbio[839:864]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim11_20"){spawbio_storelength <- spawbio[866:891]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim21_30"){spawbio_storelength <- spawbio[893:918]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim31_40"){spawbio_storelength <- spawbio[920:945]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim41_50"){spawbio_storelength <- spawbio[947:972]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim51_60"){spawbio_storelength <- spawbio[974:999]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim61_70"){spawbio_storelength <- spawbio[1001:1026]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim71_80"){spawbio_storelength <- spawbio[1028:1053]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim81_90"){spawbio_storelength <- spawbio[1055:1080]}
    if (subdir_name2 == "scen31_40" & subdir_name == "sim91_100"){spawbio_storelength <- spawbio[1082:1107]}
    
    if (subdir_name2 == "scen41_50" & subdir_name == "sim1_10"){spawbio_storelength <- spawbio[1109:1134]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim11_20"){spawbio_storelength <- spawbio[1136:1161]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim21_30"){spawbio_storelength <- spawbio[1163:1188]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim31_40"){spawbio_storelength <- spawbio[1190:1215]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim41_50"){spawbio_storelength <- spawbio[1217:1242]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim51_60"){spawbio_storelength <- spawbio[1244:1269]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim61_70"){spawbio_storelength <- spawbio[1271:1296]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim71_80"){spawbio_storelength <- spawbio[1298:1323]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim81_90"){spawbio_storelength <- spawbio[1325:1350]}
    if (subdir_name2 == "scen41_50" & subdir_name == "sim91_100"){spawbio_storelength <- spawbio[1352:1377]}
    
    SSB[x] <-
      if (basename(selected_file) == "om-length2020.dat"){SSB <- spawbio_storelength[1]} else
        if (basename(selected_file) == "om-length2021.dat"){SSB <- spawbio_storelength[2]} else
          if (basename(selected_file) == "om-length2022.dat"){SSB <- spawbio_storelength[3]} else
            if (basename(selected_file) == "om-length2023.dat"){SSB <- spawbio_storelength[4]} else
              if (basename(selected_file) == "om-length2024.dat"){SSB <- spawbio_storelength[5]} else
                if (basename(selected_file) == "om-length2025.dat"){SSB <- spawbio_storelength[6]} else
                  if (basename(selected_file) == "om-length2026.dat"){SSB <- spawbio_storelength[7]} else
                    if (basename(selected_file) == "om-length2027.dat"){SSB <- spawbio_storelength[8]} else
                      if (basename(selected_file) == "om-length2028.dat"){SSB <- spawbio_storelength[9]} else
                        if (basename(selected_file) == "om-length2029.dat"){SSB <- spawbio_storelength[10]} else
                          if (basename(selected_file) == "om-length2030.dat"){SSB <- spawbio_storelength[11]} else
                            if (basename(selected_file) == "om-length2031.dat"){SSB <- spawbio_storelength[12]} else
                              if (basename(selected_file) == "om-length2032.dat"){SSB <- spawbio_storelength[13]} else
                                if (basename(selected_file) == "om-length2033.dat"){SSB <- spawbio_storelength[14]} else
                                  if (basename(selected_file) == "om-length2034.dat"){SSB <- spawbio_storelength[15]} else
                                    if (basename(selected_file) == "om-length2035.dat"){SSB <- spawbio_storelength[16]} else
                                      if (basename(selected_file) == "om-length2036.dat"){SSB <- spawbio_storelength[17]} else
                                        if (basename(selected_file) == "om-length2037.dat"){SSB <- spawbio_storelength[18]} else
                                          if (basename(selected_file) == "om-length2038.dat"){SSB <- spawbio_storelength[19]} else
                                            if (basename(selected_file) == "om-length2039.dat"){SSB <- spawbio_storelength[20]} else
                                              if (basename(selected_file) == "om-length2040.dat"){SSB <- spawbio_storelength[21]} else
                                                if (basename(selected_file) == "om-length2041.dat"){SSB <- spawbio_storelength[22]} else
                                                  if (basename(selected_file) == "om-length2042.dat"){SSB <- spawbio_storelength[23]} else
                                                    if (basename(selected_file) == "om-length2043.dat"){SSB <- spawbio_storelength[24]} else
                                                      if (basename(selected_file) == "om-length2044.dat"){SSB <- spawbio_storelength[25]} else
                                                        if (basename(selected_file) == "om-length2045.dat"){SSB <- spawbio_storelength[26]}
  
  
  #cm2in <- read_csv("cm2in.csv", col_names = FALSE, show_col_types = FALSE)
  cm2in <- readRDS("cm2in.rds")
  lenbinuse <- as.integer(unlist(cm2in[,1]))
  Nlen_in <- length(lenbinuse)
  cm2in <- cm2in %>%
    dplyr::select(-1) %>%
    as.matrix() %>%
    I()
  om_length_in <- om_length_cm[-1] %*% t(cm2in)
  # size_data <- data.frame(fitted_prob = rep(om_length_in,3),
  #                         fitted_length = rep(lenbinuse,3),
  #                         region = rep(c("SO","NJ","NO"), each = Nlen_in),
  #                         year = rep("y2",3*Nlen_in))
  
  
  # catch-at-length and catch-per-trip for summer flounder
  
  #go into this directory here and tryCatch to sample again if this occurs
  #might be easier to just put all of source code in this script rather than jumping back and forth between scripts 
 
 # source("catch at length given stock structure - prediction.R")
  
  #SOURCE CODE START HERE
  
  numbers_at_length <- tibble(l_in_bin = lenbinuse,
                              N_l = om_length_in[1,])
  
  
  #Translate numbers from 1,000's of fish
  numbers_at_length$N_l=numbers_at_length$N_l*1000
  sum(numbers_at_length$N_l)
  
  # Import and merge the selectivity data to this file 
  #selectivity = data.frame(read_excel("rec_selectivity_by_state_cdf_star_raw_18_19.xlsx")) %>% 
  #selectivity <- readRDS("rec_selectivity.rds") %>% 
  selectivity <- selectivity %>% #readRDS("rec_selectivity_20210422.rds") %>% 
    subset(select=c(l_in_bin, state, q, E, C_l))
  
  numbers_at_length_new =  merge(selectivity,numbers_at_length,by="l_in_bin", all.x=TRUE, all.y=TRUE)
  
  
  numbers_at_length_new[is.na(numbers_at_length_new)] = 0
  numbers_at_length_new <-subset(numbers_at_length_new, N_l!=0 & state!=0)
  
  # Create catch-at-length based on the new numbers-at-length
  numbers_at_length_new$q = as.numeric(numbers_at_length_new$q)
  numbers_at_length_new$C_l_new = (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
  sum(numbers_at_length_new$C_l_new)
  
  
  
  # subset the catch-at-length new datstet by region
  numbers_at_length_MA <-subset(numbers_at_length_new, state=="MA", select=c(l_in_bin, C_l_new))
  numbers_at_length_RI <-subset(numbers_at_length_new, state=="RI", select=c(l_in_bin, C_l_new))
  numbers_at_length_CT <-subset(numbers_at_length_new, state=="CT", select=c(l_in_bin, C_l_new))
  numbers_at_length_NY <-subset(numbers_at_length_new, state=="NY", select=c(l_in_bin, C_l_new))
  numbers_at_length_NJ <-subset(numbers_at_length_new, state=="NJ", select=c(l_in_bin, C_l_new))
  numbers_at_length_DE <-subset(numbers_at_length_new, state=="DE", select=c(l_in_bin, C_l_new))
  numbers_at_length_MD <-subset(numbers_at_length_new, state=="MD", select=c(l_in_bin, C_l_new))
  numbers_at_length_VA <-subset(numbers_at_length_new, state=="VA", select=c(l_in_bin, C_l_new))
  numbers_at_length_NC <-subset(numbers_at_length_new, state=="NC", select=c(l_in_bin, C_l_new))
  
  
  
  numbers_at_length_MA$C_l_new=round(numbers_at_length_MA$C_l_new)
  numbers_at_length_RI$C_l_new=round(numbers_at_length_RI$C_l_new)
  numbers_at_length_CT$C_l_new=round(numbers_at_length_CT$C_l_new)
  numbers_at_length_NY$C_l_new=round(numbers_at_length_NY$C_l_new)
  numbers_at_length_NJ$C_l_new=round(numbers_at_length_NJ$C_l_new)
  numbers_at_length_DE$C_l_new=round(numbers_at_length_DE$C_l_new)
  numbers_at_length_MD$C_l_new=round(numbers_at_length_MD$C_l_new)
  numbers_at_length_VA$C_l_new=round(numbers_at_length_VA$C_l_new)
  numbers_at_length_NC$C_l_new=round(numbers_at_length_NC$C_l_new)
  
  
  
  tot_cat_MA_predicted <- sum(numbers_at_length_MA$C_l_new)
  tot_cat_RI_predicted <- sum(numbers_at_length_RI$C_l_new)
  tot_cat_CT_predicted <- sum(numbers_at_length_CT$C_l_new)
  tot_cat_NY_predicted <- sum(numbers_at_length_NY$C_l_new)
  tot_cat_NJ_predicted <- sum(numbers_at_length_NJ$C_l_new)
  tot_cat_DE_predicted <- sum(numbers_at_length_DE$C_l_new)
  tot_cat_MD_predicted <- sum(numbers_at_length_MD$C_l_new)
  tot_cat_VA_predicted <- sum(numbers_at_length_VA$C_l_new)
  tot_cat_NC_predicted <- sum(numbers_at_length_NC$C_l_new)
  
  
  tot_cat_MA_base <- sum(subset(selectivity, state == "MA")$C_l)
  tot_cat_RI_base <- sum(subset(selectivity, state == "RI")$C_l)
  tot_cat_CT_base <- sum(subset(selectivity, state == "CT")$C_l)
  tot_cat_NY_base <- sum(subset(selectivity, state == "NY")$C_l)
  tot_cat_NJ_base <- sum(subset(selectivity, state == "NJ")$C_l)
  tot_cat_DE_base <- sum(subset(selectivity, state == "DE")$C_l)
  tot_cat_MD_base <- sum(subset(selectivity, state == "MD")$C_l)
  tot_cat_VA_base <- sum(subset(selectivity, state == "VA")$C_l)
  tot_cat_NC_base <- sum(subset(selectivity, state == "NC")$C_l)
  
  
  
  #Create a factor that expands total catch in the prediction year
  catch_expansion_factor_MA <- round(tot_cat_MA_predicted/tot_cat_MA_base, digits=4)
  catch_expansion_factor_RI <- round(tot_cat_RI_predicted/tot_cat_RI_base, digits=4)
  catch_expansion_factor_CT <- round(tot_cat_CT_predicted/tot_cat_CT_base, digits=4)
  catch_expansion_factor_NY <- round(tot_cat_NY_predicted/tot_cat_NY_base, digits=4)
  catch_expansion_factor_NJ <- round(tot_cat_NJ_predicted/tot_cat_NJ_base, digits=4)
  catch_expansion_factor_DE <- round(tot_cat_DE_predicted/tot_cat_DE_base, digits=4)
  catch_expansion_factor_MD <- round(tot_cat_MD_predicted/tot_cat_MD_base, digits=4)
  catch_expansion_factor_VA <- round(tot_cat_VA_predicted/tot_cat_VA_base, digits=4)
  catch_expansion_factor_NC <- round(tot_cat_NC_predicted/tot_cat_NC_base, digits=4)
  
  sum(numbers_at_length_new$C_l)
  sum(numbers_at_length_new$C_l_new)
  
  ##########
  # Here, execute the catch-per trip file. 
  # This file adjusts the expected number of catch per trip by population abundances. 
 # source("predicted catch per trip by state.R")
  
  #SOURCE 2 START HERE 
  
  nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_MA*prop_rel[1]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  
  nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  fit <- readRDS("catch_copula_NO_19.rds")
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="MA"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_ma <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_MA.xlsx") 
  #saveRDS(catch_data_sim, "predicted_catch_MA.rds")
  
  
  
  
  
  ###Rhode Island
  # catch_data <- read_excel("observed_catch_NO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_RI*prop_rel[1]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_NO_19.rds")
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="RI"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_ri <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_RI.xlsx") 
  #saveRDS(catch_data_sim, "predicted_catch_RI.rds")
  
  
  
  
  ###Connecticut
  # catch_data <- read_excel("observed_catch_NO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_CT*prop_rel[1]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_NO_19.rds")
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="CT"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_ct <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_CT.xlsx") 
  #saveRDS(catch_data_sim, "predicted_catch_CT.rds")
  
  
  
  ###NEw York
  # catch_data <- read_excel("observed_catch_NO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_NY*prop_rel[1]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_NO_19.rds")
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="NY"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_ny <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_NY.xlsx") 
  #saveRDS(catch_data_sim, "predicted_catch_NY.rds")
  
  
  
  
  ###New Jersey
  # catch_data <- read_excel("observed_catch_NJ_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  
  nbfit_sf <- readRDS("nb_catch_parameters_sf_NJ_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_NJ*prop_rel[2]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  
  nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NJ_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  fit <- readRDS("catch_copula_NJ_19.rds")
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="NJ"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_nj <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_NJ.xlsx") 
  #saveRDS(catch_data_sim, "predicted_catch_NJ.rds")
  
  
  
  ###Delaware
  # catch_data <- read_excel("observed_catch_SO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  
  nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_DE*prop_rel[3]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  fit <- readRDS("catch_copula_SO_19.rds")
  
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="DE"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_de <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_DE.xlsx")
  #saveRDS(catch_data_sim, "predicted_catch_DE.rds")
  
  
  
  ###Maryland
  # catch_data <- read_excel("observed_catch_SO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_MD*prop_rel[3]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_SO_19.rds")
  
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="MD"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_md <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_MD.xlsx")
  #saveRDS(catch_data_sim, "predicted_catch_MD.rds")
  
  
  
  
  ###Virginia
  # catch_data <- read_excel("observed_catch_SO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_VA*prop_rel[3]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_SO_19.rds")
  
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="VA"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_va <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  #write_xlsx(catch_data_sim, "predicted_catch_VA.xlsx")
  #saveRDS(catch_data_sim, "predicted_catch_VA.rds")
  
  
  
  
  ###North Carolina
  # catch_data <- read_excel("observed_catch_SO_19.xlsx")
  # 
  # sf <- catch_data$sf_tot_cat
  # bsb <- catch_data$bsb_tot_cat
  
  
  #estimate the nb parameters
  # nbfit_sf = fitdistr(sf, "Negative Binomial")
  # summary(nbfit_sf)
  
  #nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")
  
  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu
  
  sf_size <- nbfit_sf$estimate['size']
  sf_size
  
  
  # We now adjust mean catch per trip by the expansion factor. 
  # We will assume that in the prediction year under a new mean catch per trip, 
  # the SD relative to the mean remains as it was in the baseline year. 
  # We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  
  
  # From ?NegBinomial:
  # "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
  # and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
  # in this parametrization." 
  
  #calculate variance and CV in baseline year
  var_sf=sf_mu+(sf_mu^2)/sf_size
  cv_sf_base = sqrt(var_sf)/sf_mu
  cv_sf_base
  
  # Now adjust the mu parameter for prediction year distribution by the expansion factor
  sf_mu_new=sf_mu*catch_expansion_factor_NC*prop_rel[3]
  
  #solve for new size parameter 
  sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)
  
  #new variance and CV
  var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
  cv_sf_new = sqrt(var_sf_new)/sf_mu_new
  
  #Check that CV old and CV new are the same
  cv_sf_base
  cv_sf_new
  
  
  # Get the black sea bass parameters 
  # nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
  # nbfit_bsb
  #nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")
  
  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu
  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size
  
  
  #t copula
  # t_cop_model <- tCopula(dim = 2)
  # m <- pobs(as.matrix(cbind(sf,bsb)))
  # fit <- fitCopula(t_cop_model, m, method = 'ml')
  # fit
  # coef(fit)
  
  #fit <- readRDS("catch_copula_SO_19.rds")
  
  
  # Set the parameters
  rho <- coef(fit)[1]
  df <- coef(fit)[2]
  
  
  t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                      paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                        list(mu=bsb_mu, size=bsb_size)))
  
  #HERE is where we get the warning
  sim_t_cop_nb <- rMvdc(30000, t_copula_nb )
  
  sf_t_nb=sim_t_cop_nb[,1]
  bsb_t_nb=sim_t_cop_nb[,2]
  
  mean(sf_t_nb)
  
  region="NC"
  #catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
  sf_catch_data_nc <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
  
  #SOURCE 2 END HERE 
  
  #####
  
  #####
  numbers_at_length_MA <- numbers_at_length_MA %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("MA",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  
  numbers_at_length_RI <- numbers_at_length_RI %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("RI",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_CT <- numbers_at_length_CT %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("CT",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_NY <- numbers_at_length_NY %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("NY",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_NJ <- numbers_at_length_NJ %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("NJ",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_DE <- numbers_at_length_DE %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("DE",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_MD <- numbers_at_length_MD %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("MD",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_VA <- numbers_at_length_VA %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("VA",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  numbers_at_length_NC <- numbers_at_length_NC %>% 
    rename(fitted_length = l_in_bin,
           nfish = C_l_new) %>% 
    mutate(fitted_prob = nfish/sum(.$nfish),
           region = rep("NC",nrow(.)),
           year = rep("y2", nrow(.))) %>% 
    I()
  
  #combine the datasets
  fitted_sizes_region_all_y2 <- bind_rows(numbers_at_length_MA, numbers_at_length_RI,
                                          numbers_at_length_CT, numbers_at_length_NY,
                                          numbers_at_length_NJ, numbers_at_length_DE, 
                                          numbers_at_length_MD, numbers_at_length_VA,numbers_at_length_NC )
  
  # fitted_sizes_region_all_y2 = as.data.frame(bind_rows(numbers_at_length_MA, numbers_at_length_RI,
  #                                                      numbers_at_length_CT, numbers_at_length_NY,
  #                                                      numbers_at_length_NJ, numbers_at_length_DE, 
  #                                                      numbers_at_length_MD, numbers_at_length_VA))
  
  #fitted_sizes_region_all_y2 <- subset(fitted_sizes_region_all_y2, select=c(fitted_prob, fitted_length, region, year))
  size_data_read <- fitted_sizes_region_all_y2 %>% 
    tibble() %>% 
    select(fitted_prob, fitted_length, region, year) %>% 
    group_by(region) %>% 
    arrange(fitted_length) %>% 
    mutate(cdf = cumsum(fitted_prob)) %>% 
    ungroup() %>% 
    mutate(region = factor(region, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))) %>% 
    split(.$region)
  
  #SOURCE END HERE 
  
  # #catch data
  # sf_catch_data_ma <- readRDS("predicted_catch_MA.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_ri <- readRDS("predicted_catch_RI.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_ct <- readRDS("predicted_catch_CT.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_ny <- readRDS("predicted_catch_NY.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_de <- readRDS("predicted_catch_DE.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_md <- readRDS("predicted_catch_MD.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_va <- readRDS("predicted_catch_VA.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_nc <- readRDS("predicted_catch_NC.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  
  # Read-in the current population length composition  #don't need this in final as it's already an object.
  #size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))
  #size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
  
  #calibration_data_table$state <- factor(calibration_data_table$state,levels=c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #calibration_data_table <- split(calibration_data_table %>% tibble(),calibration_data_table$state)
  #directed_trips_table$state <- factor(directed_trips_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  #size_data_read$region <- factor(size_data_read$region, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #size_data_read <- split(size_data_read, size_data_read$region)
  
  # loop over states
  params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"),
                 region1 = c(rep("NO",4),"NJ",rep("SO",4)),
                 calibration_data_table = calibration_data_table, #rep(list(calibration_data_table),9),
                 directed_trips_table = directed_trips_table, #rep(list(directed_trips_table),9),   #split(directed_trips_table,directed_trips_table$state), #
                 size_data_read = size_data_read, #rep(list(size_data_read),9),
                 param_draws_MA = param_draws_all,
                 costs_new_all_MA = costs_new,
                 
                 #sf_catch_data_all = c(rep(list(sf_catch_data_no),4),list(sf_catch_data_nj),rep(list(sf_catch_data_so),4)),
                 sf_catch_data_all = c(list(sf_catch_data_ma),list(sf_catch_data_ri),
                                       list(sf_catch_data_ct),list(sf_catch_data_ny),
                                       list(sf_catch_data_nj),list(sf_catch_data_de),
                                       list(sf_catch_data_md),list(sf_catch_data_va), list(sf_catch_data_nc)),
                 # prop_bsb_keep = #rep(0.33,9))  # add Lou's p* values here!
                 #   c(1-0.67,
                 #     1-0.66,
                 #     1-0.77,
                 #     1-0.87,
                 #     1-0.93,
                 #     1-0.945,
                 #     1-0.96,
                 #     1-0.92,
                 #     0.001),
                 prop_bsb_keep = c(
                   1-.53,
                   1-.38,
                   1-.7,
                   1-.83,
                   1-.92,
                   1-.942,
                   1-.96,
                   1-.92,
                   0.001), #1),
                 dchoose = rep(1,9),
                 mgmt_scen = rep(mgmt_scen,9)) #1-1.1))
  
  # params <- list(state1 = "MA",
  #                region1 = "NO",
  #                calibration_data_table = list(calibration_data_table),
  #                directed_trips_table = list(directed_trips_table),
  #                size_data_read = list(size_data_read),
  #                param_draws_MA = list(param_draws_all[[1]]),
  #                costs_new_all_MA = list(costs_new[[1]]),
  #                sf_catch_data_all = list(sf_catch_data_ma),
  #                prop_bsb_keep = 1-0.53,
  #                dchoose = 1)
  # 
  # params <- list(state1 = "NJ",
  #                region1 = "NJ",
  #                calibration_data_table = list(calibration_data_table),
  #                directed_trips_table = list(directed_trips_table),
  #                size_data_read = list(size_data_read),
  #                param_draws_MA = list(param_draws_all[[5]]),
  #                costs_new_all_MA = list(costs_new[[5]]),
  #                sf_catch_data_all = list(sf_catch_data_nj),
  #                prop_bsb_keep = 1-0.92,
  #                dchoose = 1)
  
  #source("prediction-all.R")
  #source("prediction-vec.R")
  source("prediction-vec-sim.R")
  
  # set.seed(1989)
  # simkeep <- NULL
  # simrel <- NULL
  # simagg <- NULL
  # for (jsim in 1:30) {
  # ##########  need to add link to OM scenario regulations
  # print(jsim)
  #params$dchoose <- rep(sample(1:1000,1),9)
  #profvis::profvis({
  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  xx <- purrr::pmap(params, safe_predict_rec_catch)
  #})
  
  # profvis::profvis(testMA <- predict_rec_catch(state1 = "MA",
  #                         region1 = "NO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[1]],
  #                         costs_new_all_MA = costs_new[[1]],
  #                         sf_catch_data_all = sf_catch_data_no,
  #                         prop_bsb_keep = 0.33))
  # 
  # xx <- predict_rec_catch(state1 = "NC",
  #                         region1 = "SO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[9]],
  #                         costs_new_all_MA = costs_new[[9]],
  #                         sf_catch_data_all = sf_catch_data_so)
  
  
  prediction_output_by_period <- purrr::map(xx, 1)
  
  #saveRDS(prediction_output_by_period, file = "prediction_output_by_period.rds")
  
  #aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
 
  #HERE is where it breaks when length distribution is too low
   aggregate_prediction_output <- prediction_output_by_period %>% 
    list.stack(fill = TRUE) %>% 
    mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
    dplyr::select(-state, -alt_regs) %>%  #, -period) %>% 
    group_by(sim) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) # %>% 
   
   print(paste("Worked -> proceed with RDM"))
   break}, 
error = function(e){
  message("Retrying from om_length.dat pull")
})
}
  #dplyr::select(order(colnames(.))) %>% 
  #I()
  #  = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
  #write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")
  #saveRDS(aggregate_prediction_output, file = "aggregate_prediction_output.rds") 
  ##########  
  
  pred_len <- tibble(aggregate_prediction_output) %>% 
    dplyr::select(contains("length")) %>% 
    pivot_longer(cols = 1:ncol(.), names_to = "bin",values_to = "num") %>% 
    separate(bin, into =c("type","len"),sep = "_length_") %>% 
    mutate(len = as.numeric(len)) #%>% 
  #I()
  mulen <- pred_len %>%
    group_by(type) %>%
    summarize(mulen = sum(len*num)/sum(num),
              .groups = "drop") %>% 
    pivot_wider(names_from = type,
                names_glue = "mulen_{type}",
                values_from = mulen) %>% 
    bind_cols(aggregate_prediction_output) %>%
    select(sim, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one, mulen_keep, mulen_release)
  #mulen
  
  biglen <- pred_len %>% 
    filter(type == "keep") %>% 
    mutate(numbig = ifelse(len>=28,num,0))  %>% 
    summarize(fracbig = sum(numbig)/sum(num))
  mulen <- bind_cols(mulen, biglen)  
  
  #pred_len
  out_lens <- tibble(type = rep(c("release","keep"),each=Nlen_in),
                     len = rep(lenbinuse,2)) %>% 
    left_join(pred_len, by = c("type", "len")) %>% 
    replace_na(list(num=0)) #%>% 
  #I()
  #out_lens
  #in2cm <- readr::read_csv("in2cm.csv", col_names = FALSE, show_col_types = FALSE)[,-1]
  in2cm <- readRDS("in2cm.rds")
  keep <- out_lens %>% 
    filter(type == "keep") %>% 
    dplyr::select(num) %>% 
    unlist()# %>%
  #I()
  keep <- keep %*% t(in2cm)
  release <- out_lens %>% 
    filter(type == "release") %>% 
    dplyr::select(num) %>% 
    unlist() #%>%
  #I()
  release <- release %*% t(in2cm)
  write.table(round(rbind(keep,release)/1000,3),file = "rec-catch.out", row.names = FALSE, col.names = FALSE)
  #write(with(aggregate_prediction_output,observed_trips),file = "rec-catch.out", append = TRUE)
  write.table(mulen,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
  
  extra_output <- prediction_output_by_period %>% 
    list.stack(fill = TRUE) %>% 
    mutate_at(vars(contains("length")), replace_na, replace = 0)
  
  pred_len2 <- tibble(extra_output) %>% 
    dplyr::select(state, contains("length")) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "bin",values_to = "num") %>% 
    separate(bin, into =c("type","len"),sep = "_length_") %>% 
    mutate(len = as.numeric(len))
  pred_len <- pred_len2 %>% 
    group_by(state, type) %>%
    summarize(mulen = sum(len*num)/sum(num), 
              .groups = "drop") %>% 
    pivot_wider(names_from = type,
                names_glue = "mulen_{type}",
                values_from = mulen)
  
  biglen <- pred_len2 %>% 
    filter(type == "keep") %>% 
    mutate(numbig = ifelse(len>=28,num,0))  %>% 
    group_by(state) %>% 
    summarize(fracbig = sum(numbig)/sum(num))
  pred_len <- left_join(pred_len, biglen, by = c("state"))  
  
  extra_output <- extra_output %>% 
    select(state, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one
    ) %>% 
    group_by(state) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) %>% 
    left_join(pred_len, by = c("state"))    
  RDMoutput_edit[[x]] <- extra_output %>% mutate(Nsim = x, SeasonLen = filltable$SeasonLen[x], Bag = filltable$Bag[x],
                                                 MinLen = filltable$MinLen[x], SSBcov = SSB[x])
  write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
}
#View(RDMoutput_edit[[x]])

#AGGREGATE OUTPUT 

RDMoutputbind_edit <- bind_rows(RDMoutput_edit)
#View(RDMoutputbind_edit)

#saveRDS(RDMoutputbind_edit, "~/Desktop/FlounderMSE/RDMgam/setseed/datafiles/RDMoutputbind_allOM_1BBMSY_setseed_final2.rds") #trial 1 <- cut at 2036

############################# FIT MODEL ########################################
library(mgcv)
library(gratia)

#read RDS
#RDMoutputbind_edit <- readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/datafiles/RDMoutputbind_allOM_1BBMSY_setseed_final.rds")
#RDMoutputbind_edit <- readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/datafiles/RDMoutputbind_allOM_1BBMSY_setseed_final_2.rds")

#hist(RDMoutputbind_edit$SSBcov)
#View(RDMoutputbind_edit)
#RDMoutputbind_edit <- View(RDMoutputbind_edit %>% filter(tot_keep == max(tot_keep)))

tot_keep <- RDMoutputbind_edit$tot_keep
tot_rel <- RDMoutputbind_edit$tot_rel
state <- RDMoutputbind_edit$state
SeasonLen <- RDMoutputbind_edit$SeasonLen
Bag <- RDMoutputbind_edit$Bag
MinLen <- RDMoutputbind_edit$MinLen
SSB <- RDMoutputbind_edit$SSBcov

inputdata <- data.frame(tot_keep, 
                        state, SeasonLen, Bag, MinLen, SSB) 

#View(inputdata)
hist(RDMoutputbind_edit$SSBcov, prob = TRUE, breaks = 15)

# --------------------------------------- GAM -----------------------------------------------#
#hierarchical <- need this next line for hierarchical GAM to work 
inputdata$state <- as.factor(inputdata$state)

#hierarchical without state as additional COV -> no longer using this
g1.1 <- gam(tot_keep ~  s(SSB, state, bs = "fs", k = 3) + 
              s(SeasonLen,  state, bs = "fs", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
              s(Bag,  state, bs = "fs", k = 3) + s(MinLen,  state, bs = "fs", k = 3), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML") #random effect on spline for minlen that introduces a sort of random effect by state 

summary(g1.1)
draw(g1.1)
appraise(g1.1)
g1.1pred <- predict(g1.1, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.1pred, xlim = c(0,3e7), ylim = c(0,3e7)) + abline(b = 1, a = 0, col = "red")

#with state as additional COV -> using this 
g1.1state <- gam(tot_keep ~  state + s(SSB, state, bs = "fs", k = 3) + 
                   s(SeasonLen, state, bs = "fs", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
                   s(Bag, state, bs = "fs", k = 3) + s(MinLen, state, bs = "fs", k = 3), 
                 data = inputdata, #either isolated RDM runs or total 
                 family = Gamma(link = log), method = "REML") 

#save RDS files 

#original
#saveRDS(g1.1state, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland_allOM_1BBMSY_1_state.rds")
#g1.1state <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland_allOM_1BBMSY_1_state.rds")

#set seed
#saveRDS(g1.1state, "~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_9_state_setseed.rds")
#g1.1state_prev <- readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_9_state_setseed.rds")

#set seed adjust RI-NC period treatment
#saveRDS(g1.1state, "~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_state_setseed_final.rds")
#g1.1state <- readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_state_setseed_final.rds")
#g1.1state <- readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_state_setseed_final_2.rds")

summary(g1.1state)
draw(g1.1state)
appraise(g1.1state)
gam.check(g1.1state)
g1.1pred <- predict(g1.1state, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.1pred, xlim = c(0,4e7), ylim = c(0,4e7)) + abline(b = 1, a = 0, col = "red")

#global smoother <- same diagnostics and deviations from global function look small
g1.1state_1 <- gam(tot_keep ~  state + s(SSB, k = 3) +  s(SSB, state, bs = "fs", k = 3) + 
                     s(SeasonLen, k = 3) + s(SeasonLen, state, bs = "fs", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
                     s(Bag, k = 3) + s(Bag, state, bs = "fs", k = 3) +
                     s(MinLen, k = 3) + s(MinLen, state, bs = "fs", k = 3), 
                   data = inputdata, #either isolated RDM runs or total 
                   family = Gamma(link = log), method = "REML") 

#tensor product 
g1.3 <- gam(tot_keep ~ t2(SSB, state, bs = c("tp", "re")) + 
              t2(SeasonLen, state, bs = c("tp", "re")) + #9,5,7 partial effect of smooths look strange with k any higher 
              t2(Bag, state, bs = c("tp", "re")) + t2(MinLen, state, bs = c("tp", "re")), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML")
summary(g1.3)
draw(g1.3) #this doesn't work for tensor product; use code below
appraise(g1.3)
g1.3pred <- predict(g1.3, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.3pred, xlim = c(0,3e7), ylim = c(0,3e7)) + abline(b = 1, a = 0, col = "red")

#plot smooths in way that shows each states' contribution
g1.3 <- g1.1state #only if you aren't plotting tensor product 

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SSB", sm$.smooth), ]
ggplot(sm_fs, aes(x = SSB, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Spawning Stock Biomass", x = "Spawning Stock Biomass", y = "Estimate", color = "state", fill = "state") + 
  theme(
    plot.title = element_text(size = 24),      
    axis.title.x = element_text(size = 20),    
    axis.title.y = element_text(size = 20),    
    axis.text.x = element_text(size = 18),     
    axis.text.y = element_text(size = 18)
  )

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SeasonLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = SeasonLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Season Length", x = "Season Length", y = "Estimate", color = "state", fill = "state") +
  theme(
    plot.title = element_text(size = 24),      
    axis.title.x = element_text(size = 20),    
    axis.title.y = element_text(size = 20),    
    axis.text.x = element_text(size = 18),     
    axis.text.y = element_text(size = 18)
  ) #+ theme(legend.position = "bottom")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("MinLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = MinLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Minimum Size Limit", x = "Minimum Size Limit", y = "Estimate", color = "state", fill = "state") +
  theme(
    plot.title = element_text(size = 24),      
    axis.title.x = element_text(size = 20),    
    axis.title.y = element_text(size = 20),    
    axis.text.x = element_text(size = 18),     
    axis.text.y = element_text(size = 18)
  )

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("Bag", sm$.smooth), ]
ggplot(sm_fs, aes(x = Bag, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Bag Limit", x = "Bag Limit", y = "Estimate", color = "state", fill = "state") +
  theme(
    plot.title = element_text(size = 24),      
    axis.title.x = element_text(size = 20),    
    axis.title.y = element_text(size = 20),    
    axis.text.x = element_text(size = 18),     
    axis.text.y = element_text(size = 18)
  )

#ggplot
inputdata %>% 
  mutate(Pred = g1.1pred) %>% 
  #filter(bag == 4, seaslen == 150) %>% 
  drop_na() %>% 
  ggplot() + 
  aes(y = tot_keep, x = Pred, col = SSB) + 
  geom_point(alpha=0.7) + 
  #geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  #scale_color_viridis_c() + 
  scale_color_viridis_c(breaks = c(25000, 50000, 75000, 100000, 125000)) + 
  labs(
    y = "Realized Harvest from RDM",
    x = "Expected harvest from GAM"
  ) + 
  ggtitle("GAM Predictions of Recreational Demand Model Harvest") +
  #facet_wrap(~bag) +
  xlim(0,4.5e+07) + #2e7
  ylim(0,4.5e+07) + 
# theme(legend.position = "bottom")
  NULL
