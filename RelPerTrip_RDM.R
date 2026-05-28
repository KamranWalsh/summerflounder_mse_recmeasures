library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)

#look at marginal effects of kept and released per trip of RDM

setwd("~/Desktop/FlounderMSE")

blankinput <- readRDS("blankinputtables.rds")

#FORM PLAUSIBLE DISTRIBUTION OF REGULATIONS 

sample_seas <- c(150,
                 150,
                 150,
                 150,
                 150,
                 
                 60, 
                 90,
                 120,
                 150, 
                 180, 
                 210,
                 240,
                 270, 
                 300,
                 
                 150,
                 150,
                 150,
                 150,
                 150,
                 150,
                 150)
sample_bag <- c(4,5,6,7,8, 
                4,4,4,4,4,4,4,4,4,
                4,4,4,4,4,4,4)
sample_minlen <- c(17.5, 17.5, 17.5, 17.5, 17.5,
                   17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5,
                  14,15,16,17,18,19,20)

state <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")

combinations_sample <- data.frame(SeasonLen = sample_seas, Bag = sample_bag,
                                   MinLen = sample_minlen)

rbc4 <- combinations_sample

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

#for filling in RDM output table below 

# sequential                      
filltable <- dplyr::select(rbc4[1:21,], SeasonLen, Bag, MinLen) 

Alternative = "scen1"
#extract regulations for each time step 
if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3_seedRDM/scen11_20/sim1_10")}
#if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin3/scen11_20/sim1_10")}

# CONVERT STARTING REGULATIONS INTO RDM INPUT FORMAT    
n = 21

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
Big_Data <- rbind(CTbig_data, DEbig_data, MAbig_data, MDbig_data, NCbig_data, NJbig_data, NYbig_data, 
                  RIbig_data, VAbig_data)

Big_Data <- Big_Data %>% arrange(Big_Data$Nsim)
#View(Big_Data)

n = 21 

bigdatalist = list()
bigdatalist = vector("list", length = n)
for (x in 1:21){
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
conflicts_prefer(copula::`%in%`) #why do we need this all of a sudden?
### 

#year of the prediction
#iyr <- 2040
#iyr <- as.integer(args[1])
#hcr_output <- args[2]

n = 21

RDMoutput_edit = list()
RDMoutput_edit = vector("list", length = n)

for (x in 1:21){
  
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
  
  # READ IN LENGTH DISTRIBUTION FOR EACH SET OF REGULATIONS x<-15
  Nlen <- 42 
  Nlengthbin <- bigdatalist[[x]]$Nsim[1]
  om_length_cm <- scan("~/Desktop/FlounderMSE/RDMgam/storelengths/allOMscen_allstartSSB_newersinatra/sim_storelength_allOMscenBin1/scen1_10/sim1_10/om-length2026.dat",n=Nlen+1) 
    
  SSB <-  35093.73
  
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
  source("catch at length given stock structure - prediction.R")
  
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
  aggregate_prediction_output <- prediction_output_by_period %>% 
    list.stack(fill = TRUE) %>% 
    mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
    dplyr::select(-state, -alt_regs) %>%  #, -period) %>% 
    group_by(sim) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) # %>% 
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
    select(state, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one) %>% 
    group_by(state) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) %>% 
    left_join(pred_len, by = c("state"))    
  RDMoutput_edit[[x]] <- extra_output %>% mutate(Nsim = x, SeasonLen = filltable$SeasonLen[x], Bag = filltable$Bag[x],
                                                 MinLen = filltable$MinLen[x], SSBcov = SSB)
  write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
}
#View(RDMoutput_edit[[x]])
#AGGREGATE OUTPUT 

RDMoutputbind_edit <- select(bind_rows(RDMoutput_edit), state, tot_keep,  SeasonLen, Bag, MinLen, SSBcov, tot_rel, observed_trips, n_choice_occasions)
#View(RDMoutputbind_edit)

all_results <- RDMoutputbind_edit
all_results_summed <- all_results %>% 
  mutate(group = rep(1:ceiling(n()/9), each = 9)[1:n()]) %>%
  group_by(group) %>%
  summarise(n_keep = sum(tot_keep, na.rm = TRUE), n_rel = sum(tot_rel, na.rm = TRUE),
            n_trips = sum(observed_trips, na.rm = TRUE), n_choice = sum(n_choice_occasions, na.rm = TRUE))

all_results <- all_results %>% filter(state == "CT") %>% 
  select(SeasonLen, Bag, MinLen, SSBcov) %>% 
  mutate(n_keep = all_results_summed$n_keep, n_rel = all_results_summed$n_rel,
         n_trips = all_results_summed$n_trips, n_choice = all_results_summed$n_choice)
#View(all_results)

diag_ts_bag <- head(all_results, 5) %>% 
  mutate(relpertrip = n_rel/n_trips,
         keeppertrip = n_keep/n_trips)

diag_ts_season <- all_results[6:14,] %>% 
  mutate(relpertrip = n_rel/n_trips,
         keeppertrip = n_keep/n_trips)

diag_ts_minlen <- all_results[15:21,] %>% 
  mutate(relpertrip = n_rel/n_trips,
         keeppertrip = n_keep/n_trips)

#bag
ggplot(diag_ts_bag, aes(x = Bag, y = keeppertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Bag Limit") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Bag Limit on Retention") 

ggplot(diag_ts_bag, aes(x = Bag, y = relpertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Bag Limit") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Bag Limit on Discards") 

#season
ggplot(diag_ts_season, aes(x = SeasonLen, y = keeppertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Season Length") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Season Length on Retention") 

ggplot(diag_ts_season, aes(x = SeasonLen, y = relpertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Season Length") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Season Length on Discards") 

#minlen
ggplot(diag_ts_minlen, aes(x = MinLen, y = keeppertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Minimum Size Limit") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Minimum Size Limit on Retention")

ggplot(diag_ts_minlen, aes(x = MinLen, y = relpertrip)) + 
  geom_line() + 
  geom_point() + 
  xlab("Minimum Size Limit") + 
  ylab("Discards per Trip") + 
  ggtitle("Effect of Minimum Size Limit on Discards")

#increasing minlen always decreases landings and increases discards across different population size information 
#increasing seasonlength has variable effect on discards but always increases landings across different population size information 
#bag limit has inconsistent effect on landings and discards across different population size information 

