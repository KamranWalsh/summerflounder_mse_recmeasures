## -----------------------------------------------------------------------------
#library(dplyr)
#library(tidyr)
#PCA_BioInformed_CatchTable_NYRegs <- readRDS("PCA_BiologicallyInformed_CatchTable_NYRegs.rds") # table from the BioInformedControlRule-CurrentRegstoTarget Script
PCA_BioInformed_CatchTable_NYRegs <- finaltable1
#inputtable <- readRDS('regulations_option1.rds')
blankinput <- readRDS("blankinputtables.rds") # here is the blank input table to populate with new regs

## -----------------------------------------------------------------------------
NJtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "NJ") # order of rows needs to be season, then minimum length, then bag limit. The  lookup table function sheet puts it in that order
  
for (x in 1:7){
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
if (x==1){NJinputseason <- NJinputmiddleman} 
if (x==2){NJinputsize <- NJinputmiddleman}
if (x==3){NJinputbag <- NJinputmiddleman}
if (x==4){NJinputflex <- NJinputmiddleman}
if (x==5){NJinputBL <- NJinputmiddleman}
if (x==6){NJinputBS <- NJinputmiddleman}
if (x==7){NJinputLS <- NJinputmiddleman}
}


## -----------------------------------------------------------------------------
NYtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "NY")

for (x in 1:7){
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
if (x==1){NYinputseason <- NYinputmiddleman}
if (x==2){NYinputsize <- NYinputmiddleman}
if (x==3){NYinputbag <- NYinputmiddleman}
if (x==4){NYinputflex <- NYinputmiddleman}
if (x==5){NYinputBL <- NYinputmiddleman}
if (x==6){NYinputBS <- NYinputmiddleman}
if (x==7){NYinputLS <- NYinputmiddleman}
}


## -----------------------------------------------------------------------------
NCtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "NC")

for (x in 1:7){
NCinput2 <- blankinput$NCinput2
if(NCtableHCR$SeasonLen[x]==60){NCinput2$fluke_bag[13:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[13:16] = NCtableHCR$MinLen[x]} else 
  if(NCtableHCR$SeasonLen[x]==75){NCinput2$fluke_bag[12:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[12:16] = NCtableHCR$MinLen[x]} else
    if (NCtableHCR$SeasonLen[x]==90){NCinput2$fluke_bag[11:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[11:16] = NCtableHCR$MinLen[x]} else 
      if (NCtableHCR$SeasonLen[x]==105){NCinput2$fluke_bag[10:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[10:16] = NCtableHCR$MinLen[x]} else
        if (NCtableHCR$SeasonLen[x]==120){NCinput2$fluke_bag[9:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:16] = NCtableHCR$MinLen[x]} else 
          if (NCtableHCR$SeasonLen[x]==135){NCinput2$fluke_bag[9:17] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:17] = NCtableHCR$MinLen[x]} else 
            if (NCtableHCR$SeasonLen[x]==150){NCinput2$fluke_bag[9:18] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:18] = NCtableHCR$MinLen[x]} else 
              if (NCtableHCR$SeasonLen[x]==165){NCinput2$fluke_bag[9:19] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:19] = NCtableHCR$MinLen[x]} else 
                if (NCtableHCR$SeasonLen[x]==180){NCinput2$fluke_bag[9:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:20] = NCtableHCR$MinLen[x]} else 
                  if (NCtableHCR$SeasonLen[x]==195){NCinput2$fluke_bag[8:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[8:20] = NCtableHCR$MinLen[x]} else 
                  if (NCtableHCR$SeasonLen[x]==210){NCinput2$fluke_bag[7:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[7:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 2} else 
                  if (NCtableHCR$SeasonLen[x]==225){NCinput2$fluke_bag[6:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[6:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 3} else 
                  if (NCtableHCR$SeasonLen[x]==240){NCinput2$fluke_bag[5:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4} else 
                  if (NCtableHCR$SeasonLen[x]==255){NCinput2$fluke_bag[5:21] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:21] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 2} else 
                  if (NCtableHCR$SeasonLen[x]==270){NCinput2$fluke_bag[5:22] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:22] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 3} else 
                  if (NCtableHCR$SeasonLen[x]==285){NCinput2$fluke_bag[5:23] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:23] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 4} else 
                  if (NCtableHCR$SeasonLen[x]==300){NCinput2$fluke_bag[5:24] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:24] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 5} 
NCinput2$fluke_bag <- ifelse(is.na(NCinput2$fluke_bag), 0, NCtableHCR$Bag[x])
NCinput2$fluke_min <- ifelse(is.na(NCinput2$fluke_min), 100, NCtableHCR$MinLen[x])
if (NCinput2$fluke_bag[16]!=0){NCinput2$pd_multiplier[16] <- 2}
NCinputmiddleman <- na.omit(NCinput2)
if (x==1){NCinputseason <- NCinputmiddleman}
if (x==2){NCinputsize <- NCinputmiddleman}
if (x==3){NCinputbag <- NCinputmiddleman}
if (x==4){NCinputflex <- NCinputmiddleman}
if (x==5){NCinputBL <- NCinputmiddleman}
if (x==6){NCinputBS <- NCinputmiddleman}
if (x==7){NCinputLS <- NCinputmiddleman}
}


## -----------------------------------------------------------------------------
RItableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "RI")
 
for (x in 1:7){
RIinput2 <- blankinput$RIinput2
if(RItableHCR$SeasonLen[x]==60){RIinput2$fluke_bag[13:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[13:16] = RItableHCR$MinLen[x]} else 
  if(RItableHCR$SeasonLen[x]==75){RIinput2$fluke_bag[12:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[12:16] = RItableHCR$MinLen[x]} else
    if (RItableHCR$SeasonLen[x]==90){RIinput2$fluke_bag[11:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[11:16] = RItableHCR$MinLen[x]} else 
      if (RItableHCR$SeasonLen[x]==105){RIinput2$fluke_bag[10:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[10:16] = RItableHCR$MinLen[x]} else
        if (RItableHCR$SeasonLen[x]==120){RIinput2$fluke_bag[9:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:16] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
          if (RItableHCR$SeasonLen[x]==135){RIinput2$fluke_bag[9:17] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:17] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
            if (RItableHCR$SeasonLen[x]==150){RIinput2$fluke_bag[9:18] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:18] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
              if (RItableHCR$SeasonLen[x]==165){RIinput2$fluke_bag[9:19] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:19] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                if (RItableHCR$SeasonLen[x]==180){RIinput2$fluke_bag[9:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                  if (RItableHCR$SeasonLen[x]==195){RIinput2$fluke_bag[8:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[8:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1+(1/0.867))} else 
                  if (RItableHCR$SeasonLen[x]==210){RIinput2$fluke_bag[7:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[7:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (2+(1/0.867))} else 
                  if (RItableHCR$SeasonLen[x]==225){RIinput2$fluke_bag[6:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[6:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (3+(1/0.867))} else 
                  if (RItableHCR$SeasonLen[x]==240){RIinput2$fluke_bag[5:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867))} else 
                  if (RItableHCR$SeasonLen[x]==255){RIinput2$fluke_bag[5:21] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:21] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867))} else 
                  if (RItableHCR$SeasonLen[x]==270){RIinput2$fluke_bag[5:22] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:22] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 2} else 
                  if (RItableHCR$SeasonLen[x]==285){RIinput2$fluke_bag[5:23] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:23] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 3} else 
                  if (RItableHCR$SeasonLen[x]==300){RIinput2$fluke_bag[5:24] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:24] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 4} 
RIinput2$fluke_bag <- ifelse(is.na(RIinput2$fluke_bag), 0, RItableHCR$Bag[x])
RIinput2$fluke_min <- ifelse(is.na(RIinput2$fluke_min), 100, RItableHCR$MinLen[x])
RIinputmiddleman <- na.omit(RIinput2)
if (x==1){RIinputseason <- RIinputmiddleman}
if (x==2){RIinputsize <- RIinputmiddleman}
if (x==3){RIinputbag <- RIinputmiddleman}
if (x==4){RIinputflex <- RIinputmiddleman}
if (x==5){RIinputBL <- RIinputmiddleman}
if (x==6){RIinputBS <- RIinputmiddleman}
if (x==7){RIinputLS <- RIinputmiddleman}
}


## -----------------------------------------------------------------------------
MDtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "MD")

for (x in 1:7){
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
if (x==1){MDinputseason <- MDinputmiddleman}
if (x==2){MDinputsize <- MDinputmiddleman}
if (x==3){MDinputbag <- MDinputmiddleman}
if (x==4){MDinputflex <- MDinputmiddleman}
if (x==5){MDinputBL <- MDinputmiddleman}
if (x==6){MDinputBS <- MDinputmiddleman}
if (x==7){MDinputLS <- MDinputmiddleman}
}


## -----------------------------------------------------------------------------
VAtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "VA")

for (x in 1:7){
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
if (x==1){VAinputseason <- VAinputmiddleman}
if (x==2){VAinputsize <- VAinputmiddleman}
if (x==3){VAinputbag <- VAinputmiddleman}
if (x==4){VAinputflex <- VAinputmiddleman}
if (x==5){VAinputBL <- VAinputmiddleman}
if (x==6){VAinputBS <- VAinputmiddleman}
if (x==7){VAinputLS <- VAinputmiddleman}
}


## -----------------------------------------------------------------------------
CTtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "CT")

for (x in 1:7){
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
if (x==1){CTinputseason <- CTinputmiddleman}
if (x==2){CTinputsize <- CTinputmiddleman}
if (x==3){CTinputbag <- CTinputmiddleman}
if (x==4){CTinputflex <- CTinputmiddleman}
if (x==5){CTinputBL <- CTinputmiddleman}
if (x==6){CTinputBS <- CTinputmiddleman}
if (x==7){CTinputLS <- CTinputmiddleman}
}


## -----------------------------------------------------------------------------
DEtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "DE")

for (x in 1:7){
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
if (x==1){DEinputseason <- DEinputmiddleman}
if (x==2){DEinputsize <- DEinputmiddleman}
if (x==3){DEinputbag <- DEinputmiddleman}
if (x==4){DEinputflex <- DEinputmiddleman}
if (x==5){DEinputBL <- DEinputmiddleman}
if (x==6){DEinputBS <- DEinputmiddleman}
if (x==7){DEinputLS <- DEinputmiddleman}
}


## -----------------------------------------------------------------------------
MAtableHCR <- PCA_BioInformed_CatchTable_NYRegs %>%
  filter(State == "MA")

for (x in 1:7){
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
if (x==1){MAinputseason <- MAinputmiddleman}
if (x==2){MAinputsize <- MAinputmiddleman}
if (x==3){MAinputbag <- MAinputmiddleman}
if (x==4){MAinputflex <- MAinputmiddleman}
if (x==5){MAinputBL <- MAinputmiddleman}
if (x==6){MAinputBS <- MAinputmiddleman}
if (x==7){MAinputLS <- MAinputmiddleman}
}


## -----------------------------------------------------------------------------
Season <- rbind(CTinputseason, DEinputseason, MAinputseason, MDinputseason, NCinputseason, NJinputseason, NYinputseason, RIinputseason, VAinputseason)

Size <- rbind(CTinputsize, DEinputsize, MAinputsize, MDinputsize, NCinputsize, NJinputsize, NYinputsize, RIinputsize, VAinputsize)

Bag <- rbind(CTinputbag, DEinputbag, MAinputbag, MDinputbag, NCinputbag, NJinputbag, NYinputbag, RIinputbag, VAinputbag)

All <- rbind(CTinputflex, DEinputflex, MAinputflex, MDinputflex, NCinputflex, NJinputflex, NYinputflex, RIinputflex, VAinputflex)

BagLength <- rbind(CTinputBL, DEinputBL, MAinputBL, MDinputBL, NCinputBL, NJinputBL, NYinputBL, RIinputBL, VAinputBL)

BagSeason <- rbind(CTinputBS, DEinputBS, MAinputBS, MDinputBS, NCinputBS, NJinputBS, NYinputBS, RIinputBS, VAinputBS)

LengthSeason <- rbind(CTinputLS, DEinputLS, MAinputLS, MDinputLS, NCinputLS, NJinputLS, NYinputLS, RIinputLS, VAinputLS)

pd_multiply_allregulations_input_HCRgrouping_regdistinct <- list(
   "Season" = Season,
  "Length" = Size,
  "Bag" = Bag,
  "All" = All,
  "BagLength" = BagLength,
  "BagSeason" = BagSeason,
  "LengthSeason" = LengthSeason
)
pd_multiply_allregulations_input_HCRgrouping_regdistinct

## -----------------------------------------------------------------------------
#saveRDS(pd_multiply_allregulations_input_HCRgrouping_regdistinct, file = "bioinformed_pdmultiplied_allregulations_input_HCRgrouping_regdistinct.rds")
#readRDS("bioinformed_pdmultiplied_allregulations_input_HCRgrouping_regdistinct.rds")

