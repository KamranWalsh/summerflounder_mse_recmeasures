args = commandArgs(trailingOnly=TRUE)

#arguments from Sinatra: 
# bag size, min size, season length, control rule, 
#args <- c(4,18.5,162,6,1,6350000,1.1,0.9,0.9,3)
#args <- c(4, 17.5, 275, 6, 1,    1158193,   0.226,   0.200,   1.000,   1.000, 3)
#' ---
#' title: "BioInformedControlRule-CurrentRegstoTarget"
#' author: "Kamran Walsh"
#' date: "2024-04-16"
#' output: html_document
#' ---
#' 
#' Code for testing recmeasures with option of selecting different HCRs (Percent Change Approach, Biological Reference Point, Biomass-Based Matrix) with different regulation types held constant.
#' 
## -----------------------------------------------------------------------------
#setwd("/Users/kam-macpro/Desktop/FlounderMSE")
#fluke <- readRDS("/Users/kam-macpro/Desktop/FlounderMSE/state_lookup.rds")
#View(fluke)

library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)

fluke <- readRDS("state_lookup.rds")
flukecatch <- tibble(fluke)

#' 
#' Inputs - Enter Current Regulations and Control Rule to be Tested
## -----------------------------------------------------------------------------
#Regs
bag = as.numeric(args[1]) #4
minlen = as.numeric(args[2]) #18.5
seasonlen = as.numeric(args[3]) #162
RHL <- as.numeric(args[6]) #6350000 # I just pulled this RHL value off the internet for now
BBMSY <- as.numeric(args[7]) #1.5 
RecTrend <- as.numeric(args[9]) #= 0.9
BTrend <- as.numeric(args[10]) #= 0.9
FFMSY <- as.numeric(args[8]) #= 1.1
OldBin <- as.numeric(args[11]) #= 3
SSB <- as.numeric(args[12])

NewBin <- 0
# put inputs into arguments eventually
# bag <- args[1]
# minlen <- args[2]
# seasonlen <- args[3]

#scaling no longer needed as 'calibration GLM' now being applied
#Scale RHL so it is in same space as GAM model
#RHL <- 0.69*RHL
#RHL <- RHL/0.39

#args<- NULL
#args[4] <- 6
#Control Rule
if(args[4] == 11) Alternative = "Table0"
if(args[4] == 6) Alternative = "Table1"
if(args[4] == 8) Alternative = "Table2"
if(args[4] == 10) Alternative = "Table3"
if(args[4] == 12) Alternative = "Table4" 

#additional BBM trials
if(args[4] == 13) Alternative = "Table3.1"
if(args[4] == 14) Alternative = "Table3.2"
if(args[4] == 15) Alternative = "Table3.3"
if(args[4] == 16) Alternative = "Table3.4"
if(args[4] == 17) Alternative = "Table3.5"
if(args[4] == 18) Alternative = "Table3.6"
if(args[4] == 19) Alternative = "Table3.7"

#additional BRP trials
if(args[4] == 20) Alternative = "Table2.1"
if(args[4] == 21) Alternative = "Table2.2"
if(args[4] == 22) Alternative = "Table2.3"
if(args[4] == 23) Alternative = "Table2.4"
if(args[4] == 24) Alternative = "Table2.5"
if(args[4] == 25) Alternative = "Table2.6"
if(args[4] == 26) Alternative = "Table2.7"

#Table 0 = No Change

#Table 1 = Percent Change Approach

#Table 2 = Biological Reference Point (BRP) Approach
#Table 2.1 = BRP Alternative 1 
#Table 2.2 = BRP Alternative 2 
#Table 2.3 = BRP Alternative 3 
#Table 2.4 = BRP Alternative 4 
#Table 2.5 = BRP Alternative 6
#Table 2.6 = BRP Alternative 6
#Table 2.7 = BRP Alternative 7

#Table 3 = Biomass Based Matrix (BBM) Approach
#Table 3.1 = BBM Alternative 1 
#Table 3.2 = BBM Alternative 2 
#Table 3.3 = BBM Alternative 3 
#Table 3.4 = BBM Alternative 4 
#Table 3.5 = BBM Alternative 6
#Table 3.6 = BBM Alternative 6
#Table 3.7 = BBM Alternative 7

#Table 4 = Simplified Percent Change Approach

#args[5] <- 1
# REgulation that is being changed during the HCR
if (args[5]==1) reg_to_change <- "Bag"
if (args[5]==2) reg_to_change <- "Length"
if (args[5]==3) reg_to_change <- "Season"
if (args[5]==4) reg_to_change <- "All"
if (args[5]==5) reg_to_change <- "BagLength"
if (args[5]==6) reg_to_change <- "BagSeason"
if (args[5]==7) reg_to_change <- "LengthSeason"

#' Biological Reference Point Approach Parameters
## -----------------------------------------------------------------------------
# RecTrend = 0.9
# BTrend = 0.9
# FFMSY = 1.1
# #Reduction/Liberalization Measures - should be sets of regulations
# liberal = 1.1
# initial = 1
# restrictive = 0.9
# veryrestrictive = 0.8

# #Biomass based Matrix Approach
# #Liberalization/Reduction Measures
# Bin1 <- 1.4
# Bin2 <- 1.2
# Bin3 <- 1.1
# Bin4 <- 0.9
# Bin5 <- 0.8
# Bin6 <- 0.6

######################################

#modify the expected landings based on the GLM calibration
calib_glm <- readRDS("calib_glm.rds")
nufluke <- tibble(
  pred = flukecatch$land,
  minlen = flukecatch$MinLen,
  seaslen = flukecatch$SeasonLen,
  bag = flukecatch$Bag,
  biomass = SSB
)
pred_landings <- predict(calib_glm, newdata = nufluke, type = "response")
#update the lookup table values
flukecatch$land <- as.numeric(pred_landings)

######################################

#' 
#' Determine Expected Catch Given Current Regulations from Lookup Table:
## -----------------------------------------------------------------------------
functioncatch_commonreg <- function(flukecatch, bag, minlen, seasonlen){
  flukecatch %>%
    #group_by(State) %>% #variable in the dataset not referring to the function argument
    filter(
      Bag == bag,
      MinLen == minlen,
      SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])
    )
}
catchallstates_commonreg <- functioncatch_commonreg(flukecatch, 
                                                    #state = state, #argument not used
                                                    bag = bag,
                                                    minlen = minlen,
                                                    seasonlen = seasonlen)
#catchallstates_commonreg

#' 
#' Use GAM to extract CIs:
## -----------------------------------------------------------------------------

len <- seq(10,28)
states <- c("DE", "MD", "NJ", "NY", "VA", "CT", "MA", "RI", "NC")
waves <- 2:6
Bag <- bag 
MinLen <- minlen
SeasLen <- catchallstates_commonreg$SeasonLen[1]

mround <- function(x,base){
  base*round(x/base)
}

wave_seasons <- data.frame(seas = seq(60,300,15),
                           w2 = c(rep(0,9),seq(15,60,15),rep(60,4)),
                           w3 = c(rep(0,1),seq(15,60,15),rep(60,12)),
                           w4 = rep(60,17),
                           w5 = c(rep(0,5),seq(15,60,15),rep(60,8)),
                           w6 = c(rep(0,13),seq(15,60,15)))
 
 dat.all <- expand.grid(State = states,
                           Length = len, #what does this mean
                           Bag = Bag,
                           MinLen = MinLen,
                           Wave = waves
)
dat.all$SeasonLen <- as.numeric(wave_seasons[wave_seasons$seas==as.integer(mround(as.numeric(SeasLen),15)),-1])[dat.all$Wave-1]

# dat.all$SeasonLen <- SeasonLen[dat.all$Wave-1]

gamland <- readRDS("gam_land.rds")  
#summary(gamland)
gamfit <- predict.gam(gamland, newdata = dat.all, type = "link" , se.fit = TRUE)

#lognormal CI 
upr <- exp(gamfit$fit + (1.96 * gamfit$se.fit))
lwr <- exp(gamfit$fit - (1.96 * gamfit$se.fit))

# Extract upper + lower CIs
output2 = cbind(dat.all, gamfit, lwr, upr)

outputland <- sum(exp(output2$fit))
outputland

#CIs
upperCI_test <- sum(output2$upr)
upperCI_test
lowerCI_test <- sum(output2$lwr)
lowerCI_test


#add the calibration adjustment
#calib_glm <- readRDS("mse/calib_glm.rds")
nudata <- tibble(
  pred = c(outputland, upperCI_test, lowerCI_test),
  #pred = c(4000000,4800000,3200000),
  minlen = minlen,
  seaslen = seasonlen,
  bag = bag,
  biomass = SSB
)
pred_harvest <- predict(calib_glm, newdata = nudata, 
                        se.fit=TRUE, type = "link")
#pred_harvest <- predict(calib_glm, newdata = nudata, 
 #                       se.fit=TRUE, type = "response")

expectedharvest <- exp(pred_harvest$fit[1])
upperCI <- exp(pred_harvest$fit[2] + (1.96 * pred_harvest$se.fit[2]))
lowerCI <- exp(pred_harvest$fit[3] - (1.96 * pred_harvest$se.fit[3]))

#Rec demand model expected harvest, sd, CV
#harvestrecdemand <- 8826699
#sdrecdemand <- 530839
#CVrecdemand <- 0.06

#upperCI_test/outputland
#lowerCI_test/outputland

#from randomly trying a few different starting regulations, upperCI_test/outputland varies from ~1.2 - ~1.55, lowerCI_test/outputland varies from ~0.4 - ~0.75.

#' 
#' Control Rules:
#' 
#' Percent Change Approach Bin Structure:
## -----------------------------------------------------------------------------
# Inputs 
# For now I just left all of the relevant inputs in each chunk
#RHL <- 6350000 # I just pulled this RHL value off the internet for now
# expectedharvest <- sum(catchallstates_commonreg$land) 
# upperCI <- upperCI_test
# lowerCI <- lowerCI_test


 if (Alternative == "Table1"){
#1.1
if(RHL > upperCI & BBMSY >= 1.5){x <- abs(((RHL-expectedharvest)/expectedharvest)+1); if(x > 1.4){x <- 1.4}}
  
#1.2
if(RHL > upperCI & BBMSY >= 1 & BBMSY < 1.5){x <- abs(((RHL-expectedharvest)/expectedharvest)+1); if(x > 1.2){x <- 1.2}}
  
#1.3
if(RHL > upperCI & BBMSY < 1){x <- 1.1}
  
#2.1 
if(between(RHL, lowerCI, upperCI) & BBMSY >= 1.5){x <- 1.1} 
  
#2.2
if(between(RHL, lowerCI, upperCI) & BBMSY >= 1 & BBMSY < 1.5){x <- 1} 
  
#2.3
if(between(RHL, lowerCI, upperCI) & BBMSY < 1){x <- 0.9} 
  
#3.1
if(RHL < lowerCI & BBMSY >= 1.5){x <- 0.9} 
  
#3.2
if(RHL < lowerCI & BBMSY >= 1 & BBMSY < 1.5){x <- 1-abs((RHL-expectedharvest)/expectedharvest); if(x < 0.8){x <- 0.8}} 
  
#3.3
if(RHL < lowerCI & BBMSY < 1){x <- 1-abs((RHL-expectedharvest)/expectedharvest); if(x < 0.6){x <- 0.6}} 

   # calculates target catch based off of degree of liberalization/reduction
catchallstates_commonreg_HCR <- catchallstates_commonreg
catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
#catchallstates_commonreg_HCR
}

#' Simplified Percent Change Approach 
## -----------------------------------------------------------------------------

 if (Alternative == "Table4"){
#1.1
if(RHL > upperCI & BBMSY >= 1.5){x <- 1.4}
  
#1.2
if(RHL > upperCI & BBMSY >= 1 & BBMSY < 1.5){x <- 1.2}
  
#1.3
if(RHL > upperCI & BBMSY < 1){x <- 1.1}
  
#2.1 
if(between(RHL, lowerCI, upperCI) & BBMSY >= 1.5){x <- 1.1} 
  
#2.2
if(between(RHL, lowerCI, upperCI) & BBMSY >= 1 & BBMSY < 1.5){x <- 1} 
  
#2.3
if(between(RHL, lowerCI, upperCI) & BBMSY < 1){x <- 0.9} 
  
#3.1
if(RHL < lowerCI & BBMSY >= 1.5){x <- 0.9} 
  
#3.2
if(RHL < lowerCI & BBMSY >= 1 & BBMSY < 1.5){x <-  0.8}
  
#3.3
if(RHL < lowerCI & BBMSY < 1){x <- 0.6}

   # calculates target catch based off of degree of liberalization/reduction
catchallstates_commonreg_HCR <- catchallstates_commonreg
catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
#catchallstates_commonreg_HCR
 }

#' Biological Reference Point Approach
## -----------------------------------------------------------------------------
#Biological Info
#Not sure what the relevant Rec and Biomass data/parameters are supposed to look like

# Rpresent = 1 # High recruitment is greater than or equal to median, low is less than 
# Rmedian = 1.1 
# Bpresent = 1
# Bpast = 1.1
# BBMSY = 0.5
# F = 1.1
# FMSY = 1
# 
# #Fishery Metrics
# RHL = 6350000
# upperCI = upperCI_test 
# lowerCI = lowerCI_test 
# 
# #Reduction/Liberalization Measures
# liberal = 1.1
# initial = 1
# restrictive = 0.9
# veryrestrictive = 0.8


# 
# if(Alternative == "Table2"){
# 
#   #MORTALITY
# if(FFMSY > 1){Overfishing = "yes"}
# if(FFMSY <= 1){Overfishing = "no"}
#   
#   #RECTREND
# #if(Rpresent >= Rmedian){R = "high"}
# #if(Rpresent < Rmedian){R = "low"}
#   if(RecTrend >= 1){R = "high"}
#   if(RecTrend < 1){R = "low"}
#   
#   #BIOTREND
#   # not sure if this is the correct interpretation of the biomass trend specifications 
#   
# # #if(Bpresent < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Increasing"} #stable
# # if(Bpresent > (Bpast*(1-0.04))){B = "Increasing"}
# # if(Bpresent <= (Bpast*(1-0.04))){B = "Decreasing"}
#   #if(Bpresent < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Increasing"} #stable
#   if(BTrend >= 1) {B = "Increasing"}
#   if(BTrend < 1) {B = "Decreasing"}
#   
#     
# #BIOMASS IS VERY HIGH 
# 
#     #Overfishing not occurring
# if(BBMSY >= 1.5 & Overfishing == "no" & B == "Increasing"){x <- liberal}
# if(BBMSY >= 1.5 & Overfishing == "no" & B == "Decreasing"){x <- initial}
# 
#     #Overfishing is occurring
# #RHL not exceeded
# if(BBMSY >= 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "high"){x <- initial}
# if(BBMSY >= 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "low"){x <- restrictive}  
# if(BBMSY >= 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Decreasing"){x <- restrictive}
# #RHL exceeded
# if(BBMSY >= 1.5 & Overfishing == "yes" & RHL < lowerCI){x <- restrictive}
# 
#         # I made harvest not exceeded RHL >= lowerCI instead of RHL > lowerCI to not break it in the             # unlikely event the RHL and CI bounds perfectly equal one another 
#   
# #BIOMASS IS HIGH 
# 
#     #Overfishing not occurring
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "no" & B == "Increasing"){x <- liberal}
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "no" & B == "Decreasing"){x <- initial}
# 
#     #Overfishing is occurring 
# #RHL not exceeded
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "high"){x <- initial}
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "low"){x <- restrictive}
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Decreasing"){x <- restrictive}
# #RHL exceeded
# if(BBMSY >= 1 & BBMSY < 1.5 & Overfishing == "yes" & RHL < lowerCI){x <- restrictive}
#     
# #BIOMASS IS LOW
# 
#     #Overfishing not occurring
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no" & B == "Increasing" & R == "high"){x <- initial}
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no" & B == "Increasing" & R == "low"){x <- restrictive}
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no" & B == "Decreasing"){x <- restrictive}
# 
#     #Overfishing is occurring
# #RHL not exceeded
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "high"){x <- initial}
#   if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Increasing" & R == "low"){x <- restrictive}
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes" & RHL >= lowerCI & B == "Decreasing"){x <- restrictive}
# #RHL exceeded
# if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes" & RHL < lowerCI){x <- restrictive}
# 
# #OVERFISHED 
# if(BBMSY < 0.5){x <- veryrestrictive}
# 
#   # calculates target catch based off of degree of liberalization/reduction
# catchallstates_commonreg_HCR <- catchallstates_commonreg
# catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
# #catchallstates_commonreg_HCR
# }

#' 
#' Biomass Based Matrix Approach
## -----------------------------------------------------------------------------
# 
# #Liberalization/Reduction Measures
# Bin1 <- 1.4
# Bin2 <- 1.2
# Bin3 <- 1.1
# Bin4 <- 0.9
# Bin5 <- 0.8
# Bin6 <- 0.6

#Original Approach
if(Alternative=="Table3"){

  # same thing here as in biological reference point approach; not sure if this is correct
# if(Bpresent < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
# if(Bpresent >= (Bpast*(1+0.04))){B = "Increasing"}
# if(Bpresent <= (Bpast*(1-0.04))){B = "Decreasing"}
  #if(BTrend < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
if(BBMSY >= 1.5){NewBin <- 1}
if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}

  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin
  x <- 1 + 0.1*diff
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,4)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,4))) x <- 0.95 #might want something more severe for 6
  
  
  
  # calculates target catch based off of degree of liberalization/reduction
catchallstates_commonreg_HCR <- catchallstates_commonreg
catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
#catchallstates_commonreg_HCR
}

#Alternative 3.1:

if(Alternative=="Table3.1"){
  
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin %in% c(2,4)) x <- 1.05
  if (diff == 0 & NewBin == 5) x <- 0.95 
  if (diff == 0 & NewBin == 6) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.2:

if(Alternative=="Table3.2"){
  
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}


  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff  
  
  if((OldBin == 6 & NewBin == 5)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 5 & NewBin == 4)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 4 & NewBin == 3)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 3 & NewBin == 2)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 5 & NewBin == 6)|(OldBin == 2 & NewBin == 1)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,4)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,4))) x <- 0.95 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.3:

if(Alternative=="Table3.3"){
  
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1} 
  if((abs(diff) > 1) & x<1){x <- x2}
  
  if((OldBin == 6 & NewBin == 5)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 5 & NewBin == 4)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 4 & NewBin == 3)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 3 & NewBin == 2)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 5 & NewBin == 6)|(OldBin == 2 & NewBin == 1)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,4)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,4))) x <- 0.95 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.4:

if(Alternative=="Table3.4"){
  
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1}
  if((abs(diff) > 1) & x<1){x <- x2}
  
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,4)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,4))) x <- 0.95 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.5: 

if(Alternative=="Table3.5"){
  
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff  
  
  if((OldBin == 6 & NewBin == 5)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 5 & NewBin == 4)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 4 & NewBin == 3)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 3 & NewBin == 2)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 5 & NewBin == 6)|(OldBin == 2 & NewBin == 1)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }

  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin %in% c(2,4)) x <- 1.05
  if (diff == 0 & NewBin == 5) x <- 0.95 
  if (diff == 0 & NewBin == 6) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.6:

if(Alternative=="Table3.6"){
  
  # same thing here as in biological reference point approach; not sure if this is correct
  # if(Bpresent < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
  # if(Bpresent >= (Bpast*(1+0.04))){B = "Increasing"}
  # if(Bpresent <= (Bpast*(1-0.04))){B = "Decreasing"}
  #if(BTrend < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1} 
  if((abs(diff) > 1) & x<1){x <- x2}
  
  if((OldBin == 6 & NewBin == 5)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 5 & NewBin == 4)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 4 & NewBin == 3)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 3 & NewBin == 2)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 5 & NewBin == 6)|(OldBin == 2 & NewBin == 1)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin %in% c(2,4)) x <- 1.05
  if (diff == 0 & NewBin == 5) x <- 0.95 
  if (diff == 0 & NewBin == 6) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3.7:

if(Alternative=="Table3.7"){
  
  # same thing here as in biological reference point approach; not sure if this is correct
  # if(Bpresent < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
  # if(Bpresent >= (Bpast*(1+0.04))){B = "Increasing"}
  # if(Bpresent <= (Bpast*(1-0.04))){B = "Decreasing"}
  #if(BTrend < (Bpast*(1+0.04)) & Bpresent > (Bpast*(1-0.04))){B = "Decreasing"} #stable
  B = "Decreasing"
  if(BTrend >= 1.04){B = "Increasing"}
  
  if(BBMSY >= 1.5){NewBin <- 1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Increasing"){NewBin <- 1} #{x <- Bin1}
  if(BBMSY < 1.5 & BBMSY >= 1 & B == "Decreasing"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Increasing"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & B == "Decreasing"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 0.5 & B == "Increasing"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 0.5 & B == "Decreasing"){NewBin <- 6} #{x <- Bin6}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1} 
  if((abs(diff) > 1) & x<1){x <- x2}
  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin %in% c(2,4)) x <- 1.05
  if (diff == 0 & NewBin == 5) x <- 0.95 
  if (diff == 0 & NewBin == 6) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

##############
#### Simplified Reference Point Approach

if(Alternative=="Table2"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin
  x <- 1 + 0.1*diff
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,2)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,2))) x <- 0.95 #might want something more severe for 7
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 2.1:

if(Alternative=="Table2.1"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin
  x <- 1 + 0.1*diff
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin == 2) x <- 1.05
  if (diff == 0 & NewBin %in% c(3,6)) x <- 0.95 
  if (diff == 0 & NewBin == 7) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 2.2:

if(Alternative=="Table2.2"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin
  x <- 1 + 0.1*diff
  
  if((OldBin == 7 & NewBin == 6)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 6 & NewBin == 5)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 5 & NewBin == 4)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 4 & NewBin == 3)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 3 & NewBin == 2)|(OldBin == 5 & NewBin == 6)|
     (OldBin == 2 & NewBin == 1)|(OldBin == 6 & NewBin == 7)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,2)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,2))) x <- 0.95 

  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 3:

if(Alternative=="Table2.3"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1} 
  if((abs(diff) > 1) & x<1){x <- x2}
  
  if((OldBin == 7 & NewBin == 6)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 6 & NewBin == 5)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 5 & NewBin == 4)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 4 & NewBin == 3)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 3 & NewBin == 2)|(OldBin == 5 & NewBin == 6)|
     (OldBin == 2 & NewBin == 1)|(OldBin == 6 & NewBin == 7)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,2)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,2))) x <- 0.95 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 4:

if(Alternative=="Table2.4"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1} 
  if((abs(diff) > 1) & x<1){x <- x2}
  
  #for now if remain in a bin change by 5%
  if (diff == 0 & NewBin %in% c(1,2)) x <- 1.05
  if (diff == 0 & !(NewBin %in% c(1,2))) x <- 0.95 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#####################
### No CHange (RHL multiplier)
# calculates target catch based off of degree of liberalization/reduction
if(Alternative == "Table0") {
  x <- RHL/expectedharvest
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 2.5:

if(Alternative=="Table2.5"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin
  x <- 1 + 0.1*diff
  
  if((OldBin == 7 & NewBin == 6)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 6 & NewBin == 5)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 5 & NewBin == 4)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 4 & NewBin == 3)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 3 & NewBin == 2)|(OldBin == 5 & NewBin == 6)|
     (OldBin == 2 & NewBin == 1)|(OldBin == 6 & NewBin == 7)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin == 2) x <- 1.05
  if (diff == 0 & NewBin %in% c(3,6)) x <- 0.95 
  if (diff == 0 & NewBin == 7) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 2.6:

if(Alternative=="Table2.6"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1}
  if((abs(diff) > 1) & x<1){x <- x2}
  
  if((OldBin == 7 & NewBin == 6)|(OldBin == 1 & NewBin == 2)|
     (OldBin == 6 & NewBin == 5)|(OldBin == 2 & NewBin == 3)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.1*diff
  }
  if((OldBin == 5 & NewBin == 4)|(OldBin == 3 & NewBin == 4)|
     (OldBin == 4 & NewBin == 3)|(OldBin == 4 & NewBin == 5)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.15*diff
  }
  if((OldBin == 3 & NewBin == 2)|(OldBin == 5 & NewBin == 6)|
     (OldBin == 2 & NewBin == 1)|(OldBin == 6 & NewBin == 7)){
    diff <- OldBin - NewBin 
    x <- 1 + 0.2*diff
  }
  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin == 2) x <- 1.05
  if (diff == 0 & NewBin %in% c(3,6)) x <- 0.95 
  if (diff == 0 & NewBin == 7) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

#Alternative 2.7:

if(Alternative=="Table2.7"){
  
  #MORTALITY
  if(FFMSY > 1){Overfishing = "yes"}
  if(FFMSY <= 1){Overfishing = "no"}
  
  # if(F > FMSY){Overfishing = "yes"}
  # if(F <= FMSY){Overfishing = "no"}
  
  if(BBMSY >= 1.5 & Overfishing == "no"){NewBin <- 1} #x <- Bin1}
  if(BBMSY >= 1.5 & Overfishing == "yes"){NewBin <- 4} #{x <- Bin4}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "no"){NewBin <- 2} #{x <- Bin2}
  if(BBMSY < 1.5 & BBMSY >= 1 & Overfishing == "yes"){NewBin <- 5} #{x <- Bin5}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "no"){NewBin <- 3} #{x <- Bin3}
  if(BBMSY < 1 & BBMSY >= 0.5 & Overfishing == "yes"){NewBin <- 6} #{x <- Bin6}
  if(BBMSY < 0.5){NewBin <- 7} #{x <- Bin7}
  
  #calculate degree of liberalization/reduction
  diff <- OldBin - NewBin  
  x <- 1 + 0.1*diff
  x1 <- x + 0.1
  x2 <- x - 0.1
  if((abs(diff) > 1) & x>1){x <- x1}
  if((abs(diff) > 1) & x<1){x <- x2}
  
  #more severe changes if remain in same bin 
  if (diff == 0 & NewBin == 1) x <- 1.1
  if (diff == 0 & NewBin == 2) x <- 1.05
  if (diff == 0 & NewBin %in% c(3,6)) x <- 0.95 
  if (diff == 0 & NewBin == 7) x <- 0.90 
  
  # calculates target catch based off of degree of liberalization/reduction
  catchallstates_commonreg_HCR <- catchallstates_commonreg
  catchallstates_commonreg_HCR$land <- x*catchallstates_commonreg_HCR$land
  #catchallstates_commonreg_HCR
}

# Season Length Change:
#



#' 
#' 
#' Lookup Expected Catch/Discards + Return Regs: 
#' 
#' Season Length Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

# GF generic functions
#functioncatch_seasonlen <- function(flukecatch, state, bag, minlen, target){
#  flukecatch %>% 
    #group_by(State=="NC") %>% 
 #   filter(
  #    State== state, #"NC",
   #   Bag == bag,
    #  MinLen == minlen, #) %>%
    #filter(
   #   land == max(land[land <= target])) 
#}
RobustMax <- function(x) {if (length(x)>0) max(x) else -Inf}

functioncatch_seasonlen <- function(flukecatch, state, bag, minlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state, #"NC",
      Bag == bag,
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target])) 
}

state_if <- function(flukecatch, state, bag, minlen, target, prev_result) {
  if(nrow(prev_result)==0){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      Bag == bag,
      MinLen == minlen) %>%
    filter(abs(land - target) == min(abs(land - target)))
  }
}


# North Carolina

# #find next lowest value in lookup table
# functioncatchNC_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       Bag == bag,
#       MinLen == minlen) %>%
#    filter(
#    land == max(land[land <= NCcatch_common])) 
# }
# NC_seasonlen <- functioncatchNC_seasonlen(flukecatch, State, land)
NC_seasonlen <- functioncatch_seasonlen(flukecatch, state = "NC",
                                        bag = bag,
                                        minlen = minlen,
                                        target = NCcatch_common)
NC_seasonlen <- NC_seasonlen %>% mutate(TargetMet = "TRUE")
NC_seasonlen2 <- state_if(flukecatch, state = "NC", bag = bag,
                          minlen = minlen, target = NCcatch_common,
                          prev_result = NC_seasonlen)

#return higher value if no lower value exists given constraints
# NC_if <- function(flukecatch, State, land){if(nrow(NC_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - NCcatch_common) == min(abs(land - NCcatch_common)))
# }
# }
# NC_seasonlen2 <- NC_if(flukecatch, State, land)


#Delaware
# functioncatchDE_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State=="DE",
#       Bag == bag,
#       MinLen == minlen) %>%
#    filter(
#    land == max(land[land <= DEcatch_common])) 
# }
# DE_seasonlen <- functioncatchDE_seasonlen(flukecatch, State, land)
# DE_seasonlen <- DE_seasonlen %>% mutate(TargetMet = "TRUE")
DE_seasonlen <- functioncatch_seasonlen(flukecatch, state = "DE",
                                        bag = bag,
                                        minlen = minlen,
                                        target = DEcatch_common)
DE_seasonlen <- DE_seasonlen %>% mutate(TargetMet = "TRUE")
DE_seasonlen2 <- state_if(flukecatch, state = "DE", bag = bag,
                          minlen = minlen, target = DEcatch_common,
                          prev_result = DE_seasonlen)


# DE_if <- function(flukecatch, State, land){if(nrow(DE_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State=="DE",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - DEcatch_common) == min(abs(land - DEcatch_common)))
# }
# }
# DE_seasonlen2 <- DE_if(flukecatch, State, land)

#Massachusetts
MA_seasonlen <- functioncatch_seasonlen(flukecatch, state = "MA",
                                        bag = bag,
                                        minlen = minlen,
                                        target = MAcatch_common)
MA_seasonlen <- MA_seasonlen %>% mutate(TargetMet = "TRUE")
MA_seasonlen2 <- state_if(flukecatch, state = "MA", bag = bag,
                          minlen = minlen, target = MAcatch_common,
                          prev_result = MA_seasonlen)

#Rhode Island
# functioncatchRI_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= RIcatch_common]))
# }
# RI_seasonlen <- functioncatchRI_seasonlen(flukecatch, State, land)
# RI_seasonlen <- RI_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# RI_if <- function(flukecatch, State, land){if(nrow(RI_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - RIcatch_common) == min(abs(land - RIcatch_common)))
# }
# }
# RI_seasonlen2 <- RI_if(flukecatch, State, land)
RI_seasonlen <- functioncatch_seasonlen(flukecatch, state = "RI",
                                        bag = bag,
                                        minlen = minlen,
                                        target = RIcatch_common)
RI_seasonlen <- RI_seasonlen %>% mutate(TargetMet = "TRUE")
RI_seasonlen2 <- state_if(flukecatch, state = "RI", bag = bag,
                          minlen = minlen, target = RIcatch_common,
                          prev_result = RI_seasonlen)

#New York
# functioncatchNY_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= NYcatch_common]))
# }
# NY_seasonlen <- functioncatchNY_seasonlen(flukecatch, State, land)
# NY_seasonlen <- NY_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# NY_if <- function(flukecatch, State, land){if(nrow(NY_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - NYcatch_common) == min(abs(land - NYcatch_common)))
# }
# }
# NY_seasonlen2 <- NY_if(flukecatch, State, land)
NY_seasonlen <- functioncatch_seasonlen(flukecatch, state = "NY",
                                        bag = bag,
                                        minlen = minlen,
                                        target = NYcatch_common)
NY_seasonlen <- NY_seasonlen %>% mutate(TargetMet = "TRUE")
NY_seasonlen2 <- state_if(flukecatch, state = "NY", bag = bag,
                          minlen = minlen, target = NYcatch_common,
                          prev_result = NY_seasonlen)

#Maryland
# functioncatchMD_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= MDcatch_common]))
# }
# MD_seasonlen <- functioncatchMD_seasonlen(flukecatch, State, land)
# MD_seasonlen <- MD_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# MD_if <- function(flukecatch, State, land){if(nrow(MD_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - MDcatch_common) == min(abs(land - MDcatch_common)))
# }
# }
# MD_seasonlen2 <- MD_if(flukecatch, State, land)
MD_seasonlen <- functioncatch_seasonlen(flukecatch, state = "MD",
                                        bag = bag,
                                        minlen = minlen,
                                        target = MDcatch_common)
MD_seasonlen <- MD_seasonlen %>% mutate(TargetMet = "TRUE")
MD_seasonlen2 <- state_if(flukecatch, state = "MD", bag = bag,
                          minlen = minlen, target = MDcatch_common,
                          prev_result = MD_seasonlen)

#Connecticut 
# functioncatchCT_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= CTcatch_common]))
# }
# CT_seasonlen <- functioncatchCT_seasonlen(flukecatch, State, land)
# CT_seasonlen <- CT_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# CT_if <- function(flukecatch, State, land){if(nrow(CT_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - CTcatch_common) == min(abs(land - CTcatch_common)))
# }
# }
# CT_seasonlen2 <- CT_if(flukecatch, State, land)
CT_seasonlen <- functioncatch_seasonlen(flukecatch, state = "CT",
                                        bag = bag,
                                        minlen = minlen,
                                        target = CTcatch_common)
CT_seasonlen <- CT_seasonlen %>% mutate(TargetMet = "TRUE")
CT_seasonlen2 <- state_if(flukecatch, state = "CT", bag = bag,
                          minlen = minlen, target = CTcatch_common,
                          prev_result = CT_seasonlen)

#Virginia
# functioncatchVA_seasonlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= VAcatch_common]))
# }
# VA_seasonlen <- functioncatchVA_seasonlen(flukecatch, State, land)
# VA_seasonlen <- VA_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# VA_if <- function(flukecatch, State, land){if(nrow(VA_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - VAcatch_common) == min(abs(land - VAcatch_common)))
# }
# }
# VA_seasonlen2 <- VA_if(flukecatch, State, land)
VA_seasonlen <- functioncatch_seasonlen(flukecatch, state = "VA",
                                        bag = bag,
                                        minlen = minlen,
                                        target = VAcatch_common)
VA_seasonlen <- VA_seasonlen %>% mutate(TargetMet = "TRUE")
VA_seasonlen2 <- state_if(flukecatch, state = "VA", bag = bag,
                          minlen = minlen, target = VAcatch_common,
                          prev_result = VA_seasonlen)


# New Jersey 
# functioncatchNJ_seasonlen<- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= NJcatch_common]))
# }
# NJ_seasonlen <- functioncatchNJ_seasonlen(flukecatch, State, land)
# NJ_seasonlen <- NJ_seasonlen %>% mutate(TargetMet = "TRUE")
# 
# NJ_if <- function(flukecatch, State, land){if(nrow(NJ_seasonlen)==0){
#     flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       Bag == bag,
#       MinLen == minlen) %>%
#     filter(abs(land - NJcatch_common) == min(abs(land - NJcatch_common)))
# }
# }
# NJ_seasonlen2 <- NJ_if(flukecatch, State, land)
NJ_seasonlen <- functioncatch_seasonlen(flukecatch, state = "NJ",
                                        bag = bag,
                                        minlen = minlen,
                                        target = NJcatch_common)
NJ_seasonlen <- NJ_seasonlen %>% mutate(TargetMet = "TRUE")
NJ_seasonlen2 <- state_if(flukecatch, state = "NJ", bag = bag,
                          minlen = minlen, target = NJcatch_common,
                          prev_result = NJ_seasonlen)

if(nrow(NC_seasonlen)==0){NC_seasonlen <- NC_seasonlen2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_seasonlen)==0){DE_seasonlen <- DE_seasonlen2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_seasonlen)==0){MA_seasonlen <- MA_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_seasonlen)==0){RI_seasonlen <- RI_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_seasonlen)==0){NY_seasonlen <- NY_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_seasonlen)==0){NJ_seasonlen <- NJ_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_seasonlen)==0){MD_seasonlen <- MD_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_seasonlen)==0){CT_seasonlen <- CT_seasonlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_seasonlen)==0){VA_seasonlen <- VA_seasonlen2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_seasonlen <- list(NC_seasonlen, VA_seasonlen, MD_seasonlen, DE_seasonlen, NJ_seasonlen, NY_seasonlen, CT_seasonlen, RI_seasonlen, MA_seasonlen)
targetcatchregs_seasonlen_ <- rbindlist(targetcatchregs_seasonlen, fill = TRUE, )
targetcatchregs_seasonlen_ = subset(targetcatchregs_seasonlen_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))
#targetcatchregs_seasonlen_

#' 
#' Size Limit Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

functioncatch_common_minlen <- function(flukecatch, state, bag, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State==state,
      Bag == bag,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
state_if_minlen <- function(flukecatch, state, bag, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      Bag == bag,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
    filter(abs(land - target) == min(abs(land - target)))
}
}

#North Carolina
# functioncatchNC_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= NCcatch_common]))
# }
# NC_common_minlen <- functioncatchNC_common_minlen(flukecatch, State, land)
# NC_common_minlen <- NC_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# NC_if_minlen <- function(flukecatch, State, land){if(nrow(NC_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - NCcatch_common) == min(abs(land - NCcatch_common)))
# }
# }
# NC_common_minlen2 <- NC_if_minlen(flukecatch, State, land)
NC_common_minlen <- functioncatch_common_minlen(flukecatch, state = "NC",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = NCcatch_common)
NC_common_minlen <- NC_common_minlen %>% mutate(TargetMet = "TRUE")
NC_common_minlen2 <- state_if_minlen(flukecatch, state = "NC",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = NCcatch_common,
                                     prev_result = NC_common_minlen)

#Delaware
# functioncatchDE_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State =="DE",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= DEcatch_common]))
# }
# DE_common_minlen <- functioncatchDE_common_minlen(flukecatch, State, land)
# DE_common_minlen <- DE_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# DE_if_minlen <- function(flukecatch, State, land){if(nrow(DE_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State=="DE",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - DEcatch_common) == min(abs(land - DEcatch_common)))
# }
# }
# DE_common_minlen2 <- DE_if_minlen(flukecatch, State, land)
DE_common_minlen <- functioncatch_common_minlen(flukecatch, state = "DE",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = DEcatch_common)
DE_common_minlen <- DE_common_minlen %>% mutate(TargetMet = "TRUE")
DE_common_minlen2 <- state_if_minlen(flukecatch, state = "DE",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = DEcatch_common,
                                     prev_result = DE_common_minlen)

  
#Massachusetts
# functioncatchMA_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="MA") %>% 
#     filter(
#       State=="MA",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= MAcatch_common]))
# }
# MA_common_minlen <- functioncatchMA_common_minlen(flukecatch, State, land)
# MA_common_minlen <- MA_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# MA_if_minlen <- function(flukecatch, State, land){if(nrow(MA_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="MA") %>% 
#     filter(
#       State=="MA",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - MAcatch_common) == min(abs(land - MAcatch_common)))
# }
# }
# MA_common_minlen2 <- MA_if_minlen(flukecatch, State, land)
MA_common_minlen <- functioncatch_common_minlen(flukecatch, state = "MA",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = MAcatch_common)
MA_common_minlen <- MA_common_minlen %>% mutate(TargetMet = "TRUE")
MA_common_minlen2 <- state_if_minlen(flukecatch, state = "MA",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = MAcatch_common,
                                     prev_result = MA_common_minlen)

#Rhode Island
# functioncatchRI_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= RIcatch_common]))
# }
# RI_common_minlen <- functioncatchRI_common_minlen(flukecatch, State, land)
# RI_common_minlen <- RI_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# RI_if_minlen <- function(flukecatch, State, land){if(nrow(RI_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - RIcatch_common) == min(abs(land - RIcatch_common)))
# }
# }
# RI_common_minlen2 <- RI_if_minlen(flukecatch, State, land)
RI_common_minlen <- functioncatch_common_minlen(flukecatch, state = "RI",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = RIcatch_common)
RI_common_minlen <- RI_common_minlen %>% mutate(TargetMet = "TRUE")
RI_common_minlen2 <- state_if_minlen(flukecatch, state = "RI",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = RIcatch_common,
                                     prev_result = RI_common_minlen)

#New York
# functioncatchNY_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= NYcatch_common]))
# }
# NY_common_minlen <- functioncatchNY_common_minlen(flukecatch, State, land)
# NY_common_minlen <- NY_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# NY_if_minlen <- function(flukecatch, State, land){if(nrow(NY_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - NYcatch_common) == min(abs(land - NYcatch_common)))
# }
# }
# NY_common_minlen2 <- NY_if_minlen(flukecatch, State, land)
NY_common_minlen <- functioncatch_common_minlen(flukecatch, state = "NY",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = NYcatch_common)
NY_common_minlen <- NY_common_minlen %>% mutate(TargetMet = "TRUE")
NY_common_minlen2 <- state_if_minlen(flukecatch, state = "NY",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = NYcatch_common,
                                     prev_result = NY_common_minlen)

#Maryland
# functioncatchMD_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= MDcatch_common]))
# }
# MD_common_minlen <- functioncatchMD_common_minlen(flukecatch, State, land)
# MD_common_minlen <- MD_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# MD_if_minlen <- function(flukecatch, State, land){if(nrow(MD_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - MDcatch_common) == min(abs(land - MDcatch_common)))
# }
# }
# MD_common_minlen2 <- MD_if_minlen(flukecatch, State, land)
MD_common_minlen <- functioncatch_common_minlen(flukecatch, state = "MD",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = MDcatch_common)
MD_common_minlen <- MD_common_minlen %>% mutate(TargetMet = "TRUE")
MD_common_minlen2 <- state_if_minlen(flukecatch, state = "MD",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = MDcatch_common,
                                     prev_result = MD_common_minlen)

#Connecticut
# functioncatchCT_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= CTcatch_common]))
# }
# CT_common_minlen <- functioncatchCT_common_minlen(flukecatch, State, land)
# CT_common_minlen <- CT_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# CT_if_minlen <- function(flukecatch, State, land){if(nrow(CT_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - CTcatch_common) == min(abs(land - CTcatch_common)))
# }
# }
# CT_common_minlen2 <- CT_if_minlen(flukecatch, State, land)
CT_common_minlen <- functioncatch_common_minlen(flukecatch, state = "CT",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = CTcatch_common)
CT_common_minlen <- CT_common_minlen %>% mutate(TargetMet = "TRUE")
CT_common_minlen2 <- state_if_minlen(flukecatch, state = "CT",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = CTcatch_common,
                                     prev_result = CT_common_minlen)

#Virginia
# functioncatchVA_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= VAcatch_common]))
# }
# VA_common_minlen <- functioncatchVA_common_minlen(flukecatch, State, land)
# VA_common_minlen <- VA_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# VA_if_minlen <- function(flukecatch, State, land){if(nrow(VA_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - VAcatch_common) == min(abs(land - VAcatch_common)))
# }
# }
# VA_common_minlen2 <- VA_if_minlen(flukecatch, State, land)
VA_common_minlen <- functioncatch_common_minlen(flukecatch, state = "VA",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = VAcatch_common)
VA_common_minlen <- VA_common_minlen %>% mutate(TargetMet = "TRUE")
VA_common_minlen2 <- state_if_minlen(flukecatch, state = "VA",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = VAcatch_common,
                                     prev_result = VA_common_minlen)

#New Jersey
# functioncatchNJ_common_minlen <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(
#       land == max(land[land <= NJcatch_common]))
# }
# NJ_common_minlen <- functioncatchNJ_common_minlen(flukecatch, State, land)
# NJ_common_minlen <- NJ_common_minlen %>% mutate(TargetMet = "TRUE")
# 
# NJ_if_minlen <- function(flukecatch, State, land){if(nrow(NJ_common_minlen)==0){
#     flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       Bag == bag,
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])) %>%
#     filter(abs(land - NJcatch_common) == min(abs(land - NJcatch_common)))
# }
# }
# NJ_common_minlen2 <- NJ_if_minlen(flukecatch, State, land)
NJ_common_minlen <- functioncatch_common_minlen(flukecatch, state = "NJ",
                                                bag = bag,
                                                seasonlen = seasonlen,
                                                target = NJcatch_common)
NJ_common_minlen <- NJ_common_minlen %>% mutate(TargetMet = "TRUE")
NJ_common_minlen2 <- state_if_minlen(flukecatch, state = "NJ",
                                     bag = bag,
                                     seasonlen = seasonlen,
                                     target = NJcatch_common,
                                     prev_result = NJ_common_minlen)

if(nrow(NC_common_minlen)==0){NC_common_minlen <- NC_common_minlen2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_common_minlen)==0){DE_common_minlen <- DE_common_minlen2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_common_minlen)==0){MA_common_minlen <- MA_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_common_minlen)==0){RI_common_minlen <- RI_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_common_minlen)==0){NY_common_minlen <- NY_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_common_minlen)==0){NJ_common_minlen <- NJ_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_common_minlen)==0){MD_common_minlen <- MD_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_common_minlen)==0){CT_common_minlen <- CT_common_minlen2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_common_minlen)==0){VA_common_minlen <- VA_common_minlen2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common_minlen <- list(NC_common_minlen, VA_common_minlen, MD_common_minlen, DE_common_minlen, NJ_common_minlen, NY_common_minlen, CT_common_minlen, RI_common_minlen, MA_common_minlen)
targetcatchregs_common_minlen_ <- rbindlist(targetcatchregs_common_minlen, fill = TRUE, )
targetcatchregs_common_minlen_ = subset(targetcatchregs_common_minlen_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))
#targetcatchregs_common_minlen_

#' Bag Limit Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

bag_common <- function(flukecatch, state, minlen, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen]),
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
bag_if_common <- function(flukecatch, state, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen]),
      MinLen == minlen) %>%
    filter(abs(land - target) == min(abs(land - target)))
}
}

#North Carolina
# functioncatchNC_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= NCcatch_common]))
# }
# NC_common <- functioncatchNC_common(flukecatch, State, land)
# NC_common <- NC_common %>% mutate(TargetMet = "TRUE")
# 
# NC_if_common <- function(flukecatch, State, land){if(nrow(NC_common)==0){
#     flukecatch %>% 
#     group_by(State=="NC") %>% 
#     filter(
#       State=="NC",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - NCcatch_common) == min(abs(land - NCcatch_common)))
# }
# }
# NC_common2 <- NC_if_common(flukecatch, State, land)
NC_common <- bag_common(flukecatch, state = "NC", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = NCcatch_common)
NC_common <- NC_common %>% mutate(TargetMet = "TRUE")
NC_common2 <- bag_if_common(flukecatch, state = "NC", minlen = minlen,
                            seasonlen = seasonlen,
                            target = NCcatch_common, prev_result = NC_common)

#Delaware
# functioncatchDE_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State =="DE",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= DEcatch_common]))
# }
# DE_common <- functioncatchDE_common(flukecatch, State, land)
# DE_common <- DE_common %>% mutate(TargetMet = "TRUE")
# 
# DE_if_common <- function(flukecatch, State, land){if(nrow(DE_common)==0){
#     flukecatch %>% 
#     group_by(State=="DE") %>% 
#     filter(
#       State=="DE",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - DEcatch_common) == min(abs(land - DEcatch_common)))
# }
# }
# DE_common2 <- DE_if_common(flukecatch, State, land)
DE_common <- bag_common(flukecatch, state = "DE", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = DEcatch_common)
DE_common <- DE_common %>% mutate(TargetMet = "TRUE")
DE_common2 <- bag_if_common(flukecatch, state = "DE", minlen = minlen,
                            seasonlen = seasonlen,
                            target = DEcatch_common, prev_result = DE_common)

#Massachusetts
# functioncatchMA_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="MA") %>% 
#     filter(
#       State=="MA",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= MAcatch_common]))
# }
# MA_common <- functioncatchMA_common(flukecatch, State, land)
# MA_common <- MA_common %>% mutate(TargetMet = "TRUE")
# 
# MA_if_common <- function(flukecatch, State, land){if(nrow(MA_common)==0){
#     flukecatch %>% 
#     group_by(State=="MA") %>% 
#     filter(
#       State=="MA",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - MAcatch_common) == min(abs(land - MAcatch_common)))
# }
# }
# MA_common2 <- MA_if_common(flukecatch, State, land)
MA_common <- bag_common(flukecatch, state = "MA", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = MAcatch_common)
MA_common <- MA_common %>% mutate(TargetMet = "TRUE")
MA_common2 <- bag_if_common(flukecatch, state = "MA", minlen = minlen,
                            seasonlen = seasonlen,
                            target = MAcatch_common, prev_result = MA_common)

#Rhode Island
# functioncatchRI_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= RIcatch_common]))
# }
# RI_common <- functioncatchRI_common(flukecatch, State, land)
# RI_common <- RI_common %>% mutate(TargetMet = "TRUE")
# 
# RI_if_common <- function(flukecatch, State, land){if(nrow(RI_common)==0){
#     flukecatch %>% 
#     group_by(State=="RI") %>% 
#     filter(
#       State=="RI",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - RIcatch_common) == min(abs(land - RIcatch_common)))
# }
# }
# RI_common2 <- RI_if_common(flukecatch, State, land)
RI_common <- bag_common(flukecatch, state = "RI", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = RIcatch_common)
RI_common <- RI_common %>% mutate(TargetMet = "TRUE")
RI_common2 <- bag_if_common(flukecatch, state = "RI", minlen = minlen,
                            seasonlen = seasonlen,
                            target = RIcatch_common, prev_result = RI_common)

#New York
# functioncatchNY_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= NYcatch_common]))
# }
# NY_common <- functioncatchNY_common(flukecatch, State, land)
# NY_common <- NY_common %>% mutate(TargetMet = "TRUE")
# 
# NY_if_common <- function(flukecatch, State, land){if(nrow(NY_common)==0){
#     flukecatch %>% 
#     group_by(State=="NY") %>% 
#     filter(
#       State=="NY",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - NYcatch_common) == min(abs(land - NYcatch_common)))
# }
# }
# NY_common2 <- NY_if_common(flukecatch, State, land)
NY_common <- bag_common(flukecatch, state = "NY", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = NYcatch_common)
NY_common <- NY_common %>% mutate(TargetMet = "TRUE")
NY_common2 <- bag_if_common(flukecatch, state = "NY", minlen = minlen,
                            seasonlen = seasonlen,
                            target = NYcatch_common, prev_result = NY_common)


#Maryland
# functioncatchMD_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= MDcatch_common]))
# }
# MD_common <- functioncatchMD_common(flukecatch, State, land)
# MD_common <- MD_common %>% mutate(TargetMet = "TRUE")
# 
# MD_if_common <- function(flukecatch, State, land){if(nrow(MD_common)==0){
#     flukecatch %>% 
#     group_by(State=="MD") %>% 
#     filter(
#       State=="MD",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - MDcatch_common) == min(abs(land - MDcatch_common)))
# }
# }
# MD_common2 <- MD_if_common(flukecatch, State, land)
MD_common <- bag_common(flukecatch, state = "MD", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = MDcatch_common)
MD_common <- MD_common %>% mutate(TargetMet = "TRUE")
MD_common2 <- bag_if_common(flukecatch, state = "MD", minlen = minlen,
                            seasonlen = seasonlen,
                            target = MDcatch_common, prev_result = MD_common)

#Connecticut
# functioncatchCT_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= CTcatch_common]))
# }
# CT_common <- functioncatchCT_common(flukecatch, State, land)
# CT_common <- CT_common %>% mutate(TargetMet = "TRUE")
# 
# CT_if_common <- function(flukecatch, State, land){if(nrow(CT_common)==0){
#     flukecatch %>% 
#     group_by(State=="CT") %>% 
#     filter(
#       State=="CT",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - CTcatch_common) == min(abs(land - CTcatch_common)))
# }
# }
# CT_common2 <- CT_if_common(flukecatch, State, land)
CT_common <- bag_common(flukecatch, state = "CT", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = CTcatch_common)
CT_common <- CT_common %>% mutate(TargetMet = "TRUE")
CT_common2 <- bag_if_common(flukecatch, state = "CT", minlen = minlen,
                            seasonlen = seasonlen,
                            target = CTcatch_common, prev_result = CT_common)

#Virginia
# functioncatchVA_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= VAcatch_common]))
# }
# VA_common <- functioncatchVA_common(flukecatch, State, land)
# VA_common <- VA_common %>% mutate(TargetMet = "TRUE")
# 
# VA_if_common <- function(flukecatch, State, land){if(nrow(VA_common)==0){
#     flukecatch %>% 
#     group_by(State=="VA") %>% 
#     filter(
#       State=="VA",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - VAcatch_common) == min(abs(land - VAcatch_common)))
# }
# }
# VA_common2 <- VA_if_common(flukecatch, State, land)
VA_common <- bag_common(flukecatch, state = "VA", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = VAcatch_common)
VA_common <- VA_common %>% mutate(TargetMet = "TRUE")
VA_common2 <- bag_if_common(flukecatch, state = "VA", minlen = minlen,
                            seasonlen = seasonlen,
                            target = VAcatch_common, prev_result = VA_common)

#New Jersey
# functioncatchNJ_common <- function(flukecatch, State, land){
#   flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(
#       land == max(land[land <= NJcatch_common]))
# }
# NJ_common <- functioncatchNJ_common(flukecatch, State, land)
# NJ_common <- NJ_common %>% mutate(TargetMet = "TRUE")
# 
# NJ_if_common <- function(flukecatch, State, land){if(nrow(NJ_common)==0){
#     flukecatch %>% 
#     group_by(State=="NJ") %>% 
#     filter(
#       State=="NJ",
#       SeasonLen == max(SeasonLen[SeasonLen <= seasonlen]),
#       MinLen == minlen) %>%
#     filter(abs(land - NJcatch_common) == min(abs(land - NJcatch_common)))
# }
# }
# NJ_common2 <- NJ_if_common(flukecatch, State, land)
NJ_common <- bag_common(flukecatch, state = "NJ", 
                        minlen = minlen,
                        seasonlen = seasonlen,
                        target = NJcatch_common)
NJ_common <- NJ_common %>% mutate(TargetMet = "TRUE")
NJ_common2 <- bag_if_common(flukecatch, state = "NJ", minlen = minlen,
                            seasonlen = seasonlen,
                            target = NJcatch_common, prev_result = NJ_common)

if(nrow(NC_common)==0){NC_common <- NC_common2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_common)==0){DE_common <- DE_common2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_common)==0){MA_common <- MA_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_common)==0){RI_common <- RI_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_common)==0){NY_common <- NY_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_common)==0){NJ_common <- NJ_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_common)==0){MD_common <- MD_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_common)==0){CT_common <- CT_common2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_common)==0){VA_common <- VA_common2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common <- list(NC_common, VA_common, MD_common, DE_common, NJ_common, NY_common, CT_common, RI_common, MA_common)
targetcatchregs_common_ <- rbindlist(targetcatchregs_common, fill = TRUE, )
targetcatchregs_common_ = subset(targetcatchregs_common_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))
#targetcatchregs_common_

#' Flexible Regs Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

flex_common <- function(flukecatch, state, minlen, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
flex_if_common <- function(flukecatch, state, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      #group_by(State=="NC") %>% 
      filter(
        State== state) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

#North Carolina
NC_flex <- flex_common(flukecatch, state = "NC",
                       target = NCcatch_common)
NC_flex <- NC_flex %>% mutate(TargetMet = "TRUE")
NC_flex2 <- flex_if_common(flukecatch, state = "NC",
                           target = NCcatch_common, prev_result = NC_flex)

#Delaware
DE_flex <- flex_common(flukecatch, state = "DE", 
                       target = DEcatch_common)
DE_flex <- DE_flex %>% mutate(TargetMet = "TRUE")
DE_flex2 <- flex_if_common(flukecatch, state = "DE",
                           target = DEcatch_common, prev_result = DE_flex)

#Massachusetts
MA_flex <- flex_common(flukecatch, state = "MA", 
                       target = MAcatch_common)
MA_flex <- MA_flex %>% mutate(TargetMet = "TRUE")
MA_flex2 <- flex_if_common(flukecatch, state = "MA",
                           target = MAcatch_common, prev_result = MA_flex)

#Rhode Island
RI_flex <- flex_common(flukecatch, state = "RI", 
                       target = RIcatch_common)
RI_flex <- RI_flex %>% mutate(TargetMet = "TRUE")
RI_flex2 <- flex_if_common(flukecatch, state = "RI",
                           target = RIcatch_common, prev_result = RI_flex)

#New York
NY_flex <- flex_common(flukecatch, state = "NY", 
                       target = NYcatch_common)
NY_flex <- NY_flex %>% mutate(TargetMet = "TRUE")
NY_flex2 <- flex_if_common(flukecatch, state = "NY",
                           target = NYcatch_common, prev_result = NY_flex)


#Maryland
MD_flex <- flex_common(flukecatch, state = "MD", 
                       target = MDcatch_common)
MD_flex <- MD_flex %>% mutate(TargetMet = "TRUE")
MD_flex2 <- flex_if_common(flukecatch, state = "MD",
                           target = MDcatch_common, prev_result = MD_flex)

#Connecticut
CT_flex <- flex_common(flukecatch, state = "CT", 
                       target = CTcatch_common)
CT_flex <- CT_flex %>% mutate(TargetMet = "TRUE")
CT_flex2 <- flex_if_common(flukecatch, state = "CT", 
                           target = CTcatch_common, prev_result = CT_flex)

#Virginia
VA_flex <- flex_common(flukecatch, state = "VA", 
                       target = VAcatch_common)
VA_flex <- VA_flex %>% mutate(TargetMet = "TRUE")
VA_flex2 <- flex_if_common(flukecatch, state = "VA",
                           target = VAcatch_common, prev_result = VA_flex)

#New Jersey
NJ_flex <- flex_common(flukecatch, state = "NJ", 
                       target = NJcatch_common)
NJ_flex <- NJ_flex %>% mutate(TargetMet = "TRUE")
NJ_flex2 <- flex_if_common(flukecatch, state = "NJ",
                           target = NJcatch_common, prev_result = NJ_flex)

if(nrow(NC_flex)==0){NC_flex <- NC_flex2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_flex)==0){DE_flex <- DE_flex2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_flex)==0){MA_flex <- MA_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_flex)==0){RI_flex <- RI_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_flex)==0){NY_flex <- NY_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_flex)==0){NJ_flex <- NJ_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_flex)==0){MD_flex <- MD_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_flex)==0){CT_flex <- CT_flex2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_flex)==0){VA_flex <- VA_flex2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common_flex <- list(NC_flex, VA_flex, MD_flex, DE_flex, NJ_flex, NY_flex, CT_flex, RI_flex, MA_flex)
targetcatchregs_common_flex_ <- rbindlist(targetcatchregs_common_flex, fill = TRUE, )
targetcatchregs_common_flex_ = subset(targetcatchregs_common_flex_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))
#targetcatchregs_common_

#' Bag + Length Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

BL_common <- function(flukecatch, state, minlen, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
BL_if_common <- function(flukecatch, state, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      #group_by(State=="NC") %>% 
      filter(
        State== state,
        SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

#North Carolina
NC_BL <- BL_common(flukecatch, state = "NC", seasonlen = seasonlen,
                   target = NCcatch_common)
NC_BL <- NC_BL %>% mutate(TargetMet = "TRUE")
NC_BL2 <- BL_if_common(flukecatch, state = "NC", seasonlen = seasonlen,
                       target = NCcatch_common, prev_result = NC_BL)

#Delaware
DE_BL <- BL_common(flukecatch, state = "DE", seasonlen = seasonlen,
                   target = DEcatch_common)
DE_BL <- DE_BL %>% mutate(TargetMet = "TRUE")
DE_BL2 <- BL_if_common(flukecatch, state = "DE", seasonlen = seasonlen,
                       target = DEcatch_common, prev_result = DE_BL)

#Massachusetts
MA_BL <- BL_common(flukecatch, state = "MA", seasonlen = seasonlen,
                   target = MAcatch_common)
MA_BL <- MA_BL %>% mutate(TargetMet = "TRUE")
MA_BL2 <- BL_if_common(flukecatch, state = "MA",seasonlen = seasonlen,
                       target = MAcatch_common, prev_result = MA_BL)

#Rhode Island
RI_BL <- BL_common(flukecatch, state = "RI", seasonlen = seasonlen,
                   target = RIcatch_common)
RI_BL <- RI_BL %>% mutate(TargetMet = "TRUE")
RI_BL2 <- BL_if_common(flukecatch, state = "RI",seasonlen = seasonlen,
                       target = RIcatch_common, prev_result = RI_BL)

#New York
NY_BL <- BL_common(flukecatch, state = "NY", seasonlen = seasonlen,
                   target = NYcatch_common)
NY_BL <- NY_BL %>% mutate(TargetMet = "TRUE")
NY_BL2 <- BL_if_common(flukecatch, state = "NY", seasonlen = seasonlen,
                       target = NYcatch_common, prev_result = NY_BL)


#Maryland
MD_BL <- BL_common(flukecatch, state = "MD", seasonlen = seasonlen,
                   target = MDcatch_common)
MD_BL <- MD_BL %>% mutate(TargetMet = "TRUE")
MD_BL2 <- BL_if_common(flukecatch, state = "MD", seasonlen = seasonlen,
                       target = MDcatch_common, prev_result = MD_BL)

#Connecticut
CT_BL <- BL_common(flukecatch, state = "CT", seasonlen = seasonlen,
                   target = CTcatch_common)
CT_BL <- CT_BL %>% mutate(TargetMet = "TRUE")
CT_BL2 <- BL_if_common(flukecatch, state = "CT", seasonlen = seasonlen,
                       target = CTcatch_common, prev_result = CT_BL)

#Virginia
VA_BL <- BL_common(flukecatch, state = "VA", seasonlen = seasonlen,
                   target = VAcatch_common)
VA_BL <- VA_BL %>% mutate(TargetMet = "TRUE")
VA_BL2 <- BL_if_common(flukecatch, state = "VA", seasonlen = seasonlen,
                       target = VAcatch_common, prev_result = VA_BL)

#New Jersey
NJ_BL <- BL_common(flukecatch, state = "NJ", seasonlen = seasonlen,
                   target = NJcatch_common)
NJ_BL <- NJ_BL %>% mutate(TargetMet = "TRUE")
NJ_BL2 <- BL_if_common(flukecatch, state = "NJ", seasonlen = seasonlen,
                       target = NJcatch_common, prev_result = NJ_BL)

if(nrow(NC_BL)==0){NC_BL <- NC_BL2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_BL)==0){DE_BL <- DE_BL2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_BL)==0){MA_BL <- MA_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_BL)==0){RI_BL <- RI_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_BL)==0){NY_BL <- NY_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_BL)==0){NJ_BL <- NJ_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_BL)==0){MD_BL <- MD_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_BL)==0){CT_BL <- CT_BL2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_BL)==0){VA_BL <- VA_BL2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common_BL <- list(NC_BL, VA_BL, MD_BL, DE_BL, NJ_BL, NY_BL, CT_BL, RI_BL, MA_BL)
targetcatchregs_common_BL_ <- rbindlist(targetcatchregs_common_BL, fill = TRUE, )
targetcatchregs_common_BL_ = subset(targetcatchregs_common_BL_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))

#' Bag + Season Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

BS_common <- function(flukecatch, state, minlen, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
BS_if_common <- function(flukecatch, state, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      #group_by(State=="NC") %>% 
      filter(
        State== state,
        MinLen == minlen) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

#North Carolina
NC_BS <- BS_common(flukecatch, state = "NC", minlen = minlen,
                   target = NCcatch_common)
NC_BS <- NC_BS %>% mutate(TargetMet = "TRUE")
NC_BS2 <- BS_if_common(flukecatch, state = "NC", minlen = minlen,
                       target = NCcatch_common, prev_result = NC_BS)

#Delaware
DE_BS <- BS_common(flukecatch, state = "DE", minlen = minlen,
                   target = DEcatch_common)
DE_BS <- DE_BS %>% mutate(TargetMet = "TRUE")
DE_BS2 <- BS_if_common(flukecatch, state = "DE", minlen = minlen,
                       target = DEcatch_common, prev_result = DE_BS)

#Massachusetts
MA_BS <- BS_common(flukecatch, state = "MA", minlen = minlen,
                   target = MAcatch_common)
MA_BS <- MA_BS %>% mutate(TargetMet = "TRUE")
MA_BS2 <- BS_if_common(flukecatch, state = "MA",minlen = minlen,
                       target = MAcatch_common, prev_result = MA_BS)

#Rhode Islands
RI_BS <- BS_common(flukecatch, state = "RI", minlen = minlen,
                   target = RIcatch_common)
RI_BS <- RI_BS %>% mutate(TargetMet = "TRUE")
RI_BS2 <- BS_if_common(flukecatch, state = "RI",minlen = minlen,
                       target = RIcatch_common, prev_result = RI_BS)

#New York
NY_BS <- BS_common(flukecatch, state = "NY", minlen = minlen,
                   target = NYcatch_common)
NY_BS <- NY_BS %>% mutate(TargetMet = "TRUE")
NY_BS2 <- BS_if_common(flukecatch, state = "NY", minlen = minlen,
                       target = NYcatch_common, prev_result = NY_BS)


#Maryland
MD_BS <- BS_common(flukecatch, state = "MD", minlen = minlen,
                   target = MDcatch_common)
MD_BS <- MD_BS %>% mutate(TargetMet = "TRUE")
MD_BS2 <- BS_if_common(flukecatch, state = "MD", minlen = minlen,
                       target = MDcatch_common, prev_result = MD_BS)

#Connecticut
CT_BS <- BS_common(flukecatch, state = "CT", minlen = minlen,
                   target = CTcatch_common)
CT_BS <- CT_BS %>% mutate(TargetMet = "TRUE")
CT_BS2 <- BS_if_common(flukecatch, state = "CT", minlen = minlen,
                       target = CTcatch_common, prev_result = CT_BS)

#Virginia
VA_BS <- BS_common(flukecatch, state = "VA", minlen = minlen,
                   target = VAcatch_common)
VA_BS <- VA_BS %>% mutate(TargetMet = "TRUE")
VA_BS2 <- BS_if_common(flukecatch, state = "VA", minlen = minlen,
                       target = VAcatch_common, prev_result = VA_BS)

#New Jersey
NJ_BS <- BS_common(flukecatch, state = "NJ", minlen = minlen,
                   target = NJcatch_common)
NJ_BS <- NJ_BS %>% mutate(TargetMet = "TRUE")
NJ_BS2 <- BS_if_common(flukecatch, state = "NJ", minlen = minlen,
                       target = NJcatch_common, prev_result = NJ_BS)

if(nrow(NC_BS)==0){NC_BS <- NC_BS2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_BS)==0){DE_BS <- DE_BS2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_BS)==0){MA_BS <- MA_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_BS)==0){RI_BS <- RI_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_BS)==0){NY_BS <- NY_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_BS)==0){NJ_BS <- NJ_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_BS)==0){MD_BS <- MD_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_BS)==0){CT_BS <- CT_BS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_BS)==0){VA_BS <- VA_BS2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common_BS <- list(NC_BS, VA_BS, MD_BS, DE_BS, NJ_BS, NY_BS, CT_BS, RI_BS, MA_BS)
targetcatchregs_common_BS_ <- rbindlist(targetcatchregs_common_BS, fill = TRUE, )
targetcatchregs_common_BS_ = subset(targetcatchregs_common_BS_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))

#' Length + Season Change:
## -----------------------------------------------------------------------------
NCcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg_HCR$land[catchallstates_commonreg$State=="NJ"]

LS_common <- function(flukecatch, state, minlen, seasonlen, target){
  flukecatch %>% 
    #group_by(State=="NC") %>% 
    filter(
      State== state,
      Bag == bag) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
LS_if_common <- function(flukecatch, state, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      #group_by(State=="NC") %>% 
      filter(
        State== state,
        Bag == bag) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

#North Carolina
NC_LS <- LS_common(flukecatch, state = "NC",
                   target = NCcatch_common)
NC_LS <- NC_LS %>% mutate(TargetMet = "TRUE")
NC_LS2 <- LS_if_common(flukecatch, state = "NC",
                       target = NCcatch_common, prev_result = NC_LS)

#Delaware
DE_LS <- LS_common(flukecatch, state = "DE",
                   target = DEcatch_common)
DE_LS <- DE_LS %>% mutate(TargetMet = "TRUE")
DE_LS2 <- LS_if_common(flukecatch, state = "DE",
                       target = DEcatch_common, prev_result = DE_LS)

#Massachusetts
MA_LS <- LS_common(flukecatch, state = "MA",
                   target = MAcatch_common)
MA_LS <- MA_LS %>% mutate(TargetMet = "TRUE")
MA_LS2 <- LS_if_common(flukecatch, state = "MA",
                       target = MAcatch_common, prev_result = MA_LS)

#Rhode Island
RI_LS <- LS_common(flukecatch, state = "RI",
                   target = RIcatch_common)
RI_LS <- RI_LS %>% mutate(TargetMet = "TRUE")
RI_LS2 <- LS_if_common(flukecatch, state = "RI",
                       target = RIcatch_common, prev_result = RI_LS)

#New York
NY_LS <- LS_common(flukecatch, state = "NY",
                   target = NYcatch_common)
NY_LS <- NY_LS %>% mutate(TargetMet = "TRUE")
NY_LS2 <- LS_if_common(flukecatch, state = "NY",
                       target = NYcatch_common, prev_result = NY_LS)


#Maryland
MD_LS <- LS_common(flukecatch, state = "MD",
                   target = MDcatch_common)
MD_LS <- MD_LS %>% mutate(TargetMet = "TRUE")
MD_LS2 <- LS_if_common(flukecatch, state = "MD",
                       target = MDcatch_common, prev_result = MD_LS)

#Connecticut
CT_LS <- LS_common(flukecatch, state = "CT",
                   target = CTcatch_common)
CT_LS <- CT_LS %>% mutate(TargetMet = "TRUE")
CT_LS2 <- LS_if_common(flukecatch, state = "CT",
                       target = CTcatch_common, prev_result = CT_LS)

#Virginia
VA_LS <- LS_common(flukecatch, state = "VA",
                   target = VAcatch_common)
VA_LS <- VA_LS %>% mutate(TargetMet = "TRUE")
VA_LS2 <- LS_if_common(flukecatch, state = "VA",
                       target = VAcatch_common, prev_result = VA_LS)

#New Jersey
NJ_LS <- LS_common(flukecatch, state = "NJ",
                   target = NJcatch_common)
NJ_LS <- NJ_LS %>% mutate(TargetMet = "TRUE")
NJ_LS2 <- LS_if_common(flukecatch, state = "NJ",
                       target = NJcatch_common, prev_result = NJ_LS)

if(nrow(NC_LS)==0){NC_LS <- NC_LS2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(DE_LS)==0){DE_LS <- DE_LS2 %>%  mutate(TargetMet = "FALSE")}
if(nrow(MA_LS)==0){MA_LS <- MA_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(RI_LS)==0){RI_LS <- RI_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NY_LS)==0){NY_LS <- NY_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(NJ_LS)==0){NJ_LS <- NJ_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(MD_LS)==0){MD_LS <- MD_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(CT_LS)==0){CT_LS <- CT_LS2 %>% mutate(TargetMet = "FALSE")}
if(nrow(VA_LS)==0){VA_LS <- VA_LS2 %>% mutate(TargetMet = "FALSE")}

targetcatchregs_common_LS <- list(NC_LS, VA_LS, MD_LS, DE_LS, NJ_LS, NY_LS, CT_LS, RI_LS, MA_LS)
targetcatchregs_common_LS_ <- rbindlist(targetcatchregs_common_LS, fill = TRUE, )
targetcatchregs_common_LS_ = subset(targetcatchregs_common_LS_, select = c(State, Bag, MinLen, SeasonLen, land, disc, TargetMet))


#' Aggregating Everything Together:
## -----------------------------------------------------------------------------
targetcatchregstest_seasonlength1 <- data.frame(targetcatchregs_seasonlen_, "Season Change")
targetcatchregstest_minlength1 <- data.frame(targetcatchregs_common_minlen_, "Length Change")
targetcatchregstest_baglimit1 <- data.frame(targetcatchregs_common_, "Bag Change")
targetcatchregstest_flex1 <- data.frame(targetcatchregs_common_flex_, "Any Regs")
targetcatchregstest_BL1 <- data.frame(targetcatchregs_common_BL_, "Bag + Length")
targetcatchregstest_BS1 <- data.frame(targetcatchregs_common_BS_, "Bag + Season")
targetcatchregstest_LS1 <- data.frame(targetcatchregs_common_LS_, "Length + Season")

names(targetcatchregstest_seasonlength1)[8] = "Reg Changed"
names(targetcatchregstest_minlength1)[8] = "Reg Changed"
names(targetcatchregstest_baglimit1)[8] = "Reg Changed"
names(targetcatchregstest_flex1)[8] = "Reg Changed"
names(targetcatchregstest_BL1)[8] = "Reg Changed"
names(targetcatchregstest_BS1)[8] = "Reg Changed"
names(targetcatchregstest_LS1)[8] = "Reg Changed"

finaltable1 <- rbind(targetcatchregstest_seasonlength1, targetcatchregstest_minlength1, targetcatchregstest_baglimit1,
                     targetcatchregstest_flex1, targetcatchregstest_BL1, targetcatchregstest_BS1, targetcatchregstest_LS1)

#RDS files depending on control rule selected

# #Percent Change Approach
# if(Alternative == "Table1"){
#  #saveRDS(finaltable1, file = "PCA_BiologicallyInformed_CatchTable_NYRegs.rds")
# #PCA_BioInformed_CatchTable_NYRegs <- readRDS("PCA_BiologicallyInformed_CatchTable_NYRegs.rds")
# }
# 
# #Biological Reference Point Approach 
# if(Alternative == "Table2"){
#  #saveRDS(finaltable1, file = "BRP_BiologicallyInformed_CatchTable_NYRegs.rds")
# #BRP_BioInformed_CatchTable_NYRegs <- readRDS("BRP_BiologicallyInformed_CatchTable_NYRegs.rds")
# }
# 
# #Biomass Based Matrix Approach
# if(Alternative == "Table3"){
#  #saveRDS(finaltable1, file = "BBM_BiologicallyInformed_CatchTable_NYRegs.rds")
# #BBM_BioInformed_CatchTable_NYRegs <- readRDS("BBM_BiologicallyInformed_CatchTable_NYRegs.rds")
# }

##########
# Write Output for rec demand model
source("write_regs_table.R")
new_regs_table <- pd_multiply_allregulations_input_HCRgrouping_regdistinct

if (reg_to_change == "Season") saveRDS(new_regs_table$Season,"new_regs_table.rds")
if (reg_to_change == "Bag") saveRDS(new_regs_table$Bag,"new_regs_table.rds")
if (reg_to_change == "Length") saveRDS(new_regs_table$Length,"new_regs_table.rds")
if (reg_to_change == "All") saveRDS(new_regs_table$All,"new_regs_table.rds")
if (reg_to_change == "BagLength") saveRDS(new_regs_table$BagLength, "new_regs_table.rds")
if (reg_to_change == "BagSeason") saveRDS(new_regs_table$BagSeason, "new_regs_table.rds")
if (reg_to_change == "LengthSeason") saveRDS(new_regs_table$LengthSeason, "new_regs_table.rds")

##########
# WRite Output for Sinatra
if (reg_to_change == "Season") {
exp_landings <- sum(targetcatchregstest_seasonlength1$land)
exp_discards <- sum(targetcatchregstest_seasonlength1$disc)
new_bag <- as.integer(targetcatchregstest_seasonlength1[1,2])
new_minlen <- as.numeric(targetcatchregstest_seasonlength1[1,3])
new_seasonlen <- as.integer(targetcatchregstest_seasonlength1[1,4])
}
if (reg_to_change == "Bag") {
  exp_landings <- sum(targetcatchregstest_baglimit1$land)
  exp_discards <- sum(targetcatchregstest_baglimit1$disc)
  new_bag <- as.integer(targetcatchregstest_baglimit1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_baglimit1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_baglimit1[1,4])
}
if (reg_to_change == "Length") {
  exp_landings <- sum(targetcatchregstest_minlength1$land)
  exp_discards <- sum(targetcatchregstest_minlength1$disc)
  new_bag <- as.integer(targetcatchregstest_minlength1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_minlength1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_minlength1[1,4])
}
if (reg_to_change == "All") {
  exp_landings <- sum(targetcatchregstest_flex1$land)
  exp_discards <- sum(targetcatchregstest_flex1$disc)
  new_bag <- as.integer(targetcatchregstest_flex1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_flex1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_flex1[1,4])
}

if (reg_to_change == "BagLength") {
  exp_landings <- sum(targetcatchregstest_BL1$land)
  exp_discards <- sum(targetcatchregstest_BL1$disc)
  new_bag <- as.integer(targetcatchregstest_BL1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_BL1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_BL1[1,4])
}

if (reg_to_change == "BagSeason") {
  exp_landings <- sum(targetcatchregstest_BS1$land)
  exp_discards <- sum(targetcatchregstest_BS1$disc)
  new_bag <- as.integer(targetcatchregstest_BS1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_BS1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_BS1[1,4])
}

if (reg_to_change == "LengthSeason") {
  exp_landings <- sum(targetcatchregstest_LS1$land)
  exp_discards <- sum(targetcatchregstest_LS1$disc)
  new_bag <- as.integer(targetcatchregstest_LS1[1,2])
  new_minlen <- as.numeric(targetcatchregstest_LS1[1,3])
  new_seasonlen <- as.integer(targetcatchregstest_LS1[1,4])
}

write(new_bag, file = "mgmt_regs.out")
write(new_minlen, file = "mgmt_regs.out", append = TRUE)
write(new_seasonlen, file = "mgmt_regs.out", append = TRUE)
write(exp_landings, file = "mgmt_regs.out", append = TRUE)
write(1, file = "mgmt_regs.out", append = TRUE)
write(0, file = "mgmt_regs.out", append = TRUE)
write(NewBin, file = "mgmt_regs.out", append = TRUE)
# exp_landings
# exp_discards
# new_bag
# new_minlen
# new_seasonlen

