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
#' Version of do_recmeasures with RDM GAM lookup table
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

#fluke <- readRDS("~/Desktop/FlounderMSE/state_lookup.rds")
#flukecatch <- tibble(fluke)

#' 
#' Inputs - Enter Current Regulations and Control Rule to be Tested
## -----------------------------------------------------------------------------
#Regs
bag = as.numeric(args[1]) #4
minlen = as.numeric(args[2]) #18.5
seasonlen = as.numeric(args[3]) #162
RHL <- as.numeric(args[6]) #6350000 # I just pulled this RHL value off the internet for now
#print(RHL)
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
#calib_glm <- readRDS("calib_glm.rds")
#nufluke <- tibble(
#  pred = flukecatch$land,
  #minlen = flukecatch$MinLen,
#  seaslen = flukecatch$SeasonLen,
 # bag = flukecatch$Bag,
#  biomass = SSB
#)
#pred_landings <- predict(calib_glm, newdata = nufluke, type = "response")
#update the lookup table values
#flukecatch$land <- as.numeric(pred_landings)

######################################

gamRDM2 <- readRDS("gam_RDMland_allOM_1BBMSY_state_setseed_final.rds") #disordered length
#View(RDMoutputbind_edit2)
input_seas2 <- c( 60,
                  75,
                  90,
                  105,
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
input_bag2 <- c(4,5,6,7,8)
input_minlen2 <- c(14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21)
state2 <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")
#SSB <- sample(RDMoutputbind_edit2$SSBcov, 1) #this will be replaced with whichever SSB value from operating model corresponds to a particular sim 
#SSB <- RDMoutputbind_edit2$SSBcov[1]
comb_input2 <- expand.grid(state = unique(state2), SeasonLen = unique(input_seas2), 
                           Bag = unique(input_bag2), MinLen = unique(input_minlen2))
comb_input2 <- comb_input2 %>% mutate(SSB = SSB)

#form lookup table 
gamfit2 <- predict.gam(gamRDM2, newdata = comb_input2, type = "response" , se.fit = TRUE)
flukecatch <- comb_input2 %>% mutate(land = gamfit2$fit)
names(flukecatch)[names(flukecatch) == "state"] <- "State"

flukecatch_summed <- flukecatch %>% 
  mutate(group = rep(1:ceiling(n()/9), each = 9)[1:n()]) %>%
  group_by(group) %>%
  summarise(land = sum(land, na.rm = TRUE))

flukecatch <- flukecatch %>% filter(State == "CT") %>% 
  select( Bag, MinLen, SeasonLen, SSB) %>%
  mutate(land = flukecatch_summed$land)

#specs for testing
#seasonlen <- 150
#bag <- 4
#minlen <- 17.5
#Alternative = "Table1"
#RHL = 6350000
#BBMSY = 1
#x<-1.2
#SSB = 30000
#reg_to_change = "Season"

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

#' 
#' Use GAM to extract CIs:
## -----------------------------------------------------------------------------

states <- c("DE", "MD", "NJ", "NY", "VA", "CT", "MA", "RI", "NC")
Bag <- bag 
MinLen <- minlen
SeasLen <- catchallstates_commonreg$SeasonLen[1]

 
 dat.all <- expand.grid(state = states,
                           Bag = Bag,
                           MinLen = MinLen,
                           SeasonLen = SeasLen,
                        SSB = SSB)

gamland <- gamRDM2
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
upperCI <- sum(output2$upr)
upperCI
lowerCI <- sum(output2$lwr)
lowerCI


#add the calibration adjustment
#calib_glm <- readRDS("mse/calib_glm.rds")
#nudata <- tibble(
  #pred = c(outputland, upperCI_test, lowerCI_test),
  #pred = c(4000000,4800000,3200000),
 # minlen = minlen,
 # seaslen = seasonlen,
 # bag = bag,
 # biomass = SSB
#)
#pred_harvest <- predict(calib_glm, newdata = nudata, 
                   #     se.fit=TRUE, type = "link")
#pred_harvest <- predict(calib_glm, newdata = nudata, 
 #                       se.fit=TRUE, type = "response")

#expectedharvest <- exp(pred_harvest$fit[1])
#upperCI <- exp(pred_harvest$fit[2] + (1.96 * pred_harvest$se.fit[2]))
#lowerCI <- exp(pred_harvest$fit[3] - (1.96 * pred_harvest$se.fit[3]))

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
 expectedharvest <- sum(catchallstates_commonreg$land) 
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

#' Season Length Change:
## -----------------------------------------------------------------------------

RobustMax <- function(x) {if (length(x)>0) max(x) else -Inf}

functioncatch_seasonlen <- function(flukecatch, bag, minlen, target){
  flukecatch %>% 
    filter(
      #   State== state, #"NC",
      Bag == bag,
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target])) 
}

state_if <- function(flukecatch, bag, minlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        #   State== state,
        Bag == bag,
        MinLen == minlen) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_seasonlen <- functioncatch_seasonlen(flukecatch,
                                                   bag = bag,
                                                   minlen = minlen,
                                                   target = catchallstates_commonreg_HCR$land)
expectedcatch_seasonlen <- expectedcatch_seasonlen %>% mutate(TargetMet = "TRUE")
expectedcatch_seasonlen2 <- state_if(flukecatch, bag = bag,
                                     minlen = minlen, target = catchallstates_commonreg_HCR$land,
                                     prev_result = expectedcatch_seasonlen)

if(nrow(expectedcatch_seasonlen)==0){expectedcatch_seasonlen <- expectedcatch_seasonlen2 %>%  mutate(TargetMet = "FALSE")}

#' Size Limit Change:
## -----------------------------------------------------------------------------

functioncatch_common_minlen <- function(flukecatch, bag, seasonlen, target){
  flukecatch %>% 
    filter(
      Bag == bag,
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
    filter(
      land == RobustMax(land[land <= target]))
}

state_if_minlen <- function(flukecatch, state, bag, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        Bag == bag,
        SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_minlen <- functioncatch_common_minlen(flukecatch, 
                                                    bag = bag,
                                                    seasonlen = seasonlen,
                                                    target = catchallstates_commonreg_HCR$land)

expectedcatch_minlen <- expectedcatch_minlen %>% mutate(TargetMet = "TRUE")
expectedcatch_minlen2 <- state_if_minlen(flukecatch, 
                                         bag = bag,
                                         seasonlen = seasonlen,
                                         target = catchallstates_commonreg_HCR$land,
                                         prev_result = expectedcatch_minlen)

if(nrow(expectedcatch_minlen)==0){expectedcatch_minlen <- expectedcatch_minlen2 %>%  mutate(TargetMet = "FALSE")}

#' Bag Limit Change:
## -----------------------------------------------------------------------------
bag_common <- function(flukecatch, minlen, seasonlen, target){
  flukecatch %>% 
    filter(
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen]),
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target]))
}

bag_if_common <- function(flukecatch, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen]),
        MinLen == minlen) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_common <- bag_common(flukecatch, 
                                   minlen = minlen,
                                   seasonlen = seasonlen,
                                   target = catchallstates_commonreg_HCR$land)
expectedcatch_common <- expectedcatch_common %>% mutate(TargetMet = "TRUE")
expectedcatch_common2 <- bag_if_common(flukecatch,  minlen = minlen,
                                       seasonlen = seasonlen,
                                       target = catchallstates_commonreg_HCR$land, prev_result = expectedcatch_common)

if(nrow(expectedcatch_common)==0){expectedcatch_common <- expectedcatch_common2 %>%  mutate(TargetMet = "FALSE")}

#' Flexible Regs Change:
## -----------------------------------------------------------------------------
flex_common <- function(flukecatch, minlen, seasonlen, target){
  flukecatch %>% 
    filter(
      land == RobustMax(land[land <= target]))
}
flex_if_common <- function(flukecatch, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_flex <- flex_common(flukecatch,
                                  target = catchallstates_commonreg_HCR$land)
expectedcatch_flex <- expectedcatch_flex %>% mutate(TargetMet = "TRUE")
expectedcatch_flex2 <- flex_if_common(flukecatch,
                                      target = catchallstates_commonreg_HCR$land, prev_result = expectedcatch_flex)

if(nrow(expectedcatch_flex)==0){expectedcatch_flex <- expectedcatch_flex2 %>%  mutate(TargetMet = "FALSE")}

#' Bag + Length Change:
## -----------------------------------------------------------------------------

BL_common <- function(flukecatch, minlen, seasonlen, target){
  flukecatch %>% 
    filter(
      SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
BL_if_common <- function(flukecatch, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        SeasonLen == RobustMax(SeasonLen[SeasonLen <= seasonlen])) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_BL <- BL_common(flukecatch, seasonlen = seasonlen,
                   target = catchallstates_commonreg_HCR$land)
expectedcatch_BL <- expectedcatch_BL %>% mutate(TargetMet = "TRUE")
expectedcatch_BL2 <- BL_if_common(flukecatch, seasonlen = seasonlen,
                       target = catchallstates_commonreg_HCR$land, prev_result = expectedcatch_BL)

if(nrow(expectedcatch_BL)==0){expectedcatch_BL <- expectedcatch_BL2 %>%  mutate(TargetMet = "FALSE")}

#' Bag + Season Change:
## -----------------------------------------------------------------------------

BS_common <- function(flukecatch, minlen, seasonlen, target){
  flukecatch %>% 
    filter(
      MinLen == minlen) %>%
    filter(
      land == RobustMax(land[land <= target]))
}
BS_if_common <- function(flukecatch, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        MinLen == minlen) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_BS <- BS_common(flukecatch, minlen = minlen,
                   target = catchallstates_commonreg_HCR$land)
expectedcatch_BS <- expectedcatch_BS %>% mutate(TargetMet = "TRUE")
expectedcatch_BS2 <- BS_if_common(flukecatch, minlen = minlen,
                       target = catchallstates_commonreg_HCR$land, prev_result = expectedcatch_BS)

if(nrow(expectedcatch_BS)==0){expectedcatch_BS <- expectedcatch_BS2 %>%  mutate(TargetMet = "FALSE")}

#' Length + Season Change:
## -----------------------------------------------------------------------------

LS_common <- function(flukecatch, minlen, seasonlen, target){
  flukecatch %>% 
    filter(
      Bag == bag) %>%
   filter(
      land == RobustMax(land[land <= target]))
}
LS_if_common <- function(flukecatch, minlen, seasonlen, target, prev_result) {
  if(nrow(prev_result)==0){
    flukecatch %>% 
      filter(
        Bag == bag) %>%
      filter(abs(land - target) == min(abs(land - target)))
  }
}

expectedcatch_LS <- LS_common(flukecatch, 
                   target = catchallstates_commonreg_HCR$land)
expectedcatch_LS <- expectedcatch_LS %>% mutate(TargetMet = "TRUE")
expectedcatch_LS2 <- LS_if_common(flukecatch, 
                       target = catchallstates_commonreg_HCR$land, prev_result = expectedcatch_LS)

if(nrow(expectedcatch_LS)==0){expectedcatch_LS <- expectedcatch_LS2 %>%  mutate(TargetMet = "FALSE")}

#' Aggregating Everything Together:
## -----------------------------------------------------------------------------
targetcatchregstest_seasonlength1 <- data.frame(expectedcatch_seasonlen, "Season Change")
targetcatchregstest_minlength1 <- data.frame(expectedcatch_minlen, "Length Change")
targetcatchregstest_baglimit1 <- data.frame(expectedcatch_common, "Bag Change")
targetcatchregstest_flex1 <- data.frame(expectedcatch_flex, "Any Regs")
targetcatchregstest_BL1 <- data.frame(expectedcatch_BL, "Bag + Length")
targetcatchregstest_BS1 <- data.frame(expectedcatch_BS, "Bag + Season")
targetcatchregstest_LS1 <- data.frame(expectedcatch_LS, "Length + Season")

names(targetcatchregstest_seasonlength1)[7] = "Reg Changed"
names(targetcatchregstest_minlength1)[7] = "Reg Changed"
names(targetcatchregstest_baglimit1)[7] = "Reg Changed"
names(targetcatchregstest_flex1)[7] = "Reg Changed"
names(targetcatchregstest_BL1)[7] = "Reg Changed"
names(targetcatchregstest_BS1)[7] = "Reg Changed"
names(targetcatchregstest_LS1)[7] = "Reg Changed"

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
#exp_discards <- sum(targetcatchregstest_seasonlength1$disc)
new_bag <- as.integer(targetcatchregstest_seasonlength1[1])
new_minlen <- as.numeric(targetcatchregstest_seasonlength1[2])
new_seasonlen <- as.integer(targetcatchregstest_seasonlength1[3])
}
if (reg_to_change == "Bag") {
  exp_landings <- sum(targetcatchregstest_baglimit1$land)
#  exp_discards <- sum(targetcatchregstest_baglimit1$disc)
  new_bag <- as.integer(targetcatchregstest_baglimit1[1])
  new_minlen <- as.numeric(targetcatchregstest_baglimit1[2])
  new_seasonlen <- as.integer(targetcatchregstest_baglimit1[3])
}
if (reg_to_change == "Length") {
  exp_landings <- sum(targetcatchregstest_minlength1$land)
 # exp_discards <- sum(targetcatchregstest_minlength1$disc)
  new_bag <- as.integer(targetcatchregstest_minlength1[1])
  new_minlen <- as.numeric(targetcatchregstest_minlength1[2])
  new_seasonlen <- as.integer(targetcatchregstest_minlength1[3])
}
if (reg_to_change == "All") {
  exp_landings <- sum(targetcatchregstest_flex1$land)
 # exp_discards <- sum(targetcatchregstest_flex1$disc)
  new_bag <- as.integer(targetcatchregstest_flex1[1])
  new_minlen <- as.numeric(targetcatchregstest_flex1[2])
  new_seasonlen <- as.integer(targetcatchregstest_flex1[3])
}

if (reg_to_change == "BagLength") {
  exp_landings <- sum(targetcatchregstest_BL1$land)
 # exp_discards <- sum(targetcatchregstest_BL1$disc)
  new_bag <- as.integer(targetcatchregstest_BL1[1])
  new_minlen <- as.numeric(targetcatchregstest_BL1[2])
  new_seasonlen <- as.integer(targetcatchregstest_BL1[3])
}

if (reg_to_change == "BagSeason") {
  exp_landings <- sum(targetcatchregstest_BS1$land)
 # exp_discards <- sum(targetcatchregstest_BS1$disc)
  new_bag <- as.integer(targetcatchregstest_BS1[1])
  new_minlen <- as.numeric(targetcatchregstest_BS1[2])
  new_seasonlen <- as.integer(targetcatchregstest_BS1[3])
}

if (reg_to_change == "LengthSeason") {
  exp_landings <- sum(targetcatchregstest_LS1$land)
 # exp_discards <- sum(targetcatchregstest_LS1$disc)
  new_bag <- as.integer(targetcatchregstest_LS1[1])
  new_minlen <- as.numeric(targetcatchregstest_LS1[2])
  new_seasonlen <- as.integer(targetcatchregstest_LS1[3])
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

