
library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)

#code for calculating coastwide harvest for hierarchical RDM GAM for do_recmeasures_hcr.R script
#code for showing how different regulations affect ability of GAM to predict target catch 

#adjust these depending on what regulations you want to visualize/test
bag = 8
minlen = 14
seasonlen = 300
SSB <- 29182.86 #29154.03 

gamRDM2 <-  readRDS("~/Desktop/FlounderMSE/RDMgam/setseed/models/gam_RDMland_allOM_1BBMSY_state_setseed_final_2.rds") #disordered length
#gamRDM2 <-  readRDS("~/Desktop/FlounderMSE/RDMgam/RDMPreviousAttempts/oldmodels/gam_RDMland.rds") #disordered length

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

#' Determine Expected Catch Given Current Regulations from Lookup Table: coastwide 
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

scalar <- c(1.1, 0.9, 1.2, 0.8, 1.4, 0.6)

n = length(scalar)

finaltable1 = list()
finaltable1 = vector("list", length = n)

#i <- 1

for(i in 1:length(scalar)){
catchallstates_commonreg_HCR <- catchallstates_commonreg
catchallstates_commonreg_HCR$land <- scalar[i]*catchallstates_commonreg_HCR$land

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

targetcatchregstest_seasonlength1 <- data.frame(expectedcatch_seasonlen, "Season Change")
targetcatchregstest_minlength1 <- data.frame(expectedcatch_minlen, "Length Change")
targetcatchregstest_baglimit1 <- data.frame(expectedcatch_common, "Bag Change")
targetcatchregstest_flex1 <- data.frame(expectedcatch_flex, "Any Regs")

names(targetcatchregstest_seasonlength1)[7] = "Reg Changed"
names(targetcatchregstest_minlength1)[7] = "Reg Changed"
names(targetcatchregstest_baglimit1)[7] = "Reg Changed"
names(targetcatchregstest_flex1)[7] = "Reg Changed"

finaltable <- rbind(targetcatchregstest_seasonlength1, targetcatchregstest_minlength1, targetcatchregstest_baglimit1,
                     targetcatchregstest_flex1)

if(i == 1){finaltable1[[i]] <- finaltable %>% mutate(Scen = "+10", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}
if(i == 2){finaltable1[[i]] <- finaltable %>% mutate(Scen = "-10", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}
if(i == 3){finaltable1[[i]] <- finaltable %>% mutate(Scen = "+20", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}
if(i == 4){finaltable1[[i]] <- finaltable %>% mutate(Scen = "-20", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}
if(i == 5){finaltable1[[i]] <- finaltable %>% mutate(Scen = "+40", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}
if(i == 6){finaltable1[[i]] <- finaltable %>% mutate(Scen = "-40", Target = catchallstates_commonreg_HCR$land, Original = catchallstates_commonreg$land)}

}

finaltable1_bind <- rbindlist(finaltable1) %>% janitor::clean_names()
#finaltable1_bind <- finaltable2_bind
#View(finaltable1_bind)

#plot target catch vs expected catch

library(ggplot2)
library(ggpubr)

#level <- c("-10%", "+10%", "-20%", "+20%", "-40%", "+40%")
level <- c("-40", "-20", "-10", "+10", "+20", "+40")
targetindicator <- c("Target", "Target", "Target","Target","Target","Target")
targettype <- c("Target", "TRUE", "FALSE")

#season 

seasondata <- finaltable1_bind %>% 
  filter(reg_changed == 'Season Change') 

seasondata2 <- tibble(
  Landings = c(seasondata$target, seasondata$land),
  Mortality = rep(seasondata$scen, times = 2),
  TargetMet = c(targetindicator, seasondata$target_met),
  InitialExpected = rep(seasondata$original, times = 2)
)

seasonlength <- ggplot(data = seasondata2, aes(x=factor(Mortality, levels = level),
                      y=Landings, group = factor(TargetMet, levels = targettype),
                      shape = factor(TargetMet, levels = targettype), 
                      color = factor(TargetMet, levels = targettype),
                      size = factor(TargetMet, levels = targettype))) + 
  geom_point() + scale_shape_manual(values = c(2,16,4)) + 
  scale_color_manual(values = c("blue","red","red")) + scale_size_manual(values = c(6,4,4)) +
  labs(color = "Target Catch Met", shape = "Target Catch Met", size = "Target Catch Met") +
  geom_line(aes(group = Mortality), linewidth = 0.5, col = "black") + 
  scale_y_continuous(limits = c(min(seasondata2$Landings-0.5e6), max(seasondata2$Landings+0.5e6))) + #this will need to be adjusted depending on the regulations used e.g 0.25e5 for restrictive, 1e6 for liberal, 
  theme_linedraw() + 
  xlab("Harvest Control Rules - % Reduction/Liberalization of Catch") + 
  ylab("Expected Landings") + geom_hline(yintercept = seasondata2$InitialExpected, linetype=2) +
  # geom_text(x="-40%", y=1295208.6, label="Expected Catch at Starting Regulations", size=4, col = "black") +
  theme(legend.position = "none") + theme(plot.margin = unit(c(1,1,1,1), "lines")) 

#size

lengthdata <- finaltable1_bind %>% 
  filter(reg_changed == 'Length Change') 

lengthdata2 <- tibble(
  Landings = c(lengthdata$target, lengthdata$land),
  Mortality = rep(lengthdata$scen, times = 2),
  TargetMet = c(targetindicator, lengthdata$target_met),
  InitialExpected = rep(lengthdata$original, times = 2)
)

sizelimit <- ggplot(data = lengthdata2, aes(x=factor(Mortality, levels = level), 
                                               y=Landings, group = factor(TargetMet, levels = targettype),
                                               shape = factor(TargetMet, levels = targettype), 
                                               color = factor(TargetMet, levels = targettype),
                                               size = factor(TargetMet, levels = targettype))) + 
  geom_point() + scale_shape_manual(values = c(2,16,4)) + 
  scale_color_manual(values = c("blue","red","red")) + scale_size_manual(values = c(6,4,4)) +
  labs(color = "Target Catch Met", shape = "Target Catch Met", size = "Target Catch Met") +
  geom_line(aes(group = Mortality), linewidth = 0.5, col = "black") + 
  scale_y_continuous(limits = c(min(lengthdata2$Landings-0.5e6), max(lengthdata2$Landings+0.5e6))) + #this will need to be adjusted depending on the regulations used
  theme_linedraw() + 
  xlab("Harvest Control Rules - % Reduction/Liberalization of Catch") + 
  ylab("Expected Landings") + geom_hline(yintercept = seasondata2$InitialExpected, linetype=2) +
  # geom_text(x="-40%", y=1295208.6, label="Expected Catch at Starting Regulations", size=4, col = "black") +
  theme(legend.position = "none") + theme(plot.margin = unit(c(1,1,1,1), "lines")) 

#bag 

bagdata <- finaltable1_bind %>% 
  filter(reg_changed == 'Bag Change') 

bagdata2 <- tibble(
  Landings = c(bagdata$target, bagdata$land),
  Mortality = rep(bagdata$scen, times = 2),
  TargetMet = c(targetindicator, bagdata$target_met),
  InitialExpected = rep(bagdata$original, times = 2)
)

baglimit <- ggplot(data = bagdata2, aes(x=factor(Mortality, levels = level), 
                                               y=Landings, group = factor(TargetMet, levels = targettype),
                                               shape = factor(TargetMet, levels = targettype), 
                                               color = factor(TargetMet, levels = targettype),
                                               size = factor(TargetMet, levels = targettype))) + 
  geom_point() + scale_shape_manual(values = c(2,16,4)) + 
  scale_color_manual(values = c("blue","red","red")) + scale_size_manual(values = c(6,4,4)) +
  labs(color = "Target Catch Met", shape = "Target Catch Met", size = "Target Catch Met") +
  geom_line(aes(group = Mortality), linewidth = 0.5, col = "black") + 
  scale_y_continuous(limits = c(min(bagdata2$Landings-0.5e6), max(bagdata2$Landings+0.5e6))) + #this will need to be adjusted depending on the regulations used
  theme_linedraw() +   
  xlab("Harvest Control Rules - % Reduction/Liberalization of Catch") + 
  ylab("Expected Landings") + geom_hline(yintercept = seasondata2$InitialExpected, linetype=2) +
  # geom_text(x="-40%", y=1295208.6, label="Expected Catch at Starting Regulations", size=4, col = "black") +
  theme(legend.position = "none") + theme(plot.margin = unit(c(1,1,1,1), "lines")) 


#all
anydata <- finaltable1_bind %>% 
  filter(reg_changed == 'Any Regs') 

anydata2 <- tibble(
  Landings = c(anydata$target, anydata$land),
  Mortality = rep(anydata$scen, times = 2),
  TargetMet = c(targetindicator, anydata$target_met),
  InitialExpected = rep(anydata$original, times = 2)
)

flexible <- ggplot(data = anydata2, aes(x=factor(Mortality, levels = level),
                                        y=Landings, group = factor(TargetMet, levels = targettype),
                                        shape = factor(TargetMet, levels = targettype), 
                                        color = factor(TargetMet, levels = targettype),
                                        size = factor(TargetMet, levels = targettype))) + 
  geom_point() + scale_shape_manual(values = c(2,16,4)) + 
  scale_color_manual(values = c("blue","red","red")) + scale_size_manual(values = c(6,4,4)) +
  labs(color = "Target Catch Met", shape = "Target Catch Met", size = "Target Catch Met") +
  geom_line(aes(group = Mortality), linewidth = 0.5, col = "black") + 
  scale_y_continuous(limits = c(min(anydata2$Landings-0.5e6), max(anydata2$Landings+0.5e6))) + #this will need to be adjusted depending on the regulations used
  theme_linedraw() +  
  xlab("Harvest Control Rules - % Reduction/Liberalization of Catch") + 
  xlab("Harvest Control Rules - % Reduction/Liberalization of Catch") + 
  ylab("Expected Landings") + geom_hline(yintercept = seasondata2$InitialExpected, linetype=2) +
  # geom_text(x="-40%", y=1295208.6, label="Expected Catch at Starting Regulations", size=4, col = "black") +
  theme(legend.position = "none") + theme(plot.margin = unit(c(1,1,1,1), "lines")) 

#arrange into figure 
regplots <- ggarrange(seasonlength + rremove("xlab") + rremove("ylab"),
                      sizelimit + rremove("xlab") + rremove("ylab"), 
                      baglimit + rremove("xlab") + rremove("ylab"), 
                      flexible + rremove("xlab") + rremove("ylab"),
                      labels = c("Season Length Change Only", "Size Limit Change Only",
                                 "Bag Limit Change Only", "Flexible Regulation Changes Allowed"),
                      vjust = 1.0, 
                      hjust = -0.2,
                      font.label = list(size=9),
                      common.legend = TRUE, legend = "right")
regplots <- annotate_figure(regplots, top = text_grob("", size = 10), left = text_grob("Expected Landings", size = 12, rot = 90), bottom = text_grob("Harvest Control Rules (% Reduction/Liberalization)", size = 12))

#ggsave("~/Desktop/FlounderMSE/ManuscriptPrep/Draft5Figures_final2/regplots_liberal.png",regplots,width=9,height=6.5)
#ggsave("~/Desktop/FlounderMSE/ManuscriptPrep/Draft5Figures_final2/regplots_starting.png",regplots,width=9,height=6.5)
#ggsave("~/Desktop/FlounderMSE/ManuscriptPrep/Draft5Figures_final2/regplots_restrictive.png",regplots,width=9,height=6.5)

#test individual regs 
Bag = 4
MinLen = 17.5
SeasonLen = 150
SSB = 29182.86
state2 <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")
newtest <- expand.grid(state = unique(state2), SeasonLen = SeasonLen, 
                           Bag = Bag, MinLen = MinLen, SSB = SSB)
gamfit2 <- predict.gam(gamRDM2, newdata = newtest, type = "response" , se.fit = TRUE)
flukecatchtest <- newtest %>% mutate(land = gamfit2$fit)
sum(flukecatchtest$land)


