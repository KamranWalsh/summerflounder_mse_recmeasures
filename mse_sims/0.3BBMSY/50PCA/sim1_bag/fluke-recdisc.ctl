#####Pseudo-assessment reference point determination
# CV of simulated OFL 
0.6
# CV of risk policy OFL
0.6
# autocorrelation in OFL estimate
0.707
# CV of Fref estimate
0.2
# F type  (1=FSPR, 2=Z, 5=ICES FMSY, with MSYBtrigger at 0.5 BMSY)
1
# M to use (positive is fixed value, negative integer indicates number of years to obtain average, -99 is entire time series)
0.25   #-1  #0.58
# F reference  (FMSY)
0.35
# Biomass refs
100 0
# Mbase (for Z type hcr)
-1
# Recreational sector allocation
0.45
# Mgmt type (-1 = fixed regs, 1 = RHL, 2= F-based, 3 = RHL_risk, 4= F_risk, 6 = percent_change_RHL, 7 = percent_change_ACL [not coded], 8 = bioref_RHL, 9 = bioref_ACL [not coded], 10 = biomatrix_RHL, 11= nochange, 12 = simplified_percent_change_RHL, 13-19 = biomatrix_RHL alternatives, 20-26 = bioref_RHL alternatives
6
# Implementation type (0=scalar, 1=season length, 2=gam-basedHCR, 3 = Lou's model, -1 = old R model)
3 
# Initial year regulations (season length, bag limit, minimum size)
150 4 17.5
# Regulation to change  (1 = bag limit, 2 = minimum size, 3= season length, 4 = all, 5 = bag and length, 6 = bag and season, 7 = length and season)
1
#Initial Bin (for Mgmt types 6+)
7
