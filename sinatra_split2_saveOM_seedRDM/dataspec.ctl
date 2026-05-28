###################################################
#Example specification data specification 'control' file for SESSF Operating model 'Sinatra'
#G.Fay 
#University of Washington
#6/13/2007
###################################################
#use historical data?
0
#Types of historical data available
#Fishery dependent (by fleet) - number is number of years (always count back), 0 indicates none available, neg. integer indicates avail for all years
#Catch
0 0 0 0 0 0 0
#Catch CV
0 0 0 0 0 0 0
#CPUE
16 0 0 24 0 24 24
#CPUE CV
0.426	0 0 0.214734783	0 0.174469565	0.369934783
#CPUE average catchability
4.41576E-05	0	0	7.44838E-05	0	9.48948E-05	0.000702356
#CPUE power parameter
1 1 1 1 1 1 1
#CPUE variance of random walk in catchability
0 0 0 0 0 0 0
#CPUE temporal correlation in catchability random walk 
1 1 1 1 1 1 1
#Discard data
#type (1=biomass, 2= fraction, then years by fleet)
2 0 0 0 12 0 18 10
#discard CV
0.01 0 0 0.27 0 0.75 0.75
#Length comps (2nd # is annual sample size)
#retained catch
20 70 #20 500 #5000	#60 500 #60 500 # 50 2000 #
#whole catch
0 0
#discards
12 50 #5000 #0 200 #
##
#Age comps (2nd # is annual sample size)
#do alk?
1
#retained catch
12 55 #10 500 #5000 #8 200 #-1 5000 ##
#whole catch
0 0
#discards
0 0
##
#Tagging Data
#tagging releases by fleet
#number of years
0 0 0 0 0 0 0
#what kind, 0=read-in time series, 1=annual numbers, 2= tag rate, 
0 0 0 0 0 0 0
#either annual numbers (for 1), or the tag rate
0 0 3 3 3 3 3
#Annual fleet specific vals (for read-in, these are 1:nyr for fleet 1, then fleet 2, etc)
0	0	0	0	0	0	0	0	0	0	0	0	0	0	300	484
0	0	0	0	0	0	0	0	0	0	0	0	26	82	60	0
0	0	0	0	0	0	0	0	0	0	0	0	190	386	396	509
0	0	0	0	0	0	0	0	0	0	0	0	190	386	396	509
0	0	0	0	0	0	0	0	0	0	0	0	190	386	396	509
0	0	0	0	0	0	0	0	0	0	0	0	190	386	396	509
0	0	0	0	0	0	0	0	0	0	0	0	190	386	396	509
#age to allocate releases by fleet (-1 = as distribution in catch)
-1 -1 -1 -1 -1 -1 -1
#mean detection rate by fleet
0.999 0.999 0.999 0.999 0.999 0.999 0.999
#CV in detection rate by fleet
0 0 0 0 0 0 0
#mean overdispersion parameter (>=1, 1=Poisson)
1
#CV annual overdispersions
0
#future tag releases
#what kind, 0=none, 1=annual numbers, 2= tag rate, 
0 0 0 0 0 0 0
#either annual numbers (for 1), or the tag rate
0 0 0 0 0 0 0
#Fishery independent (survey)
#number of surveys
1
#allocation of surveys to regions
1 1 1 
#type of survey (1: spawning, 0:otherwise)
0
#timing of survey (fraction of year - 0 = start, 1=end
0.5
#number of years for survey (as for above)
5
#survey CV
0
#survey Q
1
#survey selectivity at length
6.51E-06	1.02E-05	1.61E-05	2.53E-05	3.99E-05	6.27E-05	9.86E-05	0.000155087	0.000243908	0.00038358	0.000603186	0.000948399	0.00149089	0.00234296	0.00368021	0.00577628	0.00905531	0.0141692	0.0221068	0.0343361	0.052964	0.0808519	0.121539	0.178721	0.254996	0.349953	0.458508	0.571151	0.676874	0.767159	0.838246	0.890721	0.927642	0.952751	0.969434	0.980348	0.987415	0.991962	0.994875	0.996735	0.997922	0.998678	0.999159	0.999465	0.99966	0.999784	0.999862	0.999913	0.999944	0.999965	0.999978	0.999986	0.999991	0.999994
#survey lengths sample size
200
#survey ages sample size
200
#Ageing error
1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1

