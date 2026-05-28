!###################################################
!#Include file for SESSF Operating model 'Sinatra'
!#
!#G.Fay 
!#University of Washington
!#2/27/2007
!#last update 13 June 2024
!#
!!###################################################

       INTEGER Nzone,Nstock,TopAge,Ymin,Ymax,Nfleet,NLenBin,Nsurvey
       REAL*8 RecDiscSurv

       PARAMETER (Nzone=2,Nstock=2,TopAge=10)
       PARAMETER (Ymin=1990,Ymax=2060,Nfleet=5,NLenBin=45)
       PARAMETER (Nsurvey=3)
       PARAMETER (RecDiscSurv=0.9)
!      Control Parameters

       INTEGER Diag                                                  ! show diagnostic junk files
       INTEGER NoCatch					             ! catch flag for debugging 1 sets catches to zero
       INTEGER Nreg						     ! number of regions
       INTEGER Nstk						! number of stocks
       REAL*8 LamdaZero(1:Nstock,1:Nzone)			! fraction of recruits by area for each stock in 1st yr
       INTEGER Nflt						! number of fleets
       INTEGER Nsurv          ! number of surveys
       INTEGER FleetRegions(1:Nfleet,1:Nzone)			! allocation of fleets to regions (matrix of 1's and 0's)
       INTEGER Nlen						! number of length bins
       REAL*8 LenBins(1:NLenBin+1)				! length bin boundaries (lower bounds, plus upper bound of top bin)
       INTEGER Fyear,Lyear					! first and last year for historical projection - yrs catches are available (#'s projected to Lyear+1)
       INTEGER ISEEDX,ISEEDZ					!Random number seeds
       INTEGER GrangerI						! temporary global integer
       REAL*8  GrangerV(1:500)				        ! temporary global vector
       REAL*8  GrangerM(1:500,1:500)				! temporary global matrix
       INTEGER OpScen(1000)                 !operating model scenario to use for each simulation

       COMMON / ControlP / Diag,NoCatch,Nreg,Nstk,LamdaZero,Nflt,Nsurv,FleetRegions,Nlen,LenBins,Fyear,Lyear,ISEEDX,ISEEDZ,GrangerI,GrangerM,GrangerV,OpScen



!      Biology

       INTEGER MaxAge

       REAL*8 M(1:Nstock,1:Nzone,1:2,0:TopAge,Ymin:Ymax)             ! Natural Mortality rate by stock,region,sex,age,time
       REAL*8 Mzero(1:Nstock,0:TopAge)				     ! base natural mortality for read-in

       INTEGER LAflag,SLAflag,WAflag

       REAL*8 MeanLenAge(1:Nstock,1:2,0:TopAge,Ymin:Ymax)            ! mean length at age by stock,sex,time
       REAL*8 SigmaLenAge(1:Nstock,1:2,0:TopAge,Ymin:Ymax)           ! s.d. of length at age by stock,sex,time
       REAL*8 LenStart(1:Nstock,1:2,0:TopAge,Ymin:Ymax)            ! mean length at age by stock,sex,time
       REAL*8 SigmaStart(1:Nstock,1:2,0:TopAge,Ymin:Ymax)           ! s.d. of length at age by stock,sex,time
 
       REAL*8 VBLinf(1:Nstock,1:2,Ymin:Ymax)                         ! Growth parameters
       REAL*8 VBK(1:Nstock,1:2,Ymin:Ymax)
       !REAL*8 
       double precision VBTzero(1:Nstock,1:2,Ymin:Ymax)

       !REAL*8 
       double precision CVLen0(1:Nstock,1:2)                                   ! CV in length at age 0
       !REAL*8 
       double precision CVLenX(1:Nstock,1:2)			             ! CV in length at max age

       !REAL*8 
       double precision Weight(1:Nstock,1:2,0:TopAge,Ymin:Ymax)                ! mean weight at age by stock,sex,time
       !REAL*8 
       double precision WeightMid(1:Nstock,1:2,0:TopAge,Ymin:Ymax)                ! mean weight at age by stock,sex,time (middle of yr)

       !REAL*8 
       double precision WtLen(1:NlenBin,1:Nstock,1:2)				     ! weight at length
       !REAL*8 
       double precision WtLenPars(1:2,1:Nstock,1:2)				     ! weight-length parameters


       !REAL*8 
       double precision Fecundity(1:Nstock,0:TopAge,Ymin:Ymax)                 ! fecundity at age by stock,time
       double precision Maturity(1:Nstock,1:NLenBin,Ymin:Ymax)
       double precision MaxFec(1:Nstock)

       INTEGER FecType

       !REAL*8 
       double precision X(1:Nstock,1:Nzone,1:Nzone,1:2,0:TopAge,Ymin:Ymax)     ! Movement Matrix by stock, source region, sink region, sex,age,time
       !REAL*8 
       double precision Relmove(1:Nstock,1:2,0:TopAge)               ! relative probability of movement by age
       !REAL*8 
       double precision Maxmovers(1:Nstock,1:2)                      ! maximum average prop of movers
       !REAL*8 
       double precision MoveConnect(1:Nstock,1:Nzone,1:Nzone,1:2)              ! connectivity matrix among regions by stock

       COMMON / Biol / MaxAge,M,MeanLenAge,SigmaLenAge,VBLinf,VBK,VBTzero,CVLen0,CVLenX,Weight,WeightMid,WtLen,WtLenPars,Fecundity,X,Mzero,LAflag,SLAflag,WAflag,Relmove,Maxmovers,MoveConnect,Maturity,LenStart,SigmaStart,FecType,MaxFec

!      Catch-related quantities

       !REAL*8 
       double precision Uage(1:Nstock,1:Nzone,1:2,0:TopAge,Ymin:Ymax)          ! exploitation rate by stock,region,sex,age,time
       !REAL*8 
       double precision Ufleet(1:Nfleet,1:Nzone,Ymin:Ymax)                     ! exploitation rate by fleet,region,time       
	 
       !REAL*8 
       double precision Catch(1:Nfleet,1:Nzone,Ymin:Ymax)                     ! Retained catch by fleet, region, time
       REAL*8 Discard(1:Nfleet,1:Nzone,Ymin:Ymax)                   ! Discarded catch by fleet, region, time
       REAL*8 TotalCatch(1:Nfleet,1:Nzone,Ymin:Ymax)                ! Total catch by fleet, region, time
       REAL*8 RetCatch(1:Nfleet,1:Nzone,Ymin:Ymax)                ! model-derived retained catch by fleet, region, time

       COMMON / Catch / Uage,Ufleet,Catch,Discard,TotalCatch,RetCatch

!      Selectivity

       !REAL*8 
       double precision SelAge(1:Nfleet,1:Nstock,1:2,0:TopAge,Ymin:Ymax)      ! selectivity at age by fleet,stock,sex,age,time
       !REAL*8 
       double precision SelLen(1:Nfleet,1:NLenBin,Ymin:Ymax)                  ! selectivity at length by fleet and time
       !REAL*8 
       double precision FracLen(1:NLenBin,1:Nstock,1:2,0:TopAge,Ymin:Ymax)    ! fraction of fish in length bins by stock,sex,age,time
       double precision FracLenStart(1:NLenBin,1:Nstock,1:2,0:TopAge,Ymin:Ymax)    ! fraction of fish in length bins by stock,sex,age,time

       !REAL*8 
       double precision loLenBin(1:NLenBin)                                   ! lower lengths of length bins
       !REAL*8 
       double precision hiLenBin(1:NLenBin)                                   ! upper lengths of length bins


       REAL*8 DevSel(1:Nfleet,1:NLenBin,Ymin:Ymax)      ! deviations in logit of selectivity by fleet and time (eq 1.8)
       REAL*8 VarDevSel(1:Nfleet)				  ! variance in random walk in logit of selectivity by fleet (eq 1.9)
       REAL*8 CorDevSel(1:Nfleet,1:NLenBin,1:NLenBin)             ! correlation matrix among length bins of annual deviations (eq 1.9)
       REAL*8 CorSel(1:Nfleet)				          ! degree of correlation among length bins  (Omega, eq 1.10)

       COMMON / Selex / SelAge,SelLen,FracLen,loLenBin,hiLenBin,DevSel,VarDevSel,CorDevSel,CorSel,FracLenStart

!	Retention

       REAL*8 RetAge(1:Nfleet,1:Nstock,1:2,0:TopAge,Ymin:Ymax)      ! fraction retained at age by fleet,stock,sex,age,time
       REAL*8 RetLen(1:Nfleet,1:NLenBin,Ymin:Ymax)                  ! fraction retained at length by fleet and time           
       REAL*8 RetGamma(1:Nfleet,1:3,Ymin:Ymax)                      ! retention parameters by fleet, time (eq 1.12)
       INTEGER RETflag(1:Nfleet+1)
       COMMON / Retent / RetAge,RetLen,RetGamma,RETflag

!       Recruitment
       !REAL*8 
       double precision h(1:Nstock)                                ! steepness by stock
       !REAL*8 
       double precision Recruits(1:Nstock,1:Nzone,Ymin:Ymax)       ! # of recruits by stock, area, and time
       INTEGER Rzeroflag				 ! flag for reading rzero - 1=read R0, 2=Read Target Depletion and solve for R0
       !REAL*8 
       double precision Rzero(1:Nstock)                            ! Rzero by stock
       REAL*8 TargDep(1:Nstock)				 ! Target depletion
       !REAL*8 
       double precision RecDevs(1:Nzone,Ymin:Ymax)                 ! recruitment deviations by region and time
       !REAL*8 
       double precision VarRecDevs(1:Nzone,1:Nzone,Ymin:Ymax)      ! Variance-covariance matrix of the recruitment devs
       !REAL*8 
       double precision SBioZero(1:Nstock)                         ! spawning biomass by stock at Bzero
       !REAL*8 
       double precision SigmaR(Ymin:Ymax)                          ! SigmaR at time t
       !REAL*8 
       double precision SigmaRVals(1:2)                            ! SigmaR values (eq 1.21)
       !REAL*8 
       double precision SigmaRProbs(1:2)                           ! probabilities of sigma R values (eq 1.21)
       !REAL*8 
       double precision CorRecDevs(1:Nzone,1:Nzone)                ! correlation matrix of recruitment residuals among regions
       !REAL*8 
       double precision FracRec(1:Nstock,1:Nzone,Ymin:Ymax)        ! expected fraction of age0's by stock assigned to regions through time (lamda, eq 1.22)
       INTEGER RecDevFlag(3)

       COMMON / Recruit / h,Recruits,Rzero,RecDevs,VarRecDevs,SBioZero,SigmaR,SigmaRVals,SigmaRProbs,CorRecDevs,FracRec,RecDevFlag,Rzeroflag,TargDep

!      Derived Quantities

!       REAL*8 Nprop(Istk,1:Nzone,1:2,0:TopAge)			 ! Age structure
       REAL*8 N(1:Nstock,1:Nzone,1:2,0:TopAge,Ymin:Ymax)         ! Numbers by stock,region,sex,age,time
       REAL*8 SpawBio(1:Nstock,0:Nzone,Ymin:Ymax)                ! spawning biomass by stock,region,time
       REAL*8 VBio(1:Nfleet,1:Nzone,Ymin:Ymax)                   ! vulnerable biomass by fleet,region,time
       REAL*8 RetVBio(1:Nfleet,1:Nzone,Ymin:Ymax)                ! retainable vulnerable biomass by fleet,region,time
       REAL*8 RetVBioSex(1:Nfleet,1:Nzone,1:2,Ymin:Ymax)                ! retainable vulnerable biomass by fleet,region,time
       REAL*8 FemSpawBio(1:Nstock,0:Nzone,Ymin:Ymax)                ! spawning biomass by stock,region,time

       COMMON / Derived / N,SpawBio,VBio,RetVBio,RetVBioSex,FemSpawBio

! 1.2 Data

!      Catches
       INTEGER UseHistData
       INTEGER CatchFlag(1:Nfleet)
       INTEGER CatYrs(1:Nfleet,Ymin:Ymax)
       REAL*8 CatCV(1:Nfleet)
       REAL*8 RetCatchDat(1:Nfleet,1:Nzone,Ymin:Ymax)

       COMMON / CatchData / CatchFlag,CatYrs,CatCV,RetCatchDat,UseHistData

! 1.2.1 Indices of abundance
! 1.2.1.1 CPUE

	INTEGER CpueFlag(1:Nfleet)				! number of years for CPUE data (count back from Lyear-1, negative number means all years)
	INTEGER CpueYrs(1:Nfleet,1:Nzone,Ymin:Ymax)		! years for cpue data
	REAL*8 CpueCV(1:Nfleet)					! CV of cpue data
	REAL*8 CpuePow(1:Nfleet)				! power parameter for CPUE
	REAL*8 CpueQmu(1:Nfleet)				! CPUE average catchability
	REAL*8 CpueQ(1:Nfleet,1:Nzone,Ymin:Ymax)		! CPUE catchability
	REAL*8 CpueVar(1:Nfleet)				! variance in random walk of CPUE
	REAL*8 CpueCorr(1:Nfleet)				! temporal correlation of rndm walk in CPUE
	REAL*8 Cpue(1:Nfleet,1:Nzone,Ymin:Ymax)			! CPUE data
	REAL*8 CpueQdevs(1:Nfleet,1:Nzone,Ymin:Ymax)		! deviations in Q

	COMMON / CpueData / Cpue,CpueFlag,CpueCV,CpuePow,CpueQmu,CpueQ,CpueVar,CpueCorr,CpueYrs,CpueQdevs

! 1.2.1.2 Surveys

  INTEGER SurveyFlag(1:Nsurvey)
  INTEGER SurveyReg(1:Nsurvey)
  INTEGER SurveyYrs(1:Nsurvey,Ymin:Ymax)
  REAL*8 SurveyTime(1:Nsurvey)
  REAL*8 SurveyCV(1:Nsurvey)
  REAL*8 SurveyQmu(1:Nsurvey)
  REAL*8 SurveyQ(1:Nsurvey,Ymin:Ymax)
  REAL*8 SurveyB(1:Nsurvey,Ymin:Ymax)
  REAL*8 SurveyQdevs(1:Nsurvey,Ymin:Ymax)
  REAL*8 SurveySel(1:Nsurvey,1:NlenBin)
  REAL*8 SurveyVBio(1:Nsurvey,Ymin:Ymax)
  REAL*8 ZatAge(1:Nstock,1:Nzone,1:2,0:TopAge,Ymin:Ymax)
  INTEGER SurveyLengthN(1:Nsurvey)
  INTEGER SurveyAgeN(1:Nsurvey)
  REAL*8 BMSY

  COMMON / SurveyData / SurveyFlag,SurveyReg,SurveyYrs,SurveyTime,SurveyCV,SurveyQmu,SurveyQ,SurveyB,SurveyQdevs,SurveySel,SurveyVBio,ZatAge,SurveyLengthN,SurveyAgeN,BMSY
  
! 1.2.2 Size composition

	INTEGER LenflagF(1:3)					! flags for years of length data
	INTEGER LengthN(1:3) !,1:Nfleet)					! annual sample size for length comps (retained, whole, discards)
	INTEGER LenYrs(1:3,Ymin:Ymax)				! years for length comps (retained, whole, discards)

	COMMON / LenData / LenflagF,LengthN,LenYrs

! 1.2.3 Age composition

	INTEGER DoCALK
	INTEGER AgeflagF(1:3)					! flags for years of age data
	INTEGER AgeN(1:3)					! annual sample size for age comps (retained, whole, discards)
	INTEGER AgeYrs(1:3,Ymin:Ymax)				! years for age comps (retained, whole, discards)
  REAL*8 AgeErrMat(0:TopAge,0:TopAge)

	COMMON / AgeData / AgeflagF,AgeN,AgeYrs,DoCALK,AgeErrMat

! 1.2.X Discard biomass / fraction

	INTEGER Discflag(1:Nfleet+1)
	INTEGER DiscYrs(1:Nfleet,Ymin:Ymax)
	REAL*8 DiscCV(1:Nfleet)
	REAL*8 DiscDat(1:Nfleet,Ymin:Ymax)

	COMMON / DiscData / Discflag,DiscYrs,DiscDat,DiscCV

! 1.2.5. Tagging Data

        INTEGER TagRelYrs(1:Nfleet,Ymin:Ymax)
	INTEGER TagRelFlag(1:2,1:Nfleet)
        INTEGER NTagGroups
	INTEGER NTagRelease(1:Nfleet,0:Nzone,Ymin:Ymax)
	INTEGER TagGroupYr(1:20000)
	INTEGER TagGroupReg(1:20000)
        INTEGER TagRelAge(1:Nfleet)
        REAL*8 Tagged(1:20000,1:Nstock,1:Nzone,1:2,0:TopAge,1994:Ymax)
        !REAL*8 Tagged(1:20000,1:Nstock,1:Nzone,1:2,1:2,1994:Ymax)
        REAL*8 muTagDR(1:Nfleet)
        REAL*8 CVTagDR(1:Nfleet)
        REAL*8 muTagAlpha
        REAL*8 CVTagAlpha
	REAL*8 DetectRate(1:Nfleet,Ymin:Ymax)
	REAL*8 TagAlpha(1:20000,Ymin:Ymax)
        REAL*8 mulenTagged(1:20000)
	REAL*8 GuessTaggedAge(1:20000)
        INTEGER GroupReleases(1:20000)
        REAL*8 EstTagID(1:20000,1:3)
	INTEGER NGuessGroups
	INTEGER GuessRegion(1:20000)
	INTEGER EstTagAge(1:Nfleet)

        COMMON / TagData / TagRelYrs,TagRelFlag,NTagGroups,NTagRelease,Tagged,muTagDR,CVTagDR,DetectRate,muTagAlpha,CVTagAlpha,TagAlpha,TagGroupYr,TagGroupReg,TagRelAge,mulenTagged,GuessTaggedAge,EstTagID,NGuessGroups,GroupReleases,GuessRegion,EstTagAge


! TIER RULE SPECIFICATIONS

	INTEGER Tier(3)						! (1). Tier rule used  (2). method (tier 3)  (3).option for method 2 of tier 3 
	REAL*8 RBC						! RBC used in projection
	REAL*8 EstRBC(2)					! RBC from harvest strategy
	REAL*8 EstDep
	REAL*8 EstQuant(1:100)
	REAL*8 AllocPars(1:3,1:Nfleet*Nzone)			! parameters for RBC allocation by fleet and region
	REAL*8 SigmaAlloc				        ! Overdispersion in RBC allocation
	INTEGER ActiveF(1:Nfleet),NactiveF
	INTEGER HCRspecs(1:4)
	INTEGER Nprojyrs
	REAL*8 Forecat
	REAL*8 Discountfactor
	INTEGER DiscountStableYrs
	REAL*8 DiscountStableCV
	REAL*8 MaxCatch(1:Nfleet)
	REAL*8 Cref,Cstar
	REAL*8 TAC
	REAL*8 Tier3R(10)
	INTEGER Tier3I(10)
	REAL*8 MaxChange
  REAL*8 LastDev(6)

	REAL*8 PseudoRefSpecs(4)

	COMMON / TierSpecs / Tier,RBC,AllocPars,ActiveF,NactiveF,HCRspecs,EstRBC,Nprojyrs,Forecat,MaxCatch,EstDep,EstQuant,Cref,Cstar,Discountfactor,Pseudorefspecs,TAC,DiscountStableYrs,DiscountStableCV,Tier3I,Tier3R,MaxChange,LastDev,SigmaAlloc

! MASTER CONTROL PARAMETERS

	INTEGER Nproj,Nsim
	INTEGER UseMCMC
	INTEGER DUMcol
	INTEGER NOMpars(5)
	REAL*8 OpModPars(1000)
	REAL*8 OMAgeL1
	INTEGER OMNsex
	INTEGER OMrecFY,OMrecLY
	INTEGER OMgrowth(1:16)
	INTEGER OMseltype(1:Nfleet)
	INTEGER OMNselpars(1:Nfleet)
	INTEGER OMdoreten(1:Nfleet)
	INTEGER OMsteep

	COMMON / Master / Nproj,UseMCMC,NOMpars,OpModPars,Nsim,OMAgeL1,OMrecFY,OMrecLY,OMgrowth,OMseltype,OMNselpars,OMdoreten,OMNsex,DUMcol,OMsteep

! ICES FMSY CONTOL PARS

  INTEGER Ftype
  REAL*8 Fref
  REAL*8 Bref(1:2)
  REAL*8 Mbase
  REAL*8 Ftarg
  REAL*8 MuseR

  COMMON / ICES / Ftype,MuseR,Fref,Bref,Mbase,Ftarg

! FLuke CONTROL PARS

  double precision CurrRegs(1:3)
  REAL*8 RHLfrac
  REAL*8 OMrefsel(1:2,0:TopAge)
  REAL*8 OMrefpts(1:4), TrueF(Ymin:Ymax)
  INTEGER CurrBin,RegChange

  COMMON / FLUKE / CurrRegs,RHLfrac,OMrefpts,OMrefsel,CurrBin,RegChange,TrueF


