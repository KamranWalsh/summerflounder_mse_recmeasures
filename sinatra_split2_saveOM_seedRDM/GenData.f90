!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	File contains subroutines for sinatra associated with generating data
!	
!	GAVIN FAY
!	last modified on:
!	02/21/2022
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine deals with generating the historical data
    SUBROUTINE FindHistData

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Iyr,Ilen,Iflt

    WRITE(99,*) 'ISEEDX',ISEEDX
    WRITE(99,*) 'ISEEDZ',ISEEDZ

	IF (Diag.EQ.1) WRITE(99,*) 'finding historical data'

!	obtain the data specifications
	IF (Diag.EQ.1) WRITE(99,*) 'reading data specs: check histdata.junk'
	CALL ReadDataSpecs()
	IF (Diag.EQ.1) WRITE(99,*) 'finished reading data specs'

	CALL GetTierSpecs()

!	open the data output files, and write the headers
	OPEN(UNIT=31,FILE='FisheryAges.inp')   ! POSITION='APPEND'
	WRITE(31,'(A20,200(I4,1x))') 'Year Ft Rg S T Nsamp',(Age,Age=0,MaxAge) 
	OPEN(UNIT=32,FILE='FisheryLengths.inp')   ! POSITION='APPEND'
	WRITE(32,'(A21,1x,200(F6.2,1x))') 'Year Ft Rg S T  Nsamp',(lolenbin(Ilen),Ilen=1,Nlen) 
	OPEN(UNIT=33,FILE='CPUE.inp')   ! POSITION='APPEND'
	WRITE(33,'(A15)') 'Year Ft then Rg'
	OPEN(UNIT=34,FILE='Discard.inp')
	WRITE(34,'(A12)') 'Year Ft Rg D'
    OPEN(UNIT=35,FILE='TagRecaptures.inp')
    WRITE(35,'(A12)') 'Year TG Ft Rg Rec'
    OPEN(UNIT=36,FILE='CAFAges.inp')
    WRITE(36,'(A27)') 'Year Ft Rg  S  T Age Length'
	OPEN(UNIT=37,FILE='TagReleases.inp')
    WRITE(37,'(A12,200(I4,1x))') 'TG Year Ft Rg Num',(Age,Age=0,MaxAge)
    OPEN(UNIT=38,FILE='Catches.inp')
	WRITE(38,'(A12)') 'Year Ft Rg C'
    OPEN(UNIT=39,FILE='estB.inp')
    WRITE(39,'(A14)') 'pseudo ref pts' 

	OPEN(UNIT=40,FILE='SurveyIndex.inp')   
	WRITE(40,'(A15)') 'Year Sv'
	OPEN(UNIT=41,FILE='SurveyAges.inp')
	WRITE(41,'(A20,200(I4,1x))') 'Year Sv Sex Nsamp',(Age,Age=0,MaxAge) 
	OPEN(UNIT=42,FILE='SurveyLengths.inp')
	WRITE(42,'(A20,1x,200(F6.2,1x))') 'Year Sv Sex Nsamp',(lolenbin(Ilen),Ilen=1,Nlen) 

	    
!	close the data output files
	CLOSE(31)		
	CLOSE(32)
	CLOSE(33)
	CLOSE(34)
	CLOSE(35)
    CLOSE(36)
	CLOSE(37)
	CLOSE(38)
	CLOSE(39)
    CLOSE(40)
    CLOSE(41)
    CLOSE(42)


!	make sure the number of tag groups is zero
	NTagGroups = 0
	Tagged = 0.d0
	NGuessGroups = 0
	GroupReleases = 0
	GuessTaggedAge = 0.d0
     !set the values for detection rate
	CALL GetTagDetectRate(Fyear,Lyear)


	

!	generate data for historical period
	IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'generating historical data'
     WRITE(98,*) 'check .inp files'
	ENDIF
		
	IF (UseHistData.EQ.1) CALL CopyHistData()

 	 DO 8000 Iyr=Fyear,Lyear
	  CALL GenData(Iyr,1)
	  IF (Diag.EQ.1) WRITE(98,*) 'Done ',Iyr
8000 CONTINUE
 	 DO 8001 Iyr=Fyear,Lyear
	  CALL GenData(Iyr,2)
	  IF (Diag.EQ.1) WRITE(98,*) 'Done ',Iyr
8001 CONTINUE
	IF (Diag.EQ.1) WRITE(98,*) 'finished generating historical data'
	IF (Diag.EQ.1) WRITE(99,*) 'finished generating historical data'

	IF (NTagGroups.GT.0.AND.EstTagAge(1).EQ.-101) CALL ReDoTagAges()

!	write the assessment data files necessary
	CALL WriteAssessData(Lyear)	
	IF (Diag.EQ.1) WRITE(98,*) 'finished writing assessment files'
	IF (Diag.EQ.1) WRITE(99,*) 'finished writing assessment files'


	CLOSE(98)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE ReDoTagAges

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER TG,Iyr,Iflt,Ireg,Ilen,GG,Age,IGG,Flag,IG,NT,II,JAge,Jflt,Jyr,Jreg,Jlen,DUM
	INTEGER TotGroups,NumGroups(Fyear:Lyear,1:Nfleet,1:Nreg,0:TopAge)
	REAL*8 GA
	CHARACTER*2000 STRING1

	NumGroups=0
	IGG=0
	OPEN(UNIT=37,FILE='TagReleases.Inp')
	READ(37,*)
9990 READ(37,*,ERR=9991,END=9991) TG,Iyr,Iflt,Ireg,Ilen,NT,GG,GA,Age
     IF (NumGroups(Iyr,Iflt,Ireg,Age).GT.0) THEN
	  EstTagID(TG,1) = NumGroups(Iyr,Iflt,Ireg,Age)
	 ELSE
	  IGG= IGG+1
      EstTagID(TG,1) = FLOAT(IGG)
	  NumGroups(Iyr,Iflt,Ireg,Age) = IGG
	 ENDIF
     GOTO 9990
9991 CONTINUE
	CLOSE(37)
    NGuessGroups = IGG

	OPEN(UNIT=37,FILE='TagReleases.Inp')
	OPEN(UNIT=41,FILE='NewTagReleases.Inp')
	READ(37,'(A2000)') STRING1
    WRITE(41,'(A2000)') STRING1
	DO IGG=1,NTagGroups  	
     READ(37,*) TG,Iyr,Iflt,Ireg,Ilen,NT,DUM,DUM,Age
	 WRITE(41,'(I5,1x,I4,1x,I2,1x,I2,1x,I3,1x,I6,1x,F4.0,1x,F7.3,1x,I3,1x,20000(F6.0,1x))') TG,Iyr,Iflt,Ireg,Ilen,NT,(EstTagID(TG,II),II=1,2),Age  !,(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,JAge,Iyr)),JAge=0,MaxAge)
    ENDDO
	CLOSE(37)
	CLOSE(41)

    OPEN(UNIT=35,FILE='TagRecaptures.Inp')
	OPEN(UNIT=41,FILE='NewTagRecaptures.Inp')
	READ(35,'(A2000)') STRING1
	WRITE(41,'(A2000)') STRING1
9992 READ(35,*,END=9993,ERR=9993) Iyr,II,Iflt,Ireg,NT,DUM
    WRITE(41,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)') Iyr,II,Iflt,Ireg,NT,INT(EstTagID(II,1))
    GOTO 9992
9993 CONTINUE
     CLOSE(35)
	 CLOSE(41)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine copies the historical data into the .inp files
    SUBROUTINE CopyHistData

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	CHARACTER*5000 STRING1

	OPEN(UNIT=13,FILE='HistFisheryAges.inp')
	OPEN(UNIT=18,FILE='FisheryAges.inp',POSITION='APPEND')
	READ(13,*)
8901 READ(13,'(A5000)',ERR=8902,END=8902) STRING1
	 WRITE(18,'(A5000)') STRING1
	 GOTO 8901
8902 CONTINUE
	 CLOSE(13)
	 CLOSE(18)

	OPEN(UNIT=13,FILE='HistFisheryLengths.inp')
	OPEN(UNIT=18,FILE='FisheryLengths.inp',POSITION='APPEND')
	READ(13,*)
8903 READ(13,'(A5000)',ERR=8904,END=8904) STRING1
	 WRITE(18,'(A5000)') STRING1
	 GOTO 8903
8904 CONTINUE
	 CLOSE(13)
	 CLOSE(18)

	OPEN(UNIT=13,FILE='HistCPUE.inp')
	OPEN(UNIT=18,FILE='CPUE.inp',POSITION='APPEND')
	READ(13,*)
8905 READ(13,'(A5000)',ERR=8906,END=8906) STRING1
	 WRITE(18,'(A5000)') STRING1
	 GOTO 8905
8906 CONTINUE
	 CLOSE(13)
	 CLOSE(18)

	OPEN(UNIT=13,FILE='HistDiscard.inp')
	OPEN(UNIT=18,FILE='Discard.inp',POSITION='APPEND')
	READ(13,*)
8907 READ(13,'(A5000)',ERR=8908,END=8908) STRING1
	 WRITE(18,'(A5000)') STRING1
	 GOTO 8907
8908 CONTINUE
	 CLOSE(13)
	 CLOSE(18)


	OPEN(UNIT=13,FILE='HistCatches.inp')
	OPEN(UNIT=18,FILE='Catches.inp',POSITION='APPEND')
	READ(13,*)
8909 READ(13,'(A5000)',ERR=8910,END=8910) STRING1
	 WRITE(18,'(A5000)') STRING1
	 GOTO 8909
8910 CONTINUE
	 CLOSE(13)
	 CLOSE(18)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the assessment details, for writing the data files
    SUBROUTINE GetTierSpecs

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER ToSkip,II

	OPEN(UNIT=14,FILE='hcr.ctl')

	Toskip = 13
	DO II=1,Toskip
	 READ(14,*)
	ENDDO
 	READ(14,*) Tier(1)

	CLOSE(14)

    IF (Tier(1).EQ.8) Tier(1) = 1

	IF (Tier(1).EQ.3) THEN
	 OPEN(UNIT=14,FILE='tier3.ctl')
	 Toskip = 5
	 DO II=1,Toskip
	  READ(14,*)
	 ENDDO
 	 READ(14,*) Tier(2)
	 CLOSE(14)
	ENDIF


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine deals with generating the data for the projection period
    SUBROUTINE FindNewData(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Iyr,Yr1,Yr2

	IF (Diag.EQ.1) WRITE(99,*) 'getting projection data'

!	obtain the data specifications
	IF (Diag.EQ.1) WRITE(99,*) 'reading data specs: check histdata.junk'
	CALL ReadNewDataSpecs(Yr1,Yr2)
	IF (Diag.EQ.1) WRITE(99,*) 'finished reading data specs'

!	read in tier specifications
	CALL GetTierSpecs()

    !set the values for detection rate
    CALL GetTagDetectRate(Yr1,Yr2)

!	generate data for projection period
	IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'generating future data'
     WRITE(98,*) 'check .inp files'
	ENDIF
 	 DO 8500 Iyr=Yr1,Yr2
	  CALL GenData(Iyr,1)
	  IF (Diag.EQ.1) WRITE(98,*) 'Done ',Iyr
8500 CONTINUE
 	 DO 8501 Iyr=Yr1,Yr2
	  CALL GenData(Iyr,2)
	  IF (Diag.EQ.1) WRITE(98,*) 'Done ',Iyr
8501 CONTINUE
	IF (Diag.EQ.1) WRITE(98,'(A30,I4,1x,A2,I4)') 'finished generating data from ',Yr1,'to',Yr2
	IF (Diag.EQ.1) WRITE(99,'(A30,I4,1x,A2,I4)') 'finished generating data from ',Yr1,'to',Yr2

	IF (NTagGroups.GT.0.AND.EstTagAge(1).EQ.-101) CALL ReDoTagAges()

!	write the assessment data files necessary
	CALL WriteAssessData(Yr2)	
	IF (Diag.EQ.1) WRITE(98,*) 'finished writing assessment files'
	IF (Diag.EQ.1) WRITE(99,*) 'finished writing assessment files'

	CLOSE(98)

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the tag detection rate
	SUBROUTINE GetTagDetectRate(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Iyr,Yr1,Yr2,ISEED
	REAL*8 TDRtemp,XNORM,Posbit

	EXTERNAL XNORM

	Posbit = 0.000000000000001

    ISEED = ISEEDZ

	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Tag Detection Rate'
	IF (Diag.EQ.1) WRITE(98,'(A3,100(I5,1x))') 'Ft ',(Iyr,Iyr=Yr1,Yr2)
    DO 8520 Iflt=1,Nflt
	 DetectRate(Iflt,Yr1:Yr2) = muTagDR(Iflt)
	 IF (CVTagDR(Iflt).GT.0d0) THEN	 
	  DO 8521 Iyr=Yr1,Yr2 
	   TDRtemp = LOG((Posbit+muTagDR(Iflt))/(1+Posbit-(muTagDR(Iflt)))) + XNORM(5,0.d0,CVTagDR(Iflt),ISEED)
	   DetectRate(Iflt,Iyr) = EXP(TDRtemp)/(1+EXP(TDRtemp))
8521  CONTINUE
     ENDIF
     IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(F5.3,1x))') Iflt,(DetectRate(Iflt,Iyr),Iyr=Yr1,Yr2)
8520 CONTINUE	
	 	   
	RETURN
	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the degree of tag overdispersion
	SUBROUTINE GetTagAlpha(II,Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Iyr,ISEED
	REAL*8 TAtemp,XNORM

	EXTERNAL XNORM

    ISEED = ISEEDZ

	IF (Diag.EQ.1) WRITE(98,'(A30)') 'Tag recapture overdispersion'
	IF (Diag.EQ.1) WRITE(98,'(A3,I4)') 'TG ',Iyr
    TagAlpha(II,Iyr) = muTagAlpha
	IF (CVTagAlpha.GT.0) THEN
     TAtemp = XNORM(5,0.d0,LOG(1+CVTagAlpha),ISEED)
	 TagAlpha(II,Iyr) = 1.d0+(muTagAlpha-1)*EXP(TAtemp-(0.5d0*LOG(1+CVTagAlpha)**2.d0))
	ENDIF
    IF (UseMCMC.EQ.1) TagAlpha(II,Iyr) = OpModPars(NOMpars(1)-(Nreg-EstTagID(II,3)))
	!TagAlpha(II,Iyr) = 1.d0
    IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(F7.3,1x))') II,TagAlpha(II,Iyr)

	RETURN
	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.2.1    ABUNDANCE INDICES
!	1.2.1.1. CPUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the fishery CPUE data
!	Equations 1.32,1,34
!

	SUBROUTINE GetFishCPUEData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,Ireg,ISEED
	REAL*8 NewDev,XNORM

	EXTERNAL XNORM

	ISEED = ISEEDZ

	!first get the Q deviations for this year
	CALL GetQdevs(Iyr)

	!Q's for this year
	!equation 1.34
	IF (Diag.EQ.1) WRITE(98,*) 'Q by fleet and region for ',Iyr
	 DO 8620 Iflt=1,Nflt
 	  DO 8620 Ireg=1,Nreg
	   CpueQ(Iflt,Ireg,Iyr) = CpueQmu(Iflt)*EXP(CpueQdevs(Iflt,Ireg,Iyr)-0.5d0*CpueVar(Iflt)**2.d0)
	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,20(F10.8,1x))') Iyr,Iflt,Ireg,CpueQ(Iflt,Ireg,Iyr)
8620 CONTINUE

	!open write file for appending
	OPEN(UNIT=33,FILE='CPUE.inp',POSITION='APPEND')

	!CPUE data
	!Eq 1.32
	IF (Diag.EQ.1) WRITE(98,*) 'CPUE by fleet and region for ',Iyr
	 DO 8621 Iflt=1,Nflt
 	  DO 8621 Ireg=1,Nreg
	   Cpue(Iflt,Ireg,Iyr) = -99
	   IF (CpueYrs(Iflt,Ireg,Iyr).EQ.1.AND.FleetRegions(Iflt,Ireg).EQ.1) THEN
	    NewDev = XNORM(5,0.d0,CpueCV(Iflt),ISEEDX)
	    Cpue(Iflt,Ireg,Iyr) = CpueQ(Iflt,Ireg,Iyr)*(RetVBio(Iflt,Ireg,Iyr)**CpuePow(Iflt))*EXP(NewDev-0.5*(CpueCV(Iflt)**2.d0))
	    !Cpue(Iflt,Ireg,Iyr) = CpueQ(Iflt,Ireg,Iyr)*(RetVBio(Iflt,Ireg,Iyr)**CpuePow(Iflt))*EXP(NewDev)  !-0.5*(CpueCV(Iflt)**2.d0))
 	    IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,F10.2,1x,F7.4,1x,F7.4)') Iyr,Iflt,Ireg,RetVBio(IFlt,Ireg,Iyr),Cpue(Iflt,Ireg,Iyr),NewDev
		!WRITE(*,*) Cpue(Iflt,Ireg,Iyr)
		IF (HCRspecs(3).EQ.4.OR.HCRspecs(3).EQ.3) WRITE(33,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,Ireg,Iflt,Cpue(Iflt,Ireg,Iyr),CpueCV(Iflt)
!	    following line for SS2 output
		IF (HCRspecs(3).EQ.1.OR.HCRspecs(3).EQ.8) WRITE(33,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,1,Iflt,Cpue(Iflt,Ireg,Iyr),CpueCV(Iflt)

	   ENDIF
8621 CONTINUE

	CLOSE(33)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.2.1    Survey INDICES
!	09/04/17
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the survey index data
!
!

	SUBROUTINE GetSurveyIndexData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,Ireg,ISEED
	REAL*8 NewDev,XNORM

	EXTERNAL XNORM

	ISEED = ISEEDZ

	!first get the Q deviations for this year
	!CALL GetQdevs(Iyr)

	!Q's for this year
	!equation 1.34
	IF (Diag.EQ.1) WRITE(98,*) 'Q by survey for ',Iyr
	 DO 9620 Iflt=1,Nsurv
 	  !DO 8620 Ireg=1,Nreg
	   SurveyQ(Iflt,Iyr) = SurveyQmu(Iflt) !*EXP(CpueQdevs(Iflt,Ireg,Iyr)-0.5d0*CpueVar(Iflt)**2.d0)
	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,20(F10.8,1x))') Iyr,Iflt,SurveyQ(Iflt,Iyr)
9620 CONTINUE

	!open write file for appending
	OPEN(UNIT=40,FILE='SurveyIndex.inp',POSITION='APPEND')

	!Survey index data
	!
	IF (Diag.EQ.1) WRITE(98,*) 'Survey index for ',Iyr
	 DO 9621 Iflt=1,Nsurv
 	  DO 9621 Ireg=1,Nreg
	   SurveyB(Iflt,Iyr) = -99
	   IF (SurveyYrs(Iflt,Iyr).EQ.1.AND.SurveyReg(Iflt).EQ.Ireg) THEN
	    CALL GetSurveyVBio(Iflt,Iyr)
	    NewDev = XNORM(5,0.d0,CpueCV(Iflt),ISEEDX)
	    SurveyB(Iflt,Iyr) = SurveyQ(Iflt,Iyr)*SurveyVBio(Iflt,Iyr)*EXP(NewDev-0.5*(SurveyCV(Iflt)**2.d0))
 	    IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,F10.2,1x,F7.4,1x,F7.4)') Iyr,Iflt,Ireg,SurveyVBio(IFlt,Iyr),SurveyB(Iflt,Iyr),NewDev
		!WRITE(*,*) SurveyB(Iflt,Iyr)
		IF (HCRspecs(3).EQ.4.OR.HCRspecs(3).EQ.3) WRITE(40,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,Ireg,Iflt,SurveyB(Iflt,Iyr),SurveyCV(Iflt)
!	    following line for SS2 output
		IF (HCRspecs(3).EQ.1.OR.HCRspecs(3).EQ.8) WRITE(40,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,Ireg,Iflt,SurveyB(Iflt,Iyr),SurveyCV(Iflt)

	   ENDIF
9621 CONTINUE

	CLOSE(40)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine calculates the survey vulnerable biomass for a given year
!	09/04/17
	SUBROUTINE GetSurveyVBio(Iflt,Year)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Istk,Ireg,Year,Sex,Iflt,Ilen
	REAL*8 Temp


	SurveyVBio(Iflt,Year)=0.d0
    Ireg = SurveyReg(Iflt)
	  
	  DO 9641 Istk=1,Nstk
	   DO 9641 Sex=1,2
	    DO 9641 Age=0,MaxAge
	     ZatAge(Istk,Ireg,Sex,Age,Year) = M(Istk,Ireg,Sex,Age,Year)+Uage(Istk,Ireg,Sex,Age,Year)
  		  DO 9641,Ilen=1,Nlen
		   Temp = WtLen(Ilen,Istk,Sex)*SurveySel(Iflt,Ilen)*Fraclen(Ilen,Istk,Sex,Age,Year)*N(Istk,Ireg,Sex,Age,Year)*EXP(-1.d0*SurveyTime(Iflt)*ZatAge(Istk,Ireg,Sex,Age,Year))		   
		   !IF (Age.EQ.0) Temp = Temp*SelAge(Iflt,Istk,Sex,Age,Year)
!		  WRITE(98,*) Year,Iflt,Sex,Age
!		  WRITE(98,*) 
	      SurveyVBio(Iflt,Year) = SurveyVBio(Iflt,Year) + Temp
!		  RetVBio(Iflt,Ireg,Year) = RetVBio(Iflt,Ireg,Year) + Retlen(Iflt,Ilen,Year)*Temp
!		  RetVBio(Iflt,Ireg,Year) = RetVBio(Iflt,Ireg,Year) + RetAge(Iflt,Istk,Sex,Age,Year)*Temp
9641	 CONTINUE
     !VBio(Iflt,0,Year) = SUM(VBio(Iflt,1:Nreg,Year))
	 !RetVBio(Iflt,0,Year) = SUM(RetVBio(Iflt,1:Nreg,Year))
9640	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the deviations in Q for this year
!	Equation 1.35
!

	SUBROUTINE GetQdevs(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,Ireg,ISEED
	REAL*8 NewDev
	REAL*8 XNORM

	EXTERNAL XNORM

	ISEED = ISEEDZ

	 DO 8610 Iflt=1,Nflt
	  DO 8610 Ireg=1,Nreg
	    NewDev = XNORM(5,0.d0,CpueVar(Iflt),ISEED)
		CpueQdevs(Iflt,Ireg,Iyr) = CpueCorr(Iflt)*CpueQdevs(Iflt,Ireg,Iyr-1) + NewDev*SQRT(1-(CpueCorr(Iflt)**2.d0))
8610 CONTINUE

	RETURN
	
	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the catch data

	SUBROUTINE GetFishCatchData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,ISEED,Ireg
	REAL*8 XNORM,ExpCat

	EXTERNAL XNORM

	ISEED = ISEEDZ

	!open write file for appending
	OPEN(UNIT=38,FILE='Catches.inp',POSITION='APPEND')

	!Catch Data
	IF (Diag.EQ.1) WRITE(98,*) 'Catch data by fleet for ',Iyr
	 DO 8671 Iflt=1,Nflt
	  DO 8671 Ireg=1,Nreg
	   RetCatchDat(Iflt,Ireg,Iyr) = 0.d0
	   IF (CatYrs(Iflt,Iyr).EQ.1) THEN
        RetCatchDat(Iflt,Ireg,Iyr) = RetCatch(Iflt,Ireg,Iyr)
	    IF (CatCV(Iflt).GT.0) RetCatchDat(Iflt,Ireg,Iyr) = RetCatch(Iflt,Ireg,Iyr) * EXP(XNORM(5,0.d0,CatCV(Iflt),ISEED)-0.5d0*CatCV(Iflt)*CatCV(Iflt))
 	    IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F10.4)') Iyr,Iflt,Ireg,RetCatch(Iflt,Ireg,Iyr),RetCatchDat(Iflt,Ireg,Iyr)
		WRITE(38,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,Iflt,Ireg,RetCatchDat(Iflt,Ireg,Iyr)
	   ENDIF	   
8671 CONTINUE

	CLOSE(38)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.2.2 SIZE COMPOSITIONS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the discard rate/biomass data

	SUBROUTINE GetFishDiscData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,ISEED
	REAL*8 XNORM,ExpDisc

	EXTERNAL XNORM

	ISEED = ISEEDZ

	!open write file for appending
	OPEN(UNIT=34,FILE='Discard.inp',POSITION='APPEND')

	!Discard data
	IF (Diag.EQ.1) WRITE(98,*) 'Discard data by fleet for ',Iyr
	 DO 8670 Iflt=1,Nflt
	   DiscDat(Iflt,Iyr) = -99
	   IF (DiscYrs(Iflt,Iyr).EQ.1) THEN
	    ExpDisc = SUM(Discard(Iflt,1:Nreg,Iyr))
	    IF (Discflag(1).EQ.2) ExpDisc = ExpDisc/(SUM(TotalCatch(Iflt,1:Nreg,Iyr))+0.0001)
	    DiscDat(Iflt,Iyr) = ExpDisc + XNORM(5,0.d0,DiscCV(Iflt+1)*ExpDisc,ISEED)
 	    IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,F10.4,1x,F10.4)') Iyr,Iflt,ExpDisc,DiscDat(Iflt,Iyr)
		WRITE(34,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') Iyr,1,Iflt,DiscDat(Iflt,Iyr),DiscCV(Iflt)
	   ENDIF
8670 CONTINUE

	CLOSE(34)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.2.2 SIZE COMPOSITIONS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the fishery length comp data 
!
	SUBROUTINE GetFishLengthData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,II,JJ,Nlens(Nflt,Nreg,1:2),Age,Iflt,Ireg,Sex,Ilen,Part,ISEED1,ISEED2
	REAL*8 TempCatch(Nflt,Nreg,1:2),LenComp(2000),LenProps(2000),EmptySex(Nlen)
	REAL*8 PropVec(2000),NewVec(2000)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!fill empty sex vector
	EmptySex = 0.d0

	!open the output file for appending
    OPEN(UNIT=32,FILE='FisheryLengths.inp',POSITION='APPEND')
    OPEN(UNIT=38,FILE='FisheryLengths2019.out',POSITION='APPEND')

	!loop over type of composition data
  	 DO 8600 II=1,3
	  IF (LenYrs(II,Iyr).EQ.1) THEN
		    
		Nlens=0
		TempCatch=0.d0
		!Find samples sizes by fleet + region + sex
		CALL FindAgeCatches(II,Iyr,TempCatch)
		!Sum and find proportions
		IF (SUM(TempCatch(1:Nflt,1:Nreg,1:2)).LT.0.1d0) GOTO 8605
		TempCatch = TempCatch/SUM(TempCatch(1:Nflt,1:Nreg,1:2))
		!allocate age samples to fleets + regions
		!Nlens = NINT(TempCatch*LengthN(II))
		PropVec = 0.d0
		JJ=0
		DO 8602 Iflt=1,Nflt
		 DO 8602 Ireg=1,Nreg
		  DO 8602 Sex=1,2
		   JJ=JJ+1
		   PropVec(JJ) = TempCatch(Iflt,Ireg,Sex)
8602	CONTINUE
		NewVec = 0.d0
		CALL GENMUL(PropVec,LengthN(II),NewVec,ISEEDZ,.FALSE.,1)
		JJ=0
		DO 8603 Iflt=1,Nflt
		 DO 8603 Ireg=1,Nreg
		  DO 8603 Sex=1,2
		   JJ=JJ+1
		   Nlens(Iflt,Ireg,Sex) = NewVec(JJ)
8603	CONTINUE

!		Nlens=0
!		TempCatch=0.d0
!		!Find samples sizes by fleet + region + sex
!		CALL FindAgeCatches(II,Iyr,TempCatch)
!		!Sum and find proportions
!		TempCatch = TempCatch/SUM(TempCatch(1:Nflt,1:Nreg,1:2))
!		!allocate age samples to fleets + regions
!		!Nlens = NINT(TempCatch*LengthN(II))
!		DO 8604 Iflt=1,Nflt
!		 PropVec = 0.d0
!		 JJ=0
!		 DO 8602 Ireg=1,Nreg
!		  DO 8602 Sex=1,2
!		   JJ=JJ+1
!		   PropVec(JJ) = TempCatch(Iflt,Ireg,Sex)
!8602	CONTINUE
!		NewVec = 0.d0
!		IF (SUM(Catch(Iflt,1:Nreg,Iyr)).GT.0) CALL GENMUL(PropVec,LengthN(II,Iflt),NewVec,ISEEDZ,.FALSE.,1)
!		JJ=0
!		!DO 8603 Iflt=1,Nflt
!		 DO 8603 Ireg=1,Nreg
!		  DO 8603 Sex=1,2
!		   JJ=JJ+1
!		   Nlens(Iflt,Ireg,Sex) = NewVec(JJ)
!!8603	CONTINUE
!!8604	CONTINUE

		!loop over fleets and regions and sex
		DO 8601 Iflt=1,Nflt
		 DO 8601 Ireg=1,Nreg
		  DO 8601 Sex=1,2
		  IF (Nlens(Iflt,Ireg,Sex).GT.0) THEN
		   !get expected proportions
		   CALL GetLenProps(1,II,Iflt,Ireg,Sex,Iyr,LenProps)
		   !generate the age composition
!		   IF (Diag.EQ.1) WRITE(98,*) Nages(Iflt,Ireg,Sex)
!		   WRITE(99,*) 'ISEED',ISEEDZ
		   CALL GENMUL(LenProps,Nlens(Iflt,Ireg,Sex),LenComp,ISEEDZ,.FALSE.,1)
		   !write the comp to the output file
		   !Tier 3
		   IF (Tier(1).EQ.3) WRITE(32,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,Iflt,Ireg,Sex,II,NLens(Iflt,Ireg,Sex),(LenComp(Ilen),Ilen=1,Nlen)
		   !MAFMC fluke recreational discards
		   IF (Tier(1).EQ.62) WRITE(32,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,Iflt,Ireg,Sex,II,NLens(Iflt,Ireg,Sex),(LenComp(Ilen),Ilen=1,Nlen)
		   IF (Tier(1).EQ.62.AND.Diag.EQ.-1.AND.Iyr.EQ.2019) WRITE(38,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,Iflt,Ireg,Sex,II,NLens(Iflt,Ireg,Sex),(LenComp(Ilen),Ilen=1,Nlen)
		   !SS2 output
		   IF (Tier(1).EQ.1) THEN
		    IF (II.EQ.1) Part = 2
		    IF (II.EQ.2) Part = 0
		    IF (II.EQ.3) Part = 1
		    IF (Sex.EQ.1) WRITE(32,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,1,Iflt,Sex,Part,NLens(Iflt,Ireg,Sex),(LenComp(Ilen),Ilen=1,Nlen),(EmptySex(Ilen),Ilen=1,Nlen)
		    IF (Sex.EQ.2) WRITE(32,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,1,Iflt,Sex,Part,NLens(Iflt,Ireg,Sex),(EmptySex(Ilen),Ilen=1,Nlen),(LenComp(Ilen),Ilen=1,Nlen)
		   ENDIF
		  ENDIF
8601	CONTINUE

	  ENDIF
8605  CONTINUE
8600 CONTINUE


	CLOSE(32)
	CLOSE(38)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	SURVEY SIZE COMPOSITIONS
!   09/04/17
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the survey length comp data 
!
	SUBROUTINE GetSurveyLengthData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,II,JJ,Nlens(Nsurv,1:2),Age,Iflt,Ireg,Sex,Ilen,Part,ISEED1,ISEED2,Istk
	REAL*8 TempCatch(1:2),LenComp(2000),LenProps(2000),EmptySex(Nlen)
	REAL*8 PropVec(2000),NewVec(2000)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!fill empty sex vector
	EmptySex = 0.d0

	!open the output file for appending
    OPEN(UNIT=42,FILE='SurveyLengths.inp',POSITION='APPEND')

	!loop over # of surveys
  	 DO 9600 II=1,3
	  IF (SurveyYrs(II,Iyr).EQ.1) THEN
		    
		Nlens=0
		TempCatch=0.d0
		!Find sample sizes by sex
		DO 9606 Sex=1,2
		 DO 9606 Istk=1,Nstk
		  DO 9606 Age=0,MaxAge
		   DO 9606 Ilen=1,Nlen
 		    TempCatch(Sex) = TempCatch(Sex) + N(Istk,SurveyReg(II),Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(II)*ZatAge(Istk,SurveyReg(II),Sex,Age,Iyr))*FracLen(Ilen,Istk,Sex,Age,Iyr)*SurveySel(II,Ilen)
9606	CONTINUE

		!Sum and find proportions
		IF (SUM(TempCatch(1:2)).LT.0.1d0) GOTO 9605
		TempCatch = TempCatch/SUM(TempCatch(1:2))
		!allocate length samples to sex
		PropVec = 0.d0
		JJ=0
		DO 9602 Sex=1,2
		 JJ=JJ+1
		 PropVec(JJ) = TempCatch(Sex)
9602	CONTINUE
		NewVec = 0.d0
		CALL GENMUL(PropVec,SurveyLengthN(II),NewVec,ISEEDZ,.FALSE.,1)
		JJ=0
		DO 9603 Sex=1,2
		   JJ=JJ+1
		   Nlens(II,Sex) = NewVec(JJ)
9603	CONTINUE

		!loop over sex
		DO 9601 Sex=1,2
		  IF (Nlens(II,Sex).GT.0) THEN
		   !get expected proportions
		   CALL GetLenProps(2,2,II,SurveyReg(II),Sex,Iyr,LenProps)
		   !generate the length composition
!		   IF (Diag.EQ.1) WRITE(98,*) Nages(Iflt,Ireg,Sex)
!		   WRITE(99,*) 'ISEED',ISEEDZ
		   CALL GENMUL(LenProps,Nlens(II,Sex),LenComp,ISEEDZ,.FALSE.,1)
		   !write the comp to the output file
		   !Tier 3
		   IF (Tier(1).EQ.3) WRITE(42,'(I4,1x,I2,1x,I2,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,II,Ireg,Sex,NLens(II,Sex),(LenComp(Ilen),Ilen=1,Nlen)
		   !SS2 output
		   IF (Tier(1).EQ.1) THEN
		    Part = 0
		    IF (Sex.EQ.1) WRITE(42,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,1,Nflt+II,Sex,Part,NLens(II,Sex),(LenComp(Ilen),Ilen=1,Nlen),(EmptySex(Ilen),Ilen=1,Nlen)
		    IF (Sex.EQ.2) WRITE(42,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.0,1x))') Iyr,1,Nflt+II,Sex,Part,NLens(II,Sex),(EmptySex(Ilen),Ilen=1,Nlen),(LenComp(Ilen),Ilen=1,Nlen)
		   ENDIF
		  ENDIF
9601	CONTINUE

	  ENDIF
9605  CONTINUE
9600 CONTINUE


	CLOSE(42)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the fishery age comp data 
!
	SUBROUTINE GetFishAgeData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,II,JJ,Nages(Nflt,Nreg,1:2),Age,Iflt,Ireg,Sex,Part,ISEED1,ISEED2
    INTEGER Ilen
	REAL*8 TempCatch(Nflt,Nreg,1:2),AgeComp(2000),AgeProps(2000),EmptySex(2000)
	REAL*8 PropVec(2000),NewVec(2000),LenProps(2000),LenComp(2000)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!fill empty sex vector
	EmptySex = 0.d0

	!open the output file for appending
    OPEN(UNIT=31,FILE='FisheryAges.inp',POSITION='APPEND')
    OPEN(UNIT=36,FILE='CAFAges.inp',POSITION='APPEND')

    OPEN(UNIT=38,FILE='FisheryAges2019.out',POSITION='APPEND') 


	!loop over type of composition data
  	 DO 8200 II=1,3
	  IF (AgeYrs(II,Iyr).EQ.1) THEN  	
		Nages=0
		TempCatch=0.d0
		!Find samples sizes by fleet + region + sex
		CALL FindAgeCatches(II,Iyr,TempCatch)
		IF (SUM(TempCatch(1:Nflt,1:Nreg,1:2)).GT.0) THEN
		!Sum and find proportions
		TempCatch = TempCatch/SUM(TempCatch(1:Nflt,1:Nreg,1:2))
	    !IF (Iyr.EQ.2001) WRITE(*,*) Iyr,SUM(TempCatch(1:Nflt,1:Nreg,1:2))
		!allocate age samples to fleets + regions
		!Nages = NINT(TempCatch*AgeN(II))
		PropVec = 0.d0
		JJ=0
		DO 8202 Iflt=1,Nflt
		 DO 8202 Ireg=1,Nreg
		  DO 8202 Sex=1,2
		   JJ=JJ+1
		   PropVec(JJ) = TempCatch(Iflt,Ireg,Sex)
8202	CONTINUE
	    !IF (Iyr.EQ.2001) WRITE(*,*) PropVec
		NewVec = 0.d0
		CALL GENMUL(PropVec,AgeN(II),NewVec,ISEEDX,.FALSE.,1)
		JJ=0
		DO 8203 Iflt=1,Nflt
		 DO 8203 Ireg=1,Nreg
		  DO 8203 Sex=1,2
		   JJ=JJ+1
		   Nages(Iflt,Ireg,Sex) = NewVec(JJ)
8203	CONTINUE

		!loop over fleets and regions and sex
		DO 8210 Iflt=1,Nflt
		 DO 8210 Ireg=1,Nreg
		  DO 8210 Sex=1,2
		  IF (Nages(Iflt,Ireg,Sex).GT.0) THEN
           IF (Tier(1).EQ.1.AND.DoCALK.EQ.1) THEN
		   !!!!! For CAF bit
		   CALL GetLenProps(1,II,Iflt,Ireg,Sex,Iyr,LenProps)
		   CALL GENMUL(LenProps,Nages(Iflt,Ireg,Sex),LenComp,ISEEDZ,.FALSE.,1)
		   DO 8211 Ilen=1,Nlen
		    IF (LenComp(Ilen).GT.0) THEN
			 CALL GetAgeProps2(1,II,Iflt,Ireg,Sex,Iyr,Ilen,AgeProps)
			 CALL GENMUL(AgeProps,NINT(LenComp(Ilen)),AgeComp,ISEEDZ,.FALSE.,1)
              IF (II.EQ.1) Part = 2
		      IF (II.EQ.2) Part = 0
		      IF (II.EQ.3) Part = 1
		      IF (Sex.EQ.1) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Iflt,Sex,Part,1,Ilen,Ilen,NINT(LenComp(Ilen)),(AgeComp(Age),Age=1,MaxAge+1),(EmptySex(Age),Age=1,MaxAge+1)
		      IF (Sex.EQ.2) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Iflt,Sex,Part,1,Ilen,Ilen,NINT(LenComp(Ilen)),(EmptySex(Age),Age=1,MaxAge+1),(AgeComp(Age),Age=1,MaxAge+1)
            ENDIF				
8211	   CONTINUE		   
		   !!!!!
		   ELSE
		   !get expected proportions
		   CALL GetAgeProps(1,II,Iflt,Ireg,Sex,Iyr,AgeProps)
		   !generate the age composition
!		   IF (Diag.EQ.1) WRITE(98,*) Nages(Iflt,Ireg,Sex)
		   CALL GENMUL(AgeProps,Nages(Iflt,Ireg,Sex),AgeComp,ISEEDZ,.FALSE.,1)
		   !write the comp to the output file
		   !tier 3
		   IF (Tier(1).EQ.3) WRITE(31,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I12,1x,200(F10.0,1x))') Iyr,Iflt,Ireg,Sex,II,Nages(Iflt,Ireg,Sex),(AgeComp(Age),Age=1,MaxAge+1)
		   IF (Tier(1).EQ.62) WRITE(31,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I12,1x,200(F10.0,1x))') Iyr,Iflt,Ireg,Sex,II,Nages(Iflt,Ireg,Sex),(AgeComp(Age),Age=1,MaxAge+1)
		   IF (Tier(1).EQ.62.AND.Diag.EQ.-1.AND.Iyr.EQ.2019) WRITE(38,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I12,1x,200(F10.0,1x))') Iyr,Iflt,Ireg,Sex,II,Nages(Iflt,Ireg,Sex),(AgeComp(Age),Age=1,MaxAge+1)		   
		   !SS2 output
		   IF (Tier(1).EQ.1) THEN
		    IF (II.EQ.1) Part = 2
		    IF (II.EQ.2) Part = 0
		    IF (II.EQ.3) Part = 1
		    IF (Sex.EQ.1) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Iflt,Sex,Part,1,-1,-1,Nages(Iflt,Ireg,Sex),(AgeComp(Age),Age=1,MaxAge+1),(EmptySex(Age),Age=1,MaxAge+1)
		    IF (Sex.EQ.2) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Iflt,Sex,Part,1,-1,-1,Nages(Iflt,Ireg,Sex),(EmptySex(Age),Age=1,MaxAge+1),(AgeComp(Age),Age=1,MaxAge+1)
		   ENDIF
		   IF (Tier(2).EQ.3) THEN
		   !write the ages to the CAF file for ALKs
		   DO 8204 Age=0,MaxAge
		    IF (AgeComp(Age+1).GT.0) THEN
              !WRITE(99,*) 'Age',Age
			  !WRITE(99,*) AgeComp(Age+1)
			 !get length of each age
			 !expected proportions of length at age
			 CALL GetLenProps2(Age,II,Iflt,Ireg,Sex,Iyr,LenProps)
			  !WRITE(99,'(200(F6.4,1x))') (LenProps(Ilen),Ilen=1,Nlen)
             !get the lengths
			 CALL GENMUL(LenProps,NINT(AgeComp(Age+1)),LenComp,ISEEDZ,.FALSE.,1)
			  !WRITE(99,*) 'Length at age'
			  !WRITE(99,'(200(F6.4,1x))') (LenComp(Ilen),Ilen=1,Nlen)
			 !write to file
			 DO 8205 Ilen=1,Nlen
			  IF (LenComp(Ilen).GT.0) THEN
			    DO JJ=1,LenComp(Ilen)
				 WRITE(36,'(I4,1x,4(I2,1x),I3,1x,F6.2)') Iyr,Iflt,Ireg,Sex,II,Age,lolenbin(Ilen)
				ENDDO
			  ENDIF
8205	     CONTINUE			 
			ENDIF
8204	   CONTINUE
		   ENDIF
		   ENDIF
		  ENDIF
8210	CONTINUE
       ENDIF
	  ENDIF
8200 CONTINUE


	CLOSE(31)
    CLOSE(36)
    CLOSE(38)

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the survey age comp data 
!   09/04/17
!
	SUBROUTINE GetSurveyAgeData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,II,JJ,Nages(Nsurv,1:2),Age,Iflt,Ireg,Sex,Part,ISEED1,ISEED2,Istk
    INTEGER Ilen
	REAL*8 TempCatch(1:2),AgeComp(2000),AgeProps(2000),EmptySex(2000)
	REAL*8 PropVec(2000),NewVec(2000),LenProps(2000),LenComp(2000)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!fill empty sex vector
	EmptySex = 0.d0

	!open the output file for appending
    OPEN(UNIT=41,FILE='SurveyAges.inp',POSITION='APPEND')
    !OPEN(UNIT=36,FILE='CAFAges.inp',POSITION='APPEND')


	!loop over # of surveys
  	 DO 9200 II=1,Nsurv
  	  Ireg = SurveyReg(II)
	  IF (SurveyYrs(II,Iyr).EQ.1) THEN  	
		Nages=0
		TempCatch=0.d0
		!Find samples sizes by sex

		DO 9212 Sex=1,2
		 DO 9212 Istk=1,Nstk
		  DO 9212 Age=0,MaxAge
		   DO 9212 Ilen=1,Nlen
 		    TempCatch(Sex) = TempCatch(Sex) + N(Istk,SurveyReg(II),Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(II)*ZatAge(Istk,SurveyReg(II),Sex,Age,Iyr))*FracLen(Ilen,Istk,Sex,Age,Iyr)*SurveySel(II,Ilen)
9212	CONTINUE

		IF (SUM(TempCatch(1:2)).GT.0) THEN
		!Sum and find proportions
		TempCatch = TempCatch/SUM(TempCatch(1:2))
	    !IF (Iyr.EQ.2001) WRITE(*,*) Iyr,SUM(TempCatch(1:Nflt,1:Nreg,1:2))
		!allocate age samples to fleets + regions
		PropVec = 0.d0
		JJ=0
		DO 9202 Sex=1,2
		   JJ=JJ+1
		   PropVec(JJ) = TempCatch(Sex)
9202	CONTINUE
	    !IF (Iyr.EQ.2001) WRITE(*,*) PropVec
		NewVec = 0.d0
		CALL GENMUL(PropVec,SurveyAgeN(II),NewVec,ISEEDX,.FALSE.,1)
		JJ=0
		  DO 9203 Sex=1,2
		   JJ=JJ+1
		   Nages(II,Sex) = NewVec(JJ)
9203	CONTINUE

		!loop over sex
		  DO 9210 Sex=1,2
		  IF (Nages(II,Sex).GT.0) THEN
           IF (Tier(1).EQ.1.AND.DoCALK.EQ.1) THEN
		   !!!!! For CAF bit
		   CALL GetLenProps(2,2,II,Ireg,Sex,Iyr,LenProps)
		   CALL GENMUL(LenProps,Nages(II,Sex),LenComp,ISEEDZ,.FALSE.,1)
		   DO 9211 Ilen=1,Nlen
		    IF (LenComp(Ilen).GT.0) THEN
			 CALL GetAgeProps2(2,2,II,Ireg,Sex,Iyr,Ilen,AgeProps)
			 CALL GENMUL(AgeProps,NINT(LenComp(Ilen)),AgeComp,ISEEDZ,.FALSE.,1)
              Part = 0
		      IF (Sex.EQ.1) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Nflt+II,Sex,Part,1,Ilen,Ilen,NINT(LenComp(Ilen)),(AgeComp(Age),Age=1,MaxAge+1),(EmptySex(Age),Age=1,MaxAge+1)
		      IF (Sex.EQ.2) WRITE(31,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Nflt+II,Sex,Part,1,Ilen,Ilen,NINT(LenComp(Ilen)),(EmptySex(Age),Age=1,MaxAge+1),(AgeComp(Age),Age=1,MaxAge+1)
            ENDIF				
9211	   CONTINUE		   
		   !!!!!
		   ELSE
		   !get expected proportions
		   CALL GetAgeProps(2,2,II,Ireg,Sex,Iyr,AgeProps)
		   !generate the age composition
!		   IF (Diag.EQ.1) WRITE(98,*) Nages(Iflt,Ireg,Sex)
		   CALL GENMUL(AgeProps,Nages(II,Sex),AgeComp,ISEEDZ,.FALSE.,1)
		   !write the comp to the output file
		   !tier 3
		   IF (Tier(1).EQ.3) WRITE(41,'(I4,1x,I2,1x,I2,1x,I1,1x,I12,1x,200(F10.0,1x))') Iyr,II,Ireg,Sex,Nages(II,Sex),(AgeComp(Age),Age=1,MaxAge+1)
		   !SS2 output
		   IF (Tier(1).EQ.1) THEN
		    Part = 0
		    IF (Sex.EQ.1) WRITE(41,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Nflt+II,Sex,Part,1,-1,-1,Nages(II,Sex),(AgeComp(Age),Age=1,MaxAge+1),(EmptySex(Age),Age=1,MaxAge+1)
		    IF (Sex.EQ.2) WRITE(41,'(I4,1x,7(I2,1x),I12,1x,200(F10.0,1x))') Iyr,1,Nflt+II,Sex,Part,1,-1,-1,Nages(II,Sex),(EmptySex(Age),Age=1,MaxAge+1),(AgeComp(Age),Age=1,MaxAge+1)
		   ENDIF
!		   IF (Tier(2).EQ.3) THEN
!		   !write the ages to the CAF file for ALKs
!		   DO 8204 Age=0,MaxAge
!		    IF (AgeComp(Age+1).GT.0) THEN
!              !WRITE(99,*) 'Age',Age
!			  !WRITE(99,*) AgeComp(Age+1)
!			 !get length of each age
!			 !expected proportions of length at age
!			 CALL GetLenProps2(Age,II,Iflt,Ireg,Sex,Iyr,LenProps)
!			  !WRITE(99,'(200(F6.4,1x))') (LenProps(Ilen),Ilen=1,Nlen)
 !            !get the lengths
!			 CALL GENMUL(LenProps,NINT(AgeComp(Age+1)),LenComp,ISEEDZ,.FALSE.,1)
!			  !WRITE(99,*) 'Length at age'
!			  !WRITE(99,'(200(F6.4,1x))') (LenComp(Ilen),Ilen=1,Nlen)
!			 !write to file
!			 DO 8205 Ilen=1,Nlen
!			  IF (LenComp(Ilen).GT.0) THEN
!			    DO JJ=1,LenComp(Ilen)
!				 WRITE(36,'(I4,1x,4(I2,1x),I3,1x,F6.2)') Iyr,Iflt,Ireg,Sex,II,Age,lolenbin(Ilen)
!				ENDDO
!			  ENDIF
!8205	     CONTINUE			 
!			ENDIF
!8204	   CONTINUE
!		   ENDIF
		   ENDIF
		  ENDIF
9210	CONTINUE
       ENDIF
	  ENDIF
9200 CONTINUE


	CLOSE(41)
!    CLOSE(36)

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the length comp proportions for a given fleet/survey, region, sex
!   FOR A GIVEN AGE (FOR generation of the CAF file)

	SUBROUTINE GetLenProps2(Age,II,Iflt,Ireg,Sex,Iyr,LenProps)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Id,II,Iflt,Ireg,Sex,Iyr,Istk,Age,Ilen
	REAL*8 LenProps(2000),RetTilde,Timing

	Timing = 0.5d0

	LenProps = 0.d0

	!check for allocation of fleet to this region
	IF (FleetRegions(Iflt,Ireg).EQ.0) RETURN
	!check for allocation of survey to this region


	DO 8401 Ilen=1,Nlen
	 LenProps(Ilen) = 0.d0
	  !equation 1.39
	  IF (II.EQ.2) RetTilde = 1.d0
	  IF (II.EQ.1) RetTilde = RetLen(Iflt,Ilen,Iyr)
	  IF (II.EQ.3) RetTilde = 1.d0-RetLen(Iflt,Ilen,Iyr)
	  !eqn 1.38
	  DO 8402 Istk=1,Nstk
	    LenProps(Ilen) = LenProps(Ilen) + Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
8402  CONTINUE
	  !fishery
	  !IF (Id.EQ.1) LenProps(Ilen) = LenProps(Ilen)*SelLen(Iflt,Ilen,Iyr)*RetTilde
	  LenProps(Ilen) = LenProps(Ilen)*SelLen(Iflt,Ilen,Iyr)*RetTilde
	  !survey
      !IF (Id.EQ.2) AgeProps(Age+1) = AgeProps(Age+1) + RetTilde*SelAge(Iflt,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(Timing*(LOG(1.d0-Uage(Istk,Ireg,Sex,Age,Iyr))-M(Istk,Ireg,Sex,Age,Iyr)))
8401 CONTINUE

	!normalise to get proportions
	LenProps = LenProps/SUM(LenProps(1:Nlen))
    Id=1
	IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,II,Id,(LenProps(Ilen),Ilen=1,Nlen)



	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the numbers at length for a given fleet/survey, region, sex

	SUBROUTINE GetLenNums(Id,II,Iflt,Ireg,Sex,Iyr,LenProps)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Id,II,Iflt,Ireg,Sex,Iyr,Istk,Age,Ilen
	REAL*8 LenProps(2000),RetTilde,Timing

	Timing = 0.5d0

	LenProps = 0.d0

	!check for allocation of fleet to this region
	IF (Id.EQ.1.AND.FleetRegions(Iflt,Ireg).EQ.0) RETURN
	!check for allocation of survey to this region
!	2024-06-13, GF deleting next line for error troubleshooting purposes	
!    IF (Id.EQ.2.AND.SurveyReg(Iflt).NE.Ireg) RETURN


	DO 8401 Ilen=1,Nlen
	 LenProps(Ilen) = 0.d0
	  !equation 1.39
	  IF (Id.EQ.2.OR.II.EQ.2) RetTilde = 1.d0
	  IF (Id.EQ.1.AND.II.EQ.1) RetTilde = RetLen(Iflt,Ilen,Iyr)
	  IF (Id.EQ.1.AND.II.EQ.3) RetTilde = 1.d0-RetLen(Iflt,Ilen,Iyr)
	  !eqn 1.38
	  DO 8402 Istk=1,Nstk
	   DO 8402 Age=0,MaxAge
	    IF (Id.EQ.1) LenProps(Ilen) = LenProps(Ilen) + Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
	    IF (Id.EQ.2) LenProps(Ilen) = LenProps(Ilen) + Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(Iflt)*ZatAge(Istk,Ireg,Sex,Age,Iyr))
8402  CONTINUE
	  !fishery
	  IF (Id.EQ.1) LenProps(Ilen) = LenProps(Ilen)*SelLen(Iflt,Ilen,Iyr)*RetTilde
	  !survey
      !IF (Id.EQ.2) LenProps(Ilen) = LenProps(Ilen)*SurveySel(Iflt,Ilen)
8401 CONTINUE

	!!normalise to get proportions
	!LenProps = LenProps/SUM(LenProps(1:Nlen))

	!WRITE(*,'(I4,1x,I2,1x,I2,1x,I1,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,(LenProps(Ilen),Ilen=1,Nlen)

	IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,II,Id,(LenProps(Ilen),Ilen=1,Nlen)


	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the length comp proportions for a given fleet/survey, region, sex
!	equations 1.37, 1.38, 1.39

	SUBROUTINE GetLenProps(Id,II,Iflt,Ireg,Sex,Iyr,LenProps)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Id,II,Iflt,Ireg,Sex,Iyr,Istk,Age,Ilen
	REAL*8 LenProps(2000),RetTilde,Timing

	Timing = 0.5d0

	LenProps = 0.d0

	!check for allocation of fleet to this region
	IF (Id.EQ.1.AND.FleetRegions(Iflt,Ireg).EQ.0) RETURN
	!check for allocation of survey to this region
!	2024-06-13, GF deleting next line for error troubleshooting purposes	
!    IF (Id.EQ.2.AND.SurveyReg(Iflt).NE.Ireg) RETURN


	DO 8401 Ilen=1,Nlen
	 LenProps(Ilen) = 0.d0
	  !equation 1.39
	  IF (Id.EQ.2.OR.II.EQ.2) RetTilde = 1.d0
	  IF (Id.EQ.1.AND.II.EQ.1) RetTilde = RetLen(Iflt,Ilen,Iyr)
	  IF (Id.EQ.1.AND.II.EQ.3) RetTilde = 1.d0-RetLen(Iflt,Ilen,Iyr)
	  !eqn 1.38
	  DO 8402 Istk=1,Nstk
	   DO 8402 Age=0,MaxAge
	    IF (Id.EQ.1) LenProps(Ilen) = LenProps(Ilen) + Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
	    IF (Id.EQ.2) LenProps(Ilen) = LenProps(Ilen) + Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(Iflt)*ZatAge(Istk,Ireg,Sex,Age,Iyr))
8402  CONTINUE
	  !fishery
	  IF (Id.EQ.1) LenProps(Ilen) = LenProps(Ilen)*SelLen(Iflt,Ilen,Iyr)*RetTilde
	  !survey
      !IF (Id.EQ.2) LenProps(Ilen) = LenProps(Ilen)*SurveySel(Iflt,Ilen)
8401 CONTINUE

	!normalise to get proportions
	LenProps = LenProps/SUM(LenProps(1:Nlen))

	!WRITE(*,'(I4,1x,I2,1x,I2,1x,I1,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,(LenProps(Ilen),Ilen=1,Nlen)

	IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,II,Id,(LenProps(Ilen),Ilen=1,Nlen)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the age comp proportions for a given LENGTH BIN

	SUBROUTINE GetAgeProps2(Id,II,Iflt,Ireg,Sex,Iyr,Ilen,AgeProps)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Id,II,Iflt,Ireg,Sex,Iyr,Istk,Age,Ilen
	REAL*8 AgeProps(2000),RetTilde,Timing

	Timing = 0.5d0

	AgeProps = 0.d0

	IF (Id.EQ.1.AND.FleetRegions(Iflt,Ireg).EQ.0) RETURN
	!check for allocation of survey to this region


	DO 8400 Age=0,MaxAge
	 AgeProps(Age+1) = 0.d0
	  !equation 1.38
	  IF (Id.EQ.2.OR.II.EQ.2) RetTilde = 1.d0
	  IF (Id.EQ.1.AND.II.EQ.1) RetTilde = RetLen(Iflt,Ilen,Iyr)
	  IF (Id.EQ.1.AND.II.EQ.3) RetTilde = 1.d0-RetLen(Iflt,Ilen,Iyr)
	 DO 8400 Istk=1,Nstk
	  !eqn 1.37
	  !fishery
	  IF (Id.EQ.1) AgeProps(Age+1) = AgeProps(Age+1) + RetTilde*SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
	  !survey
      IF (Id.EQ.2) AgeProps(Age+1) = AgeProps(Age+1) + RetTilde*SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-Timing*M(Istk,Ireg,Sex,Age,Iyr))
8400 CONTINUE

	!normalise to get proportions
	AgeProps = AgeProps/SUM(AgeProps(1:MaxAge+1))

!	IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,II,Id,(AgeProps(Age),Age=1,MaxAge+1)



	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the age comp proportions for a given fleet/survey, region, sex
!	equations 1.36, 1.37, 1.38

	SUBROUTINE GetAgeProps(Id,II,Iflt,Ireg,Sex,Iyr,AgeProps)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Id,II,Iflt,Ireg,Sex,Iyr,Istk,Age,Ilen
	REAL*8 AgeProps(2000),RetTilde,Timing

	Timing = 0.5d0

	AgeProps = 0.d0

	IF (Id.EQ.1.AND.FleetRegions(Iflt,Ireg).EQ.0) RETURN
	!check for allocation of survey to this region
	!IF (Id.EQ.2.AND.SurveyReg(Iflt).NE.Ireg) RETURN


	DO 8400 Age=0,MaxAge
	 !AgeProps(Age+1) = 0.d0
	 DO 8400 Ilen=1,Nlen
	  !equation 1.38
	  IF (Id.EQ.2.OR.II.EQ.2) RetTilde = 1.d0
	  IF (Id.EQ.1.AND.II.EQ.1) RetTilde = RetLen(Iflt,Ilen,Iyr)
	  IF (Id.EQ.1.AND.II.EQ.3) RetTilde = 1.d0-RetLen(Iflt,Ilen,Iyr)
	 DO 8400 Istk=1,Nstk
	  !eqn 1.37
	  !fishery
	  !IF (Id.EQ.1) AgeProps(Age+1) = AgeProps(Age+1) + RetTilde*SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
	  !survey
      !IF (Id.EQ.2) AgeProps(Age+1) = AgeProps(Age+1) + SurveySel(Iflt,Ilen)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(Iflt)*ZatAge(Istk,Ireg,Sex,Age,Iyr))
	  !fishery
	  IF (Id.EQ.1) AgeProps(1:(MaxAge+1)) = AgeProps(1:(MaxAge+1)) + RetTilde*SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))*AgeErrMat(0:MaxAge,Age)
	  !survey
      IF (Id.EQ.2) AgeProps(1:(MaxAge+1)) = AgeProps(1:(MaxAge+1)) + SurveySel(Iflt,Ilen)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-1.d0*SurveyTime(Iflt)*ZatAge(Istk,Ireg,Sex,Age,Iyr))*AgeErrMat(0:MaxAge,Age)

8400 CONTINUE

	!normalise to get proportions
	AgeProps = AgeProps/SUM(AgeProps(1:MaxAge+1))

	IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,I1,1x,I6,1x,200(F6.4,1x))') Iyr,Iflt,Ireg,Sex,II,Id,(AgeProps(Age),Age=1,MaxAge+1)



	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine finds the Age comp sample size by fleet, region and sex
!	
	SUBROUTINE FindAgeCatches(Itype,Iyr,TempCatch)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Itype,Iyr,Ireg,Iflt,Sex,Istk,Age,Ilen,II

	REAL*8 TempCatch(Nflt,Nreg,2),Temp 

	IF (Diag.EQ.1) WRITE(98,'(A15,I4)') 'tempcatch for ',Iyr


	 TempCatch=0.d0
 	 DO 8300 Iflt=1,Nflt
	 DO 8300 Ireg=1,Nreg
	  DO 8300 Sex=1,2
	   DO 8302 Ilen=1,Nlen
		Temp = 0.d0	    
        DO 8301 Istk=1,Nstk	    
	     DO 8301 Age=0,MaxAge
		  Temp = Temp + WtLen(Ilen,Istk,Sex)*Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
8301	CONTINUE
		Temp = Temp*Ufleet(Iflt,Ireg,Iyr)*SelLen(Iflt,Ilen,Iyr)
		!retained 
		IF (Itype.EQ.1) TempCatch(Iflt,Ireg,Sex) = TempCatch(Iflt,Ireg,Sex) + Temp*RetLen(Iflt,Ilen,Iyr)
		!whole catch
		IF (Itype.EQ.2) TempCatch(Iflt,Ireg,Sex) = TempCatch(Iflt,Ireg,Sex) + Temp
		!discard
		IF (Itype.EQ.3) TempCatch(Iflt,Ireg,Sex) = TempCatch(Iflt,Ireg,Sex) + Temp*(1.d0-RetLen(Iflt,Ilen,Iyr))
8302  CONTINUE
      IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,F8.2)') Iflt,Ireg,Sex,TempCatch(Iflt,Ireg,Sex)
8300 CONTINUE



	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	TAGGING DATA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the tag recapture data 
!
	SUBROUTINE GetTagRecaptureData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Istk,Iyr,II,JJ,KK,Age,Iflt,Ireg,Sex,Ilen,Part,ISEED1,ISEED2,LL
	INTEGER NewRecap,NumRecaps,Flag,Recaps(1:Nflt*Nreg)
	REAL*8 ExpRecaps(0:Nflt,0:Nreg),GammaBit,GammaSD,PoisMu
	REAL*8 RecapProbs(1:Nflt*Nreg),RanNum
    REAL*8 AssignProbs(2000),NewAssign(2000)
    REAL*8 GAMMAZ,POISSN
    REAL*4 RAN5
	INTEGER IBin1,IBin2

	EXTERNAL GAMMAZ,POISSN,RAN5,IBin1,IBin2

	
	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX
	
	!open the output file for appending
    OPEN(UNIT=35,FILE='TagRecaptures.inp',POSITION='APPEND')

	IF (Diag.EQ.1) WRITE(98,'(A8,1x,I4)') 'TagRecap',Iyr

	  !IF (Iyr.GE.2010) WRITE(99,*) Iyr,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))

	!loop over tagging groups
  	 DO 8800 II=1,NTagGroups
	  !IF (Diag.EQ.1) WRITE(98,'(I2,1x,F7.2)') II,SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr))
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,F7.2)') II,SUM(Tagged(II,1:Nstk,1:Nzone,1:2,2,Iyr))
	  !IF (EstTagID(II,1).EQ.1) WRITE(99,'(I2,1x,I4,1x,F7.2,1x,F7.2)') II,Iyr,(SUM(Tagged(II,1:Nstk,Ireg,1:2,0:TopAge,Iyr)),Ireg=1,Nreg)
	  !IF (SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr)).GT.0) THEN	  
	  IF (SUM(Tagged(II,1:Nstk,1:Nzone,1:2,2,Iyr)).GT.0) THEN	  
	  !evaluate number of recaptures for this tagged group
	   DO 8801 Iflt=1,Nflt
 	    DO 8801 Ireg=1,Nreg
	     ExpRecaps(Iflt,Ireg) = 0
		 DO 8802 Istk=1,Nstk
 		 DO 8802 Sex=1,2
		 !DO 8802 Age=0,MaxAge
		  Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
		  !ExpRecaps(Iflt,Ireg) = ExpRecaps(Iflt,Ireg) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,Age,Iyr)*DetectRate(Iflt,Iyr)  !*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
		  !ExpRecaps(Iflt,Ireg) = ExpRecaps(Iflt,Ireg) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,Age,Iyr) !*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
		  ExpRecaps(Iflt,Ireg) = ExpRecaps(Iflt,Ireg) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,2,Iyr) !*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
8802	 CONTINUE		  
!		 ExpRecaps(Iflt,Ireg) = DetectRate(Iflt,Iyr)*ExpRecaps(Iflt,Ireg)
         IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,F7.2)') Iflt,Ireg,ExpRecaps(Iflt,Ireg)
8801   CONTINUE
       ExpRecaps(0,0) = SUM(ExpRecaps(1:Nflt,1:Nreg))
	   PoisMu = ExpRecaps(0,0)
	   CALL GetTagAlpha(II,Iyr)
	   !IF(TagAlpha(II,Iyr).GT.1) THEN
        GammaSD = SQRT(TagAlpha(II,Iyr)-1.d0)
	    GammaBit = GAMMAZ(1.d0,GammaSD,ISEEDZ)
	    PoisMu = ExpRecaps(0,0)*GammaBit   !/TagAlpha(II,Iyr)
	   !ENDIF
	   !IF (Iyr.EQ.2007.AND.Diag.EQ.1) WRITE(98,*) 'here'
	   RanNum = RAN5(ISEEDZ)
	   NumRecaps = POISSN(PoisMu,RanNum)
	   IF (Diag.EQ.1) WRITE(98,'(4(F6.2,1x),I6,1x,F6.4)') ExpRecaps(0,0),GammaSD,GammaBit,PoisMu,NumRecaps,RanNum
	   !IF (NumRecaps.GT.INT(SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr)))) NumRecaps = INT(SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr)))
	   IF (NumRecaps.GT.INT(SUM(Tagged(II,1:Nstk,1:Nzone,1:2,2,Iyr)))) NumRecaps = INT(SUM(Tagged(II,1:Nstk,1:Nzone,1:2,2,Iyr)))
	   IF (Diag.EQ.1) WRITE(98,'(4(F6.2,1x),I6,1x,F6.4)') ExpRecaps(0,0),GammaSD,GammaBit,PoisMu,NumRecaps,RanNum
      
	   !distribute recaptures by fleet and region
	   JJ=0
	   RecapProbs=0
	   Flag=0
	   DO 8803 Iflt=1,Nflt
	    DO 8803 Ireg=1,Nreg
		 JJ=JJ+1
	     RecapProbs(JJ) = ExpRecaps(Iflt,Ireg)/ExpRecaps(0,0)
		 IF (RecapProbs(JJ).EQ.1d0) Flag=JJ
8803   CONTINUE
       IF (Diag.EQ.1) WRITE(98,'(200(F6.2,1x))') (RecapProbs(Age),Age=1,JJ)
	   Recaps=0
       IF (Flag.EQ.0) THEN
	     LL=0
		 Assignprobs=0.d0
		 DO KK=1,JJ
          IF (RecapProbs(KK).GT.0) THEN
           LL=LL+1
		   AssignProbs(LL) = RecapProbs(KK)
		  ENDIF
		 ENDDO
         CALL GENMUL(AssignProbs,NumRecaps,NewAssign,ISEEDZ,.FALSE.,1)
	     !CALL GENMUL(RecapProbs,NumRecaps,Recaps,ISEED1,.FALSE.,1)
		 Recaps=0
		 LL=0
         DO KK=1,JJ
          IF (RecapProbs(KK).GT.0) THEN
           LL=LL+1
		   Recaps(KK) = NewAssign(LL)
		  ENDIF
		 ENDDO
       ENDIF
	   IF (Flag.GT.0) Recaps(Flag) = NumRecaps
       IF (Diag.EQ.1) WRITE(98,'(200(I6,1x))') (Recaps(Age),Age=1,JJ)
	   JJ=0
	   DO 8804 Iflt=1,Nflt
	    DO 8804 Ireg=1,Nreg
		 JJ=JJ+1
		 IF (Recaps(JJ).GT.0) THEN
		  !NewRecap = IBin1(Recaps(JJ),DetectRate(Iflt,Iyr),ISEED1,1)
		  IF (Recaps(JJ).LT.10) THEN
		   !Ibin is bad for small N so if N<10 do N bernoulli trials!
		   NewRecap=0
		   DO 8894 KK = 1,Recaps(JJ)
		    RanNum = RAN5(ISEEDZ)
			IF (RanNum.LE.DetectRate(Iflt,Iyr)) NewRecap = NewRecap+1
8894	   CONTINUE
		  ELSE
           NewRecap = Recaps(JJ)-IBin2(Recaps(JJ),DetectRate(Iflt,Iyr),ISEEDZ,1)
		  ENDIF
          IF (Diag.EQ.1) WRITE(98,'(200(I6,1x))') Recaps(JJ),NewRecap
		  !IF (NewRecap.GT.0) WRITE(35,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') Iyr,II,Iflt,Ireg,Recaps(JJ) !NewRecap
		  IF (NewRecap.GT.0) WRITE(35,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)') Iyr,II,Iflt,Ireg,NewRecap,INT(EstTagID(II,1))
		 ENDIF
8804   CONTINUE



	  !remove recaptures from tagged population      
	  JJ = 0
	   DO 8805 Iflt=1,Nflt
	    DO 8805 Ireg=1,Nreg
		 JJ=JJ+1
		 IF (Recaps(JJ).GT.0) THEN
          !assign recaptures to stock, sex, and age
		  AssignProbs = 0.d0
		  KK=0
		  DO 8806 Istk=1,Nstk
		   DO 8806 Sex=1,2
            !DO 8806 Age=0,MaxAge
			 Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
             KK=KK+1
!			 AssignProbs(KK) = SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,Age,Iyr)   !!*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
			 AssignProbs(KK) = SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,2,Iyr)   !!*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
8806      CONTINUE
		 AssignProbs = AssignProbs/SUM(AssignProbs)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,200(F6.3,1x))') Recaps(JJ),(AssignProbs(Age),Age=1,KK)
         CALL GENMUL(AssignProbs,Recaps(JJ),NewAssign,ISEEDZ,.FALSE.,1)  !***
		 IF (Diag.EQ.1) WRITE(98,'(200(F6.0,1x))') (NewAssign(Age),Age=1,KK)
		 KK=0	
		 DO 8807 Istk=1,Nstk
		  DO 8807 Sex=1,2
		    Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
           !DO 8807 Age=0,MaxAge
            KK=KK+1
			!IF (Iyr.EQ.2011.AND.II.GE.473.AND.II.LE.485) WRITE(99,*) KK,NewAssign(KK)
		    !Tagged(II,Istk,Ireg,Sex,Age,Iyr) = Tagged(II,Istk,Ireg,Sex,Age,Iyr)-NewAssign(KK)
		    Tagged(II,Istk,Ireg,Sex,2,Iyr) = Tagged(II,Istk,Ireg,Sex,2,Iyr)-NewAssign(KK)
            !IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).LT.0) Tagged(II,Istk,Ireg,Sex,Age,Iyr) = 0.d0
            IF (Tagged(II,Istk,Ireg,Sex,2,Iyr).LT.0) Tagged(II,Istk,Ireg,Sex,2,Iyr) = 0.d0
8807     CONTINUE
        ENDIF

8805   CONTINUE


      
	    IF (Diag.EQ.1) THEN
		 DO 8808 Istk=1,Nstk
		  DO 8808 Ireg=1,Nreg
		  DO 8808 Sex=1,2
!			WRITE(98,'(I4,1x,I4,1x,I2,1x,I2,1x,I2,1x,200(F6.2,1x))') Iyr,II,Istk,Ireg,Sex,(Tagged(II,Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)
			WRITE(98,'(I4,1x,I4,1x,I2,1x,I2,1x,I2,1x,200(F6.2,1x))') Iyr,II,Istk,Ireg,Sex,(Tagged(II,Istk,Ireg,Sex,Age,Iyr),Age=1,2)
8808	 CONTINUE
	    ENDIF  


	  	  
	  
	  ENDIF
8800  CONTINUE

	CLOSE(35)

	  !IF (Iyr.GE.2010) WRITE(99,*) Iyr,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))


	RETURN


	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the tag recapture data 
!
	SUBROUTINE AltGetTagRecaptureData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Istk,Iyr,II,JJ,KK,Age,Iflt,Ireg,Sex,Ilen,Part,ISEED1,ISEED2,LL
	INTEGER NewRecap,NumRecaps,Flag,Recaps(1:Nflt*Nreg),Itemp
	REAL*8 ExpRecaps(0:Nflt,0:Nreg),GammaBit,GammaSD,PoisMu
	REAL*8 RecapProbs(1:Nflt*Nreg),RanNum
    REAL*8 AssignProbs(2000),NewAssign(2000)
    REAL*8 GAMMAZ,POISSN
    REAL*4 RAN5
	INTEGER IBin1,IBin2

	EXTERNAL GAMMAZ,POISSN,RAN5,IBin1,IBin2

	
	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX
	
	!open the output file for appending
    OPEN(UNIT=35,FILE='TagRecaptures.inp',POSITION='APPEND')

	IF (Diag.EQ.1) WRITE(98,'(A8,1x,I4)') 'TagRecap',Iyr

	  !IF (Iyr.GE.2010) WRITE(99,*) Iyr,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))

	!loop over tagging groups
  	 DO 8810 II=1,NTagGroups
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,F7.2)') II,SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr))
	  !IF (EstTagID(II,1).EQ.1) WRITE(99,'(I2,1x,I4,1x,F7.2,1x,F7.2)') II,Iyr,(SUM(Tagged(II,1:Nstk,Ireg,1:2,0:TopAge,Iyr)),Ireg=1,Nreg)
	  IF (SUM(Tagged(II,1:Nstk,1:Nzone,1:2,0:TopAge,Iyr)).GT.0) THEN	 
	   ExpRecaps = 0.d0
	   NumRecaps = 0
		 DO 8812 Istk=1,Nstk
		 DO 8812 Ireg=1,Nreg
 		 DO 8812 Sex=1,2
		 DO 8812 Age=0,MaxAge
		  IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).GT.0) THEN
		   IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).GT.10) THEN
		    Itemp = IBin1(NINT(Tagged(II,Istk,Ireg,Sex,Age,Iyr)),Uage(Istk,Ireg,Sex,Age,Iyr),ISEEDZ,1)
		   ELSE
		    Itemp = 0
			DO JJ=1,NINT(Tagged(II,Istk,Ireg,Sex,Age,Iyr))
			 RanNum = RAN5(ISEEDZ)
			 IF (RanNum.LE.Uage(Istk,Ireg,Sex,Age,Iyr)) Itemp = Itemp+1
			ENDDO
		   ENDIF
           IF (Diag.EQ.1) WRITE(98,*) Tagged(II,Istk,Ireg,Sex,Age,Iyr),Uage(Istk,Ireg,Sex,Age,Iyr),Itemp
		   NumRecaps = NumRecaps + Itemp
		   Tagged(II,Istk,Ireg,Sex,Age,Iyr) = Tagged(II,Istk,Ireg,Sex,Age,Iyr) - FLOAT(Itemp)
		  ENDIF
		  !ExpRecaps(Iflt,Ireg) = ExpRecaps(Iflt,Ireg) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,Age,Iyr) !*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
8812	 CONTINUE		  
		 IF (Diag.EQ.1) WRITE(98,'(I2,1x,I4)') II,NumRecaps
	  IF (NumRecaps.GT.0) THEN
  	  !get proportions by fleet to allocate recaps
	   DO 8811 Iflt=1,Nflt
 	    DO 8811 Ireg=1,Nreg
		 DO 8821 Istk=1,Nstk
 		 DO 8821 Sex=1,2
		 DO 8821 Age=0,MaxAge
 		  ExpRecaps(Iflt,Ireg) = ExpRecaps(Iflt,Ireg) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*Tagged(II,Istk,Ireg,Sex,Age,Iyr)
8821     CONTINUE
!		 ExpRecaps(Iflt,Ireg) = DetectRate(Iflt,Iyr)*ExpRecaps(Iflt,Ireg)
         IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,F7.2)') Iflt,Ireg,ExpRecaps(Iflt,Ireg)
8811   CONTINUE
       ExpRecaps(0,0) = SUM(ExpRecaps(1:Nflt,1:Nreg))
	   !distribute recaptures by fleet and region
	   JJ=0
	   RecapProbs=0
	   Flag=0
	   DO 8814 Iflt=1,Nflt
	    DO 8814 Ireg=1,Nreg
		 JJ=JJ+1
	     RecapProbs(JJ) = ExpRecaps(Iflt,Ireg)/ExpRecaps(0,0)
		 IF (RecapProbs(JJ).EQ.1d0) Flag=JJ
8814   CONTINUE
       IF (Diag.EQ.1) WRITE(98,'(200(F6.2,1x))') (RecapProbs(Age),Age=1,JJ)
	   Recaps=0
       IF (Flag.EQ.0) THEN
	     LL=0
		 Assignprobs=0.d0
		 DO KK=1,JJ
          IF (RecapProbs(KK).GT.0) THEN
           LL=LL+1
		   AssignProbs(LL) = RecapProbs(KK)
		  ENDIF
		 ENDDO
         CALL GENMUL(AssignProbs,NumRecaps,NewAssign,ISEEDZ,.FALSE.,1)
	     !CALL GENMUL(RecapProbs,NumRecaps,Recaps,ISEED1,.FALSE.,1)
		 Recaps=0
		 LL=0
         DO KK=1,JJ
          IF (RecapProbs(KK).GT.0) THEN
           LL=LL+1
		   Recaps(KK) = NewAssign(LL)
		  ENDIF
		 ENDDO
       ENDIF
	   IF (Flag.GT.0) Recaps(Flag) = NumRecaps
       IF (Diag.EQ.1) WRITE(98,'(200(I6,1x))') (Recaps(Age),Age=1,JJ)
	   JJ=0
	   DO 8815 Iflt=1,Nflt
	    DO 8815 Ireg=1,Nreg
		 JJ=JJ+1
		 IF (Recaps(JJ).GT.0) THEN
		  !NewRecap = IBin1(Recaps(JJ),DetectRate(Iflt,Iyr),ISEED1,1)
		  IF (Recaps(JJ).LT.10) THEN
		   !Ibin is bad for small N so if N<10 do N bernoulli trials!
		   NewRecap=0
		   DO 8816 KK = 1,Recaps(JJ)
		    RanNum = RAN5(ISEEDZ)
			IF (RanNum.LE.DetectRate(Iflt,Iyr)) NewRecap = NewRecap+1
8816	   CONTINUE
		  ELSE
           NewRecap = Recaps(JJ)-IBin2(Recaps(JJ),DetectRate(Iflt,Iyr),ISEEDZ,1)
		  ENDIF
          IF (Diag.EQ.1) WRITE(98,'(200(I6,1x))') Recaps(JJ),NewRecap
		  !IF (NewRecap.GT.0) WRITE(35,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') Iyr,II,Iflt,Ireg,Recaps(JJ) !NewRecap
		  IF (NewRecap.GT.0) WRITE(35,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)') Iyr,II,Iflt,Ireg,NewRecap,INT(EstTagID(II,1))
		 ENDIF
8815   CONTINUE


      ENDIF
	    IF (Diag.EQ.1) THEN
		 DO 8818 Istk=1,Nstk
		  DO 8818 Ireg=1,Nreg
		  DO 8818 Sex=1,2
			WRITE(98,'(I4,1x,I4,1x,I2,1x,I2,1x,I2,1x,200(F6.2,1x))') Iyr,II,Istk,Ireg,Sex,(Tagged(II,Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)
8818	 CONTINUE
	    ENDIF  

	  	  
	  
	  ENDIF
8810  CONTINUE

	CLOSE(35)

	  !IF (Iyr.GE.2010) WRITE(99,*) Iyr,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))


	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine generates the data for a given year
!	
!	
    SUBROUTINE GenData(Iyr,Iflag)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,II,Iflag

	IF (Iflag.EQ.1) THEN

	IF (Iyr.GT.Lyear.OR.UseHistData.EQ.0) THEN

!	****fishery catch data****
!	used if want observed retained catch to be uncertain
    CALL GetFishCatchData(Iyr)
    !IF (Iyr.EQ.Lyear) STOP

!	****fishery CPUE data****
	CALL GetFishCPUEData(Iyr)

!	****fishery Discard data****
	CALL GetFishDiscData(Iyr)

	IF (Tier(1).NE.4) THEN

!	****fishery Length data****
	CALL GetFishLengthData(Iyr)

!	****fishery age data****
	CALL GetFishAgeData(Iyr)

!	****survey index****
	CALL GetSurveyIndexData(Iyr)

!	****survey lengths****
	CALL GetSurveyLengthData(Iyr)

!	****survey ages****
	CALL GetSurveyAgeData(Iyr)


	ENDIF

	ENDIF

!	****pseudo Tier 1 assessment****
!	IF (Iyr.GE.Lyear) CALL pseudorefData(Iyr)
!	IF (Iyr.GE.Lyear) CALL pseudorefData2(Iyr)
	IF (Iyr.GE.Lyear) CALL pseudorefData4(Iyr)	
    
	ELSE

!   ****TAGS****

!	****Tag Releases****
!    CALL AltTagReleases(Iyr)
!	CALL TagReleases(Iyr)

	!IF (Iyr.GE.1995) THEN
	 !DO II=1,NTagGroups
	 ! IF (EstTagID(II,1).EQ.1) WRITE(99,*) Iyr,II,SUM(Tagged(II,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))
	 !ENDDO
	 !WRITE(99,*) Iyr,1,SUM(Tagged(1,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))
	!ENDIF

    !IF (Iyr.EQ.2007) WRITE(99,*) 'here'
    IF (Diag.EQ.1) WRITE(98,*) 'got here'
    !first remove 1/2 natural mortality from all tagged fish (prior to adding this year's releases)
    IF (NTagGroups.GT.0) CALL UpdateTags(Iyr,1)
    IF (Diag.EQ.1) WRITE(98,*) 'got here'


!   ****fishery tag recapture data****
    CALL GetTagRecaptureData(Iyr)
!    CALL AltGetTagRecaptureData(Iyr)

!   remove 2nd 1/2 natural mortality from all tagged fish, do mvmt, and increment ages
    IF (NTagGroups.GT.0) CALL UpdateTags(Iyr,2)

	ENDIF


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Get the pseudo ref pts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine updates the age structure for the tagged population 
!
	SUBROUTINE pseudorefData(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Istk
	REAL*8 Temp,NewDev,Temp2,Catchprops(1:10,1:10)
	REAL*8 XNORM

	EXTERNAL XNORM

	Istk=1
	Temp = SpawBio(Istk,0,Iyr)/SBiozero(Istk)
	NewDev = XNORM(5,0.d0,PseudoRefSpecs(1),ISEEDX)
    !WRITE(*,*) Temp,NewDev
	Temp = Temp*EXP(NewDev-0.5*(PseudoRefSpecs(1)**2.d0))
    !WRITE(*,*) Temp

	Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	Temp2 = SUM(Ufleet(1:Nflt,1:Nreg,Iyr)*Catchprops(1:Nflt,1:Nreg))
	!WRITE(*,*) Temp2
	Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2
	NewDev = XNORM(5,0.d0,PseudoRefSpecs(2),ISEEDX)
	!WRITE(*,*) Temp2,NewDev
	Temp2 = Temp2*EXP(NewDev-0.5*(PseudoRefSpecs(2)**2.d0))
    !WRITE(*,*) Temp2

    OPEN(UNIT=39,FILE='estB.inp',POSITION='APPEND')
	WRITE(39,'(I4,1x,F6.3,1x,F7.4)') Iyr,Temp,Temp2
	CLOSE(39)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Get the pseudo ref pts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine generates biomass for a pseudo-assessment
!
	SUBROUTINE pseudorefData2(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Istk,Age,II,Sex,Iflt,u1,u2,u3
	REAL*8 Temp,NewDev,Temp2,Catchprops(1:10,1:10),Temp3,Temp4,Temp5
	REAL*8 Fuse,GetFtarg,pSelAge(1:2,0:100),Muse
	REAL*8 XNORM

	EXTERNAL XNORM
	EXTERNAL GetFtarg


    IF (Diag.EQ.1) WRITE(98,*) 'biomass estimate for pseudoasst for ',Iyr
    !WRITE(*,*) 'prf ',PseudoRefSpecs(1:4)
    
	Istk=1
	Temp = SpawBio(Istk,0,Iyr)   !/SBiozero(Istk)
	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(1),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(1) 
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(1),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(1) = NewDev
	Temp = Temp*EXP(NewDev-0.5*(PseudoRefSpecs(1)**2.d0))
    !WRITE(*,*) Temp


    !Now F
	!Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!Temp2 = SUM(Ufleet(1:Nflt,1:Nreg,Iyr)*Catchprops(1:Nflt,1:Nreg))
	!WRITE(*,*) Temp2
	!Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2

    !2019/07/30
	Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!WRITE(*,*) Catchprops(1:Nflt,1:Nreg)
	!WRITE(*,*) TotalCatch(1:Nflt,1:Nreg,Iyr)
	!WRITE(*,*) RetVBio(1:Nflt,1:Nreg,Iyr)
	Temp2 = SUM(Catchprops(1:Nflt,1:Nreg)*TotalCatch(1:Nflt,1:Nreg,Iyr)/RetVBio(1:Nflt,1:Nreg,Iyr))
	!WRITE(*,*) Temp2
	Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(2),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(2)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(2),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(2) = NewDev
	!WRITE(*,*) Temp2,NewDev
	Temp2 = Temp2*EXP(NewDev-0.5*(PseudoRefSpecs(2)**2.d0))
    !WRITE(*,*) Temp2




    !get the FMSY ref point

    !get specifications
	OPEN(UNIT=13,FILE='flukerefs.ctl')
	IF (HCRspecs(3).EQ.62) THEN
	 CLOSE(13)
	 OPEN(UNIT=13,FILE='fluke-recdisc.ctl')
	ENDIF

	DO II =1,10
	 READ(13,*)
	ENDDO
	READ(13,*) Ftype
	READ(13,*)
	READ(13,*) Muse
	READ(13,*)
	READ(13,*) Fref
	
    CLOSE(13)

	Sex=1
	pSelAge =0.d0
	DO Iflt=1,Nflt
	!WRITE(*,*) pCatch(Iflt)
	DO Age=0,MaxAge
 	 pSelAge(Sex,Age) = pSelAge(Sex,Age) + SelAge(Iflt,1,Sex,Age,Iyr)*SUM(Catchprops(Iflt,1:Nreg))
	ENDDO
	ENDDO

	Temp3 = GetFtarg(Fref,Muse,Iyr,pSelAge,1)
	!WRITE(*,*) Fref,Muse,Temp3

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(4),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(3)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(4),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(3) = NewDev
	!WRITE(*,*) Temp3,NewDev
	Temp3 = Temp3*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp3


    !Recruitment trend
    u1 = Iyr - 2
    u2 = Iyr - 3
    u3 = Iyr - 5
    IF (u1.LE.Fyear) THEN
     u1 = Fyear+1
     u2 = Fyear+1
     u3 = Fyear+1
    ELSEIF (u2.LE.Fyear) THEN
     u2 = Fyear+1
     u3 = Fyear+1
    ELSEIF (u3.LE.Fyear) THEN
     u3 = Fyear+1
    ENDIF

!    Temp4 = (SUM(Recruits(1:Nstk,1:Nreg,(Iyr-2):Iyr))/3) / (SUM(Recruits(1:Nstk,1:Nreg,(Iyr-5):(Iyr-3))/3))
    Temp4 = (SUM(Recruits(1:Nstk,1:Nreg,u1:Iyr))/(Iyr-u1+1)) / (SUM(Recruits(1:Nstk,1:Nreg,u3:u2)/(u2-u3+1)))
    IF (Iyr.EQ.Fyear) Temp4  = 1.d0

    !Biomass trend
    u1 = Iyr - 2
    u2 = Iyr - 3

    IF (Iyr-2.LT.Fyear) THEN
     u1 = Fyear
     u2 = Fyear
    ELSEIF (Iyr-3.LT.Fyear) THEN
     u2 = Fyear
    ENDIF

!    Temp5 = SUM(SpawBio(Istk,0,(Iyr-2):Iyr) / SpawBio(Istk,0,(Iyr-3):(Iyr-1)))
    Temp5 = SUM(SpawBio(Istk,0,u1:Iyr) / SpawBio(Istk,0,u2:(Iyr-1)))
    Temp5 = Temp5/(Iyr-u1+1)

    OPEN(UNIT=39,FILE='estB.inp',POSITION='APPEND')
	WRITE(39,'(I4,1x,F20.4,1x,F7.4,1x,F7.4,1x,F7.4,1x,F7.4)') Iyr,Temp,Temp2,Temp3,Temp4,Temp5
	CLOSE(39)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Get the pseudo assessment
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine generates stock assessment results based on a multivariate normal correlated through time
!   updated 2024-06-13
!
	SUBROUTINE pseudorefData4(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Istk,Age,II,Sex,Iflt,u1,u2,u3,Ilen,Ireg
	REAL*8 Temp,NewDevs(6),Temp2,Catchprops(1:10,1:10),Temp3,Temp4,Temp5,Temp6
	REAL*8 Fuse,GetFtarg,pSelAge(1:2,0:100),Muse,TempVBio, TempSel(NLenBin)
	REAL*8 XNORM, XTEMP(6), MEANS(6), TT(6,6), SG(6), NewDev, GetSPR

	EXTERNAL XNORM
	EXTERNAL GetFtarg
	EXTERNAL GetSPR


    IF (Diag.EQ.1) WRITE(98,*) 'biomass estimate for pseudoasst for ',Iyr
    !WRITE(*,*) 'prf ',PseudoRefSpecs(1:4)
    
    MEANS = 0.d0
    SG = PseudoRefSpecs(4)
    TT = 0.d0
    DO II = 1,5
     TT(II,II) = 1.d0
    ENDDO
    TT(1,2) = -0.75
    TT(2,1) = -0.75
    TT(1,4) = 0.7
    TT(4,1) = 0.7
    TT(1,5) = 0.85
    TT(5,1) = 0.85
    TT(1,6) = 0.35
    TT(6,1) = 0.35
    TT(2,3) = 0.95
    TT(3,2) = 0.95
    TT(2,4) = -0.5
    TT(4,2) = -0.5
    TT(2,5) = -0.7
    TT(5,2) = -0.7
    TT(2,6) = 0.1
    TT(6,2) = 0.1
    TT(4,5) = 0.7
    TT(5,4) = 0.7
    TT(4,6) = 0.3
    TT(6,4) = 0.3
    TT(5,6) = 0.2
    TT(6,5) = 0.2
    !WRITE(*,*) TT
    !STOP

!    GenMult(VEC,MEANS,ISEED,NPARS,TT,SG,MPA)
    CALL GenMult(XTEMP, MEANS, ISEEDX, 6, TT, SG, 6)
    IF (Iyr.EQ.2020) WRITE(*,*) XTEMP
    !WRITE(*,*) SG
    !IF (Iyr.EQ.2020) STOP

	Istk=1
	Temp = SpawBio(Istk,0,Iyr)   !/SBiozero(Istk)
	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(1) 
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(1) 
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(1)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(1) = NewDev
	Temp = Temp*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp

    IF (Iyr.EQ.2020) WRITE(*,*) Istk

    !Now F
	!Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!Temp2 = SUM(Ufleet(1:Nflt,1:Nreg,Iyr)*Catchprops(1:Nflt,1:Nreg))
	!WRITE(*,*) Temp2
	!Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2

    !2019/07/30
	Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!WRITE(*,*) Catchprops(1:Nflt,1:Nreg)
	!WRITE(*,*) TotalCatch(1:Nflt,1:Nreg,Iyr)
	!WRITE(*,*) RetVBio(1:Nflt,1:Nreg,Iyr)

    TempVBio = 0.d0
    TempSel = 0.d0
	DO Iflt=1,Nflt
	 DO Ilen=1,Nlen
	  TempSel(Ilen) = TempSel(Ilen) + SUM(Catchprops(Iflt,1:Nreg))*Sellen(Iflt,Ilen,Iyr)
	 ENDDO
	ENDDO
	 DO Ireg=1,Nreg
	  DO Istk=1,Nstk
	   DO Sex=1,2
	    DO Age=0,MaxAge
  		  DO Ilen=1,Nlen
		   TempVBio = TempVBio + WtLen(Ilen,Istk,Sex)*TempSel(Ilen)*Fraclen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
!		   IF (Age.EQ.0) Temp = Temp*SelAge(Iflt,Istk,Sex,Age,Iyr)
!		  WRITE(98,*) Year,Iflt,Sex,Age
!		  WRITE(98,*) 
     ENDDO
     ENDDO
     ENDDO
     ENDDO
     ENDDO
    !WRITE(*,*) TempSel
    !WRITE(*,*) TempVBio

	!Temp2 = SUM(Catchprops(1:Nflt,1:Nreg)*TotalCatch(1:Nflt,1:Nreg,Iyr)/RetVBio(1:Nflt,1:Nreg,Iyr))
    Temp2 = SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))/TempVBio
	!WRITE(*,*) Temp2
	Temp2 = -1.d0*LOG(1.d0-Temp2)
	IF (Iyr.EQ.2020) WRITE(*,*) Temp2
	TrueF(Iyr) = Temp2

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(2)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(2)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(2)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(2) = NewDev
	!WRITE(*,*) Temp2,NewDev
	Temp2 = Temp2*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp2


    Istk=1

    !get the FMSY ref point

    !get specifications
	OPEN(UNIT=13,FILE='flukerefs.ctl')
	IF (HCRspecs(3).GE.62) THEN
	 CLOSE(13)
	 OPEN(UNIT=13,FILE='fluke-recdisc.ctl')
	ENDIF

	DO II =1,10
	 READ(13,*)
	ENDDO
	READ(13,*) Ftype
	READ(13,*)
	READ(13,*) Muse
	READ(13,*)
	READ(13,*) Fref
	
    CLOSE(13)

	Sex=1
	pSelAge =0.d0
	DO Iflt=1,Nflt
	!WRITE(*,*) pCatch(Iflt)
	DO Age=0,MaxAge
 	 pSelAge(Sex,Age) = pSelAge(Sex,Age) + SelAge(Iflt,1,Sex,Age,Iyr)*SUM(Catchprops(Iflt,1:Nreg))
	ENDDO
	ENDDO

	Temp3 = GetFtarg(Fref,Muse,Iyr,pSelAge,1)
	Temp6 = Rzero(1)*GetSPR(Temp3,Muse,pSelAge,Iyr,1)
	!IF (Iyr.EQ.2020) THEN
	! WRITE(*,*) Fref,Muse,Temp3,Temp6
    ! STOP
    !ENDIF

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(3)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(3)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(3)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(3) = NewDev
	!WRITE(*,*) Temp3,NewDev
	Temp3 = Temp3*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp3


    !Recruitment trend
    u1 = Iyr - 2
    u2 = Iyr - 3
    u3 = Iyr - 5
    IF (u1.LE.Fyear) THEN
     u1 = Fyear+1
     u2 = Fyear+1
     u3 = Fyear+1
    ELSEIF (u2.LE.Fyear) THEN
     u2 = Fyear+1
     u3 = Fyear+1
    ELSEIF (u3.LE.Fyear) THEN
     u3 = Fyear+1
    ENDIF

!    Temp4 = (SUM(Recruits(1:Nstk,1:Nreg,(Iyr-2):Iyr))/3) / (SUM(Recruits(1:Nstk,1:Nreg,(Iyr-5):(Iyr-3))/3))
    Temp4 = (SUM(Recruits(1:Nstk,1:Nreg,u1:Iyr))/(Iyr-u1+1)) / (SUM(Recruits(1:Nstk,1:Nreg,u3:u2)/(u2-u3+1)))
    IF (Iyr.EQ.Fyear) Temp4  = 1.d0


	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(4)
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(4)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(4)
    ENDIF
    LastDev(4) = NewDev
	Temp4 = Temp4*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))

    !Biomass trend

!    Temp5 = SUM(SpawBio(Istk,0,(Iyr-2):Iyr) / SpawBio(Istk,0,(Iyr-3):(Iyr-1)))
    IF (Iyr.GE.(Lyear+3)) Temp5 = SUM(SpawBio(Istk,0,(Iyr-2):Iyr) / SpawBio(Istk,0,(Iyr-3):(Iyr-1))) / 3.d0
    IF (Iyr.EQ.(Lyear+2)) Temp5 = SUM(SpawBio(Istk,0,(Iyr-1):Iyr) / SpawBio(Istk,0,(Iyr-2):(Iyr-1))) / 2.d0
    IF (Iyr.EQ.(Lyear+1)) Temp5 = SpawBio(Istk,0,Iyr) / SpawBio(Istk,0,Iyr-1)
    IF (Iyr.EQ.Lyear) Temp5 = 1.d0

!    Temp5 = SUM(SpawBio(Istk,0,u1:Iyr) / SpawBio(Istk,0,u2:u3))
!    Temp5 = Temp5/(Iyr-u1+1)

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(5)
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(5)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(5)
    ENDIF
    LastDev(5) = NewDev
	Temp5 = Temp5*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))

    
    ! BMSY
	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XTEMP(6)
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(6)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XTEMP(6)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(6) = NewDev
	!WRITE(*,*) Temp3,NewDev
	Temp6 = Temp6*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp6


    OPEN(UNIT=39,FILE='estB.inp',POSITION='APPEND')
	WRITE(39,'(I4,1x,F16.4,1x,F7.4,1x,F7.4,1x,F7.4,1x,F7.4,1x,F16.4)') Iyr,Temp,Temp2,Temp3,Temp4,Temp5,Temp6
	CLOSE(39)

    !STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Get the pseudo ref pts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine generates biomass for a OFL-based pseudo-assessment
!
	SUBROUTINE pseudorefData3(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Istk,Age,II,Sex,Iflt
	REAL*8 Temp,NewDev,Temp2,Catchprops(1:10,1:10),Temp3
	REAL*8 Fuse,GetFtarg,pSelAge(1:2,0:100),Muse
	REAL*8 XNORM

	EXTERNAL XNORM
	EXTERNAL GetFtarg


    IF (Diag.EQ.1) WRITE(98,*) 'biomass estimate for pseudoasst for ',Iyr
    !WRITE(*,*) 'prf ',PseudoRefSpecs(1:4)
    
	Istk=1
	Temp = SpawBio(Istk,0,Iyr)   !/SBiozero(Istk)
	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(1),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(1) 
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(1),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(1) = NewDev
	Temp = Temp*EXP(NewDev-0.5*(PseudoRefSpecs(1)**2.d0))
    !WRITE(*,*) Temp


    !Now F
	!Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!Temp2 = SUM(Ufleet(1:Nflt,1:Nreg,Iyr)*Catchprops(1:Nflt,1:Nreg))
	!WRITE(*,*) Temp2
	!Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2

    !2019/07/30
	Catchprops(1:Nflt,1:Nreg) = TotalCatch(1:Nflt,1:Nreg,Iyr)/SUM(TotalCatch(1:Nflt,1:Nreg,Iyr))
	!WRITE(*,*) Catchprops(1:Nflt,1:Nreg)
	!WRITE(*,*) TotalCatch(1:Nflt,1:Nreg,Iyr)
	!WRITE(*,*) RetVBio(1:Nflt,1:Nreg,Iyr)
	Temp2 = SUM(Catchprops(1:Nflt,1:Nreg)*TotalCatch(1:Nflt,1:Nreg,Iyr)/RetVBio(1:Nflt,1:Nreg,Iyr))
	!WRITE(*,*) Temp2
	Temp2 = -1.d0*LOG(1.d0-Temp2)
	!WRITE(*,*) Temp2

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(2),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(2)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(2),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(2) = NewDev
	!WRITE(*,*) Temp2,NewDev
	Temp2 = Temp2*EXP(NewDev-0.5*(PseudoRefSpecs(2)**2.d0))
    !WRITE(*,*) Temp2




    !get the FMSY ref point

    !get specifications
	OPEN(UNIT=13,FILE='flukerefs.ctl')
	IF (HCRspecs(3).EQ.62) THEN
	 CLOSE(13)
	 OPEN(UNIT=13,FILE='fluke-recdisc.ctl')
	ENDIF

	DO II =1,10
	 READ(13,*)
	ENDDO
	READ(13,*) Ftype
	READ(13,*)
	READ(13,*) Muse
	READ(13,*)
	READ(13,*) Fref
	
    CLOSE(13)

	Sex=1
	pSelAge =0.d0
	DO Iflt=1,Nflt
	!WRITE(*,*) pCatch(Iflt)
	DO Age=0,MaxAge
 	 pSelAge(Sex,Age) = pSelAge(Sex,Age) + SelAge(Iflt,1,Sex,Age,Iyr)*SUM(Catchprops(Iflt,1:Nreg))
	ENDDO
	ENDDO

	Temp3 = GetFtarg(Fref,Muse,Iyr,pSelAge,1)
	!WRITE(*,*) Fref,Muse,Temp3

	IF (Iyr.EQ.Lyear) THEN
	 NewDev = XNORM(5,0.d0,PseudoRefSpecs(4),ISEEDX)
	 !WRITE(*,*) Temp,NewDev
	ELSE
	 NewDev = PseudoRefSpecs(3)*LastDev(3)
	 NewDev = NewDev + SQRT(1.d0-(PseudoRefSpecs(3)**2.d0))*XNORM(5,0.d0,PseudoRefSpecs(4),ISEEDX)
     !WRITE(*,*) Temp,NewDev,LastDev
    ENDIF
    LastDev(3) = NewDev
	!WRITE(*,*) Temp3,NewDev
	Temp3 = Temp3*EXP(NewDev-0.5*(PseudoRefSpecs(4)**2.d0))
    !WRITE(*,*) Temp3


    OPEN(UNIT=39,FILE='estB.inp',POSITION='APPEND')
	WRITE(39,'(I4,1x,F20.4,1x,F7.4,1x,F7.4)') Iyr,Temp,Temp2,Temp3
	CLOSE(39)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Update tagged numbers at age
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine updates the age structure for the tagged population 
!
	SUBROUTINE UpdateTags(Iyr,Flag)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Ireg,Jreg,Age,Sex,Istk,Iyr,Flag,KK
	REAL*8 Ntemp(Nreg,0:MaxAge),Ntemp2(Nreg,0:MaxAge),Nalive,Ndie,RanNum,Nmove(1:Nreg)
	REAL*8 AssignProbs(2000),NewAssign(2000)
	INTEGER Nsurvive
    REAL*4 RAN5
	INTEGER IBin1,IBin2

	EXTERNAL RAN5,IBin1,IBin2

	Ntemp = 0.d0
	Ntemp2 = 0.d0

	!IF (Iyr.GE.2010) WRITE(99,*) Iyr,Flag,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))

    IF (Flag.EQ.1) THEN
!	 first half-year's mortality
	 DO 9020 II=1,NTagGroups
	 DO 9020 Istk=1,Nstk	  
	   DO 9020 Ireg=1,Nreg	    
 	    DO 9020 Sex=1,2
!		  DO 9020 Age= 0,MaxAge
		   Nalive = Tagged(II,Istk,Ireg,Sex,Age,Iyr)
		   Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
!		   Tagged(II,Istk,Ireg,Sex,0:MaxAge,Iyr) = Tagged(II,Istk,Ireg,Sex,0:MaxAge,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,0:MaxAge,Iyr))
		   Tagged(II,Istk,Ireg,Sex,2,Iyr) = Tagged(II,Istk,Ireg,Sex,2,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
!			IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).GE.10) THEN
!			 Nsurvive = Ibin2(Nalive,EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr)),ISEEDZ,1)
!		     Tagged(II,Istk,Ireg,Sex,Age,Iyr) = Nsurvive
!			ELSE
!			 Nsurvive = 0
!			 DO KK=1,NINT(Nalive)
!			  RanNum = RAN5(ISEEDZ)
!			  IF(RanNum.LE.EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))) Nsurvive=Nsurvive+1
!			 ENDDO
!			 Tagged(II,Istk,Ireg,Sex,Age,Iyr) = Nsurvive
!			ENDIF
		   IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).LT.0) Tagged(II,Istk,Ireg,Sex,Age,Iyr) = 0.d0
9020 CONTINUE	    
	ENDIF

    !IF (Iyr.GE.2010) WRITE(99,*) Iyr,Flag,SUM(Tagged(459:471,1:Nstk,1:Nreg,1:2,0:MaxAge,Iyr))

	IF (Flag.EQ.2) THEN
!   2nd half year's mortality, movement and increment ages
!	update numbers at age
     DO 9021 II=1,NTagGroups
	 DO 9021 Istk=1,Nstk	  
	  DO 9021 Sex=1,2
	
!	   first apply mortality
!	   equation 1.3	   
	   DO 9022 Ireg=1,Nreg	    
!		DO 9022 Age=0,MaxAge
		  Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
!         Ntemp(Ireg,Age) = Tagged(II,Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
          Ntemp(Ireg,1) = Tagged(II,Istk,Ireg,Sex,2,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
!          Ntemp(Ireg,Age) = Tagged(II,Istk,Ireg,Sex,Age,Iyr)*(1.d0-Uage(Istk,Ireg,Sex,Age,Iyr))*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
!		  Nalive = Tagged(II,Istk,Ireg,Sex,Age,Iyr)
		  Nalive = Tagged(II,Istk,Ireg,Sex,2,Iyr)
!		  IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr).GE.10) THEN
!		   Nsurvive = Ibin2(Nalive,EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr)),ISEEDZ,1)
!		   Ntemp(Ireg,Age) = Nsurvive
!	   	  ELSE
!		   Nsurvive = 0
!		   DO KK=1,NINT(Nalive)
!		    RanNum = RAN5(ISEEDZ)
!		    IF(RanNum.LE.EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))) Nsurvive=Nsurvive+1
!		   ENDDO
!		   Ntemp(Ireg,Age) = Nsurvive
!		  ENDIF
9022	CONTINUE	    

!	   movement
!	   equation 1.2
	   Ntemp2(1:Nreg,0:MaxAge) = 0.d0
!	   DO 9023 Age=0,MaxAge
		!WRITE(99,'(I2,1x,F6.1,1x,F6.1)') Age,Ntemp(1:Nreg,Age)
!		IF (SUM(Ntemp(1:Nreg,Age)).GT.0) THEN
		IF (SUM(Ntemp(1:Nreg,1)).GT.0) THEN
		DO 9024 Ireg=1,Nreg		 
  	     Age = INT(Tagged(II,Istk,Ireg,Sex,1,Iyr))
		 AssignProbs = 0.d0
		 AssignProbs(1:Nreg) = X(Istk,Ireg,1:Nreg,Sex,Age,Iyr)
!		 CALL GENMUL(AssignProbs,NINT(Ntemp(Ireg,Age)),NewAssign,ISEEDZ,.FALSE.,1) 
		 CALL GENMUL(AssignProbs,NINT(Ntemp(Ireg,1)),NewAssign,ISEEDZ,.FALSE.,1) 
!		 Ntemp2(1:Nreg,Age) = Ntemp2(1:Nreg,Age) + NewAssign(1:Nreg)
		 Ntemp2(1:Nreg,1) = Ntemp2(1:Nreg,1) + NewAssign(1:Nreg)
9024    CONTINUE
		!WRITE(99,'(I2,1x,F6.1,1x,F6.1)') Age,Ntemp2(1:Nreg,Age)
		ELSE
!		 Ntemp2(1:Nreg,Age) = Ntemp(1:Nreg,Age)
		 Ntemp2(1:Nreg,1) = Ntemp(1:Nreg,1)
        ENDIF
!9023   CONTINUE
		
!		Ntemp2 = Ntemp
		 
!	   increment ages and store
	   DO 9025 Ireg=1,Nreg
!	    DO 9026 Age=1,MaxAge
!	     Tagged(II,Istk,Ireg,Sex,Age,Iyr+1) = Ntemp2(Ireg,Age-1)
	     Tagged(II,Istk,Ireg,Sex,2,Iyr+1) = Ntemp2(Ireg,1)
	     Tagged(II,Istk,Ireg,Sex,1,Iyr+1) = Tagged(II,Istk,Ireg,Sex,1,Iyr)+1
!		 IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr+1).LT.0) Tagged(II,Istk,Ireg,Sex,Age,Iyr+1) = 0.d0
		 IF (Tagged(II,Istk,Ireg,Sex,2,Iyr+1).LT.0) Tagged(II,Istk,Ireg,Sex,2,Iyr+1) = 0.d0
		 !IF (Tagged(II,Istk,Ireg,Sex,Age,Iyr+1).LT.1) Tagged(II,Istk,Ireg,Sex,Age,Iyr+1) = 0.d0
         IF (Tagged(II,Istk,Ireg,Sex,1,Iyr+1).GT.MaxAge) Tagged(II,Istk,Ireg,Sex,1,Iyr+1) = FLOAT(MaxAge)
!9026	CONTINUE
!		plus group
!	    Tagged(II,Istk,Ireg,Sex,MaxAge,Iyr+1) = Tagged(II,Istk,Ireg,Sex,MaxAge,Iyr+1) + Ntemp2(Ireg,MaxAge)
!		IF (Tagged(II,Istk,Ireg,Sex,MaxAge,Iyr+1).LT.0) Tagged(II,Istk,Ireg,Sex,MaxAge,Iyr+1) = 0.d0
9025   CONTINUE


9021 CONTINUE
    ENDIF

    RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Get Regional tagging release numbers
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine inputs the tag releases for the current year

	SUBROUTINE GetRegTagReleases(Iyr,Iflt)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Sex,Istk,Iflt,Ireg,Iyr,JJ,ISEED1,ISEED2
	REAL*8 PropVec(2000),NewVec(2000)
	
	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

    !What to do depends on type of input
    IF (TagRelFlag(1,Iflt).EQ.1) NTagRelease(Iflt,0,Iyr) = TagRelFlag(2,Iflt)
    IF (TagRelFlag(1,Iflt).EQ.2) NTagRelease(Iflt,0,Iyr) = TagRelFlag(2,Iflt)*SUM(TotalCatch(Iflt,1:Nreg,Iyr))

    IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I6)') Iyr,Iflt,0,NTagRelease(Iflt,0,Iyr)

    !Get catch (in numbers) proportions by region
	PropVec = 0.d0
	 DO 9010 Istk=1,Nstk
	  DO 9010 Sex=1,2
       DO 9010 Age=0,MaxAge
	    PropVec(1:Nreg) = PropVec(1:Nreg) + Ufleet(Iflt,1:Nreg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)*N(Istk,1:Nreg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,1:Nreg,Sex,Age,Iyr))
9010 CONTINUE
     PropVec = PropVec/SUM(PropVec)
	 !allocate total tags by region
	 NewVec = 0.d0	 
     CALL GENMUL(PropVec,NTagRelease(Iflt,0,Iyr),NewVec,ISEEDX,.FALSE.,1)
	 DO 9011 Ireg=1,Nreg
	  NTagRelease(Iflt,Ireg,Iyr) = NewVec(Ireg)
	  IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I6)') Iyr,Iflt,Ireg,NTagRelease(Iflt,Ireg,Iyr) 
9011 CONTINUE


    RETURN

	END	 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the tagged 'subgroup' release numbers
    SUBROUTINE GetSubGroupReleases(Iyr,Iflt,Ireg,SubGroupNumbers,NumSubGroups,Lowlen,Toplen)

    IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,Iyr,Nsubgroups,DUM(6),II,Iflag,NumSubGroups,JJ,KK
	INTEGER TempProps(2*Nlen),Nobs,Ilen,Toplen(100),Lowlen(100)
	REAL*8 Cuts(100),LenProps(Nlen),LenProps2(Nlen),GuessGrowthpars(3),SubGroupNumbers(100),Temp


	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*)
	READ(18,*)
	READ(18,*)
    DO II=1,16 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	READ(18,*)
	DO II=1,3
	 READ(18,*) GuessGrowthpars(II)
	ENDDO
	CLOSE(18)


	SubGroupNumbers = 0

    IF (EstTagAge(Iflt).GT.-1) THEN
	 SubGroupNumbers(1) = 1.d0
	 NumSubGroups = 1
	 RETURN
	ENDIF
	IF (EstTagAge(Iflt).EQ.-101) THEN
     RETURN
	ELSE
	 Nsubgroups = ABS(EstTagAge(Iflt))
	 IF (Nsubgroups.GT.Nlen) Nsubgroups = Nlen
	 OPEN(UNIT=15,FILE='FisheryLengths.inp')
     LenProps = 0	 
	 READ(15,*)
5000 READ(15,*,END=5001,ERR=5001) (DUM(II),II=1,6),(TempProps(II),II=1,2*Nlen)
	 IF (DUM(1).EQ.Iyr.AND.DUM(3).EQ.Iflt.AND.DUM(2).EQ.Ireg) THEN
      LenProps(1:Nlen) = LenProps(1:Nlen) + TempProps(1:Nlen) + TempProps((Nlen+1):(2*Nlen))
     ENDIF
	 GOTO 5000
5001 CONTINUE
     CLOSE(15)

    IF (Nsubgroups.EQ.Nlen) THEN
	 LenProps = LenProps/SUM(LenProps)
     NumSubGroups = 0
	 DO Ilen=1,Nlen
	  IF (LenProps(Ilen).GT.0) THEN
	   NumSubGroups = NumSubgroups + 1
	   SubGroupNumbers(NumSubGroups) = LenProps(Ilen)
	   mulenTagged(NTagGroups+NumSubGroups) = lolenBin(Ilen)+(hilenBin(Ilen)-lolenBin(Ilen))/2.d0
	   GuessTaggedAge(NTagGroups+NumSubGroups) = INT(GuessGrowthpars(3)-LOG(1.d0-mulenTagged(NTagGroups+NumSubGroups)/GuessGrowthpars(1))/GuessGrowthpars(2))
      ENDIF
     ENDDO
	 SubGroupNumbers(1:NumSubGroups) = SubGroupNumbers(1:NumSubGroups)/SUM(SubGroupNumbers(1:NumSubGroups))
    ELSE
	 LenProps2 = LenProps
     Nobs = SUM(LenProps)
	 !WRITE(*,*) Nobs
	 !WRITE(*,*) LenProps(1:Nlen)
	 DO Ilen=2,Nlen
	  LenProps(Ilen) = LenProps(Ilen-1)+LenProps(Ilen)
	 ENDDO
	 LenProps = LenProps/LenProps(Nlen)
     !WRITE(*,*) LenProps(1:Nlen)
     Cuts=0.d0
	 Cuts(1) = 1.d0/Nsubgroups
	 DO II=2,Nsubgroups
	  Cuts(II) = Cuts(II-1)+1.d0/Nsubgroups
	 ENDDO
     !WRITE(*,*) (Cuts(II),II=1,Nsubgroups)
	 DO II=1,Nsubgroups
	  DO Ilen=Nlen,1,-1
	   IF (LenProps(Ilen).GE.Cuts(II)) SubGroupNumbers(II) = LenProps(Ilen)
	  ENDDO
	 ENDDO
     !WRITE(*,*) (SubGroupNumbers(II),II=1,Nsubgroups)
	 DO II=Nsubgroups,2,-1
	  SubGroupNumbers(II) = SubGroupNumbers(II)-SubGroupNumbers(II-1)
	 ENDDO

     !WRITE(*,*) (SubGroupNumbers(II),II=1,Nsubgroups)


     DO II=1,Nsubgroups
      
	 ENDDO
     TopLen(Nsubgroups)=Nlen
     DO II=1,Nsubgroups-1
      DO Ilen=Nlen,1,-1
	   IF (LenProps(Ilen).GE.Cuts(II)) TopLen(II)=Ilen
	  ENDDO
     ENDDO
	 LowLen = 1
	 DO II=2,Nsubgroups
	  LowLen(II) = TopLen(II-1)+1
	 ENDDO
	 !WRITE(*,*) (LowLen(II),II=1,Nsubgroups)
	 !WRITE(*,*) (TopLen(II),II=1,Nsubgroups)
	 DO II=1,Nsubgroups
	  mulenTagged(NTagGroups+II) = 0.d0
	  Temp = 0.d0
	  DO Ilen=Lowlen(II),Toplen(II)
	   Temp = Temp + LenProps2(Ilen)/(Nobs*SubGroupNumbers(II))
	   !WRITE(*,*) II,Ilen,Temp
	   IF (Temp.GE.0.5d0) THEN
	    mulenTagged(NTagGroups+II) = lolenBin(Ilen)+(hilenBin(Ilen)-lolenBin(Ilen))/2.d0
		!WRITE(*,*) NTagGroups+II,mulenTagged(NTagGroups+II)
		GOTO 5002
       ENDIF
!	   mulenTagged(NTagGroups+II) = mulenTagged(NTagGroups+II) + (lolenBin(lowlen(II))+(hilenBin(toplen(II))-lolenBin(lowlen(II)))/2.d0)*LenProps2(Ilen)
      ENDDO
5002 CONTINUE
!	  mulenTagged(NTagGroups+II) = mulenTagged(NTagGroups+II)/(Nobs*SubGroupNumbers(II))
	 ENDDO
	 DO II=1,Nsubgroups
	  GuessTaggedAge(NTagGroups+II) = INT(GuessGrowthpars(3)-LOG(1.d0-mulenTagged(NTagGroups+II)/GuessGrowthpars(1))/GuessGrowthpars(2))
	 ENDDO
	 DO II=2,Nsubgroups
      IF (GuessTaggedAge(NTagGroups+II).EQ.GuessTaggedAge(NTagGroups+II-1)) THEN
       SubGroupNumbers(II-1) = SubGroupNumbers(II-1) + SubGroupNumbers(II)
       SubGroupNumbers(II) = 0.d0
	   Toplen(II-1) = Toplen(II)
      ENDIF
	 ENDDO
	 NumSubGroups=0
	 DO II=1,Nsubgroups
	  IF (SubGroupNumbers(II).GT.0) THEN
       NumSubGroups = NumSubGroups+1
       SubGroupNumbers(NumSubGroups) = SubGroupNumbers(II)
	   Lowlen(NumSubGroups) = Lowlen(II)
	   Toplen(NumSubGroups) = Toplen(II)
       GuessTaggedAge(NTagGroups+NumSubGroups) = GuessTaggedAge(NTagGroups+II)
       mulenTagged(NTagGroups+NumSubGroups) = mulenTagged(NTagGroups+II)
	  ENDIF
	 ENDDO
	 !DO II=1,Nsubgroups
	  !IF (SubGroupNumbers(II).GT.0) NumSubGroups = NumSubGroups +1
	 !ENDDO
	 !STOP
    ENDIF

	ENDIF 

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	NEW TAGGING RELEASES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine inputs the releases for the current year
!
!	UPDATED 19th October 2011
!
	SUBROUTINE AltTagReleases(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Sex,Istk,Iflt,Ireg,Iyr,JJ,ISEED1,ISEED2,NumSubGroups,Lowlen(100),Toplen(100),Ivec(2000),Jage
	INTEGER Igroup,Ilen,II,Nestgroups,Maxgp,Jlen,Last,Itemp
	REAL*8 PropVec(2000),NewVec(2000),SubGroupNumbers(100),AgeVec(1000),LenVec(2000),CumVec(2000),Temp
	REAL*8 GuessGrowthpars(3),TempVec(0:2000)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!open the output file for appending
    OPEN(UNIT=37,FILE='TagReleases.inp',POSITION='APPEND')


          	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*)
	READ(18,*)
	READ(18,*)
    DO II=1,16 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	READ(18,*)
	DO II=1,3
	 READ(18,*) GuessGrowthpars(II)
	ENDDO
	CLOSE(18)
    !WRITE(99,*) GuessGrowthpars(1:3)


	 !Releases, 
 	 !DO 9000 Iflt=1,Nflt
	 DO Iflt=1,Nflt
     !1st check to see if any in this year
	  IF (TagRelYrs(Iflt,Iyr).EQ.1) THEN
	   !allocate tag releases by region based on catch proportions (overdisperse?)
	   CALL GetRegTagReleases(Iyr,Iflt)
      ENDIF

	 ENDDO
	   !loop over region
	   DO 9001 Ireg=1,Nreg 
	   !{
	   ! if tag releases, 
	    DO 9001 Iflt=1,Nflt
	    LenVec = 0.d0
	    IF (NTagRelease(Iflt,Ireg,Iyr).GT.0) THEN

        IF (TagRelAge(Iflt).EQ.-1) THEN
		 
		 PropVec = 0.d0
	     DO 9007 Ilen=1,Nlen
		  DO 9007 Istk=1,Nstk
 		   DO 9007 Sex=1,2
		    DO 9007 Age=0,MaxAge		
             PropVec(Ilen) = PropVec(Ilen) + SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9007     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,(PropVec(Age),Age=1,Nlen)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,NTagRelease(Iflt,Ireg,Iyr),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,(NewVec(Age),Age=1,Nlen)
         LenVec = LenVec + NewVec

		 ENDIF
		 ENDIF
	
9000	 CONTINUE

	    IF (SUM(LenVec).GT.0) THEN

         IVec(1) = 0
		 DO Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) IVec(1)=IVec(1)+1
		 ENDDO
		 !Iflt = 1
		 IF (EstTagAge(Iflt).GT.0) Nestgroups = 1
		 IF (EstTagAge(Iflt).LT.0) Nestgroups = INT(-1*EstTagAge(Iflt))
		 IF (Nestgroups.GT.IVec(1)) Nestgroups = IVec(1)

    !     WRITE(99,*) NTagRelease(Iflt,Ireg,Iyr)
         Temp = NTagRelease(Iflt,Ireg,Iyr) / Nestgroups
		 !Temp = SUM(NTagRelease(1:Nflt,Ireg,Iyr)) / Nestgroups
         CumVec(1) = LenVec(1)
		 DO Ilen=2,Nlen
!		  CumVec(Ilen) = CumVec(Ilen-1) + NewVec(Ilen)
		  CumVec(Ilen) = CumVec(Ilen-1) + LenVec(Ilen)
		 ENDDO
         Ivec=0
	!	 WRITE(99,*) Temp
		 Last = 0
		 DO Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) THEN
		   IF (CumVec(Ilen)/Temp.GT.FLOAT(Last)) Last=Last+1
		   IF (Last.GT.Nestgroups) Last = Nestgroups
		   Ivec(Ilen) = Last
!		   Itemp = ANINT(CumVec(Ilen)/Temp)
 !          IF (Itemp.GT.Last) Last = Last+1
!		   Ivec(Ilen) = Last+1
		  ENDIF
		  IF (Iyr.EQ.2010) WRITE(99,*) Iflt,LenVec(Ilen),CumVec(Ilen),IVec(Ilen)
		 ENDDO
 
		 
		 MaxGp = 0
         DO 9009 Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) THEN

	   !allocate tags to stock,sex, and ages
		 PropVec = 0.d0
		 JJ=0	     
		 DO 9012 Age=0,MaxAge
	     DO 9012 Sex=1,2
          DO 9012 Istk=1,Nstk
		    JJ=JJ+1
            PropVec(JJ) = PropVec(JJ) + FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9012     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I3,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,Ilen,(PropVec(Age),Age=1,JJ)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,INT(LenVec(Ilen)),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I3,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,Ilen,(NewVec(Age),Age=1,JJ)
		 JJ=0
		 TempVec = 0.d0	     
  	     DO 9013 Age=0,MaxAge
		  DO 9013 Sex=1,2
		   DO 9013 Istk=1,Nstk
		    JJ=JJ+1
            TempVec(Age) = TempVec(Age) + NewVec(JJ)
9013      CONTINUE 
		 JJ=0
		 DO 9014 Age=0,MaxAge
		   IF (TempVec(Age).GT.0) THEN
		    NTagGroups=NTagGroups+1							
		    EstTagID(NTagGroups,1) = NGuessGroups + IVec(Ilen)
            !WRITE(*,*) Age,NTagGroups,TempVec(Age)
		   IF (Ivec(Ilen).GT.MaxGp) MaxGp = IVec(Ilen)
           IF ((lolenbin(Ilen)+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0).GE.GuessGrowthpars(1)) THEN
		    EstTagID(NTagGroups,2) = MaxAge
		   ELSE
		    EstTagID(NTagGroups,2) = GuessGrowthpars(3)-LOG(1.d0-(lolenbin(Ilen)+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0)/GuessGrowthpars(1))/GuessGrowthpars(2)
		   ENDIF
		   IF (EstTagAge(Iflt).GT.0) EstTagID(NTagGroups,2) = EstTagAge(Iflt)
		   EstTagID(NTagGroups,3) = Ireg
		   !WRITE(*,*) Iyr,Iflt,NGuessGroups,Ivec(Ilen),LenVec(Ilen)
		   GroupReleases(NGuessGroups + IVec(Ilen)) = GroupReleases(NGuessGroups + IVec(Ilen)) + ANINT(TempVec(Age))  !ANINT(LenVec(Ilen))
		   GuessTaggedAge(NGuessGroups + IVec(Ilen)) = GuessTaggedAge(NGuessGroups + IVec(Ilen)) + TempVec(Age)*EstTagID(NTagGroups,2)  !LenVec(Ilen)*EstTagID(NTagGroups,2)
		   GuessRegion(NGuessGroups + IVec(Ilen)) = Ireg
	   
	   
           ENDIF
		   DO 9015 Sex=1,2
		    DO 9015 Istk=1,Nstk
			 JJ=JJ+1
		     IF (NewVec(JJ).GT.0) THEN
			  !Tagged(NTagGroups,Istk,Ireg,Sex,Age,Iyr) = NewVec(JJ)
			  Tagged(NTagGroups,Istk,Ireg,Sex,2,Iyr) = NewVec(JJ)
              Tagged(NTagGroups,Istk,Ireg,Sex,1,Iyr) = Age
			 ENDIF
			 !WRITE(*,*) Sex,Istk,JJ,NewVec(JJ)
9015	   CONTINUE
           !write tag releases to data file - totals (and maybe age structure)
!		   IF (TempVec(Age).GT.0) WRITE(37,'(I5,1x,I4,1x,I2,1x,I2,1x,I3,1x,I6,1x,F4.0,1x,F7.3,1x,I3,1x,20000(F6.0,1x))') NTagGroups,Iyr,Iflt,Ireg,Ilen,INT(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,0:MaxAge,Iyr))),(EstTagID(NTagGroups,II),II=1,2),Age,(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,JAge,Iyr)),JAge=0,MaxAge)
		   IF (TempVec(Age).GT.0) WRITE(37,'(I5,1x,I4,1x,I2,1x,I2,1x,I3,1x,I6,1x,F4.0,1x,F7.3,1x,I3,1x,20000(F6.0,1x))') NTagGroups,Iyr,Iflt,Ireg,Ilen,INT(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,2,Iyr))),(EstTagID(NTagGroups,II),II=1,2),Age,(SUM(Tagged(NTagGroups,1,Ireg,1:2,JAge,Iyr)),JAge=1,2)

		  !IF (TempVec(Age).GT.0) STOP

9014     CONTINUE
           
        
         ENDIF  !CLOSE IF STATEMENT FOR Lenbins

9009    CONTINUE

        NGuessGroups = NGuessGroups + MaxGp
      ! ENDIF

		ENDIF
9001   CONTINUE
!	  ENDIF          
!9000 CONTINUE
	
	CLOSE(37)

	IF (Diag.EQ.1) WRITE(98,*) 'end of tag releases for ',Iyr

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	TAGGING RELEASES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine inputs the releases for the current year
!
!	UPDATED 21st January 2011
!
	SUBROUTINE TagReleases(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Sex,Istk,Iflt,Ireg,Iyr,JJ,ISEED1,ISEED2,NumSubGroups,Lowlen(100),Toplen(100),Ivec(2000)
	INTEGER Igroup,Ilen,II,Nestgroups,Maxgp,Jlen,Last,Itemp
	REAL*8 PropVec(2000),NewVec(2000),SubGroupNumbers(100),AgeVec(1000),LenVec(2000),CumVec(2000),Temp
	REAL*8 GuessGrowthpars(3)

	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!open the output file for appending
    OPEN(UNIT=37,FILE='TagReleases.inp',POSITION='APPEND')


          	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*)
	READ(18,*)
	READ(18,*)
    DO II=1,16 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	READ(18,*)
	DO II=1,3
	 READ(18,*) GuessGrowthpars(II)
	ENDDO
	CLOSE(18)
    !WRITE(99,*) GuessGrowthpars(1:3)


	 !Releases, 
 	 !DO 9000 Iflt=1,Nflt
	 DO Iflt=1,Nflt
     !1st check to see if any in this year
	  IF (TagRelYrs(Iflt,Iyr).EQ.1) THEN
	   !allocate tag releases by region based on catch proportions (overdisperse?)
	   CALL GetRegTagReleases(Iyr,Iflt)
      ENDIF

	 ENDDO
	   !loop over region
	   DO 9001 Ireg=1,Nreg 
	   !{
	   ! if tag releases, 
	    LenVec = 0.d0
	    DO 9000 Iflt=1,Nflt
	    IF (NTagRelease(Iflt,Ireg,Iyr).GT.0) THEN

        IF (TagRelAge(Iflt).EQ.-1) THEN
		 
		 PropVec = 0.d0
	     DO 9007 Ilen=1,Nlen
		  DO 9007 Istk=1,Nstk
 		   DO 9007 Sex=1,2
		    DO 9007 Age=0,MaxAge		
             PropVec(Ilen) = PropVec(Ilen) + SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9007     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,(PropVec(Age),Age=1,Nlen)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,NTagRelease(Iflt,Ireg,Iyr),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,(NewVec(Age),Age=1,Nlen)
         LenVec = LenVec + NewVec

		 ENDIF
		 ENDIF
	
9000	 CONTINUE

	    IF (SUM(LenVec).GT.0) THEN

         IVec(1) = 0
		 DO Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) IVec(1)=IVec(1)+1
		 ENDDO
		 Iflt = 1
		 Nestgroups = INT(-1*EstTagAge(Iflt))
		 IF (Nestgroups.GT.IVec(1)) Nestgroups = IVec(1)

    !     WRITE(99,*) NTagRelease(Iflt,Ireg,Iyr)
         !Temp = NTagRelease(Iflt,Ireg,Iyr) / Nestgroups
		 Temp = SUM(NTagRelease(1:Nflt,Ireg,Iyr)) / Nestgroups
         CumVec(1) = LenVec(1)
		 DO Ilen=2,Nlen
!		  CumVec(Ilen) = CumVec(Ilen-1) + NewVec(Ilen)
		  CumVec(Ilen) = CumVec(Ilen-1) + LenVec(Ilen)
		 ENDDO
         Ivec=0
	!	 WRITE(99,*) Temp
		 Last = 0
		 DO Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) THEN
		   IF (CumVec(Ilen)/Temp.GT.FLOAT(Last)) Last=Last+1
		   IF (Last.GT.Nestgroups) Last = Nestgroups
		   Ivec(Ilen) = Last
!		   Itemp = ANINT(CumVec(Ilen)/Temp)
 !          IF (Itemp.GT.Last) Last = Last+1
!		   Ivec(Ilen) = Last+1
		  ENDIF
		  !WRITE(99,*) LenVec(Ilen),CumVec(Ilen),IVec(Ilen)
		 ENDDO
 
		 
		 MaxGp = 0
         DO 9009 Ilen=1,Nlen
		  IF (LenVec(Ilen).GT.0) THEN
           NTagGroups=NTagGroups+1
		   EstTagID(NTagGroups,1) = NGuessGroups + IVec(Ilen)
		   IF (Ivec(Ilen).GT.MaxGp) MaxGp = IVec(Ilen)
           IF ((lolenbin(Ilen)+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0).GE.GuessGrowthpars(1)) THEN
		    EstTagID(NTagGroups,2) = MaxAge
		   ELSE
		    EstTagID(NTagGroups,2) = GuessGrowthpars(3)-LOG(1.d0-(lolenbin(Ilen)+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0)/GuessGrowthpars(1))/GuessGrowthpars(2)
		   ENDIF
		   EstTagID(NTagGroups,3) = Ireg
		   GroupReleases(NGuessGroups + IVec(Ilen)) = GroupReleases(NGuessGroups + IVec(Ilen)) + ANINT(LenVec(Ilen))
		   GuessTaggedAge(NGuessGroups + IVec(Ilen)) = GuessTaggedAge(NGuessGroups + IVec(Ilen)) + LenVec(Ilen)*EstTagID(NTagGroups,2)
		   GuessRegion(NGuessGroups + IVec(Ilen)) = Ireg
	   
	   
	   !allocate tags to stock,sex, and ages
		 PropVec = 0.d0
		 JJ=0	     
	     DO 9002 Sex=1,2
          DO 9002 Istk=1,Nstk
		   DO 9002 Age=0,MaxAge
		    JJ=JJ+1
            PropVec(JJ) = PropVec(JJ) + FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9002     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I3,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,Ilen,(PropVec(Age),Age=1,JJ)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,INT(LenVec(Ilen)),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,I3,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,Ilen,(NewVec(Age),Age=1,JJ)
		 JJ=0	     
		 DO 9003 Sex=1,2
		  DO 9003 Istk=1,Nstk
		   DO 9003 Age=0,MaxAge
		    JJ=JJ+1
            Tagged(NTagGroups,Istk,Ireg,Sex,Age,Iyr)  = NewVec(JJ)
9003     CONTINUE 
         !write tag releases to data file - totals (and maybe age structure)
		 WRITE(37,'(I5,1x,I4,1x,I2,1x,I2,1x,I3,1x,I6,1x,F4.0,1x,F7.3,1x,20000(F6.0,1x))') NTagGroups,Iyr,Iflt,Ireg,Ilen,INT(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,0:MaxAge,Iyr))),(EstTagID(NTagGroups,II),II=1,2),(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,Age,Iyr)),Age=0,MaxAge)
        
         ENDIF  !CLOSE IF STATEMENT FOR Lenbins

9009    CONTINUE
        NGuessGroups = NGuessGroups + MaxGp
      ! ENDIF

		ENDIF
9001   CONTINUE
!	  ENDIF          
!9000 CONTINUE
	
	CLOSE(37)

	IF (Diag.EQ.1) WRITE(98,*) 'end of tag releases for ',Iyr

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	TAGGING RELEASES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine inputs the releases for the current year
!
	SUBROUTINE OldTagReleases(Iyr)   !old

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Sex,Istk,Iflt,Ireg,Iyr,JJ,ISEED1,ISEED2,NumSubGroups,Lowlen(100),Toplen(100)
	INTEGER Igroup,Ilen,II
	REAL*8 PropVec(2000),NewVec(2000),SubGroupNumbers(100),AgeVec(1000)
	
	ISEED1 = ISEEDZ
	ISEED2 = ISEEDX

	!open the output file for appending
    OPEN(UNIT=37,FILE='TagReleases.inp',POSITION='APPEND')


          	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*)
	READ(18,*)
	READ(18,*)
    DO II=1,16 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	CLOSE(18)



	 !Releases, 
 	 DO 9000 Iflt=1,Nflt
     !1st check to see if any in this year
	  IF (TagRelYrs(Iflt,Iyr).EQ.1) THEN
	   !allocate tag releases by region based on catch proportions (overdisperse?)
	   CALL GetRegTagReleases(Iyr,Iflt)
	   !loop over region
	   DO 9001 Ireg=1,Nreg 
	   !{
	   ! if tag releases, 
	    IF (NTagRelease(Iflt,Ireg,Iyr).GT.0) THEN
		 IF (EstTagAge(Iflt).EQ.-101) THEN
		   !tags have true age structure
 
		 PropVec = 0.d0
		 JJ=0	     
		 DO 9007 Istk=1,Nstk
		  DO 9007 Sex=1,2
		   DO 9007 Age=0,MaxAge
		    JJ=JJ+1
			DO 9007 Ilen=1,Nlen
             PropVec(JJ) = PropVec(JJ) + SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9007     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,(PropVec(Age),Age=1,JJ)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,NTagRelease(Iflt,Ireg,Iyr),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,(NewVec(Age),Age=1,JJ)
		 JJ=0
		 AgeVec=0	     
		 DO 9008 Istk=1,Nstk
		  DO 9008 Sex=1,2
		   DO 9008 Age=0,MaxAge
		    JJ=JJ+1
			IF (NewVec(JJ).GT.0) AgeVec(Age+1)=1
9008	 CONTINUE
         !WRITE(*,*) AgeVec
		 DO Age=0,MaxAge
		  IF (AgeVec(Age+1).EQ.1) THEN
  		   NTagGroups = NTagGroups+1
		   GuessTaggedAge(NTagGroups) = Age
		   JJ=0	     
		   DO 9009 Istk=1,Nstk
		    DO 9009 Sex=1,2
		     JJ=(Istk-1)*2*(MaxAge+1)+(Sex-1)*(MaxAge+1)+Age+1
			  Tagged(NTagGroups,Istk,Ireg,Sex,Age,Iyr)  = NewVec(JJ)		  
9009       CONTINUE 
          	     !write tag releases to data file - totals (and maybe age structure)
		   WRITE(37,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,20000(F6.0,1x))') NTagGroups,Iyr,Iflt,Ireg,INT(SUM(Tagged(NTagGroups,1:Nstk,Ireg,1:2,Age,Iyr)))

		  ENDIF
         ENDDO


		 ELSE 
         !get the proportions of total regional tags in each of the sub tag groups
		 NumSubGroups=0
		 Lowlen = 1
		 Toplen = Nlen
		 CALL GetSubGroupReleases(Iyr,Iflt,Ireg,SubGroupNumbers,NumSubGroups,Lowlen,Toplen)
		 DO 9006 Igroup=1,NumSubGroups
       !   increment n tag groups
         NTagGroups = NTagGroups + 1
		 TagGroupYr(NTagGroups) = Iyr
         TagGroupReg(NTagGroups) = Ireg
		 IF (Diag.EQ.1) WRITE(98,'(I2,1x,I4,1x,I2)') NTagGroups,TagGroupYr(NTagGroups),TagGroupReg(NTagGroups)
       IF (TagRelAge(Iflt).EQ.-1) THEN
	   !allocate tags to stock,sex, and ages
		 PropVec = 0.d0
		 JJ=0	     
		 DO 9002 Istk=1,Nstk
		  DO 9002 Sex=1,2
		   DO 9002 Age=0,MaxAge
		    JJ=JJ+1
		    DO 9002 Ilen=Lowlen(Igroup),Toplen(Igroup)
             PropVec(JJ) = PropVec(JJ) + SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9002     CONTINUE 
		 PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,(PropVec(Age),Age=1,JJ)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,INT(SubGroupNumbers(Igroup)*NTagRelease(Iflt,Ireg,Iyr)),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,(NewVec(Age),Age=1,JJ)
		 JJ=0	     
		 DO 9003 Istk=1,Nstk
		  DO 9003 Sex=1,2
		   DO 9003 Age=0,MaxAge
		    JJ=JJ+1
            Tagged(NTagGroups,Istk,Ireg,Sex,Age,Iyr)  = NewVec(JJ)
9003     CONTINUE 
       ENDIF
	   IF (TagRelAge(Iflt).NE.-1) THEN
	   !allocate tags to stock and sex given fixed release age
		 PropVec = 0.d0
		 JJ=0
		 Age = TagRelAge(Iflt)	     
		 DO 9004 Istk=1,Nstk
		  DO 9004 Sex=1,2
		    JJ=JJ+1
            PropVec(JJ) = PropVec(JJ) + SelAge(Iflt,Istk,Sex,Age,Iyr)*N(Istk,Ireg,Sex,Age,Iyr)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Iyr))
9004     CONTINUE 
         PropVec = PropVec/SUM(PropVec)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.4,1x))') Iyr,Iflt,Ireg,(PropVec(Age),Age=1,JJ)
		 NewVec = 0.d0
		 CALL GENMUL(PropVec,NTagRelease(Iflt,Ireg,Iyr),NewVec,ISEEDX,.FALSE.,1)
		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I2,1x,2000(F6.0,1x))') Iyr,Iflt,Ireg,(NewVec(Age),Age=1,JJ)
		 JJ=0	     
		 DO 9005 Istk=1,Nstk
		  DO 9005 Sex=1,2
		    JJ=JJ+1
            Tagged(NTagGroups,Istk,Ireg,Sex,TagRelAge,Iyr)  = NewVec(JJ)
9005     CONTINUE 
         ENDIF
	     !write tag releases to data file - totals (and maybe age structure)
		 WRITE(37,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,20000(F6.0,1x))') NTagGroups,Iyr,Iflt,Ireg,INT(SubGroupNumbers(Igroup)*NTagRelease(Iflt,Ireg,Iyr)),(NewVec(Age),Age=1,JJ)
9006    CONTINUE
		ENDIF
		ENDIF
9001   CONTINUE
	  ENDIF          
9000 CONTINUE
	
	CLOSE(37)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine reads in the specifications for generating the historical data
!	Currently only deals with fishery age comps, and fishery length comps
!	
    SUBROUTINE ReadDataSpecs

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER toskip,II,Iyr,JJ,Ireg,Iflt,Tagflag(1:Nfleet),Ilen,Age
	REAL*8 Yr1

!	diagnostics
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=98,FILE='histdata.junk')
	 WRITE(98,'(A45)') 'specifications for generating historical data'
	ENDIF

!	open the relevant file
	OPEN(UNIT=13,FILE='dataspec.ctl')

	toskip = 7
	 DO 8100 II=1,toskip
	  READ(13,*)
8100 CONTINUE
    

!   ****use actual historical dataset?****
	READ(13,*) UseHistData
    READ(13,*)
    READ(13,*)
    READ(13,*)

!	****fishery catch data****
    IF (Diag.EQ.1) WRITE(98,*) 'fishery Catch'	
	 READ(13,*) (CatchFlag(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CatCV(Iflt),Iflt=1,Nflt)
!	get years for Catch
	 CatYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,200(I4,1x))') 'Ft Rg',(Iyr,Iyr=Fyear,Lyear)
	 DO 8119 Iflt=1,Nflt
	   IF (CatchFlag(Iflt).EQ.-1) THEN
	     DO 8120 Iyr=Fyear,Lyear
	      IF (SUM(Catch(Iflt,1:Nreg,Iyr)).GT.0) CatYrs(Iflt,Iyr) = 1
8120     CONTINUE
	   ELSEIF (CatchFlag(Iflt).GT.0) THEN
	    JJ=0
	    DO 8121 Iyr=Lyear,Fyear,-1
		 IF (SUM(Catch(Iflt,1:Nreg,Iyr)).GT.0) THEN
		  CatYrs(Iflt,Iyr) = 1
	      JJ=JJ+1
		 ENDIF
		 IF (JJ.EQ.CatchFlag(Iflt)) GOTO 8122	 	   
8121    CONTINUE
	   ENDIF
8122   IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(CatYrs(Iflt,Iyr),Iyr=Fyear,Lyear)
8119 CONTINUE


!	****fishery CPUE data****

	 IF (Diag.EQ.1) WRITE(98,*) 'fishery CPUE'	
	 READ(13,*)
	 READ(13,*) (CPUEflag(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueCV(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueQmu(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpuePow(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueVar(Iflt),Iflt=1,Nflt)
 	 READ(13,*)
	 READ(13,*) (CpueCorr(Iflt),Iflt=1,Nflt)
!	get years for CPUE
	 CPUEYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,200(I4,1x))') 'Ft Rg',(Iyr,Iyr=Fyear,Lyear)
	 DO 8108 Iflt=1,Nflt
	  DO 8108 Ireg=1,Nreg 
	   IF (CPUEflag(Iflt).EQ.-1) THEN
	     DO 8107 Iyr=Fyear,Lyear
	      IF (Catch(Iflt,Ireg,Iyr).GT.0) CpueYrs(Iflt,Ireg,Iyr) = 1
8107     CONTINUE
	   ELSEIF (CPUEflag(Iflt).GT.0) THEN
	    JJ=0
	    DO 8109 Iyr=Lyear,Fyear,-1
		 IF (Catch(Iflt,Ireg,Iyr).GT.0) THEN
		  CpueYrs(Iflt,Ireg,Iyr) = 1
	      JJ=JJ+1
		 ENDIF
		 IF (JJ.EQ.CPueflag(Iflt)) GOTO 8110	 	   
8109    CONTINUE
	   ENDIF
8110   IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,200(I4,1x))') Iflt,Ireg,(CpueYrs(Iflt,Ireg,Iyr),Iyr=Fyear,Lyear)
8108 CONTINUE

!	****fishery discard data****
	READ(13,*)
	READ(13,*)
	IF (Diag.EQ.1) WRITE(98,*) 'fishery discards'
	READ(13,*) (Discflag(II),II=1,Nflt+1)
	IF (Diag.EQ.1) WRITE(98,'(11(I2,1x))') (Discflag(II),II=1,Nflt+1)
	READ(13,*)
	READ(13,*) (DiscCV(II),II=1,Nflt)
	IF (Diag.EQ.1) WRITE(98,'(11(F6.2,1x))') (DiscCV(II),II=1,Nflt)

!	 get years for discards
	 DiscYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'DY',(Iyr,Iyr=Fyear,Lyear)
	 DO 8111 Iflt=1,Nflt
	  IF (Discflag(Iflt+1).EQ.-1) THEN
	   DO 8112 Iyr=Fyear,Lyear
	    IF (SUM(TotalCatch(Iflt,1:Nreg,Iyr)).GT.0) DiscYrs(Iflt,Iyr) = 1
8112   CONTINUE
	  ELSEIF (Discflag(Iflt+1).GT.0) THEN
	   JJ=0
	   DO 8113 Iyr=Lyear,Fyear,-1
	    IF (SUM(TotalCatch(Iflt,1:Nreg,Iyr)).GT.0) THEN
	     DiscYrs(Iflt,Iyr) = 1
	 	 JJ=JJ+1
	    ENDIF
	    IF (JJ.EQ.Discflag(Iflt+1)) GOTO 8114
8113   CONTINUE
	  ENDIF
8114  IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(DiscYrs(Iflt,Iyr),Iyr=Fyear,Lyear)
8111 CONTINUE


!	****fishery Length data****
	 READ(13,*)
	 IF (Diag.EQ.1) WRITE(98,*) 'fishery LENGTH comps'	
	 DO 8104 II=1,3
	  READ(13,*)
	  READ(13,*) LenflagF(II),LengthN(II)
	  !READ(13,*) LenflagF(II),(LengthN(II,Iflt),Iflt=1,Nflt)		
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,50(I5,1x))') LenflagF(II),LengthN(II)
	  !IF (Diag.EQ.1) WRITE(98,'(I2,1x,50(I5,1x))') LenflagF(II),(LengthN(II,Iflt),Iflt=1,Nflt)
8104 CONTINUE

!	get years for lengths
	 LenYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'LY',(Iyr,Iyr=Fyear,Lyear)
	 DO 8105 II=1,3
	  IF (LenflagF(II).EQ.-1) LenYrs(II,Fyear:Lyear) = 1
	  IF (LenflagF(II).GT.0) THEN
	   DO 8106 JJ=1,LenflagF(II)
	    LenYrs(II,Lyear+1-JJ) = 1
8106   CONTINUE
	  ENDIF	 
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') II,(LenYrs(II,Iyr),Iyr=Fyear,Lyear)
8105 CONTINUE



!	****fishery age data****
	 READ(13,*)
	 READ(13,*)
	 IF (Diag.EQ.1) WRITE(98,*) 'fishery AGE comps'
	 READ(13,*)
	 READ(13,*) DoCALK
	 IF (Diag.EQ.1) WRITE(98,*) 'doCALK',DoCALK
	 DO 8101 II=1,3
	  READ(13,*)
	  READ(13,*) AgeflagF(II),AgeN(II)		
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I5)') AgeflagF(II),AgeN(II)
8101 CONTINUE

!	 get years for ages
	 AgeYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'AY',(Iyr,Iyr=Fyear,Lyear)
	 DO 8102 II=1,3
	  IF (AgeflagF(II).EQ.-1) AgeYrs(II,Fyear:Lyear) = 1
	  IF (AgeflagF(II).GT.0) THEN
	   DO 8103 JJ=1,AgeflagF(II)
	    AgeYrs(II,Lyear+1-JJ) = 1
8103   CONTINUE
	  ENDIF
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') II,(AgeYrs(II,Iyr),Iyr=Fyear,Lyear)
8102 CONTINUE
	 

!	****fishery tag release data****
      READ(13,*)
      READ(13,*)
      READ(13,*)
	  READ(13,*)
	  IF (Diag.EQ.1) WRITE(98,*) 'fishery tag release data'
	  READ(13,*) (Tagflag(II),II=1,Nflt)
!	  get years for tag releases
	  TagRelYrs=0
	  IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'TY',(Iyr,Iyr=Fyear,Lyear)
	  DO 8115 Iflt=1,Nflt
	   IF (Tagflag(Iflt).EQ.-1) TagRelYrs(Iflt,Fyear:Lyear) = 1
	   IF (Tagflag(Iflt).GT.0) THEN
	    DO 8116 JJ=1,Tagflag(Iflt)
	    TagRelYrs(Iflt,Lyear+1-JJ) = 1
8116    CONTINUE
	   ENDIF	 
	   IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(TagRelYrs(Iflt,Iyr),Iyr=Fyear,Lyear)
8115  CONTINUE


      IF (Diag.EQ.1) WRITE(98,'(A20)') 'Tag ID flags'
      DO 8117 JJ=1,2
       READ(13,*)
 	   READ(13,*) (TagRelFlag(JJ,II),II=1,Nflt)
       IF (Diag.EQ.1) WRITE(98,'(20(I4,1x))') (TagRelFlag(JJ,II),II=1,Nflt)
8117  CONTINUE

      READ(13,*)
	  DO 8118 Iflt=1,Nflt
	   IF (TagRelFlag(1,Iflt).EQ.0) THEN
	    Yr1 = Lyear-SUM(TagRelYrs(Iflt,Fyear:Lyear))+1	   
	    READ(13,*) (NTagRelease(Iflt,0,Iyr),Iyr=Yr1,Lyear)
			    WRITE(*,*) (NTagRelease(Iflt,0,Iyr),Iyr=Yr1,Lyear)
		IF (Diag.EQ.1) THEN
		 WRITE(98,'(A4,1x,200(I4,1x))') 'Ft',(Iyr,Iyr=Yr1,Lyear)
         WRITE(98,'(I4,1x,200(I6,1x))') Iflt,(NTagRelease(Iflt,0,Iyr),Iyr=Yr1,Lyear)
		ENDIF
       ELSE
	    READ(13,*)
       ENDIF
8118  CONTINUE



      !STOP

	 READ(13,*)
	 READ(13,*) (TagRelAge(Iflt),Iflt=1,Nflt)
!	 WRITE(*,*) (TagRelAge(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (muTagDR(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CVTagDR(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) muTagAlpha
	 READ(13,*)
	 READ(13,*) CVTagAlpha


 !    WRITE(*,*) 'here tags in'

     IF (Diag.EQ.1) THEN
	  WRITE(98,'(10(I3,1x))') (TagRelAge(Iflt),Iflt=1,Nflt)
	  WRITE(98,'(10(F6.2,1x))') (muTagDR(Iflt),Iflt=1,Nflt)
	  WRITE(98,'(10(F6.2,1x))') (CVTagDR(Iflt),Iflt=1,Nflt)
      WRITE(98,'(10(F6.2,1x))') muTagAlpha
      WRITE(98,'(10(F6.2,1x))') CVTagAlpha
     ENDIF

	 READ(13,*)
	 READ(13,*)
	 READ(13,*)
	 READ(13,*)
	 READ(13,*)	 	 	 	 


!      WRITE(*,*) 'here 4'

!	****survey specifications****
!	****survey index****

	 IF (Diag.EQ.1) WRITE(98,*) 'Survey Index'	
	 READ(13,*)
	 READ(13,*)
	 READ(13,*) Nsurv
	 READ(13,*)
	 READ(13,*) (SurveyReg(Iflt),Iflt=1,Nsurv)
	 READ(13,*)
	 READ(13,*)
	 READ(13,*)
	 READ(13,*) (SurveyTime(Iflt),Iflt=1,Nsurv) 
	 READ(13,*)
	 READ(13,*) (Surveyflag(Iflt),Iflt=1,Nsurv)
	 READ(13,*)
	 READ(13,*) (SurveyCV(Iflt),Iflt=1,Nsurv)
	 READ(13,*)
	 READ(13,*) (SurveyQmu(Iflt),Iflt=1,Nsurv)
	 READ(13,*)
	 DO Iflt=1,Nsurv
	  READ(13,*) (SurveySel(Iflt,Ilen),Ilen=1,Nlen)
	 ENDDO
!	get years for surveys
	 SurveyYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,200(I4,1x))') 'Ft Rg',(Iyr,Iyr=Fyear,Lyear)
	 DO 8208 Iflt=1,Nsurv
	   IF (Surveyflag(Iflt).EQ.-1) THEN
	     DO 8207 Iyr=Fyear,Lyear
	      SurveyYrs(Iflt,Iyr) = 1
8207     CONTINUE
	   ELSEIF (Surveyflag(Iflt).GT.0) THEN
	    JJ=0
	    DO 8209 Iyr=Lyear,Fyear,-1		
		  SurveyYrs(Iflt,Iyr) = 1
	      JJ=JJ+1
		 IF (JJ.EQ.Surveyflag(Iflt)) GOTO 8210	 	   
8209    CONTINUE
	   ENDIF
8210   IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(SurveyYrs(Iflt,Iyr),Iyr=Fyear,Lyear)
8208 CONTINUE

 !     WRITE(*,*) 'here 5'

!	****survey lengths****
	 IF (Diag.EQ.1) WRITE(98,*) 'Survey LENGTH comps'	
	 READ(13,*)
	 READ(13,*) (SurveyLengthN(Iflt),Iflt=1,Nsurv)
	 IF (Diag.EQ.1) WRITE(98,'(50(I5,1x))') (SurveyLengthN(Iflt),Iflt=1,Nsurv)


!	****survey ages****
	 IF (Diag.EQ.1) WRITE(98,*) 'fishery AGE comps'
	 READ(13,*)
	 READ(13,*) (SurveyAgeN(Iflt),Iflt=1,Nsurv)		
	 IF (Diag.EQ.1) WRITE(98,'(5(I5,1x))') (SurveyAgeN(Iflt),Iflt=1,Nsurv)

!   **** Ageing Error ****
	 IF (Diag.EQ.1) WRITE(98,*) 'Ageing error matrix'
	 READ(13,*)
	 DO 8211 Age=0,MaxAge
	  READ(13,*) (AgeErrMat(Age,II),II=0,MaxAge)		
	  IF (Diag.EQ.1) WRITE(98,'(200(F6.3,1x))') (AgeErrMat(Age,II),II=0,MaxAge)
8211 CONTINUE
	 DO 8212 Age=0,MaxAge
	  AgeErrMat(0:MaxAge,Age) = AgeErrMat(0:MaxAge,Age)/SUM(AgeErrMat(0:MaxAge,Age))
8212 CONTINUE

   CLOSE(13)

  !    WRITE(*,*) 'here 6'

   OPEN(UNIT=13,FILE='PseudoRefs.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)

	CLOSE(13)

   OPEN(UNIT=13,FILE='flukerefs.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)
   READ(13,*)
   READ(13,*) Pseudorefspecs(3)
   READ(13,*)
   READ(13,*) Pseudorefspecs(4)   

	CLOSE(13)


    IF (HCRspecs(3).GE.62) THEN

   OPEN(UNIT=13,FILE='fluke-recdisc.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)
   READ(13,*)
   READ(13,*) Pseudorefspecs(3)
   READ(13,*)
   READ(13,*) Pseudorefspecs(4)   

	CLOSE(13)

   ENDIF

    IF (HCRspecs(3).EQ.51) THEN

   OPEN(UNIT=13,FILE='icesfmsyrefs.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)

	CLOSE(13)

    ENDIF

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine reads in the specifications for generating the data during the projection period
!	Currently only deals with CPUE, discards, fishery age comps & fishery length comps
!	
    SUBROUTINE ReadNewDataSpecs(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER toskip,II,Iyr,JJ,Yr1,Yr2,Iflt,Ireg,Tagflag(1:Nfleet)

!	diagnostics
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=98,FILE='histdata.junk',POSITION='APPEND')
	 WRITE(98,'(A45)') 'specifications for generating projection data'
	ENDIF

!	open the relevant file
	OPEN(UNIT=13,FILE='dataspec.ctl')

	toskip = 11
	 DO 8500 II=1,toskip
	  READ(13,*)
8500 CONTINUE	  

!	****fishery catch data****

    IF (Diag.EQ.1) WRITE(98,*) 'fishery Catch'	
	 READ(13,*) (CatchFlag(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CatCV(Iflt),Iflt=1,Nflt)
	 READ(13,*)
!	get years for Catch
	 CatYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,200(I4,1x))') 'Ft Rg',(Iyr,Iyr=Fyear,Lyear)
	 DO 8521 Iflt=1,Nflt
	     DO 8522 Iyr=Yr1,Yr2
	      IF (SUM(Catch(Iflt,1:Nreg,Iyr)).GT.0.AND.CatchFlag(Iflt).NE.0) CatYrs(Iflt,Iyr) = 1
8522     CONTINUE
         IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(CatYrs(Iflt,Iyr),Iyr=Yr1,Yr2)
8521 CONTINUE

!	****fishery CPUE data****

	 IF (Diag.EQ.1) WRITE(98,*) 'fishery CPUE'	
	 READ(13,*) (CPUEflag(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueCV(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueQmu(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpuePow(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CpueVar(Iflt),Iflt=1,Nflt)
 	 READ(13,*)
	 READ(13,*) (CpueCorr(Iflt),Iflt=1,Nflt)
!	get years for CPUE
	 CPUEYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,200(I4,1x))') 'Ft Rg',(Iyr,Iyr=Yr1,Yr2)
	 DO 8508 Iflt=1,Nflt
	  DO 8508 Ireg=1,Nreg 
	   DO 8507 Iyr=Yr1,Yr2
	    IF (Catch(Iflt,Ireg,Iyr).GT.0.AND.CPUEflag(Iflt).NE.0) CpueYrs(Iflt,Ireg,Iyr) = 1
8507   CONTINUE
       IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,200(I4,1x))') Iflt,Ireg,(CpueYrs(Iflt,Ireg,Iyr),Iyr=Yr1,Yr2)
8508 CONTINUE

!	****fishery discard data****
	READ(13,*)
	READ(13,*)
	IF (Diag.EQ.1) WRITE(98,*) 'fishery discards'
	READ(13,*) (Discflag(II),II=1,Nflt+1)
	IF (Diag.EQ.1) WRITE(98,'(11(I2,1x))') (Discflag(II),II=1,Nflt+1)
	READ(13,*)
	READ(13,*) (DiscCV(II),II=1,Nflt)
	IF (Diag.EQ.1) WRITE(98,'(11(F6.2,1x))') (DiscCV(II),II=1,Nflt)

!	 get years for discards
	 DiscYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'DY',(Iyr,Iyr=Yr1,Yr2)
	 DO 8511 Iflt=1,Nflt
	  DO 8512 Iyr=Yr1,Yr2
	    IF (SUM(TotalCatch(Iflt,1:Nreg,Iyr)).GT.0.AND.Discflag(Iflt+1).NE.0) DiscYrs(Iflt,Iyr) = 1
8512   CONTINUE
      IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(DiscYrs(Iflt,Iyr),Iyr=Yr1,Yr2)
8511 CONTINUE


!	****fishery Length data****
	  READ(13,*)
	 IF (Diag.EQ.1) WRITE(98,*) 'fishery LENGTH comps'	
	 DO 8504 II=1,3
	  READ(13,*)
	  READ(13,*) LenflagF(II),LengthN(II)
      !READ(13,*) LenflagF(II),(LengthN(II,Iflt),Iflt=1,Nflt)		
	  !IF (Diag.EQ.1) WRITE(98,'(I2,1x,50(I5,1x))') LenflagF(II),(LengthN(II,Iflt),Iflt=1,Nflt)
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I5)') LenflagF(II),LengthN(II)
8504 CONTINUE

!	get years for lengths
	 LenYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'LY',(Iyr,Iyr=Yr1,Yr2)
	 DO 8505 II=1,3
	  IF (LenflagF(II).NE.0) LenYrs(II,Yr1:Yr2) = 1
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') II,(LenYrs(II,Iyr),Iyr=Yr1,Yr2)
8505 CONTINUE

!	****fishery age data****
	 READ(13,*)
	 READ(13,*)
	 IF (Diag.EQ.1) WRITE(98,*) 'fishery AGE comps'
	 READ(13,*)
	 READ(13,*) DoCALK
	 DO 8502 II=1,3
	  READ(13,*)
	  READ(13,*) AgeflagF(II),AgeN(II)		
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I5)') AgeflagF(II),AgeN(II)
8502 CONTINUE

!	 get years for ages
	 AgeYrs=0
	 IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'AY',(Iyr,Iyr=Yr1,Yr2)
	 DO 8503 II=1,3
	  IF (AgeflagF(II).NE.0) AgeYrs(II,Yr1:Yr2) = 1
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') II,(AgeYrs(II,Iyr),Iyr=Yr1,Yr2)
8503 CONTINUE


!	****fishery tag release data****
      READ(13,*)
      READ(13,*)
      READ(13,*)
	  READ(13,*)
	  IF (Diag.EQ.1) WRITE(98,*) 'fishery tag release data'
	  READ(13,*) (Tagflag(II),II=1,Nflt)
!	  get years for tag releases
	  TagRelYrs=0
	  IF (Diag.EQ.1) WRITE(98,'(A2,1x,200(I4,1x))') 'TY',(Iyr,Iyr=Yr1,Yr2)
	  DO 8515 Iflt=1,Nflt
	   IF (Tagflag(Iflt).EQ.-1) TagRelYrs(Iflt,Yr1:Yr2) = 1
	   IF (Tagflag(Iflt).GT.0) TagRelYrs(Iflt,Yr1:Yr2) = 1
	   IF (Diag.EQ.1) WRITE(98,'(I2,1x,200(I4,1x))') Iflt,(TagRelYrs(Iflt,Iyr),Iyr=Yr1,Yr2)
8515  CONTINUE

      IF (Diag.EQ.1) WRITE(98,'(A20)') 'Tag ID flags'
      DO 8517 JJ=1,2
       READ(13,*)
 	   READ(13,*) (TagRelFlag(JJ,II),II=1,Nflt)
       IF (Diag.EQ.1) WRITE(98,'(20(I4,1x))') (TagRelFlag(JJ,II),II=1,Nflt)
8517  CONTINUE

      READ(13,*)
	  DO 8518 Iflt=1,Nflt
	   IF (TagRelFlag(1,Iflt).EQ.0) THEN
	    II = Lyear-SUM(TagRelYrs(Iflt,Fyear:Lyear))+1	   
	    READ(13,*) (NTagRelease(Iflt,0,Iyr),Iyr=II,Lyear)
		NTagRelease(Iflt,0,Yr1:Yr2) = NTagRelease(Iflt,0,Lyear)
		!IF (Diag.EQ.1) THEN
		! WRITE(98,'(A4,1x,200(I4,1x))') 'Ft',(Iyr,Iyr=Yr1,Yr2)
        ! WRITE(98,'(I4,1x,200(I6,1x))') Iflt,(NTagRelease(Iflt,0,Iyr),Iyr=Yr1,Yr2)
		!ENDIF
       ELSE
	    READ(13,*)
       ENDIF
8518  CONTINUE

	 READ(13,*)
	 READ(13,*) (TagRelAge(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (muTagDR(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) (CVTagDR(Iflt),Iflt=1,Nflt)
	 READ(13,*)
	 READ(13,*) muTagAlpha
	 READ(13,*)
	 READ(13,*) CVTagAlpha

     IF (Diag.EQ.1) THEN
	  WRITE(98,'(10(F6.2,1x))') (muTagDR(Iflt),Iflt=1,Nflt)
	  WRITE(98,'(10(F6.2,1x))') (CVTagDR(Iflt),Iflt=1,Nflt)
      WRITE(98,'(10(F6.2,1x))') muTagAlpha
      WRITE(98,'(10(F6.2,1x))') CVTagAlpha
     ENDIF


	!future tag release specs
      IF (Diag.EQ.1) WRITE(98,'(A20)') 'Tag ID flags'
	  READ(13,*)
      DO 8519 JJ=1,2
       READ(13,*)
 	   READ(13,*) (TagRelFlag(JJ,II),II=1,Nflt)
       IF (Diag.EQ.1) WRITE(98,'(20(I4,1x))') (TagRelFlag(JJ,II),II=1,Nflt)
8519  CONTINUE

	  DO 8520 Iflt=1,Nflt
	   IF (TagRelFlag(1,Iflt).EQ.0) NTagRelease(Iflt,0,Yr1:Yr2) = 0
	   IF (Diag.EQ.1) THEN
		WRITE(98,'(A4,1x,200(I4,1x))') 'Ft',(Iyr,Iyr=Yr1,Yr2)
        WRITE(98,'(I4,1x,200(I6,1x))') Iflt,(NTagRelease(Iflt,0,Iyr),Iyr=Yr1,Yr2)
	   ENDIF
8520  CONTINUE

   CLOSE(13)

   OPEN(UNIT=13,FILE='PseudoRefs.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)

    IF (HCRspecs(3).EQ.51) THEN

   OPEN(UNIT=13,FILE='icesfmsyrefs.ctl')

   READ(13,*)
   READ(13,*)
   READ(13,*) Pseudorefspecs(1)
   READ(13,*)
   READ(13,*) Pseudorefspecs(2)

	CLOSE(13)

    ENDIF

!	****survey specifications****

!	****survey index****

!	****survey lengths****

!	****survey ages****

	CLOSE(13)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine deals with writing the assessment data files, which may
!	differ from the base data files written during data generation
!
	SUBROUTINE WriteAssessData(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Yr2

	IF (Tier(1).EQ.3.AND.Tier(2).EQ.3) THEN
!	 CALL WriteCAF()
	ELSEIF (Tier(1).EQ.1) THEN
	 CALL WriteSS3dat(Yr2)
	 CALL WriteSS3ctl(Yr2)
     !CALL WriteRecSS3dat(Yr2)
	 !CALL WriteRecSS3ctl(Yr2)
	 CALL WriteSS2dat(Yr2)
	ELSEIF (Tier(1).EQ.8) THEN
	 CALL WriteSS3dat(Yr2)
	 CALL WriteSS3ctl(Yr2)
	ELSEIF (Tier(1).EQ.3.AND.Tier(2).EQ.2) THEN
	 IF (Yr2.EQ.Lyear) CALL ReadTier3specs()
	 CALL WritePuntdat(Yr2)
	ENDIF

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine deals with reading in the Tier 3 specs
!
	SUBROUTINE ReadTier3specs

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'
	INTEGER II,DUM
	
	OPEN(UNIT=13,FILE='Tier3.ctl')
	DO II=1,13
 	 READ(13,*)
	ENDDO
    READ(13,*) DUM,Tier3I(1)  !fleet
    READ(13,*)
	READ(13,*) Tier3I(2)   !nageyrs
	READ(13,*)
	READ(13,*) Tier3I(3)   !ncatyrs
	READ(13,*)
	READ(13,*) Tier3R(1)   !estM
	!WRITE(*,*) Tier3R(1)
	!STOP
	READ(13,*)
	READ(13,*) Tier3R(2)   !esth
	READ(13,*)
	READ(13,*) Tier3R(3)   !fracM for control rule
	READ(13,*)
	READ(13,*) Tier3R(4)   !cum err of F estimate
	READ(13,*)
	READ(13,*) Tier3I(4)   !HCRtype
	READ(13,*)
	READ(13,*) (Tier3I(II),II=5,7)   !ref points
	CLOSE(13)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the dat file for the Andre equilibrium approach
!
	SUBROUTINE WritePuntdat(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Yr2,IDUM,Iflt,JJ,II,Jyr,Jflt,Nsamp,ITempArray(100,100),Npts
	REAL*8 Temp,Temptot(100),Temp2,TempCat,AgeComp(0:200),EstDisc(100),TempArray(100,100)
	

	OPEN(UNIT=18,FILE='punt.dat')

	WRITE(18,'(A1)') '#'
	WRITE(18,'(I4)') MaxAge
	WRITE(18,'(A1)') '#'
	WRITE(18,'(F7.4)') Tier3R(1)
	WRITE(18,'(A1)') '#'
	WRITE(18,'(F7.4)') Tier3R(2)
	WRITE(18,'(A1)') '#'
	WRITE(18,'(200(F7.3,1x))') (Weight(1,1,Age,Yr2),Age=0,MaxAge)
	WRITE(18,'(A1)') '#'
	WRITE(18,'(200(F7.4,1x))') (Fecundity(1,Age,Yr2),Age=0,MaxAge)
	WRITE(18,'(A1)') '#'

	TempCat = 0.d0
	DO Jyr=(Yr2-Tier3I(3)+1),Yr2
	 TempCat = TempCat + SUM(RetCatch(1:Nflt,1:Nreg,Jyr))
	ENDDO
	!WRITE(*,*) TempCat,Tier3I(3),FLOAT(Tier3I(3))
	TempCat = TempCat / FLOAT(Tier3I(3))

	TempTot = 0.d0
	TempArray = 0.d0
	ItempArray = 0
	II=0
	EstDisc = 0.d0
	OPEN(UNIT=14,FILE='Discard.inp')
	READ(14,*)
41021 READ(14,*,END=41020,ERR=41020) Jyr,IDUM,Iflt,Temp
	!WRITE(99,*) Jyr,Iflt,Temp
	IF (Jyr.GE.(Yr2-HCRspecs(4)+1).AND.Jyr.LE.Yr2) THEN
	 TempArray((HCRspecs(4)-(Yr2-Jyr)),Iflt) = Temp
	 ItempArray((HCRspecs(4)-(Yr2-Jyr)),Iflt) = 1
	ENDIF
	GOTO 41021
41020 CONTINUE	  
	CLOSE(14)
	DO Jyr=1,HCRspecs(4)
	 Npts = SUM(ItempArray(Jyr,1:Nflt))
	 IF (Npts.EQ.1) EstDisc(Jyr) = SUM(TempArray(Jyr,1:Nflt))
	 IF (Npts.GT.1) THEN
	  TempTot = 0.d0
	  DO Iflt=1,Nflt
	   TempTot(Jyr) = TempTot(Jyr) + ItempArray(Jyr,Iflt)*SUM(RetCatch(Iflt,1:Nreg,(Yr2-HCRspecs(4)+Jyr)))
	   TempArray(Jyr,Iflt) = TempArray(Jyr,Iflt)*TempTot(Iflt)
	  ENDDO
	  EstDisc(Jyr) = SUM(TempArray(Jyr,1:Nflt))/TempTot(Jyr)
	 ENDIF
	ENDDO
	Temp = 0.d0
	Temp2 = 0.d0
	DO II=1,HCRspecs(4)
	 !WRITE(*,*) II, EstDisc(II),(1.d0/(2.d0**(HCRspecs(4)-II)))
	 IF (SUM(ItempArray(II,1:Nflt)).GT.0) THEN
	  Temp = Temp + (1.d0/(2.d0**(HCRspecs(4)-II)))*EstDisc(II) !TempTot(Yr2-II+1)
	  Temp2 = Temp2 + (1.d0/(2.d0**(HCRspecs(4)-II)))
	 ENDIF
	ENDDO
	Temp = Temp/Temp2
	!WRITE(*,*) TempCat,Temp
    TempCat = TempCat/(1.d0-Temp)
	!WRITE(*,*) TempCat
	IF (TempCat.LE.1.d0) TempCat = 1.0d0
	WRITE(18,'(F15.0)') TempCat
	OPEN(UNIT=14,FILE='FisheryAges.inp')
	JJ=0
	READ(14,*)
41022 READ(14,*,END=41023,ERR=41023) Jyr,Jflt
	 IF (Jyr.GE.(Yr2-Tier3I(2)+1).AND.Jyr.LE.Yr2.AND.Jflt.EQ.Tier3I(1)) JJ=JJ+1
	 GOTO 41022
41023 CONTINUE
	 CLOSE(14)
	WRITE(18,'(A1)') '#'
	WRITE(18,'(I4)') JJ
	WRITE(18,'(A1)') '#'
	OPEN(UNIT=14,FILE='FisheryAges.inp')
	READ(14,*)
	!WRITE(*,*) Yr2,Tier3I(1:2)
41024 READ(14,*,END=41025,ERR=41025) Jyr,Jflt,IDUM,IDUM,IDUM,Nsamp,(AgeComp(Age),Age=0,MaxAge)
	 !WRITE(*,*) Jyr,Jflt
	 IF (Jyr.GE.(Yr2-Tier3I(2)+1).AND.Jyr.LE.Yr2.AND.Jflt.EQ.Tier3I(1)) WRITE(18,'(I4,1x,I6,1x,200(F14.7,1x))') Jyr,Nsamp,(AgeComp(Age),Age=1,MaxAge+1)
	 GOTO 41024
41025 CONTINUE
	 CLOSE(14)
	WRITE(18,'(A1)') '#'
	WRITE(18,'(I2)') 2
    WRITE(18,'(3(I3,1x))') (Tier3I(II),II=5,7)
	CLOSE(18)

!	STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the SS2.dat file
!
	SUBROUTINE WriteSS2dat(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER	Yr2,II,Iflt,Npts,Jflt,Iyr,JJ,DUM1,DUM2,Nestflt
	REAL*8 Temp(1000)
	CHARACTER*27 String
	CHARACTER*8 CHAR1
	CHARACTER*4000 CHAR2

	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*) String    ! = 'fleet1%fleet2%fleet3%fleet4'
	CLOSE(18)

	OPEN(UNIT=18,FILE='SS2.dat')

    !Nestflt = INT(SUM(FleetRegions))

	WRITE(18,'(I4)') Fyear	!1st yr
	WRITE(18,'(I4)') Yr2		!last yr
	WRITE(18,'(I1)') 1		!n seasons
	WRITE(18,'(I2)') 12		!months in season
	WRITE(18,'(I1)') 1		!spawning season
	WRITE(18,'(I2)') Nflt	!# of fleets
	WRITE(18,'(I1)') 0 !number of surveys - need to change once data spec / generation stuff for this written
	WRITE(18,'(A27)') String !string of fleet names etc. - perhaps read in from tier1.ctl
	Temp(1:Nflt) = 0.5d0
	WRITE(18,'(10(F5.3,1x))') (Temp(II),II=1,Nflt)
	WRITE(18,'(I1)') 2	!number of genders
	WRITE(18,'(I3)') MaxAge !plus group age
	WRITE(18,*)
	!Landings
	Temp(1:Nflt) = 0.d0
	WRITE(18,'(10(F2.0,1x))') (Temp(II),II=1,Nflt)
	 DO 8701 Iyr=Fyear,Yr2
	  WRITE(18,'(10(F10.2,1x))') (SUM(Catch(Iflt,1:Nreg,Iyr)),Iflt=1,Nflt)
8701 CONTINUE	
	WRITE(18,*)
	!CPUE/Surveys
	!ignore spatial stuff for now because initial tests only 1 region
	OPEN(UNIT=15,FILE='CPUE.inp')
	Npts = 0 
	READ(15,*)
	 DO 8702 II=1,20000
 	  READ(15,*,END=8791,ERR=8791) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8702 CONTINUE
8791 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of cpue points
	 DO 8703 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='CPUE.inp')
	  READ(15,*)
	  DO 8704 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8704  CONTINUE
	  CLOSE(15)
8703 CONTINUE

	!discards
	WRITE(18,*)
	WRITE(18,'(I2)') Discflag(1)  !discard type
	OPEN(UNIT=15,FILE='Discard.Inp')
	Npts=0
	READ(15,*)
	 DO 8721 II=1,200000
	  READ(15,*,END=8794,ERR=8794) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8721 CONTINUE
8794 CONTINUE
    CLOSE(15)
	WRITE(18,'(I4)') Npts	!number of discard data
	 DO 8722 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='Discard.Inp')
	  READ(15,*)
	  DO 8723 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8723  CONTINUE
      CLOSE(15)
8722 CONTINUE

	!mean body wt
	WRITE(18,*)
	WRITE(18,'(I2)') 0  !discard type

	!composition conditioners
	WRITE(18,*)
	WRITE(18,'(F7.4)') -0.0001
	WRITE(18,'(F7.4)') 0.0001

	WRITE(18,'(I3)') Nlen !number of length bins
	WRITE(18,'(100(F7.2,1x))') (lolenbin(II),II=1,Nlen) !lower bound of length bins

	!length comps
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryLengths.inp')
	Npts = 0 
	READ(15,*)
	 DO 8705 II=1,200000
 	  READ(15,'(A8,I2,A)',END=8792,ERR=8792) CHAR1,Iflt,CHAR2
	  Npts = Npts + 1
8705 CONTINUE
8792 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of length comps
	 DO 8706 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='FisheryLengths.inp')
	  READ(15,*)
	  DO 8707 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8707  CONTINUE
	  CLOSE(15)
8706 CONTINUE

	
	!N age bins
	WRITE(18,*)
	WRITE(18,'(I4)') MaxAge+1
	WRITE(18,'(200(I4,1x))') (II,II=0,MaxAge)

	!ageing error
	 WRITE(18,*) 
	 WRITE(18,'(I1)') 1
	 DO 8708 II=0,MaxAge
	  Temp(II+1) = II+0.5
8708 CONTINUE
	 WRITE(18,'(200(F5.1,1x))') (Temp(II),II=1,MaxAge+1)
	 Temp(1:MaxAge+1) = 0.001
	 WRITE(18,'(200(F5.3,1x))') (Temp(II),II=1,MaxAge+1)
	
	!age comps
	WRITE(18,*)
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryAges.inp')
	Npts = 0 
	READ(15,*)
	 DO 8709 II=1,200000
 	  READ(15,'(A8,I2,A)',END=8793,ERR=8793) CHAR1,Iflt,CHAR2
	  Npts = Npts + 1
8709 CONTINUE
8793 CONTINUE
	CLOSE(15)
    WRITE(18,*) Npts  !number of Age comps
	 DO 8710 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='FisheryAges.inp')
	  READ(15,*)
	  DO 8711 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8711  CONTINUE
	  CLOSE(15)
8710 CONTINUE

	!Mean size at age
	WRITE(18,*)
	WRITE(18,'(I2)') 0

	WRITE(18,*)
	WRITE(18,'(I2)') 0 	!N environmnt variables
	WRITE(18,'(I2)') 0 	!N environmnt observations

	WRITE(18,*)
	WRITE(18,'(I3)') 999

	CLOSE(18)

	!STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the SS3.ctl file  (v3.303a)
!
	SUBROUTINE WriteSS3ctl(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER AssessType,NRecYrs,II,Yr2,IROW,Nestflt
	CHARACTER*27 String
	CHARACTER*40 ctlfile
	CHARACTER*200 CTLstring
	CHARACTER*200 Tagline(100)
	CHARACTER*12 Tag,Tag2
	CHARACTER*3 IsEnd


	!Read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,5
	 READ (18,*)
	ENDDO
	READ(18,*) NrecYrs
	DO II=1,3
	 READ (18,*)
	ENDDO
	READ(18,*) AssessType
	READ(18,*)
	READ(18,*) String    ! = 'fleet1%fleet2%fleet3%fleet4'
	READ(18,*)
	READ(18,*) ctlfile
	DO II=1,3
	 READ (18,*)
	ENDDO
	DO II=1,5
	 READ(18,*)
	 READ(18,'(A200)') Tagline(II)
	ENDDO
	
!	DO II=1,4+Nestfleet
!	 READ(18,*)
! 	 READ(18,*) Tagline(II)
 !   ENDDO
	CLOSE(18)


    Nestflt = 0
    IF (AssessType.EQ.1) Nestflt = Nflt
    IF (AssessType.EQ.2) Nestflt = INT(SUM(FleetRegions))


	OPEN(UNIT=15,FILE='SS3.ctl')
	OPEN(UNIT=18,FILE=ctlfile)

      
     IROW=0
9100 READ(18,'(A200)') CTLstring
      IROW = IROW+1
	  IsEnd = CTLString
	  Tag = CTLstring
	  WRITE(15,'(A200)') CTLstring
	  IF (IsEnd.EQ.'999'.AND.IROW.GT.80) GOTO 9101
	  IF (Tag.EQ.'1 #do_recdev') THEN
	   READ(18,'(A200)') CTLstring
	   WRITE(15,'(A200)') CTLstring
	   WRITE(15,'(I4,1x,A68)') Yr2-NRecYrs,'# last year of main recr_devs; forecast devs start in following year'
	   READ(18,*)
	   DO II=1,8
	    READ(18,'(A200)') CTLstring
	    WRITE(15,'(A200)') CTLstring
	   ENDDO
	   WRITE(15,'(I4,1x,A31)') Yr2-NRecYrs,'#_last_yr_fullbias_adj_in_MPD'
       WRITE(15,'(I4,1x,A36)') Yr2-NRecYrs+1,'#_first_recent_yr_nobias_adj_in_MPD'     	  
	   READ(18,*)
	   READ(18,*)
	  ENDIF
     IF (Tag.EQ.'1 # TG_custo') THEN
	  DO II=1,NGuessGroups  !NTagGroups
	   WRITE(15,'(A200)') Tagline(1)
      ENDDO
	  DO II=1,NGuessGroups  !NTagGroups
	   WRITE(15,'(A200)') Tagline(2)
      ENDDO
	  !DO II=1,Nreg
	  ! WRITE(15,'(A60)') '1 100 1.9 2 1 0.001 4 0 0 0 0 0 0 0 # TG_overdispersion_1_  '
	  !ENDDO
	  !DO II=(Nreg+1),NGuessGroups  !NTagGroups
	  ! WRITE(15,'(A200)') Tagline(3)
      !ENDDO
	  DO II=1,NGuessGroups  !NTagGroups
	   WRITE(15,'(A200)') Tagline(3)
      ENDDO	  
	  DO II=1,Nestflt
	   WRITE(15,'(A200)') Tagline(4)
      ENDDO
	  DO II=1,Nestflt
	   WRITE(15,'(A200)') Tagline(5)
      ENDDO
9102  READ(18,'(A200)') CTLstring
      Tag2 = CTLstring
      IF (Tag2.EQ.'1 #_Variance') THEN
	   WRITE(15,'(A200)') CTLstring
	   GOTO 9100
	  ELSE
	   GOTO 9102
	  ENDIF
	 ENDIF

     GOTO 9100

9101 CONTINUE

    
  !file = scan("starter.ss2",skip=1,what=list(ctl=""),n=1,comment.char="#")
  !ncols=125 
  !rawctl <- read.table(file=file$ctl,col.names=c(seq(1,ncols,by=1)),fill=T,quote="",colClasses="character",nrows=-1,comment.char="")
  !totlines = length(rawctl[,1])
  !begin  <- match("start_rec_year",substring(rawctl[,2],1,nchar("start_rec_year")))
  !recvec = rawctl[begin+1,1:5]
  !recvec[2] = Yr2-nyrs  
  !#paste to new ctl file
  !write.table(rawctl[1:begin,],file$ctl,quote=F,row.names=F,col.names=F)
  !write(as.matrix(recvec),file$ctl,ncolumns=5,append=T)
  !write.table(rawctl[begin+2:totlines,],file$ctl,quote=F,row.names=F,col.names=F,append=T)


	!STOP

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the SS3.dat file  (v3.303a)
!
	SUBROUTINE WriteSS3dat(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER	Yr2,II,Iflt,Npts,Jflt,Iyr,JJ,DUM1,DUM2,Nestflt,Itemp(1000),IG,Ireg
	INTEGER AssessType,Jyr,JG,Ntagrecaps(1:1500,1850:2150,1:10),Ntagrecaps2(1:1500,1890:2100,1:10),Ntagrels2(1:10000,3)
	REAL*8 Temp(1000),GuessTaggedAge2(1:10000),AgeErr(1:2,0:1000),DUM3
	CHARACTER*35 String
	CHARACTER*8 CHAR1
	CHARACTER*4000 CHAR2


	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='tooth_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*) AssessType
    Nestflt = 0
    IF (AssessType.EQ.1) Nestflt = Nflt
	IF (Nflt.EQ.7) Nestflt = 11
    IF (AssessType.EQ.2) Nestflt = INT(SUM(FleetRegions))
	READ(18,*)
	READ(18,*) String    ! = 'fleet1%fleet2%fleet3%fleet4'
    READ(18,*)
	READ(18,*)
	READ(18,*)
	READ(18,*) (AgeErr(1,II),II=0,MaxAge)
	READ(18,*) (AgeErr(2,II),II=0,MaxAge)
	DO II=1,11 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	CLOSE(18)


	OPEN(UNIT=18,FILE='SS3.dat')

    WRITE(18,'(A)') '#_Number_of_datafiles: 1'
    WRITE(18,'(A)') '#_start_nudata: 2'


	IF (SUM(RetCatch(1:Nflt,1:Nreg,1915)).GT.0) THEN
	 WRITE(18,'(I4)') 1915
	ELSEIF (SUM(RetCatch(1:Nflt,1:Nreg,1947)).GT.0) THEN
	 WRITE(18,'(I4)') 1947
	ELSE
	 WRITE(18,'(I4)') 1975 !Fyear	!1st yr
	ENDIF
	WRITE(18,'(I4)') Yr2		!last yr
	WRITE(18,'(I1)') 1		!n seasons
	WRITE(18,'(I2)') 12		!months in season
	WRITE(18,'(I1)') 1		!spawning season
	WRITE(18,'(I2)') Nestflt	!# of fleets
	WRITE(18,'(I1)') 0 !number of surveys - need to change once data spec / generation stuff for this written
	II = 2
	IF (AssessType.EQ.3) II=1
	IF (Nreg.EQ.1) THEN
	 WRITE(18,'(I2)') Nreg !# of Areas
	ELSE 
	 WRITE(18,'(I2)') II	!# of Areas
	ENDIF
	WRITE(18,'(A35)') String !string of fleet names etc. - perhaps read in from tier1.ctl
	Temp(1:Nestflt) = 0.5d0
	WRITE(18,'(10(F5.3,1x))') (Temp(II),II=1,Nestflt)  !season timing
    Itemp(1:Nestflt) = 1
	DO II=1,Nestflt
	 DO JJ=1,Nreg
	  IF (FleetRegions(II,JJ).EQ.1) Itemp(II) = JJ
	 ENDDO
	ENDDO
	WRITE(18,'(10(I1,1x))') (Itemp(II),II=1,Nestflt)  !area assignment
	Itemp(1:Nestflt) = 1
	WRITE(18,'(10(I1,1x))') (Itemp(II),II=1,Nestflt)  ! catch units (1=bio, 2=#s)
	Temp(1:Nestflt) = 0.0001d0                       
	WRITE(18,'(10(F6.4,1x))') (Temp(II),II=1,Nestflt) ! sd of log(catch)
	WRITE(18,'(I1)') 2	!number of genders
	WRITE(18,'(I3)') MaxAge !plus group age
	WRITE(18,*)
	!Landings
	Temp(1:Nestflt) = 0.d0
	WRITE(18,'(10(F2.0,1x))') (Temp(II),II=1,Nestflt)   !init_equilibrium catch
	 II=0
	 DO Iyr=Fyear,Yr2
	  IF (SUM(RetCatch(1:Nflt,1:Nreg,Iyr)).GT.0d0) II=II+1 
     ENDDO
     WRITE(18,'(I3)') II   !number of lines of catch
     DO 8701 Iyr=Fyear,Yr2
      IF (SUM(RetCatch(1:Nflt,1:Nreg,Iyr)).GT.0d0) THEN
       Temp = 0.d0
	   IF (AssessType.EQ.1) THEN
	    DO Iflt=1,Nestflt
	     Temp(Iflt) = SUM(RetCatch(Iflt,1:Nreg,Iyr))
	    ENDDO
	   ENDIF
	   IF (AssessType.EQ.2) THEN
	    Jflt = 0
	    DO Iflt=1,Nflt
	     DO Ireg=1,Nreg
		  Jflt=Jflt+1
		  Temp(Jflt) = RetCatch(Iflt,Ireg,Iyr)
		 ENDDO
	    ENDDO
       ENDIF
	   IF (Nestflt.EQ.2) WRITE(18,'(2(F10.3,1x),I4,1x,I2)') (Temp(Iflt),Iflt=1,Nestflt),Iyr,1
	   IF (Nestflt.EQ.3) WRITE(18,'(3(F10.3,1x),I4,1x,I2)') (Temp(Iflt),Iflt=1,Nestflt),Iyr,1
	   IF (Nestflt.EQ.5) WRITE(18,'(5(F10.3,1x),I4,1x,I2)') (Temp(Iflt),Iflt=1,Nestflt),Iyr,1
	   IF (Nestflt.EQ.6) WRITE(18,'(6(F10.3,1x),I4,1x,I2)') (Temp(Iflt),Iflt=1,Nestflt),Iyr,1
       IF (Nestflt.EQ.11) WRITE(18,'(11(F10.3,1x),I4,1x,I2)') (Temp(Iflt),Iflt=1,Nestflt),Iyr,1
	  ENDIF
8701 CONTINUE	
	WRITE(18,*)
	!CPUE/Surveys
	!ignore spatial stuff for now because initial tests only 1 region
	OPEN(UNIT=15,FILE='CPUE.inp')
	Npts = 0 
	READ(15,*)
	 DO 8702 II=1,20000
 	  READ(15,*,END=8791,ERR=8791) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8702 CONTINUE
8791 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of cpue points
	 DO 8703 Iflt=1,Nestflt
	  OPEN(UNIT=15,FILE='CPUE.inp')
	  READ(15,*)
	  DO 8704 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8704  CONTINUE
	  CLOSE(15)
8703 CONTINUE

	!discards
	WRITE(18,*)
	WRITE(18,'(I2)') Discflag(1)  !discard type
	OPEN(UNIT=15,FILE='Discard.Inp')
	Npts=0
	READ(15,*)
	 DO 8721 II=1,200000
	  READ(15,*,END=8794,ERR=8794) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8721 CONTINUE
8794 CONTINUE
    CLOSE(15)
	WRITE(18,'(I4)') Npts	!number of discard data
	 DO 8722 Iflt=1,Nestflt
	  OPEN(UNIT=15,FILE='Discard.Inp')
	  READ(15,*)
	  DO 8723 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8723  CONTINUE
      CLOSE(15)
8722 CONTINUE

	!mean body wt
	WRITE(18,*)
	WRITE(18,'(I2)') 0  !N_meanbodywt obs

    !composition conditioners
    WRITE(18,'(I1)') 1
	!WRITE(18,'(I1)') 2
    !WRITE(18,'(I2)') 5
    !WRITE(18,'(I4)') 10
    !WRITE(18,'(I4)') 200

	WRITE(18,*)
	WRITE(18,'(I2)') -1
	WRITE(18,'(F9.7)') 0.0000001

	WRITE(18,'(I2)') 0   

	WRITE(18,'(I3)') Nlen !number of length bins
	WRITE(18,'(100(F7.2,1x))') (lolenbin(II),II=1,Nlen) !lower bound of length bins

	!length comps
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryLengths.inp')
	Npts = 0 
	READ(15,*)
	 DO 8705 II=1,200000
	  READ(15,'(A8,I2,A)',END=8792,ERR=8792) CHAR1,Iflt,CHAR2
	  !WRITE(*,*) CHAR1,Iflt
	  Npts = Npts + 1
8705 CONTINUE
8792 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of length comps
	 DO 8706 Iflt=1,Nestflt
	  OPEN(UNIT=15,FILE='FisheryLengths.inp')
	  READ(15,*)
	  DO 8707 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8707  CONTINUE
	  CLOSE(15)
8706 CONTINUE

	
	!N age bins
	WRITE(18,*)
	WRITE(18,'(I4)') MaxAge+1
	WRITE(18,'(200(I4,1x))') (II,II=0,MaxAge)

	!ageing error
	 WRITE(18,*) 
	 WRITE(18,'(I1)') 1
!	 DO 8708 II=0,MaxAge
!	  Temp(II+1) = II+0.5
!8708 CONTINUE
	 WRITE(18,'(200(F5.1,1x))') (AgeErr(1,II),II=0,MaxAge)
	 Temp(1:MaxAge+1) = 0.001
	 WRITE(18,'(200(F7.4,1x))') (AgeErr(2,II),II=0,MaxAge)
	
	!age comps
	WRITE(18,*)
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryAges.inp')
	Npts = 0 
	READ(15,*)
	 DO 8709 II=1,200000
 	  READ(15,'(A8,I2,A)',END=8793,ERR=8793) CHAR1,Iflt,CHAR2
	  Npts = Npts + 1
8709 CONTINUE
8793 CONTINUE
	CLOSE(15)
    WRITE(18,*) Npts  !number of Age comps
	WRITE(18,'(I2)') 1  !lenbin method
	WRITE(18,'(I2)') 0  !combine genders below this bin
	 DO 8710 Iflt=1,Nestflt
	  OPEN(UNIT=15,FILE='FisheryAges.inp')
	  READ(15,*)
	  DO 8711 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8711  CONTINUE
	  CLOSE(15)
8710 CONTINUE

	!Mean size at age
	WRITE(18,*)
	WRITE(18,'(I2)') 0

	WRITE(18,*)
	WRITE(18,'(I2)') 0 	!N environmnt variables
	WRITE(18,'(I2)') 0 	!N environmnt observations


	WRITE(18,'(I2)') 0 !Nsizefreq methods
    

	!TAGGING DATA
	IF (NTagGroups.GT.0) THEN

     DO II=1,NGuessGroups
	  GuessTaggedAge2(II) = GuessTaggedAge(II)/GroupReleases(II)
	 ENDDO

	WRITE(18,'(I2)') 1 !do tags
!    OPEN(UNIT=15,FILE='TagReleases.Inp')
!	Npts = 0
!	READ(15,*)
!	 DO 8712 JJ=1,200000
! 	  READ(15,'(I4,A)',END=8782,ERR=8782) IG,CHAR1
!	  Npts = IG
!8712 CONTINUE
!8782 CONTINUE
!	CLOSE(15)
!    WRITE(18,*) Npts  !number of Tag groups
     WRITE(18,*) NGuessGroups  !number of Tag groups
    
    IF (EstTagAge(1).EQ.-101) THEN
	 OPEN(UNIT=15,FILE='NewTagRecaptures.Inp')
	ELSE	
	 OPEN(UNIT=15,FILE='TagRecaptures.Inp')
	ENDIF
	Npts = 0
	Ntagrecaps = 0
	Ntagrecaps2 = 0
	READ(15,*)
!	READ(15,*,END=8783,ERR=8783) Jyr,DUM1,Jflt,DUM1,DUM1,JG
!	Npts = Npts + 1
!	 DO 8713 JJ=1,200000
! 	  READ(15,'(I4,A)',END=8783,ERR=8783) Iyr,CHAR1
8713  READ(15,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)',END=8783,ERR=8783) Iyr,DUM1,Iflt,DUM1,JG,IG
	  Ntagrecaps(IG,Iyr,Iflt) = 1
	  Ntagrecaps2(IG,Iyr,Iflt) = Ntagrecaps2(IG,Iyr,Iflt)+JG
	  !WRITE(99,*) Iyr,Iflt,IG
	  !STOP
	  !IF (Jyr.NE.Iyr.OR.Jflt.NE.Iflt.OR.JG.NE.IG) Npts = Npts+1
	  !Jyr = Iyr
	  !Jflt = Iflt
	  !JG = IG
	 GOTO 8713
8783 CONTINUE
	CLOSE(15)
	Npts = SUM(Ntagrecaps)
    WRITE(18,'(I4)') Npts  !number of Tag recapture events 

	WRITE(18,'(I2)') 1 !mixing latency period
	WRITE(18,'(I2)') 99 !max seasons to track recoveires

    IF (EstTagAge(1).EQ.-101) THEN
	  OPEN(UNIT=15,FILE='NewTagReleases.Inp')
      READ(15,*)
	  Ntagrels2 = 0
	  GuessTaggedAge2 = 0
8714  READ(15,*,END=8784,ERR=8784) DUM1,Iyr,Iflt,Ireg,DUM2,Itemp(1),IG,DUM3,Itemp(2)
	  Ntagrels2(IG,1) = Ntagrels2(IG,1)+Itemp(1)
      Ntagrels2(IG,2) = Iyr
      Ntagrels2(IG,3) = Ireg
	  GuessTaggedAge2(IG) = Itemp(2)
	  GOTO 8714
8784 CONTINUE
	CLOSE(15)
    DO IG=1,NGuessGroups
	 WRITE(18,'(I4,1x,I2,1x,I4,1x,I2,1x,I3,1x,I2,1x,I2,1x,I6)') IG,Ntagrels2(IG,3),Ntagrels2(IG,2),1,999,0,INT(GuessTaggedAge2(IG)),Ntagrels2(IG,1)
	ENDDO

	ELSE

    !OPEN(UNIT=15,FILE='TagReleases.Inp')
    !READ(15,*)
      JG = 0  
	  OPEN(UNIT=15,FILE='TagReleases.Inp')
      READ(15,*)
	  DO 8712 JJ=1,200000
	   !READ(15,'(A200)',END=8782,ERR=8782) CHAR2
	   READ(15,'(I5,1x,I4,1x,I2,1x,I2,1x,I3,1x,I6,1x,I3)',END=8782,ERR=8782) DUM1,Iyr,Iflt,Ireg,DUM2,Itemp(1),IG
!	   WRITE(99,*) JJ,JG,IG
!	   WRITE(99,'(I6,1x,A200)') JJ,CHAR2
	   IF (IG.GT.JG) THEN
	    IF (EstTagAge(Iflt).GT.0) WRITE(18,'(I4,1x,I2,1x,I4,1x,I2,1x,I3,1x,I2,1x,I2,1x,I6)') IG,Ireg,Iyr,1,999,0,EstTagAge(Iflt),GroupReleases(IG)
        IF (EstTagAge(Iflt).LE.-1) WRITE(18,'(I4,1x,I2,1x,I4,1x,I2,1x,I3,1x,I2,1x,I2,1x,I6)') IG,Ireg,Iyr,1,999,0,INT(GuessTaggedAge2(IG)),GroupReleases(IG)
		JG = JG+1
	   ENDIF
8712  CONTINUE
8782  CONTINUE
     CLOSE(15)      

	ENDIF

	!extra bit for overdispersion specification, needed for gavss3
 !   WRITE(18,*)
!	WRITE(18,'(10000(I3,1x))') (GuessRegion(II),II=1,NGuessGroups)

    WRITE(18,*)
	!tag recaptures
	!Ntagrecaps = 0
	!OPEN(UNIT=15,FILE='TagRecaptures.Inp')
!	READ(15,*)
 !   READ(15,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)',END=8786,ERR=8786) Iyr,DUM1,Iflt,Ireg,Itemp(1),Itemp(2)
!	 Ntagrecaps(Itemp(2),Iyr,Iflt) = Itemp(1)

    DO 8715 IG=1,NGuessGroups
     DO 8715 Iyr=Fyear,Yr2
	  DO 8715 Iflt=1,Nflt
	   IF(Ntagrecaps(IG,Iyr,Iflt).GT.0) WRITE(18,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') IG,Iyr,1,Iflt,Ntagrecaps2(IG,Iyr,Iflt)

!	 Npts = 0
 !    OPEN(UNIT=15,FILE='TagRecaptures.Inp')     	
!	 READ(15,*)
!	 Jyr = Fyear-1
!     Jflt = 0
!	 DO 8716 JJ=1,200000
!	  READ(15,'(I4,1x,I4,1x,I2,1x,I2,1x,I6,1x,I6)',END=8786,ERR=8786) Iyr,DUM1,Iflt,Ireg,Itemp(1),Itemp(2)
!	  !IF (Itemp(1).EQ.II) WRITE(18,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') Itemp(1),Iyr,1,Iflt,Itemp(2)
!	  IF (Itemp(2).EQ.II) THEN
 !      IF (Jyr.NE.Iyr.OR.Jflt.NE.Iflt) THEN
!	    IF (Npts.GT.0) THEN
!		 WRITE(18,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') Itemp(2),Jyr,1,Jflt,Npts
!		 Npts = 0
!		ENDIF
!		IF (Npts.EQ.0) Npts = Itemp(1)		
!	   ELSE
!	    Npts = Npts + Itemp(1)
!	   ENDIF
 !      Jyr = Iyr
!	   Jflt = Iflt
!	  ENDIF
!8716 CONTINUE
8715 CONTINUE		 
!8786 CLOSE(15)
!     IF (Npts.GT.0) WRITE(18,'(I4,1x,I4,1x,I2,1x,I2,1x,I6)') II,Jyr,1,Jflt,Npts
    WRITE(18,'(A18)') '# end of tag data'

    ELSE	
	 WRITE(18,'(I2)') 0 !don't do tags
    ENDIF


	WRITE(18,'(I2)') 0 !no morphcomp data

	WRITE(18,*)
	WRITE(18,'(I3)') 999

	CLOSE(18)

	!STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the SS3.ctl file  (v3.303a) for the recreational fishing analyses
!
	SUBROUTINE WriteRecSS3ctl(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER AssessType,NRecYrs,II,Yr2,IROW,Nestflt
	CHARACTER*27 String
	CHARACTER*40 ctlfile
	CHARACTER*200 CTLstring
	CHARACTER*200 Tagline(100)
	CHARACTER*12 Tag,Tag2
	CHARACTER*3 IsEnd

	!Read from tier1.ctl
	OPEN(UNIT=18,FILE='rec_ss3_tier1.ctl')
	DO II=1,5
	 READ (18,*)
	ENDDO
	READ(18,*) NrecYrs
	DO II=1,3
	 READ (18,*)
	ENDDO
	READ(18,*) AssessType
	READ(18,*)
	READ(18,*) String    ! = 'fleet1%fleet2%fleet3%fleet4'
	READ(18,*)
	READ(18,*) ctlfile
	DO II=1,3
	 READ (18,*)
	ENDDO
	DO II=1,5
	 READ(18,*)
	 READ(18,'(A200)') Tagline(II)
	ENDDO
	
!	DO II=1,4+Nestfleet
!	 READ(18,*)
! 	 READ(18,*) Tagline(II)
 !   ENDDO
	CLOSE(18)

    Nestflt = 0
    IF (AssessType.EQ.1) Nestflt = Nflt
    IF (AssessType.EQ.2) Nestflt = INT(SUM(FleetRegions))


	OPEN(UNIT=15,FILE='SS3.ctl')
	OPEN(UNIT=18,FILE=ctlfile)

      
     IROW=0
9100 READ(18,'(A200)') CTLstring
      IROW = IROW+1
	  IsEnd = CTLString
	  Tag = CTLstring
	  WRITE(15,'(A200)') CTLstring
	  IF (IsEnd.EQ.'999'.AND.IROW.GT.80) GOTO 9101
	  IF (Tag.EQ.'1 #do_recdev') THEN
	   READ(18,'(A200)') CTLstring
	   WRITE(15,'(A200)') CTLstring
	   WRITE(15,'(I4,1x,A68)') Yr2-NRecYrs,'# last year of main recr_devs; forecast devs start in following year'
	   READ(18,*)
	   DO II=1,8
	    READ(18,'(A200)') CTLstring
	    WRITE(15,'(A200)') CTLstring
	   ENDDO
	   WRITE(15,'(I4,1x,A31)') Yr2-NRecYrs,'#_last_yr_fullbias_adj_in_MPD'
       WRITE(15,'(I4,1x,A36)') Yr2-NRecYrs+1,'#_first_recent_yr_nobias_adj_in_MPD'     	  
	   READ(18,*)
	   READ(18,*)
	  ENDIF
     IF (Tag.EQ.'1 # TG_custo') THEN
	  DO II=1,NTagGroups
	   WRITE(15,'(A200)') Tagline(1)
      ENDDO
	  DO II=1,NTagGroups
	   WRITE(15,'(A200)') Tagline(2)
      ENDDO	  
	  DO II=1,NTagGroups
	   WRITE(15,'(A200)') Tagline(3)
      ENDDO
	  DO II=1,Nestflt
	   WRITE(15,'(A200)') Tagline(4)
      ENDDO
	  DO II=1,Nestflt
	   WRITE(15,'(A200)') Tagline(5)
      ENDDO
9102  READ(18,'(A200)') CTLstring
      Tag2 = CTLstring
      IF (Tag2.EQ.'1 #_Variance') THEN
	   WRITE(15,'(A200)') CTLstring
	   GOTO 9100
	  ELSE
	   GOTO 9102
	  ENDIF
	 ENDIF

     GOTO 9100

9101 CONTINUE

    
  !file = scan("starter.ss2",skip=1,what=list(ctl=""),n=1,comment.char="#")
  !ncols=125 
  !rawctl <- read.table(file=file$ctl,col.names=c(seq(1,ncols,by=1)),fill=T,quote="",colClasses="character",nrows=-1,comment.char="")
  !totlines = length(rawctl[,1])
  !begin  <- match("start_rec_year",substring(rawctl[,2],1,nchar("start_rec_year")))
  !recvec = rawctl[begin+1,1:5]
  !recvec[2] = Yr2-nyrs  
  !#paste to new ctl file
  !write.table(rawctl[1:begin,],file$ctl,quote=F,row.names=F,col.names=F)
  !write(as.matrix(recvec),file$ctl,ncolumns=5,append=T)
  !write.table(rawctl[begin+2:totlines,],file$ctl,quote=F,row.names=F,col.names=F,append=T)


	!STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine writes the SS3.dat file  (v3.303a) for the recreational fishing analyses
!
	SUBROUTINE WriteRecSS3dat(Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER	Yr2,II,Iflt,Npts,Jflt,Iyr,JJ,DUM1,DUM2,Nestflt,Itemp(1000),IG,Ireg
	INTEGER AssessType
	REAL*8 Temp(1000),EstCatCV
	CHARACTER*27 String
	CHARACTER*8 CHAR1
	CHARACTER*2000 CHAR2

	!temporary - will read from tier1.ctl
	OPEN(UNIT=18,FILE='rec_ss3_tier1.ctl')
	DO II=1,9
	 READ (18,*)
	ENDDO
	READ(18,*) AssessType
    Nestflt = 0
    IF (AssessType.EQ.1) Nestflt = Nflt
    IF (AssessType.EQ.2) Nestflt = INT(SUM(FleetRegions))
	READ(18,*)
	READ(18,*) String    ! = 'fleet1%fleet2%fleet3%fleet4'
    DO II=1,16 !5+Nestflt
     READ(18,*)
	ENDDO
	READ(18,*) (EstTagAge(II),II=1,Nflt)
	!DO II=1,5
	 !READ(18,*)
	!ENDDO
	!READ(18,*) EstCatCV
	CLOSE(18)

	OPEN(UNIT=18,FILE='SS3.dat')

    WRITE(18,'(A)') '#_Number_of_datafiles: 1'
    WRITE(18,'(A)') '#_start_nudata: 2'

	WRITE(18,'(I4)') 1950 !Fyear	!1st yr
	WRITE(18,'(I4)') Yr2		!last yr
	WRITE(18,'(I1)') 1		!n seasons
	WRITE(18,'(I2)') 12		!months in season
	WRITE(18,'(I1)') 1		!spawning season
	WRITE(18,'(I2)') Nestflt	!# of fleets
	WRITE(18,'(I1)') 0 !number of surveys - need to change once data spec / generation stuff for this written
	WRITE(18,'(I2)') 1	!# of Areas
	WRITE(18,'(A27)') String !string of fleet names etc. - perhaps read in from tier1.ctl
	Temp(1:Nestflt) = 0.5d0
	WRITE(18,'(10(F5.3,1x))') (Temp(II),II=1,Nestflt)  !season timing
    Itemp(1:Nestflt) = 1
	WRITE(18,'(10(I1,1x))') (Itemp(II),II=1,Nestflt)  !area assignment
	WRITE(18,'(10(I1,1x))') (Itemp(II),II=1,Nestflt)  ! catch units (1=bio, 2=#s)
	Temp(1:Nestflt) = 0.0001d0
	!IF (EstCatCV.GT.0) Temp(1) = EstCatCV                       
!	DO Iflt=1,Nestflt
!	 IF (CatCV(Iflt).GT.0) Temp(Iflt) = CatCV(Iflt)
!	ENDDO
	WRITE(18,'(10(F6.4,1x))') (Temp(II),II=1,Nestflt) ! sd of log(catch)
	WRITE(18,'(I1)') 2	!number of genders
	WRITE(18,'(I3)') MaxAge !plus group age
	WRITE(18,*)
	!Landings
	Temp(1:Nestflt) = 0.d0
	WRITE(18,'(10(F2.0,1x))') (Temp(II),II=1,Nestflt)   !init_equilibrium catch
	 II=0
	 DO Iyr=Fyear,Yr2
	  IF (SUM(RetCatchDat(1:Nflt,1:Nreg,Iyr)).GT.0d0) II=II+1 
     ENDDO
     WRITE(18,'(I3)') II   !number of lines of catch
     DO 8701 Iyr=Fyear,Yr2
      IF (SUM(RetCatchDat(1:Nflt,1:Nreg,Iyr)).GT.0d0) THEN
       Itemp = 0.d0
	   IF (AssessType.EQ.1) THEN
	    DO Iflt=1,Nestflt
	     Itemp(Iflt) = NINT(SUM(RetCatchDat(Iflt,1:Nreg,Iyr)))
	    ENDDO
	   ENDIF
	   IF (AssessType.EQ.2) THEN
	    Jflt = 0
	    DO Iflt=1,Nflt
	     DO Ireg=1,Nreg
		  Jflt=Jflt+1
		  Itemp(Jflt) = NINT(RetCatchDat(Iflt,Ireg,Iyr))
		 ENDDO
	    ENDDO
       ENDIF
	   WRITE(18,'(10(I8,1x))') (Itemp(Iflt),Iflt=1,Nestflt),Iyr,1
	  ENDIF
8701 CONTINUE	
	WRITE(18,*)
	!CPUE/Surveys
	!ignore spatial stuff for now because initial tests only 1 region
	OPEN(UNIT=15,FILE='CPUE.inp')
	Npts = 0 
	READ(15,*)
	 DO 8702 II=1,20000
 	  READ(15,*,END=8791,ERR=8791) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8702 CONTINUE
8791 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of cpue points
	 DO 8703 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='CPUE.inp')
	  READ(15,*)
	  DO 8704 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8704  CONTINUE
	  CLOSE(15)
8703 CONTINUE

	!discards
	WRITE(18,*)
	WRITE(18,'(I2)') Discflag(1)  !discard type
	OPEN(UNIT=15,FILE='Discard.Inp')
	Npts=0
	READ(15,*)
	 DO 8721 II=1,200000
	  READ(15,*,END=8794,ERR=8794) DUM1,DUM2,Iflt,(Temp(JJ),JJ=1,2)
	  Npts = Npts + 1
8721 CONTINUE
8794 CONTINUE
    CLOSE(15)
	WRITE(18,'(I4)') Npts	!number of discard data
	 DO 8722 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='Discard.Inp')
	  READ(15,*)
	  DO 8723 II=1,Npts
	   READ(15,*) DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
	   IF (Jflt.EQ.Iflt) WRITE(18,'(I4,1x,I2,1x,I2,1x,F10.4,1x,F5.3)') DUM1,DUM2,Jflt,(Temp(JJ),JJ=1,2)
8723  CONTINUE
      CLOSE(15)
8722 CONTINUE

	!mean body wt
	WRITE(18,*)
	WRITE(18,'(I2)') 0  !N_meanbodywt obs

    !composition conditioners
    WRITE(18,'(I1)') 1
	!WRITE(18,'(I1)') 2
    !WRITE(18,'(I2)') 5
    !WRITE(18,'(I4)') 10
    !WRITE(18,'(I4)') 200

	WRITE(18,*)
	WRITE(18,'(I2)') -1
	WRITE(18,'(F9.7)') 0.0000001

	WRITE(18,'(I2)') 0   

	WRITE(18,'(I3)') Nlen !number of length bins
	WRITE(18,'(100(F7.2,1x))') (lolenbin(II),II=1,Nlen) !lower bound of length bins

	!length comps
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryLengths.inp')
	Npts = 0 
	READ(15,*)
	 DO 8705 II=1,200000
 	  READ(15,'(A8,I2,A)',END=8792,ERR=8792) CHAR1,Iflt,CHAR2
	  Npts = Npts + 1
8705 CONTINUE
8792 CONTINUE
	CLOSE(15)
    WRITE(18,'(I4)') Npts  !number of length comps
	 DO 8706 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='FisheryLengths.inp')
	  READ(15,*)
	  DO 8707 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8707  CONTINUE
	  CLOSE(15)
8706 CONTINUE

	
	!N age bins
	WRITE(18,*)
	WRITE(18,'(I4)') MaxAge+1
	WRITE(18,'(200(I4,1x))') (II,II=0,MaxAge)

	!ageing error
	 WRITE(18,*) 
	 WRITE(18,'(I1)') 1
	 DO 8708 II=0,MaxAge
	  Temp(II+1) = II+0.5
8708 CONTINUE
	 WRITE(18,'(200(F5.1,1x))') (Temp(II),II=1,MaxAge+1)
	 Temp(1:MaxAge+1) = 0.001
	 WRITE(18,'(200(F5.3,1x))') (Temp(II),II=1,MaxAge+1)
	
	!age comps
	WRITE(18,*)
	!get number of data points
	OPEN(UNIT=15,FILE='FisheryAges.inp')
	Npts = 0 
	READ(15,*)
	 DO 8709 II=1,200000
 	  READ(15,'(A8,I2,A)',END=8793,ERR=8793) CHAR1,Iflt,CHAR2
	  Npts = Npts + 1
8709 CONTINUE
8793 CONTINUE
	CLOSE(15)
    WRITE(18,*) Npts  !number of Age comps
	WRITE(18,'(I2)') 1  !lenbin method
	WRITE(18,'(I2)') 0  !combine genders below this bin
	 DO 8710 Iflt=1,Nflt
	  OPEN(UNIT=15,FILE='FisheryAges.inp')
	  READ(15,*)
	  DO 8711 II=1,Npts
	   READ(15,'(A8,I2,A)') CHAR1,Jflt,CHAR2
	   IF (Jflt.EQ.IFlt) WRITE(18,'(A8,1x,I2,1x,A)') CHAR1,Jflt,CHAR2
8711  CONTINUE
	  CLOSE(15)
8710 CONTINUE

	!Mean size at age
	WRITE(18,*)
	WRITE(18,'(I2)') 0

	WRITE(18,*)
	WRITE(18,'(I2)') 0 	!N environmnt variables
	WRITE(18,'(I2)') 0 	!N environmnt observations


	WRITE(18,'(I2)') 0 !Nsizefreq methods
    

	!TAGGING DATA
	WRITE(18,'(I2)') 0 !don't do tags

	WRITE(18,'(I2)') 0 !no morphcomp data

	WRITE(18,*)
	WRITE(18,'(I3)') 999

	CLOSE(18)

	!STOP

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!