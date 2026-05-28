! ************************************************************************
!
!     This library contains all the routines related to generating random
!     variables and condutcing numerical work used by assess.for
!         XNORM   - Generates a normal random variate
!         STUDENT - Generates a t-random variables
!         RAN?    - Generates a uniform random variate
!         GENMULT - Generates a multivariate random variate (normal / t)
!         FIT     - The Amoeba non-linear minimization method
!
! ************************************************************************
!
      REAL*8 FUNCTION XNORM(INO,MEAN,SIGG,ISEED)
!
!     Uniform to Normal conversion routine
!
      INTEGER ISEED,INO
      REAL*8 Z1, Z2, MEAN, SIGG
      REAL*4 RAN1,RAN2,RAN3,RAN4,RAN5
!
!     Ignore zero calls
      IF (SIGG.EQ.0.0) THEN
        XNORM = 0.0
        RETURN
      ENDIF
!
      IF (INO.EQ.1) THEN
        Z1 = RAN1(ISEED)
   1    Z2 = RAN1(ISEED)
        IF (Z2.LE.0) GOTO 1
      ENDIF
      IF (INO.EQ.2) THEN
        Z1 = RAN2(ISEED)
   2    Z2 = RAN2(ISEED)
        IF (Z2.LE.0) GOTO 2
      ENDIF
      IF (INO.EQ.3) THEN
        Z1 = RAN3(ISEED)
   3    Z2 = RAN3(ISEED)
        IF (Z2.LE.0) GOTO 3
      ENDIF
      IF (INO.EQ.4) THEN
        Z1 = RAN4(ISEED)
   4    Z2 = RAN4(ISEED)
        IF (Z2.LE.0) GOTO 4
      ENDIF
	IF (INO.EQ.5) THEN
        Z1 = RAN5(ISEED)
   5    Z2 = RAN5(ISEED)
        IF (Z2.LE.0) GOTO 5
      ENDIF
!
      XNORM = SIN(6.283185*Z1) * SQRT(-2.*LOG(Z2)) * SIGG+MEAN
!
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      REAL*8 FUNCTION STUDENT(INO,MEAN,SIGG,d,ISEED)
!
!     This subroutine generates variables from a t-distribution.
!
      REAL*8 V1,V2,E,Y
      REAL*8 d,MEAN,SIGG
      INTEGER ISEED,INUM
      REAL*4 RAN1, RAN2,RAN3
      EXTERNAL RAN1,RAN2,RAN3
!
      INUM = 0
1     IF (INO.EQ.1) THEN
        V1 = 2.0*RAN1(ISEED)-1.0
        V2 = 2.0*RAN1(ISEED)-1.0
      ENDIF
      IF (INO.EQ.2) THEN
        V1 = 2.0*RAN2(ISEED)-1.0
        V2 = 2.0*RAN2(ISEED)-1.0
      ENDIF
      IF (INO.EQ.3) THEN
        V1 = 2.0*RAN3(ISEED)-1.0
        V2 = 2.0*RAN3(ISEED)-1.0
      ENDIF
      IF (V1.EQ.0) GOTO 1
      IF (V1**2.0+V2**2.0.GT.1.0) GOTO 1
      INUM = INUM +1
      Y = SQRT(d)*V2/V1
      E = (-d/2.0+0.5)*LOG(1.0+Y*Y/d)
      IF (LOG(RAN1(ISEED)+0.000001).GT.E) GOTO 1
      STUDENT = Y*SIGG + MEAN
!
      RETURN
      END
!
! ------------------------------------------------------------------------------

      REAL*8 FUNCTION POISSN (XM,RANNO)

      IMPLICIT NONE
      REAL*8 F,SS,R,XM,RANNO
      INTEGER I

      R = RANNO
      F = EXP(-XM)
      SS = F
      I = 0
   99 IF (SS.GE.R) GOTO 100
      I = I + 1
      F = F*XM/DBLE(I)
      SS = SS + F
      GOTO 99
  100 POISSN = DBLE(I)

      END

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
!
      REAL*8 FUNCTION GAMMAZ (PARX,PARS,ISEED)
!
!     Approximate random gamma generator: PARX = mean; PARS= sd
!
      EXTERNAL RAN3
      REAL*4 RAN3
      REAL*8 PARX,PARS,X,S,P,F,FN
      INTEGER ISEED,I
!
      X = PARX
      S = PARS
!
!     Convert to gamma parameters
      s = s*s / x
!
      P = 0.D0
      IF (S.GT.0.D0) THEN
        X = X/S
        I = INT(X)
        FN = AINT(X)
        IF (I.GT.0) THEN
!
!         Count down from i to zero
100       IF (I.NE.0) THEN
            F = RAN3(ISEED)
            IF (F.LE.0.D0) THEN
              WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
              F = 0.00001D0
            ENDIF
            P = P + LOG(F)
            I = I - 1
            GOTO 100
          ENDIF
        ENDIF
!
!       Usually this is the case (unless x is an integer)
        IF (X.GT.FN) THEN
          F = RAN3(ISEED)
          IF (F.LE.0.D0) THEN
            WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
            F = 0.00001D0
          ENDIF
          GAMMAZ = -S*( (X-FN)*LOG(F) + P )
        ELSE
          GAMMAZ = -S*P
        ENDIF

      ELSE
!
        GAMMAZ = X
!
      ENDIF
!
      RETURN
      END
!
! ------------------------------------------------------------------------------
      
      SUBROUTINE GENMUL(prop,TC,C,ISEED,WithRep,IStoc)

!     This subroutine generates a multinomial random variable

      IMPLICIT NONE
!
!     Global variables
      REAL*8 prop(2000),C(2000),Prop2(2000)
      INTEGER ISEED,ISEED3,TC,IStoc
      LOGICAL WithRep
!
!     Local variables
      REAL*8 tot,NTOT,Rmax
      INTEGER A,A1,NP,DUM(15000),I,ICOUNT,Itot,Ipnt
      REAL*4 RAN2,RAN3
      EXTERNAL RAN2,RAN3
!
!     Generate a random number seed
      ISEED3 = -100000*RAN2(ISEED) 

!
!     Check if some catches were made!
      DO 9 a = 1,2000
       C(a) =  0.0
9     CONTINUE       
	
      IF (TC.eq.0) RETURN


!     First total the probabilities and then normalise
      tot = 0.0
      DO 10 a = 1,2000
       tot = tot + prop(a)
10    CONTINUE
      DO 11 a = 1,2000
       prop(a) = prop(a)/tot
11    CONTINUE
!
!     Check whether this is Stochastic
      IF (Istoc.EQ.0) THEN
       Itot = 0
       DO 999 a = 1,2000
        Prop2(a) = prop(a)*TC
        C(a) = NINT(Prop2(a))
        Prop2(a) = C(a)-Prop2(a)
        Itot = Itot + NINT(C(a))
999    CONTINUE        
       Rmax = 0
       !WRITE(*,*) Itot,TC
       IF (Itot.GT.TC) THEN
        DO 9991 a = 1,2000
         IF (Prop2(a).LT.Rmax) THEN
          Rmax = Prop2(a)
          Ipnt = a
         ENDIF
9991    CONTINUE
        !WRITE(*,*) 'Ab',C(Ipnt),prop2(Ipnt)
        C(Ipnt) = C(Ipnt) - 1
        Itot = Itot - 1
        Prop2(Ipnt) = 0
        GOTO 999
       ENDIF 
       IF (Itot.LT.TC) THEN
        DO 9992 a = 1,2000
         IF (Prop2(a).GT.Rmax) THEN
          Rmax = Prop2(a)
          Ipnt = a
         ENDIF
9992    CONTINUE
        !WRITE(*,*) 'Be',C(Ipnt),prop2(Ipnt)
        C(Ipnt) = C(Ipnt) + 1
        Itot = Itot + 1
        Prop2(Ipnt) = 0
        GOTO 999
       ENDIF 
       RETURN
      ENDIF  
!
!     Adjsut to handle "without-replacement" sampling
      NTOT = 10000.0
      IF (WithRep.AND.tot.LT.14000) NTOT = tot
!
!     Do special set up for index matrix
      NP = 0
      DO 13 a = 1,2000
       ICOUNT = NINT(prop(a)*NTOT)
       IF (ICOUNT.GT.0) THEN
        DO 14 I = 1,ICOUNT
         DUM(NP+I) = a
14      CONTINUE
        NP = NP + ICOUNT
       ENDIF
13    CONTINUE

!     Do actual generation
      DO 15 I = 1,TC

!      Generate an age
16     a1 = INT(RAN3(ISEED3)*NP+1)
       a = DUM(a1)
       IF (a.LE.0.AND.WithRep) GOTO 16
       C(a) = C(a) + 1
       IF (WithRep) DUM(a1) = -1
!
15    CONTINUE


      RETURN
      END
!      
! ------------------------------------------------------------------------------
!
      FUNCTION RAN1(ISEED)
!
!     Copyright Numerical Recipes Software
!     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
!     CUP, Cambridge 1986 (Page 199).
!
!     Function returns a uniform random deviate between 0.0 & 1.0.
!     Set ISEED to any negative value to reinitialize the sequence.
!     Note: ISEED is reset to 1 on exit
!
      IMPLICIT REAL*8(M)
      PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      SAVE MA,INEXT,INEXTP,IFF
!
      DIMENSION MA(55)
      DATA IFF /0/
      IF (ISEED.LT.0.OR.IFF.EQ.0) THEN
        IFF = 1
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11      CONTINUE
        DO 13 K = 1,4
          DO 12 I = 1,55
            MA(I) = MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12        CONTINUE
   13      CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN1 = MJ*FAC
      RETURN
      END
!
! ------------------------------------------------------------------------------
!
      FUNCTION RAN2(ISEED)
!
!     Copyright Numerical Recipes Software
!     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
!     CUP, Cambridge 1986 (Page 199).
!
!     Function returns a uniform random deviate between 0.0 & 1.0.
!     Set ISEED to any negative value to reinitialize the sequence.
!     Note: ISEED is reset to 1 on exit
!
      IMPLICIT REAL*8(M)
      PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      SAVE MA,INEXT,INEXTP,IFF
!
      DIMENSION MA(55)
      DATA IFF /0/
      IF (ISEED.LT.0.OR.IFF.EQ.0) THEN
        IFF = 1
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11      CONTINUE
        DO 13 K = 1,4
          DO 12 I = 1,55
            MA(I) = MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12        CONTINUE
   13      CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN2 = MJ*FAC
      RETURN
      END
!
! ------------------------------------------------------------------------------
!
      FUNCTION RAN3(ISEED)
!
!     Copyright Numerical Recipes Software
!     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
!     CUP, Cambridge 1986 (Page 199).
!
!     Function returns a uniform random deviate between 0.0 & 1.0.
!     Set ISEED to any negative value to reinitialize the sequence.
!     Note: ISEED is reset to 1 on exit
!
      IMPLICIT REAL*8(M)
      PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      SAVE MA,INEXT,INEXTP,IFF
!
      DIMENSION MA(55)
      DATA IFF /0/
      IF (ISEED.LT.0.OR.IFF.EQ.0) THEN
        IFF = 1
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11      CONTINUE
        DO 13 K = 1,4
          DO 12 I = 1,55
            MA(I) = MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12        CONTINUE
   13      CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN3 = MJ*FAC
      RETURN
      END
!
! ------------------------------------------------------------------------------
!
      FUNCTION RAN4(ISEED)
!
!     Copyright Numerical Recipes Software
!     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
!     CUP, Cambridge 1986 (Page 199).
!
!     Function returns a uniform random deviate between 0.0 & 1.0.
!     Set ISEED to any negative value to reinitialize the sequence.
!     Note: ISEED is reset to 1 on exit
!
      IMPLICIT REAL*8(M)
      PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      SAVE MA,INEXT,INEXTP,IFF
!
      DIMENSION MA(55)
      DATA IFF /0/
      IF (ISEED.LT.0.OR.IFF.EQ.0) THEN
        IFF = 1
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11      CONTINUE
        DO 13 K = 1,4
          DO 12 I = 1,55
            MA(I) = MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12        CONTINUE
   13      CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN4 = MJ*FAC
      RETURN
      END
!
! ------------------------------------------------------------------------------
!
      FUNCTION RAN5(ISEED)
!
!     Copyright Numerical Recipes Software
!     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
!     CUP, Cambridge 1986 (Page 199).
!
!     Function returns a uniform random deviate between 0.0 & 1.0.
!     Set ISEED to any negative value to reinitialize the sequence.
!     Note: ISEED is reset to 1 on exit
!
      IMPLICIT REAL*8(M)
      PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      SAVE MA,INEXT,INEXTP,IFF
!
      DIMENSION MA(55)
      DATA IFF /0/
      IF (ISEED.LT.0.OR.IFF.EQ.0) THEN
        IFF = 1
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11      CONTINUE
        DO 13 K = 1,4
          DO 12 I = 1,55
            MA(I) = MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12        CONTINUE
   13      CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN5 = MJ*FAC
!	WRITE(99,*) RAN5
      RETURN
      END
!
! -------------------------------------------------------------------------
!
      SUBROUTINE GenMult(VEC,MEANS,ISEED,NPARS,TT,SG,MPA)
!
!     Generate from a multivariant normal (Taken from MANTST, (C) C.
!     Allison (IWC). Set IDIST to 1 for normal and to 2 for t.
!     RESRES is the probability of having selected this option.
!
      IMPLICIT NONE
!
!     Global variables
      INTEGER MPA
      REAL*8 VEC(MPA),MEANS(MPA),TT(MPA,MPA),SG(MPA)
      INTEGER ISEED,NPARS
      REAL*8 XNORM
      EXTERNAL XNORM
!
!     Local variables
      REAL*8 AA(500),X,SIGS
      INTEGER*4 I,J
!
!     Initial errors to zero
      DO 1400 I = 1,NPARS
        AA(I) = 0.0
1400  CONTINUE
!
!     Generate the multivariante normal
      DO 1500 I = 1,NPARS
        SIGS = SG(I)
        X = XNORM(4,0.0d0,SIGS,ISEED) 
        DO 1600 J = 1,NPARS
          AA(J) = AA(J) + X*TT(J,I)
1600    CONTINUE
1500  CONTINUE
!
!     Now complete the generation
      DO 1800 I = 1,NPARS
        VEC(I) = MEANS(I) + AA(I)
1800  CONTINUE
!
      RETURN
701   FORMAT(1x,I3,2(1x,F7.5),1x,F10.3,1x,2(G10.5,1x),F7.3,1x,G10.5)
702   FORMAT(1x,I3,1x,F7.3,2(1x,G10.5))
      END
!
! *********************************************************************

      INTEGER FUNCTION IBin1(Mean,Prob,ISEED,ISTOC)

!     This subroutine implements the binomial*
!
!     Global variables
      INTEGER Mean,ISEED,ISTOC
      REAL*8 Prob
!
!     Local variables 
      REAL*8 Randd,RBin1
      INTEGER GenBin
      EXTERNAL GenBin
!
!     Common block for rounding
      COMMON / Bin1Rnd / BoolBin1,Round
      REAL*8 Round
      LOGICAL BoolBin1 
!
!     Quick check
      IF (Prob.LE.0.D0) THEN
        RBin1 = 0
      ELSE IF (Prob.GE.1.D0) THEN
        RBin1 = Mean
!
!     What to do depends on whether this a stochastic model
      ELSE IF (ISTOC.GE.1) THEN
!
        Randd = RAN1(ISEED)
        RBin1 = GenBin(Randd,Prob,Mean)
! 
      ELSE
!
!       Deterministic model
        RBin1 = Mean*Prob
        IF (ABS(RBin1-INT(RBin1)-0.5).LT.0.0001) THEN
         IF (BoolBin1) THEN
           RBin1 = RBin1 + 0.5
         ELSE
           RBin1 = RBin1 - 0.5
         ENDIF
         BoolBin1 = .NOT.BoolBin1
        ENDIF     
!
      ENDIF
      IBin1 = NINT(RBin1)

      RETURN
      END



! *********************************************************************

      INTEGER FUNCTION IBin2(Mean,Prob,ISEED,ISTOC)

!     This subroutine implements the binomial*
!
!     Global variables
      INTEGER Mean,ISEED,ISTOC
      REAL*8 Prob
!
!     Local variables 
      REAL*8 Randd,RBin1
      INTEGER GenBin2
      EXTERNAL GenBin2
!
!     Common block for rounding
      COMMON / Bin1Rnd / BoolBin1,Round
      REAL*8 Round
      LOGICAL BoolBin1 
!
	
!     Quick check
      IF (Prob.LE.0.D0) THEN
        RBin1 = 0
      ELSE IF (Prob.GE.1.D0) THEN
        RBin1 = Mean
!
!     What to do depends on whether this a stochastic model
      ELSE IF (ISTOC.GE.1) THEN
!
        Randd = RAN1(ISEED)
        RBin1 = GenBin2(Randd,1.d0-Prob,Mean)
! 
      ELSE
!
!       Deterministic model
        RBin1 = Mean*(1.d0-Prob)
        IF (ABS(RBin1-INT(RBin1)-0.5).LT.0.0001) THEN
         IF (BoolBin1) THEN
           RBin1 = RBin1 + 0.5
         ELSE
           RBin1 = RBin1 - 0.5
         ENDIF
         BoolBin1 = .NOT.BoolBin1
        ENDIF     
!
      ENDIF
      IBin2 = NINT(RBin1)
	

      RETURN
      END




! *********************************************************************

      DOUBLE PRECISION FUNCTION Bin2(Mean,Prob,ISEED,IStoc)

!     This subroutine implements the binomial* (as BIN1 but from opposite end)

!     Global variables
      REAL*8 Mean,Prob,Randd,RNN
      INTEGER INN, GenBin
      EXTERNAL GenBin
!
!     Quick check
      IF (Prob.LE.0.D0) THEN
        Bin2 = 0.d0
      ELSE IF (Prob.GE.1.D0) THEN
        Bin2 = Mean
!
!     What to do depends on whether this a stochastic model
      ELSE IF (ISTOC.GE.1) THEN
!
        Ivar = Ivar + 1
        INN = INT(Mean)
        Randd = RAN1(ISEED)
        RNN = GenBin(Randd,1.d0-Prob,INN)
        Bin2 = INN - RNN + (Mean-INN)*Prob
!
      ELSE
!
!       Deterministic model 
        Bin2 = Mean*Prob 
!        
      ENDIF

      RETURN
      END

!--------------------------------------------------------------------------
!
      INTEGER FUNCTION GenBin2(Rand,Lamda,NN)
!
!     This subroutine generates from a Binomial distribution
!
!     Global variables
      DOUBLE PRECISION Rand,Lamda,Nall
      INTEGER NN
!      
      COMMON / Bintemp / BinCon,MaxN
      DOUBLE PRECISION BinCon(0:500,0:500)
      INTEGER MaxN
!
!     Local variables
      DOUBLE PRECISION P,Q,PR,Ompr,Bound,YY,Cump,Temp,L1,L2
      INTEGER Status,II
!
      IF (NN.EQ.0) THEN
       GenBin2 = 0
       RETURN
      ENDIF


!	MaxN=250

      IF (NN.LE.MaxN) THEN

!
!       Binomial generator is poor for low N so use array BINCON 
        CUMP = 0.d0
        L1 = log(Lamda)
        L2 = log(1.d0-Lamda)
        DO 2000 II = 0,NN
          YY = II
          Temp = BinCon(NN,II) + YY*L1 + (NN-II)*L2
		CUMP = CUMP + Exp(Temp)
          IF (Cump.GT.Rand) THEN
            GenBin2 = II
            Return
          ENDIF
2000    CONTINUE
!
      ELSE

        P = Rand
        Q = 1.d0 - P
        Pr = Lamda
        OMPR = 1.d0 - Lamda
        Nall = NN
        CALL CDFBIN(2,p,q,YY,Nall,Pr,Ompr,Status,Bound)
        IF (STatus.EQ.1) THEN
          CALL CDFBIN(2,p,q,YY,Nall,OmPr,pr,Status,Bound)
          YY = Nall - YY
        ENDIF  
        IF (STatus.NE.0) WRITE(*,*) YY,p,Pr,Nall,Status,Bound
      ENDIF  

      GenBin2 = YY


!
      RETURN
      END
!
!--------------------------------------------------------------------------
!
      INTEGER FUNCTION GenBin(Rand,Lamda,NN)
!
!     This subroutine generates from a Binomial distribution
!
!     Global variables
      DOUBLE PRECISION Rand,Lamda,Nall
      INTEGER NN
!      
      COMMON / Bintemp / BinCon,MaxN
      DOUBLE PRECISION BinCon(0:500,0:500)
      INTEGER MaxN
!
!     Local variables
      DOUBLE PRECISION P,Q,PR,Ompr,Bound,YY,Cump,Temp,L1,L2
      INTEGER Status,II
!
      IF (NN.EQ.0) THEN
       GenBin = 0
       RETURN
      ENDIF


      IF (NN.LE.MaxN) THEN

!
!       Binomial generator is poor for low N so use array BINCON 
        CUMP = 0.d0
        L1 = log(Lamda)
        L2 = log(1.d0-Lamda)
        DO 2000 II = 0,NN
          YY = II
          Temp = BinCon(NN,II) + YY*L1 + (NN-II)*L2
          CUMP = CUMP + Exp(Temp)
          IF (Cump.GT.Rand) THEN
            GenBin = II
            Return
          ENDIF
2000    CONTINUE
!
      ELSE

        P = Rand
        Q = 1.d0 - P
        Pr = Lamda
        OMPR = 1.d0 - Lamda
        Nall = NN
        CALL CDFBIN(2,p,q,YY,Nall,Pr,Ompr,Status,Bound)
        IF (STatus.EQ.1) THEN
          CALL CDFBIN(2,p,q,YY,Nall,OmPr,pr,Status,Bound)
          YY = Nall - YY
        ENDIF  
        IF (STatus.NE.0) WRITE(*,*) YY,p,Pr,Nall,Status,Bound
      ENDIF  

      GenBin = YY


!
      RETURN
      END
!
! =====================================================================
!
      SUBROUTINE BinSet()
!
!     BINSET sets log [N! / n! (N-n)! ] for use in generating binomials
!
      COMMON / Bintemp / BinCon,MaxN
      DOUBLE PRECISION BinCon(0:500,0:500)
      INTEGER MaxN
!
!     Local variables
      INTEGER N1,II
      DOUBLE PRECISION NF1,NF2,Nfact
!
      MaxN = 500
      DO 1000 N1 = 1,MaxN
        NFact = 0
        DO 1100 II = 1,N1
         Nfact = NFact + log(DBLE(II))
1100    CONTINUE
!       Nfact = log(N1!)
        NF2 = NFact
        NF1 = 0.D0
        DO 2000 II = 0,N1
          BinCon(N1,II) = Nfact - Nf1 - Nf2
          IF (N1-II-1.LE.0) THEN 
!           N-n =< 1. Gives BinCon=0
            NF2 = 0.d0
            NF1 = NFact
          ELSE
            NF2 = NF2 - log(N1-II+0.d0)
            NF1 = NF1 + log(II+1.d0)
          ENDIF
2000    CONTINUE
!
1000  CONTINUE
!
      RETURN
      END
      
! ---------------------------------------------------------------------------

      SUBROUTINE SORT(X,M)

!     USE A QUICK-SORT TO SORT ALL THE DATA

!     IMPLICIT NONE
      REAL*8 X(2000),ST1(2000),MID
      INTEGER*4 M,LEFT(2000),RIGHT(2000),STKLEN
      INTEGER*4 LEFTS,RIGHTS,LS,RS,IC

!     Check for Daft call
      IF (M.LT.2) RETURN

!     Set up initial conditions
      LEFT(1) = 1
      RIGHT(1) = M
      STKLEN = 1

99    IF (STKLEN.EQ.0) GOTO 100
                                        
!     Set up the Pointers for this run
      MID = x(LEFT(STKLEN))
      LEFTS = LEFT(STKLEN)
      RIGHTS = RIGHT(STKLEN)
      LS = LEFT(STKLEN)
      RS = RIGHT(STKLEN)

!     Do a one-level sort
      DO 10 IC = LEFT(STKLEN)+1,RIGHT(STKLEN)
                                                                  
!      Check whether the current is less than the middle
       IF (X(IC).GT.MID) THEN
         ST1(RIGHTS) = X(IC)
         RIGHTS = RIGHTS - 1
       ELSE
         ST1(LEFTS) = X(IC)
         LEFTS = LEFTS + 1
       ENDIF
10    CONTINUE
                    
!     Store the middle value
      ST1(LEFTS) = x(LEFT(STKLEN))
                         
!     Replace the data
      DO 11 IC = LEFT(STKLEN),RIGHT(STKLEN)
       x(IC) = ST1(IC)
11    CONTINUE
      STKLEN = STKLEN - 1
        
!     update right pointer
      IF ((LEFTS-LS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = LS
        RIGHT(STKLEN) = LEFTS - 1
      ENDIF
        
!     update left pointer
      IF ((RS-RIGHTS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = RIGHTS + 1
        RIGHT(STKLEN) = RS
      ENDIF

      GOTO 99
100   CONTINUE

      RETURN
      END
!      
!! -------------------------------------------------------------------------
!!
!      REAL*8 FUNCTION CUMNOM(X)
!
!!     Work out the cumulative normal distribution
!
!      REAL*8 Z,X,ERFCC
!
!      Z = X/SQRT(2.0)
!
!      IF (X.LT.0) THEN
!        CUMNOM = ERFCC(-z)/2.0
!      ELSE
!        CUMNOM = 1-ERFCC(Z)/2.0
!      ENDIF
!
!      RETURN
!      END
!!
!! -------------------------------------------------------------------------
!!
!      REAL*8 FUNCTION ERFCC(X)
!
!!     Returns the complementry error FUNCTION ERFC(X)
!
!      REAL*8 X,Z,T
!
!      Z = ABS(X)
!      T = 1.0/(1.0+0.5*Z)
!      ERFCC = T*EXP(-Z*Z-1.26551223+T*(1.00002368+T*(0.37409196+T*(0.09678418+T*(-0.18628806+T*(0.27886807+T*(-1.13520398+T*(1.48851587+T*(-0.82215223+T*0.17087277)))))))))
!      IF (X.LT.0) ERFCC = 2.0-ERFCC
!
!      RETURN
!      END

!**************************************************************************

      SUBROUTINE FIT(X,SS,NDIM,FUNK)

!     Set up the parameters for a fit

!     IMPLICIT NONE
      REAL*8 P(201,200),Y(201),X(200),SS,FUNK,TOL,GRD
      INTEGER*4 NDIM,I,J,ITER
      EXTERNAL FUNK

!     SET UP TOLERANCES AND GRIDDING
      TOL = 0.000002
      GRD = 1.2

      DO 1 I=1,NDIM+1
         DO 2 J=1,NDIM
            P(I,J)=X(J)
            IF((I-1).EQ.J) P(I,J) = GRD*P(I,J)
2        CONTINUE
1     CONTINUE

      DO 3 I=1,NDIM+1
         DO 4 J=1,NDIM
            X(J)=P(I,J)
4        CONTINUE
         Y(I)=FUNK(X)
3     CONTINUE

      CALL AMOEBA(P,Y,201,200,NDIM,TOL,ITER,FUNK)

      DO 5 J = 1,NDIM
         X(J) = P(1,J)
5     CONTINUE
      SS = Y(1)

      RETURN
      END

      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,ITER,FUNK)
!
!     MULTIDIMENSIONAL MINIMISATION OF THE FUNCTION FUNK(X) WHERE X IS
!     AN NDIM-DIMENSIONAL VECTOR, BY THE DOWNHILL SIMPLEX METHOD OF
!     NELDER AND MEAD. INPUT IS A MATRIX P WHOSE NDIM+1 ROWS ARE THE
!     NDIM-DIMENSIONAL VECTORS WHICH ARE THE VERTICES OF THE STARTING
!     SIMPLEX. [LOGICAL DIMENSIONS OF P ARE P(NDIM+1,NDIM); PHYSICAL
!     DIMENSIONS ARE INPUT AS P(MP,NP).] ALSO INPUT IS THE VECTOR Y
!     OF LENGTH NDIM+1, WHOSE COMPONENTS MUST BE PRE-INITIALISED TO
!     THE VALUES OF FUNK EVALUATED AT THE NDIM+1 VERTICES (ROWS) OF P;
!     AND FTOL IS THE FRACTIONAL CONVERGENCE TOLERANCE TO BE ACHIEVED
!     IN THE FUNCTION VALUE (N.B.!). ON OUTPUT, P AND Y WILL HAVE BEEN
!     RESET TO NDIM+1 NEW POINTS ALL WITHIN FTOL OF A MINIMUM FUNCTION
!     VALUE, AND ITER GIVES THE NUMBER OF ITERATIONS TAKEN.
!
!     FROM: NUMERICAL RECIPES - THE ART OF SCIENTIFIC COMPUTING
!           BY W. H. PRESS ET AL, CAMBRIDGE UNIVERSITY PRESS
!           ISBN 0-251-30811-9
!
!     ********************************************************************
!

!     SPECIFY THE MAXIMUM NUMBER OF DIMENSIONS, THREE PARAMETERS WHICH
!     DEFINE THE EXPANSIONS AND CONTRACTIONS, AND THE MAXIMUM NUMBER OF
!     ITERATIONS ALLOWED

!     IMPLICIT NONE
      REAL*8 ALPHA,BETA,GAMMA
      INTEGER*4 ITMAX
      PARAMETER (ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=500)

!     Global Data
      INTEGER*4 MP,NP,NDIM,ITER
      REAL*8 P(MP,NP),Y(MP),PR(200),PRR(200),PBAR(200),FTOL,FUNK
      EXTERNAL FUNK

!     Local Data
      REAL*8 RTOL,YPR,YPRR
      INTEGER*4 I,ILO,IHI,INHI,J,MPTS

!     NOTE THAT MP IS THE PHYSICAL DIMENSION CORRESPONDING TO THE LOGICAL
!     DIMENSION MPTS, NP TO NDIM.

      MPTS = NDIM+1
      ITER=0

!     FIRST WE MUST DETERMINE WHICH POINT IS THE HIGHEST (WORST), NEXT
!     HIGHEST, AND LOWEST (BEST).

1     ILO=1
      IF(Y(1).GT.Y(2)) THEN
         IHI=1
         INHI=2
      ELSE
         IHI=2
         INHI=1
      ENDIF
      DO 11 I=1,MPTS
         IF(Y(I).LT.Y(ILO)) ILO=I
         IF(Y(I).GT.Y(IHI)) THEN
            INHI=IHI
            IHI=I
         ELSE IF(Y(I).GT.Y(INHI)) THEN
            IF(I.NE.IHI) INHI=I
         ENDIF
11    CONTINUE

!     COMPUTE THE FRACTIONAL RANGE FROM THE HIGHEST TO THE LOWEST AND
!     RETURN IF SATISFACTORY

      RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
      IF(RTOL.LT.FTOL) RETURN
      IF(ITER.EQ.ITMAX) THEN
        OPEN(UNIT=51,FILE='ESTSUM.OUT',ACCESS='APPEND')
        WRITE(6,200)
        WRITE(51,*) RTOL,FTOL
        CLOSE(51)
200      FORMAT(1H ,'AMOEBA EXCEEDING MAXIMUM ITERATIONS')
         RETURN
      ENDIF
      ITER=ITER+1

      DO 12 J=1,NDIM
         PBAR(J)=0.
12    CONTINUE

!     BEGIN A NEW ITERATION. COMPUTE THE VECTOR AVERAGE OF ALL POINTS
!     EXCEPT THE HIGHEST, I.E. THE CENTRE OF THE "FACE" OF THE SIMPLEX
!     ACROSS FROM THE HIGH POINT. WE WILL SUBSEQUENTLY EXPLORE ALONG
!     THE RAY FROM THE HIGH POINT THROUGH THE CENTRE.

      DO 14 I=1,MPTS
         IF(I.NE.IHI) THEN
            DO 13 J=1,NDIM
               PBAR(J)=PBAR(J)+P(I,J)
13          CONTINUE
         ENDIF
14    CONTINUE

!     EXTRAPOLATE BY A FACTOR ALPHA THROUGH THE FACE, I.E. REFLECT THE
!     SIMPLEX FROM THE HIGH POINT

      DO 15 J=1,NDIM
         PBAR(J)=PBAR(J)/NDIM
         PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15    CONTINUE

!     EVALUATE THE FUNCTION AT THE REFLECTED POINT

      YPR=FUNK(PR)

!     GIVES A BETTER RESULT THAN THE BEST POINT, SO TRY AN ADDITIONAL
!     EXTRAPOLATION BY A FACTOR GAMMA

      IF(YPR.LE.Y(ILO)) THEN
         DO 16 J=1,NDIM
            PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16       CONTINUE

!        CHECK THE FUNCTION THERE

         YPRR=FUNK(PRR)

!        THE ADDITIONAL EXTRAPOLATION SUCCEEDED, AND REPLACES THE
!        HIGHEST POINT

         IF(YPRR.LT.Y(ILO)) THEN
            DO 17 J=1,NDIM
               P(IHI,J)=PRR(J)
17          CONTINUE
            Y(IHI)=YPRR
         ELSE

!        THE ADDITIONAL EXTRAPOLATION FAILED, BUT WE CAN STILL USE THE
!        REFLECTED POINT

         DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18       CONTINUE
         Y(IHI)=YPR
      ENDIF

!     THE REFLECTED POINT IS WORSE THAN THE SECOND HIGHEST

      ELSE IF(YPR.GE.Y(INHI)) THEN

!        IF IT'S BETTER THAN THE HIGHEST, THEN REPLACE THE HIGHEST

         IF(YPR.LT.Y(IHI)) THEN
            DO 19 J=1,NDIM
               P(IHI,J)=PR(J)
19          CONTINUE
            Y(IHI)=YPR
         ENDIF

!        BUT LOOK FOR AN INTERMEDIATE LOWER POINT; IN OTHER WORDS
!        PERFORM A CONTRACTION OF THE SIMPLEX ALONG ONE DIMENSION
!        AND THEN EVALUATE THE FUNCTION

         DO 21 J=1,NDIM
            PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21       CONTINUE
         YPRR=FUNK(PRR)

!        CONTRACTION GIVES AN IMPROVEMENT, SO ACCEPT IT

         IF(YPRR.LT.Y(IHI)) THEN
            DO 22 J=1,NDIM
               P(IHI,J)=PRR(J)
22          CONTINUE
            Y(IHI)=YPRR
         ELSE

!           CAN'T SEEM TO GET RID OF THAT HIGH POINT. BETTER CONTRACT
!           AROUND THE LOWEST (BEST) POINT

            DO 24 I=1,MPTS
               IF(I.NE.ILO) THEN
                  DO 23 J=1,NDIM
                     PR(J)=0.5*(P(I,J)+P(ILO,J))
                     P(I,J)=PR(J)
23                CONTINUE
                  Y(I)=FUNK(PR)
               ENDIF
24          CONTINUE
         ENDIF

      ELSE

!        WE ARRIVE HERE IF THE ORIGINAL REFLECTION GIVES A MIDDLING
!        POINT. REPLACE THE OLD HIGH POINT AND CONTINUE

         DO 25 J=1,NDIM
            P(IHI,J)=PR(J)
25       CONTINUE
         Y(IHI)=YPR

      ENDIF

      GO TO 1

      END
!

