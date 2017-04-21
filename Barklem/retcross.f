	subroutine RETCROSS(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
c**********************************************************************
c  Returns the cross section at 10000 m/s from tables for neutrals etc
c   returns IFAIL=1 if not on the tables or some other error, else 0
c  Paul Barklem Dec 1997
c**********************************************************************
c
c NSupp,NSlow = upper and lower state effective principal quantum numbers
c Lupp,Llow = angular momentum quantum numbers
c CROSS = hydrogen broadening cross-section for v=10000m/s
c ALPHA = velocity parameter
c
	implicit none
	real*4 NSupp,NSlow,CROSS,ALPHA
	integer Lupp,Llow,IFAIL,CHECK,TABLE
c
  	CHECK=ABS(Lupp-Llow)
	if (CHECK.ne.1) then
	 IFAIL=1
	 print *,'#######Forbidden transition!'
	 return
	end if	
c
	TABLE=Llow+Lupp
	if (TABLE.eq.1) 
     ;       call SP(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
	if (TABLE.eq.3) 
     ;       call PD(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
	if (TABLE.eq.5) 
     ;       call DF(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
	if ((TABLE.ne.1).and.(TABLE.ne.3).and.(TABLE.ne.5))
     ;   then
	 IFAIL=1
	 print *,'#######Transition not covered by current data'
	 return
        end if
c
	return
	end

	subroutine SP(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
c**********************************************************************
c  Returns the cross section at 10000 m/s from s-p table 
c   returns IFAIL=1 if not on the tables, else 0
c  Paul Barklem Dec 1997
c**********************************************************************
c
	implicit none
	real*4 NSupp,NSlow,CROSS,ALPHA,NSS,NSP,
     ;         CS(21,18),AL(21,18),CS2(21,18),AL2(21,18),
     ;         NSSG(21),NSPG(18)
	integer Lupp,Llow,IFAIL,I,J
	character*70 COMMENTS
c
	if (Lupp.eq.1) then
	 NSP=NSupp
	 NSS=NSlow
	else
	 NSP=NSlow
	 NSS=NSupp
	end if
c
	if ((NSS.gt.3.).or.(NSS.lt.1.).or.(NSP.gt.3.).or.(NSP.lt.1.3))
     ;   then
	 IFAIL=1
	 print *,'#######Outside range of tables! (s-p)'
         return 
	end if
c
c  read in the table data
c   3 lines of comments preceed
c
	open(15,file='spdata.dat',form='formatted',status='old')
	do I=1,3
	read(15,'(A70)') COMMENTS
c	print *,COMMENTS
	end do	 
c
          do I=1,21
            read(15,*) (CS(I,J),J=1,18)
          end do
          read(15,'(A70)') COMMENTS  ! a single comment line between data
	  do I=1,21
            read(15,*) (AL(I,J),J=1,18)
          end do
c
	do I=1,21
	 NSSG(I)=1.0+(I-1)*0.1
	end do
	do I=1,18
	 NSPG(I)=1.3+(I-1)*0.1
	end do
c
c   setup second derivative table for spline
c
	call SPLIE2(NSSG,NSPG,CS,21,18,CS2)
	call SPLIE2(NSSG,NSPG,AL,21,18,AL2)	
c
c   run bicubic spline interpolation
c
	call SPLIN2(NSSG,NSPG,CS,CS2,21,18,NSS,NSP,CROSS)
	call SPLIN2(NSSG,NSPG,AL,AL2,21,18,NSS,NSP,ALPHA)
c
	close(15)
	return
	end		

	subroutine PD(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
c**********************************************************************
c  Returns the cross section at 10000 m/s from p-d table 
c   returns IFAIL=1 if not on the tables, else 0
c  Paul Barklem Dec 1997
c**********************************************************************
c
	implicit none
	real*4 NSupp,NSlow,CROSS,ALPHA,NSD,NSP,
     ;         CS(18,18),AL(18,18),CS2(18,18),AL2(18,18),
     ;         NSPG(18),NSDG(18)
	integer Lupp,Llow,IFAIL,I,J
	character*70 COMMENTS
c
	if (Lupp.eq.2) then
	 NSD=NSupp
	 NSP=NSlow
	else
	 NSD=NSlow
	 NSP=NSupp
	end if
c
	if ((NSP.gt.3.).or.(NSP.lt.1.3).or.(NSD.gt.4.).or.(NSD.lt.2.3))
     ;   then
	 IFAIL=1
	 print *,'#######Outside range of tables! (p-d)'
         return 
	end if
c
c  read in the table data
c   3 lines of comments preceed
c
	open(15,file='pddata.dat',form='formatted',status='old')
	do I=1,3
	read(15,'(A70)') COMMENTS
c	print *,COMMENTS
	end do	 	 
c
          do I=1,18
            read(15,*) (CS(I,J),J=1,18)
          end do
          read(15,'(A70)')COMMENTS  ! a single comment line between data
	  do I=1,18
            read(15,*) (AL(I,J),J=1,18)
          end do
c
c  make arrays of the nstar values
c
	do I=1,18
	 NSPG(I)=1.3+(I-1)*0.1
	 NSDG(I)=2.3+(I-1)*0.1
	end do
c
c   setup second derivative table for spline
c
	call SPLIE2(NSPG,NSDG,CS,18,18,CS2)
	call SPLIE2(NSPG,NSDG,AL,18,18,AL2)	
c
c   run bicubic spline interpolation
c
	call SPLIN2(NSPG,NSDG,CS,CS2,18,18,NSP,NSD,CROSS)
	call SPLIN2(NSPG,NSDG,AL,AL2,18,18,NSP,NSD,ALPHA)
c
c
	close(15)
	return
	end		
	
	subroutine DF(NSlow,NSupp,Llow,Lupp,CROSS,ALPHA,IFAIL)
c**********************************************************************
c  Returns the cross section at 10000 m/s from d-f table 
c   returns IFAIL=1 if not on the tables, else 0
c  Paul Barklem Dec 1997
c**********************************************************************
c
	implicit none
	real*4 NSupp,NSlow,CROSS,ALPHA,NSD,NSF,
     ;         CS(18,18),AL(18,18),CS2(18,18),AL2(18,18),
     ;         NSDG(18),NSFG(18)
	integer Lupp,Llow,IFAIL,I,J
	character*70 COMMENTS
c
	if (Lupp.eq.3) then
	 NSF=NSupp
	 NSD=NSlow
	else
	 NSF=NSlow
	 NSD=NSupp
	end if
c
	if ((NSD.gt.4.).or.(NSD.lt.2.3).or.(NSF.gt.5.).or.(NSF.lt.3.3))
     ;   then
	 IFAIL=1
	 print *,'#######Outside range of tables! (d-f)'
         return 
	end if
c
c  read in the table data
c   3 lines of comments preceed
c
	open(15,file='dfdata.dat',form='formatted',status='old')
	do I=1,3
	read(15,'(A70)') COMMENTS
c	print *,COMMENTS
	end do	 	 
c
          do I=1,18
            read(15,*) (CS(I,J),J=1,18)
c	    print *,(CS(I,J),J=1,18)
          end do
          read(15,'(A70)')COMMENTS  ! a single comment line between data
	  do I=1,18
            read(15,*) (AL(I,J),J=1,18)
c	    print *,(AL(I,J),J=1,18)
          end do
c
c  make arrays of the nstar values
c
	do I=1,18
	 NSDG(I)=2.3+(I-1)*0.1
	 NSFG(I)=3.3+(I-1)*0.1
	end do
c
c   setup second derivative table for spline
c
	call SPLIE2(NSDG,NSFG,CS,18,18,CS2)
	call SPLIE2(NSDG,NSFG,AL,18,18,AL2)	
c
c   run bicubic spline interpolation
c
	call SPLIN2(NSDG,NSFG,CS,CS2,18,18,NSD,NSF,CROSS)
	call SPLIN2(NSDG,NSFG,AL,AL2,18,18,NSD,NSF,ALPHA)
c
c
	close(15)
	return
	end

	subroutine WIDTH(CROSS,ALPHA,TEMP,A,HALFWIDTH)
c**********************************************************************
c  Computes the linewidth (half width at half maxima) given
c   the cross-section and velocity parameter and tempertature
c  Paul Barklem Dec 1997
c**********************************************************************
c
c CROSS = cross section in atomic units at 10000m/s
c ALPHA = velocity parameter
c TEMP = temperature in Kelvin
c HALFWIDTH = half width per unit hydrogen atom density (rad /s cm^-3)
c A = Atomic Mass of the absorbing atom eg. A(Mg)=24.32
c RMASS = reduced mass
c MEANVEL = mean kinetic particle velocity in m/s
c
	implicit none
	real*4 CROSS,ALPHA,TEMP,HALFWIDTH,MEANVEL,RMASS,CROSSMEAN,
     ;       CROSSM,A,PI,K,M0,A0
	real*8 DGAMMA
c
	parameter (PI=3.141592653)
	parameter (K=1.380658E-23)      !boltzmanns constant J/K
	parameter (M0=1.660540E-27)     !unit atomic mass kg (Carbon 12)
	parameter (A0=5.29177249E-11)   !bohr radius m
c 
	CROSSM=CROSS*A0*A0 !cross section to m^2
	RMASS=M0/(1./1.008+1./A) !calculate reduced mass
        MEANVEL=SQRT(8.0*K*TEMP/PI/RMASS) !array of mean velocities
c
c  work out cross section at mean perturber velocity for this Temp
c
        CROSSMEAN= CROSSM*((MEANVEL/10000.0)**(-ALPHA)) 
c
c the half-half width per unit perturber density m^3 rad/s
c
	HALFWIDTH=(4.0/PI)**(ALPHA/2.0) *
     ;	              SNGL(DGAMMA((4.0-ALPHA)/2.d0) )*
     ;                 MEANVEL*CROSSMEAN
c
	HALFWIDTH=HALFWIDTH * 1.e6 ! to rad/s cm^3
c
	return
	end


c**********************************************************************
c  The next routine is from the NETLIB archive
c   Computes the Gamma function
c**********************************************************************

      DOUBLE PRECISION FUNCTION DGAMMA(X)
C----------------------------------------------------------------------
C    From NETLIB, group SPECFUN
C     'AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL FUNCTIONS',
C     LECTURE NOTES IN MATHEMATICS, 506, NUMERICAL ANALYSIS DUNDEE,
C     1975, G. A. WATSON (ED.), SPRINGER VERLAG, BERLIN, 1976.
C*******************************************************************
C
C EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
C
C EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
C          1.0 + EPS .GT. 1.0
C XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE
C          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION
C                  GAMMA(XBIG) = XINF.
C XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
C          1/XMININ IS MACHINE REPRESENTABLE.
C XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER.
C
C
C ERROR RETURNS
C
C  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
C     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
C     TO BE FREE OF UNDERFLOW AND OVERFLOW.
C
C OTHER SUBPROGRAMS REQUIRED (DOUBLE PRECISION VERSION)
C
C     DBLE,DEXP,DLOG,DSIN,FLOAT,IFIX,SNGL
C
      double precision C,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,
     1     SUM,TWELVE,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      integer I,J,N
      logical PARITY
      dimension C(7),P(8),Q(8)
C----------------------------------------------------------------------
C  MATHEMATICAL CONSTANTS
C----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,ZERO/1.0D0,0.5D0,12.0D0,0.0D0/
      DATA SQRTPI/0.9189385332046727417803297D0/
      DATA PI/3.1415926535897932384626434D0/
C----------------------------------------------------------------------
C  MACHINE DEPENDENT PARAMETERS
C----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/34.844D0,5.883D-39,2.776D-17/
      DATA XINF/1.7014D38/
C----------------------------------------------------------------------
C  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
C     APPROXIMATION OVER (1,2).
C----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
     1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
     2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
     3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
      DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
     1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
     2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
     3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
C----------------------------------------------------------------------
C  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
C----------------------------------------------------------------------
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     1     -5.952379913043012D-04,7.93650793500350248D-04,
     2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     3      5.7083835261D-03/
C----------------------------------------------------------------------
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y .GT. ZERO) GO TO 200
C----------------------------------------------------------------------
C  ARGUMENT IS NEGATIVE
C----------------------------------------------------------------------
      Y = -X
      J = IFIX(SNGL(Y))
      RES = Y - DBLE(FLOAT(J))
      IF (RES .EQ. ZERO) GO TO 700
      IF (J .NE. (J/2)*2) PARITY = .TRUE.
      FACT = -PI / DSIN(PI*RES)
      Y = Y + ONE
C----------------------------------------------------------------------
C  ARGUMENT IS POSITIVE
C----------------------------------------------------------------------
  200 IF (Y .LT. EPS) GO TO 650
      IF (Y .GE. TWELVE) GO TO 300
      Y1 = Y
      IF (Y .GE. ONE) GO TO 210
C----------------------------------------------------------------------
C  0.0 .LT. ARGUMENT .LT. 1.0
C----------------------------------------------------------------------
      Z = Y
      Y = Y + ONE
      GO TO 250
C----------------------------------------------------------------------
C  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
C----------------------------------------------------------------------
  210 N = IFIX(SNGL(Y)) - 1
      Y = Y - DBLE(FLOAT(N))
      Z = Y - ONE
C----------------------------------------------------------------------
C  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
C----------------------------------------------------------------------
  250 XNUM = ZERO
      XDEN = ONE
      DO 260 I = 1, 8
         XNUM = (XNUM + P(I)) * Z
         XDEN = XDEN * Z + Q(I)
  260 CONTINUE
      RES = XNUM / XDEN + ONE
      IF (Y .EQ. Y1) GO TO 900
      IF (Y1 .GT. Y) GO TO 280
C----------------------------------------------------------------------
C  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
C----------------------------------------------------------------------
      RES = RES / Y1
      GO TO 900
C----------------------------------------------------------------------
C  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
C----------------------------------------------------------------------
  280 DO 290 I = 1, N
         RES = RES * Y
         Y = Y + ONE
  290 CONTINUE
      GO TO 900
C----------------------------------------------------------------------
C  EVALUATE FOR ARGUMENT .GE. 12.0,
C----------------------------------------------------------------------
  300 IF (Y .GT. XBIG) GO TO 700
      YSQ = Y * Y
      SUM = C(7)
      DO 350 I = 1, 6
         SUM = SUM / YSQ + C(I)
  350 CONTINUE
      SUM = SUM/Y - Y + SQRTPI
      SUM = SUM + (Y-HALF)*DLOG(Y)
      RES = DEXP(SUM)
      GO TO 900
C----------------------------------------------------------------------
C  ARGUMENT .LT. EPS
C----------------------------------------------------------------------
  650 IF (Y .LT. XMININ) GO TO 700
      RES = ONE / Y
      GO TO 900
C----------------------------------------------------------------------
C  RETURN FOR SINGULARITIES, EXTREME ARGUMENTS, ETC.
C----------------------------------------------------------------------
  700 DGAMMA = XINF
      print *,'function DGAMMA has extreme argument :'
      print *,'argument is : ',X
      print *,'function returns machine upper bound '
      GO TO 950
C----------------------------------------------------------------------
C  FINAL ADJUSTMENTS AND RETURN
C----------------------------------------------------------------------
  900 IF (PARITY) RES = -RES
      IF (FACT .NE. ONE) RES = FACT / RES
      DGAMMA = RES
  950 RETURN
C ---------- LAST CARD OF GAMMA ----------
      END


		

c**********************************************************************
c  The following are Numerical Recipes routines
c   W. Press, S. Teukolsky, W. Vetterling and B. Flannery
c  " Numerical Recipes in Fortran: The Art of Scientific 
c     Computing"   Second Edition, Cambridge Univ. Press
c**********************************************************************


      SUBROUTINE SPLIE2(X1A,X2A,YA,M,N,Y2A)
      PARAMETER (NN=100)
      DIMENSION X1A(M),X2A(N),YA(M,N),Y2A(M,N),YTMP(NN),Y2TMP(NN)
      DO 13 J=1,M
        DO 11 K=1,N
          YTMP(K)=YA(J,K)
11      CONTINUE
        CALL SPLINE(X2A,YTMP,N,1.E30,1.E30,Y2TMP)
        DO 12 K=1,N
          Y2A(J,K)=Y2TMP(K)
12      CONTINUE
13    CONTINUE
      RETURN
      END

      SUBROUTINE SPLIN2(X1A,X2A,YA,Y2A,M,N,X1,X2,Y)
      PARAMETER (NN=100)
      DIMENSION X1A(M),X2A(N),YA(M,N),Y2A(M,N),YTMP(NN),Y2TMP(NN),YYTMP(
     *NN)
      DO 12 J=1,M
        DO 11 K=1,N
          YTMP(K)=YA(J,K)
          Y2TMP(K)=Y2A(J,K)
11      CONTINUE
        CALL SPLINT(X2A,YTMP,Y2TMP,N,X2,YYTMP(J))
12    CONTINUE
      CALL SPLINE(X1A,YYTMP,M,1.E30,1.E30,Y2TMP)
      CALL SPLINT(X1A,YYTMP,Y2TMP,M,X1,Y)
      RETURN
      END

      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) THEN
        print*,'Bad XA input.'
        stop
      ENDIF
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END
