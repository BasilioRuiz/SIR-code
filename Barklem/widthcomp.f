c***************************************************************
c widthcomp.f
c Paul Barklem Jan 1999, UPPSALA
c
c  This program is designed to process an input file of atomic 
c   transition data and output a similar file of cross sections
c   and linewidths per perturber
c
c  some updates March 2009, including one bug fix
c***************************************************************

	PROGRAM WIDTHCOMP
c***************************************************************
c The main program
c***************************************************************
c
	IMPLICIT NONE
	INTEGER NUMIN,I,J,NUMLINES,SECONDS,TIME,AN,IFAIL,IFAIL_INT
	INTEGER LLOW, LUPP, Z_N, COUNT, IGOOD
	REAL*8 LINE(50000,10)
	REAL*8 ELOW,EUPP,ELIMIT_LOW,ELIMIT_UPP,WL
	REAL*4 CROSS, ALPHA, HALFWIDTH, TEMP, FULLWIDTH
	REAL*4 LGFW, LGC6,LGFW_U, LGC6_U, Rupp, Rlow, A
	REAL*4 NSTARUPP, NSTARLOW
	REAL*4 EVLOW,EVUPP,JLOW,JUPP
	CHARACTER*30 INPUTFILE, VERSION,
     ;               OUTPUTFILE, OUTPUTFILE_SHORT
	CHARACTER*100 CARD(50000)
	CHARACTER*3 HEAD, SPEC(50000)
	CHARACTER*2 ELEMENT
	CHARACTER*1 TYPE(50000)
	INTEGER ION(50000)
	CHARACTER*24 TODAY,CTIME
c
	PARAMETER (NUMIN=7) !number of input data per line
	PARAMETER (VERSION='VERSION 2.0')
c
	PRINT*,'Enter the Input File name --->'
	READ(*,'(A30)') INPUTFILE
c
c  We will have two output files, one a nice long one for humans
c   and the second an abbreviated one for computer reading
c
	PRINT *,'Enter a name for the LONG Output File --->'
	READ(*,'(A30)') OUTPUTFILE
c
	PRINT *,'Enter a name for the SHORT Output File --->'
	READ(*,'(A30)') OUTPUTFILE_SHORT
c
c	Open the input file
c
	OPEN(17,file=INPUTFILE,form='formatted',status='old')
	OPEN(14,file=OUTPUTFILE,form='formatted',status='unknown')
	OPEN(18,file=OUTPUTFILE_SHORT,form='formatted',status='unknown')
c
	IFAIL=0
	I=0
	J=0
	COUNT=0
	IGOOD=0
c
c       read lines of file
c
99	CONTINUE
c	
	READ(17,'(A3)') HEAD
	IF ((HEAD.EQ.'end').OR.(HEAD.EQ.'END')) GO TO 500
	IF ((HEAD.NE.'###').AND.(HEAD.NE.'   ')
     ;                     .AND.(HEAD.NE.'$$$')) THEN
	  I=I+1
	  COUNT=COUNT+1
c
c   assume a cross section input first
c
	  TYPE(I) = 'C'
	  BACKSPACE(17)
	  READ(17,*) SPEC(I), ION(I), (LINE(I,J),J=1,8)
c	  print *,SPEC(I), ION(I), (LINE(I,J),J=1,8)
c
	  IF (LINE(I,2).ge.0) THEN 
c
c       we have line data type input 
c
	     TYPE(I)='L'
	     BACKSPACE(17)
	     READ(17,*) SPEC(I), ION(I), (LINE(I,J),J=1,9)
c	     print *,SPEC(I), ION(I), (LINE(I,J),J=1,9)
	   END IF
	END IF
	IF (HEAD.eq.'$$$') then
c
c   a comment line to be printed to output
c
	  I=I+1
	  BACKSPACE(17)
 42	  FORMAT(A3,A100)
	  READ(17,42) SPEC(I),CARD(I)
	END IF
C	  
	GOTO 99
c
c	end of file found
c
500	CONTINUE
c
c	store number of lines read
c
	NUMLINES=I
	I=0
c
c       Setup Output file header
c
	SECONDS = TIME()
	TODAY = CTIME(SECONDS)
71	FORMAT(A16,1X,A30)
72	FORMAT(A5,1X,A24)
73	FORMAT(A9,1X,I6,1X,A6)
74 	FORMAT(A30,A50)
75	FORMAT(A4,5X,A5,6X,A4,5X,A6,7X,A4,5X,A6,3X,A1,6X,A1,5X,A5,
     ;   1X,A5,2X,A5,
     ;   4X,A5,1X,4A12)  
	WRITE(14,'(150A)') ('*',J=1,150)
	WRITE(14,74) VERSION, 'By Paul Barklem, Paul.Barklem@fysast.uu.se'
	WRITE (14,'(A)') ' '
	WRITE (14,71)'Input Filename:',INPUTFILE
	WRITE (14,71)'Output Filename: ',OUTPUTFILE
	WRITE (14,72)'Time:',TODAY
	WRITE (14,'(A)') ' '
	WRITE (14,73)'Processed',COUNT,'Lines.'
	WRITE(14,'(150A)') ('*',J=1,150)
        WRITE(14,75)'Spec','Wavel','Elow','Elimit','Eupp',
     ;     'Elimit','L','J','N*low',
     ;     'N*upp','Sigma','Alpha','Log(FWHM/N)',
c     ;     ' ', ' ', ' '
     ;      'Log(C6:U)', 
     ;      'Log(FWHM/N)', 'Log(C6:U)'
       WRITE(14,'(127X,A21)') '----Unsold values----'
c
c  	now compute each line and output to the file
c
	TEMP = 10000.
	do I=1,NUMLINES
	  IFAIL = 0
	  IFAIL_INT = 0
	  CROSS = 0
	  ALPHA = 0
	  ELOW = -1 ! for these a negative will indicate not available for now 
	  EUPP = -1 
	  ELIMIT_LOW = -1 
	  ELIMIT_UPP = -1 
	  if (SPEC(I).eq.'$$$') then
	    WRITE(14,*) ' '
	    WRITE(14,'(A100)') CARD(I)
	  else
	         ELEMENT = SPEC(I)(1:2)
	         Z_N = ION(I)
		 WL = LINE(I,1)
		 Llow = INT(LINE(I,5))
		 Lupp = INT(LINE(I,6))
	         Jlow = LINE(I,7)
	         Jupp = LINE(I,8)
	     CALL UPPER(ELEMENT)
		 CALL WEIGHT(ELEMENT,A,AN)
	        if (A.eq.0) then 
81 	           FORMAT(A8,A2,A11)
	           WRITE (*,81)'Element ',ELEMENT,' not found!'
	           IFAIL = 1
	        end if
	     if (TYPE(I).eq.'L') then
	         Elow = LINE(I,2)
	         Eupp = LINE(I,4)
	         Elimit_low = LINE(I,3)
	         Elimit_upp = LINE(I,5)
	         Llow = INT(LINE(I,6))
	         Lupp = INT(LINE(I,7))
	         Jlow = LINE(I,8)
	         Jupp = LINE(I,9)
c
	         if (Z_N.ne.1) then  
82 	           FORMAT(A30,I1)
	           WRITE (*,82)'Invalid Ionisation Stage ',Z_N
	           IFAIL = 1
	         end if
c
	         NSTARupp = Z_N/SQRT((Elimit_upp-Eupp)/109678.758)
	         NSTARlow = Z_N/SQRT((Elimit_low-Elow)/109678.758)
c
	         if (IFAIL.eq.0) then 
	     call RETCROSS(NSTARlow,NSTARupp,Llow,Lupp,CROSS,ALPHA,
     ;                  IFAIL_INT)
	         end if
	         if (IFAIL_INT.ne.0) then
51	           FORMAT(A35,F9.3)
52		   FORMAT( 'n* are ',f6.2,' -> ',f6.2) 
	           WRITE(6,51) 'Unable to get xsection for line at ',WL
	           WRITE(6,'(A30)') 'for above reason.'
                   WRITE(6,52) NSTARlow,NSTARupp
		   GOTO 555
	         end if
	     else
	       CROSS = LINE(I,3)
	       ALPHA = LINE(I,4)
	     end if
c
c      now compute line widths for all
c
	    call WIDTH(CROSS,ALPHA,TEMP,A,HALFWIDTH)
	    FULLWIDTH=2.*HALFWIDTH
	    LGFW = 0.
	  if (IFAIL_INT.eq.0) then 
	    LGFW = LOG10(FULLWIDTH)
        LGC6 = 2.5 * LGFW - 12.32
c  below from Aller        
        Rupp = NSTARupp*NSTARupp/2.*(5.*NSTARupp*NSTARupp+1.
     ;         -3.*Lupp*(Lupp+1))
        Rlow = NSTARlow*NSTARlow/2.*(5.*NSTARlow*NSTARlow+1.
     ;         -3.*Llow*(Llow+1))
        LGC6_U = LOG10(6.46E-34*(Rupp-Rlow))
c   below at 10000 K 
        LGFW_U = (LGC6_U +12.32)/2.5  
      endif
	    IGOOD=IGOOD+1
c
c 	WRITE computed data to file
c
c 91	FORMAT(A2,1X,I1,1X,F9.3,1X,F10.3,1X,F10.3,1X,F10.3,1X,F10.3,
c     ;             1X,I1,A2,I1,1X,F3.1,A2,F3.1,1X,F5.3,1X,F5.3,1X,F7.2,
c     ;             1X,F7.3,1X,F11.3)
 91	FORMAT(A2,1X,I1,1X,F9.3,1X,F10.3,1X,F10.3,1X,F10.3,1X,F10.3,
     ;             1X,I1,A2,I1,1X,F3.1,A2,F3.1,1X,F5.3,1X,F5.3,1X,F7.2,
     ;             1X,F7.3,1X,F11.3,1X,F11.3,1X,F11.3,1X,F11.3)
 92	FORMAT(A2,1X,I1,1X,F9.3,5X,A20,20X,I1,A2,I1,1X,F3.1,A2,F3.1,
     ;             13X,F7.2,1X,F7.3,1X,F7.3)
 93	FORMAT(I2,A,2I1,1X,F10.3,2(1X,F4.1),1X,F7.3,1X,F7.3,1X,F8.2,1X,
     ;         F6.3,4(1X,F10.3),1X,I1,1X,I1,1X,F7.3)
	if (TYPE(I).eq.'L') then
	  WRITE(14,91)ELEMENT, Z_N, WL,Elow, Elimit_low, Eupp, Elimit_upp, 
     ; Llow,'->',Lupp,Jlow,'->',Jupp,NSTARlow, NSTARupp, CROSS,
     ; ALPHA, LGFW
     ; ,LGC6, LGFW_U, LGC6_U 
	else
c
c      now compute line widths for all
c
	    call WIDTH(CROSS,ALPHA,TEMP,A,HALFWIDTH)
	    FULLWIDTH=2.*HALFWIDTH
	    LGFW = 0.
	  if (IFAIL_INT.eq.0) LGFW = LOG10(FULLWIDTH)
	    IGOOD=IGOOD+1
	  WRITE(14,92) ELEMENT, Z_N, WL,'cross section input',
     ;                  Llow,'->',Lupp,Jlow,'->',Jupp, CROSS, ALPHA,LGFW 
	end if
c
c  if available compute the levels in eV for short output
c
	  IF (ELOW.GE.0.0) THEN
	    EVLOW = ELOW/109678.758*13.59
	    EVUPP = EUPP/109678.758*13.59
 	  ELSE
	    EVLOW = -1
	    EVUPP = -1
	  END IF
	  WRITE(18,93) AN,'.',0,Z_N-1,WL,JLOW,JUPP,EVLOW,EVUPP,CROSS,
     ;          ALPHA, ELOW,ELIMIT_LOW,EUPP,ELIMIT_UPP,LLOW,LUPP,LGFW 
	END IF
C
C  BAIL OUT POINT FOR BAD LINES
C
 555	CONTINUE
C
C  END MAIN LOOP
C
	END DO
c
	WRITE(14,'(150A)') ('*',J=1,150)
	WRITE(14,'(I6,1X,20A)') IGOOD,'LINES DONE' 
c	WRITE(18,'(A3)') 'END'
c
c	close all the files
c	
	close(14)
	close(17)
	close(18)
c
	print *,' '
	print *,'Output written to file!'
	end
	

	subroutine UPPER(STRING)
c***************************************************************
c  routine converts an element to first upper second lower eg Fe
c***************************************************************
	implicit none
	character STRING*2, FIRST*1, SECOND*1
c
	FIRST = STRING(1:1)
	SECOND = STRING(2:2)
c
	if (ICHAR(FIRST).gt.97) then
	  FIRST = CHAR(ICHAR(FIRST)-32)
	end if
	if ((ICHAR(SECOND).lt.97).and.(ICHAR(SECOND).gt.64)) then
	  SECOND = CHAR(ICHAR(SECOND)+32)
	end if
	STRING = FIRST//SECOND
c
	end

	subroutine WEIGHT(ELEMENT,A, AN)
c***************************************************************
c  gets the atomic weight
c***************************************************************
c
	implicit none
	integer AN, I
	character*2 ELEMENT, ELEMEN(99)
	real*4 A,AMASS(99)
c
	common /MASSES/AMASS,ELEMEN
c
	do I = 1,99
	  if (ELEMENT.eq.ELEMEN(I)) then
	   AN = I
	   A = AMASS(I)
	   return
	  end if
	end do
	AN = 0
	A = 0
c
	end
	

	subroutine NUMCHAR(C, I)
c***************************************************************
c  changes a number in character form to the integer corresponding
c***************************************************************
c
	implicit none
	integer I, J
	character C
c
	J = ICHAR(C)
	I = J - 48	
c
	if ((I.lt.1).or.(I.gt.9)) I=0
c
	end


c
      BLOCK DATA
C
C  STORE CHEMICAL ELEMENTS NAMES AND MASSES
C
	CHARACTER*2 ELEMEN(99)
	REAL*4 AMASS(99)
	COMMON /MASSES/AMASS,ELEMEN
C
      DATA ELEMEN/
C       1    2    3    4    5    6    7    8    9    10   11   12
     1 'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne','Na','Mg',
     2 'Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti','V ','Cr',
     3 'Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr',
     4 'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd',
     5 'In','Sn','Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd',
     6 'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf',
     7 'Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po',
     8 'At','Rn','Fr','Ra','Ac','Th','Pa','U ','Np','Pu','Am','Cm',
     9 'Bk','Cf','Es'/
      DATA AMASS/
     1   1.008,  4.003,  6.941,  9.012, 10.811, 12.011, 14.007, 15.999,
     2  18.998, 20.179, 22.990, 24.305, 26.982, 28.086, 30.974, 32.060,
     3  35.453, 39.948, 39.102, 40.080, 44.956, 47.900, 50.941, 51.996,
     4  54.938, 55.847, 58.933, 58.710, 63.546, 65.370, 69.720, 72.590,
     5  74.922, 78.960, 79.904, 83.800, 85.468, 87.620, 88.906, 91.220,
     6  92.906, 95.940, 98.906,101.070,102.905,106.400,107.868,112.400,
     7 114.820,118.690,121.750,127.600,126.905,131.300,132.905,137.340,
     8 138.906,140.120,140.908,144.240,146.000,150.400,151.960,157.250,
     9 158.925,162.500,164.930,167.260,168.934,170.040,174.970,178.490,
     A 180.948,183.850,186.200,190.200,192.200,195.090,196.967,200.590,
     B 204.370,207.190,208.981,210.000,210.000,222.000,223.000,226.025,
     C 227.000,232.038,230.040,238.029,237.048,242.000,242.000,245.000,
     D 248.000,252.000,253.000/
C
      END
