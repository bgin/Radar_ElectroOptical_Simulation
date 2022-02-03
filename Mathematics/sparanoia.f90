










!-------------------------------------------------------------------------------
subroutine sparanoia()
implicit none
character(len=*),parameter :: ident="@(#)sparanoia(3f): test real value operations in programming environment"
!!!!!   COMMON /STDIO/ IN, OUT
        integer in, out
!!!!!   COMMON /PGNUM/PGNUMB
        integer       pgnumb
!!!!!   COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        real r1, r2, r3, r4, sticky
!
!!!!!   COMMON /I3TYPE/ IEEE
        integer         ieee
!        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
!
!!!!!   COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
!!!!!~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        integer         fails, sdefct, defect, flaws
        real                              radix, ulppls, ulpmin, precis, w, mulgrd, divgrd, subgrd, a1, onemin
        logical flagf
!
!        FAILS  ... NUMBER OF FAILURES
!        SDEFCT ... NUMBER OF SERIOUS DEFECTS
!        DEFECT ... NUMBER OF DEFECTS
!        FLAWS  ... NUMBER OF FLAWS
!
!        RADIX  ... COMPUTED RADIX OF THE MACHINE
!        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
!        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
!        PRECIS ... COMPUTED PRECISION OF THE MACHINE
!        W      ... RADIX ** PRECIS
!        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
!        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
!        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
!        A1     ...
!        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
!
!!!!!   COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
!!!!!~                   HALF, MINUS1
        real fp0, fp1, fp2, fp3, fp4, fp8, fp9, fp27, fp32, half, minus1
!
!!!!!   COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        real c1, h1, minpos, nulps, uflthr, phony0
!        C1     ... 1/C ~= RADIX^LARGE_INTEGER
!        H1     ... MAX (2,RADIX)
!        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
!        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
!        UFLTHR ... THE UNDERFLOW THRESHOLD U0
!        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
!#######################################################################
      call runtests()
      close(3,status='delete')
      close(4,status='delete')
      return
      contains
!#######################################################################
      subroutine runtests()
      implicit none
!
        integer start
!        ... FLAG TO TELL WHETHER WE ARE RESTARTING OR
!        ... STARTING FROM SCRATCH
        integer temp
        integer miles, numtry, from
!        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
!        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
!        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
!
!
!
! DISABLE INTERRUPTS FOR LAHEY F77L.
!       LOGICAL FLAG
!       CALL INVALOP(FLAG)
!       CALL OVEFL(FLAG)
!       CALL UNDFL(FLAG)
!       CALL DVCHK(FLAG)
!
! DISABLE INTERRUPTS FOR MICROSOFT FORTRAN  (MASK MUST BE INTEGER*2)
!       INTEGER*2 MASK
!       MASK=4927
!       CALL LCWRQQ(MASK)
! DISABLE INTERRUPTS FOR WATFIV
!       CALL TRAPS(100,100,100,100,100)
! NO SPECIAL CALL IS REQUIRED FOR FORTVS.
!
!
        from = 0
!
! IN = INPUT UNIT, OUT = OUTPUT UNIT -- YOU MAY HAVE TO CHANGE THESE.
        in = 5
        out = 6
! THE FOLLOWING OPENS MAY BE NEEDED FOR SOME VERSIONS.
!       OPEN(IN,FILE='CON')
!       OPEN(OUT,FILE='CON')
!#######################################################################
!
!
!       CHECK FOR RESTART
!
!
!#######################################################################
        write(out,100)
        write(out,110)
100     format(' Is this a program restart after failure (1)')
110     format(' or a start from scratch (0) ?')
        read(in,120) start
120     format(i1)
        flagf = start .ne. 0
!  ***  FOR FORTRAN 66 AND FORTRAN 77 SUBSET, COMMENT OUT THE INQUIRE:
        inquire(file='TST',exist=flagf)
        if(flagf) then
            open(3,file='TST',form='UNFORMATTED',status='OLD')
            rewind 3
            open(4,file='LOG',form='UNFORMATTED',status='OLD')
            rewind 4
        else
            open(3,file='TST',form='UNFORMATTED',status='NEW')
            open(4,file='LOG',form='UNFORMATTED',status='NEW')
        endif
        if (start .eq. 0) go to 10000
        read(4) fp0,fp1,fp2,fp3,fp4,fp8,fp9,fp27,fp32,half,minus1
        read(4) fails,sdefct,defect,flaws,radix,ulppls,ulpmin,precis
        read(4) w, mulgrd,divgrd, subgrd,a1,onemin
        read(4) c1, h1, minpos, nulps, uflthr, phony0,ieee
        read(4) r1,r2,r3,r4,sticky
        read(4) pgnumb,miles
        rewind 4
        from = miles
        write(out,10001)from
10001   format(' Restarting from milestone ',i5,'.')
        if (from .eq.   7) go to   881
        if (from .eq.  79) go to  3959
        if (from .eq.  90) go to  3960
        if (from .ge. 105 .and. from .le. 109) go to 10100
        if (from .eq. 115) go to 10100
        if (from .ge. 120 .and. from .le. 125) go to 10100
        if (from .eq. 131) go to 10100
        if (from .eq. 161) go to 10200
        if (from .ge. 201 .and. from .le. 205) go to 10200
        if (from .eq. 211) go to 10300
        if (from .eq. 212) go to 10300
        call badmil
!
!             FIRST TWO ASSIGNMENTS USE INTEGERS ON RIGHT HAND SIDE
!
10000   continue
        fp0 = 0
        fp1 = 1
!910     continue
        fp2 = fp1 + fp1
!960     continue
        fp3 = fp2 + fp1
        fp4 = fp3 + fp1
!980     continue
        minus1 = -fp1
!1000    continue
        half = fp1 / fp2
!1100    continue
        fp8 = fp4 + fp4
        fp9 = fp3 * fp3
        fp27 = fp9 * fp3
        fp32 = fp8 * fp4
!
        write(out, 10)
        write(out, 40)
10      format(' A  Paranoid  Program  to  Diagnose  Floating-point Arithmetic')
40      format('          ... Single-Precision Version  ...')
!
!#######################################################################
!
        numtry = 20
!        ...  NUMTRY = #( RANDOM TRIALS OF X*Y=Y*X , ETC.)
        pgnumb=0
!        ... PGNUMB = #( PAGE OF DIAGNOSIS ); MILES=MILESTONE IN PROGRAM
        miles=0
!        ... COUNT FAILURES, SERIOUS DEFECTS, DEFECTS, FLAWS
        fails = 0
        sdefct = 0
        defect = 0
        flaws = 0
!#######################################################################
!
!
!       PRINT BIG INTRO MESSAGES
!
!
!#######################################################################
        call intro (miles)
!#######################################################################
!
!
!       SMALL INTEGER TESTING
!
!
!#######################################################################
!880     continue
        miles = 7
        call page (miles)
881     continue
        call smlint (miles,from)
        from = 0
!#######################################################################
!
!
!       FIND RADIX B AND PRECISION P
!
!
!#######################################################################
        call radx (miles)
!#######################################################################
!
!
!       TEST FOR EXTRA PRECISION IN SUBEXPRESSIONS
!
!
!#######################################################################
!1680    continue
        miles = 30
        call extra
!1860    continue
        call page (miles)
!#######################################################################
!
!
!       CHECK FOR GUARD DIGITS AND NORMALIZATION IN SUBTRACTION
!
!
!#######################################################################
!1870    continue
        miles = 35
        call guard
!2240    continue
        miles = 40
        call page (miles)
!#######################################################################
!
!
!       TEST ROUNDING IN MULTIPLY, DIVIDE, AND ADD/SUBTRACT.
!
!
!#######################################################################
        call round (miles)
!2910    continue
        miles = 60
!#######################################################################
!
!
!       TEST FOR COMMUTATIVE MULTIPLICATION
!
!
!#######################################################################
        call commut (numtry)
!3010    continue
        miles = 70
!#######################################################################
!
!
!       TEST SQUARE ROOT
!
!
!#######################################################################
3959    continue
        call square (from, miles, numtry)
        from = 0
3960    continue
        miles = 90
        call page (miles)
!#######################################################################
!
!
!       TEST Y TO POWER X
!
!
!#######################################################################
        call power (miles,from)
        from = 0
!#######################################################################
!
!
!       TEST UNDERFLOW THRESHOLDS
!
!
!#######################################################################
10100   continue
        call underf (miles,numtry,from)
        from = 0
        call page (miles)
!#######################################################################
!
!
!       TEST OVERFLOW THRESHOLDS
!
!
!#######################################################################
10200   continue
        call overf (miles, from)
        from = 0
!6110    continue
        miles = 210
!
!
!
10300   continue
        call zeros(miles,from)
        from = 0
        call page (miles)
!6150    continue
        if (fails .gt. 0) write(out, 6151) fails
6151    format (' The number of  FAILUREs  encountered =       ',i4)
!6160    continue
        if (sdefct .gt. 0) write(out, 6161) sdefct
6161    format (' The number of  SERIOUS DEFECTs  discovered = ',i4)
!6170    continue
        if (defect .gt. 0) write(out, 6171) defect
6171    format (' The number of  DEFECTs  discovered =         ',i4)
!6180    continue
        if (flaws .gt. 0) write(out, 6181) flaws
6181    format (' The number of  FLAWs  discovered =           ',i4)
!6190    continue
        if (fails+sdefct+defect+flaws .gt. 0) goto 6270
        write(out, 6200)
6200    format(' No failures, defects nor flaws have been discovered.')
!6210    continue
        if (r1+r2+r3+r4 .lt. fp4) goto 6260
!6220    continue
        if (sticky .lt. fp1 .or. (radix-fp2)*(radix-fp9-fp1) .ne. fp0)goto 6250
!6230    continue
        temp = 854
        if (radix .eq. fp2 .and.(precis - fp4*fp3*fp2) * (precis - fp27-fp27+fp1) .eq. fp0) temp = 754
        write(out,6240) temp
6240    format (' Rounding appears to conform to the proposed IEEE standard  P', i3)
        if (ieee .eq. 0) write(out, 6241)
6241    format (' except possibly for Single Rounding during Gradual Underflow.')
6250    continue
        write(out, 6251)
6251    format(' The arithmetic diagnosed appears to be Excellent!')
        goto 6310
6260    continue
        write(out, 6261)
6261    format(' The arithmetic diagnosed seems Satisfactory.')
        goto 6310
6270    continue
        if (fails+sdefct+defect .eq. 0 .and. flaws .gt. 0)write(out, 6271)
6271    format(' The arithmetic diagnosed seems Satisfactory though flawed.')
!6280    continue
        if (fails+sdefct .eq. 0 .and. defect .gt. 0) write(out, 6281)
6281    format(' The arithmetic diagnosed may be Acceptable despite inconvenient Defects.')
!6290    continue
        if (fails+sdefct .gt. 0) write(out, 6291)
6291    format(' The arithmetic diagnosed has unacceptable Serious Defects.')
!6300    continue
        if (fails .gt. 0) write(out, 6301)
6301    format(' Potentially fatal FAILURE may have spoiled this program''s subsequent diagnoses.')
6310    continue
        write(out, 6311)
6311    format(' End of Test.')
        return
        end subroutine runtests
!#######################################################################
      subroutine commut( numtry)
      implicit none
!
!
        integer numtry
        real r9, x, x9, y, y9, z, z9
        integer i, nn
!
!2920    continue
        write(out,2921) numtry
2921    format(/' Does multiplication commute? Testing if  x*y = y*x  for', i4,' random pairs:')
!2930    continue
        r9 =  sqrt(fp3)
        i = numtry + 1
        x9 = fp0 / fp3
2960    continue
        call random (x, y, x9, r9)
        y9=x9
        call random (x, y, x9, r9)
        z=x9*y9
        y=y9*x9
        z9=z-y
        i=i-1
        if (i .gt. 0 .and. z9 .eq. fp0) goto 2960
!2970    continue
        if (i .gt. 0) goto 3000
!2980    continue
        x9=fp0+half/fp3
        y9=(ulppls+ulpmin)+fp0
        z=x9*y9
        y=y9*x9
        z9=(fp0+half/fp3)*((ulppls+ulpmin)+fp0)-((ulppls+ulpmin)+fp0)*(fp0+half/fp3)
        if (z9 .ne. fp0) goto 3000
        write(out,2990) numtry
2990    format(' No failure found in ',i4,' randomly chosen pairs.')
        return
3000    continue
        defect=defect+1
        write(out, 3001) x9, y9
        write(out, 3002) z, y, z9
        nn=numtry-i+1
        write(out, 3003) nn
3001    format(' DEFECT:  x*y = y*x  violated at  x = ',e15.7,', y = ',e15.7)
3002    format('  x*y =',e15.7,',  y*x =',e15.7,',  x*y-y*x =',e15.7)
3003    format('    ... pair no.', i4)
        return
        end subroutine commut
!#######################################################################
!---
      subroutine random (x, y, x9, r9)
      implicit none
        real x, y, x9, r9
!2950    continue
        x=x9+r9
        y=x*x
        y=y*y
        x=x*y
        y=x-aint(x)
        x9=y+x*.000005
        return
        end subroutine random
!#######################################################################
!---
      subroutine extra
      implicit none
!
!
        real q, x, x1, y, y1, z, z1, z2, xx
!
        write(out,1681)
1681    format (' Test for extra-precise subexpressions:')
!
!1690    continue
        x =  abs( ((fp4 / fp3 - fp1) - fp1 / fp4) * fp3 - fp1 / fp4)
1700    continue
        z2 = x
        x = (fp1 + (half * z2 + fp32 * z2 * z2)) - fp1
        if (z2 .gt. x .and. x .gt. fp0) goto 1700
!1710    continue
        y =  abs( (fp3/fp4 - fp2/fp3) * fp3 - fp1/fp4)
        z=y
        x=y
1720    continue
        z1=z
        z=(fp1/fp2 - ((fp1/fp2-(half*z1 + fp32*z1*z1))+fp1/fp2)) + fp1/fp2
        if (z1 .gt. z .and. z .gt. fp0) goto 1720
1730    continue
        y1=y
        y=(half - ((half-(half*y1 + fp32*y1*y1))+half)) + half
        if (y1 .gt. y .and. y .gt. fp0) goto 1730
!1740    continue
        x1=x
        x=((half*x1+fp32*x1*x1)-onemin)+onemin
        if (x1 .gt. x  .and. x  .gt. fp0) goto 1730
!1750    continue
        if (x1 .eq. y1 .and. x1 .eq. z1)  goto 1780
!1760    continue
        sdefct=sdefct+1
        write(out, 1761)
        write(out, 1762) x1, y1, z1
        write(out, 1763)
        write(out, 1770)
        write(out, 1771)
        write(out, 1772)
1761    format(' SERIOUS DEFECT: disagreements among the values  X1, Y1, Z1')
1762    format(' respectively ',e15.7,',    ',e15.7,',    ',e15.7)
1763    format(' are symptoms of inconsistencies introduced by extra-precise')
1770    format(' evaluation of allegedly  "optimized"  arithmetic')
1771    format(' subexpressions.  Possibly some part of this')
1772    format(' test is inconsistent; PLEASE NOTIFY KARPINSKI !')
        if (x1 .eq. ulpmin .or. y1 .eq. ulpmin .or. z1 .eq. ulpmin)goto 1850
1780    continue
        if (z1 .ne. ulpmin .or. z2 .ne. ulppls) goto 1790
        write(out, 1781)
1781    format(' Subexpressions do not appear to be calculated with extra precision.')
        return
1790    continue
        if (z1 .lt. ulpmin .and. z2 .lt. ulppls) goto 1810
!1800    continue
        fails=fails+1
        write(out, 1801)
        write(out, 1802)
        xx=z1-ulpmin
        write(out, 1803) ulpmin, xx
        xx=z2-ulppls
        write(out, 1804) ulppls, xx
1801    format(' FAILURE: precision test is inconsistent.')
1802    format(' PLEASE NOTIFY KARPINSKI !')
1803    format(' ulpmin =  ', e15.7, '    z1 - ulpmin = ', e15.7)
1804    format(' ulppls =  ', e15.7, '    z1 - ulppls = ', e15.7)
        return
1810    continue
        if (z1 .gt. fp0 .and. z2 .gt. fp0) goto 1830
!1820    continue
        write(out, 1821)     radix
        write(out, 1822)
        write(out, 1823) z1, z2
        write(out, 1824)
        write(out, 1825)
1821    format(' Because of an unusual radix  b =', f4.0,',')
1822    format(' or exact rational arithmetic,')
1823    format(' a result  z1 =',e15.7,'  or  z2 =',e15.7)
1824    format(' of an extra-precision test is inconsistent.')
1825    format(' PLEASE NOTIFY KARPINSKI !')
        if (z1 .eq. z2) goto 1850
1830    continue
        x = z1/ulpmin
        y = z2/ulppls
        if (y .gt. x) x=y
!1840    continue
        q = -alog(x)
        write(out, 1841)
        xx=q/alog(radix)
        write(out, 1842) xx
        xx=q/alog(fp8+fp2)
        write(out, 1843) xx
1841    format(' Some subexpressions appear to be calculated extra-precisely')
1842    format(' with about   ',e15.7,' extra base b digits, i.e.')
1843    format(' roughly ',e15.7,' extra significant decimals.')
1850    continue
        write(out, 1851)
1851    format(' That feature is not tested further by this program.')
        return
        end subroutine extra
!#######################################################################
!---
      subroutine guard
      implicit none
!
!
!        ... LOCAL VARIABLES
        real r, s, t, x, y, z
!        ... CONSTANTS
        real b9
!
        b9 = radix - ulppls
!
!1040    continue
        mulgrd = 1.0
        divgrd = 1.0
        subgrd = 1.0
!
!1880    continue
        if (radix .lt. fp2) goto 1920
!1890    continue
        x = w / (radix * radix)
        y = x + fp1
        z = y - x
        t = z + ulppls
        x = t - z
        if (x .eq. ulppls) goto 1910
!1900    continue
        fails = fails + 1
        write(out, 1905)
1905    format(' FAILURE: subtraction is not normalized so  x=y  does not imply  x+z=y+z !')
        goto 1920
1910    continue
        write(out,1911)
1911    format(' Subtraction appears to be normalized as it should.')
1920    continue
        write(out,1930)
1930    format(' Checking for guard digits in multiply divide and subtract.')
!1940    continue
        y=onemin*fp1
        z=fp1*onemin
        x=onemin-half
        y=(y-half)-x
        z=(z-half)-x
        x=fp1+ulppls
        t = x * radix
!1950    continue
        r = radix * x
        x=t-radix
        x=x-radix*ulppls
        t=r-radix
        t=t-radix*ulppls
        x=x*(radix-fp1)
        t=t*(radix-fp1)
        if (x .eq. fp0 .and. y .eq. fp0 .and. z .eq. fp0 .and. t .eq. fp0)goto 1980
!1960    continue
        sdefct=sdefct+1
!1970    continue
        mulgrd=fp0
        write(out, 1971)
1971    format(' SERIOUS DEFECT: multiplication lacks a guard digit violating  1*x = x .')
1980    continue
        z = radix * ulppls
        x=fp1+z
        y= abs((x+z)-x*x)-ulppls
        x=fp1-ulppls
        z= abs((x-ulppls)-x*x)-ulpmin
        if (y .le. fp0 .and. z .le. fp0) goto 2000
!1990    continue
        fails=fails+1
        write(out, 1991)
1991    format(' FAILURE: multiplication  gets too many last digits wrong.')
2000    continue
        y=fp1-ulppls
        x=fp1+ulppls
        z=fp1/y
        y=z-x
        x = fp1/fp3
        z = fp3/fp9
        x=x-z
        t = fp9/fp27
!2010    continue
        z=z-t
        if (x .eq. fp0 .and. y .eq. fp0 .and. z .eq. fp0) goto 2040
!2020    continue
        defect=defect+1
!2030    continue
        divgrd=fp0
        write(out,2031)
        write(out,2032)
2031    format(' DEFECT: division lacks a guard digit so error can exceed 1 ulp')
2032    format(' or  1/3  and  3/9  and  9/27  may disagree.')
2040    continue
        y=onemin/fp1
        x=onemin-half
        y=(y-half)-x
        x=fp1+ulppls
        t=x/fp1
        x=t-x
        if (x .eq. fp0 .and. y .eq. fp0) goto 2070
!2050    continue
        sdefct = sdefct + 1
        defect = defect - 1 + divgrd
!2060    continue
        divgrd=fp0
        write(out, 2061)
2061    format(' SERIOUS DEFECT:  division lacks a guard digit violating  x/1 = x .')
2070    continue
        x=fp1/(fp1+ulppls)
        y=x-half-half
        if (y .lt. fp0) goto 2100
!2080    continue
        sdefct=sdefct+1
!2090    continue
        write(out, 2091)
        write(out, 2092)
2091    format(' VERY SERIOUS DEFECT:  computed value of  1/1.00...001')
2092    format(' is not less than  1 .')
2100    continue
        x=fp1-ulppls
        y = fp1 + radix * ulppls
        z = x * radix
        t = y * radix
        r = z / radix
        s = t / radix
        x = r - x
        y = s - y
        if (x .eq. fp0 .and. y .eq. fp0) goto 2130
!2110    continue
        fails = fails + 1
        write(out, 2120)
        write(out, 2121)
2120    format(' FAILURE: multiplication  and/or  division')
2121    format(' gets too many last digits wrong.')
2130    continue
        y = fp1-ulpmin
        x = fp1-onemin
        y = fp1-y
        t = radix - ulppls
        z = radix - b9
        t = radix - t
!2140    continue
        if (x .eq. ulpmin .and. y .eq. ulpmin .and. z .eq. ulppls .and.  t .eq. ulppls) goto 2230
!2150    continue
        sdefct=sdefct+1
!2160    continue
        subgrd=fp0
        write(out, 2161)
2161    format(' SERIOUS DEFECT: subtraction lacks a guard digit so cancellation is obscured.')
!2170    continue
        if (onemin .eq. fp1 .or. onemin-fp1 .lt. fp0) return
!2180    continue
        sdefct=sdefct+1
        write(out, 2190)
        write(out, 2191)
        write(out, 2200)
        write(out, 2210)
        write(out, 2220)
2190    format(' VERY SERIOUS DEFECT:')
2191    format('   comparison alleges  (1-ulpmin) < 1  although')
2200    format('   subtraction yields  (1-ulpmin) - 1 = 0, thereby vitiating')
2210    format('   such precautions against division by zero as')
2220    format('   ...  if (x=1.0) then ..... else .../(x-1.0)...')
!
2230    continue
        if (mulgrd * divgrd * subgrd .eq. fp1) write(out, 2231)
2231    format(' These operations appear to have guard digits as they should.')
        return
        end subroutine guard
!#######################################################################
!---
        subroutine intro(miles)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!
!
!       PRINT THE LARGE BANNER INTRODUCTION - GOES ON FOR SEVERAL
!       PAGES
!
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
        integer miles
!        ... NUMBER OF MILESTONE TO PRINTOUT AS WE GO ALONG
        write(out,170)
        write(out,180)
        call page(miles)
        write(out,190)
        call page(miles)
        write(out,200)
170     format(                                                         &
     &    ' Lest this program stop prematurely, i.e. before displaying' &
     &   /'   "End of Test"   '                                         &
     &   /' try to persuade the computer NOT to terminate execution'    &
     &   /' whenever an error such as Over/Underflow or Division by'    &
     &   /' Zero occurs, but rather to persevere with a surrogate value'&
     &   /' after, perhaps, displaying some warning.  If persuasion'    &
     &   /' avails naught, don''t despair but run this program anyway')
180     format(                                                         &
     &    ' to see how many milestones it passes, and then run it'      &
     &   /' again.  It should pick up just beyond the error and'        &
     &   /' continue.  If it does not, it needs further debugging.'     &
     &  //' Users are invited to help debug and augment this program'   &
     &   /' so that it will cope with unanticipated and newly found'    &
     &   /' compilers and arithmetic pathologies.')
190     format(                                                         &
     &   /' Please send suggestions and interesting results to'         &
     &        /9x,'Richard Karpinski'                                   &
     &        /9x,'Computer Center U-76'                                &
     &        /9x,'University of California'                            &
     &        /9x,'San Francisco, CA 94143-0704'                        &
     &        /9x,'USA'/                                                &
     &        /' In doing so, please include the following information:'&
     &        /9x,'Precision:   Single;'/9x,'Version: 31 July 1986;'    &
     &        /9x,'Computer:'//9x,'Compiler:'//9x,'Optimization level:'/&
     &        /9x,'Other relevant compiler options:'/)
200     format(                                                         &
     &        /' BASIC version (C) 1983 by Prof. W. M. Kahan.'          &
     &        /' Translated to FORTRAN by T. Quarles and G. Taylor.'    &
     &        /' Modified to ANSI 66/ANSI 77 compatible subset by'      &
     &        /' Daniel Feenberg and David Gay.'                        &
     &        /' You may redistribute this program freely if you'       &
     &        /' acknowledge the source.')
!#####################################################################
        write(out,480)
        call page (miles)
        write(out,530)
        write(out,590)
        write(out,640)
480     format(//                                                       &
     &  ' Running this program should reveal these characteristics:'//  &
     &  ' b = radix ( 1, 2, 4, 8, 10, 16, 100, 256, or ... ) .'/        &
     &  ' p = precision, the number of significant  b-digits carried.'/ &
     &  ' u2 = b/b^p = one ulp (unit in the last place) of 1.000xxx..'/ &
     &  ' u1 = 1/b^p = one ulp of numbers a little less than 1.0.')
530     format(' g1, g2, g3 tell whether adequate guard digits are carried;'/  &
     &  ' 1 = yes, 0 = no;  g1 for mult.,  g2 for div., g3 for subt.'/  &
     &  ' r1,r2,r3,r4  tell whether arithmetic is rounded or chopped;'/ &
     &  ' 0=chopped, 1=correctly rounded, -1=some other rounding;'/     &
     &  ' r1 for mult., r2 for div., r3 for add/subt., r4 for sqrt.'/   &
     &  ' s=1 when a sticky bit is used correctly in rounding; else s=0 &
     &.')
590     format(' u0 = an underflow threshold.'/                         &
     & ' e0 and z0 tell whether underflow is abrupt, gradual or fuzzy'/ &
     & ' v = an overflow threshold, roughly.'/                          &
     & ' v0  tells, roughly, whether  infinity  is represented.'/       &
     & ' Comparisons are checked for consistency with subtraction')
640     format('        and for contamination by pseudo-zeros.'/        &
     & ' Sqrt is tested. so is  y^x  for (mostly) integers  x .'/       &
     & ' Extra-precise subexpressions are revealed but not yet tested.' &
     & /' Decimal-binary conversion is not yet tested for accuracy.')
!#####################################################################
        call page (miles)
        write(out,690)
        write(out,760)
        write(out,780)
        write(out,820)
690     format(' The program attempts to discriminate among:'/          &
     &         '     >FLAWs, like lack of a sticky bit, '/              &
     &         '     >SERIOUS DEFECTs, like lack of a guard digit, and'/&
     &         '     >FAILUREs, like  2+2 = 5 .'/                       &
     &         ' Failures may confound subsequent diagnoses.')
760     format(/                                                        &
     &  ' The diagnostic capabilities of this program go beyond an'/    &
     &  ' earlier program called  "Machar", which can be found at the'/ &
     &' end of the book "Software Manual for the Elementary Functions"')
780     format(                                                         &
     &  ' (1980) by W. J. Cody and W. Waite. Although both programs'/   &
     &  ' try to discover the radix (b), precision (p) and         '/   &
     &  ' range (over/underflow thresholds) of the arithmetic, this'/   &
     &  ' program tries to cope with a wider variety of pathologies')
820     format(                                                         &
     &  ' and to say how well the arithmetic is implemented.'/          &
     &  ' The program is based upon a conventional radix'/              &
     &  ' representation for floating-point numbers,'/                  &
     &  ' but also allows for logarithmic encoding (b = 1)'/            &
     &  ' as used by certain early wang machines.'/)
        return
        end subroutine intro
!#######################################################################
!---
        subroutine logit (mile)
      implicit none
!
        integer mile
!        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
!
!        THIS ROUTINE FORCES A CHECKPOINT BY  WRITING ALL GLOBAL
!        INFORMATION TO A FILE FOR LATER RESTARTING.
!
        write(4) fp0,fp1,fp2,fp3,fp4,fp8,fp9,fp27,fp32,half,minus1
        write(4) fails,sdefct,defect,flaws,radix,ulppls,ulpmin,precis
        write(4) w, mulgrd,divgrd, subgrd,a1,onemin
        write(4) c1, h1, minpos, nulps, uflthr, phony0, ieee
        write(4) r1, r2, r3, r4, sticky
        write(4) pgnumb,mile
        rewind 4
        return
        end subroutine logit
!#######################################################################
!---
!       SUBROUTINE TO TEST IF  Y = X
!
        subroutine cmpxy (x, y, z, q, n)
      implicit none
!
!
        real fp0, x, xx, y, z
        integer q, n
        data fp0/0.e0/
        y=z**q
!4040    continue
        if (y .eq. x) goto 4080
        if (z .gt. fp0) go to 4050
        if (q .gt. fp0) go to 4050
        write(out, 40401) z, q, y
40401   format(' WARNING: computed  (',e16.8,')^(',i3,') = ',e16.8)
        go to 40601
4050    continue
        if (n .gt. 0) goto 4070
!4060    continue
        defect = defect + 1
        write(out, 4061) z, q, y
40601   continue
        write(out, 4062) x
        xx=y-x
        write(out, 4063) xx
4061    format(' DEFECT: computed  (',e16.8,')^(',i3,') = ',e16.8)
4062    format('    compares unequal to correct ',e16.8)
4063    format('    they differ by  ',e16.8)
!                       INCREMENT COUNT OF DISCREPANCIES
4070    continue
        n = n + 1
4080    continue
        return
        end subroutine cmpxy
!#######################################################################
!---
!       SUBROUTINE TO PRINT N AND PAUSE IF N > 0.
!
        subroutine prt2 (n, miles)
      implicit none
        integer n, miles
!4290    continue
        if (n .eq. 0) write(out, 4291)
4291    format(' No discrepancies found.'/)
!4310    continue
        if (n .gt. 0) call page(miles)
!       ---- PAUSE ----
!4320    continue
        return
        end subroutine prt2
!#######################################################################
!---
!       SUBROUTINE TO COMPARE  Z^I  WITH  X = Z*Z*...*Z  ( I TIMES )
!
        subroutine pwrcmp (x, z, i, m, n)
      implicit none
!
!
        real x, z
        integer i, m, n
        real y
        integer q
3990    continue
        y = z ** i
        q = i
!                               TEST WHETHER  Y=X
        call cmpxy (x, y, z, q, n)
!4000    continue
        i = i + 1
!                               WITH  X = Z^M
        if (i .gt. m) return
!4010    continue
        x = z * x
        if (x .lt. w) goto 3990
!4020    continue
        return
        end subroutine pwrcmp
!#######################################################################
!---
!       SUBROUTINE TO PRINT COUNT  N  OF DISCREPANCIES
!
        subroutine prtcnt (n)
      implicit none
        integer n
!4100    continue
        if (n .gt. 0) write(out, 4101) n
4101    format(' Similar discrepancies have occurred ',i4,' times.')
!4110    continue
        return
        end subroutine prtcnt
!#######################################################################
!---
        subroutine badsqr(sflag,z,y)
      implicit none
        integer sflag
!        ... INTEGER FLAG TO INDICATE NEED TO USE PREFIX "SERIOUS"
        real z
!        ... SQUARE OF SQUARE ROOT OF Z (WRONG)
        real y
!        ... REAL NUMBER WHOSE SQUARE ROOT SQUARED IS WRONG
        if(sflag .eq. 1) go to 5745
        write(out,5740) z
5740    format(' DEFECT:  comparison alleges that what prints as  z = ',e16.8)
        go to 5748
5745    continue
        write(out,5747) z
5747    format(' SERIOUS DEFECT:  comparison alleges that what prints as z = ',e16.8)
5748    continue
        write(out,5749) y
5749    format(17x,'is too far from  sqrt(z)^2 = ',e16.8,'.')
        return
        end subroutine badsqr
!#######################################################################
!---
        subroutine overf (miles, from)
      implicit none
!
!        ... NUMBER OF MILESTONES PASSED
        integer miles
!        ... COUNTER OF MILESTONES PREVIOUSLY REACHED
        integer from
!
!        ... LOCAL VARIABLES
        integer i
        real v9, x, y, z, temp
!        ... SATURATION VALUE AFTER FLOATING POINT OVERFLOW
        real sat
!        ... OVERFLOW THRESHOLD
        real v
!        ... FLAG TO INDICATE WHETHER DEFECT IS SERIOUS OR NOT
        integer zflag
!
        integer i0
!
!       IBM        SIGN-MAGNITUDE, SATURATION VALUE, NO TRAP
!       PRIME      TWOS-COMPLEMENT, SATURATION VALUE, NO TRAP
!       VAX        SIGN-MAGNITUDE, TRAP
!       ELXSI      SIGN-MAGNITUDE, TRAP OR INFINITY SYMBOL IF NO TRAP
!       CDC        SIGN-MAGNITUDE, INFINITY SYMBOL, NO TRAP
!
!           CALL TO UNIX TO ENABLE TRAPS (FAULTS)
!           ON FP UNDERFLOW AND FIXED POINT OVERFLOW
!
        if (from .eq. 0) goto 5500
!
!           REASSIGN VALUES TO VARIABLES USING THE LOG FILE,
!           THEN GO TO RESTART POINT
!
        read(3) i,v,sat,v9,x,y,z,zflag
        rewind 3
        if (from .eq. 161) goto 5582
        if (from .eq. 170) goto 5680
        if (from .eq. 175) goto 5810
        if (from .ge. 201 .and. from .le. 205) go to 5999
        call badmil
!
5500    continue
        write(out,5510)
5510    format(' Searching for overflow threshold:')
        miles = 161
        call logit (miles)
!
!                  SET Y TO -1 * A LARGE POWER OF THE RADIX
!5520    continue
        y = -c1
        v9 = h1 * y
!
!                  MULTIPLY BY RADIX (H1) UNTIL OVERFLOW OCCURS
!
5530    continue
        v = y
        y = v9
        write(3) i,v,sat,v9,x,y,z,zflag
        rewind 3
        v9 = h1 * y
        if (v9 .lt. y) goto 5530
!
!       SYSTEM DOES NOT TRAP ON OVERFLOW
!
!           POSSIBILITIES:
!               V9 > Y,  V9 IS THE VALUE RETURNED AFTER OVERFLOW
!                        Y IS THE LARGEST POWER OF THE RADIX
!                        V IS THE SECOND LARGEST POWER OF THE RADIX
!
!               V9 == Y, BOTH ARE SATURATION VALUE
!                        V IS THE LARGEST POWER OF THE RADIX
!
!               V9 == Y, BOTH ARE INFINITY SYMBOL
!                        V IS THE LARGEST POWER OF THE RADIX
!
!       TEST 1: VALUE RETURNED AFTER OVERFLOW SHRINKS IN MAGNITUDE
!
!5540    continue
        if (v9 .eq. y) goto 5545
        sdefct = sdefct + 1
        write(out, 5541) y, v9
5541    format(' SERIOUS DEFECT: overflow past  ', 1pe16.8,'  shrinks to ', 1pe16.8)
!
!       TEST 2: TWO'S COMPLEMENT MACHINE SATURATES AT NEGATIVE
!               LARGEST POWER OF THE RADIX
!               NEED TO DISTINGUISH SYSTEM WITH OVERFLOW SYMBOLS
!               FROM ONE WITHOUT THEM
!
5545    continue
        write(out,5546) y
5546    format(' Can " z = -y " overflow?  trying it on  y = ',1pe16.8)
        sat = -y
        if (v - y .eq. v + sat) goto 5560
!5550    continue
        flaws = flaws + 1
        write(out, 5551)
5551    format(' Finds a FLAW:  -(-y) differs from y.')
        goto 5590
5560    continue
        write(out, 5561)
5561    format(' Seems O.K.')
        goto 5590
!
!       RESTART POINT FOR SYSTEMS THAT TRAP ON OVERFLOW
!
!             V9 = Y =  -(LARGEST POWER OF RADIX)
!             V      =  -(SECOND LARGEST POWER OF RADIX)
!
!       TEST 2: TWO'S COMPLEMENT MACHINE
!
5582    continue
        write(out,5583) y
5583    format(' Can " z = -y " overflow?  trying it on  y = ',1pe16.8)
!
!           PUT SOMETHING HERE TO HANDLE THE TRAP
!
        sat = -y
        if (v - y .eq. v + sat) goto 5585
        flaws = flaws + 1
        write(out, 5584)
5584    format('  Finds a FLAW:  -(-y) differs from y.')
        goto 5587
5585    continue
        write(out, 5586)
5586    format(' Seems O.K.')
!
!           THIS CODE WORKS FOR A SIGN-MAGNITUDE MACHINE,
!           BUT FAILS FOR A TWOS-COMPLEMENT ONE
!
5587    continue
        v = y * (h1 * ulppls - h1)
        v = v + y * ((fp1 - h1) * ulppls)
!
        write(out, 5588) v
5588    format(' Overflow threshold is  v = ',1pe16.8)
        write(out, 5589)
5589    format(' There is no saturation value because'/' the system traps on overflow.')
        goto 5640
!
!       NON-TRAPPING SYSTEMS (CONTINUED)
!
5590    continue
        y = v * (h1 * ulppls - h1)
        z = y + v * ((fp1 - h1) * ulppls)
        if (z .lt. sat) y = z
!5600    continue
        if (y .lt. sat) v = y
!
!                  THE OVERFLOW THRESHOLD EQUALS THE SATURATION VALUE
!                  IF THE LATTER BEHAVES AS A NUMBER RATHER THAN AN
!                  OVERFLOW SYMBOL.  AN OVERFLOW SYMBOL IS NOT
!                  CHANGED WHEN ANY NUMBER IS ADDED TO OR SUBTRACTED
!                  FROM IT.
!
!5610    continue
        if (sat - v .lt. sat) v = sat
!
        write(out, 5620) v
5620    format(' Overflow threshold is  v = ',1pe16.8)
        write(out, 5630) sat
5630    format(' Overflow saturates at  sat = ',1pe16.8)
!
!
!
5640    continue
        miles = 163
        write(out, 5641)
5641    format(' No overflow should be signaled for  v*1 = ')
        temp = v * fp1
        write(out, 5642) temp
5642    format('                                           ',1pe16.8)
        write(out, 5643)
5643    format('                            nor for  v/1 = ')
        temp = v / fp1
        write(out, 5644) temp
5644    format('                                           ',1pe16.8)
        write(out,5649)
5649    format(' Any overflow signal separating this  *  from one above is a DEFECT.')
!
!       NEED TO ADD CODE HERE TO HANDLE OVERFLOWS JUST ABOVE
!
!
!
!5650    continue
        miles=170
!
!       PROBLEM: SAT NOT DEFINED IF WE TRAPPED ON OVERFLOW ABOVE
!
!5660    continue
        if (-v .lt. v .and. -sat .lt. sat .and. -uflthr .lt. v .and. uflthr .lt. v) goto 5680
!5670    continue
        fails = fails + 1
        write(out,5672)
5672    format(' FAILURE: comparisons are confused by overflow.')
!
!
!
5680    continue
        miles = 175
        i = 0
        z = uflthr
5700    continue
        i = i + 1
        if (z .eq. fp0) go to 5770
!5710    continue
        v9= sqrt(z)
        y=v9*v9
        if (.not. (y/(fp1-radix * nulps) .lt. z .or. y .gt. (fp1+radix*nulps)*z)) goto 5770
!5720    continue
        if (v9 .gt. ulpmin) goto 5750
!5730    continue
        zflag=0
        defect=1+defect
        goto 5760
5750    continue
        zflag=1
        sdefct=1+sdefct
5760    continue
        call badsqr(zflag,z,y)
5770    continue
        goto (5780, 5790, 5800),i
5780    continue
        z = minpos
        goto 5700
5790    continue
        z = phony0
        goto 5700
!
!
!
5800    continue
        i=0
        z=v
5810    continue
        miles=180
!5820    continue
        if (radix .ne. 2. .or. precis .ne. 56. .or. phony0 .eq. 0. .or. -fp0 .eq. fp0) goto 5850
!5830    continue
        fails=1+fails
! FAILURE: ATTEMPTS TO EVALUATE  SQR(OVERFLOW THRESHOLD V)  IN DOUBLE
! PRECISION IN  BASIC  ON THE  IBM PC  DISPLAY THE WORD  " OVERFLOW "
! AND THEN DISABLE THE KEYBOARD !  THIS IS DISASTROUS.
        goto 5920
!
!
!
5850    continue
        v9 =  sqrt(z)
        x = (fp1 - radix * nulps) * v9
        v9 = v9 * x
        if (.not. (v9 .lt. (fp1-fp2*radix*nulps)*z .or. v9 .gt. z)) goto 5900
!5860    continue
        y=v9
        if (x .lt. w) go to 5880
!5870    continue
        zflag = 0
        defect = defect + 1
        goto 5890
5880    continue
        zflag = 1
        sdefct = sdefct + 1
5890    continue
        i = 1
        call badsqr(zflag,z,y)
5900    continue
        if (i .eq. 1) go to 5920
!5910    continue
        i = 1
        z = sat
        goto 5850
!
!
!
5920    continue
        miles = 190
        call page (miles)
!5940    continue
        x = uflthr * v
        y = radix * radix
        if (x * y .ge. fp1 .and. x .le. y) goto 5990
!5950    continue
        if (x * y .ge. ulpmin .and. x .le. y / ulpmin) goto 5970
!5960    continue
        defect = defect + 1
        write(out, 5961) x
5961    format(' DEFECT: badly unbalanced range;  UFLTHR * V  =', e13.5,' IS TOO FAR FROM 1.')
        goto 5990
5970    continue
        flaws = flaws + 1
        write(out, 5971) x
5971    format(' FLAW: unbalanced range;  UFLTHR * V  =', e13.5,' IS TOO FAR FROM 1.')
!
!                               TEST  X/X   VS.  1
5990    continue
        i0 = 1
        go to 6000
5999    continue
        i0 = from - 200
!
6000    do 6100 i = i0, 5
!6010    continue
        x = onemin
        if (i .eq. 2) x = fp1 + ulppls
        if (i .eq. 3) x = v
        if (i .eq. 4) x = uflthr
        if (i .eq. 5) x = radix
        miles = 200 + i
        if (from .ne. miles) go to 6050
        sdefct = sdefct + 1
        write(out,6011) x
6011    format(' SERIOUS DEFECT: x/x traps when x = ',e13.5)
        go to 6100
6050    continue
        y = x
        call logit(miles)
        v9 = (y / x - half) - half
        if (v9 .eq. fp0) goto 6100
        if (v9 .eq. -ulpmin .and. i .lt. 5) goto 6080
!6070    continue
        fails = fails + 1
        write(out, 6071) x
6071    format(' FAILURE:  x/x differs from 1 when x = ',e13.5)
        goto 6090
6080    continue
        sdefct = sdefct + 1
        write(out, 6081) x
6081    format(' SERIOUS DEFECT:  x/x differs from 1 when x = ',e13.5)
6090    continue
        write(out, 6091) v9
6091    format('           instead,  x/x - 1/2 - 1/2 = ',e13.5)
6100    continue
        return
        end subroutine overf
!#######################################################################
!---
        subroutine page(mile)
      implicit none
!
        integer mile
!        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
!
!        THIS PROGRAM RUNS INTERACTIVELY PUTTING ALL OUTPUT TO A SCREEN.
!        THE NEXT SUBPROGRAM PAUSES, ALLOWING YOU TO READ THE SCREEN OR
!        COPY IT.
        write(out,110)
110     format(/' To continue diagnosis, press return.')
        read(in,111)
111     format (a4)
        pgnumb=pgnumb+1
        write(out,112) mile, pgnumb
112     format(' Diagnosis resumes after milestone  #',i5,',    ... page ',i5/)
        write(4) fp0,fp1,fp2,fp3,fp4,fp8,fp9,fp27,fp32,half,minus1
        write(4) fails,sdefct,defect,flaws,radix,ulppls,ulpmin,precis
        write(4) w, mulgrd,divgrd, subgrd,a1,onemin
        write(4) c1, h1, minpos, nulps, uflthr, phony0, ieee
        write(4) r1, r2, r3, r4, sticky
        write(4) pgnumb,mile
        rewind 4
        mile=mile+1
        return
        end subroutine page
!#######################################################################
!---
        subroutine partuf (z, zname, miles, partu, restrt)
      implicit none
        logical restrt
!
!
        integer partu
!        ... FLAG TO INDICATE THE PRESENCE OF PARTIAL UNDERFLOW
        real z
!        ... VALUE TO TEST FOR PARTIAL UNDERFLOW
        character(len=8) :: zname
!        ... NAME OF THE VARIABLE Z (IN A8 FORMAT) FOR OUTPUT
        integer miles
!        ... NUMBER OF MILESTONE REACHED SO FAR FOR OUTPUT
        real dummy
!        ... TEMPORARY VARIABLE TO HOLD A RESULT THAT MAY ACTUALLY
!        ... UNDERFLOW
        real multp1
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... MULT BY 1
        real multp2
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... 1 * SMALL
        real divtmp
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... DIV BY 1
!       ___ SUBROUTINE TO TEST  Z  FOR PARTIAL UNDERFLOW ___
!4640    continue
        partu=0
        if (restrt) go to 4740
        call logit(miles)
        if (z .eq. fp0) goto 4850
        write(out, 4660) zname, zname, zname, zname
4660    format(' Since comparison denies  ',a8,' = 0,'/'  evaluating  (',a8,' + ',a8,') / ',a8,'  should be safe;')
        dummy = (z + z) / z
        write(out, 4665) zname, zname, zname, dummy
4665    format(' what the machine gets for  (',a8,' + ',a8,') / ',a8,'  is'/10x, e15.7)
        if ( abs(dummy-fp2) .lt. radix*ulppls) go to 4750
!4670    continue
        if (dummy .lt. fp1 .or. dummy .gt. fp2) goto 4740
        partu=1
        defect=defect+1
        write(out,4675)
4675    format(' This is a DEFECT.'/)
        goto 4760
4740    continue
        partu=1
        sdefct=sdefct+1
        write(out,4745)
4745    format(' This is a VERY SERIOUS DEFECT.')
        goto 4760
4750    continue
        write(out,4751)
4751    format(' This is O.K. provided over/underflow has not just been signaled.')
4760    continue
        multp1=z*fp1
        multp2=fp1*z
        divtmp=z/fp1
        if (z .eq. multp1 .and. z .eq. multp2 .and. z .eq. divtmp)go to 4840
!4770    continue
        partu=1
        defect=defect+1
        write(out,4780)zname,z
4780    format(' DEFECT:  what prints as  ',a8,' =',e16.8,' compares different from')
!4790    continue
        if (.not. (z .eq. multp1)) write(out,4795)zname,multp1
4795    format('           ',a8,'*1 = ',e16.8)
!4800    continue
        if (.not. (z .eq. multp2 .or. multp2 .eq. multp1)) write(out,4805) zname,multp2
4805    format('           1*',a8,' = ',e16.8)
!4810    continue
        if (.not. (z .eq. divtmp)) write(out,4815)zname,divtmp
4815    format('           ',a8,'/1 = ',e16.8)
!4820    continue
        if (multp2 .eq. multp1) go to 4840
!4830    continue
        defect=defect+1
        write(out,4831)
4831    format(' DEFECT: multiplication does not commute; comparison alleges that')
        write(out,4833)zname,multp2,zname,multp1
4833    format('         1*',a8,' =',e16.8,'  differs from  ',a8,'*1 =',e16.8)
4840    continue
        if (partu .gt. 0) call page(miles)
!       ---- PAUSE ----
4850    continue
        return
        end subroutine partuf
!#######################################################################
        subroutine power(miles,from)
      implicit none
!---
!
        integer miles,from
!       ... LOCAL VARIABLES
        integer i, m, n, n1
        integer numtry
        real x, z, a
!       ... CONSTANTS
        real fp0, fp1, fp2, fp3, fp4, fp8, minus1
!
        fp0 = 0.0
        fp1 = 1.0
        fp2 = fp1 + fp1
        fp3 = fp2 + fp1
        fp4 = fp3 + fp1
        fp8 = fp4 + fp4
        minus1 = -fp1
        a = fp1 / a1
        numtry = 20
!
!3970    continue
        write(out,3971)
3971    format(' Testing powers  z^i  for small integers  z  and  i :')
        if(from .ne. 90) write(out,3972)
3972    format(' Start with 0.**0 .')
!4120    continue
        n = 0
        i = 0
        z = -fp0
        m = 3
        if(from.eq.90) goto 4160
        if (from .ne. 0) call badmil
!                       TEST POWERS OF ZERO
4130    continue
        x = fp1
        call pwrcmp (x, z, i, m, n)
        if (i .gt. 10) goto 4150
!4140    continue
        i = 1023
        call pwrcmp (x, z, i, m, n)
4150    continue
        if (z .eq. minus1) goto  4170
!                       IF (-1)^N IS INVALID, REPLACE 'MINUS1' BY 'FP1'
4160    continue
        z = minus1
        i = -4
        goto 4130
!                       PRINT  N  IF  N > 0.
4170    continue
        call prtcnt (n)
!4180    continue
        n1 = n
        n = 0
        z = a1
        m =   int(fp2 * alog(w) / alog(a1) )
!
!                       LOOP
!
4190    continue
        x = z
        i = 1
        call pwrcmp (x, z, i, m, n)
        if (z .eq. a) goto 4210
!4200    continue
        z = a
        goto 4190
!
!       POWERS OF RADIX  B  HAVE BEEN TESTED; NEXT TRY A FEW PRIMES.
!
4210    continue
        miles=100
!4220    continue
        m = numtry
        z = fp3
4230    continue
        x = z
        i = 1
        call pwrcmp (x, z, i, m, n)
4240    continue
        z = z + fp2
        if (fp3 * aint (z / fp3) .eq. z) goto 4240
!4250    continue
        if (z .lt. fp8 * fp3) goto 4230
!4260    continue
        if (n .gt. 0) write(out,4261)
4261    format(' Error like this may invalidate financial'/' calculations involving interest rates.')
!4270    continue
        call prtcnt (n)
        n = n + n1
        call prt2 (n, miles)
        return
        end subroutine power
!#######################################################################
!---
        subroutine radx(miles)
      implicit none
!
!
        integer miles
!        ... COUNT OF MILESTONES PASSED
        real x,y,z
!        ... SCRATCH VARIABLES
        real ulpmsv,radsav
!        ... TEMPS TO SAVE ULPMIN, ULPPLS, RADIX, PRECIS IN WHILE
!        ... RECOMPUTING
        real third,sixth
!        ... TEMPS TO HOLD APPROX VALUES OF 1/3 AND 1/6 IN WHILE
!        ... ACCUMULATING ERROR FOR RADIX COMPUTATIONS.
        real b9, t8
        integer i
!
        t8 = 240.0
!
        write(out,1160)
1160    format(/' Searching for radix and precision...')
!       LOOKING FOR W TO BE BIG ENOUGH TO MAKE 1 INSIGNIFICANT AT THE
!       PRECISION AVAILABLE.  INCREASE BY POWERS OF 2 UNTIL WE FIND IT.
!1170    continue
        w=fp1
1180    continue
        w=w+w
        y=w+fp1
        z=y-w
        y=z-fp1
        if ((-fp1+ abs(y)) .lt. fp0) goto 1180
!       ... NOW  W  IS JUST BIG ENOUGH THAT  |((W+1)-W)-1| >= 1 ...
!        I.E. 1 IS INSIGNIFICANT RELATIVE TO W.
!1200    continue
        precis=fp0
        y=fp1
1210    continue
        radix=w+y
        y=y+y
        radix=radix-w
        if (radix .eq. fp0) goto 1210
!
!1220    continue
        if (radix .ge. fp2) goto 1235
        radix = fp1
        write(out,1230)     radix
1230    format(' Radix = ',f7.0)
        goto 1270
!          BASE IS 1, SO IT IS NOT CHARACTERIZED BY A PRECISION, SO
!          DON'T BOTHER TO HUNT FOR IT..
!
!
!               ... RADIX >= 2 ...
!        NOW TRY TO FIND THE PRECISION (# SIG. DIGITS)
1235    continue
        write(out,1236) radix
1236    format(' Radix = ',f4.0)
!1240    continue
        w=fp1
1250    continue
        precis=precis+fp1
        w=w*radix
        y=w+fp1
        z=y-w
        if (z .eq. fp1) goto 1250
!        ... NOW  W = RADIX^PRECIS  IS BARELY TOO BIG TO SATISFY
!        ... (W+1)-W = 1  .
1270    continue
        ulpmin=fp1/w
        ulppls=radix*ulpmin
        write(out,1271) ulpmin
        write(out,1280)
1271    format(' Closest relative separation found is ',1pe16.8)
1280    format(' Recalculating radix and precision ')
!1290    continue
        radsav=radix
        ulpmsv=ulpmin
!                                               ...  SAVE OLD VALUES
!1300    continue
        x=fp4/3.0e0
        third=x-fp1
        sixth=(fp1/fp2)-third
        x=sixth+sixth
        x= abs(x-third)
        if (x .lt. ulppls) x=ulppls
!       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 + ...
1320    continue
        ulppls=x
        y=(fp1/fp2)*ulppls+3.2e1*ulppls*ulppls
        y=fp1+y
        x=y-fp1
!       X=> ((X/2) + EPSILON) MOD (1 ULP OF 1+)
        if (ulppls .gt. x .and. x .gt. fp0) goto 1320
!        IF X DOES NOT UNDERFLOW TO 0, THEN IT IS STILL (UNKNOWN) * ULP
!        SO TRY AGAIN....  OTHERWISE, PREVIOUS VALUE (ULPPLS) IS 1 ULP
!       ... NOW  ULPPLS = 1 ULP OF  1 + ...
!1340    continue
        x=fp2/3.0e0
        sixth=x-(fp1/fp2)
        third=sixth+sixth
        x=third-(fp1/fp2)
        x= abs(x+sixth)
        if (x .lt. ulpmin) x=ulpmin
!       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 - ...
1360    continue
        ulpmin=x
        y=(fp1/fp2)*ulpmin+3.2e1*ulpmin*ulpmin
        y=(fp1/fp2)-y
        x=(fp1/fp2)+y
        y=(fp1/fp2)-x
        x=(fp1/fp2)+y
!        X => (X/2 = EPSILON) MOD (1 ULP OF 1-)
        if (ulpmin .gt. x .and. x .gt. fp0) goto 1360
!       ... NOW  ULPMIN = 1 ULP OF  1 - ...
!
!       NOW TO SUMMARIZE THE RESULTS
!
!1380    continue
        if (ulpmin .eq. ulpmsv) write(out,1381)
1381    format(' confirms closest relative separation .')
!1390    continue
        if (ulpmin .ne. ulpmsv) write(out,1391) ulpmin
1391    format(' gets better closest relative separation = ', e13.5)
!1400    continue
        w=fp1/ulpmin
        onemin = (half - ulpmin) + half
!       ... = 1 - ULPMIN = NEXTAFTER(1.0, 0)
!1410    continue
        radix=aint(.01 + ulppls/ulpmin)
        if (radix .eq. radsav) write(out,1411)
1411    format(' Radix confirmed.')
!1420    continue
        if (radix .ne. radsav) write(out,1421) radix
1421    format(' mystery: recalculated radix = ', e13.5)
!
!       ... RADICES 1, 2 AND 10 PASS MUSTER
!
!1430    continue
        if (radix .eq. fp2 .or. radix .eq. 1.0e1 .or. radix .eq. fp1)goto 1470
!1440    continue
        if (radix .gt. 1.6e1) goto 1460
!1450    continue
        flaws=flaws+1
        write(out,1451) radix
1451    format(' FLAW: radix =',f4.0,' is not so good as 2 or 10.')
        goto 1470
1460    continue
        defect=defect+1
        write(out,1461) radix
1461    format(' DEFECT: radix =',f4.0,' is so big that roundoff propagates capriciously.')
1470    continue
        miles=20
!       TEST FOR FUZZY COMPARISON ... ==================================
!1480    continue
        if (onemin-half .lt. half) goto 1510
!1490    continue
        fails=fails+1
        write(out,1500)
1500    format(' FAILURE: (1-u1)-1/2 < 1/2  is false, so this program may malfunction.')
1510    continue
        x=onemin
        i=1
1520    continue
        y=x-half
        z=y-half
        if (x .ne. fp1 .or. z .eq. fp0) goto 1540
!1530    continue
        fails=fails+1
        write(out,1535)
1535    format(' FAILURE: comparison is fuzzy; it alleges x=1 although')
        write(out,1537)z
1537    format('         subtraction yields  (x - 1/2) - 1/2 = ',d16.8)
1540    continue
        if (i .eq. 0) goto 1560
!1550    continue
        x=fp1+ulppls
        i=0
        goto 1520
1560    continue
        miles=25
!       END OF TEST FOR FUZZY COMPARISON.===============================
!1570    continue
        b9 = radix - fp1
        b9=(b9-ulppls)+fp1
        if (radix .eq. fp1) go to 1610
!       ... B9 = NEXTAFTER(RADIX, 0)
!1580    continue
        x=-t8*alog(ulpmin)/alog(radix)
        y=aint(half+x)
        if (  abs(x-y)*fp4 .lt. fp1 ) x=y
!1590    continue
        precis = x/t8
        y=aint(half + precis)
        if (  abs(precis-y)*t8 .lt. half ) precis=y
!       PURIFY INTEGERS.
!1600    continue
        if (precis .eq. aint(precis)) goto 1640
1610    continue
        write(out,1611)
1611    format(' Precision cannot be characterized by an integer number of sig. digits;')
        if (radix .gt. fp1) goto 1625
        write(out,1620)
1620    format(' Logarithmic encoding (radix=1) has precision characterized solely by  u1 .')
        goto 1650
1625    continue
        write(out,1630)
1630    format(' but, by itself, this is a minor flaw.')
1640    continue
        write(out,1641) radix,precis
1641    format(' The number of significant digits of radix ',f4.0,' is ' , f6.2)
1650    continue
        if (ulppls*fp9*fp9*t8 .lt. fp1) go to 1670
!1660    continue
        sdefct=sdefct+1
        write(out,1665)
1665    format(' SERIOUS DEFECT: precision worse than  5 sig. dec. is usually inadequate.')
1670    continue
        return
        end subroutine radx
!#######################################################################
!---
        subroutine round (miles)
      implicit none
        integer miles
!
!
!
!       ... LOCAL VARIABLES
        real a, b1, q, s1, t, x, y, y1, y2, z
!       ... CONSTANTS
        real b2, t5, b9
!
        b2 = radix / fp2
        t5 = fp1 + half
        b9 = ((radix - fp1) - ulppls) + fp1
!
        write(out,2250)
2250    format(' Checking for rounding in multiply, divide and add/subtract:')
!2260    continue
        r1 = minus1
        r2 = minus1
        r3 = minus1
!               IS  RADIX  A POWER OF  2  OR  10 ?
!2270    continue
        a1 = fp2
2280    continue
        a = radix
2290    continue
        x = a
        a = a / a1
        if (aint(a) .eq. a) goto 2290
!2300    continue
        if (x .eq. fp1) goto 2340
!               RADIX  IS A POWER OF  A1; IF RADIX=1 THEN  A1=2.
!2310    continue
        if (a1 .gt. fp3) goto 2330
!2320    continue
        a1 = fp9 + fp1
        goto 2280
!               IS  RADIX  A POWER OF  10 ?
2330    a1 = radix
!               UNLESS  B  IS A POWER OF  A1  AND  A1 = 2 OR 10.
2340    continue
        a=fp1/a1
        x=a1
        y=a
2350    continue
        z=x*y-half
        if (z .eq. half) goto 2370
!2360    continue
        fails=fails+1
        write(out,2361) x, y, x, x
2361    format(' FAILURE:  1/',e13.5,' = ',e13.5,', and  ',e13.5,'*(1/',e13.5,') differs from  1.')
2370    continue
        if (x .eq. radix) goto 2390
!2380    continue
        x = radix
        y = fp1 / x
        goto 2350
2390    continue
        y2=fp1+ulppls
        y1=fp1-ulppls
        x=t5-ulppls
        y=t5+ulppls
        z=(x-ulppls)*y2
        t=y*y1
        z=z-x
        t=t-x
        x=x*y2
        y=(y+ulppls)*y1
        x=x-t5
        y=y-t5
        if (.not. ( x .eq. fp0 .and. y .eq. fp0 .and. z .eq. fp0 .and. t .le. fp0 )) goto 2460
!2400    continue
        x=(t5+ulppls)*y2
        y=t5-ulppls-ulppls
        z=t5+ulppls+ulppls
        t=(t5-ulppls)*y1
        x=x-(z+ulppls)
        sticky = y * y1
        s1 = z * y2
        t = t - y
        y = (ulppls-y) + sticky
        z = s1 - (z+ulppls+ulppls)
        sticky = (y2+ulppls) * y1
        y1 = y2 * y1
        sticky = sticky - y2
        y1 = y1 - half
!2410    continue
        if(x.eq.fp0.and.y.eq.fp0.and.z.eq.fp0.and.t.eq.fp0.and.sticky.eq.fp0.and.y1.eq.half) r1 = fp1
!2420    continue
        if(x+ulppls.eq.fp0.and.y.lt.fp0.and.z+ulppls.eq.fp0.and.t.lt.fp0.and.sticky+ulppls.eq.fp0.and.y1.lt.half)r1=fp0
!2430    continue
        if (r1 .eq. fp0) write(out,2431)
2431    format(' Multiplication appears to be chopped.')
!2440    continue
        if (r1 .eq. fp1) write(out,2441)
2441    format (' Multiplication appears to be correctly rounded.')
!2450    continue
        if (r1-mulgrd .eq. fp1) write(out,2451)
2451    format(' FAILURE: multiplication test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2460    continue
        if (r1 .eq. minus1) write(out,2461)
2461    format(' Multiplication is neither chopped nor correctly rounded.')
!2470    continue
        miles=45
!       ================================================================
!2480    continue
        y2=fp1+ulppls
        y1=fp1-ulppls
        z=t5+ulppls+ulppls
        x=z/y2
        t=t5-ulppls-ulppls
        y=(t-ulppls)/y1
        z=(z+ulppls)/y2
        x=x-t5
        y=y-t
        t=t/y1
        z=z-(t5+ulppls)
        t=(ulppls-t5)+t
        if ( x .gt. fp0 .or. y .gt. fp0 .or. z .gt. fp0 .or. t .gt. fp0)goto 2540
!2490    continue
        x=t5/y2
        y=t5-ulppls
        z=t5+ulppls
        x=x-y
        t=t5/y1
        y=y/y1
        t=t-(z+ulppls)
        y=y-z
        z=z/y2
        y1=(y2+ulppls)/y2
        z=z-t5
        y2=y1-y2
        y1=(onemin-ulpmin)/onemin
        if(x.eq.fp0.and.y.eq.fp0.and.z.eq.fp0.and.t.eq.fp0.and.y2.eq.fp0.and.y1-half.eq.onemin-half) r2=fp1
!2500    continue
        if(x.lt.fp0.and.y.lt.fp0.and.z.lt.fp0.and.t.lt.fp0.and.y2.lt.fp0.and.y1-half.lt.onemin-half) r2=fp0
!2510    continue
        if (r2 .eq. fp0) write(out,2511)
2511    format (' Division appears to be chopped.')
!2520    continue
        if (r2 .eq. fp1) write(out,2521)
2521    format (' Division appears to be correctly rounded.')
!2530    continue
        if (r2-divgrd .eq. fp1) write(out,2531)
2531    format(' FAILURE:  division test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2540    continue
        if (r2 .eq. minus1) write(out,2541)
2541    format (' Division is neither chopped nor correctly rounded.')
!
!       ================================================================
!
!2550    continue
        b1 = fp1 / radix
        if (b1 * radix - half .eq. half) goto 2580
!2560    continue
        fails=fails+1
        write(out,2570)
2570    format(' FAILURE:  radix * (1 / radix)  differs from  1.')
2580    continue
        miles=50
!
!       ================================================================
!
!2590    continue
        if ( (onemin + ulpmin) - half .eq. half         .and.  (b9 + ulppls) - fp1  .eq. radix - fp1 ) goto 2610
!2600    continue
        fails=fails+1
        write(out,2601)
2601    format(' FAILURE: incomplete carry-propagation in addition.')
2610    continue
        x = fp1 - ulpmin * ulpmin
        y = fp1 + ulppls * (fp1 - ulppls)
        z = onemin - half
        x = (x - half) - z
        y = y - fp1
!2620    continue
        if (x .ne. fp0 .or. y .ne. fp0) goto 2640
!2630    continue
        r3 = fp0
        write(out,2631)
2631    format (' Add/subtract appears to be chopped.')
2640    continue
        if (subgrd .eq. fp0) goto 2710
!2650    continue
        x = (half + ulppls) * ulppls
        y = (half - ulppls) * ulppls
        x = fp1 + x
        y = fp1 + y
        x = (fp1 + ulppls) - x
        y = fp1 - y
!2660    continue
        if (x .ne. fp0 .or. y .ne. fp0) goto 2710
!2670    continue
        x = (half + ulppls) * ulpmin
        y = (half - ulppls) * ulpmin
        x = fp1 - x
        y = fp1 - y
        x = onemin - x
        y = fp1 - y
!2680    continue
        if (x .ne. fp0 .or. y .ne. fp0) goto 2710
!2690    continue
        r3 = minus1 - fp2 * r3
        write(out,2691)
2691    format (' Add/subtract appears to be correctly rounded.')
!2700    continue
        if (r3 - subgrd .eq. fp1) write(out,2701)
2701    format(' FAILURE:  add/subtract test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2710    continue
        if (r3 .eq. minus1) write(out,2711)
2711    format(' Add/subtract neither chopped nor correctly rounded.')
!2720    continue
        s1 = fp1
        x = fp1+half*(fp1+half)
        y = (fp1+ulppls)*half
        z = x-y
        t = y-x
        sticky = z+t
!2730    continue
        if (sticky .eq. fp0) goto 2770
!2740    continue
        s1=fp0
        flaws=flaws+1
        write(out,2750) sticky
        write(out,2760) x, y
2750    format(' FLAW: nonzero  (x-y)+(y-x) = ',e16.8,' when')
2760    format('      x = ',e16.8,'  and  y = ',e16.8)
!
!       ================================================================
!
2770    continue
        sticky = fp0
        if(mulgrd*divgrd*subgrd.lt.fp1.or.r1.lt.fp1.or.r2.lt.fp1.or.r3.lt.fp1.or.aint(b2).ne.b2) goto 2890
        write(out,2780)
2780    format(' checking for sticky bit:')
!2790    continue
        x=(half+ulpmin)*ulppls
        y=half*ulppls
        z=fp1+y
        t=fp1+x
        if (z-fp1 .gt. fp0 .or. t-fp1 .lt. ulppls) goto 2890
!2800    continue
        z=t+y
        y=z-x
        if (z-t .lt. ulppls .or. y-t .ne. fp0) goto 2890
!2810    continue
        x=(half+ulpmin)*ulpmin
        y=half*ulpmin
        z=fp1-y
        t=fp1-x
        if (z-fp1 .ne. fp0 .or. t-onemin .ne. fp0) goto 2890
!2820    continue
        z=(half-ulpmin)*ulpmin
        t=onemin-z
        q=onemin-y
        if (t-onemin .ne. fp0 .or. (onemin-ulpmin)-q .ne. fp0) goto 2890
!2830    continue
        z = (fp1 + ulppls) * t5
        t = (t5 + ulppls) - z + ulppls
        x = fp1 + half / radix
        y = fp1 + radix * ulppls
        z = x * y
!2840    continue
        if (t .ne. fp0 .or. (x + radix * ulppls) - z .ne. fp0) goto 2890
!2850    continue
        if (radix .eq. fp2) goto 2870
!2860    continue
        x = fp2 + ulppls
        y = x / fp2
        if (y - fp1 .ne. fp0) goto 2890
2870    continue
        sticky = s1
!2880    continue
        if (sticky .eq. fp1) write(out,2881)
2881    format (' Sticky bit appears to be used correctly.')
2890    continue
        if (sticky .eq. fp0)  write(out,2891)
2891    format (' Sticky bit used incorrectly or not at all.')
!2900    continue
        if(mulgrd*divgrd*subgrd.eq.fp0.or.r1.lt.fp0.or.r2.lt.fp0.or.r3.lt.fp0) then
           flaws=flaws+1
           write(out,29001)
29001      format(' FLAW: lack(s) of guard digits or failure(s) to correctly round or chop',/, &
           & ' (noted above) count as one flaw in the final tally below.')
           end if
        return
        end subroutine round
!#######################################################################
!---
        subroutine smlint(miles,from)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!       TESTS ON SMALL INTEGERS
!
!
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
!
!
        integer ipartu
        integer miles
        integer from
!        ... INTEGER NUMBER IF MILESTONES REACHED
        character(len=8) :: charz
!        ... CHARACTER CONSTANT 'Z'
        real minone
!        ... TEMPORARY TO HOLD MINUS ONE
        real half
!        ... TEMPORARY TO HOLD ONE HALF
        real five
!        ... TEMPORARY TO HOLD FIVE
        real eight
!        ... TEMPORARY TO HOLD EIGHT
        real nine
!        ... TEMPORARY TO HOLD NINE
        real temp12
!        ... TEMPORARY TO HOLD 12
        real temp20
!        ... TEMPORARY TO HOLD 20
        real temp27
!        ... TEMPORARY TO HOLD 27
        real temp32
!        ... TEMPORARY TO HOLD 32
        real temp48
!        ... TEMPORARY TO HOLD 48
        real temp60
!        ... TEMPORARY TO HOLD 60
        real temp80
!        ... TEMPORARY TO HOLD 80
        real tmp240
!        ... TEMPORARY TO HOLD 240
        real temp
!        ... TEMPORARY VARIABLE TO HOLD VARIOUS VERY SHORT TERM VALUES
        real tempz
!        ... TEMPORARY VARIABLE TO HOLD A TEMP. PREVIOUSLY KNOWN AS Z
!        ... INITIALIZE SOME CONSTANTS
        data charz/'Z'/
        if (from .eq. 7) go to 951
        if (from .ne. 0) call badmil
        write(out,891)
891     format(' Program is now RUNNING tests on small integers:')
!
!       ... LOOK FOR SOME OBVIOUS MISTAKES
        if(0.0e0+0.0e0.eq.0.0e0.and.1.0e0-1.0e0.eq.0.0e0.and.1.0e0.gt.0.0e0.and.1.0e0+1.0e0.eq.2.0e0) goto 930
        fails=fails+1
        write(out,920)
920     format(' FAILURE: violation of  0+0=0  or  1-1=0  or  1>0  or 1+1 = 2.')
930     continue
        temp=0.0e0
        tempz=-temp
        if (tempz .eq. 0.0e0) goto 960
        fails=fails+1
        write(out,940)
        write(out,941)
940     format(' FAILURE: comparison alleges that minus zero, obtained by')
941     format(' setting  x = 0.  and then  z = -x ,  is nonzero!')
!        ... CALL TO ROUTINE TO CHECK FOR PARTIAL UNDERFLOW USING MINUS
!        ... ZERO DON'T REALLY HAVE INFO ON WHAT A UNIT IN THE LAST
!        ... PLACE IS OR WHAT THE RADIX IS SINCE WE HAVEN'T GOTTEN TO
!        ... SUCH SOPHISTICATED STUFF YET, SO PICK SOME ARBITRARY VALUES
!        ... FOR NOW TO GET US THROUGH THIS NEXT TEST.
!950     continue
        ulppls=.001
        radix=1
951     continue
        call partuf(tempz, charz, miles, ipartu, from .eq. 7)
!
!
960     continue
        if (4.0e0+2.0e0*(-2.0e0) .eq. 0.0e0 .and. (4.0e0-3.0e0)-1.0e0.eq. 0.0e0) goto 980
!970     continue
        fails=fails+1
        write(out,971)
971     format(' FAILURE: violation of   3+1 = 2*2 .')
!
980     continue
        minone=-1.0e0
        if(minone+1.0e0.eq.0.0e0.and.1.0e0+minone.eq.0.0e0.and.minone+abs(minone).eq.0.0e0.and.minone+minone*minone.eq.0.0e0)&
        & goto 1000
!990     continue
        fails=fails+1
        write(out,991)
991     format(' FAILURE: violation of   -1 + 1 = 0 .')
!
1000    continue
        half=1.0e0/2.0e0
        if (half+minone+half .eq. 0.0e0) goto 1020
!1010    continue
        fails=fails+1
        write(out,1011)
1011    format(' FAILURE: violation of   1/2 - 1 + 1/2 = 0 .')
1020    continue
        miles=10
!1100    continue
        nine=3.0e0*3.0e0
        temp27=nine*3.0e0
        eight=4.0e0+4.0e0
        temp32=4.0e0*eight
        if (temp32-temp27-4.0e0-1.0e0 .eq. 0.0e0) goto 1120
!1110    continue
        fails=fails+1
        write(out,1111)
1111    format(' FAILURE: violation of   32 - 27 - 4 - 1 = 0 .')
!
1120    continue
        five=4.0e0+1.0e0
        temp20=4.0e0*five
        temp12=3.0e0*4.0e0
        tmp240=temp20*temp12
!1130    continue
        temp80=tmp240/3.0e0
        temp60=tmp240/4.0e0
        temp48=tmp240/five
        temp80=temp80-4.0e0*temp20
        temp60=temp60-five*temp12
        temp48=temp48-4.0e0*temp12
        if ( temp80 .eq. 0.0e0 .and. temp60 .eq. 0.0e0 .and. temp48 .eq. 0.0e0 ) goto 1150
!1140    continue
        fails=fails+1
        write(out,1141)
1141    format(' FAILURE: violation of 240/3 = 80 or 240/4 = 60 or 240/5 = 48 .')
1150    continue
        if (fails .ne. 0) goto 1160
        write(out,1151)
1151    format (' -1, 0, 1/2 , 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K.')
1160    continue
        return
        end subroutine smlint
!#######################################################################
!---
        subroutine square (from, miles, numtry)
      implicit none
!
!
        integer from, miles, numtry
!       ... LOCAL VARIABLES
        real d, d4, e5, e6, e7, q, u, x, x1, x8, y, y1, y2, z, z1, z2
        real temp,temp1,temp2
        integer i, j
!       ... CONSTANTS
        real b2
        real b9, b1
!
!                       TRAP INTEGER OVERFLOWS
!                       OTHER EXCEPTIONS ARE TRAPPED BY DEFAULT
        b2 = radix / fp2
        b9 = ((radix - fp1) - ulppls) + fp1
        b1 = fp1 / radix
        if (from .eq. 79) go to 3058
        if (from .ne. 0) call badmil
!
!3020    continue
        write(out,3021)
3021    format(/' Running tests of square root...')
        miles = 79
        call logit(miles)
        x = fp0
        i = 0
3030    continue
        y =  sqrt(x)
        if (y .eq. x .and. y - half .eq. x - half) goto 3050
!3040    continue
        fails = fails + 1
        write(out,3041) x, y
3041    format(' FAILURE:  sqrt(',e9.1,'), miscalculated as ',e15.7)
3050    continue
        x = -x
        i = i + 1
        if (i .eq. 1) goto 3030
        go to 3060
3058    continue
        write(out,3059)
3059    format(' FAILURE:  sqrt(-0.0) stops the machine.')
        fails = fails + 1
        i = 2
3060    continue
        x = fp1
        i = i + 1
        if (i .eq. 3) goto 3030
!       ... RECORD MIN AND MAX ERRORS.
!3070    continue
        e5 = fp0
        e7 = fp0
!       ... TEST WHETHER  SQRT(X*X)  =  X
!3150    continue
        j = 0
        x = radix
        u = ulppls
        call sqrerr (x, u, j, e5, e7, .true.)
!3160    continue
        x = b1
        u = b1 * ulpmin
        call sqrerr (x, u, j, e5, e7, .true.)
!3170    continue
        x = w
        u = fp1
        call sqrerr (x, u, j, e5, e7, .true.)
!3180    continue
        x = ulpmin
        u = ulpmin * ulpmin
        call sqrerr (x, u, j, e5, e7, .true.)
!                       IF SQRT HAS SERIOUS DEFECTS, THEN PAUSE
!3190    continue
        if (j .eq. 0) goto 3210
!3200    continue
        sdefct = sdefct + j
        call page(miles)
!
!
!
3210    continue
        write(out,3211) numtry
3211    format(' Testing if  sqrt(x*x)  =  x  for  ',i4,' integers  x.')
!3220    continue
        j = 0
        x = fp2
        y = radix
        if (radix .eq. fp1) goto 3240
!                       LOOP TO DETERMINE ??
3230    continue
        x = y
        y = radix * x
        if (y - x .lt. numtry) goto 3230
3240    continue
        u = x*ulppls
!
        do 3260 i = 1,numtry
        x = x + fp1
        call sqrerr (x, u, j, e5, e7, .false.)
        if (j .gt. 0) goto 3280
3260    continue
!
        write(out,3270)
3270    format(' Found no discrepancies.')
        goto 3300
3280    continue
        defect = defect + j
!
!                       TEST SQRT FOR MONOTONICITY.
!
3300    continue
        i = -1
        x = b9
        y = radix
        z = radix + radix * ulppls
!
!                       LOOP
!
3310    continue
        i = i + 1
        x =  sqrt(x)
        q =  sqrt(y)
        z =  sqrt(z)
        if (.not. (x .gt. q .or. q .gt. z)) goto 3330
!3320    continue
        defect = defect + 1
        write(out,3321) y
3321    format(' DEFECT:  sqrt(x) is non - monotonic for  x  near ', e15.7)
        goto 3390
3330    continue
        q = aint(q + half)
        if (.not. (i .gt. 0 .or. q*q .eq. radix)) goto 3380
!3340    continue
        if (i .gt. 0) goto 3360
!3350    continue
        y = q
        x = y - ulppls
        z = y + ulppls
        goto 3310
!
3360    continue
        if (i .gt. 1) goto 3380
!3370    continue
        y = y * b1
        x = y - ulpmin
        z = y + ulpmin
        goto 3310
3380    continue
        write(out,3381)
3381    format(' Sqrt has passed a test for monotonicity.')
3390    continue
        miles = 80
!
!       TEST SQRT FOR ACCURACY  =====================================
!               E5 = MIN{ERROR + 1/2}
!               E7 = MAX{ERROR - 1/2}
!
!3400    continue
        e5 = e5 + half
        e7 = e7 - half
!3410    continue
        y = ( sqrt(fp1 + ulppls) - fp1)/ulppls
        e6 = (y - fp1) + ulppls/fp8
        if (e6 .gt. e7) e7 = e6
!3420    continue
        e6 = y + ulppls/fp8
        if (e6 .lt. e5) e5 = e6
!3430    continue
        y = (( sqrt(onemin) - ulppls) - (fp1 - ulppls))/ulpmin
        e6 = y + ulpmin/fp8
        if (e6 .gt. e7) e7 = e6
!3440    continue
        e6 = (y + fp1) + ulpmin/fp8
        if (e6 .lt. e5) e5 = e6
!3450    continue
        i = 0
        u = ulppls
        x = u
!
!                       LOOP
!
3460    continue
        i = i + 1
        y =  sqrt((x + ulpmin + x) + onemin)
        y = ((y - ulppls) - ((fp1 - ulppls) + x))/u
        z = ((ulpmin - x) + onemin)*half*x*x/u
        e6 = (y + half) + z
        if (e6 .lt. e5) e5 = e6
!3470    continue
        e6 = (y - half) + z
        if (e6 .gt. e7) e7 = e6
!3480    continue
        if (i .eq. 4) goto 3530
        if (i .eq. 2) goto 3520
!3500    continue
        x = u *  sign(fp1,x) * aint( fp8 / (fp9 *  sqrt(u)) )
        goto 3460
!
3520    continue
        u = ulpmin
        x = -u
        goto 3460
!
3530    continue
        miles = 85
        r4 = minus1
!3540    continue
        if (radix .eq. fp1) goto 3900
!3550    continue
        write(out,3551)
3551    format(' Testing whether  sqrt  is rounded or chopped:')
!3560    continue
        d = aint(half + radix ** (fp1 + precis - aint(precis)))
!
!       ...  =  B^(1 + FRACT)  IF  P  =  INTEGER  +  FRACT.
!
!3570    continue
        x = d / radix
        y = d / a1
        if (x .ne. aint(x) .or. y .ne. aint(y)) goto 3700
!3580    continue
        x = fp0
        z2 = x
        y = fp1
        y2 = y
        z1 = radix - fp1
        d4 = fp4 * d
!
!       LOOP: FOR  Y  =  1, 3, 5, ...  MAXIMIZE  Y2  =  Y*Y MOD 4D .
!
3600    continue
        if (.not. (y2 .gt. z2)) goto 3650
!3610    continue
        q = radix
        y1 = y
!                       IF NEW Y2 > OLD, CHECK THAT  GCD(Y,B)  =  1
3620    continue
        temp = half - q / y1
        temp1 = aint(temp)
        if (temp1 .gt. temp) temp1 = temp1 - fp1
        x1 =  abs(q + temp1 * y1)
        q = y1
        y1 = x1
        if (x1 .gt. fp0) goto 3620
!
!3630    continue
        if (q .gt. fp1) goto 3650
!                       IF GCD(Y,B)  .GT.  1 THEN SKIP OVER Y ;  ELSE
!3640    continue
        z2 = y2
        z = y
!                       AND GCD(Z, RADIX)  = 1
3650    continue
        y = y + fp2
        x = x + fp8
        y2 = y2 + x
        if (.not. (y2 .lt. d4)) y2 = y2 - d4
!                       =  Y*Y MOD 4D
!3660    continue
        if (y .lt. d) goto 3600
!                       ELSE  0 < Z < D  &  Z2 = Z^2 MOD 4D  IS MAXIMAL
!3670    continue
        x8 = d4 - z2
        q = (x8 + z * z) / d4
        x8 = x8 / fp8
        if (q .ne. aint(q)) goto 3700
3680    continue
        x = z1 * z
        x = x - aint(x / radix) * radix
        if (x .eq. fp1) goto 3800
!                       WITH  1  =  Z*Z1 MOD B
!3690    continue
        z1 = z1 - fp1
        if (z1 .gt. fp0) goto 3680
!                       ELSE FAILURE!
3700    continue
        fails = fails + 1
        write(out,3701) w
        write(out,3702)
3701    format(' FAILURE: anomalous arithmetic with integers < b^p  = ', e15.7)
3702    format ('         foils test whether  sqrt  rounds or chops.')
        goto 3940
!
!                       - B/2   <=   Z1 == 1/Z MOD B   <=   B/2
!
3800    continue
        if (z1 .gt. b2) z1 = z1 - radix
!
!                       LOOP UNTIL  D  =  B^(P - 1) .
!
3810    continue
        call newd (x, z1, q, z, d)
        if (ulppls * d .lt. onemin) goto 3810
!
!3820    continue
        if (d * radix - d .ne. w - d) goto 3700
!3830    continue
        z2 = d
        i = 0
!               COUNT HOW MANY TESTS OF  SQRT(D*X) = Y YIELD RESULTS.
!3840    continue
        y = d + (fp1 + z) * half
        x = d + z + q
        call sqrtdx (x, z2, i, d, y2, y, x8, e5, e7)
!3850    continue
        y = d + (fp1 - z) * half + d
        x = d - z + d
        x = x + q + x
        call sqrtdx (x, z2, i, d, y2, y, x8, e5, e7)
!3860    continue
        call newd (x, z1, q, z, d)
        if (d - z2 .ne. w - z2) goto 3700
!3870    continue
        y = (d - z2) + (z2 + (fp1 - z) * half)
        x = (d - z2) + (z2 - z + q)
        call sqrtdx (x, z2, i, d, y2, y, x8, e5, e7)
!3880    continue
        y = (fp1 + z) * half
        x = q
        call sqrtdx (x, z2, i, d, y2, y, x8, e5, e7)
!3890    continue
        if (i .eq. 0) goto 3700
3900    continue
        if (e5 .lt. 0 .or. e7 .gt. 0) goto 3920
!3910    R4 = FP1
        write(out,3911)
3911    format (' Square root appears to be correctly rounded.')
        return
!
3920    continue
        if (e7 + ulppls .gt. ulppls - half .or.  e5          .gt. half          .or.  e5 + radix  .lt. half) goto 3940
!3930    continue
        r4 = fp0
        write(out,3931)
3931    format (' Square root appears to be chopped.')
        return
3940    continue
        write(out,3941)
        temp=e5-half
        temp2=half+e7
        write(out,3942) temp, temp2
3941    format (' Square root is neither chopped nor correctly rounded.')
3942    format(' Observed errors run from  ',e15.7,'  to  ',e15.7,' ulps.')
        if (e7 - e5 .lt. radix * radix) return
!3950    continue
        sdefct = sdefct + 1
        write(out,3951)
3951    format(' SERIOUS DEFECT: sqrt gets too many last digits wrong.')
        return
        end subroutine square
!#######################################################################
!---
!       ____ SUBROUTINE TO ASSESS ERROR   SQRT(X*X) - X  IN ULPS. ____
!
        subroutine sqrerr (x, u, j, e5, e7, serous)
      implicit none
!
!
        integer j
        real x, u,    e5, e7
        logical serous
        real e6, b1
        b1 = 1.0 / radix
!3090    continue
        e6 = (( sqrt(x * x) - x * b1) - (x - x * b1)) / u
        if (e6 .eq. 0.0) return
!3100    continue
        if (e6 .lt. e5) e5 = e6
!3110    continue
        if (e6 .gt. e7) e7 = e6
!3120    continue
        j = j + 1
        if (.not. serous) write(out,31210) x*x, x, u*e6
        if (serous) write(out,31211) x*x, x, u*e6
31210   format (' DEFECT: sqrt(', e15.7,') - ',e15.7,'  =  ', e15.7)
31211   format (' SERIOUS DEFECT: sqrt(', e15.7,') - ',e15.7,'  =  ', e15.7)
        write(out,3122)
3122    format(' instead of correct value  0 .')
        return
        end subroutine sqrerr
!#######################################################################
!---
!       THIS SUBROUTINE PUTS  NEWD = B*D  AND
!                             NEWZ^2 MOD NEWD = Z^2 MOD D
!
        subroutine newd (x, z1, q, z, d)
      implicit none
!
!
        real x, z1, q, z, d
        real temp, temp1
!
!3720    continue
        x = z1 * q
        temp = half - x / radix
        temp1 = aint(temp)
        if (temp1 .gt. temp) temp1 = temp1 - fp1
        x = temp1 * radix + x
        q = (q - x*z) / radix + x * x * (d / radix)
        z = z - fp2 * x * d
        if (z .gt. fp0) goto 3740
!3730    continue
        z = -z
        z1 = -z1
3740    continue
        d = radix * d
        return
        end subroutine newd
!#######################################################################
!---
!       THIS SUBROUTINE TESTS IF
!                SQRT(D*X) =  SQRT((Y - 1/2)^2 + X8/2) ROUNDS TO  Y
!
        subroutine sqrtdx (x, z2, i, d, y2, y, x8, e5, e7)
      implicit none
!
!
        real x, z2, x2, d, y2, y, x8, e5, e7
        integer i
        real e6
!
!3760    continue
        if (x - radix .lt. z2 - radix .or. x - z2 .gt. w - z2) return
!3770    continue
        i = i + 1
        x2 =  sqrt(x * d)
        y2 = (x2 - z2) - (y - z2)
        x2 = x8/(y - half)
        x2 = x2 - half * x2 * x2
        e6 = (y2 + half) + (half - x2)
        if (e6 .lt. e5) e5 = e6
!3780    continue
        e6 = y2 - x2
        if (e6 .gt. e7) e7 = e6
!3790    continue
        return
        end subroutine sqrtdx
!#######################################################################
!---
        subroutine underf(miles, numtry, from)
      implicit none
        integer           miles, numtry, from
!        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
!        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
!        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
!
!
        integer accur, error, i, iq, partu
!        ACCUR  ... FLAG TO INDICATE SUCCESS/FAILURE OF ACCURACY TESTS
!        ERROR  ... COUNT OF ERRORS DETECTED TESTING POWERS.
!        I      ... SCRATCH FOR ENUMERATING CASES
!        IQ     ... TEMPORARY FOR HOLDING INTEGER EXPONENTS
!        PARTU  ... FLAG TO INDICATE THE DETECTION OF PARTIAL UNDERFLOW
!
        real c, epsp1, exp2, h, mindif
!        C      ... 1/(RADIX^LARGE_INTEGER)
!        EPSP1  ... EPSILON + 1 (1 + (SMALL INTEGER)* 1 ULP OF 1+...)
!        EXP2   ... VALUE OF E ^ 2
!        H      ... MIN (1/RADIX, 1/2)
!        MINDIF ... MINIMUM POSITIVE NUMBER FOUND BY ADDITION/SUBTR.
!
!        ... LOCAL VARIABLES
        real d, q, r, t0, v9, x, y, y1, y2, z, z9,temp
        character(len=8) :: charz0, chare0
!        CHARZ0 ... CHARACTER CONSTANT 'Z0'
!        CHARE0 ... CHARACTER CONSTANT 'E0'
        data charz0/ ' PHONY0'/,chare0/ ' MINPOS'/
!
        if(from .eq. 0 ) go to 4330
!          WE MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT
!       MUST READ THE LOG FILE BACK IN.
        read(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        read(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        if (from .eq. 105) go to 4390
        if (from .eq. 106) go to 4410
        if (from .eq. 107) go to 4450
        if (from .eq. 108) phony0 = 0
        if (from .eq. 108) go to 4522
        if (from .eq. 109) go to 4860
        if (from .eq. 115) go to 4631
        if (from .eq. 120) go to 4890
        if (from .eq. 121) go to 4941
        if (from .eq. 122) go to 5011
        if (from .eq. 123) go to 5160
        if (from .eq. 124) go to 5190
        if (from .eq. 125) go to 5175
        if (from .eq. 131) go to 53021
!               MAKES NO SENSE TO TALK ABOUT UNDERFLOW STICKING, SINCE
!               UNDERFLOW ABORTS THE PROGRAM....
        call badmil
4330    continue
        write(out,4335)
4335    format(' Seeking underflow threshold and min positive number:')
        miles = 105
        call logit(miles)
!4340    continue
        d = ulpmin
        if (precis .eq. aint(precis)) goto 4370
!4350    continue
        d = fp1 / radix
        x = precis
4360    continue
        d = d / radix
        x = x - fp1
        if (x .gt. fp0) go to 4360
!       IF NON-INTEGRAL PRECISION NOW HAVE D = 1 RIGHT SHIFTED BY PRECIS
!       DIGITS (IN BASE "RADIX")
!       IF INTEGRAL PRECISION, ULPMIN IS THIS NUMBER - PRE-COMPUTED.
4370    continue
        y = fp1
        z = d
!       ... D = A POWER OF  1/RADIX < 1
4380    continue
        c=y
        y=z
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        z=y*y
        if (y .gt. z .and. z+z .gt. z) go to 4380
!          MILESTONE 106
4390    continue
        miles = 106
        call logit(miles)
        y=c
        z=y*d
4400    continue
        c=y
        y=z
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        z=y*d
        if (y .gt. z .and. z+z .gt. z) go to 4400
!       MILESTONE 107
4410    continue
        miles = 107
        call logit(miles)
        h1=radix
        if (h1 .lt. fp2) h1=fp2
!4420    continue
        h=fp1/h1
!        ... 1/H1 = H = MIN{ 1/RADIX, 1/2 }
!4430    continue
        c1=fp1/c
        minpos=c
        z=minpos*h
!       ... C = 1/RADIX^(BIG INTEGER) << 1 << C1 = 1/C
4440    continue
        y=minpos
        minpos=z
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        z=minpos*h
        if (minpos .gt. z .and. z+z .gt. z) go to 4440
!       MILESTONE 108
4450    continue
        miles = 108
        call logit(miles)
        uflthr = minpos
        mindif=fp0
        q=fp0
        nulps=ulppls
        epsp1=fp1+nulps
        d=c*epsp1
        if (d .gt. c) go to 4490
!4460    continue
        nulps=radix*ulppls
        epsp1=fp1+nulps
        d=c*epsp1
        if (d .gt. c) go to 4490
        write(out,4470)
4470    format(' FAILURE: multiplication  gets too many last digits wrong.')
!       ... MULTIPLICATION IS TOO CRUDE
!4480    continue
        fails=fails+1
        t0 = minpos
        y1=fp0
        phony0 = z
        call page(miles)
        goto 4570
4490    continue
        t0=d
        phony0=t0*h
        uflthr=fp0
4500    continue
        y1=t0
        t0=phony0
        if (mindif+mindif .gt. mindif) go to 4520
!4510    continue
        y2 = t0 * h1
        mindif= abs(y1-y2)
        q=y1
        if (uflthr .eq. fp0 .and. y1 .ne. y2) uflthr=y1
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        miles = 108
        call logit(miles)
4520    continue
        phony0=t0*h
        if (t0 .gt. phony0 .and. phony0+phony0 .gt. phony0) go to 4500
4522    continue
        miles = 109
        call logit(miles)
!        ... NOW  1 >> C=1/RADIX^(INTEGER)  >=   Y    >   MINPOS=Y*H
!                  >~ Z:=MINPOS*H >~ 0 ,
!        ... AND  1 >> D=(1+NULPS)*C >= UFLTHR >= Q >= Y1 > T0:=Y1*H
!                  >~ PHONY0:=T0*H >~ 0 ,
!        ... AND  UFLTHR = D/RADIX^INTEGER  IS FIRST TO VIOLATE
!                  (UFLTHR*H)/H=UFLTHR , ELSE  UFLTHR=0 ;
!        ... AND  Q:=UFLTHR/RADIX^INTEGER  IS FIRST WITH  MINDIF :=
!                  |(Q*H)/H - Q| > 0, ELSE Q=Y1.
4570    continue
        if (phony0 .eq. fp0) go to 4860
!        ... TEST  PHONY0  FOR 'PHONEY-ZERO' VIOLATING  PHONY0<T0 OR
!                  PHONY0<PHONY0+PHONY0  ...
        write(out,4590)
4590    format(/)
        z=phony0
        if (phony0 .gt. fp0) goto 4620
!4600    continue
        fails=fails+1
        write(out,4601)
4601    format(' FAILURE:  positive expressions can underflow to an allegedly')
        write(out,4602)phony0
4602    format('          negative value z0 that prints out as ', e16.8)
        x=-phony0
        if (x .gt. 0) go to 4630
        write(out,4610)x
4610    format('          but  -z0, which should then be positive, isn''t; it prints out as', e16.8)
        goto 4630
4620    continue
        flaws=flaws+1
        write(out,4623)
4623    format(' FLAW: underflow can stick at an allegedly positive value  z0')
        write(out,4626)phony0
4626    format( '       that prints out as ', e16.8)
4630    continue
        miles=115
!       PARTUF INCLUDES CALL LOGIT(MILES)
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
4631    continue
        call partuf (z, charz0, miles, partu, from .eq. 115)
!       ... END OF TEST FOR 'PHONEY-ZERO'.
4860    continue
        miles=120
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
!       ==============================================================
!4870    continue
        if (c1*y .le. c1*y1) go to 4890
!       ... AS HAPPENS ON MOST MACHINES.
!4880    continue
        epsp1=h*epsp1
        minpos=t0
!       = LEAST POSITIVE NO. ON HP 3000
4890    continue
        if (mindif .eq. 0 .or. mindif .eq. minpos) go to 4930
!4900    continue
        if (mindif .lt. minpos) go to 4920
!4910    continue
        defect=defect+1
        write(out,4912)
4912    format(' DEFECT: differences underflow at a higher threshold than products.')
        goto 4930
4920    continue
        defect=defect+1
        write(out,4922)
4922    format(' DEFECT: products underflow at a higher threshold than differences.')
        if (phony0 .eq. fp0) minpos=mindif
!       ... BUT NOT IF PSEUDO-ZEROS EXIST.
4930    continue
        write(out,4935) minpos
4935    format(' Smallest strictly positive number found is  minpos  =', 1pe16.8)
!4940    continue
        z = minpos
        miles = 121
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
4941    continue
        call partuf (z, chare0, miles, partu, from .eq. 121)
        t0=minpos
        if (partu .eq. 1) t0=y
!       FOR CDC 7600
!4950    continue
        i=4
        if (mindif .eq. fp0) i=3
!       ...  I=1 IF MINDIF=0=UFLTHR  ,   I=2 IF MINDIF>0=UFLTHR  ,
!4960    continue
        if (uflthr .eq. fp0) i=i-2
!           ...  I=3 IF MINDIF=0<UFLTHR  ,   I=4 IF MINDIF>0 & UFLTHR>0
!4970    continue
        goto (4980, 5090, 5010, 5130),i
!       ... CASE STATEMENT
4980    continue
        uflthr=t0
        if (c1*q .eq. (c1*y)*epsp1) go to 5010
!4990    continue
        fails=fails+1
        uflthr=y
        write(out,4993)
4993    format(' FAILURE: either accuracy deteriorates as numbers approach a threshold')
        write(out,4996)uflthr,c
4996    format(' of ',e16.8,' coming down from  ',e16.8,',')
        write(out,4997)
4997    format(' or else  multiplication  gets too many last digits wrong.')
        call page(miles)
!
!        ___ TEST FOR  X-Z = 0  ALTHOUGH  X  .NE.  Z ___
!
5010    continue
        miles = 122
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        r =  sqrt(t0 / uflthr)
        go to 5012
5011    continue
        r = fp1
5012    continue
        if (r .gt. h) goto 5030
!5020    continue
        z=r*uflthr
        x=z*(fp1+r*h*(fp1+h))
        goto 5040
5030    continue
        z=uflthr
        x=z*(fp1+h*h*(fp1+h))
5040    continue
        miles = 123
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        if (x .eq. z .or. x-z .ne. fp0) go to 5160
!5050    continue
        flaws=flaws+1
        write(out,5055)x,z
5055    format(' FLAW:  x =',e16.8,' is unequal to  z =',e16.8,' ,')
        z9 = x - z
        write(out,5057) z9
5057    format(' yet  x-z  yields ', e15.7)
        write(out,5060)
5060    format(' Should this not signal underflow, this is a SERIOUS DEFECT that causes confusion when innocent statements like')
        write(out,5063)
5063    format(' if (x.eq.z) then ... else ... ( f(x)-f(z) )/(x-z) ...')
        write(out,5070)+(x/z-half)-half
5070    format(' encounter division by zero although actually  x/z = 1 +',e16.8)
        go to 5160
!       ... END OF TEST FOR  X-Z = 0  &  X  .NE.  Z
5090    continue
!        CASE I=2
!       UFLTHR = 0 < MINDIF  !
!5100    continue
        fails=fails+1
        write(out,5102)
5102    format(' FAILURE: underflow confuses comparison, which alleges that  q = y ')
        write(out,5104)
5104    format('         while denying that  |q-y| = 0 ; these values print out as')
        temp= abs(q-y2)
        write(out,5106)q,y2,temp
5106    format(' q =',e16.8,',  y =',e16.8,',  |q-y| =',e16.8,' ,')
        temp = q/y2 - half
        write(out,5110) temp - half
5110    format(' and  q/y = 1 + ',e16.8)
!5120    continue
        uflthr=q
        goto 5010
!        CASE I=4 ;  UFLTHR > 0  &  MINDIF > 0
5130    continue
!5140    continue
        if (.not. (q .eq. uflthr .and. mindif .eq. minpos .and. abs(uflthr-mindif/nulps) .le. mindif)) go to 5010
        write(out,5150)
        write(out,5155)
5150    format(' Underflow is gradual; it incurs  absolute error = ')
5155    format(' (roundoff in underflow threshold) < minpos.')
        y=minpos*c1
        y=y*(1.5e0+ulppls)
        x=c1*(fp1+ulppls)
        y=y/x
        ieee=0
        if (y .eq. minpos) ieee=1
!       ... IEEE=1 UNLESS GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED.)
5160    continue
        write(out,5163)uflthr
5163    format(' The  underflow threshold is ',e16.8,' , below which')
        write(out,5165)
5165    format(' calculation may suffer larger relative error than merely roundoff.')
!5170    continue
        miles = 124
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        y2=ulpmin*ulpmin
        y=y2*y2
        miles = 125
        write(3) accur, c, epsp1, error, exp2, h, i, iq, mindif, partu
        write(3) d, q, r, t0, v9, x, y, y1, y2, z, z9
        rewind 3
        call logit(miles)
        y2=y*ulpmin
5175    continue
        if (y2 .gt. uflthr) go to 5220
!5180    continue
        if (y .gt. minpos) go to 5200
5190    continue
        sdefct=sdefct+1
        i=4
        write(out,5195)
5195    format(' SERIOUS ')
        goto 5210
5200    continue
        defect=defect+1
        i=5
5210    continue
        write(out,5212)i
5212    format(' DEFECT:  range is too narrow;   ulpmin^',i5,'  underflows.')
5220    continue
        miles=130
        call page(miles)
!       ---- PAUSE ---- ==================================
!5230    continue
        y = -aint(half - 240.0 * alog(uflthr) / alog(h1)) / 240
        y2=y+y
        write(out,5240)h1,y
5240    format(' since underflow occurs below the threshold  ='/10x,'(',1pe16.8,')^(',1pe16.8,') ,')
        write(out,5245)h1,y2
5245    format(' only underflow should afflict the expression'/10x,'(',1pe16.8,')^(',1pe16.8,') ;')
        write(out,5247)
5247    format(' actually calculating it yields   ')
        miles = 131
        call logit(miles)
!5250    continue
        v9 = h1 ** (y2)
        write(out,5255) v9
5255    format(1x,e16.8)
        if (v9 .ge. fp0 .and. v9 .le. (radix+radix*nulps)*uflthr)go to 5270
!5260    continue
        sdefct=sdefct+1
        write(out,5263)
5263    format(' SERIOUS')
        goto 5300
5270    continue
        if (v9 .gt. uflthr*(fp1+nulps)) go to 5290
        write(out,5280)
5280    format(' This computed value is O.K.')
        goto 5310
5290    continue
        defect=defect+1
5300    continue
        write(out,5302)uflthr
5302    format(' DEFECT: this is not between 0 and  underflow threshold=',e16.8)
        go to 5310
53021   continue
        flaws = flaws + 1
        write(out,53022)
53022   format(' FLAW: underflow trap from ** .')
5310    continue
        miles=140
!       ======================================================
!       CALCULATE  EXP2 = EXP(2) = 7.389056099...
!5330    continue
        x=fp0
        i=2
        y=fp2*fp3
        q=fp0
        accur=0
5340    continue
        z=x
        i=i+1
        y=y/(i+i)
        r=y+q
        x=z+r
        q=(z-x)+r
        if (x .gt. z) go to 5340
!5350    continue
        z=(1.5e0+fp1/fp8)+x/(1.5e0 * fp32)
        x=z*z
        exp2=x*x
        x=onemin
        y=x-ulpmin
        write(out,5360) exp2
5360    format(' Testing  x^((x+1)/(x-1)) vs. exp(2) = ',e16.8,'  as  x-> 1.')
5370    continue
        do 5415 i=1 , numtry
!5380    continue
        z=x-(1/radix)
        z=(x+fp1)/(z-(fp1-(1/radix)))
        q=x**z-exp2
        if ( abs(q) .gt. 240. * ulppls) go to 5420
!5390    continue
        z=(y-x)*fp2+y
        x=y
        y=z
        z = fp1+(x-onemin)*(x-onemin)
        if (z .le. fp1) goto 5400
5415    continue
5400    continue
        if (x .gt. fp1) go to 5440
!5410    continue
        x=fp1+ulppls
        y=ulppls+ulppls+x
        goto 5370
5420    continue
        accur=1
        defect=defect+1
        temp=+(x-(1/radix))-(fp1-(1/radix))
        write(out,5425)temp,z
5425    format(' DEFECT:  calculated  (1 + (',e16.8,'))^(',e16.8,')')
        write(out,5427)q
5427    format('         differs from correct value by  ',e16.8)
        write(out,5430)
5430    format(' This much error may spoil financial calculations involving tiny interest rates.')
        goto 5450
5440    continue
        if (accur .eq. 0) write(out,5445)
5445    format(' Accuracy seems adequate.')
5450    continue
        miles=150
!       =======================================================
        write(out,5460)
5460    format(' Testing powers  z^q  at four nearly extreme values:')
        error=0
        z=a1
        iq =  int(half-alog(c) / alog(a1))
5470    continue
        x=c1
        call cmpxy(x,y,z,iq,error)
        iq=-iq
        x=c
        call cmpxy(x,y,z,iq,error)
        if (z .lt. fp1) go to 5490
!5480    continue
        z=1/a1
        goto 5470
5490    continue
        call prtcnt (error)
        call prt2(error,miles)
!       ... PRINT COUNT OF DISCREPANCIES.
!5500    continue
        miles=160
        return
        end subroutine underf
!#######################################################################
!---
        subroutine zeros(miles,from)
      implicit none
        integer miles
!
!
!       MILESTONE REACHED SO FAR
        integer from
!       MILESTONE TO RESTART AT
        real q9
!       TEMPORARY TO THROW RANDOM STUFF INTO.
        if(from .eq. 0) go to 6110
!       MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT.
!       DON'T NEED A LOG FILE FOR THIS ROUTINE.
        if (from .eq. 211) go to 7000
        if (from .eq. 212) go to 6130
        call badmil
6110    continue
        write(out,6120)
6120    format (/' What messages and/or values does division by zero produce?')
        write(out,6123)
6123    format(' About to compute 1/0...')
        miles = 211
        call logit(miles)
        q9 = fp1 / fp0
        write(out,6121) q9
6121    format(' Trying to compute  1/0  produces ', 1pe15.7)
7000    continue
        miles = 212
        write(out,6124)
6124    format(' About to compute 0/0...')
        call logit(miles)
        q9 = fp0 / fp0
        write(out,6122) q9
6122    format (' Trying to compute  0/0  produces ', 1pe15.7/)
6130    continue
        miles = 220
        return
        end subroutine zeros
!#######################################################################
      subroutine badmil
      implicit none
      write(out,11110)
11110 format(' Unrecognized restart milestone - PLEASE NOTIFY KARPINSKI !')
      stop
      end subroutine badmil
!#######################################################################
end subroutine sparanoia
!#######################################################################
