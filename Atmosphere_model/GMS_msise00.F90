

    !===========================================================!
    ! Modified by Bernard Gingold on 06/06/2018 16:57 PM GMT+2  !
    !===========================================================!


    SUBROUTINE GTD7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
!
!-----------------------------------------------------------------------
!     NRLMSISE-00
!     -----------
!        Neutral Atmosphere Empirical Model from the surface to lower
!        exosphere
!
!        NEW FEATURES:
!          *Extensive satellite drag database used in model generation
!          *Revised O2 (and O) in lower thermosphere
!          *Additional nonlinear solar activity term
!          *"ANOMALOUS OXYGEN" NUMBER DENSITY, OUTPUT D(9)
!           At high altitudes (> 500 km), hot atomic oxygen or ionized
!           oxygen can become appreciable for some ranges of subroutine
!           inputs, thereby affecting drag on satellites and debris. We
!           group these species under the term "anomalous oxygen," since
!           their individual variations are not presently separable with
!           the drag data used to define this model component.
!
!        SUBROUTINES FOR SPECIAL OUTPUTS:
!        
!        HIGH ALTITUDE DRAG: EFFECTIVE TOTAL MASS DENSITY 
!        (SUBROUTINE GTD7D, OUTPUT D(6))
!           For atmospheric drag calculations at altitudes above 500 km,
!           call SUBROUTINE GTD7D to compute the "effective total mass
!           density" by including contributions from "anomalous oxygen."
!           See "NOTES ON OUTPUT VARIABLES" below on D(6).
!
!        PRESSURE GRID (SUBROUTINE GHP7)
!          See subroutine GHP7 to specify outputs at a pressure level
!          rather than at an altitude.
!
!        OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
! 
!     INPUT VARIABLES:
!        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
!              (Year ignored in current model)
!        SEC - UT(SEC)
!        ALT - ALTITUDE(KM)
!        GLAT - GEODETIC LATITUDE(DEG)
!        GLONG - GEODETIC LONGITUDE(DEG)
!        STL - LOCAL APPARENT SOLAR TIME(HRS; see Note below)
!        F107A - 81 day AVERAGE OF F10.7 FLUX (centered on day DDD)
!        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
!        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
!           - ARRAY CONTAINING:
!             (1) DAILY AP
!             (2) 3 HR AP INDEX FOR CURRENT TIME
!             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
!             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
!             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
!             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
!                    TO CURRENT TIME
!             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
!                    TO CURRENT TIME
!        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
!                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
!                 MASS 17 IS Anomalous O ONLY.)
!
!     NOTES ON INPUT VARIABLES: 
!        UT, Local Time, and Longitude are used independently in the
!        model and are not of equal importance for every situation.  
!        For the most physically realistic calculation these three
!        variables should be consistent (STL=SEC/3600+GLONG/15).
!        The Equation of Time departures from the above formula
!        for apparent local time can be included if available but
!        are of minor importance.
!
!        F107 and F107A values used to generate the model correspond
!        to the 10.7 cm radio flux at the actual distance of the Earth
!        from the Sun rather than the radio flux at 1 AU. The following
!        site provides both classes of values:
!        ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_RADIO/FLUX/
!
!        F107, F107A, and AP effects are neither large nor well
!        established below 80 km and these parameters should be set to
!        150., 150., and 4. respectively.
!
!     OUTPUT VARIABLES:
!        D(1) - HE NUMBER DENSITY(CM-3)
!        D(2) - O NUMBER DENSITY(CM-3)
!        D(3) - N2 NUMBER DENSITY(CM-3)
!        D(4) - O2 NUMBER DENSITY(CM-3)
!        D(5) - AR NUMBER DENSITY(CM-3)                       
!        D(6) - TOTAL MASS DENSITY(GM/CM3)
!        D(7) - H NUMBER DENSITY(CM-3)
!        D(8) - N NUMBER DENSITY(CM-3)
!        D(9) - Anomalous oxygen NUMBER DENSITY(CM-3)
!        T(1) - EXOSPHERIC TEMPERATURE
!        T(2) - TEMPERATURE AT ALT
!
!     NOTES ON OUTPUT VARIABLES:
!        TO GET OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.) 
!
!        O, H, and N are set to zero below 72.5 km
!
!        T(1), Exospheric temperature, is set to global average for
!        altitudes below 120 km. The 120 km gradient is left at global
!        average value for altitudes below 72 km.
!
!        D(6), TOTAL MASS DENSITY, is NOT the same for subroutines GTD7 
!        and GTD7D
!
!          SUBROUTINE GTD7 -- D(6) is the sum of the mass densities of the
!          species labeled by indices 1-5 and 7-8 in output variable D.
!          This includes He, O, N2, O2, Ar, H, and N but does NOT include
!          anomalous oxygen (species index 9).
!
!          SUBROUTINE GTD7D -- D(6) is the "effective total mass density
!          for drag" and is the sum of the mass densities of all species
!          in this model, INCLUDING anomalous oxygen.
!        
!     SWITCHES: The following is for test and special purposes:
!          
!        TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW),
!        WHERE SW IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1. 
!        FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
!        FOR THE FOLLOWING VARIATIONS
!               1 - F10.7 EFFECT ON MEAN  2 - TIME INDEPENDENT
!               3 - SYMMETRICAL ANNUAL    4 - SYMMETRICAL SEMIANNUAL
!               5 - ASYMMETRICAL ANNUAL   6 - ASYMMETRICAL SEMIANNUAL
!               7 - DIURNAL               8 - SEMIDIURNAL
!               9 - DAILY AP             10 - ALL UT/LONG EFFECTS
!              11 - LONGITUDINAL         12 - UT AND MIXED UT/LONG
!              13 - MIXED AP/UT/LONG     14 - TERDIURNAL
!              15 - DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
!              16 - ALL TINF VAR         17 - ALL TLB VAR
!              18 - ALL TN1 VAR           19 - ALL S VAR
!              20 - ALL TN2 VAR           21 - ALL NLB VAR
!              22 - ALL TN3 VAR           23 - TURBO SCALE HEIGHT VAR
!
!        To get current values of SW: CALL TRETRV(SW)
!-----------------------------------------------------------------------
!
      use mod_kinds, only : dp, int4
      implicit none
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) ::  tlb, s, db04, db16, db28, db32, db40, db48, db01, &
       za, t0, z0, g0, rl, dd, db14, tr12
      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0 &
       ,G0,RL,DD,DB14,TR12
       !DIR$ ATTRIBUTES ALIGN : 64 :: /GTS3C/
      real(kind=dp) :: tn1, tn2, tn3, tgn1, tgn2, tgn3
      COMMON/MESO7/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)
       !DIR$ ATTRIBUTES ALIGN : 64 :: /MESO7/
      real(kind=dp) :: ptm, pdm
      COMMON/LOWER7/PTM(10),PDM(10,8)
       !DIR$ ATTRIBUTES ALIGN : 64 :: /LOWER7/
      real(kind=dp) :: pt, pd, ps, pdl, ptl, pma, sam
      COMMON/PARM7/PT(150),PD(150,9),PS(150),PDL(25,2),PTL(100,4), &
       PMA(100,10),SAM(100)
       !DIR$ ATTRIBUTES ALIGN : 64 :: /PARM7/
      character(len=4) :: isd, ist, nam
      COMMON/DATIM7/ISD(3),IST(2),NAM(2)

      character(len=4) ::  ISDATE, ISTIME, NAME
      COMMON/DATIME/ISDATE(3),ISTIME(2),NAME(2)

      integer(kind=int4) :: isw
      real(kind=dp) ::  sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW
       
      real(kind=dp) :: pavgm
      COMMON/MAVG7/PAVGM(10)

      real(kind=dp) :: dm04, dm16, dm28, dm32, dm40, dm01, dm14
      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14

      real(kind=dp) :: gsurf, re
      COMMON/PARMB/GSURF,RE

      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) :: iyd, mass
      real(kind=dp) ::  sec, alt, glat, glong, stl, f107a, f107
      real(kind=dp) ::  D(9),T(2),AP(7)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) ::  i, j, mssx
      real(kind=dp) ::  dmc, dm28m, tz, dz28, dmr, v1
      real(kind=dp) ::  altt, xlat, xmm
      real(kind=dp) ::  DS(9),TS(2)

      integer(kind=int4) ::  mn3
      real(kind=dp) ::  ZN3(5)
      DATA MN3/5/,ZN3/32.5_dp,20._dp,15._dp,10._rdp,0._dp/

      integer(kind=int4) ::  mn2
      real(kind=dp) ZN2(4)
      DATA MN2/4/,ZN2/72.5_dp,55._dp,45._dp,32.5_dp/

      integer(kind=dp) ::  mssl
      real(kind=dp) ::  zmix, alast
      DATA ZMIX/62.5_dp/,ALAST/99999._dp/,MSSL/-999/

      real(kind=dp) SV(25)
      DATA SV/25*1._dp/

      SAVE
!
!-------------------------External Functions----------------------------
!
      EXTERNAL GTD7BK

      real(kind=dp) densm, glob7s, vtst7
      external densm, glob7s, vtst7
!
!-----------------------------------------------------------------------
!
      IF(ISW.NE.64999) CALL TSELEC(SV)
!      Put identification data into common/datime/
      DO 1 I=1,3
        ISDATE(I)=ISD(I)
    1 CONTINUE
      DO 2 I=1,2
        ISTIME(I)=IST(I)
        NAME(I)=NAM(I)
    2 CONTINUE
!
!        Test for changed input
      V1=VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,1)
!       Latitude variation of gravity (none for SW(2)=0)
      XLAT=GLAT
      IF(SW(2).EQ.0) XLAT=45._dp
      CALL GLATF(XLAT,GSURF,RE)
!
      XMM=PDM(5,3)
!
!       THERMOSPHERE/MESOSPHERE (above ZN2(1))
      ALTT=MAX(ALT,ZN2(1))
      MSSX=MASS
!       Only calculate N2 in thermosphere if alt in mixed region
      IF(ALT.LT.ZMIX.AND.MASS.GT.0) MSSX=28
!       Only calculate thermosphere if input parameters changed
!         or altitude above ZN2(1) in mesosphere
      IF(V1.EQ.1._r8.OR.ALT.GT.ZN2(1).OR.ALAST.GT.ZN2(1).OR.MSSX.NE.MSSL) &
       THEN
        CALL GTS7(IYD,SEC,ALTT,GLAT,GLONG,STL,F107A,F107,AP,MSSX,DS,TS)
        DM28M=DM28
!         metric adjustment
        IF(IMR.EQ.1) DM28M=DM28*1.E6_dp
        MSSL=MSSX
      ENDIF
      T(1)=TS(1)
      T(2)=TS(2)
      IF(ALT.GE.ZN2(1)) THEN
        DO 5 J=1,9
          D(J)=DS(J)
    5   CONTINUE
        GOTO 10
      ENDIF
!
!       LOWER MESOSPHERE/UPPER STRATOSPHERE [between ZN3(1) and ZN2(1)]
!         Temperature at nodes and gradients at end nodes
!         Inverse temperature a linear function of spherical harmonics
!         Only calculate nodes if input changed
       IF(V1.EQ.1._r8.OR.ALAST.GE.ZN2(1)) THEN
        TGN2(1)=TGN1(2)
        TN2(1)=TN1(5)
        TN2(2)=PMA(1,1)*PAVGM(1)/(1._dp-SW(20)*GLOB7S(PMA(1,1)))
        TN2(3)=PMA(1,2)*PAVGM(2)/(1._dp-SW(20)*GLOB7S(PMA(1,2)))
        TN2(4)=PMA(1,3)*PAVGM(3)/(1._dp-SW(20)*SW(22)*GLOB7S(PMA(1,3)))
        TGN2(2)=PAVGM(9)*PMA(1,10)*(1._dp+SW(20)*SW(22)*GLOB7S(PMA(1,10))) &
        *TN2(4)*TN2(4)/(PMA(1,3)*PAVGM(3))**2
        TN3(1)=TN2(4)
       ENDIF
       IF(ALT.GE.ZN3(1)) GOTO 6
!
!       LOWER STRATOSPHERE AND TROPOSPHERE [below ZN3(1)]
!         Temperature at nodes and gradients at end nodes
!         Inverse temperature a linear function of spherical harmonics
!         Only calculate nodes if input changed
        IF(V1.EQ.1._r8.OR.ALAST.GE.ZN3(1)) THEN
         TGN3(1)=TGN2(2)
         TN3(2)=PMA(1,4)*PAVGM(4)/(1._dp-SW(22)*GLOB7S(PMA(1,4)))
         TN3(3)=PMA(1,5)*PAVGM(5)/(1._dp-SW(22)*GLOB7S(PMA(1,5)))
         TN3(4)=PMA(1,6)*PAVGM(6)/(1._dp-SW(22)*GLOB7S(PMA(1,6)))
         TN3(5)=PMA(1,7)*PAVGM(7)/(1._dp-SW(22)*GLOB7S(PMA(1,7)))
         TGN3(2)=PMA(1,8)*PAVGM(8)*(1._dp+SW(22)*GLOB7S(PMA(1,8))) &
         *TN3(5)*TN3(5)/(PMA(1,7)*PAVGM(7))**2
        ENDIF
    6   CONTINUE
        IF(MASS.EQ.0) GOTO 50
!          LINEAR TRANSITION TO FULL MIXING BELOW ZN2(1)
        DMC=0
        IF(ALT.GT.ZMIX) DMC=1._dp-(ZN2(1)-ALT)/(ZN2(1)-ZMIX)
        DZ28=DS(3)
!      ***** N2 DENSITY ****
        DMR=DS(3)/DM28M-1._dp
        D(3)=DENSM(ALT,DM28M,XMM,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)
        D(3)=D(3)*(1._dp+DMR*DMC)
!      ***** HE DENSITY ****
        D(1)=0
        IF(MASS.NE.4.AND.MASS.NE.48) GOTO 204
          DMR=DS(1)/(DZ28*PDM(2,1))-1._dp
          D(1)=D(3)*PDM(2,1)*(1._dp+DMR*DMC)
  204   CONTINUE
!      **** O DENSITY ****
        D(2)=0
        D(9)=0
  216   CONTINUE
!      ***** O2 DENSITY ****
        D(4)=0
        IF(MASS.NE.32.AND.MASS.NE.48) GOTO 232
          DMR=DS(4)/(DZ28*PDM(2,4))-1._r8
          D(4)=D(3)*PDM(2,4)*(1._dp+DMR*DMC)
  232   CONTINUE
!      ***** AR DENSITY ****
        D(5)=0
        IF(MASS.NE.40.AND.MASS.NE.48) GOTO 240
          DMR=DS(5)/(DZ28*PDM(2,5))-1._r8
          D(5)=D(3)*PDM(2,5)*(1._dp+DMR*DMC)
  240   CONTINUE
!      ***** HYDROGEN DENSITY ****
        D(7)=0
!      ***** ATOMIC NITROGEN DENSITY ****
        D(8)=0
!
!       TOTAL MASS DENSITY
!
        IF(MASS.EQ.48) THEN
         D(6) = 1.66E-24_dp*(4._dp*D(1)+16._dp*D(2)+28._dp*D(3)+32._dp*D(4)+40._dp*D(5)+ &
             D(7)+14._dp*D(8))  
         IF(IMR.EQ.1) D(6)=D(6)/1000._dp
         ENDIF
         T(2)=TZ
   10 CONTINUE
      GOTO 90
   50 CONTINUE
      DD=DENSM(ALT,1._dp,0._dp,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)                
      T(2)=TZ
   90 CONTINUE
      ALAST=ALT
      RETURN
      END SUBROUTINE GTD7

!================================================================================================

      SUBROUTINE GTD7D(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS, &
       D,T)
!
!-----------------------------------------------------------------------
!     NRLMSISE-00
!     -----------
!        This subroutine provides Effective Total Mass Density for
!        output D(6) which includes contributions from "anomalous
!        oxygen" which can affect satellite drag above 500 km.  This
!        subroutine is part of the distribution package for the 
!        Neutral Atmosphere Empirical Model from the surface to lower
!        exosphere.  See subroutine GTD7 for more extensive comments.
!
!     INPUT VARIABLES:
!        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
!              (Year ignored in current model)
!        SEC - UT(SEC)
!        ALT - ALTITUDE(KM)
!        GLAT - GEODETIC LATITUDE(DEG)
!        GLONG - GEODETIC LONGITUDE(DEG)
!        STL - LOCAL APPARENT SOLAR TIME(HRS; see Note below)
!        F107A - 81 day AVERAGE OF F10.7 FLUX (centered on day DDD)
!        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
!        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
!           - ARRAY CONTAINING:
!             (1) DAILY AP
!             (2) 3 HR AP INDEX FOR CURRENT TIME
!             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
!             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
!             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
!             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
!                    TO CURRENT TIME
!             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
!                    TO CURRENT TIME
!        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
!                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
!                 MASS 17 IS Anomalous O ONLY.)
!
!     NOTES ON INPUT VARIABLES: 
!        UT, Local Time, and Longitude are used independently in the
!        model and are not of equal importance for every situation.  
!        For the most physically realistic calculation these three
!        variables should be consistent (STL=SEC/3600+GLONG/15).
!        The Equation of Time departures from the above formula
!        for apparent local time can be included if available but
!        are of minor importance.
!
!        F107 and F107A values used to generate the model correspond
!        to the 10.7 cm radio flux at the actual distance of the Earth
!        from the Sun rather than the radio flux at 1 AU.
!
!     OUTPUT VARIABLES:
!        D(1) - HE NUMBER DENSITY(CM-3)
!        D(2) - O NUMBER DENSITY(CM-3)
!        D(3) - N2 NUMBER DENSITY(CM-3)
!        D(4) - O2 NUMBER DENSITY(CM-3)
!        D(5) - AR NUMBER DENSITY(CM-3)                       
!        D(6) - TOTAL MASS DENSITY(GM/CM3) [includes anomalous oxygen]
!        D(7) - H NUMBER DENSITY(CM-3)
!        D(8) - N NUMBER DENSITY(CM-3)
!        D(9) - Anomalous oxygen NUMBER DENSITY(CM-3)
!        T(1) - EXOSPHERIC TEMPERATURE
!        T(2) - TEMPERATURE AT ALT
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-------------------------------Commons---------------------------------
!
      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) ::  iyd, mass
      real(kind=dp) ::  sec, alt, glat, glong, stl, f107a, f107
      real(kind=dp) ::  D(9),T(2),AP(7)
!
!---------------------------Local variables-----------------------------
!
      real(kind=dp) ::  DS(9),TS(2)
!
!-----------------------------------------------------------------------
!
      CALL GTD7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
!       TOTAL MASS DENSITY
!
        IF(MASS.EQ.48) THEN
         D(6) = 1.66E-24_dp*(4._dp*D(1)+16._dp*D(2)+28._dp*D(3)+32._dp*D(4)+40._dp*D(5)+ &
             D(7)+14._dp*D(8)+16._dp*D(9))  
         IF(IMR.EQ.1) D(6)=D(6)/1000._dp
         ENDIF
      RETURN
      END SUBROUTINE GTD7D

!================================================================================================

      SUBROUTINE GHP7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP, &
        D,T,PRESS)
!         
!-----------------------------------------------------------------------
!       FIND ALTITUDE OF PRESSURE SURFACE (PRESS) FROM GTD7
!     INPUT:
!        IYD - YEAR AND DAY AS YYDDD
!        SEC - UT(SEC)
!        GLAT - GEODETIC LATITUDE(DEG)
!        GLONG - GEODETIC LONGITUDE(DEG)
!        STL - LOCAL APPARENT SOLAR TIME(HRS)
!        F107A - 3 MONTH AVERAGE OF F10.7 FLUX
!        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
!        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
!           - ARRAY CONTAINING:
!             (1) DAILY AP
!             (2) 3 HR AP INDEX FOR CURRENT TIME
!             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
!             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
!             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
!             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
!                    TO CURRENT TIME
!             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 59 HRS PRIOR
!                    TO CURRENT TIME
!        PRESS - PRESSURE LEVEL(MB)
!     OUTPUT:
!        ALT - ALTITUDE(KM) 
!        D(1) - HE NUMBER DENSITY(CM-3)
!        D(2) - O NUMBER DENSITY(CM-3)
!        D(3) - N2 NUMBER DENSITY(CM-3)
!        D(4) - O2 NUMBER DENSITY(CM-3)
!        D(5) - AR NUMBER DENSITY(CM-3)
!        D(6) - TOTAL MASS DENSITY(GM/CM3)
!        D(7) - H NUMBER DENSITY(CM-3)
!        D(8) - N NUMBER DENSITY(CM-3)
!        D(9) - HOT O NUMBER DENSITY(CM-3)
!        T(1) - EXOSPHERIC TEMPERATURE
!        T(2) - TEMPERATURE AT ALT
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      !use cam_logfile,  only : iulog
      implicit none
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) ::  gsurf, re
      COMMON/PARMB/GSURF,RE

      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) ::  iyd
      real(kind=dp) ::  sec, alt, glat, glong, stl, f107a, f107, press
      real(kind=dp) ::  D(9),T(2),AP(7)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) ::  l, iday
      real(kind=dp) ::  p, xn, diff, sh, g, xm, z, zi, pl, cl, ca, cd, cl2

      real(kind=dp) ::  bm, rgas
      DATA BM/1.3806E-19_dp/,RGAS/831.4_dp/

      integer(kind=int4) ::  ltest
      real(kind=dp) ::  test
      DATA TEST/.00043_dp/,LTEST/12/

      SAVE
!
!-----------------------------------------------------------------------
!
      PL=LOG10(PRESS)
!      Initial altitude estimate
      IF(PL.GE.-5._dp) THEN
         IF(PL.GT.2.5_dp) ZI=18.06_dp*(3.00_dp-PL)
         IF(PL.GT..75_dp.AND.PL.LE.2.5_dp) ZI=14.98_dp*(3.08_dp-PL)
         IF(PL.GT.-1._dp.AND.PL.LE..75_dp) ZI=17.8_dp*(2.72_dp-PL)
         IF(PL.GT.-2._dp.AND.PL.LE.-1._dp) ZI=14.28_dp*(3.64_dp-PL)
         IF(PL.GT.-4._dp.AND.PL.LE.-2._dp) ZI=12.72_dp*(4.32_dp-PL)
         IF(PL.LE.-4._dp) ZI=25.3_dp*(.11_dp-PL)
         IDAY=MOD(IYD,1000)
         CL=GLAT/90._dp
         CL2=CL*CL
         IF(IDAY.LT.182) CD=1._dp-IDAY/91.25_dp
         IF(IDAY.GE.182) CD=IDAY/91.25_r8-3._dp
         CA=0
         IF(PL.GT.-1.11_dp.AND.PL.LE.-.23_dp) CA=1.0_dp
         IF(PL.GT.-.23_dp) CA=(2.79_dp-PL)/(2.79_dp+.23_dp)
         IF(PL.LE.-1.11_dp.AND.PL.GT.-3._dp) CA=(-2.93_dp-PL)/(-2.93_dp+1.11_dp)
         Z=ZI-4.87_dp*CL*CD*CA-1.64_dp*CL2*CA+.31_dp*CA*CL
      ENDIF
      IF(PL.LT.-5._dp) Z=22._dp*(PL+4._dp)**2+110
!      ITERATION LOOP
      L=0
   10 CONTINUE
        L=L+1
        CALL GTD7(IYD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,48,D,T)
        XN=D(1)+D(2)+D(3)+D(4)+D(5)+D(7)+D(8)
        P=BM*XN*T(2)
        IF(IMR.EQ.1) P=P*1.E-6_dp
        DIFF=PL-LOG10(P)
        IF(ABS(DIFF).LT.TEST .OR. L.EQ.LTEST) GOTO 20
        XM=D(6)/XN/1.66E-24_dp
        IF(IMR.EQ.1) XM = XM*1.E3_dp
        G=GSURF/(1._dp+Z/RE)**2
        SH=RGAS*T(2)/(XM*G)
!         New altitude estimate using scale height
        IF(L.LT.6) THEN
          Z=Z-SH*DIFF*2.302_dp
        ELSE
          Z=Z-SH*DIFF
        ENDIF
        GOTO 10
   20 CONTINUE
      IF(L.EQ.LTEST) write(6,100) PRESS,DIFF
  100 FORMAT(1X,29HGHP7 NOT CONVERGING FOR PRESS, 1PE12.2,E12.2)
      ALT=Z
      RETURN
      END SUBROUTINE GHP7

!================================================================================================

      SUBROUTINE GLATF(LAT,GV,REFF)
!         
!-----------------------------------------------------------------------
!      CALCULATE LATITUDE VARIABLE GRAVITY (GV) AND EFFECTIVE
!      RADIUS (REFF)
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!------------------------------Arguments--------------------------------
!
      REAL(kind=dp) ::  LAT
      real(kind=dp) ::  gv, reff
!
!---------------------------Local variables-----------------------------
!
      real(kind=dp) ::  c2

      real(kind=dp) ::  dgtr
      DATA DGTR/1.74533E-2_dp/

      SAVE
!
!-----------------------------------------------------------------------
!
      C2 = COS(2._dp*DGTR*LAT)
      GV = 980.616_dp*(1._dp-.0026373_dp*C2)
      REFF = 2._dp*GV/(3.085462E-6_dp + 2.27E-9_dp*C2)*1.E-5_dp
      RETURN
      END SUBROUTINE GLATF

!================================================================================================

      FUNCTION VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,IC)
!         
!-----------------------------------------------------------------------
!       Test if geophysical variables or switches changed and save
!       Return 0 if unchanged and 1 if changed
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) ::  vtst7
!
!-------------------------------Commons---------------------------------
!
      integer(kind=int4) ::  isw
      real(kind=dp) ::  sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) ::  iyd, ic
      real(kind=dp) ::  sec, glat, glong, stl, f107a, f107
      real(kind=dp) ::  AP(7)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) ::  i

      integer(kind=int4)  IYDL(2)
      real(kind=dp) SECL(2),GLATL(2),GLL(2)
      DATA IYDL/2*-999/,SECL/2*-999._dp/,GLATL/2*-999._dp/,GLL/2*-999._dp/

      real(kind=dp) STLL(2),FAL(2),FL(2),APL(7,2)
      DATA STLL/2*-999._dp/,FAL/2*-999._dp/,FL/2*-999._dp/,APL/14*-999._dp/

      real(kind=dp) SWL(25,2),SWCL(25,2)
      DATA SWL/50*-999._dp/,SWCL/50*-999._dp/

      SAVE
!
!-----------------------------------------------------------------------
!
      VTST7=0
      IF(IYD.NE.IYDL(IC)) GOTO 10
      IF(SEC.NE.SECL(IC)) GOTO 10
      IF(GLAT.NE.GLATL(IC)) GOTO 10
      IF(GLONG.NE.GLL(IC)) GOTO 10
      IF(STL.NE.STLL(IC)) GOTO 10
      IF(F107A.NE.FAL(IC)) GOTO 10
      IF(F107.NE.FL(IC)) GOTO 10
      DO 5 I=1,7
        IF(AP(I).NE.APL(I,IC)) GOTO 10
    5 CONTINUE
      DO 7 I=1,25
        IF(SW(I).NE.SWL(I,IC)) GOTO 10
        IF(SWC(I).NE.SWCL(I,IC)) GOTO 10
    7 CONTINUE
      GOTO 20
   10 CONTINUE
      VTST7=1
      IYDL(IC)=IYD
      SECL(IC)=SEC
      GLATL(IC)=GLAT
      GLL(IC)=GLONG
      STLL(IC)=STL
      FAL(IC)=F107A
      FL(IC)=F107
      DO 15 I=1,7
        APL(I,IC)=AP(I)
   15 CONTINUE
      DO 16 I=1,25
        SWL(I,IC)=SW(I)
        SWCL(I,IC)=SWC(I)
   16 CONTINUE
   20 CONTINUE
      RETURN
      END FUNCTION VTST7

!================================================================================================

      SUBROUTINE GTS7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
!         
!-----------------------------------------------------------------------
!     Thermospheric portion of NRLMSISE-00
!     See GTD7 for more extensive comments
!
!        OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
! 
!     INPUT VARIABLES:
!        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
!              (Year ignored in current model)
!        SEC - UT(SEC)
!        ALT - ALTITUDE(KM) (>72.5 km)
!        GLAT - GEODETIC LATITUDE(DEG)
!        GLONG - GEODETIC LONGITUDE(DEG)
!        STL - LOCAL APPARENT SOLAR TIME(HRS; see Note below)
!        F107A - 81 day AVERAGE OF F10.7 FLUX (centered on day DDD)
!        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
!        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
!           - ARRAY CONTAINING:
!             (1) DAILY AP
!             (2) 3 HR AP INDEX FOR CURRENT TIME
!             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
!             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
!             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
!             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
!                    TO CURRENT TIME
!             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
!                    TO CURRENT TIME
!        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
!                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
!                 MASS 17 IS Anomalous O ONLY.)
!
!     NOTES ON INPUT VARIABLES: 
!        UT, Local Time, and Longitude are used independently in the
!        model and are not of equal importance for every situation.  
!        For the most physically realistic calculation these three
!        variables should be consistent (STL=SEC/3600+GLONG/15).
!        The Equation of Time departures from the above formula
!        for apparent local time can be included if available but
!        are of minor importance.
!
!        F107 and F107A values used to generate the model correspond
!        to the 10.7 cm radio flux at the actual distance of the Earth
!        from the Sun rather than the radio flux at 1 AU. The following
!        site provides both classes of values:
!        ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_RADIO/FLUX/
!
!        F107, F107A, and AP effects are neither large nor well
!        established below 80 km and these parameters should be set to
!        150., 150., and 4. respectively.
!
!     OUTPUT VARIABLES:
!        D(1) - HE NUMBER DENSITY(CM-3)
!        D(2) - O NUMBER DENSITY(CM-3)
!        D(3) - N2 NUMBER DENSITY(CM-3)
!        D(4) - O2 NUMBER DENSITY(CM-3)
!        D(5) - AR NUMBER DENSITY(CM-3)                       
!        D(6) - TOTAL MASS DENSITY(GM/CM3) [Anomalous O NOT included]
!        D(7) - H NUMBER DENSITY(CM-3)
!        D(8) - N NUMBER DENSITY(CM-3)
!        D(9) - Anomalous oxygen NUMBER DENSITY(CM-3)
!        T(1) - EXOSPHERIC TEMPERATURE
!        T(2) - TEMPERATURE AT ALT
!-----------------------------------------------------------------------
!
      
      use mod_kinds, only : int4, dp
      implicit none
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) ::  tlb, s, db04, db16, db28, db32, db40, db48, db01
      real(kind=dp) ::  za, t0, z0, g0, rl, dd, db14, tr12
      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0 &
       ,G0,RL,DD,DB14,TR12

      real(kind=dp) ::  tn1, tn2, tn3, tgn1, tgn2, tgn3
      COMMON/MESO7/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)

      real(kind=dp) ptm, pdm
      COMMON/LOWER7/PTM(10),PDM(10,8)

      real(kind=dp)  pt, pd, ps, pdl, ptl, pma, sam
      COMMON/PARM7/PT(150),PD(150,9),PS(150),PDL(25,2),PTL(100,4), &
       PMA(100,10),SAM(100)

      integer(kind=int4) ::  isw
      real(kind=dp) sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW

      real(kind=dp) tinfg, gb, rout, tt
      COMMON/TTEST/TINFG,GB,ROUT,TT(15)

      real(kind=dp) dm04, dm16, dm28, dm32, dm40, dm01, dm14
      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14

      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
!
!------------------------------Arguments--------------------------------
!
      integer iyd, mass
      real(kind=dp) sec, alt, glat, glong, stl, f107a, f107
      real(kind=dp) D(9),T(2),AP(*)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=dp) ::  i, j
      real(kind=dp) ::  zh01, b01, g1, hc40, zc40, hcc01, zcc01, zc01
      real(kind=dp) ::  zhm01, hc01, hcc32, zcc32, zc32, zhm32, hc32
      real(kind=dp) ::  b40, zhm40, zh40, rc32, g40, t2, zsht, tho
      real(kind=dp) ::  g16h, db16h, ddum, zmho, zsho, b14, zhm14
      real(kind=dp) ::  zh14, rc01, g14, zcc14, rc14, hcc14, hc14, zc14
      real(kind=dp) ::  b32, xmm, z, zhf, g28, day, xmd, b28, zhm28
      real(kind=dp) ::  zh28, v2
      real(kind=dp) ::  tinf, yrd, hc16, zc16, zhm16, zh16, b16
      real(kind=dp) ::  g32, zh32, rc16, hcc16, zcc16, zh04, b04, g4, tz
      real(kind=dp) ::  g16, hc04, zhm04, zc04

      integer(kind=int4) mt(11)
      DATA MT/48,0,4,16,28,32,40,1,49,14,17/

      real(kind=dp) altl(8)
      DATA ALTL/200._dp,300._dp,160._dp,250._dp,240._dp,450._dp,320._dp,450._dp/

      integer mn1
      real(kind=dp) ZN1(5)
      DATA MN1/5/,ZN1/120._dp,110._dp,100._dp,90._dp,72.5_dp/

      real(kind=dp) dgtr, dr, alast
      DATA DGTR/1.74533E-2_dp/,DR/1.72142E-2_dp/,ALAST/-999._dp/

      real(kind=dp) ALPHA(9)
      DATA ALPHA/-0.38_dp,0._dp,0._dp,0._dp,0.17_dp,0._dp,-0.38_dp,0._dp,0._dp/

      SAVE
!
!-------------------------External Functions----------------------------
!
      real(kind=dp) ccor, dnet, densu, globe7, glob7s, scalh, vtst7
      external ccor, dnet, densu, globe7, glob7s, scalh, vtst7
!
!-----------------------------------------------------------------------
!
!        Test for changed input
      V2=VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,2)
!
      YRD=IYD
      ZA=PDL(16,2)
      ZN1(1)=ZA
      DO 2 J=1,9
        D(J)=0.__dp
    2 CONTINUE
!        TINF VARIATIONS NOT IMPORTANT BELOW ZA OR ZN1(1)
      IF(ALT.GT.ZN1(1)) THEN
        IF(V2.EQ.1._dp.OR.ALAST.LE.ZN1(1)) TINF=PTM(1)*PT(1) &
        *(1._dp+SW(16)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PT))
      ELSE
        TINF=PTM(1)*PT(1)
      ENDIF
      T(1)=TINF
!          GRADIENT VARIATIONS NOT IMPORTANT BELOW ZN1(5)
      IF(ALT.GT.ZN1(5)) THEN
        IF(V2.EQ.1.OR.ALAST.LE.ZN1(5)) G0=PTM(4)*PS(1) &
         *(1._dp+SW(19)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PS))
      ELSE
        G0=PTM(4)*PS(1)
      ENDIF
!      Calculate these temperatures only if input changed
      IF(V2.EQ.1._dp .OR. ALT.LT.300._dp) &
        TLB=PTM(2)*(1._dp+SW(17)*GLOBE7(YRD,SEC,GLAT,GLONG,STL, &
        F107A,F107,AP,PD(1,4)))*PD(1,4)
       S=G0/(TINF-TLB)
!       Lower thermosphere temp variations not significant for
!        density above 300 km
       IF(ALT.LT.300._r8) THEN
        IF(V2.EQ.1._r8.OR.ALAST.GE.300._r8) THEN
         TN1(2)=PTM(7)*PTL(1,1)/(1._r8-SW(18)*GLOB7S(PTL(1,1)))
         TN1(3)=PTM(3)*PTL(1,2)/(1._r8-SW(18)*GLOB7S(PTL(1,2)))
         TN1(4)=PTM(8)*PTL(1,3)/(1._r8-SW(18)*GLOB7S(PTL(1,3)))
         TN1(5)=PTM(5)*PTL(1,4)/(1._r8-SW(18)*SW(20)*GLOB7S(PTL(1,4)))
         TGN1(2)=PTM(9)*PMA(1,9)*(1._r8+SW(18)*SW(20)*GLOB7S(PMA(1,9))) &
         *TN1(5)*TN1(5)/(PTM(5)*PTL(1,4))**2
        ENDIF
       ELSE
        TN1(2)=PTM(7)*PTL(1,1)
        TN1(3)=PTM(3)*PTL(1,2)
        TN1(4)=PTM(8)*PTL(1,3)
        TN1(5)=PTM(5)*PTL(1,4)
        TGN1(2)=PTM(9)*PMA(1,9) &
        *TN1(5)*TN1(5)/(PTM(5)*PTL(1,4))**2
       ENDIF
!
      Z0=ZN1(4)
      T0=TN1(4)
      TR12=1._dp
!
      IF(MASS.EQ.0) GO TO 50
!       N2 variation factor at Zlb
      G28=SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,  &
       AP,PD(1,3))
      DAY=MOD(YRD,1000._dp)
!        VARIATION OF TURBOPAUSE HEIGHT
      ZHF=PDL(25,2) &
          *(1._dp+SW(5)*PDL(25,1)*SIN(DGTR*GLAT)*COS(DR*(DAY-PT(14))))
      YRD=IYD
      T(1)=TINF
      XMM=PDM(5,3)
      Z=ALT
!
      DO 10 J = 1,11
      IF(MASS.EQ.MT(J))   GO TO 15
   10 CONTINUE
      write(iulog,100) MASS
      GO TO 90
   15 IF(Z.GT.ALTL(6).AND.MASS.NE.28.AND.MASS.NE.48) GO TO 17
!
!       **** N2 DENSITY ****
!
!      Diffusive density at Zlb
      DB28 = PDM(1,3)*EXP(G28)*PD(1,3)
!      Diffusive density at Alt
      D(3)=DENSU(Z,DB28,TINF,TLB, 28._dp,ALPHA(3),T(2),PTM(6),S,MN1,ZN1, &
       TN1,TGN1)
      DD=D(3)
!      Turbopause
      ZH28=PDM(3,3)*ZHF
      ZHM28=PDM(4,3)*PDL(6,2) 
      XMD=28._dp-XMM
!      Mixed density at Zlb
      B28=DENSU(ZH28,DB28,TINF,TLB,XMD,ALPHA(3)-1._dp,TZ,PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      IF(Z.GT.ALTL(3).OR.SW(15).EQ.0._dp) GO TO 17
!      Mixed density at Alt
      DM28=DENSU(Z,B28,TINF,TLB,XMM,ALPHA(3),TZ,PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
!      Net density at Alt
      D(3)=DNET(D(3),DM28,ZHM28,XMM,28._dp)
   17 CONTINUE
      GO TO (20,50,20,25,90,35,40,45,25,48,46),  J
   20 CONTINUE
!
!       **** HE DENSITY ****
!
!       Density variation factor at Zlb
      G4 = SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,1))
!      Diffusive density at Zlb
      DB04 = PDM(1,1)*EXP(G4)*PD(1,1)
!      Diffusive density at Alt
      D(1)=DENSU(Z,DB04,TINF,TLB, 4._dp,ALPHA(1),T(2),PTM(6),S,MN1,ZN1, &
       TN1,TGN1)
      DD=D(1)
      IF(Z.GT.ALTL(1).OR.SW(15).EQ.0._dp) GO TO 24
!      Turbopause
      ZH04=PDM(3,1)
!      Mixed density at Zlb
      B04=DENSU(ZH04,DB04,TINF,TLB,4._dp-XMM,ALPHA(1)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM04=DENSU(Z,B04,TINF,TLB,XMM,0._dp,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM04=ZHM28
!      Net density at Alt
      D(1)=DNET(D(1),DM04,ZHM04,XMM,4._dp)
!      Correction to specified mixing ratio at ground
      RL=LOG(B28*PDM(2,1)/B04)
      ZC04=PDM(5,1)*PDL(1,2)
      HC04=PDM(6,1)*PDL(2,2)
!      Net density corrected at Alt
      D(1)=D(1)*CCOR(Z,RL,HC04,ZC04)
   24 CONTINUE
      IF(MASS.NE.48)   GO TO 90
   25 CONTINUE
!
!      **** O DENSITY ****
!
!       Density variation factor at Zlb
      G16= SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,2))
!      Diffusive density at Zlb
      DB16 =  PDM(1,2)*EXP(G16)*PD(1,2)
!       Diffusive density at Alt
      D(2)=DENSU(Z,DB16,TINF,TLB, 16._r8,ALPHA(2),T(2),PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      DD=D(2)
      IF(Z.GT.ALTL(2).OR.SW(15).EQ.0._dp) GO TO 34
!  Corrected from PDM(3,1) to PDM(3,2)  12/2/85
!       Turbopause
      ZH16=PDM(3,2)
!      Mixed density at Zlb
      B16=DENSU(ZH16,DB16,TINF,TLB,16-XMM,ALPHA(2)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM16=DENSU(Z,B16,TINF,TLB,XMM,0._dp,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM16=ZHM28
!      Net density at Alt
      D(2)=DNET(D(2),DM16,ZHM16,XMM,16._dp)
!   3/16/99 Change form to match O2 departure from diff equil near 150
!   km and add dependence on F10.7
!      RL=LOG(B28*PDM(2,2)*ABS(PDL(17,2))/B16)
      RL=PDM(2,2)*PDL(17,2)*(1._dp+SW(1)*PDL(24,1)*(F107A-150._dp))
      HC16=PDM(6,2)*PDL(4,2)
      ZC16=PDM(5,2)*PDL(3,2)
      D(2)=D(2)*CCOR(Z,RL,HC16,ZC16)
!       Chemistry correction
      HCC16=PDM(8,2)*PDL(14,2)
      ZCC16=PDM(7,2)*PDL(13,2)
      RC16=PDM(4,2)*PDL(15,2)
!      Net density corrected at Alt
      D(2)=D(2)*CCOR(Z,RC16,HCC16,ZCC16)
   34 CONTINUE
      IF(MASS.NE.48.AND.MASS.NE.49) GO TO 90
   35 CONTINUE
!
!       **** O2 DENSITY ****
!
!       Density variation factor at Zlb
      G32= SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,5))
!      Diffusive density at Zlb
      DB32 = PDM(1,4)*EXP(G32)*PD(1,5)
!       Diffusive density at Alt
      D(4)=DENSU(Z,DB32,TINF,TLB, 32._r8,ALPHA(4),T(2),PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      IF(MASS.EQ.49) THEN
         DD=DD+2._dp*D(4)
      ELSE
         DD=D(4)
      ENDIF
      IF(SW(15).EQ.0._dp) GO TO 39
      IF(Z.GT.ALTL(4)) GO TO 38
!       Turbopause
      ZH32=PDM(3,4)
!      Mixed density at Zlb
      B32=DENSU(ZH32,DB32,TINF,TLB,32._r8-XMM,ALPHA(4)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM32=DENSU(Z,B32,TINF,TLB,XMM,0._r8,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM32=ZHM28
!      Net density at Alt
      D(4)=DNET(D(4),DM32,ZHM32,XMM,32._dp)
!       Correction to specified mixing ratio at ground
      RL=LOG(B28*PDM(2,4)/B32)
      HC32=PDM(6,4)*PDL(8,2)
      ZC32=PDM(5,4)*PDL(7,2)
      D(4)=D(4)*CCOR(Z,RL,HC32,ZC32)
   38 CONTINUE
!      Correction for general departure from diffusive equilibrium above Zlb
      HCC32=PDM(8,4)*PDL(23,2)
      ZCC32=PDM(7,4)*PDL(22,2)
      RC32=PDM(4,4)*PDL(24,2)*(1._r8+SW(1)*PDL(24,1)*(F107A-150._dp))
!      Net density corrected at Alt
      D(4)=D(4)*CCOR(Z,RC32,HCC32,ZCC32)
   39 CONTINUE
      IF(MASS.NE.48)   GO TO 90
   40 CONTINUE
!
!       **** AR DENSITY ****
!
!       Density variation factor at Zlb
      G40= SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,6))
!      Diffusive density at Zlb
      DB40 = PDM(1,5)*EXP(G40)*PD(1,6)
!       Diffusive density at Alt
      D(5)=DENSU(Z,DB40,TINF,TLB, 40._dp,ALPHA(5),T(2),PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      DD=D(5)
      IF(Z.GT.ALTL(5).OR.SW(15).EQ.0._dp) GO TO 44
!       Turbopause
      ZH40=PDM(3,5)
!      Mixed density at Zlb
      B40=DENSU(ZH40,DB40,TINF,TLB,40._r8-XMM,ALPHA(5)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM40=DENSU(Z,B40,TINF,TLB,XMM,0._dp,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM40=ZHM28
!      Net density at Alt
      D(5)=DNET(D(5),DM40,ZHM40,XMM,40._dp)
!       Correction to specified mixing ratio at ground
      RL=LOG(B28*PDM(2,5)/B40)
      HC40=PDM(6,5)*PDL(10,2)
      ZC40=PDM(5,5)*PDL(9,2)
!      Net density corrected at Alt
      D(5)=D(5)*CCOR(Z,RL,HC40,ZC40)
   44 CONTINUE
      IF(MASS.NE.48)   GO TO 90
   45 CONTINUE
!
!        **** HYDROGEN DENSITY ****
!
!       Density variation factor at Zlb
      G1 = SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,7))
!      Diffusive density at Zlb
      DB01 = PDM(1,6)*EXP(G1)*PD(1,7)
!       Diffusive density at Alt
      D(7)=DENSU(Z,DB01,TINF,TLB,1._dp,ALPHA(7),T(2),PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      DD=D(7)
      IF(Z.GT.ALTL(7).OR.SW(15).EQ.0._dp) GO TO 47
!       Turbopause
      ZH01=PDM(3,6)
!      Mixed density at Zlb
      B01=DENSU(ZH01,DB01,TINF,TLB,1._r8-XMM,ALPHA(7)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM01=DENSU(Z,B01,TINF,TLB,XMM,0._dp,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM01=ZHM28
!      Net density at Alt
      D(7)=DNET(D(7),DM01,ZHM01,XMM,1._dp)
!       Correction to specified mixing ratio at ground
      RL=LOG(B28*PDM(2,6)*ABS(PDL(18,2))/B01)
      HC01=PDM(6,6)*PDL(12,2)
      ZC01=PDM(5,6)*PDL(11,2)
      D(7)=D(7)*CCOR(Z,RL,HC01,ZC01)
!       Chemistry correction
      HCC01=PDM(8,6)*PDL(20,2)
      ZCC01=PDM(7,6)*PDL(19,2)
      RC01=PDM(4,6)*PDL(21,2)
!      Net density corrected at Alt
      D(7)=D(7)*CCOR(Z,RC01,HCC01,ZCC01)
   47 CONTINUE
      IF(MASS.NE.48)   GO TO 90
   48 CONTINUE
!
!        **** ATOMIC NITROGEN DENSITY ****
!
!       Density variation factor at Zlb
      G14 = SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,8))
!      Diffusive density at Zlb
      DB14 = PDM(1,7)*EXP(G14)*PD(1,8)
!       Diffusive density at Alt
      D(8)=DENSU(Z,DB14,TINF,TLB,14._dp,ALPHA(8),T(2),PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      DD=D(8)
      IF(Z.GT.ALTL(8).OR.SW(15).EQ.0._dp) GO TO 49
!       Turbopause
      ZH14=PDM(3,7)
!      Mixed density at Zlb
      B14=DENSU(ZH14,DB14,TINF,TLB,14._dp-XMM,ALPHA(8)-1._dp, &
        T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
!      Mixed density at Alt
      DM14=DENSU(Z,B14,TINF,TLB,XMM,0._r8,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
      ZHM14=ZHM28
!      Net density at Alt
      D(8)=DNET(D(8),DM14,ZHM14,XMM,14._dp)
!       Correction to specified mixing ratio at ground
      RL=LOG(B28*PDM(2,7)*ABS(PDL(3,1))/B14)
      HC14=PDM(6,7)*PDL(2,1)
      ZC14=PDM(5,7)*PDL(1,1)
      D(8)=D(8)*CCOR(Z,RL,HC14,ZC14)
!       Chemistry correction
      HCC14=PDM(8,7)*PDL(5,1)
      ZCC14=PDM(7,7)*PDL(4,1)
      RC14=PDM(4,7)*PDL(6,1)
!      Net density corrected at Alt
      D(8)=D(8)*CCOR(Z,RC14,HCC14,ZCC14)
   49 CONTINUE
      IF(MASS.NE.48) GO TO 90
   46 CONTINUE
!
!        **** Anomalous OXYGEN DENSITY ****
!
      G16H = SW(21)*GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,9))
      DB16H = PDM(1,8)*EXP(G16H)*PD(1,9)
      THO=PDM(10,8)*PDL(7,1)
      DD=DENSU(Z,DB16H,THO,THO,16._r8,ALPHA(9),T2,PTM(6),S,MN1, &
       ZN1,TN1,TGN1)
      ZSHT=PDM(6,8)
      ZMHO=PDM(5,8)
      ZSHO=SCALH(ZMHO,16._dp,THO)
      D(9)=DD*EXP(-ZSHT/ZSHO*(EXP(-(Z-ZMHO)/ZSHT)-1._dp))
      IF(MASS.NE.48) GO TO 90
!
!       TOTAL MASS DENSITY
!
      D(6) = 1.66E-24_dp*(4._dp*D(1)+16._dp*D(2)+28._dp*D(3)+32._dp*D(4)+40._dp*D(5)+ &
             D(7)+14._dp*D(8))
      DB48=1.66E-24_dp*(4._dp*DB04+16._dp*DB16+28._dp*DB28+32._dp*DB32+40._dp*DB40+DB01+ &
              14._dp*DB14)
      GO TO 90
!       TEMPERATURE AT ALTITUDE
   50 CONTINUE
      Z=ABS(ALT)
      DDUM  = DENSU(Z,1._dp, TINF,TLB,0._dp,0._dp,T(2),PTM(6),S,MN1,ZN1,TN1,TGN1)
   90 CONTINUE
!       ADJUST DENSITIES FROM CGS TO KGM
      IF(IMR.EQ.1) THEN
        DO 95 I=1,9
          D(I)=D(I)*1.E6_dp
   95   CONTINUE
        D(6)=D(6)*0.001_dp
      ENDIF
      ALAST=ALT
      RETURN
  100 FORMAT(1X,'MASS', I5, '  NOT VALID')
      END SUBROUTINE GTS7

!================================================================================================

      SUBROUTINE METERS(METER)
!         
!-----------------------------------------------------------------------
!      Convert outputs to Kg & Meters if METER true
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-------------------------------Commons---------------------------------
!
      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
!
!------------------------------Arguments--------------------------------
!
      LOGICAL METER

      SAVE
!
!-----------------------------------------------------------------------
!
      IMR=0
      IF(METER) IMR=1
      END SUBROUTINE METERS

!================================================================================================

      FUNCTION SCALH(ALT,XM,TEMP)
!         
!-----------------------------------------------------------------------
!      Calculate scale height (km)
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) scalh
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) gsurf, re
      COMMON/PARMB/GSURF,RE
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) alt, xm, temp
!
!---------------------------Local variables-----------------------------
!
      real(kind=dp) g

      real(kind=dp) rgas
      DATA RGAS/831.4_dp/

      SAVE
!
!-----------------------------------------------------------------------
!
      G=GSURF/(1._r8+ALT/RE)**2
      SCALH=RGAS*TEMP/(G*XM)
      RETURN
      END FUNCTION SCALH

!================================================================================================

      FUNCTION GLOBE7(YRD,SEC,LAT,LONG,TLOC,F107A,F107,AP,P)
!         
!-----------------------------------------------------------------------
!       CALCULATE G(L) FUNCTION 
!       Upper Thermosphere Parameters
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) globe7
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) tinf, gb, rout, t
      COMMON/TTEST/TINF,GB,ROUT,T(15)

      integer(kind=int4)  isw
      real(kind=dp) sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW

      integer(kind=int4) iyr
      real(kind=dp) plg, ctloc, stloc, c2tloc, s2tloc, c3tloc, s3tloc
      real(kind=dp) day, df, dfa, apd, apdf, apt, xlong
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC, &
       DAY,DF,DFA,APD,APDF,APT(4),XLONG,IYR
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) yrd, sec, tloc, f107a, f107
      REAL(kind=dp) LAT, LONG
      real(kind=dp) P(*),AP(*)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) ::  i, j
      real(kind=dp) t72, t81, t71, f1, f2, exp1, p45, t82, p44, c2, c4, s, c
      real(kind=dp) cd32, cd39, cd18, s2, cd14

      real(kind=dp) dgtr, dr, xl, tll
      DATA DGTR/1.74533E-2_dp/,DR/1.72142E-2_dp/, XL/1000._dp/,TLL/1000._dp/

      real(kind=dp) sw9, dayl, p14, p18, p32
      DATA SW9/1._dp/,DAYL/-1._dp/,P14/-1000._dp/,P18/-1000._dp/,P32/-1000._dp/

      integer(kind=int4) ::  nsw
      real(kind=dp) hr, sr, sv(25), p39
      DATA HR/.2618_dp/,SR/7.2722E-5_dp/,SV/25*1._dp/,NSW/14/,P39/-1000._dp/

      SAVE
!
!-------------------------Statement Functions----------------------------
!
      real(kind=dp) g0, sumex, sg0, a, ex
!       3hr Magnetic activity functions
!      Eq. A24d
      G0(A)=(A-4._dp+(P(26)-1._dp)*(A-4._dp+(EXP(-ABS(P(25))*(A-4._dp))-1._dp)/ABS(P(25 &
      ))))

!       Eq. A24c
      SUMEX(EX)=1._dp+(1._dp-EX**19)/(1._dp-EX)*EX**(.5_dp)
!       Eq. A24a
      SG0(EX)=(G0(AP(2))+(G0(AP(3))*EX+G0(AP(4))*EX*EX+G0(AP(5))*EX**3 &
       +(G0(AP(6))*EX**4+G0(AP(7))*EX**12)*(1._dp-EX**8)/(1._dp-EX)) &
       )/SUMEX(EX)
!
!-----------------------------------------------------------------------
!
      IF(ISW.NE.64999) CALL TSELEC(SV)
      DO 10 J=1,14
       T(J)=0
   10 CONTINUE
      IF(SW(9).GT.0) SW9=1._dp
      IF(SW(9).LT.0) SW9=-1._dp
      IYR = YRD/1000._dp
      DAY = YRD - IYR*1000._dp
      XLONG=LONG
!      Eq. A22 (remainder of code)
      IF(XL.EQ.LAT)   GO TO 15
!          CALCULATE LEGENDRE POLYNOMIALS
      C = SIN(LAT*DGTR)
      S = COS(LAT*DGTR)
      C2 = C*C
      C4 = C2*C2
      S2 = S*S
      PLG(2,1) = C
      PLG(3,1) = 0.5_dp*(3._dp*C2 -1._dp)
      PLG(4,1) = 0.5_dp*(5._dp*C*C2-3._dp*C)
      PLG(5,1) = (35._dp*C4 - 30._dp*C2 + 3._dp)/8._dp
      PLG(6,1) = (63._dp*C2*C2*C - 70._dp*C2*C + 15._dp*C)/8._dp
      PLG(7,1) = (11._dp*C*PLG(6,1) - 5._dp*PLG(5,1))/6._dp
!     PLG(8,1) = (13.*C*PLG(7,1) - 6.*PLG(6,1))/7.
      PLG(2,2) = S
      PLG(3,2) = 3._dp*C*S
      PLG(4,2) = 1.5_dp*(5._dp*C2-1._dp)*S
      PLG(5,2) = 2.5_dp*(7._dp*C2*C-3._dp*C)*S
      PLG(6,2) = 1.875_dp*(21._dp*C4 - 14._dp*C2 +1._dp)*S
      PLG(7,2) = (11._dp*C*PLG(6,2)-6._dp*PLG(5,2))/5._dp
!     PLG(8,2) = (13.*C*PLG(7,2)-7.*PLG(6,2))/6.
!     PLG(9,2) = (15.*C*PLG(8,2)-8.*PLG(7,2))/7.
      PLG(3,3) = 3._dp*S2
      PLG(4,3) = 15._dp*S2*C
      PLG(5,3) = 7.5_dp*(7._dp*C2 -1._dp)*S2
      PLG(6,3) = 3._dp*C*PLG(5,3)-2._dp*PLG(4,3)
      PLG(7,3)=(11._dp*C*PLG(6,3)-7._dp*PLG(5,3))/4._dp
      PLG(8,3)=(13._dp*C*PLG(7,3)-8._dp*PLG(6,3))/5._dp
      PLG(4,4) = 15._dp*S2*S
      PLG(5,4) = 105._dp*S2*S*C 
      PLG(6,4)=(9._dp*C*PLG(5,4)-7._dp*PLG(4,4))/2._dp
      PLG(7,4)=(11._dp*C*PLG(6,4)-8._dp*PLG(5,4))/3._dp
      XL=LAT
   15 CONTINUE
      IF(TLL.EQ.TLOC)   GO TO 16
      IF(SW(7).EQ.0.AND.SW(8).EQ.0.AND.SW(14).EQ.0) GOTO 16
      STLOC = SIN(HR*TLOC)
      CTLOC = COS(HR*TLOC)
      S2TLOC = SIN(2._dp*HR*TLOC)
      C2TLOC = COS(2._dp*HR*TLOC)
      S3TLOC = SIN(3._dp*HR*TLOC)
      C3TLOC = COS(3._dp*HR*TLOC)
      TLL = TLOC
   16 CONTINUE
      IF(DAY.NE.DAYL.OR.P(14).NE.P14) CD14=COS(DR*(DAY-P(14)))
      IF(DAY.NE.DAYL.OR.P(18).NE.P18) CD18=COS(2._dp*DR*(DAY-P(18)))
      IF(DAY.NE.DAYL.OR.P(32).NE.P32) CD32=COS(DR*(DAY-P(32)))
      IF(DAY.NE.DAYL.OR.P(39).NE.P39) CD39=COS(2._dp*DR*(DAY-P(39)))
      DAYL = DAY
      P14 = P(14)
      P18 = P(18)
      P32 = P(32)
      P39 = P(39)
!         F10.7 EFFECT
      DF = F107 - F107A
      DFA=F107A-150._dp
      T(1) =  P(20)*DF*(1._dp+P(60)*DFA) + P(21)*DF*DF + P(22)*DFA &
       + P(30)*DFA**2
      F1 = 1._dp + (P(48)*DFA +P(20)*DF+P(21)*DF*DF)*SWC(1)
      F2 = 1._dp + (P(50)*DFA+P(20)*DF+P(21)*DF*DF)*SWC(1)
!        TIME INDEPENDENT
      T(2) = &
        (P(2)*PLG(3,1) + P(3)*PLG(5,1)+P(23)*PLG(7,1)) &
       +(P(15)*PLG(3,1))*DFA*SWC(1) &
       +P(27)*PLG(2,1)
!        SYMMETRICAL ANNUAL
      T(3) = &
       (P(19) )*CD32
!        SYMMETRICAL SEMIANNUAL
      T(4) = &
       (P(16)+P(17)*PLG(3,1))*CD18
!        ASYMMETRICAL ANNUAL
      T(5) =  F1* &
        (P(10)*PLG(2,1)+P(11)*PLG(4,1))*CD14
!         ASYMMETRICAL SEMIANNUAL
      T(6) =    P(38)*PLG(2,1)*CD39
!        DIURNAL
      IF(SW(7).EQ.0) GOTO 200
      T71 = (P(12)*PLG(3,2))*CD14*SWC(5)
      T72 = (P(13)*PLG(3,2))*CD14*SWC(5)
      T(7) = F2* &
       ((P(4)*PLG(2,2) + P(5)*PLG(4,2) + P(28)*PLG(6,2) &
       + T71)*CTLOC &
       + (P(7)*PLG(2,2) + P(8)*PLG(4,2) +P(29)*PLG(6,2) &
       + T72)*STLOC)
  200 CONTINUE
!        SEMIDIURNAL
      IF(SW(8).EQ.0) GOTO 210
      T81 = (P(24)*PLG(4,3)+P(36)*PLG(6,3))*CD14*SWC(5) 
      T82 = (P(34)*PLG(4,3)+P(37)*PLG(6,3))*CD14*SWC(5)
      T(8) = F2* &
       ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC &
       +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)
  210 CONTINUE
!        TERDIURNAL
      IF(SW(14).EQ.0) GOTO 220
      T(14) = F2* &
       ((P(40)*PLG(4,4)+(P(94)*PLG(5,4)+P(47)*PLG(7,4))*CD14*SWC(5))* &
       S3TLOC &
       +(P(41)*PLG(4,4)+(P(95)*PLG(5,4)+P(49)*PLG(7,4))*CD14*SWC(5))* &
       C3TLOC)
  220 CONTINUE
!          MAGNETIC ACTIVITY BASED ON DAILY AP

      IF(SW9.EQ.-1._dp) GO TO 30
      APD=(AP(1)-4._dp)
      P44=P(44)
      P45=P(45)
      IF(P44.LT.0) P44=1.E-5_dp
      APDF = APD+(P45-1._dp)*(APD+(EXP(-P44*APD)-1._dp)/P44)
      IF(SW(9).EQ.0) GOTO 40
      T(9)=APDF*(P(33)+P(46)*PLG(3,1)+P(35)*PLG(5,1)+ &
       (P(101)*PLG(2,1)+P(102)*PLG(4,1)+P(103)*PLG(6,1))*CD14*SWC(5)+ &
       (P(122)*PLG(2,2)+P(123)*PLG(4,2)+P(124)*PLG(6,2))*SWC(7)* &
       COS(HR*(TLOC-P(125))))
      GO TO 40
   30 CONTINUE
      IF(P(52).EQ.0) GO TO 40
      EXP1 = EXP(-10800._dp*ABS(P(52))/(1._dp+P(139)*(45._dp-ABS(LAT))))
      IF(EXP1.GT..99999_dp) EXP1=.99999_dp
      IF(P(25).LT.1.E-4_dp) P(25)=1.E-4_dp
      APT(1)=SG0(EXP1)
!      APT(2)=SG2(EXP1)
!      APT(3)=SG0(EXP2)
!      APT(4)=SG2(EXP2)
      IF(SW(9).EQ.0) GOTO 40
      T(9) = APT(1)*(P(51)+P(97)*PLG(3,1)+P(55)*PLG(5,1)+ &
       (P(126)*PLG(2,1)+P(127)*PLG(4,1)+P(128)*PLG(6,1))*CD14*SWC(5)+ &
       (P(129)*PLG(2,2)+P(130)*PLG(4,2)+P(131)*PLG(6,2))*SWC(7)* &
       COS(HR*(TLOC-P(132))))
  40  CONTINUE
      IF(SW(10).EQ.0.OR.LONG.LE.-1000._dp) GO TO 49
!        LONGITUDINAL
      IF(SW(11).EQ.0) GOTO 230
      T(11)= (1._dp+P(81)*DFA*SWC(1))* &
      ((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2) &
       +P(104)*PLG(2,2)+P(105)*PLG(4,2)+P(106)*PLG(6,2) &
       +SWC(5)*(P(110)*PLG(2,2)+P(111)*PLG(4,2)+P(112)*PLG(6,2))*CD14)* &
           COS(DGTR*LONG) &
       +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2) &
       +P(107)*PLG(2,2)+P(108)*PLG(4,2)+P(109)*PLG(6,2) &
       +SWC(5)*(P(113)*PLG(2,2)+P(114)*PLG(4,2)+P(115)*PLG(6,2))*CD14)* &
        SIN(DGTR*LONG))
  230 CONTINUE
!        UT AND MIXED UT,LONGITUDE
      IF(SW(12).EQ.0) GOTO 240
      T(12)=(1._dp+P(96)*PLG(2,1))*(1._dp+P(82)*DFA*SWC(1))* &
      (1._dp+P(120)*PLG(2,1)*SWC(5)*CD14)* &
      ((P(69)*PLG(2,1)+P(70)*PLG(4,1)+P(71)*PLG(6,1))* &
           COS(SR*(SEC-P(72))))
      T(12)=T(12)+SWC(11)* &
       (P(77)*PLG(4,3)+P(78)*PLG(6,3)+P(79)*PLG(8,3))* &
           COS(SR*(SEC-P(80))+2._dp*DGTR*LONG)*(1._dp+P(138)*DFA*SWC(1))
  240 CONTINUE
!        UT,LONGITUDE MAGNETIC ACTIVITY
      IF(SW(13).EQ.0) GOTO 48
      IF(SW9.EQ.-1._dp) GO TO 45
      T(13)= APDF*SWC(11)*(1._dp+P(121)*PLG(2,1))* &
      ((P( 61)*PLG(3,2)+P( 62)*PLG(5,2)+P( 63)*PLG(7,2))* &
           COS(DGTR*(LONG-P( 64)))) &
       +APDF*SWC(11)*SWC(5)* &
       (P(116)*PLG(2,2)+P(117)*PLG(4,2)+P(118)*PLG(6,2))* &
           CD14*COS(DGTR*(LONG-P(119))) &
       + APDF*SWC(12)* &
       (P( 84)*PLG(2,1)+P( 85)*PLG(4,1)+P( 86)*PLG(6,1))* &
           COS(SR*(SEC-P( 76)))
      GOTO 48
   45 CONTINUE
      IF(P(52).EQ.0) GOTO 48
      T(13)=APT(1)*SWC(11)*(1._dp+P(133)*PLG(2,1))* &
      ((P(53)*PLG(3,2)+P(99)*PLG(5,2)+P(68)*PLG(7,2))* &
           COS(DGTR*(LONG-P(98)))) &
       +APT(1)*SWC(11)*SWC(5)* &
       (P(134)*PLG(2,2)+P(135)*PLG(4,2)+P(136)*PLG(6,2))* &
           CD14*COS(DGTR*(LONG-P(137))) &
       +APT(1)*SWC(12)* &
       (P(56)*PLG(2,1)+P(57)*PLG(4,1)+P(58)*PLG(6,1))* &
           COS(SR*(SEC-P(59)))
   48 CONTINUE
!  PARMS NOT USED: 83, 90,100,140-150
   49 CONTINUE
      TINF=P(31)
      DO 50 I = 1,NSW
   50 TINF = TINF + ABS(SW(I))*T(I)
      GLOBE7 = TINF
      RETURN
      END FUNCTION GLOBE7

!================================================================================================

      SUBROUTINE TSELEC(SV)
!         
!-----------------------------------------------------------------------
!        SET SWITCHES
!        Output in  COMMON/CSW/SW(25),SWC(25),ISW
!        SW FOR MAIN TERMS, SWC FOR CROSS TERMS
!  
!        TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SV),
!        WHERE SV IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1. 
!        FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
!
!        To get current values of SW: CALL TRETRV(SW)
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-------------------------------Commons---------------------------------
!
      integer(kind=int4)  isw
      real(kind=dp) sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) SV(*), SVV(*)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4)  i
      real(kind=dp) SAV(25)

      SAVE
!
!-----------------------------------------------------------------------
!
      DO 100 I = 1,25
        SAV(I)=SV(I)
        SW(I)=MOD(SV(I),2._dp)
        IF(ABS(SV(I)).EQ.1.OR.ABS(SV(I)).EQ.2._dp) THEN
          SWC(I)=1._dp
        ELSE
          SWC(I)=0._dp
        ENDIF
  100 CONTINUE
      ISW=64999
      RETURN
      ENTRY TRETRV(SVV)
      DO 200 I=1,25
        SVV(I)=SAV(I)
  200 CONTINUE
      END SUBROUTINE TSELEC

!================================================================================================

      FUNCTION GLOB7S(P)
!         
!-----------------------------------------------------------------------
!      VERSION OF GLOBE FOR LOWER ATMOSPHERE 10/26/99
!-----------------------------------------------------------------------
!
      
      use mod_kinds, only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) glob7s
!
!-------------------------------Commons---------------------------------
!
      integer iyr
      real(kind=dp) plg, ctloc, stloc, c2tloc, s2tloc, c3tloc, s3tloc
      real(kind=dp) day, df, dfa, apd, apdf, apt
      REAL(kind=dp) LONG
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC, &
       DAY,DF,DFA,APD,APDF,APT(4),LONG,IYR

      integer(kind=int4) isw
      real(kind=dp) sw, swc
      COMMON/CSW/SW(25),SWC(25),ISW
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) P(*)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) i, j
      real(kind=dp) t81, cd32, cd14, cd18, t72, cd39, t71, tt, t82
      real(kind=dp) T(14)

      real(kind=dp) dr, dgtr, pset
      DATA DR/1.72142E-2_dp/,DGTR/1.74533E-2_dp/,PSET/2._dp/

      real(kind=dp) dayl, p32, p18, p14, p39
      DATA DAYL/-1._dp/,P32,P18,P14,P39/4*-1000._dp/

      SAVE
!
!-----------------------------------------------------------------------
!
!       CONFIRM PARAMETER SET
      IF(P(100).EQ.0) P(100)=PSET
      IF(P(100).NE.PSET) THEN
        write(6,900) PSET,P(100)
  900   FORMAT(1X,'WRONG PARAMETER SET FOR GLOB7S',3F10.1)
        STOP
      ENDIF
      DO 10 J=1,14
        T(J)=0._r8
   10 CONTINUE
      IF(DAY.NE.DAYL.OR.P32.NE.P(32)) CD32=COS(DR*(DAY-P(32)))
      IF(DAY.NE.DAYL.OR.P18.NE.P(18)) CD18=COS(2._dp*DR*(DAY-P(18)))       
      IF(DAY.NE.DAYL.OR.P14.NE.P(14)) CD14=COS(DR*(DAY-P(14)))
      IF(DAY.NE.DAYL.OR.P39.NE.P(39)) CD39=COS(2._dp*DR*(DAY-P(39)))
      DAYL=DAY
      P32=P(32)
      P18=P(18)
      P14=P(14)
      P39=P(39)
!
!       F10.7
      T(1)=P(22)*DFA
!       TIME INDEPENDENT
      T(2)=P(2)*PLG(3,1)+P(3)*PLG(5,1)+P(23)*PLG(7,1) &
           +P(27)*PLG(2,1)+P(15)*PLG(4,1)+P(60)*PLG(6,1)
!       SYMMETRICAL ANNUAL
      T(3)=(P(19)+P(48)*PLG(3,1)+P(30)*PLG(5,1))*CD32
!       SYMMETRICAL SEMIANNUAL
      T(4)=(P(16)+P(17)*PLG(3,1)+P(31)*PLG(5,1))*CD18
!       ASYMMETRICAL ANNUAL
      T(5)=(P(10)*PLG(2,1)+P(11)*PLG(4,1)+P(21)*PLG(6,1))*CD14
!       ASYMMETRICAL SEMIANNUAL
      T(6)=(P(38)*PLG(2,1))*CD39
!        DIURNAL
      IF(SW(7).EQ.0) GOTO 200
      T71 = P(12)*PLG(3,2)*CD14*SWC(5)
      T72 = P(13)*PLG(3,2)*CD14*SWC(5)
      T(7) =  &
       ((P(4)*PLG(2,2) + P(5)*PLG(4,2) &
       + T71)*CTLOC &
       + (P(7)*PLG(2,2) + P(8)*PLG(4,2) &
       + T72)*STLOC)
  200 CONTINUE
!        SEMIDIURNAL
      IF(SW(8).EQ.0) GOTO 210
      T81 = (P(24)*PLG(4,3)+P(36)*PLG(6,3))*CD14*SWC(5) 
      T82 = (P(34)*PLG(4,3)+P(37)*PLG(6,3))*CD14*SWC(5)
      T(8) =  &
       ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC &
       +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)
  210 CONTINUE
!        TERDIURNAL
      IF(SW(14).EQ.0) GOTO 220
      T(14) = P(40)*PLG(4,4)*S3TLOC &
       +P(41)*PLG(4,4)*C3TLOC
  220 CONTINUE
!       MAGNETIC ACTIVITY
      IF(SW(9).EQ.0) GOTO 40
      IF(SW(9).EQ.1) &
       T(9)=APDF*(P(33)+P(46)*PLG(3,1)*SWC(2))
      IF(SW(9).EQ.-1) &
       T(9)=(P(51)*APT(1)+P(97)*PLG(3,1)*APT(1)*SWC(2))
   40 CONTINUE
      IF(SW(10).EQ.0.OR.SW(11).EQ.0.OR.LONG.LE.-1000._dp) GO TO 49
!        LONGITUDINAL
      T(11)= (1._r8+PLG(2,1)*(P(81)*SWC(5)*COS(DR*(DAY-P(82))) &
                 +P(86)*SWC(6)*COS(2._dp*DR*(DAY-P(87)))) &
              +P(84)*SWC(3)*COS(DR*(DAY-P(85))) &
                 +P(88)*SWC(4)*COS(2._dp*DR*(DAY-P(89)))) &
       *((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2) &
         +P(75)*PLG(2,2)+P(76)*PLG(4,2)+P(77)*PLG(6,2) &
          )*COS(DGTR*LONG) &
        +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2) &
         +P(78)*PLG(2,2)+P(79)*PLG(4,2)+P(80)*PLG(6,2) &
          )*SIN(DGTR*LONG))
   49 CONTINUE
      TT=0._dp
      DO 50 I=1,14
   50 TT=TT+ABS(SW(I))*T(I)
      GLOB7S=TT
      RETURN
      END FUNCTION GLOB7S

!================================================================================================

      FUNCTION DENSU(ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,S2, &
        MN1,ZN1,TN1,TGN1)
!         
!-----------------------------------------------------------------------
!       Calculate Temperature and Density Profiles for MSIS models
!       New lower thermo polynomial 10/30/89
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) densu
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) gsurf, re
      COMMON/PARMB/GSURF,RE

      integer(kind=int4) mp, ii, jg, lt, ierr, ifun, n, j
      real(kind=dp) qpb, dv
      COMMON/LSQV/MP,II,JG,LT,QPB(50),IERR,IFUN,N,J,DV(60)
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) mn1
      real(kind=dp) alt, dlb, tinf, tlb, xm, alpha, tz, zlb, s2
      real(kind=dp) ZN1(MN1),TN1(MN1),TGN1(2)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) k, mn
      real(kind=dp) densa, dta, expl, gamm, gamma, glb, ta, tt, t1, t2
      real(kind=dp) x, y, yd1, yd2, yi, z, za, zg, zg2, zgdif, z1, z2
      real(kind=dp) XS(5),YS(5),Y2OUT(5)

      real(kind=dp) rgas
      DATA RGAS/831.4_dp/

      SAVE
!
!-------------------------Statement Functions----------------------------
!
      real(kind=dp) zeta, zz, zl
      ZETA(ZZ,ZL)=(ZZ-ZL)*(RE+ZL)/(RE+ZZ)
!
!-----------------------------------------------------------------------
!
!CCCCCwrite(iulog,*) 'DB',ALT,DLB,TINF,TLB,XM,ALPHA,ZLB,S2,MN1,ZN1,TN1
      DENSU=1._dp
!        Joining altitude of Bates and spline
      ZA=ZN1(1)
      Z=MAX(ALT,ZA)
!      Geopotential altitude difference from ZLB
      ZG2=ZETA(Z,ZLB)
!      Bates temperature
      TT=TINF-(TINF-TLB)*EXP(-S2*ZG2)
      TA=TT
      TZ=TT
      DENSU=TZ
      IF(ALT.GE.ZA) GO TO 10
!
!       CALCULATE TEMPERATURE BELOW ZA
!      Temperature gradient at ZA from Bates profile
      DTA=(TINF-TA)*S2*((RE+ZLB)/(RE+ZA))**2
      TGN1(1)=DTA 
      TN1(1)=TA
      Z=MAX(ALT,ZN1(MN1))
      MN=MN1
      Z1=ZN1(1)
      Z2=ZN1(MN)
      T1=TN1(1)
      T2=TN1(MN)
!      Geopotental difference from Z1
      ZG=ZETA(Z,Z1)
      ZGDIF=ZETA(Z2,Z1)
!       Set up spline nodes
      DO 20 K=1,MN
        XS(K)=ZETA(ZN1(K),Z1)/ZGDIF
        YS(K)=1._dp/TN1(K)
   20 CONTINUE
!        End node derivatives
      YD1=-TGN1(1)/(T1*T1)*ZGDIF
      YD2=-TGN1(2)/(T2*T2)*ZGDIF*((RE+Z2)/(RE+Z1))**2
!       Calculate spline coefficients
      CALL SPLINE(XS,YS,MN,YD1,YD2,Y2OUT)
      X=ZG/ZGDIF
      CALL SPLINT(XS,YS,Y2OUT,MN,X,Y)
!       temperature at altitude
      TZ=1._dp/Y
      DENSU=TZ
   10 IF(XM.EQ.0._r8) GO TO 50
!
!      CALCULATE DENSITY ABOVE ZA
      GLB=GSURF/(1._dp+ZLB/RE)**2
      GAMMA=XM*GLB/(S2*RGAS*TINF)
      EXPL=EXP(-S2*GAMMA*ZG2)
      IF(EXPL.GT.50.OR.TT.LE.0._dp) THEN
        EXPL=50._dp
      ENDIF
!       Density at altitude
      DENSA=DLB*(TLB/TT)**(1._dp+ALPHA+GAMMA)*EXPL
      DENSU=DENSA
      IF(ALT.GE.ZA) GO TO 50
!
!      CALCULATE DENSITY BELOW ZA
      GLB=GSURF/(1._dp+Z1/RE)**2
      GAMM=XM*GLB*ZGDIF/RGAS
!       integrate spline temperatures
      CALL SPLINI(XS,YS,Y2OUT,MN,X,YI)
      EXPL=GAMM*YI
      IF(EXPL.GT.50._dp.OR.TZ.LE.0._dp) THEN
        EXPL=50._dp
      ENDIF
!       Density at altitude
      DENSU=DENSU*(T1/TZ)**(1._dp+ALPHA)*EXP(-EXPL)
   50 CONTINUE
      RETURN
      END FUNCTION DENSU

!================================================================================================

      FUNCTION DENSM(ALT,D0,XM,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)
!         
!-----------------------------------------------------------------------
!       Calculate Temperature and Density Profiles for lower atmos.
!-----------------------------------------------------------------------
!
      use mod_kinds , only : int4, dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) densm
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) gsurf, pe
      COMMON/PARMB/GSURF,RE

      real(kind=dp) taf
      COMMON/FIT/TAF

      integer(kind=int4) mp, ii, jg, ierr, ifun, n, j
      real(kind=dp) qpb, dv
      COMMON/LSQV/MP,II,JG,LT,QPB(50),IERR,IFUN,N,J,DV(60)
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) mn3, mn2
      real(kind=dp) alt, d0, xm, tz
      real(kind=dp) ZN3(MN3),TN3(MN3),TGN3(2)
      real(kind=dp) ZN2(MN2),TN2(MN2),TGN2(2)
!
!---------------------------Local variables-----------------------------
!
      integer k, lt, mn
      real(kind=dp) x, yd1, yd2, y, yi, expl, glb, gamm, zgdif, z
      real(kind=dp) re, z1, z2, zg, t1, t2
      real(kind=dp) XS(10),YS(10),Y2OUT(10)

      real(kind=dp) rgas
      DATA RGAS/831.4_dp/

      SAVE
!
!-------------------------Statement Functions----------------------------
!
      real(kind=dp) zeta, zz, zl
      ZETA(ZZ,ZL)=(ZZ-ZL)*(RE+ZL)/(RE+ZZ)
!
!-----------------------------------------------------------------------
!
      DENSM=D0
      IF(ALT.GT.ZN2(1)) GOTO 50
!      STRATOSPHERE/MESOSPHERE TEMPERATURE
      Z=MAX(ALT,ZN2(MN2))
      MN=MN2
      Z1=ZN2(1)
      Z2=ZN2(MN)
      T1=TN2(1)
      T2=TN2(MN)
      ZG=ZETA(Z,Z1)
      ZGDIF=ZETA(Z2,Z1)
!       Set up spline nodes
      DO 210 K=1,MN
        XS(K)=ZETA(ZN2(K),Z1)/ZGDIF
        YS(K)=1._dp/TN2(K)
  210 CONTINUE
      YD1=-TGN2(1)/(T1*T1)*ZGDIF
      YD2=-TGN2(2)/(T2*T2)*ZGDIF*((RE+Z2)/(RE+Z1))**2
!       Calculate spline coefficients
      CALL SPLINE(XS,YS,MN,YD1,YD2,Y2OUT)
      X=ZG/ZGDIF
      CALL SPLINT(XS,YS,Y2OUT,MN,X,Y)
!       Temperature at altitude
      TZ=1._dp/Y
      IF(XM.EQ.0._dp) GO TO 20
!
!      CALCULATE STRATOSPHERE/MESOSPHERE DENSITY 
      GLB=GSURF/(1._dp+Z1/RE)**2
      GAMM=XM*GLB*ZGDIF/RGAS
!       Integrate temperature profile
      CALL SPLINI(XS,YS,Y2OUT,MN,X,YI)
      EXPL=GAMM*YI
      IF(EXPL.GT.50._dp) EXPL=50._dp
!       Density at altitude
      DENSM=DENSM*(T1/TZ)*EXP(-EXPL)
   20 CONTINUE
      IF(ALT.GT.ZN3(1)) GOTO 50
!
!      TROPOSPHERE/STRATOSPHERE TEMPERATURE
      Z=ALT
      MN=MN3
      Z1=ZN3(1)
      Z2=ZN3(MN)
      T1=TN3(1)
      T2=TN3(MN)
      ZG=ZETA(Z,Z1)
      ZGDIF=ZETA(Z2,Z1)
!       Set up spline nodes
      DO 220 K=1,MN
        XS(K)=ZETA(ZN3(K),Z1)/ZGDIF
        YS(K)=1._dp/TN3(K)
  220 CONTINUE
      YD1=-TGN3(1)/(T1*T1)*ZGDIF
      YD2=-TGN3(2)/(T2*T2)*ZGDIF*((RE+Z2)/(RE+Z1))**2
!       Calculate spline coefficients
      CALL SPLINE(XS,YS,MN,YD1,YD2,Y2OUT)
      X=ZG/ZGDIF
      CALL SPLINT(XS,YS,Y2OUT,MN,X,Y)
!       temperature at altitude
      TZ=1._dp/Y
      IF(XM.EQ.0._dp) GO TO 30
!
!      CALCULATE TROPOSPHERIC/STRATOSPHERE DENSITY 
!     
      GLB=GSURF/(1._dp+Z1/RE)**2
      GAMM=XM*GLB*ZGDIF/RGAS
!        Integrate temperature profile
      CALL SPLINI(XS,YS,Y2OUT,MN,X,YI)
      EXPL=GAMM*YI
      IF(EXPL.GT.50._dp) EXPL=50._dp
!        Density at altitude
      DENSM=DENSM*(T1/TZ)*EXP(-EXPL)
   30 CONTINUE
   50 CONTINUE
      IF(XM.EQ.0) DENSM=TZ
      RETURN
      END FUNCTION DENSM

!================================================================================================

      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
!         
!-----------------------------------------------------------------------
!        CALCULATE 2ND DERIVATIVES OF CUBIC SPLINE INTERP FUNCTION
!        ADAPTED FROM NUMERICAL RECIPES BY PRESS ET AL
!        X,Y: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
!        N: SIZE OF ARRAYS X,Y
!        YP1,YPN: SPECIFIED DERIVATIVES AT X(1) AND X(N); VALUES
!                 >= 1E30 SIGNAL SIGNAL SECOND DERIVATIVE ZERO
!        Y2: OUTPUT ARRAY OF SECOND DERIVATIVES
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4)  n
      real(kind=dp) X(N),Y(N),Y2(N)
      real(kind=dp) yp1, ypn
!
!-----------------------------Parameters------------------------------
!
      integer(kind=int4) nmax
      PARAMETER (NMAX=100)
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) i, k
      real(kind=dp) qn, un, sig, p, U(NMAX)

      SAVE
!
!-----------------------------------------------------------------------
!
      IF(YP1.GT..99E30_dp) THEN
        Y2(1)=0
        U(1)=0
      ELSE
        Y2(1)=-.5_dp
        U(1)=(3._dp/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2._dp
        Y2(I)=(SIG-1._dp)/P
        U(I)=(6._dp*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1)) &
          /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
   11 CONTINUE
      IF(YPN.GT..99E30_dp) THEN
        QN=0
        UN=0
      ELSE
        QN=.5_dp
        UN=(3._dp/(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1._dp)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
   12 CONTINUE
      RETURN
      END SUBROUTINE SPLINE

!================================================================================================

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
!         
!-----------------------------------------------------------------------
!        CALCULATE CUBIC SPLINE INTERP VALUE
!        ADAPTED FROM NUMERICAL RECIPES BY PRESS ET AL.
!        XA,YA: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
!        Y2A: ARRAY OF SECOND DERIVATIVES
!        N: SIZE OF ARRAYS XA,YA,Y2A
!        X: ABSCISSA FOR INTERPOLATION
!        Y: OUTPUT VALUE
!-----------------------------------------------------------------------
!
    
      use mod_kinds, only : int4, dp  
      implicit none
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4) n
      real(kind=dp) XA(N),YA(N),Y2A(N)
      real(kind=dp) x, y
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) k, klo, khi
      real(kind=dp) a, b, h

      SAVE
!
!-----------------------------------------------------------------------
!
      KLO=1
      KHI=N
    1 CONTINUE
      IF(KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X) THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
        GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF(H.EQ.0) write(6,*) 'BAD XA INPUT TO SPLINT'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+ &
        ((A*A*A-A)*Y2A(KLO)+(B*B*B-B)*Y2A(KHI))*H*H/6._dp
      RETURN
      END SUBROUTINE SPLINT

!================================================================================================

      SUBROUTINE SPLINI(XA,YA,Y2A,N,X,YI)
!         
!-----------------------------------------------------------------------
!       INTEGRATE CUBIC SPLINE FUNCTION FROM XA(1) TO X
!        XA,YA: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
!        Y2A: ARRAY OF SECOND DERIVATIVES
!        N: SIZE OF ARRAYS XA,YA,Y2A
!        X: ABSCISSA ENDPOINT FOR INTEGRATION
!        Y: OUTPUT VALUE
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none
!
!------------------------------Arguments--------------------------------
!
      integer(kind=int4)  n
      real(kind=dp) XA(N),YA(N),Y2A(N)
      real(kind=dp) x, yi
!
!---------------------------Local variables-----------------------------
!
      integer(kind=int4) khi, klo
      real(kind=dp) a, b, a2, b2, h, xx

      SAVE
!
!-----------------------------------------------------------------------
!
      YI=0
      KLO=1
      KHI=2
    1 CONTINUE
      IF(X.GT.XA(KLO).AND.KHI.LE.N) THEN
        XX=X
        IF(KHI.LT.N) XX=MIN(X,XA(KHI))
        H=XA(KHI)-XA(KLO)
        A=(XA(KHI)-XX)/H
        B=(XX-XA(KLO))/H
        A2=A*A
        B2=B*B
        YI=YI+((1._dp-A2)*YA(KLO)/2._dp+B2*YA(KHI)/2._dp+ &
           ((-(1._dp+A2*A2)/4._dp+A2/2._r8)*Y2A(KLO)+ &
           (B2*B2/4._dp-B2/2._dp)*Y2A(KHI))*H*H/6._dp)*H
        KLO=KLO+1
        KHI=KHI+1
        GOTO 1
      ENDIF
      RETURN
      END SUBROUTINE SPLINI

!================================================================================================

      FUNCTION DNET(DD,DM,ZHM,XMM,XM)
!         
!-----------------------------------------------------------------------
!       TURBOPAUSE CORRECTION FOR MSIS MODELS
!         Root mean density
!       8/20/80
!          DD - diffusive density
!          DM - full mixed density
!          ZHM - transition scale length
!          XMM - full mixed molecular weight
!          XM  - species molecular weight
!          DNET - combined density
!-----------------------------------------------------------------------
!
     
      use mod_kinds, only : dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) dnet
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) dd, dm, zhm, xmm, xm
!
!---------------------------Local variables-----------------------------
!
      real(kind=dp) a, ylog

      SAVE
!
!-----------------------------------------------------------------------
!
      A=ZHM/(XMM-XM)
      IF(DM.GT.0.AND.DD.GT.0) GOTO 5
        write(iulog,*) 'DNET LOG ERROR',DM,DD,XM
        IF(DD.EQ.0.AND.DM.EQ.0) DD=1._dp
        IF(DM.EQ.0) GOTO 10
        IF(DD.EQ.0) GOTO 20
    5 CONTINUE
      YLOG=A*LOG(DM/DD)
      IF(YLOG.LT.-10._dp) GO TO 10
      IF(YLOG.GT.10._dp)  GO TO 20
        DNET=DD*(1._dp+EXP(YLOG))**(1/A)
        GO TO 50
   10 CONTINUE
        DNET=DD
        GO TO 50
   20 CONTINUE
        DNET=DM
        GO TO 50
   50 CONTINUE
      RETURN
      END FUNCTION DNET

!================================================================================================

      FUNCTION  CCOR(ALT, R,H1,ZH)
!         
!-----------------------------------------------------------------------
!        CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
!        ALT - altitude
!        R - target ratio
!        H1 - transition scale length
!        ZH - altitude of 1/2 R
!-----------------------------------------------------------------------
!
      use mod_kinds, only : dp
      implicit none
!
!-----------------------------Return Value------------------------------
!
      real(kind=dp) ccor
!
!------------------------------Arguments--------------------------------
!
      real(kind=dp) alt, r, h1, zh
!
!---------------------------Local variables-----------------------------
!
      real(kind=dp) e, ex

      SAVE
!
!-----------------------------------------------------------------------
!
      E=(ALT-ZH)/H1
      IF(E.GT.70._dp) GO TO 20
      IF(E.LT.-70._dp) GO TO 10
        EX=EXP(E)
        CCOR=R/(1._dp+EX)
        GO TO 50
   10   CCOR=R
        GO TO 50
   20   CCOR=0._dp
        GO TO 50
   50 CONTINUE
      CCOR=EXP(CCOR)
       RETURN
      END FUNCTION  CCOR

!================================================================================================

      BLOCK DATA GTD7BK
!         
!-----------------------------------------------------------------------
!       NRLMSISE-00 13-APR-00   
!-----------------------------------------------------------------------
!
      use mod_kinds, only : int4, dp
      implicit none 
!
!-------------------------------Commons---------------------------------
!
      real(kind=dp) ::  ptm, pdm
      COMMON/LOWER7/PTM(10),PDM(10,8)

      real(kind=dp) pavgm
      COMMON/MAVG7/PAVGM(10)

      CHARACTER(len=4) ISDATE, ISTIME, NAME
      COMMON/DATIM7/ISDATE(3),ISTIME(2),NAME(2)
      DATA ISDATE/'13-A','PR-0','0   '/,ISTIME/'17:4','6:08'/
      DATA NAME/'MSIS','E-00'/

      integer(kind=int4) ::  imr
      COMMON/METSEL/IMR
      DATA IMR/0/

      real(kind=dp) pt1, pt2, pt3, pa1, pa2, pa3, &
       pb1, pb2, pb3, pc1, pc2, pc3, &
       pd1, pd2, pd3, pe1, pe2, pe3, &
       pf1, pf2, pf3, pg1, pg2, pg3, &
       ph1, ph2, ph3, pi1, pi2, pi3, &
       pj1, pj2, pj3, pk1, pl1, pl2, &
       pm1, pm2, pn1, pn2, po1, po2, &
       pp1, pp2, pq1, pq2, pr1, pr2, &
       ps1, ps2, pu1, pu2, pv1, pv2, &
       pw1, pw2, px1, px2, py1, py2, &
       pz1, pz2, paa1, paa2
      COMMON/PARM7/PT1(50),PT2(50),PT3(50),PA1(50),PA2(50),PA3(50), &
       PB1(50),PB2(50),PB3(50),PC1(50),PC2(50),PC3(50), &
       PD1(50),PD2(50),PD3(50),PE1(50),PE2(50),PE3(50), &
       PF1(50),PF2(50),PF3(50),PG1(50),PG2(50),PG3(50), &
       PH1(50),PH2(50),PH3(50),PI1(50),PI2(50),PI3(50), &
       PJ1(50),PJ2(50),PJ3(50),PK1(50),PL1(50),PL2(50), &
       PM1(50),PM2(50),PN1(50),PN2(50),PO1(50),PO2(50), &
       PP1(50),PP2(50),PQ1(50),PQ2(50),PR1(50),PR2(50), &
       PS1(50),PS2(50),PU1(50),PU2(50),PV1(50),PV2(50), &
       PW1(50),PW2(50),PX1(50),PX2(50),PY1(50),PY2(50), &
       PZ1(50),PZ2(50),PAA1(50),PAA2(50)
!         TEMPERATURE
      DATA PT1/ &
        9.86573E-01_dp, 1.62228E-02_dp, 1.55270E-02_dp,-1.04323E-01_dp,-3.75801E-03_dp, &
       -1.18538E-03_dp,-1.24043E-01_dp, 4.56820E-03_dp, 8.76018E-03_dp,-1.36235E-01_dp, &
       -3.52427E-02_dp, 8.84181E-03_dp,-5.92127E-03_dp,-8.61650E+00_dp, 0.00000E+00_dp, &
        1.28492E-02_dp, 0.00000E+00_dp, 1.30096E+02_dp, 1.04567E-02_dp, 1.65686E-03_dp, &
       -5.53887E-06_dp, 2.97810E-03_dp, 0.00000E+00_dp, 5.13122E-03_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-7.27026E-06_dp, &
        0.00000E+00_dp, 6.74494E+00_dp, 4.93933E-03_dp, 2.21656E-03_dp, 2.50802E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-2.08841E-02_dp,-1.79873E+00_dp, 1.45103E-03_dp, &
        2.81769E-04_dp,-1.44703E-03_dp,-5.16394E-05_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        5.72562E-03_dp, 5.07493E-05_dp, 4.36148E-03_dp, 1.17863E-04_dp, 4.74364E-03_dp/
      DATA PT2/ &
        6.61278E-03_dp, 4.34292E-05_dp, 1.44373E-03_dp, 2.41470E-05_dp, 2.84426E-03_dp, &
        8.56560E-04_dp, 2.04028E-03_dp, 0.00000E+00_dp,-3.15994E+03_dp,-2.46423E-03_dp, &
        1.13843E-03_dp, 4.20512E-04_dp, 0.00000E+00_dp,-9.77214E+01_dp, 6.77794E-03_dp, &
        5.27499E-03_dp, 1.14936E-03_dp, 0.00000E+00_dp,-6.61311E-03_dp,-1.84255E-02_dp, &
       -1.96259E-02_dp, 2.98618E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        6.44574E+02_dp, 8.84668E-04_dp, 5.05066E-04_dp, 0.00000E+00_dp, 4.02881E+03_dp, &
       -1.89503E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.21407E-04_dp, 2.06780E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -1.20410E-02_dp,-3.63963E-03_dp, 9.92070E-05_dp,-1.15284E-04_dp,-6.33059E-05_dp, &
       -6.05545E-01_dp, 8.34218E-03_dp,-9.13036E+01_dp, 3.71042E-04_dp, 0.00000E+00_dp/
      DATA PT3/ &
        4.19000E-04_dp, 2.70928E-03_dp, 3.31507E-03_dp,-4.44508E-03_dp,-4.96334E-03_dp, &
       -1.60449E-03_dp, 3.95119E-03_dp, 2.48924E-03_dp, 5.09815E-04_dp, 4.05302E-03_dp, &
        2.24076E-03_dp, 0.00000E+00_dp, 6.84256E-03_dp, 4.66354E-04_dp, 0.00000E+00_dp, &
       -3.68328E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.46870E+02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.09501E-03_dp, 4.65156E-04_dp, 5.62583E-04_dp, 3.21596E+00_dp, &
        6.43168E-04_dp, 3.14860E-03_dp, 3.40738E-03_dp, 1.78481E-03_dp, 9.62532E-04_dp, &
        5.58171E-04_dp, 3.43731E+00_dp,-2.33195E-01_dp, 5.10289E-04_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-9.25347E+04_dp, 0.00000E+00_dp,-1.99639E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         HE DENSITY
      DATA PA1/ &
        1.09979E+00_dp,-4.88060E-02_dp,-1.97501E-01_dp,-9.10280E-02_dp,-6.96558E-03_dp, &
        2.42136E-02_dp, 3.91333E-01_dp,-7.20068E-03_dp,-3.22718E-02_dp, 1.41508E+00_dp, &
        1.68194E-01_dp, 1.85282E-02_dp, 1.09384E-01_dp,-7.24282E+00_dp, 0.00000E+00_dp, &
        2.96377E-01_dp,-4.97210E-02_dp, 1.04114E+02_dp,-8.61108E-02_dp,-7.29177E-04_dp, &
        1.48998E-06_dp, 1.08629E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.31090E-02_dp, &
        1.12818E-01_dp,-5.75005E-02_dp,-1.29919E-02_dp,-1.78849E-02_dp,-2.86343E-06_dp, &
        0.00000E+00_dp,-1.51187E+02_dp,-6.65902E-03_dp, 0.00000E+00_dp,-2.02069E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 4.32264E-02_dp,-2.80444E+01_dp,-3.26789E-03_dp, &
        2.47461E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 9.82100E-02_dp, 1.22714E-01_dp, &
       -3.96450E-02_dp, 0.00000E+00_dp,-2.76489E-03_dp, 0.00000E+00_dp, 1.87723E-03_dp/
      DATA PA2/ &
       -8.09813E-03_dp, 4.34428E-05_dp,-7.70932E-03_dp, 0.00000E+00_dp,-2.28894E-03_dp, &
       -5.69070E-03_dp,-5.22193E-03_dp, 6.00692E-03_dp,-7.80434E+03_dp,-3.48336E-03_dp, &
       -6.38362E-03_dp,-1.82190E-03_dp, 0.00000E+00_dp,-7.58976E+01_dp,-2.17875E-02_dp, &
       -1.72524E-02_dp,-9.06287E-03_dp, 0.00000E+00_dp, 2.44725E-02_dp, 8.66040E-02_dp, &
        1.05712E-01_dp, 3.02543E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -6.01364E+03_dp,-5.64668E-03_dp,-2.54157E-03_dp, 0.00000E+00_dp, 3.15611E+02_dp, &
       -5.69158E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-4.47216E-03_dp,-4.49523E-03_dp, &
        4.64428E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        4.51236E-02_dp, 2.46520E-02_dp, 6.17794E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -3.62944E-01_dp,-4.80022E-02_dp,-7.57230E+01_dp,-1.99656E-03_dp, 0.00000E+00_dp/
      DATA PA3/ &
       -5.18780E-03_dp,-1.73990E-02_dp,-9.03485E-03_dp, 7.48465E-03_dp, 1.53267E-02_dp, &
        1.06296E-02_dp, 1.18655E-02_dp, 2.55569E-03_dp, 1.69020E-03_dp, 3.51936E-02_dp, &
       -1.81242E-02_dp, 0.00000E+00_dp,-1.00529E-01_dp,-5.10574E-03_dp, 0.00000E+00_dp, &
        2.10228E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.73255E+02_dp, 5.07833E-01_dp, &
       -2.41408E-01_dp, 8.75414E-03_dp, 2.77527E-03_dp,-8.90353E-05_dp,-5.25148E+00_dp, &
       -5.83899E-03_dp,-2.09122E-02_dp,-9.63530E-03_dp, 9.77164E-03_dp, 4.07051E-03_dp, &
        2.53555E-04_dp,-5.52875E+00_dp,-3.55993E-01_dp,-2.49231E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.86026E+01_dp, 0.00000E+00_dp, 3.42722E-04_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         O DENSITY
      DATA PB1/ &
        1.02315E+00_dp,-1.59710E-01_dp,-1.06630E-01_dp,-1.77074E-02_dp,-4.42726E-03_dp, &
        3.44803E-02_dp, 4.45613E-02_dp,-3.33751E-02_dp,-5.73598E-02_dp, 3.50360E-01_dp, &
        6.33053E-02_dp, 2.16221E-02_dp, 5.42577E-02_dp,-5.74193E+00_dp, 0.00000E+00_dp, &
        1.90891E-01_dp,-1.39194E-02_dp, 1.01102E+02_dp, 8.16363E-02_dp, 1.33717E-04_dp, &
        6.54403E-06_dp, 3.10295E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.38205E-02_dp, &
        1.23910E-01_dp,-1.39831E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.95915E-06_dp, &
        0.00000E+00_dp,-7.14651E-01_dp,-5.01027E-03_dp, 0.00000E+00_dp,-3.24756E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 4.42173E-02_dp,-1.31598E+01_dp,-3.15626E-03_dp, &
        1.24574E-03_dp,-1.47626E-03_dp,-1.55461E-03_dp, 6.40682E-02_dp, 1.34898E-01_dp, &
       -2.42415E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 6.13666E-04_dp/
      DATA PB2/ &
       -5.40373E-03_dp, 2.61635E-05_dp,-3.33012E-03_dp, 0.00000E+00_dp,-3.08101E-03_dp, &
       -2.42679E-03_dp,-3.36086E-03_dp, 0.00000E+00_dp,-1.18979E+03_dp,-5.04738E-02_dp, &
       -2.61547E-03_dp,-1.03132E-03_dp, 1.91583E-04_dp,-8.38132E+01_dp,-1.40517E-02_dp, &
       -1.14167E-02_dp,-4.08012E-03_dp, 1.73522E-04_dp,-1.39644E-02_dp,-6.64128E-02_dp, &
       -6.85152E-02_dp,-1.34414E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        6.07916E+02_dp,-4.12220E-03_dp,-2.20996E-03_dp, 0.00000E+00_dp, 1.70277E+03_dp, &
       -4.63015E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.25360E-03_dp,-2.96204E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        3.92786E-02_dp, 1.31186E-02_dp,-1.78086E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -3.90083E-01_dp,-2.84741E-02_dp,-7.78400E+01_dp,-1.02601E-03_dp, 0.00000E+00_dp/
      DATA PB3/ &
       -7.26485E-04_dp,-5.42181E-03_dp,-5.59305E-03_dp, 1.22825E-02_dp, 1.23868E-02_dp, &
        6.68835E-03_dp,-1.03303E-02_dp,-9.51903E-03_dp, 2.70021E-04_dp,-2.57084E-02_dp, &
       -1.32430E-02_dp, 0.00000E+00_dp,-3.81000E-02_dp,-3.16810E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-9.05762E-04_dp,-2.14590E-03_dp,-1.17824E-03_dp, 3.66732E+00_dp, &
       -3.79729E-04_dp,-6.13966E-03_dp,-5.09082E-03_dp,-1.96332E-03_dp,-3.08280E-03_dp, &
       -9.75222E-04_dp, 4.03315E+00_dp,-2.52710E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         N2 DENSITY
      DATA PC1/ &
        1.16112E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.33725E-02_dp, 0.00000E+00_dp, &
        3.48637E-02_dp,-5.44368E-03_dp, 0.00000E+00_dp,-6.73940E-02_dp, 1.74754E-01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.74712E+02_dp, 0.00000E+00_dp, &
        1.26733E-01_dp, 0.00000E+00_dp, 1.03154E+02_dp, 5.52075E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 8.13525E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-2.50482E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.48894E-03_dp, &
        6.16053E-04_dp,-5.79716E-04_dp, 2.95482E-03_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PC2/ &
        0.00000E+00_dp, 2.47425E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PC3/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         TLB
      DATA PD1/ &
        9.44846E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.08617E-02_dp, 0.00000E+00_dp, &
       -2.44019E-02_dp, 6.48607E-03_dp, 0.00000E+00_dp, 3.08181E-02_dp, 4.59392E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.74712E+02_dp, 0.00000E+00_dp, &
        2.13260E-02_dp, 0.00000E+00_dp,-3.56958E+02_dp, 0.00000E+00_dp, 1.82278E-04_dp, &
        0.00000E+00_dp, 3.07472E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 3.83054E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -1.93065E-03_dp,-1.45090E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.23493E-03_dp, 1.36736E-03_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        3.71469E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PD2/ &
        5.10250E-03_dp, 2.47425E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 3.68756E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PD3/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         O2 DENSITY
      DATA PE1/ &
        1.38720E+00_dp, 1.44816E-01_dp, 0.00000E+00_dp, 6.07767E-02_dp, 0.00000E+00_dp, &
        2.94777E-02_dp, 7.46900E-02_dp, 0.00000E+00_dp,-9.23822E-02_dp, 8.57342E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.38636E+01_dp, 0.00000E+00_dp, &
        7.71653E-02_dp, 0.00000E+00_dp, 8.18751E+01_dp, 1.87736E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.49667E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-3.67874E+02_dp, 5.48158E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        1.22631E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PE2/ &
        8.17187E-03_dp, 3.71617E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.10826E-03_dp, &
       -3.13640E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -7.35742E-02_dp,-5.00266E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.94965E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PE3/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         AR DENSITY
      DATA PF1/ &
        1.04761E+00_dp, 2.00165E-01_dp, 2.37697E-01_dp, 3.68552E-02_dp, 0.00000E+00_dp, &
        3.57202E-02_dp,-2.14075E-01_dp, 0.00000E+00_dp,-1.08018E-01_dp,-3.73981E-01_dp, &
        0.00000E+00_dp, 3.10022E-02_dp,-1.16305E-03_dp,-2.07596E+01_dp, 0.00000E+00_dp, &
        8.64502E-02_dp, 0.00000E+00_dp, 9.74908E+01_dp, 5.16707E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 3.46193E+02_dp, 1.34297E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.48509E-03_dp, &
       -1.54689E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        1.47753E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PF2/ &
        1.89320E-02_dp, 3.68181E-05_dp, 1.32570E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        3.59719E-03_dp, 7.44328E-03_dp,-1.00023E-03_dp,-6.50528E+03_dp, 0.00000E+00_dp, &
        1.03485E-02_dp,-1.00983E-03_dp,-4.06916E-03_dp,-6.60864E+01_dp,-1.71533E-02_dp, &
        1.10605E-02_dp, 1.20300E-02_dp,-5.20034E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -2.62769E+03_dp, 7.13755E-03_dp, 4.17999E-03_dp, 0.00000E+00_dp, 1.25910E+04_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.23595E-03_dp, 4.60217E-03_dp, &
        5.71794E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -3.18353E-02_dp,-2.35526E-02_dp,-1.36189E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.03522E-02_dp,-6.67837E+01_dp,-1.09724E-03_dp, 0.00000E+00_dp/
      DATA PF3/ &
       -1.38821E-02_dp, 1.60468E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.51574E-02_dp, &
       -5.44470E-04_dp, 0.00000E+00_dp, 7.28224E-02_dp, 6.59413E-02_dp, 0.00000E+00_dp, &
       -5.15692E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.70367E+03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.36131E-02_dp, 5.38153E-03_dp, 0.00000E+00_dp, 4.76285E+00_dp, &
       -1.75677E-02_dp, 2.26301E-02_dp, 0.00000E+00_dp, 1.76631E-02_dp, 4.77162E-03_dp, &
        0.00000E+00_dp, 5.39354E+00_dp, 0.00000E+00_dp,-7.51710E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-8.82736E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!          H DENSITY
      DATA PG1/ &
        1.26376E+00_dp,-2.14304E-01_dp,-1.49984E-01_dp, 2.30404E-01_dp, 2.98237E-02_dp, &
        2.68673E-02_dp, 2.96228E-01_dp, 2.21900E-02_dp,-2.07655E-02_dp, 4.52506E-01_dp, &
        1.20105E-01_dp, 3.24420E-02_dp, 4.24816E-02_dp,-9.14313E+00_dp, 0.00000E+00_dp, &
        2.47178E-02_dp,-2.88229E-02_dp, 8.12805E+01_dp, 5.10380E-02_dp,-5.80611E-03_dp, &
        2.51236E-05_dp,-1.24083E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp,-3.48190E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.89885E-05_dp, &
        0.00000E+00_dp, 1.53595E+02_dp,-1.68604E-02_dp, 0.00000E+00_dp, 1.01015E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.84552E-04_dp, &
       -1.22181E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
       -1.04927E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-5.91313E-03_dp/
      DATA PG2/ &
       -2.30501E-02_dp, 3.14758E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.26956E-02_dp, &
        8.35489E-03_dp, 3.10513E-04_dp, 0.00000E+00_dp, 3.42119E+03_dp,-2.45017E-03_dp, &
       -4.27154E-04_dp, 5.45152E-04_dp, 1.89896E-03_dp, 2.89121E+01_dp,-6.49973E-03_dp, &
       -1.93855E-02_dp,-1.48492E-02_dp, 0.00000E+00_dp,-5.10576E-02_dp, 7.87306E-02_dp, &
        9.51981E-02_dp,-1.49422E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        2.65503E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 6.37110E-03_dp, 3.24789E-04_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        6.14274E-02_dp, 1.00376E-02_dp,-8.41083E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.27099E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PG3/ &
       -3.94077E-03_dp,-1.28601E-02_dp,-7.97616E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-6.71465E-03_dp,-1.69799E-03_dp, 1.93772E-03_dp, 3.81140E+00_dp, &
       -7.79290E-03_dp,-1.82589E-02_dp,-1.25860E-02_dp,-1.04311E-02_dp,-3.02465E-03_dp, &
        2.43063E-03_dp, 3.63237E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!          N DENSITY
      DATA PH1/ &
        7.09557E+01_dp,-3.26740E-01_dp, 0.00000E+00_dp,-5.16829E-01_dp,-1.71664E-03_dp, &
        9.09310E-02_dp,-6.71500E-01_dp,-1.47771E-01_dp,-9.27471E-02_dp,-2.30862E-01_dp, &
       -1.56410E-01_dp, 1.34455E-02_dp,-1.19717E-01_dp, 2.52151E+00_dp, 0.00000E+00_dp, &
       -2.41582E-01_dp, 5.92939E-02_dp, 4.39756E+00_dp, 9.15280E-02_dp, 4.41292E-03_dp, &
        0.00000E+00_dp, 8.66807E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 9.74701E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 6.70217E+01_dp,-1.31660E-03_dp, 0.00000E+00_dp,-1.65317E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 8.50247E-02_dp, 2.77428E+01_dp, 4.98658E-03_dp, &
        6.15115E-03_dp, 9.50156E-03_dp,-2.12723E-02_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
       -2.38645E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.37380E-03_dp/
      DATA PH2/ &
       -8.41918E-03_dp, 2.80145E-05_dp, 7.12383E-03_dp, 0.00000E+00_dp,-1.66209E-02_dp, &
        1.03533E-04_dp,-1.68898E-02_dp, 0.00000E+00_dp, 3.64526E+03_dp, 0.00000E+00_dp, &
        6.54077E-03_dp, 3.69130E-04_dp, 9.94419E-04_dp, 8.42803E+01_dp,-1.16124E-02_dp, &
       -7.74414E-03_dp,-1.68844E-03_dp, 1.42809E-03_dp,-1.92955E-03_dp, 1.17225E-01_dp, &
       -2.41512E-02_dp, 1.50521E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        1.60261E+03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.54403E-04_dp,-1.87270E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        2.76439E-02_dp, 6.43207E-03_dp,-3.54300E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-2.80221E-02_dp, 8.11228E+01_dp,-6.75255E-04_dp, 0.00000E+00_dp/
      DATA PH3/ &
       -1.05162E-02_dp,-3.48292E-03_dp,-6.97321E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.45546E-03_dp,-1.31970E-02_dp,-3.57751E-03_dp,-1.09021E+00_dp, &
       -1.50181E-02_dp,-7.12841E-03_dp,-6.64590E-03_dp,-3.52610E-03_dp,-1.87773E-02_dp, &
       -2.22432E-03_dp,-3.93895E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!        HOT O DENSITY
      DATA PI1/ &
        6.04050E-02_dp, 1.57034E+00_dp, 2.99387E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.51018E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-8.61650E+00_dp, 1.26454E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 5.50878E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 6.23881E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
       -9.45934E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PI2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PI3/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!          S PARAM  
      DATA PJ1/ &
        9.56827E-01_dp, 6.20637E-02_dp, 3.18433E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        3.94900E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-9.24882E-03_dp,-7.94023E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.74712E+02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.74677E-03_dp, 0.00000E+00_dp, 1.54951E-02_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-6.99007E-04_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.24362E-02_dp,-5.28756E-03_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PJ2/ &
        0.00000E+00_dp, 2.47425E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PJ3/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!          TURBO
      DATA PK1/ &
        1.09930E+00_dp, 3.90631E+00_dp, 3.07165E+00_dp, 9.86161E-01_dp, 1.63536E+01_dp, &
        4.63830E+00_dp, 1.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.93318E-02_dp, 1.18339E-01_dp, &
        1.22732E+00_dp, 1.02669E-01_dp, 1.17681E+00_dp, 2.12185E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.08607E+00_dp, 1.34836E+00_dp, 1.10016E+00_dp, 7.34129E-01_dp, &
        1.15241E+00_dp, 2.22784E+00_dp, 7.95907E-01_dp, 4.03601E+00_dp, 4.39732E+00_dp, &
        1.23435E+02_dp,-4.52411E-02_dp, 1.68986E-06_dp, 7.44294E-01_dp, 1.03604E+00_dp, &
        1.72783E+02_dp, 1.17681E+00_dp, 2.12185E+00_dp,-7.83697E-01_dp, 9.49154E-01_dp/
!         LOWER BOUNDARY
      DATA PTM/ &
        1.04130E+03_dp, 3.86000E+02_dp, 1.95000E+02_dp, 1.66728E+01_dp, 2.13000E+02_dp, &
        1.20000E+02_dp, 2.40000E+02_dp, 1.87000E+02_dp,-2.00000E+00_dp, 0.00000E+00_dp/
      DATA PDM/ &
        2.45600E+07_dp, 6.71072E-06_dp, 1.00000E+02_dp, 0.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        8.59400E+10_dp, 1.00000E+00_dp, 1.05000E+02_dp,-8.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 9.00000E+01_dp, 2.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        2.81000E+11_dp, 0.00000E+00_dp, 1.05000E+02_dp, 2.80000E+01_dp, 2.89500E+01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        3.30000E+10_dp, 2.68270E-01_dp, 1.05000E+02_dp, 1.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 1.10000E+02_dp,-1.00000E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        1.33000E+09_dp, 1.19615E-02_dp, 1.05000E+02_dp, 0.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        1.76100E+05_dp, 1.00000E+00_dp, 9.50000E+01_dp,-8.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 9.00000E+01_dp, 2.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        1.00000E+07_dp, 1.00000E+00_dp, 1.05000E+02_dp,-8.00000E+00_dp, 1.10000E+02_dp, &
        1.00000E+01_dp, 9.00000E+01_dp, 2.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
!
        1.00000E+06_dp, 1.00000E+00_dp, 1.05000E+02_dp,-8.00000E+00_dp, 5.50000E+02_dp, &
        7.60000E+01_dp, 9.00000E+01_dp, 2.00000E+00_dp, 0.00000E+00_dp, 4.00000E+03_dp/
!         TN1(2)
      DATA PL1/ &
        1.00858E+00_dp, 4.56011E-02_dp,-2.22972E-02_dp,-5.44388E-02_dp, 5.23136E-04_dp, &
       -1.88849E-02_dp, 5.23707E-02_dp,-9.43646E-03_dp, 6.31707E-03_dp,-7.80460E-02_dp, &
       -4.88430E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-7.60250E+00_dp, 0.00000E+00_dp, &
       -1.44635E-02_dp,-1.76843E-02_dp,-1.21517E+02_dp, 2.85647E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 6.31792E-04_dp, 0.00000E+00_dp, 5.77197E-03_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-8.90272E+03_dp, 3.30611E-03_dp, 3.02172E-03_dp, 0.00000E+00_dp, &
       -2.13673E-03_dp,-3.20910E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.76034E-03_dp, &
        2.82487E-03_dp,-2.97592E-04_dp,-4.21534E-03_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        8.96456E-03_dp, 0.00000E+00_dp,-1.08596E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PL2/ &
        5.57917E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 9.65405E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!         TN1(3)
      DATA PM1/ &
        9.39664E-01_dp, 8.56514E-02_dp,-6.79989E-03_dp, 2.65929E-02_dp,-4.74283E-03_dp, &
        1.21855E-02_dp,-2.14905E-02_dp, 6.49651E-03_dp,-2.05477E-02_dp,-4.24952E-02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.19148E+01_dp, 0.00000E+00_dp, &
        1.18777E-02_dp,-7.28230E-02_dp,-8.15965E+01_dp, 1.73887E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-1.44691E-02_dp, 2.80259E-04_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.16584E+02_dp, 3.18713E-03_dp, 7.37479E-03_dp, 0.00000E+00_dp, &
       -2.55018E-03_dp,-3.92806E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.89757E-03_dp, &
       -1.33549E-03_dp, 1.02661E-03_dp, 3.53775E-04_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
       -9.17497E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PM2/ &
        3.56082E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.00902E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!         TN1(4)
      DATA PN1/ &
        9.85982E-01_dp,-4.55435E-02_dp, 1.21106E-02_dp, 2.04127E-02_dp,-2.40836E-03_dp, &
        1.11383E-02_dp,-4.51926E-02_dp, 1.35074E-02_dp,-6.54139E-03_dp, 1.15275E-01_dp, &
        1.28247E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp,-5.30705E+00_dp, 0.00000E+00_dp, &
       -3.79332E-02_dp,-6.24741E-02_dp, 7.71062E-01_dp, 2.96315E-02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 6.81051E-03_dp,-4.34767E-03_dp, 8.66784E-02_dp, &
        1.58727E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.07003E+01_dp,-2.76907E-03_dp, 4.32474E-04_dp, 0.00000E+00_dp, &
        1.31497E-03_dp,-6.47517E-04_dp, 0.00000E+00_dp,-2.20621E+01_dp,-1.10804E-03_dp, &
       -8.09338E-04_dp, 4.18184E-04_dp, 4.29650E-03_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PN2/ &
       -4.04337E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-9.52550E-04_dp, &
        8.56253E-04_dp, 4.33114E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.21223E-03_dp, &
        2.38694E-04_dp, 9.15245E-04_dp, 1.28385E-03_dp, 8.67668E-04_dp,-5.61425E-06_dp, &
        1.04445E+00_dp, 3.41112E+01_dp, 0.00000E+00_dp,-8.40704E-01_dp,-2.39639E+02_dp, &
        7.06668E-01_dp,-2.05873E+01_dp,-3.63696E-01_dp, 2.39245E+01_dp, 0.00000E+00_dp, &
       -1.06657E-03_dp,-7.67292E-04_dp, 1.54534E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!         TN1(5) TN2(1)
      DATA PO1/ &
        1.00320E+00_dp, 3.83501E-02_dp,-2.38983E-03_dp, 2.83950E-03_dp, 4.20956E-03_dp, &
        5.86619E-04_dp, 2.19054E-02_dp,-1.00946E-02_dp,-3.50259E-03_dp, 4.17392E-02_dp, &
       -8.44404E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 4.96949E+00_dp, 0.00000E+00_dp, &
       -7.06478E-03_dp,-1.46494E-02_dp, 3.13258E+01_dp,-1.86493E-03_dp, 0.00000E+00_dp, &
       -1.67499E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.12686E-04_dp, 8.66784E-02_dp, &
        1.58727E-01_dp,-4.64167E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        4.37353E-03_dp,-1.99069E+02_dp, 0.00000E+00_dp,-5.34884E-03_dp, 0.00000E+00_dp, &
        1.62458E-03_dp, 2.93016E-03_dp, 2.67926E-03_dp, 5.90449E+02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.17266E-03_dp,-3.58890E-04_dp, 8.47001E-02_dp, 1.70147E-01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 1.38673E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PO2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.60571E-03_dp, &
        6.28078E-04_dp, 5.05469E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.57829E-03_dp, &
       -4.00855E-04_dp, 5.04077E-05_dp,-1.39001E-03_dp,-2.33406E-03_dp,-4.81197E-04_dp, &
        1.46758E+00_dp, 6.20332E+00_dp, 0.00000E+00_dp, 3.66476E-01_dp,-6.19760E+01_dp, &
        3.09198E-01_dp,-1.98999E+01_dp, 0.00000E+00_dp,-3.29933E+02_dp, 0.00000E+00_dp, &
       -1.10080E-03_dp,-9.39310E-05_dp, 1.39638E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN2(2)
      DATA PP1/ &
        9.81637E-01_dp,-1.41317E-03_dp, 3.87323E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.58707E-02_dp, &
       -8.63658E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.02226E+00_dp, 0.00000E+00_dp, &
       -8.69424E-03_dp,-1.91397E-02_dp, 8.76779E+01_dp, 4.52188E-03_dp, 0.00000E+00_dp, &
        2.23760E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-7.07572E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
       -4.11210E-03_dp, 3.50060E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-8.36657E-03_dp, 1.61347E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-1.45130E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PP2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.24152E-03_dp, &
        6.43365E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.33255E-03_dp, &
        2.42657E-03_dp, 1.60666E-03_dp,-1.85728E-03_dp,-1.46874E-03_dp,-4.79163E-06_dp, &
        1.22464E+00_dp, 3.53510E+01_dp, 0.00000E+00_dp, 4.49223E-01_dp,-4.77466E+01_dp, &
        4.70681E-01_dp, 8.41861E+00_dp,-2.88198E-01_dp, 1.67854E+02_dp, 0.00000E+00_dp, &
        7.11493E-04_dp, 6.05601E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN2(3)
      DATA PQ1/ &
        1.00422E+00_dp,-7.11212E-03_dp, 5.24480E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-5.28914E-02_dp, &
       -2.41301E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.12219E+01_dp,-1.03830E-02_dp, &
       -3.28077E-03_dp, 1.65727E-02_dp, 1.68564E+00_dp,-6.68154E-03_dp, 0.00000E+00_dp, &
        1.45155E-02_dp, 0.00000E+00_dp, 8.42365E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-4.34645E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.16780E-02_dp, &
        0.00000E+00_dp,-1.38459E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 7.04573E-03_dp,-4.73204E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 1.08767E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PQ2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-8.08279E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.21769E-04_dp, &
       -2.27387E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.26769E-03_dp, &
        3.16901E-03_dp, 4.60316E-04_dp,-1.01431E-04_dp, 1.02131E-03_dp, 9.96601E-04_dp, &
        1.25707E+00_dp, 2.50114E+01_dp, 0.00000E+00_dp, 4.24472E-01_dp,-2.77655E+01_dp, &
        3.44625E-01_dp, 2.75412E+01_dp, 0.00000E+00_dp, 7.94251E+02_dp, 0.00000E+00_dp, &
        2.45835E-03_dp, 1.38871E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN2(4) TN3(1)
      DATA PR1/ &
        1.01890E+00_dp,-2.46603E-02_dp, 1.00078E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-6.70977E-02_dp, &
       -4.02286E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.29466E+01_dp,-7.47019E-03_dp, &
        2.26580E-03_dp, 2.63931E-02_dp, 3.72625E+01_dp,-6.39041E-03_dp, 0.00000E+00_dp, &
        9.58383E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.85291E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 1.39717E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 9.19771E-03_dp,-3.69121E+02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-1.57067E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PR2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-7.07265E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.92953E-03_dp, &
       -2.77739E-03_dp,-4.40092E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.47280E-03_dp, &
        2.95035E-04_dp,-1.81246E-03_dp, 2.81945E-03_dp, 4.27296E-03_dp, 9.78863E-04_dp, &
        1.40545E+00_dp,-6.19173E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-7.93632E+01_dp, &
        4.44643E-01_dp,-4.03085E+02_dp, 0.00000E+00_dp, 1.15603E+01_dp, 0.00000E+00_dp, &
        2.25068E-03_dp, 8.48557E-04_dp,-2.98493E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN3(2)
      DATA PS1/ &
        9.75801E-01_dp, 3.80680E-02_dp,-3.05198E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.85575E-02_dp, &
        5.04057E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.76046E+02_dp, 1.44594E-02_dp, &
       -1.48297E-03_dp,-3.68560E-03_dp, 3.02185E+01_dp,-3.23338E-03_dp, 0.00000E+00_dp, &
        1.53569E-02_dp, 0.00000E+00_dp,-1.15558E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 4.89620E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.00616E-02_dp, &
       -8.21324E-03_dp,-1.57757E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 6.63564E-03_dp, 4.58410E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-2.51280E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PS2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 9.91215E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-8.73148E-04_dp, &
       -1.29648E-03_dp,-7.32026E-05_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-4.68110E-03_dp, &
       -4.66003E-03_dp,-1.31567E-03_dp,-7.39390E-04_dp, 6.32499E-04_dp,-4.65588E-04_dp, &
       -1.29785E+00_dp,-1.57139E+02_dp, 0.00000E+00_dp, 2.58350E-01_dp,-3.69453E+01_dp, &
        4.10672E-01_dp, 9.78196E+00_dp,-1.52064E-01_dp,-3.85084E+03_dp, 0.00000E+00_dp, &
       -8.52706E-04_dp,-1.40945E-03_dp,-7.26786E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN3(3)
      DATA PU1/ &
        9.60722E-01_dp, 7.03757E-02_dp,-3.00266E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.22671E-02_dp, &
        4.10423E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.63070E+02_dp, 1.06073E-02_dp, &
        5.40747E-04_dp, 7.79481E-03_dp, 1.44908E+02_dp, 1.51484E-04_dp, 0.00000E+00_dp, &
        1.97547E-02_dp, 0.00000E+00_dp,-1.41844E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 5.77884E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 9.74319E-03_dp, &
        0.00000E+00_dp,-2.88015E+03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-4.44902E-03_dp,-2.92760E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 2.34419E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PU2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.36685E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-4.65325E-04_dp, &
       -5.50628E-04_dp, 3.31465E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.06179E-03_dp, &
       -3.08575E-03_dp,-7.93589E-04_dp,-1.08629E-04_dp, 5.95511E-04_dp,-9.05050E-04_dp, &
        1.18997E+00_dp, 4.15924E+01_dp, 0.00000E+00_dp,-4.72064E-01_dp,-9.47150E+02_dp, &
        3.98723E-01_dp, 1.98304E+01_dp, 0.00000E+00_dp, 3.73219E+03_dp, 0.00000E+00_dp, &
       -1.50040E-03_dp,-1.14933E-03_dp,-1.56769E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN3(4)
      DATA PV1/ &
        1.03123E+00_dp,-7.05124E-02_dp, 8.71615E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-3.82621E-02_dp, &
       -9.80975E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.89286E+01_dp, 9.57341E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 8.66153E+01_dp, 7.91938E-04_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 4.68917E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 7.86638E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 9.90827E-03_dp, &
        0.00000E+00_dp, 6.55573E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-4.00200E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 7.07457E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PV2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.72268E-03_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.04970E-04_dp, &
        1.21560E-03_dp,-8.05579E-06_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.49941E-03_dp, &
       -4.57256E-04_dp,-1.59311E-04_dp, 2.96481E-04_dp,-1.77318E-03_dp,-6.37918E-04_dp, &
        1.02395E+00_dp, 1.28172E+01_dp, 0.00000E+00_dp, 1.49903E-01_dp,-2.63818E+01_dp, &
        0.00000E+00_dp, 4.70628E+01_dp,-2.22139E-01_dp, 4.82292E-02_dp, 0.00000E+00_dp, &
       -8.67075E-04_dp,-5.86479E-04_dp, 5.32462E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TN3(5) SURFACE TEMP TSL
      DATA PW1/ &
        1.00828E+00_dp,-9.10404E-02_dp,-2.26549E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-2.32420E-02_dp, &
       -9.08925E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.36105E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-1.24957E+01_dp,-5.87939E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.79765E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.01237E+03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-1.75553E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PW2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.29699E-03_dp, &
        1.26659E-03_dp, 2.68402E-04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.17894E-03_dp, &
        1.48746E-03_dp, 1.06478E-04_dp, 1.34743E-04_dp,-2.20939E-03_dp,-6.23523E-04_dp, &
        6.36539E-01_dp, 1.13621E+01_dp, 0.00000E+00_dp,-3.93777E-01_dp, 2.38687E+03_dp, &
        0.00000E+00_dp, 6.61865E+02_dp,-1.21434E-01_dp, 9.27608E+00_dp, 0.00000E+00_dp, &
        1.68478E-04_dp, 1.24892E-03_dp, 1.71345E-03_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TGN3(2) SURFACE GRAD TSLG
      DATA PX1/ &
        1.57293E+00_dp,-6.78400E-01_dp, 6.47500E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-7.62974E-02_dp, &
       -3.60423E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 1.28358E+02_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 4.68038E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.67898E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.90994E+04_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.15706E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PX2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TGN2(1) TGN1(2)
      DATA PY1/ &
        8.60028E-01_dp, 3.77052E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.17570E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 7.77757E-03_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 1.01024E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 6.54251E+02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PY2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.56959E-02_dp, &
        1.91001E-02_dp, 3.15971E-02_dp, 1.00982E-02_dp,-6.71565E-03_dp, 2.57693E-03_dp, &
        1.38692E+00_dp, 2.82132E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.81511E+02_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          TGN3(1) TGN2(2)
      DATA PZ1/ &
        1.06029E+00_dp,-5.25231E-02_dp, 3.73034E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.31072E-02_dp, &
       -3.88409E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp,-1.65295E+02_dp,-2.13801E-01_dp, &
       -4.38916E-02_dp,-3.22716E-01_dp,-8.82393E+01_dp, 1.18458E-01_dp, 0.00000E+00_dp, &
       -4.35863E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp,-1.19782E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 2.62229E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp,-5.37443E+01_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp,-4.55788E-01_dp, 0.00000E+00_dp, 0.00000E+00_dp/
      DATA PZ2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 3.84009E-02_dp, &
        3.96733E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 5.05494E-02_dp, &
        7.39617E-02_dp, 1.92200E-02_dp,-8.46151E-03_dp,-1.34244E-02_dp, 1.96338E-02_dp, &
        1.50421E+00_dp, 1.88368E+01_dp, 0.00000E+00_dp, 0.00000E+00_dp,-5.13114E+01_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        5.11923E-02_dp, 3.61225E-02_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 2.00000E+00_dp/
!          SEMIANNUAL MULT SAM
      DATA PAA1/ &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, &
        1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp, 1.00000E+00_dp/
      DATA PAA2/ &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, &
        0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp, 0.00000E+00_dp/
!         MIDDLE ATMOSPHERE AVERAGES
      DATA PAVGM/ &
        2.61000E+02_dp, 2.64000E+02_dp, 2.29000E+02_dp, 2.17000E+02_dp, 2.17000E+02_dp, &
        2.23000E+02_dp, 2.86760E+02_dp,-2.93940E+00_dp, 2.50000E+00_dp, 0.00000E+00_dp/

      END BLOCK DATA GTD7BK

!================================================================================================

