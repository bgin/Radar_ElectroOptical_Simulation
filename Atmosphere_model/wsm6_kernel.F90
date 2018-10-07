
    
#include "Config.fpp"


#ifdef IINSIDE
#define _NISLFV_RAIN_PLM_ nislfv_rain_plm_ii
#define _NISLFV_RAIN_PLM6_ nislfv_rain_plm6_ii
#else
#define _NISLFV_RAIN_PLM_ nislfv_rain_plm
#define _NISLFV_RAIN_PLM6_ nislfv_rain_plm6
#endif



MODULE module_mp_wsm6
!
   use module_kinds, only : R64P
   IMPLICIT NONE
!
   REAL(R64P), PARAMETER, PUBLIC :: dtcldcr     = 120._R64P ! maximum time step for minor loops
   REAL(R64P), PARAMETER, PUBLIC :: n0r = 8.e6_R64P         ! intercept parameter rain
   REAL(R64P), PARAMETER, PUBLIC :: n0g = 4.e6_R64P         ! intercept parameter graupel
   REAL(R64P), PARAMETER, PUBLIC :: avtr = 841.9_R64P       ! a constant for terminal velocity of rain
   REAL(R64P), PARAMETER, PUBLIC :: bvtr = 0.8_R64P         ! a constant for terminal velocity of rain
   REAL(R64P), PARAMETER, PUBLIC :: r0 = .8e-5_R64P         ! 8 microm  in contrast to 10 micro m
   REAL(R64P), PARAMETER, PUBLIC :: peaut = .55_R64P        ! collection efficiency
   REAL(R64P), PARAMETER, PUBLIC :: xncr = 3.e8_R64P        ! maritime cloud in contrast to 3.e8 in tc80
   REAL(R64P), PARAMETER, PUBLIC :: xmyu = 1.718e-5_R64P    ! the dynamic viscosity kgm-1s-1
   REAL(R64P), PARAMETER, PUBLIC :: avts = 11.72_R64P       ! a constant for terminal velocity of snow
   REAL(R64P), PARAMETER, PUBLIC :: bvts = .41_R64P         ! a constant for terminal velocity of snow
   REAL(R64P), PARAMETER, PUBLIC :: avtg = 330._R64P        ! a constant for terminal velocity of graupel
   REAL(R64P), PARAMETER, PUBLIC :: bvtg = 0.8_R64P         ! a constant for terminal velocity of graupel
   REAL(R64P), PARAMETER, PUBLIC :: deng = 500._R64P        ! density of graupel
   REAL(R64P), PARAMETER, PUBLIC :: n0smax =  1.e11_R64P    ! maximum n0s (t=-90C unlimited)
   REAL(R64P), PARAMETER, PUBLIC :: lamdarmax = 8.e4_R64P   ! limited maximum value for slope parameter of rain
   REAL(R64P), PARAMETER, PUBLIC :: lamdasmax = 1.e5_R64P   ! limited maximum value for slope parameter of snow
   REAL(R64P), PARAMETER, PUBLIC :: lamdagmax = 6.e4_R64P   ! limited maximum value for slope parameter of graupel
   REAL(R64P), PARAMETER, PUBLIC :: dicon = 11.9_R64P       ! constant for the cloud-ice diamter
   REAL(R64P), PARAMETER, PUBLIC :: dimax = 500.e-6_R64P    ! limited maximum value for the cloud-ice diamter
   REAL(R64P), PARAMETER, PUBLIC :: n0s = 2.e6_R64P         ! temperature dependent intercept parameter snow
   REAL(R64P), PARAMETER, PUBLIC :: alpha = .12_R64P        ! .122 exponen factor for n0s
   REAL(R64P), PARAMETER, PUBLIC :: pfrz1 = 100._R64P       ! constant in Biggs freezing
   REAL(R64P), PARAMETER, PUBLIC :: pfrz2 = 0.66_R64P       ! constant in Biggs freezing
   REAL(R64P), PARAMETER, PUBLIC :: qcrmin = 1.e-9_R64P     ! minimun values for qr, qs, and qg
   REAL(R64P), PARAMETER, PUBLIC :: eacrc = 1.0_R64P        ! Snow/cloud-water collection efficiency
   REAL(R64P), PARAMETER, PUBLIC :: dens  =  100.0_R64P     ! Density of snow
   REAL(R64P), PARAMETER, PUBLIC :: qs0   =  6.e-4_R64P     ! threshold amount for aggretion to occur
   REAL(R64P), SAVE ::                                      &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr, &
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,    &
             bvtr6,g6pbr,                             &
             precr1,precr2,roqimax,bvts1,             &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,     &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r, &
             pidn0s,xlv1,pacrc,pi,                    &
             bvtg1,bvtg2,bvtg3,bvtg4,g1pbg,           &
             g3pbg,g4pbg,g5pbgo2,pvtg,pacrg,          &
             precg1,precg2,pidn0g,                    &
             rslopermax,rslopesmax,rslopegmax,        &
             rsloperbmax,rslopesbmax,rslopegbmax,     &
             rsloper2max,rslopes2max,rslopeg2max,     &
             rsloper3max,rslopes3max,rslopeg3max
CONTAINS
!===================================================================
!
!===================================================================
!
  SUBROUTINE wsm62D(t, q                                          &   
                   ,qci, qrs, den, p, delz                        &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                   ,graupel,graupelncv                            &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a 6-class GRAUPEL phase microphyiscs scheme (WSM6) of the 
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. A theoretical background of the ice-microphysics and related
!  processes in the WSMMPs are described in Hong et al. (2004).
!  All production terms in the WSM6 scheme are described in Hong and Lim (2006).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WSM6 cloud scheme
!
!  Coded by Song-You Hong and Jeong-Ock Jade Lim (Yonsei Univ.)
!           Summer 2003
!
!  Implemented by Song-You Hong (Yonsei Univ.) and Jimy Dudhia (NCAR)
!           Summer 2004
!
!  History :  semi-lagrangian scheme sedimentation(JH), and clean up
!             Hong, August 2009
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc.
!             Dudhia, Hong and Lim (DHL, 2008) J. Meteor. Soc. Japan
!             Lin, Farley, Orville (LFO, 1983) J. Appl. Meteor.
!             Rutledge, Hobbs (RH83, 1983) J. Atmos. Sci.
!             Rutledge, Hobbs (RH84, 1984) J. Atmos. Sci.
!             Juang and Hong (JH, 2010) Mon. Wea. Rev.
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte,  &
                                      lat
  REAL(R64P), DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL(R64P), DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci
  REAL(R64P), DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qrs
  REAL(R64P), DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL(R64P), DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                               p, &
                                                            delz
  REAL(R64P), INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
                                                             t0c, &
                                                            den0, &
                                                              rd, &
                                                              rv, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL(R64P), DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  REAL(R64P), DIMENSION( ims:ime, jms:jme ), OPTIONAL,                  &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv
  REAL(R64P), DIMENSION( ims:ime, jms:jme ), OPTIONAL,                  &
        INTENT(INOUT) ::                                 graupel, &
                                                      graupelncv
! LOCAL VAR
  REAL(R64P), DIMENSION( its:ite , kts:kte , 3) ::                      &
                                                              rh, &
                                                              qs, &
                                                          rslope, &
                                                         rslope2, &
                                                         rslope3, &
                                                         rslopeb, &
                                                         qrs_tmp, & 
                                                            falk, &
                                                            fall, &
                                                           work1
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                         &
                                                           fallc, &
                                                           falkc, &
                                                          work1c, &
                                                          work2c, &
                                                           workr, &
                                                           worka
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                         &
                                                         den_tmp, &
                                                        delz_tmp
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                         &
                                                           pigen, &
                                                           pidep, &
                                                           pcond, &
                                                           prevp, &
                                                           psevp, &
                                                           pgevp, &
                                                           psdep, &
                                                           pgdep, &
                                                           praut, &
                                                           psaut, &
                                                           pgaut, &
                                                           piacr, &
                                                           pracw, &
                                                           praci, &
                                                           pracs, &
                                                           psacw, &
                                                           psaci, &
                                                           psacr, &
                                                           pgacw, &
                                                           pgaci, &
                                                           pgacr, &
                                                           pgacs, &
                                                           paacw, &
                                                           psmlt, &
                                                           pgmlt, &
                                                           pseml, &
                                                           pgeml
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                         &
                                                            qsum, &
                                                              xl, &
                                                             cpm, &
                                                           work2, &
                                                          denfac, &
                                                             xni, &
                                                         denqrs1, &
                                                         denqrs2, &
                                                         denqrs3, &
                                                          denqci, & 
                                                          delta2, &
                                                          delta3, &
                                                           n0sfac
  REAL(R64P), DIMENSION( its:ite ) ::                          delqrs1, &
                                                         delqrs2, &
                                                         delqrs3, &
                                                           delqi  
  REAL(R64P), DIMENSION( its:ite ) ::                        tstepsnow, &
                                                      tstepgraup
  INTEGER, DIMENSION( its:ite ) ::                         mstep, &
                                                           numdt
  LOGICAL, DIMENSION( its:ite ) ::                        flgcld
  REAL(R64P)  ::                                                        &
            cpmcal, xlcal, diffus,                                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, holdrg, supcol, supcolt, pvt,    &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, fallsum_qg,                     &
            vt2i,vt2r,vt2s,vt2g,acrfac,egs,egi,                   &
            xlwork2, factor, source, value,                       &
            xlf, pfrzdtc, pfrzdtr, supice, alpha2
  REAL(R64P)  :: vt2ave
  REAL(R64P)  :: holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                   &
            iprt, latd, lond, loop, loops, ifsat, n, idim, kdim
  INTEGER :: itest,ktest
! Temporaries used for inlining fpvs function
  REAL(R64P)  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
! variables for optimization
  REAL(R64P), DIMENSION( its:ite ) ::                             tvec1
  REAL                       ::                              temp
!
!=================================================================
!   compute internal functions
!
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!     Optimizatin : A**B => exp(log(A)*(B))
!
      diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
      viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
      xka(x,y) = 1.414e3*viscos(x,y)*y
      diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
      venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
                     /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
      conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!

!DIR$ ASSUME_ALIGNED t:64,qci:64,qrs:64,q:64,den:64,p:64,delz:64,rain:64,rainncv:64,sr:64
!DIR$ ASSUME_ALIGNED snow:64,snowncv:64,graupel:64,graupelncv:64


!DIR$ ATTRIBUTES ALIGN : 64 :: rh
!DIR$ ATTRIBUTES ALIGN : 64 :: qs
!DIR$ ATTRIBUTES ALIGN : 64 :: rslope
!DIR$ ATTRIBUTES ALIGN : 64 :: rslope2
!DIR$ ATTRIBUTES ALIGN : 64 :: rslope3
!DIR$ ATTRIBUTES ALIGN : 64 :: rslopeb
!DIR$ ATTRIBUTES ALIGN : 64 :: qrs_tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: falk
!DIR$ ATTRIBUTES ALIGN : 64 :: fall
!DIR$ ATTRIBUTES ALIGN : 64 :: work1
!DIR$ ATTRIBUTES ALIGN : 64 :: fallc
!DIR$ ATTRIBUTES ALIGN : 64 :: falkc
!DIR$ ATTRIBUTES ALIGN : 64 :: work1c
!DIR$ ATTRIBUTES ALIGN : 64 :: work2c
!DIR$ ATTRIBUTES ALIGN : 64 :: workr
!DIR$ ATTRIBUTES ALIGN : 64 :: worka
!DIR$ ATTRIBUTES ALIGN : 64 :: den_tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: delz_tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: pigen
!DIR$ ATTRIBUTES ALIGN : 64 :: pidep
!DIR$ ATTRIBUTES ALIGN : 64 :: pcond
!DIR$ ATTRIBUTES ALIGN : 64 :: prevp
!DIR$ ATTRIBUTES ALIGN : 64 :: psevp 
!DIR$ ATTRIBUTES ALIGN : 64 :: pgevp 
!DIR$ ATTRIBUTES ALIGN : 64 :: psdep 
!DIR$ ATTRIBUTES ALIGN : 64 :: pgdep 
!DIR$ ATTRIBUTES ALIGN : 64 :: praut 
!DIR$ ATTRIBUTES ALIGN : 64 :: psaut
!DIR$ ATTRIBUTES ALIGN : 64 :: pgaut
!DIR$ ATTRIBUTES ALIGN : 64 :: piacr
!DIR$ ATTRIBUTES ALIGN : 64 :: pracw
!DIR$ ATTRIBUTES ALIGN : 64 :: praci
!DIR$ ATTRIBUTES ALIGN : 64 :: pracs
!DIR$ ATTRIBUTES ALIGN : 64 :: psacw
!DIR$ ATTRIBUTES ALIGN : 64 :: psaci
!DIR$ ATTRIBUTES ALIGN : 64 :: psacr
!DIR$ ATTRIBUTES ALIGN : 64 :: pgacw
!DIR$ ATTRIBUTES ALIGN : 64 :: pgaci
!DIR$ ATTRIBUTES ALIGN : 64 :: pgacr
!DIR$ ATTRIBUTES ALIGN : 64 :: pgacs
!DIR$ ATTRIBUTES ALIGN : 64 :: paacw
!DIR$ ATTRIBUTES ALIGN : 64 :: psmlt
!DIR$ ATTRIBUTES ALIGN : 64 :: pgmlt
!DIR$ ATTRIBUTES ALIGN : 64 :: pseml
!DIR$ ATTRIBUTES ALIGN : 64 :: pgeml
!DIR$ ATTRIBUTES ALIGN : 64 :: qsum
!DIR$ ATTRIBUTES ALIGN : 64 :: xl
!DIR$ ATTRIBUTES ALIGN : 64 :: cpm
!DIR$ ATTRIBUTES ALIGN : 64 :: work2
!DIR$ ATTRIBUTES ALIGN : 64 :: denfac
!DIR$ ATTRIBUTES ALIGN : 64 :: xni
!DIR$ ATTRIBUTES ALIGN : 64 :: denqrs1
!DIR$ ATTRIBUTES ALIGN : 64 :: denqrs2
!DIR$ ATTRIBUTES ALIGN : 64 :: denqrs3
!DIR$ ATTRIBUTES ALIGN : 64 :: denqci
!DIR$ ATTRIBUTES ALIGN : 64 :: delta2
!DIR$ ATTRIBUTES ALIGN : 64 :: delta3
!DIR$ ATTRIBUTES ALIGN : 64 :: n0sfac
!DIR$ ATTRIBUTES ALIGN : 64 :: delqrs1
!DIR$ ATTRIBUTES ALIGN : 64 :: delqrs2
!DIR$ ATTRIBUTES ALIGN : 64 :: delqrs3
!DIR$ ATTRIBUTES ALIGN : 64 :: delqi  
!DIR$ ATTRIBUTES ALIGN : 64 :: tstepsnow
!DIR$ ATTRIBUTES ALIGN : 64 :: tstepgraup
      
      idim = ite-its+1
      kdim = kte-kts+1
itest=979
ktest=1
!
!----------------------------------------------------------------
!     padding 0 for negative values generated by dynamics
!
      do k = kts, kte
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
    !DIR$ PREFETCH qci:0:1
    !DIR$ PREFETCH qrs:0:1
!DIR$ ENDIF
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$ UNROLL = 2
        do i = its, ite
          qci(i,k,1) = max(qci(i,k,1),0.0_R64P)
          qrs(i,k,1) = max(qrs(i,k,1),0.0_R64P)
          qci(i,k,2) = max(qci(i,k,2),0.0_R64P)
          qrs(i,k,2) = max(qrs(i,k,2),0.0_R64P)
          qrs(i,k,3) = max(qrs(i,k,3),0.0_R64P)
        enddo
      enddo
!
!----------------------------------------------------------------
!     latent heat for phase changes and heat capacity. neglect the
!     changes during microphysical process calculation
!     emanuel(1994)
!
      do k = kts, kte
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$ UNROLL = 2
        do i = its, ite
          cpm(i,k) = cpmcal(q(i,k))
          xl(i,k) = xlcal(t(i,k))
        enddo
      enddo
      do k = kts, kte
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$ UNROLL = 2
        do i = its, ite
          delz_tmp(i,k) = delz(i,k)
          den_tmp(i,k) = den(i,k)
        enddo
      enddo
!
!----------------------------------------------------------------
!    initialize the surface rain, snow, graupel
!
      do i = its, ite
        rainncv(i) = 0._R64P
        if(PRESENT (snowncv) .AND. PRESENT (snow)) snowncv(i,lat) = 0._R64P
        if(PRESENT (graupelncv) .AND. PRESENT (graupel)) graupelncv(i,lat) = 0._R64P
        sr(i) = 0.
! new local array to catch step snow and graupel
        tstepsnow(i) = 0._R64P
        tstepgraup(i) = 0._R64P
      enddo
!
!----------------------------------------------------------------
!     compute the minor time steps.
!
      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!

!!DIR$ VECTOR ALIGNED

      do i = its, ite
        mstep(i) = 1
        flgcld(i) = .true.
      enddo

      do k = kts, kte
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$ UNROLL = 2
        do i = its, ite
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01_R64P
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1._R64P-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99_R64P*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1._R64P-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1._R64P-tr))
          endif
          qs(i,k,2) = min(qs(i,k,2),0.99_R64P*p(i,k))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$  UNROLL = 2
        do i = its, ite
          prevp(i,k) = 0._R64P
          psdep(i,k) = 0._R64P
          pgdep(i,k) = 0._R64P
          praut(i,k) = 0._R64P
          psaut(i,k) = 0._R64P
          pgaut(i,k) = 0._R64P
          pracw(i,k) = 0._R64P
          praci(i,k) = 0._R64P
          piacr(i,k) = 0._R64P
          psaci(i,k) = 0._R64P
          psacw(i,k) = 0._R64P
          pracs(i,k) = 0._R64P
          psacr(i,k) = 0._R64P
          pgacw(i,k) = 0._R64P
          paacw(i,k) = 0._R64P
          pgaci(i,k) = 0._R64P
          pgacr(i,k) = 0._R64P
          pgacs(i,k) = 0._R64P
          pigen(i,k) = 0._R64P
          pidep(i,k) = 0._R64P
          pcond(i,k) = 0._R64P
          psmlt(i,k) = 0._R64P
          pgmlt(i,k) = 0._R64P
          pseml(i,k) = 0._R64P
          pgeml(i,k) = 0._R64P
          psevp(i,k) = 0._R64P
          pgevp(i,k) = 0._R64P
          falk(i,k,1) = 0._R64P
          falk(i,k,2) = 0._R64P
          falk(i,k,3) = 0._R64P
          fall(i,k,1) = 0._R64P
          fall(i,k,2) = 0._R64P
          fall(i,k,3) = 0._R64P
          fallc(i,k) = 0._R64P
          falkc(i,k) = 0._R64P
          xni(i,k) = 1.e3_R64P
        enddo
      enddo
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7_R64P*temp,1.e3_R64P),1.e6_R64P)
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!----------------------------------------------------------------
      do k = kts, kte
!DIR$  VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$  UNROLL = 2          
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
        enddo
      enddo
      call slope_wsm6(qrs_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2,rslope3, & 
                     work1,its,ite,kts,kte)
!
      do k = kte, kts, -1

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$  UNROLL = 2
        do i = its, ite
          workr(i,k) = work1(i,k,1)
          qsum(i,k) = max( (qrs(i,k,2)+qrs(i,k,3)), 1.E-15_R64P)
          IF ( qsum(i,k) .gt. 1.e-15_R64P ) THEN
            worka(i,k) = (work1(i,k,2)*qrs(i,k,2) + work1(i,k,3)*qrs(i,k,3)) &
                      /qsum(i,k)
          ELSE
            worka(i,k) = 0._R64P
          ENDIF
          denqrs1(i,k) = den(i,k)*qrs(i,k,1)
          denqrs2(i,k) = den(i,k)*qrs(i,k,2)
          denqrs3(i,k) = den(i,k)*qrs(i,k,3)
          if(qrs(i,k,1).le.0.0_R64P) workr(i,k) = 0.0_R64P
        enddo
      enddo
      call _NISLFV_RAIN_PLM_(its,ite,kts,kte,den_tmp,denfac,t,delz_tmp,workr,denqrs1,  &
                           delqrs1,dtcld,1,1)
      call _NISLFV_RAIN_PLM6_(its,ite,kts,kte,den_tmp,denfac,t,delz_tmp,worka,         & 
                           denqrs2,denqrs3,delqrs2,delqrs3,dtcld,1,1)
      do k = kts, kte
!DIR$  VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$  UNROLL = 2
        do i = its, ite
          qrs(i,k,1) = max(denqrs1(i,k)/den(i,k),0._R64P)
          qrs(i,k,2) = max(denqrs2(i,k)/den(i,k),0._R64P)
          qrs(i,k,3) = max(denqrs3(i,k)/den(i,k),0._R64P)
          fall(i,k,1) = denqrs1(i,k)*workr(i,k)/delz(i,k)
          fall(i,k,2) = denqrs2(i,k)*worka(i,k)/delz(i,k)
          fall(i,k,3) = denqrs3(i,k)*worka(i,k)/delz(i,k)
        enddo
      enddo
      do i = its, ite
        fall(i,1,1) = delqrs1(i)/delz(i,1)/dtcld
        fall(i,1,2) = delqrs2(i)/delz(i,1)/dtcld
        fall(i,1,3) = delqrs3(i)/delz(i,1)/dtcld
      enddo
      do k = kts, kte
!DIR$  VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
!DIR$  UNROLL = 2
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
        enddo
      enddo
      call slope_wsm6(qrs_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2,rslope3, &
                     work1,its,ite,kts,kte)
!
      do k = kte, kts, -1 

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1._R64P)
          if(t(i,k).gt.t0c) then
!---------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!---------------------------------------------------------------
            xlf = xlf0
            work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
            if(qrs(i,k,2).gt.0._R64P) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2._R64P       &
                         *n0sfac(i,k)*(precs1*rslope2(i,k,2)                 &
                         +precs2*work2(i,k)*coeres)
              psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep(i),                &
                          -qrs(i,k,2)/mstep(i)),0.)
              qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
              qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
              t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)
            endif
!---------------------------------------------------------------
! pgmlt: melting of graupel [HL A23]  [LFO 47]
!       (T>T0: G->R)
!---------------------------------------------------------------
            if(qrs(i,k,3).gt.0.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgmlt(i,k) = xka(t(i,k),den(i,k))/xlf                          &
                         *(t0c-t(i,k))*(precg1*rslope2(i,k,3)                &
                         +precg2*work2(i,k)*coeres)
              pgmlt(i,k) = min(max(pgmlt(i,k)*dtcld/mstep(i),                &
                          -qrs(i,k,3)/mstep(i)),0._R64P)                          
              qrs(i,k,3) = qrs(i,k,3) + pgmlt(i,k)
              qrs(i,k,1) = qrs(i,k,1) - pgmlt(i,k)
              t(i,k) = t(i,k) + xlf/cpm(i,k)*pgmlt(i,k)
            endif
          endif
        enddo
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      do k = kte, kts, -1
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          if(qci(i,k,2).le.0._R64P) then
            work1c(i,k) = 0._R64P
          else
            xmi = den(i,k)*qci(i,k,2)/xni(i,k)
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25_R64P)
            work1c(i,k) = 1.49e4_R64P*exp(log(diameter)*(1.31_R64P))
          endif
        enddo
      enddo
!
!  forward semi-laglangian scheme (JH), PCM (piecewise constant),  (linear)
!
      do k = kte, kts, -1
        do i = its, ite
          denqci(i,k) = den(i,k)*qci(i,k,2)
        enddo
      enddo
      call _NISLFV_RAIN_PLM_(its,ite,kts,kte,den_tmp,denfac,t,delz_tmp,work1c,denqci,  &
                           delqi,dtcld,1,0)
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          qci(i,k,2) = max(denqci(i,k)/den(i,k),0._R64P)
        enddo
      enddo
 !DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do i = its, ite
        fallc(i,1) = delqi(i)/delz(i,1)/dtcld
      enddo
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do i = its, ite
        fallsum = fall(i,kts,1)+fall(i,kts,2)+fall(i,kts,3)+fallc(i,kts)
        fallsum_qsi = fall(i,kts,2)+fallc(i,kts)
        fallsum_qg = fall(i,kts,3)
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,kts)/denr*dtcld*1000._R64P + rainncv(i)
          rain(i) = fallsum*delz(i,kts)/denr*dtcld*1000._R64P + rain(i)
        endif
        if(fallsum_qsi.gt.0._R64P) then
          tstepsnow(i)   = fallsum_qsi*delz(i,kts)/denr*dtcld*1000._R64P            &
                           +tstepsnow(i)
        IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN
          snowncv(i,lat) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000._R64P            & 
                           +snowncv(i,lat)
          snow(i,lat) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000._R64P + snow(i,lat)
        ENDIF
        endif
        if(fallsum_qg.gt.0._R64P) then
          tstepgraup(i)  = fallsum_qsi*delz(i,kts)/denr*dtcld*1000._R64P            &
                           +tstepgraup(i)
        IF ( PRESENT (graupelncv) .AND. PRESENT (graupel)) THEN
          graupelncv(i,lat) = fallsum_qg*delz(i,kts)/denr*dtcld*1000._R64P          &   
                              + graupelncv(i,lat)
          graupel(i,lat) = fallsum_qg*delz(i,kts)/denr*dtcld*1000._R64P + graupel(i,lat)
        ENDIF
        endif
!       if(fallsum.gt.0.)sr(i)=(snowncv(i,lat) + graupelncv(i,lat))/(rainncv(i)+1.e-12)
        if(fallsum.gt.0._R64P)sr(i)=(tstepsnow(i) + tstepgraup(i))/(rainncv(i)+1.e-12_R64P)
      enddo
!
!---------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0.and.qci(i,k,2).gt.0._R64P) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0._R64P
          endif
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40._R64P.and.qci(i,k,1).gt.0._R64P) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0._R64P
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0._R64P.and.qci(i,k,1).gt.qmin) then
!           pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                         &
!              *den(i,k)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            supcolt=min(supcol,50._R64P)
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcolt)-1._R64P)                        &
            *den(i,k)/denr/xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! pgfrz: freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->G)
!---------------------------------------------------------------
          if(supcol.gt.0._R64P.and.qrs(i,k,1).gt.0._R64P) then
!           pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k)                    &
!                 *(exp(pfrz2*supcol)-1.)*rslope3(i,k,1)**2                    &
!                 *rslope(i,k,1)*dtcld,qrs(i,k,1))
            temp = rslope3(i,k,1)
            temp = temp*temp*rslope(i,k,1)
            supcolt=min(supcol,50._R64P)
            pfrzdtr = min(20._R64P*(pi*pi)*pfrz1*n0r*denr/den(i,k)                  &
                  *(exp(pfrz2*supcolt)-1.)*temp*dtcld,                         &
                  qrs(i,k,1))
            qrs(i,k,3) = qrs(i,k,3) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     update the slope parameters for microphysics computation
!
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
        enddo
      enddo
      call slope_wsm6(qrs_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2,rslope3, &
                     work1,its,ite,kts,kte)
!------------------------------------------------------------------
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          
        do i = its, ite
          work1(i,k,1) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k,1))
          work1(i,k,2) = diffac(xls,p(i,k),t(i,k),den(i,k),qs(i,k,2))
          work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
        enddo
      enddo
!
!===============================================================
!
! warm rain processes
!
! - follows the processes in RH83 and LFO except for autoconcersion
!
!===============================================================
!
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          if(qci(i,k,1).gt.qc0) then
            praut(i,k) = qck1*qci(i,k,1)**(7._R64P/3._R64P)
            praut(i,k) = min(praut(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pracw(i,k) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)               &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0._R64P) then
            coeres = rslope2(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1._R64P)*(precr1*rslope2(i,k,1)                 &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0._R64P) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
            else
              prevp(i,k) = min(prevp(i,k),satdt/2)
            endif
          endif
        enddo
      enddo
!
!===============================================================
!
! cold rain processes
!
! - follows the revised ice microphysics processes in HDC
! - the processes same as in RH83 and RH84  and LFO behave
!   following ice crystal hapits defined in HDC, inclduing
!   intercept parameter for snow (n0s), ice crystal number
!   concentration (ni), ice nuclei number concentration
!   (n0i), ice diameter (d)
!
!===============================================================
!
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = its, ite
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1._R64P)
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k)                                  &
!                      *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7_R64P*temp,1.e3),1.e6_R64P)
          eacrs = exp(0.07_R64P*(-supcol))
!
          xmi = den(i,k)*qci(i,k,2)/xni(i,k)
          diameter  = min(dicon * sqrt(xmi),dimax)
          vt2i = 1.49e4_R64P*diameter**1.31_R64P
          vt2r=pvtr*rslopeb(i,k,1)*denfac(i,k)
          vt2s=pvts*rslopeb(i,k,2)*denfac(i,k)
          vt2g=pvtg*rslopeb(i,k,3)*denfac(i,k)
          qsum(i,k) = max( (qrs(i,k,2)+qrs(i,k,3)), 1.E-15_R64P)
          if(qsum(i,k) .gt. 1.e-15_R64P) then
          vt2ave=(vt2s*qrs(i,k,2)+vt2g*qrs(i,k,3))/(qsum(i,k))
          else
          vt2ave=0._R64P
          endif
          if(supcol.gt.0._R64Pand.qci(i,k,2).gt.qmin) then
            if(qrs(i,k,1).gt.qcrmin) then
!-------------------------------------------------------------
! praci: Accretion of cloud ice by rain [HL A15] [LFO 25]
!        (T<T0: I->R)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k,1)+2.*diameter*rslope2(i,k,1)            &
                      +diameter**2*rslope(i,k,1)
              praci(i,k) = pi*qci(i,k,2)*n0r*abs(vt2r-vt2i)*acrfac*0.25_R64P
              praci(i,k) = min(praci(i,k),qci(i,k,2)/dtcld)
!-------------------------------------------------------------
! piacr: Accretion of rain by cloud ice [HL A19] [LFO 26]
!        (T<T0: R->S or R->G)
!-------------------------------------------------------------
              piacr(i,k) = pi**2*avtr*n0r*denr*xni(i,k)*denfac(i,k)            &
                          *g6pbr*rslope3(i,k,1)*rslope3(i,k,1)                 &
                          *rslopeb(i,k,1)/24._R64P/den(i,k)
              piacr(i,k) = min(piacr(i,k),qrs(i,k,1)/dtcld)
            endif
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by snow [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.qcrmin) then
              acrfac = 2._R64P*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      +diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                          *abs(vt2ave-vt2i)*acrfac*0.25_R64P
              psaci(i,k) = min(psaci(i,k),qci(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! pgaci: Accretion of cloud ice by graupel [HL A17] [LFO 41]
!        (T<T0: I->G)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.qcrmin) then
              egi = exp(0.07_R64P*(-supcol))
              acrfac = 2._R64P*rslope3(i,k,3)+2.*diameter*rslope2(i,k,3)            &
                      +diameter**2*rslope(i,k,3)
              pgaci(i,k) = pi*egi*qci(i,k,2)*n0g*abs(vt2ave-vt2i)*acrfac*0.25_R64P
              pgaci(i,k) = min(pgaci(i,k),qci(i,k,2)/dtcld)
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)   &    
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pgacw: Accretion of cloud water by graupel [HL A6] [LFO 40]
!        (T<T0: C->G, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pgacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)               &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! paacw: Accretion of cloud water by averaged snow/graupel 
!        (T<T0: C->G or S, and T>=T0: C->R) 
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qrs(i,k,3).gt.qcrmin) then
            paacw(i,k) = (qrs(i,k,2)*psacw(i,k)+qrs(i,k,3)*pgacw(i,k))         & 
                        /(qsum(i,k))
           endif      
!-------------------------------------------------------------
! pracs: Accretion of snow by rain [HL A11] [LFO 27]
!         (T<T0: S->G)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            if(supcol.gt.0) then
              acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)*rslope(i,k,1)          &
                      +2.*rslope3(i,k,2)*rslope2(i,k,2)*rslope2(i,k,1)         &
                      +.5_R64P*rslope2(i,k,2)*rslope2(i,k,2)*rslope3(i,k,1)
              pracs(i,k) = pi**2*n0r*n0s*n0sfac(i,k)*abs(vt2r-vt2ave)          &
                          *(dens/den(i,k))*acrfac
              pracs(i,k) = min(pracs(i,k),qrs(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! psacr: Accretion of rain by snow [HL A10] [LFO 28]
!         (T<T0:R->S or R->G) (T>=T0: enhance melting of snow)
!-------------------------------------------------------------
            acrfac = 5._R64P*rslope3(i,k,1)*rslope3(i,k,1)*rslope(i,k,2)            &
                    +2._R64P*rslope3(i,k,1)*rslope2(i,k,1)*rslope2(i,k,2)           &
                    +.5_R64P*rslope2(i,k,1)*rslope2(i,k,1)*rslope3(i,k,2)
            psacr(i,k) = pi**2*n0r*n0s*n0sfac(i,k)*abs(vt2ave-vt2r)            &
                        *(denr/den(i,k))*acrfac
            psacr(i,k) = min(psacr(i,k),qrs(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pgacr: Accretion of rain by graupel [HL A12] [LFO 42]
!         (T<T0: R->G) (T>=T0: enhance melting of graupel)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            acrfac = 5._R64P*rslope3(i,k,1)*rslope3(i,k,1)*rslope(i,k,3)            &
                    +2._R64P*rslope3(i,k,1)*rslope2(i,k,1)*rslope2(i,k,3)           &
                    +.5_R64P*rslope2(i,k,1)*rslope2(i,k,1)*rslope3(i,k,3)
            pgacr(i,k) = pi**2*n0r*n0g*abs(vt2ave-vt2r)*(denr/den(i,k))        &
                        *acrfac
            pgacr(i,k) = min(pgacr(i,k),qrs(i,k,1)/dtcld)
          endif
!
!-------------------------------------------------------------
! pgacs: Accretion of snow by graupel [HL A13] [LFO 29]
!        (S->G): This process is eliminated in V3.0 with the 
!        new combined snow/graupel fall speeds
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qrs(i,k,2).gt.qcrmin) then
            pgacs(i,k) = 0._R64P
          endif
          if(supcol.le.0) then
            xlf = xlf0
!-------------------------------------------------------------
! pseml: Enhanced melting of snow by accretion of water [HL A34]
!        (T>=T0: S->R)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0._R64P)                                               &
              pseml(i,k) = min(max(cliq*supcol*(paacw(i,k)+psacr(i,k))         &
                          /xlf,-qrs(i,k,2)/dtcld),0._R64P)
!-------------------------------------------------------------
! pgeml: Enhanced melting of graupel by accretion of water [HL A24] [RH84 A21-A22]
!        (T>=T0: G->R)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0._R64P)                                               &
              pgeml(i,k) = min(max(cliq*supcol*(paacw(i,k)+pgacr(i,k))         &
                          /xlf,-qrs(i,k,3)/dtcld),0._R64P)
          endif
          if(supcol.gt.0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
                pidep(i,k) = max(max(pidep(i,k),satdt/2),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)/dtcld)
              else
                pidep(i,k) = min(min(pidep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (T<T0: V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k,2)   &    
                           + precs2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)/dtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt/2),supice)
              else
                psdep(i,k) = min(min(psdep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt))          &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pgdep: deposition/sublimation rate of graupel [HL A21] [LFO 46]
!        (T<T0: V->G or G->V)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgdep(i,k) = (rh(i,k,2)-1.)*(precg1*rslope2(i,k,3)               &
                              +precg2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              if(pgdep(i,k).lt.0.) then
                pgdep(i,k) = max(pgdep(i,k),-qrs(i,k,3)/dtcld)
                pgdep(i,k) = max(max(pgdep(i,k),satdt/2),supice)
              else
                pgdep(i,k) = min(min(pgdep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)+pgdep(i,k)).ge.          &
                abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL 50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)-pgdep(i,k)
              xni0 = 1.e3_R64P*exp(0.1*supcol)
              roqi0 = 4.92e-11_R64P*xni0**1.33_R64P
              pigen(i,k) = max(0._R64P,(roqi0/den(i,k)-max(qci(i,k,2),0._R64P))/dtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
              psaut(i,k) = max(0._R64P,(qci(i,k,2)-qimax)/dtcld)
            endif
!
!-------------------------------------------------------------
! pgaut: conversion(aggregation) of snow to graupel [HL A4] [LFO 37]
!        (T<T0: S->G)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0.) then
              alpha2 = 1.e-3_R64P*exp(0.09_R64P*(-supcol))
              pgaut(i,k) = min(max(0._R64P,alpha2*(qrs(i,k,2)-qs0)),qrs(i,k,2)/dtcld)
            endif
          endif
!
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>=T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psevp(i,k) = (rh(i,k,1)-1.)*n0sfac(i,k)*(precs1                  &
                           *rslope2(i,k,2)+precs2*work2(i,k)                   &
                           *coeres)/work1(i,k,1)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0._R64P)
            endif
!-------------------------------------------------------------
! pgevp: Evaporation of melting graupel [HL A25] [RH84 A19]
!       (T>=T0: G->V)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0..and.rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgevp(i,k) = (rh(i,k,1)-1.)*(precg1*rslope2(i,k,3)               &
                         +precg2*work2(i,k)*coeres)/work1(i,k,1)
              pgevp(i,k) = min(max(pgevp(i,k),-qrs(i,k,3)/dtcld),0._R64P)
            endif
          endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
          delta2(i,k)=0._R64P
          delta3(i,k)=0._R64P
          if(qrs(i,k,1).lt.1.e-4.and.qrs(i,k,2).lt.1.e-4) delta2(i,k)=1._R64P
          if(qrs(i,k,1).lt.1.e-4) delta3(i,k)=1._R64P
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     cloud water
!
          value = max(qmin,qci(i,k,1))
          source = (praut(i,k)+pracw(i,k)+paacw(i,k)+paacw(i,k))*dtcld
          if(t(i,k).le.t0c) then
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
            endif
          else
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
            endif
          endif
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     cloud ice
!
          if(t(i,k).le.t0c) then
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)-pigen(i,k)-pidep(i,k)+praci(i,k)+psaci(i,k)   &
                     +pgaci(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
            endif
          endif
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     rain
!
          value = max(qmin,qrs(i,k,1))
          if(t(i,k).le.t0c) then
            source = (-praut(i,k)-prevp(i,k)-pracw(i,k)+piacr(i,k)+psacr(i,k)  &
                      +pgacr(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
            endif
          else
            source = (-paacw(i,k)-praut(i,k)+pseml(i,k)+pgeml(i,k)-pracw(i,k)  &
                      -paacw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
            endif
          endif
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     snow
!
          if(t(i,k).le.t0c) then
            value = max(qmin,qrs(i,k,2))
            source = -(psdep(i,k)+psaut(i,k)-pgaut(i,k)+paacw(i,k)+piacr(i,k)  &
                     *delta3(i,k)+praci(i,k)*delta3(i,k)                       &
                     -pracs(i,k)*(1.-delta2(i,k))                              &
                     +psacr(i,k)*delta2(i,k)+psaci(i,k)-pgacs(i,k) )*dtcld
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              pgaut(i,k) = pgaut(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
            endif
          else
            value = max(qcrmin,qrs(i,k,2))
            source=(pgacs(i,k)-pseml(i,k)-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              psevp(i,k) = psevp(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
            endif
          endif
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     graupel
!
          if(t(i,k).le.t0c) then
            value = max(qmin,qrs(i,k,3))
            source = -(pgdep(i,k)+pgaut(i,k)                                   &
                     +piacr(i,k)*(1.-delta3(i,k))+praci(i,k)*(1.-delta3(i,k))  &
                     +psacr(i,k)*(1.-delta2(i,k))+pracs(i,k)*(1.-delta2(i,k))  &
                     +pgaci(i,k)+paacw(i,k)+pgacr(i,k)+pgacs(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgdep(i,k) = pgdep(i,k)*factor
              pgaut(i,k) = pgaut(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
            endif
          else
            value = max(qcrmin,qrs(i,k,3))
            source=-(pgacs(i,k)+pgevp(i,k)+pgeml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              pgevp(i,k) = pgevp(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
            endif
          endif
        enddo
      enddo
      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
!
!     update
!
          if(t(i,k).le.t0c) then
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pgdep(i,k)+pigen(i,k)+pidep(i,k))
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                           +paacw(i,k)+paacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                           +prevp(i,k)-piacr(i,k)-pgacr(i,k)                   &
                           -psacr(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+praci(i,k)                 &
                           +psaci(i,k)+pgaci(i,k)-pigen(i,k)-pidep(i,k))       &
                           *dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)+paacw(i,k)      &
                           -pgaut(i,k)+piacr(i,k)*delta3(i,k)                  &
                           +praci(i,k)*delta3(i,k)+psaci(i,k)-pgacs(i,k)       &
                           -pracs(i,k)*(1.-delta2(i,k))+psacr(i,k)*delta2(i,k))&
                           *dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgdep(i,k)+pgaut(i,k)                 &
                           +piacr(i,k)*(1.-delta3(i,k))                        &
                           +praci(i,k)*(1.-delta3(i,k))                        &
                           +psacr(i,k)*(1.-delta2(i,k))                        &
                           +pracs(i,k)*(1.-delta2(i,k))+pgaci(i,k)+paacw(i,k)  &
                           +pgacr(i,k)+pgacs(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pgdep(i,k)+pidep(i,k)+pigen(i,k))       &
                      -xl(i,k)*prevp(i,k)-xlf*(piacr(i,k)+paacw(i,k)           &
                      +paacw(i,k)+pgacr(i,k)+psacr(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
            work2(i,k)=-(prevp(i,k)+psevp(i,k)+pgevp(i,k))
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                    +paacw(i,k)+paacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                    +prevp(i,k)+paacw(i,k)+paacw(i,k)-pseml(i,k)               &
                    -pgeml(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psevp(i,k)-pgacs(i,k)                 &
                    +pseml(i,k))*dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgacs(i,k)+pgevp(i,k)                 &
                    +pgeml(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k)+pgevp(i,k))              &
                      -xlf*(pseml(i,k)+pgeml(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          
        do i = its, ite
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99_R64P*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1._R64P-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1._R64P-tr))
          endif
          qs(i,k,2) = min(qs(i,k,2),0.99_R64P*p(i,k))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          
        do i = its, ite
          work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                          &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k) = q(i,k)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte


!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))          

        do i = its, ite
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0_R64P
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0_R64P
        enddo
      enddo
      enddo                  ! big loops
  END SUBROUTINE wsm62d
!--------------------------------------------------------------------------
      subroutine slope_wsm6(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL(R64P), DIMENSION( its:ite , kts:kte,3) ::                                     &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &                                                 
                                                                      rslope2, &                                                 
                                                                      rslope3, &                                                 
                                                                           vt
  REAL(R64P), DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar, lamdas, lamdag, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,t:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64

      do k = kts, kte

!DIR$ VECTOR ALIGNED
!DIR$  SIMD VECTORLENGTHFOR(REAL(KIND=8))

        do i = its, ite
          supcol = t0c-t(i,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1._R64P)
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k))
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k),n0sfac(i,k))
            rslopeb(i,k,2) = rslope(i,k,2)**bvts
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
          if(qrs(i,k,3).le.qcrmin)then
            rslope(i,k,3) = rslopegmax
            rslopeb(i,k,3) = rslopegbmax
            rslope2(i,k,3) = rslopeg2max
            rslope3(i,k,3) = rslopeg3max
          else
            rslope(i,k,3) = 1./lamdag(qrs(i,k,3),den(i,k))
            rslopeb(i,k,3) = rslope(i,k,3)**bvtg
            rslope2(i,k,3) = rslope(i,k,3)*rslope(i,k,3)
            rslope3(i,k,3) = rslope2(i,k,3)*rslope(i,k,3)
          endif
          vt(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)
          vt(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)
          vt(i,k,3) = pvtg*rslopeb(i,k,3)*denfac(i,k)
          if(qrs(i,k,1).le.0.0) vt(i,k,1) = 0.0
          if(qrs(i,k,2).le.0.0) vt(i,k,2) = 0.0
          if(qrs(i,k,3).le.0.0) vt(i,k,3) = 0.0
        enddo
      enddo
  END subroutine slope_wsm6
!-----------------------------------------------------------------------------
#ifndef IINSIDE
      subroutine slope_rain(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   & 
                            vt,kts,kte)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_rain 
  IMPLICIT NONE
  INTEGER       ::               kts,kte
  REAL(R64P), DIMENSION( kts:kte) ::                                                 &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &      
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  REAL(R64P), DIMENSION( kts:kte ) ::                                                &
                                                                       n0sfac
  REAL(R64P)       ::  lamdar, x, y, z, supcol
  integer :: k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,t:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
          if(qrs(k).le.qcrmin)then
            rslope(k) = rslopermax
            rslopeb(k) = rsloperbmax
            rslope2(k) = rsloper2max
            rslope3(k) = rsloper3max
          else
            rslope(k) = 1._R64P/lamdar(qrs(k),den(k))
            rslopeb(k) = rslope(k)**bvtr
            rslope2(k) = rslope(k)*rslope(k)
            rslope3(k) = rslope2(k)*rslope(k)
          endif
          vt(k) = pvtr*rslopeb(k)*denfac(k)
          if(qrs(k).le.0.0) vt(k) = 0.0_R64P
      enddo
  END subroutine slope_rain
!------------------------------------------------------------------------------
      subroutine slope_snow(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,kts,kte)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_snow
  IMPLICIT NONE
  INTEGER       ::               kts,kte
  REAL(R64P), DIMENSION( kts:kte) ::                                                 &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &  
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  REAL(R64P), DIMENSION( kts:kte ) ::                                                &
                                                                       n0sfac
  REAL(R64P)       ::  lamdas, x, y, z, supcol
  integer :: k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,t:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
          supcol = t0c-t(k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(k) = max(min(exp(alpha*supcol),n0smax/n0s),1._R64P)
          if(qrs(k).le.qcrmin)then
            rslope(k) = rslopesmax
            rslopeb(k) = rslopesbmax
            rslope2(k) = rslopes2max
            rslope3(k) = rslopes3max
          else
            rslope(k) = 1./lamdas(qrs(k),den(k),n0sfac(k))
            rslopeb(k) = rslope(k)**bvts
            rslope2(k) = rslope(k)*rslope(k)
            rslope3(k) = rslope2(k)*rslope(k)
          endif
          vt(k) = pvts*rslopeb(k)*denfac(k)
          if(qrs(k).le.0.0) vt(k) = 0.0
      enddo
  END subroutine slope_snow
!----------------------------------------------------------------------------------
      subroutine slope_graup(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,  &
                            vt,kts,kte)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_graup
  IMPLICIT NONE
  INTEGER       :: kts,kte
  REAL(R64P), DIMENSION( kts:kte) ::                                                 &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &  
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  REAL(R64P), DIMENSION( kts:kte ) ::                                                &
                                                                       n0sfac
  REAL(R64P)       ::  lamdag, x, y, z, supcol
  integer :: j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,t:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          if(qrs(k).le.qcrmin)then
            rslope(k) = rslopegmax
            rslopeb(k) = rslopegbmax
            rslope2(k) = rslopeg2max
            rslope3(k) = rslopeg3max
          else
            rslope(k) = 1./lamdag(qrs(k),den(k))
            rslopeb(k) = rslope(k)**bvtg
            rslope2(k) = rslope(k)*rslope(k)
            rslope3(k) = rslope2(k)*rslope(k)
          endif
          vt(k) = pvtg*rslopeb(k)*denfac(k)
          if(qrs(k).le.0.0) vt(k) = 0.0
      enddo
  END subroutine slope_graup
!---------------------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm(its,ite,kts,kte,denl,denfacl,tkl,dzl,wwl,rql,precip,dt,id,iter)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  its,ite,kts,kte,id
      real(R64P) ::  dt
      real(R64P), dimension(its:ite,kts:kte) ::  dzl,wwl,rql
      real(R64P), dimension(its,ite)         ::  precip
      real(R64P), dimension(its:ite,kts:kte) ::  denl,denfacl,tkl
!
      integer  i,k,n,m,kk,kb,kt,iter
      real(R64P) ::  tl,tl2,qql,dql,qqd
      real(R64P) ::  th,th2,qqh,dqh
      real(R64P) ::  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real(R64P) ::  allold, allnew, zz, dzamin, cflmax, decfl
      real(R64P), dimension(kts:kte)   ::  dz, ww, qq, wd, wa, was
      real(R64P), dimension(kts:kte)   ::  den, denfac, tk
      real(R64P), dimension(kts:kte+1) ::  wi, zi, za
      real(R64P), dimension(kts:kte)   ::  qn, qr,tmp,tmp1,tmp2,tmp3
      real(R64P), dimension(kts:kte+1) ::  dza, qa, qmi, qpi
!

!DIR$ ASSUME_ALIGNED denl:64,denfacl:64,tkl:64,dzl:64,wwl:64,rql:64,precip:64
      
!DIR$ ATTRIBUTES ALIGN : 64 :: dz
!DIR$ ATTRIBUTES ALIGN : 64 :: ww
!DIR$ ATTRIBUTES ALIGN : 64 :: qq
!DIR$ ATTRIBUTES ALIGN : 64 :: wd
!DIR$ ATTRIBUTES ALIGN : 64 :: wa
!DIR$ ATTRIBUTES ALIGN : 64 :: was
!DIR$ ATTRIBUTES ALIGN : 64 :: den
!DIR$ ATTRIBUTES ALIGN : 64 :: denfac
!DIR$ ATTRIBUTES ALIGN : 64 :: tk
!DIR$ ATTRIBUTES ALIGN : 64 :: wi
!DIR$ ATTRIBUTES ALIGN : 64 :: zi
!DIR$ ATTRIBUTES ALIGN : 64 :: za
!DIR$ ATTRIBUTES ALIGN : 64 :: qn
!DIR$ ATTRIBUTES ALIGN : 64 :: qr
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp1
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp3
!DIR$ ATTRIBUTES ALIGN : 64 :: dza
!DIR$ ATTRIBUTES ALIGN : 64 :: qa
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi      
      precip(:) = 0._R64P
!
      i_loop : do i=its,ite
! -----------------------------------
      dz(:) = dzl(i,:)
      qq(:) = rql(i,:)
      ww(:) = wwl(i,:)
      den(:) = denl(i,:)
      denfac(:) = denfacl(i,:)
      tk(:) = tkl(i,:)
! skip for no precipitation for all layers
      allold = 0.0
      do k=kts,kte
        allold = allold + qq(k)
      enddo
      if(allold.le.0.0) then
        cycle i_loop
      endif
!
! compute interface values
      zi(kts)=0.0
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do k=kts,kte
        zi(k+1) = zi(k)+dz(k)
      enddo
!
! save departure wind
      wd(:) = ww(:)
      n=1
 100  continue
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
      wi(kts) = ww(kts)
      wi(kte+1) = ww(kte)
      do k=kts+1,kte
        wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
      enddo
! 3rd order interpolation to get wi
      fa1 = 9./16.
      fa2 = 1./16.
      wi(kts) = ww(kts)
      wi(kts+1) = 0.5_R64P*(ww(kts+1)+ww(kts))
      do k=kts+2,kte-1
        wi(k) = fa1*(ww(k)+ww(k-1))-fa2*(ww(k+1)+ww(k-2))
      enddo
      wi(kte) = 0.5*(ww(kte)+ww(kte-1))
      wi(kte+1) = ww(kte)
!
! terminate of top of raingroup
      do k=kts+1,kte
        if( ww(k).eq.0.0 ) wi(k)=ww(k-1)
      enddo
!
! diffusivity of wi
      con1 = 0.05_R64P
      do k=kte,kts,-1
        decfl = (wi(k+1)-wi(k))*dt/dz(k)
        if( decfl .gt. con1 ) then
          wi(k) = wi(k+1) - con1*dz(k)/dt
        endif
      enddo
! compute arrival point
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))      
      do k=kts,kte+1
        za(k) = zi(k) - wi(k)*dt
      enddo
!
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))     
      do k=kts,kte
        dza(k) = za(k+1)-za(k)
      enddo
      dza(kte+1) = zi(kte+1) - za(kte+1)
!
! computer deformation at arrival point
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))      
      do k=kts,kte
        qa(k) = qq(k)*dz(k)/dza(k)
        qr(k) = qa(k)/den(k)
      enddo
      qa(kte+1) = 0.0
!     call maxmin(kte-kts+1,1,qa,' arrival points ')
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_rain(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,kts,kte)
        if( n.ge.2 ) wa(kts:kte)=0.5*(wa(kts:kte)+was(kts:kte))
        do k=kts,kte
!#ifdef DEBUG
!        print*,' slope_wsm3 ',qr(k)*1000.,den(k),denfac(k),tk(k),tmp(k),tmp1(k),tmp2(k),ww(k),wa(k)
!#endif
! mean wind is average of departure and new arrival winds
          ww(k) = 0.5_R64P* ( wd(k)+wa(k) )
        enddo
        was(:) = wa(:)
        n=n+1
        go to 100
      endif
!
! estimate values at arrival cell interface with monotone
      do k=kts+1,kte
        dip=(qa(k+1)-qa(k))/(dza(k+1)+dza(k))
        dim=(qa(k)-qa(k-1))/(dza(k-1)+dza(k))
        if( dip*dim.le.0.0 ) then
          qmi(k)=qa(k)
          qpi(k)=qa(k)
        else
          qpi(k)=qa(k)+0.5*(dip+dim)*dza(k)
          qmi(k)=2.0*qa(k)-qpi(k)
          if( qpi(k).lt.0.0 .or. qmi(k).lt.0.0 ) then
            qpi(k) = qa(k)
            qmi(k) = qa(k)
          endif
        endif
      enddo
      qpi(kts)=qa(kts)
      qmi(kts)=qa(kts)
      qmi(kte+1)=qa(kte+1)
      qpi(kte+1)=qa(kte+1)
!
! interpolation to regular point
      qn = 0.0
      kb=kts
      kt=kts
      intp : do k=kts,kte
             kb=max(kb-1,kts)
             kt=max(kt-1,kts)
! find kb and kt
             if( zi(k).ge.za(kte+1) ) then
               exit intp
             else
               find_kb : do kk=kb,kte
                         if( zi(k).le.za(kk+1) ) then
                           kb = kk
                           exit find_kb
                         else
                           cycle find_kb
                         endif
               enddo find_kb
               find_kt : do kk=kt,kte
                         if( zi(k+1).le.za(kk) ) then
                           kt = kk
                           exit find_kt
                         else
                           cycle find_kt
                         endif
               enddo find_kt
               kt = kt - 1
! compute q with piecewise constant method
               if( kt.eq.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 th=(zi(k+1)-za(kb))/dza(kb)
                 tl2=tl*tl
                 th2=th*th
                 qqd=0.5_R64P*(qpi(kb)-qmi(kb))
                 qqh=qqd*th2+qmi(kb)*th
                 qql=qqd*tl2+qmi(kb)*tl
                 qn(k) = (qqh-qql)/(th-tl)
               else if( kt.gt.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 tl2=tl*tl
                 qqd=0.5_R64P*(qpi(kb)-qmi(kb))
                 qql=qqd*tl2+qmi(kb)*tl
                 dql = qa(kb)-qql
                 zsum  = (1.-tl)*dza(kb)
                 qsum  = dql*dza(kb)
                 if( kt-kb.gt.1 ) then
                 do m=kb+1,kt-1
                   zsum = zsum + dza(m)
                   qsum = qsum + qa(m) * dza(m)
                 enddo
                 endif
                 th=(zi(k+1)-za(kt))/dza(kt)
                 th2=th*th
                 qqd=0.5_R64P*(qpi(kt)-qmi(kt))
                 dqh=qqd*th2+qmi(kt)*th
                 zsum  = zsum + th*dza(kt)
                 qsum  = qsum + dqh*dza(kt)
                 qn(k) = qsum/zsum
               endif
               cycle intp
             endif
!
       enddo intp
!
! rain out
      sum_precip: do k=kts,kte
                    if( za(k).lt.0.0 .and. za(k+1).lt.0.0 ) then
                      precip(i) = precip(i) + qa(k)*dza(k)
                      cycle sum_precip
                    else if ( za(k).lt.0.0 .and. za(k+1).ge.0.0 ) then
                      precip(i) = precip(i) + qa(k)*(0.0-za(k))
                      exit sum_precip
                    endif
                    exit sum_precip
      enddo sum_precip
!
! replace the new values


!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      rql(i,:) = qn(:)
!
! ----------------------------------
      enddo i_loop
!
  END SUBROUTINE nislfv_rain_plm
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm6(its,ite,kts,kte,denl,denfacl,tkl,dzl,wwl,rql,rql2, precip1, precip2,dt,id,iter)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  its,ite,kts,kte,id
      real(R64P) ::  dt
      real(R64P), dimension(its:ite,kts:kte) ::  dzl,wwl,rql,rql2
      real(R64P), dimension(its:ite)         ::  precip,precip1,precip2
      real(R64P), dimension(its:ite,kts:kte) ::  denl,denfacl,tkl
!
      integer  i,k,n,m,kk,kb,kt,iter,ist
      real(R64P) ::  tl,tl2,qql,dql,qqd
      real(R64P) ::  th,th2,qqh,dqh
      real(R64P) ::  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real(R64P) ::  allold, allnew, zz, dzamin, cflmax, decfl
      real(R64P), dimension(kts:kte)   ::   dz, ww, qq, qq2, wd, wa, wa2, was
      real(R64P), dimension(kts:kte)   ::   den, denfac, tk
      real(R64P), dimension(kts:kte+1) ::   wi, zi, za
      real(R64P), dimension(kts,kte)   ::   qn, qr,qr2,tmp,tmp1,tmp2,tmp3
      real(R64P), dimension(kts:kte+1) ::   dza, qa, qa2,qmi, qpi
!

!DIR$ ASSUME_ALIGNED denl:64,denfacl:64,tkl:64,dzl:64,wwl:64,rql:64,rql2:64,precip1:64,precip2:64
      
!DIR$ ATTRIBUTES ALIGN : 64 :: dz
!DIR$ ATTRIBUTES ALIGN : 64 :: ww
!DIR$ ATTRIBUTES ALIGN : 64 :: qq
!DIR$ ATTRIBUTES ALIGN : 64 :: wd
!DIR$ ATTRIBUTES ALIGN : 64 :: wa
!DIR$ ATTRIBUTES ALIGN : 64 :: was
!DIR$ ATTRIBUTES ALIGN : 64 :: den
!DIR$ ATTRIBUTES ALIGN : 64 :: denfac
!DIR$ ATTRIBUTES ALIGN : 64 :: tk
!DIR$ ATTRIBUTES ALIGN : 64 :: wi
!DIR$ ATTRIBUTES ALIGN : 64 :: zi
!DIR$ ATTRIBUTES ALIGN : 64 :: za
!DIR$ ATTRIBUTES ALIGN : 64 :: qn
!DIR$ ATTRIBUTES ALIGN : 64 :: qr
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp1
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp3
!DIR$ ATTRIBUTES ALIGN : 64 :: dza
!DIR$ ATTRIBUTES ALIGN : 64 :: qa
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi   

      precip(:) = 0.0_R64P
      precip1(:) = 0.0_R64P
      precip2(:) = 0.0_R64P
!
      i_loop : do i=its,ite
! -----------------------------------
      dz(:) = dzl(i,:)
      qq(:) = rql(i,:)
      qq2(:) = rql2(i,:)
      ww(:) = wwl(i,:)
      den(:) = denl(i,:)
      denfac(:) = denfacl(i,:)
      tk(:) = tkl(i,:)
! skip for no precipitation for all layers
      allold = 0.0_R64P
!DIR$ SIMD REDUCTION(+,allold)
      do k=kts,kte
        allold = allold + qq(k)
      enddo
      if(allold.le.0.0) then
        cycle i_loop
      endif
!
! compute interface values
      zi(kts)=0.0_R64P
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do k=kts,kte
        zi(k+1) = zi(k)+dz(k)
      enddo
!
! save departure wind
      wd(:) = ww(:)
      n=1
 100  continue
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
      wi(kts) = ww(kts)
      wi(kte+1) = ww(kte)
      do k=kts+1,kte
        wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
      enddo
! 3rd order interpolation to get wi
      fa1 = 9._R64P/16._R64P
      fa2 = 1._R64P/16._R64P
      wi(kts) = ww(kts)
      wi(kts+1) = 0.5*(ww(kts+1)+ww(kts))
      do k=kts+2,kte-1
        wi(k) = fa1*(ww(k)+ww(k-1))-fa2*(ww(k+1)+ww(k-2))
      enddo
      wi(kte) = 0.5_R64P*(ww(kte)+ww(kte-1))
      wi(kte+1) = ww(kte)
!
! terminate of top of raingroup
      do k=kts+1,kte
        if( ww(k).eq.0.0 ) wi(k)=ww(k-1)
      enddo
!
! diffusivity of wi
      con1 = 0.05_R64P
      do k=kte,kts,-1
        decfl = (wi(k+1)-wi(k))*dt/dz(k)
        if( decfl .gt. con1 ) then
          wi(k) = wi(k+1) - con1*dz(k)/dt
        endif
      enddo
! compute arrival point
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do k=kts,kte+1
        za(k) = zi(k) - wi(k)*dt
      enddo
!
      do k=kts,kte
        dza(k) = za(k+1)-za(k)
      enddo
      dza(kte+1) = zi(kte+1) - za(kte+1)
!
! computer deformation at arrival point
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do k=kts,kte
        qa(k) = qq(k)*dz(k)/dza(k)
        qa2(k) = qq2(k)*dz(k)/dza(k)
        qr(k) = qa(k)/den(k)
        qr2(k) = qa2(k)/den(k)
      enddo
      qa(kte+1) = 0.0
      qa2(kte+1) = 0.0
!     call maxmin(kte-kts+1,1,qa,' arrival points ')
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_snow(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,kts,kte)
        call slope_graup(qr2,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa2,kts,kte)
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do k = kts, kte
          tmp(k) = max((qr(k)+qr2(k)), 1.E-15_R64P)
          IF ( tmp(k) .gt. 1.e-15_R64P ) THEN
            wa(k) = (wa(k)*qr(k) + wa2(k)*qr2(k))/tmp(k)
          ELSE
            wa(k) = 0.
          ENDIF
        enddo
        if( n.ge.2 ) wa(kts:kte)=0.5_R64P*(wa(kts:kte)+was(kts:kte))
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do k=kts,kte
!#ifdef DEBUG
!        print*,' slope_wsm3 ',qr(k)*1000.,den(k),denfac(k),tk(k),tmp(k),tmp1(k),tmp2(k), &
!           ww(k),wa(k)
!#endif
! mean wind is average of departure and new arrival winds
          ww(k) = 0.5_R64P* ( wd(k)+wa(k) )
        enddo
        was(:) = wa(:)
        n=n+1
        go to 100
      endif
      ist_loop : do ist = 1, 2
      if (ist.eq.2) then
       qa(:) = qa2(:)
      endif
!
      precip(i) = 0.
!
! estimate values at arrival cell interface with monotone
      do k=kts+1,kte
        dip=(qa(k+1)-qa(k))/(dza(k+1)+dza(k))
        dim=(qa(k)-qa(k-1))/(dza(k-1)+dza(k))
        if( dip*dim.le.0.0 ) then
          qmi(k)=qa(k)
          qpi(k)=qa(k)
        else
          qpi(k)=qa(k)+0.5*(dip+dim)*dza(k)
          qmi(k)=2.0*qa(k)-qpi(k)
          if( qpi(k).lt.0.0 .or. qmi(k).lt.0.0 ) then
            qpi(k) = qa(k)
            qmi(k) = qa(k)
          endif
        endif
      enddo
      qpi(kts)=qa(kts)
      qmi(kts)=qa(kts)
      qmi(kte+1)=qa(kte+1)
      qpi(kte+1)=qa(kte+1)
!
! interpolation to regular point
      qn = 0.0
      kb=kts
      kt=kts
      intp : do k=kts,kte
             kb=max(kb-1,kts)
             kt=max(kt-1,kts)
! find kb and kt
             if( zi(k).ge.za(kte+1) ) then
               exit intp
             else
               find_kb : do kk=kb,kte
                         if( zi(k).le.za(kk+1) ) then
                           kb = kk
                           exit find_kb
                         else
                           cycle find_kb
                         endif
               enddo find_kb
               find_kt : do kk=kt,kte
                         if( zi(k+1).le.za(kk) ) then
                           kt = kk
                           exit find_kt
                         else
                           cycle find_kt
                         endif
               enddo find_kt
               kt = kt - 1
! compute q with piecewise constant method
               if( kt.eq.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 th=(zi(k+1)-za(kb))/dza(kb)
                 tl2=tl*tl
                 th2=th*th
                 qqd=0.5_R64P*(qpi(kb)-qmi(kb))
                 qqh=qqd*th2+qmi(kb)*th
                 qql=qqd*tl2+qmi(kb)*tl
                 qn(k) = (qqh-qql)/(th-tl)
               else if( kt.gt.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 tl2=tl*tl
                 qqd=0.5_R64P*(qpi(kb)-qmi(kb))
                 qql=qqd*tl2+qmi(kb)*tl
                 dql = qa(kb)-qql
                 zsum  = (1._R64P-tl)*dza(kb)
                 qsum  = dql*dza(kb)
                 if( kt-kb.gt.1 ) then
                 do m=kb+1,kt-1
                   zsum = zsum + dza(m)
                   qsum = qsum + qa(m) * dza(m)
                 enddo
                 endif
                 th=(zi(k+1)-za(kt))/dza(kt)
                 th2=th*th
                 qqd=0.5_R64P*(qpi(kt)-qmi(kt))
                 dqh=qqd*th2+qmi(kt)*th
                 zsum  = zsum + th*dza(kt)
                 qsum  = qsum + dqh*dza(kt)
                 qn(k) = qsum/zsum
               endif
               cycle intp
             endif
!
       enddo intp
!
! rain out
      sum_precip: do k=kts,kte
                    if( za(k).lt.0.0 .and. za(k+1).lt.0.0 ) then
                      precip(i) = precip(i) + qa(k)*dza(k)
                      cycle sum_precip
                    else if ( za(k).lt.0.0 .and. za(k+1).ge.0.0 ) then
                      precip(i) = precip(i) + qa(k)*(0.0-za(k))
                      exit sum_precip
                    endif
                    exit sum_precip
      enddo sum_precip
!
! replace the new values
      if(ist.eq.1) then
        rql(i,:) = qn(:)
        precip1(i) = precip(i)
      else
        rql2(i,:) = qn(:)
        precip2(i) = precip(i)
      endif
      enddo ist_loop
!
! ----------------------------------
      enddo i_loop
!
  END SUBROUTINE nislfv_rain_plm6
!---------------------------------------------------------------------------------
#else
!-------------------------------------------------------------------
      subroutine slope_rain_ii(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,&
                            vt,its,ite,kts,kte,lmask)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_rain_ii
  IMPLICIT NONE
  INTEGER       :: its,ite,kts,kte
  REAL(R64P), DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &      
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  LOGICAL :: lmask(its:ite)
  REAL(R64P)       ::  lamdar, x, y, z, supcol
  integer :: i, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
        do i = its, ite
         if (lmask(i)) then
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopermax
            rslopeb(i,k) = rsloperbmax
            rslope2(i,k) = rsloper2max
            rslope3(i,k) = rsloper3max
          else
            rslope(i,k) = 1./lamdar(qrs(i,k),den(i,k))
            rslopeb(i,k) = rslope(i,k)**bvtr
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvtr*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
         endif
        enddo
      enddo
  END subroutine slope_rain_ii
!------------------------------------------------------------------------------
      subroutine slope_snow_ii(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,&
                            vt,its,ite,kts,kte,lmask)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_snow_ii
  IMPLICIT NONE
  INTEGER       :: its,ite,kts,kte
  REAL(R64P), DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &  
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  REAL(R64P), DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  LOGICAL :: lmask(its:ite)
  REAL(R64P)       ::  lamdas, x, y, z, supcol
  integer :: i, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,t:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64,n0sfac:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
        do i = its, ite
         if (lmask(i)) then
          supcol = t0c-t(i,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1._R64P)
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopesmax
            rslopeb(i,k) = rslopesbmax
            rslope2(i,k) = rslopes2max
            rslope3(i,k) = rslopes3max
          else
            rslope(i,k) = 1./lamdas(qrs(i,k),den(i,k),n0sfac(i,k))
            rslopeb(i,k) = rslope(i,k)**bvts
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvts*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
         endif
        enddo
      enddo
  END subroutine slope_snow_ii
!----------------------------------------------------------------------------------
      subroutine slope_graup_ii(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,&
                            vt,its,ite,kts,kte,lmask)
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(core_4th_gen_avx) :: slope_graup_ii
  IMPLICIT NONE
  INTEGER       :: its,ite,kts,kte
  REAL(R64P), DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &  
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL(R64P), PARAMETER  :: t0c = 273.15_R64P
  LOGICAL :: lmask(its:ite)
  REAL(R64P)       ::  lamdag, x, y, z, supcol
  integer :: i, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
!

!DIR$ ASSUME_ALIGNED qrs:64,den:64,denfac:64,rslope:64,rslopeb:64,rslope2:64,rslope3:64,vt:64
!DIR$ VECTOR ALIGNED

      do k = kts, kte
        do i = its, ite
         if (lmask(i)) then
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopegmax
            rslopeb(i,k) = rslopegbmax
            rslope2(i,k) = rslopeg2max
            rslope3(i,k) = rslopeg3max
          else
            rslope(i,k) = 1./lamdag(qrs(i,k),den(i,k))
            rslopeb(i,k) = rslope(i,k)**bvtg
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvtg*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
         endif
        enddo
      enddo
  END subroutine slope_graup_ii
!---------------------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm_ii(its,ite,kts,kte,denl,denfacl,tkl,dzl,wwl,rql,precip,dt,id,iter)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  its,ite,kts,kte,id
      real(R64P) ::  dt
      real(R64P), dimension(its:ite,kts:kte)  ::   dzl,wwl,rql
      real(R64P), dimension(its:ite)          ::   precip
      real(R64P), dimension(its:ite,kts:kte)  ::   denl,denfacl,tkl
!
      integer  i,k,n,m,kk,iter
#ifdef MASK_HISTOGRAM
      integer intp_count(kts:kte),intp_hist(0:ite-its+1)
#endif
      real(R64P) ::  dim,dip,con1,fa1,fa2,decfl
      real(R64P), dimension(its:ite)             ::  allold
      real(R64P), dimension(its:ite,kts:kte)    ::  dz, ww, qq, wd, wa, was
      real(R64P), dimension(its:ite,kts:kte)    ::  den, denfac, tk
      real(R64P), dimension(its:ite,kts:kte+1)  ::  wi, zi, za
      real(R64P), dimension(its:ite,kts:kte)    ::  qn, qr,tmp,tmp1,tmp2,tmp3
      real(R64P), dimension(its:ite,kts:kte+1)  ::  dza, qa, qmi, qpi
      logical, dimension(its:ite) ::  lmask
!
      INTEGER minkb, minkt
      LOGICAL, DIMENSION(its:ite) :: intp_mask, tmask
      INTEGER, DIMENSION(its:ite) :: kb, kt
      REAL(R64P),    DIMENSION(its:ite)  :: tl,tl2,th,th2,qqd,qqh,qql,zsum,qsum,dql,dqh
      REAL(R64P),    DIMENSION(its:ite)  :: za_gath_t,za_gath_b
      REAL(R64P),    DIMENSION(its:ite)  :: qa_gath_b
      REAL(R64P),    DIMENSION(its:ite)  :: dza_gath_t,dza_gath_b
      REAL(R64P),    DIMENSION(its:ite)  :: qpi_gath_t,qpi_gath_b
      REAL(R64P),    DIMENSION(its:ite)  :: qmi_gath_t,qmi_gath_b
!

!DIR$ ASSUME_ALIGNED denl:64,denfacl:64,tkl:64,dzl:64,wwl:64,rql:64,precip:64

!DIR$ ATTRIBUTES ALIGN : 64 :: dz
!DIR$ ATTRIBUTES ALIGN : 64 :: ww
!DIR$ ATTRIBUTES ALIGN : 64 :: qq
!DIR$ ATTRIBUTES ALIGN : 64 :: wd
!DIR$ ATTRIBUTES ALIGN : 64 :: wa
!DIR$ ATTRIBUTES ALIGN : 64 :: was
!DIR$ ATTRIBUTES ALIGN : 64 :: den
!DIR$ ATTRIBUTES ALIGN : 64 :: denfac
!DIR$ ATTRIBUTES ALIGN : 64 :: tk
!DIR$ ATTRIBUTES ALIGN : 64 :: wi
!DIR$ ATTRIBUTES ALIGN : 64 :: zi
!DIR$ ATTRIBUTES ALIGN : 64 :: za
!DIR$ ATTRIBUTES ALIGN : 64 :: qn
!DIR$ ATTRIBUTES ALIGN : 64 :: qr
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp1
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp3
!DIR$ ATTRIBUTES ALIGN : 64 :: dza
!DIR$ ATTRIBUTES ALIGN : 64 :: qa
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi
!DIR$ ATTRIBUTES ALIGN : 64 :: tl,tl2,th,th2,qqd,qqh,qql,zsum,qsum,dql,dqh
!DIR$ ATTRIBUTES ALIGN : 64 :: za_gath_t,za_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qa_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: dza_gath_t,dza_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi_gath_t,qpi_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi_gath_t,qmi_gath_b
      

      precip(:) = 0.0
!
      do k=kts,kte
!DIR$ VECTOR ALIGNED 
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
! -----------------------------------
            dz(i,k) = dzl(i,k)
            qq(i,k) = rql(i,k)
            ww(i,k) = wwl(i,k)
            den(i,k) = denl(i,k)
            denfac(i,k) = denfacl(i,k)
            tk(i,k) = tkl(i,k)
         enddo
      enddo
! skip for no precipitation for all layers
      do i=its,ite
            allold(i) = 0.0_R64P
      enddo
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
            allold(i) = allold(i) + qq(i,k)
        enddo
      enddo
      if (maxval(allold).le.0.0) return
          lmask = allold .gt. 0.0
!
! compute interface values
      do i=its,ite
        if (lmask(i)) then
            zi(i,kts)=0.0_R64P
        endif
      enddo
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i=its,ite
           if (lmask(i)) then
               zi(i,k+1) = zi(i,k)+dz(i,k)
           endif
         enddo
      enddo
!
! save departure wind
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
            if (lmask(i)) then
                wd(i,k) = ww(i,k)
            endif
         enddo
      enddo
      n=1
      do while (n.le.(iter+1))
         do i=its,ite
 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
            if (lmask(i)) then
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
                wi(i,kts) = ww(i,kts)
                wi(i,kte+1) = ww(i,kte)
            endif
        enddo
      do k=kts+1,kte
          do i=its,ite
             if (lmask(i)) then
                 wi(i,k) = (ww(i,k)*dz(i,k-1)+ww(i,k-1)*dz(i,k))/(dz(i,k-1)+dz(i,k))
             endif
          enddo
      enddo
! 3rd order interpolation to get wi
      fa1 = 9._R64P/16._R64P
      fa2 = 1._R64P/16._R64P
      do i=its,ite
         if (lmask(i)) then
              wi(i,kts) = ww(i,kts)
              wi(i,kts+1) = 0.5*(ww(i,kts+1)+ww(i,kts))
         endif
      enddo
      do k=kts+2,kte-1
         do i=its,ite
            if (lmask(i)) then
                wi(i,k) = fa1*(ww(i,k)+ww(i,k-1))-fa2*(ww(i,k+1)+ww(i,k-2))
           endif
         enddo
      enddo
      do i=its,ite
         if (lmask(i)) then
             wi(i,kte) = 0.5_R64P*(ww(i,kte)+ww(i,kte-1))
             wi(i,kte+1) = ww(i,kte)
         endif
      enddo
!
! terminate of top of raingroup
      do k=kts+1,kte
         do i=its,ite
            if (lmask(i)) then
               if( ww(i,k).eq.0.0 ) wi(i,k)=ww(i,k-1)
           endif
         enddo
      enddo
!
! diffusivity of wi
      con1 = 0.05_R64P
      do k=kte,kts,-1
          do i=its,ite
             if (lmask(i)) then
                 decfl = (wi(i,k+1)-wi(i,k))*dt/dz(i,k)
                 if( decfl .gt. con1 ) then
                     wi(i,k) = wi(i,k+1) - con1*dz(i,k)/dt
                 endif
             endif
          enddo
      enddo
! compute arrival point
      do k=kts,kte+1
         do i=its,ite
            if (lmask(i)) then
                za(i,k) = zi(i,k) - wi(i,k)*dt
            endif
          enddo
      enddo
!
      do k=kts,kte
 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
            if (lmask(i)) then
                 dza(i,k) = za(i,k+1)-za(i,k)
            endif
          enddo
      enddo
      do i=its,ite
          if (lmask(i)) then
              dza(i,kte+1) = zi(i,kte+1) - za(i,kte+1)
           endif
      enddo
!
! computer deformation at arrival point
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
            if (lmask(i)) then
                qa(i,k) = qq(i,k)*dz(i,k)/dza(i,k)
                qr(i,k) = qa(i,k)/den(i,k)
            endif
         enddo
      enddo
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do i=its,ite
          if (lmask(i)) then
              qa(i,kte+1) = 0.0
!     call maxmin(kte-kts+1,1,qa(i,:),' arrival points ')
          endif
      enddo
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_rain_ii(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,its,ite,kts,kte,lmask)
        if( n.ge.2 ) then
        do k=kts,kte
 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
           do i=its,ite
              if (lmask(i)) then
                  wa(i,k)=0.5_R64P*(wa(i,k)+was(i,k))
              endif
           enddo
        enddo
        endif
        do k=kts,kte
           do i=its,ite
              if (lmask(i)) then
!#ifdef DEBUG
!        print*,' slope_wsm3 ',qr(i,k)*1000.,den(i,k),denfac(i,k),tk(i,k),tmp(i,k),tmp1(i,k),tmp2(i,k),ww(i,k),wa(i,k)
!#endif
! mean wind is average of departure and new arrival winds
                  ww(i,k) = 0.5_R64P * ( wd(i,k)+wa(i,k) )
                  was(i,k) = wa(i,k)
              endif
            enddo
          enddo
      endif
      n=n+1
      enddo
!
! estimate values at arrival cell interface with monotone
      do k=kts+1,kte
         do i=its,ite
            if (lmask(i)) then
                dip=(qa(i,k+1)-qa(i,k))/(dza(i,k+1)+dza(i,k))
                dim=(qa(i,k)-qa(i,k-1))/(dza(i,k-1)+dza(i,k))
                if( dip*dim.le.0.0 ) then
                    qmi(i,k)=qa(i,k)
                    qpi(i,k)=qa(i,k)
                else
                    qpi(i,k)=qa(i,k)+0.5_R64P*(dip+dim)*dza(i,k)
                    qmi(i,k)=2.0_R64P*qa(i,k)-qpi(i,k)
                    if( qpi(i,k).lt.0.0 .or. qmi(i,k).lt.0.0 ) then
                        qpi(i,k) = qa(i,k)
                        qmi(i,k) = qa(i,k)
                    endif
               endif
           endif
        enddo
      enddo
      do i=its,ite
          if (lmask(i)) then
              qpi(i,kts)=qa(i,kts)
              qmi(i,kts)=qa(i,kts)
              qmi(i,kte+1)=qa(i,kte+1)
              qpi(i,kte+1)=qa(i,kte+1)
          endif
      enddo
!
! interpolation to regular point
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! BEGIN WSM5 CODE FROM John Michalakes !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      qn = 0.0_R64P
      kb=kts  ! kb is a vector
      kt=kts  ! kt is a vector
#ifdef MASK_HISTOGRAM
      intp_hist = 0
#endif
      INTP : do k=kts,kte
             kb=max(kb-1,kts)
             kt=max(kt-1,kts)
! find kb and kt
             intp_mask = ( zi(:,k).lt.za(:,kte+1) .AND. lmask )
             tmask = intp_mask
             minkb = 999
             minkt = 999
             DO i=its,ite
               IF ( tmask(i) .AND. kb(i) .lt. minkb ) minkb = kb(i)
               IF ( tmask(i) .AND. kt(i) .lt. minkt ) minkt = kt(i)
             ENDDO
             find_kb : do kk=minkb,kte
               DO i=its,ite
               IF ( tmask(i) .AND. zi(i,k).le.za(i,kk+1) ) THEN
                 kb(i) = kk
                 tmask(i) = .FALSE.
               ENDIF
               ENDDO
             enddo find_kb

             tmask = intp_mask
             find_kt : do kk=minkt,kte
               DO i=its,ite
               IF ( tmask(i) .AND. zi(i,k+1).le.za(i,kk) ) THEN
                 kt(i) = kk
                 tmask(i) = .FALSE.
               ENDIF
               ENDDO
             enddo find_kt
             kt = max(kt - 1,kts)

!#define RANGE_CHECKING
#ifndef RANGE_CHECKING
# define DX1 (i+(kb(i)-1)*(ite-its+1)),1
# define DX2 (i+(kt(i)-1)*(ite-its+1)),1
#else
# define DX1 i,kb(i)
# define DX2 i,kt(i)
#endif
!DEC$ SIMD
             DO i = its,ite
               qa_gath_b(i) = qa(DX1)
               za_gath_b(i) = za(DX1)
               dza_gath_b(i) = dza(DX1)
               qpi_gath_b(i) = qpi(DX1)
               qmi_gath_b(i) = qmi(DX1)
             ENDDO
!DEC$ SIMD
             DO i = its,ite
               za_gath_t(i) = za(DX2)
               dza_gath_t(i) = dza(DX2)
               qpi_gath_t(i) = qpi(DX2)
               qmi_gath_t(i) = qmi(DX2)
             ENDDO
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             DO i = its,ite
             IF ( kt(i) .eq. kb(i) .AND. intp_mask(i) ) THEN
               tl(i)=(zi(i,k)-za_gath_b(i))/dza_gath_b(i)
               th(i)=(zi(i,k+1)-za_gath_b(i))/dza_gath_b(i)
               tl2(i) = tl(i)*tl(i)
               th2(i) = th(i)*th(i)
               qqd(i)=0.5*(qpi_gath_b(i)-qmi_gath_b(i))
               qqh(i)=qqd(i)*th2(i)+qmi_gath_b(i)*th(i)
               qql(i)=qqd(i)*tl2(i)+qmi_gath_b(i)*tl(i)
               qn(i,k) = (qqh(i)-qql(i))/(th(i)-tl(i))
             ELSE IF ( kt(i) .gt. kb(i) .AND. intp_mask(i) ) THEN
               tl(i)=(zi(i,k)-za_gath_b(i))/dza_gath_b(i)
               tl2(i)=tl(i)*tl(i)
               qqd(i)=0.5*(qpi_gath_b(i)-qmi_gath_b(i))
               qql(i)=qqd(i)*tl2(i)+qmi_gath_b(i)*tl(i)
               dql(i) = qa_gath_b(i)-qql(i)
               zsum(i)  = (1.-tl(i))*dza_gath_b(i)
               qsum(i)  = dql(i)*dza_gath_b(i)
             ENDIF
             ENDDO
#ifdef MASK_HISTOGRAM
             intp_count(k) = 0
             DO i = its,ite
               IF ( kt(i) .ge. kb(i) .AND. intp_mask(i) ) THEN
                 intp_count(k) = intp_count(k) + 1
               ENDIF
             ENDDO
#endif
             DO i = its,ite
               if( kt(i)-kb(i).gt.1 .AND. intp_mask(i) ) then
                 do m=kb(i)+1,kt(i)-1
                     zsum(i) = zsum(i) + dza(i,m)
                     qsum(i) = qsum(i) + qa(i,m) * dza(i,m)
                 enddo
               endif
             ENDDO
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             DO i = its,ite
             IF ( kt(i) .gt. kb(i) .AND. intp_mask(i) ) THEN
               th(i)=(zi(i,k+1)-za_gath_t(i))/dza_gath_t(i)
               th2(i) = th(i)*th(i)
               qqd(i)=0.5_R64P*(qpi_gath_t(i)-qmi_gath_t(i))
               dqh(i)=qqd(i)*th2(i)+qmi_gath_t(i)*th(i)
               zsum(i)  = zsum(i) + th(i)*dza_gath_t(i)
               qsum(i)  = qsum(i) + dqh(i)*dza_gath_t(i)
               qn(i,k) = qsum(i)/zsum(i)
             ENDIF
             ENDDO
       ENDDO intp
#ifdef MASK_HISTOGRAM
       do k=kts,kte
!print *,'DEBUG:  intp_count(',k,') = ',intp_count(k)
         IF ((intp_count(k) < 0) .OR. (intp_count(k) > (ite-its+1))) THEN
           print *,'ERROR:  intp_count(',k,') = ',intp_count(k)
           stop
         ENDIF
         intp_hist(intp_count(k)) = intp_hist(intp_count(k)) + 1
       enddo
       if (ite-its+1 == 8) then
         write (6,110) intp_hist
110   format ('intp_hist =  ',9i3)
       else
         do i=its,ite
           print *,'intp_hist(',i,') = ',intp_hist(i)
         enddo
       endif
#endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END WSM5 CODE FROM John Michalakes !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! rain out
      intp_mask = lmask
      sum_precip: do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             DO i = its,ite
             IF (za(i,k).lt.0.0.and.za(i,k+1).lt.0.0.AND.intp_mask(i)) THEN
               precip(i) = precip(i) + qa(i,k)*dza(i,k)
             ELSE IF (za(i,k).lt.0.0.and.za(i,k+1).ge.0.0.AND.intp_mask(i)) THEN
               precip(i) = precip(i) + qa(i,k)*(0.0-za(i,k))
               intp_mask(i) = .FALSE.
             ENDIF
             ENDDO
      enddo sum_precip
!
! replace the new values
      do k=kts,kte


!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
             if (lmask(i)) then
                 rql(i,k) = qn(i,k)
             endif
        enddo
      enddo
!
! ----------------------------------
!
  END SUBROUTINE nislfv_rain_plm_ii
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm6_ii(its,ite,kts,kte,denl,denfacl,tkl,dzl,wwl,rql,rql2, precip1, precip2,dt,id,iter)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  its,ite,kts,kte,id
      real(R64P) ::  dt
      real(R64P), dimension(its:ite,kts:kte) ::  dzl,wwl,rql,rql2
      real(R64P), dimension(its:ite)         ::  precip,precip1,precip2
      real(R64P), dimension(its:ite,kts:kte) ::  denl,denfacl,tkl
!
      integer  i,k,n,m,kk,iter,ist
      real(R64P) ::  dim,dip,con1,fa1,fa2,decfl
      real(R64P), dimension(its:ite) ::  allold
      real(R64P), dimension(its:ite,kts:kte)   ::      dz, ww, qq, qq2, wd, wa, wa2, was
      real(R64P), dimension(its:ite,kts:kte)   ::      den, denfac, tk
      real(R64P), dimension(its:ite,kts:kte+1) ::      wi, zi, za
      real(R64P), dimension(its:ite,kts:kte)   ::      qn, qr,qr2,tmp,tmp1,tmp2,tmp3
      real(R64P), dimension(its:ite,kts:kte+1) ::      dza, qa, qa2,qmi, qpi
      logical, dimension(its:ite) ::  lmask
!
      INTEGER minkb, minkt
      LOGICAL, DIMENSION(its:ite) :: intp_mask, tmask
      INTEGER, DIMENSION(its:ite) :: kb, kt
      REAL(R64P),    DIMENSION(its:ite) :: tl,tl2,th,th2,qqd,qqh,qql,zsum,qsum,dql,dqh
      REAL(R64P),    DIMENSION(its:ite) :: za_gath_t,za_gath_b
      REAL(R64P),    DIMENSION(its:ite) :: qa_gath_b
      REAL(R64P),    DIMENSION(its:ite) :: dza_gath_t,dza_gath_b
      REAL(R64P),    DIMENSION(its:ite) :: qpi_gath_t,qpi_gath_b
      REAL(R64P),    DIMENSION(its:ite) :: qmi_gath_t,qmi_gath_b
!

!DIR$ ASSUME_ALIGNED denl:64,denfacl:64,tkl:64,dzl:64,wwl:64,rql:64,precip1:64,precip2:64

!DIR$ ATTRIBUTES ALIGN : 64 :: dz
!DIR$ ATTRIBUTES ALIGN : 64 :: ww
!DIR$ ATTRIBUTES ALIGN : 64 :: qq
!DIR$ ATTRIBUTES ALIGN : 64 :: qq2
!DIR$ ATTRIBUTES ALIGN : 64 :: wd
!DIR$ ATTRIBUTES ALIGN : 64 :: wa
!DIR$ ATTRIBUTES ALIGN : 64 :: wa2
!DIR$ ATTRIBUTES ALIGN : 64 :: was
!DIR$ ATTRIBUTES ALIGN : 64 :: den
!DIR$ ATTRIBUTES ALIGN : 64 :: denfac
!DIR$ ATTRIBUTES ALIGN : 64 :: tk
!DIR$ ATTRIBUTES ALIGN : 64 :: wi
!DIR$ ATTRIBUTES ALIGN : 64 :: zi
!DIR$ ATTRIBUTES ALIGN : 64 :: za
!DIR$ ATTRIBUTES ALIGN : 64 :: qn
!DIR$ ATTRIBUTES ALIGN : 64 :: qr
!DIR$ ATTRIBUTES ALIGN : 64 :: qr2
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp1
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp3
!DIR$ ATTRIBUTES ALIGN : 64 :: dza
!DIR$ ATTRIBUTES ALIGN : 64 :: qa
!DIR$ ATTRIBUTES ALIGN : 64 :: qa2     
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi
!DIR$ ATTRIBUTES ALIGN : 64 :: tl,tl2,th,th2,qqd,qqh,qql,zsum,qsum,dql,dqh
!DIR$ ATTRIBUTES ALIGN : 64 :: za_gath_t,za_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qa_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: dza_gath_t,dza_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qpi_gath_t,qpi_gath_b
!DIR$ ATTRIBUTES ALIGN : 64 :: qmi_gath_t,qmi_gath_b
      
      precip(:)  = 0.0_R64P
      precip1(:) = 0.0_R64P
      precip2(:) = 0.0_R64P
!
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
! -----------------------------------
            dz(i,k) = dzl(i,k)
            qq(i,k) = rql(i,k)
            qq2(i,k) = rql2(i,k)
            ww(i,k) = wwl(i,k)
            den(i,k) = denl(i,k)
            denfac(i,k) = denfacl(i,k)
            tk(i,k) = tkl(i,k)
         enddo
      enddo
! skip for no precipitation for all layers
      do i=its,ite
         allold(i) = 0.0
      enddo
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i=its,ite
             allold(i) = allold(i) + qq(i,k)
         enddo
      enddo
      if (maxval(allold).le.0.0) return
                lmask = allold .gt. 0.0
!
! compute interface values
      do i=its,ite
          if(lmask(i)) then
             zi(i,kts)=0.0
          endif
      enddo
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))          
          do i=its,ite
             if(lmask(i)) then
                zi(i,k+1) = zi(i,k)+dz(i,k)
             endif
          enddo
      enddo
!
! save departure wind
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))          
         do i=its,ite
            if(lmask(i)) then
               wd(i,k) = ww(i,k)
            endif
         enddo
      enddo
      n=1
      do while (n.le.(iter+1))
         do i=its,ite
             if(lmask(i)) then
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
                wi(i,kts) = ww(i,kts)
                wi(i,kte+1) = ww(i,kte)
             endif
         enddo
      do k=kts+1,kte
         do i=its,ite
            if(lmask(i)) then
                wi(i,k) = (ww(i,k)*dz(i,k-1)+ww(i,k-1)*dz(i,k))/(dz(i,k-1)+dz(i,k))
            endif
         enddo
      enddo
! 3rd order interpolation to get wi
      fa1 = 9._R64P/16._R64P
      fa2 = 1._R64P/16._R64P
      do i=its,ite
         if(lmask(i)) then
            wi(i,kts) = ww(i,kts)
            wi(i,kts+1) = 0.5_R64P*(ww(i,kts+1)+ww(i,kts))
         endif
      enddo
      do k=kts+2,kte-1
         do i=its,ite
            if(lmask(i)) then
               wi(i,k) = fa1*(ww(i,k)+ww(i,k-1))-fa2*(ww(i,k+1)+ww(i,k-2))
            endif
         enddo
      enddo
      do i=its,ite
         if(lmask(i)) then
            wi(i,kte) = 0.5_R64P*(ww(i,kte)+ww(i,kte-1))
            wi(i,kte+1) = ww(i,kte)
         endif
      enddo
!
! terminate of top of raingroup
      do k=kts+1,kte
         do i=its,ite
            if(lmask(i)) then
              if( ww(i,k).eq.0.0 ) wi(i,k)=ww(i,k-1)
            endif
         enddo
      enddo
!
! diffusivity of wi
      con1 = 0.05_R64P
      do k=kte,kts,-1
          do i=its,ite
             if(lmask(i)) then
                decfl = (wi(i,k+1)-wi(i,k))*dt/dz(i,k)
                if( decfl .gt. con1 ) then
                    wi(i,k) = wi(i,k+1) - con1*dz(i,k)/dt
                endif
              endif
          enddo
      enddo
! compute arrival point
      do k=kts,kte+1
         do i=its,ite
            if(lmask(i)) then
               za(i,k) = zi(i,k) - wi(i,k)*dt
            endif
         enddo
      enddo
!
      do k=kts,kte
         do i=its,ite
            if(lmask(i)) then
               dza(i,k) = za(i,k+1)-za(i,k)
            endif
         enddo
      enddo
      do i=its,ite
          if(lmask(i)) then
             dza(i,kte+1) = zi(i,kte+1) - za(i,kte+1)
          endif
      enddo
!
! computer deformation at arrival point
      do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))          
         do i=its,ite
            if(lmask(i)) then
               qa(i,k) = qq(i,k)*dz(i,k)/dza(i,k)
               qa2(i,k) = qq2(i,k)*dz(i,k)/dza(i,k)
               qr(i,k) = qa(i,k)/den(i,k)
               qr2(i,k) = qa2(i,k)/den(i,k)
            endif
         enddo
      enddo
      do i=its,ite
          if(lmask(i)) then
             qa(i,kte+1) = 0.0
             qa2(i,kte+1) = 0.0
!     call maxmin(kte-kts+1,1,qa(i,:),' arrival points ')
          endif
      enddo
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_snow_ii(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,its,ite,kts,kte,lmask)
        call slope_graup_ii(qr2,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa2,its,ite,kts,kte,lmask)
        do k = kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))            
            do i=its,ite
               if(lmask(i)) then
                  tmp(i,k) = max((qr(i,k)+qr2(i,k)), 1.E-15_R64P)
                  IF ( tmp(i,k) .gt. 1.e-15_R64P ) THEN
                      wa(i,k) = (wa(i,k)*qr(i,k) + wa2(i,k)*qr2(i,k))/tmp(i,k)
                  ELSE
                      wa(i,k) = 0._R64P
                  ENDIF
             endif
           enddo
        enddo
        if( n.ge.2 ) then
           do k=kts,kte
              do i=its,ite
                if(lmask(i)) then
                  wa(i,k)=0.5_R64P*(wa(i,k)+was(i,k))
                endif
              enddo
          enddo
        endif
        do k=kts,kte
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))            
           do i=its,ite
              if(lmask(i)) then
!#ifdef DEBUG
!        print*,' slope_wsm3 ',qr(i,k)*1000.,den(i,k),denfac(i,k),tk(i,k),tmp(i,k),tmp1(i,k),tmp2(i,k), &
!           ww(i,k),wa(i,k)
!#endif
! mean wind is average of departure and new arrival winds
                  ww(i,k) = 0.5_R64P* ( wd(i,k)+wa(i,k) )
                  was(i,k) = wa(i,k)
               endif
            enddo
        enddo
      endif
      n=n+1
      enddo
      ist_loop : do ist = 1, 2
      if (ist.eq.2) then
          do k=kts,kte+1
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             do i=its,ite
                if(lmask(i)) then
                    qa(i,k) = qa2(i,k)
                endif
             enddo
          enddo
      endif
!
 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
      do i=its,ite
         if(lmask(i)) then
            precip(i) = 0._R64P
         endif
      enddo
!
! estimate values at arrival cell interface with monotone
      do k=kts+1,kte
         do i=its,ite
            if(lmask(i)) then
               dip=(qa(i,k+1)-qa(i,k))/(dza(i,k+1)+dza(i,k))
               dim=(qa(i,k)-qa(i,k-1))/(dza(i,k-1)+dza(i,k))
               if( dip*dim.le.0.0 ) then
                   qmi(i,k)=qa(i,k)
                   qpi(i,k)=qa(i,k)
               else
                   qpi(i,k)=qa(i,k)+0.5*(dip+dim)*dza(i,k)
                   qmi(i,k)=2.0*qa(i,k)-qpi(i,k)
                   if( qpi(i,k).lt.0.0 .or. qmi(i,k).lt.0.0 ) then
                        qpi(i,k) = qa(i,k)
                        qmi(i,k) = qa(i,k)
                   endif
             endif
          endif
         enddo
      enddo
      do i=its,ite
         if(lmask(i)) then
            qpi(i,kts)=qa(i,kts)
            qmi(i,kts)=qa(i,kts)
            qmi(i,kte+1)=qa(i,kte+1)
            qpi(i,kte+1)=qa(i,kte+1)
         endif
      enddo
!
! interpolation to regular point
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! BEGIN WSM5 CODE FROM John Michalakes !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      qn = 0.0_R64P
      kb=kts  ! kb is a vector
      kt=kts  ! kt is a vector
      INTP : do k=kts,kte
             kb=max(kb-1,kts)
             kt=max(kt-1,kts)
! find kb and kt
             intp_mask = ( zi(:,k).lt.za(:,kte+1) .AND. lmask )
             tmask = intp_mask
             minkb = 999
             minkt = 999
             DO i=its,ite
               IF ( tmask(i) .AND. kb(i) .lt. minkb ) minkb = kb(i)
               IF ( tmask(i) .AND. kt(i) .lt. minkt ) minkt = kt(i)
             ENDDO
             find_kb : do kk=minkb,kte
               DO i=its,ite
               IF ( tmask(i) .AND. zi(i,k).le.za(i,kk+1) ) THEN
                 kb(i) = kk
                 tmask(i) = .FALSE.
               ENDIF
               ENDDO
             enddo find_kb

             tmask = intp_mask
             find_kt : do kk=minkt,kte
               DO i=its,ite
               IF ( tmask(i) .AND. zi(i,k+1).le.za(i,kk) ) THEN
                 kt(i) = kk
                 tmask(i) = .FALSE.
               ENDIF
               ENDDO
             enddo find_kt
             kt = max(kt - 1,kts)

!#define RANGE_CHECKING
#ifndef RANGE_CHECKING
# define DX1 (i+(kb(i)-1)*(ite-its+1)),1
# define DX2 (i+(kt(i)-1)*(ite-its+1)),1
#else
# define DX1 i,kb(i)
# define DX2 i,kt(i)
#endif
!DEC$ SIMD
             DO i = its,ite
               qa_gath_b(i) = qa(DX1)
               za_gath_b(i) = za(DX1)
               dza_gath_b(i) = dza(DX1)
               qpi_gath_b(i) = qpi(DX1)
               qmi_gath_b(i) = qmi(DX1)
             ENDDO
!DEC$ SIMD
             DO i = its,ite
               za_gath_t(i) = za(DX2)
               dza_gath_t(i) = dza(DX2)
               qpi_gath_t(i) = qpi(DX2)
               qmi_gath_t(i) = qmi(DX2)
             ENDDO
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             DO i = its,ite
             IF ( kt(i) .eq. kb(i) .AND. intp_mask(i) ) THEN
               tl(i)=(zi(i,k)-za_gath_b(i))/dza_gath_b(i)
               th(i)=(zi(i,k+1)-za_gath_b(i))/dza_gath_b(i)
               tl2(i) = tl(i)*tl(i)
               th2(i) = th(i)*th(i)
               qqd(i)=0.5*(qpi_gath_b(i)-qmi_gath_b(i))
               qqh(i)=qqd(i)*th2(i)+qmi_gath_b(i)*th(i)
               qql(i)=qqd(i)*tl2(i)+qmi_gath_b(i)*tl(i)
               qn(i,k) = (qqh(i)-qql(i))/(th(i)-tl(i))
             ELSE IF ( kt(i) .gt. kb(i) .AND. intp_mask(i) ) THEN
               tl(i)=(zi(i,k)-za_gath_b(i))/dza_gath_b(i)
               tl2(i)=tl(i)*tl(i)
               qqd(i)=0.5*(qpi_gath_b(i)-qmi_gath_b(i))
               qql(i)=qqd(i)*tl2(i)+qmi_gath_b(i)*tl(i)
               dql(i) = qa_gath_b(i)-qql(i)
               zsum(i)  = (1.-tl(i))*dza_gath_b(i)
               qsum(i)  = dql(i)*dza_gath_b(i)
             ENDIF
             ENDDO
             DO i = its,ite
               if( kt(i)-kb(i).gt.1 .AND. intp_mask(i) ) then
                 do m=kb(i)+1,kt(i)-1
                     zsum(i) = zsum(i) + dza(i,m)
                     qsum(i) = qsum(i) + qa(i,m) * dza(i,m)
                 enddo
               endif
             ENDDO
 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
             DO i = its,ite
             IF ( kt(i) .gt. kb(i) .AND. intp_mask(i) ) THEN
               th(i)=(zi(i,k+1)-za_gath_t(i))/dza_gath_t(i)
               th2(i) = th(i)*th(i)
               qqd(i)=0.5_R64P*(qpi_gath_t(i)-qmi_gath_t(i))
               dqh(i)=qqd(i)*th2(i)+qmi_gath_t(i)*th(i)
               zsum(i)  = zsum(i) + th(i)*dza_gath_t(i)
               qsum(i)  = qsum(i) + dqh(i)*dza_gath_t(i)
               qn(i,k) = qsum(i)/zsum(i)
             ENDIF
             ENDDO
       ENDDO intp
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END WSM5 CODE FROM John Michalakes !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! rain out
      intp_mask = lmask
      sum_precip: do k=kts,kte
             DO i = its,ite
             IF (za(i,k).lt.0.0.and.za(i,k+1).lt.0.0.AND.intp_mask(i)) THEN
               precip(i) = precip(i) + qa(i,k)*dza(i,k)
             ELSE IF (za(i,k).lt.0.0.and.za(i,k+1).ge.0.0.AND.intp_mask(i)) THEN
               precip(i) = precip(i) + qa(i,k)*(0.0-za(i,k))
               intp_mask(i) = .FALSE.
             ENDIF
             ENDDO
      enddo sum_precip
!
! replace the new values
      if(ist.eq.1) then
        do k=kts,kte


!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
            do i=its,ite
               if(lmask(i)) then
                  rql(i,k) = qn(i,k)
               endif
            enddo
        enddo


 !DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i=its,ite
           if(lmask(i)) then
              precip1(i) = precip(i)
            endif
        enddo
      else
        do k=kts,kte


!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i=its,ite
            if(lmask(i)) then
               rql2(i,k) = qn(i,k)
            endif
           enddo
        enddo


!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i=its,ite
            if(lmask(i)) then
                precip2(i) = precip(i)
            endif
         enddo
       endif
    enddo ist_loop
!
! ----------------------------------
!
  END SUBROUTINE nislfv_rain_plm6_ii
#endif
!+---+-----------------------------------------------------------------+
  ! Read array from unitno and convert from "no-chunk" to "chunk"
  SUBROUTINE readarray2(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(OUT) :: arr(:,:)
    CHARACTER(LEN=*), INTENT(IN)  :: arrname
    INTEGER,          INTENT(IN)  :: unitno
    INTEGER,          INTENT(IN)  :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:)
    INTEGER :: i,j,ij,jsize,CHUNK,ipn
    CHUNK = size(arr,1)
    jsize = size(arr,2)
    ALLOCATE(tmparr(ips:ipe))
    read(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do i = 1,CHUNK
        ! replicate last column if needed
        ipn = min(ij+i+ips-1,ipe)
        arr(i,j) = tmparr(ipn)
      enddo
    enddo
    DEALLOCATE(tmparr)
  END SUBROUTINE readarray2

!+---+-----------------------------------------------------------------+
  ! Read array from unitno and convert from "no-chunk" to "chunk"
  SUBROUTINE readarray3(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(OUT) :: arr(:,:,:)
    CHARACTER(LEN=*), INTENT(IN)  :: arrname
    INTEGER,          INTENT(IN)  :: unitno
    INTEGER,          INTENT(IN)  :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:,:)
    INTEGER :: i,j,k,ij,ksize,jsize,CHUNK,ipn
    CHUNK = size(arr,1)
    ksize = size(arr,2)
    jsize = size(arr,3)
    ALLOCATE(tmparr(ips:ipe,ksize))
    read(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do i = 1,CHUNK
        ! replicate last column if needed
        ipn = min(ij+i+ips-1,ipe)
        do k = 1,ksize
          arr(i,k,j) = tmparr(ipn,k)
        enddo
      enddo
    enddo
    DEALLOCATE(tmparr)
  END SUBROUTINE readarray3

!+---+-----------------------------------------------------------------+
  ! Read array from unitno and convert from "no-chunk" to "chunk"
  SUBROUTINE readarray4(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(OUT) :: arr(:,:,:,:)
    CHARACTER(LEN=*), INTENT(IN)  :: arrname
    INTEGER,          INTENT(IN)  :: unitno
    INTEGER,          INTENT(IN)  :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:,:,:)
    INTEGER :: i,j,k,ij,ksize,jsize,CHUNK,ipn,m,msize
    CHUNK = size(arr,1)
    ksize = size(arr,2)
    msize = size(arr,3)
    jsize = size(arr,4)
    ALLOCATE(tmparr(ips:ipe,ksize,msize))
    read(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do m = 1,msize
        do i = 1,CHUNK
          ! replicate last column if needed
          ipn = min(ij+i+ips-1,ipe)
          do k = 1,ksize
            arr(i,k,m,j) = tmparr(ipn,k,m)
          enddo
        enddo
      enddo
    enddo
    DEALLOCATE(tmparr)
  END SUBROUTINE readarray4

!+---+-----------------------------------------------------------------+
  ! Convert array from "chunk" to "no-chunk" and write to unitno.  
  SUBROUTINE writearray2(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(IN) :: arr(:,:)
    CHARACTER(LEN=*), INTENT(IN) :: arrname
    INTEGER,          INTENT(IN) :: unitno
    INTEGER,          INTENT(IN) :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:)
    INTEGER :: i,j,ij,jsize,CHUNK,ipn
    CHUNK = size(arr,1)
    jsize = size(arr,2)
    ALLOCATE(tmparr(ips:ipe))
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do i = 1,CHUNK
        ipn = ij+i+ips-1
        ! skip any replicated columns
        if ((ips<=ipn).and.(ipn<=ipe)) then
          tmparr(ipn) = arr(i,j)
        endif
      enddo
    enddo
    write(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    DEALLOCATE(tmparr)
  END SUBROUTINE writearray2

!+---+-----------------------------------------------------------------+
  ! Convert array from "chunk" to "no-chunk" and write to unitno.  
  SUBROUTINE writearray3(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(IN) :: arr(:,:,:)
    CHARACTER(LEN=*), INTENT(IN) :: arrname
    INTEGER,          INTENT(IN) :: unitno
    INTEGER,          INTENT(IN) :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:,:)
    INTEGER :: i,j,k,ij,ksize,jsize,CHUNK,ipn
    CHUNK = size(arr,1)
    ksize = size(arr,2)
    jsize = size(arr,3)
    ALLOCATE(tmparr(ips:ipe,ksize))
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do i = 1,CHUNK
        ipn = ij+i+ips-1
        ! skip any replicated columns
        if ((ips<=ipn).and.(ipn<=ipe)) then
          do k = 1,ksize
            tmparr(ipn,k) = arr(i,k,j)
          enddo
        endif
      enddo
    enddo
    write(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    DEALLOCATE(tmparr)
  END SUBROUTINE writearray3

!+---+-----------------------------------------------------------------+
  ! Convert array from "chunk" to "no-chunk" and write to unitno.  
  SUBROUTINE writearray4(arr,arrname,unitno,ips,ipe)
    REAL,             INTENT(IN) :: arr(:,:,:,:)
    CHARACTER(LEN=*), INTENT(IN) :: arrname
    INTEGER,          INTENT(IN) :: unitno
    INTEGER,          INTENT(IN) :: ips,ipe
    REAL, ALLOCATABLE :: tmparr(:,:,:)
    INTEGER :: i,j,k,ij,ksize,jsize,CHUNK,ipn,m,msize
    CHUNK = size(arr,1)
    ksize = size(arr,2)
    msize = size(arr,3)
    jsize = size(arr,4)
    ALLOCATE(tmparr(ips:ipe,ksize,msize))
    do j = 1,jsize
      ij = (j-1)*CHUNK
      do m = 1,msize
        do i = 1,CHUNK
          ipn = ij+i+ips-1
          ! skip any replicated columns
          if ((ips<=ipn).and.(ipn<=ipe)) then
            do k = 1,ksize
              tmparr(ipn,k,m) = arr(i,k,m,j)
            enddo
          endif
        enddo
      enddo
    enddo
    write(unitno) tmparr
    print *,' max ',trim(arrname),' = ',maxval(tmparr),' at ',maxloc(tmparr)
    print *,' min ',trim(arrname),' = ',minval(tmparr),' at ',minloc(tmparr)
    DEALLOCATE(tmparr)
  END SUBROUTINE writearray4

  SUBROUTINE firstTouch(t, q                                      &   
                   ,qci, qrs, den, p, delz                        &
                   ,lat                                           &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                   ,graupel,graupelncv                            &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  Mimic memory access patterns of wsm62D() while setting physics arrays 
!  to zero.  
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte,  &
                                      lat
  REAL(R64P), DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL(R64P), DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci
  REAL(R64P), DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qrs
  REAL(R64P), DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL(R64P), DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                             den, &
                                                               p, &
                                                            delz
  REAL(R64P), DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  REAL(R64P), DIMENSION( ims:ime, jms:jme ), OPTIONAL,                  &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv
  REAL(R64P), DIMENSION( ims:ime, jms:jme ), OPTIONAL,                  &
        INTENT(INOUT) ::                                 graupel, &
                                                      graupelncv
!DIR$ ASSUME_ALIGNED t:64,qci:64,qrs:64,q:64,den:64,p:64,delz:64
!DIR$ ASSUME_ALIGNED rain:64,rainncv:64,sr:64,snow:64,snowncv:64,graupel:64,graupelncv:64
! LOCAL VAR
  INTEGER :: i, k
!
      do k = kts, kte
        do i = its, ite
          t(i,k) = 0.
          q(i,k) = 0.
          qci(i,k,1) = 0.
          qci(i,k,2) = 0.
          qrs(i,k,1) = 0.
          qrs(i,k,2) = 0.
          qrs(i,k,3) = 0.
          den(i,k) = 0.
          p(i,k) = 0.
          delz(i,k) = 0.
        enddo
      enddo
      do i = its, ite
        rain(i) = 0.
        rainncv(i) = 0.
        sr(i) = 0.
      enddo
      if (PRESENT(snow)) then
        do i = its, ite
          snow(i,lat) = 0.
        enddo
      endif
      if (PRESENT(snowncv)) then
        do i = its, ite
          snowncv(i,lat) = 0.
        enddo
      endif
      if (PRESENT(graupel)) then
        do i = its, ite
          graupel(i,lat) = 0.
        enddo
      endif
      if (PRESENT(graupelncv)) then
        do i = its, ite
          graupelncv(i,lat) = 0.
        enddo
      endif
  END SUBROUTINE firstTouch

END MODULE module_mp_wsm6




  