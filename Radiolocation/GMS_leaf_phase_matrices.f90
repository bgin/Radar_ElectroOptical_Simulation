


module  mod_leaf_phase_matrices


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_leaf_phase_matrices'
 !          
 !          Purpose:
  !                   Computation of leaf phase matrices
 !          History:
 !                        
 !                          Date: 20-06-2020
  !                         Time: 16:09 GMT+2
  !                         
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 2
 !                      Micro: 0
 !
 !         
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,  only : int4,sp
     implicit none
     public

     !File version info
     integer(kind=int4),   parameter :: MOD_LEAF_PHASE_MATRICES_MAJOR = 1
     integer(kind=int4),   parameter :: MOD_LEAF_PHASE_MATRICES_MINOR = 0
     integer(kind=int4),   parameter :: MOD_LEAF_PHASE_MATRICES_MICRO = 0
     integer(kind=int4),   parameter :: MOD_LEAF_PHASE_MATRICES_FULLVER = &
          1000*MOD_LEAF_PHASE_MATRICES_MAJOR+100*MOD_LEAF_PHASE_MATRICES_MINOR+ &
          10*MOD_LEAF_PHASE_MATRICES_MICRO
     character(*),         parameter :: MOD_LEAF_PHASE_MATRICES_CREATE_DATE = "20-06-2020 16:09  +00200 (SAT 20 JUN 2020 GMT+2)"
     character(*),         parameter :: MOD_LEAF_PHASE_MATRICES_BUILD_DATE  = __DATE__ ":" __TIME__
     character(*),         parameter :: MOD_LEAF_PHASE_MATRICES_ID = &
          "$Id: GMS_leaf_phase_matrices.f90 1000 +00200 2020-06-20 16:09 beniekg@gmail.com $"

#ifndef LEAF_PHASE_MATRIX_AUTOVECTORIZE
#define LEAF_PHASE_MATRIX_AUTOVECTORIZE 0
#endif
     

   contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine compute_leaf_phase_matrices(l4x4phm,sm2x2avg,l2x2mp,l2x2mn, &
                                            stokes4x4m, &
                                            scat2x2m,lvar,    &
                                            crown_height,trunk_height,dens, &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0) !GCC$ ATTRIBUTES hot :: compute_leaf_phase_matrices !GCC$ ATTRIBUTES aligned(32) :: compute_leaf_phase_matrices
#elif defined __INTEL_COMPILER
     subroutine compute_leaf_phase_matrices(l4x4phm,sm2x2avg,l2x2mp,l2x2mn, &
                                            stokes4x4m, &
                                            scat2x2m,lvar,lvarv, &
                                            crown_height,trunk_height,dens,  &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0)
                  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: compute_leaf_phase_matrices
#endif

           real(kind=sp),     dimension(4,4,4),  intent(inout)  :: l4x4phm !result
           complex(kind=sp),  dimension(2,2,2),  intent(inout)  :: sm2x2avg
           complex(kind=sp),  dimension(2,2),    intent(inout)  :: l2x2mp ! result complex 2x2 m matrix positive dir
           complex(kind=sp),  dimension(2,2),    intent(inout)  :: l2x2mn ! result complex 2x2 m matrix negative dir
           real(kind=sp),     dimension(4,4),    intent(out)    :: stokes4x4m !result
           complex(kind=sp),  dimension(2,2),    intent(out)    :: scat2x2m   !result
         
          
           real(kind=sp),                        intent(in)  :: lvar  ! leaf variation of orientation function 
           real(kind=sp),                        intent(in)  :: lvarv ! leaf variation variable
           real(kind=sp),                        intent(in)  :: crown_height
           real(kind=sp),                        intent(in)  :: trunk_height
           real(kind=sp),                        intent(in)  :: dens
           real(kind=sp),                        intent(in)  :: ldiam ! leaves diamater
           real(kind=sp),                        intent(in)  :: lthick ! leaves thickness
           real(kind=sp),                        intent(in)  :: lmg
           real(kind=sp),                        intent(in)  :: lrho
           real(kind=sp),                        intent(in)  :: ldens
           real(kind=sp),                        intent(in)  :: theta ! radar scanning angle
           real(kind=sp),                        intent(in)  :: ctheta ! cosine of radar scanning angle
           real(kind=sp),                        intent(in)  :: stheta ! sine of radr scaning angle
           complex(kind=sp), dimension(12),      intent(in)  :: leaves_epsr  ! leaves dialectric constant
           complex(kind=sp), dimension(12),      intent(in)  :: leaves_epsrc ! leaves dialectric contant
           real(kind=sp),                        intent(in)  :: rad_freq
           real(kind=sp),                        intent(in)  :: rad_wv
           real(kind=sp),                        intent(in)  :: rad_k0
           ! Locals
#if defined __INTEL_COMPILER
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t1
           !DIR$ ATTRIBUTES ALIGN : 64 :: l4x4phm_t1
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t2
           !DIR$ ATTRIBUTES ALIGN : 64 :: l4x4phm_t2
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t3
           !DIR$ ATTRIBUTES ALIGN : 64 :: l4x4phm_t3
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t1
           !DIR$ ATTRIBUTES ALIGN : 64 :: sm2x2avg_t1
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t2
           !DIR$ ATTRIBUTES ALIGN : 64 :: sm2x2avg_t2
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t3
           !DIR$ ATTRIBUTES ALIGN : 64 :: sm2x2avg_t3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t1     !GCC$ ATTRIBUTES aligned(64) :: l4x4phm_t1
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t2     !GCC$ ATTRIBUTES aligned(64) :: l4x4phm_t2
           real(kind=sp), dimension(4,4,4) :: l4x4phm_t3     !GCC$ ATTRIBUTES aligned(64) :: l4x4phm_t3
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t1 !GCC$ ATTRIBUTES aligned(64) :: sm2x2avg_t1
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t2 !GCC$ ATTRIBUTES aligned(64) :: sm2x2avg_t2
           complex(kind=sp), dimension(2,2,2) :: sm2x2avg_t3 !GCC$ ATTRIBUTES aligned(64) :: sm2x2avg_t3
#endif
           complex(kind=sp), automatic :: j,cwork,work,orient_distr
           real(kind=sp),    automatic :: tr_start1,tr_stop1,dt_rad1
           real(kind=sp),    automatic :: tr_start2,tr_stop2,dt_rad2
           real(kind=sp),    automatic :: tr_start3,tr_stop3,dt_rad3
           real(kind=sp),    automatic :: pr_start1,pr_stop1,dp_rad1
           real(kind=sp),    automatic :: pr_start2,pr_stop2,dp_rad2
           real(kind=sp),    automatic :: pr_start3,pr_stop3,dp_rad3
           real(kind=sp),    automatic :: dp1t1,dp2t2,dp3t3
           real(kind=sp),    automatic :: t0,norm
           real(kind=sp),    automatic :: thinc
           real(kind=sp),    automatic :: phinc
           real(kind=sp),    automatic :: thsc
           real(kind=sp),    automatic :: phsc
           real(kind=sp),    automatic :: thdr
           real(kind=sp),    automatic :: phdr
           real(kind=sp),    automatic :: t1,t2,t3,t4,    &
                                          t5,t6,t7,t8,    &
                                          t9,t10,t11,t12, &
                                          t13,t14,t15,t16
           integer(kind=int4), automatic :: nth1,nth2,nth3
           integer(kind=int4), automatic :: nph1,nph2,nph3
           integer(kind=int4), automatic :: ii,j,l,k,jj
           logical(kind=int4), automatic :: po
           ! EXec code ...
           t0 = ldiam/100.0_sp
           if((rad_wv/t0)<1.5_sp) then
              po = .true.
           else
              po = .false.
           end if
           !! Phase and extiction matrices for leaves
           norm = 6.283185307179586_sp
           call set_leaf_quadrature_bounds(nth1,tr_start1,tr_stop1,dt_rad1, &
                                           nth2,tr_start2,tr_stop2,dt_rad2, &
                                           nth3,tr_start3,tr_stop3,dt_rad3, &
                                           nph1,pr_start1,pr_stop1,dp_rad1, &
                                           nph2,pr_start2,pr_stop2,dp_rad2, &
                                           nph3,pr_start3,pr_stop3,dp_rad3)
           dp1t1 = dp_rad1*dt_rad1
           dp2t2 = dp_rad2*dt_rad2
           dp3t3 = dp_rad3*dt_rad3
           orient_distr = 0.0_sp
           l4x4phm_t1 = 0.0_sp
           sm2x2avg_t1 = cmplx(0.0_sp,0.0_sp)
           if((nth1/=0).and.(nph1/=0)) then
              t1=0.0_sp
              t2=0.0_sp
              t3=0.0_sp
              t4=0.0_sp
              t5=0.0_sp
              t6=0.0_sp
              t7=0.0_sp
              t8=0.0_sp
              t9=0.0_sp
              t10=0.0_sp
              t11=0.0_sp
              t12=0.0_sp
              t13=0.0_sp
              t14=0.0_sp
              t15=0.0_sp
              t16=0.0_sp
              do jj=1, nth1
                 thdr = tr_start1+dt_rad1*real(jj-1,kind=sp)
                 orient_distr = compute_leaf_odf(thdr)
                 if(orient_distr>0.0_sp) then
                    do ii=1, nph1
                       phdr = pr_start1+dp_rad1*real(ii-1,kind=sp)
                       thinc = theta
                       thsc  = 3.141592653589793_sp-theta
                       phinc = 3.141592653589793_sp
                       phsc  = 0.0_sp
                       if(po) then
                           call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                       else
                           call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                       end if
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t1_1_1_1.inc'
#endif
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t1_1_1_1.inc'
#endif
                    ! case 2
                    thinc =  3.141592653589793_sp-theta
                    thsc  =  3.141592653589793_sp-theta
                    phinc =  0.0_sp
                    phsc  =  3.141592653589793_sp
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,2) = t1
                       end do
                    end do
#else
                include 'l4x4phm_t1_1_1_2.inc'
#endif
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,2) = t1
                       end do
                    end do
#else                    
                  include 'l4x4phm_t1_1_1_2.inc'
#endif
                    ! case 3
                    thinc = theta
                    thsc  = theta
                    phinc = 3.141592653589793_sp
                    phsc  = 0.0_sp
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,3) = t1
                       end do
                    end do
#else
                 include 'l4x4phm_t1_1_1_3.inc'
#endif
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,3) = t1
                       end do
                    end do
#else                    
                  include 'l4x4phm_t1_1_1_3.inc'
#endif
                    ! case 4
                    thinc = 3.141592653589793_sp-theta
                    thsc  = theta
                    phinc = 0.0_sp
                    phsc  = 3.141592653589793_sp
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,4) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t1_1_1_4.inc'
#endif
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
                    call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t1(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t1(l,k,4) = t1
                       end do
                    end do
#else
                 include 'l4x4phm_t1_1_1_4.inc'
#endif
                    ! Extinction matrix: case 1
                    thinc = theta
                    thsc  = thinc
                    phinc = 3.141592653589793_sp
                    phsc  = phinc
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
#include 'sm2x2avg_t1_1_1_1.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t1_1_1_1.inc'
                    ! Extinction matrix: case 2
                    thinc =   3.141592653589793_sp-theta
                    thsc  =   3.141592653589793_sp-theta
                    phinc =   0.0_sp
                    phsc  =   phinc
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
#include 'sm2x2avg_t1_1_1_2.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t1_1_1_2.inc'
                 end do
              end if
           end do
        end if
        
        l4x4phm_t2 = 0.0_sp
        sm2x2avg_t2 = cmplx(0.0_sp,0.0_sp)
        if((nth2/=0).and.(nph2/=0)) then
           t1=0.0_sp
           t2=0.0_sp
           t3=0.0_sp
           t4=0.0_sp
           t5=0.0_sp
           t6=0.0_sp
           t7=0.0_sp
           t8=0.0_sp
           t9=0.0_sp
           t10=0.0_sp
           t11=0.0_sp
           t12=0.0_sp
           t13=0.0_sp
           t14=0.0_sp
           t15=0.0_sp
           t16=0.0_sp
           do jj=1, nth2
               thdr = tr_start2+dt_rad2*real(jj-1,kind=sp)
               orient_distr = compute_leaf_odf(thdr)
               if(orient_distr>0.0_sp) then
                  do ii=1, nph2
                     phdr = pr_start2+dp_rad2*real(ii-1,kind=sp)
                     thinc = theta
                     thsc  = 3.141592653589793_sp-theta
                     phinc = 3.141592653589793_sp
                     phsc  = 0.0_sp
                     if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                     else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                     end if
                     call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_1.inc'
#endif
                   scat2x2m(1,2) = -scat2x2m(1,2)
                   scat2x2m(2,1) = -scat2x2m(2,1)
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_1.inc'
#endif
                  ! phase matrix: case 2
                  thinc = 3.141592653589793_sp-theta
                  thsc  = 3.141592653589793_sp-theta
                  phinc = 0.0_sp
                  phsc  = 3.141592653589793_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                  else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                  end if
                  call stokes_matrix(scat2x2m,stokes4x4m)  
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,2) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_2.inc'
#endif
                  scat2x2m(1,2) = -scat2x2m(1,2)
                  scat2x2m(2,1) = -scat2x2m(2,1)
                  call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,2) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_2.inc'
#endif
                  ! phase matric: case 3
                  thinc = theta
                  thsc  = theta
                  phinc =  3.141592653589793_sp
                  phsc  = 0.0_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                  else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                  end if
                  call stokes_matrix(scat2x2m,stokes4x4m)  
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                   do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,3) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_3.inc'
#endif
                  scat2x2m(1,2) = -scat2x2m(1,2)
                  scat2x2m(2,1) = -scat2x2m(2,1)
                  call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                   do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,3) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_3.inc'
#endif
                  ! phase matrix: case 4
                  thinc =  3.141592653589793_sp-theta
                  thsc  = theta
                  phinc = 0.0_sp
                  phsc  =  3.141592653589793_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                  else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                  end if
                  call stokes_matrix(scat2x2m,stokes4x4m)  
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                   do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,4) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_4.inc'
#endif
                  scat2x2m(1,2) = -scat2x2m(1,2)
                  scat2x2m(2,1) = -scat2x2m(2,1)
                  call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                   do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t2(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t2(l,k,4) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t2_1_1_4.inc'
#endif
                  ! Extinction matrix: case 1
                  thinc = theta
                  thsc  = thinc
                  phinc = 3.141592653589793_sp
                  phsc  = phinc
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                  else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                  end if
#include 'sm2x2avg_t2_1_1_1.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t2_1_1_1.inc'
                    ! Extinction matrix: case 2
                    thinc = 3.141592653589793_sp-theta
                    thsc  = 3.141592653589793_sp-theta
                    phinc = 0.0_sp
                    phsc  = phinc
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                    else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                    end if
#include 'sm2x2avg_t2_1_1_2.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t2_1_1_2.inc'
                 end do
              end if
           end do
        end if
        l4x4phm_t3 = 0.0_sp
        sm2x2avg_t3 = cmplx(0.0_sp,0.0_sp)
        if((nth3/=0).and.(nph3/=0)) then
           t1=0.0_sp
           t2=0.0_sp
           t3=0.0_sp
           t4=0.0_sp
           t5=0.0_sp
           t6=0.0_sp
           t7=0.0_sp
           t8=0.0_sp
           t9=0.0_sp
           t10=0.0_sp
           t11=0.0_sp
           t12=0.0_sp
           t13=0.0_sp
           t14=0.0_sp
           t15=0.0_sp
           t16=0.0_sp
           do jj=1, nth3
               thdr = tr_start3+dt_rad3*real(jj-1,kind=sp)
               orient_distr = compute_leaf_odf(thdr)
               if(orient_distr>0.0_sp) then
                  do ii=1, nph3
                     phdr = pr_start3+dp_rad3*real(ii-1,kind=sp)
                     thinc = theta
                     thsc  = 3.141592653589793_sp-theta
                     phinc = 3.141592653589793_sp
                     phsc  = 0.0_sp
                     if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                     else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                     end if
                     call stokes_matrix(scat2x2m,stokes4x4m)
                     ! phase matrix: case 1
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_1.inc'
#endif
                   scat2x2m(1,2) = -scat2x2m(1,2)
                   scat2x2m(2,1) = -scat2x2m(2,1)
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,1)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,1) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_1.inc'
#endif
                  ! phase matrix: case 2
                  thinc = 3.141592653589793_sp-theta
                  thsc  = 3.141592653589793_sp-theta
                  phinc = 0.0_sp
                  thsc  = 3.141592653589793_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                   else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                   end if
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,2) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_2.inc'
#endif
                   scat2x2m(1,2) = -scat2x2m(1,2)
                   scat2x2m(2,1) = -scat2x2m(2,1)
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,2)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,2) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_2.inc'
#endif
                  ! phase matrix: case 3
                  thinc = theta
                  thsc  = theta
                  phinc = 3.141592653589793_sp
                  phsc  = 0.0_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                   else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                   end if
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,3) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_3.inc'
#endif
                   scat2x2m(1,2) = -scat2x2m(1,2)
                   scat2x2m(2,1) = -scat2x2m(2,1)
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,3)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,3) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_3.inc'
#endif
                  ! phase matrix: case 4
                  thinc = 3.141592653589793_sp-theta
                  thsc  = theta
                  phinc = 0.0_sp
                  phsc  = 3.141592653589793_sp
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                   else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                   end if
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,4) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_4.inc'
#endif
                   scat2x2m(1,2) = -scat2x2m(1,2)
                   scat2x2m(2,1) = -scat2x2m(2,1)
                   call stokes_matrix(scat2x2m,stokes4x4m)
#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 4
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 4
                          t1 = l4x4phm_t3(l,k,4)+orient_distr*stokes4x4m(l,k)
                          l4x4phm_t3(l,k,4) = t1
                       end do
                    end do
#else
                  include 'l4x4phm_t3_1_1_4.inc'
#endif
                  ! Extinction matrix: case 1
                  thinc = theta
                  thsc  = thinc
                  phinc = 3.141592653589793_sp
                  phsc  = phinc
                  if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                  else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                  end if
#include 'sm2x2avg_t3_1_1_1.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t3_1_1_1.inc'
                    ! Extinction matrix: case 2
                    thinc =  3.141592653589793_sp-theta
                    thsc  =  3.141592653589793_sp-theta
                    phinc = 0.0_sp
                    phsc  = phinc
                    if(po) then
                       call leaf_phys_optics_approx(thinc,phinc,thsc,phsc, &
                                                    thdr,phdr,rad_freq,rad_k0, &
                                                    rad_wv,lmg,lrho,ldens,   &
                                                    ldiam,lthick,epsr,epsrc, &
                                                    scat2x2m)
                   else
                       call leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                                    rad_freq,rad_k0,rad_wv,lmg,  &
                                                    lrho,ldens,ldiam,    &
                                                    lthick,epsr,epsrc,scat2x2m)
                   end if
#include 'sm2x2avg_t3_1_1_2.inc'
                    scat2x2m(1,2) = -scat2x2m(1,2)
                    scat2x2m(2,1) = -scat2x2m(2,1)
#include 'sm2x2avg_t3_1_1_2.inc'
                 end do
              end if
           end do
        end if
        ! Phase and M matrices
        work = 1.0_sp/norm
        do i=1,4
#if defined __INTEL_COMPILER
           !DIR$ UNROLL_AND_JAM (4)
#endif
           do k=1,4
#if defined __INTEL_COMPILER
              !DIR$ SIMD
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
              !GCC$ UNROLL 4
#endif
              do l=1,4
                 l4x4phm(l,k,i) = cwork*(dp1t1*l4x4phm_t1(l,k,i)+ &
                                         dp2t2*l4x4phm_t2(l,k,i)+ &
                                         dp3t3*l4x4phm_t3(l,k,i))
              end do
           end do
        end do
        cwork = j*6.283185307179586_sp/(norm*rad_k0)
        ! Loop fusion
        do k=1,2
#if defined __INTEL_COMPILER
           !DIR$ SIMD
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           !GCC$ VECTOR
#endif
           do l=1,2
              l2x2mp(l,k) = cwork*(dp1t1*sm2x2avg_t1(l,k,1)+ &
                                   dp2t2*sm2x2avg_t2(l,k,1)+ &
                                   dp3t3*sm2x2avg_t3(l,k,1))
              l2x2mn(l,k) = cwork*(dp1t1*sm2x2avg_t1(l,k,2)+ &
                                   dp2t2*sm2x2avg_t2(l,k,2)+ &
                                   dp3t3*sm2x2avg_t3(l,k,2))
           end do
        end do
        
     end subroutine compute_leaf_phase_matrices

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine leaf_dielectric(epsr,leaf_mg,leaf_rho,leaf_dens,  &
                                leaf_diam,leaf_tau,dry_dens,      &
                           soil_tmp,water_tmp,veg_tmp,theta,rad_freq) !GCC$ ATTRIBUTES hot :: leaf_dielectric !GCC$ ATTRIBUTES aligned(16)  :: leaf_dielectric !GCC$ ATTRIBUTES inline :: leaf_dielectric
#elif defined __INTEL_COMPILER
                        !DIR$ ATTRIBUTES INLINE :: leaf_dielectric
      subroutine leaf_dielectric(epsr,leaf_mg,leaf_rho,leaf_dens,  &
                                leaf_diam,leaf_tau,dry_dens,      &
                                water_tmp,veg_tmp,theta,rad_freq)
                        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: leaf_dielectric
#endif
                 complex(kind=sp),       intent(out) :: epsr
                 real(kind=sp),          intent(in)  :: leaf_mg,leaf_rho,leaf_dens, &
                                                        leaf_diam,leaf_tau
                 logical(kind=int4),     intent(in)  :: dry_dens
                 real(kind=sp),          intent(in)  :: water_tmp,veg_tmp, &
                                                        theta,rad_freq
                 ! Exec code ....
                 if(dry_dens) then
                    call veg_dielectric_2(epsr,leaf_mg,leaf_rho,veg_tmp,theta,rad_freq)
                 else
                    call veg_dielectric_1(epsr,leaf_mg,veg_tmp,theta,rad_freq)
                 end if
      end subroutine leaf_dielectric
      
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      subroutine veg_dielectric_2(epsr,mg,veg_rho, &
           tempC,theta,rad_freq) !GCC$ ATTRIBUTES hot :: veg_dielectric_2 !GCC$ ATTRIBUTES aligned(64) :: veg_dielectric_2 !GCC$ ATTRIBUTES inline :: veg_dielectric_2
#elif defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: veg_dielectric_2
      subroutine veg_dielectric_2(epsr,mg,veg_rho, &
           tempC,theta,rad_freq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: veg_dielectric_2
#endif
           complex(kind=sp),   intent(out) :: epsr
           real(kind=sp),      intent(in)  :: mg,veg_rho,tempC,theta,rad_freq
           ! Locals
           complex(kind=sp), automatic :: e,f,g,w
           real(kind=sp),    automatic :: mv,a,b,c,d
           real(kind=sp),    automatic :: top,fn,en,ein
           real(kind=sp),    automatic :: t0
           ! Exec code ....
           mv = mg*veg_rho/(1.0_sp*(1.0_sp-veg_rho))
           t0 = mv*mv
           a  = 1.7_sp+3.20_sp*mv+6.5_sp*t0
           b  = mv*(0.82_sp*mv+0.166_sp)
           c  = 31.4_sp*t0/(59.5_sp*t0+1.0_sp)
           !
           top = 1.1109e-10_sp+tempC*(-3.824e-12_sp+tempC* &
                (6.938e-14_sp-tempC*5.096e-16_sp))
           fn  = 1.0_sp/(top*1.09_sp)
           en  = 88.045_sp+tempC*(-0.4147_sp+tempC*(6.295e-4_sp +  &
                tempC*1.075e-5_sp))
           ein = 4.9_sp
           e = cmplx(1.0_sp,(rad_freq/fn))
           d = 22.74_sp
           f = cmplx(0.0_sp,(d/rad_freq))
           w = 0.707106781186548_sp*cmplx(1.0_sp,1.0_sp)*sqrt(rad_freq/0.18_sp)
           g = 1.0_sp*w
           epsr = a+b*(4.9_sp+(en-ein)/e-f)+c*(2.9_sp+55.0_sp/g)
     end subroutine veg_dielectric_2

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine veg_dielectric_1(epsr,mg,tempC,theta,rad_freq) !GCC$ ATTRIBUTES hot :: veg_dielectric_1 !GCC$ ATTRIBUTES aligned(64) :: veg_dielectric_1 !GCC$ ATTRIBUTES inline :: veg_dielectric_1
#elif defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: veg_dielectric_1
     subroutine veg_dielectric_1(epsr,mg,tempC,theta,rad_freq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: veg_dielectric_1 
#endif 
           complex(kind=sp),   intent(out) :: epsr
           real(kind=sp),      intent(in)  :: mg,tempC,theta,rad_freq
           ! Locals
           complex(kind=sp),  automatic :: e,f,g,w
           real(kind=sp),     automatic :: top,fn,en,ein,t0
           real(kind=sp),     automatic :: a,b,c,d
           ! Exec code ....
           t0 = mg*mg
           a  = 1.7_sp-0.74_sp*mg+6.16_sp*t0
           b  = mg*(0.55_sp*mg-0.076_sp)
           c = 4.64_sp*t0/(7.36_sp*t0+1.0_sp)
           d = 22.74_sp
           top = 1.1109e-10_sp+tempC*(-3.824e-12_sp+tempC* &
                (6.938e-14_sp-tempC*5.096e-16_sp))
           fn  = 1.0_sp/(top*1.09_sp)
           en  = 88.045_sp+tempC*(-0.4147_sp+tempC*(6.295e-4_sp +  &
                tempC*1.075e-5_sp))
           ein = 4.9_sp
           e = cmplx(1.0_sp,(rad_freq/fn))
           f = cmplx(0.0_sp,(d/rad_freq))
           w = 0.707106781186548_sp*cmplx(1.0_sp,1.0_sp)*sqrt(rad_freq/0.18_sp)
           g = 1.0_sp*w
           epsr = a+b*(4.9_sp+(en-ein)/e-f)+c*(2.9_sp+55.0_sp/g)
     end subroutine veg_dielectric_1
     
      
     
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine set_leaf_quadrature_bounds(nth1,tr_start1,tr_stop1,dt_rad1, &
                                           nth2,tr_start2,tr_stop2,dt_rad2, &
                                           nth3,tr_start3,tr_stop3,dt_rad3, &
                                           nph1,pr_start1,pr_stop1,dp_rad1, &
                                           nph2,pr_start2,pr_stop2,dp_rad2, &
                                           nph3,pr_start3,pr_stop3,dp_rad3) !GCC$ ATTRIBUTES cold :: set_leaf_quadrature_bounds !GCC$ ATTRIBUTES aligned(16) :: set_leaf_quadrature_bounds
#elif defined __INTEL_COMPILER
      subroutine set_leaf_quadrature_bounds(nth1,tr_start1,tr_stop1,dt_rad1, &
                                           nth2,tr_start2,tr_stop2,dt_rad2, &
                                           nth3,tr_start3,tr_stop3,dt_rad3, &
                                           nph1,pr_start1,pr_stop1,dp_rad1, &
                                           nph2,pr_start2,pr_stop2,dp_rad2, &
                                           nph3,pr_start3,pr_stop3,dp_rad3)
                    !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: set_leaf_quadrature_bounds
#endif

              integer(kind=int4),     intent(inout) :: nth1
              real(kind=sp),          intent(inout) :: tr_start1,tr_stop1,dt_rad1
              integer(kind=int4),     intent(inout) :: nth2
              real(kind=sp),          intent(inout) :: tr_start2,tr_stop2,dt_rad2
              integer(kind=int4),     intent(inout) :: nth3
              real(kind=sp),          intent(inout) :: tr_start3,tr_stop3,dt_rad3
              integer(kind=int4),     intent(inout) :: nph1
              real(kind=sp),          intent(inout) :: pr_start1,pr_stop1,dp_rad1
              integer(kind=int4),     intent(inout) :: nph2
              real(kind=sp),          intent(inout) :: pr_start2,pr_stop2,dp_rad2
              integer(kind=int4),     intent(inout) :: nph3
              real(kind=sp),          intent(inout) :: pr_start3,pr_stop3,dp_rad3
              ! LOcals
              real(kind=sp), automatic :: td_start1,td_stop1,dt_deg1, &
                                          td_start2,td_stop2,dt_deg2, &
                                          td_start3,td_stop3,dt_deg3 &
                                          pd_start1,pd_stop1,dp_deg1, &
                                          pd_start2,pd_stop2,dp_deg2, &
                                          pd_start3,pd_stop3,dp_deg3
              real(kind=sp), parameter :: t0 = 0.017453292519943_sp
              ! EXec code ...
              td_start1 = 2.5_sp
              td_stop1 = 177.5_sp
              dt_deg1  = 5.0_sp
              nth1  = 35
              tr_start1 = t0*td_start1
              tr_stop1  = t0*td_stop1
              dt_rad1   = t0*dt_deg1
              pd_start1 = 2.5_sp
              pd_stop1 = 177.5_sp
              dp_deg1 = 5.0_sp
              nph1 = 36
              pr_start1 = t0*pd_start1
              pr_stop1  = t0*pd_stop1
              dp_rad1   = t0*dp_deg1
              td_start2 = 0.0_sp
              td_stop2 = 0.0_sp
              dt_deg2 = 0.0_sp
              nth2 = 0
              tr_start2 = 0.0_sp
              tr_stop2  = 0.0_sp
              dt_rad2   = 0.0_sp
              pd_start2 = 0.0_sp
              pd_stop2 = 0.0_sp
              dp_deg2 = 0.0_sp
              nph2 = 0
              pr_start2 = 0.0_sp
              pr_stop2 = 0.0_sp
              dp_rad2 = 0.0_sp
              td_start3 = 0.0_sp
              td_stop3 = 0.0_sp
              dt_deg3 = 0.0_sp
              nth3 = 0
              tr_start3 = 0.0_sp
              tr_stop3  = 0.0_sp
              dt_rad3 = 0.0_sp
              pd_start3 = 0.0_sp
              pd_stop3 = 0.0_sp
              pd_deg3 = 0.0_sp
              nph3 = 0
              pr_start3 = 0.0_sp
              pr_stop3 = 0.0_sp
              dp_rad3 = 0.0_sp
      end subroutine set_leaf_quadrature_bounds
      
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      real(kind=sp) function compute_leaf_odf(th) !GCC$ ATTRIBUTES hot :: compute_leaf_odf !GCC$ ATTRIBUTES aligned(16) :: compute_leaf_odf !GCC$ ATTRIBUTES inline :: compute_leaf_odf
#elif defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: compute_leaf_odf
      real(kind=sp) function compute_leaf_odf(th)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: compute_leaf_odf
#endif
            use mod_tree_orient
            use, intrinsic :: IEEE_ARITHMETIC
            real(kind=sp),      intent(in) :: th
            ! Locals
            real(kind=sp), automatic :: mu,nu,pi_tol
            real(kind=sp), paramaeter :: tol = 8.727e-7_sp
            ! Exec code ....
            if(leaf_orient<1 .or. leaf_orient>7) then
               compute_leaf_odf = IEEE_VALUE(IEEE_QUIET_NAN)
               return
            end if
            pi_tol = 1.570796326794897_sp-tol
            if(leaf_orient==1) then
               if(th>1.570796326794897_sp) then
                  compute_leaf_odf = 0.0_sp
               else if(th==1.570796326794897_sp) then
                  compute_leaf_odf = 0.5_sp
               else
                  compute_leaf_odf = sin(th)
               end if
            else if(leaf_orient==2) then
               if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 2.770_sp
                  mu = 1.172_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            else if(leaf_orient==3) then
               if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 1.1720_sp
                  mu = 2.7700_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            else if(leaf_orient==4) then
               if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 3.326_sp
                  mu = 3.326_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            else if(leaf_orient==5) then
               if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 0.433_sp
                  mu = 0.433_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            else if (leaf_orient==6) then
               if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 1.0_sp
                  mu = 1.0_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            else if(leaf_orient==7) then
                 if(th>=pi_tol) then
                  compute_leaf_odf = 0.0_sp
               else if(th<=tol) then
                  compute_leaf_odf = 0.0_sp
               else
                  nu = 1.101_sp
                  mu = 1.930_sp
                  call leaf_angle_orient(mu,nu,th,compute_leaf_odf)
               end if
            end if
          end function compute_leaf_odf
      

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine  leaf_angle_orient(mu,nu,th,pdf) !GCC$ ATTRIBUTES hot :: leaf_angle_orient !GCC$ ATTRIBUTES aligned(64) :: leaf_angle_orient !GCC$ ATTRIBUTES inline :: leaf_angle_orient
#elif defined __INTEL_COMPILER
             !DIR$ ATTRIBUTES INLINE :: leaf_angle_orient
     subroutine  leaf_angle_orient(mu,nu,th,pdf)
             !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: leaf_angle_orient
#endif
           real(kind=sp),      intent(in)    :: mu
           real(kind=sp),      intent(in)    :: nu
           real(kind=sp),      intent(in)    :: th
           real(kind=sp),      intent(inout) :: pdf
           ! Locals
           real(kind=sp), automatic :: t0,t1,t2
           real(kind=sp), parameter :: two_over_pi = 0.636619772367581_sp
           real(kind=sp), parameter :: half_pi     = 1.570796326794897_sp
           t0 = two_over_pi*exp(gamma(mu+nu)-gamma(mu)-gamma(nu))
           t1 = 1.0_sp-th/half_pi**(nu-1.0_sp)
           t2 = th/half_pi**(mu-1.0_sp)
           pdf = t0*t1*t2
      end subroutine  leaf_angle_orient
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      subroutine leaf_phys_optics_approx(thinc,phinc,thsc,phsc,     &
                                         thdr,phdr,rad_freq,rad_k0, &
                                         rad_wv,leaf_mg,leaf_rho,   &
                                         leaf_dens,leaf_diam,leaf_tau, &
                                         epsr,epsrc,scat_mat) !GCC$ ATTRIBUTES hot :: leaf_phys_optics_approx !GCC$ ATTRIBUTES aligned(16) :: leaf_phys_optics_approx
#elif defined __INTEL_COMPILER
       subroutine leaf_phys_optics_approx(thinc,phinc,thsc,phsc,     &
                                         thdr,phdr,rad_freq,rad_k0, &
                                         rad_wv,leaf_mg,leaf_rho,   &
                                         leaf_dens,leaf_diam,leaf_tau, &
                                         epsr,epsrc,scat_mat)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: leaf_phys_optics_approx
#endif
           real(kind=sp),                     intent(in) :: thinc
           real(kind=sp),                     intent(in) :: phinc
           real(kind=sp),                     intent(in) :: thsc
           real(kind=sp),                     intent(in) :: phsc
           real(kind=sp),                     intent(in) :: thdr
           real(kind=sp),                     intent(in) :: phdr
           real(kind=sp),                     intent(in) :: rad_freq
           real(kind=sp),                     intent(in) :: rad_k0
           real(kind=sp),                     intent(in) :: rad_wv
           real(kind=sp),                     intent(in) :: leaf_mg
           real(kind=sp),                     intent(in) :: leaf_rho
           real(kind=sp),                     intent(in) :: leaf_dens
           real(kind=sp),                     intent(in) :: leaf_diam
           real(kind=sp),                     intent(in) :: leaf_tau
           complex(kind=sp),  dimension(12),  intent(in) :: epsr
           complex(kind=sp),  dimension(12),  intent(in) :: epsrc
           complex(kind=sp),  dimension(2,2), intent(inout) :: scat_mat
           ! LOcals
           complex(kind=sp), automatic :: j,eps,res,const
           complex(kind=sp), automatic :: gamh,game,gamhe_c1,gamhe_c2
           real(kind=sp), automatic :: tau,q,p,a,b,tol
           real(kind=sp), automatic :: cosphi1
           real(kind=sp), automatic :: thj,phij
           real(kind=sp), automatic :: sin_thi,cos_thi,sin_thj,cos_thj
           real(kind=sp), automatic :: sin_ths,cos_ths,cos_phj,sin_phij,sin_phji
           real(kind=sp), automatic :: cos_phij,cos_phji,sin_phsj,sin_phjs,cos_phjs
           real(kind=sp), automatic :: cos_beta,sin_beta,sin_phi,cos_phi,cos_beta_phi
           real(kind=sp), automatic :: sin_phpr,cos_phpr,sin_betapr
           real(kind=sp), automatic :: u,v,sinu,sinv,sinu_u,sinv_v
           real(kind=sp), automatic :: cosb_p,sintj_ti,sints_tj
           real(kind=sp), automatic :: costj_sinpjs,costs_sinpsj
           real(kind=sp), automatic :: sinpij_cospsj,cpij_cti_ctj,cpsj_cts_ctj
           real(kind=sp), automatic :: s1,s2,s3,s4,s5,w1,w2,t0,t1,t2,t3,t4
           ! Exec code ....
           tol = 0.0001_sp
           t0  = sin(thdr)
           j = cmplx(0.0_sp,1.0_sp)
           t1 = sin(thinc)
           tau = leaf_tau/100.0_sp
           t2 = cos(phdr-phinc)
           eps = epsr(4)
           t3 = cos(thinc)
           a = 0.5_sp*1.772453850905516_sp*leaf_diam/100.0_sp
           t4 = cos(thdr)
           b = a
           cosphi1 = -(t0*t1*t2*t3*t4)
           if(cosphi1<0.0_sp) then
              thj = 3.141592653589793_sp-thinc
              phij = 3.141592653589793_sp+phinc
              cosphi1 = -cosphi1
           else
              thj = thinc
              phij = phinc
           end if
           sin_thi = t1
           cos_thi = t3
           sin_thj = sin(thj)
           cos_thj = cos(thj)
           sin_ths = sin(thsc)
           cos_ths = cos(thsc)
           cos_phij = cos(phij)
           sin_phij = sin(phinc-phij)
           sin_phji = -sin_phij
           w1 = sin_thi*sin_phji
           cos_phij = cos(phinc-phij)
           q = 1.0_sp/sqrt(1.0_sp-w1*w1)
           cos_phji = cos_phij
           cos_beta = q*cosphi1
           sin_phsj = sin(phsc-phij)
           sin_beta = q*(-cos_thj*sin_thi*cos_phji+&
                         cos_thi*sin_thj) 
           sin_phjs = -sin_phsj
           sin_phi  = sin_thi*sin_phij
           cos_phsj = cos(phsc-phij)
           cos_phi  = sqrt(1.0_sp-sin_phi*sin_phi)
           cos_beta_phi = cos_beta*cos_phi
           sin_phpr = sin_ths*sin_phsj
           cos_phpr = sqrt(1.0_sp-sin_phpr*sin_phpr)
           sin_betapr = (cos_ths*sin_thi- &
                cos_thj*sin_ths*cos_phsj)/cos_phpr
           p = 1.0_sp/sqrt(1.0_sp-cos_beta_phi*cos_beta_phi)
           res = j/(rad_k0*tau*(eps-1.0_sp))
           gamh = 1.0_sp/(1.0_sp+2.0_sp*res/cosphi1)
           game = 1.0_sp/(1.0_sp+2.0_sp*res*cosphi1)
           u = 0.5_sp*rad_k0*a*(sin_phi-sin_phpr)
           sinu = sin(u)
           v = 0.5_sp*rad_k0*b*(sin_beta*cos_phi-sin_betapr*cos_phpr)
           sinv = sin(v)
           if(abs(sinu)<=tol) then
              sinu_u = 1.0_sp
           else
              sinu_u = sinu/u
           end if
           cosb_p = cos_beta*cos_phi
           gamhe_c1 = (gamh-game)*cosb_p
           gamhe_c2 = gamh-cosb_p*cosb_p*game
           !
           if(abs(sinv)<=tol) then
              sinv_v = 1.0_sp
           else
              sinv_v = sinv/v
           end if
           const = -j*rad_k0*a*b*sinv_v*sinu_u*p*p/6.283185307179586_sp
           sintj_ti = sin_thj*sin_thi
           sints_tj = sin_ths*sin_thj
           costj_sinpjs = cos_thj*sin_phjs
           s5 = sin_phij*costj_sinpjs
           costs_sinpsj = cos_ths*sin_phsj
           s3 = sin_phij*costs_sinpsj
           sinpij_cospsj = sin_phij*cos_phsj
           cpij_cti_ctj  = cos_phij*cos_thi*cos_thj
           s1 = sintj_ti+cpij_cti_ctj
           cpsj_cts_ctj  = cos_phsj*cos_ths*cos_thj
           s2 = sints_tj+cpsj_cts_ctj
           w1 = s1*s2+cos_thi*s3
           s4 = sin_phij*s2
           w2 = cos_phij*s2+cos_thj*s3
           scat_mat(1,1) = const*(w1*gamhe_c1+w2*gamhe_c2)
           w1 = -cos_thj*s4+cos_phij*costs_sinpsj
           w2 = -cos_thi*s4+s1*costs_sinpsj
           scat_mat(1,2) = const*(w1*gamhe_c1+w2*gamhe_c2)
           w1 = s1-costj_sinpjs+cos_thi*sinpij_cospsj
           w2 = cos_phij*costj_sinpjs+cos_thj*sinpij_cospsj
           scat_mat(2,1) = const*(w1*gamhe_c1+w2*gamhe_c2)
           w1 = -cos_phij*s5+cos_phij*cos_phsj
           w2 = -cos_thi*s5+s1*cos_phsj
           scat_mat(2,2) = const*(w1+gamhe_c1+w2*gamhe_c2)
     end subroutine leaf_phys_optics_approx

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                         rad_freq,rad_k0,rad_wv,leaf_mg,  &
                                         leaf_rho,leaf_dens,leaf_diam,    &
                                         leaf_tau,epsr,epsrc,scat_mat) !GCC$ ATTRIBUTES hot :: leaf_rayleigh_scattering !GCC$ ATTRIBUTES aligned(16) :: leaf_rayleigh_scattering
#elif defined __INTEL_COMPILER
     subroutine leaf_rayleigh_scattering(thinc,phinc,thsc,phsc,thdr,phdr, &
                                         rad_freq,rad_k0,rad_wv,leaf_mg,  &
                                         leaf_rho,leaf_dens,leaf_diam,    &
                                         leaf_tau,epsr,epsrc,scat_mat)
               !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: leaf_rayleigh_scattering
#endif 
           real(kind=sp),                     intent(in) :: thinc
           real(kind=sp),                     intent(in) :: phinc
           real(kind=sp),                     intent(in) :: thsc
           real(kind=sp),                     intent(in) :: phsc
           real(kind=sp),                     intent(in) :: thdr
           real(kind=sp),                     intent(in) :: phdr
           real(kind=sp),                     intent(in) :: rad_freq
           real(kind=sp),                     intent(in) :: rad_k0
           real(kind=sp),                     intent(in) :: rad_wv
           real(kind=sp),                     intent(in) :: leaf_mg
           real(kind=sp),                     intent(in) :: leaf_rho
           real(kind=sp),                     intent(in) :: leaf_dens
           real(kind=sp),                     intent(in) :: leaf_diam
           real(kind=sp),                     intent(in) :: leaf_tau
           complex(kind=sp),  dimension(12),  intent(in) :: epsr
           complex(kind=sp),  dimension(12),  intent(in) :: epsrc
           complex(kind=sp),  dimension(2,2), intent(inout) :: scat_mat
           ! LOcals
#if defined __INTEL_COMPILER
           real(kind=sp), dimension(4), automatic :: xhat,yhat,zhat
           !DIR$ ATTRIBUTES ALIGN : 16 :: xhat,yhat,zhat
           real(kind=sp), dimension(4), automatic :: xhatl,yhatl,zhatl
           !DIR$ ATTRIBUTES ALIGN : 16 :: xhatl,yhatl,zhatl
           real(kind=sp), dimension(4), automatic :: khati,khats
           !DIR$ ATTRIBUTES ALIGN : 16 :: khati,khats
           real(kind=sp), dimension(4), automatic :: hhati,vhati,hhats,vhats
           !DIR$ ATTRIBUTES ALIGN : 16 :: hhati,vhati,hhats,vhats
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           real(kind=sp), dimension(4), automatic :: xhat,yhat,zhat     !GCC$ ATTRIBUTES aligned(16) :: xhat,yhat,zhat
           real(kind=sp), dimension(4), automatic :: xhatl,yhatl,zhatl  !GCC$ ATTRIBUTES aligned(16) :: xhatl,yhatl,zhatl
           real(kind=sp), dimension(4), automatic :: khati,khats        !GCC$ ATTRIBUTES aligned(16) :: khati,khats
           real(kind=sp), dimension(4), automatic :: hhati,vhati,hhats,vhats !GCC$ ATTRIBUTES aligned(16) :: hhati,vhati,hhats,vhats
#endif
           complex(kind=sp), automatic ::   eps,cdum
           complex(kind=sp), automatic ::   Vd, cduma, cdumb, cdumc
           real(kind=sp),    automatic ::   dum,dum2,sumcheck,t_lf, d_lf
           real(kind=sp),    automatic ::   vhsdxhl,vhsdyhl,vhsdzhl,hhsdxhl,hhsdyhl,hhsdzhl
           real(kind=sp),    automatic ::   vhidxhl,vhidyhl,vhidzhl,hhidxhl,hhidyhl,hhidzhl
           real(kind=sp),    automatic ::   Ae,Be,Ce,Ac,Ab,Aa,Vo
           real(kind=sp),    automatic ::   t0,t1,t2,t3,t4,t5,t6,t7,t8,t9
           ! Exec code ....
          
         
           xhat(1) = 1.0_sp
           xhat(2) = 0.0_sp
           xhat(3) = 0.0_sp
           xhat(4) = 0.0_sp
           t0 = sin(thinc)
           t1 = cos(phinc)
           khati(1) = t0*t1
           yhat(1) = 0.0_sp
           t6 = cos(thdr)
           yhat(2) = 1.0_sp
           t7 = cos(phdr)
           yhat(3) = 0.0_sp
           yhat(4) = 0.0_sp
           t8 = sin(thdr)
           khati(2) = t0*t1
           zhat(1) = 0.0_sp
           zhat(2) = 0.0_sp
           t9 = sin(phdr)
           zhat(3) = 1.0_sp
           zhat(4) = 0.0_sp
           khati(3) = t1
           khati(4) = 1.0_sp
           t2 = sin(thsc)
           t3 = cos(phsc)
           call vec1x3_smooth(khati,khati)
           t4 = sin(phsc)
           t5 = sin(phinc)
           khats(1) = t2*t3
           khats(2) = t2*t4
           khats(3) = cos(thsc)
           khats(4) = 1.0_sp
           call vec1x3_smooth(khats,khats)
           hhati(1) = -t5
           hhati(2) = t1
           hhati(3) = 0.0_sp
           hhati(4) = 1.0_sp
           call vec1x3_smooth(hhati,hhati)
           hhats(1) = -t4
           call cross_prod(hhati,khati,vhati)
           hhats(2) = t3
           call vec1x3_smooth(vhati,vhati)
           hhats(3) = 0.0_sp
           hhats(4) = 1.0_sp
           call vec1x3_smooth(hhats,hhats)
           call cross_prod(hhats,khats,vhats)
           call vec1x3_smooth(vhats,vhats)
           xhatl(1) = t6*t7
           xhatl(2) = t6*t8
           xhatl(3) = -t8
           xhatl(4) = 1.0_sp
           call vec1x3_smooth(xhatl,xhatl)
           yhatl(1) = -t8
           yhatl(2) = t7
           yhatl(3) = 0.0_sp
           yhatl(4) = 1.0_sp
           call vec1x3_smooth(yhatl,yhatl)
           zhatl(1) = t8*t7
           zhatl(2) = t8*t9
           zhatl(3) = t6
           zhatl(4) = 1.0_sp
           call vec1x3_smooth(zhatl,zhatl)
           t_lf = leaf_tau
           d_lf = leaf_diam
           eps  = epsrc(4)
           call dot_prod(vhats,xhatl,vhsdxhl)
           call dot_prod(vhats,yhatl,vhsdyhl)
           call dot_prod(vhats,zhatl,vhsdzhl)
           dum = 1.5_sp**0.33333333333333_sp
           Ce  = (t_lf/200.0_sp)*dum
           Ae  = (d_lf/200.0_sp)*dum
           Be  = Ae
           call dot_prod(hhats,xhatl,hhsdxhl)
           call dot_prod(hhats,yhatl,hhsdyhl)
           call dot_prod(hhats,zhatl,hhsdzhl)
           dum = Ae*Ae-Ce*Ce
           dum2 = (sqrt(dum))/Ce
           Ac =  (2.0_sp/(dum**1.5_sp))*(dum2-atan(dum2))
           Ab =  (2.0/(Ae*Be*Ce)-Ac)*0.5_sp
           Aa = Ab
           call dot_prod(vhati,xhatl,vhidxhl)
           call dot_prod(vhati,yhatl,vhidyhl)
           call dot_prod(vhati,zhatl,vhidzhl)
           Vo = 12.566370614359173_sp*Ae*Be*Ce*0.3333333333333_sp
           Vd = (Ae*Be*Ce*0.5_sp)*(eps-1.0_sp)
           call dot_prod(hhati,xhatl,hhidxhl)
           call dot_prod(hhati,yhatl,hhidyhl)
           call dot_prod(hhati,zhatl,hhidzhl)
           cdum = (rad_k0*rad_k0/12.566370614359173_sp)*Vo*(eps-1.0_sp)
           cduma = 1.0_sp+ Vd*Aa
           cdumb = cduma
           cdumc = 1.0_sp+ Vd*Ac
           scat_mat(1,1) = cdum*(vhsdxhl*vhidxhl/cduma + &
                           vhsdyhl*vhidyhl/cdumb + vhsdzhl*vhidzhl/cdumc)

           scat_mat(1,2) = cdum*(hhsdxhl*vhidxhl/cduma + &
                         hhsdyhl*vhidyhl/cdumb + hhsdzhl*vhidzhl/cdumc)

           scat_mat(2,1) = cdum*(vhsdxhl*hhidxhl/cduma + &
                           vhsdyhl*hhidyhl/cdumb + vhsdzhl*hhidzhl/cdumc)

           scat_mat(2,2) = cdum*(hhsdxhl*hhidxhl/cduma + &
                           hhsdyhl*hhidyhl/cdumb + hhsdzhl*hhidzhl/cdumc)
     end subroutine leaf_rayleigh_scattering
     
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine vec1x3_smooth(in,out) !GCC$ ATTRIBUTES hot :: vec1x3_smooth !GCC$ ATTRIBUTES aligned(64) :: vec1x3_smooth !GCC$ ATTRIBUTES inline :: vec1x3_smooth
#elif defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: vec1x3_smooth
     subroutine vec1x3_smooth(in,out)
       !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: vec1x3_smooth
       !DIR$ ATTRIBUTES VECTOR :: vec1x3_smooth
#endif
         real(kind=sp), dimension(4),  intent(inout)    :: in
         real(kind=sp), dimension(4),  intent(inout) :: out
         !Locals
         real(kind=sp), automatic :: mag
         ! Exec code ....
         mag = sqrt(in(1)*in(1)+in(2)*in(2)+in(3)*in(3))
         if(mag/=0.0_sp) then
            in = in/mag
            call check_mag_tol(in,out)
            mag = sqrt(in(1)*in(1)+in(2)*in(2)+in(3)*in(3))
            out = in/mag
         else
            out = in
         end if
     end subroutine vec1x3_smooth
     
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine check_mag_tol(in,out) !GCC$ ATTRIBUTES hot :: check_mag_tol !GCC$ ATTRIBUTES aligned(64) :: check_mag_tol !GCC$ ATTRIBUTES inline :: check_mag_tol
#elif defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: check_mag_tol
     subroutine check_mag_tol(in,out)
       !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: check_mag_tol
       !DIR$ ATTRIBUTES VECTOR :: check_mag_tol
#endif
          real(kind=sp), dimension(4), intent(in) :: in
          real(kind=sp), dimension(4), intent(inout) :: out
          ! Locals
          real(kind=sp), parameter :: tol = 0.00001_sp
          logical(kind=int4), automatic :: bres
          ! Exec code
          bres = .false.
          bres = in<tol
          if(bres) then
             out = 0.0_sp
          else
             out = in
          end if
     end subroutine check_mag_tol

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine cross_prod(a,b,c) !GCC$ ATTRIBUTES hot :: cross_prod !GCC$ ATTRIBUTES aligned(64) :: cross_prod !GCC$ ATTRIBUTES inline :: cross_prod
#elif defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: cross_prod
       subroutine cross_prod(a,b,c)
        !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: cross_prod
#endif
         real(kind=sp), dimension(4), intent(in) :: a
         real(kind=sp), dimension(4), intent(in) :: b
         real(kind=sp), dimension(4), intent(inout) :: c
         ! Exec code ....
         c(1) = a(2)*b(3)-a(3)*b(2)
         c(2) = a(3)*b(1)-a(1)*b(3)
         c(3) = a(1)*b(2)-a(2)*b(1)
     end subroutine cross_prod

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine dot_prod(a,b,c) !GCC$ ATTRIBUTES hot :: dot_prod !GCC$ ATTRIBUTES aligned(64) :: dot_prod !GCC$ ATTRIBUTES inline :: dot_prod
#elif defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: dot_prod
     subroutine dot_prod(a,b,c)
         !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: dot_prod
#endif
         real(kind=sp), dimension(4), intent(in) :: a
         real(kind=sp), dimension(4), intent(in) :: b
         real(kind=sp),               intent(inout) :: c
         ! Exec code ....
         c = a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
     end subroutine dot_prod

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine stokes_matrix(scat_mat,stokes_mat) !GCC$ ATTRIBUTES hot :: stokes_matrix !GCC$ ATTRIBUTES aligned(64) :: stokes_matrix !GCC$ ATTRIBUTES inline :: stokes_matrix
#elif defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: stokes_matrix
     subroutine stokes_matrix(scat_mat,stokes_mat)
       !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: stokes_matrix
#endif
         complex(kind=sp),  dimension(2,2), intent(in) :: scat_mat
         real(kind=sp),     dimension(4,4), intent(inout) :: stokes_mat
         ! lOCALS
         complex(kind=sp), automatic :: CW1121C,CW1122C,CW1112C,CW2122C,&
                                         CW1221C,CW1222C,CW1,CW2
         real(kind=sp),    automatic :: w1,w2,w3,w4
         ! EXec code ....
         w1 = cabs(scat_mat(1,1))
         stokes_mat(1,1) = w1*w1
         w2 = cabs(scat_mat(1,2))
         stokes_mat(1,2) = w2*w2
         w3 = cabs(scat_mat(2,1))
         stokes_mat(2,1) = w3*w3
         w4 = cabs(scat_mat(2,2))
         stokes_mat(2,2) = w4*w4
         CW1121C = scat_mat(1,1)*conjg(scat_mat(2,1))
         CW1222C = scat_mat(1,2)*conjg(scat_mat(2,2))
         CW1112C = scat_mat(1,1)*conjg(scat_mat(1,2))
         CW2122C = scat_mat(2,1)*conjg(scat_mat(2,2))

         CW1122C = scat_mat(1,1)*conjg(scat_mat(2,2))
         CW1221C = scat_mat(1,2)*conjg(scat_mat(2,1))

         CW1 =  CW1122C + CW1221C
         CW2 =  CW1122C - CW1221C

         stokes_mat(1,3) = 2.0_sp*REAL(CW1121C)
         stokes_mat(1,4) = 2.0_sp*AIMAG(CW1121C)

         stokes_mat(2,3) = 2.0_sp*REAL(CW1222C)
         stokes_mat(2,4) = 2.0_sp*AIMAG(CW1222C)

         stokes_mat(3,1) = REAL(CW1112C)
         stokes_mat(3,2) = REAL(CW2122C)
         stokes_mat(3,3) = REAL(CW1)
         stokes_mat(3,4) = AIMAG(CW1)

         stokes_mat(4,1) = -AIMAG(CW1112C)
         stokes_mat(4,2) = -AIMAG(CW2122C)
         stokes_mat(4,3) = -AIMAG(CW2)
         stokes_mat(4,4) = REAL(CW2)
     end subroutine stokes_matrix
       
     

end module mod_leaf_phase_matrices
