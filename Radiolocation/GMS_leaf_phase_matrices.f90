


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

   contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine compute_leaf_phase_matrices(l4x4phm,sm2x2avg,l2x2mp,l2x2mn, &
                                            eig1x4lp,eig1x4ln,eig4x4mp,     &
                                            eig4x4mn,eig4x4mpi,eig4x4mni,   &
                                            expa4x4mp,expa4x4mn,stokes4x4m, &
                                            scat2x2m,thinc,phinc,thsc,phsc, &
                                            thdr,phdr,z,delta,norm,lvar,    &
                                            crown_height,trunk_height,dens, &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0) !GCC$ ATTRIBUTES hot :: compute_leaf_phase_matrices !GCC$ ATTRIBUTES aligned(32) :: compute_leaf_phase_matrices
#elif defined __INTEL_COMPILER
     subroutine compute_leaf_phase_matrices(l4x4phm,sm2x2avg,l2x2mp,l2x2mn, &
                                            eig1x4lp,eig1x4ln,eig4x4mp,     &
                                            eig4x4mn,eig4x4mpi,eig4x4mni,   &
                                            expa4x4mp,expa4x4mn,stokes4x4m, &
                                            scat2x2m,thinc,phinc,thsc,phsc, &
                                            thdr,phdr,z,delta,norm,lvar,lvarv, &
                                            crown_height,trunk_height,dens,  &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0)
                  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: compute_leaf_phase_matrices
#endif

           real(kind=sp),     dimension(4,4,4),  intent(out)  :: l4x4phm
           complex(kind=sp),  dimension(2,2,2),  intent(in)  :: sm2x2avg
           complex(kind=sp),  dimension(2,2),    intent(out)  :: l2x2mp ! complex 2x2 m matrix positive dir
           complex(kind=sp),  dimension(2,2),    intent(out)  :: l2x2mn ! complex 2x2 m matrix negative dir
           complex(kind=sp),  dimension(4),      intent(in)  :: eig1x4lp
           complex(kind=sp),  dimension(4),      intent(in)  :: eig1x4ln
           complex(kind=sp),  dimension(4,4),    intent(in)  :: eig4x4mp
           complex(kind=sp),  dimension(4,4),    intent(in)  :: eig4x4mn
           complex(kind=sp),  dimension(4,4),    intent(in)  :: eig4x4mpi
           complex(kind=sp),  dimension(4,4),    intent(in)  :: eig4x4mni
           real(kind=sp),     dimension(4,4),    intent(in)  :: expa4x4mp
           real(kind=sp),     dimension(4,4),    intent(in)  :: expa4x4mn
           real(kind=sp),     dimension(4,4),    intent(out) :: stokes4x4m
           complex(kind=sp),  dimension(2,2),    intent(out) :: scat2x2m
           real(kind=sp),                        intent(in)  :: thinc
           real(kind=sp),                        intent(in)  :: phinc
           real(kind=sp),                        intent(in)  :: thsc
           real(kind=sp),                        intent(in)  :: phsc
           real(kind=sp),                        intent(in)  :: thdr
           real(kind=sp),                        intent(in)  :: phdr
           real(kind=sp),                        intent(in)  :: z
           real(kind=sp),                        intent(in)  :: delta
           real(kind=sp),                        intent(in)  :: norm
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
           complex(kind=sp), dimension(12),      intent(in)  :: leaves_epsr  ! leaves disalectric constant
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
           complex(kind=sp), automatic :: j,cwork
           real(kind=sp),    automatic :: tr_start1,tr_stop1,dt_rad1
           real(kind=sp),    automatic :: tr_start2,tr_stop2,dt_rad2
           real(kind=sp),    automatic :: tr_start3,tr_stop3,dt_rad3
           real(kind=sp),    automatic :: pr_start1,pr_stop1,dp_rad1
           real(kind=sp),    automatic :: pr_start2,pr_stop2,dp_rad2
           real(kind=sp),    automatic :: pr_start3,pr_stop3,dp_rad3
           real(kind=sp),    automatic :: dp1t1,dp2t2,dp3t3
           integer(kind=int4), automatic :: nth1,nth2,nth3
           integer(kind=int4), automatic :: nph1,nph2,nph3
           integer(kind=int4), automatic :: ii,j,l,k,jj
           logical(kind=int4), automatic :: po
           
     end subroutine compute_leaf_phase_matrices
     

















end module mod_leaf_phase_matrices
