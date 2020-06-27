


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
                                            scat2x2m, ,z,delta,lvar,    &
                                            crown_height,trunk_height,dens, &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0) !GCC$ ATTRIBUTES hot :: compute_leaf_phase_matrices !GCC$ ATTRIBUTES aligned(32) :: compute_leaf_phase_matrices
#elif defined __INTEL_COMPILER
     subroutine compute_leaf_phase_matrices(l4x4phm,sm2x2avg,l2x2mp,l2x2mn, &
                                            eig1x4lp,eig1x4ln,eig4x4mp,     &
                                            eig4x4mn,eig4x4mpi,eig4x4mni,   &
                                            expa4x4mp,expa4x4mn,stokes4x4m, &
                                            scat2x2m,z,delta,lvar,lvarv, &
                                            crown_height,trunk_height,dens,  &
                                            ldiam,lthick,lmg,lrho,ldens,theta,&
                                            ctheta,stheta,leaves_epsr, &
                                            leaves_epsrc,rad_freq,rad_wv,rad_k0)
                  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: compute_leaf_phase_matrices
#endif

           real(kind=sp),     dimension(4,4,4),  intent(inout)  :: l4x4phm
           complex(kind=sp),  dimension(2,2,2),  intent(inout)  :: sm2x2avg
           complex(kind=sp),  dimension(2,2),    intent(inout)  :: l2x2mp ! complex 2x2 m matrix positive dir
           complex(kind=sp),  dimension(2,2),    intent(inout)  :: l2x2mn ! complex 2x2 m matrix negative dir
           complex(kind=sp),  dimension(4),      intent(inout)  :: eig1x4lp
           complex(kind=sp),  dimension(4),      intent(inout)  :: eig1x4ln
           complex(kind=sp),  dimension(4,4),    intent(inout)  :: eig4x4mp
           complex(kind=sp),  dimension(4,4),    intent(inout)  :: eig4x4mn
           complex(kind=sp),  dimension(4,4),    intent(inout)  :: eig4x4mpi
           complex(kind=sp),  dimension(4,4),    intent(inout)  :: eig4x4mni
           real(kind=sp),     dimension(4,4),    intent(inout)  :: expa4x4mp
           real(kind=sp),     dimension(4,4),    intent(inout)  :: expa4x4mn
           real(kind=sp),     dimension(4,4),    intent(outout) :: stokes4x4m
           complex(kind=sp),  dimension(2,2),    intent(outout) :: scat2x2m
           real(kind=sp),                        intent(in)  :: z
           real(kind=sp),                        intent(in)  :: delta
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
           complex(kind=sp), automatic :: j,cwork,orient_distr
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
           if((nth1/=0).and.(nph1/=0)) then
              do jj=1, nth1
                 thdr = tr_start1+dt_rad1*real(jj-1,kind=sp)
                 orient_distr = compute_leaf_odf(thdr)
                 do ii=1, nph1
                    phdr = pr_start1+dp_rad1*real(ii-1,kind=sp)
                    thinc = theta
                    thsc  = 3.141592653589793_sp-theta
                    phinc = 3.141592653589793_sp
                    phsc  = 0.0_sp
                    if(po) then
                       
     end subroutine compute_leaf_phase_matrices
     
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine set_leaf_quadrature_bounds(nth1,tr_start1,tr_stop1,dt_rad1, &
                                           nth2,tr_start2,tr_stop2,dt_rad2, &
                                           nth3,tr_start3,tr_stop3,dt_rad3, &
                                           nph1,pr_start1,pr_stop1,dp_rad1, &
                                           nph2,pr_start2,pr_stop2,dp_rad2, &
                                           nph3,pr_start3,pr_stop3,dp_rad3) !GCC$ ATTRIBUTES hot :: set_leaf_quadrature_bounds !GCC$ ATTRIBUTES aligned(16) :: set_leaf_quadrature_bounds
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
       end subroutine leaf_phys_optics_approx
       


end module mod_leaf_phase_matrices
