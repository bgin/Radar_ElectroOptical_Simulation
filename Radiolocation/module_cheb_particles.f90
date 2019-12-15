
#include "Config.fpp"

module  mod_cheb_particles

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cheb_particles'
 !          
 !          Purpose:
 !                      This module models 'Chebyshev Particles' aggregated concentration 
 !                       
 !          History:
 !                        Date: 24-07-2018
 !                        Time: 11:17 GMT+2
 !                        Completely modified on: 03-05-2019, 13:49 GMT+2
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                           'Scattering of Radiation by Moderately Non-Spherical Particles' 
 !                            By A. Mugnai and W.J. Wiscombe (1986)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use module_kinds,    only : int1, int4, sp
    use mod_vectypes,    only : YMM8r4_t, Mask8_t,YMM8i4_t
    implicit none
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MAJOR = 2
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MINOR = 0
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MICRO = 0
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_FULLVER = 1000*MOD_CHEB_PARTICLES_MAJOR + &
                                                                     100*MOD_CHEB_PARTICLES_MINOR  + &
                                                                     10*MOD_CHEB_PARTICLES_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_CREATE_DATE = "24-07-2018 11:17 +00200 (TUE 24 JUL 2018 GMT+2)"
    
    ! Module build date  (  should be set after successful compilation)
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_BUILD_DATE = __DATE__ " "  __TIME__
    
    ! Module author info
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_DESCRIPT = "Model of system of  chebyshev particles (hydrometeors)."
    
    ! Constants
    
    real(kind=sp),    parameter, private  :: Deg90Rad = 1.5708_sp
    type(YMM8r4_t),   parameter, private  :: vD90Rad = YMM8r4_t(Deg90Rad)
    type(YMM8r4_t),   parameter, private  :: vScaleToCm = YMM8r4_t(3000.0_sp)
    ! For loop-unrolling
    real(kind=sp), dimension(0:7), parameter, private :: VINC = [1.0_sp,2.0_sp,3.0_sp, &
                                                         4.0_sp,5.0_sp,6.0_sp,7.0_sp,8.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC2 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                         13.0_sp,14.0_sp,15.0_sp,16.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC3 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                          21.0_sp,22.0_sp,23.0_sp,24.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC4 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                          29.0_sp,30.0_sp,31.0_sp]



    
    ! Low spacial and temporal frequency derived data type
    type, public :: ChebParticlesLTS_t
        
          
          public
          ! Dimensioning indices
          ! maximum parametrized points on curve (parametric checbyshev particles formula)
          integer(kind=int4)              :: nxpts
          integer(kind=int4)              :: nypts
          integer(kind=int4)              :: nzpts
          ! max values per particle's shape 
          integer(kind=int4)              :: nshpts
          integer(kind=int4)              :: np
          ! Time evolution steps
          integer(kind=int4)              :: nt
          ! Particles types  (dust-like,grail,snow,hail)
          character(len=32)                :: particles_type
          ! Chebyshev particles shapes  i.e. (r = r0[1+eTn(cos(theta))]
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_shape          
          type(YMM8r4_t), allocatable, dimension(:,:)  :: particles_shape
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)  :: particles_shape !GCC$ ATTRIBUTES aligned(64) :: particles_shape
#endif
          ! Chebyshev particles radii
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_radii          
          real(kind=sp), allocatable, dimension(:)      :: particles_radii
#elif defined __GFORTRAN__
          real(kind=sp), allocatable, dimension(:)      :: particles_radii !GCC$ ATTRIBUTES aligned(64) :: particles_radii
#endif
          ! Chebyshev particles  surface (units of mm^2), (total)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_surface          
          real(kind=sp), allocatable, dimension(:)          :: particles_surf
#elif defined __GFORTRAN__
          real(kind=sp), allocatable, dimension(:)          :: particles_surf !GCC$ ATTRIBUTES aligned(64) :: particles_surf
#endif
          ! Chebyshev particles  parametric equation in x - dimension (non-dimensional)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_x          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramx
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramx !GCC$ ATTRIBUTES aligned(64) :: particles_paramx
#endif
          ! Chebyshev particles   parametric equation in y  - dimension (non-dimensional)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_y          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramy
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramy !GCC$ ATTRIBUTES aligned(64) :: particles_paramy
#endif
          ! Chebyshev particles  parametric equation in z - dimension (non-dimensional)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_z          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramz
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramz !GCC$ ATTRIBUTES aligned(64) :: particles_paramz
#endif
          ! Particles per bin mass (kg) (time-invariant)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_mass          
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_mass
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_mass  !GCC$ ATTRIBUTES aligned(64) :: particles_mass
#endif
          ! Particles (coupled)  temperature (units of Celsius)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: paricles_temp          
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_temp
#elif defined __GFORTRAN__      
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_temp !GCC$ ATTRIBUTES aligned(64) :: particles_temp
#endif
     end type ChebParticlesLTS_t      

          
         

           
          
       

         
          
         

          
         

          
         
          
         

         

         
      
          
      

          
       

          
    
!============================================================================================================!
       ! High temporal and spatial freuqency derived data type.
!============================================================================================================!
       type, public :: ChebParticlesHTS_t
          public

          ! Trajectory of Chebyshev particles (aggregated) range in (spherical coordinate system) per bin ( units of meters (m))
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_range          
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_range
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_range !GCC$ ATTRIBUTES aligned(64) :: particles_range
#endif
          ! Trajectory of Chebyshev particles (aggregated) per bin theta spherical coordinates component (units of radian (rad))
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_theta
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_theta
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_theta !GCC$ ATTRIBUTES aligned(64) :: particles_theta
#endif
          ! Trajectory of Chebyshev particles (aggregated)  per bin phi spehrical coordinates compoment (units of radian (rad))
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_phi          
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_phi
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_phi   !GCC$ ATTRIBUTES aligned(64) :: particles_phi
#endif
          ! Chebyshev particles fall velocity (m/s^-1)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vfall          
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_vfall
#elif defined __GFORTRAN__ 
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_vfall !GCC$ ATTRIBUTES aligned(64) :: particles_vfall
#endif
          ! Chebyshev particles (coupled) per bin fall velocity range component (spherical) (time evolution)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vr          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vr
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vr  !GCC$ ATTRIBUTES aligned(64) :: particles_vr
#endif
          ! Chebyshev particles (coupled) velocity theta component  (spherical)  (time evolution)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vtheta          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vth
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vth  !GCC$ ATTRIBUTES aligned(64) :: particles_vth
#endif
          ! Chebyshev particles (coupled) velocity phi component  (spherical)   (time evolution)
#if defined __INTEL_COMPILER
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vphi          
          type(YMM8r4_t), allocatable, dimension(:,:)     :: particles_vphi
#elif defined __GFORTRAN__
          type(YMM8r4_t), allocatable, dimension(:,:)     :: particles_vphi  !GCC$ ATTRIBUTES aligned(64) :: particles_vphi
#endif          
       end type ChebParticlesHTS_t
    
   contains

#if defined __GFORTRAN__
     subroutine InitChebParticles(PartLTS,PartHTS,nxpts,nypts,nzpts,nt,nparticles, &
                                  nshpts,part_type,errstate,iounit, &
                                  verbose,logging, filename,append )  !GCC$ ATTRIBUTES cold :: InitChebParticles !GCC$ ATTRIBUTES aligned(32) :: InitChebParticles
#elif defined __INTEL_COMPILER
     subroutine InitChebParticles(PartLTS,PartHTS,nxpts,nypts,nzpts,nt,nparticles, &
                                  nshpts,part_type,errstate,iounit, &
                                  verbose,logging, filename,append )
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: InitChebParticles
#endif
           use mod_print_error, only : handle_fatal_memory_error
                               
           type(ChebParticlesLTS_t),    intent(inout) :: PartLTS
           type(ChebParticlesHTS_t),    intent(inout) :: PartHTS
           integer(kind=int4),          intent(inout) :: nxpts
           integer(kind=int4),          intent(inout) :: nypts
           integer(kind=int4),          intent(inout) :: nzpts
           integer(kind=int4),          intent(in)    :: nt
           integer(kind=int4),          intent(inout) :: np
           integer(kind=int4),          intent(in)    :: nshpts
           character(len=32),           intent(in)    :: part_type
           logical(kind=int1),          intent(inout) :: errstate
           integer(kind=int4),          intent(in)    :: iounit
           logical(kind=int4),          intent(in)    :: logging
           logical(kind=int4),          intent(in)    :: verbose
           logical(kind=int4),          intent(in)    :: append
           character(len=*),            intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           integer(kind=int4), automatic :: rem
           ! Exec code .... 
           rem = 0
           rem = MOD(nxpts,8)
           if(rem) then
              nxpts = nxpts-rem
           end if
           rem = MOD(nypts,8)
           if(rem) then
              nypts = nypts-rem
           end if
           rem = MOD(nzpts,8)
           if(rem) then
              nzpts = nzpts-rem
           end if
           rem = MOD(np,8)
           if(rem) then
              np  =  np-rem
           end if
           rem = MOD(nt,8)
           if(rem) then
              nt = nt-rem
           end if
           rem = MOD(nshpts,8)
           if(rem) then
              nshpts = nshpts-rem
           end if
           PartLTS.nxpts = nxpts
           PartLTS.nypts = nypts
           PartLTS.nzpts = nzpts
           PartLTS.nt    = nt
           PartLTS.np    = np
           PartLTS.nshpts = nshpts
           PartLTS.part_type = part_type
           allocate(PartLTS.particles_shape(PartLTS.nshpts,PartLTS.np), &
                    PartLTS.particles_radii(PartLTS.np),                &
                    PartLTS.particles_surf(PartLTS.np),                 &
                    PartLTS.particles_paramx(PartLTS.nxpts,PartLTS.np), &
                    PartLTS.particles_paramy(PartLTS.nypts,PartLTS.np), &
                    PartLTS.particles_paramz(PartLTS.nzpts,PartLTS.np), &
                    PartLTS.particles_mass(PartLTS.np),                 &
                    PartLTS.particles_temp(PartLTS.np), STAT=aerr,ERRMSG=emsg) 
                                                                                
           if(aerr /= 0) goto 9999        
           aerr = 0
           emsg = " "
           allocate(PartHTS.particles_range(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_theta(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_phi(PartLTS.nt,  PartLTS.np),     &
                    PartHTS.particles_vfall(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_vr(PartLTS.nt,   PartLTS.np),     &
                    PartHTS.particles_vth(PartLTS.nt,  PartLTS.np),     &
                    PartHTS.particles_vphi(PartLTS.nt, PartLTS.np),     &
                    STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
           errstate = .false.
           return
9999       call  handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_cheb_particles, subroutine: InitChebParticles -- Memory Allocation Failure !!", &                                                     
                              "module: mod_cheb_particles, subroutine: InitChebParticles -- Memory Allocation Failure !!", &
                                                    emsg, 304) 
          
     end subroutine InitChebParticles

    !===================================================================
    !                   Computational procedures 
    !  These procedures are not type bound in order to facilitate potential
    !  OpenMP parallelization (to be done later).
    !  Only Chebyshev Particles of type T2 and T4 should be used.
    !===================================================================
#if defined __GFORTRAN__
     subroutine ComputeShape(parts,partr,cn,cdef,nshpts,np) !GCC$ ATTRIBUTES hot :: ComputeShape !GCC$ ATTRIBUTES aligned(32) :: ComputeShape
#elif defined __INTEL_COMPILER
     subroutine ComputeShape(parts,partr,cn,cdef,nshpts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeShape
#endif
           use mod_cheb_particles_common, only : ComputeShape_YMM8r4
           type(YMM8r4_t),  contiguous, dimension(:,:), intent(inout)  :: parts
           real(kind=sp),   contiguous, dimension(:),   intent(inout)  :: partr
           real(kind=sp),   contiguous, dimension(:),   intent(in)     :: cn
           real(kind=sp),   contiguous, dimension(:),   intent(in)     :: cdef
           integer(kind=int4),                          intent(in)     :: nshpts
           integer(kind=int4),                          intent(in)     :: np
           ! EXec code ...
           call ComputeShape_YMM8r4(parts,    &
                                    partr,    &
                                    cn,       &
                                    cdef,     &
                                    nshpts,   &
                                    np)
          
     end subroutine ComputeShape    
         
          
           
    

#if defined __GFORTRAN__
     subroutine ComputeXparam(paramx,radii,cn,cdef,nxpts,np) !GCC$ ATTRIBUTES hot :: ComputeXparam !GCC$ ATTRIBUTES aligned(32) :: ComputeXparam
#elif defined __INTEL_COMPILER
     subroutine ComputeXparam(paramx,radii,cn,cdef,nxpts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeXparam
#endif
           use mod_cheb_particles_common, only : ComputeXparam_YMM8r4
           type(YMM8r4_t), contiguous, dimension(:,:), intent(inout) :: paramx
           real(kind=sp),  contiguous, dimension(:),   intent(inout) :: radii
           real(kind=sp),  contiguous, dimension(:),   intent(in)    :: cn
           real(kind=sp),  contiguous, dimension(:),   intent(in)    :: cdef
           integer(kind=int4),                         intent(in)    :: nxpts
           integer(kind=int4),                         intent(in)    :: np
           ! EXec code ....
           call ComputeXparam_YMM8r4(paramx,   &
                                     radii,    &
                                     cn,       &
                                     cdef,     &
                                     nxpts,    &
                                     np)
           
     end subroutine ComputeXparam   
          
         
          
        
         
        
           
               
#if defined __GFORTRAN__    
     subroutine ComputeYparam(paramy,radii,cn,cdef,nypts,np) !GCC$ ATTRIBUTES hot :: ComputeYparam  !GCC$ ATTRIBUTES aligned(32) :: ComputeYparam
#elif defined __INTEL_COMPILER
     subroutine ComputeYparam(paramy,radii,cn,cdef,nypts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeYparam
#endif
           use mod_cheb_particles_common, only : ComputeYparam_YMM8r4
           type(YMM8r4_t), contiguous, dimension(:,:), intent(inout) :: paramy
           real(kind=sp),  contiguous, dimension(:),   intent(inout) :: radii
           real(kind=sp),  contiguous, dimension(:),   intent(in)    :: cn
           real(kind=sp),  contiguous, dimension(:),   intent(in)    :: cdef
           integer(kind=int4),                         intent(in)    :: nypts
           integer(kind=int4),                         intent(in)    :: np
           ! Exec code .....
           call ComputeYparam_YMM8r4(paramy,   &
                                     radii,    &
                                     cn,       &
                                     cdef,     &
                                     nypts,    &
                                     np)
        
     end subroutine ComputeYparam    
           
          
         
         
            
   
#if defined __GFORTRAN__
     subroutine ComputeZparam(paramz,radii,cn,cdef,nzpts,np) !GCC$ ATTRIBUTES hot :: ComputeZparam !GCC$ ATTRIBUTES aligned(32) :: ComputeZparam
#elif defined __INTEL_COMPILER
     subroutine ComputeZparam(paramz,radii,cn,cdef,nzpts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeZparam
#endif
            use mod_cheb_particles_common, only : ComputeZparam_YMM8r4
          
            type(YMM8r4_t),  contiguous, dimension(:,:), intent(inout) :: paramy
            real(kind=sp),   contiguous, dimension(:),   intent(inout) :: radii
            real(kind=sp),   contiguous, dimension(:),   intent(in)    :: cn
            real(kind=sp),   contiguous, dimension(:),   intent(in)    :: cdef
            integer(kind=int4),                          intent(in)    :: nzpts
            integer(kind=int4),                          intent(in)    :: np
            
            ! EXec code .....
            call ComputeZparam_YMM8r4(paramz,    &
                                      radii,     &
                                      cn,        &
                                      cdef,      &
                                      nzpts,     &
                                      np)
     end subroutine ComputeZparam   
            
           
          
               
                   
                
                    
     !==========================================================================80
     !            Computes Chebyshev particles surface area
     !            Scalar version only.
     !==========================================================================80
#if defined __GFORTRAN__
     subroutine ComputeSurface_scalar(surf,radii,ac,acdef,np)   !GCC$ ATTRIBUTES hot :: ComputeSurface_scalar !GCC$ ATTRIBUTES aligned(64) :: ComputeSurface_scalar
#elif defined __INTEL_COMPILER
     subroutine ComputeSurface_scalar(surf,radii,ac,acdef,np)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeSurface_scalar
#endif
          use mod_constants, only : pi2r4_const
          real(kind=sp),  contiguous, dimension(:),  intent(inout) :: surf
          real(kind=sp),  contiguous, dimension(:),  intent(in)    :: radii
          real(kind=sp),  contiguous, dimension(:),  intent(in)    :: ac
          real(kind=sp),  contiguous, dimension(:),  intent(in)    :: acdef
          integer(kind=int4),                        intent(in)    :: np
          
          ! Locals
          integer(kind=int4) :: i,cd
          ! Keeping hot data aligned to L1D Cache line
          type :: CacheLineAligned_t
             real(kind=sp) :: term1,term2,term3,term4, &
                              term5,term5b,term6,term7,&
                              term8,totev,totod,tmp1,  &
                              tmp2,tmp3
             integer(kind=int1), dimension(0:7) :: pad
          end type CacheLineAligned_t
          type(CacheLineAligned_t) :: cla
          ! Exec code ....
          cla.term1  = 0.0_sp
          cla.term2  = 0.0_sp
          cla.term3  = 0.0_sp
          cla.term4  = 0.0_sp
          cla.term5  = 0.0_sp
          cla.term5b = 0.0_sp
          cla.term6  = 0.0_sp
          cla.term7  = 0.0_sp
          cla.term8  = 0.0_sp
          cla.totev  = 0.0_sp
          cla.totod  = 0.0_sp
          cla.tmp1   = 0.0_sp
          cla.tmp2   = 0.0_sp
          cla.tmp3   = 0.0_sp
          surf       = 0.0_sp
          do i=1, np
             cla.term1 = 4.0_sp*pi2r_const*radii(i)**2     
             cd = int(ac(i),kind=int4)
             tmp1 = ac(i)
             tmp2 = acdef(i)
             tmp3 = tmp1**2-1.0_sp
             if(iand(cd,1) == 0) then     
                cla.term2 = 1.0_sp-2.0_sp*tmp2/(tmp1**2-1.0_sp)
                cla.term3 = tmp2**2*(tmp1**4+2.0_sp*tmp1**2-1.0_sp) / &
                            (4.0_sp*tmp1**2-1.0_sp)
                cla.term4 = 3.0_sp*tmp2**4*tmp1**8 / &
                            (64.0_sp*tmp1**4-12.0_sp*tmp1**2+1.0_sp)
                
                cla.term5 = -6.0_sp*tmp2**5*tmp1**8
                cla.term5b = 1.0_sp/tmp3*9.0_sp*tmp3*25.0*tmp3
                cla.totev  = cla.term1*(cla.term2+cla.term3-cla.term4-cla.term5*cla.term5b)
                surf(i) = cla.totev
             else              
                cla.term2 = 1.0_sp+tmp2**2*(tmp1**4+2.0_sp*tmp3)/(4.0_sp*tmp3)         
                cla.term3 = 3.0_sp*tmp2**4*tmp1**4*0.015625_sp
                cla.term4 = 1.0_sp+20.0_sp*tmp3/(16.0_sp*tmp3*4.0_sp*tmp3)
                cla.totod = cla.term1*(cla.term2-cla.term3*cla.term4)
                surf(i) = cla.totod
             end if
          end do
     end subroutine ComputeSurface_scalar    
                  
                                        
            
             
              
              
               
               
               
               
          
    
          
  


     
    
    
  
  
 
    

 
 

  
    
   
          
          
          
          
          
          
          
          
          
          
          
          


    
  

                  
  
                           
                       
      
  
    
    
   
 
 
   
    
    
    
    
    
    
end module mod_cheb_particles
