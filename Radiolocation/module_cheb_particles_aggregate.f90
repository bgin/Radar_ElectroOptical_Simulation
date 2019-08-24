



module mod_cheb_particles_aggregate

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cheb_particles_aggregate'
 !          
 !          Purpose:
 !                      This module models 'Chebyshev Particles' aggregated ensemble
 !                       
 !          History:
 !                        Date: 04-08-2018
 !                        Time: 11:51 GMT+2
 !                        Modified on: 17-08-2019 15:08 PM GMT+2
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
    use mod_vectypes,    only : YMM8r4_t
    implicit none
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MAJOR = 2
    
    ! Minor version
    integer(kind=int4),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MINOR = 0
    
    ! Micro version
    integer(kind=int4),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MICRO = 0
    
    ! Module full version
    integer(kind=int4),  parameter, public :: MOD_CHEB_PARTICLES_AGGREAGTE_FULLVER = 1000*MOD_CHEB_PARTICLES_AGGREGATE_MAJOR + &
                                                                                100*MOD_CHEB_PARTICLES_AGGREGATE_MINOR  + &
                                                                                10*MOD_CHEB_PARTICLES_AGGREGATE_MICRO
    ! Module creation date
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_CREATE_DATE = "04-08-2018 11:41 +00200 (SAT 04 AUG 2018 GMT+2) "
    
    ! Module build date (  should be set after successful compilation)
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_DESCRIPT = "Model of aggregated chebyshev particles (hydrometeors)."
    
    ! Constants
    
    real(kind=dp),    parameter, private :: Deg90Rad = 1.5708_sp
    type(YMM8r4_t),   parameter, private :: vD90Rad    = YMM8r4_t(Deg90Rad)
    type(YMM8r4_t),   parameter, private  :: vScaleToCm = YMM8r4_t(3000.0_sp)
    ! For loop-unrolling
    real(kind=sp), dimension(0:7), parameter, private :: VINC0 = [1.0_sp,2.0_sp,3.0_sp, &
                                                         4.0_sp,5.0_sp,6.0_sp,7.0_sp,8.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC2 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                         13.0_sp,14.0_sp,15.0_sp,16.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC3 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                          21.0_sp,22.0_sp,23.0_sp,24.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC4 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                          29.0_sp,30.0_sp,31.0_sp]
    
     ! Low spacial and temporal frequency derived data type
    type, public :: PartAggregateLTS_t
        
          public
          ! Number of particles  in aggregated emsemble
          integer(kind=int4)              :: m_np 
          ! Particles aggregate ID number
          integer(kind=int4)              :: m_ID
            ! Time evolution steps
          integer(kind=int4)              :: m_nt
            ! Maximal number of parametric equation points
          integer(kind=int4)              :: m_nxpts, m_nypts, m_nzpts
           ! Total volume of particles per ensemble
          real(kind=sp)                   :: m_tpv
           ! Total particles surface area per ensemble
          real(kind=sp)                   :: m_tpsa
           ! Total particles mass per ensemble
          real(kind=sp)                   :: m_tpm
           ! Hydrometeor type
          character(len=32)               :: m_htype
           ! Ensemble shape  only 2 types supported for now: -- ( spheroid, chebyshev-particle)
          character(len=64)              :: m_esh
           ! Chebyshev particles shape in aggregated assembly ( (r = r0[1+eTn(cos(theta))])
          ! [r,np], where r = parametric shape (cross-section), np =  n-th particle
          type(YMM8r4_t), allocatable, dimension(:,:)   :: m_pcs
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pcs
           ! Chebyshev particles radii in aggregate ensemble
          real(kind=sp), allocatable, dimension(:)     :: m_pradii
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pradii
           ! Chebyshev particles aggregate shape approximated by 3D parametric equations.
          ! Components form location of the particle in the ensemble.
          ! [3,np], where first dimension represents coordinate components
          ! second dimension represent number of particles.
          real(kind=sp), allocatable, dimension(:,:)    :: m_pes
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pes
           ! Chebyshev particles ensemble( per each particle) parametric equation in x - dimension (non-dimensional)
          ! [paramx,np]
          type(YMM8r4_t), allocatable, dimension(:,:)    :: m_ppx
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ppx
           ! Chebyshev particles ensemble (per each particle)  parametric equation in y  - dimension (non-dimensional)
          type(YMM8r4_t), allocatable, dimension(:,:)    :: m_ppy
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ppy
           ! Chebyshev particles ensemble (per each particle)  parametric equation in z  - dimension (non-dimensional)
          type(YMM8r4_t), allocatable, dimension(:,:)    :: m_ppz
!DIR$    ATTRIBUTES ALIGN : 64 :: m_ppz
         
     end type  PartAggregateLTS_t     
         
     ! High temporal and spatial freuqency derived data type.     
     type, public :: PartAggregateHTS_t
           public
           ! Trajectory of Chebyshev particles ensemble, radial distance component (spherical coordinate system)
           ! [nt]
           real(kind=sp), allocatable, dimension(:)    :: m_prdist
!DIR$     ATTRIBUTES ALIGN : 64 :: m_prdist
           ! Trajectory of Chebyshev particles ensemble, theta angle component (spherical coordinate system)
           ! [nt]
           real(kind=sp), allocatable, dimension(:)    :: m_ptheta
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ptheta
          ! Trajectory of Chebyshev particles ensemble, phi angle component (spherical coordinate system)
          ! [nt]
          real(kind=sp), allocatable, dimension(:)     :: m_pphi
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pphi
          ! Chebyshev particles ensemble fall speed 
          ![nt]
          real(kind=sp), allocatable, dimension(:)       :: m_pfv
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pvf
     end type PartAggregateHTS_t
          
          
     contains      
         
     subroutine InitPartAggregate(PartLTS,PartHTS,np,id,nt,htype,esh,nxpts,nypts,  &     
                                  nzpts,msval,err,iounit,verbose,logging,filename,append)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitPartAggregate
           use mod_print_error, only : handle_fatal_memory_error
           type(PartAggregateLTS_t),          intent(in)       :: PartLTS
           type(PartAggregateHTS_t),          intent(in)       :: PartHTS
           integer(kind=int4),                intent(inout)    :: np
           integer(kind=int4),                intent(in)       :: id
           integer(kind=int4),                intent(inout)    :: nt
           character(len=32),                 intent(in)       :: htype
           character(len=64),                 intent(in)       :: esh
           integer(kind=int4),                intent(inout)    :: nxpts
           integer(kind=int4),                intent(inout)    :: nypts
           integer(kind=int4),                intent(inout)    :: nzpts
           integer(kind=int4),                intent(inout)    :: msval
           logical(kind=int1),                intent(in)       :: err
           integer(kind=int4),                intent(in)       :: iounit
           logical(kind=int4),                intent(in)       :: verbose
           logical(kind=int4),                intent(in)       :: logging
           character(len=*),                  intent(in)       :: filename
           logical(kind=int4),                intent(in)       :: append
           ! LOcals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           integer(kind=int4), automatic :: rem
           ! EXec code ....
           rem = 0
           rem = MOD(np,8)
           if(rem) np = np-rem
           rem = MOD(nt,8)
           if(rem) nt = nt-rem
           rem = MOD(nxpts,8)
           if(rem) nxpts = nxpts-rem
           rem = MOD(nypts,8)
           if(rem) nypts = nypts-rem
           rem = MOD(nzpts,8)
           if(rem) nzpts = nzpts-rem
           rem = MOD(msval,8)
           if(rem) msval = msval-rem
           rem = 0
           PartLTS.m_np     = np
           PartLTS.m_ID     = id
           PartLTS.m_nt     = nt
           PartLTS.m_nxpts  = nxpts
           PartLTS.m_nypts  = nypts
           PartLTS.m_nzpts  = nzpts
           PartLTS.m_tpv   = -1.0_sp
           PartLTS.m_tpsa  = -1.0_sp
           PartLTS.m_tpm   = -1.0_sp
           PartLTS.m_htype = htype
           PartLTS.m_esh   = esh
           allocate(PartLTS.m_pcs(msval,PartLTS.m_np),            &
                    PartLTS.m_pradii(PartLTS.m_np),               &
                    PartLTS.m_pes(PartLTS.m_np,3),                &
                    PartLTS.m_ppx(PartLTS.m_nxpts,PartLTS.m_np),  &
                    PartLTS.m_ppy(PartLTS.m_nypts,PartLTS.m_np),  &
                    PartLTS.m_ppz(PartLTS.m_nzpts,PartLTS.m_np),  &
                    STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
           aerr = 0
           emsg = " "
           allocate(PartHTS.m_prdist(PartHTS.m_nt),               &
                    PartHTS.m_ptheta(PartHTS.m_nt),               &
                    PartHTS.m_pphi(PartHTS.m_nt),                 &
                    PartHTS.m_pfv(PartHTS.m_nt),                  &
                    STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
           err = .false.
           return
9999       call  handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_cheb_particles_aggregate, subroutine: InitPartAggregate -- Memory Allocation Failure !!", &                                                                          "module: mod_cheb_particles, subroutine: InitChebParticles -- Memory Allocation Failure !!", emsg, 304)
     end subroutine InitPartAggregate                                                 
                             
    !===============================================================================
    ! Compute particles shape as a cross-section
    ! This code relies on compuation of radial distance as function of un-perturbed
    ! sphere radius, deformity coefficient and T(n) curve.
    !===============================================================================
     subroutine ComputeShape(PartLTS,msval,np,cn,cdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeShape
           use mod_cheb_particles_common, only : ComputeShape_YMM8r4
           type(PartAggregateLTS_t),          intent(inout) :: PartLTS
           integer(kind=int4),                intent(in)    :: msval
           integer(kind=int4),                intent(in)    :: np
           real(kind=sp),   dimension(np),    intent(inout) :: cn
           real(kind=sp),   dimension(np),    intent(inout) :: cdef
           ! EXec code ...
           call ComputeShape_YMM8r4(PartLTS.m_pcs,           &
                                    PartLTS.m_pradii,        &
                                    cn,                      &
                                    cdef,                    &
                                    msval)
     end subroutine ComputeShape
        
          
          
          
         
          
         
          
         
          
         
          
        
          
         
          
       
       
          
          
   
    
    
    
  
          
        
    
 
    

    

    

    

    
   
    
  
    
  


   
    
  
       
    
    !=================================================================71
    !     Computation of ensemble shape.
    !     As for now only 3 shapes are supported:
    !     1) Cylindrical
    !     2) Pure spherical
    !     3) Chebyshev particle like
    !=================================================================71
    subroutine ComputeEnsemblShape( pes,np,inz,incz,shape_name, &
                                       r,inphi,inth,incphi,incth,sphrad, &
                                       chebn,cdeform,verbose,ierr)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeEnsembleShape
          
          real(kind=sp),       dimension(np,3),     intent(inout)         :: pes
          integer(kind=int4),                       intent(in)            :: np
          real(kind=sp),                            intent(in), optional  :: inz
          real(kind=sp),                            intent(in), optional  :: incz
          character(len=64),                        intent(in)            :: shape_name
          real(kind=sp),                            intent(in), optional  :: r
          real(kind=sp),                            intent(in), optional  :: inphi
          real(kind=sp),                            intent(in), optional  :: inth
          real(kind=sp),                            intent(in), optional  :: incphi
          real(kind=sp),                            intent(in), optional  :: incth
          real(kind=sp),                            intent(in), optional  :: sphrad
          real(kind=sp),                            intent(in), optional  :: chebn
          real(kind=sp),                            intent(in), optional  :: cdeform
          integer(kind=int4),                       intent(inout)         :: ierr
         
          ! Locals
          integer(kind=int4)     :: i
          real(kind=sp)          :: term1,phi,theta,x,y,z,u
          ! Exec code ....
          term1 = 0.0_sp
          phi   = 0.0_sp
          theta = 0.0_sp
          x     = 0.0_sp
          y     = 0.0_sp
          z     = 0.0_sp
          u     = 0.0_sp
          ! Begin computation
          select case (trim(shape_name))
          case("Cylindrical")
              if(  present(inz)  .AND.  &
                   present(incz) .AND.  &
                   present(r)    .AND.  &
                   present(inth) .AND.  &
                   present(incth)       ) then
                 
                    ! Parametric equation 
                    ! Begin computation
                   z = inz
                   theta = inth
                   do i = 1,  np
                        theta = theta + incth
                        z = z + incz
                        pes(i,1) = r * cos(theta)
                        pes(i,2) = r * sin(theta)
                        pes(i,3) = z
                    end do
              else
                  
                   ierr = -1
                   return
              end if   
          case("Spheroidal")
              if(present(r)      .AND. &
                 present(inth)   .AND. &
                 present(incth)  .AND. &
                 present(inphi)  .AND. &
                 present(incphi)        )  then
                    ! Parametric equation 
                    ! Begin computation
                    theta = inth
                    phi   = inphi
 
                    do i = 1,  np
                        theta = theta + incth
                        phi   = phi   + incphi
                        u = r * cos(phi)
                        pes(i,1) = sqrt(r**2 - u**2) * cos(theta)
                        pes(i,2) = sqrt(r**2 - u**2) * sin(theta)
                        pes(i,3) = u
                    end do
       
              else
                 
                  ierr = -1
                  return
              end if         
          case("ChebyshevParticle")
               if(present(inth)   .AND.  &
                  present(incth)  .AND.  &
                  present(inphi)  .AND.  &
                  present(incphi) .AND.  &
                  present(sphrad) .AND.  &
                  present(chebn)  .AND.  &
                  present(cdeform)              ) then
                    ! Parametric equation 
                    ! Begin computation
                    theta = inth
                    phi   = inphi
                     
 
                    do i = 1,   np
                        theta = theta + incth
                        phi   = phi   + incphi
                        term1 = sphrad * (1.0_R64P + cdeform * cos(chebn * theta)
                        x = term1 * sin(theta) * cos(phi)
                        pes(i,1) = x
                        y = term1 * sin(theta) * sin(phi)
                        pes(i,2) = y
                        z = term1 * cos(theta)
                        pes(i,3) = z
                    end do
          
            
                  ierr = -1
                  return
              end if        
           case default
               
                ierr = -2
                return
          end select
          ierr = 0    
      end subroutine  ComputeEnsembleShape  
       
         
   
    
    !=================================================================71
    !   
    !     Chebyshev Particles parametric equation in parameter 'x'
    !     x = r0[1+- eTn(cos(theta))]sin(theta) cos(phi)
    !
    !=================================================================71
    subroutine ComputeXparam(PartLTS,np,cn,cdef)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeXparam
           use mod_cheb_particles_common, only : ComputeXparam_YMM8r4
           type(PartAggregateLTS_t),          intent(inout) :: PartLTS
           integer(kind=int4),                intent(in)    :: np
           real(kind=sp),  dimension(np),     intent(inout) :: cn
           real(kind=sp),  dimension(np),     intent(inout) :: cdef
           ! Exec code ....
           call ComputeXparam_YMM8r4(PartLTS.m_ppx,  &
                                     PartLTS.m_pradii, &
                                     cn,               &
                                     cdef,             &
                                     PartLTS.m_nxpts,  &
                                     np)
    end subroutine ComputeXparam
 
     
    !==========================================================================80
    !     Chebyshev Particles parametric equation in parameter 'y'
    !     y = r0[1+- eTn(cos(theta))]sin(theta) sin(phi)
    !==========================================================================80
    subroutine ComputeYparam(PartLTS,np,cn,cdef)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeYparam
           use mod_cheb_particles_common, only : ComputeYparam_YMM8r4
           type(PartAggregateLTS_t),          intent(inout) :: PartLTS
           integer(kind=int4),                intent(in)    :: np
           real(kind=sp),  dimension(np),     intent(inout) :: cn
           real(kind=sp),  dimension(np),     intent(inout) :: cdef
           ! Exec code ....
           call ComputeYparam_YMM8r4(PartLTS.m_ppy,   &
                                     PartLTS.m_pradii, &
                                     cn,               &
                                     cdef,             &
                                     PartLTS.nypts,    &
                                     np)
    end subroutine ComputeYparam
    
  
     
    
    !==========================================================================80
    !     Chebyshev Particles parametric equation in parameter 'z'
    !     z = r0[1+- eTn(cos(theta))]sin(theta)
    !==========================================================================80
    subroutine ComputeZparam(PartLTS,np,cn,cdef)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeZparam
           use mod_cheb_particles_common, only : ComputeZparam_YMM8r4
           type(PartAggregateLTS_t),          intent(inout) :: PartLTS
           integer(kind=int4),                intent(in)    :: np
           real(kind=sp),  dimension(np),     intent(inout) :: cn
           real(kind=sp),  dimension(np),     intent(inout) :: cdef
           ! Exec code ....
           call ComputeYparam_YMM8r4(PartLTS.m_ppz,   &
                                     PartLTS.m_pradii, &
                                     cn,               &
                                     cdef,             &
                                     PartLTS.nzpts,    &
                                     np)
    end subroutine ComputeZparam

         
   
      
    
    !==========================================================================80
    !           Computing Chebyshev ensemble particles volume.
    !           No argument verification is performed on array arguments like:
    !           sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine  ComputeEnsembleVol(tpv,np,sphrad,chebn,cdeform)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeEnsembleVol
          use mod_constants,  only : pi2_const
          real(kind=sp),                     intent(out)   ::  tpv
          integer(kind=int4),                  intent(in)    ::  np
          real(kind=sp),     dimension(np),  intent(in)    ::  sphrad
          real(kind=sp),     dimension(np),  intent(in)    ::  chebn
          real(kind=sp),     dimension(np),  intent(in)    ::  cdeform
       
          ! Locals
          integer(kind=int4) :: i
          real(kind=sp)    :: term1,term1a,term2,term3,term4

          ! Exec code ....
        
         
          term1 = 0.0_sp
          term1a = 0.0_sp
          term2 = 0.0_sp
          term3 = 0.0_sp
          term4 = 0.0_sp

          do i = 1,   np
                        term1 =   0.333333333333_sp*4.0_sp*pi2_const*sphrad(i)**3
                        term1a =  1.0_sp + 1.5_sp * cdeform(i)**2 * &
                                  (4.0_sp*chebn(i)**2-2.0_sp/4.0_sp*chebn(i)**2-1.0_sp)
                        if(iand(int(chebn(i),kind=4),1) == 0) then
                                term2 = 3.0_sp * cdeform(i) * (1.0_sp + cdeform(i)**2*0.25_sp) / &
                                        (chebn(i)**2-1.0_sp)
                                term3 = 0.25_sp*cdeform(i)**3 / &
                                        (9.0_sp*chebn(i)**2-1.0_sp)
                                term4 = term1 * (term1a - term2 - term3)
                                tpv = tpv + term4
                        else
                                term2 = term1 * term1a
                                tpv = tpv + term2
                        end if
          end do
                    
    end subroutine ComputeEnsembleVol
    
    !==========================================================================80
    !            Computes Chebyshev particles surface area (total per ensemble)  
    !            No argument verification is performed on array arguments like:
    !            sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine ComputeEnsembleSurf(tps,np,sphrad,chebn,cdeform)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeEnsembleVol
          use mod_constants, only : pi2_const
          real(kind=sp),                      intent(out)   :: tps
          integer(kind=int4),                   intent(in)    :: np
          real(kind=sp),       dimension(np), intent(in)    :: sphrad
          real(kind=sp),       dimension(np), intent(in)    :: chebn
          real(kind=sp),       dimension(np), intent(in)    :: cdeform
          
          ! Locals
          integer(kind=int4) :: i
          real(kind=sp)    :: term1,term2,term3,term4,term5,term5a,tmp

          ! Exec code ...
         
          term1  = 0.0_sp
          term2  = 0.0_sp
          term3  = 0.0_sp
          term4  = 0.0_sp
          term5  = 0.0_sp
          term5a = 0.0_sp
          tmp    = 0.0_sp

          do i = 1,    np
              term1 = 4.0_sp * pi2_const * sphrad(i)**2
              
              if(iand(int(chebn(i),kind=4),1) == 0) then
                    term2 = 1.0_sp - 2.0_sp * cdeform(i)/(chebn(i)**2-1.0_sp)
                    term3 = cdeform(i)**2*(chebn(i)**4+2.0_sp*chebn(i)**2-1.0_sp) / &
                            (4.0_sp*chebn(i)**2-1.0_sp)
                    term4 =  3.0_sp*cdeform(k)**4*chebn(i)**8 / &
                             (64.0_sp*chebn(i)**4-12.0_sp*chebn(i)**2+1.0_sp)
                    term5 =  -6.0_sp*cdeform(k)**5*chebn(i)**8
                    term5a =             1.0_sp /        &
                             ((chebn(i)**2-1.0_sp)*(9.0_sp*chebn(i)**2-1.0_sp)*(25.0_sp*chebn(i)**2-1.0_sp))
                    tmp = term1 * (term2 + term3 - term4 - term5 * term5a)
                    tps = tps + tmp
              else
                    term2 = 1.0_sp + cdeform(k)**2*(chebn(i)**4+2.0_sp*chebn(i)**2-1.0_sp) / &
                                    (4.0_sp*chebn(i)**2-1.0_sp)
                    term3 = 3.0_sp*cdeform(k)**4*chebn(i)**4*0.015625_sp
                    term4 = 1.0_sp + 20.0_sp*chebn(i)**2-1.0_sp / &
                                    ((16.0_sp*chebn(i)**2-1.0_sp)*(4.0_sp*chebn(i)**2-1.0_sp))
                    tmp = term1 * (term2 - term3 * term4)
                    tps = tps + tmp
              end if
              
          end do
        
          
    end subroutine
    
    !============================================================================
    !      Vertical falling speed computation based on
    !     "Fall Velocities of Hydrometeors in the Atmosphere: 
    !      Refinements to Continous Analytical Power Law    "
    !      by Vitaliy I. Khvorostyanov and Judith A. Curry
    !      
    !      Adding as an option turbulence correction coefficients.
    !============================================================================
    subroutine ComputeVfall(pfv,nt,aRe,bRe,vb,kvisc,nx,ny,nz,A,rho_b,rho_f,mD, &
                             Re,bRet,aRet                        )
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeVfall
          real(kind=sp),       dimension(nt),       intent(out)    :: pfv
          integer(kind=int4),                         intent(in)     :: nt
          real(kind=sp),       dimension(nt),       intent(in)     :: aRe
          real(kind=sp),       dimension(nt),       intent(in)     :: bRe
          real(kind=sp),                            intent(in)     :: vb
          real(kind=sp),       dimension(nx,ny,nz), intent(in)     :: kvisc
          integer(kind=int4),                         intent(in)     :: nx
          integer(kind=int4),                         intent(in)     :: ny
          integer(kind=int4),                         intent(in)     :: nz
          real(kind=sp),                            intent(in)     :: A
          real(kind=sp),                            intent(in)     :: rho_b
          real(kind=sp),      dimension(nx,ny,nz),  intent(in)     :: rho_f
          real(kind=sp),                            intent(in)     :: mD
          real(kind=sp),                            intent(in)     :: Re
          real(kind=sp),      dimension(nt),        intent(in)     :: bRet  ! Turbulence correction term
          real(kind=sp),      dimension(nt),        intent(in)     :: aRet  ! Turbulence correction term
        
          ! Locals
          integer(kind=int4) :: i,ix,iy,iz
          real(kind=sp)    :: term1,term2,term2a,term3,inva,t1,t2
         
          term1  = 0.0_sp
          term2  = 0.0_sp
          term2a = 0.0_sp
          term3  = 0.0_sp
          t1     = 0.0_sp
          t2     = 0.0_sp
          term2 = (2.0_sp*vb*9.81_sp) / A
          if((abs(Re) - 999.0_sp) <= EPSILON(0.0_sp) ) then
             ! Obtain ieee floating-point register state 
         
              do i = 1,  nt
                 t1 = aRet(i)
                 t2 = bRet(i)
                  do iz = 1,  nz
                      do iy = 1,  ny
                          do ix = 1,  nx
                              term1 = t1 * kvisc(ix,iy,iz)**(1.0_sp-2.0_sp*t2)
                             
                              if( rho_b > rho_f(ix,iy,iz) ) then
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) )
                              else
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) - 1.0_sp )
                              end if
                              term3 = mD**2.0_sp*t2 - 1.0_sp
                             
                          end do
                      end do
                  end do
                        pfv(i) = term1 * (term2 * term2a)**t2 * term3
              end do
  
          else
                   ! Obtain ieee floating-point register state 
              
              do i = 1,  nt
                 t1 = aRe(i)
                 t2 = bRe(i)
                  do iz = 1,  nz
                      do iy = 1,  ny
                          do ix = 1,  nx
                              term1 = t1 * kvisc(ix,iy,iz)**(1.0_sp-2.0_sp*t2)
                              
                              if( rho_b > rho_f(ix,iy,iz) ) then
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) )
                              else
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) - 1.0_sp )
                              end if
                              term3 = mD**2.0_sp*t2 - 1.0_sp
                             
                          end do
                      end do
                  end do
                       pfv(i) = term1 * (term2 * term2a)**t2 * term3
              end do
            
 
          end if
    
    end subroutine
    
    
    
end module mod_cheb_particles_aggregate
