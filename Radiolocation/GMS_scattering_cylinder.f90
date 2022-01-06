

module cylindrical_scattering


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'cylinderical_scattering'
 !          
 !          Purpose:
 !                        This module is adapted from the Hashim Yousif 'ASMAA' program.
 !                        This is a default version only slightly modified from the
 !                        original program.
 !          History:
 !                        
 !                        Date: 06-01-2022
 !                        Time: 10:21 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Adapted by:
 !                 
 !                   Bernard Gingold
 !         
 !          Original author:
 !               HASHIM YOUSIF
 !               UNIVERSITY OF PITTSBURGH AT BRADFORD
 !               BRADFORD, PA 16701
 !               YOUSIF@PITT.EDU
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp
     use omp_lib
     implicit none
     public

     integer(kind=i4), private :: isize  = 40000
     integer(kind=i4), private :: nsize  = 512     !rounded to multiplicity of 16
     integer(kind=i4), private :: iff    = 1024    !rounded to multiplicity of 16
     integer(kind=i4), private :: igg    = 1024    !rounded to multiplicity of 16
     real(kind=sp),    private :: PI     = 3.1415926535897932384626_sp
     real(kind=sp),    private :: PI180  = 0.0174532925199432957692_sp
     real(kind=sp),    private :: _2PI   = 6.2831853071795864769253_sp
     complex(kind=sp), private :: CCC   = complex(0.0_sp,1.0_sp)

     
      
   contains

     type, public :: CScatterer
           public
           
           complex(kind=sp), dimension(-isize:isize) :: am     ! TM polarization scattering coeffs
           complex(kind=sp), dimension(-isize:isize) :: bm     ! As above
           complex(kind=sp), dimension(-isize:isize) :: cm     ! TE polarization scattering coeffs
           complex(kind=sp), dimension(-isize:isize) :: dm
           real(kind=sp),    dimension(0:nsize)      :: i1    ! Magnitude squared
           real(kind=sp),    dimension(0:nsize)      :: i2    ! As above
           real(kind=sp),    dimension(0:nsize)      :: i3    ! As above
           real(kind=sp),    dimension(0:nsize)      :: i4    ! As above
           real(kind=sp),    dimension(128)          :: s12    ! Scattering matrix row
           real(kind=sp),    dimension(128)          :: s13    ! As above
           real(kind=sp),    dimension(128)          :: s14    ! As above
           real(kind=sp),    dimension(128)          :: s22    ! As above
           real(kind=sp),    dimension(128)          :: s23    ! As above
           real(kind=sp),    dimension(128)          :: s24    ! As above
           real(kind=sp),    dimension(128)          :: s33    ! As above
           real(kind=sp),    dimension(128)          :: s34    ! As above
           real(kind=sp),    dimension(128)          :: s44    ! As above
           real(kind=sp)    :: refmed
           real(kind=sp)    :: wavel0
           real(kind=sp)    :: radius
           real(kind=sp)    :: ref0
           real(kind=sp)    :: refm0
           real(kind=sp)    :: rperb1
           real(kind=sp)    :: dtheta
           real(kind=sp)    :: anglei
           real(kind=sp)    :: anglef
           integer(kind=i4) :: nang !number of angles
           
     end type CScatterer
     
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))     
     subroutine compute_scattering(refmed,wavel0,radius,ref0,  &
                               refm0,rperb1,dtheta,anglei, &
                               anglef,number,am,bm,cm,dm,  &
                               s12,s13,s14,s22,s23,s24,    &
                               s33,s34,s44,i1,i2,i3,i4) !GCC$ ATTRIBUTES aligned(32) :: compute_scattering !GCC$ ATTRIBUTES hot :: compute_scattering
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine compute_scattering(refmed,wavel0,radius,ref0,  &
                               refm0,rperb1,dtheta,anglei, &
                               anglef,number,am,bm,cm,dm,  &
                               s12,s13,s14,s22,s23,s24,    &
                               s33,s34,s44,i1,i2,i3,i4)
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: compute_scattering
#endif
        real(kind=sp),                             intent(in) :: refmed  ! REFMED IS THE REFRACTIVE INDEX OF THE MEDIUM OUSTIDE THE CYLINDER
        real(kind=sp),                             intent(in) :: wavel0  ! WAVEL0 IS THE FREE SPACE WAVELENGTH OF THE INCIDENT RADIATION
                                                                         ! IN METERS OR THE SAME UNITS AS RADIUS (RAD)
        real(kind=sp),                             intent(in) :: radius  ! RAD IS THE RADIUS OF THE CYLINDER . RAD HAS
                                                                         ! THE SAME UNIT AS WAVEL0
        real(kind=sp),                             intent(in) :: ref0    ! REF0 IS THE REAL PART OF THE REFRACTIVE
                                                                         ! INDEX OF THE CYLINDER
        real(kind=sp),                             intent(in) :: refm0   ! REFM0 IS THE IMAGINARY PART OF THE REFRACTIVE
                                                                         ! INDEX OF THE CYLINDER. IT MUST BE A POSITIVE 
                                                                         ! REAL NUMBER
        real(kind=sp),                             intent(in) :: rperb1  ! RPERB1 IS THE RELATIVE PERMEABILITY OF THE
                                                                         ! CYLINDER WITH RESPECT TO THE OUTSIDE MEDIUM.
                                                                         ! PERB1=1 MEANS THAT THE PERMEABILITY OF THE CYLINDER IS THE
                                                                         ! SAME AS THE OUTSIDE MEDIUM.
                                                                         ! THIS PARAMETER MUST BE A POSITIVE REAL NUMBER
        real(kind=sp),                             intent(in) :: dtheta  ! DTHETA IS THE ANGLE BETWEEN THE INCIDENT FIELD
                                                                         ! AND THE AXIS OF THE CYLINDER IN DEGREES
        real(kind=sp),                             intent(in) :: anglei  ! ANGLEI IS THE INITIAL SCATTERING ANGLE IN DEGREES.
                                                                         ! THE FORWARD SCATTERING ANGLE CORRESPONDS TO ANGLEI=0.
        real(kind=sp),                             intent(in) :: anglef  ! ANGLEF IS THE FINAL SCATTERING ANGLE IN DEGREES.
        integer(kind=i4),                          intent(in) :: number  ! NUMBER IS THE NUMBER OF INTERVALS BETWEEN ANGLEI and ANGLEF
        complex(kind=sp), dimension(-isize:isize), intent(out) :: am     ! TM polarization scattering coeffs
        complex(kind=sp), dimension(-isize:isize), intent(out) :: bm     ! As above
        complex(kind=sp), dimension(-isize:isize), intent(out) :: cm     ! TE polarization scattering coeffs
        complex(kind=sp), dimension(-isize:isize), intent(out) :: dm
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ASSUME_ALIGNED am:64
          !DIR$ ASSUME_ALIGNED bm:64
          !DIR$ ASSUME_ALIGNED cm:64
          !DIR$ ASSUME_ALIGNED dm:64
#endif
        real(kind=sp),    dimension(number),      intent(out) :: s12    ! Scattering matrix row
        real(kind=sp),    dimension(number),      intent(out) :: s13    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s14    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s22    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s23    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s24    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s33    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s34    ! As above
        real(kind=sp),    dimension(number),      intent(out) :: s44    ! As above
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ASSUME_ALIGNED s12:64
          !DIR$ ASSUME_ALIGNED s13:64
          !DIR$ ASSUME_ALIGNED s14:64
          !DIR$ ASSUME_ALIGNED s22:64
          !DIR$ ASSUME_ALIGNED s23:64
          !DIR$ ASSUME_ALIGNED s24:64
          !DIR$ ASSUME_ALIGNED s33:64
          !DIR$ ASSUME_ALIGNED s34:64
          !DIR$ ASSUME_ALIGNED s44:64
#endif
        real(kind=sp),  dimension(0:nsize),     intent(out) :: i1    ! Magnitude squared
        real(kind=sp),  dimension(0:nsize),     intent(out) :: i2    ! As above
        real(kind=sp),  dimension(0:nsize),     intent(out) :: i3    ! As above
        real(kind=sp),  dimension(0:nsize),     intent(out) :: i4    ! As above
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED i1:64
        !DIR$ ASSUME_ALIGNED i2:64
        !DIR$ ASSUME_ALIGNED i3:64
        !DIR$ ASSUME_ALIGNED i4:64
#endif
         ! Locals
        complex(kind=sp), dimension(isize)   :: hv
        complex(kind=sp), dimension(isize)   :: hvd
        complex(kind=sp), dimension(isize)   :: bjud
        complex(kind=sp), dimension(isize)   :: bju
        complex(kind=sp), dimension(0:isize) :: g
        complex(kind=sp), dimension(0:nsize) :: ttm0
        complex(kind=sp), dimension(0:nsize) :: tte0
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: hv
          !DIR$ ATTRIBUTES ALIGN : 64 :: hvd
          !DIR$ ATTRIBUTES ALIGN : 64 :: bjud
          !DIR$ ATTRIBUTES ALIGN : 64 :: bju
          !DIR$ ATTRIBUTES ALIGN : 64 :: g
          !DIR$ ATTRIBUTES ALIGN : 64 :: ttm0
          !DIR$ ATTRIBUTES ALIGN : 64 :: tte0
        
#endif
        real(kind=sp), dimension(isize)   :: bjv
        real(kind=sp), dimension(isize)   :: byv
        real(kind=sp), dimension(0:nsize) :: angle
        real(kind=sp), dimension(0:nsize) :: s11
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: bjv
          !DIR$ ATTRIBUTES ALIGN : 64 :: byv
          !DIR$ ATTRIBUTES ALIGN : 64 :: angle
          !DIR$ ATTRIBUTES ALIGN : 64 :: s11
#endif
        complex(kind=sp), automatic :: wan1,awan1,u
        real(kind=sp),    automatic :: ref1,theta,wano,wavel, &
                                       refm1
        real(kind=sp),    automatic :: awano,v,afv,perb,perb1, &
                                       epso,eo,ho,cpara,cperb, &
                                       extm,exte,anorm
        real(kind=sp),    automatic :: t0,t1,t2,t3
        integer(kind=i4), automatic :: nbg,i
        !Exec code ....
          wavel=wavel0/refmed
          ref1=ref0/refmed
          refm1=refm0/refmed
          theta=dtheta*PI180
          wano=_2PI/wavel
          awano=wano*sin(theta)
          v=awano*radius
          wan1=wano*cmplx(ref1,-refm1)
          t0=wan1*wan1
          t1=wan0*cos(theta)
          awan1=sqrt(t0-t1*t1)
          u=awan1*radius
          afy=_2PI*3.0E+8_sp/wavel
          perb=PI*4.0E-7_sp
          perb1=rperb1*perb
          epso=0.0000000000088419412829_sp
          nbg=v+4.0_sp*(v)**0.334_sp+2
          eo=1.0_sp
          ho=1.0_sp
      end subroutine compute_scattering

     
     













end module cylinderical_scattering
