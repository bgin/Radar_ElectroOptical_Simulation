

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
     real(kind=sp),    private :: PI2    = 1,5707963267948966192313_sp
     real(kind=sp),    private :: PI180  = 0.0174532925199432957692_sp
     real(kind=sp),    private :: _2PI   = 6.2831853071795864769253_sp
     complex(kind=sp), private :: CCC   = complex(0.0_sp,1.0_sp)

     
      
   

     type, public :: cyl_scatterer_soa
           public
           
           complex(kind=sp), dimension(-isize:isize)   :: am     ! TM polarization scattering coeffs
           complex(kind=sp), dimension(-isize:isize)   :: bm     ! As above
           complex(kind=sp), dimension(-isize:isize)   :: cm     ! TE polarization scattering coeffs
           complex(kind=sp), dimension(-isize:isize)   :: dm
           real(kind=sp),    dimension(0:nsize)        :: i1    ! Magnitude squared
           real(kind=sp),    dimension(0:nsize)        :: i2    ! As above
           real(kind=sp),    dimension(0:nsize)        :: i3    ! As above
           real(kind=sp),    dimension(0:nsize)        :: i4    ! As above
           real(kind=sp),    dimension(0:nsize)        :: s11
           real(kind=sp),    dimension(0:127)          :: s12    ! Scattering matrix row
           real(kind=sp),    dimension(0:127)          :: s13    ! As above
           real(kind=sp),    dimension(0:127)          :: s14    ! As above
           real(kind=sp),    dimension(0:127)          :: s22    ! As above
           real(kind=sp),    dimension(0:127)          :: s23    ! As above
           real(kind=sp),    dimension(0:127)          :: s24    ! As above
           real(kind=sp),    dimension(0:127)          :: s33    ! As above
           real(kind=sp),    dimension(0:127)          :: s34    ! As above
           real(kind=sp),    dimension(0:127)          :: s44    ! As above
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
           
     end type cyl_scatterer_soa


     type, public :: cyl_scatterers_grid_asoa
           public
           integer(kind=i4) :: n_scatteres
#if defined(__INTEL_COMPILER) || defined(__ICC)           
           type(cyl_scatterer_soa), allocatable, dimension(:) :: scatterers_grid
           !DIR$ ATTRIBUTES ALIGN : 64 :: scatterers_grid
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(cyl_scatterer_soa), allocatable, dimension(:) :: scatterers_grid !GCC$ ATTRIBUTES aligned(64) :: scatterers_grid
#endif
     end type cyl_scatterers_grid_asoa


      contains

        
     
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))     
     subroutine compute_scattering(refmed,wavel0,radius,ref0,  &
                               refm0,rperb1,dtheta,anglei, &
                               anglef,number,am,bm,cm,dm,  &
                               s11,s12,s13,s14,s22,s23,s24,    &
                               s33,s34,s44,i1,i2,i3,i4) !GCC$ ATTRIBUTES aligned(32) :: compute_scattering !GCC$ ATTRIBUTES hot :: compute_scattering
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine compute_scattering(refmed,wavel0,radius,ref0,  &
                               refm0,rperb1,dtheta,anglei, &
                               anglef,number,am,bm,cm,dm,  &
                               s11,s12,s13,s14,s22,s23,s24,    &
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
        real(kind=sp),    dimension(0:nsize),     intent(out) :: s11
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
        integer(kind=i4), automatic :: nbg,i,mst,nmx
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
          
          call bessel_start(v,nbg+2,mst)
          call logder_start(u,nbg,nmx)
          call check(isize,nsize,nbg,nang,mst,iff,igg,nmx)
          call bessel(v,bjv,byv,isize,nbg+2,iff,mst)
          call drlog(u,nbg,g,isize,igg,nmx)
          
          !$OMP SIMD LINEAR(i:1)
          do i=1,nbg+2
             hv(i)=cmplx(bjv(i),-byv(i))
          end do
          
          call cderv(hvd,hv,nbg,isize)
          call coeff(am,bm,v,u,wano,wan1,theta,  &
               nbg,afy,bjv,hv,hvd,g,epso,perb,perb,eo,1,isize)
          call coeff(cm,dm.u,v,wano,wan1,theta,nbg,afy,&
               bjv,hv,hvd,g,epso,perb,perb1,ho,2,isize)

          call amplitude(t1,nbg,1.0_sp,am, &
                         anglei,anglef,nang,theta,isize,nsize)
          call amplitude(t4,nbg,perb*afy/wano,bm,&
                         anglei,anglef,nang,theta,isize,nsize)
          call amplitude(t2,nbg,1.0_sp,cm, &
                         anglei,anglef,nang,theta,isize,nsize)
          call amplitude(t3,nbg,wano/(perb*afy),dm ,&
                         anglei,anglef,nang,theta,isize,nsize)
          call cross(cperp,dm,cm,2,nbg,perb,epso,awano,afy,isize)
          call amplitude(ttm0,nbg,1.0_sp,am,0.0_sp,180.0_sp,0, &
               theta,isize,nsize)
          extm=-4.0_sp/WANO*real(ttm0(0))
          call amplitude(tte0,nbg,1.0_sp,cm,0.0_sp,180.0_sp,0, &
               theta,isize,nsize)
          exte=-4.0_sp/wano*real(tte0(0))
          anorm=0.5_sp*(ttm0(0)*conjg(ttm0(0))+tte0(0)*conjg(tte0(0)))
          call sij(t1,t2,t3,t4,i1,i2,i3,i4, &
                   s11,s12,s13,s14,s22,s23, &
                   s24,s33,s34,s44,anglei,   &
                   anglef,nang,anorm,nsize, &
                   angle)
    
     end subroutine compute_scattering


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine bessel_start(x,norder,nst) !GCC$ ATTRIBUTES aligned(32) :: bessel_start !GCC$ ATTRIBUTES inline :: bessel_start
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine bessel_start(x,norder,nst)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: bessel_start
        !DIR$ ATTRIBUTES INLINE :: bessel_start
        !DIR$ OPTIMIZE : 3
#endif
         real(kind=sp),      intent(in)  :: x
         integer(kind=i4),   intent(in)  :: norder
         integer(kind=i4),   intent(out) :: nst
         ! Locals
         integer(kind=i4), automatic :: n1,n2
         ! Exec code ...
         if(norder/=1) then
            n1=10.0_sp+8.41_sp*(x)**0.393_sp
            n2=20.0_sp+norder+4.7_sp*norder**0.24_sp
            nst=max(n1,n2)
         else
            nst=10.0_sp+x+8.41_sp*x**0.393
         end if
     end subroutine bessel_start


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine logder_start(arg,nn,nmx) !GCC$ ATTRIBUTES aligned(32) :: logder_start !GCC$ ATTRIBUTES inline :: logder_start
#elif defined(__INTEL_COMPILER) || defined(__ICC)       
     subroutine logder_start(arg,nn,nmx)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: logder_start
          !DIR$ ATTRIBUTES INLINE :: logder_start
          !DIR$ OPTIMIZE : 3
#endif
         complex(kind=sp),      intent(in)  :: arg
         integer(kind=i4),      intent(in)  :: nn
         integer(kind=i4),      intent(out) :: nmx
         !Locals
         integer(kind=i4), automatic :: nbg
         n1=abs(arg)
         nmx=1.1_sp*max(nn,n1)+15
     end subroutine logder_start

     
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine cderv(xout,xin,nstop,isize)  !GCC$ ATTRIBUTES aligned(32) :: cderv !GCC$ ATTRIBUTES inline :: cderv
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine cderv(xout,xin,nstop,isize)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: logder_start
          !DIR$ ATTRIBUTES INLINE :: logder_start
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cderv
#endif
         complex(kind=sp), dimension(isize), intent(out) :: xout
         complex(kind=sp), dimension(isize), intent(in)  :: xin
         integer(kind=i4),                   intent(in)  :: nstop
         integer(kind=i4),                   intent(in)  :: isize
         ! Locals
         integer(kind=i4), automatic :: i
         do i=1,nstop+1
            if(i==1) xout(i) = -xin(i)
            if(i/=1) then
               xout(i)=0.5_sp*(xin(i-1)-xin(i+1))
            end if
         end do
     end subroutine cderv
       
       
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine coeff(as,bs,v,u,wano,wan1,theta,nbg, &
                      afy,bjv,hv,g,epso,perb,perb1,  &
                      ampl,mode,isize) !GCC$ ATTRIBUTES aligned(32) :: coeff !GCC$ ATTRIBUTES hot :: coeff
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine coeff(as,bs,v,u,wano,wan1,theta,nbg, &
                      afy,bjv,hv,g,epso,perb,perb1,  &
                      ampl,mode,isize)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: coeff
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: coeff
#endif
         complex(kind=sp),  dimension(-isize:isize) :: as
         complex(kind=sp),  dimension(-isize:isize) :: bs
         real(kind=sp)                              :: v
         complex(kind=sp)                           :: u
         real(kind=sp)                              :: wano
         complex(kind=sp)                           :: wan1
         real(kind=sp)                              :: theta
         integer(kind=i4)                           :: nbg
         real(kind=sp)                              :: afy
         real(kind=sp),     dimension(isize)        :: bjv
         complex(kind=sp),  dimension(isize)        :: hv
         complex(kind=sp),  dimension(isize)        :: hvd
         complex(kind=sp),  dimension(0:isize)      :: g
         real(kind=sp)                              :: epso
         real(kind=sp)                              :: perb
         real(kind=sp)                              :: perb1
         real(kind=sp)                              :: ampl
         integer(kind=i4)                           :: mode
         integer(kind=i4)                           :: isize
         ! Locals
         complex(kind=sp), automatic :: qm,sm,r,n,x,az,d
         real(kind=sp),    automatic :: t0,t1
         integer(kind=i4), automatic :: jj,m
         ! Exec code ..

         !$OMP SIMD LINEAR(m:1)
         do m=1, nbg
            jj=m+1
            qm=g(m)/u
            sm=hvd(jj)/(v*hv(jj))
            r=(1.0_sp/u)**2-(1.0_sp/v)**2
            n=wan1/wano
            t0=cos(theta)
            if(mode==1) x=perb1/perb
            if(mode==2) x=perb/perb1*(wan1/wano)**2
            d=(sm-x*qm)*(sm-n*n*qm/x)
            if(m==0) goto 5
            d=d-(r*m*t0)**2
5           d=d*PI*(v*hj(jj)**2*0.5_sp
            az=CCC**m*ampl*sin(theta)
            as(m) = -az*(bjv(jj)/hv(jj)+CCC*(sm-x*qm)/d)
            if(mode.eq.1.and.theta.ne.PI2) then
               bs(m)=wano/(perb*afy)*az*r*m*t0/d
            endif
            if(mode.eq.2.and.theta.ne.PI2) then
               bs(m)=-wano/(epso*afy)*az*r*m*t0/d
            endif
            if(theta.eq.PI2) bs(m)=0.0_sp
         end do
         !$OMP SIMD LINEAR(m:1)
         do m=1,nbg
            as(-m)=(-1)**m*as(m)
            bs(-m)=-(-1)**m*bs(m)
         end do
     end subroutine coeff
     

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine amplitude(tt,nbg,xfactor,bb1,anglei, &
                          anglef,nang,theta,isize,nsize) !GCC$ ATTRIBUTES aligned(32) :: amplitude !GCC$ ATTRIBUTES inline :: amplitude
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine amplitude(tt,nbg,xfactor,bb1,anglei, &
         anglef,nang,theta,isize,nsize)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: amplitude
          !DIR$ ATTRIBUTES INLINE :: amplitude
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: amplitude

#endif
        complex(kind=sp),   dimension(0:nsize)      :: tt
        integer(kind=i4)                            :: nbg
        real(kind=sp)                               :: xfactor
        complex(kind=sp),   dimension(-isize:isize) :: bb1
        real(kind=sp)                               :: anglei
        real(kind=sp)                               :: anglef
        integer(kind=i4)                            :: nang
        real(kind=sp)                               :: theta
        integer(kind=i4)                            :: isize
        integer(kind=i4)                            :: nsize
        ! Locals
        complex(kind=sp), automatic :: sumt,emph
        real(kind=sp),    automatic :: angle,phi,inv,st
        integer(kind=i4), automatic :: i,m
        ! Exec code ...
        inv = 1.0_sp/real(nang,kind=sp)
        st=sin(theta)
        do i=0,nang
           if(nang.ne.0) then
              angle=anglei+real(i,kind=sp)*(anglef-anglei)*inv
           end if
           if(nang.eq.0) angle=englei
           phi=(angle+180.0)*PI180
           sumt=cmplx(0.0_sp,0.0_sp)
           !DIR$ OMP SIMD REDUCTION(+:sumt)
           do m=-nbg,nbg
              emph=cmplx(cos(m*phi),-sin(m*phi))
              sumt=sumt+bb1(m)*emph*CCC**m
           end do
           tt(i)=xfactor*sumt/st
        end do
    end subroutine amplitude
    

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine sij(t1,t2,t3,t4,i1,i2,i3,i4, &
                   s11,s12,s13,s14,s22,s23,s24, &
                   s33,s34,s44,             &
                   anglei,anglef,nang,anorm,&
                   nsize,angle) !GCC$ ATTRIBUTES aligned(32) :: sij !GCC$ ATTRIBUTES hot :: sij
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine sij(t1,t2,t3,t4,i1,i2,i3,i4, &
                     s11,s12,s13,s14,s22,s23,s24, &
                     s33,s34,s44, 
                     anglei,anglef,nang,anorm, &
                     nsize,angle)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: sij
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: sij
#endif
        complex(kind=sp),  dimension(0:nsize) :: t1
        complex(kind=sp),  dimension(0:nsize) :: t2
        complex(kind=sp),  dimension(0:nsize) :: t3
        complex(kind=sp),  dimension(0:nsize) :: t4
        real(kind=sp),     dimension(0:nsize) :: i1
        real(kind=sp),     dimension(0:nsize) :: i2
        real(kind=sp),     dimension(0:nsize) :: i3
        real(kind=sp),     dimension(0:nsize) :: i4
        real(kind=sp),     dimension(0:nsize) :: s11
        real(kind=sp),     dimension(0:nang)  :: s12
        real(kind=sp),     dimension(0:nang)  :: s13
        real(kind=sp),     dimension(0:nang)  :: s14
        real(kind=sp),     dimension(0:nang)  :: s22
        real(kind=sp),     dimension(0:nang)  :: s23
        real(kind=sp),     dimension(0:nang)  :: s24
        real(kind=sp),     dimension(0:nang) :: s33
        real(kind=sp),     dimension(0:nang) :: s34
        real(kind=sp),     dimension(0:nang) :: s44
        
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED t1:64
        !DIR$ ASSUME_ALIGNED t2:64
        !DIR$ ASSUME_ALIGNED t3:64
        !DIR$ ASSUME_ALIGNED t4:64
        !DIR$ ASSUME_ALIGNED i1:64
        !DIR$ ASSUME_ALIGNED i2:64
        !DIR$ ASSUME_ALIGNED i3:64
        !DIR$ ASSUME_ALIGNED i4:64
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
        real(kind=sp)                         :: anglei
        real(kind=sp)                         :: anglef
        integer(kind=i4)                      :: nang
        real(kind=sp)                         :: anorm
        integer(kind=i4)                      :: nsize
        real(kind=sp),     dimension(0:nsize) :: angle
       
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED angle:64
      
#endif
        complex(kind=sp), automatic :: c1,c2,c3,c4
        real(kind=sp),    automatic :: inv,t0,inv2,y
        real(kind=sp),    automatic :: x13,x14,x22,x23,x24,x33,x34, &
                                       x44
        integer(kind=i4), automatic :: i
        !Exec code ...
        inv = 1.0_sp/real(nang,kind=sp)
        inv2 = 1.0_sp/anorm
        !$OMP SIMD LINEAR(i:1) UNROLL PARTIAL(4)
        do i=0,nang
           if(nang.ne.0) then
              angle(i)=anglei+real(i,kind=sp)*(anglef-anglei)*inv
           end if
           if(nang.eq.0) angle(i)=anglei
           c1=conjg(t1(i))
           c2=conjg(t2(i))
           c3=conjg(t3(i))
           c4=conjg(t4(i))
           i1(i)=t1(i)*c1
           i2(i)=t2(i)*c2
           i3(i)=t3(i)*c3
           i4(i)=t4(i)*c4
           s11(i)=(i1(i)+i2(i)+i3(i)+i4(i))*0.5_sp
           y=s11(i)
           t0=(i1(i)-i2(i)-i3(i)+i4(i))*0.5_sp
           s12(i)=t0/s11(i)
           s11(i)=s11(i)*inv2
           x13=real(t1(i)*c3+t4(i)*c2
           s13(i)=x13/y
           x14=aimag(t3(i)*c1+t2(i)*c4
           s14(i)=x14/y
           x22=(i1(i)+i2(i)-i4(i)-i3(i))*0.5_sp
           s22(i)=x22/y
           x23=real(t1(i)*c3-t2(i)*c4)
           s23(i)=x23/y
           x24=aimag(t3(t)*c1+t4(t)*c2)
           s24(i)=x24/y
           x33=real(t2(i)*c1+t3(i)*c4)
           s33(i)=x33/y
           x34=aimag(t2(i)*c1+t3(i)*c4)
           s34(i)=x34/y
           x44=real(t1(i)*c2-t4(i)*c3)
           s44=x34/y
       end do   
    
    end subroutine sij

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine cross(cr,as,bs,mode,nbg,perb, &
                     epso,awano,afy,isize) !GCC$ ATTRIBUTES aligned(32) :: cross !GCC$ ATTRIBUTES inline :: cross
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine cross(cr,as,bs,mode,nbg,perb, &
                     epso,awano,afy,isize)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cross
          !DIR$ ATTRIBUTES INLINE :: cross
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 ::cross
#endif
         real(kind=sp)                              :: cr
         complex(kind=sp),  dimension(-isize:isize) :: as
         complex(kind=sp),  dimension(-isize:isize) :: bs
#if defined(__INTEL_COMPILER) || defined(__ICC)
            !DIR$ ASSUME_ALIGNED as:64
            !DIR$ ASSUME_ALIGNED bs:64
#endif         
         integer(kind=i4)                           :: mode
         integer(kind=i4)                           :: nbg
         real(kind=sp)                              :: perb
         real(kind=sp)                              :: epso
         real(kind=sp)                              :: awano
         real(kind=sp)                              :: afy
         integer(kind=i4)                           :: isize
         ! Locals
         complex(kind=sp), automatic :: t0,t1
         real(kind=sp),    automatic :: suma,sumb
         real(kind=sp),    automatic :: xfactor,eta
         integer(kind=i4), automatic :: i
         ! Exec code ...
         !eta=376.9911184307751886155172_sp
         eta=Z'40778FDB9EFFEA47'
         suma=0.0_sp
         sumb=0.0_sp
         !$OMP SIMD REDUCTION(+:suma) REDUCTION(+:sumb)
         do i=-nbg,nbg
            t0=as(i)
            suma=suma+t0*conjg(t0)
            t1=bs(i)
            sumb=sumb+t1*conjg(t1)
         end do
         suma=suma*epso
         sumb=sumb*perb
         if(mode.eq.1) xfactor=4.0_sp*eta*afy/(awano*awano)
         if(mode.eq.2) xfactor=4.0_sp*afy/(eta*awano*awano)
         cr=xfactor*(suma+sumb)
     end subroutine cross


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))       
    subroutine drlog(arg,nn,g,isize,igg,nmx) !GCC$ ATTRIBUTES aligned(32) :: drlog !GCC$ ATTRIBUTES inline :: drlog
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine drlog(arg,nn,g,isize,igg,nmx)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: drlog
          !DIR$ ATTRIBUTES INLINE :: drlog
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: drlog
#endif
        complex(kind=sp)                          :: arg
        integer(kind=i4)                          :: nn
        complex(kind=sp),  dimension(0:igg+isize) :: g
        integer(kind=i4)                          :: isize
        integer(kind=i4)                          :: igg
        integer(kind=i4)                          :: nmx
        ! Locals
        complex(kind=sp), automatic :: inv,c0,c1
        real(kind=sp),    automatic :: t0,t1
        integer(kind=i4), automatic :: i
        ! Exec code ....
        g(nmx)=cmplx(0.0_sp,0.0_sp)
        inv=1.0_sp/arg
        !$OMP SIMD
        do i=nmx,1,-1
           t0=real(i,kind=sp)
           c0=t0*inv+g(i)
           t1=real(i-1,kind=sp)
           c1=t1*inv-1.0_sp
           g(i-1)=c1/c0
        end do
    end subroutine drlog
      
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine bessel(x,bj,by,isize,nc,iff,mst) !GCC$ ATTRIBUTES aligned(32) :: bessel !GCC$ ATTRIBUTES inline :: bessel
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine bessel(x,bj,by,isize,nc,iff,mst)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: bessel
          !DIR$ ATTRIBUTES INLINE :: bessel
          !DIR$ OPTIMIZE : 3
          !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: bessel
#endif
         real(kind=sp)                     :: x
         real(kind=sp),   dimension(isize) :: bj
         real(kind=sp),   dimension(isize) :: by
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED bj:64
         !DIR$ ASSUME_ALIGNED by:64
#endif
         integer(kind=i4)                  :: isize
         integer(kind=i4)                  :: nc
         integer(kind=i4)                  :: iff
         integer(kind=i4)                  :: mst
         ! Locals
         real(kind=sp), dimension(isize+iff), automatic :: f
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: f
#endif
         real(kind=dp), parameter :: gamma = Z'3FE2788CFC6FB5FF' !0.57721566490153
         real(kind=dp), parameter :: twodivpi = Z'3FE45F306DC9C877' !0.63661977236758
         real(kind=sp),    automatic   :: xstop,alpha,rn,inv
         integer(kind=i4), automatic   :: nstop,nmx,j,n,nn,k,mst,m1,l,ml,m2
         integer(kind=i4), automatic   :: ll,m3,m4,kk,ns
         ! Exec code .....
           nstop=nc
           mst=(mst/2)*2  
           f(mst+1)=0.0
           f(mst)=1.0e-32_sp
           m1=mst-1
           !$OMP SIMD LINEAR(l:1)
           do l=1,m1
              ml=mst-l
              f(ml)=2.0_sp*float(ml)*f(ml+1)/x-f(ml+2)
           end do
           alpha=f(1)
           m2=mst-2
           !$OMP SIMD REDUCTION(+:alpha)
           do ll=2,m2,2
              alpha=alpha+2.0_dp*f(ll+1)
           end do
           inv=1.0_sp/alpha
           !$OMP SIMD LINEAR(n:1)
           do n=1,m1
               bj(n)=f(n)*alpha
           end do
           by(1)=bj(1)*(alog(x/2.0_sp)+gamma)
           m4=mst/2-1
           !$OMP SIMD LINEAR(l:1)
           do l=1,m4
                by(1)=by(1)-2.0_sp*((-1.0_sp)**l)*bj(2*l+1)/float(l)
           end do
           by(1)=twodivpi*by(1)
           by(2)=(bj(2)*by(1)-twodivpi/x)/bj(1)
           ns=nstop-1
           !$OMP SIMD LINEAR(kk:1)
           do kk=1,ns
               by(kk+2)=2.0_sp*float(kk)*by(kk+1)/x-by(kk)
           end do
    end subroutine bessel
      

     SUBROUTINE CHECK(ISIZE,NSIZE,NBG,NUMBER,MST,IFF,IGG,NMX)
!C
!C  THIS SUBROUTINE IS TO ENSURE THAT NO ARRAY
!C  IS OVERWRITTEN USING EQ.(30)
!C
        INTEGER(kind=i4) ::  ISIZE,NSIZE,NBG,NUMBER,MST,IFF,IGG,NMX
        INTEGER(kind=i4) ::  IERROR,JERROR,KERROR,LERROR
        IERROR=0
        JERROR=0
        KERROR=0
        LERROR=0
!C
        IF((IGG+ISIZE).LT.NMX)THEN
        CALL MESSAGE
        IERROR=1
        WRITE(6,17)NMX-ISIZE
 17     FORMAT('ALLOCATED SPACE FOR PARAMETER IGG IS NOT
     6  ENOUGH, IGG MUST BE AT LEAST',I7)
        WRITE(6,*)
        ENDIF
!C 
        IF((IFF+ISIZE).LT.MST)THEN
        CALL MESSAGE
        JERROR=1
        WRITE(6,11)MST-IFF
  11    FORMAT(3X,'ALLOCATED SPACE FOR PARAMETER IFF IS NOT
     6  ENOUGH, IFF MUST BE AT LEAST', I7)
        WRITE(6,*) 
        END IF
!C      
        IF(ISIZE.LE.NBG+2)THEN
        CALL MESSAGE
        KERROR=1
        WRITE(6,10)NBG+2
  10    FORMAT(3X,'ALLOCATED SPACE FOR ISIZE IS NOT
     6  ENOUGH, ISIZE MUST BE AT LEAST' ,I7)
        WRITE(6,*)
        END IF
!C
        IF(NSIZE.LT.NUMBER+2)THEN
        CALL MESSAGE
        LERROR=1
        WRITE(6,20)NUMBER+2
  20    FORMAT(3X,'ALLOCATED SPACE FOR NSIZE IS NOT ENOUGH,
     6  NSIZE MUST BE AT LEAST' ,I7)
        WRITE(6,*)
        END IF

        IF(IERROR.EQ.1.OR.JERROR.EQ.1)STOP
        IF(KERROR.EQ.1.OR.LERROR.EQ.1)STOP
      
      END SUBROUTINE CHECK
      
   SUBROUTINE MESSAGE
      WRITE(6,10)
 10   FORMAT(3X,'THE ALLOCATED SPACE OF THE PARAMETER')
      WRITE(6,20)
 20   FORMAT('INDICATED BELOW IS NOT SUFFICIENT, THIS IS CORRECTED BY')
      WRITE(6,30) 
 30   FORMAT('LOCATING THE RIGHT SPACE FOR THE PARAMETER')
      WRITE(6,40)
 40   FORMAT('IN THE STATEMENT OF THE MAIN PROGRAM')
    
    END SUBROUTINE MESSAGE
    





end module cylindrical_scattering
