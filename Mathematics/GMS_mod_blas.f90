

module mod_blas


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_blas
 !          
 !          Purpose:
  !                      This module contains an explicitly vectorized complex
  !                      blas implementation, which is based on packed AoS
  !                      complex data type (AVX512c8f64_t).
 !
 !          History:
 !                        Date: 29-11-2019
 !                        Time: 10:41 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !                      As per original authors request -- the name of the
 !                      modified subroutine/function was changed to include the 'gms' prefix.
 !                 
 !          References:
 !         
 !                            Based on LAPACK package
 !                            Copyright (c) 1992-2013 The University of Tennessee and The University
 !                            of Tennessee Research Foundation.  All rights
 !                            reserved.
 !                            Copyright (c) 2000-2013 The University of California Berkeley. All
 !                            rights reserved.
 !                            Copyright (c) 2006-2013 The University of Colorado Denver.  All rights
 !                            reserved.
 !
 !                            $COPYRIGHT$
 !
 !                            Additional copyrights may follow
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use module_kinds, only : int4,dp
    use mod_avx512c8f64
    implicit none

     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_BLAS_MAJOR = 1
    ! MInor version
    integer(kind=int4), parameter, public :: MOD_BLAS_MINOR = 0
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_BLAS_MICRO = 0
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_BLAS_FULLVER = &
         1000*MOD_BLAS_MAJOR+100*MOD_BLAS_MINOR+10*MOD_BLAS_MICRO
    !Module creation date
    character(*),       parameter, public :: MOD_BLAS_CREATION_DATE = "29-11-2019 10:55 +00200 (FRI 29 NOV 2019 GMT+2)"
    ! Module build date
    character(*),       parameter, public :: MOD_BLAS_BUILD_DATE    = __DATE__ " " __TIME__
    ! Module author info
    character(*)        parameter, public :: MOD_BLAS_AUTHOR = "LAPACK original authors[all rights reserved] -- This version was  modified by Bernard Gingold, contact: beniekg@gmail.com"
    ! Module short description
    character(*)        parameter, public :: MOD_BLAS_SYNOPSIS = "Explicitly vectorized complex  blas implementation, which is based on packed AoS complex data type (AVX512c8f64_t) "
                                                                  
    ! public

  contains

!*  Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!*
!*> \ingroup complex16_blas_level1
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>     jack dongarra, 3/11/78.
!*>     modified 12/3/93, array(1) declarations changed to array(*)
!       Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)
!*> \endverbatim
!*>

    subroutine gms_zaxpy(n,za,zx,incx,zy,incy)
      
      !DIR$ ATTRIBUTES CODE_ALIGNED : 32 :: gms_zaxpy
      !DIR$ ATTRIBUTES VECTOR :: gms_zaxpy
      use mod_vecconsts, only : v8_n0
      integer(kind=int4),                intent(in)    :: n
      type(AVX512c8f64_t),               intent(in)    :: za
      type(AVX512c8f64_t), dimension(*), intent(in)    :: zx
      !DIR$ ASSUME_ALIGNED zx:64
      integer(kind=int4),                intent(in)    :: incx
      type(AVX512c8f64_t), dimension(*), intent(inout) :: zy
      !DIR$ ASSUME_ALIGNED zy:64
      integer(kind=int4),                intent(in)    :: incy
      ! Locals
      integer(kind=int4), automatic :: i,ix,iy
      ! EXec code .....
      if(n<=0) return
      if(all(cabs_zmm8c8(za) == v8_n0.v)) return
      if(incx==1 .and. incy==1) then
         ! *        code for both increments equal to 1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            zy(i) = zy(i)+za*zx(i)
         end do

         !*        code for unequal increments or equal increments
         !*          not equal to 1
      else
         ix=1
         iy=1
         if(incx<0) ix=(-n+1)*incx+1
         if(incy<0) iy=(-n+1)*incy+1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            zy(iy) = zy(iy)+za*zx(ix)
            ix = ix+incx
            iy = iy+incy
         end do
      end if
    end subroutine gms_zaxpy

!*  Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!*
!*> \ingroup complex16_blas_level1
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>     jack dongarra, linpack, 4/11/78.
!*>     modified 12/3/93, array(1) declarations changed to array(*)
!       Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)    
!*> \endverbatim
!*>

    subroutine gms_zcopy(n,zx,incx,zy,incy)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zcopy
      !DIR$ ATTRIBUTES VECTOR :: gms_zcopy
      integer(kind=int4),                intent(in)  :: n
      type(AVX512c8f64_t), dimension(*), intent(in)  :: zx
      !DIR$ ASSUME_ALIGNED zx:64
      integer(kind=int4),                intent(in)  :: incx
      type(AVX512c8f64_t), dimension(*), intent(out) :: zy
      integer(kind=dint4),               intent(in)  :: incy
      ! LOcals
      integer(kind=int4), automatic :: i,ix,iy
      ! EXec code ...
      if(n<=0) return
      if(incx==1 .and. incy==1) then
         
         !  code for both increments equal to 1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            zy(i) = zx(i)
         end do

         !  code for unequal increments or equal increments
         !*          not equal to 1
       else
         ix=1
         iy=1
         if(incx<0) ix=(-n+1)*incx+1
         if(incy<0) iy=(-n+1)*incy+1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            zy(iy) = zx(ix)
            ix = ix+incx
            iy = iy+incy
         end do
      end if
    end subroutine gms_zcopy

!     Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!*
!1*> \ingroup complex16_blas_level1
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>     jack dongarra, 3/11/78.
!*>     modified 12/3/93, array(1) declarations changed to array(*)
!       Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)     
!*> \endverbatim
    
    function gms_zdotc(n,zx,incx,zy,incy) result(dotc)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zdotc
      !DIR$ ATTRIBUTES VECTOR :: gms_zdotc
      integer(kind=int4),                intent(in) :: n
      type(AVX512c8f64_t), dimension(*), intent(in) :: zx
      !DIR$ ASSUME_ALIGNED zx:64
      integer(kind=int4),                intent(in) :: incx
      type(AVX512c8f64_t), dimension(*), intent(in) :: zy
      !DIR$ ASSUME_ALIGNED zy:64
      integer(kind=int4),                intent(in) :: incy
      !DIR$ ATTRIBUTES ALIGN : 64 :: dotc
      type(AVX512c8f64_t) :: dotc
      !DIR$ ATTRIBUTES ALIGN : 64 :: ztemp
      type(AVX512c8f64_t), automatic :: ztemp
      integer(kind=int4), automatic :: i,ix,iy
      ! EXec code ....
      !
      if(n<=0) return
      ztemp = default_init()
      if(incx==1 .and. incy==1) then
         !   code for both increments equal to 1
         
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ztemp = ztemp+conjugate(zx(i))*zy(i)
         end do
      else
         !   code for unequal increments or equal increments
         !*          not equal to 1
         ix=1
         iy=1
         if(incx<0) ix=(-n+1)*incx+1
         if(incy<0) iy=(-n+1)*incy+1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ztemp = ztemp+conjugate(zx(ix))*zy(iy)
            ix = ix+incx
            iy = iy+incy
         end do
         zdotc = default_init()
      end if  
      zdotc = ztemp
    end function gms_zdotc

!     Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!*
!1*> \ingroup complex16_blas_level1
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>     jack dongarra, 3/11/78.
!*>     modified 12/3/93, array(1) declarations changed to array(*)
!       Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)     
!*> \endverbatim

    function gms_zdotu(n,zx,incx,zy,incy) result(zdotu)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zdotu
      !DIR$ ATTRIBUTES VECTOR :: gms_zdotu
      integer(kind=int4),                intent(in) :: n
      type(AVX512c8f64_t), dimension(*), intent(in) :: zx
      !DIR$ ASSUME_ALIGNED zx:64
      integer(kind=int4),                intent(in) :: incx
      type(AVX512c8f64_t), dimension(*), intent(in) :: zy
      !DIR$ ASSUME_ALIGNED zy:64
      integer(kind=int4),                intent(in) :: incy
      ! LOcals
      !DIR$ ATTRIBUTES ALIGN : 64 :: zdotu
      type(AVX512c8f64_t) :: zdotu
      !DIR$ ATTRIBUTES ALIGN : 64 :: ztemp
      type(AVX512c8f64_t), automatic :: ztemp
      integer(kind=int4),  automatic :: i,ix,iy
      ! Exec code ....
      if(n<=0) return
      ztemp = default_init()
      if(incx==1 .and. incy==1) then

         !  code for both increments equal to 1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ztemp = ztemp+zx(i)*zy(i)
         end do

         !  code for unequal increments or equal increments
         !*          not equal to 1
      else
         ix=1
         iy=1
         if(incx<0) ix=(-n+1)*incx+1
         if(incy<0) iy=(-n+1)*incy+1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ztemp = ztemp+zx(ix)*zy(iy)
            ix = ix+incx
            iy = iy+incy
         end do
      end if
      zdotu = default_init()
      zdotu = ztemp
    end function gms_zdotu

!    *
!*  Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*  Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)     
!*> \date December 2016

    subroutine gms_zdrot(n,cx,incx,cy,incy,c,s)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zdrot
      !DIR$ ATTRIBUTES VECTOR :: gms_zdrot
      use mod_vectypes, only : ZMM8r8_t
      integer(kind=int4),                intent(in)    :: n
      type(AVX512c8f64_t), dimension(*), intent(inout) :: cx
      !DIR$ ASSUME_ALIGNED cx:64
      integer(kind=int4),                intent(in)    :: incx
      type(AVX512c8f64_t), dimension(*), intent(inout) :: cy
      !DIR$ ASSUME_ALIGNED cy:64
      integer(kind=int4),                intent(in)    :: incy
      type(ZMM8r8_t),                    intent(in)    :: c
      !DIR$ ASSUME_ALIGNED c:64
      type(ZMM8r8_t),                    intent(in)    :: s
      !DIR$ ASSUME_ALIGNED s:64
      !DIR$ ATTRIBUTES ALIGN : 64 :: ztemp
      type(AVX512c8f64_t), automatic :: ztemp
      integer(kind=int4),  automatic :: i,ix,iy
      ! EXec code ...
      if(n<=0) return
      ctemp = default_init()
      if(incx==1 .and. incy==1) then
         !  code for both increments equal to 1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ctemp = c*cx(i)+s*cy(i)
            cy(i) = c*cy(i)-s*cx(i)
            cx(i) = ctemp
         end do

         !  code for unequal increments or equal increments not equal
         !*          to 1
      else
         ix=1
         iy=1
         if(incx<0) ix=(-n+1)*incx+1
         if(incy<0) iy=(-n+1)*incy+1
         !DIR$ VECTOR ALIGNED
         !DIR$ VECTOR ALWAYS
         do i = 1,n
            ctemp  = c*cx(ix)+s*cy(iy)
            cy(iy) = c*cy(iy)-s*cx(ix)
            cx(ix) = ctemp
            ix = ix+incx
            iy = iy+incy
         end do
       end if
    end subroutine gms_zdrot

!     Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!1*
!*> \ingroup complex16_blas_level1
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>     jack dongarra, 3/11/78.
!*>     modified 3/93 to return if incx .le. 0.
!*>     modified 12/3/93, array(1) declarations changed to array(*)
!       Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features)       
!*> \endverbatim

    subroutine gms_zdscal(n,da,zx,incx)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zdscal
       !DIR$ ATTRIBUTES VECTOR :: gms_zdscal
       integer(kind=int4),                intent(in)    :: n
       complex(kind=dp),                  intent(in)    :: da
       type(AVX512c8f64_t), dimension(*), intent(inout) :: zx
       !DIR$ ASSUME_ALIGNED zx:64
       integer(kind=int4),                intent(in)    :: incx
       ! LOcals
       integer(kind=int4), automatic :: i,nincx
       ! Exec code ....
       if(n<=0 .or. incx<0) return
       if(incx==1) then
          !  code for increment equal to 1
          !DIR$ VECTOR ALIGNED
          !DIR$ VECTOR ALWAYS
          do i = 1,n
             zx(i) = da*zx(i)
          end do
       else
          !   *        code for increment not equal to 1
          nincx=n*incx
          !DIR$ VECTOR ALIGNED
          !DIR$ VECTOR ALWAYS
          do i = 1,nincx,incx
             zx(i) = da*zx(i)
          end do
       end if
    end subroutine gms_zdscal

!    *> \date December 2016
!*
!*> \ingroup complex16_blas_level2
!*
!*> \par Further Details:
!*  =====================
!*>
!*> \verbatim
!*>
!*>  Level 2 Blas routine.
!*>  The vector and matrix arguments are not referenced when N = 0, or M = 0
!*>
!*>  -- Written on 22-October-1986.
!*>     Jack Dongarra, Argonne National Lab.
!*>     Jeremy Du Croz, Nag Central Office.
!*>     Sven Hammarling, Nag Central Office.
!*>     Richard Hanson, Sandia National Labs.
!  Modified by Bernard Gingold on 29-11-2019 (removing build-in complex*16 data type,using modern Fortran features) 
!*> \endverbatim

    subroutine gms_zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: gms_zgbmv
        !
        character(len=5),                      intent(in) :: trans
        integer(kind=int4),                    intent(in) :: m
        integer(kind=int4),                    intent(in) :: n
        integer(kind=int4),                    intent(in) :: kl
        integer(kind=int4),                    intent(in) :: ku
        complex(kind=dp),                      intent(in) :: alpha
        complex(kind=dp),                      intent(in) :: beta
        type(AVX512c8f64_t), dimension(lda,*), intent(in) :: a
        !DIR$ ASSUME_ALIGNED a:64
        integer(kind=int4),                    intent(in) :: lda
        type(AVX512c8f64_t), dimension(*),     intent(in) :: x
        !DIR$ ASSUME_ALIGNED x:64
        integer(kind=int4),                    intent(in) :: incx
        complex(kind=dp),                      intent(in) :: beta
        type(AVX512c8f64_t), dimension(*),     intent(inout) :: y
        integer(kind=int4),                    intent(in) :: incy
        ! LOcals
        !DIR$ ATTRIBUTES ALIGN : 64 :: temp
        type(AVX512c8f64_t), automatic :: temp
        !DIR$ ATTRIBUTES ALIGN : 64 :: VCZERO
        type(AVX512c8f64_t), automatic :: VCZERO
        integer(kind=dp),    automatic :: i,info,ix,iy,j,jk,k,kup1,kx,ky,lenx,leny
        logical(kind=int4),  automatic :: noconj
        complex(kind=dp), parameter :: ONE  = (1.0_dp,0.0_dp)
        complex(kind=dp), parameter :: ZERO = (0.0_dp,0.0_dp)
        ! EXec code ....
        info = 0
        if(.not.lsame(TRANS,'N') .and. .not.lsame(TRANS,'T') .and. &
           .not.lsame(TRANS,'C')) then
           info = 1
        else if(m<0) then
           info = 2
        else if(n<0) then
           info = 3
        else if(kl<0) then
           info = 4
        else if(ku<0) then
           info = 5
        else if(lda<(kl+ku+1)) then
           info = 8
        else if(incx==0) then
           info = 10
        else if(incy==0) then
           info = 13
        end if
        if(info/=0) then
           call xerbla('GMS_ZGBMV',info)
           return
        end if
        !  Quick return if possible.
        if((m==0) .or. (n==0) .or. &
             ((alpha==ZERO) .and. (beta==ONE))) return
        noconj = lsame(TRANS,'T')
        !  Set  LENX  and  LENY, the lengths of the vectors x and y, and set
        !*     up the start points in  X  and  Y.
        if(lsame(TRANS,'N')) then
           lenx = n
           leny = m
        else
           lenx = m
           leny = n
        end if
        if(incx>0) then
           kx = 1
        else
           kx = 1-(lenx-1)*incx
        end if
        if(incy>0) then
           ky = 1
        else
           ky = 1-(leny-1)*incy
        end if
        ! *     Start the operations. In this version the elements of A are
        ! *     accessed sequentially with one pass through the band part of A.
        ! *
        ! *     First form  y := beta*y.
        VCZERO = default_init()
        if(beta/=one) then
           if(incy==1) then
              if(beta==zero) then
                 !DIR$ VECTOR ALIGNED
                 !DIR$ VECTOR ALWAYS
                 do i=1,leny
                    y(i) = VCZERO
                 end do
              else
                 !DIR$ VECTOR ALIGNED
                 !DIR$ VECTOR ALWAYS
                 do i=1,leny
                    y(i) = beta*y(i)
                 end do
              end if
           else
              iy=ky
              if(beta==zero) then
                 !DIR$ VECTOR ALIGNED
                 !DIR$ VECTOR ALWAYS
                 do i=1,leny
                    y(iy) = VCZERO
                    iy = iy+incy
                 end do
              else
                 !DIR$ VECTOR ALIGNED
                 !DIR$ VECTOR ALWAYS
                 do i=1,leny
                    y(iy) = beta*y(iy)
                    iy = iy+incy
                 end do
              end if
           end if
        end if
        if(alpha==zero) return
        kup1 = ku+1
        
    end subroutine gms_zgbmv
    
end module mod_blas
