
module mod_complex_arithm

     
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_complex_arithm'
 !          
 !          Purpose:
 !                    Complex arithmetic module.
 !                   
 !                     
 !          History:
 !                         Date: 03-08-2017
  !                        Time: 10:31 GMT+2
  !         Modified:
  !                        Date: 30-07-2020
  !                        Time: 17:56 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 1
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    
    use mod_kinds,  only : int4, sp
    implicit none
    public
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter :: MOD_COMPLEX_ARITHM_MAJOR = 1
    
    ! Minor version
    integer(kind=int4), parameter :: MOD_COMPLEX_ARITHM_MINOR = 1
    
    ! Micro version
    integer(kind=int4), parameter :: MOD_COMPLEX_ARITHM_MICRO = 0
    
    ! Module/file full version
    integer(kind=int4), parameter :: MOD_COMPLEX_ARITHM_FULLVER = 1000*MOD_COMPLEX_ARITHM_MAJOR+100*MOD_COMPLEX_ARITHM_MINOR + &
                                                                     10*MOD_COMPLEX_ARITHM_MICRO
    
    ! Module/file creation date
    character(*),  parameter :: MOD_COMPLEX_ARITHM_CREATE_DATE = "06-08-2017 11:32 +00200 (SUN 06 AUG 2017 GMT+2)"
    
    ! Module/file build date/time (should be set after latest successful build)
    character(*),  parameter :: MOD_COMPLEX_ARITHM_BUILD_DATE = __DATE__ "" __TIME__
    
    ! Module/file author info
    character(*),  parameter :: MOD_COMPLEX_ARITHM_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter :: MOD_COMPLEX_ARITHM_DESCRIPT = "Simple scalar complex numbers arithmetic module."
    
    contains
    
    !======================================================60
    !  subroutine: sincos
    !              Sine and Cosine of radian argument
    !              operating on scalar argument
    ! @Return: 
    !              real(kind=4) array of size=2
      !======================================================60
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      subroutine sincos(rad,scval) !GCC$ ATTRIBUTES hot :: sincos !GCC$ ATTRIBUTES inline :: sincos !GCC$ ATTRIBUTES aligned(32) :: sincos
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: sincos
      subroutine sincos(rad,scval)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: sincos 
#endif
          real(kind=sp),               intent(in)    :: rad
          real(kind=sp), dimension(2), intent(inout) :: scval
          ! Start of executable statements
          scval(1) = COS(rad)
          scval(2) = SIN(rad)
    end subroutine
    
    !======================================================60
    !  subroutine: v_sincos
    !              Sine and Cosine of radian arguments 
    !              operating on vector argument
    ! @Return: 
    !             real(kind=8) array(rank 2) of size equal to 
    !             argument array size.
    !             No error checking is made inside the
    !             subroutine.
    !======================================================60
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
    subroutine v_sincos(vrad,vres) !GCC$ ATTRIBUTES hot :: v_sincos !GCC$ ATTRIBUTES inline :: v_sincos !GCC$ ATTRIBUTES aligned(32) :: v_sincos
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: v_sincos
      subroutine v_sincos(vrad,vres)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: v_sincos 
#endif
          real(kind=sp), dimension(:),   intent(in)    :: vrad

          real(kind=sp), dimension(:,:), intent(inout) :: vres
!DIR$     ASSUME_ALIGNED vres:32
          ! Locals
          integer(kind=int4) :: i
          ! Start of executable statements
          !DIR$     ASSUME_ALIGNED vrad:64
          !DIR$     ASSUME_ALIGNED vrad:64
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
 ! attempt to vectorize, Compiler should use calls to svml library.          
          do i = 1, SIZE(vrad)
              vres(1,i) = COS(vrad(i))
              vres(2,i) = SIN(vrad(i))
          end do
    end subroutine
    
    !======================================================60
    !  subroutine: init_from_rad
    !              Initializes scalar complex number from
    !              sincos value of its real argument(radian)
    !  @Returns:   complex c
    !======================================================60
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
    subroutine init_from_rad(rad,cres) !GCC$ ATTRIBUTES hot :: init_from_rad !GCC$ ATTRIBUTES inline :: init_rad_from !GCC$ ATTRIBUTES aligned(32) :: init_from_rad
#elif defined __ICC || defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES INLINE :: init_from_rad
    subroutine init_from_rad(rad,cres)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: init_from_rad  
#endif
          real(kind=sp),    intent(in)  :: rad
          complex(kind=sp), intent(out) :: cres
          ! Locals
          real(kind=sp), dimension(2) :: scv = [0._sp,0._sp]
          ! Start of executable statements
          call sincos(rad,scv)
          cres = CMPLX(scv(1),scv(2))
    end subroutine
    
    !======================================================60
    !  subroutine: initvec_from_rad
    !              Initializes vector of complex numbers from
    !              vector of sincos values of its real 
    !              argument(radian)
    !  @Returns:   vector of complex numbers
    !======================================================60
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
    subroutine initvec_from_rad(vrad,vres,vcres) !GCC$ ATTRIBUTES hot :: initvec_from_rad !GCC$ ATTRIBUTES inline :: initvec_rad_from !GCC$ ATTRIBUTES aligned(32) :: initvec_from_rad
#elif defined __ICC || defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: initvec_from_rad
    subroutine initvec_from_rad(vrad,vres,vcres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: initvec_from_rad  
#endif
          real(kind=sp), dimension(:),    intent(in)    :: vrad
          real(kind=sp), dimension(:,:),  intent(inout) :: vres
          complex(kind=sp), dimension(:), intent(out)   :: vcres

          ! Locals
          integer(kind=int4) :: i
          ! Start of executable statements
          call v_sincos(vrad,vres)
          ! Attempt to vectorize
          !DIR$     ASSUME_ALIGNED vrad:64
          !DIR$     ASSUME_ALIGNED vres:64
          !DIR$     ASSUME_ALIGNED vcres:64
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, SIZE(vrad)
              vcres(i) = CMPLX(vres(1,i),vres(2,i))
          end do
    end subroutine
    
    
    !======================================================60
    ! subroutine:
    !              vcmag - Magnitude of complex number value.
    !              vector arguments (in),(out)
    ! @Returns: array of scalars of type real(kind=8) which represents
    !           magnitude of complex number
    !======================================================60
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
    subroutine vcmag(vc,vres) !GCC$ ATTRIBUTES hot :: vcmag !GCC$ ATTRIBUTES inline :: vcmag !GCC$ ATTRIBUTES aligned(32) :: vcmag
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: vcmag
    subroutine vcmag(vc,vres)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: vcmag
#endif
          complex(kind=sp), dimension(:), intent(in)  :: vc

          real(kind=sp),    dimension(:), intent(out) :: vres

          ! Locals
          integer(kind=int4) :: i
          real(kind=sp)    :: tmp
          tmp = 0.0_sp
          !DIR$ ASSUME_ALIGNED vc:64
          !DIR$ ASSUME_ALIGNED vres:64
          do i = 1, SIZE(vc)
              tmp = (REAL(vc(i),kind=sp)*REAL(vc(i),kind=sp))+(AIMAG(vc(i),kind=sp)+AIMAG(vc(i),kind=sp))
              vres(i) = SQRT(tmp)
          end do
    end subroutine
    
    !======================================================60
    ! function:
    !              cmag - Magnitude of complex number value.
    !              scalar argument (in)
    ! @Returns: scalar of type real(kind=8) which represents
    !           magnitude of complex number
    !======================================================60

    pure function cmag(c) result(mag)
         
          complex(kind=sp), intent(in) :: c
          ! Locals
          real(kind=sp) :: mag,tmp
          ! Start of executable statements
          tmp = (REAL(c,kind=sp)*REAL(c,kind=sp))+(AIMAG(c,kind=sp)*AIMAG(c,kind=sp))
          mag = SQRT(tmp)
   end function       
   
    
    !======================================================60
    ! function: cnorm - Norm of complex number
    ! @Returns: scalar real valued
    !======================================================60
    pure function cnorm(c) result(norm)
         
          complex(kind=sp), intent(in) :: c
          ! Locals
          real(kind=sp) :: norm
          ! Start of executable statements
          norm = ((REAL(c,kind=sp)*REAL(c,kind=sp))+(AIMAG(c,kind=sp)*AIMAG(c,kind=sp))
    end function
    
    !======================================================60
    ! function: carg
    ! @Returns: ATAN2 of complex number components
    !======================================================60
    pure function carg(c) result(arg)
         
          complex(kind=sp), intent(in) :: c
          ! Locals
          real(kind=sp) :: arg,tre,tim
          ! Start of executable statememtns
          tre = REAL(c,kind=sp)
          tim = AIMAG(c,kind=sp)
          arg = ATAN2(tre,tim)
    end function
    
end module mod_complex_arithm
