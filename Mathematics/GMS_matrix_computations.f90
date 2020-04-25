

module mod_matrix_computations

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_matrix_computations'
 !          
 !          Purpose:
  !                   Helper complex matrix computations
 !          History:
 !                        
 !                          Date: 25-04-2020
  !                         Time: 15:48 GMT+2
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

     use mod_kinds, only : int4,sp
     implicit none
     public
     ! File version info
     ! Major version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MAJOR = 1
     ! Minor version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MINOR = 0
     ! mICRO VERSION
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MICRO = 0
     ! Full version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_FULLVER = &
          1000*MOD_MATRIX_COMPUTATIONS_MAJOR+100*MOD_MATRIX_COMPUTATIONS_MINOR + &
          10*MOD_MATRIX_COMPUTATIONS_MICRO
     ! Module create date
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_CREATE_DATE = "25-04-2020 16:03 +00200 (SAT 25 APR 2020 GMT+2)"
     ! Module build date (set by pre-processor)
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_BUILD_DATE  = __DATE__ " " __TIME__

     ! Module version ID
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_ID = &
                            "$Id: GMS_matrix_computations.f90 1000 +00200 2020-04-25 16:03 beniekg@gmail.com $"

     contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v1(l,q,invq,z,res) !GCC$ ATTRIBUTES hot :: expmat4x4_cr4v1 !GCC$ ATTRIBUTES aligned(16) :: expmat4x4_cr4v1
#elif defined __ICC || defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v1(l,q,invq,z,res)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: expmat4x4_cr4v1
#endif
           complex(kind=sp),    dimension(4),    intent(in)    :: l
           complex(kind=sp),    dimension(4,4),  intent(in)    :: q
           complex(kind=sp),    dimension(4,4),  intent(in)    :: invq
           real(kind=sp),                        intent(in)    :: z
           complex(kind=sp),    dimension(4,4),  intent(inout) :: res
           ! locals
           complex(kind=sp), dimension(4,4) :: det
           integer(kind=int4) :: i,j
           ! Exec code ....
           ! First touch (this loop should be executed from LSD)
           do j=1,4
              do i=1,4
                 det(i,j) = cmplx(0.0_sp,0.0_sp)
              end do
           end do
           do i=1,4
              det(i,i) = cexp(l(i)*z)
           end do
           ! call mat4x4mul
              
       end subroutine expmat4x4_cr4v1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v2(l,q,invq,z,res) !GCC$ ATTRIBUTES hot :: expmat4x4_crv2 !GCC$ ATTRIBUTES aligned(16) :: expmat4x4_cr4v2
#elif defined __ICC || defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v2(l,q,invq,z,res)
           !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: expmat4x4_crv2
#endif 
           complex(kind=sp),    dimension(4),    intent(in)    :: l
           complex(kind=sp),    dimension(4,4),  intent(in)    :: q
           complex(kind=sp),    dimension(4,4),  intent(in)    :: invq
           real(kind=sp),                        intent(in)    :: z
           complex(kind=sp),    dimension(4,4),  intent(inout) :: res
           ! locals
           complex(kind=sp), dimension(4,4) :: det
           real(kind=sp),    dimension(4,4) :: qre
           real(kind=sp),    dimension(4,4) :: qim
           real(kind=sp),    dimension(4,4) :: lre
           real(kind=sp),    dimension(4,4) :: lim
           real(kind=sp),    dimension(4,4) :: iqre
           real(kind=sp),    dimension(4,4) :: iqim
           real(kind=sp),    dimension(4,4) :: FRre
           real(kind=sp),    dimension(4,4) :: FRim
           real(kind=sp),    dimension(4,4) :: FIre
           real(kind=sp),    dimension(4,4) :: FIim
           real(kind=sp),    dimension(4,4) :: diff
           real(kind=sp),    dimension(4,4) :: sum
           integer(kind=int4) :: j,k,i
           ! Exec code ....
           do j=1,4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif
              do i=1,4
                 det(i,j)  = cmplx(0.0_sp,0.0_sp)
                 lre(i,j)  = 0.0_sp
                 lim(i,j)  = 0.0_sp
                 qre(i,j)  = real(q(i,j),kind=sp)
                 qim(i,j)  = aimag(q(i,j),kind=sp)
                 iqre(i,j) = real(invq(i,j),kind=sp)
                 iqim(i,j) = aimag(invq(i,j),kind=sp)
              end do
           end do
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif           
           do i=1, 4
              det(i,i) = cexp(l(i)*z)
              lre(i,i) = real(det(i,i),kind=sp)
              lim(i,i) = aimag(det(i,i),kind=sp)
           end do
           do j=1, 4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif                
              do i=1, 4
                 FRre(i,j) = iqre(i,j)*lre(i,j)
                 FIre(i,j) = iqim(i,j)*lre(i,j)
                 FRim(i,j) = iqre(i,j)*lim(i,j)
                 FIim(i,j) = iqim(i,j)*lim(i,j)
              end do
           end do

           do j=1, 4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif  
              do i=1, 4
                 diff(i,j) = FRre(i,j)-FIim(i,j)
                 sum(i,j)  = FRim(i,j)+FIre(i,j)
              end do
           end do
           
       end subroutine expmat4x4_crv2
       


        














end module mod_matrix_computations
