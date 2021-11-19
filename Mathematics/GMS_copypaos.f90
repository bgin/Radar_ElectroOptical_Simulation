



module mod_copypaos

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_copypaos'
 !          
 !          Purpose:
 !                      1D array of scalar primitive types to PAOS array copy subroutines.
 !          History:
 !                        
 !                        Date: 08-11-2018
 !                        Time: 18:54 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,     only : i1,i4, sp, dp, ep
     use mod_vecconsts, only : v8_n0, v4_n0
                              
     implicit none
     public
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MOD_COPYPAOS_MAJOR = 1
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_MINOR = 0
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_MICRO = 0
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_FULLVER = 1000*MOD_COPYPAOS_MAJOR+   &
                                                                    100*MOD_COPYPAOS_MINOR+    &
                                                                    10*MOD_COPYPAOS_MICRO
    
    ! Module creation date 
    character(*),       parameter, public :: MOD_COPYPAOS_CREATE_DATE = "08-11-2018 19:02 +00200 (THU 08 NOV 2018 GMT+2) " 
    
    ! Module build date
    character(*),       parameter, public :: MOD_COPYPAOS_BUILD_DATE =  __DATE__ " " __TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_COPYPAOS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_COPYPAOS_SYNOPSIS = " 1D array of scalar primitive types to PAOS array copy subroutines."
    
    ! Module parameters
    
    integer(kind=i4), parameter, private :: v2len  = 2
    
    integer(kind=i4), parameter, private :: v3len  = 3
    
    integer(kind=i4), parameter, private :: v4len  = 4
    
    integer(kind=i4), parameter, private :: v8len  = 8
    
    integer(kind=i4), parameter, private :: v16len = 16
    
    contains
    
    ! Fast copy without arrays size conformance checking
    ! Hope it will enable auto-vectorization of this subroutine.
      ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
      subroutine copy_r8_xmm2r8(output,input) !GCC$ ATTRIBUTES cold :: copy_r8_xmm2r8  !GCC$ ATTRIBUTES always_inline :: copy_r8_xmm2r8 !GCC$ ATTRIBUTES align(32) :: copy_r8_xmm2r8
#elif defined __INTEL_COMPILER
      subroutine copy_r8_xmm2r8(output,input)

        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_xmm2r8
        !DIR$ ATTRIBUTES VECTOR :: copy_r8_xmm2r8
#endif
          use mod_vectypes, only : XMM2r8_t
          type(XMM2r8_t),  contiguous,   dimension(:),   intent(inout) :: output  
          real(kind=dp),   contiguous,   dimension(:),   intent(in)    :: input    
          ! Locals
          integer(kind=i4),automatic :: i
          integer(kind=i4),automatic :: j
             
              
       
          ! Exec code ....
         
          do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 1
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED input:64
                !DIR$ ASSUME_ALIGNED output:64
#endif
                  output(i).v(j) = input(i*v2len+j) 
              end do
          end do
    end subroutine copy_r8_xmm2r8
    
    ! Fast copy without arrays size conformance checking
    ! Hope it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm2r8_r8(output,inout) !GCC$ ATTRIBUTES cold :: copy_xmm2r8_r8 !GCC$ ATTRIBUTES always_inline :: copy_xmm2r8_r8 !GCC$ ATTRIBUTES align(32) :: copy_xmm2r8_r8
#elif defined __INTEL_COMPILER
    subroutine copy_xmm2r8_r8(output,input)

      !DIR$ ATTRIBUTES VECTOR :: copy_xmm2r8_r8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm2r8_r8
#endif
    use mod_vectypes,  only : XMM2r8_t
    real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
    type(XMM2r8_t), contiguous, dimension(:), intent(in)    :: input
    ! Locals
   
         integer(kind=i4),automatic :: i
         integer(kind=i4),automatic :: j

    ! Exec code ....
    do i = 0, size(input,dim=1)-1
      
#if defined __INTEL_COMPILER
       !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
       !GCC$ VECTOR
#endif
       do j = 0, 1
#if defined __INTEL_COMPILER
          !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
            output(i*v2len+j) = input(i).v(j)
        end do
    end do
    end subroutine copy_xmm2r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_i4_xmm2i4(output,input) !GCC$ ATTRIBUTES cold :: copy_i4_xmm2i4 !GCC$ ATTRIBUTES always_inline :: copy_i4_xmm2i4 !GCC$ ATTRIBUTES align(32) :: copy_i4_xmm2i4
#elif defined __INTEL_COMPILER
    subroutine copy_i4_xmm2i4(output,input)

      !DIR$ ATTRIBUTES VECTOR :: copy_i4_xmm2i4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_i4_xmm2i4
#endif
          use mod_vectypes, only : XMM2i4_t
          type(XMM2i4_t),     contiguous,  dimension(:),   intent(inout) :: output
          integer(kind=int4), contiguous, dimension(:),   intent(in)    :: input
          ! Locals
         
              integer(kind=i4) :: i
              integer(kind=i4) :: j
         


          ! Exec code ....
          do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER             
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 3
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64
                !DIR$ ASSUME_ALIGNED input:64
#endif
                  output(i).v(j) = input(i*v4len+j)
              end do
          end do
    end subroutine copy_i4_xmm2i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm2i4_i4(output,input) !GCC$ ATTRIBUTES cold :: copy_xmm2i4_i4 !GCC$ ATTRIBUTES always_inline :: copy_xmm2i4_i4 !GCC$ ATTRIBUTES align(32) :: copy_xmm2i4_i4
#elif defined __INTEL_COMPILER
    subroutine copy_xmm2i4_i4(output,input)

       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm2i4_i4
       !DIR$  ATTRIBUTES VECTOR :: copy_xmm2i4_i4
#endif
          use mod_vectypes,  only : XMM2i4_t
          integer(kind=i4), contiguous, dimension(:), intent(inout) :: output
          type(XMM2i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
        
           
             integer(kind=i4) :: i
             integer(kind=i4) :: j

          ! Exec code ...
          do i = 0, size(input,dim=1)-1
             
#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 1
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                  output(i*v2len+j) = input(i).v(j)
              end do
           end do
    end subroutine copy_xmm2i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r8_ymm3r8(output,input) !GCC$ ATTRIBUTES cold :: copy_r8_ymm3r8 !GCC$ ATTRIBUTES always_inline :: copy_r8_ymm3r8 !GCC$ ATTRIBUTES aligned(32) :: copy_r8_ymm3r8
#elif defined __INTEL_COMPILER
    subroutine copy_r8_ymm3r8(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_ymm3r8
!DIR$ ATTRIBUTES VECTOR :: copy_r8_ymm3r8
#endif
          use mod_vectypes, only : YMM3r8_t
          type(YMM3r8_t),  contiguous,       dimension(:),  intent(inout) :: output
          real(kind=dp),   contiguous,       dimension(:),  intent(in)    :: input
          ! Locals
         
               
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ....
         do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER             
            !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
            !GCC$ VECTOR
#endif
            do j = 0, 2
#if defined __INTEL_COMPILER
               !DIR$ ASSUME_ALIGNED output:64
               !DIR$ ASSUME_ALIGNED input:64
#endif
                  output(i).v(j) = input(i*v3len+j)
              end do
          end do
    end subroutine copy_r8_ymm3r8  
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_ymm3r8_r8(output,input) !GCC$ ATTRIBUTES cold :: copy_ymm3r8_r8 !GCC$ ATTRIBUTES always_inline :: copy_ymm3r8_r8 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm3r8_r8
#elif defined __INTEL_COMPILER
    subroutine copy_ymm3r8_r8(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_ymm3r8_r8
!DIR$ ATTRIBUTES VECTOR :: copy_ymm3r8_r8
#endif
          use mod_vectypes,  only : YMM3r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(YMM3r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
        
               integer(kind=i4) :: i
               integer(kind=i4) :: j

          ! Exec code ....
          do i = 0, size(input,dim=1)-1
           
#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 2
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64,input:64
#endif                
                  output(i*v3len+j) = input(i).v(j)
              end do
          end do
    end subroutine copy_ymm3r8_r8
    
   
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_i4_xmm3i4(output,input) !GCC$ ATTRIBUTES cold :: copy_i4_xmm3i4 !GCC$ ATTRIBUTES always_inline :: copy_i4_xmm3i4 !GCC$ ATTRIBUTES aligned(32) :: copy_i4_xmm3i4
#elif defined __INTEL_COMPILER
    subroutine copy_i4_xmm3i4(output,input)
!
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_i4_xmm3i4
      !DIR$ ATTRIBUTES  VECTOR :: copy_i4_xmm3i4
#endif
          use mod_vectypes, only : XMM3i4_t
          type(XMM3i4_t),     contiguous,  dimension(:),   intent(inout) :: output
          integer(kind=i4), contiguous,  dimension(:),   intent(in)    :: input
          ! Locals
         
              
               integer(kind=i4) :: i
               integer(kind=i4) :: j
        
          ! Exec code ...
          do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER              
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 2
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED input:64
                !DIR$ ASSUME_ALIGNED output:64
#endif
                  output(i).v(j) = input(i*v3len+j)
              end do
          enddo
    end subroutine copy_i4_xmm3i4  
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm3i4_i4(output,input) !GCC$ ATTRIBUTES cold :: copy_xmm3i4_i4 !GCC$ ATTRIBUTES always_inline :: copy_xmm3i4_i4 !GCC$ ATTRIBUTES aligned(32) :: copy_xmm3i4_i4
#elif defined __INTEL_COMPILER
    subroutine copy_xmm3i4_i4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm3i4_i4
!DIR$ ATTRIBUTES VECTOR :: copy_xmm3i4_i4
#endif
          use mod_vectypes, only : XMM3i4_t
          integer(kind=i4), contiguous, dimension(:), intent(inout) :: output
          type(XMM3i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
             
               integer(kind=i4) :: i
               integer(kind=i4) :: j

          ! Exec code ....
          do i = 0, size(input,dim=1)-1
            
#if defined __INTEL_COMPILER
             !DIR$         VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 2
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED input:64,output:64
#endif
                  output(i*v3len+j) = input(i).v(j)
              end do
          end do
    end subroutine copy_xmm3i4_i4
    
   
   
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r8_ymm4r8(output,input) !GCC$ ATTRIBUTES cold :: copy_r8_ymm4r8 !GCC$ ATTRIBUTES always_inline :: copy_r8_ymm4r8 !GCC$ ATTRIBUTES aligned(32) :: copy_r8_ymm4r8
#elif defined __INTEL_COMPILER
    subroutine copy_r8_ymm4r8(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_ymm4r8
!DIR$ ATTRIBUTES VECTOR :: copy_r8_ymm4r8
#endif
          use mod_vectypes, only :  YMM4r8_t
          type(YMM4r8_t),  contiguous,      dimension(:),   intent(inout) :: output
          real(kind=dp),   contiguous,      dimension(:),   intent(in)    :: input
          ! Locals
        
              
               integer(kind=i4) :: i
               integer(kind=i4) :: j
        
          ! Exec code ....
         do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
            !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
            !GCC$ VECTOR
#endif
            do j = 0, 3
#if defined __INTEL_COMPILER
               !DIR$ ASSUME_ALIGNED output:64
               !DIR$ ASSUME_ALIGNED input:64
#endif
                  output(i).v(j) = input(i*v4len+j)
              end do
          end do
    end subroutine copy_r8_ymm4r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_ymm4r8_r8(output,input) !GCC$ ATTRIBUTES cold :: copy_ymm4r8_r8 !GCC$ ATTRIBUTES always_inline :: copy_ymm4r8 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm4r8_r8
#elif defined __INTEL_COMPILER
    subroutine copy_ymm4r8_r8(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_ymm4r8_r8
!DIR$ ATTRIBUTES VECTOR :: copy_ymm4r8_r8
#endif
          use mod_vectypes,  only : YMM4r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(YMM4r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
       
               integer(kind=i4) :: i
               integer(kind=i4) :: j

          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 3
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                  output(i*v4len+j) = input(i).v(j)
              end do
          end do
    end subroutine copy_ymm4r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_i4_xmm4i4(output,input) !GCC$ ATTRIBUTES cold :: copy_i4_xmm4i4 !GCC$ ATTRIBUTES always_inline :: copy_i4_xmm4i4 !GCC$ ATTRIBUTES aligned(32) :: copy_i4_xmm4i4
#elif defined __INTEL_COMPILER
    subroutine copy_i4_xmm4i4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_i4_xmm4i4
!DIR$ ATTRIBUTES VECTOR :: copy_i4_xmm4i4
#endif
          use mod_vectypes, only : XMM4i4_t
          type(XMM4i4_t),     contiguous,   dimension(:), intent(inout) :: output
          integer(kind=i4), contiguous,  dimension(:), intent(in)    :: input
          ! Locals
        
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ....
          do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER                 
             !DIR$  VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 3
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64
                !DIR$ ASSUME_ALIGNED input:64
#endif
                   output(i).v(j) = input(i*v4len+j)
             end do
          end do
    end subroutine copy_i4_xmm4i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm4i4_i4(output,input) !GCC$ ATTRIBUTES cold :: copy_xmm4i4_i4 !GCC$ ATTRIBUTES always_inline :: copy_xmm4i4_i4 !GCC$ ATTRIBUTES aligned(32) :: copy_xmm4i4_i4
#elif defined __INTEL_COMPILER
    subroutine copy_xmm4i4_i4(output,input)

!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm4i4_i4
!DIR$  ATTRIBUTES VECTOR :: copy_xmm4i4_i4
#endif
          use mod_vectypes, only : XMM4i4_t
          integer(kind=i4), contiguous, dimension(:), intent(inout) :: output
          type(XMM4i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j

          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$        VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                     do j = 0,  3
#if defined __INTEL_COMPILER
                        !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                         output(i*v4len+j) = input(i).v(j)
                     end do
          end do
    end subroutine copy_xmm4i4_i4
   
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_i4_ymm8i4(output,input) !GCC$ ATTRIBUTES cold :: copy_i4_ymm8i4 !GCC$ ATTRIBUTES always_inline :: copy_i4_ymm8i4 !GCC$ ATTRIBUTES aligned(32) :: copy_i4_ymm8i4
#elif defined __INTEL_COMPILER
    subroutine copy_i4_ymm8i4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_i4_ymm8i4
!DIR$ ATTRIBUTES VECTOR :: copy_i4_ymm8i4
#endif
          use mod_vectypes, only : YMM8i4_t
          type(YMM8i4_t),     contiguous,   dimension(:), intent(inout) :: output
          integer(kind=i4), contiguous,   dimension(:), intent(in)    :: input
          ! Locals
       
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ....
               do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
                  !DIR$  VECTOR ALWAYS
#elif defined __GFORTRAN__
                  !GCC$ VECTOR
#endif
                  do j = 0, 7
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif
                      output(i).v(j) = input(i*v8len+j)
                 end do
          end do
    end subroutine copy_i4_ymm8i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_ymm8i4_i4(output,input) !GCC$ ATTRIBUTES cold :: copy_ymm8i4_i4 !GCC$ ATTRIBUTES always_inline :: copy_ymm8i4_i4 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm8i4_i4
#elif defined __INTEL_COMPILER
    subroutine copy_ymm8i4_i4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_ymm8i4_i4
!DIR$ ATTRIBUTES VECTOR :: copy_ymm8i4_i4
#endif
          use mod_vectypes, only : YMM8i4_t
          integer(kind=i4), contiguous, dimension(:), intent(inout) :: output
          type(YMM8i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
             do j = 0, 7
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                  output(i*v8len+j) = input(i).v(j)
              end do
          end do
    end subroutine copy_ymm8i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_i4_zmm16i4(output,input) !GCC$ ATTRIBUTES cold :: copy_i4_zmm16i4 !GCC$ ATTRIBUTES always_inline :: copy_i4_zmm16i4 !GCC$ ATTRIBUTES aligned(32) :: copy_i4_zmm16i4
#elif defined __INTEL_COMPILER
    subroutine copy_i4_zmm16i4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_i4_zmm16i4
!DIR$ ATTRIBUTES VECTOR :: copy_i4_zmm16i4
#endif
          use mod_vectypes,  only : ZMM16i4_t
          type(ZMM16i4_t),    contiguous, dimension(:), intent(inout) :: output
          integer(kind=i4), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ...
           do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER                  
              !DIR$  VECTOR ALWAYS
#elif defined __GFORTRAN__
              !GCC$ VECTOR
#endif
                do j = 0, 15
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED output:64
                   !DIR$ ASSUME_ALIGNED input:64
#endif
                     output(i).v(j) = input(i*v16len+j)
                end do
          end do
    end subroutine copy_i4_zmm16i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_zmm16i4_i4(output,input) !GCC$ ATTRIBUTES cold :: copy_zmm16i4_i4 !GCC$ ATTRIBUTES always_inline :: copy_zmm16i4_i4 !GCC$ ATTRIBUTES aligned(32) :: copy_zmm16i4_i4
#elif defined __INTEL_COMPILER
    subroutine copy_zmm16i4_i4(output,input)

!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_zmm16i4_i4
!DIR$  ATTRIBUTES VECTOR :: copy_zmm16i4_i4
#endif
          use mod_vectypes, only : ZMM16i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(ZMM16i4_t),    contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                  do j = 0, 15
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                      output(i*v16len+j) = input.v(j)
                  end do
          end do
    end subroutine copy_zmm16i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r8_zmm8r8(output,input) !GCC$ ATTRIBUTES cold :: copy_r8_zmm8r8 !GCC$ ATTRIBUTES always_inline :: copy_r8_zmm8r8 !GCC$ ATTRIBUTES aligned(32) :: copy_r8_zmm8r8
#elif defined __INTEL_COMPILER
    subroutine copy_r8_zmm8r8(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_zmm8r8
!DIR$ ATTRIBUTES VECTOR :: copy_r8_zmm8r8
#endif
          use mod_vectypes, only : ZMM8r8_t
          type(ZMM8r8_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ...
               do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
                  !DIR$  VECTOR ALWAYS
#elif defined __GFORTRAN__
                  !GCC$ VECTOR
#endif
                  do j = 0, 7
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif
                     output(i).v(j) = input(i*v8len+j)
                 end do
          end do
    end subroutine copy_r8_zmm8r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_zmm8r8_r8(output,input) !GCC$ ATTRIBUTES cold :: copy_zmm8r8_r8 !GCC$ ATTRIBUTES always_inline :: copy_zmm8r8_r8 !GCC$ ATTRIBUTES aligned(32) :: copy_zmm8r8_r8
#elif defined __INTEL_COMPILER
    subroutine copy_zmm8r8_r8(output,input)

!DIR$   ATTRIBUTES CODE_ALIGN : 32 :: copy_zmm8r8_r8
!DIR$   ATTRIBUTES VECTOR :: copy_zmm8r8_r8
#endif
          use mod_vectypes,  only : ZMM8r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(ZMM8r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
        
               integer(kind=i4) :: i
               integer(kind=i4) :: j


          ! Exec code .....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                    do j = 0, 7
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                        output(i*v8len+j) = input(i).v(j)
                    end do
          end do
    end subroutine copy_zmm8r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r4_xmm2r4(output,input) !GCC$ ATTRIBUTES cold :: copy_r4_xmm2r4 !GCC$ ATTRIBUTES always_inline :: copy_r4_xmm2r4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_xmm2r4
#elif defined __GFORTRAN__
    subroutine copy_r4_xmm2r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_xmm2r4
!DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm2r4
#endif
          use mod_vectypes,  only : XMM2r4_t
          type(XMM2r4_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=sp),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         
          ! Exec code ....
               do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
                  !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
                  !GCC$ VECTOR
#endif
                  do j = 0, 1
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif                     
                     output(i).v(j) = input(i*v2len+j)
                 end do
          end do
    end subroutine copy_r4_xmm2r4
     
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm2r4_r4(output,input) !GCC$ ATTRIBUTES cold :: copy_xmm2r4_r4 !GCC$ ATTRIBUTES always_inline :: copy_xmm2r4_r4 !GCC$ ATTRIBUTES aligned(32) :: copy_xmm2r4_r4
#elif defined __INTEL_COMPILER
    subroutine copy_xmm2r4_r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm2r4_r4
!DIR$ ATTRIBUTES VECTOR :: copy_xmm2r4_r4
#endif
          use mod_vectypes, only : XMM2r4_t
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output
          type(XMM2r4_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
        
               integer(kind=i4) :: i
               integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                    do j = 0, 1
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                        output(i*v2len+j) = input(i).v(j)
                    end do
          end do
    end subroutine copy_xmm2r4_r4
    
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r4_xmm3r4(output,input) !GCC$ ATTRIBUTES cold :: copy_r4_xmm3r4 !GCC$ ATTRIBUTES always_inline :: copy_r4_xmm3r4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_xmm3r4
#elif defined __INTEL_COMPILER
    subroutine copy_r4_xmm3r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_xmm3r4
      !DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm3r4
#endif
          use mod_vectypes, only :  XMM3r4_t
          type(XMM3r4_t),  contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),   contiguous,  dimension(:), intent(in)    :: input
         ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j
         

         ! Exec code ....
         do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER             
            !DIR$    VECTOR ALWAYS
#elif defined __GFORTRAN__
            !GCC$ VECTOR
#endif
               do j = 0, 2
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif                  
                   output(i).v(j) = input(i+v3len*j)
              end do
    end do
    end subroutine copy_r4_xmm3r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_xmm3r4_r4(output,input) !GCC$ ATTRIBUTES cold :: copy_xmm3r4_r4 !GCC$ ATTRIBUTES always_inline :: copy_xmm3r4_r4 !GCC$ ATTRIBUTES aligned(32) :: copy_xmm3r4_r4
#elif defined __GFORTRAN__
    subroutine copy_xmm3r4_r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm3r4_r4
      !DIR$ ATTRIBUTES VECTOR :: copy_xmm3r4_r4
#endif
          use mod_vectypes, only : XMM3r4_t
          real(kind=sp),   contiguous,  dimension(:), intent(inout)    :: output
          type(XMM3r4_t),  contiguous,  dimension(:), intent(in)       :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j


          ! Exec code .....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                 do j = 0,  2
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output:64,input:64
#endif                
                     output(i*v3len+j) = input(i).v(j)
                 end do
          end do
    end subroutine copy_xmm3r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r4_xmm4r4(output,input) !GCC$ ATTRIBUTES cold :: copy_r4_xmm4r4 !GCC$ ATTRIBUTES always_inline :: copy_r4_xmm4r4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_xmm4r4
#elif defined __INTEL_COMPILER
    subroutine copy_r4_xmm4r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_xmm4r4
      !DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm4r4
#endif
          use mod_vectypes, only :  XMM4r4_t
          type(XMM4r4_t),     contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),      contiguous,  dimension(:), intent(in)    :: input
          ! Locals
         
               integer(kind=i4) :: i
               integer(kind=i4) :: j
                 
          ! Exec code ....
         do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
            !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
            !GCC$ VECTOR
#endif
              do j = 0, 3
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif                 
                   output(i).v(j) = input(i*v4len+j)
              end do
           end do
    end subroutine copy_r4_xmm4r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
    subroutine copy_xmm4r4_r4(output,input)
#if defined __INTEL_COMPILER
!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm4r4_r4
      !DIR$  ATTRIBUTES VECTOR :: copy_xmm4r4_r4
#endif
           use mod_vectypes, only :  XMM4r4_t
           real(kind=sp),   contiguous, dimension(:), intent(inout) :: output
           type(XMM4r4_t),  contiguous, dimension(:), intent(in)    :: input
           ! Locals
            
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
#if defined __INTEL_COMPILER           
                 !DIR$     ATTRIBUTES ALIGN : 64 :: tmp
#endif
            type(XMM4r4_t)  :: tmp

            ! Exec code ....
            do i = 0, size(input,dim=1)-1
#if defined __INTEL_COMPILER
             !DIR$ ASSUME_ALIGNED input:64
#endif                
               tmp = input(i)
#if defined __INTEL_COMPILER               
               !DIR$       VECTOR ALWAYS
#endif
                    do j = 0,  3
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output:64
#endif                     
                        output(i*v3len+j) = tmp.v(j)
                    end do
            end do
    end subroutine copy_xmm4r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r4_ymm8r4(output,input)  !GCC$ ATTRIBUTES cold :: copy_r4_ymm8r4 !GCC$ ATTRIBUTES always_inline :: copy_r4_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm8r4
#elif defined __GFORTRAN__
    subroutine copy_r4_ymm8r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_ymm8r4
      !DIR$ ATTRIBUTES VECTOR :: copy_r4_ymm8r4
#endif
          use mod_vectypes,  only : YMM8r4_t
          type(YMM8r4_t),    contiguous, dimension(:), intent(inout) :: output
          real(kind=sp),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          !Exec code ....
            do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
               !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
               !GCC$ VECTOR
#endif
                  do j = 0, 7
#if defined __INTEL_COMPILER
                     !DIR$ ASSUME_ALIGNED output:64
                     !DIR$ ASSUME_ALIGNED input:64
#endif                   
                    output(i).v(j) = input(i*v8len+j)
                  end do
          end do
    end subroutine copy_r4_ymm8r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_ymm8r4_r4(output,input) !GCC$ ATTRIBUTES cold :: copy_ymm8r4_r4 !GCC$ ATTRIBUTES always_inline :: copy_ymm8r4_r4 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm8r4_r4
#elif defined __INTEL_COMPILER
    subroutine copy_ymm8r4_r4(output,input)

!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_ymm8r8_r4
      !DIR$  ATTRIBUTES VECTOR  :: copy_ymm8r4_r4
#endif
          use mod_vectypes,  only : YMM8r4_t
          real(kind=sp),    contiguous, dimension(:), intent(inout) :: output
          type(YMM8r4_t),   contiguous, dimension(:), intent(in)    :: input
          ! Locals
          
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$         VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                    do j = 0,  7
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output:64,input:64
#endif                  
                        output(i*v8len+j) = input(i).v(j)
                    end do
          end do
    end subroutine copy_ymm8r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_r4_zmm16r4(output,input) !GCC$ ATTRIBUTES cold :: copy_r4_zmm16r4 !GCC$ ATTRIBUTES always_inline :: copy_r4_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_zmm16r4
#elif defined __INTEL_COMPILER
    subroutine copy_r4_zmm16r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_zmm16r4
!DIR$ ATTRIBUTES VECTOR :: copy_r4_zmm16r4
#endif
          use mod_vectypes, only : ZMM16r4_t
          type(ZMM16r4_t),  contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),    contiguous,  dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          ! Exec code ....
          do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
             !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                do j = 0, 15
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                    output(i).v(j) = input(i*v16len+j)
                end do
           end do
    end subroutine copy_r4_zmm16r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
#if defined __GFORTRAN__
    subroutine copy_zmm16r4_r4(output,input) !GCC$ ATTRIBUTES cold :: copy_zmm16r4_r4 !GCC$ ATTRIBUTES always_inline :: copy_zmm16r4_r4 !GCC$ ATTRIBUTES aligned(32) :: copy_zmm16r4_r4
#elif defined __INTEL_COMPILER
    subroutine copy_zmm16r4_r4(output,input)

!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_zmm16r4_r4
      !DIR$ ATTRIBUTES VECTOR :: copy_zmm16r4_r4
#endif
          use mod_vectypes, only : ZMM16r4_t
          real(kind=sp),    contiguous, dimension(:), intent(inout) :: output
          type(ZMM16r4_t),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j

          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                do j = 0, 15
#if defined __INTEL_COMPILER
                !DIR$ ASSUME_ALIGNED output:64,input:64
#endif
                    output(i*v16len+j) = input(i).v(j)
                end do
          end do
    end subroutine copy_zmm16r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part
#if defined __GFORTRAN__
    subroutine copy_r4_xmm2c4(output,input1,input2) !GCC$ ATTRIBUTES cold :: copy_r4_xmm2c4 !GCC$ ATTRIBUTES always_inline :: copy_r4_xmm2c4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_xmm2c4
#elif defined __INTEL_COMPILER
    subroutine copy_r4_xmm2c4(output,input1,input2)

      !DIR$  ATTRIBUTES VECTOR :: copy_r4_xmm2c4
      !DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_r4_xmm2c4
#endif
          use mod_vectypes, only :  XMM2c4_t
          type(XMM2c4_t),            dimension(:), intent(inout) :: output
          real(kind=sp), contiguous, dimension(:), intent(in)    :: input1
          real(kind=sp), contiguous, dimension(:), intent(in)    :: input2
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          ! Exec code ....
           do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
              !DIR$  VECTOR ALWAYS
#elif defined __GFORTRAN__
              !GCC$ VECTOR
#endif
              do j = 0,  1
#if defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED input1:64,input2:64,output:64
#endif
                      output(i).re(j) = input1(i*v2len+j)
                      output(i).im(j) = input2(i*v2len+j)
                 end do
          end do
    end subroutine copy_r4_xmm2c4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part
#if defined __GFORTRAN__
    subroutine copy_xmm2c4_r4(output1,output2,input) !GCC$ ATTRIBUTES cold :: copy_xmm2c4_r4 !GCC$ ATTRIBUTES always_inline :: copy_xmm2c4_r4 !GCC$ ATTRIBUTES aligned(32) :: copy_xmm2c4_r4
#elif defined __INTEL_COMPILER
    subroutine copy_xmm2c4_r4(output1,output2,input)

!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_xmm2c4_r4
      !DIR$  ATTRIBUTES VECTOR :: copy_xmm2c4_r4
#endif
          use mod_vectypes, only : XMM2c4_t
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output1
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output2
          type(XMM2c4_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                do j = 0,  1
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED output1:64,output2:64,input:64
#endif
                        output1(i*v2len+j) = input(i).re(j)
                        output2(i*v2len+j) = input(i).im(j)
                end do
           end do
    end subroutine copy_xmm2c4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_r8_ymm4c8(output,input1,input2)
#if defined __INTEL_COMPILER
!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_ymm4c8
      !DIR$  ATTRIBUTES VECTOR :: copy_r8_ymm4c8
#endif
          use mod_vectypes, only :  YMM4c8_t
          type(YMM4c8_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input1
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input2
          ! Locals
          
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          ! Exec code ....
           do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER     
              !DIR$ VECTOR ALWAYS
#endif
               do j = 0, 3
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED input1:64,input2:64,output:64
#endif                   
                    output(i).re(j) = input1(i*v4len+j)
                    output(i).im(j) = input2(i*v4len+j)
               end do
          end do
    end subroutine copy_r8_ymm4c8
    ! Description:  same as above
#if defined __GFORTRAN__
    subroutine copy_r4_ymm8c4(output,input1,input2) !GCC$ ATTRIBUTES cold :: copy_r4_ymm8c4 !GCC$ ATTRIBUTES always_inline :: copy_r4_ymm8c4 !GCC$ ATTRIBUTES aligned(32) :: copy_r4_ymm8c4
#elif defined __INTEL_COMPILER
    subroutine copy_r4_ymm8c4(output,input1,input2)

!DIR$ ATTRIBUTES VECTOR :: copy_r4_ymm8c4
      !DIR$ ATTRIBUTES CODE_ALIGN:32 :: copy_r4_ymm8c4
#endif
      use mod_vectypes, only : YMM8c4_t
      type(YMM8c4_t), contiguous, dimension(:), intent(inout) :: output
      real(kind=sp),  contiguous, dimension(:), intent(in)    :: input1
      real(kind=sp),  contiguous, dimension(:), intent(in)    :: input2
      !  Locals
      integer(kind=i4), automatic :: i,j
      ! Exec code ...
      do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER
         !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__
         !GCC$ VECTOR
#endif
         do j = 0, 7
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED input1:64,input2:64,output:64
#endif            
            output(i).re(j) = input1(i*vlen8+j)
            output(i).im(j) = input2(i*vlen8+j)
         end do
      end do
    end subroutine copy_r4_ymm8c4
    
     ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
#if defined __GFORTRAN__
    subroutine copy_ymm4c8_r8(output1,output2,input) !GCC$ ATTRIBUTES cold :: copy_ymm4c8_r8 !GCC$ ATTRIBUTES always_inline :: copy_ymm4c8_r8 !GCC$ ATTRIBUTES aligned(32) :: copy_ymm4c8_r4
#elif defined __INTEL_COMPILER
    subroutine copy_ymm4c8_r8(output1,output2,input)

!DIR$   ATTRIBUTES CODE_ALIGN : 32 :: copy_ymm4c8_r8
      !DIR$   ATTRIBUTES VECTOR :: copy_ymm4c8_r8
#endif
          use mod_vectypes,  only : YMM4c8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output1
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output2
          type(YMM4c8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j


          ! Exec code ....
          do i = 0, size(input,dim=1)-1

#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#elif defined __GFORTRAN__
             !GCC$ VECTOR
#endif
                    do j = 0,  3
#if defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED output1:64,output2:64,input:64
#endif                
                        output1(i*v4len+j) = input.re(j)
                        output2(i*v4len+j) = input.im(j)
                    end do
           end do
     end subroutine copy_ymm4c8_r8

     subroutine copy_ymm8c4_r4(output1,output2,input)
#if defined __INTEL_COMPILER
!DIR$ ATTRIBUTES VECTOR :: copy_ymm8c4_r4
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: copy_ymm8c4_r4
#endif
           use mod_vectypes, only : YMM8c4_t
           real(kind=sp),  contiguous, dimension(:), intent(inout) :: output1
           real(kind=sp),  contiguous, dimension(:), intent(inout) :: output2
           type(YMM8c4_t), contiguous, dimension(:), intent(in)    :: input
           ! Locals
           integer(kind=i4), automatic :: i,j
#if defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 32 :: tmp
#endif
           type(YMM8c4_t), automatic :: tmp
           ! Exec code .....
           do i=0, size(input,dim=1)-1
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED input:32
#endif
              tmp = input(i)
#if defined __INTEL_COMPILER
              !DIR$   VECTOR ALWAYS
#endif
              do j=0, 7
#if defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED output1:64,output2:64
#endif
                 output1(i*vlen8+j) = tmp.re(j)
                 output2(i*vlen8+j) = tmp.im(j)
              end do
           end do
     end subroutine copy_ymm8c4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 8*size(output,dim=1) == size(input,dim=1)   -- imaginary part
     subroutine copy_r8_zmm8c8(output,input1,input2)
#if defined __INTEL_COMPILER
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_r8_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: copy_r8_zmm8c8
#endif
          use mod_vectypes,  only : ZMM8c8_t
          type(ZMM8c8_t), contiguous,     dimension(:),   intent(inout)   :: output
          real(kind=dp),  contiguous,     dimension(:),   intent(in)      :: input1
          real(kind=dp),  contiguous,     dimension(:),   intent(in)      :: input2
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          ! Exec code ....
           do i = 0, size(output,dim=1)-1
#if defined __INTEL_COMPILER                   
              !DIR$ VECTOR ALWAYS
#endif
                do j = 0, 7
#if defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED input1:64,input2:64,output:64
#endif
                     output(i).re(j) = input1(i*v8len+j)
                     output(i).im(j) = input2(i*v8len+j)
                end do
         end do
    end subroutine copy_r8_zmm8c8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 8*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_zmm8c8_r8(output1,output2,input)
#if defined __INTEL_COMPILER
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_zmm8c8_r8
      !DIR$ ATTRIBUTES VECTOR :: copy_zmm8c8_r8
#endif
          use mod_vectypes,  only : ZMM8c8_t
          real(kind=dp),    contiguous, dimension(:), intent(inout) :: output1
          real(kind=dp),    contiguous, dimension(:), intent(inout) :: output2
          type(ZMM8c8_t),   contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
#if defined __INTEL_COMPILER         
                 !DIR$     ATTRIBUTES ALIGN : 64 :: tmp
#endif
          type(ZMM8c8_t)  :: tmp

          ! Exec code ...
          do i = 0, size(input,dim=1)-1
#if defined __INTEL_COMPILER
             !DIR$ ASSUME_ALIGNED input:64
#endif
             tmp = input(i)
#if defined __INTEL_COMPILER
             !DIR$     VECTOR ALWAYS
#endif
                    do j = 0,  7
#if defined __INTEL_COMPILER
                       !DIR$ ASSUME_ALIGNED output1:64,output2:64
#endif
                        output1(i*v8len+j) = tmp.re(j)
                        output2(i*v8len+j) = tmp.im(j)
                    end do
          end do
    end subroutine copy_zmm8c8_r8
    
    ! Fast copy without arrays size conformance checking
    
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part

    subroutine copy_r16_ymm2c16(output,input1,input2)

          use mod_vectypes,  only : YMM2c16_t
          type(YMM2c16_t),  contiguous, dimension(:), intent(inout) :: output
          real(kind=ep),    contiguous, dimension(:), intent(in)    :: input1
          real(kind=ep),    contiguous, dimension(:), intent(in)    :: input2
          ! Locals
        
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
          ! Exec code ...
          do i = 0, size(output,dim=1)-1
                 do j = 0,  1
                     output(i).re(j) = input1(i*v2len+j)
                     output(i).im(j) = input2(i*v2len+j)
                 end do
          end do
    end subroutine 
    
    ! Fast copy without arrays size conformance checking
    
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_zmm2c16_r16(output1,output2,input)
          use mod_vectypes,  only : ZMM2c16_t
          real(kind=ep),    contiguous, dimension(:), intent(inout) :: output1
          real(kind=ep),    contiguous, dimension(:), intent(inout) :: output2
          type(ZMM2c16_t),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
          
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
        
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM2c16_t)  :: tmp

          ! Exec code ....
          do i = 0, size(input,dim=1)-1
                 tmp = input(i)
                 do j = 0,  1
                      output1(i*v2len+j) = tmp.re(j)
                      output2(i*v2len+j) = tmp.im(j)
                 end do
          end do
    end subroutine copy_zmm2c16_r16
    
    ! Fast copy without arrays size conformance checking
    
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_r16_zmm4c16(output,input1,input2)
          use mod_vectypes,  only : ZMM4c16_t
          type(ZMM4c16_t),   contiguous, dimension(:), intent(inout) :: output
          real(kind=ep),     contiguous, dimension(:), intent(in)    :: input1
          real(kind=ep),     contiguous, dimension(:), intent(in)    :: input2
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
          
          ! Exec code ....
          do i = 0,  size(input1,dim=1)-1
              do j = 0, 3
                  output(i).re(j) = input1(i*v4len+j)
                  output(i).im(j) = input2(i*v4len+j)
              end do
          end do
    end subroutine copy_r16_zmm4c16
    
    ! Fast copy without arrays size conformance checking
    
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_zmm4c16_r16(output1,output2,input)
          use mod_vectypes,  only : ZMM4c16_t
          real(kind=ep),   contiguous, dimension(:), intent(inout) :: output1
          real(kind=ep),   contiguous, dimension(:), intent(inout) :: output2
          type(ZMM4c16_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
         
                 integer(kind=i4) :: i
                 integer(kind=i4) :: j
         
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM4c16_t) ::  tmp
!
          ! Exec code ....
          do i = 0, size(input,dim=1)-1
                tmp = input(i)
                do j = 0,  3
                    output1(i*v4len+j) = tmp.re(j)
                    output2(i*v4len+j) = tmp.im(j)
                end do
          end do
    end subroutine copy_zmm4c16_r16
    
end module mod_copypaos
