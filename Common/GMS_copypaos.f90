

#inlcude "Config.fpp"

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
     use mod_kinds,     only : int1,int4, sp, dp, ep
     use mod_vecconsts, only : v8_n0, v4_n0, xmm2r8_zero, xmm2i4_zero, &
                               ymm3r8_zero
     implicit none
     public
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_COPYPAOS_FULLVER = 1000_int4*MOD_COPYPAOS_MAJOR+   &
                                                                    100_int4*MOD_COPYPAOS_MINOR+    &
                                                                    10_int4*MOD_COPYPAOS_MICRO
    
    ! Module creation date 
    character(*),       parameter, public :: MOD_COPYPAOS_CREATE_DATE = "08-11-2018 19:02 +00200 (THU 08 NOV 2018 GMT+2) " 
    
    ! Module build date
    character(*),       parameter, public :: MOD_COPYPAOS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_COPYPAOS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_COPYPAOS_SYNOPSIS = " 1D array of scalar primitive types to PAOS array copy subroutines."
    
    ! Module parameters
    
    integer(kind=int4), parameter, private :: v2len  = 2
    
    integer(kind=int4), parameter, private :: v3len  = 3
    
    integer(kind=int4), parameter, private :: v4len  = 4
    
    integer(kind=int4), parameter, private :: v8len  = 8
    
    integer(kind=int4), parameter, private :: v16len = 16
    
    contains
    
    ! Fast copy without arrays size conformance checking
    ! Hope it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_r8_xmm2r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_xmm2r8
          use mod_vectypes, only : XMM2r8_t
          type(XMM2r8_t),  contiguous   dimension(:),   intent(inout) :: output  
          real(kind=dp),   contiguous,  dimension(:),   intent(in)    :: input    
          ! Locals
          type :: ca_t
              sequence
              integer(kind=int4) :: i
              integer(kind=int4) :: j
          end type ca_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(ca_t) :: idx
          ! Exec code ....
         
          do idx.i = 0, size(output,dim=1)-1
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 1
                  output(idx.i).v(idx.j) = input(idx.i*v2len+idx.j)
              end do
          end do
    end subroutine copy_r8_xmm2r8
    
    ! Fast copy without arrays size conformance checking
    ! Hope it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_xmm2r8_r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_xmm2r8_r8
    use mod_vectypes,  only : XMM2r8_t
    real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
    type(XMM2r8_t), contiguous, dimension(:), intent(in)    :: input
    ! Locals
    type :: ca_t
         sequence
         integer(kind=int4) :: i
         integer(kind=int4) :: j
    end type ca_t
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
    type(XMM2r8_t) :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: idx
    type(ca_t)     :: idx
   
    ! Exec code ....
    do idx.i = 0, size(input,dim=1)-1
        tmp = input(idx.i)
!DIR$ VECTOR ALWAYS
        do idx.j = 0, 1
            output(idx.i*v2len+idx.j) = tmp.v(idx.j)
        end do
    end do
    end subroutine copy_xmm2r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_i4_xmm2i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_xmm2i4
          use mod_vectypes, only : XMM2i4_t
          type(XMM2i4_t),     contiguous  dimension(:),   intent(inout) :: output
          integer(kind=int4), contiguous, dimension(:),   intent(in)    :: input
          ! Locals
          type :: indices_t
              sequence
              integer(kind=int4) :: i
              integer(kind=int4) :: j
          end type indices_t

!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
             
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 1
                  output(idx.i).v(idx.j) = input(idx.i*v2len+idx.j)
              end do
          end do
    end subroutine copy_i4_xmm2i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_xmm2i4_i4(output,input)
!DIR$  ATTRIBUTES VECTOR :: copy_xmm2i4_i4
          use mod_vectypes,  only : XMM2i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(XMM2i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
             sequence
             integer(kind=int4) :: i
             integer(kind=int4) :: j
          end type indices_t
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM2i4_t)  :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ...
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 1
                  output(idx.i*v2len+idx.j) = tmp.v(idx.j)
              end do
           end do
    end subroutine copy_xmm2i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_r8_ymm3r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_ymm3r8
          use mod_vectypes, only : YMM3r8_t
          type(YMM3r8_t),  contiguous,       dimension(:),  intent(inout) :: output
          real(kind=dp),   contiguous,       dimension(:),  intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 2
                  output(idx.i).v(idx.j) = input(idx.i*v3len+idx.j)
              end do
          end do
    end subroutine copy_r8_ymm3r8  
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_ymm3r8_r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_ymm3r8_r8
          use mod_vectypes,  only : YMM3r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(YMM3r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM3r8_t)  :: tmp
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 2
                  output(idx.i*v3len+idx.j) = tmp.v(idx.j)
              end do
          end do
    end subroutine copy_ymm3r8_r8
    
   
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_i4_xmm3i4(output,input)
!DIR$ ATTRIBUTES  VECTOR :: copy_i4_xmm3i4
          use mod_vectypes, only : XMM3i4_t
          type(XMM3i4_t),     contiguous,  dimension(:),   intent(inout) :: output
          integer(kind=int4), contiguous,  dimension(:),   intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ...
          do idx.i = 0, size(output,dim=1)-1
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 2
                  output(idx.i).v(idx.j) = input(idx.i*v3len+idx.j)
              end do
          enddo
    end subroutine copy_i4_xmm3i4  
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_xmm3i4_i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_xmm3i4_i4
          use mod_vectypes, only : XMM3i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(XMM3i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM3i4_t)  :: tmp
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$         VECTOR ALWAYS
              do idx.j = 0, 2
                  output(idx.i*v3len+idx.j) = tmp.v(idx.j)
              end do
          end do
    end subroutine copy_xmm3i4_i4
    
   
   
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_r8_ymm4r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_ymm4r8
          use mod_vectypes, only :  YMM4r8_t
          type(YMM4r8_t),  contiguous,      dimension(:),   intent(inout) :: output
          real(kind=dp),   contiguous,      dimension(:),   intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 3
                  output(idx.i).v(idx.j) = input(idx.i*v4len+idx.j)
              end do
          end do
    end subroutine copy_r8_ymm4r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1) 
    subroutine copy_ymm4r8_r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_ymm4r8_r8
          use mod_vectypes,  only : YMM4r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(YMM4r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM4r8_t)  :: tmp
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                 tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 3
                  output(idx.i*v4len+idx.j) = temp.v(idx.j)
              end do
          end do
    end subroutine copy_ymm4r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
    subroutine copy_i4_xmm4i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_i4_xmm4i4
          use mod_vectypes, only : XMM4i4_t
          type(XMM4i4_t),     contiguous   dimension(:), intent(inout) :: output
          integer(kind=int4), contiguous,  dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$  VECTOR ALWAYS
             do idx.j = 0, 3
                   output(idx.i).v(idx.j) = input(idx.i*v4len+idx.j)
             end do
          end do
    end subroutine copy_i4_xmm4i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
    subroutine copy_xmm4i4_i4(output,input)
!DIR$  ATTRIBUTES VECTOR :: copy_xmm4i4_i4
          use mod_vectypes, only : XMM4i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(XMM4i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM4i4_t) :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                     tmp = input(idx.i)
!DIR$        VECTOR ALWAYS
                     do idx.j = 0,  3
                         output(idx.i*v4len*idx.j) = tmp.v(idx.j)
                     end do
          end do
    end subroutine copy_xmm4i4_i4
   
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_i4_ymm8i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_i4_ymm8i4
          use mod_vectypes, only : YMM8i4_t
          type(YMM8i4_t),     contiguous,   dimension(:), intent(inout) :: output
          integer(kind=int4), contiguous,   dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$  VECTOR ALWAYS
                 do idx.j = 0, 7
                      output(idx.i).v(idx.j) = input(idx.i*v8len+idx.j)
                 end do
          end do
    end subroutine copy_i4_ymm8i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_ymm8i4_i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_ymm8i4_i4
          use mod_vectypes, only : YMM8i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(YMM8i4_t),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM8i4_t)   :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
              do idx.j = 0, 7
                  output(idx.i*v8len+idx.j) = tmp.v(idx.j)
              end do
          end do
    end subroutine copy_ymm8i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
    subroutine copy_i4_zmm16i4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_i4_zmm16i4
          use mod_vectypes,  only : ZMM16i4_t
          type(ZMM16i4_t),    contiguous, dimension(:), intent(inout) :: output
          integer(kind=int4), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ...
          do idx.i = 0, size(output,dim=1)-1
!DIR$  VECTOR ALWAYS
                do idx.j = 0, 15
                     output(idx.i).v(idx.j) = input(idx.i*v16len+idx.j)
                end do
          end do
    end subroutine copy_i4_zmm16i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
    subroutine copy_zmm16i4_i4(output,input)
!DIR$  ATTRIBUTES VECTOR :: copy_zmm16i4_i4
          use mod_vectypes, only : ZMM16i4_t
          integer(kind=int4), contiguous, dimension(:), intent(inout) :: output
          type(ZMM16i4_t),    contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM16i4_t) :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                  do idx.j = 0, 15
                      output(idx.i*v16len+idx.j) = tmp.v(idx.j)
                  end do
          end do
    end subroutine copy_zmm16i4_i4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r8_zmm8r8(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_zmm8r8
          use mod_vectypes, only : ZMM8r8_t
          type(ZMM8r8_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ...
          do idx.i = 0, size(output,dim=1)-1
!DIR$  VECTOR ALWAYS
                 do idx.j = 0, 7
                     output(idx.i).v(idx.j) = input(idx.i*v8len+idx.j)
                 end do
          end do
    end subroutine copy_r8_zmm8r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_zmm8r8_r8(output,input)
!DIR$   ATTRIBUTES VECTOR :: copy_zmm8r8_r8
          use mod_vectypes,  only : ZMM8r8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output
          type(ZMM8r8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM8r8_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code .....
          do idx.i = 0, size(input,dim=1)-1
                tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                    do idx.j = 0, 7
                        output(idx.i*v8len+idx.j) = tmp.v(idx.j)
                    end do
          end do
    end subroutine copy_zmm8r8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r4_xmm2r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm2r4
          use mod_vectypes,  only : XMM2r4_t
          type(XMM2r4_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=sp),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
                 do idx.j = 0, 1
                     output(idx.i).v(jidx.) = input(idx.i*v2len+idx.j)
                 end do
          end do
    end subroutine copy_r4_xmm2r4
     
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)
    subroutine copy_xmm2r4_r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_xmm2r4_r4
          use mod_vectypes, only : XMM2r4_t
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output
          type(XMM2r4_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM2r4_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                tmp = input(idx.i)
!DIR$     ALWAYS VECTOR
                    do j = 0, 1
                        output(idx.i*v2len+idx.j) = tmp.v(idx.j)
                    end do
          end do
    end subroutine copy_xmm2r4_r4
    
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r4_xmm3r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm3r4
          use mod_vectypes, only :  XMM3r4_t
          type(XMM3r4_t),  contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),   contiguous,  dimension(:), intent(in)    :: input
         ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
         ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$    VECTOR ALWAYS
              do idx.j = 0, 2
                   output(idx.i).v(idx.j) = input(idx.i+v3len*idx.j)
              end do
    end do
    end subroutine copy_r4_xmm3r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 3*size(output,dim=1) == size(input,dim=1)
    subroutine copy_xmm3r4_r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_xmm3r4_r4
          use mod_vectypes, only : XMM3r4_t
          real(kind=sp),   contiguous,  dimension(:), intent(inout)    :: output
          type(XMM3r4_t),  contiguous,  dimension(:), intent(in)       :: input
          ! Locals
          type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM3r4_t)   :: tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code .....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                 do idx.j = 0,  2
                     output(idx.i*v3len+idx.j) = tmp.v(idx.j)
                 end do
          end do
    end subroutine copy_xmm3r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r4_xmm4r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r4_xmm4r4 
          use mod_vectypes, only :  XMM4r4_t
          type(XMM4r4_t),     contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),      contiguous,  dimension(:), intent(in)    :: input
          ! Locals
           type :: indices_t
               sequence
               integer(kind=int4) :: i
               integer(kind=int4) :: j
          end type indices_t
!DIR$ ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx          
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
              do idx.j = 0, 3
                   output(idx.i).v(idx.j) = input(idx.i*v4len+idx.j)
              end do
           end do
    end subroutine copy_r4_xmm4r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)
    subroutine copy_xmm4r4_r4(output,input)
!DIR$  ATTRIBUTES VECTOR :: copy_xmm4r4_r4
           use mod_vectypes, only :  XMM4r4_t
           real(kind=sp),   contiguous, dimension(:), intent(inout) :: output
           type(XMM4r4_t),  contiguous, dimension(:), intent(in)    :: input
           ! Locals
            type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
            end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
            type(XMM4r4_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
            type(indices_t) :: idx
            ! Exec code ....
            do idx.i = 0, size(input,dim=1)-1
                 tmp = input(idx.i)
!DIR$       VECTOR ALWAYS
                    do idx.j = 0,  3
                        output(idx.i*v3len+idx.j) = tmp.v(idx.j)
                    end do
            end do
    end subroutine copy_xmm4r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r4_ymm8r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r4_ymm8r4
          use mod_vectypes,  only : YMM8r4_t
          type(YMM8r4_t),    contiguous, dimension(:), intent(inout) :: output
          real(kind=sp),     contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          !Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
                  do idx.j = 0, 7
                    output(idx.i).v(idx.j) = input(idx.i*v8len+idx.j)
                  end do
          end do
    end subroutine copy_r4_ymm8r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)
    subroutine copy_ymm8r4_r4(output,input)
!DIR$  ATTRIBUTES VECTOR  :: copy_ymm8r4_r4
          use mod_vectypes,  only : YMM8r4_t
          real(kind=sp),    contiguous, dimension(:), intent(inout) :: output
          type(YMM8r4_t),   contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM8r4_t)    :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)   :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$         VECTOR ALWAYS
                    do idx.j = 0,  7
                        output(idx.i*v8len+idx.j) = tmp.v(idx.j)
                    end do
          end do
    end subroutine copy_ymm8r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
    subroutine copy_r4_zmm16r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_r4_zmm16r4
          use mod_vectypes, only : ZMM16r4_t
          type(ZMM16r4_t),  contiguous,  dimension(:), intent(inout) :: output
          real(kind=sp),    contiguous,  dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
                do idx.j = 0, 15
                    output(idx.i).v(idx.j) = input(idx.i*v16len+idx.j)
                end do
           end do
    end subroutine copy_r4_zmm16r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 16*size(output,dim=1) == size(input,dim=1)
    subroutine copy_zmm16r4_r4(output,input)
!DIR$ ATTRIBUTES VECTOR :: copy_zmm16r4_r4
          use mod_vectypes, only : ZMM16r4_t
          real(kind=sp),    contiguous, dimension(:), intent(inout) :: output
          type(ZMM16r4_t),  contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM16r4_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
              tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                do idx.j = 0, 15
                    output(idx.i*v16len+idx.j) = tmp.v(idx.j)
                end do
          end do
    end subroutine copy_zmm16r4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_r4_xmm2c4(output,input1,input2)
!DIR$  ATTRIBUTES VECTOR :: copy_r4_xmm2c4
          use mod_vectypes, only :  XMM2c4_t
          type(XMM2c4_t),            dimension(:), intent(inout) :: output
          real(kind=sp), contiguous, dimension(:), intent(in)    :: input1
          real(kind=sp), contiguous, dimension(:), intent(in)    :: input2
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$   ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$  VECTOR ALWAYS
                 do idx.j = 0,  1
                      output(idx.i).re(idx.j) = input1(idx.i*v2len+idx.j)
                      output(idx.i).im(idx.j) = input2(idx.i*v2len+idx.j)
                 end do
          end do
    end subroutine copy_r4_xmm2c4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 2*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 2*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_xmm2c4_r4(output1,output2,input)
!DIR$  ATTRIBUTES VECTOR :: copy_xmm2c4_r4
          use mod_vectypes, only : XMM2c4_t
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output1
          real(kind=sp),  contiguous, dimension(:), intent(inout) :: output2
          type(XMM2c4_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(XMM2c4_t)    :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)   :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
               tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                do idx.j = 0,  1
                        output1(idx.i*v2len+idx.j) = tmp.re(idx.j)
                        output2(idx.i*v2len+idx.j) = tmp.im(idx.j)
                end do
           end do
    end subroutine copy_xmm2c4_r4
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_r8_ymm4c8(output,input1,input2)
!DIR$  ATTRIBUTES VECTOR :: copy_r8_ymm4c8
          use mod_vectypes, only :  YMM4c8_t
          type(YMM4c8_t), contiguous, dimension(:), intent(inout) :: output
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input1
          real(kind=dp),  contiguous, dimension(:), intent(in)    :: input2
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
                do idx.j = 0, 3
                    output(idx.i).re(idx.j) = input(idx.i*v4len+idx.j)
                    output(idx.i).im(idx.j) = input(idx.i*v4len+idx.j)
               end do
          end do
    end subroutine copy_r8_ymm4c8
    
     ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 4*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 4*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_ymm4c8_r8(output1,output2,input)
!DIR$   ATTRIBUTES VECTOR :: copy_ymm4c8_r8
          use mod_vectypes,  only : YMM4c8_t
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output1
          real(kind=dp),  contiguous, dimension(:), intent(inout) :: output2
          type(YMM4c8_t), contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM4c8_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                    do idx.j = 0,  3
                        output1(idx.i*v4len+idx.j) = tmp.re(j)
                        output2(idx.i*v4len+idx.j) = tmp.im(j)
                    end do
           end do
    end subroutine copy_ymm4c8_r8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 8*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_r8_zmm8c8(output,input1,input2)
!DIR$ ATTRIBUTES VECTOR :: copy_r8_zmm8c8
          use mod_vectypes,  only : ZMM8c8_t
          type(ZMM8c8_t), contiguous,     dimension(:),   intent(inout)   :: output
          real(kind=dp),  contiguous,     dimension(:),   intent(in)      :: input1
          real(kind=dp),  contiguous,     dimension(:),   intent(in)      :: input2
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$  ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0, size(output,dim=1)-1
!DIR$ VECTOR ALWAYS
                do idx.j = 0, 7
                     output(idx.i).re(idx.j) = input1(idx.i*v8len+idx.j)
                     output(idx.i).im(idx.j) = input2(idx.i*v8len+idx.j)
                end do
         end do
    end subroutine copy_r8_zmm8c8
    
    ! Fast copy without arrays size conformance checking
    ! Probably it will enable auto-vectorization of this subroutine.
    ! 8*size(output,dim=1) == size(input,dim=1)   -- real      part
    ! 8*size(output,dim=1) == size(input,dim=1)   -- imaginary part
    subroutine copy_zmm8c8_r8(output1,output2,input)
!DIR$ ATTRIBUTES VECTOR :: copy_zmm8c8_r8
          use mod_vectypes,  only : ZMM8c8_t
          real(kind=dp),    contiguous, dimension(:), intent(inout) :: output1
          real(kind=dp),    contiguous, dimension(:), intent(inout) :: output2
          type(ZMM8c8_t),   contiguous, dimension(:), intent(in)    :: input
          ! Locals
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM8c8_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ...
          do idx.i = 0, size(input,dim=1)-1
                tmp = input(idx.i)
!DIR$     VECTOR ALWAYS
                    do idx.j = 0,  7
                        output1(idx.i*v8len+idx.j) = tmp.re(idx.j)
                        output2(idx.i*v8len+idx.j) = tmp.im(idx.j)
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
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$   ATTRIBUTES ALIGN : 64 ::  idx
          type(indices_t)  :: idx
          ! Exec code ...
          do idx.i = 0, size(output,dim=1)-1
                 do idx.j = 0,  1
                     output(idx.i).re(idx.j) = input1(idx.i*v2len+idx.j)
                     output(idx.i).im(idx.j) = input2(idx.i*v2len+idx.j)
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
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM2c16_t)  :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                 tmp = input(idx.i)
                 do idx.j = 0,  1
                      output1(idx.i*v2len+idx.j) = tmp.re(idx.j)
                      output2(idx.i*v2len+idx.j) = tmp.im(idx.j)
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
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$   ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t) :: idx
          ! Exec code ....
          do idx.i = 0,  size(input,dim=1)-1
              do idx.j = 0, 3
                  output(idx.i).re(idx.j) = input1(idx.i*v4len+idx.j)
                  output(idx.i).im(idx.j) = input2(idx.i*v4len+idx.j)
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
          type :: indices_t
                 sequence
                 integer(kind=int4) :: i
                 integer(kind=int4) :: j
          end type indices_t
!DIR$ ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM4c16_t) ::  tmp
!DIR$ ATTRIBUTES ALIGN : 64 :: idx
          type(indices_t)  :: idx
          ! Exec code ....
          do idx.i = 0, size(input,dim=1)-1
                tmp = input(idx.i)
                do idx.j = 0,  3
                    output1(idx.i*v4len+idx.j) = tmp.re(idx.j)
                    output2(idx.i*v4len+idx.j) = tmp.im(idx.j)
                end do
          end do
    end subroutine copy_zmm4c16_r16
    
end module mod_copypaos