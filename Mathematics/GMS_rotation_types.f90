

module rotation_types


    !=================================================================================!
    !  Rotation types i.e. "Orientation Matrix, Quaternion, Euler Angles,
    !  Rodrigues Vector".
    !  Interoperable with corresponding C-side structures.
    !
    !=================================================================================!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    !use mod_kinds, only : int4
    use mod vectypes, only : ZMM16r4_t,ZMM8r8_t,YMM8r8_t,YMM4r8_t
    use, intrinsic :: ISO_C_BINDING
    implicit none

    ! AVX512 Complaint types

    !// Interoperable with corresponding Fortran structure.
    !// Direct Cosine Matrix single precision
    type, bind(c), public :: RotM9x16v16
       sequence
       type(ZMM16r4_t) :: row1
       type(ZMM16r4_t) :: row2
       type(ZMM16r4_t) :: row3
       type(ZMM16r4_t) :: row4
       type(ZMM16r4_t) :: row5
       type(ZMM16r4_t) :: row6
       type(ZMM16r4_t) :: row7
       type(ZMM16r4_t) :: row8
       type(ZMM16r4_t) :: row9
    end type RotM9x16v16

    !// Direct Cosine Matrix double precision
    type, bind(c), public :: RotM9x8v8
       sequence
       type(ZMM8r8_t) :: row1
       type(ZMM8r8_t) :: row2
       type(ZMM8r8_t) :: row3
       type(ZMM8r8_t) :: row4
       type(ZMM8r8_t) :: row5
       type(ZMM8r8_t) :: row6
       type(ZMM8r8_t) :: row7
       type(ZMM8r8_t) :: row8
       type(ZMM8r8_t) :: row9
    end type RotM9x8v8

    ! //Quaternion single precision
    type, bind(c), public :: Q4x16v16
       sequence
       type(ZMM16r4_t) :: qx
       type(ZMM16r4_t) :: qy
       type(ZMM16r4_t) :: qz
       type(ZMM16r4_t) :: qw
    end type Q4x16v16

    ! //Quaternion double precision
    type, bind(c), public :: Q4x8v8
       sequence
       type(ZMM8r8_t) :: qx
       type(ZMM8r8_t) :: qy
       type(ZMM8r8_t) :: qz
       type(ZMM8r8_t) :: qw
    end type Q4x8v8

    !// Euler Angles single precision
    type, bind(c), public :: EA3x16v16
       sequence
       type(ZMM16r4_t) :: alpha
       type(ZMM16r4_t) :: beta
       type(ZMM16r4_t) :: gamma
    end type EA3x16v16

    !// Euler Angles  double precision 
    type, bind(c), public :: EA3x8v8
       sequence
       type(ZMM8r8_t) :: alpha
       type(ZMM8r8_t) :: beta
       type(ZMM8r8_t) :: gamma
    end type EA3x8v8

    ! // Axis-angles pair single precision
    type, bind(c), public :: AX4x16v16
       sequence
       type(ZMM16r4_t) :: ax_1
       type(ZMM16r4_t) :: ax_2
       type(ZMM16r4_t) :: ax_3
       type(ZMM16r4_t) :: ax_4
    end type AX4x16v16

    ! // Axis-angles pair double precision
    type, bind(c), public :: AX4x8v8
       sequence
       type(ZMM8r8_t) :: ax_1
       type(ZMM8r8_t) :: ax_2
       type(ZMM8r8_t) :: ax_3
       type(ZMM8r8_t) :: ax_4
    end type AX4x8v8

    ! // Rodrigues Vector single precision
    type, bind(c), public :: RV4x16v16
       sequence
       type(ZMM16r4_t) :: rx
       type(ZMM16r4_t) :: ry
       type(ZMM16r4_t) :: rz
       type(ZMM16r4_t) :: rw
    end type RV4x16v16

    !  // Rodrigues Vector double precision
    type, bind(c), public :: RV4x8v8
       sequence
       type(ZMM8r8_t) :: rx
       type(ZMM8r8_t) :: ry
       type(ZMM8r8_t) :: rz
       type(ZMM8r8_t) :: rw
    end type RV4x8v8


    ! AVX/AVX2 Complaint types

    !// Interoperable with corresponding Fortran structure.
    !// Direct Cosine Matrix single precision
    type, bind(c), public :: RMat9x8v8
       sequence
       type(YMM8r4_t) :: row1
       type(YMM8r4_t) :: row2
       type(YMM8r4_t) :: row3
       type(YMM8r4_t) :: row4
       type(YMM8r4_t) :: row5
       type(YMM8r4_t) :: row6
       type(YMM8r4_t) :: row7
       type(YMM8r4_t) :: row8
       type(YMM8r4_t) :: row9
    end type RMat9x8v8

    !// Direct Cosine Matrix double precision
    type, bind(c), public :: RMat9x4v4
       sequence
       type(YMM4r8_t) :: row1
       type(YMM4r8_t) :: row2
       type(YMM4r8_t) :: row3
       type(YMM4r8_t) :: row4
       type(YMM4r8_t) :: row5
       type(YMM4r8_t) :: row6
       type(YMM4r8_t) :: row7
       type(YMM4r8_t) :: row8
       type(YMM4r8_t) :: row9
    end type RMat9x4v4
    
    !// Quaternion single precision
    type, bind(c), public :: Qu4x8v8
         sequence
         type(YMM8r4_t) :: qx
         type(YMM8r4_t) :: qy
         type(YMM8r4_t) :: qz
         type(YMM8r4_t) :: qw
    end type Qu4x8v8
    
    !// Quaternion double precision
    type, bind(c), public :: Qu4x4v4
         sequence
         type(YMM4r8_t) :: qx
         type(YMM4r8_t) :: qy
         type(YMM4r8_t) :: qz
         type(YMM4r8_t) :: qw
    end type Qu4x4v4

    !// Euler Angles single precision
    type, bind(c), public :: EAng3x8v8
       sequence
       type(YMM8r8_t) :: alpha
       type(YMM8r4_t) :: beta
       type(YMM8r4_t) :: gamma
    end type EAng3x8v8

    !// Euler Angles double precision
    type, bind(c), public :: EAng3x4v4
       sequence
       type(YMM4r8_t) :: alpha
       type(YMM4r8_t) :: beta
       type(YMM4r8_t) :: gamma
    end type EAng3x4v4

    ! // Axis-angles pair single precision
    type, bind(c), public :: AA4x8v8
       sequence
       type(YMM8r4_t) :: ax1
       type(YMM8r4_t) :: ax2
       type(YMM8r4_t) :: ax3
       type(YMM8r4_t) :: ax4
    end type AA4x8v8

    ! // Axis-angles pair double precision
    type, bind(c), public :: AA4x4v4
       sequence
       type(YMM4r8_t) :: ax1
       type(YMM4r8_t) :: ax2
       type(YMM4r8_t) :: ax3
       type(YMM4r8_t) :: ax4
    end type AA4x4v4

    ! // Rodrigues Vector single precision
    type, bind(c), public :: RVec4x8v8
       sequence
       type(YMM8r4_t) :: rx
       type(YMM8r4_t) :: ry
       type(YMM8r4_t) :: rz
       type(YMM8r4_t) :: rw
    end type RVec4x8v8

    ! // Rodrigues Vector single precision
    type, bind(c), public :: RVec4x4v4
       sequence
       type(YMM4r8_t) :: rx
       type(YMM4r8_t) :: ry
       type(YMM4r8_t) :: rz
       type(YMM4r8_t) :: rw
    end type RVec4x4v4

    

end module rotation_types
