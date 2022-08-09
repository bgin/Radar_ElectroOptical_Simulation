

module eos_sensor


!======================================================!
! Various characteristics of Electro-Optical Systems   !
! Based mainly on Miroshenko M.M book (rus):           !
! "Mathematical Theory of Electro-Optical Sensors"     !
!======================================================!
!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         eos_noise_immune
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !          History:
 !                        Date: 09-08-2022
 !                        Time: 09:44 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                       Miroshenko M.M book (rus):          
 !                      "Mathematical Theory of Electro-Optical Sensors"     
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
   use mod_kinds,    only : i4,sp,dp
   use mod_vectypes, only : ZMM16r4_t, ZMM8r8_t
   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_FULLVER =   &
            1000*EOS_SENSOR_MAJOR+100*EOS_SENSOR_MINOR+10*EOS_SENSOR_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_CREATE_DATE = "09-08-2022 09:44 +00200 (TUE 09 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_SYNOPSIS    = "EO Sensors characteristics and models."


     ! Scanning mirror derived type
     
     type, public :: scanning_mirror

           sequence
           real(kind=sp) :: gamma ! angle of mirror position relative to obiective optical axis (p. 53, 1.2)
           real(kind=sp) :: gamma0 ! angle of fixing
           real(kind=sp) :: phi   ! sensor fov
           real(kind=sp) :: F     ! Focal length
           real(kind=sp) :: H     ! distance from the focal length to target image
           real(kind=sp) :: Dmax  ! size of mirror vertical plane
           real(kind=sp) :: Dmin  ! size of mirror horizontal plane
           
     end type scanning_mirror


     contains

     
     subroutine param_gamma_r4(sm)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_gamma_r4
        !dir$ forceinline :: param_gamma_r4
        type(scanning_mirror), intent(inout) :: sm
        sm.gamma = 0.5_sp*sm.phi*0.5_sp
     end subroutine param_gamma_r4

     ! Formula 1, p.54
     !Тогда длина перпендикуляра SN, опущенного из 
     !светящейся точки на плоскость зеркала
     pure elemental function compute_SN_r4(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r4
        !dir$ forceinline :: compute_SN_r4
        real(kind=sp),  intent(in) :: R
        real(kind=sp),  intent(in), optional :: phi
        real(kind=sp),  intent(in), optional :: gamma
        real(kind=sp) :: SN
        if(present(phi)) then
            SN = R*sin(phi)
        else if(present(gamma)) then
            SN = R*cos(gamma)
        end if
     end function compute_SN_r4


     pure elemental function compute_SN_r8(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r8
        !dir$ forceinline :: compute_SN_r8
        real(kind=dp),  intent(in) :: R
        real(kind=dp),  intent(in), optional :: phi
        real(kind=dp),  intent(in), optional :: gamma
        real(kind=dp) :: SN
        if(present(phi)) then
            SN = R*sin(phi)
        else if(present(gamma)) then
            SN = R*cos(gamma)
        end if
     end function compute_SN_r8


     pure function compute_SN_zmm16r4(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_zmm16r4
        !dir$ forceinline :: compute_SN_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  intent(in), optional :: phi
        type(ZMM16r4_t),  intent(in), optional :: gamma
        type(ZMM16r4_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_zmm16r4

     
      pure function compute_SN_zmm8r8(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_zmm8r8
        !dir$ forceinline :: compute_SN_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  intent(in), optional :: phi
        type(ZMM8r8_t),  intent(in), optional :: gamma
        type(ZMM8r8_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_zmm8r8


     ! Formula 2, p. 54
     ! расстояние SM от светящейся точки до ее изображения
     pure elemental function compute_SM_r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r4
        !dir$ forceinline :: compute_SM_r4
        real(kind=sp), intent(in) :: R
        real(kind=sp), intent(in) :: phi
        real(kind=sp), intent(in) :: gamma
        real(kind=sp) :: SM
        SM = 2.0_sp*compute_SN_r4(R,phi,gamma)
     end function compute_SM_r4
 

     pure elemental function compute_SM_r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r8
        !dir$ forceinline :: compute_SM_r8
        real(kind=dp), intent(in) :: R
        real(kind=dp), intent(in) :: phi
        real(kind=dp), intent(in) :: gamma
        real(kind=dp) :: SM
        SM = 2.0_sp*compute_SN_r8(R,phi,gamma)
     end function compute_SM_r8

     
     pure function compute_SM_zmm16r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_zmm16r4
        !dir$ forceinline :: compute_SM_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  intent(in), optional :: phi
        type(ZMM16r4_t),  intent(in), optional :: gamma
        type(ZMM16r4_t) :: SM
        type(ZMM16r4_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_zmm16r4(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_zmm16r4


     pure function compute_SM_zmm8r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_zmm8r8
        !dir$ forceinline :: compute_SM_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  intent(in), optional :: phi
        type(ZMM8r8_t),  intent(in), optional :: gamma
        type(ZMM8r8_t) :: SM
        type(ZMM88r8_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_zmm8r8(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_zmm8r8


     




end module eos_sensor
