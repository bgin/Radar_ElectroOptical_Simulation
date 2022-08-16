

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


     !Сканирующее зеркало для обеспечения осмотра всего поля
     !обзора ф необходимо повернуть на угол, обеспечивающий 
     !совмещение края изображения источника излучения с отверстием 
     !диафрагмы а, находящимся в центре поля. Для этого необходимо 
     !повернуть изображение светящейся точки S на угол ф/2
     ! Formula 1, p. 56
     pure elemental function ratio_FH_r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_r4
        !dir$ forceinline :: ratio_FH_r4
        real(kind=sp),  intent(in) :: psi
        real(kind=sp),  intent(in) :: phi
        real(kind=sp) :: FH
        real(kind=sp), automatic :: hpsi,hphi
        hpsi = 0.5_sp*psi
        hphi = 0.5_sp*phi
        FH   = tan(hpsi)/tan(hphi)
     end function ratio_FH_r4


     pure elemental function ratio_FH_r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_r8
        !dir$ forceinline :: ratio_FH_r8
        real(kind=dp),  intent(in) :: psi
        real(kind=dp),  intent(in) :: phi
        real(kind=dp) :: FH
        real(kind=dp), automatic :: hpsi,hphi
        hpsi = 0.5_dp*psi
        hphi = 0.5_dp*phi
        FH   = tan(hpsi)/tan(hphi)
     end function ratio_FH_r8


     pure function ratio_FH_zmm16r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_zmm16r4
        !dir$ forceinline :: ratio_FH_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_zmm16r4
        type(ZMM16r4_t),  intent(in) :: psi
        type(ZMM16r4_t),  intent(in) :: phi
        type(ZMM16r4_t) :: FH
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_zmm16r4


     pure function ratio_FH_zmm8r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_zmm8r8
        !dir$ forceinline :: ratio_FH_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_zmm8r8
        type(ZMM8r8_t),  intent(in) :: psi
        type(ZMM8r8_t),  intent(in) :: phi
        type(ZMM8r8_t) :: FH
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_zmm8r8


     ! следовательно, угол установки сканирующего зеркала
     ! Formula 4, p. 56
     pure elemental function scan_mirror_ang_r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_r4
        !dir$ forceinline :: scan_mirror_ang_r4
        real(kind=sp),  intent(in) :: gam0
        real(kind=sp),  intent(in) :: psi
        real(kind=sp),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        real(kind=sp) :: gamma
        real(kind=sp), automatic :: t0
        if(dir=="pos") then
           t0 = gam0+0.5_sp*phi*0.5_sp
           gamma = t0*ratio_FH_r4(psi,phi) 
        else if(dir=="neg") then
           t0 = gam0-0.5_sp*phi*0.5_sp
           gamma = t0*ratio_FH_r4(psi,phi)
        end if
     end function scan_mirror_ang_r4


     pure elemental function scan_mirror_ang_r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_r8
        !dir$ forceinline :: scan_mirror_ang_r8
        real(kind=dp),  intent(in) :: gam0
        real(kind=dp),  intent(in) :: psi
        real(kind=dp),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        real(kind=dp) :: gamma
        real(kind=dp), automatic :: t0
        if(dir=="pos") then
           t0 = gam0+0.5_dp*phi*0.5_dp
           gamma = t0*ratio_FH_r8(psi,phi) 
        else if(dir=="neg") then
           t0 = gam0-0.5_dp*phi*0.5_dp
           gamma = t0*ratio_FH_r8(psi,phi)
        end if
     end function scan_mirror_ang_r8


     pure function scan_mirror_ang_zmm16r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_zmm16r4
        !dir$ forceinline :: scan_mirror_ang_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_zmm16r4
        type(ZMM16r4_t),  intent(in) :: gam0
        type(ZMM16r4_t),  intent(in) :: psi
        type(ZMM16r4_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(ZMM16r4_t) :: gamma
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_zmm16r4(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_zmm16r4(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_zmm16r4


     pure function scan_mirror_ang_zmm8r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_zmm8r8
        !dir$ forceinline :: scan_mirror_ang_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_zmm8r8
        type(ZMM8r8_t),  intent(in) :: gam0
        type(ZMM8r8_t),  intent(in) :: psi
        type(ZMM8r8_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(ZMM8r8_t) :: gamma
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_zmm8r8(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_zmm8r8(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_zmm8r8



     ! Maximum size of (verical) diameter of scanning mirror.
     ! Formula 2, page. 56, part: 1.3
     ! Anax = [h tg (6/2) + do6/2] [2 cos y' + sin yf {tg (у' + 6/2) +
     !+ tg(Y'-6/2)}].
     pure function compute_Dmax_r4(h,delta,d_ob,gamma) result(Dmax)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_Dmax_r4
        !dir$ forceinline :: compute_Dmax_r4
        real(kind=sp),  intent(in) :: h
        real(kind=sp),  intent(in) :: delta
        real(kind=sp),  intent(in) :: d_ob
        real(kind=sp),  intent(in) :: gamma
        real(kind=sp) :: Dmax
        real(kind=sp), automatic :: delta2,d_ob2,cosg,sing,t0,t1,t2,tant0,tant1
        real(kind=sp), automatic :: t3,t4,t5
        delta2 = 0.5_sp*delta
        d_ob2  = 0.5_sp*d_ob
        cosg   = cos(gamma)
        if(delta<=gamma) then
           t0  = h*delta+d_ob
           Dmax= t0/cosg
        end if
        t0     = gamma+delta2
        t1     = gamma-delta2
        sing   = sin(gamma)
        tant1  = tan(t0)
        tant2  = tan(t1)
        t3     = h*tan(delta2)+d_ob2
        t4     = 2.0_sp*cosg+sing
        t5     = tant1+tant2
        Dmax   = t3*t4*t5
     end function compute_Dmax_r4


     pure function compute_Dmax_r8(h,delta,d_ob,gamma) result(Dmax)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_Dmax_r8
        !dir$ forceinline :: compute_Dmax_r8
        real(kind=dp),  intent(in) :: h
        real(kind=dp),  intent(in) :: delta
        real(kind=dp),  intent(in) :: d_ob
        real(kind=dp),  intent(in) :: gamma
        real(kind=dp) :: Dmax
        real(kind=dp), automatic :: delta2,d_ob2,cosg,sing,t0,t1,t2,tant0,tant1
        real(kind=dp), automatic :: t3,t4,t5
        delta2 = 0.5_dp*delta
        d_ob2  = 0.5_dp*d_ob
        cosg   = cos(gamma)
        if(delta<=gamma) then
           t0  = h*delta+d_ob
           Dmax= t0/cosg
        end if 
        t0     = gamma+delta2
        t1     = gamma-delta2
        sing   = sin(gamma)
        tant1  = tan(t0)
        tant2  = tan(t1)
        t3     = h*tan(delta2)+d_ob2
        t4     = 2.0_dp*cosg+sing
        t5     = tant1+tant2
        Dmax   = t3*t4*t5
     end function compute_Dmax_r8


     ! Размер зеркала в направлении, перпендикулярном плоскости
     ! чертежа, приблизительно равен
     ! Formula 2, p. 58
     pure function compute_Dmin_r4(h,delta,d_ob) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_Dmin_r4
          !dir$ forceinline :: compute_Dmin_r4
          real(kind=sp),  intent(in) :: h
          real(kind=sp),  intent(in) :: delta
          real(kind=sp),  intent(in) :: d_ob
          real(kind=sp) :: Dmin
          Dmin = h*delta+d_ob
     end function compute_Dmin_r4


     pure function compute_Dmin_r8(h,delta,d_ob) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_Dmin_r8
          !dir$ forceinline :: compute_Dmin_r8
          real(kind=dp),  intent(in) :: h
          real(kind=dp),  intent(in) :: delta
          real(kind=dp),  intent(in) :: d_ob
          real(kind=dp) :: Dmin
          Dmin = h*delta+d_ob
     end function compute_Dmin_r8


     !Если зеркало осуществляет сканирование в пространстве
     !изображений его размеры
     ! Formula 7, p. 58
     pure function Dmax_imag_scan_r4(H,F,B,d_ob,gamma, &
                                     psi,phi,d) result(Dmax)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmax_imag_scan_r4
          !dir$ forceinline :: Dmax_imag_scan_r4
          real(kind=sp),  intent(in) :: H
          real(kind=sp),  intent(in) :: F
          real(kind=sp),  intent(in) :: B
          real(kind=sp),  intent(in) :: d_ob
          real(kind=sp),  intent(in) :: gamma
          real(kind=sp),  intent(in) :: psi
          real(kind=sp),  intent(in) :: phi
          real(kind=sp),  intent(in) :: d
          real(kind=sp) :: Dmax
          real(kind=sp), automatic :: t0,t1,t2,t3
          real(kind=sp), automatic :: cosg,sing,tanp1,tanp2,psi2,phi2
          psi2  = 0.5_sp*psi
          if(psi2<=gamma .and. &
             B<=d) then
             phi2 = 0.5_sp*phi
             t0   = (F+F)*tan(phi2)
             t1   = (H/F)*d_ob
             t2   = sin(gamma)
             Dmax = (t0+t1)*t2
          end if
          t0    = (H/F)*(d_ob-B)+B
          cosg  = cos(gamma)
          tanp1 = gamma+psi2
          tanp2 = gamma-psi2
          sing  = sin(gamma)
          t1    = 2.0_sp*cosg+sing
          t2    = tan(tanp1)+tan(tanp2)
          t3    = 0.5_sp*t1*t2
          Dmax  = t0*t3
     end function Dmax_imag_scan_r4


     pure function Dmax_imag_scan_r8(H,F,B,d_ob,gamma, &
                                     psi,phi,d) result(Dmax)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmax_imag_scan_r8
          !dir$ forceinline :: Dmax_imag_scan_r8
          real(kind=dp),  intent(in) :: H
          real(kind=dp),  intent(in) :: F
          real(kind=dp),  intent(in) :: B
          real(kind=dp),  intent(in) :: d_ob
          real(kind=dp),  intent(in) :: gamma
          real(kind=dp),  intent(in) :: psi
          real(kind=dp),  intent(in) :: phi
          real(kind=dp),  intent(in) :: d
          real(kind=dp) :: Dmax
          real(kind=dp), automatic :: t0,t1,t2,t3
          real(kind=dp), automatic :: cosg,sing,tanp1,tanp2,psi2,phi2
          psi2  = 0.5_dp*psi
          if(psi2<=gamma .and. &
             B<=d) then
             phi2 = 0.5_dp*phi
             t0   = (F+F)*tan(phi2)
             t1   = (H/F)*d_ob
             t2   = sin(gamma)
             Dmax = (t0+t1)*t2
          end if
          t0    = (H/F)*(d_ob-B)+B
          cosg  = cos(gamma)
          tanp1 = gamma+psi2
          tanp2 = gamma-psi2
          sing  = sin(gamma)
          t1    = 2.0_dp*cosg+sing
          t2    = tan(tanp1)+tan(tanp2)
          t3    = 0.5_dp*t1*t2
          Dmax  = t0*t3
     end function Dmax_imag_scan_r8


     pure function Dmin_imag_scan_r4(H,F,d_ob,B) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmin_imag_scan_r4
          !dir$ forceinline :: Dmin_imag_scan_r4
          real(kind=sp),     intent(in) :: H
          real(kind=sp),     intent(in) :: F
          real(kind=sp),     intent(in) :: d_ob
          real(kind=sp),     intent(in) :: B
          real(kind=sp) :: Dmin
          real(kind=sp), automatic :: t0,t1
          t0   = H/F
          t1   = (d_ob-B)+B
          Dmin = t0*t1 
     end function Dmin_imag_scan_r4


     pure function Dmin_imag_scan_r8(H,F,d_ob,B) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmin_imag_scan_r8
          !dir$ forceinline :: Dmin_imag_scan_r8
          real(kind=dp),     intent(in) :: H
          real(kind=dp),     intent(in) :: F
          real(kind=dp),     intent(in) :: d_ob
          real(kind=dp),     intent(in) :: B
          real(kind=dp) :: Dmin
          real(kind=dp), automatic :: t0,t1
          t0   = H/F
          t1   = (d_ob-B)+B
          Dmin = t0*t1 
     end function Dmin_imag_scan_r8


    !величина расфокусировки
    !Formula 1, p. 59
    pure elemental function defocus_cof_r4(l2,alpha,O,inf) result(dc)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: defocus_cof_r4
          !dir$ forceinline :: defocus_cof_r4
          real(kind=sp),    intent(in) :: l2
          real(kind=sp),    intent(in) :: alpha
          real(kind=sp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=sp) :: df
          real(kind=sp), automatic :: cos2a,icos
          cos2a = cos(alpha+alpha)
          icos  = 1.0_sp/cos2a
          if(inf) then
             df    = l2/(icos-1.0_sp)*O
          else
             df    = l2/(icos-1.0_sp)
          end if
    end function defocus_cof_r4


    pure elemental function defocus_cof_r8(l2,alpha,O,inf) result(dc)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: defocus_cof_r8
          !dir$ forceinline :: defocus_cof_r8
          real(kind=dp),    intent(in) :: l2
          real(kind=dp),    intent(in) :: alpha
          real(kind=dp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=dp) :: df
          real(kind=dp), automatic :: cos2a,icos
          cos2a = cos(alpha+alpha)
          icos  = 1.0_dp/cos2a
          if(inf) then
             df    = l2/(icos-1.0_dp)*O
          else
             df    = l2/(icos-1.0_dp)
          end if
    end function defocus_cof_r8


    pure function defocus_cof_zmm16r4(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_zmm16r4
        !dir$ forceinline :: defocus_cof_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_zmm16r4
        type(ZMM16r4_t),  intent(in) :: l2
        type(ZMM16r4_t),  intent(in) :: alpha
        type(ZMM16r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(ZMM16r4_t) :: dc
        type(ZMM16r4_t), automatic :: cos2a,icos
        type(ZMM16r4_t), parameter :: one = ZMM16r4_t(1.0_sp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_zmm16r4


    pure function defocus_cof_zmm8r8(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_zmm8r8
        !dir$ forceinline :: defocus_cof_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_zmm8r8
        type(ZMM8r8_t),   intent(in) :: l2
        type(ZMM8r8_t),   intent(in) :: alpha
        type(ZMM8r8_t),   intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(ZMM8r8_t) :: dc
        type(ZMM8r8_t), automatic :: cos2a,icos
        type(ZMM8r8_t), parameter :: one = ZMM8r8_t(1.0_dp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_zmm8r8


    ! Диаметр кружка рассеяния р
    ! Formula 3, p.59
    pure elemental function circle_dispersion_r4(d,l1,l2,alpha,O,inf) result(rho)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circle_dispersion_r4
          !dir$ forceinline :: circle_dispersion_r4
          real(kind=sp),    intent(in) :: d
          real(kind=sp),    intent(in) :: l1
          real(kind=sp),    intent(in) :: l2
          real(kind=sp),    intent(in) :: alpha
          real(kind=sp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=sp) :: rho
          real(kind=sp), automatic :: t0,t1
          t0  = d/(l1+l2)
          t1  = defocus_cof_r4(l2,alpha,O,inf)
          rho = t0*t1
    end function circle_dispersion_r4


    pure elemental function circle_dispersion_r8(d,l1,l2,alpha,O,inf) result(rho)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circle_dispersion_r8
          !dir$ forceinline :: circle_dispersion_r8
          real(kind=dp),    intent(in) :: d
          real(kind=dp),    intent(in) :: l1
          real(kind=dp),    intent(in) :: l2
          real(kind=dp),    intent(in) :: alpha
          real(kind=dp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=dp) :: rho
          real(kind=dp), automatic :: t0,t1
          t0  = d/(l1+l2)
          t1  = defocus_cof_r8(l2,alpha,O,inf)
          rho = t0*t1
    end function circle_dispersion_r8


    pure function circle_dispersion_zmm16r4(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_zmm16r4
        !dir$ forceinline :: circle_dispersion_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_zmm16r4
        type(ZMM16r4_t),  intent(in) :: d
        type(ZMM16r4_t),  intent(in) :: l1
        type(ZMM16r4_t),  intent(in) :: l2
        type(ZMM16r4_t),  intent(in) :: alpha
        type(ZMM16r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(ZMM16r4_t) :: rho
        type(ZMM16r4_t), automatic :: t0,t1
        !dir$ attributes align : 64 :: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_zmm16r4(l2,alpha,O,inf)
        rho= t0.v*t1.v
    end function circle_dispersion_zmm16r4


    pure function circle_dispersion_zmm8r8(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_zmm8r8
        !dir$ forceinline :: circle_dispersion_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_zmm8r8
        type(ZMM8r8_t),  intent(in) :: d
        type(ZMM8r8_t),  intent(in) :: l1
        type(ZMM8r8_t),  intent(in) :: l2
        type(ZMM8r8_t),  intent(in) :: alpha
        type(ZMM8r8_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(ZMM8r8_t) :: rho
        type(ZMM8r8_t), automatic :: t0,t1
        !dir$ attributes align : 64 :: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_zmm8r8(l2,alpha,O,inf)
        rho= t0.v*t1.v
     end function circle_dispersion_zmm8r8

      
     !Formula 2, p. 59
     pure elemental function circ_dispers_diam_r4(l1,l2,alpha,O,inf) result(ratio)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: circ_dispers_diam_r4
         !dir$ forceinline :: circ_dispers_diam_r4
         real(kind=sp),    intent(in) :: l1
         real(kind=sp),    intent(in) :: l2
         real(kind=sp),    intent(in) :: alpha
         real(kind=sp),    intent(in) :: O
         logical(kind=i4), intent(in) :: inf
         real(kind=sp) :: ratio
         real(kind=sp), automatic :: t0,t1
         t0    = l1+l2
         t1    = defocus_cos_r4(l2,alpha,O,inf)
         ratio = t1/t0
     end function circ_dispers_diam_r4


     pure elemental function circ_dispers_diam_r4(l1,l2,alpha,O,inf) result(ratio)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: circ_dispers_diam_r8
         !dir$ forceinline :: circ_dispers_diam_r8
         real(kind=dp),    intent(in) :: l1
         real(kind=dp),    intent(in) :: l2
         real(kind=dp),    intent(in) :: alpha
         real(kind=dp),    intent(in) :: O
         logical(kind=i4), intent(in) :: inf
         real(kind=dp) :: ratio
         real(kind=dp), automatic :: t0,t1
         t0    = l1+l2
         t1    = defocus_cos_r4(l2,alpha,O,inf)
         ratio = t1/t0
     end function circ_dispers_diam_r4


     pure function circ_dispers_diam_zmm16r4(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_zmm16r4
        !dir$ forceinline :: circ_dispers_diam_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_zmm16r4
        type(ZMM16r4_t),     intent(in) :: l1
        type(ZMM16r4_t),     intent(in) :: l2
        type(ZMM16r4_t),     intent(in) :: alpha
        type(ZMM16r4_t),     intent(in) :: O
        logical(kind=i4),    intent(in) :: inf
        type(ZMM16r4_t) :: ratio
        type(ZMM16r4_t), automatic :: t0,t1
        !dir$ attributes align : 64 :: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_zmm16r4(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_zmm16r4


     pure function circ_dispers_diam_zmm8r8(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_zmm8r8
        !dir$ forceinline :: circ_dispers_diam_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_zmm8r8
        type(ZMM8r8_t),     intent(in) :: l1
        type(ZMM8r8_t),     intent(in) :: l2
        type(ZMM8r8_t),     intent(in) :: alpha
        type(ZMM8r8_t),     intent(in) :: O
        logical(kind=i4),   intent(in) :: inf
        type(ZMM8r8_t) :: ratio
        type(ZMM8r8_t), automatic :: t0,t1
        !dir$ attributes align : 64 :: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_zmm8r8(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_zmm8r8
      
       
     pure elemental function defocus_small_ang_r4(O,l2,alpha) result(rho)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: defocus_small_ang_r4
         !dir$ forceinline :: defocus_small_ang_r4
         use mod_fpcompare, only : Compare_Float
         real(kind=sp),   intent(in) :: O
         real(kind=sp),   intent(in) :: L2
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp) :: rho
         real(kind=sp), automatic :: t0,t1,t2,alpha2
         alpha2 = alpha+alpha
         t0     = cos(alpha2)
         t1     = 1.0_sp-alpha2*alpha2*0.5_sp
         if(Compare_FLoat(t0,t1)) then
            t2  = l2*0.5_sp
            rho = O*t2*alpha2*alpha2
         end if
         rho = tiny(1.0_sp)
      end function defocus_small_ang_r4


      pure elemental function defocus_small_ang_r8(O,l2,alpha) result(rho)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: defocus_small_ang_r8
         !dir$ forceinline :: defocus_small_ang_r8
         use mod_fpcompare, only : Compare_Float
         real(kind=dp),   intent(in) :: O
         real(kind=dp),   intent(in) :: L2
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp) :: rho
         real(kind=dp), automatic :: t0,t1,t2,alpha2
         alpha2 = alpha+alpha
         t0     = cos(alpha2)
         t1     = 1.0_dp-alpha2*alpha2*0.5_dp
         if(Compare_FLoat(t0,t1)) then
            t2  = l2*0.5_dp
            rho = O*t2*alpha2*alpha2
         end if
         rho = tiny(1.0_dp)
      end function defocus_small_ang_r8


      pure elemental function traj_scan_dxdt_r4(dx,dt) result(dxdt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dxdt_r4
         !dir$ forceinline :: traj_scan_dxdt_r4
         real(kind=sp), dimension(0:1), intent(in) :: dx
         real(kind=sp), dimension(0:1), intent(in) :: dt
         real(kind=sp) :: dxdt
         dxdt = dx(1)-dx(0)/(dt(1)-dt(0))
      end function traj_scan_dxdt_r4


      pure elemental function traj_scan_dxdt_r8(dx,dt) result(dxdt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dxdt_r8
         !dir$ forceinline :: traj_scan_dxdt_r8
         real(kind=dp), dimension(0:1), intent(in) :: dx
         real(kind=dp), dimension(0:1), intent(in) :: dt
         real(kind=dp) :: dxdt
         dxdt = dx(1)-dx(0)/(dt(1)-dt(0))
      end function traj_scan_dxdt_r8


      pure elemental function traj_scan_dydt_r4(dy,dt) result(dydt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dydt_r4
         !dir$ forceinline :: traj_scan_dydt_r4
         real(kind=sp), dimension(0:1), intent(in) :: dx=y
         real(kind=sp), dimension(0:1), intent(in) :: dt
         real(kind=sp) :: dydt
         dxdt = dy(1)-dy(0)/(dt(1)-dt(0))
      end function traj_scan_dydt_r4

       
      pure elemental function traj_scan_dydt_r8(dy,dt) result(dydt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dydt_r8
         !dir$ forceinline :: traj_scan_dydt_r8
         real(kind=dp), dimension(0:1), intent(in) :: dx=y
         real(kind=dp), dimension(0:1), intent(in) :: dt
         real(kind=dp) :: dydt
         dxdt = dy(1)-dy(0)/(dt(1)-dt(0))
      end function traj_scan_dydt_r8


      ! СКАНИРОВАНИЕ ЗЕРКАЛОМ, ВРАЩАЮЩИМСЯ
      ! ВОКРУГ ОСИ, НЕПЕРПЕНДИКУЛЯРНОЙ К НЕМУ
      ! Formula 1, p. 100
      pure elemental function fov_x_axis_r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_r4
         !dir$ forceinline :: fov_x_axis_r4
         real(kind=sp),  intent(in) :: H
         real(kind=sp),  intent(in) :: delta
         real(kind=sp),  intent(in) :: gamma
         real(kind=sp) :: ax
         real(kind=sp), automatic :: gamm2,tdel
         gamm2 = 0.5_sp*gamma
         tdel  = tan(delta)
         ax    = H*tdel/cos(gamm2)
      end function fov_x_axis_r4


      pure elemental function fov_x_axis_r8(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_r8
         !dir$ forceinline :: fov_x_axis_r8
         real(kind=dp),  intent(in) :: H
         real(kind=dp),  intent(in) :: delta
         real(kind=dp),  intent(in) :: gamma
         real(kind=dp) :: ax
         real(kind=dp), automatic :: gamm2,tdel
         gamm2 = 0.5_dp*gamma
         tdel  = tan(delta)
         ax    = H*tdel/cos(gamm2)
      end function fov_x_axis_r8


      pure elemental function fov_y_axis_r4(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_r4
         !dir$ forceinline :: fov_y_axis_r4
         real(kind=sp),  intent(in) :: H
         real(kind=sp),  intent(in) :: delta
         real(kind=sp),  intent(in) :: gamma
         real(kind=sp) :: ay
         real(kind=sp) :: ax,t0
         t0 = 0.5_sp*gamma
         ax = fov_x_axis_r4(H,delta,gamma)
         ay = t0*ax
      end function fov_y_axis_r4


      pure elemental function fov_y_axis_r8(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_r8
         !dir$ forceinline :: fov_y_axis_r8
         real(kind=dp),  intent(in) :: H
         real(kind=dp),  intent(in) :: delta
         real(kind=dp),  intent(in) :: gamma
         real(kind=dp) :: ay
         real(kind=dp) :: ax,t0
         t0 = 0.5_dp*gamma
         ax = fov_x_axis_r8(H,delta,gamma)
         ay = t0*ax
      end function fov_y_axis_r8


      pure function fov_x_axis_zmm16r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_zmm16r4
         !dir$ forceinline :: fov_x_axis_zmm16r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_zmm16r4
         type(ZMM16r4_t),   intent(in) :: H
         type(ZMM16r4_t),   intent(in) :: delta
         type(ZMM16r4_t),   intent(in) :: gamma
         type(ZMM16r4_t) :: ax
         type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
         type(ZMM16r4_t), automatic :: gamm2,tdel
         !dir$ attributes align : 64 :: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
      end function fov_x_axis_zmm16r4


     pure function fov_x_axis_zmm8r8(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_zmm8r8
         !dir$ forceinline :: fov_x_axis_zmm8r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_zmm8r8
         type(ZMM8r8_t),   intent(in) :: H
         type(ZMM8r8_t),   intent(in) :: delta
         type(ZMM8r8_t),   intent(in) :: gamma
         type(ZMM8r8_t) :: ax
         type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
         type(ZMM8r8_t), automatic :: gamm2,tdel
         !dir$ attributes align : 64 :: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
      end function fov_x_axis_zmm8r8


      pure function fov_y_axis_zmm16r4(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_zmm16r4
         !dir$ forceinline :: fov_y_axis_zmm16r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_y_axis_zmm16r4
         type(ZMM16r4_t),   intent(in) :: H
         type(ZMM16r4_t),   intent(in) :: delta
         type(ZMM16r4_t),   intent(in) :: gamma
         type(ZMM16r4_t) :: ay
         type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
         type(ZMM16r4_t), automatic :: ax,t0
         t0  = half.v*gamma
         ax  = fov_x_axis_zmm16r4(H,delta,gamma)
         ay  = ax.v*cos(t0.v)
      end function fov_y_axis_zmm16r4
       

      pure function fov_y_axis_zmm8r8(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_zmm8r8
         !dir$ forceinline :: fov_y_axis_zmm8r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_y_axis_zmm8r8
         type(ZMM8r8_t),   intent(in) :: H
         type(ZMM8r8_t),   intent(in) :: delta
         type(ZMM8r8_t),   intent(in) :: gamma
         type(ZMM8r8_t) :: ay
         type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
         type(ZMM8r8_t), automatic :: ax,t0
         t0  = half.v*gamma
         ax  = fov_x_axis_zmm16r4(H,delta,gamma)
         ay  = ax.v*cos(t0.v)
      end function fov_y_axis_zmm8r8


      !Если рабочая зона сканирования ограничена углом G, то
      !ширина захвата
      !Formula 3, p. 100
     pure elemental function scan_width_r4(H,gamma,theta) result(B)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_r4
         !dir$ forceinline :: scan_width_r4
         real(kind=sp),   intent(in) :: H
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: theta
         real(kind=sp) :: B
         real(kind=sp), automatic :: gam2,th2,t0,t1
         gam2  = 0.5_sp*gamma
         th2   = 0.5_sp*theta
         t0    = tan(gam2)
         t1    = sin(th2)
         B     = (H+H)*t0*t1
      end function scan_width_r4


      pure elemental function scan_width_r8(H,gamma,theta) result(B)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_r8
         !dir$ forceinline :: scan_width_r8
         real(kind=dp),   intent(in) :: H
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: theta
         real(kind=dp) :: B
         real(kind=dp), automatic :: gam2,th2,t0,t1
         gam2  = 0.5_dp*gamma
         th2   = 0.5_dp*theta
         t0    = tan(gam2)
         t1    = sin(th2)
         B     = (H+H)*t0*t1
      end function scan_width_r8


      pure function scan_width_zmm16r4(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_zmm16r4
        !dir$ forceinline :: scan_width_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_zmm16r4
        type(ZMM16r4_t),  intent(in) :: H
        type(ZMM16r4_t),  intent(in) :: gamma
        type(ZMM16r4_t),  intent(in) :: theta
        type(ZMM16r4_t) :: B
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 64 :: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
      end function scan_width_zmm16r4


      pure function scan_width_zmm8r8(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_zmm8r8
        !dir$ forceinline :: scan_width_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_zmm8r8
        type(ZMM8r8_t),  intent(in) :: H
        type(ZMM8r8_t),  intent(in) :: gamma
        type(ZMM8r8_t),  intent(in) :: theta
        type(ZMM8r8_t) :: B
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 64 :: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
      end function scan_width_zmm8r8


      !Плоскопараллельная пластинка, установленная за 
      !объективом, изменяет ход лучей таким образом, что изображение
      ! светящейся точки отодвигается и его положение зависит от угла у
      !между оптической осью и нормалью N к поверхности пластинки
      ! Formula 7,8 p. 106
      pure elemental function refract_shift_r4(i1,delta,alfa,gamma,n) result(l)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: refract_shift_r4
         !dir$ forceinline :: refract_shift_r4
         use mod_fpcompare, only : Compare_Float
         real(kind=sp),   intent(in) :: i1
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alfa
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: l
         real(kind=sp), automatic :: ag,num,den,sin2,sag,t0,t1
         ag  = alfa-gamma
         if(Compare_Float(i1,ag)) then
            sag  = sin(ag)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sag
            den  = n*n-sag
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t2
         else if(alfa==0.0_sp) then
            sag  = sin(gamma)
            t0   = -delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sin2
            den  = n*n-sin2
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t1
         else
            sag  = sin(i1)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sin2
            den  = n*n-sin2
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t1
         end if
      end function refract_shift_r4


      pure elemental function refract_shift_r8(i1,delta,alfa,gamma,n) result(l)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: refract_shift_r8
         !dir$ forceinline :: refract_shift_r8
         use mod_fpcompare, only : Compare_Float
         real(kind=dp),   intent(in) :: i1
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alfa
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: l
         real(kind=dp), automatic :: ag,num,den,sin2,sag,t0,t1
         ag  = alfa-gamma
         if(Compare_Float(i1,ag)) then
            sag  = sin(ag)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sag
            den  = n*n-sag
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t2
         else if(alfa==0.0_dp) then
            sag  = sin(gamma)
            t0   = -delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sin2
            den  = n*n-sin2
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t1
         else
            sag  = sin(i1)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sin2
            den  = n*n-sin2
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t1
         end if
     end function refract_shift_r8

       
     pure function refract_shift_zmm16r4(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_zmm16r4
            !dir$ forceinline ::  refract_shift_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_zmm16r4
            use mod_fpcompare, only : zmm16r4_equalto_zmm16r4
            use mod_vectypes,  only : Mask16_t
            type(ZMM16r4_t),   intent(in) :: i1
            type(ZMM16r4_t),   intent(in) :: delta
            type(ZMM16r4_t),   intent(in) :: alfa
            type(ZMM16r4_t),   intent(in) :: gamma
            type(ZMM16r4_t),   intent(in) :: n
            type(ZMM16r4_t) :: l
            type(ZMM16r4_t), parameter :: one = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t), parameter :: zer = ZMM16r4_t(0.0_sp)
            type(ZMM16r4_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 64 :: ag,num,den,sin2,sag,t0,t1
            type(Mask16_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = zmm16r4_equalto_zmm16r4(i1,ag)
            m2  = zmm16r4_equalto_zmm16r4(alfa,zer) 
            if(all(m1)) then
               sag  = sin(ag.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sag.v
               den  = n.v*n.v*sag.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else if(all(m2)) then
               sag  = sin(gamma.v)
               t0   = -delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else
               sag  = sin(i1.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            end if
       end function refract_shift_zmm16r4


       pure function refract_shift_zmm8r8(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_zmm8r8
            !dir$ forceinline ::  refract_shift_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_zmm8r8
            use mod_fpcompare, only : zmm8r8_equalto_zmm8r8
            use mod_vectypes,  only : Mask16_t
            type(ZMM8r8_t),   intent(in) :: i1
            type(ZMM8r8_t),   intent(in) :: delta
            type(ZMM8r8_t),   intent(in) :: alfa
            type(ZMM8r8_t),   intent(in) :: gamma
            type(ZMM8r8_t),   intent(in) :: n
            type(ZMM8r8_t) :: l
            type(ZMM8r8_t), parameter :: one = ZMM8r8_t(1.0_dp)
            type(ZMM8r8_t), parameter :: zer = ZMM8r8_t(0.0_dp)
            type(ZMM8r8_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 64 :: ag,num,den,sin2,sag,t0,t1
            type(Mask8_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = zmm8r8_equalto_zmm8r8(i1,ag)
            m2  = zmm8r8_equalto_zmm8r8(alfa,zer) 
            if(all(m1)) then
               sag  = sin(ag.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sag.v
               den  = n.v*n.v*sag.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else if(all(m2)) then
               sag  = sin(gamma.v)
               t0   = -delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else
               sag  = sin(i1.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            end if
        end function refract_shift_zmm8r8  

          

          
       

      !Formula 1, p. 108
      subroutine project_xy_axis_r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_xy_axis_r4
         !dir$ forceinline :: project_xy_axis_r4
         real(kind=sp),  intent(in)  :: l
         real(kind=sp),  intent(in)  :: alpha
         real(kind=sp),  intent(out) :: xl
         real(kind=sp),  intent(out) :: yl
         real(kind=sp), automatic :: absl
         absl = abs(l)
         xl = absl*cos(alpha)
         yl = absl*sin(alpha)
      end subroutine project_xy_axis_r4
 

      subroutine project_xy_axis_r8(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_xy_axis_r8
         !dir$ forceinline :: project_xy_axis_r8
         real(kind=dp),  intent(in)  :: l
         real(kind=dp),  intent(in)  :: alpha
         real(kind=dp),  intent(out) :: xl
         real(kind=dp),  intent(out) :: yl
         real(kind=dp), automatic :: absl
         absl = abs(l)
         xl = absl*cos(alpha)
         yl = absl*sin(alpha)
     end subroutine project_xy_axis_r8


     subroutine project_xy_axis_zmm16r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_zmm16r4
         !dir$ forceinline ::  project_xy_axis_zmm16r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_zmm16r4
         type(ZMM16r4_t),  intent(in) :: l
         type(ZMM16r4_t),  intent(in) :: alpha
         type(ZMM16r4_t),  intent(in) :: xl
         type(ZMM16r4_t),  intent(in) :: yl
         type(ZMM16r4_t), automatic :: absl
         !dir$ attributes align : 64 :: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
     end subroutine project_xy_axis_zmm16r4

     
     subroutine project_xy_axis_zmm8r8(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_zmm8r8
         !dir$ forceinline ::  project_xy_axis_zmm8r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_zmm8r8
         type(ZMM8r8_t),  intent(in) :: l
         type(ZMM8r8_t),  intent(in) :: alpha
         type(ZMM8r8_t),  intent(in) :: xl
         type(ZMM8r8_t),  intent(in) :: yl
         type(ZMM8r8_t), automatic :: absl
         !dir$ attributes align : 64 :: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
      end subroutine project_xy_axis_zmm8r8

       
      !Величину смещения луча s вдоль перпендикуляра к 
      !поверхности пластинки
      ! Formula 2, p. 108
      pure elemental function s_shift_r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::s_shift_r4
         !dir$ forceinline :: s_shift_r4
         real(kind=sp),   intent(in) :: l
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp) :: s
         real(kind=sp), automatic :: ag,sag
         ag = alpha-gamma
         sag= sin(ag)
         s  = l/sag
      end function s_shift_r4


      pure elemental function s_shift_r8(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::s_shift_r8
         !dir$ forceinline :: s_shift_r8
         real(kind=dp),   intent(in) :: l
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp) :: s
         real(kind=dp), automatic :: ag,sag
         ag = alpha-gamma
         sag= sin(ag)
         s  = l/sag
      end function s_shift_r8


      pure function s_shift_zmm16r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_zmm16r4
         !dir$ forceinline ::  s_shift_zmm16r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_zmm16r4
         type(ZMM16r4_t),  intent(in) :: l
         type(ZMM16r4_t),  intent(in) :: alpha
         type(ZMM16r4_t),  intent(in) :: gamma
         type(ZMM16r4_t) :: s
         type(ZMM16r4_t), automatic :: ag,sag
         !dir$ attributes align : 64 :: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
      end function s_shift_zmm16r4


      pure function s_shift_zmm8r8(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_zmm8r8
         !dir$ forceinline ::  s_shift_zmm8r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_zmm8r8
         type(ZMM8r8_t),  intent(in) :: l
         type(ZMM8r8_t),  intent(in) :: alpha
         type(ZMM8r8_t),  intent(in) :: gamma
         type(ZMM8r8_t) :: s
         type(ZMM8r8_t), automatic :: ag,sag
         !dir$ attributes align : 64 :: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
      end function s_shift_zmm8r8

      ! Проекции s на оси координат равны
      ! Formula 4, p. 108
      subroutine project_s_xy_r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_r4
         !dir$ forceinline :: project_s_xy_r4
         real(kind=sp),   intent(in)  :: s
         real(kind=sp),   intent(in)  :: gamma
         real(kind=sp),   intent(out) :: xs
         real(kind=sp),   intent(in)  :: ys
         xs = s*cos(gamma)
         ys = s*sin(gamma)
      end subroutine project_s_xy_r4


      subroutine project_s_xy_r8(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_r8
         !dir$ forceinline :: project_s_xy_r8
         real(kind=dp),   intent(in)  :: s
         real(kind=dp),   intent(in)  :: gamma
         real(kind=dp),   intent(out) :: xs
         real(kind=dp),   intent(in)  :: ys
         xs = s*cos(gamma)
         ys = s*sin(gamma)
      end subroutine project_s_xy_r8


      subroutine project_s_xy_zmm16r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_zmm16r4
         !dir$ forceinline ::  project_s_xy_zmm16r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_zmm16r4
         type(ZMM16r4_t),   intent(in)  :: s
         type(ZMM16r4_t),   intent(in)  :: gamma
         type(ZMM16r4_t),   intent(out) :: xs
         type(ZMM16r4_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_zmm16r4 

      subroutine project_s_xy_zmm8r8(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_zmm8r8
         !dir$ forceinline ::  project_s_xy_zmm8r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_zmm8r8
         type(ZMM8r8_t),   intent(in)  :: s
         type(ZMM8r8_t),   intent(in)  :: gamma
         type(ZMM8r8_t),   intent(out) :: xs
         type(ZMM8r8_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_zmm8r8


      ! что расстояния от начала координат О до точек
      ! пересечения лучей, образующих с горизонталью угла ±а, с 
      ! перпендикуляром к пластинке
      ! Formula 1, p. 110
      pure elemental function ray_intercept_pa_r4(delta,alpha,gamma,n) result(sp)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_pa_r4
         !dir$ forceinline :: ray_intercept_pa_r4
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: sp
         real(kind=sp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)-gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sp  = delta*1.0_sp-(num/den)
      end function ray_intercept_pa_r4


      pure elemental function ray_intercept_pa_r8(delta,alpha,gamma,n) result(sp)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_pa_r8
         !dir$ forceinline :: ray_intercept_pa_r8
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: sp
         real(kind=dp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)-gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sp  = delta*1.0_dp-(num/den)
      end function ray_intercept_pa_r8


      pure function ray_intercept_pa_zmm16r4(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_zmm16r4
           !dir$ forceinline ::  ray_intercept_pa_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_zmm16r4
           type(ZMM16r4_t),  intent(in) :: delta
           type(ZMM16r4_t),  intent(in) :: alpha
           type(ZMM16r4_t),  intent(in) :: gamma
           type(ZMM16r4_t),  intent(in) :: n
           type(ZMM16r4_t) :: sp
           type(ZMM16r4_t), parameter :: one = ZMM16r4_t(1.0_sp)
           type(ZMM16r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
      end function ray_intercept_pa_zmm16r4

 
      pure function ray_intercept_pa_zmm8r8(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_zmm8r8
           !dir$ forceinline ::  ray_intercept_pa_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_zmm8r8
           type(ZMM8r8_t),  intent(in) :: delta
           type(ZMM8r8_t),  intent(in) :: alpha
           type(ZMM8r8_t),  intent(in) :: gamma
           type(ZMM8r8_t),  intent(in) :: n
           type(ZMM8r8_t) :: sp
           type(ZMM8r8_t), parameter :: one = ZMM8r8_t(1.0_dp)
           type(ZMM8r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
       end function ray_intercept_pa_zmm8r8


      pure elemental function ray_intercept_na_r4(delta,alpha,gamma,n) result(sn)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_na_r4
         !dir$ forceinline :: ray_intercept_na_r4
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: sn
         real(kind=sp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)+gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sn  = delta*1.0_sp-(num/den)
      end function ray_intercept_na_r4


      pure elemental function ray_intercept_na_r8(delta,alpha,gamma,n) result(sn)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_na_r8
         !dir$ forceinline :: ray_intercept_na_r8
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: sn
         real(kind=dp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)+gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sn  = delta*1.0_dp-(num/den)
      end function ray_intercept_na_r8


      pure function ray_intercept_na_zmm16r4(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_zmm16r4
           !dir$ forceinline ::  ray_intercept_pa_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_zmm16r4
           type(ZMM16r4_t),  intent(in) :: delta
           type(ZMM16r4_t),  intent(in) :: alpha
           type(ZMM16r4_t),  intent(in) :: gamma
           type(ZMM16r4_t),  intent(in) :: n
           type(ZMM16r4_t) :: sn
           type(ZMM16r4_t), parameter :: one = ZMM16r4_t(1.0_sp)
           type(ZMM16r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
      end function ray_intercept_na_zmm16r4
 
 
      pure function ray_intercept_na_zmm8r8(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_zmm8r8
           !dir$ forceinline ::  ray_intercept_na_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_zmm8r8
           type(ZMM8r8_t),  intent(in) :: delta
           type(ZMM8r8_t),  intent(in) :: alpha
           type(ZMM8r8_t),  intent(in) :: gamma
           type(ZMM8r8_t),  intent(in) :: n
           type(ZMM8r8_t) :: sn
           type(ZMM8r8_t), parameter :: one = ZMM8r8_t(1.0_dp)
           type(ZMM8r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
       end function ray_intercept_na_zmm8r8


       ! Formula 3, p. 110
       pure function ray_diff_r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_r4
           !dir$ forceinline :: ray_diff_r4
           real(kind=sp),   intent(in) :: delta
           real(kind=sp),   intent(in) :: alpha
           real(kind=sp),   intent(in) :: gamma
           real(kind=sp),   intent(in) :: n
           real(kind=sp),   intent(in) :: u
           real(kind=sp) :: ds
           real(kind=sp), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           n   = n*n
           u2  = u*0.5_sp
           u2g = u2-gamma
           t2  = sin(u2g)
           su2 = t2*t2
           if(n2>=su2) then
              t3 = (-2.0_sp*delta)/n
              t4 = sin(u2)
              t5 = sin(gamma)
              ds = t3*t4*t5
           else
              t0  = ray_intercept_pa_r4(delta,alpha,gamma,n)
              t1  = ray_intercept_na_r4(delta,alpha,gamma,n)
              ds  = t0-t1
           end if
       end function ray_diff_r4

         
       pure function ray_diff_r8(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_r8
           !dir$ forceinline :: ray_diff_r8
           real(kind=dp),   intent(in) :: delta
           real(kind=dp),   intent(in) :: alpha
           real(kind=dp),   intent(in) :: gamma
           real(kind=dp),   intent(in) :: n
           real(kind=dp),   intent(in) :: u
           real(kind=dp) :: ds
           real(kind=dp), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           n   = n*n
           u2  = u*0.5_dp
           u2g = u2-gamma
           t2  = sin(u2g)
           su2 = t2*t2
           if(n2>=su2) then
              t3 = (-2.0_dp*delta)/n
              t4 = sin(u2)
              t5 = sin(gamma)
              ds = t3*t4*t5
           else
           t0  = ray_intercept_pa_r8(delta,alpha,gamma,n)
           t1  = ray_intercept_na_r8(delta,alpha,gamma,n)
           ds  = t0-t1
        end function ray_diff_r8


        pure function ray_diff_zmm16r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_zmm16r4
           !dir$ forceinline ::  ray_diff_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_zmm16r4
           use mod_fpcompare, only :  zmm16r4_rgt_zmm16r4
           type(ZMM16r4_t),  intent(in) :: delta
           type(ZMM16r4_t),  intent(in) :: alpha
           type(ZMM16r4_t),  intent(in) :: gamma
           type(ZMM16r4_t),  intent(in) :: n
           type(ZMM16r4_t),  intent(in) :: u
           type(ZMM16r4_t) :: ds
           type(ZMM16r4_t), parameter :: two  = ZMM16r4_t(2.0_sp)
           type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
           type(ZMM16r4_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 64 :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask16_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = zmm16r4_rgt_zmm16r4(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_zmm16r4(delta,alpha,gamma,n)
              t1 = ray_intercept_na_zmm16r4(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
        end function ray_diff_zmm16r4


        pure function ray_diff_zmm8r8(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_zmm8r8
           !dir$ forceinline ::  ray_diff_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_zmm8r8
           use mod_fpcompare, only :  zmm8r8_rgt_zmm8r8
           type(ZMM8r8_t),  intent(in) :: delta
           type(ZMM8r8_t),  intent(in) :: alpha
           type(ZMM8r8_t),  intent(in) :: gamma
           type(ZMM8r8_t),  intent(in) :: n
           type(ZMM8r8_t),  intent(in) :: u
           type(ZMM8r8_t) :: ds
           type(ZMM8r8_t), parameter :: two  = ZMM8r8_t(2.0_dp)
           type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
           type(ZMM8r8_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 64 :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask8_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = zmm8r8_rgt_zmm8r8(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_zmm8r8(delta,alpha,gamma,n)
              t1 = ray_intercept_na_zmm8r8(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
        end function ray_diff_zmm8r8

         
        !Поле точек пересечения лучей, преломленных пластинкой,
        !относительно оси Ох (рис. 87) имеет симметрию, поэтому 
        !упростим обозначения и выполним расчет соответствующих 
        !координат на основании
        ! Formula 6,7, p. 111
        subroutine compute_dxdy_r4(alpha,beta,delta,gamma,n,u,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_dxdy_r4
           !dir$ forceinline ::  compute_dxdy_r4
           real(kind=sp),    intent(in)  :: alpha
           real(kind=sp),    intent(in)  :: beta
           real(kind=sp),    intent(in)  :: delta
           real(kind=sp),    intent(in)  :: gamma
           real(kind=sp),    intent(in)  :: n
           real(kind=sp),    intent(in)  :: u
           real(kind=sp),    intent(out) :: dx
           real(kind=sp),    intent(out) :: dy
           real(kind=sp), automatic :: ag,ds,t0,t1,t2
           ag  = alpha+gamma
           ds  = ray_diff_r4(delta,alfa,gamma,n,u)
           t0  = sin(ag)
           t1  = 2.0_sp*sin(alpha)
           t2  = 2.0_sp*cos(alpha)
           dx  = t0/t1*ds
           dy  = t0/t2*ds
        end subroutine compute_dxdy_r4


        subroutine compute_dxdy_r8(alpha,beta,delta,gamma,n,u,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_dxdy_r8
           !dir$ forceinline ::  compute_dxdy_r8
           real(kind=dp),    intent(in)  :: alpha
           real(kind=dp),    intent(in)  :: beta
           real(kind=dp),    intent(in)  :: delta
           real(kind=dp),    intent(in)  :: gamma
           real(kind=dp),    intent(in)  :: n
           real(kind=dp),    intent(in)  :: u
           real(kind=dp),    intent(out) :: dx
           real(kind=dp),    intent(out) :: dy
           real(kind=dp), automatic :: ag,ds,t0,t1,t2
           ag  = alpha+gamma
           ds  = ray_diff_r8(delta,alfa,gamma,n,u)
           t0  = sin(ag)
           t1  = 2.0_dp*sin(alpha)
           t2  = 2.0_dp*cos(alpha)
           dx  = t0/t1*ds
           dy  = t0/t2*ds
       end subroutine compute_dxdy_r8


       subroutine compute_dxdy_zmm16r4(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_zmm16r4
            !dir$ forceinline ::  compute_dxdy_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_zmm16r4
            type(ZMM16r4_t),   intent(in)  :: alpha
            type(ZMM16r4_t),   intent(in)  :: beta
            type(ZMM16r4_t),   intent(in)  :: delta
            type(ZMM16r4_t),   intent(in)  :: gamma
            type(ZMM16r4_t),   intent(in)  :: n
            type(ZMM16r4_t),   intent(in)  :: u
            type(ZMM16r4_t),   intent(out) :: dx
            type(ZMM16r4_t),   intent(out) :: dy
            type(ZMM16r4_t), parameter :: two = ZMM16r4_t(2.0_sp)
            type(ZMM16r4_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_zmm16r4(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
        end subroutine compute_dxdy_zmm16r4


        subroutine compute_dxdy_zmm8r8(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_zmm8r8
            !dir$ forceinline ::  compute_dxdy_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_zmm8r8
            type(ZMM8r8_t),   intent(in)  :: alpha
            type(ZMM8r8_t),   intent(in)  :: beta
            type(ZMM8r8_t),   intent(in)  :: delta
            type(ZMM8r8_t),   intent(in)  :: gamma
            type(ZMM8r8_t),   intent(in)  :: n
            type(ZMM8r8_t),   intent(in)  :: u
            type(ZMM8r8_t),   intent(out) :: dx
            type(ZMM8r8_t),   intent(out) :: dy
            type(ZMM8r8_t), parameter :: two = ZMM8r8_t(2.0_dp)
            type(ZMM8r8_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_zmm8r8(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
        end subroutine compute_dxdy_zmm8r8

        ! Formula 7,8  p. 111
        subroutine compute_xy_r4(alpha,beta,delta,gamma,n,u,x,y)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xy_r4
           !dir$ forceinline ::  compute_xy_r4
           real(kind=sp),    intent(in)  :: alpha
           real(kind=sp),    intent(in)  :: beta
           real(kind=sp),    intent(in)  :: delta
           real(kind=sp),    intent(in)  :: gamma
           real(kind=sp),    intent(in)  :: n
           real(kind=sp),    intent(in)  :: u
           real(kind=sp),    intent(out) :: x
           real(kind=sp),    intent(out) :: y
           real(kind=sp), automatic :: sag,cag,pa,dx,dy,xs,ys
           sag  = sin(gamma)
           cag  = cos(gamma)
           pa   = ray_intercept_pa_r4(delta,alpha,gamma,n)
           xs   = pa*sag
           ys   = pa*cag
           call compute_dxdy_r4(alpha,beta,delta,gamma,n,u,dx,dy)
           x    = xs+dx
           y    = ys+dx
        end subroutine compute_xy_r4

    
        subroutine compute_xy_r8(alpha,beta,delta,gamma,n,u,x,y)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xy_r8
           !dir$ forceinline ::  compute_xy_r8
           real(kind=dp),    intent(in)  :: alpha
           real(kind=dp),    intent(in)  :: beta
           real(kind=dp),    intent(in)  :: delta
           real(kind=dp),    intent(in)  :: gamma
           real(kind=dp),    intent(in)  :: n
           real(kind=dp),    intent(in)  :: u
           real(kind=dp),    intent(out) :: x
           real(kind=dp),    intent(out) :: y
           real(kind=dp), automatic :: sag,cag,pa,dx,dy,xs,ys
           sag  = sin(gamma)
           cag  = cos(gamma)
           pa   = ray_intercept_pa_r8(delta,alpha,gamma,n)
           xs   = pa*sag
           ys   = pa*cag
           call compute_dxdy_r8(alpha,beta,delta,gamma,n,u,dx,dy)
           x    = xs+dx
           y    = ys+dx
        end subroutine compute_xy_r8


        subroutine compute_xy_zmm16r4(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_zmm16r4
            !dir$ forceinline ::  compute_xy_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_zmm16r4
            type(ZMM16r4_t),   intent(in)  :: alpha
            type(ZMM16r4_t),   intent(in)  :: beta
            type(ZMM16r4_t),   intent(in)  :: delta
            type(ZMM16r4_t),   intent(in)  :: gamma
            type(ZMM16r4_t),   intent(in)  :: n
            type(ZMM16r4_t),   intent(in)  :: u
            type(ZMM16r4_t),   intent(out) :: x
            type(ZMM16r4_t),   intent(out) :: y
            type(ZMM16r4_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 64 :: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_zmm16r4(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_zmm16r4(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
        end subroutine compute_xy_zmm16r4


        subroutine compute_xy_zmm8r8(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_zmm8r8
            !dir$ forceinline ::  compute_xy_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_zmm8r8
            type(ZMM8r8_t),   intent(in)  :: alpha
            type(ZMM8r8_t),   intent(in)  :: beta
            type(ZMM8r8_t),   intent(in)  :: delta
            type(ZMM8r8_t),   intent(in)  :: gamma
            type(ZMM8r8_t),   intent(in)  :: n
            type(ZMM8r8_t),   intent(in)  :: u
            type(ZMM8r8_t),   intent(out) :: x
            type(ZMM8r8_t),   intent(out) :: y
            type(ZMM8r8_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 64 :: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_zmm8r8(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_zmm8r8(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
        end subroutine compute_xy_zmm8r8


        subroutine compute_xdyd_r4(gamma,u,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xdyd_r4
           !dir$ forceinline ::  compute_xdyd_r4
           real(kind=sp),  intent(in)  :: gamma
           real(kind=sp),  intent(in)  :: u
           real(kind=sp),  intent(in)  :: n
           real(kind=sp),  intent(out) :: xd
           real(kind=sp),  intent(out) :: yd
           real(kind=sp), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
           cosg = cos(gamma)
           sing = sin(gamma)
           u2   = u*0.5_sp
           n2   = n*n
           u2gs = u2+gamma
           ungd = u2-gamma
           t0   = sin(u2gs)
           sin2s= t0*t0
           t1   = sin(u2gd)
           sin2d= t1*t1
           t2   = 1.0_sp/(4.0_sp*sin(u2))
           t3   = sqrt(n2-sin2s)
           t0   = sin2s/t3
           t4   = sqrt(n2-sin2d)
           t1   = sin2d/t4
           dx   = cosg-t2*(t0+t1)
           t2   = 1.0_sp/(4.0_sp*cos(u2)
           dy   = sing-t2*(t0-t1)
        end subroutine compute_xdyd_r4


       subroutine compute_xdyd_r8(gamma,u,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xdyd_r8
           !dir$ forceinline ::  compute_xdyd_r8
           real(kind=dp),  intent(in)  :: gamma
           real(kind=dp),  intent(in)  :: u
           real(kind=dp),  intent(in)  :: n
           real(kind=dp),  intent(out) :: xd
           real(kind=dp),  intent(out) :: yd
           real(kind=dp), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
           cosg = cos(gamma)
           sing = sin(gamma)
           u2   = u*0.5_dp
           n2   = n*n
           u2gs = u2+gamma
           ungd = u2-gamma
           t0   = sin(u2gs)
           sin2s= t0*t0
           t1   = sin(u2gd)
           sin2d= t1*t1
           t2   = 1.0_dp/(4.0_dp*sin(u2))
           t3   = sqrt(n2-sin2s)
           t0   = sin2s/t3
           t4   = sqrt(n2-sin2d)
           t1   = sin2d/t4
           dx   = cosg-t2*(t0+t1)
           t2   = 1.0_dp/(4.0_dp*cos(u2)
           dy   = sing-t2*(t0-t1)
        end subroutine compute_xdyd_r8


        subroutine compute_xdyd_zmm16r4(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_zmm16r4
            !dir$ forceinline ::  compute_xdyd_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_zmm16r4
            type(ZMM16r4_t),  intent(in)  :: gamma
            type(ZMM16r4_t),  intent(in)  :: u
            type(ZMM16r4_t),  intent(in)  :: n
            type(ZMM16r4_t),  intent(out) :: xd
            type(ZMM16r4_t),  intent(out) :: yd
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t), parameter :: four = ZMM16r4_t(4.0_sp)
            type(ZMM16r4_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
            cosg = cos(gamma.v)
            sing = sin(gamma.v)
            u2   = u.v*half.v
            n2   = n.v*n.v
            u2gs = u2.v+gamma.v
            ungd = u2.v-gamma.v
            t0   = sin(u2gs.v)
            sin2s= t0.v*t0.v
            t1   = sin(u2gd.v)
            sin2d= t1.v*t1.v
            t2   = one.v/(four.v*sin(u2.v))
            t3   = sqrt(n2.v-sin2s.v)
            t0   = sin2s.v/t3.v
            t4   = sqrt(n2.v-sin2d.v)
            t1   = sin2d.v/t4.v
            dx   = cosg.v-t2.v*(t0.v+t1.v)
            t2   = one.v/(four.v*cos(u2.v)
            dy   = sing.v-t2.v*(t0.v-t1.v)
        end subroutine compute_xdyd_zmm16r4


        subroutine compute_xdyd_zmm8r8(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_zmm8r8
            !dir$ forceinline ::  compute_xdyd_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_zmm8r8
            type(ZMM8r8_t),  intent(in)  :: gamma
            type(ZMM8r8_t),  intent(in)  :: u
            type(ZMM8r8_t),  intent(in)  :: n
            type(ZMM8r8_t),  intent(out) :: xd
            type(ZMM8r8_t),  intent(out) :: yd
            type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
            type(ZMM8r8_t), parameter :: one  = ZMM8r8_t(1.0_dp)
            type(ZMM8r8_t), parameter :: four = ZMM8r8_t(4.0_dp)
            type(ZMM8r8_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
            cosg = cos(gamma.v)
            sing = sin(gamma.v)
            u2   = u.v*half.v
            n2   = n.v*n.v
            u2gs = u2.v+gamma.v
            ungd = u2.v-gamma.v
            t0   = sin(u2gs.v)
            sin2s= t0.v*t0.v
            t1   = sin(u2gd.v)
            sin2d= t1.v*t1.v
            t2   = one.v/(four.v*sin(u2.v))
            t3   = sqrt(n2.v-sin2s.v)
            t0   = sin2s.v/t3.v
            t4   = sqrt(n2.v-sin2d.v)
            t1   = sin2d.v/t4.v
            dx   = cosg.v-t2.v*(t0.v+t1.v)
            t2   = one.v/(four.v*cos(u2.v)
            dy   = sing.v-t2.v*(t0.v-t1.v)
        end subroutine compute_xdyd_zmm8r8


        subroutine paraxial_xdyd_r4(gamma,alpha,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  paraxial_xdyd_r4
           !dir$ forceinline ::  paraxial_xdyd_r4 
           real(kind=sp),  intent(in)  :: gamma
           real(kind=sp),  intent(in)  :: alpha
           real(kind=sp),  intent(in)  :: n
           real(kind=sp),  intent(out) :: xd
           real(kind=sp),  intent(out) :: yd
           real(kind=sp), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
           n2    = n*n
           cosg  = cos(gamma)
           cos2g = cos(gamma+gamma)
           sing  = sin(gamma)
           sin4g = sing*sing*sing*sing
           num   = n2*cos2g+sin4g
           den   = (n2-sing*sing)**1.5_sp
           xd    = cosg-num/den
           t0    = sqrt(n2-sing*sing)
           t1    = 1.0_sp-cosg/t0
           yd    = sing*t1
        end subroutine paraxial_xdyd_r4

     
        subroutine paraxial_xdyd_r8(gamma,alpha,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  paraxial_xdyd_r8
           !dir$ forceinline ::  paraxial_xdyd_r8
           real(kind=dp),  intent(in)  :: gamma
           real(kind=dp),  intent(in)  :: alpha
           real(kind=dp),  intent(in)  :: n
           real(kind=dp),  intent(out) :: xd
           real(kind=dp),  intent(out) :: yd
           real(kind=dp), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
           n2    = n*n
           cosg  = cos(gamma)
           cos2g = cos(gamma+gamma)
           sing  = sin(gamma)
           sin4g = sing*sing*sing*sing
           num   = n2*cos2g+sin4g
           den   = (n2-sing*sing)**1.5_dp
           xd    = cosg-num/den
           t0    = sqrt(n2-sing*sing)
           t1    = 1.0_dp-cosg/t0
           yd    = sing*t1
        end subroutine paraxial_xdyd_r8

end module eos_sensor
