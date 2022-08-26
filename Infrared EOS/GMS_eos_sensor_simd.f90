

module eos_sensor_simd


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
 !                         eos_sensor_simd
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !                        This module contains only explicitly vectorized (SIMD)
 !                        versions of many function implemented in eos_sensor (scalar)
 !                        module
 !          History:
 !                        Date: 09-24-2022
 !                        Time: 17:16 GMT+2
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
   use mod_vectypes, only : YMM8r4_t, YMM4r8_t, ZMM16r4_t, ZMM8r8_t
   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_FULLVER =   &
            1000*EOS_SENSOR_SIMD_MAJOR+100*EOS_SENSOR_SIMD_MINOR+10*EOS_SENSOR_SIMD_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_SIMD_CREATE_DATE = "24-08-2022 17:17 +00200 (WED 24 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_SIMD_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_SIMD_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_SIMD_SYNOPSIS    = "EO Sensors characteristics and models explicitly vectorized (SIMD)."

     
     contains

      ! Formula 1, p.54
     !Тогда длина перпендикуляра SN, опущенного из 
     !светящейся точки на плоскость зеркала  
     pure function compute_SN_zmm16r4(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_zmm16r4
        !dir$ attributes forceinline :: compute_SN_zmm16r4
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
        !dir$ attributes forceinline :: compute_SN_zmm8r8
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
     pure function compute_SM_zmm16r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_zmm16r4
        !dir$ attributes forceinline :: compute_SM_zmm16r4
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
        !dir$ attributes forceinline :: compute_SM_zmm8r8
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
     pure function ratio_FH_zmm16r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_zmm16r4
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
        !dir$ attributes forceinline :: ratio_FH_zmm8r8
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
     pure function scan_mirror_ang_zmm16r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_zmm16r4
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
        !dir$ attributes forceinline :: scan_mirror_ang_zmm8r8
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

      !величина расфокусировки
      !Formula 1, p. 59
     pure function defocus_cof_zmm16r4(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_zmm16r4
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
        !dir$ attributes forceinline :: defocus_cof_zmm8r8
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
    pure function circle_dispersion_zmm16r4(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_zmm16r4
        !dir$ attributes forceinline :: circle_dispersion_zmm16r4
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
        !dir$ attributes forceinline :: circle_dispersion_zmm8r8
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
     pure function circ_dispers_diam_zmm16r4(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_zmm16r4
        !dir$ attributes forceinline :: circ_dispers_diam_zmm16r4
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
        !dir$ attributes forceinline :: circ_dispers_diam_zmm8r8
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

      ! СКАНИРОВАНИЕ ЗЕРКАЛОМ, ВРАЩАЮЩИМСЯ
      ! ВОКРУГ ОСИ, НЕПЕРПЕНДИКУЛЯРНОЙ К НЕМУ
      ! Formula 1, p. 100

     pure function fov_x_axis_zmm16r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_zmm16r4
         !dir$ attributes forceinline :: fov_x_axis_zmm16r4
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
         !dir$ attributes forceinline :: fov_x_axis_zmm8r8
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


       !Если рабочая зона сканирования ограничена углом G, то
      !ширина захвата
      !Formula 3, p. 100

     pure function scan_width_zmm16r4(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_zmm16r4
        !dir$ attributes forceinline :: scan_width_zmm16r4
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
        !dir$ attributes forceinline :: scan_width_zmm8r8
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
      pure function refract_shift_zmm16r4(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_zmm16r4
            !dir$ attributes forceinline ::  refract_shift_zmm16r4
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
            !dir$ attributes forceinline ::  refract_shift_zmm8r8
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
    subroutine project_xy_axis_zmm16r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_zmm16r4
         !dir$ attributes forceinline ::  project_xy_axis_zmm16r4
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
         !dir$ attributes forceinline ::  project_xy_axis_zmm8r8
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
     pure function s_shift_zmm16r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_zmm16r4
         !dir$ attributes forceinline ::  s_shift_zmm16r4
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
         !dir$ attributes forceinline ::  s_shift_zmm8r8
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
     subroutine project_s_xy_zmm16r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_zmm16r4
         !dir$ attributes forceinline ::  project_s_xy_zmm16r4
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
         !dir$ attributes forceinline ::  project_s_xy_zmm8r8
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
      pure function ray_intercept_pa_zmm16r4(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_zmm16r4
           !dir$ attributes forceinline ::  ray_intercept_pa_zmm16r4
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
           !dir$ attributes forceinline ::  ray_intercept_pa_zmm8r8
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


     pure function ray_intercept_na_zmm16r4(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_zmm16r4
           !dir$ attributes forceinline ::  ray_intercept_pa_zmm16r4
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
           !dir$ attributes forceinline ::  ray_intercept_na_zmm8r8
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

   pure function ray_diff_zmm16r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_zmm16r4
           !dir$ attributes forceinline ::  ray_diff_zmm16r4
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
           !dir$ attributes forceinline ::  ray_diff_zmm8r8
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

     subroutine compute_dxdy_zmm16r4(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_zmm16r4
            !dir$ attributes forceinline ::  compute_dxdy_zmm16r4
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
            !dir$ attributes forceinline ::  compute_dxdy_zmm8r8
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


     subroutine compute_xy_zmm16r4(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_zmm16r4
            !dir$ attributes forceinline ::  compute_xy_zmm16r4
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
            !dir$ attributes forceinline ::  compute_xy_zmm8r8
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


     subroutine compute_xdyd_zmm16r4(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_zmm16r4
            !dir$ attributes forceinline ::  compute_xdyd_zmm16r4
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
            !dir$ attributes forceinline ::  compute_xdyd_zmm8r8
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

     !СКАНИРОВАНИЕ ВРАЩАЮЩИМИСЯ ОБЪЕКТИВАМИ
     !Formula 1, p. 121
     subroutine fov_axay_zmm16r4(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_zmm16r4
            !dir$ attributes forceinline ::  fov_axay_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_zmm16r4
            type(ZMM16r4_t),  intent(in) :: H
            type(ZMM16r4_t),  intent(in) :: delx
            type(ZMM16r4_t),  intent(in) :: dely
            type(ZMM16r4_t),  intent(in) :: phi
            type(ZMM16r4_t),  intent(out):: ax
            type(ZMM16r4_t),  intent(out):: ay
            type(ZMM16r4_t),  parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t),  parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
      end subroutine fov_axay_zmm16r4
        
        
      subroutine fov_axay_zmm8r8(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_zmm8r8
            !dir$ attributes forceinline ::  fov_axay_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_zmm8r8
            type(ZMM8r8_t),  intent(in) :: H
            type(ZMM8r8_t),  intent(in) :: delx
            type(ZMM8r8_t),  intent(in) :: dely
            type(ZMM8r8_t),  intent(in) :: phi
            type(ZMM8r8_t),  intent(out):: ax
            type(ZMM8r8_t),  intent(out):: ay
            type(ZMM8r8_t),  parameter :: half = ZMM8r8_t(0.5_dp)
            type(ZMM8r8_t),  parameter :: one  = ZMM8r8_t(1.0_dp)
            type(ZMM8r8_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
     end subroutine fov_axay_zmm8r8


     subroutine  fov_dxdy_zmm16r4(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_zmm16r4
            !dir$ attributes forceinline ::  fov_dxdy_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_zmm16r4
            type(ZMM16r4_t),   intent(in) :: x
            type(ZMM16r4_t),   intent(in) :: y
            type(ZMM16r4_t),   intent(in) :: F
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16r4_t),   intent(out):: dx
            type(ZMM16r4_t),   intent(out):: dy
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 64 :: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
      end subroutine fov_dxdy_zmm16r4
        
        
      subroutine  fov_dxdy_zmm8r8(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_zmm8r8
            !dir$ attributes forceinline ::  fov_dxdy_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_zmm8r8
            type(ZMM8r8_t),   intent(in) :: x
            type(ZMM8r8_t),   intent(in) :: y
            type(ZMM8r8_t),   intent(in) :: F
            type(ZMM8r8_t),   intent(in) :: phi
            type(ZMM8r8_t),   intent(out):: dx
            type(ZMM8r8_t),   intent(out):: dy
            type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
            type(ZMM8r8_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 64 :: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
     end subroutine fov_dxdy_zmm8r8

    
     subroutine volt_impulse_uxuy_zmm16r4(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_zmm16r4
            !dir$ attributes forceinline ::  volt_impulse_uxuy_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_zmm16r4
            type(ZMM16r4_t),   intent(in) :: u
            type(ZMM16r4_t),   intent(in) :: om1
            type(ZMM16r4_t),   intent(in) :: om2
            type(ZMM16r4_t),   intent(in) :: t
            type(ZMM16r4_t),   intent(out):: ux
            type(ZMM16r4_t),   intent(out):: uy
            type(ZMM16r4_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
       end subroutine volt_impulse_uxuy_zmm16r4


       subroutine volt_impulse_uxuy_zmm8r8(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_zmm8r8
            !dir$ attributes forceinline ::  volt_impulse_uxuy_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_zmm8r8
            type(ZMM8r8_t),   intent(in) :: u
            type(ZMM8r8_t),   intent(in) :: om1
            type(ZMM8r8_t),   intent(in) :: om2
            type(ZMM8r8_t),   intent(in) :: t
            type(ZMM8r8_t),   intent(out):: ux
            type(ZMM8r8_t),   intent(out):: uy
            type(ZMM8r8_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
     end subroutine volt_impulse_uxuy_zmm8r8


       
      
        
 
     subroutine const_flux_spectr_unroll_16x_ymm8r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_ymm8r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM8r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM8r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 32 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM8r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 32 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(YMM8r4_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 32 :: f8,f9,f10,f11,f12,f13,f14,f15
           type(YMM8r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 32 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM8r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 32 :: c8,c9,c10,c11,c12,c13,c14,c15
           type(YMM8r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(YMM8r4_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 32 :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(YMM8r4_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<8) then
              return
           else if(n==8) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>8 .and. n<=128) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),8
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>128) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+56+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+64+ii)
                    c8.v(ii)       = twopi.v*f8.v*hT.v
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v-(f8.v*hT.v*f8.v*hT.v)
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+72+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+72+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+80+ii)
                    c10.v(ii)      = twopi.v*f10.v*hT.v
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v-(f10.v*hT.v*f10.v*hT.v)
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+88+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v-(f11.v*hT.v*f11.v*hT.v)
                    Phi0f(i+88+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+96+ii)
                    c12.v(ii)      = twopi.v*f12.v*hT.v
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v-(f12.v*hT.v*f12.v*hT.v)
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+104+ii)
                    c13.v(ii)      = twopi.v*f13.v*hT.v
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v-(f13.v*hT.v*f13.v*hT.v)
                    Phi0f(i+104+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+112+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v-(f14.v*hT.v*f14.v*hT.v)
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+120+ii)
                    c15.v(ii)      = twopi.v*f15.v*hT.v
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v-(f15.v*hT.v*f15.v*hT.v)
                    Phi0f(i+120+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_ymm8r4


       subroutine const_flux_spectr_unroll_8x_ymm8r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_ymm8r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM8r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM8r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 32 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(YMM8r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 32 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM8r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(YMM8r4_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<8) then
              return
           else if(n==8) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>8 .and. n<=64) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),8
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>64) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+56+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
              
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_8x_ymm8r4



      subroutine const_flux_spectr_unroll_4x_ymm8r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_ymm8r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM8r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3
           type(YMM8r4_t) :: f0,f1,f2,f3
           !dir$ attributes align : 32 :: f0,f1,f2,f3
           type(YMM8r4_t) :: c0,c1,c2,c3
           !dir$ attributes align : 32 :: c0,c1,c2,c3
           type(YMM8r4_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3
           type(YMM8r4_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<8) then
              return
           else if(n==8) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>8 .and. n<=32) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),8
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>32) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                               
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_4x_ymm8r4


       subroutine const_flux_spectr_unroll_16x_ymm4r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_ymm4r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_ymm4r8
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(YMM4r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM4r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 32 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM4r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 32 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(YMM4r8_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 32 :: f8,f9,f10,f11,f12,f13,f14,f15
           type(YMM4r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 32 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM4r8_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 32 :: c8,c9,c10,c11,c12,c13,c14,c15
           type(YMM4r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(YMM4r8_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 32 :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(YMM4r8_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=64) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>64) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+28+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+32+ii)
                    c8.v(ii)       = twopi.v*f8.v*hT.v
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v-(f8.v*hT.v*f8.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+36+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+36+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+40+ii)
                    c10.v(ii)      = twopi.v*f10.v*hT.v
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v-(f10.v*hT.v*f10.v*hT.v)
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+44+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v-(f11.v*hT.v*f11.v*hT.v)
                    Phi0f(i+44+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+48+ii)
                    c12.v(ii)      = twopi.v*f12.v*hT.v
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v-(f12.v*hT.v*f12.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+52+ii)
                    c13.v(ii)      = twopi.v*f13.v*hT.v
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v-(f13.v*hT.v*f13.v*hT.v)
                    Phi0f(i+52+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+56+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v-(f14.v*hT.v*f14.v*hT.v)
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+60+ii)
                    c15.v(ii)      = twopi.v*f15.v*hT.v
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v-(f15.v*hT.v*f15.v*hT.v)
                    Phi0f(i+60+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_ymm8r4


      subroutine const_flux_spectr_unroll_8x_ymm4r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_ymm4r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_ymm4r8
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(YMM4r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM4r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 32 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(YMM4r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 32 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM4r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(YMM4r8_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=32) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>32) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+28+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                   
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_8x_ymm8r4


       
      subroutine const_flux_spectr_unroll_4x_ymm4r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_ymm4r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_ymm4r8
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(YMM4r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3
           type(YMM4r8_t) :: f0,f1,f2,f3
           !dir$ attributes align : 32 :: f0,f1,f2,f3
           type(YMM4r8_t) :: c0,c1,c2,c3
           !dir$ attributes align : 32 :: c0,c1,c2,c3
           type(YMM4r8_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3
           type(YMM4r8_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=16) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>16) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(3)),16
                  call mm_prefetch(freq(i*16),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_4x_ymm8r4


       subroutine const_flux_spectr_unroll_16x_zmm16r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_zmm16r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM16r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM16r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(ZMM16r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM16r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 64 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM16r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 64 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(ZMM16r4_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 64 :: f8,f9,f10,f11,f12,f13,f14,f15
           type(ZMM16r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 64 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM16r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 64 :: c8,c9,c10,c11,c12,c13,c14,c15
           type(ZMM16r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(ZMM16r4_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 64 :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(ZMM16r4_t) :: hT,Phi0T
           !dir$ attributes align : 64 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<16) then
              return
           else if(n==16) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>16 .and. n<=256) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),16
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>256) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),256
                  call mm_prefetch(freq(i*256),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+64+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+80+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+96+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+112+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+128+ii)
                    c8.v(ii)       = twopi.v*f8.v*hT.v
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v-(f8.v*hT.v*f8.v*hT.v)
                    Phi0f(i+128+ii)= Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+144+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+144+ii)= Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+160+ii)
                    c10.v(ii)      = twopi.v*f10.v*hT.v
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v-(f10.v*hT.v*f10.v*hT.v)
                    Phi0f(i+160+ii)= Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+176+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v-(f11.v*hT.v*f11.v*hT.v)
                    Phi0f(i+176+ii)= Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+192+ii)
                    c12.v(ii)      = twopi.v*f12.v*hT.v
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v-(f12.v*hT.v*f12.v*hT.v)
                    Phi0f(i+192+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+208+ii)
                    c13.v(ii)      = twopi.v*f13.v*hT.v
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v-(f13.v*hT.v*f13.v*hT.v)
                    Phi0f(i+208+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+224+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v-(f14.v*hT.v*f14.v*hT.v)
                    Phi0f(i+224+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+240+ii)
                    c15.v(ii)      = twopi.v*f15.v*hT.v
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v-(f15.v*hT.v*f15.v*hT.v)
                    Phi0f(i+240+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_zmm16r4


       subroutine const_flux_spectr_unroll_8x_zmm16r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_zmm16r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM16r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM16r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(ZMM16r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM16r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 64 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(ZMM16r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 64 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM16r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(ZMM16r4_t) :: hT,Phi0T
           !dir$ attributes align : 64 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<16) then
              return
           else if(n==16) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>16 .and. n<=128) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),16
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>128) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+64+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+80+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+96+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+112+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_8x_zmm16r4


       subroutine const_flux_spectr_unroll_4x_zmm16r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_zmm16r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM16r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM16r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(ZMM16r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3
           type(ZMM16r4_t) :: f0,f1,f2,f3
           !dir$ attributes align : 64 :: f0,f1,f2,f3
           type(ZMM16r4_t) :: c0,c1,c2,c3
           !dir$ attributes align : 64 :: c0,c1,c2,c3
           type(ZMM16r4_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3
           type(ZMM16r4_t) :: hT,Phi0T
           !dir$ attributes align : 64 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<16) then
              return
           else if(n==16) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>16 .and. n<=64) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),16
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>64) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(15)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,15
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_4x_zmm16r4


       subroutine const_flux_spectr_unroll_16x_zmm8r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_zmm8r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM8r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM8r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(ZMM8r8_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(ZMM8r8_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM8r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 64 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM8r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 64 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(ZMM8r8_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 64 :: f8,f9,f10,f11,f12,f13,f14,f15
           type(ZMM8r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 64 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM8r8_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 64 :: c8,c9,c10,c11,c12,c13,c14,c15
           type(ZMM8r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(ZMM8r8_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 64 :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(ZMM8r8_t) :: hT,Phi0T
           !dir$ attributes align : 64 :: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<8) then
              return
           else if(n==8) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>8 .and. n<=128) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),8
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>128) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+64+ii)
                    c8.v(ii)       = twopi.v*f8.v*hT.v
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v-(f8.v*hT.v*f8.v*hT.v)
                    Phi0f(i+64+ii)= Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+72+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+72+ii)= Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+80+ii)
                    c10.v(ii)      = twopi.v*f10.v*hT.v
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v-(f10.v*hT.v*f10.v*hT.v)
                    Phi0f(i+80+ii)= Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+88+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v-(f11.v*hT.v*f11.v*hT.v)
                    Phi0f(i+88+ii)= Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+96+ii)
                    c12.v(ii)      = twopi.v*f12.v*hT.v
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v-(f12.v*hT.v*f12.v*hT.v)
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+104+ii)
                    c13.v(ii)      = twopi.v*f13.v*hT.v
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v-(f13.v*hT.v*f13.v*hT.v)
                    Phi0f(i+104+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+112+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v-(f14.v*hT.v*f14.v*hT.v)
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+120+ii)
                    c15.v(ii)      = twopi.v*f15.v*hT.v
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v-(f15.v*hT.v*f15.v*hT.v)
                    Phi0f(i+120+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_zmm8r8


       



       






       


























end module eos_sensor_simd
