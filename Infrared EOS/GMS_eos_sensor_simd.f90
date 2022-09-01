

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


     !
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc0 = [1.0_sp,2.0_sp,3.0_sp,4.0_sp, &
                                                              5.0_sp,6.0_sp,7.0_sp,8.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc1 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                              13.0_sp,14.0_sp,15.0_sp,16.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc2 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                              21.0_sp,22.0_sp,23.0_sp,24.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc3 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                              29.0_sp,30.0_sp,31.0_sp,32.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc4 = [33.0_sp,34.0_sp,35.0_sp,36.0_sp, &
                                                              37.0_sp,38.0_sp,39.0_sp,40.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc5 = [41.0_sp,42.0_sp,43.0_sp,44.0_sp, &
                                                              45.0_sp,46.0_sp,47.0_sp,48.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc6 = [49.0_sp,50.0_sp,51.0_sp,52.0_sp, &
                                                              53.0_sp,54.0_sp,55.0_sp,56.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc7 = [57.0_sp,58.0_sp,59.0_sp,60.0_sp, &
                                                              61.0_sp,62.0_sp,63.0_sp,64.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc8 = [65.0_sp,66.0_sp,67.0_sp,68.0_sp, &
                                                              69.0_sp,70.0_sp,71.0_sp,72.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc9 = [73.0_sp,74.0_sp,75.0_sp,76.0_sp, &
                                                              77.0_sp,78.0_sp,79.0_sp,80.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc10= [81.0_sp,82.0_sp,83.0_sp,84.0_sp, &
                                                              85.0_sp,86.0_sp,87.0_sp,88.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc11= [89.0_sp,90.0_sp,91.0_sp,92.0_sp, &
                                                              93.0_sp,94.0_sp,95.0_sp,96.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc12= [97.0_sp,98.0_sp,99.0_sp,100.0_sp, &
                                                              101.0_sp,102.0_sp,103.0_sp,104.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc13= [105.0_sp,106.0_sp,107.0_sp,108.0_sp, &
                                                              109.0_sp,110.0_sp,111.0_sp,112.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc14= [113.0_sp,114.0_sp,115.0_sp,116.0_sp, &
                                                              117.0_sp,118.0_sp,119.0_sp,120.0_sp]
     real(kind=sp), dimension(0:7), parameter :: ymm8vinc15= [121.0_sp,122.0_sp,123.0_sp,124.0_sp, &
                                                              125.0_sp,126.0_sp,127.0_sp,128.0_sp]
     
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc0 = [1.0_dp,2.0_dp,3.0_dp,4.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc1 = [5.0_dp,6.0_dp,7.0_dp,8.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc2 = [9.0_dp,10.0_dp,11.0_dp,12.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc3 = [13.0_dp,14.0_dp,15.0_dp,16.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc4 = [17.0_dp,18.0_dp,19.0_dp,20.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc5 = [21.0_dp,22.0_dp,23.0_dp,24.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc6 = [25.0_dp,26.0_dp,27.0_dp,28.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc7 = [29.0_dp,30.0_dp,31.0_dp,32.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc8 = [33.0_dp,34.0_dp,35.0_dp,36.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc9 = [37.0_dp,38.0_dp,39.0_dp,40.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc10= [41.0_dp,42.0_dp,43.0_dp,44.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc11= [45.0_dp,46.0_dp,47.0_dp,48.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc12= [49.0_dp,50.0_dp,51.0_dp,52.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc13= [53.0_dp,54.0_dp,55.0_dp,56.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc14= [57.0_dp,58.0_dp,59.0_dp,60.0_dp]
     real(kind=dp), dimension(0:3), parameter :: ymm4vinc15= [61.0_dp,62.0_dp,63.0_dp,64.0_dp]

     real(kind=dp), dimension(0:15), parameter :: zmm16vinc0 = [1.0_sp,2.0_sp,3.0_sp,4.0_sp,   &
		                                                5.0_sp,6.0_sp,7.0_sp,8.0_sp,   &
							        9.0_sp,10.0_sp,11.0_sp,12.0_sp,&
							        13.0_sp,14.0_sp,15.0_sp,16.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc1 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
		                                                21.0_sp,22.0_sp,23.0_sp,24.0_sp, &
							        25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
							        29.0_sp,30.0_sp,31.0_sp,32.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc2 = [33.0_sp,34.0_sp,35.0_sp,36.0_sp, &
		                                                37.0_sp,38.0_sp,39.0_sp,40.0_sp, &
							        41.0_sp,42.0_sp,43.0_sp,44.0_sp, &
							        45.0_sp,46.0_sp,47.0_sp,48.0_sp]
  
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc3 = [49.0_sp,50.0_sp,51.0_sp,52.0_sp, &
		                                                53.0_sp,54.0_sp,55.0_sp,56.0_sp, &
							        57.0_sp,58.0_sp,59.0_sp,60.0_sp, &
							        61.0_sp,62.0_sp,63.0_sp,64.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc4 = [65.0_sp,66.0_sp,67.0_sp,68.0_sp, &
                                                                69.0_sp,70.0_sp,71.0_sp,72.0_sp, &
                                                                73.0_sp,74.0_sp,75.0_sp,76.0_sp, &
                                                                77.0_sp,78.0_sp,79.0_sp,80.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc5 = [81.0_sp,82.0_sp,83.0_sp,84.0_sp, &
                                                                85.0_sp,86.0_sp,87.0_sp,88.0_sp, &
                                                                89.0_sp,90.0_sp,91.0_sp,92.0_sp, &
                                                                93.0_sp,94.0_sp,95.0_sp,96.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc6 = [97.0_sp,98.0_sp,99.0_sp,100.0_sp, &
                                                                101.0_sp,102.0_sp,103.0_sp,104.0_sp, &
                                                                105.0_sp,106.0_sp,107.0_sp,108.0_sp, &
                                                                109.0_sp,110.0_sp,111.0_sp,112.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc7 = [113.0_sp,114.0_sp,115.0_sp,116.0_sp, &
                                                                117.0_sp,118.0_sp,119.0_sp,120.0_sp, &
                                                                121.0_sp,122.0_sp,123.0_sp,124.0_sp, &
                                                                125.0_sp,126.0_sp,127.0_sp,128.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc8 = [129.0_sp,130.0_sp,131.0_sp,132.0_sp, &
                                                                133.0_sp,134.0_sp,135.0_sp,136.0_sp, &
                                                                137.0_sp,138.0_sp,139.0_sp,140.0_sp, &
                                                                141.0_sp,142.0_sp,143.0_sp,144.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc9 = [145.0_sp,146.0_sp,147.0_sp,148.0_sp, &
                                                                149.0_sp,150.0_sp,151.0_sp,152.0_sp, &
                                                                153.0_sp,154.0_sp,155.0_sp,156.0_sp, &
                                                                157.0_sp,158.0_sp,159.0_sp,160.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc10= [161.0_sp,162.0_sp,163.0_sp,164.0_sp, &
                                                                165.0_sp,166.0_sp,167.0_sp,168.0_sp, &
                                                                169.0_sp,170.0_sp,171.0_sp,172.0_sp, &
                                                                173.0_sp,174.0_sp,175.0_sp,176.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc11= [177.0_sp,178.0_sp,179.0_sp,180.0_sp,  &
                                                                181.0_sp,182.0_sp,183.0_sp,184.0_sp,  &
                                                                185.0_sp,186.0_sp,187.0_sp,188.0_sp,  &
                                                                189.0_sp,190.0_sp,191.0_sp,192.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc12= [193.0_sp,194.0_sp,195.0_sp,196.0_sp,  &
                                                                197.0_sp,198.0_sp,199.0_sp,200.0_sp, &
                                                                201.0_sp,202.0_sp,203.0_sp,204.0_sp, &
                                                                205.0_sp,206.0_sp,207.0_sp,208.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc13= [209.0_sp,210.0_sp,211.0_sp,212.0_sp, &
                                                                213.0_sp,214.0_sp,215.0_sp,216.0_sp, &
                                                                217.0_sp,218.0_sp,219.0_sp,220.0_sp, &
                                                                221.0_sp,222.0_sp,223.0_sp,224.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc14= [225.0_sp,226.0_sp,227.0_sp,228.0_sp, &
                                                                229.0_sp,230.0_sp,231.0_sp,232.0_sp, &
                                                                233.0_sp,234.0_sp,235.0_sp,236.0_sp, &
                                                                237.0_sp,238.0_sp,239.0_sp,240.0_sp]
     real(kind=dp), dimension(0:15), parameter :: zmm16vinc15= [241.0_sp,242.0_sp,243.0_sp,244.0_sp, &
                                                                245.0_sp,246.0_sp,247.0_sp,248.0_sp, &
                                                                249.0_sp,250.0_sp,251.0_sp,252.0_sp, &
                                                                253.0_sp,254.0_sp,255.0_sp,256.0_sp]
     
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc0 = [1.0_dp,2.0_dp,3.0_dp,4.0_dp, &
                                                              5.0_dp,6.0_dp,7.0_dp,8.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc1 = [9.0_dp,10.0_dp,11.0_dp,12.0_dp, &
                                                              13.0_dp,14.0_dp,15.0_dp,16.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc2 = [17.0_dp,18.0_dp,19.0_dp,20.0_dp, &
                                                              21.0_dp,22.0_dp,23.0_dp,24.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc3 = [25.0_dp,26.0_dp,27.0_dp,28.0_dp, &
                                                              29.0_dp,30.0_dp,31.0_dp,32.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc4 = [33.0_dp,34.0_dp,35.0_dp,36.0_dp, &
                                                              37.0_dp,38.0_dp,39.0_dp,40.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc5 = [41.0_dp,42.0_dp,43.0_dp,44.0_dp, &
                                                              45.0_dp,46.0_dp,47.0_dp,48.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc6 = [49.0_dp,50.0_dp,51.0_dp,52.0_dp, &
                                                              53.0_dp,54.0_dp,55.0_dp,56.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc7 = [57.0_dp,58.0_dp,59.0_dp,60.0_dp, &
                                                              61.0_dp,62.0_dp,63.0_dp,64.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc8 = [65.0_dp,66.0_dp,67.0_dp,68.0_dp, &
                                                              69.0_dp,70.0_dp,71.0_dp,72.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc9 = [73.0_dp,74.0_dp,75.0_dp,76.0_dp, &
                                                              77.0_dp,78.0_dp,79.0_dp,80.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc10= [81.0_dp,82.0_dp,83.0_dp,84.0_dp, &
                                                              85.0_dp,86.0_dp,87.0_dp,88.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc11= [89.0_dp,90.0_dp,91.0_dp,92.0_dp, &
                                                              93.0_dp,94.0_dp,95.0_dp,96.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc12= [97.0_dp,98.0_dp,99.0_dp,100.0_dp, &
                                                              101.0_dp,102.0_dp,103.0_dp,104.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc13= [105.0_dp,106.0_dp,107.0_dp,108.0_dp, &
                                                              109.0_dp,110.0_dp,111.0_dp,112.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc14= [113.0_dp,114.0_dp,115.0_dp,116.0_dp, &
                                                              117.0_dp,118.0_dp,119.0_dp,120.0_dp]
     real(kind=dp), dimension(0:7), parameter :: zmm8vinc15= [121.0_dp,122.0_dp,123.0_dp,124.0_dp, &
                                                              125.0_dp,126.0_dp,127.0_dp,128.0_dp]
    

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

  
     pure function compute_SN_ymm8r4(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_ymm8r4
        !dir$ attributes forceinline :: compute_SN_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  intent(in), optional :: phi
        type(YMM8r4_t),  intent(in), optional :: gamma
        type(YMM8r4_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_ymm8r4

     
     pure function compute_SN_ymm4r8(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_ymm4r8
        !dir$ attributes forceinline :: compute_SN_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  intent(in), optional :: phi
        type(YMM4r8_t),  intent(in), optional :: gamma
        type(YMM4r8_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_ymm4r8


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
        type(ZMM8r8_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_zmm8r8(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_zmm8r8


     !AVX/AVX2 versions
     pure function compute_SM_ymm8r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_ymm8r4
        !dir$ attributes forceinline :: compute_SM_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  intent(in), optional :: phi
        type(YMM8r4_t),  intent(in), optional :: gamma
        type(YMM8r4_t) :: SM
        type(YMM8r4_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_ymm8r4(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_ymm8r4


     pure function compute_SM_ymm4r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_ymm4r8
        !dir$ attributes forceinline :: compute_SM_ymmyr8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  intent(in), optional :: phi
        type(YMM4r8_t),  intent(in), optional :: gamma
        type(YMM4r8_t) :: SM
        type(YMM4r8_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_ymm4r8(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_ymm4r8


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

     !AVX/AVX2 versions.
     pure function ratio_FH_ymm8r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_ymm8r4
        type(YMM8r4_t),  intent(in) :: psi
        type(YMM8r4_t),  intent(in) :: phi
        type(YMM8r4_t) :: FH
        type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
        type(YMM8r4_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_ymm8r4


     pure function ratio_FH_ymm4r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_ymm4r8
        type(YMM4r8_t),  intent(in) :: psi
        type(YMM4r8_t),  intent(in) :: phi
        type(YMM4r8_t) :: FH
        type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
        type(YMM4r8_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_ymm4r8

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


     !AVX/AVX2 versions
     pure function scan_mirror_ang_ymm8r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_ymm8r4
        type(YMM8r4_t),  intent(in) :: gam0
        type(YMM8r4_t),  intent(in) :: psi
        type(YMM8r4_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(YMM8r4_t) :: gamma
        type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
        type(YMM8r4_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_ymm8r4(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_ymm8r4(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_ymm8r4


     pure function scan_mirror_ang_ymm4r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_ymm4r8
        type(YMM4r8_t),  intent(in) :: gam0
        type(YMM4r8_t),  intent(in) :: psi
        type(YMM4r8_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(YMM4r8_t) :: gamma
        type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
        type(YMM4r8_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_ymm4r8(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_ymm4r8(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_ymm4r8


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


    !AVX/AVX2 versions
    pure function defocus_cof_ymm8r4(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_ymm8r4
        !dir$ attributes forceinline :: defocus_cof_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_ymm8r4
        type(YMM8r4_t),  intent(in) :: l2
        type(YMM8r4_t),  intent(in) :: alpha
        type(YMM8r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(YMM8r4_t) :: dc
        type(YMM8r4_t), automatic :: cos2a,icos
        type(YMM8r4_t), parameter :: one = YMM8r4_t(1.0_sp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_ymm8r4


    pure function defocus_cof_ym4r8(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_ymm4r8
        !dir$ attributes forceinline :: defocus_cof_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_ymm4r8
        type(YMM4r8_t),   intent(in) :: l2
        type(YMM4r8_t),   intent(in) :: alpha
        type(YMM4r8_t),   intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(YMM4r8_t) :: dc
        type(YMM4r8_t), automatic :: cos2a,icos
        type(YMM4r8_t), parameter :: one = YMM4r8_t(1.0_dp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_ymm4r8

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


     !AVX/AVX2 versions
     pure function circle_dispersion_ymm8r4(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_ymm8r4
        !dir$ attributes forceinline :: circle_dispersion_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_ymm8r4
        type(YMM8r4_t),  intent(in) :: d
        type(YMM8r4_t),  intent(in) :: l1
        type(YMM8r4_t),  intent(in) :: l2
        type(YMM8r4_t),  intent(in) :: alpha
        type(YMM8r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(YMM8r4_t) :: rho
        type(YMM8r4_t), automatic :: t0,t1
        !dir$ attributes align : 32 :: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_ymm8r4(l2,alpha,O,inf)
        rho= t0.v*t1.v
    end function circle_dispersion_ymm8r4


    pure function circle_dispersion_ymm4r8(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_ymm4r8
        !dir$ attributes forceinline :: circle_dispersion_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_ymm4r8
        type(YMM4r8_t),  intent(in) :: d
        type(YMM4r8_t),  intent(in) :: l1
        type(YMM4r8_t),  intent(in) :: l2
        type(YMM4r8_t),  intent(in) :: alpha
        type(YMM4r8_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(YMM4r8_t) :: rho
        type(YMM4r8_t), automatic :: t0,t1
        !dir$ attributes align : 32 :: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_ymm4r8(l2,alpha,O,inf)
        rho= t0.v*t1.v
     end function circle_dispersion_ymm4r8


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


     !AVX/AVX2 versions
     pure function circ_dispers_diam_ymm8r4(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_ymm8r4
        !dir$ attributes forceinline :: circ_dispers_diam_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_ymm8r4
        type(YMM8r4_t),     intent(in) :: l1
        type(YMM8r4_t),     intent(in) :: l2
        type(YMM8r4_t),     intent(in) :: alpha
        type(YMM8r4_t),     intent(in) :: O
        logical(kind=i4),    intent(in) :: inf
        type(YMM8r4_t) :: ratio
        type(YMM8r4_t), automatic :: t0,t1
        !dir$ attributes align : 32 :: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_ymm8r4(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_ymm8r4


     pure function circ_dispers_diam_ymm4r8(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_ymm4r8
        !dir$ attributes forceinline :: circ_dispers_diam_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_ymm4r8
        type(YMM4r8_t),     intent(in) :: l1
        type(YMM4r8_t),     intent(in) :: l2
        type(YMM4r8_t),     intent(in) :: alpha
        type(YMM4r8_t),     intent(in) :: O
        logical(kind=i4),   intent(in) :: inf
        type(YMM4r8_t) :: ratio
        type(YMM4r8_t), automatic :: t0,t1
        !dir$ attributes align : 32 :: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_ymm4r8(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_ymm4r8

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


      !AVX/AVX2 versions.
      pure function fov_x_axis_ymm8r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_ymm8r4
         !dir$ attributes forceinline :: fov_x_axis_ymm8r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_ymm8r4
         type(YMM8r4_t),   intent(in) :: H
         type(YMM8r4_t),   intent(in) :: delta
         type(YMM8r4_t),   intent(in) :: gamma
         type(YMM8r4_t) :: ax
         type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
         type(YMM8r4_t), automatic :: gamm2,tdel
         !dir$ attributes align : 32 :: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
      end function fov_x_axis_ymm8r4


     pure function fov_x_axis_ymm4r8(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_ymm4r8
         !dir$ attributes forceinline :: fov_x_axis_ymm4r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_ymm4r8
         type(YMM4r8_t),   intent(in) :: H
         type(YMM4r8_t),   intent(in) :: delta
         type(YMM4r8_t),   intent(in) :: gamma
         type(YMM4r8_t) :: ax
         type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
         type(YMM4r8_t), automatic :: gamm2,tdel
         !dir$ attributes align : 32 :: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
      end function fov_x_axis_ymm4r8

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

      !AVX/AVX2 versions
      pure function scan_width_ymm8r4(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_ymm8r4
        !dir$ attributes forceinline :: scan_width_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_ymm8r4
        type(YMM8r4_t),  intent(in) :: H
        type(YMM8r4_t),  intent(in) :: gamma
        type(YMM8r4_t),  intent(in) :: theta
        type(YMM8r4_t) :: B
        type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
        type(YMM8r4_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 32 :: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
      end function scan_width_ymm8r4


      pure function scan_width_ymm4r8(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_ymm4r8
        !dir$ attributes forceinline :: scan_width_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_ymm4r8
        type(YMM4r8_t),  intent(in) :: H
        type(YMM4r8_t),  intent(in) :: gamma
        type(YMM4r8_t),  intent(in) :: theta
        type(YMM4r8_t) :: B
        type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
        type(YMM4r8_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 32 :: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
      end function scan_width_ymm4r8 

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


     !AVX/AVX2 version
     pure function refract_shift_ymm8r4(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_ymm8r4
            !dir$ attributes forceinline ::  refract_shift_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_ymm8r4
            use mod_fpcompare, only : ymm8r4_equalto_ymm8r4
            use mod_vectypes,  only : Mask8_t
            type(YMM8r4_t),   intent(in) :: i1
            type(YMM8r4_t),   intent(in) :: delta
            type(YMM8r4_t),   intent(in) :: alfa
            type(YMM8r4_t),   intent(in) :: gamma
            type(YMM8r4_t),   intent(in) :: n
            type(YMM8r4_t) :: l
            type(YMM8r4_t), parameter :: one = YMM8r4_t(1.0_sp)
            type(YMM8r4_t), parameter :: zer = YMM8r4_t(0.0_sp)
            type(YMM8r4_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 32 :: ag,num,den,sin2,sag,t0,t1
            type(Mask8_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = ymm8r4_equalto_ymm8r4(i1,ag)
            m2  = ymm8r4_equalto_ymm8r4(alfa,zer) 
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
       end function refract_shift_ymm8r4


       pure function refract_shift_ymm4r8(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_ymm4r8
            !dir$ attributes forceinline ::  refract_shift_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_ymm4r8
            use mod_fpcompare, only : ymm4r8_equalto_ymm4r8
            use mod_vectypes,  only : Mask4_t
            type(YMM4r8_t),   intent(in) :: i1
            type(YMM4r8_t),   intent(in) :: delta
            type(YMM4r8_t),   intent(in) :: alfa
            type(YMM4r8_t),   intent(in) :: gamma
            type(YMM4r8_t),   intent(in) :: n
            type(YMM4r8_t) :: l
            type(YMM4r8_t), parameter :: one = YMM4r8_t(1.0_dp)
            type(YMM4r8_t), parameter :: zer = YMM4r8_t(0.0_dp)
            type(YMM4r8_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 32 :: ag,num,den,sin2,sag,t0,t1
            type(Mask4_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = ymm4r8_equalto_ymm4r8(i1,ag)
            m2  = ymm4r8_equalto_ymm4r8(alfa,zer) 
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
     end function refract_shift_ymm4r8  



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


     !AVX/AVX2 versions
     subroutine compute_xdyd_ymm8r4(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_ymm8r4
            !dir$ attributes forceinline ::  compute_xdyd_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_ymm8r4
            type(YMM8r4_t),  intent(in)  :: gamma
            type(YMM8r4_t),  intent(in)  :: u
            type(YMM8r4_t),  intent(in)  :: n
            type(YMM8r4_t),  intent(out) :: xd
            type(YMM8r4_t),  intent(out) :: yd
            type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
            type(YMM8r4_t), parameter :: one  = YMM8r4_t(1.0_sp)
            type(YMM8r4_t), parameter :: four = YMM8r4_t(4.0_sp)
            type(YMM8r4_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
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
      end subroutine compute_xdyd_ymm8r4


      subroutine compute_xdyd_ymm4r8(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_ymm4r8
            !dir$ attributes forceinline ::  compute_xdyd_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_ymm4r8
            type(YMM4r8_t),  intent(in)  :: gamma
            type(YMM4r8_t),  intent(in)  :: u
            type(YMM4r8_t),  intent(in)  :: n
            type(YMM4r8_t),  intent(out) :: xd
            type(YMM4r8_t),  intent(out) :: yd
            type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
            type(YMM4r8_t), parameter :: one  = YMM4r8_t(1.0_dp)
            type(YMM4r8_t), parameter :: four = YMM4r8_t(4.0_dp)
            type(YMM4r8_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
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
     end subroutine compute_xdyd_ymm4r8


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


     !AVX/AVX2 versions.
     subroutine fov_axay_ymm8r4(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_ymm8r4
            !dir$ attributes forceinline ::  fov_axay_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_ymm8r4
            type(YMM8r4_t),  intent(in) :: H
            type(YMM8r4_t),  intent(in) :: delx
            type(YMM8r4_t),  intent(in) :: dely
            type(YMM8r4_t),  intent(in) :: phi
            type(YMM8r4_t),  intent(out):: ax
            type(YMM8r4_t),  intent(out):: ay
            type(YMM8r4_t),  parameter :: half = YMM8r4_t(0.5_sp)
            type(YMM8r4_t),  parameter :: one  = YMM8r4_t(1.0_sp)
            type(YMM8r4_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
      end subroutine fov_axay_ymm8r4
        
        
      subroutine fov_axay_ymm4r8(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_ymm4r8
            !dir$ attributes forceinline ::  fov_axay_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_ymm4r8
            type(YMM4r8_t),  intent(in) :: H
            type(YMM4r8_t),  intent(in) :: delx
            type(YMM4r8_t),  intent(in) :: dely
            type(YMM4r8_t),  intent(in) :: phi
            type(YMM4r8_t),  intent(out):: ax
            type(YMM4r8_t),  intent(out):: ay
            type(YMM4r8_t),  parameter :: half = YMM4r8_t(0.5_dp)
            type(YMM4r8_t),  parameter :: one  = YMM4r8_t(1.0_dp)
            type(YMM4r8_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
     end subroutine fov_axay_ymm4r8


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


     !AVX/AVX2 versions
     subroutine  fov_dxdy_ymm8r4(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_ymm8r4
            !dir$ attributes forceinline ::  fov_dxdy_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_ymm8r4
            type(YMM16r4_t),   intent(in) :: x
            type(YMM16r4_t),   intent(in) :: y
            type(YMM16r4_t),   intent(in) :: F
            type(YMM16r4_t),   intent(in) :: phi
            type(YMM16r4_t),   intent(out):: dx
            type(YMM16r4_t),   intent(out):: dy
            type(YMM16r4_t), parameter :: half = YMM8r4_t(0.5_sp)
            type(YMM16r4_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 32 :: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
      end subroutine fov_dxdy_ymm8r4
        
        
      subroutine  fov_dxdy_ymm4r8(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_ymm4r8
            !dir$ attributes forceinline ::  fov_dxdy_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_ymm4r8
            type(YMM8r8_t),   intent(in) :: x
            type(YMM8r8_t),   intent(in) :: y
            type(YMM8r8_t),   intent(in) :: F
            type(YMM8r8_t),   intent(in) :: phi
            type(YMM8r8_t),   intent(out):: dx
            type(YMM8r8_t),   intent(out):: dy
            type(YMM8r8_t), parameter :: half = YMM4r8_t(0.5_dp)
            type(YMM8r8_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 64 :: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
     end subroutine fov_dxdy_ymm4r8

    
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


     !AVX/AVX2
     subroutine volt_impulse_uxuy_ymm8r4(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_ymm8r4
            !dir$ attributes forceinline ::  volt_impulse_uxuy_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_ymm8r4
            type(YMM8r4_t),   intent(in) :: u
            type(YMM8r4_t),   intent(in) :: om1
            type(YMM8r4_t),   intent(in) :: om2
            type(YMM8r4_t),   intent(in) :: t
            type(YMM8r4_t),   intent(out):: ux
            type(YMM8r4_t),   intent(out):: uy
            type(YMM8r4_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
       end subroutine volt_impulse_uxuy_ymm8r4


       subroutine volt_impulse_uxuy_ymm4r8(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_ymm4r8
            !dir$ attributes forceinline ::  volt_impulse_uxuy_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_ymm4r8
            type(YMM4r8_t),   intent(in) :: u
            type(YMM4r8_t),   intent(in) :: om1
            type(YMM4r8_t),   intent(in) :: om2
            type(YMM4r8_t),   intent(in) :: t
            type(YMM4r8_t),   intent(out):: ux
            type(YMM4r8_t),   intent(out):: uy
            type(YMM4r8_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
     end subroutine volt_impulse_uxuy_ymm4r8
  
      
        
 
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+56+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+64+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+72+ii)
                    c9.v(ii)       = twopi.v(ii)*f9.v(ii)*hT.v(ii)
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v(ii)-(f9.v(ii)*hT.v(ii)*f9.v(ii)*hT.v(ii))
                    Phi0f(i+72+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+80+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+88+ii)
                    c11.v(ii)      = twopi.v(ii)*f11.v(ii)*hT.v(ii)
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+88+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+96+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+104+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+104+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+112+ii)
                    c14.v(ii)      = twopi.v(ii)*f14.v(ii)*hT.v(ii)
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+120+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
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
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+28+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+32+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+36+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+36+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+40+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+44+ii)
                    c11.v(ii)      = twopi.v(ii)*f11.v(ii)*hT.v(ii)
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+44+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+48+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+52+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+52+ii) = Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+56+ii)
                    c14.v(ii)      = twopi.v(ii)*f14.v(ii)*hT.v(ii)
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+60+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
                    Phi0f(i+60+ii) = Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
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
       end subroutine const_flux_spectr_unroll_16x_ymm4r8


      subroutine const_flux_spectr_unroll_8x_ymm4r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_ymm4r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
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
       end subroutine const_flux_spectr_unroll_8x_ymm4r8


       
      subroutine const_flux_spectr_unroll_4x_ymm4r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_ymm4r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(YMM4r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+64+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+80+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+96+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v(ii)-(f6.v(ii)*hT.v(ii)*f6.v(ii)*hT.v(ii))
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+112+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+128+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+128+ii)= Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+144+ii)
                    c9.v(ii)       = twopi.v(ii)*f9.v(ii)*hT.v(ii)
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v(ii)-(f9.v(ii)*hT.v(ii)*f9.v(ii)*hT.v(ii))
                    Phi0f(i+144+ii)= Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+160+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+160+ii)= Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+176+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+176+ii)= Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+192+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+192+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+208+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+208+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+224+ii)
                    c14.v(ii)      = twopi.v(ii)*f14.v(ii)*hT.v(ii)
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+224+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+240+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
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
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+64+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+80+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+96+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v(ii)-(f6.v(ii)*hT.v(ii)*f6.v(ii)*hT.v(ii))
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+112+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+16+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+32+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+48+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
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
           type(ZMM8r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(ZMM8r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v(ii)-(f6.v(ii)*hT.v(ii)*f6.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+64+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+64+ii)= Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+72+ii)
                    c9.v(ii)       = twopi.v(ii)*f9.v(ii)*hT.v(ii)
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v(ii)-(f9.v(ii)*hT.v(ii)*f9.v(ii)*hT.v(ii))
                    Phi0f(i+72+ii)= Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+80+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+80+ii)= Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+88+ii)
                    c11.v(ii)      = twopi.v(ii)*f11.v(ii)*hT.v(ii)
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+88+ii)= Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+96+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+104+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+104+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+112+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+120+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
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


       subroutine const_flux_spectr_unroll_8x_zmm8r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_zmm8r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM8r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM8r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(ZMM8r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM8r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 64 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(ZMM8r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 64 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM8r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
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
           else if(n>8 .and. n<=64) then
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v(ii)-(f6.v(ii)*hT.v(ii)*f6.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+56+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
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
       end subroutine const_flux_spectr_unroll_8x_zmm8r8


       subroutine const_flux_spectr_unroll_4x_zmm8r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_zmm8r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(ZMM8r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(ZMM8r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_r), parameter :: half  = YMM8r4_t(0.5_dp)
           type(ZMM8r8_t), parameter :: one   = YMM8r4_t(1.0_dp)
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3
           type(ZMM8r8_t) :: f0,f1,f2,f3
           !dir$ attributes align : 64 :: f0,f1,f2,f3
           type(ZMM8r8_t) :: c0,c1,c2,c3
           !dir$ attributes align : 64 :: c0,c1,c2,c3
           type(ZMM8r8_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 64 :: sa0,sa1,sa2,sa3
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
           else if(n>8 .and. n<=32) then
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
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
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
              do i=1,iand(n,not(7)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
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
       end subroutine const_flux_spectr_unroll_4x_zmm8r8


       !Идеальный гармонический модулятор
       !Formula 1,2 p. 187
       
       subroutine ideal_modulator_unroll_16x_zmm16r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_zmm16r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: f0
           type(ZMM16r4_t),               intent(in)  :: phi0
           type(ZMM16r4_t),               intent(in)  :: rho0
           type(ZMM16r4_t),               intent(in)  :: rho1
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(ZMM16r4_t) :: t8,t9,t10,t11,t12,t13,t14,t15
           type(ZMM16r4_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(ZMM16r4_t) :: s8,s9,s10,s11,s12,s13,s14,s15
           type(ZMM16r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM16r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           type(ZMM16r4_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(ZMM16r4_t) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           type(ZMM16r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>16 .and. n<=256) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),16
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,15
                     t0.v(ii)     = zmm16vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>256) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),256
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm16vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm16vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+16)  = s1.v(ii)
                       c1.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+16)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm16vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+32)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+32)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm16vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+48)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+48)  = c3.v(ii)
                       t4.v(ii)        = ri+zmm16vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+64)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+64)  = c4.v(ii)
                       t5.v(ii)        = ri+zmm16vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+80)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+80)  = c5.v(ii)
                       t6.v(ii)        = ri+zmm16vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+96)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+96)  = c6.v(ii)
                       t7.v(ii)        = ri+zmm16vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+112) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+112) = c7.v(ii) 
                       t8.v(ii)        = ri+zmm16vinc8(ii)
                       psi8.v(ii)      = om0.v(ii)*t8.v(ii)+phi0.v(ii)
                       s8.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi8.v(ii))
                       rhot_s(idx+128) = s8.v(ii)
                       c8.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi8.v(ii))
                       rhot_c(idx+128) = c8.v(ii)
                       t9.v(ii)        = ri+zmm16vinc9(ii)
                       psi9.v(ii)      = om0.v(ii)*t9.v(ii)+phi0.v(ii)
                       s9.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi9.v(ii))
                       rhot_s(idx+144) = s9.v(ii)
                       c9.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi9.v(ii))
                       rhot_c(idx+144) = c9.v(ii)
                       t10.v(ii)       = ri+zmm16vinc10(ii)
                       psi10.v(ii)     = om0.v(ii)*t10.v(ii)+phi0.v(ii)
                       s10.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi10.v(ii))
                       rhot_s(idx+160) = s10.v(ii)
                       c10.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi10.v(ii))
                       rhot_c(idx+160) = c10.v(ii)
                       t11.v(ii)       = ri+zmm16vinc11(ii)
                       psi11.v(ii)     = om0.v(ii)*t11.v(ii)+phi0.v(ii)
                       s11.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi11.v(ii))
                       rhot_s(idx+176) = s11.v(ii)
                       c11.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi11.v(ii))
                       rhot_c(idx+176) = c11.v(ii)
                       t12.v(ii)       = ri+zmm16vinc12(ii)
                       psi12.v(ii)     = om0.v(ii)*t12.v(ii)+phi0.v(ii)
                       s12.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi12.v(ii))
                       rhot_s(idx+194) = s12.v(ii)
                       c12.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi12.v(ii))
                       rhot_c(idx+194) = c12.v(ii)
                       t13.v(ii)       = ri+zmm16vinc13(ii)
                       psi13.v(ii)     = om0.v(ii)*t13.v(ii)+phi0.v(ii)
                       s13.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi13.v(ii))
                       rhot_s(idx+210) = s13.v(ii)
                       c13.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi13.v(ii))
                       rhot_c(idx+210) = c13.v(ii)
                       t14.v(ii)       = ri+zmm16vinc14(ii)
                       psi14.v(ii)     = om0.v(ii)*t14.v(ii)+phi0.v(ii)
                       s14.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi14.v(ii))
                       rhot_s(idx+226) = s14.v(ii)
                       c14.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi14.v(ii))
                       rhot_c(idx+226) = c14.v(ii)
                       t15.v(ii)       = ri+zmm16vinc15(ii)
                       psi15.v(ii)     = om0.v(ii)*t15.v(ii)+phi0.v(ii)
                       s15.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi15.v(ii))
                       rhot_s(idx+240) = s15.v(ii)
                       c15.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi15.v(ii))
                       rhot_c(idx+240) = c15.v(ii)
                   end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_16x_zmm16r4


       subroutine ideal_modulator_unroll_8x_zmm16r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_zmm16r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: f0
           type(ZMM16r4_t),               intent(in)  :: phi0
           type(ZMM16r4_t),               intent(in)  :: rho0
           type(ZMM16r4_t),               intent(in)  :: rho1
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(ZMM16r4_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(ZMM16r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM16r4_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(ZMM16r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>16 .and. n<=128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),16
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,15
                     t0.v(ii)     = zmm16vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),128
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm16vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm16vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+16)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+16)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm16vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+32)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+32)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm16vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+48)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+48)  = c3.v(ii)
                       t4.v(ii)        = ri+zmm16vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+64)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+64)  = c4.v(ii)
                       t5.v(ii)        = ri+zmm16vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+80)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+80)  = c5.v(ii)
                       t6.v(ii)        = ri+zmm16vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+96)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+96)  = c6.v(ii)
                       t7.v(ii)        = ri+zmm16vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+112) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+112) = c7.v(ii) 
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_8x_zmm16r4


       subroutine ideal_modulator_unroll_4x_zmm16r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_zmm16r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: f0
           type(ZMM16r4_t),               intent(in)  :: phi0
           type(ZMM16r4_t),               intent(in)  :: rho0
           type(ZMM16r4_t),               intent(in)  :: rho1
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t) :: t0,t1,t2,t3
           type(ZMM16r4_t) :: s0,s1,s2,s3
           type(ZMM16r4_t) :: c0,c1,c2,c3
           type(ZMM16r4_t) :: psi0,psi1,psi2,psi3
           type(ZMM16r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>16 .and. n<=64) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),16
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,15
                     t0.v(ii)     = zmm16vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>64) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(15)),64
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm16vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm16vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+16)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+16)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm16vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+32)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+32)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm16vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+48)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+48)  = c3.v(ii)
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_4x_zmm16r4


       subroutine ideal_modulator_unroll_16x_zmm8r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_zmm8r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),               intent(in)  :: f0
           type(ZMM8r8_t),               intent(in)  :: phi0
           type(ZMM8r8_t),               intent(in)  :: rho0
           type(ZMM8r8_t),               intent(in)  :: rho1
           type(ZMM8r8_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(ZMM8r8_t) :: t8,t9,t10,t11,t12,t13,t14,t15
           type(ZMM8r8_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(ZMM8r8_t) :: s8,s9,s10,s11,s12,s13,s14,s15
           type(ZMM8r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM8r8_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           type(ZMM8r8_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(ZMM8r8_t) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           type(ZMM8r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = zmm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),128
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                       t4.v(ii)        = ri+zmm8vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+32)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+32)  = c4.v(ii)
                       t5.v(ii)        = ri+zmm8vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+40)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+40)  = c5.v(ii)
                       t6.v(ii)        = ri+zmm8vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+48)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+48)  = c6.v(ii)
                       t7.v(ii)        = ri+zmm8vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+56) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+56) = c7.v(ii) 
                       t8.v(ii)        = ri+zmm8vinc8(ii)
                       psi8.v(ii)      = om0.v(ii)*t8.v(ii)+phi0.v(ii)
                       s8.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi8.v(ii))
                       rhot_s(idx+64) = s8.v(ii)
                       c8.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi8.v(ii))
                       rhot_c(idx+64) = c8.v(ii)
                       t9.v(ii)        = ri+zmm8vinc9(ii)
                       psi9.v(ii)      = om0.v(ii)*t9.v(ii)+phi0.v(ii)
                       s9.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi9.v(ii))
                       rhot_s(idx+72) = s9.v(ii)
                       c9.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi9.v(ii))
                       rhot_c(idx+72) = c9.v(ii)
                       t10.v(ii)       = ri+zmm8vinc10(ii)
                       psi10.v(ii)     = om0.v(ii)*t10.v(ii)+phi0.v(ii)
                       s10.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi10.v(ii))
                       rhot_s(idx+80) = s10.v(ii)
                       c10.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi10.v(ii))
                       rhot_c(idx+80) = c10.v(ii)
                       t11.v(ii)       = ri+zmm8vinc11(ii)
                       psi11.v(ii)     = om0.v(ii)*t11.v(ii)+phi0.v(ii)
                       s11.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi11.v(ii))
                       rhot_s(idx+88) = s11.v(ii)
                       c11.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi11.v(ii))
                       rhot_c(idx+88) = c11.v(ii)
                       t12.v(ii)       = ri+zmm8vinc12(ii)
                       psi12.v(ii)     = om0.v(ii)*t12.v(ii)+phi0.v(ii)
                       s12.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi12.v(ii))
                       rhot_s(idx+96) = s12.v(ii)
                       c12.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi12.v(ii))
                       rhot_c(idx+96) = c12.v(ii)
                       t13.v(ii)       = ri+zmm8vinc13(ii)
                       psi13.v(ii)     = om0.v(ii)*t13.v(ii)+phi0.v(ii)
                       s13.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi13.v(ii))
                       rhot_s(idx+104) = s13.v(ii)
                       c13.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi13.v(ii))
                       rhot_c(idx+104) = c13.v(ii)
                       t14.v(ii)       = ri+zmm8vinc14(ii)
                       psi14.v(ii)     = om0.v(ii)*t14.v(ii)+phi0.v(ii)
                       s14.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi14.v(ii))
                       rhot_s(idx+112) = s14.v(ii)
                       c14.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi14.v(ii))
                       rhot_c(idx+112) = c14.v(ii)
                       t15.v(ii)       = ri+zmm8vinc15(ii)
                       psi15.v(ii)     = om0.v(ii)*t15.v(ii)+phi0.v(ii)
                       s15.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi15.v(ii))
                       rhot_s(idx+120) = s15.v(ii)
                       c15.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi15.v(ii))
                       rhot_c(idx+120) = c15.v(ii)
                   end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_16x_zmm8r8


       subroutine ideal_modulator_unroll_8x_zmm8r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_zmm8r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),               intent(in)  :: f0
           type(ZMM8r8_t),               intent(in)  :: phi0
           type(ZMM8r8_t),               intent(in)  :: rho0
           type(ZMM8r8_t),               intent(in)  :: rho1
           type(ZMM8r8_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(ZMM8r8_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(ZMM8r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(ZMM8r8_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(ZMM8r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=64) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = zmm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),64
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                       t4.v(ii)        = ri+zmm8vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+32)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+32)  = c4.v(ii)
                       t5.v(ii)        = ri+zmm8vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+40)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+40)  = c5.v(ii)
                       t6.v(ii)        = ri+zmm8vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+48)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+48)  = c6.v(ii)
                       t7.v(ii)        = ri+zmm8vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+56) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+56) = c7.v(ii) 
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_8x_zmm8r8


       subroutine ideal_modulator_unroll_4x_zmm8r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_zmm8r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),               intent(in)  :: f0
           type(ZMM8r8_t),               intent(in)  :: phi0
           type(ZMM8r8_t),               intent(in)  :: rho0
           type(ZMM8r8_t),               intent(in)  :: rho1
           type(ZMM8r8_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_t) :: t0,t1,t2,t3
           type(ZMM8r8_t) :: s0,s1,s2,s3
           type(ZMM8r8_t) :: c0,c1,c2,c3
           type(ZMM8r8_t) :: psi0,psi1,psi2,psi3
           type(ZMM8r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=64) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = zmm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>128) then
              !dir$ assume_aligned rhot_s:64           
              !dir$ assume_aligned rhot_c:64
              do i=1,iand(n,not(7)),32
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+zmm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+zmm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+zmm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+zmm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_4x_zmm8r8


  
       subroutine ideal_modulator_unroll_16x_ymm8r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_ymm8r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: f0
           type(YMM8r4_t),                intent(in)  :: phi0
           type(YMM8r4_t),                intent(in)  :: rho0
           type(YMM8r4_t),                intent(in)  :: rho1
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(YMM8r4_t) :: t8,t9,t10,t11,t12,t13,t14,t15
           type(YMM8r4_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(YMM8r4_t) :: s8,s9,s10,s11,s12,s13,s14,s15
           type(YMM8r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM8r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           type(YMM8r4_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(YMM8r4_t) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           type(YMM8r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=128) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = ymm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>128) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),128
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                       t4.v(ii)        = ri+ymm8vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+32)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+32)  = c4.v(ii)
                       t5.v(ii)        = ri+ymm8vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+40)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+40)  = c5.v(ii)
                       t6.v(ii)        = ri+ymm8vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+48)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+48)  = c6.v(ii)
                       t7.v(ii)        = ri+ymm8vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+56) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+56) = c7.v(ii) 
                       t8.v(ii)        = ri+ymm8vinc8(ii)
                       psi8.v(ii)      = om0.v(ii)*t8.v(ii)+phi0.v(ii)
                       s8.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi8.v(ii))
                       rhot_s(idx+64) = s8.v(ii)
                       c8.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi8.v(ii))
                       rhot_c(idx+64) = c8.v(ii)
                       t9.v(ii)        = ri+ymm8vinc9(ii)
                       psi9.v(ii)      = om0.v(ii)*t9.v(ii)+phi0.v(ii)
                       s9.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi9.v(ii))
                       rhot_s(idx+72) = s9.v(ii)
                       c9.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi9.v(ii))
                       rhot_c(idx+72) = c9.v(ii)
                       t10.v(ii)       = ri+ymm8vinc10(ii)
                       psi10.v(ii)     = om0.v(ii)*t10.v(ii)+phi0.v(ii)
                       s10.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi10.v(ii))
                       rhot_s(idx+80) = s10.v(ii)
                       c10.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi10.v(ii))
                       rhot_c(idx+80) = c10.v(ii)
                       t11.v(ii)       = ri+ymm8vinc11(ii)
                       psi11.v(ii)     = om0.v(ii)*t11.v(ii)+phi0.v(ii)
                       s11.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi11.v(ii))
                       rhot_s(idx+88) = s11.v(ii)
                       c11.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi11.v(ii))
                       rhot_c(idx+88) = c11.v(ii)
                       t12.v(ii)       = ri+ymm8vinc12(ii)
                       psi12.v(ii)     = om0.v(ii)*t12.v(ii)+phi0.v(ii)
                       s12.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi12.v(ii))
                       rhot_s(idx+96) = s12.v(ii)
                       c12.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi12.v(ii))
                       rhot_c(idx+96) = c12.v(ii)
                       t13.v(ii)       = ri+ymm8vinc13(ii)
                       psi13.v(ii)     = om0.v(ii)*t13.v(ii)+phi0.v(ii)
                       s13.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi13.v(ii))
                       rhot_s(idx+104) = s13.v(ii)
                       c13.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi13.v(ii))
                       rhot_c(idx+104) = c13.v(ii)
                       t14.v(ii)       = ri+ymm8vinc14(ii)
                       psi14.v(ii)     = om0.v(ii)*t14.v(ii)+phi0.v(ii)
                       s14.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi14.v(ii))
                       rhot_s(idx+112) = s14.v(ii)
                       c14.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi14.v(ii))
                       rhot_c(idx+112) = c14.v(ii)
                       t15.v(ii)       = ri+ymm8vinc15(ii)
                       psi15.v(ii)     = om0.v(ii)*t15.v(ii)+phi0.v(ii)
                       s15.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi15.v(ii))
                       rhot_s(idx+120) = s15.v(ii)
                       c15.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi15.v(ii))
                       rhot_c(idx+120) = c15.v(ii)
                   end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_16x_ymm8r4



      subroutine ideal_modulator_unroll_8x_ymm8r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_ymm8r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: f0
           type(YMM8r4_t),                intent(in)  :: phi0
           type(YMM8r4_t),                intent(in)  :: rho0
           type(YMM8r4_t),                intent(in)  :: rho1
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(YMM8r4_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(YMM8r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM8r4_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(YMM8r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=64) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = ymm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>64) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),64
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                       t4.v(ii)        = ri+ymm8vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+32)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+32)  = c4.v(ii)
                       t5.v(ii)        = ri+ymm8vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+40)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+40)  = c5.v(ii)
                       t6.v(ii)        = ri+ymm8vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+48)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+48)  = c6.v(ii)
                       t7.v(ii)        = ri+ymm8vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+56) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+56) = c7.v(ii) 
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_8x_ymm8r4


       subroutine ideal_modulator_unroll_4x_ymm8r4(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_ymm8r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: f0
           type(YMM8r4_t),                intent(in)  :: phi0
           type(YMM8r4_t),                intent(in)  :: rho0
           type(YMM8r4_t),                intent(in)  :: rho1
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t) :: t0,t1,t2,t3
           type(YMM8r4_t) :: s0,s1,s2,s3
           type(YMM8r4_t) :: c0,c1,c2,c3
           type(YMM8r4_t) :: psi0,psi1,psi2,psi3
           type(YMM8r4_t) :: om0
           real(kind=sp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>8 .and. n<=32) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),8
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(4)
                  !dir$ vector always
                  do ii=0,7
                     t0.v(ii)     = ymm8vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                   t        = real(i,kind=sp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>32) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(7)),32
	           ri              = real(i,kind=sp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm8vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm8vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+8)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+8)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm8vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+16)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+16)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm8vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+24)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+24)  = c3.v(ii)
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i, n
                 t        = real(i,kind=sp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_4x_ymm8r4


       subroutine ideal_modulator_unroll_16x_ymm4r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_ymm4r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: f0
           type(YMM4r8_t),                intent(in)  :: phi0
           type(YMM4r8_t),                intent(in)  :: rho0
           type(YMM4r8_t),                intent(in)  :: rho1
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(YMM4r8_t) :: t8,t9,t10,t11,t12,t13,t14,t15
           type(YMM4r8_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(YMM4r8_t) :: s8,s9,s10,s11,s12,s13,s14,s15
           type(YMM4r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM4r8_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           type(YMM4r8_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(YMM4r8_t) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           type(YMM4r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>4 .and. n<=64) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),4
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(8)
                  !dir$ vector always
                  do ii=0,3
                     t0.v(ii)     = ymm4vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>64) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),64
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm4vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm4vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+4)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+4)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm4vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+8)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+8)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm4vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+12)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+12)  = c3.v(ii)
                       t4.v(ii)        = ri+ymm4vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+16)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+16)  = c4.v(ii)
                       t5.v(ii)        = ri+ymm4vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+20)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+20)  = c5.v(ii)
                       t6.v(ii)        = ri+ymm4vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+24)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+24)  = c6.v(ii)
                       t7.v(ii)        = ri+ymm4vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+28) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+28) = c7.v(ii) 
                       t8.v(ii)        = ri+ymm4vinc8(ii)
                       psi8.v(ii)      = om0.v(ii)*t8.v(ii)+phi0.v(ii)
                       s8.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi8.v(ii))
                       rhot_s(idx+32) = s8.v(ii)
                       c8.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi8.v(ii))
                       rhot_c(idx+32) = c8.v(ii)
                       t9.v(ii)        = ri+ymm4vinc9(ii)
                       psi9.v(ii)      = om0.v(ii)*t9.v(ii)+phi0.v(ii)
                       s9.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi9.v(ii))
                       rhot_s(idx+36) = s9.v(ii)
                       c9.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi9.v(ii))
                       rhot_c(idx+36) = c9.v(ii)
                       t10.v(ii)       = ri+ymm4vinc10(ii)
                       psi10.v(ii)     = om0.v(ii)*t10.v(ii)+phi0.v(ii)
                       s10.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi10.v(ii))
                       rhot_s(idx+40) = s10.v(ii)
                       c10.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi10.v(ii))
                       rhot_c(idx+40) = c10.v(ii)
                       t11.v(ii)       = ri+ymm4vinc11(ii)
                       psi11.v(ii)     = om0.v(ii)*t11.v(ii)+phi0.v(ii)
                       s11.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi11.v(ii))
                       rhot_s(idx+44) = s11.v(ii)
                       c11.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi11.v(ii))
                       rhot_c(idx+44) = c11.v(ii)
                       t12.v(ii)       = ri+ymm4vinc12(ii)
                       psi12.v(ii)     = om0.v(ii)*t12.v(ii)+phi0.v(ii)
                       s12.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi12.v(ii))
                       rhot_s(idx+48) = s12.v(ii)
                       c12.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi12.v(ii))
                       rhot_c(idx+48) = c12.v(ii)
                       t13.v(ii)       = ri+ymm4vinc13(ii)
                       psi13.v(ii)     = om0.v(ii)*t13.v(ii)+phi0.v(ii)
                       s13.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi13.v(ii))
                       rhot_s(idx+52) = s13.v(ii)
                       c13.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi13.v(ii))
                       rhot_c(idx+52) = c13.v(ii)
                       t14.v(ii)       = ri+ymm4vinc14(ii)
                       psi14.v(ii)     = om0.v(ii)*t14.v(ii)+phi0.v(ii)
                       s14.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi14.v(ii))
                       rhot_s(idx+56) = s14.v(ii)
                       c14.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi14.v(ii))
                       rhot_c(idx+56) = c14.v(ii)
                       t15.v(ii)       = ri+ymm4vinc15(ii)
                       psi15.v(ii)     = om0.v(ii)*t15.v(ii)+phi0.v(ii)
                       s15.v(ii)       = rho0.v(ii)+rho1.v(ii)*sin(psi15.v(ii))
                       rhot_s(idx+60) = s15.v(ii)
                       c15.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi15.v(ii))
                       rhot_c(idx+60) = c15.v(ii)
                   end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_16x_ymm4r8



       subroutine ideal_modulator_unroll_8x_ymm4r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_ymm4r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: f0
           type(YMM4r8_t),                intent(in)  :: phi0
           type(YMM4r8_t),                intent(in)  :: rho0
           type(YMM4r8_t),                intent(in)  :: rho1
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t) :: t0,t1,t2,t3,t4,t5,t6,t7
           type(YMM4r8_t) :: s0,s1,s2,s3,s4,s5,s6,s7
           type(YMM4r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM4r8_t) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           type(YMM4r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>4 .and. n<=32) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),4
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(8)
                  !dir$ vector always
                  do ii=0,3
                     t0.v(ii)     = ymm4vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>32) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),32
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm4vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm4vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+4)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+4)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm4vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+8)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+8)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm4vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+12)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+12)  = c3.v(ii)
                       t4.v(ii)        = ri+ymm4vinc4(ii)
                       psi4.v(ii)      = om0.v(ii)*t4.v(ii)+phi0.v(ii)
                       s4.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi4.v(ii))
                       rhot_s(idx+16)  = s4.v(ii)
                       c4.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi4.v(ii))
                       rhot_c(idx+16)  = c4.v(ii)
                       t5.v(ii)        = ri+ymm4vinc5(ii)
                       psi5.v(ii)      = om0.v(ii)*t5.v(ii)+phi0.v(ii)
                       s5.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi5.v(ii))
                       rhot_s(idx+20)  = s5.v(ii)
                       c5.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi5.v(ii))
                       rhot_c(idx+20)  = c5.v(ii)
                       t6.v(ii)        = ri+ymm4vinc6(ii)
                       psi6.v(ii)      = om0.v(ii)*t6.v(ii)+phi0.v(ii)
                       s6.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi6.v(ii))
                       rhot_s(idx+24)  = s6.v(ii)
                       c6.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi6.v(ii))
                       rhot_c(idx+24)  = c6.v(ii)
                       t7.v(ii)        = ri+ymm4vinc7(ii)
                       psi7.v(ii)      = om0.v(ii)*t7.v(ii)+phi0.v(ii)
                       s7.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi7.v(ii))
                       rhot_s(idx+28) = s7.v(ii)
                       c7.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi7.v(ii))
                       rhot_c(idx+28) = c7.v(ii) 
                  end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_8x_ymm4r8


       subroutine ideal_modulator_unroll_4x_ymm4r8(rhot_s,rhot_c, &
                                                     n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_ymm4r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: f0
           type(YMM4r8_t),                intent(in)  :: phi0
           type(YMM4r8_t),                intent(in)  :: rho0
           type(YMM4r8_t),                intent(in)  :: rho1
           type(YMM4r8_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t) :: t0,t1,t2,t3
           type(YMM4r8_t) :: s0,s1,s2,s3
           type(YMM4r8_t) :: c0,c1,c2,c3
           type(YMM4r8_t) :: psi0,psi1,psi2,psi3
           type(YMM4r8_t) :: om0
           real(kind=dp) :: t,psi,s,c,ri
           integer(kind=i4) :: i,ii,j,idx
           om0 = twopi.v*f0.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           else if(n>4 .and. n<=16) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),4
                  !dir$ vector aligned
                  !dir$ ivdep
                  !dir$ vector vectorlength(8)
                  !dir$ vector always
                  do ii=0,3
                     t0.v(ii)     = ymm4vinc0(ii)
                     psi0.v(ii)   = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                     s0.v(ii)     = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                     rhot_s(i+ii) = s0.v(ii)
                     c0.v(ii)     = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                     rhot_c(i+ii) = c0.v(ii)
                  end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                   t        = real(i,kind=dp)
                   psi      = om0.v(0)*t+phi0.v(0)
                   s        = rho0.v(0)+rho1.v(0)*sin(psi)
                   rhot_s(i)= s
                   c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                   rhot_c(i)= c  
              end do 
              return
           else if(n>16) then
              !dir$ assume_aligned rhot_s:32           
              !dir$ assume_aligned rhot_c:32
              do i=1,iand(n,not(3)),16
	           ri              = real(i,kind=dp)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                       idx             = i+ii
                      
                       t0.v(ii)        = ri+ymm4vinc0(ii)
                       psi0.v(ii)      = om0.v(ii)*t0.v(ii)+phi0.v(ii)
                       s0.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi0.v(ii))
                       rhot_s(idx)     = s0.v(ii)
                       c0.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi0.v(ii))
                       rhot_c(idx)     = c0.v(ii)
                       t1.v(ii)        = ri+ymm4vinc1(ii)
                       psi1.v(ii)      = om0.v(ii)*t1.v(ii)+phi0.v(ii)
                       s1.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi1.v(ii))
                       rhot_s(idx+4)  = s1.v(ii)
                       c01.v(ii)       = rho0.v(ii)+rho1.v(ii)*cos(psi1.v(ii))
                       rhot_c(idx+4)  = c1.v(ii)
                       t2.v(ii)        = ri+ymm4vinc2(ii)
                       psi2.v(ii)      = om0.v(ii)*t2.v(ii)+phi0.v(ii)
                       s2.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi2.v(ii))
                       rhot_s(idx+8)  = s2.v(ii)
                       c2.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi2.v(ii))
                       rhot_c(idx+8)  = c2.v(ii)
                       t3.v(ii)        = ri+ymm4vinc3(ii)
                       psi3.v(ii)      = om0.v(ii)*t3.v(ii)+phi0.v(ii)
                       s3.v(ii)        = rho0.v(ii)+rho1.v(ii)*sin(psi3.v(ii))
                       rhot_s(idx+12)  = s3.v(ii)
                       c3.v(ii)        = rho0.v(ii)+rho1.v(ii)*cos(psi3.v(ii))
                       rhot_c(idx+12)  = c3.v(ii)
                   end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i, n
                 t        = real(i,kind=dp)
                 psi      = om0.v(0)*t+phi0.v(0)
                 s        = rho0.v(0)+rho1.v(0)*sin(psi)
                 rhot_s(i)= s
                 c        = rho0.v(0)+rho1.v(0)*cos(psi0)
                 rhot_c(i)= c
              end do
              return
           end if
       end subroutine ideal_modulator_unroll_4x_ymm4r8



        !Ошибни изготовления растра —
        !модулятора излучения
        !Formula 2, p. 189
       subroutine rect_pulse_flux_unroll_16x_zmm16r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(ZMM16r4_t),               intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: Tin
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t), parameter :: half  = ZMM16r4_t(0.5_sp)
           type(ZMM16r4_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(ZMM16r4_t) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           type(ZMM16r4_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(ZMM16r4_t) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(ZMM16r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM16r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>16 .and. n<=256) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),16
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=16,min=1,avg=8
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>256) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),256
                  call mm_prefetch(freq(i*256),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+16)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+32)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+48)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+64)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+64) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+80)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+80) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+96)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+96) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+112)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+112)= Phi0fk.v(ii)*sinc7.v(ii) 
                      fk8          = fk(idx+128)
                      arg8         = twopi.v(ii)*fk8.v(ii)*hThin.v(ii)
                      sinc8        = sin(arg8.v(ii))/arg8.v(ii)
                      Phik(idx+128)= Phi0fk.v(ii)*sinc8.v(ii) 
                      fk9          = fk(idx+144)
                      arg9         = twopi.v(ii)*fk9.v(ii)*hThin.v(ii)
                      sinc9        = sin(arg9.v(ii))/arg9.v(ii)
                      Phik(idx+144)= Phi0fk.v(ii)*sinc9.v(ii) 
                      fk10         = fk(idx+160)
                      arg10        = twopi.v(ii)*fk10.v(ii)*hThin.v(ii)
                      sinc10       = sin(arg10.v(ii))/arg10.v(ii)
                      Phik(idx+160)= Phi0fk.v(ii)*sinc10.v(ii) 
                      fk11         = fk(idx+176)
                      arg11        = twopi.v(ii)*fk11.v(ii)*hThin.v(ii)
                      sinc11       = sin(arg11.v(ii))/arg11.v(ii)
                      Phik(idx+176)= Phi0fk.v(ii)*sinc11.v(ii) 
                      fk12         = fk(idx+192)
                      arg12        = twopi.v(ii)*fk12.v(ii)*hThin.v(ii)
                      sinc12       = sin(arg12.v(ii))/arg12.v(ii)
                      Phik(idx+192)= Phi0fk.v(ii)*sinc12.v(ii)
                      fk13         = fk(idx+208)
                      arg13        = twopi.v(ii)*fk13.v(ii)*hThin.v(ii)
                      sinc13       = sin(arg13.v(ii))/arg13.v(ii)
                      Phik(idx+208)= Phi0fk.v(ii)*sinc13.v(ii)  
                      fk14         = fk(idx+224)
                      arg14        = twopi.v(ii)*fk14.v(ii)*hThin.v(ii)
                      sinc14       = sin(arg14.v(ii))/arg14.v(ii)
                      Phik(idx+224)= Phi0fk.v(ii)*sinc14.v(ii)  
                      fk15         = fk(idx+240)
                      arg15        = twopi.v(ii)*fk15.v(ii)*hThin.v(ii)
                      sinc15       = sin(arg15.v(ii))/arg15.v(ii)
                      Phik(idx+240)= Phi0fk.v(ii)*sinc15.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_16x_zmm16r4


       subroutine rect_pulse_flux_unroll_8x_zmm16r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_8x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_8x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_8x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(ZMM16r4_t),               intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: Tin
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t), parameter :: half  = ZMM16r4_t(0.5_sp)
           type(ZMM16r4_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(ZMM16r4_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(ZMM16r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>16 .and. n<=128) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),16
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=16,min=1,avg=8
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>128) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+16)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+32)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+48)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+64)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+64) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+80)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+80) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+96)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+96) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+112)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+112)= Phi0fk.v(ii)*sinc7.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_8x_zmm16r4
 

       subroutine rect_pulse_flux_unroll_4x_zmm16r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_zmm16r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(ZMM16r4_t),               intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(ZMM16r4_t),               intent(in)  :: Tin
           type(ZMM16r4_t), parameter :: twopi = ZMM16r4_t(6.283185307179586476925286766559_sp)
           type(ZMM16r4_t), parameter :: half  = ZMM16r4_t(0.5_sp)
           type(ZMM16r4_t) :: fk0,fk1,fk2,fk3
           type(ZMM16r4_t) :: sinc0,sinc1,sinc2,sinc3
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3
           type(ZMM16r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<16) then
              return
           else if(n==16) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>16 .and. n<=64) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),16
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=16,min=1,avg=8
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>64) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(15)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,15
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+16)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+32)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+48)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc3.v(ii)  
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=16,min=1,avg=8
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_4x_zmm16r4



       subroutine rect_pulse_flux_unroll_16x_zmm8r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_zmm8r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           type(ZMM8r8_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: Tin
           type(ZMM8r8_t), parameter :: twopi = ZMM8r8_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_t), parameter :: half  = ZMM8r8_t(0.5_dp)
           type(ZMM8r8_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(ZMM8r8_t) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           type(ZMM8r8_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(ZMM8r8_t) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(ZMM8r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM8r8_t) :: hTin,Phi0fk
           real(kind=dp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>8 .and. n<=128) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(7)),8
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=8,min=1,avg=4
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>128) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(7)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+8)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+16)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+24)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+32)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+40)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+40) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+48)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+56)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+56)= Phi0fk.v(ii)*sinc7.v(ii) 
                      fk8          = fk(idx+64)
                      arg8         = twopi.v(ii)*fk8.v(ii)*hThin.v(ii)
                      sinc8        = sin(arg8.v(ii))/arg8.v(ii)
                      Phik(idx+64)= Phi0fk.v(ii)*sinc8.v(ii) 
                      fk9          = fk(idx+72)
                      arg9         = twopi.v(ii)*fk9.v(ii)*hThin.v(ii)
                      sinc9        = sin(arg9.v(ii))/arg9.v(ii)
                      Phik(idx+72)= Phi0fk.v(ii)*sinc9.v(ii) 
                      fk10         = fk(idx+80)
                      arg10        = twopi.v(ii)*fk10.v(ii)*hThin.v(ii)
                      sinc10       = sin(arg10.v(ii))/arg10.v(ii)
                      Phik(idx+80)= Phi0fk.v(ii)*sinc10.v(ii) 
                      fk11         = fk(idx+88)
                      arg11        = twopi.v(ii)*fk11.v(ii)*hThin.v(ii)
                      sinc11       = sin(arg11.v(ii))/arg11.v(ii)
                      Phik(idx+88)= Phi0fk.v(ii)*sinc11.v(ii) 
                      fk12         = fk(idx+96)
                      arg12        = twopi.v(ii)*fk12.v(ii)*hThin.v(ii)
                      sinc12       = sin(arg12.v(ii))/arg12.v(ii)
                      Phik(idx+96)= Phi0fk.v(ii)*sinc12.v(ii)
                      fk13         = fk(idx+104)
                      arg13        = twopi.v(ii)*fk13.v(ii)*hThin.v(ii)
                      sinc13       = sin(arg13.v(ii))/arg13.v(ii)
                      Phik(idx+104)= Phi0fk.v(ii)*sinc13.v(ii)  
                      fk14         = fk(idx+112)
                      arg14        = twopi.v(ii)*fk14.v(ii)*hThin.v(ii)
                      sinc14       = sin(arg14.v(ii))/arg14.v(ii)
                      Phik(idx+112)= Phi0fk.v(ii)*sinc14.v(ii)  
                      fk15         = fk(idx+120)
                      arg15        = twopi.v(ii)*fk15.v(ii)*hThin.v(ii)
                      sinc15       = sin(arg15.v(ii))/arg15.v(ii)
                      Phik(idx+120)= Phi0fk.v(ii)*sinc15.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_16x_zmm8r8


       subroutine rect_pulse_flux_unroll_4x_zmm8r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_zmm8r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_zmm8r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           type(ZMM8r8_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: Tin
           type(ZMM8r8_t), parameter :: twopi = ZMM8r8_t(6.283185307179586476925286766559_dp)
           type(ZMM8r8_t), parameter :: half  = ZMM8r8_t(0.5_dp)
           type(ZMM8r8_t) :: fk0,fk1,fk2,fk3
           type(ZMM8r8_t) :: sinc0,sinc1,sinc2,sinc3
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3
           type(ZMM8r8_t) :: hTin,Phi0fk
           real(kind=dp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>8 .and. n<=32) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(7)),8
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=8,min=1,avg=4
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>32) then
              !dir$ assume_aligned fk:64           
              !dir$ assume_aligned Phik:64
              do i=1,iand(n,not(7)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,7
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+8)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+16)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+24)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+32)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_4x_zmm8r8


       subroutine rect_pulse_flux_unroll_16x_ymm8r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(YMM8r4_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: Tin
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(YMM8r4_t) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           type(YMM8r4_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(YMM8r4_t) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(YMM8r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM8r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>8 .and. n<=128) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),8
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=8,min=1,avg=4
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>128) then
              !dir$ assume_aligned fk:32           
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+8)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+16)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+24)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+32)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+40)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+40) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+48)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+56)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+56)= Phi0fk.v(ii)*sinc7.v(ii) 
                      fk8          = fk(idx+64)
                      arg8         = twopi.v(ii)*fk8.v(ii)*hThin.v(ii)
                      sinc8        = sin(arg8.v(ii))/arg8.v(ii)
                      Phik(idx+64)= Phi0fk.v(ii)*sinc8.v(ii) 
                      fk9          = fk(idx+72)
                      arg9         = twopi.v(ii)*fk9.v(ii)*hThin.v(ii)
                      sinc9        = sin(arg9.v(ii))/arg9.v(ii)
                      Phik(idx+72)= Phi0fk.v(ii)*sinc9.v(ii) 
                      fk10         = fk(idx+80)
                      arg10        = twopi.v(ii)*fk10.v(ii)*hThin.v(ii)
                      sinc10       = sin(arg10.v(ii))/arg10.v(ii)
                      Phik(idx+80)= Phi0fk.v(ii)*sinc10.v(ii) 
                      fk11         = fk(idx+88)
                      arg11        = twopi.v(ii)*fk11.v(ii)*hThin.v(ii)
                      sinc11       = sin(arg11.v(ii))/arg11.v(ii)
                      Phik(idx+88)= Phi0fk.v(ii)*sinc11.v(ii) 
                      fk12         = fk(idx+96)
                      arg12        = twopi.v(ii)*fk12.v(ii)*hThin.v(ii)
                      sinc12       = sin(arg12.v(ii))/arg12.v(ii)
                      Phik(idx+96)= Phi0fk.v(ii)*sinc12.v(ii)
                      fk13         = fk(idx+104)
                      arg13        = twopi.v(ii)*fk13.v(ii)*hThin.v(ii)
                      sinc13       = sin(arg13.v(ii))/arg13.v(ii)
                      Phik(idx+104)= Phi0fk.v(ii)*sinc13.v(ii)  
                      fk14         = fk(idx+112)
                      arg14        = twopi.v(ii)*fk14.v(ii)*hThin.v(ii)
                      sinc14       = sin(arg14.v(ii))/arg14.v(ii)
                      Phik(idx+112)= Phi0fk.v(ii)*sinc14.v(ii)  
                      fk15         = fk(idx+120)
                      arg15        = twopi.v(ii)*fk15.v(ii)*hThin.v(ii)
                      sinc15       = sin(arg15.v(ii))/arg15.v(ii)
                      Phik(idx+120)= Phi0fk.v(ii)*sinc15.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_16x_ymm8r4


       subroutine rect_pulse_flux_unroll_8x_ymm8r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_8x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_8x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_8x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(YMM8r4_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: Tin
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(YMM8r4_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(YMM8r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>8 .and. n<=64) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),8
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=8,min=1,avg=4
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>64) then
              !dir$ assume_aligned fk:32           
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+8)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+16)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+24)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+32)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+32) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+40)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+40) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+48)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+48) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+56)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+56)= Phi0fk.v(ii)*sinc7.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_8x_ymm8r4


       subroutine rect_pulse_flux_unroll_4x_ymm8r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           type(YMM8r4_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: Tin
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_t), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t) :: fk0,fk1,fk2,fk3
           type(YMM8r4_t) :: sinc0,sinc1,sinc2,sinc3
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3
           type(YMM8r4_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<8) then
              return
           else if(n==8) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>8 .and. n<=32) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),8
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=8,min=1,avg=4
               do j=i,n
                  fk       = fk(j)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(j)  = Phi0fk.v(0)*sinc
               end do
               return
           else if(n>32) then
              !dir$ assume_aligned fk:32           
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(7)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do ii=0,7
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+8)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+16)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+24)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc3.v(ii)  
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_4x_ymm8r4





      



 




       



       





       
 
   



       







       



       






       


























end module eos_sensor_simd
