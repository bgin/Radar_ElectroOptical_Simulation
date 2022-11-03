

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


     subroutine compute_SN_unroll_16x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM16r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm16r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_zmm16r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_zmm16r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_zmm16r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_zmm16r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_zmm16r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_zmm16r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_zmm16r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_zmm16r4(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_zmm16r4


     subroutine compute_SN_unroll_16x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM16r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm16r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_zmm16r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_zmm16r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_zmm16r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_zmm16r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_zmm16r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_zmm16r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_zmm16r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_zmm16r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_zmm16r4


     subroutine compute_SN_unroll_8x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm16r4(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_zmm16r4


     subroutine compute_SN_unroll_8x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm16r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_zmm16r4


     subroutine compute_SN_unroll_4x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_zmm16r4


     subroutine compute_SN_unroll_4x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm16r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_zmm16r4


     subroutine compute_SN_unroll_2x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1
        type(ZMM16r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_zmm16r4


     subroutine compute_SN_unroll_2x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1
        type(ZMM16r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm16r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_zmm16r4


     subroutine compute_SN_rolled_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_zmm16r4
        !dir$ attributes forceinline :: compute_SN_rolled_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0
        type(ZMM16r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_zmm16r4


     subroutine compute_SN_rolled_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SN_rolled_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0
        type(ZMM16r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm16r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_zmm16r4

     
     subroutine compute_SN_dispatch_zmm16r4(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SN_dispatch_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_zmm16r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_zmm16r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_zmm16r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_zmm16r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_zmm16r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_zmm16r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_zmm16r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_zmm16r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_zmm16r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_zmm16r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_zmm16r4

     
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


     subroutine compute_SN_unroll_16x_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM8r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm8r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_zmm8r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_zmm8r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_zmm8r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_zmm8r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_zmm8r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_zmm8r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_zmm8r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_zmm8r8(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_zmm8r8


     subroutine compute_SN_unroll_16x_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM8r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm8r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_zmm8r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_zmm8r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_zmm8r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_zmm8r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_zmm8r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_zmm8r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_zmm8r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_zmm8r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_zmm8r8


     subroutine compute_SN_unroll_8x_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm8r8(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_zmm8r8


     subroutine compute_SN_unroll_8x_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_zmm8r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_zmm8r8


     subroutine compute_SN_unroll_4x_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_zmm8r8


     subroutine compute_SN_unroll_4x_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_zmm8r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_zmm8r8


     subroutine compute_SN_unroll_2x_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1
        type(ZMM8r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_zmm8r8


     subroutine compute_SN_unroll_2x_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1
        type(ZMM8r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_zmm8r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_zmm8r8


     subroutine compute_SN_rolled_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_zmm8r8
        !dir$ attributes forceinline :: compute_SN_rolled_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0
        type(ZMM8r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_zmm8r8


     subroutine compute_SN_rolled_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SN_rolled_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0
        type(ZMM8r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_zmm8r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_zmm8r8

     
     subroutine compute_SN_dispatch_zmm8r8(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SN_dispatch_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_zmm8r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_zmm8r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_zmm8r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_zmm8r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_zmm8r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_zmm8r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_zmm8r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_zmm8r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_zmm8r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_zmm8r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_zmm8r8


  
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


     subroutine compute_SN_unroll_16x_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM8r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm8r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_ymm8r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_ymm8r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_ymm8r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_ymm8r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_ymm8r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_ymm8r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_ymm8r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_ymm8r4(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_ymm8r4


     subroutine compute_SN_unroll_16x_omp_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM8r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm8r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_ymm8r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_ymm8r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_ymm8r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_ymm8r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_ymm8r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_ymm8r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_ymm8r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_ymm8r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_ymm8r4


     subroutine compute_SN_unroll_8x_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm8r4(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_ymm8r4


     subroutine compute_SN_unroll_8x_omp_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm8r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_ymm8r4


     subroutine compute_SN_unroll_4x_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3
        type(YMM8r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_ymm8r4


     subroutine compute_SN_unroll_4x_omp_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3
        type(YMM8r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm8r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_ymm8r4


     subroutine compute_SN_unroll_2x_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1
        type(YMM8r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_ymm8r4


     subroutine compute_SN_unroll_2x_omp_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1
        type(YMM8r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm8r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_ymm8r4


     subroutine compute_SN_rolled_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_ymm8r4
        !dir$ attributes forceinline :: compute_SN_rolled_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0
        type(YMM8r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_ymm8r4


     subroutine compute_SN_rolled_omp_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SN_rolled_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0
        type(YMM8r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm8r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_ymm8r4

     
     subroutine compute_SN_dispatch_ymm8r4(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SN_dispatch_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_ymm8r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_ymm8r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_ymm8r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_ymm8r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_ymm8r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_ymm8r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_ymm8r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_ymm8r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_ymm8r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_ymm8r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_ymm8r4


     
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


     subroutine compute_SN_unroll_16x_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM4r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm4r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_ymm4r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_ymm4r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_ymm4r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_ymm4r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_ymm4r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_ymm4r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_ymm4r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_ymm4r8(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_ymm4r8


     subroutine compute_SN_unroll_16x_omp_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_16x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM4r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm4r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_ymm4r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_ymm4r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_ymm4r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_ymm4r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_ymm4r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_ymm4r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_ymm4r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_ymm4r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_ymm4r8


     subroutine compute_SN_unroll_8x_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm4r8(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_ymm4r8


     subroutine compute_SN_unroll_8x_omp_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_8x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_ymm4r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_ymm4r8


     subroutine compute_SN_unroll_4x_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3
        type(YMM4r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_ymm4r8


     subroutine compute_SN_unroll_4x_omp_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3
        type(YMM4r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_ymm4r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_ymm4r8


     subroutine compute_SN_unroll_2x_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1
        type(YMM4r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_ymm4r8


     subroutine compute_SN_unroll_2x_omp_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_2x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1
        type(YMM4r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_ymm4r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_ymm4r8


     subroutine compute_SN_rolled_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_ymm4r8
        !dir$ attributes forceinline :: compute_SN_rolled_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0
        type(YMM4r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(48
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_ymm4r8


     subroutine compute_SN_rolled_omp_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_rolled_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SN_rolled_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0
        type(YMM4r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_ymm4r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_ymm4r8

     
     subroutine compute_SN_dispatch_ymm4r8(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SN_dispatch_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_ymm4r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_ymm4r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_ymm4r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_ymm4r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_ymm4r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_ymm4r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_ymm4r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_ymm4r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_ymm4r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_ymm4r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_ymm4r8




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


     subroutine compute_SM_unroll_16x_zmm16r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM16r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm16r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_zmm16r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_zmm16r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_zmm16r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_zmm16r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_zmm16r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_zmm16r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_zmm16r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_zmm16r4(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_zmm16r4


     subroutine compute_SM_unroll_16x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM16r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SN_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm16r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_zmm16r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_zmm16r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_zmm16r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_zmm16r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_zmm16r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_zmm16r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_zmm16r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_zmm16r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_zmm16r4


     subroutine compute_SM_unroll_8x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm16r4(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_zmm16r4


     subroutine compute_SM_unroll_8x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm16r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm16r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm16r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm16r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_zmm16r4


     subroutine compute_SM_unroll_4x_zmm16r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_zmm16r4


     subroutine compute_SM_unroll_4x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1,p2,p3
        type(ZMM16r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm16r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm16r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_zmm16r4


     subroutine compute_SM_unroll_2x_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1
        type(ZMM16r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_zmm16r4


     subroutine compute_SM_unroll_2x_omp_zmm16r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0,p1
        type(ZMM16r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm16r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm16r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_zmm16r4


     subroutine compute_SM_rolled_zmm16r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_zmm16r4
        !dir$ attributes forceinline :: compute_SM_rolled_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0
        type(ZMM16r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_zmm16r4


     subroutine compute_SM_rolled_omp_zmm16r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_omp_zmm16r4
        !dir$ attributes forceinline :: compute_SM_rolled_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_zmm16r4
        use omp_lib
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM16r4_t), automatic :: p0
        type(ZMM16r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SM)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm16r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_zmm16r4

     
     subroutine compute_SM_dispatch_zmm16r4(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SM_dispatch_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM16r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_zmm16r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_zmm16r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_zmm16r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_zmm16r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_zmm16r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_zmm16r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_zmm16r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_zmm16r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_zmm16r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_zmm16r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_zmm16r4



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


     subroutine compute_SM_unroll_16x_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM8r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm8r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_zmm8r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_zmm8r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_zmm8r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_zmm8r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_zmm8r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_zmm8r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_zmm8r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_zmm8r8(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_zmm8r8


     subroutine compute_SM_unroll_16x_omp_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(ZMM8r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm8r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_zmm8r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_zmm8r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_zmm8r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_zmm8r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_zmm8r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_zmm8r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_zmm8r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_zmm8r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_zmm8r8


     subroutine compute_SM_unroll_8x_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm8r8(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_zmm8r8


     subroutine compute_SM_unroll_8x_omp_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_zmm8r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_zmm8r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_zmm8r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_zmm8r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_zmm8r8


     subroutine compute_SM_unroll_4x_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_unroll_4x_zmm8r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_zmm8r8


     subroutine compute_SM_unroll_4x_omp_zmm8r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1,p2,p3
        type(ZMM8r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_zmm8r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_zmm8r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_zmm8r8


     subroutine compute_SM_unroll_2x_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1
        type(ZMM8r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_zmm8r8


     subroutine compute_SM_unroll_2x_omp_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0,p1
        type(ZMM8r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_zmm8r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_zmm8r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_zmm8r8


     subroutine compute_SM_rolled_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_zmm8r8
        !dir$ attributes forceinline :: compute_SM_rolled_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0
        type(ZMM8r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_zmm8r8


     subroutine compute_SM_rolled_omp_zmm8r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_omp_zmm8r8
        !dir$ attributes forceinline :: compute_SM_rolled_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_zmm8r8
        use omp_lib
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(ZMM8r8_t), automatic :: p0
        type(ZMM8r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SM)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_zmm8r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_zmm8r8

     
     subroutine compute_SM_dispatch_zmm8r8(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SM_dispatch_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n), intent(in) :: gamma
        type(ZMM8r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_zmm8r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_zmm8r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_zmm8r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_zmm8r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_zmm8r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_zmm8r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_zmm8r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_zmm8r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_zmm8r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_zmm8r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_zmm8r8



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


     subroutine compute_SM_unroll_16x_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM8r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SN_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SN_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SN_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SN_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SN_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SN_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SN_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SN_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SN_ymm8r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SN_ymm8r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SN_ymm8r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SN_ymm8r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SN_ymm8r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SN_ymm8r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SN_ymm8r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SN_ymm8r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SN_ymm8r4(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_ymm8r4


     subroutine compute_SM_unroll_16x_omp_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM8r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm8r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_ymm8r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_ymm8r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_ymm8r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_ymm8r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_ymm8r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_ymm8r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_ymm8r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_ymm8r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_ymm8r4


     subroutine compute_SM_unroll_8x_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm8r4(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_ymm8r4


     subroutine compute_SM_unroll_8x_omp_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM8r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm8r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm8r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm8r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm8r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm8r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_ymm8r4


     subroutine compute_SM_unroll_4x_ymm8r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3
        type(YMM8r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm8r4(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_ymm8r4


     subroutine compute_SM_unroll_4x_omp_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1,p2,p3
        type(YMM8r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm8r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm8r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_ymm8r4


     subroutine compute_SM_unroll_2x_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1
        type(YMM8r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_ymm8r4


     subroutine compute_SM_unroll_2x_omp_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0,p1
        type(YMM8r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm8r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm8r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_ymm8r4


     subroutine compute_SM_rolled_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_ymm8r4
        !dir$ attributes forceinline :: compute_SM_rolled_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0
        type(YMM8r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_ymm8r4


     subroutine compute_SM_rolled_omp_ymm8r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_omp_ymm8r4
        !dir$ attributes forceinline :: compute_SM_rolled_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_ymm8r4
        use omp_lib
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM8r4_t), automatic :: p0
        type(YMM8r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SM)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm8r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_ymm8r4

     
     subroutine compute_SM_dispatch_ymm8r4(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SM_dispatch_ymm8r4
        type(YMM8r4_t),  intent(in) :: R
        type(YMM8r4_t),  dimension(1:n), intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n), intent(in) :: gamma
        type(YMM8r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_ymm8r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_ymm8r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_ymm8r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_ymm8r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_ymm8r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_ymm8r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_ymm8r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_ymm8r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_ymm8r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_ymm8r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_ymm8r4




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


     subroutine compute_SM_unroll_16x_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM4r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm4r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_ymm4r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_ymm4r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_ymm4r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_ymm4r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_ymm4r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_ymm4r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_ymm4r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_ymm4r8(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_ymm4r8


     subroutine compute_SM_unroll_16x_omp_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_16x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(YMM4r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm4r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_ymm4r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_ymm4r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_ymm4r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_ymm4r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_ymm4r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_ymm4r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_ymm4r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_ymm4r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_ymm4r8


     subroutine compute_SM_unroll_8x_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm4r8(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_ymm4r8


     subroutine compute_SM_unroll_8x_omp_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_8x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(YMM4r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_ymm4r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_ymm4r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_ymm4r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_ymm4r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_ymm4r8


     subroutine compute_SM_unroll_4x_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_4x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3
        type(YMM4r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_ymm4r8


     subroutine compute_SM_unroll_4x_omp_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_4x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1,p2,p3
        type(YMM4r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_ymm4r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_ymm4r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_ymm4r8


     subroutine compute_SM_unroll_2x_ymm4r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1
        type(YMM4r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_ymm4r8


     subroutine compute_SM_unroll_2x_omp_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_unroll_2x_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0,p1
        type(YMM4r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_ymm4r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_ymm4r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_ymm4r8


     subroutine compute_SM_rolled_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_ymm4r8
        !dir$ attributes forceinline :: compute_SM_rolled_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0
        type(YMM4r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(48
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_ymm4r8


     subroutine compute_SM_rolled_omp_ymm4r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_rolled_omp_ymm4r8
        !dir$ attributes forceinline :: compute_SM_rolled_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_ymm4r8
        use omp_lib
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(YMM4r8_t), automatic :: p0
        type(YMM4r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_ymm4r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_ymm4r8

     
     subroutine compute_SM_dispatch_ymm4r8(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_SM_dispatch_ymm4r8
        type(YMM4r8_t),  intent(in) :: R
        type(YMM4r8_t),  dimension(1:n), intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n), intent(in) :: gamma
        type(YMM4r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_ymm4r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_ymm4r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_ymm4r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_ymm4r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_ymm4r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_ymm4r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_ymm4r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_ymm4r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_ymm4r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_ymm4r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_ymm4r8




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


     subroutine ratio_FH_unroll_16x_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm16r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm16r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm16r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm16r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_zmm16r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_zmm16r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_zmm16r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_zmm16r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_zmm16r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_zmm16r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_zmm16r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_zmm16r4(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_zmm16r4


     subroutine ratio_FH_unroll_16x_omp_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_omp_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm16r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm16r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm16r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm16r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_zmm16r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_zmm16r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_zmm16r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_zmm16r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_zmm16r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_zmm16r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_zmm16r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_zmm16r4(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_zmm16r4


     subroutine ratio_FH_unroll_8x_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm16r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm16r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm16r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm16r4(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_zmm16r4


     subroutine ratio_FH_unroll_8x_omp_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_omp_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm16r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm16r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm16r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm16r4(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_zmm16r4


     subroutine ratio_FH_unroll_4x_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_zmm16r4


     subroutine ratio_FH_unroll_4x_omp_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_omp_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM16r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm16r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm16r4(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_zmm16r4



       subroutine ratio_FH_unroll_2x_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM16r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_zmm16r4


     subroutine ratio_FH_unroll_2x_omp_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_omp_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM16r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm16r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm16r4(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_zmm16r4



      subroutine ratio_FH_rolled_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_rolled_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM16r4_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_zmm16r4


     subroutine ratio_FH_rolled_omp_zmm16r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_omp_zmm16r4
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM16r4_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM16r4_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm16r4(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_zmm16r4



     subroutine ratio_FH_dispatch_zmm16r4(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_dispatch_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_zmm16r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_zmm16r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_zmm16r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_zmm16r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_zmm16r4(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_zmm16r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_zmm16r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_zmm16r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_zmm16r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_zmm16r4(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_zmm16r4
      

     




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

    
     subroutine ratio_FH_unroll_16x_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm8r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm8r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm8r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm8r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_zmm8r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_zmm8r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_zmm8r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_zmm8r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_zmm8r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_zmm8r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_zmm8r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_zmm8r8(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_zmm8r8


     subroutine ratio_FH_unroll_16x_omp_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_omp_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm8r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm8r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm8r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm8r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_zmm8r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_zmm8r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_zmm8r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_zmm8r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_zmm8r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_zmm8r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_zmm8r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_zmm8r8(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_zmm8r8


     subroutine ratio_FH_unroll_8x_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm8r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm8r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm8r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm8r8(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_zmm8r8


     subroutine ratio_FH_unroll_8x_omp_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_omp_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_zmm8r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_zmm8r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_zmm8r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_zmm8r8(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_zmm8r8


     subroutine ratio_FH_unroll_4x_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_zmm8r8


     subroutine ratio_FH_unroll_4x_omp_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_omp_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM8r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_zmm8r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_zmm8r8(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_zmm8r8



       subroutine ratio_FH_unroll_2x_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM8r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_zmm8r8


     subroutine ratio_FH_unroll_2x_omp_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_omp_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM8r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_zmm8r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_zmm8r8(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_zmm8r8



      subroutine ratio_FH_rolled_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_rolled_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM8r8_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_zmm8r8


     subroutine ratio_FH_rolled_omp_zmm8r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_omp_zmm8r8
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(ZMM8r8_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM8r8_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_zmm8r8(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_zmm8r8



     subroutine ratio_FH_dispatch_zmm8r8(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_dispatch_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in) :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_zmm8r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_zmm8r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_zmm8r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_zmm8r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_zmm8r8(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_zmm8r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_zmm8r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_zmm8r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_zmm8r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_zmm8r8(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_zmm8r8
      

     

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


     subroutine ratio_FH_unroll_16x_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm8r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm8r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm8r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm8r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_ymm8r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_ymm8r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_ymm8r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_ymm8r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_ymm8r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_ymm8r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_ymm8r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_ymm8r4(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_ymm8r4


     subroutine ratio_FH_unroll_16x_omp_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_omp_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm8r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm8r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm8r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm8r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_ymm8r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_ymm8r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_ymm8r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_ymm8r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_ymm8r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_ymm8r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_ymm8r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_ymm8r4(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_ymm8r4


     subroutine ratio_FH_unroll_8x_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm8r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm8r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm8r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm8r4(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_ymm8r4


     subroutine ratio_FH_unroll_8x_omp_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_omp_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm8r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm8r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm8r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm8r4(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_ymm8r4


     subroutine ratio_FH_unroll_4x_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_ymm8r4


     subroutine ratio_FH_unroll_4x_omp_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_omp_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(YMM8r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm8r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm8r4(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_ymm8r4



       subroutine ratio_FH_unroll_2x_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(YMM8r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_ymm8r4


     subroutine ratio_FH_unroll_2x_omp_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_omp_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(YMM8r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm8r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm8r4(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_ymm8r4



      subroutine ratio_FH_rolled_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_rolled_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(YMM8r4_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_ymm8r4


     subroutine ratio_FH_rolled_omp_ymm8r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_omp_ymm8r4
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM8r4_t), automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(YMM8r4_t), automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm8r4(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_ymm8r4



     subroutine ratio_FH_dispatch_ymm8r4(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_dispatch_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in) :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_ymm8r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_ymm8r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_ymm8r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_ymm8r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_ymm8r4(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_ymm8r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_ymm8r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_ymm8r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_ymm8r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_ymm8r4(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_ymm8r4
      



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


     subroutine ratio_FH_unroll_16x_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm4r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm4r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm4r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm4r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_ymm4r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_ymm4r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_ymm4r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_ymm4r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_ymm4r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_ymm4r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_ymm4r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_ymm4r8(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_ymm4r8


     subroutine ratio_FH_unroll_16x_omp_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_16x_omp_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm4r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm4r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm4r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm4r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_ymm4r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_ymm4r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_ymm4r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_ymm4r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_ymm4r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_ymm4r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_ymm4r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_ymm4r8(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_ymm4r8


     subroutine ratio_FH_unroll_8x_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm4r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm4r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm4r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm4r8(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_ymm4r8


     subroutine ratio_FH_unroll_8x_omp_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_8x_omp_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned psi:64
        !dir$ assume_aligned FH:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_ymm4r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_ymm4r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_ymm4r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_ymm4r8(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_ymm4r8


     subroutine ratio_FH_unroll_4x_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_ymm4r8


     subroutine ratio_FH_unroll_4x_omp_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_4x_omp_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM4r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_ymm4r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_ymm4r8(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_ymm4r8



       subroutine ratio_FH_unroll_2x_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM4r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_ymm4r8


     subroutine ratio_FH_unroll_2x_omp_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_unroll_2x_omp_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM4r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_ymm4r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_ymm4r8(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_ymm4r8



      subroutine ratio_FH_rolled_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_rolled_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM4r8_t), automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_ymm4r8


     subroutine ratio_FH_rolled_omp_ymm4r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_rolled_omp_ymm4r8
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(YMM4r8_t), automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM4r8_t), automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:32
        !dir$ assume_aligned psi:32
        !dir$ assume_aligned FH:32
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_ymm4r8(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_ymm4r8



     subroutine ratio_FH_dispatch_ymm4r8(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_dispatch_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in) :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_ymm4r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_ymm4r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_ymm4r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_ymm4r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_ymm4r8(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_ymm4r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_ymm4r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_ymm4r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_ymm4r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_ymm4r8(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_ymm4r8
      

     

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


     subroutine scan_mirror_ang_unroll_16x_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(ZMM16r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm16r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm16r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm16r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm16r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_zmm16r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_zmm16r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_zmm16r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_zmm16r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_zmm16r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_zmm16r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_zmm16r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_zmm16r4(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_zmm16r4


     
     subroutine scan_mirror_ang_unroll_16x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_omp_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(ZMM16r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm16r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm16r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm16r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm16r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_zmm16r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_zmm16r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_zmm16r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_zmm16r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_zmm16r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_zmm16r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_zmm16r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_zmm16r4(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_zmm16r4


     subroutine scan_mirror_ang_unroll_8x_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm16r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm16r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm16r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm16r4(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_zmm16r4


     
     subroutine scan_mirror_ang_unroll_8x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_omp_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm16r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm16r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm16r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm16r4(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_zmm16r4


     subroutine scan_mirror_ang_unroll_4x_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 64 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_zmm16r4


     
     subroutine scan_mirror_ang_unroll_4x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_omp_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM16r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        type(ZMM16r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 64 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm16r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm16r4(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_zmm16r4


    subroutine scan_mirror_ang_unroll_2x_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM16r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        type(ZMM16r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 64 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_zmm16r4


     
     subroutine scan_mirror_ang_unroll_2x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_omp_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM16r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        type(ZMM16r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 64 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm16r4(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_zmm16r4



     subroutine scan_mirror_ang_rolled_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM16r4_t)   , automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        type(ZMM16r4_t)   , automatic :: g0
        !dir$ attributes align : 64 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_zmm16r4


     
     subroutine scan_mirror_ang_rolled_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_omp_zmm16r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM16r4_t)   , automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM16r4_t)   , automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        type(ZMM16r4_t)   , automatic :: g0
        !dir$ attributes align : 64 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm16r4(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_zmm16r4



     subroutine scan_mirror_ang_dispatch_zmm16r4(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: scan_mirror_ang_dispatch_zmm16r4
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM16r4_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM16r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_zmm16r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_zmm16r4


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


     subroutine scan_mirror_ang_unroll_16x_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(ZMM8r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm8r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm8r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm8r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm8r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_zmm8r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_zmm8r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_zmm8r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_zmm8r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_zmm8r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_zmm8r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_zmm8r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_zmm8r8(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_zmm8r8


     
     subroutine scan_mirror_ang_unroll_16x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_omp_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(ZMM8r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm8r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm8r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm8r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm8r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_zmm8r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_zmm8r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_zmm8r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_zmm8r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_zmm8r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_zmm8r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_zmm8r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_zmm8r8(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_zmm8r8


     subroutine scan_mirror_ang_unroll_8x_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm8r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm8r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm8r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm8r8(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_zmm8r8


     
     subroutine scan_mirror_ang_unroll_8x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_omp_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 64 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r9(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_zmm8r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_zmm8r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_zmm8r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_zmm8r8(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_zmm8r8


     subroutine scan_mirror_ang_unroll_4x_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 64 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_zmm8r8


     
     subroutine scan_mirror_ang_unroll_4x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_omp_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 64 :: ps0,ps1,ps2,ps3
        type(ZMM8r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 64 :: ph0,ph1,ph2,ph3
        type(ZMM8r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 64 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_zmm8r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_zmm8r8(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_zmm8r8


    subroutine scan_mirror_ang_unroll_2x_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM8r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        type(ZMM8r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 64 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_zmm8r8


     
     subroutine scan_mirror_ang_unroll_2x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_omp_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 64 :: ps0,ps1
        type(ZMM8r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 64 :: ph0,ph1
        type(ZMM8r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 64 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_zmm8r8(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_zmm8r8



     subroutine scan_mirror_ang_rolled_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM8r8_t)   , automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        type(ZMM8r8_t)   , automatic :: g0
        !dir$ attributes align : 64 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_zmm8r8


     
     subroutine scan_mirror_ang_rolled_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_omp_zmm8r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(ZMM8r8_t)   , automatic :: ps0
        !dir$ attributes align : 64 :: ps0
        type(ZMM8r8_t)   , automatic :: ph0
        !dir$ attributes align : 64 :: ph0
        type(ZMM8r8_t)   , automatic :: g0
        !dir$ attributes align : 64 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_zmm8r8(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_zmm8r8



     subroutine scan_mirror_ang_dispatch_zmm8r8(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: scan_mirror_ang_dispatch_zmm8r8
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(ZMM8r8_t),  dimension(1:n),  intent(in)  :: phi
        type(ZMM8r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_zmm8r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_zmm8r8




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


     subroutine scan_mirror_ang_unroll_16x_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(YMM8r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm8r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm8r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm8r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm8r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_ymm8r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_ymm8r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_ymm8r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_ymm8r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_ymm8r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_ymm8r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_ymm8r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_ymm8r4(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_ymm8r4


     
     subroutine scan_mirror_ang_unroll_16x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_omp_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(YMM8r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:31
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm8r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm8r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm8r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm8r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_ymm8r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_ymm8r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_ymm8r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_ymm8r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_ymm8r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_ymm8r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_ymm8r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_ymm8r4(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_ymm8r4


     subroutine scan_mirror_ang_unroll_8x_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm8r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm8r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm8r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm8r4(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_ymm8r4


     
     subroutine scan_mirror_ang_unroll_8x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_omp_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm8r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm8r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm8r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm8r4(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_ymm8r4


     subroutine scan_mirror_ang_unroll_4x_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 32 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_ymm8r4


     
     subroutine scan_mirror_ang_unroll_4x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_omp_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM8r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        type(YMM8r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 32 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm8r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm8r4(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_ymm8r4


    subroutine scan_mirror_ang_unroll_2x_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM8r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        type(YMM8r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 32 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_ymm8r4


     
     subroutine scan_mirror_ang_unroll_2x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_omp_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM8r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        type(YMM8r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 32 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm8r4(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_ymm8r4



     subroutine scan_mirror_ang_rolled_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM8r4_t)   , automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        type(YMM8r4_t)   , automatic :: g0
        !dir$ attributes align : 32 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_ymm8r4


     
     subroutine scan_mirror_ang_rolled_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_omp_ymm8r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_ymm8r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM8r4_t)   , automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM8r4_t)   , automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        type(YMM8r4_t)   , automatic :: g0
        !dir$ attributes align : 32 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm8r4(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_ymm8r4



     subroutine scan_mirror_ang_dispatch_ymm8r4(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: scan_mirror_ang_dispatch_ymm8r4
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM8r4_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM8r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_ymm8r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_ymm8r4



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


     subroutine scan_mirror_ang_unroll_16x_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(YMM4r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm4r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm4r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm4r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm4r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_ymm4r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_ymm4r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_ymm4r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_ymm4r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_ymm4r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_ymm4r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_ymm4r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_ymm4r8(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_ymm4r8


     
     subroutine scan_mirror_ang_unroll_16x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_omp_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(YMM4r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:31
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm4r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm4r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm4r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm4r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_ymm4r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_ymm4r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_ymm4r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_ymm4r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_ymm4r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_ymm4r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_ymm4r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_ymm4r8(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_ymm4r8


     subroutine scan_mirror_ang_unroll_8x_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm4r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm4r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm4r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm4r8(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_ymm4r8


     
     subroutine scan_mirror_ang_unroll_8x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_omp_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 32 :: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_ymm4r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_ymm4r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_ymm4r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_ymm4r8(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_ymm4r8


     subroutine scan_mirror_ang_unroll_4x_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 32 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_ymm4r8


     
     subroutine scan_mirror_ang_unroll_4x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_omp_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 32 :: ps0,ps1,ps2,ps3
        type(YMM4r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 32 :: ph0,ph1,ph2,ph3
        type(YMM4r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 32 :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_ymm4r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_ymm4r8(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_ymm4r8


    subroutine scan_mirror_ang_unroll_2x_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM4r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        type(YMM4r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 32 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_ymm4r8


     
     subroutine scan_mirror_ang_unroll_2x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_omp_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 32 :: ps0,ps1
        type(YMM4r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 32 :: ph0,ph1
        type(YMM4r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 32 :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_ymm4r8(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_ymm4r8



     subroutine scan_mirror_ang_rolled_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMMr8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM4r8_t)   , automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        type(YMM4r8_t)   , automatic :: g0
        !dir$ attributes align : 32 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:64
           !dir$ assume_aligned phi:64
           !dir$ assume_aligned psi:64
           !dir$ assume_aligned gamma:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_ymm4r8


     
     subroutine scan_mirror_ang_rolled_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_omp_ymm4r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_ymm4r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(YMM4r8_t)   , automatic :: ps0
        !dir$ attributes align : 32 :: ps0
        type(YMM4r8_t)   , automatic :: ph0
        !dir$ attributes align : 32 :: ph0
        type(YMM4r8_t)   , automatic :: g0
        !dir$ attributes align : 32 :: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:32
           !dir$ assume_aligned phi:32
           !dir$ assume_aligned psi:32
           !dir$ assume_aligned gamma:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_ymm4r8(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_ymm4r8



     subroutine scan_mirror_ang_dispatch_ymm4r8(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: scan_mirror_ang_dispatch_ymm4r8
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: psi
        type(YMM4r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMM4r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_ymm4r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_ymm4r8



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


    subroutine defocus_cof_unroll_16x_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(ZMM16r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm16r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm16r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm16r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm16r4(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_zmm16r4(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_zmm16r4(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_zmm16r4(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_zmm16r4(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_zmm16r4(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_zmm16r4(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_zmm16r4(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_zmm16r4(l2,a15,O,inf) 
        end do
    end subroutine defocus_cof_unroll_16x_zmm16r4


    subroutine defocus_cof_unroll_16x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_omp_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_omp_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(ZMM16r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm16r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm16r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm16r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm16r4(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_zmm16r4(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_zmm16r4(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_zmm16r4(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_zmm16r4(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_zmm16r4(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_zmm16r4(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_zmm16r4(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_zmm16r4(l2,a15,O,inf) 
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_16x_zmm16r4


    subroutine defocus_cof_unroll_8x_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm16r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm16r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm16r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm16r4(l2,a7,O,inf)
         end do
    end subroutine defocus_cof_unroll_8x_zmm16r4


    subroutine defocus_cof_unroll_8x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_omp_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_omp_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm16r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm16r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm16r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm16r4(l2,a7,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_8x_zmm16r4


     subroutine defocus_cof_unroll_4x_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 64 :: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
        end do
    end subroutine defocus_cof_unroll_4x_zmm16r4


    subroutine defocus_cof_unroll_4x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_omp_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_omp_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 64 :: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm16r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm16r4(l2,a3,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_4x_zmm16r4

    
    subroutine defocus_cof_unroll_2x_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1
        !dir$ attributes align : 64 :: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
        end do
    end subroutine defocus_cof_unroll_2x_zmm16r4


    


    subroutine defocus_cof_unroll_2x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_omp_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_omp_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0,a1
        !dir$ attributes align : 64 :: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm16r4(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm16r4(l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_2x_zmm16r4


    subroutine defocus_cof_rolled_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_rolled_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0
        !dir$ attributes align : 64 :: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
        end do
    end subroutine defocus_cof_rolled_zmm16r4


    subroutine defocus_cof_rolled_omp_zmm16r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_omp_zmm16r4
        !dir$ attributes forceinline :: defocus_cof_rolled_omp_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_omp_zmm16r4
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM16r4_t), automatic :: a0
        !dir$ attributes align : 64 :: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0)
        !$omp shared(n,alpha,dc,O,inf)
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm16r4(l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_rolled_zmm16r4


    subroutine defocus_cof_dispatch_zmm16r4(l2,alpha,O,inf,dc,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 ::
        type(ZMM16r4_t),                 intent(in) :: l2
        type(ZMM16r4_t), dimension(1:n), intent(in) :: alpha
        type(ZMM16r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM16r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        integer(kind=i4),                intent(in) :: unroll_cnt
        logical(kind=i4),                intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_zmm16r4(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_zmm16r4(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_zmm16r4(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_zmm16r4(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_zmm16r4(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select
        else
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_omp_zmm16r4(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_omp_zmm16r4(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select 
        end if
    end subroutine defocus_cof_dispatch_zmm16r4



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


    subroutine defocus_cof_unroll_16x_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(ZMM8r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm8r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm8r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm8r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm8r8(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_zmm8r8(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_zmm8r8(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_zmm8r8(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_zmm8r8(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_zmm8r8(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_zmm8r8(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_zmm8r8(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_zmm8r8(l2,a15,O,inf) 
        end do
    end subroutine defocus_cof_unroll_16x_zmm8r8


    subroutine defocus_cof_unroll_16x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_omp_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_omp_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(ZMM8r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm8r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm8r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm8r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm8r8(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_zmm8r8(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_zmm8r8(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_zmm8r8(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_zmm8r8(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_zmm8r8(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_zmm8r8(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_zmm8r8(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_zmm8r8(l2,a15,O,inf) 
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_16x_zmm8r8


    subroutine defocus_cof_unroll_8x_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm8r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm8r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm8r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm8r8(l2,a7,O,inf)
         end do
    end subroutine defocus_cof_unroll_8x_zmm8r8


    subroutine defocus_cof_unroll_8x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_omp_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_omp_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 64 :: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_zmm8r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_zmm8r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_zmm8r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_zmm8r8(l2,a7,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_8x_zmm8r8


     subroutine defocus_cof_unroll_4x_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 64 :: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
        end do
    end subroutine defocus_cof_unroll_4x_zmm8r8


    subroutine defocus_cof_unroll_4x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_omp_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_omp_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 64 :: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_zmm8r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_zmm8r8(l2,a3,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_4x_zmm8r8

    
    subroutine defocus_cof_unroll_2x_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1
        !dir$ attributes align : 64 :: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
        end do
    end subroutine defocus_cof_unroll_2x_zmm8r8


    


    subroutine defocus_cof_unroll_2x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_omp_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_omp_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0,a1
        !dir$ attributes align : 64 :: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_zmm8r8(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0,a1)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_zmm8r8(l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_2x_zmm8r8


    subroutine defocus_cof_rolled_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_rolled_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0
        !dir$ attributes align : 64 :: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
        end do
    end subroutine defocus_cof_rolled_zmm8r8


    subroutine defocus_cof_rolled_omp_zmm8r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_omp_zmm8r8
        !dir$ attributes forceinline :: defocus_cof_rolled_omp_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_omp_zmm8r8
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(ZMM8r8_t), automatic :: a0
        !dir$ attributes align : 64 :: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:64
        !dir$ assume_aligned dc:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=256) &
        !$omp firstprivate(m1) private(a0)
        !$omp shared(n,alpha,dc,O,inf)
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_zmm8r8(l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_rolled_zmm8r8


    subroutine defocus_cof_dispatch_zmm8r8(l2,alpha,O,inf,dc,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 ::
        type(ZMM8r8_t),                 intent(in) :: l2
        type(ZMM8r8_t), dimension(1:n), intent(in) :: alpha
        type(ZMM8r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(ZMM8r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        integer(kind=i4),                intent(in) :: unroll_cnt
        logical(kind=i4),                intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_zmm8r8(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_zmm8r8(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_zmm8r8(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_zmm8r8(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_zmm8r8(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select
        else
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_omp_zmm8r8(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_omp_zmm8r8(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select 
        end if
    end subroutine defocus_cof_dispatch_zmm8r8


    


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


     !AVX/AVX2
     subroutine project_xy_axis_ymm8r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_ymm8r4
         !dir$ attributes forceinline ::  project_xy_axis_ymm8r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_ymm8r4
         type(YMM8r4_t),  intent(in) :: l
         type(YMM8r4_t),  intent(in) :: alpha
         type(YMM8r4_t),  intent(in) :: xl
         type(YMM8r4_t),  intent(in) :: yl
         type(YMM8r4_t), automatic :: absl
         !dir$ attributes align : 32 :: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
     end subroutine project_xy_axis_ymm8r4

     
     subroutine project_xy_axis_ymm4r8(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_ymm4r8
         !dir$ attributes forceinline ::  project_xy_axis_ymm4r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_ymm4r8
         type(YMM4r8_t),  intent(in) :: l
         type(YMM4r8_t),  intent(in) :: alpha
         type(YMM4r8_t),  intent(in) :: xl
         type(YMM4r8_t),  intent(in) :: yl
         type(YMM4r8_t), automatic :: absl
         !dir$ attributes align : 32 :: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
     end subroutine project_xy_axis_ymm4r8 

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


     !AVX/AVX2 version
     pure function s_shift_ymm8r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_ymm8r4
         !dir$ attributes forceinline ::  s_shift_ymm8r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_ymm8r4
         type(YMM8r4_t),  intent(in) :: l
         type(YMM8r4_t),  intent(in) :: alpha
         type(YMM8r4_t),  intent(in) :: gamma
         type(YMM8r4_t) :: s
         type(YMM8r4_t), automatic :: ag,sag
         !dir$ attributes align : 32 :: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
     end function s_shift_ymm8r4


     pure function s_shift_ymm4r8(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_ymm4r8
         !dir$ attributes forceinline ::  s_shift_ymm4r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_ymm4r8
         type(YMM4r8_t),  intent(in) :: l
         type(YMM4r8_t),  intent(in) :: alpha
         type(YMM4r8_t),  intent(in) :: gamma
         type(YMM4r8_t) :: s
         type(YMM4r8_t), automatic :: ag,sag
         !dir$ attributes align : 32 :: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
     end function s_shift_ymm4r8


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


      !AVX/AVX2 version
      subroutine project_s_xy_ymm8r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_ymm8r4
         !dir$ attributes forceinline ::  project_s_xy_ymm8r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_ymm8r4
         type(YMM8r4_t),   intent(in)  :: s
         type(YMM8r4_t),   intent(in)  :: gamma
         type(YMM8r4_t),   intent(out) :: xs
         type(YMM8r4_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_ymm8r4 

      subroutine project_s_xy_ymm4r8(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_ymm4r8
         !dir$ attributes forceinline ::  project_s_xy_ymm4r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_ymm4r8
         type(YMM4r8_t),   intent(in)  :: s
         type(YMM4r8_t),   intent(in)  :: gamma
         type(YMM4r8_t),   intent(out) :: xs
         type(YMM4r8_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_ymm4r8




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


     !AVX/AVX2 version
     pure function ray_intercept_pa_ymm8r4(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_ymm8r4
           !dir$ attributes forceinline ::  ray_intercept_pa_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_ymm8r4
           type(YMM8r4_t),  intent(in) :: delta
           type(YMM8r4_t),  intent(in) :: alpha
           type(YMM8r4_t),  intent(in) :: gamma
           type(YMM8r4_t),  intent(in) :: n
           type(YMM8r4_t) :: sp
           type(YMM8r4_t), parameter :: one = YMM8r4_t(1.0_sp)
           type(YMM8r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
      end function ray_intercept_pa_ymm8r4

 
     pure function ray_intercept_pa_ymm4r8(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_ymm4r8
           !dir$ attributes forceinline ::  ray_intercept_pa_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_ymm4r8
           type(YMM4r8_t),  intent(in) :: delta
           type(YMM4r8_t),  intent(in) :: alpha
           type(YMM4r8_t),  intent(in) :: gamma
           type(YMM4r8_t),  intent(in) :: n
           type(YMM4r8_t) :: sp
           type(YMM4r8_t), parameter :: one = YMM4r8_t(1.0_dp)
           type(YMM4r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
     end function ray_intercept_pa_ymm4r8


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


    !AVX/AVX2 versions.
    pure function ray_intercept_na_ymm8r4(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_ymm8r4
           !dir$ attributes forceinline ::  ray_intercept_na_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_ymm8r4
           type(YMM8r4_t),  intent(in) :: delta
           type(YMM8r4_t),  intent(in) :: alpha
           type(YMM8r4_t),  intent(in) :: gamma
           type(YMM8r4_t),  intent(in) :: n
           type(YMM8r4_t) :: sn
           type(YMM8r4_t), parameter :: one = YMM8r4_t(1.0_sp)
           type(YMM8r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 32 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
    end function ray_intercept_na_ymm8r4
 
 
    pure function ray_intercept_na_ymm4r8(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_ymm4r8
           !dir$ attributes forceinline ::  ray_intercept_na_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_ymm4r8
           type(YMM4r8_t),  intent(in) :: delta
           type(YMM4r8_t),  intent(in) :: alpha
           type(YMM4r8_t),  intent(in) :: gamma
           type(YMM4r8_t),  intent(in) :: n
           type(YMM4r8_t) :: sn
           type(YMM4r8_t), parameter :: one = YMM4r8_t(1.0_dp)
           type(YMM4r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 64 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
    end function ray_intercept_na_ymm4r8

    subroutine paraxial_xdyd_zmm16r4(gamma,alpha,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: paraxial_xdyd_zmm16r4
            !dir$ attributes forceinline ::  paraxial_xdyd_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  paraxial_xdyd_zmm16r4
            type(ZMM16r4_t),   intent(in)  :: gamma
            type(ZMM16r4_t),   intent(in)  :: alpha
            type(ZMM16r4_t),   intent(in)  :: n
            type(ZMM16r4_t),   intent(out) :: xd
            type(ZMM16r4_t),   intent(out) :: yd
            type(ZMM16r4_t), parameter :: one = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
            n2    = n.v*n.v
            cosg  = cos(gamma.v)
            cos2g = cos(gamma.v+gamma.v)
            sing  = sin(gamma.v)
            sin4g = sing.v*sing.v*sing.v*sing.v
            num   = n2.v*cos2g.v+sin4g.v
            den   = (n2.v-sing.v*sing.v)**1.5_sp
            xd    = cosg.v-num.v/den.v
            t0    = sqrt(n2.v-sing.v*sing.v)
            t1    = one.v-cosg.v/t0.v
            yd    = sing.v*t1.v
    end subroutine paraxial_xdyd_zmm16r4
    
    subroutine paraxial_xdyd_zmm8r8(gamma,alpha,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: paraxial_xdyd_zmm8r8
            !dir$ attributes forceinline ::  paraxial_xdyd_zmm8r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  paraxial_xdyd_zmm8r8
            type(ZMM8r8_t),   intent(in)  :: gamma
            type(ZMM8r8_t),   intent(in)  :: alpha
            type(ZMM8r8_t),   intent(in)  :: n
            type(ZMM8r8_t),   intent(out) :: xd
            type(ZMM8r8_t),   intent(out) :: yd
            type(ZMM8r8_t), parameter :: one = ZMM8r8_t(1.0_dp)
            type(ZMM8r8_t), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
            n2    = n.v*n.v
            cosg  = cos(gamma.v)
            cos2g = cos(gamma.v+gamma.v)
            sing  = sin(gamma.v)
            sin4g = sing.v*sing.v*sing.v*sing.v
            num   = n2.v*cos2g.v+sin4g.v
            den   = (n2.v-sing.v*sing.v)**1.5_dp
            xd    = cosg.v-num.v/den.v
            t0    = sqrt(n2.v-sing.v*sing.v)
            t1    = one.v-cosg.v/t0.v
            yd    = sing.v*t1.v
        end subroutine paraxial_xdyd_zmm8r8
    
    
        
	
	


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


      !AVX/AVX2 versions
      pure function ray_diff_ymm8r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_ymm8r4
           !dir$ attributes forceinline ::  ray_diff_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_ymm8r4
           use mod_fpcompare, only :  ymm8r4_rgt_ymm8r4
           type(YMM8r4_t),  intent(in) :: delta
           type(YMM8r4_t),  intent(in) :: alpha
           type(YMM8r4_t),  intent(in) :: gamma
           type(YMM8r4_t),  intent(in) :: n
           type(YMM8r4_t),  intent(in) :: u
           type(YMM8r4_t) :: ds
           type(YMM8r4_t), parameter :: two  = YMM8r4_t(2.0_sp)
           type(YMM8r4_t), parameter :: half = YMM8r4_t(0.5_sp)
           type(YMM8r4_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 32 :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask8_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = ymm8r4_rgt_ymm8r4(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_ymm8r4(delta,alpha,gamma,n)
              t1 = ray_intercept_na_ymm8r4(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
        end function ray_diff_ymm8r4


      pure function ray_diff_ymm4r8(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_ymm4r8
           !dir$ attributes forceinline ::  ray_diff_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_ymm4r8
           use mod_fpcompare, only :  ymm4r8_rgt_ymm4r8
           type(YMM4r8_t),  intent(in) :: delta
           type(YMM4r8_t),  intent(in) :: alpha
           type(YMM4r8_t),  intent(in) :: gamma
           type(YMM4r8_t),  intent(in) :: n
           type(YMM4r8_t),  intent(in) :: u
           type(YMM4r8_t) :: ds
           type(YMM4r8_t), parameter :: two  = YMM4r8_t(2.0_dp)
           type(YMM4r8_t), parameter :: half = YMM4r8_t(0.5_dp)
           type(YMM4r8_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 32 :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask4_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = ymm4r8_rgt_ymm4r8(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_ymm4r8(delta,alpha,gamma,n)
              t1 = ray_intercept_na_ymm4r8(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
      end function ray_diff_ymm4r8



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

    
     !AVX/AVX2 versions
      subroutine compute_dxdy_ymm8r4(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_ymm8r4
            !dir$ attributes forceinline ::  compute_dxdy_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_ymm8r4
            type(YMM8r4_t),   intent(in)  :: alpha
            type(YMM8r4_t),   intent(in)  :: beta
            type(YMM8r4_t),   intent(in)  :: delta
            type(YMM8r4_t),   intent(in)  :: gamma
            type(YMM8r4_t),   intent(in)  :: n
            type(YMM8r4_t),   intent(in)  :: u
            type(YMM8r4_t),   intent(out) :: dx
            type(YMM8r4_t),   intent(out) :: dy
            type(YMM8r4_t), parameter :: two = YMM8r4_t(2.0_sp)
            type(YMM8r4_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_ymm8r4(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
      end subroutine compute_dxdy_ymm8r4


      subroutine compute_dxdy_ymm4r8(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_ymm4r8
            !dir$ attributes forceinline ::  compute_dxdy_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_ymm4r8
            type(YMM4r8_t),   intent(in)  :: alpha
            type(YMM4r8_t),   intent(in)  :: beta
            type(YMM4r8_t),   intent(in)  :: delta
            type(YMM4r8_t),   intent(in)  :: gamma
            type(YMM4r8_t),   intent(in)  :: n
            type(YMM4r8_t),   intent(in)  :: u
            type(YMM4r8_t),   intent(out) :: dx
            type(YMM4r8_t),   intent(out) :: dy
            type(YMM4r8_t), parameter :: two = YMM4r8_t(2.0_dp)
            type(YMM4r8_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_ymm4r8(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
     end subroutine compute_dxdy_ymm4r8



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


    !AVX/AVX2 versions
     subroutine compute_xy_ymm8r4(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_ymm8r4
            !dir$ attributes forceinline ::  compute_xy_ymm8r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_ymm8r4
            type(YMM8r4_t),   intent(in)  :: alpha
            type(YMM8r4_t),   intent(in)  :: beta
            type(YMM8r4_t),   intent(in)  :: delta
            type(YMM8r4_t),   intent(in)  :: gamma
            type(YMM8r4_t),   intent(in)  :: n
            type(YMM8r4_t),   intent(in)  :: u
            type(YMM8r4_t),   intent(out) :: x
            type(YMM8r4_t),   intent(out) :: y
            type(YMM8r4_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 32 :: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_ymm8r4(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_ymm8r4(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
     end subroutine compute_xy_ymm8r4


     subroutine compute_xy_ymm4r8(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_ymm4r8
            !dir$ attributes forceinline ::  compute_xy_ymm4r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_ymm4r8
            type(YMM4r8_t),   intent(in)  :: alpha
            type(YMM4r8_t),   intent(in)  :: beta
            type(YMM4r8_t),   intent(in)  :: delta
            type(YMM4r8_t),   intent(in)  :: gamma
            type(YMM4r8_t),   intent(in)  :: n
            type(YMM4r8_t),   intent(in)  :: u
            type(YMM4r8_t),   intent(out) :: x
            type(YMM4r8_t),   intent(out) :: y
            type(YMM4r8_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 32 :: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_ymm4r8(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_ymm4r8(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
     end subroutine compute_xy_ymm4r8


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


  
       subroutine rect_pulse_flux_unroll_16x_ymm4r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           type(YMM4r8_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: Tin
           type(YMM4r8_t), parameter :: twopi = YMM4r8_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t), parameter :: half  = YMM4r8_t(0.5_dp)
           type(YMM4r8_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(YMM4r8_t) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           type(YMM4r8_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(YMM4r8_t) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(YMM4r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM4r8_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>4 .and. n<=64) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(3)),4
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=4,min=1,avg=2
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
              do i=1,iand(n,not(3)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+4)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+4) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+8)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+12)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+12) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+16)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+20)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+20) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+24)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+28)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                      Phik(idx+28)= Phi0fk.v(ii)*sinc7.v(ii) 
                      fk8          = fk(idx+32)
                      arg8         = twopi.v(ii)*fk8.v(ii)*hThin.v(ii)
                      sinc8        = sin(arg8.v(ii))/arg8.v(ii)
                      Phik(idx+32)= Phi0fk.v(ii)*sinc8.v(ii) 
                      fk9          = fk(idx+36)
                      arg9         = twopi.v(ii)*fk9.v(ii)*hThin.v(ii)
                      sinc9        = sin(arg9.v(ii))/arg9.v(ii)
                      Phik(idx+36)= Phi0fk.v(ii)*sinc9.v(ii) 
                      fk10         = fk(idx+40)
                      arg10        = twopi.v(ii)*fk10.v(ii)*hThin.v(ii)
                      sinc10       = sin(arg10.v(ii))/arg10.v(ii)
                      Phik(idx+40)= Phi0fk.v(ii)*sinc10.v(ii) 
                      fk11         = fk(idx+44)
                      arg11        = twopi.v(ii)*fk11.v(ii)*hThin.v(ii)
                      sinc11       = sin(arg11.v(ii))/arg11.v(ii)
                      Phik(idx+44)= Phi0fk.v(ii)*sinc11.v(ii) 
                      fk12         = fk(idx+48)
                      arg12        = twopi.v(ii)*fk12.v(ii)*hThin.v(ii)
                      sinc12       = sin(arg12.v(ii))/arg12.v(ii)
                      Phik(idx+48)= Phi0fk.v(ii)*sinc12.v(ii)
                      fk13         = fk(idx+52)
                      arg13        = twopi.v(ii)*fk13.v(ii)*hThin.v(ii)
                      sinc13       = sin(arg13.v(ii))/arg13.v(ii)
                      Phik(idx+52)= Phi0fk.v(ii)*sinc13.v(ii)  
                      fk14         = fk(idx+56)
                      arg14        = twopi.v(ii)*fk14.v(ii)*hThin.v(ii)
                      sinc14       = sin(arg14.v(ii))/arg14.v(ii)
                      Phik(idx+56)= Phi0fk.v(ii)*sinc14.v(ii)  
                      fk15         = fk(idx+60)
                      arg15        = twopi.v(ii)*fk15.v(ii)*hThin.v(ii)
                      sinc15       = sin(arg15.v(ii))/arg15.v(ii)
                      Phik(idx+60)= Phi0fk.v(ii)*sinc15.v(ii) 
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_16x_ymm4r8


       subroutine rect_pulse_flux_unroll_8x_ymm4r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_8x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_8x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_8x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           type(YMM4r8_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: Tin
           type(YMM4r8_t), parameter :: twopi = YMM4r8_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t), parameter :: half  = YMM4r8_t(0.5_dp)
           type(YMM4r8_t) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           type(YMM4r8_t) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           type(YMM4r8_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>4 .and. n<=32) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(3)),4
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=4,min=1,avg=2
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
              do i=1,iand(n,not(3)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+4)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+4) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+8)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+12)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+12) = Phi0fk.v(ii)*sinc3.v(ii)  
                      fk4          = fk(idx+16)
                      arg4         = twopi.v(ii)*fk4.v(ii)*hThin.v(ii)
                      sinc4        = sin(arg4.v(ii))/arg4.v(ii)
                      Phik(idx+16) = Phi0fk.v(ii)*sinc4.v(ii) 
                      fk5          = fk(idx+20)
                      arg5         = twopi.v(ii)*fk5.v(ii)*hThin.v(ii)
                      sinc5        = sin(arg5.v(ii))/arg5.v(ii)
                      Phik(idx+20) = Phi0fk.v(ii)*sinc5.v(ii)
                      fk6          = fk(idx+24)
                      arg6         = twopi.v(ii)*fk6.v(ii)*hThin.v(ii)
                      sinc6        = sin(arg6.v(ii))/arg6.v(ii)
                      Phik(idx+24) = Phi0fk.v(ii)*sinc6.v(ii)  
                      fk7          = fk(idx+28)
                      arg7         = twopi.v(ii)*fk7.v(ii)*hThin.v(ii)
                      sinc7        = sin(arg7.v(ii))/arg7.v(ii)
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_8x_ymm4r8



       subroutine rect_pulse_flux_unroll_4x_ymm4r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_ymm4r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           type(YMM4r8_t),                intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           type(YMM4r8_t),                intent(in)  :: Tin
           type(YMM4r8_t), parameter :: twopi = YMM4r8_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_t), parameter :: half  = YMM4r8_t(0.5_dp)
           type(YMM4r8_t) :: fk0,fk1,fk2,fk3
           type(YMM4r8_t) :: sinc0,sinc1,sinc2,sinc3
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3
           type(YMM4r8_t) :: hTin,Phi0fk
           real(kind=sp)   :: fk,sinc,arg
           integer(kind=i4) :: i,ii,j,idx
           hTin.v   = half.v*Tin.v
           Phi0fk.v = Phi0.v*Tin.v
           if(n<4) then
              return
           else if(n==4) then
              do i=1,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           else if(n>4 .and. n<=16) then
              !dir$ assume_aligned fk:32          
              !dir$ assume_aligned Phik:32
              do i=1,iand(n,not(3)),4
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      fk0   = fk(ii+i)
                      arg0  = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0 = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i) = Phi0fk.v(ii)*sinc0.v(ii) 
                   end do
               end do
               ! Remainder loop
               !dir$ loop_count max=4,min=1,avg=2
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
              do i=1,iand(n,not(3)),16
                  call mm_prefetch(freq(i*16),FOR_K_PREFETCH_T1)
                
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(8)
                   !dir$ vector always
                   do ii=0,3
                      idx          = ii+i
                      fk0          = fk(ii+i)
                      arg0         = twopi.v(ii)*fk0.v(ii)*hThin.v(ii)
                      sinc0        = sin(arg0.v(ii))/arg0.v(ii)
                      Phik(ii+i)   = Phi0fk.v(ii)*sinc0.v(ii) 
                      fk1          = fk(idx+4)
                      arg1         = twopi.v(ii)*fk1.v(ii)*hThin.v(ii)
                      sinc1        = sin(arg1.v(ii))/arg1.v(ii)
                      Phik(idx+4) = Phi0fk.v(ii)*sinc1.v(ii) 
                      fk2          = fk(idx+8)
                      arg2         = twopi.v(ii)*fk2.v(ii)*hThin.v(ii)
                      sinc2        = sin(arg2.v(ii))/arg2.v(ii)
                      Phik(idx+8) = Phi0fk.v(ii)*sinc2.v(ii)
                      fk3          = fk(idx+12)
                      arg3         = twopi.v(ii)*fk3.v(ii)*hThin.v(ii)
                      sinc3        = sin(arg3.v(ii))/arg3.v(ii)
                      Phik(idx+12) = Phi0fk.v(ii)*sinc3.v(ii)  
                   end do
               end do
                ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                  fk       = fk(i)
                  arg      = twopi.v(0)*fk*hTin.v(0)
                  sinc     = sin(arg)/arg
                  Phik(i)  = Phi0fk.v(0)*sinc
              end do
              return
           end if
       end subroutine rect_pulse_flux_unroll_4x_ymm4r8



       subroutine rect_pulse_amp_unroll_16x_zmm16r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_zmm16r4
           use mod_fpcompare, only : zmm16r4_equalto_zmm16r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM16r4_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM16r4_t),                intent(in)  :: tin
           type(ZMM16r4_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_sp)
           type(ZMM16r4_t), parameter :: two = ZMM16r4_t(2.0_sp)
           type(ZMM16r4_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(ZMM16r4_t) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           !dir$ attributes align : 64 :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM16r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 64 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM16r4_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 64 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(ZMM16r4_t) :: k8,k9,k10,k11,k12,k13,k14,k15
           !dir$ attributes align : 64 :: k8,k9,k10,k11,k12,k13,k14,k15
           type(ZMM16r4_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask16_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<16) return
           twoT = two.v/T.v
           m = zmm16r4_equalto_zmm16r4(tin,twoT)
           if(all(m)) then
              if(n==16) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>16 .and. n<=256) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>256) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),256
                    call mm_prefetch(k(i*256),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+16)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+32)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+48)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+64)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+64) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+80)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+80) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+96)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+96) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+112)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+112)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                         k8.v(ii)   = k(idx+128)
                         arg8.v(ii) = k8.v(ii)*pi2.v(ii)
                         Ak(idx+128)= Phi0.v(ii)*(sin(arg8.v(ii))/arg8.v(ii))
                         k9.v(ii)   = k(idx+144)
                         arg9.v(ii) = k9.v(ii)*pi2.v(ii)
                         Ak(idx+144)= Phi0.v(ii)*(sin(arg9.v(ii))/arg9.v(ii))
                         k10.v(ii)  = k(idx+160)
                         arg10.v(ii)= k10.v(ii)*pi2.v(ii)
                         Ak(idx+160)= Phi0.v(ii)*(sin(arg10.v(ii))/arg10.v(ii))
                         k11.v(ii)  = k(idx+176)
                         arg11.v(ii)= k11.v(ii)*pi2.v(ii)
                         Ak(idx+176)= Phi0.v(ii)*(sin(arg11.v(ii))/arg11.v(ii))
                         k12.v(ii)  = k(idx+192)
                         arg12.v(ii)= k12.v(ii)*pi2.v(ii)
                         Ak(idx+192)= Phi0.v(ii)*(sin(arg12.v(ii))/arg12.v(ii))
                         k13.v(ii)  = k(idx+208)
                         arg13.v(ii)= k13.v(ii)*pi2.v(ii)
                         Ak(idx+208)= Phi0.v(ii)*(sin(arg13.v(ii))/arg13.v(ii))
                         k14.v(ii)  = k(idx+224)
                         arg14.v(ii)= k14.v(ii)*pi2.v(ii)
                         Ak(idx+224)= Phi0.v(ii)*(sin(arg14.v(ii))/arg14.v(ii))
                         k15.v(ii)  = k(idx+240)
                         arg15.v(ii)= k15.v(ii)*pi2.v(ii)
                         Ak(idx+240)= Phi0.v(ii)*(sin(arg15.v(ii))/arg15.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==16) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>16 .and. n<=256) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>256) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),256
                    call mm_prefetch(Phik(i*256),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+64)
                         Ak(idx+64)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+80)
                         Ak(idx+80)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+96)
                         Ak(idx+96)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+112)
                         Ak(idx+112) = twoT.v(ii)*phik7.v(ii)
                         phik8.v(ii) = Phik(idx+128)
                         Ak(idx+128) = twoT.v(ii)*phik8.v(ii)
                         phik9.v(ii) = Phik(idx+144)
                         Ak(idx+144) = twoT.v(ii)*phik9.v(ii)
                         phik10.v(ii)= Phik(idx+160)
                         Ak(idx+160) = twoT.v(ii)*phik10.v(ii)
                         phik11.v(ii)= Phik(idx+176)
                         Ak(idx+176) = twoT.v(ii)*phik11.v(ii)
                         phik12.v(ii)= Phik(idx+192)
                         Ak(idx+192) = twoT.v(ii)*phik12.v(ii)
                         phik13.v(ii)= Phik(idx+208)
                         Ak(idx+208) = twoT.v(ii)*phik13.v(ii) 
                         phik14.v(ii)= Phik(idx+224)
                         Ak(idx+224) = twoT.v(ii)*phik14.v(ii)
                         phik15.v(ii)= Phik(idx+240)
                         Ak(idx+240) = twoT.v(ii)*phik15.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_16x_zmm16r4


       subroutine rect_pulse_amp_unroll_8x_zmm16r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_zmm16r4
           use mod_fpcompare, only : zmm16r4_equalto_zmm16r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM16r4_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM16r4_t),                intent(in)  :: tin
           type(ZMM16r4_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_sp)
           type(ZMM16r4_t), parameter :: two = ZMM16r4_t(2.0_sp)
           type(ZMM16r4_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM16r4_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 64 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(ZMM16r4_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask16_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<16) return
           twoT = two.v/T.v
           m = zmm16r4_equalto_zmm16r4(tin,twoT)
           if(all(m)) then
              if(n==16) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>16 .and. n<=128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),128
                    call mm_prefetch(k(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+16)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+32)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+48)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+64)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+64) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+80)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+80) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+96)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+96) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+112)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+112)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==16) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>16 .and. n<=128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),128
                    call mm_prefetch(Phik(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+64)
                         Ak(idx+64)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+80)
                         Ak(idx+80)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+96)
                         Ak(idx+96)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+112)
                         Ak(idx+112) = twoT.v(ii)*phik7.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_8x_zmm16r4


       subroutine rect_pulse_amp_unroll_4x_zmm16r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_zmm16r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_zmm16r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_zmm16r4
           use mod_fpcompare, only : zmm16r4_equalto_zmm16r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM16r4_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM16r4_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM16r4_t),                intent(in)  :: tin
           type(ZMM16r4_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_sp)
           type(ZMM16r4_t), parameter :: two = ZMM16r4_t(2.0_sp)
           type(ZMM16r4_t) :: phik0,phik1,phik2,phik3
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3
           type(ZMM16r4_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3
           type(ZMM16r4_t) :: k0,k1,k2,k3
           !dir$ attributes align : 64 :: k0,k1,k2,k3
           type(ZMM16r4_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask16_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<16) return
           twoT = two.v/T.v
           m = zmm16r4_equalto_zmm16r4(tin,twoT)
           if(all(m)) then
              if(n==16) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>16 .and. n<=64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(15)),64
                    call mm_prefetch(k(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+16)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+32)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+48)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==16) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>16 .and. n<=64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),16
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(15)),64
                    call mm_prefetch(Phik(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,15
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik3.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=16,min=1,avg=8
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_4x_zmm16r4



       subroutine rect_pulse_amp_unroll_16x_zmm8r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_zmm8r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_zmm8r8
           use mod_fpcompare, only : zmm8r8_equalto_zmm8r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM8r8_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM8r8_t),                intent(in)  :: tin
           type(ZMM8r8_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_dp)
           type(ZMM8r8_t), parameter :: two = ZMM16r4_t(2.0_dp)
           type(ZMM8r8_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(ZMM8r8_t) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           !dir$ attributes align : 64 :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM8r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 64 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(ZMM8r8_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 64 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(ZMM8r8_t) :: k8,k9,k10,k11,k12,k13,k14,k15
           !dir$ attributes align : 64 :: k8,k9,k10,k11,k12,k13,k14,k15
           type(ZMM8r8_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = zmm8r8_equalto_zmm8r8(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),128
                    call mm_prefetch(k(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+32)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+40)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+40) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+48)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+56)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+56)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                         k8.v(ii)   = k(idx+64)
                         arg8.v(ii) = k8.v(ii)*pi2.v(ii)
                         Ak(idx+64)= Phi0.v(ii)*(sin(arg8.v(ii))/arg8.v(ii))
                         k9.v(ii)   = k(idx+72)
                         arg9.v(ii) = k9.v(ii)*pi2.v(ii)
                         Ak(idx+72)= Phi0.v(ii)*(sin(arg9.v(ii))/arg9.v(ii))
                         k10.v(ii)  = k(idx+80)
                         arg10.v(ii)= k10.v(ii)*pi2.v(ii)
                         Ak(idx+80)= Phi0.v(ii)*(sin(arg10.v(ii))/arg10.v(ii))
                         k11.v(ii)  = k(idx+88)
                         arg11.v(ii)= k11.v(ii)*pi2.v(ii)
                         Ak(idx+88)= Phi0.v(ii)*(sin(arg11.v(ii))/arg11.v(ii))
                         k12.v(ii)  = k(idx+96)
                         arg12.v(ii)= k12.v(ii)*pi2.v(ii)
                         Ak(idx+96)= Phi0.v(ii)*(sin(arg12.v(ii))/arg12.v(ii))
                         k13.v(ii)  = k(idx+104)
                         arg13.v(ii)= k13.v(ii)*pi2.v(ii)
                         Ak(idx+104)= Phi0.v(ii)*(sin(arg13.v(ii))/arg13.v(ii))
                         k14.v(ii)  = k(idx+112)
                         arg14.v(ii)= k14.v(ii)*pi2.v(ii)
                         Ak(idx+112)= Phi0.v(ii)*(sin(arg14.v(ii))/arg14.v(ii))
                         k15.v(ii)  = k(idx+120)
                         arg15.v(ii)= k15.v(ii)*pi2.v(ii)
                         Ak(idx+120)= Phi0.v(ii)*(sin(arg15.v(ii))/arg15.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),128
                    call mm_prefetch(Phik(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+40)
                         Ak(idx+40)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+56)
                         Ak(idx+56) = twoT.v(ii)*phik7.v(ii)
                         phik8.v(ii) = Phik(idx+64)
                         Ak(idx+64) = twoT.v(ii)*phik8.v(ii)
                         phik9.v(ii) = Phik(idx+72)
                         Ak(idx+72) = twoT.v(ii)*phik9.v(ii)
                         phik10.v(ii)= Phik(idx+80)
                         Ak(idx+80) = twoT.v(ii)*phik10.v(ii)
                         phik11.v(ii)= Phik(idx+88)
                         Ak(idx+88) = twoT.v(ii)*phik11.v(ii)
                         phik12.v(ii)= Phik(idx+96)
                         Ak(idx+96) = twoT.v(ii)*phik12.v(ii)
                         phik13.v(ii)= Phik(idx+104)
                         Ak(idx+104) = twoT.v(ii)*phik13.v(ii) 
                         phik14.v(ii)= Phik(idx+112)
                         Ak(idx+112) = twoT.v(ii)*phik14.v(ii)
                         phik15.v(ii)= Phik(idx+120)
                         Ak(idx+120) = twoT.v(ii)*phik15.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_16x_zmm8r8


       
       subroutine rect_pulse_amp_unroll_8x_zmm8r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_zmm8r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_zmm8r8
           use mod_fpcompare, only : zmm8r8_equalto_zmm8r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM8r8_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM8r8_t),                intent(in)  :: tin
           type(ZMM8r8_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_dp)
           type(ZMM8r8_t), parameter :: two = ZMM16r4_t(2.0_dp)
           type(ZMM8r8_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(ZMM8r8_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 64 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(ZMM8r8_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = zmm8r8_equalto_zmm8r8(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),64
                    call mm_prefetch(k(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+32)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+40)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+40) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+48)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+56)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+56)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),64
                    call mm_prefetch(Phik(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+40)
                         Ak(idx+40)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+56)
                         Ak(idx+56) = twoT.v(ii)*phik7.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_8x_zmm8r8



       subroutine rect_pulse_amp_unroll_4x_zmm8r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_zmm8r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_zmm8r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_zmm8r8
           use mod_fpcompare, only : zmm8r8_equalto_zmm8r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(ZMM8r8_t),                intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(ZMM8r8_t),                intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(ZMM8r8_t),                intent(in)  :: tin
           type(ZMM8r8_t), parameter :: pi2 = ZMM16r4_t(1.5707963267948966192313216916398_dp)
           type(ZMM8r8_t), parameter :: two = ZMM16r4_t(2.0_dp)
           type(ZMM8r8_t) :: phik0,phik1,phik2,phik3
           !dir$ attributes align : 64 :: phik0,phik1,phik2,phik3
           type(ZMM8r8_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 64 :: arg0,arg1,arg2,arg3
           type(ZMM8r8_t) :: k0,k1,k2,k3
           !dir$ attributes align : 64 :: k0,k1,k2,k3
           type(ZMM8r8_t) :: twoT,kpi2
           !dir$ attributes align : 64 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = zmm8r8_equalto_zmm8r8(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=32) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>32) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned k:64
                 do i=1,iand(n,not(7)),32
                    call mm_prefetch(k(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=32) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>32) then
                 !dir$ assume_aligned Ak:64           
                 !dir$ assume_aligned Phik:64
                 do i=1,iand(n,not(7)),32
                    call mm_prefetch(Phik(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_4x_zmm8r8



       
       subroutine rect_pulse_amp_unroll_16x_ymm8r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_ymm8r4
           use mod_fpcompare, only : ymm8r4_equalto_ymm8r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(YMM8r4_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM8r4_t),                 intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(YMM8r4_t),                 intent(in)  :: tin
           type(YMM8r4_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_sp)
           type(YMM8r4_t), parameter :: two = YMM8r4_t(2.0_sp)
           type(YMM8r4_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(YMM8r4_t) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           !dir$ attributes align : 32 :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM8r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 32 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM8r4_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 32 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(YMM8r4_t) :: k8,k9,k10,k11,k12,k13,k14,k15
           !dir$ attributes align : 32 :: k8,k9,k10,k11,k12,k13,k14,k15
           type(YMM8r4_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = ymm8r4_equalto_ymm8r4(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=128) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),128
                    call mm_prefetch(k(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+32)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+40)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+40) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+48)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+56)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+56)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                         k8.v(ii)   = k(idx+64)
                         arg8.v(ii) = k8.v(ii)*pi2.v(ii)
                         Ak(idx+64)= Phi0.v(ii)*(sin(arg8.v(ii))/arg8.v(ii))
                         k9.v(ii)   = k(idx+72)
                         arg9.v(ii) = k9.v(ii)*pi2.v(ii)
                         Ak(idx+72)= Phi0.v(ii)*(sin(arg9.v(ii))/arg9.v(ii))
                         k10.v(ii)  = k(idx+80)
                         arg10.v(ii)= k10.v(ii)*pi2.v(ii)
                         Ak(idx+80)= Phi0.v(ii)*(sin(arg10.v(ii))/arg10.v(ii))
                         k11.v(ii)  = k(idx+88)
                         arg11.v(ii)= k11.v(ii)*pi2.v(ii)
                         Ak(idx+88)= Phi0.v(ii)*(sin(arg11.v(ii))/arg11.v(ii))
                         k12.v(ii)  = k(idx+96)
                         arg12.v(ii)= k12.v(ii)*pi2.v(ii)
                         Ak(idx+96)= Phi0.v(ii)*(sin(arg12.v(ii))/arg12.v(ii))
                         k13.v(ii)  = k(idx+104)
                         arg13.v(ii)= k13.v(ii)*pi2.v(ii)
                         Ak(idx+104)= Phi0.v(ii)*(sin(arg13.v(ii))/arg13.v(ii))
                         k14.v(ii)  = k(idx+112)
                         arg14.v(ii)= k14.v(ii)*pi2.v(ii)
                         Ak(idx+112)= Phi0.v(ii)*(sin(arg14.v(ii))/arg14.v(ii))
                         k15.v(ii)  = k(idx+120)
                         arg15.v(ii)= k15.v(ii)*pi2.v(ii)
                         Ak(idx+120)= Phi0.v(ii)*(sin(arg15.v(ii))/arg15.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=128) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>128) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),128
                    call mm_prefetch(Phik(i*128),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+40)
                         Ak(idx+40)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+56)
                         Ak(idx+56) = twoT.v(ii)*phik7.v(ii)
                         phik8.v(ii) = Phik(idx+64)
                         Ak(idx+64) = twoT.v(ii)*phik8.v(ii)
                         phik9.v(ii) = Phik(idx+72)
                         Ak(idx+72) = twoT.v(ii)*phik9.v(ii)
                         phik10.v(ii)= Phik(idx+80)
                         Ak(idx+80) = twoT.v(ii)*phik10.v(ii)
                         phik11.v(ii)= Phik(idx+88)
                         Ak(idx+88) = twoT.v(ii)*phik11.v(ii)
                         phik12.v(ii)= Phik(idx+96)
                         Ak(idx+96) = twoT.v(ii)*phik12.v(ii)
                         phik13.v(ii)= Phik(idx+104)
                         Ak(idx+104) = twoT.v(ii)*phik13.v(ii) 
                         phik14.v(ii)= Phik(idx+112)
                         Ak(idx+112) = twoT.v(ii)*phik14.v(ii)
                         phik15.v(ii)= Phik(idx+120)
                         Ak(idx+120) = twoT.v(ii)*phik15.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_16x_ymm8r4



       subroutine rect_pulse_amp_unroll_8x_ymm8r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_ymm8r4
           use mod_fpcompare, only : ymm8r4_equalto_ymm8r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(YMM8r4_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM8r4_t),                 intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(YMM8r4_t),                 intent(in)  :: tin
           type(YMM8r4_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_sp)
           type(YMM8r4_t), parameter :: two = YMM8r4_t(2.0_sp)
           type(YMM8r4_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM8r4_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 32 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(YMM8r4_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = ymm8r4_equalto_ymm8r4(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),64
                    call mm_prefetch(k(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+32)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+32) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+40)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+40) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+48)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+48) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+56)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+56)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),64
                    call mm_prefetch(Phik(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+32)
                         Ak(idx+32)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+40)
                         Ak(idx+40)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+48)
                         Ak(idx+48)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+56)
                         Ak(idx+56) = twoT.v(ii)*phik7.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_8x_ymm8r4



       subroutine rect_pulse_amp_unroll_4x_ymm8r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_ymm8r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_ymm8r4
           use mod_fpcompare, only : ymm8r4_equalto_ymm8r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           type(YMM8r4_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM8r4_t),                 intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           type(YMM8r4_t),                 intent(in)  :: tin
           type(YMM8r4_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_sp)
           type(YMM8r4_t), parameter :: two = YMM8r4_t(2.0_sp)
           type(YMM8r4_t) :: phik0,phik1,phik2,phik3
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3
           type(YMM8r4_t) :: k0,k1,k2,k3
           !dir$ attributes align : 32 :: k0,k1,k2,k3
           type(YMM8r4_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask8_t) :: m
           real(kind=sp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<8) return
           twoT = two.v/T.v
           m = ymm8r4_equalto_ymm8r4(tin,twoT)
           if(all(m)) then
              if(n==8) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>8 .and. n<=32) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>32) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(7)),32
                    call mm_prefetch(k(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+8)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+16)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+24)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==8) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>8 .and. n<=32) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),8
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do ii=0,7
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>32) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(7)),32
                    call mm_prefetch(Phik(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,7
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik3.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=8,min=1,avg=4
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_4x_ymm8r4


       
       subroutine rect_pulse_amp_unroll_16x_ymm4r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_ymm4r8
           use mod_fpcompare, only : ymm4r8_equalto_ymm4r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(YMM4r8_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM4r8_t),                 intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           type(YMM4r8_t),                 intent(in)  :: tin
           type(YMM4r8_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_dp)
           type(YMM4r8_t), parameter :: two = YMM8r4_t(2.0_dp)
           type(YMM4r8_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(YMM4r8_t) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           !dir$ attributes align : 32 :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM4r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 32 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM4r8_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 32 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(YMM4r8_t) :: k8,k9,k10,k11,k12,k13,k14,k15
           !dir$ attributes align : 32 :: k8,k9,k10,k11,k12,k13,k14,k15
           type(YMM4r8_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask4_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<4) return
           twoT = two.v/T.v
           m = ymm4r8_equalto_ymm4r8(tin,twoT)
           if(all(m)) then
              if(n==4) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>4 .and. n<=64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),64
                    call mm_prefetch(k(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+4)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+4) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+8)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+12)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+12) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+16)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+20)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+20) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+24)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+28)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+28)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                         k8.v(ii)   = k(idx+32)
                         arg8.v(ii) = k8.v(ii)*pi2.v(ii)
                         Ak(idx+32)= Phi0.v(ii)*(sin(arg8.v(ii))/arg8.v(ii))
                         k9.v(ii)   = k(idx+36)
                         arg9.v(ii) = k9.v(ii)*pi2.v(ii)
                         Ak(idx+36)= Phi0.v(ii)*(sin(arg9.v(ii))/arg9.v(ii))
                         k10.v(ii)  = k(idx+40)
                         arg10.v(ii)= k10.v(ii)*pi2.v(ii)
                         Ak(idx+40)= Phi0.v(ii)*(sin(arg10.v(ii))/arg10.v(ii))
                         k11.v(ii)  = k(idx+44)
                         arg11.v(ii)= k11.v(ii)*pi2.v(ii)
                         Ak(idx+44)= Phi0.v(ii)*(sin(arg11.v(ii))/arg11.v(ii))
                         k12.v(ii)  = k(idx+48)
                         arg12.v(ii)= k12.v(ii)*pi2.v(ii)
                         Ak(idx+48)= Phi0.v(ii)*(sin(arg12.v(ii))/arg12.v(ii))
                         k13.v(ii)  = k(idx+52)
                         arg13.v(ii)= k13.v(ii)*pi2.v(ii)
                         Ak(idx+52)= Phi0.v(ii)*(sin(arg13.v(ii))/arg13.v(ii))
                         k14.v(ii)  = k(idx+56)
                         arg14.v(ii)= k14.v(ii)*pi2.v(ii)
                         Ak(idx+56)= Phi0.v(ii)*(sin(arg14.v(ii))/arg14.v(ii))
                         k15.v(ii)  = k(idx+60)
                         arg15.v(ii)= k15.v(ii)*pi2.v(ii)
                         Ak(idx+60)= Phi0.v(ii)*(sin(arg15.v(ii))/arg15.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==4) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>4 .and. n<=64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),64
                    call mm_prefetch(Phik(i*64),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+4)
                         Ak(idx+4)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+12)
                         Ak(idx+12)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+20)
                         Ak(idx+20)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+28)
                         Ak(idx+28) = twoT.v(ii)*phik7.v(ii)
                         phik8.v(ii) = Phik(idx+32)
                         Ak(idx+32) = twoT.v(ii)*phik8.v(ii)
                         phik9.v(ii) = Phik(idx+36)
                         Ak(idx+36) = twoT.v(ii)*phik9.v(ii)
                         phik10.v(ii)= Phik(idx+40)
                         Ak(idx+40) = twoT.v(ii)*phik10.v(ii)
                         phik11.v(ii)= Phik(idx+44)
                         Ak(idx+44) = twoT.v(ii)*phik11.v(ii)
                         phik12.v(ii)= Phik(idx+48)
                         Ak(idx+48) = twoT.v(ii)*phik12.v(ii)
                         phik13.v(ii)= Phik(idx+52)
                         Ak(idx+52) = twoT.v(ii)*phik13.v(ii) 
                         phik14.v(ii)= Phik(idx+56)
                         Ak(idx+56) = twoT.v(ii)*phik14.v(ii)
                         phik15.v(ii)= Phik(idx+60)
                         Ak(idx+60) = twoT.v(ii)*phik15.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_16x_ymm4r8


   
      subroutine rect_pulse_amp_unroll_8x_ymm4r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_ymm4r8
           use mod_fpcompare, only : ymm4r8_equalto_ymm4r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(YMM4r8_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM4r8_t),                 intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           type(YMM4r8_t),                 intent(in)  :: tin
           type(YMM4r8_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_dp)
           type(YMM4r8_t), parameter :: two = YMM8r4_t(2.0_dp)
           type(YMM4r8_t) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM4r8_t) :: k0,k1,k2,k3,k4,k5,k6,k7
           !dir$ attributes align : 32 :: k0,k1,k2,k3,k4,k5,k6,k7
           type(YMM4r8_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask4_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<4) return
           twoT = two.v/T.v
           m = ymm4r8_equalto_ymm4r8(tin,twoT)
           if(all(m)) then
              if(n==4) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>4 .and. n<=32) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>32) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),32
                    call mm_prefetch(k(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+4)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+4) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+8)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+12)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+12) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                         k4.v(ii)   = k(idx+16)
                         arg4.v(ii) = k4.v(ii)*pi2.v(ii)
                         Ak(idx+16) = Phi0.v(ii)*(sin(arg4.v(ii))/arg4.v(ii))
                         k5.v(ii)   = k(idx+20)
                         arg5.v(ii) = k5.v(ii)*pi2.v(ii)
                         Ak(idx+20) = Phi0.v(ii)*(sin(arg5.v(ii))/arg5.v(ii))
                         k6.v(ii)   = k(idx+24)
                         arg6.v(ii) = k6.v(ii)*pi2.v(ii)
                         Ak(idx+24) = Phi0.v(ii)*(sin(arg6.v(ii))/arg6.v(ii))
                         k7.v(ii)   = k(idx+28)
                         arg7.v(ii) = k7.v(ii)*pi2.v(ii)
                         Ak(idx+28)= Phi0.v(ii)*(sin(arg7.v(ii))/arg7.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==4) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>4 .and. n<=32) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>64) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),32
                    call mm_prefetch(Phik(i*32),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+4)
                         Ak(idx+4)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+12)
                         Ak(idx+12)  = twoT.v(ii)*phik3.v(ii)
                         phik4.v(ii) = Phik(idx+16)
                         Ak(idx+16)  = twoT.v(ii)*phik4.v(ii)
                         phik5.v(ii) = Phik(idx+20)
                         Ak(idx+20)  = twoT.v(ii)*phik5.v(ii)
                         phik6.v(ii) = Phik(idx+24)
                         Ak(idx+24)  = twoT.v(ii)*phik6.v(ii)
                         phik7.v(ii) = Phik(idx+28)
                         Ak(idx+28) = twoT.v(ii)*phik7.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_8x_ymm4r8



       subroutine rect_pulse_amp_unroll_4x_ymm4r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_ymm4r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_ymm4r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_ymm4r8
           use mod_fpcompare, only : ymm4r8_equalto_ymm4r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           type(YMM4r8_t),                 intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           type(YMM4r8_t),                 intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           type(YMM4r8_t),                 intent(in)  :: tin
           type(YMM4r8_t), parameter :: pi2 = YMM8r4_t(1.5707963267948966192313216916398_dp)
           type(YMM4r8_t), parameter :: two = YMM8r4_t(2.0_dp)
           type(YMM4r8_t) :: phik0,phik1,phik2,phik3
           !dir$ attributes align : 32 :: phik0,phik1,phik2,phik3
           type(YMM4r8_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3
           type(YMM4r8_t) :: k0,k1,k2,k3
           !dir$ attributes align : 32 :: k0,k1,k2,k3
           type(YMM4r8_t) :: twoT,kpi2
           !dir$ attributes align : 32 :: twoT,kpi2
           type(Mask4_t) :: m
           real(kind=dp)  :: k,arg,phikx
           integer(kind=i4) :: i,ii,j,idx
           if(n<4) return
           twoT = two.v/T.v
           m = ymm4r8_equalto_ymm4r8(tin,twoT)
           if(all(m)) then
              if(n==4) then
                 do i=1,n
                    k     = k(i)
                    arg   = k0*pi2.v(0)
                    Ak(i) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
              else if(n>4 .and. n<=16) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx        = ii+i
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
             else if(n>16) then
                 !dir$ assume_aligned Ak:32          
                 !dir$ assume_aligned k:32
                 do i=1,iand(n,not(3)),16
                    call mm_prefetch(k(i*16),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         k0.v(ii)   = k(idx)
                         arg0.v(ii) = k0.v(ii)*pi2.v(ii)
                         Ak(idx)    = Phi0.v(ii)*(sin(arg0.v(ii))/arg0.v(ii))
                         k1.v(ii)   = k(idx+4)
                         arg1.v(ii) = k1.v(ii)*pi2.v(ii)
                         Ak(idx+4) = Phi0.v(ii)*(sin(arg1.v(ii))/arg1.v(ii))
                         k2.v(ii)   = k(idx+8)
                         arg2.v(ii) = k2.v(ii)*pi2.v(ii)
                         Ak(idx+8) = Phi0.v(ii)*(sin(arg2.v(ii))/arg2.v(ii))
                         k3.v(ii)   = k(idx+12)
                         arg3.v(ii) = k3.v(ii)*pi2.v(ii)
                         Ak(idx+12) = Phi0.v(ii)*(sin(arg3.v(ii))/arg3.v(ii))
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                    k     = k(j)
                    arg   = k0*pi2.v(0)
                    Ak(j) = Phi0.v(0)*(sin(arg0)/arg0)
                 end do
                 return
            else
              if(n==4) then
                 do i=1,n
                    phikx = Phik(i)
                    Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              else if(n>4 .and. n<=16) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),4
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx         = ii+i
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                      end do
                  end do
                  ! Remainder loop
                  !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
             else if(n>16) then
                 !dir$ assume_aligned Ak:32           
                 !dir$ assume_aligned Phik:32
                 do i=1,iand(n,not(3)),16
                    call mm_prefetch(Phik(i*16),FOR_K_PREFETCH_T1)
                      !dir$ vector aligned
                      !dir$ ivdep
                      !dir$ vector vectorlength(8)
                      !dir$ vector always
                      do ii=0,3
                         idx = i+ii
                         phik0.v(ii) = Phik(idx)
                         Ak(idx)     = twoT.v(ii)*phik0.v(ii)
                         phik1.v(ii) = Phik(idx+4)
                         Ak(idx+4)  = twoT.v(ii)*phik1.v(ii)
                         phik2.v(ii) = Phik(idx+8)
                         Ak(idx+8)  = twoT.v(ii)*phik2.v(ii)
                         phik3.v(ii) = Phik(idx+12)
                         Ak(idx+12)  = twoT.v(ii)*phik3.v(ii)
                      end do
                  end do
                   ! Remainder loop
                   !dir$ loop_count max=4,min=1,avg=2
                  do j=i,n
                     phikx = Phik(i)
                     Ak(i) = twoT.v(0)*phikx
                 end do
                 return
              end if
           end if
       end subroutine rect_pulse_amp_unroll_4x_ymm4r8







       




       





       



       





       


 


       
  

      



 




       



       





       
 
   



       







       



       






       


























end module eos_sensor_simd
