

#include "GMS_config.fpp"

!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module spec_funcs_zmm8r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         spec_funcs_zmm8r8
 !          
 !          Purpose:
 !                       Various vectorized special functions.
 !                        
 !          History:
 !                        Date: 08-28-2023
 !                        Time: 16:09 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Vectorized by Bernard Gingold (based on different authors work)
 !                      The details stated by the specific function description.
 !                 
 !          References:
 !         
 !                      Provided at the specific function description
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,dp
    use mod_vectypes, only : ZMM8r8_t,Mask8_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_FULLVER =   &
            1000*SPEC_FUNCS_ZMM8R8_MAJOR+100*SPEC_FUNCS_ZMM8R8_MINOR+10*SPEC_FUNCS_ZMM8R8_MICRO
     ! Module creation date
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_CREATE_DATE = "28-08-2022 06:11 +00200 (MON 28 AUG 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_SYNOPSIS    = "Vectorized various special functions" 
     
     
     !! calcei_zm8r8 constants arrays (saved).
     !dir$ attributes align : 64 :: calcei_a
      type(ZMM8r8_t), dimension(0:6), save :: calcei_a = [ZMM8r8_t(1.1669552669734461083368e+2_dp),   &
	                                                  ZMM8r8_t(2.1500672908092918123209e+3_dp),   &
                                                          ZMM8r8_t(1.5924175980637303639884e+4_dp),   &
                                                          ZMM8r8_t(8.9904972007457256553251e+4_dp),   &
                                                          ZMM8r8_t(1.5026059476436982420737e+5_dp),   &
                                                          ZMM8r8_t(-1.4815102102575750838086e+5_dp),  &
                                                          ZMM8r8_t(5.0196785185439843791020e+0_dp)]
      type(ZMM8r8_t), dimension(0:6), save :: calcei_b = [ZMM8r8_t(4.0205465640027706061433e+1_dp),   & 
                                                          ZMM8r8_t(7.5043163907103936624165e+2_dp),   &
                                                          ZMM8r8_t(8.1258035174768735759855e+3_dp),   & 
                                                          ZMM8r8_t(5.2440529172056355429883e+4_dp),   & 
                                                          ZMM8r8_t(1.8434070063353677359298e+5_dp),   & 
                                                          ZMM8r8_t(2.5666493484897117319268e+5_dp)]
      type(ZMM8r8_t), dimension(0:8), save :: calcei_c = [ZMM8r8_t(3.828573121022477169108e-1_dp),    & 
                                                          ZMM8r8_t(1.107326627786831743809e+1_dp),    &
                                                          ZMM8r8_t(7.246689782858597021199e+1_dp),    & 
                                                          ZMM8r8_t(1.700632978311516129328e+2_dp),    & 
                                                          ZMM8r8_t(1.698106763764238382705e+2_dp),    &
                                                          ZMM8r8_t(7.633628843705946890896e+1_dp),    & 
                                                          ZMM8r8_t(1.487967702840464066613e+1_dp),    & 
                                                          ZMM8r8_t(9.999989642347613068437e-1_dp),    & 
                                                          ZMM8r8_t(1.737331760720576030932e-8_dp)]
      type(ZMM8r8_t), dimension(0:8), save :: calcei_d = [ZMM8r8_t(8.258160008564488034698e-2_dp),    & 
	                                                  ZMM8r8_t(4.344836335509282083360e+0_dp),    & 
                                                          ZMM8r8_t(4.662179610356861756812e+1_dp),    & 
                                                          ZMM8r8_t(1.775728186717289799677e+2_dp),    & 
                                                          ZMM8r8_t(2.953136335677908517423e+2_dp),    & 
                                                          ZMM8r8_t(2.342573504717625153053e+2_dp),    & 
                                                          ZMM8r8_t(9.021658450529372642314e+1_dp),    & 
                                                          ZMM8r8_t(1.587964570758947927903e+1_dp),    & 
                                                          ZMM8r8_t(1.000000000000000000000e+0_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_e = [ZMM8r8_t(1.3276881505637444622987e+2_dp),   &
                                                          ZMM8r8_t(3.5846198743996904308695e+4_dp),   &
                                                          ZMM8r8_t(1.7283375773777593926828e+5_dp),   &
                                                          ZMM8r8_t(2.6181454937205639647381e+5_dp),   &
                                                          ZMM8r8_t(1.7503273087497081314708e+5_dp),   & 
                                                          ZMM8r8_t(5.9346841538837119172356e+4_dp),   &
                                                          ZMM8r8_t(1.0816852399095915622498e+4_dp),   &
                                                          ZMM8r8_t(1.0611777263550331766871e+03_dp),  &
                                                          ZMM8r8_t(5.2199632588522572481039e+1_dp),   &
                                                          ZMM8r8_t(9.9999999999999999087819e-1_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_f = [ZMM8r8_t(3.9147856245556345627078e+4_dp),   &
                                                          ZMM8r8_t(2.5989762083608489777411e+5_dp),   &
                                                          ZMM8r8_t(5.5903756210022864003380e+5_dp),   &
                                                          ZMM8r8_t(5.4616842050691155735758e+5_dp),   &
                                                          ZMM8r8_t(2.7858134710520842139357e+5_dp),   &
                                                          ZMM8r8_t(7.9231787945279043698718e+4_dp),   &
                                                          ZMM8r8_t(1.2842808586627297365998e+4_dp),   &
                                                          ZMM8r8_t(1.1635769915320848035459e+3_dp),   &
                                                          ZMM8r8_t(5.4199632588522559414924e+1_dp),   &
                                                          ZMM8r8_t(1.0000000000000000000000e+0_dp)]
      type(ZMM8r8_t), dimension(0:3), save :: calcei_plg=[ZMM8r8_t(-2.4562334077563243311e+01_dp),    &
                                                          ZMM8r8_t(2.3642701335621505212e+02_dp),     &
                                                          ZMM8r8_t(-5.4989956895857911039e+02_dp),    &
                                                          ZMM8r8_t(3.5687548468071500413e+02_dp)]
      type(ZMM8r8_t), dimension(0:3), save :: calcei_qlg=[ZMM8r8_t(-3.5553900764052419184e+01_dp),    &
                                                          ZMM8r8_t(1.9400230218539473193e+02_dp),     &
                                                          ZMM8r8_t(-3.3442903192607538956e+02_dp),    &
                                                          ZMM8r8_t(1.7843774234035750207e+02_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_p  =[ZMM8r8_t(-1.2963702602474830028590e+01_dp), &
                                                          ZMM8r8_t(-1.2831220659262000678155e+03_dp), &
                                                          ZMM8r8_t(-1.4287072500197005777376e+04_dp), &
                                                          ZMM8r8_t(-1.4299841572091610380064e+06_dp), &
                                                          ZMM8r8_t(-3.1398660864247265862050e+05_dp), &
                                                          ZMM8r8_t(-3.5377809694431133484800e+08_dp), &
                                                          ZMM8r8_t(3.1984354235237738511048e+08_dp),  &
                                                          ZMM8r8_t(-2.5301823984599019348858e+10_dp), &
                                                          ZMM8r8_t(1.2177698136199594677580e+10_dp),  &
                                                          ZMM8r8_t(-2.0829040666802497120940e+11_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_q  =[ZMM8r8_t(7.6886718750000000000000e+01_dp),  &
                                                          ZMM8r8_t(-5.5648470543369082846819e+03_dp), &
                                                          ZMM8r8_t(1.9418469440759880361415e+05_dp),  &
                                                          ZMM8r8_t(-4.2648434812177161405483e+06_dp), &
                                                          ZMM8r8_t(6.4698830956576428587653e+07_dp),  &
                                                          ZMM8r8_t(-7.0108568774215954065376e+08_dp), &
                                                          ZMM8r8_t(5.4229617984472955011862e+09_dp),  &
                                                          ZMM8r8_t(-2.8986272696554495342658e+10_dp), &
                                                          ZMM8r8_t(9.8900934262481749439886e+10_dp),  &
                                                          ZMM8r8_t(-8.9673749185755048616855e+10_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_r  =[ZMM8r8_t(-2.645677793077147237806e+00_dp),  &
                                                          ZMM8r8_t(-2.378372882815725244124e+00_dp),  &
                                                          ZMM8r8_t(-2.421106956980653511550e+01_dp),  & 
                                                          ZMM8r8_t(1.052976392459015155422e+01_dp),   &
                                                          ZMM8r8_t(1.945603779539281810439e+01_dp),   &
                                                          ZMM8r8_t(-3.015761863840593359165e+01_dp),  &
                                                          ZMM8r8_t(1.120011024227297451523e+01_dp),   &
                                                          ZMM8r8_t(-3.988850730390541057912e+00_dp),  &
                                                          ZMM8r8_t(9.565134591978630774217e+00_dp),   & 
                                                          ZMM8r8_t(9.981193787537396413219e-1_dp)]
      type(ZMM8r8_t), dimension(0:8), save :: calcei_s  =[ZMM8r8_t(1.598517957704779356479e-4_dp),    &
                                                          ZMM8r8_t(4.644185932583286942650e+00_dp),   &
                                                          ZMM8r8_t(3.697412299772985940785e+02_dp),   &
                                                          ZMM8r8_t(-8.791401054875438925029e+00_dp),  &
                                                          ZMM8r8_t(7.608194509086645763123e+02_dp),   &
                                                          ZMM8r8_t(2.852397548119248700147e+01_dp),   &
                                                          ZMM8r8_t(4.731097187816050252967e+02_dp),   &
                                                          ZMM8r8_t(-2.369210235636181001661e+02_dp),  &
                                                          ZMM8r8_t(1.249884822712447891440e+00_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_p1 =[ZMM8r8_t(-1.647721172463463140042e+00_dp),  &
                                                          ZMM8r8_t(-1.860092121726437582253e+01_dp),  &
                                                          ZMM8r8_t(-1.000641913989284829961e+01_dp),  &
                                                          ZMM8r8_t(-2.105740799548040450394e+01_dp),  &
                                                          ZMM8r8_t(-9.13483569999874255243e-1_dp),    &
                                                          ZMM8r8_t(-3.323612579343962284333e+01_dp),  &
                                                          ZMM8r8_t(2.495487730402059440626e+01_dp),   &
                                                          ZMM8r8_t(2.652575818452799819855e+01_dp),   &
                                                          ZMM8r8_t(-1.845086232391278674524e+00_dp),  &
                                                          ZMM8r8_t(9.999933106160568739091e-1_dp)]
      type(ZMM8r8_t), dimension(0:8), save :: calcei_q1 =[ZMM8r8_t(9.792403599217290296840e+01_dp),   &
                                                          ZMM8r8_t(6.403800405352415551324e+01_dp),   &
                                                          ZMM8r8_t(5.994932325667407355255e+01_dp),   &
                                                          ZMM8r8_t(2.538819315630708031713e+02_dp),   &
                                                          ZMM8r8_t(4.429413178337928401161e+01_dp),   &
                                                          ZMM8r8_t(1.192832423968601006985e+03_dp),   &
                                                          ZMM8r8_t(1.991004470817742470726e+02_dp),   &
                                                          ZMM8r8_t(-1.093556195391091143924e+01_dp),  &
                                                          ZMM8r8_t(1.001533852045342697818e+00_dp)]
      type(ZMM8r8_t), dimension(0:9), save :: calcei_p2 =[ZMM8r8_t(1.75338801265465972390e+02_dp),    &
                                                          ZMM8r8_t(-2.23127670777632409550e+02_dp),   &
                                                          ZMM8r8_t(-1.81949664929868906455e+01_dp),   &
                                                          ZMM8r8_t(-2.79798528624305389340e+01_dp),   &
                                                          ZMM8r8_t(-7.63147701620253630855e+00_dp),   &
                                                          ZMM8r8_t(-1.52856623636929636839e+01_dp),   &
                                                          ZMM8r8_t(-7.06810977895029358836e+00_dp),   &
                                                          ZMM8r8_t(-5.00006640413131002475e+00_dp),   &
                                                          ZMM8r8_t(-3.00000000320981265753e+00_dp),   &
                                                          ZMM8r8_t(1.00000000000000485503e+00_dp)]
      type(ZMM8r8_t), dimension(0:8), save :: calcei_q2 =[ZMM8r8_t(3.97845977167414720840e+04_dp),    &
                                                          ZMM8r8_t(3.97277109100414518365e+00_dp),    &
                                                          ZMM8r8_t(1.37790390235747998793e+02_dp),    &
                                                          ZMM8r8_t(1.17179220502086455287e+02_dp),    &
                                                          ZMM8r8_t(7.04831847180424675988e+01_dp),    &
                                                          ZMM8r8_t(-1.20187763547154743238e+01_dp),   &
                                                          ZMM8r8_t(-7.99243595776339741065e+00_dp),   &
                                                          ZMM8r8_t(-2.99999894040324959612e+00_dp),   &
                                                          ZMM8r8_t(1.99999999999048104167e+00_dp)]
       
     contains
     
!! =============================================================================================================== //
!!                                  'Saved' arrays preload routines.
!!================================================================================================================ //  


      pure function preload_calcei_a() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_a
              !dir$ attributes forceinline :: preload_calcei_a
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_a
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2
              type(ZMM8r8_t), automatic :: t0,t1,t2
              t0.v    = calcei_a(0).v+calcei_a(1).v
              t1.v    = calcei_a(2).v+calcei_a(3).v
              t2.v    = calcei_a(4).v+calcei_a(5).v
              summa.v = t0.v+t1.v+t2.v
       end function preload_calcei_a 
       
       
       pure function preload_calcei_b() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_b
              !dir$ attributes forceinline :: preload_calcei_b
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_b
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2
              type(ZMM8r8_t), automatic :: t0,t1,t2
              t0.v    = calcei_b(0).v+calcei_b(1).v
              t1.v    = calcei_b(2).v+calcei_b(3).v
              t2.v    = calcei_b(4).v+calcei_b(5).v
              summa.v = t0.v+t1.v+t2.v
       end function preload_calcei_b 
       
       
      pure function preload_calcei_c() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_c
              !dir$ attributes forceinline :: preload_calcei_c
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_c
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v    = calcei_c(0).v+calcei_c(1).v
              t1.v    = calcei_c(2).v+calcei_c(3).v
              t2.v    = calcei_c(4).v+calcei_c(5).v
              t3.v    = calcei_c(6).v+calcei_c(7).v+ &
                        calcei_c(8).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calcei_c 
       
       
       pure function preload_calcei_d() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_d
              !dir$ attributes forceinline :: preload_calcei_d
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_d
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v    = calcei_d(0).v+calcei_d(1).v
              t1.v    = calcei_d(2).v+calcei_d(3).v
              t2.v    = calcei_d(4).v+calcei_d(5).v
              t3.v    = calcei_d(6).v+calcei_d(7).v+ &
                        calcei_d(8).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calcei_d 
       
       
       pure function preload_calcei_e() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_e
              !dir$ attributes forceinline :: preload_calcei_e
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_e
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_e(0).v+calcei_e(1).v
              t1.v    = calcei_e(2).v+calcei_e(3).v
              t2.v    = calcei_e(4).v+calcei_e(5).v
              t3.v    = calcei_e(6).v+calcei_e(7).v
              t4.v    = calcei_e(8).v+calcei_e(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_e 
       
       
       pure function preload_calcei_f() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_f
              !dir$ attributes forceinline :: preload_calcei_f
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_f
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_f(0).v+calcei_f(1).v
              t1.v    = calcei_f(2).v+calcei_f(3).v
              t2.v    = calcei_f(4).v+calcei_f(5).v
              t3.v    = calcei_f(6).v+calcei_f(7).v
              t4.v    = calcei_f(8).v+calcei_f(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_f 
       
       
       pure function preload_calcei_plg() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_plg
              !dir$ attributes forceinline :: preload_calcei_plg
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_plg
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1
              type(ZMM8r8_t), automatic :: t0,t1
              t0.v    = calcei_plg(0).v+calcei_plg(1).v
              t1.v    = calcei_plg(2).v+calcei_plg(3).v
              summa.v = t0.v+t1.v
       end function preload_calcei_plg 
       
       
       pure function preload_calcei_qlg() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_qlg
              !dir$ attributes forceinline :: preload_calcei_qlg
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_qlg
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1
              type(ZMM8r8_t), automatic :: t0,t1
              t0.v    = calcei_qlg(0).v+calcei_qlg(1).v
              t1.v    = calcei_qlg(2).v+calcei_qlg(3).v
              summa.v = t0.v+t1.v
       end function preload_calcei_qlg 
       
       
       pure function preload_calcei_p() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_p
              !dir$ attributes forceinline :: preload_calcei_p
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_p
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_p(0).v+calcei_p(1).v
              t1.v    = calcei_p(2).v+calcei_p(3).v
              t2.v    = calcei_p(4).v+calcei_p(5).v
              t3.v    = calcei_p(6).v+calcei_p(7).v
              t4.v    = calcei_p(8).v+calcei_p(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_p 
       
       
       pure function preload_calcei_q() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_q
              !dir$ attributes forceinline :: preload_calcei_q
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_q
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_q(0).v+calcei_q(1).v
              t1.v    = calcei_q(2).v+calcei_q(3).v
              t2.v    = calcei_q(4).v+calcei_q(5).v
              t3.v    = calcei_q(6).v+calcei_q(7).v
              t4.v    = calcei_q(8).v+calcei_q(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_q 
       
       
       pure function preload_calcei_r() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_r
              !dir$ attributes forceinline :: preload_calcei_r
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_r
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_r(0).v+calcei_r(1).v
              t1.v    = calcei_r(2).v+calcei_r(3).v
              t2.v    = calcei_r(4).v+calcei_r(5).v
              t3.v    = calcei_r(6).v+calcei_r(7).v
              t4.v    = calcei_r(8).v+calcei_r(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_r 
       
       
       pure function preload_calcei_s() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_s
              !dir$ attributes forceinline :: preload_calcei_s
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_s
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v    = calcei_r(0).v+calcei_r(1).v
              t1.v    = calcei_r(2).v+calcei_r(3).v
              t2.v    = calcei_r(4).v+calcei_r(5).v
              t3.v    = calcei_r(6).v+calcei_r(7).v+ &
                        calcei_r(8).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calcei_s 
       
       
       pure function preload_calcei_p1() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_p1
              !dir$ attributes forceinline :: preload_calcei_p1
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_p1
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_p1(0).v+calcei_p1(1).v
              t1.v    = calcei_p1(2).v+calcei_p1(3).v
              t2.v    = calcei_p1(4).v+calcei_p1(5).v
              t3.v    = calcei_p1(6).v+calcei_p1(7).v
              t4.v    = calcei_p1(8).v+calcei_p1(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_p1 
       
       
       
       
       
       
       
       
       
       
     
#if 0
/*
               !*****************************************************************************80
!
!! BESEI0 evaluates the exponentially scaled Bessel I0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the modified Bessel
!    function of the first kind of order zero multiplied by EXP(-ABS(X)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESEI0, the value of the function.
!
               
*/

#endif


          pure function besei0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besei0_zmm8r8
              !dir$ attributes forceinline :: besei0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besei0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calci0_zmm8r8(x,jint)
          end function besei0_zmm8r8
          

#if 0
 /*
!*****************************************************************************80
!
!! BESEI1 evaluates the exponentially scaled Bessel I1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the first kind of order one
!    multiplied by EXP(-ABS(X)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESEI1, the value of the function.
*/	         

#endif      


          pure function besei1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besei1_zmm8r8
              !dir$ attributes forceinline :: besei1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besei1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calci1_zmm8r8(x,jint) 
          end function besei1_zmm8r8
          

#if 0         
/*
   !*****************************************************************************80
!
!! BESEK0 evaluates the exponentially scaled Bessel K0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order zero
!    multiplied by the exponential function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!    0 < X.
!
!    Output, real ( kind = 8 ) BESK0, the value of the function.
*/   
#endif


           pure function besek0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besek0_zmm8r8
              !dir$ attributes forceinline :: besek0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besek0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calck0_zmm8r8(x,jint) 
          end function besek0_zmm8r8    
          

#if 0          
/*
!*****************************************************************************80
!
!! BESEK1 evaluates the exponentially scaled Bessel K1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order one
!    multiplied by the exponential function, for arguments
!    XLEAST <= ARG <= XMAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESEK1, the value of the function.
*/	             
#endif


          pure function besek1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besek1_zmm8r8
              !dir$ attributes forceinline :: besek1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besek1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calck1_zmm8r8(x,jint) 
          end function besek1_zmm8r8   
          
#if 0          
/*
     !*****************************************************************************80
!
!! BESI0 evaluates the Bessel I0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for
!    modified Bessel functions of the first kind of order zero for
!    arguments ABS(ARG) <= XMAX.
!
!    See comments heading CALCI0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESI0, the value of the function.
*/ 
#endif


          pure function besi0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besi0_zmm8r8
              !dir$ attributes forceinline :: besi0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besi0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = calci0_zmm8r8(x,jint) 
          end function besi0_zmm8r8  
          
          
#if 0
/*
    !*****************************************************************************80
!
!! BESI1 evaluates the Bessel I1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for
!    modified Bessel functions of the first kind of order one for
!    arguments ABS(ARG) <= XMAX.
!
!    See comments heading CALCI1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESI1, the value of the function.        
*/
#endif  


           pure function besi1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besi1_zmm8r8
              !dir$ attributes forceinline :: besi1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besi1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = calci1_zmm8r8(x,jint) 
          end function besi1_zmm8r8   
          
          
#if 0
/*
    *****************************************************************************80
!
!! BESJ0 evaluates the Bessel J0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the first kind of order zero for arguments  |X| <= XMAX
!
!    See comments heading CALJY0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESJ0, the value of the function.       
*/
#endif    


         pure function besj0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besj0_zmm8r8
              !dir$ attributes forceinline :: besj0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besj0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 0
              val  = calcjy0_zmm8r8(x,jint) 
          end function besj0_zmm8r8   
          
          
#if 0
   /*
*****************************************************************************80
!
!! BESJ1 evaluates the Bessel J1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the first kind of order zero for arguments  |X| <= XMAX
!
!    See comments heading CALJY1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESJ1, the value of the function.
*/	 

#endif

   
          pure function besj1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besj1_zmm8r8
              !dir$ attributes forceinline :: besj1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besj1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 0
              val  = calcjy1_zmm8r8(x,jint) 
          end function besj1_zmm8r8   
          
          
#if 0
/*
    !*****************************************************************************80
!
!! BESK0 evaluates the Bessel K0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order zero
!    for arguments 0.0 < ARG <= XMAX.
!
!    See comments heading CALCK0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESK0, the value of the function.
*/
#endif   


         pure function besk0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besk0_zmm8r8
              !dir$ attributes forceinline :: besk0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besk0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = calck0_zmm8r8(x,jint) 
          end function besk0_zmm8r8   
          
          
#if 0
/*
      *****************************************************************************80
!
!! BESK1 evaluates the Bessel K1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order one
!    for arguments XLEAST <= ARG <= XMAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESK1, the value of the function.        
*/  
#endif


         pure function besk1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besk1_zmm8r8
              !dir$ attributes forceinline :: besk1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besk1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = calck1_zmm8r8(x,jint) 
          end function besk1_zmm8r8   
          
          
#if 0
/*
  !*****************************************************************************80
!
!! BESY0 evaluates the Bessel Y0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the second kind of order zero for arguments 0 < X <= XMAX.
!
!    See comments heading CALJY0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESY0, the value of the function.
*/	      
#endif


          pure function besy0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besy0_zmm8r8
              !dir$ attributes forceinline :: besy0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besy0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = caly0_zmm8r8(x,jint) 
          end function besy0_zmm8r8   
          
          
#if 0
/*
    !*****************************************************************************80
!
!! BESY1 evaluates the Bessel Y1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the second kind of order zero for arguments 0 < X <= XMAX.
!
!    See comments heading CALJY1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESY1, the value of the function.     

*/    
#endif


          pure function besy1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besy1_zmm8r8
              !dir$ attributes forceinline :: besy1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besy1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 1
              val  = caly1_zmm8r8(x,jint) 
          end function besy1_zmm8r8   

!! /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !!

#if 0
        !*****************************************************************************80
!
!! CALCEI computes various exponential integrals.
!
!  Discussion:
!
!    This routine computes the exponential integrals Ei(x),
!    E1(x), and  exp(-x)*Ei(x) for real arguments x where
!
!           integral (from t=-oo to t=x) (exp(t)/t),  x > 0,
!    Ei(x) =
!          -integral (from t=-x to t=+oo) (exp(t)/t),  x < 0,
!
!    and where the first integral is a principal value integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Henry Thacher,
!    Rational Chebyshev Approximations for the Exponential
!    Integral E1(x),
!    Mathematics of Computation,
!    Volume 22, Number 103, July 1968, pages 641-649.
!
!    William Cody, Henry Thacher,
!    Chebyshev Approximations for the Exponential
!    Integral Ei(x),
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 289-303.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  The argument must not
!    be zero.  If JINT = 2, then the argument must be strictly positive.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = EI ( ARG );
!    2, RESULT = EONE ( ARG );
!    3, RESULT = EXPEI ( ARG ).
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, Ei(x);
!    2, -Ei(-x);
!    3, exp(-x)*Ei(x).
!
#endif   


           pure function calcei_zmm8r8(arg,jint) result(val)
           
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calcei_zmm8r8
              !dir$ attributes forceinline :: calcei_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_zmm8r8  
               type(ZMM8r8_t),   intent(in) :: arg
               integer(kind=i4), intent(in) :: jint
               type(ZMM8r8_t)  :: val
              
           end function calcei_zmm8r8


           

end module spec_funcs_zmm8r8
