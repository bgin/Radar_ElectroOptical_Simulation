

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
     !dir$ attributes align : 64 :: calcei_b
     !dir$ attributes align : 64 :: calcei_c
     !dir$ attributes align : 64 :: calcei_d
     !dir$ attributes align : 64 :: calcei_e
     !dir$ attributes align : 64 :: calcei_f
     !dir$ attributes align : 64 :: calcei_plg
     !dir$ attributes align : 64 :: calcei_qlg
     !dir$ attributes align : 64 :: calcei_p
     !dir$ attributes align : 64 :: calcei_q
     !dir$ attributes align : 64 :: calcei_r
     !dir$ attributes align : 64 :: calcei_s
     !dir$ attributes align : 64 :: calcei_p1
     !dir$ attributes align : 64 :: calcei_q1
     !dir$ attributes align : 64 :: calcei_p2
     !dir$ attributes align : 64 :: calcei_q2
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
                                                          
                                                          
     !!
     !! calci0_zmm8r8 constant arrays (saved)
     !!
     !dir$ attributes align : 64 :: calci0_p
     !dir$ attributes align : 64 :: calci0_q
     !dir$ attributes align : 64 :: calci0_pp
     !dir$ attributes align : 64 :: calci0_qq
     type(ZMM8r8_t), dimension(0:14), save :: calci0_p  =[ZMM8r8_t(-5.2487866627945699800e-18_dp),        &
                                                          ZMM8r8_t(-1.5982226675653184646e-14_dp),        &
                                                          ZMM8r8_t(-2.6843448573468483278e-11_dp),        &
                                                          ZMM8r8_t(-3.0517226450451067446e-08_dp),        &
                                                          ZMM8r8_t(-2.5172644670688975051e-05_dp),        &
                                                          ZMM8r8_t(-1.5453977791786851041e-02_dp),        &
                                                          ZMM8r8_t(-7.0935347449210549190e+00_dp),        &
                                                          ZMM8r8_t(-2.4125195876041896775e+03_dp),        &
                                                          ZMM8r8_t(-5.9545626019847898221e+05_dp),        &
                                                          ZMM8r8_t(-1.0313066708737980747e+08_dp),        &
                                                          ZMM8r8_t(-1.1912746104985237192e+10_dp),        &
                                                          ZMM8r8_t(-8.4925101247114157499e+11_dp),        &
                                                          ZMM8r8_t(-3.2940087627407749166e+13_dp),        &
                                                          ZMM8r8_t(-5.5050369673018427753e+14_dp),        &
                                                          ZMM8r8_t(-2.2335582639474375249e+15_dp)]
      type(ZMM8r8_t), dimension(0:4), save ::  calci0_q =[ZMM8r8_t(3.7277560179962773046e+03_dp),         &
                                                          ZMM8r8_t(6.5158506418655165707e+06_dp),         &
                                                          ZMM8r8_t(-6.5626560740833869295e+09_dp),        &
                                                          ZMM8r8_t(3.7604188704092954661e+12_dp),         &
                                                          ZMM8r8_t(-9.7087946179594019126d+14_dp)]
      type(ZMM8r8_t), dimension(0:7), save ::  calci0_pp=[ZMM8r8_t(3.9843750000000000000e-01_dp),         & 
                                                          ZMM8r8_t(2.9205384596336793945e+00_dp),         &
                                                          ZMM8r8_t(-2.4708469169133954315e+00_dp),        &
                                                          ZMM8r8_t(4.7914889422856814203e-01_dp),         &
                                                          ZMM8r8_t(-3.7384991926068969150e-03_dp),        &
                                                          ZMM8r8_t(-2.6801520353328635310e-03_dp),        &
                                                          ZMM8r8_t(9.9168777670983678974e-05_dp),         &
                                                          ZMM8r8_t(-2.1877128189032726730e-06_dp)]
      type(ZMM8r8_t), dimension(0:6), save ::  calci0_qq=[ZMM8r8_t(-3.1446690275135491500e+01_dp),        & 
                                                          ZMM8r8_t(8.5539563258012929600e+01_dp),         &
                                                          ZMM8r8_t(-6.0228002066743340583e+01_dp),        &
                                                          ZMM8r8_t(1.3982595353892851542e+01_dp),         &
                                                          ZMM8r8_t(-1.1151759188741312645e+00_dp),        &
                                                          ZMM8r8_t(3.2547697594819615062e-02_dp),         &
                                                          ZMM8r8_t(-5.5194330231005480228e-04_dp)] 
    
    !!
    !! calci1_zmm8r8 constant arrays (saved)
    !!
     type(ZMM8r8_t), dimension(0:14), save :: calci1_p = [ZMM8r8_t(-1.9705291802535139930e-19_dp),           &
                                                          ZMM8r8_t(-6.5245515583151902910e-16_dp),           &
                                                          ZMM8r8_t(-1.1928788903603238754e-12_dp),           &
                                                          ZMM8r8_t(-1.4831904935994647675e-09_dp),           &
                                                          ZMM8r8_t(-1.3466829827635152875e-06_dp),           &
                                                          ZMM8r8_t(-9.1746443287817501309e-04_dp),           &
                                                          ZMM8r8_t(-4.7207090827310162436e-01_dp),           &
                                                          ZMM8r8_t(-1.8225946631657315931e+02_dp),           &
                                                          ZMM8r8_t(-5.1894091982308017540e+04_dp),           &
                                                          ZMM8r8_t(-1.0588550724769347106e+07_dp),           &
                                                          ZMM8r8_t(-1.4828267606612366099e+09_dp),           &
                                                          ZMM8r8_t(-1.3357437682275493024e+11_dp),           &
                                                          ZMM8r8_t(-6.9876779648010090070e+12_dp),           &
                                                          ZMM8r8_t(-1.7732037840791591320e+14_dp),           &
                                                          ZMM8r8_t(-1.4577180278143463643e+15_dp)]
     type(ZMM8r8_t), dimension(0:4), save ::  calci1_q = [ZMM8r8_t(-4.0076864679904189921e+03_dp),           &
                                                          ZMM8r8_t(7.4810580356655069138e+06_dp),            &
                                                          ZMM8r8_t(-8.0059518998619764991e+09_dp),           &
                                                          ZMM8r8_t(4.8544714258273622913e+12_dp),            &
                                                          ZMM8r8_t(-1.3218168307321442305e+15_dp)]
     type(ZMM8r8_t), dimension(0:7), save ::  calci1_pp =[ZMM8r8_t(-6.0437159056137600000e-02_dp),           &
                                                          ZMM8r8_t(4.5748122901933459000e-01_dp),            &
                                                          ZMM8r8_t(-4.2843766903304806403e-01_dp),           &
                                                          ZMM8r8_t(9.7356000150886612134e-02_dp),            &
                                                          ZMM8r8_t(-3.2457723974465568321e-03_dp),           &
                                                          ZMM8r8_t(-3.6395264712121795296e-04_dp),           &
                                                          ZMM8r8_t( 1.6258661867440836395e-05_dp),           &
                                                          ZMM8r8_t(-3.6347578404608223492e-07_dp)]
     type(ZMM8r8_t), dimension(0:5), save ::  calci1_qq =[ZMM8r8_t(-3.8806586721556593450e+00_dp),           &
                                                          ZMM8r8_t(3.2593714889036996297e+00_dp),            &
                                                          ZMM8r8_t(-8.5017476463217924408e-01_dp),           &
                                                          ZMM8r8_t(7.4212010813186530069e-02_dp),            &
                                                          ZMM8r8_t(-2.2835624489492512649e-03_dp),           &
                                                          ZMM8r8_t(3.7510433111922824643e-05_dp)]
    !!
    !! calck0_zmm8r8 constant arrays (saved)
    !!
     type(ZMM8r8_t), dimension(0:5), save ::  calck0_p = [ZMM8r8_t(5.8599221412826100000e-04_dp),            & 
	                                                  ZMM8r8_t(1.3166052564989571850e-01_dp),            &
                                                          ZMM8r8(1.1999463724910714109e+01_dp),              & 
                                                          ZMM8r8(4.6850901201934832188e+02_dp),              & 
                                                          ZMM8r8(5.9169059852270512312e+03_dp),              &
                                                          ZMM8r8(2.4708152720399552679e+03_dp)]
     type(ZMM8r8_t), dimension(0:1), save ::  calck0_q = [ZMM8r8_t(-2.4994418972832303646e+02_dp),           & 
	                                                  ZMM8r8_t(2.1312714303849120380e+04_dp)] 
     type(ZMM8r8_t), dimension(0:3), save ::  calck0_f = [ZMM8r8_t(-1.6414452837299064100e+00_dp),           &
	                                                  ZMM8r8_t(-2.9601657892958843866e+02_dp),           &
                                                          ZMM8r8_t(-1.7733784684952985886e+04_dp),           &           
                                                          ZMM8r8_t(-4.0320340761145482298e+05_dp)]
     type(ZMM8r8_t), dimension(0:2), save ::  calck0_g = [ZMM8r8_t(2.5064972445877992730e+02_dp),            &
	                                                 [ZMM8r8_t(2.9865713163054025489e+04_dp),            & 
                                                         [ZMM8r8_t(-1.6128136304458193998e+06_dp)]
     type(ZMM8r8_t), dimension(0:9), save ::  calck0_pp= [ZMM8r8_t(1.1394980557384778174e+02_dp),            & 
	                                                  ZMM8r8_t(3.6832589957340267940e+03_dp),            & 
                                                          ZMM8r8_t(3.1075408980684392399e+04_dp),            & 
                                                          ZMM8r8_t(1.0577068948034021957e+05_dp),            & 
                                                          ZMM8r8_t(1.7398867902565686251e+05_dp),            &
                                                          ZMM8r8_t(1.5097646353289914539e+05_dp),            & 
                                                          ZMM8r8_t(7.1557062783764037541e+04_dp),            & 
                                                          ZMM8r8_t(1.8321525870183537725e+04_dp),            & 
                                                          ZMM8r8_t(2.3444738764199315021e+03_dp),            & 
                                                          ZMM8r8_t(1.1600249425076035558e+02_dp)]
     type(ZMM8r8_t), dimension(0:9), save ::  calck0_qq= [ZMM8r8_t(2.0013443064949242491e+02_dp),            & 
	                                                  ZMM8r8_t(4.4329628889746408858e+03_dp),            & 
                                                          ZMM8r8_t(3.1474655750295278825e+04_dp),            &
                                                          ZMM8r8_t(9.7418829762268075784e+04_dp),            & 
                                                          ZMM8r8_t(1.5144644673520157801e+05_dp),            & 
                                                          ZMM8r8_t(1.2689839587977598727e+05_dp),            & 
                                                          ZMM8r8_t(5.8824616785857027752e+04_dp),            & 
                                                          ZMM8r8_t(1.4847228371802360957e+04_dp),            & 
                                                          ZMM8r8_t(1.8821890840982713696e+03_dp),            & 
                                                          ZMM8r8_t(9.2556599177304839811e+01_dp)]
     
                                         
     contains
     
!! =============================================================================================================== //
!!                                  'Saved' arrays preload_calcei routines.
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
       
       
       pure function preload_calcei_q1() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_q1
              !dir$ attributes forceinline :: preload_calcei_q1
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_q1
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v    = calcei_q1(0).v+calcei_q1(1).v
              t1.v    = calcei_q1(2).v+calcei_q1(3).v
              t2.v    = calcei_q1(4).v+calcei_q1(5).v
              t3.v    = calcei_q1(6).v+calcei_q1(7).v+ &
                        calcei_q1(8).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calcei_q1 
       
       
        pure function preload_calcei_p2() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_p2
              !dir$ attributes forceinline :: preload_calcei_p2
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_p2
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3,t4
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3,t4
              t0.v    = calcei_p2(0).v+calcei_p2(1).v
              t1.v    = calcei_p2(2).v+calcei_p2(3).v
              t2.v    = calcei_p2(4).v+calcei_p2(5).v
              t3.v    = calcei_p2(6).v+calcei_p2(7).v
              t4.v    = calcei_p2(8).v+calcei_p2(9).v
              summa.v = t0.v+t1.v+t2.v+t3.v+t4.v
       end function preload_calcei_p2 
       
       
       pure function preload_calcei_q2() result(summa)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calcei_q2
              !dir$ attributes forceinline :: preload_calcei_q2
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calcei_q2
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v    = calcei_q2(0).v+calcei_q2(1).v
              t1.v    = calcei_q2(2).v+calcei_q2(3).v
              t2.v    = calcei_q2(4).v+calcei_q2(5).v
              t3.v    = calcei_q2(6).v+calcei_q2(7).v+ &
                        calcei_q2(8).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calcei_q2 
       
!! =============================================================================================================== //
!!                                  'Saved' arrays preload_calci0 routines.
!!================================================================================================================ //        
       
       
       pure function preload_calci0_p() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci0_p
              !dir$ attributes forceinline :: preload_calci0_p
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci0_p
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              !dir$ attributes align : 64 :: t4,t5,t6
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t4,t5,t6
              t0.v = calci0_p(0).v+calci0_p(1).v
              t1.v = calci0_p(2).v+calci0_p(3).v
              t2.v = calci0_p(4).v+calci0_p(5).v
              t3.v = calci0_p(6).v+calci0_p(7).v
              t4.v = calci0_p(8).v+calci0_p(9).v
              t5.v = calci0_p(10).v+calci0_p(11).v
              t6.v = calci0_p(12).v+calci0_p(13).v+ &
                     calc0_p(14).v
              summa.v = t0.v+t1.v+t2.v+t3.v+ &
                        t4.v+t5.v+t6.v
       end function preload_calci0_p
       
       
       pure function preload_calci0_q() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci0_q
              !dir$ attributes forceinline :: preload_calci0_q
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci0_q
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1
              type(ZMM8r8_t), automatic :: t0,t1
              t0.v = calci0_p(0).v+calci0_p(1).v
              t1.v = calci0_p(2).v+calci0_p(3).v+ &
                     calci0_p(4)
              summa.v = t0.v+t1.v
       end function preload_calci0_q
       
       
       pure function preload_calci0_pp() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci0_pp
              !dir$ attributes forceinline :: preload_calci0_pp
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci0_pp
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v = calci0_pp(0).v+calci0_pp(1).v
              t1.v = calci0_pp(2).v+calci0_pp(3).v
              t2.v = calci0_pp(4).v+calci0_pp(5).v
              t3.v = calci0_pp(6).v+calci0_pp(7).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calci0_pp
       
       
       pure function preload_calci0_qq() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci0_qq
              !dir$ attributes forceinline :: preload_calci0_qq
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci0_qq
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2
              type(ZMM8r8_t), automatic :: t0,t1,t2
              t0.v = calci0_qq(0).v+calci0_qq(1).v
              t1.v = calci0_qq(2).v+calci0_qq(3).v
              t2.v = calci0_qq(4).v+calci0_qq(5).v+ &
                     calci0_qq(6).v
              summa.v = t0.v+t1.v+t2.v
       end function preload_calci0_qq
       
!! =============================================================================================================== //
!!                                  'Saved' arrays preload_calci1 routines.
!!================================================================================================================ //        
       
       
       pure function preload_calci1_p() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calc1_p
              !dir$ attributes forceinline :: preload_calci1_p
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci1_p
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              !dir$ attributes align : 64 :: t4,t5,t6
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t4,t5,t6
              t0.v = calci1_p(0).v+calci1_p(1).v
              t1.v = calci1_p(2).v+calci1_p(3).v
              t2.v = calci1_p(4).v+calci1_p(5).v
              t3.v = calci1_p(6).v+calci1_p(7).v
              t4.v = calci1_p(8).v+calci1_p(9).v
              t5.v = calci1_p(10).v+calci1_p(11).v
              t6.v = calci1_p(12).v+calci1_p(13).v+ &
                     calc1_p(14).v
              summa.v = t0.v+t1.v+t2.v+t3.v+ &
                        t4.v+t5.v+t6.v
       end function preload_calci1_p       
       
       
       pure function preload_calci1_q() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci1_q
              !dir$ attributes forceinline :: preload_calci1_q
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci1_q
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1
              type(ZMM8r8_t), automatic :: t0,t1
              t0.v = calci1_p(0).v+calci1_p(1).v
              t1.v = calci1_p(2).v+calci1_p(3).v+ &
                     calci1_p(4).v
              summa.v = t0.v+t1.v
       end function preload_calci1_q
       
       
       pure function preload_calci1_pp() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci1_pp
              !dir$ attributes forceinline :: preload_calci1_pp
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci1_pp
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2,t3
              type(ZMM8r8_t), automatic :: t0,t1,t2,t3
              t0.v = calci1_pp(0).v+calci1_pp(1).v
              t1.v = calci1_pp(2).v+calci1_pp(3).v
              t2.v = calci1_pp(4).v+calci1_pp(5).v
              t3.v = calci1_pp(6).v+calci1_pp(7).v
              summa.v = t0.v+t1.v+t2.v+t3.v
       end function preload_calci1_pp
       
       
       pure function preload_calci1_qq() result(summa)
       
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_calci1_qq
              !dir$ attributes forceinline :: preload_calci1_qq
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_calci0_qq
              type(ZMM8r8_t) :: summa
              !dir$ attributes align : 64 :: t0,t1,t2
              type(ZMM8r8_t), automatic :: t0,t1,t2
              t0.v = calci1_qq(0).v+calci1_qq(1).v
              t1.v = calci1_qq(2).v+calci1_qq(3).v
              t2.v = calci1_qq(4).v+calci1_qq(5).v
              
              summa.v = t0.v+t1.v+t2.v
       end function preload_calci1_qq

!! =============================================================================================================== //
!!                                  'Saved' arrays preload_calck0 routines.
!!================================================================================================================ //         
     
        
       
     
     
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


         subroutine calcei_zmm8r8(arg,val,jint) 
           
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calcei_zmm8r8
              !dir$ attributes forceinline :: calcei_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_zmm8r8  
               type(ZMM8r8_t),   intent(in)   :: arg
               type(ZMM8r8_t),   intent(out)  :: val
               integer(kind=i4), intent(in)   :: jint
               
               !dir$ attributes align : 64 :: q
               !dir$ attributes align : 64 :: qlq
               !dir$ attributes align : 64 :: qx
               !dir$ attributes align : 64 :: px
               type(ZMM8r8_t), dimension(0:9), automatic  :: q
               type(ZMM8r8_t), dimension(0:9), automatic  :: qlq
               type(ZMM8r8_t), dimension(0:9), automatic  :: qx
               type(ZMM8r8_t), dimension(0:9), automatic  :: px
               !dir$ attributes align : 64 :: zero
               !dir$ attributes align : 64 :: p037
               !dir$ attributes align : 64 :: half
               !dir$ attributes align : 64 :: one
               !dir$ attributes align : 64 :: two
               !dir$ attributes align : 64 :: three
               !dir$ attributes align : 64 :: four
               !dir$ attributes align : 64 :: six
               !dir$ attributes align : 64 :: twlve
               !dir$ attributes align : 64 :: two4
               !dir$ attributes align : 64 :: frty
               !dir$ attributes align : 64 :: exp40
               !dir$ attributes align : 64 :: x01
               !dir$ attributes align : 64 :: x11
               !dir$ attributes align : 64 :: x02
               !dir$ attributes align : 64 :: x0
               !dir$ attributes align : 64 :: xinf
               !dir$ attributes align : 64 :: xmax
               !dir$ attributes align : 64 :: xbig
               type(ZMM8r8_t), parameter :: zero = ZMM8r8_t(0.0e+0_dp)
               type(ZMM8r8_t), parameter :: p037 = ZMM8r8_t(0.037e+0_dp)
               type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5e+0_dp)
               type(ZMM8r8_t), parameter :: one  = ZMM8r8_t(1.0e+0_dp)
               type(ZMM8r8_t), parameter :: two  = ZMM8r8_t(2.0e+0_dp)
               type(ZMM8r8_t), parameter :: three= ZMM8r8_t(3.0e+0_dp)
               type(ZMM8r8_t), parameter :: four = ZMM8r8_t(4.0e+0_dp)
               type(ZMM8r8_t), parameter :: six  = ZMM8r8_t(6.0e+0_dp)
               type(ZMM8r8_t), parameter :: twlve= ZMM8r8_t(12.0e+0_dp)
               type(ZMM8r8_t), parameter :: two4 = ZMM8r8_t(24.0e+0_dp)
               type(ZMM8r8_t), parameter :: frty = ZMM8r8_t(40.0e+0_dp)
               type(ZMM8r8_t), parameter :: exp40= ZMM8r8_t(2.3538526683701998541e+17)
               type(ZMM8r8_t), parameter :: x01  = ZMM8r8_t(381.5e+0_dp)
               type(ZMM8r8_t), parameter :: x11  = ZMM8r8_t(1024.0e+0_dp)
               type(ZMM8r8_t), parameter :: x02  = ZMM8r8_t(-5.1182968633365538008e-5_dp)
               type(ZMM8r8_t), parameter :: x0   = ZMM8r8_t(3.7250741078136663466e-1_dp)
               type(ZMM8r8_t), parameter :: xinf = ZMM8r8_t(1.79e+308_dp)
               type(ZMM8r8_t), parameter :: xmax = ZMM8r8_t(716.351e+0_dp)
               type(ZMM8r8_t), parameter :: xbig = ZMM8r8_t(701.84e+0_dp)
               !dir$ attributes align : 64 :: ei
               !dir$ attributes align : 64 :: frac
               !dir$ attributes align : 64 :: res
               !dir$ attributes align : 64 :: sump
               !dir$ attributes align : 64 :: sumq
               !dir$ attributes align : 64 :: t
               !dir$ attributes align : 64 :: w
               !dir$ attributes align : 64 :: x
               !dir$ attributes align : 64 :: mx0
               !dir$ attributes align : 64 :: y
               !dir$ attributes align : 64 :: ysq
               !dir$ attributes align : 64 :: t0
               !dir$ attributes align : 64 :: t1
               type(ZMM8r8_t), automatic :: ei,frac
               type(ZMM8r8_t), automatic :: res
               type(ZMM8r8_t), automatic :: sump,sumq
               type(ZMM8r8_t), automatic :: t,w
               type(ZMM8r8_t), automatic :: x,mx0
               type(ZMM8r8_t), automatic :: y,ysq
               type(ZMM8r8_t), automatic :: t0,t1
               type(Mask8_t),  automatic :: msk1
               type(Mask8_t),  automatic :: msk2
               type(Mask8_t),  automatic :: msk3
               type(Mask8_t),  automatic :: msk4
               type(Mask8_t),  automatic :: msk5
               type(Mask8_t),  automatic :: msk6
               type(Mask8_t),  automatic :: msk7
               type(Mask8_t),  automatic :: msk8
               type(Mask8_t),  automatic :: msk9
               type(Mask8_t),  automatic :: msk10
               x.v    = arg.v
               msk1.m = (x.v==zero.v)
               msk2.m = (x.v<zero.v)
               msk6.m = (x.v<six.v)
               msk8.m = (x.v<twlve.v)
               msk9.m = (x.v<=two4.v)
               if(all(msk1.m)) then
               
                   ei.v = -xinf.v
                   if(jint==2) ei.v = -ei.v
                   ! /*
	           !             !
                   !              !  Calculate EI for negative argument or for E1.
                   !             !   
	           !          */
	       else if(all(msk2.m).or.jint==2) then
	       
	             y.v    = abs(x.v)
	             msk3.m = (y.v<one.v)
	             msk4.m = (y.v<=four.v)
	             msk5.m = (xbig.v<y.v)
	             if(all(msk3.m)) then
	           
	                sump.v = calcei_a[6].v,y.v,calcei_a(0).v)
	                sumq.v = y.v+calcei_b(0).v
	                sump.v = sump.v*y.v+calcei_a(1).v
	                sumq.v = sumq.v*y.v+calcei_b(1).v
	                sump.v = sump.v*y.v+calcei_a(2).v
	                sumq.v = sumq.v*y.v+calcei_b(2).v
	                sump.v = sump.v*y.v+calcei_a(3).v
	                sumq.v = sumq.v*y.v+calcei_b(3).v
	                sump.v = sump.v*y.v+calcei_a(4).v
	                sumq.v = sumq.v*y.v+calcei_b(4).v
	                sump.v = sump.v*y.v+calcei_a(5).v
	                sumq.v = sumq.v*y.v+calcei_b(5).v
	                ei.v   = log(y.v)-(sump.v/sumq.v)
	                if(jint==3) ei.v = ei.v*exp(y.v)
	              
	             else if(all(msk4.m)) then
	              
	                w.v    = one.v/y.v
	                sump.v = calcei_c(0).v
	                sumq.v = calcei_d(0).v
	                sump.v = sump.v*w.v+calcei_c(1).v
	                sumq.v = sumq.v*w.v+calcei_d(1).v
	                sump.v = sump.v*w.v+calcei_c(2).v
	                sumq.v = sumq.v*w.v+calcei_d(2).v
	                sump.v = sump.v*w.v+calcei_c(3).v
	                sumq.v = sumq.v*w.v+calcei_d(3).v
	                sump.v = sump.v*w.v+calcei_c(4).v
	                sumq.v = sumq.v*w.v+calcei_d(4).v
	                sump.v = sump.v*w.v+calcei_c(5).v
	                sumq.v = sumq.v*w.v+calcei_d(5).v
	                sump.v = sump.v*w.v+calcei_c(6).v
	                sumq.v = sumq.v*w.v+calcei_d(6).v
	                sump.v = sump.v*w.v+calcei_c(7).v
	                sumq.v = sumq.v*w.v+calcei_d(7).v
	                sump.v = sump.v*w.v+calcei_c(8).v
	                sumq.v = sumq.v*w.v+calcei_d(8).v
	                ei.v   = -sump.v/sumq.v
	              
	                if(jint/=3) ei.v = ei.v*exp(-y.v) 
	              
	             else 
	               
	                if(all(msk5.m).and.jint<3) then
	              
	                    ei.v = zero.v
	               
	                else
	               
	                    w.v    = one.v/y.v
	                    sump.v = calcei_e(0).v
	                    sumq.v = calcei_f(0).v
	                    sump.v = sump.v*w.v+calcei_e(1).v
	                    sumq.v = sumq.v*w.v+calcei_f(1).v
	                    sump.v = sump.v*w.v+calcei_e(2).v
	                    sumq.v = sumq.v*w.v+calcei_f(2).v
	                    sump.v = sump.v*w.v+calcei_e(3).v
	                    sumq.v = sumq.v*w.v+calcei_f(3).v 
	                    sump.v = sump.v*w.v+calcei_e(4).v
	                    sumq.v = sumq.v*w.v+calcei_f(4).v
	                    sump.v = sump.v*w.v+calcei_e(5).v
	                    sumq.v = sumq.v*w.v+calcei_f(5).v
	                    sump.v = sump.v*w.v+calcei_e(6).v
	                    sumq.v = sumq.v*w.v+calcei_f(6).v
	                    sump.v = sump.v*w.v+calcei_e(7).v
	                    sumq.v = sumq.v*w.v+calcei_f(7).v
	                    sump.v = sump.v*w.v+calcei_e(8).v
	                    sumq.v = sumq.v*w.v+calcei_f(8).v
	                    sump.v = sump.v*w.v+calcei_e(9).v
	                    sumq.v = sumq.v*w.v+calcei_f(9).v
	                    t0.v   = sump.v/sumq.v
	                    t1.v   = one.v-w.v
	                    ei.v   = -w.v*t0.v*t1.v
	               
	                    if(jint/=3) ei.v = -y.v*ei.v
	               
	                end if
	           
	             end if
	       
	             if(jint==2) ei.v = -ei.v
	              !    /*
	              !                  !
                      !                  !  To improve conditioning, rational approximations are expressed
                      !                  !  in terms of Chebyshev polynomials for 0 <= X < 6, and in
                      !                  !  continued fraction form for larger X.
                      !                  !
	              !               */
	       else if(all(msk6.m)) then
	             
	             t.v     = x.v+x.v
	             t.v     = (t.v/three.v)-two.v
	             px(0).v = zero.v
	             qx(0).v = zero.v
	             px(1).v = p(0).v
	             qx(1).v = q(0).v
	             px(2).v = t.v*px(1).v-px(0).v+p(1).v
	             qx(2).v = t.v*qx(1).v-qx(0).v+q(1).v
	             px(3).v = t.v*px(2).v-px(1).v+p(2).v
	             qx(3).v = t.v*qx(2).v-qx(1).v+q(2).v
	             px(4).v = t.v*px(3).v-px(2).v+p(3).v
	             qx(4).v = t.v*qx(3).v-qx(2).v+q(3).v
	             px(5).v = t.v*px(4).v-px(3).v+p(4).v
	             qx(5).v = t.v*qx(4).v-qx(3).v+q(4).v
	             px(6).v = t.v*px(5).v-px(4).v+p(5).v
	             qx(6).v = t.v*qx(5).v-qx(4).v+q(5).v
	             px(7).v = t.v*px(6).v-px(5).v+p(6).v
	             qx(7).v = t.v*qx(6).v-qx(5).v+q(6).v
	             px(8).v = t.v*px(7).v-px(6).v+p(7).v
	             qx(8).v = t.v*qx(7).v-qx(6).v+q(7).v
	             t0.v    = half.v*t.v
	             sump.v  = t0.v*px(9).v-px(8).v+p(9).v
	             sumq.v  = t0.v*qx(9).v-qx(8).v+q(9).v
	             frac.v  = sump.v/sumq.v
	             t0.v    = x.v-x01.v/x11.v
	             xmx0.v  = t0.v-x02.v
	             msk7.m  = (p037.v<=abs(xmx0.v)
	             
	             if(all(msk7.m)) then
	                   t0.v = x.v/x0.v
	                   ei.v = frac.v*xmx0.v+log(t0.v)
	                   if(jint==3) ei.v = exp(-x.v)*ei.v
	             else
	                 !  //Special approximation to ln(X/X0) for X close to X0. 
	                 y.v    = xmx0.v/(x.v+x0.v)
	                 ysq.v  = y.v*y.v
	                 sump.v = calcei_plg(0).v
	                 sumq.v = ysq.v+calcei_qlg(0).v
	                 sump.v = sump.v*ysq.v+calcei_plg(1).v
	                 sumq.v = sumq.v*ysq.v+calcei_qlg(1).v
	                 sump.v = sump.v*ysq.v+calcei_plg(2).v
	                 sumq.v = sumq.v*ysq.v+calcei_qlg(2).v
	                 sump.v = sump.v*ysq.v+calcei_plg(3).v
	                 sumq.v = sumq.v*ysq.v+calcei_qlg(3).v
	                 t0.v   = sumq.v*(x.v+x0.v)+frac.v
	                 ei.v   = (sump.v/t0.v)*xmx0.v
	                 
	                 if(jint==3) ei.v = exp(-x.v)*ei.v 
	             end if
	             
	       else if(all(msk8.m)) then
	       
	              frac.v = zero.v
	              frac.v= calcei_s(0).v/(calcei_r(0).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(1).v/(calcei_r(1).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(2).v/(calcei_r(2).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(3).v/(calcei_r(3).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(4).v/(calcei_r(4).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(5).v/(calcei_r(5).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(6).v/(calcei_r(6).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(7).v/(calcei_r(7).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_s(8).v/(calcei_r(8).v+ &
	                                          x.v+frac.v)
	              ei.v  = (calcei_r(9).v+frac.v)/x.v
	              
	              if(jint==3) ei.v = ei.v*exp(x.v)
	              
	        else if(all(msk9.m)) then
	        
	              frac.v = zero.v
	              frac.v= calcei_q1(0).v/(calcei_p1(0).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(1).v/(calcei_p1(1).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(2).v/(calcei_p1(2).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(3).v/(calcei_p1(3).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(4).v/(calcei_p1(4).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(5).v/(calcei_p1(5).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(6).v/(calcei_p1(6).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(7).v/(calcei_p1(7).v+ &
	                                          x.v+frac.v)
	              frac.v= calcei_q1(8).v/(calcei_p1(8).v+ &
	                                          x.v+frac.v)
	              ei.v  = (calcei_p1(9).v+frac.v)/x.v
	              
	              if(jint/=3) ei.v = ei.v*exp(x.v)
	                                     
	        else
	             
	               msk10.m = (xmax.v<=x.v)
	               if(all(msk10.m).and.jint==3) then
	                   ei.v = xinf.v
	               else
	                   y.v    = one.v/x.v
	                   frac.v = zero.v
	                   frac.v= calcei_q2(0).v/(calcei_p2(0).v+ &
	                                               x.v+frac.v)
	                   frac.v= calcei_q2(1).v/(calcei_p2(1).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(2).v/(calcei_p2(2).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(3).v/(calcei_p2(3).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(4).v/(calcei_p2(4).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(5).v/(calcei_p2(5).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(6).v/(calcei_p2(6).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(7).v/(calcei_p2(7).v+ &
	                                          x.v+frac.v)
	                   frac.v= calcei_q2(8).v/(calcei_p2(8).v+ &
	                                          x.v+frac.v)   
	                   frac.v= calcei_p2(9).v+frac.v
	                   ei.v  = frac.v*y.v*y.v+y.v
	                   
	                   if(jint/=3) then
	                      msk10.m = (x.v<=(xmax.v-two4.v))
	                      if(all(msk10.m)) then
	                          ei.v = ei.v*exp(x.v)
	                      else
	                          t0.v = x.v-frty.v
	                          ei.v = ei.v*t0.v*exp40.v
	                      end if
	                   end if
	               end if
	           end if
	           val.v = ei.v
       end subroutine calcei_zmm8r8
       
       
#if 0
   *
    !*****************************************************************************80
!
!! CALCI0 computes various I0 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the first kind
!    and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.
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
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 1, then
!    the argument must be less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = I0(x);
!    2, RESULT = exp(-x) * I0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, I0(x);
!    2, exp(-x) * I0(x);      
*/
#endif


        subroutine calci0_zmm8r8(arg,val,jint)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci0_zmm8r8
              !dir$ attributes forceinline :: calci0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_zmm8r8  
               type(ZMM8r8_t),   intent(in)   :: arg
               type(ZMM8r8_t),   intent(out)  :: val
               integer(kind=i4), intent(in)   :: jint 
               !dir$ attributes align : 64 :: one
               !dir$ attributes align : 64 :: one5
               !dir$ attributes align : 64 :: exp40
               !dir$ attributes align : 64 :: frty
               !dir$ attributes align : 64 :: rec15
               !dir$ attributes align : 64 :: two25
               !dir$ attributes align : 64 :: xsmall
               !dir$ attributes align : 64 :: xinf
               !dir$ attributes align : 64 :: xmax
               type(ZMM8r8_t),   parameter    :: one   = ZMM8r8_t(1.0e+0_dp)
               type(ZMM8r8_t),   parameter    :: one5  = ZMM8r8_t(15.0e+0_dp)
               type(ZMM8r8_t),   parameter    :: exp40 = ZMM8r8_t(2.353852668370199854e+17_dp)
               type(ZMM8r8_t),   parameter    :: frty  = ZMM8r8_t(40.0e+0_dp)
               type(ZMM8r8_t),   parameter    :: rec15 = ZMM8r8_t(6.6666666666666666666e-2_dp)
               type(ZMM8r8_t),   parameter    :: two25 = ZMM8r8_t(225.0e+0_dp)
               type(ZMM8r8_t),   parameter    :: xsmall= ZMM8r8_t(5.55e-17_dp)
               type(ZMM8r8_t),   parameter    :: xinf  = ZMM8r8_t(1.79e+308_dp)
               type(ZMM8r8_t),   parameter    :: xmax  = ZMM8r8_t(713.986e+0_dp)
               !dir$ attributes align : 64 :: a
               !dir$ attributes align : 64 :: b
               !dir$ attributes align : 64 :: sump
               !dir$ attributes align : 64 :: sumq
               !dir$ attributes align : 64 :: x
               !dir$ attributes align : 64 :: xx
               !dir$ attributes align : 64 :: t0
               !dir$ attributes align : 64 :: t1
               type(ZMM8r8_t),   automatic    :: a,b
               type(ZMM8r8_t),   automatic    :: sump,sumq
               type(ZMM8r8_t),   automatic    :: x,xx
               type(ZMM8r8_t),   automatic    :: t0,t1
               type(Mask8_t),    automatic    :: msk1,msk2
               type(Mask8_t),    automatic    :: msk3,msk4
               x.v    = abs(arg.v)
               msk1.m = (x.v<xsmall.v)
               msk2.m = (x.v<one5.v)
               msk3.m = (one5.v<=x.v)
               if(all(msk1.m)) then
                  val.v = one.v
               else if(all(msk2.m)) then
                  xx.v   = x.v*x.v
                  sump.v = calci0_p(0).v
                  sump.v = sump.v*xx.v+calci0_p(1).v
                  sump.v = sump.v*xx.v+calci0_p(2).v
                  sump.v = sump.v*xx.v+calci0_p(3).v
                  sump.v = sump.v*xx.v+calci0_p(4).v
                  sump.v = sump.v*xx.v+calci0_p(5).v
                  sump.v = sump.v*xx.v+calci0_p(6).v
                  sump.v = sump.v*xx.v+calci0_p(7).v
                  sump.v = sump.v*xx.v+calci0_p(8).v
                  sump.v = sump.v*xx.v+calci0_p(9).v
                  sump.v = sump.v*xx.v+calci0_p(10).v
                  sump.v = sump.v*xx.v+calci0_p(11).v
                  sump.v = sump.v*xx.v+calci0_p(12).v
                  sump.v = sump.v*xx.v+calci0_p(13).v
                  sump.v = sump.v*xx.v+calci0_p(14).v
                  xx.v   = xx.v-two25.v
                  sumq.v = (((( &
                             xx.v+calcei0_q(0).v) &
                           * xx.v+calcei0_q(1).v) &
                           * xx.v+calcei0_q(2).v) &
                           * xx.v+calcei0_q(3).v) &
                           * xx.v+calcei0_q(4)
                  val.v  = sump.v/sumq.v
                  if(jint==2) val.v = val.v*exp(-x.v)
             else if(all(msk3.m)) then
                     msk4.m = (xmax.v<=x.v)
                     if(jint==1.and.all(msk4.m)) then
                         val.v = xinf.v
                     else
                         xx.v    = one.v/(x.v-rec15.v)
                         sump.v  = ((((((   &
                                      calci0_pp(0).v      &
                                   * xx.v+calci0_pp(1))   &
                                   * xx.v+calci0_pp(2))   &
                                   * xx.v+calci0_pp(3))   &
                                   * xx.v+calci0_pp(4))   &
                                   * xx.v+calci0_pp(5))   &
                                   * xx.v+calci0_pp(6))   &
                                   * xx.v+calci0_pp(7)
                         sumq.v  = ((((((    &
                                     xx.v+calci0_qq(0).v) &
                                   * xx.v+calci0_qq(1).v) &
                                   * xx.v+calci0_qq(2).v) &
                                   * xx.v+calci0_qq(3).v) &
                                   * xx.v+calci0_qq(4).v) &
                                   * xx.v+calci0_qq(5).v) &
                                   * xx.v+calci0_qq(6).v
                        val.v    = sump.v/sumq.v
                        if(jint==2) val.v = (val.v-calci0_pp(0)/sqrt(x.v))
                    else
                        msk4.m = (x.v<=(xmamx.v-one5.v))
                        if(all(msk4.m)) then
                            a.v = exp(x.v)
                            b.v = one.v
                        else
                            a.v = exp(x.v-frty.v)
                            b.v = exp40.v
                        end if
                        t0.v  = calci0_pp(1).v*a.v
                        t1.v  = sqrt(x.v)
                        val.v = ((val.v*a.v-t0.v)/t1.v)*b.v
                    end if
                  end if
               end if
        end subroutine calci0_zmm8r8
        
        
#if 0
  !*****************************************************************************80
!
!! CALCI1 computes various I1 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functioons of the first kind
!    and order one, I1(X) and EXP(-ABS(X))*I1(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.
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
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 1, then
!    the argument must be less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = I1(x);
!    2, RESULT = exp(-x) * I1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, I1(x);
!    2, exp(-x) * I1(x);   
#endif


        subroutine calci1_zmm8r8(arg,val,jint)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci1_zmm8r8
              !dir$ attributes forceinline :: calci1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_zmm8r8  
              type(ZMM8r8_t),   intent(in)   :: arg
              type(ZMM8r8_t),   intent(out)  :: val
              integer(kind=i4), intent(in)   :: jint 
              !dir$ attributes align : 64 :: one
              !dir$ attributes align : 64 :: one5
              !dir$ attributes align : 64 :: exp40
              !dir$ attributes align : 64 :: frty
              !dir$ attributes align : 64 :: rec15
              !dir$ attributes align : 64 :: two25
              !dir$ attributes align : 64 :: xsmall
              !dir$ attributes align : 64 :: xinf
              !dir$ attributes align : 64 :: xmax
              !dir$ attributes align : 64 :: half
              !dir$ attributes align : 64 :: zero
              !dir$ attributes align : 64 :: pbar
              type(ZMM8r8_t),   parameter    :: one   = ZMM8r8_t(1.0e+0_dp)
              type(ZMM8r8_t),   parameter    :: one5  = ZMM8r8_t(15.0e+0_dp)
              type(ZMM8r8_t),   parameter    :: exp40 = ZMM8r8_t(2.353852668370199854e+17_dp)
              type(ZMM8r8_t),   parameter    :: frty  = ZMM8r8_t(40.0e+0_dp)
              type(ZMM8r8_t),   parameter    :: rec15 = ZMM8r8_t(6.6666666666666666666e-2_dp)
              type(ZMM8r8_t),   parameter    :: two25 = ZMM8r8_t(225.0e+0_dp)
              type(ZMM8r8_t),   parameter    :: xsmall= ZMM8r8_t(5.55e-17_dp)
              type(ZMM8r8_t),   parameter    :: xinf  = ZMM8r8_t(1.79e+308_dp)
              type(ZMM8r8_t),   parameter    :: xmax  = ZMM8r8_t(713.986e+0_dp)
              type(ZMM8r8_t),   parameter    :: half  = ZMM8r8_t(0.5e+00_dp)
              type(ZMM8r8_t),   parameter    :: zero  = ZMM8r8_t(0.0e+00_dp)
              type(ZMM8r8_t),   parameter    :: pbar  = ZMM8r8_t(3.98437500e-01_dp)
              !dir$ attributes align : 64 :: sump
              !dir$ attributes align : 64 :: sumq
              !dir$ attributes align : 64 :: x
              !dir$ attributes align : 64 :: a
              !dir$ attributes align : 64 :: b
              !dir$ attributes align : 64 :: t0
              !dir$ attributes align : 64 :: xx 
              type(ZMM8r8_t),   automatic    :: sump,sumq
              type(ZMM8r8_t),   automatic    :: x,a
              type(ZMM8r8_t),   automatic    :: b,t0
              type(ZMM8r8_t),   automatic    :: xx
              type(Mask8_t),    automatic    :: msk1,msk2
              type(Mask8_t),    automatic    :: msk3,msk4
              type(Mask8_t),    automatic    :: msk5
              x.v    = abs(arg.v)
              msk1.m = (x.v<small.v)
              msk2.m = (x.v<one5.v)
              msk3.m = (xmax.v<x.v)
              if(all(msk1.m)) then
                  val.v = half.v*x.v
              else if(all(msk2.m)) then
                  xx.v  = x.v*x.v
                  sump.v= calci1.p(0).v
                  sump.v= sump.v*xx.v+calci1.p(1).v
                  sump.v= sump.v*xx.v+calci1.p(2).v
                  sump.v= sump.v*xx.v+calci1.p(3).v
                  sump.v= sump.v*xx.v+calci1.p(4).v
                  sump.v= sump.v*xx.v+calci1.p(6).v
                  sump.v= sump.v*xx.v+calci1.p(7).v
                  sump.v= sump.v*xx.v+calci1.p(8).v
                  sump.v= sump.v*xx.v+calci1.p(9).v
                  sump.v= sump.v*xx.v+calci1.p(10).v
                  sump.v= sump.v*xx.v+calci1.p(11).v
                  sump.v= sump.v*xx.v+calci1.p(12).v
                  sump.v= sump.v*xx.v+calci1.p(13).v
                  sump.v= sump.v*xx.v+calci1.p(14).v
                  xx.v  = xx.v-two25.v
                  sumq.v= (((((  &
                            xx.v+calci1.q(0).v)  &
                          * xx.v+calci1.q(1).v)  &
                          * xx.v+calci1.q(2).v)  &
                          * xx.v+calci1.q(3).v)  &
                          * xx.v+calci1.q(4).v
                  val.v = (sump.v/sumq.v)*x.v
                  if(jint==2) val.v = val.v*exp(-x.v)
              else if(jint==1.and.all(msk3.m)) then
                      val.v = xinf.v        
              else
                      xx.v   = one.v/x.v-rec15.v
                      sump.v = ((((((   &
                                      calci1.pp(0)     &
                               * xx.v+calci1.pp(1).v)  &
                               * xx.v+calci1.pp(2).v)  &
                               * xx.v+calci1.pp(3).v)  &
                               * xx.v+calci1.pp(4).v)  &
                               * xx.v+calci1.pp(5).v)  &
                               * xx.v+calci1.pp(6).v)  &
                               * xx.v+calci1.pp(7)
                      sumq.v = (((((    &
                                 xx.v+calci1.qq(0).v)  &
                               * xx.v+calci1.qq(1).v)  &
                               * xx.v+calci1.qq(2).v)  &
                               * xx.v+calci1.qq(3).v)  &
                               * xx.v+calci1.qq(4).v)  &
                               * xx.v+calci1.qq(5).v
                      val.v  = sump.v/sumq.v
                      msk4.m = (xmax.v-one.v<x.v)
                      if(jint/=1) then 
                         val.v = val.v+pbar.v/sqrt(x.v)
                      else
                         
                         if(all(msk4.m)) then
                            a.v = exp(x.v-frty.40)
                            b.v = exp40.v
                         else
                            a.v = exp(x.v)
                            b.v = one.v
                         end if
                      t0.v   = val.v*a.v+pbar.v*a.v
                      val.v  = (t0.v/sqrt(x.v))*b.v
                  end if
              end if
              msk5.m = (arg.v<zero.v)
              if(all(msk5.m)) val.v = -val.v
        end subroutine calci1_zmm8r8
        
        
#if 0
/*
*****************************************************************************80
!
!! CALCK0 computes various K0 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the second kind
!    and order zero, K0(X) and EXP(X)*K0(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of near
!    minimax rational approximations generated by Russon and Blair,
!    Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!    1969.
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
!    Input, real ( kind = 8 ) ARG, the argument.  0 < ARG is
!    always required.  If JINT = 1, then the argument must also be
!    less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = K0(x);
!    2, RESULT = exp(x) * K0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, K0(x);
!    2, exp(x) * K0(x);
*/
#endif


      
         subroutine calck0_zmm8r8(val,arg,jint)
              
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calck0_zmm8r8
              !dir$ attributes forceinline :: calck0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_zmm8r8  
              type(ZMM8r8_t),   intent(in)   :: arg
              type(ZMM8r8_t),   intent(out)  :: val
              integer(kind=i4), intent(in)   :: jint  
              
         end subroutine calck0_zmm8r8

           

end module spec_funcs_zmm8r8
