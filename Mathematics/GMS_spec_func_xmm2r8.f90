

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

module spec_func_xmm2r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         spec_funcs_xmm2r8
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

    use mod_kinds,    only : i4,i8,dp
    use mod_vectypes, only : XMM2r8_t,Mask2_t
    implicit none 
    public
    
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPEC_FUNCS_XMM2R8_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPEC_FUNCS_XMM2R8_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPEC_FUNCS_XMM2R8_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPEC_FUNCS_XMM2R8_FULLVER =   &
            1000*SPEC_FUNCS_XMM2R8_MAJOR+100*SPEC_FUNCS_XMM2R8_MINOR+10*SPEC_FUNCS_XMM2R8_MICRO
     ! Module creation date
     character(*),        parameter :: SPEC_FUNCS_XMM2R8_CREATE_DATE = "28-08-2022 06:11 +00200 (MON 28 AUG 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPEC_FUNCS_XMM2R8_BUILD_DATE  =  __DATE__ 
     character(*),        parameter :: SPEC_FUNCS_XMM2R8_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: SPEC_FUNCS_XMM2R8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPEC_FUNCS_XMM2R8_SYNOPSIS    = "Vectorized various special functions" 
   
     
     !! calcei_xmm2r8 constants arrays (saved).
     
     !dir$ attributes align : 16 :: calcei_a
     !dir$ attributes align : 16 :: calcei_b
     !dir$ attributes align : 16 :: calcei_c
     !dir$ attributes align : 16 :: calcei_d
     !dir$ attributes align : 16 :: calcei_e
     !dir$ attributes align : 16 :: calcei_f
     !dir$ attributes align : 16 :: calcei_plg
     !dir$ attributes align : 16 :: calcei_qlg
     !dir$ attributes align : 16 :: calcei_p
     !dir$ attributes align : 16 :: calcei_q
     !dir$ attributes align : 16 :: calcei_r
     !dir$ attributes align : 16 :: calcei_s
     !dir$ attributes align : 16 :: calcei_p1
     !dir$ attributes align : 16 :: calcei_q1
     !dir$ attributes align : 16 :: calcei_p2
     !dir$ attributes align : 16 :: calcei_q2
     type(XMM2r8_t), dimension(0:6), save :: calcei_a = [XMM2r8_t(1.1669552669734461083368e+2_dp),   &
	                                                  XMM2r8_t(2.1500672908092918123209e+3_dp),   &
                                                          XMM2r8_t(1.5924175980637303639884e+4_dp),   &
                                                          XMM2r8_t(8.9904972007457256553251e+4_dp),   &
                                                          XMM2r8_t(1.5026059476436982420737e+5_dp),   &
                                                          XMM2r8_t(-1.4815102102575750838086e+5_dp),  &
                                                          XMM2r8_t(5.0196785185439843791020e+0_dp)]
      type(XMM2r8_t), dimension(0:5), save :: calcei_b = [XMM2r8_t(4.0205465640027706061433e+1_dp),   & 
                                                          XMM2r8_t(7.5043163907103936624165e+2_dp),   &
                                                          XMM2r8_t(8.1258035174768735759855e+3_dp),   & 
                                                          XMM2r8_t(5.2440529172056355429883e+4_dp),   & 
                                                          XMM2r8_t(1.8434070063353677359298e+5_dp),   & 
                                                          XMM2r8_t(2.5666493484897117319268e+5_dp)]
      type(XMM2r8_t), dimension(0:8), save :: calcei_c = [XMM2r8_t(3.828573121022477169108e-1_dp),    & 
                                                          XMM2r8_t(1.107326627786831743809e+1_dp),    &
                                                          XMM2r8_t(7.246689782858597021199e+1_dp),    & 
                                                          XMM2r8_t(1.700632978311516129328e+2_dp),    & 
                                                          XMM2r8_t(1.698106763764238382705e+2_dp),    &
                                                          XMM2r8_t(7.633628843705946890896e+1_dp),    & 
                                                          XMM2r8_t(1.487967702840464066613e+1_dp),    & 
                                                          XMM2r8_t(9.999989642347613068437e-1_dp),    & 
                                                          XMM2r8_t(1.737331760720576030932e-8_dp)]
      type(XMM2r8_t), dimension(0:8), save :: calcei_d = [XMM2r8_t(8.258160008564488034698e-2_dp),    & 
	                                                  XMM2r8_t(4.344836335509282083360e+0_dp),    & 
                                                          XMM2r8_t(4.662179610356861756812e+1_dp),    & 
                                                          XMM2r8_t(1.775728186717289799677e+2_dp),    & 
                                                          XMM2r8_t(2.953136335677908517423e+2_dp),    & 
                                                          XMM2r8_t(2.342573504717625153053e+2_dp),    & 
                                                          XMM2r8_t(9.021658450529372642314e+1_dp),    & 
                                                          XMM2r8_t(1.587964570758947927903e+1_dp),    & 
                                                          XMM2r8_t(1.000000000000000000000e+0_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_e = [XMM2r8_t(1.3276881505637444622987e+2_dp),   &
                                                          XMM2r8_t(3.5846198743996904308695e+4_dp),   &
                                                          XMM2r8_t(1.7283375773777593926828e+5_dp),   &
                                                          XMM2r8_t(2.6181454937205639647381e+5_dp),   &
                                                          XMM2r8_t(1.7503273087497081314708e+5_dp),   & 
                                                          XMM2r8_t(5.9346841538837119172356e+4_dp),   &
                                                          XMM2r8_t(1.0816852399095915622498e+4_dp),   &
                                                          XMM2r8_t(1.0611777263550331766871e+03_dp),  &
                                                          XMM2r8_t(5.2199632588522572481039e+1_dp),   &
                                                          XMM2r8_t(9.9999999999999999087819e-1_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_f = [XMM2r8_t(3.9147856245556345627078e+4_dp),   &
                                                          XMM2r8_t(2.5989762083608489777411e+5_dp),   &
                                                          XMM2r8_t(5.5903756210022864003380e+5_dp),   &
                                                          XMM2r8_t(5.4616842050691155735758e+5_dp),   &
                                                          XMM2r8_t(2.7858134710520842139357e+5_dp),   &
                                                          XMM2r8_t(7.9231787945279043698718e+4_dp),   &
                                                          XMM2r8_t(1.2842808586627297365998e+4_dp),   &
                                                          XMM2r8_t(1.1635769915320848035459e+3_dp),   &
                                                          XMM2r8_t(5.4199632588522559414924e+1_dp),   &
                                                          XMM2r8_t(1.0000000000000000000000e+0_dp)]
      type(XMM2r8_t), dimension(0:3), save :: calcei_plg=[XMM2r8_t(-2.4562334077563243311e+01_dp),    &
                                                          XMM2r8_t(2.3642701335621505212e+02_dp),     &
                                                          XMM2r8_t(-5.4989956895857911039e+02_dp),    &
                                                          XMM2r8_t(3.5687548468071500413e+02_dp)]
      type(XMM2r8_t), dimension(0:3), save :: calcei_qlg=[XMM2r8_t(-3.5553900764052419184e+01_dp),    &
                                                          XMM2r8_t(1.9400230218539473193e+02_dp),     &
                                                          XMM2r8_t(-3.3442903192607538956e+02_dp),    &
                                                          XMM2r8_t(1.7843774234035750207e+02_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_p  =[XMM2r8_t(-1.2963702602474830028590e+01_dp), &
                                                          XMM2r8_t(-1.2831220659262000678155e+03_dp), &
                                                          XMM2r8_t(-1.4287072500197005777376e+04_dp), &
                                                          XMM2r8_t(-1.4299841572091610380064e+06_dp), &
                                                          XMM2r8_t(-3.1398660864247265862050e+05_dp), &
                                                          XMM2r8_t(-3.5377809694431133484800e+08_dp), &
                                                          XMM2r8_t(3.1984354235237738511048e+08_dp),  &
                                                          XMM2r8_t(-2.5301823984599019348858e+10_dp), &
                                                          XMM2r8_t(1.2177698136199594677580e+10_dp),  &
                                                          XMM2r8_t(-2.0829040666802497120940e+11_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_q  =[XMM2r8_t(7.6886718750000000000000e+01_dp),  &
                                                          XMM2r8_t(-5.5648470543369082846819e+03_dp), &
                                                          XMM2r8_t(1.9418469440759880361415e+05_dp),  &
                                                          XMM2r8_t(-4.2648434812177161405483e+06_dp), &
                                                          XMM2r8_t(6.4698830956576428587653e+07_dp),  &
                                                          XMM2r8_t(-7.0108568774215954065376e+08_dp), &
                                                          XMM2r8_t(5.4229617984472955011862e+09_dp),  &
                                                          XMM2r8_t(-2.8986272696554495342658e+10_dp), &
                                                          XMM2r8_t(9.8900934262481749439886e+10_dp),  &
                                                          XMM2r8_t(-8.9673749185755048616855e+10_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_r  =[XMM2r8_t(-2.645677793077147237806e+00_dp),  &
                                                          XMM2r8_t(-2.378372882815725244124e+00_dp),  &
                                                          XMM2r8_t(-2.421106956980653511550e+01_dp),  & 
                                                          XMM2r8_t(1.052976392459015155422e+01_dp),   &
                                                          XMM2r8_t(1.945603779539281810439e+01_dp),   &
                                                          XMM2r8_t(-3.015761863840593359165e+01_dp),  &
                                                          XMM2r8_t(1.120011024227297451523e+01_dp),   &
                                                          XMM2r8_t(-3.988850730390541057912e+00_dp),  &
                                                          XMM2r8_t(9.565134591978630774217e+00_dp),   & 
                                                          XMM2r8_t(9.981193787537396413219e-1_dp)]
      type(XMM2r8_t), dimension(0:8), save :: calcei_s  =[XMM2r8_t(1.598517957704779356479e-4_dp),    &
                                                          XMM2r8_t(4.644185932583286942650e+00_dp),   &
                                                          XMM2r8_t(3.697412299772985940785e+02_dp),   &
                                                          XMM2r8_t(-8.791401054875438925029e+00_dp),  &
                                                          XMM2r8_t(7.608194509086645763123e+02_dp),   &
                                                          XMM2r8_t(2.852397548119248700147e+01_dp),   &
                                                          XMM2r8_t(4.731097187816050252967e+02_dp),   &
                                                          XMM2r8_t(-2.369210235636181001661e+02_dp),  &
                                                          XMM2r8_t(1.249884822712447891440e+00_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_p1 =[XMM2r8_t(-1.647721172463463140042e+00_dp),  &
                                                          XMM2r8_t(-1.860092121726437582253e+01_dp),  &
                                                          XMM2r8_t(-1.000641913989284829961e+01_dp),  &
                                                          XMM2r8_t(-2.105740799548040450394e+01_dp),  &
                                                          XMM2r8_t(-9.13483569999874255243e-1_dp),    &
                                                          XMM2r8_t(-3.323612579343962284333e+01_dp),  &
                                                          XMM2r8_t(2.495487730402059440626e+01_dp),   &
                                                          XMM2r8_t(2.652575818452799819855e+01_dp),   &
                                                          XMM2r8_t(-1.845086232391278674524e+00_dp),  &
                                                          XMM2r8_t(9.999933106160568739091e-1_dp)]
      type(XMM2r8_t), dimension(0:8), save :: calcei_q1 =[XMM2r8_t(9.792403599217290296840e+01_dp),   &
                                                          XMM2r8_t(6.403800405352415551324e+01_dp),   &
                                                          XMM2r8_t(5.994932325667407355255e+01_dp),   &
                                                          XMM2r8_t(2.538819315630708031713e+02_dp),   &
                                                          XMM2r8_t(4.429413178337928401161e+01_dp),   &
                                                          XMM2r8_t(1.192832423968601006985e+03_dp),   &
                                                          XMM2r8_t(1.991004470817742470726e+02_dp),   &
                                                          XMM2r8_t(-1.093556195391091143924e+01_dp),  &
                                                          XMM2r8_t(1.001533852045342697818e+00_dp)]
      type(XMM2r8_t), dimension(0:9), save :: calcei_p2 =[XMM2r8_t(1.75338801265465972390e+02_dp),    &
                                                          XMM2r8_t(-2.23127670777632409550e+02_dp),   &
                                                          XMM2r8_t(-1.81949664929868906455e+01_dp),   &
                                                          XMM2r8_t(-2.79798528624305389340e+01_dp),   &
                                                          XMM2r8_t(-7.63147701620253630855e+00_dp),   &
                                                          XMM2r8_t(-1.52856623636929636839e+01_dp),   &
                                                          XMM2r8_t(-7.06810977895029358836e+00_dp),   &
                                                          XMM2r8_t(-5.00006640413131002475e+00_dp),   &
                                                          XMM2r8_t(-3.00000000320981265753e+00_dp),   &
                                                          XMM2r8_t(1.00000000000000485503e+00_dp)]
      type(XMM2r8_t), dimension(0:8), save :: calcei_q2 =[XMM2r8_t(3.97845977167414720840e+04_dp),    &
                                                          XMM2r8_t(3.97277109100414518365e+00_dp),    &
                                                          XMM2r8_t(1.37790390235747998793e+02_dp),    &
                                                          XMM2r8_t(1.17179220502086455287e+02_dp),    &
                                                          XMM2r8_t(7.04831847180424675988e+01_dp),    &
                                                          XMM2r8_t(-1.20187763547154743238e+01_dp),   &
                                                          XMM2r8_t(-7.99243595776339741065e+00_dp),   &
                                                          XMM2r8_t(-2.99999894040324959612e+00_dp),   &
                                                          XMM2r8_t(1.99999999999048104167e+00_dp)]
                                                       
                                                          
     !!
     !! calci0_xmm2r8 constant arrays (saved)
     !!
     !dir$ attributes align : 16 :: calci0_p
     !dir$ attributes align : 16 :: calci0_q
     !dir$ attributes align : 16 :: calci0_pp
     !dir$ attributes align : 16 :: calci0_qq
     type(XMM2r8_t), dimension(0:14), save :: calci0_p  =[XMM2r8_t(-5.2487866627945699800e-18_dp),        &
                                                          XMM2r8_t(-1.5982226675653184646e-14_dp),        &
                                                          XMM2r8_t(-2.6843448573468483278e-11_dp),        &
                                                          XMM2r8_t(-3.0517226450451067446e-08_dp),        &
                                                          XMM2r8_t(-2.5172644670688975051e-05_dp),        &
                                                          XMM2r8_t(-1.5453977791786851041e-02_dp),        &
                                                          XMM2r8_t(-7.0935347449210549190e+00_dp),        &
                                                          XMM2r8_t(-2.4125195876041896775e+03_dp),        &
                                                          XMM2r8_t(-5.9545626019847898221e+05_dp),        &
                                                          XMM2r8_t(-1.0313066708737980747e+08_dp),        &
                                                          XMM2r8_t(-1.1912746104985237192e+10_dp),        &
                                                          XMM2r8_t(-8.4925101247114157499e+11_dp),        &
                                                          XMM2r8_t(-3.2940087627407749166e+13_dp),        &
                                                          XMM2r8_t(-5.5050369673018427753e+14_dp),        &
                                                          XMM2r8_t(-2.2335582639474375249e+15_dp)]
      type(XMM2r8_t), dimension(0:4), save ::  calci0_q =[XMM2r8_t(3.7277560179962773046e+03_dp),         &
                                                          XMM2r8_t(6.5158506418655165707e+06_dp),         &
                                                          XMM2r8_t(-6.5626560740833869295e+09_dp),        &
                                                          XMM2r8_t(3.7604188704092954661e+12_dp),         &
                                                          XMM2r8_t(-9.7087946179594019126e+14_dp)]
      type(XMM2r8_t), dimension(0:7), save ::  calci0_pp=[XMM2r8_t(-3.9843750000000000000e-01_dp),         & 
                                                          XMM2r8_t(2.9205384596336793945e+00_dp),         &
                                                          XMM2r8_t(-2.4708469169133954315e+00_dp),        &
                                                          XMM2r8_t(4.7914889422856814203e-01_dp),         &
                                                          XMM2r8_t(-3.7384991926068969150e-03_dp),        &
                                                          XMM2r8_t(-2.6801520353328635310e-03_dp),        &
                                                          XMM2r8_t(9.9168777670983678974e-05_dp),         &
                                                          XMM2r8_t(-2.1877128189032726730e-06_dp)]
      type(XMM2r8_t), dimension(0:6), save ::  calci0_qq=[XMM2r8_t(-3.1446690275135491500e+01_dp),        & 
                                                          XMM2r8_t(8.5539563258012929600e+01_dp),         &
                                                          XMM2r8_t(-6.0228002066743340583e+01_dp),        &
                                                          XMM2r8_t(1.3982595353892851542e+01_dp),         &
                                                          XMM2r8_t(-1.1151759188741312645e+00_dp),        &
                                                          XMM2r8_t(3.2547697594819615062e-02_dp),         &
                                                          XMM2r8_t(-5.5194330231005480228e-04_dp)] 
    
    !!
    !! calci1_xmm2r8 constant arrays (saved)
    !!
     !dir$ attributes align : 16 :: calci1_p
     !dir$ attributes align : 16 :: calci1_q
     !dir$ attributes align : 16 :: calci1_pp
     !dir$ attributes align : 16 :: calci1_qq
     type(XMM2r8_t), dimension(0:14), save :: calci1_p = [XMM2r8_t(-1.9705291802535139930e-19_dp),           &
                                                          XMM2r8_t(-6.5245515583151902910e-16_dp),           &
                                                          XMM2r8_t(-1.1928788903603238754e-12_dp),           &
                                                          XMM2r8_t(-1.4831904935994647675e-09_dp),           &
                                                          XMM2r8_t(-1.3466829827635152875e-06_dp),           &
                                                          XMM2r8_t(-9.1746443287817501309e-04_dp),           &
                                                          XMM2r8_t(-4.7207090827310162436e-01_dp),           &
                                                          XMM2r8_t(-1.8225946631657315931e+02_dp),           &
                                                          XMM2r8_t(-5.1894091982308017540e+04_dp),           &
                                                          XMM2r8_t(-1.0588550724769347106e+07_dp),           &
                                                          XMM2r8_t(-1.4828267606612366099e+09_dp),           &
                                                          XMM2r8_t(-1.3357437682275493024e+11_dp),           &
                                                          XMM2r8_t(-6.9876779648010090070e+12_dp),           &
                                                          XMM2r8_t(-1.7732037840791591320e+14_dp),           &
                                                          XMM2r8_t(-1.4577180278143463643e+15_dp)]
     type(XMM2r8_t), dimension(0:4), save ::  calci1_q = [XMM2r8_t(-4.0076864679904189921e+03_dp),           &
                                                          XMM2r8_t(7.4810580356655069138e+06_dp),            &
                                                          XMM2r8_t(-8.0059518998619764991e+09_dp),           &
                                                          XMM2r8_t(4.8544714258273622913e+12_dp),            &
                                                          XMM2r8_t(-1.3218168307321442305e+15_dp)]
     type(XMM2r8_t), dimension(0:7), save ::  calci1_pp =[XMM2r8_t(-6.0437159056137600000e-02_dp),           &
                                                          XMM2r8_t(4.5748122901933459000e-01_dp),            &
                                                          XMM2r8_t(-4.2843766903304806403e-01_dp),           &
                                                          XMM2r8_t(9.7356000150886612134e-02_dp),            &
                                                          XMM2r8_t(-3.2457723974465568321e-03_dp),           &
                                                          XMM2r8_t(-3.6395264712121795296e-04_dp),           &
                                                          XMM2r8_t( 1.6258661867440836395e-05_dp),           &
                                                          XMM2r8_t(-3.6347578404608223492e-07_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  calci1_qq =[XMM2r8_t(-3.8806586721556593450e+00_dp),           &
                                                          XMM2r8_t(3.2593714889036996297e+00_dp),            &
                                                          XMM2r8_t(-8.5017476463217924408e-01_dp),           &
                                                          XMM2r8_t(7.4212010813186530069e-02_dp),            &
                                                          XMM2r8_t(-2.2835624489492512649e-03_dp),           &
                                                          XMM2r8_t(3.7510433111922824643e-05_dp)]
    !!
    !! calck0_xmm2r8 constant arrays (saved)
    !!
    !dir$ attributes align : 16 :: calck0_p
    !dir$ attributes align : 16 :: calck0_q
    !dir$ attributes align : 16 :: calck0_f
    !dir$ attributes align : 16 :: calck0_g
    !dir$ attributes align : 16 :: calck0_pp
    !dir$ attributes align : 16 :: calck0_qq
     type(XMM2r8_t), dimension(0:5), save ::  calck0_p = [XMM2r8_t(5.8599221412826100000e-04_dp),            & 
	                                                  XMM2r8_t(1.3166052564989571850e-01_dp),            &
                                                          XMM2r8_t(1.1999463724910714109e+01_dp),              & 
                                                          XMM2r8_t(4.6850901201934832188e+02_dp),              & 
                                                          XMM2r8_t(5.9169059852270512312e+03_dp),              &
                                                          XMM2r8_t(2.4708152720399552679e+03_dp)]
     type(XMM2r8_t), dimension(0:1), save ::  calck0_q = [XMM2r8_t(-2.4994418972832303646e+02_dp),           & 
	                                                  XMM2r8_t(2.1312714303849120380e+04_dp)] 
     type(XMM2r8_t), dimension(0:3), save ::  calck0_f = [XMM2r8_t(-1.6414452837299064100e+00_dp),           &
	                                                  XMM2r8_t(-2.9601657892958843866e+02_dp),           &
                                                          XMM2r8_t(-1.7733784684952985886e+04_dp),           &           
                                                          XMM2r8_t(-4.0320340761145482298e+05_dp)]
     type(XMM2r8_t), dimension(0:2), save ::  calck0_g = [XMM2r8_t(2.5064972445877992730e+02_dp),            &
	                                                  XMM2r8_t(2.9865713163054025489e+04_dp),            & 
                                                          XMM2r8_t(-1.6128136304458193998e+06_dp)]
     type(XMM2r8_t), dimension(0:9), save ::  calck0_pp= [XMM2r8_t(1.1394980557384778174e+02_dp),            & 
	                                                  XMM2r8_t(3.6832589957340267940e+03_dp),            & 
                                                          XMM2r8_t(3.1075408980684392399e+04_dp),            & 
                                                          XMM2r8_t(1.0577068948034021957e+05_dp),            & 
                                                          XMM2r8_t(1.7398867902565686251e+05_dp),            &
                                                          XMM2r8_t(1.5097646353289914539e+05_dp),            & 
                                                          XMM2r8_t(7.1557062783764037541e+04_dp),            & 
                                                          XMM2r8_t(1.8321525870183537725e+04_dp),            & 
                                                          XMM2r8_t(2.3444738764199315021e+03_dp),            & 
                                                          XMM2r8_t(1.1600249425076035558e+02_dp)]
     type(XMM2r8_t), dimension(0:9), save ::  calck0_qq= [XMM2r8_t(2.0013443064949242491e+02_dp),            & 
	                                                  XMM2r8_t(4.4329628889746408858e+03_dp),            & 
                                                          XMM2r8_t(3.1474655750295278825e+04_dp),            &
                                                          XMM2r8_t(9.7418829762268075784e+04_dp),            & 
                                                          XMM2r8_t(1.5144644673520157801e+05_dp),            & 
                                                          XMM2r8_t(1.2689839587977598727e+05_dp),            & 
                                                          XMM2r8_t(5.8824616785857027752e+04_dp),            & 
                                                          XMM2r8_t(1.4847228371802360957e+04_dp),            & 
                                                          XMM2r8_t(1.8821890840982713696e+03_dp),            & 
                                                          XMM2r8_t(9.2556599177304839811e+01_dp)]
   
     
    !!
    !! caljy1_xmm2r8 constant arrays (saved)
    !!  
     !dir$ attributes align : 16 :: caljy1_plg
     !dir$ attributes align : 16 :: caljy1_qlg
     !dir$ attributes align : 16 :: caljy1_pj0
     !dir$ attributes align : 16 :: caljy1_qj0
     !dir$ attributes align : 16 :: caljy1_pj1
     !dir$ attributes align : 16 :: caljy1_qj1
     !dir$ attributes align : 16 :: caljy1_py0
     !dir$ attributes align : 16 :: caljy1_qy0
     !dir$ attributes align : 16 :: caljy1_py1
     !dir$ attributes align : 16 :: caljy1_qy1
     !dir$ attributes align : 16 :: caljy1_p0
     !dir$ attributes align : 16 :: caljy1_q0
     !dir$ attributes align : 16 :: caljy1_p1
     !dir$ attributes align : 16 :: caljy1_q1
     type(XMM2r8_t), dimension(0:3), save ::  caljy1_plg =[XMM2r8_t(-2.4562334077563243311e+1_dp), &
	                                                   XMM2r8_t(2.3642701335621505212e+2_dp),  &
                                                           XMM2r8_t(-5.4989956895857911039e+2_dp), &
                                                           XMM2r8_t(3.5687548468071500413e+2_dp)]
     type(XMM2r8_t), dimension(0:3), save ::  caljy1_qlg =[XMM2r8_t(-3.5553900764052419184e+1_dp), &
	                                                   XMM2r8_t(1.9400230218539473193e+2_dp),  &
                                                           XMM2r8_t(-3.3442903192607538956e+2_dp), &
                                                           XMM2r8_t(1.7843774234035750207e+2_dp)]
     type(XMM2r8_t), dimension(0:6), save ::  caljy1_pj0 =[XMM2r8_t(9.8062904098958257677e+5_dp),  &
	                                                   XMM2r8_t(-1.1548696764841276794e+8_dp), & 
                                                           XMM2r8_t(6.6781041261492395835e+9_dp),  &
                                                           XMM2r8_t(-1.4258509801366645672e+11_dp),& 
                                                           XMM2r8_t(-4.4615792982775076130e+3_dp), & 
                                                           XMM2r8_t(1.0650724020080236441e+1_dp),  &
                                                           XMM2r8_t(-1.0767857011487300348e-2_dp)]
     type(XMM2r8_t), dimension(0:4), save ::  caljy1_qj0 =[XMM2r8_t(5.9117614494174794095e+5_dp),  & 
	                                                   XMM2r8_t(2.0228375140097033958e+8_dp),  & 
                                                           XMM2r8_t(4.2091902282580133541e+10_dp), & 
                                                           XMM2r8_t(4.1868604460820175290e+12_dp), & 
                                                           XMM2r8_t(1.0742272239517380498e+03_dp)]
     type(XMM2r8_t), dimension(0:7), save ::  caljy1_pj1 =[XMM2r8_t(4.6179191852758252280e+00_dp), &
	                                                   XMM2r8_t(-7.1329006872560947377e+3_dp), &
                                                           XMM2r8_t(4.5039658105749078904e+6_dp),  &
                                                           XMM2r8_t(-1.4437717718363239107e+9_dp), &
                                                           XMM2r8_t(2.3569285397217157313e+11_dp), &
                                                           XMM2r8_t(-1.6324168293282543629e+13_dp),&
                                                           XMM2r8_t(1.1357022719979468624e+14_dp), & 
                                                           XMM2r8_t(1.0051899717115285432e+15_dp)]
     type(XMM2r8_t), dimension(0:6), save ::  caljy1_qj1 =[XMM2r8_t(1.1267125065029138050e+6_dp),  & 
	                                                   XMM2r8_t(6.4872502899596389593e+8_dp),  &
                                                           XMM2r8_t(2.7622777286244082666e+11_dp), & 
                                                           XMM2r8_t(8.4899346165481429307e+13_dp), &
                                                           XMM2r8_t(1.7128800897135812012e+16_dp), & 
                                                           XMM2r8_t(1.7253905888447681194e+18_dp), & 
                                                           XMM2r8_t(1.3886978985861357615e+3_dp)]
     type(XMM2r8_t), dimension(0:6), save ::  caljy1_py0 =[XMM2r8_t(2.2157953222280260820e+5_dp),  &
	                                                   XMM2r8_t(-5.9157479997408395984e+7_dp), & 
                                                           XMM2r8_t(7.2144548214502560419e+9_dp),  &
                                                           XMM2r8_t(-3.7595974497819597599e+11_dp),&
                                                           XMM2r8_t(5.4708611716525426053e+12_dp), & 
                                                           XMM2r8_t(4.0535726612579544093e+13_dp), & 
                                                           XMM2r8_t(-3.1714424660046133456e+2_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  caljy1_qy0 =[XMM2r8_t(8.2079908168393867438e+2_dp),  & 
	                                                   XMM2r8_t(3.8136470753052572164e+5_dp),  &
                                                           XMM2r8_t(1.2250435122182963220e+8_dp),  & 
                                                           XMM2r8_t(2.7800352738690585613e+10_dp), &
                                                           XMM2r8_t(4.1272286200406461981e+12_dp), & 
                                                           XMM2r8_t(3.0737873921079286084e+14_dp)]
     type(XMM2r8_t), dimension(0:8), save ::  caljy1_py1 =[XMM2r8_t(1.9153806858264202986e+6_dp),  &
	                                                   XMM2r8_t(-1.1957961912070617006e+9_dp), & 
                                                           XMM2r8_t(3.7453673962438488783e+11_dp), &
                                                           XMM2r8_t(-5.9530713129741981618e+13_dp),& 
                                                           XMM2r8_t(4.0686275289804744814e+15_dp), &
                                                           XMM2r8_t(-2.3638408497043134724e+16_dp),&
                                                           XMM2r8_t(-5.6808094574724204577e+18_dp),& 
                                                           XMM2r8_t(1.1514276357909013326e+19_dp), & 
                                                           XMM2r8_t(-1.2337180442012953128e+3_dp)]
     type(XMM2r8_t), dimension(0:7), save ::  caljy1_qy1 =[XMM2r8_t(1.2855164849321609336e+3_dp),  & 
	                                                   XMM2r8_t(1.0453748201934079734e+6_dp),  & 
                                                           XMM2r8_t(6.3550318087088919566e+8_dp),  & 
                                                           XMM2r8_t(3.0221766852960403645e+11_dp), & 
                                                           XMM2r8_t(1.1187010065856971027e+14_dp), & 
                                                           XMM2r8_t(3.0837179548112881950e+16_dp), &
                                                           XMM2r8_t(5.6968198822857178911e+18_dp), & 
                                                           XMM2r8_t(5.3321844313316185697e+20_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  caljy1_p0  =[XMM2r8_t(1.0982405543459346727e+5_dp),  &
	                                                   XMM2r8_t(-1.5235293511811373833e+6_dp), &
                                                           XMM2r8_t(-6.6033732483649391093e+06_dp),&
                                                           XMM2r8_t(-9.9422465050776411957e+6_dp), &
                                                           XMM2r8_t(-4.4357578167941278571e+6_dp), &
                                                           XMM2r8_t(-1.6116166443246101165e+3_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  caljy1_q0  =[XMM2r8_t(-1.0726385991103820119e+5_dp), &
	                                                   XMM2r8_t(-1.5118095066341608816e+6_dp), &
                                                           XMM2r8_t(-6.5853394797230870728e+6_dp), &
                                                           XMM2r8_t(-9.9341243899345856590e+6_dp), & 
                                                           XMM2r8_t(-4.4357578167941278568e+6_dp), &
                                                           XMM2r8_t(-1.4550094401904961825e+3_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  caljy1_p1  =[XMM2r8_t(1.7063754290207680021e+3_dp),  & 
	                                                   XMM2r8_t(1.8494262873223866797e+4_dp),  & 
                                                           XMM2r8_t(6.6178836581270835179e+4_dp),  & 
                                                           XMM2r8_t(8.5145160675335701966e+4_dp),  &
                                                           XMM2r8_t(3.3220913409857223519e+4_dp),  & 
                                                           XMM2r8_t(3.5265133846636032186e+1_dp)]
     type(XMM2r8_t), dimension(0:5), save ::  caljy1_q1  =[XMM2r8_t(3.7890229745772202641e+4_dp),  & 
	                                                   XMM2r8_t(4.0029443582266975117e+5_dp),  &
                                                           XMM2r8_t(1.4194606696037208929e+6_dp),  & 
                                                           XMM2r8_t(1.8194580422439972989e+6_dp),  &
                                                           XMM2r8_t(7.0871281941028743574e+5_dp),  & 
                                                           XMM2r8_t(8.6383677696049909675e+2_dp)]
     
    
    
      
     contains
     



       
       


          



 

         

     
        

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
  

#if 0

            function besek0_xmm2r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes forceinline :: besek0_xmm2r8
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t), automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 2
              call calck0_xmm2r8(x,t0,jint) 
              val = t0 
          end function besek0_xmm2r8    
          
#endif 
     

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
	             

#if 0

           function besek1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
             
              !dir$ attributes forceinline :: besek1_xmm2r8
              
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t),   automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 2
              call calck1_xmm2r8(x,t0,jint) 
              val = t0 
          end function besek1_xmm2r8   

#endif          
         

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




           function besi0_xmm2r8(x) result(val)
               
              !dir$ optimize:3
            
              !dir$ attributes forceinline :: besi0_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t), automatic :: t0 
              call calci0_xmm2r8(x,t0) 
              val = t0 
          end function besi0_xmm2r8  
          
          


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

 


            function besi1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
            
              !dir$ attributes forceinline :: besi1_xmm2r8
              
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t),   automatic :: t0 
              call calci1_xmm2r8(x,t0) 
              val = t0 
          end function besi1_xmm2r8   
          
          


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

   
#if 0

          function besj0_xmm2r8(x) result(val)
               
              !dir$ optimize:3
             
              !dir$ attributes forceinline :: besj0_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t),   automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 0
              call calcjy0_xmm2r8(x,t0,jint) 
              val = t0 
          end function besj0_xmm2r8   
          
#endif           

 
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


#if 0   
           function besj1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
             
              !dir$ attributes forceinline :: besj1_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
               type(XMM2r8_t), automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 0
              call calcjy1_xmm2r8(x,t0,jint) 
              val = t0 
          end function besj1_xmm2r8   
          
#endif           

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



          function besk0_xmm2r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besk0_xmm2r8
              
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t),   automatic :: t0 
              call calck0_xmm2r8(x,t0)
              val = t0  
          end function besk0_xmm2r8   
          
          

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



          function besk1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besk1_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t), automatic :: t0 
              call calck1_xmm2r8(x,t0) 
              val = t0 
          end function besk1_xmm2r8   
          
         

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



           function besy0_xmm2r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besy0_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t), automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 1
              call caljy0_xmm2r8(x,t0,jint) 
              val = t0 
          end function besy0_xmm2r8   
          
          

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


#if 0

           function besy1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
             
              !dir$ attributes forceinline :: besy1_xmm2r8
             
              type(XMM2r8_t),   intent(in) :: x
              type(XMM2r8_t)  :: val
              type(XMM2r8_t), automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 1
              call caly1_xmm2r8(x,t0,jint) 
              val = t0 
          end function besy1_xmm2r8 

#endif   

!! /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !!


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



        subroutine calci0_xmm2r8(arg,val)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci0_xmm2r8
              !dir$ attributes forceinline :: calci0_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_xmm2r8  
               type(XMM2r8_t),   intent(in)   :: arg
               type(XMM2r8_t),   intent(out)  :: val
              
               !dir$ attributes align : 16 :: one
               !dir$ attributes align : 16 :: one5
               !dir$ attributes align : 16 :: exp40
               !dir$ attributes align : 16 :: frty
               !dir$ attributes align : 16 :: rec15
               !dir$ attributes align : 16 :: two25
               !dir$ attributes align : 16 :: xsmall
               !dir$ attributes align : 16 :: xinf
               !dir$ attributes align : 16 :: xmax
               type(XMM2r8_t),   parameter    :: one   = XMM2r8_t([1.0e+0_dp,1.0e+0_dp])
               type(XMM2r8_t),   parameter    :: one5  = XMM2r8_t([15.0e+0_dp,15.0e+0_dp])
               type(XMM2r8_t),   parameter    :: exp40 = XMM2r8_t([2.353852668370199854e+17_dp,2.353852668370199854e+17_dp])
               type(XMM2r8_t),   parameter    :: frty  = XMM2r8_t([40.0e+0_dp,40.0e+0_dp])
               type(XMM2r8_t),   parameter    :: rec15 = XMM2r8_t([6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp])
               type(XMM2r8_t),   parameter    :: two25 = XMM2r8_t([225.0e+0_dp,225.0e+0_dp])
               type(XMM2r8_t),   parameter    :: xsmall= XMM2r8_t([5.55e-17_dp,5.55e-17_dp])
               type(XMM2r8_t),   parameter    :: xinf  = XMM2r8_t([1.79e+308_dp,1.79e+308_dp])
               type(XMM2r8_t),   parameter    :: xmax  = XMM2r8_t([713.986e+0_dp,713.986e+0_dp])
               type(XMM2r8_t),   parameter    :: p1    = XMM2r8_t([-5.2487866627945699800e-18_dp,-5.2487866627945699800e-18_dp])
               type(XMM2r8_t),   parameter    :: p2    = XMM2r8_t([-1.5982226675653184646e-14_dp,-1.5982226675653184646e-14_dp])
               type(XMM2r8_t),   parameter    :: p3    = XMM2r8_t([-2.6843448573468483278e-11_dp,-2.6843448573468483278e-11_dp])
               type(XMM2r8_t),   parameter    :: p4    = XMM2r8_t([-3.0517226450451067446e-08_dp,-3.0517226450451067446e-08_dp])
               type(XMM2r8_t),   parameter    :: p5    = XMM2r8_t([-2.5172644670688975051e-05_dp,-2.5172644670688975051e-05_dp])
               type(XMM2r8_t),   parameter    :: p6    = XMM2r8_t([-1.5453977791786851041e-02_dp,-1.5453977791786851041e-02_dp])
               type(XMM2r8_t),   parameter    :: p7    = XMM2r8_t([-7.0935347449210549190e+00_dp,-7.0935347449210549190e+00_dp])
               type(XMM2r8_t),   parameter    :: p8    = XMM2r8_t([-2.4125195876041896775e+03_dp,-2.4125195876041896775e+03_dp])
               type(XMM2r8_t),   parameter    :: p9    = XMM2r8_t([-5.9545626019847898221e+05_dp,-5.9545626019847898221e+05_dp])
               type(XMM2r8_t),   parameter    :: p10   = XMM2r8_t([-1.0313066708737980747e+08_dp,-1.0313066708737980747e+08_dp])
               type(XMM2r8_t),   parameter    :: p11   = XMM2r8_t([-1.1912746104985237192e+10_dp,-1.1912746104985237192e+10_dp])
               type(XMM2r8_t),   parameter    :: p12   = XMM2r8_t([-8.4925101247114157499e+11_dp,-8.4925101247114157499e+11_dp])
               type(XMM2r8_t),   parameter    :: p13   = XMM2r8_t([-3.2940087627407749166e+13_dp,-3.2940087627407749166e+13_dp])
               type(XMM2r8_t),   parameter    :: p14   = XMM2r8_t([-5.5050369673018427753e+14_dp,-5.5050369673018427753e+14_dp])
               type(XMM2r8_t),   parameter    :: p15   = XMM2r8_t([-2.2335582639474375249e+15_dp,-2.2335582639474375249e+15_dp])
               type(XMM2r8_t),   parameter    :: q1    = XMM2r8_t([-3.7277560179962773046e+03_dp,-3.7277560179962773046e+03_dp])        
               type(XMM2r8_t),   parameter    :: q2    = XMM2r8_t([6.5158506418655165707e+06_dp,6.5158506418655165707e+06_dp])         
               type(XMM2r8_t),   parameter    :: q3    = XMM2r8_t([-6.5626560740833869295e+09_dp,-6.5626560740833869295e+09_dp])        
               type(XMM2r8_t),   parameter    :: q4    = XMM2r8_t([3.7604188704092954661e+12_dp,3.7604188704092954661e+12_dp])         
               type(XMM2r8_t),   parameter    :: q5    = XMM2r8_t([-9.7087946179594019126e+14_dp,-9.7087946179594019126e+14_dp])
               type(XMM2r8_t),   parameter    :: pp1   = XMM2r8_t([-3.9843750000000000000e-01_dp,-3.9843750000000000000e-01_dp])         
               type(XMM2r8_t),   parameter    :: pp2   = XMM2r8_t([2.9205384596336793945e+00_dp,2.9205384596336793945e+00_dp])         
               type(XMM2r8_t),   parameter    :: pp3   = XMM2r8_t([-2.4708469169133954315e+00_dp,-2.4708469169133954315e+00_dp])        
               type(XMM2r8_t),   parameter    :: pp4   = XMM2r8_t([4.7914889422856814203e-01_dp,4.7914889422856814203e-01_dp])         
               type(XMM2r8_t),   parameter    :: pp5   = XMM2r8_t([-3.7384991926068969150e-03_dp,-3.7384991926068969150e-03_dp])        
               type(XMM2r8_t),   parameter    :: pp6   = XMM2r8_t([-2.6801520353328635310e-03_dp,-2.6801520353328635310e-03_dp])        
               type(XMM2r8_t),   parameter    :: pp7   = XMM2r8_t([9.9168777670983678974e-05_dp,9.9168777670983678974e-05_dp])        
               type(XMM2r8_t),   parameter    :: pp8   = XMM2r8_t([-2.1877128189032726730e-06_dp,-2.1877128189032726730e-06_dp])
               type(XMM2r8_t),   parameter    :: qq1   = XMM2r8_t([-3.1446690275135491500e+01_dp,-3.1446690275135491500e+01_dp])        
               type(XMM2r8_t),   parameter    :: qq2   = XMM2r8_t([8.5539563258012929600e+01_dp,8.5539563258012929600e+01_dp])         
               type(XMM2r8_t),   parameter    :: qq3   = XMM2r8_t([-6.0228002066743340583e+01_dp,-6.0228002066743340583e+01_dp])        
               type(XMM2r8_t),   parameter    :: qq4   = XMM2r8_t([1.3982595353892851542e+01_dp,1.3982595353892851542e+01_dp])         
               type(XMM2r8_t),   parameter    :: qq5   = XMM2r8_t([-1.1151759188741312645e+00_dp,-1.1151759188741312645e+00_dp])        
               type(XMM2r8_t),   parameter    :: qq6   = XMM2r8_t([3.2547697594819615062e-02_dp,3.2547697594819615062e-02_dp])         
               type(XMM2r8_t),   parameter    :: qq7   = XMM2r8_t([-5.5194330231005480228e-04_dp,-5.5194330231005480228e-04_dp])
               !dir$ attributes align : 16 :: a
               !dir$ attributes align : 16 :: b
               !dir$ attributes align : 16 :: sump
               !dir$ attributes align : 16 :: sumq
               !dir$ attributes align : 16 :: x
               !dir$ attributes align : 16 :: xx
               !dir$ attributes align : 16 :: t0
               !dir$ attributes align : 16 :: t1
               !dir$ attributes align : 16 :: p1 
               !dir$ attributes align : 16 :: p2 
               !dir$ attributes align : 16 :: p3 
               !dir$ attributes align : 16 :: p4 
               !dir$ attributes align : 16 :: p5 
               !dir$ attributes align : 16 :: p6 
               !dir$ attributes align : 16 :: p7 
               !dir$ attributes align : 16 :: p8 
               !dir$ attributes align : 16 :: p9 
               !dir$ attributes align : 16 :: p10 
               !dir$ attributes align : 16 :: p11 
               !dir$ attributes align : 16 :: p12 
               !dir$ attributes align : 16 :: p13 
               !dir$ attributes align : 16 :: p14 
               !dir$ attributes align : 16 :: p15
               !dir$ attributes align : 16 :: q1 
               !dir$ attributes align : 16 :: q2 
               !dir$ attributes align : 16 :: q3 
               !dir$ attributes align : 16 :: q4 
               !dir$ attributes align : 16 :: q5 
               !dir$ attributes align : 16 :: pp1 
               !dir$ attributes align : 16 :: pp2 
               !dir$ attributes align : 16 :: pp3 
               !dir$ attributes align : 16 :: pp5 
               !dir$ attributes align : 16 :: pp6 
               !dir$ attributes align : 16 :: pp7 
               !dir$ attributes align : 16 :: pp8
               !dir$ attributes align : 16 :: qq1 
               !dir$ attributes align : 16 :: qq2 
               !dir$ attributes align : 16 :: qq3 
               !dir$ attributes align : 16 :: qq4 
               !dir$ attributes align : 16 :: qq5 
               !dir$ attributes align : 16 :: qq6 
               !dir$ attributes align : 16 :: qq7
               type(XMM2r8_t),   automatic    :: a,b
               type(XMM2r8_t),   automatic    :: sump,sumq
               type(XMM2r8_t),   automatic    :: x,xx
               type(XMM2r8_t),   automatic    :: t0,t1
               
               type(Mask2_t),    automatic    :: msk1,msk2
               type(Mask2_t),    automatic    :: msk3,msk4
                                
               x.v    = abs(arg.v)
               msk1.m = (x.v<xsmall.v)
               msk2.m = (x.v<one5.v)
               msk3.m = (one5.v<=x.v)
               !if(any(msk1.m)) then
               where(msk1.m)
             
                  val.v = one.v
               else where(msk2.m) 
           
                  xx.v   = x.v*x.v
                  sump.v = p1.v
                  sump.v = sump.v*xx.v+p2.v
                  sump.v = sump.v*xx.v+p3.v
                  sump.v = sump.v*xx.v+p4.v
                  sump.v = sump.v*xx.v+p5.v
                  sump.v = sump.v*xx.v+p6.v
                  sump.v = sump.v*xx.v+p7.v
                  sump.v = sump.v*xx.v+p8.v
                  sump.v = sump.v*xx.v+p9.v
                  sump.v = sump.v*xx.v+p10.v
                  sump.v = sump.v*xx.v+p11.v
                  sump.v = sump.v*xx.v+p12.v
                  sump.v = sump.v*xx.v+p13.v
                  sump.v = sump.v*xx.v+p14.v
                  sump.v = sump.v*xx.v+p15.v
 
                  xx.v   = xx.v-two25.v
                  sumq.v = (((( &
                             xx.v+q1.v) &
                           * xx.v+q2.v) &
                           * xx.v+q3.v) &
                           * xx.v+q4.v) &
                           * xx.v+q5.v 
                  val.v  = sump.v/sumq.v
                  
               else where(msk3.m) 
              
                     msk4.m = (xmax.v<=x.v)
                     where(msk4.m)
                         val.v = xinf.v
                     else where 
                         xx.v    = one.v/x.v-rec15.v
                         sump.v  = ((((((   &
                                      pp1.v      &
                                   * xx.v+pp2.v)   &
                                   * xx.v+pp3.v)   &
                                   * xx.v+pp4.v)   &
                                   * xx.v+pp5.v)   &
                                   * xx.v+pp6.v)   &
                                   * xx.v+pp7.v)   &
                                   * xx.v+pp8.v
                         sumq.v  = ((((((    &
                                     xx.v+qq1.v) &
                                   * xx.v+qq2.v) &
                                   * xx.v+qq3.v) &
                                   * xx.v+qq4.v) &
                                   * xx.v+qq5.v) &
                                   * xx.v+qq6.v) &
                                   * xx.v+qq7.v
                        val.v    = sump.v/sumq.v
                        
                        
                           msk4.m = (x.v<=(xmax.v-one5.v))
                           where(msk4.m) 
                              a.v = exp(x.v)
                              b.v = one.v
                           else where 
                              a.v = exp(x.v-frty.v)
                              b.v = exp40.v
                            end where 
                            t0.v  = pp1.v*a.v
                            t1.v  = sqrt(x.v)
                            val.v = ((val.v*a.v-t0.v)/t1.v)*b.v
                    end where 
              end where 
               
        end subroutine calci0_xmm2r8
        
        


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



        subroutine calci1_xmm2r8(arg,val)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci1_xmm2r8
              !dir$ attributes forceinline :: calci1_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_xmm2r8  
              type(XMM2r8_t),   intent(in)   :: arg
              type(XMM2r8_t),   intent(out)  :: val
              
              !dir$ attributes align : 16 :: one
              !dir$ attributes align : 16 :: one5
              !dir$ attributes align : 16 :: exp40
              !dir$ attributes align : 16 :: frty
              !dir$ attributes align : 16 :: rec15
              !dir$ attributes align : 16 :: two25
              !dir$ attributes align : 16 :: xsmall
              !dir$ attributes align : 16 :: xinf
              !dir$ attributes align : 16 :: xmax
              !dir$ attributes align : 16 :: half
              !dir$ attributes align : 16 :: zero
             
              type(XMM2r8_t),   parameter    :: one   = XMM2r8_t([1.0e+0_dp,1.0e+0_dp])
              type(XMM2r8_t),   parameter    :: one5  = XMM2r8_t([15.0e+0_dp,15.0e+0_dp])
              type(XMM2r8_t),   parameter    :: exp40 = XMM2r8_t([2.353852668370199854e+17_dp,2.353852668370199854e+17_dp])
              type(XMM2r8_t),   parameter    :: frty  = XMM2r8_t([40.0e+0_dp,40.0e+0_dp])
              type(XMM2r8_t),   parameter    :: rec15 = XMM2r8_t([6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp])
              type(XMM2r8_t),   parameter    :: two25 = XMM2r8_t([225.0e+0_dp,225.0e+0_dp])
              type(XMM2r8_t),   parameter    :: xsmall= XMM2r8_t([5.55e-17_dp,5.55e-17_dp])
              type(XMM2r8_t),   parameter    :: xinf  = XMM2r8_t([1.79e+308_dp,1.79e+308_dp])
              type(XMM2r8_t),   parameter    :: xmax  = XMM2r8_t([713.986e+0_dp,713.986e+0_dp])
              type(XMM2r8_t),   parameter    :: half  = XMM2r8_t([0.5e+00_dp,0.5e+00_dp])
              type(XMM2r8_t),   parameter    :: zero  = XMM2r8_t([0.0e+00_dp,0.0e+00_dp])
              type(XMM2r8_t),   parameter    :: pbar  = XMM2r8_t([3.98437500e-01_dp,3.98437500e-01_dp])
              type(XMM2r8_t),   parameter    :: p1    = XMM2r8_t([-1.9705291802535139930e-19_dp,-1.9705291802535139930e-19_dp])           
              type(XMM2r8_t),   parameter    :: p2    = XMM2r8_t([-6.5245515583151902910e-16_dp,-6.5245515583151902910e-16_dp])           
              type(XMM2r8_t),   parameter    :: p3    = XMM2r8_t([-1.1928788903603238754e-12_dp,-1.1928788903603238754e-12_dp])           
              type(XMM2r8_t),   parameter    :: p4    = XMM2r8_t([-1.4831904935994647675e-09_dp,-1.4831904935994647675e-09_dp])           
              type(XMM2r8_t),   parameter    :: p5    = XMM2r8_t([-1.3466829827635152875e-06_dp,-1.3466829827635152875e-06_dp])           
              type(XMM2r8_t),   parameter    :: p6    = XMM2r8_t([-9.1746443287817501309e-04_dp,-9.1746443287817501309e-04_dp])           
              type(XMM2r8_t),   parameter    :: p7    = XMM2r8_t([-4.7207090827310162436e-01_dp,-4.7207090827310162436e-01_dp])           
              type(XMM2r8_t),   parameter    :: p8    = XMM2r8_t([-1.8225946631657315931e+02_dp,-1.8225946631657315931e+02_dp])           
              type(XMM2r8_t),   parameter    :: p9    = XMM2r8_t([-5.1894091982308017540e+04_dp,-5.1894091982308017540e+04_dp])           
              type(XMM2r8_t),   parameter    :: p10   = XMM2r8_t([-1.0588550724769347106e+07_dp,-1.0588550724769347106e+07_dp])           
              type(XMM2r8_t),   parameter    :: p11   = XMM2r8_t([-1.4828267606612366099e+09_dp,-1.4828267606612366099e+09_dp])           
              type(XMM2r8_t),   parameter    :: p12   = XMM2r8_t([-1.3357437682275493024e+11_dp,-1.3357437682275493024e+11_dp])           
              type(XMM2r8_t),   parameter    :: p13   = XMM2r8_t([-6.9876779648010090070e+12_dp,-6.9876779648010090070e+12_dp])           
              type(XMM2r8_t),   parameter    :: p14   = XMM2r8_t([-1.7732037840791591320e+14_dp,-1.7732037840791591320e+14_dp])           
              type(XMM2r8_t),   parameter    :: p15   = XMM2r8_t([-1.4577180278143463643e+15_dp,-1.4577180278143463643e+15_dp])
              type(XMM2r8_t),   parameter    :: q1    = XMM2r8_t([-4.0076864679904189921e+03_dp,-4.0076864679904189921e+03_dp])           
              type(XMM2r8_t),   parameter    :: q2    = XMM2r8_t([7.4810580356655069138e+06_dp,7.4810580356655069138e+06_dp])            
              type(XMM2r8_t),   parameter    :: q3    = XMM2r8_t([-8.0059518998619764991e+09_dp,-8.0059518998619764991e+09_dp])           
              type(XMM2r8_t),   parameter    :: q4    = XMM2r8_t([4.8544714258273622913e+12_dp,4.8544714258273622913e+12_dp])            
              type(XMM2r8_t),   parameter    :: q5    = XMM2r8_t([-1.3218168307321442305e+15_dp,-1.3218168307321442305e+15_dp])
              type(XMM2r8_t),   parameter    :: pp1   = XMM2r8_t([-6.0437159056137600000e-02_dp,-6.0437159056137600000e-02_dp])           
              type(XMM2r8_t),   parameter    :: pp2   = XMM2r8_t([4.5748122901933459000e-01_dp,4.5748122901933459000e-01_dp])            
              type(XMM2r8_t),   parameter    :: pp3   = XMM2r8_t([-4.2843766903304806403e-01_dp,-4.2843766903304806403e-01_dp])           
              type(XMM2r8_t),   parameter    :: pp4   = XMM2r8_t([9.7356000150886612134e-02_dp,9.7356000150886612134e-02_dp])            
              type(XMM2r8_t),   parameter    :: pp5   = XMM2r8_t([-3.2457723974465568321e-03_dp,-3.2457723974465568321e-03_dp])           
              type(XMM2r8_t),   parameter    :: pp6   = XMM2r8_t([-3.6395264712121795296e-04_dp,-3.6395264712121795296e-04_dp])           
              type(XMM2r8_t),   parameter    :: pp7   = XMM2r8_t([1.6258661867440836395e-05_dp,1.6258661867440836395e-05_dp])           
              type(XMM2r8_t),   parameter    :: pp8   = XMM2r8_t([-3.6347578404608223492e-07_dp,-3.6347578404608223492e-07_dp])
              type(XMM2r8_t),   parameter    :: qq1   = XMM2r8_t([-3.8806586721556593450e+00_dp,-3.8806586721556593450e+00_dp])           
              type(XMM2r8_t),   parameter    :: qq2   = XMM2r8_t([3.2593714889036996297e+00_dp,3.2593714889036996297e+00_dp])            
              type(XMM2r8_t),   parameter    :: qq3   = XMM2r8_t([-8.5017476463217924408e-01_dp,-8.5017476463217924408e-01_dp])           
              type(XMM2r8_t),   parameter    :: qq4   = XMM2r8_t([7.4212010813186530069e-02_dp,7.4212010813186530069e-02_dp])            
              type(XMM2r8_t),   parameter    :: qq5   = XMM2r8_t([-2.2835624489492512649e-03_dp,-2.2835624489492512649e-03_dp])           
              type(XMM2r8_t),   parameter    :: qq6   = XMM2r8_t([3.7510433111922824643e-05_dp,3.7510433111922824643e-05_dp])
              !dir$ attributes align : 16 :: sump
              !dir$ attributes align : 16 :: sumq
              !dir$ attributes align : 16 :: x
              !dir$ attributes align : 16 :: a
              !dir$ attributes align : 16 :: b
              !dir$ attributes align : 16 :: t0
              !dir$ attributes align : 16 :: xx 
              !dir$ attributes align : 16 :: pbar 
              !dir$ attributes align : 16 :: p1
              !dir$ attributes align : 16 :: p2
              !dir$ attributes align : 16 :: p3
              !dir$ attributes align : 16 :: p4
              !dir$ attributes align : 16 :: p5
              !dir$ attributes align : 16 :: p6
              !dir$ attributes align : 16 :: p7
              !dir$ attributes align : 16 :: p8
              !dir$ attributes align : 16 :: p9
              !dir$ attributes align : 16 :: p10
              !dir$ attributes align : 16 :: p11
              !dir$ attributes align : 16 :: p12
              !dir$ attributes align : 16 :: p13
              !dir$ attributes align : 16 :: p14
              !dir$ attributes align : 16 :: p15
              !dir$ attributes align : 16 :: q1
              !dir$ attributes align : 16 :: q2
              !dir$ attributes align : 16 :: q3
              !dir$ attributes align : 16 :: q4
              !dir$ attributes align : 16 :: q5
              !dir$ attributes align : 16 :: pp1
              !dir$ attributes align : 16 :: pp2
              !dir$ attributes align : 16 :: pp3
              !dir$ attributes align : 16 :: pp4
              !dir$ attributes align : 16 :: pp5
              !dir$ attributes align : 16 :: pp6
              !dir$ attributes align : 16 :: pp7
              !dir$ attributes align : 16 :: pp8
              !dir$ attributes align : 16 :: qq1
              !dir$ attributes align : 16 :: qq2
              !dir$ attributes align : 16 :: qq3
              !dir$ attributes align : 16 :: qq4
              !dir$ attributes align : 16 :: qq5
              !dir$ attributes align : 16 :: qq6
              type(XMM2r8_t),   automatic    :: sump,sumq
              type(XMM2r8_t),   automatic    :: x,a
              type(XMM2r8_t),   automatic    :: b,t0
              type(XMM2r8_t),   automatic    :: xx
              type(Mask2_t),    automatic    :: msk1,msk2
              type(Mask2_t),    automatic    :: msk3,msk4
              type(Mask2_t),    automatic    :: msk5,mge15

              x.v    = abs(arg.v)

              msk1.m  = (x.v<xsmall.v)
              msk2.m  = (x.v<one5.v)
              mge15.m = (x.v>=one5.v)
              msk3.m  = (xmax.v<x.v)
              where(msk1.m) 
                 val.v = half.v*x.v
              else where(msk2.m)
                  xx.v  = x.v*x.v
                  sump.v= p1.v
                  sump.v= sump.v*xx.v+p2.v
                  sump.v= sump.v*xx.v+p3.v
                  sump.v= sump.v*xx.v+p4.v
                  sump.v= sump.v*xx.v+p5.v
                  sump.v= sump.v*xx.v+p6.v
                  sump.v= sump.v*xx.v+p7.v
                  sump.v= sump.v*xx.v+p8.v
                  sump.v= sump.v*xx.v+p9.v
                  sump.v= sump.v*xx.v+p10.v
                  sump.v= sump.v*xx.v+p11.v
                  sump.v= sump.v*xx.v+p12.v
                  sump.v= sump.v*xx.v+p13.v
                  sump.v= sump.v*xx.v+p14.v
                  sump.v= sump.v*xx.v+p15.v 
                  xx.v  = xx.v-two25.v
                  sumq.v= (((((  &
                            xx.v+q1.v)  &
                          * xx.v+q2.v)  &
                          * xx.v+q3.v)  &
                          * xx.v+q4.v)  &
                          * xx.v+q5.v)
                  val.v = (sump.v/sumq.v)*x.v
                  
                    
              else where
                      
                      xx.v   = one.v/x.v-rec15.v
                      sump.v = ((((((   &
                                      pp1.v     &
                               * xx.v+pp2.v)  &
                               * xx.v+pp3.v)  &
                               * xx.v+pp4.v)  &
                               * xx.v+pp5.v)  &
                               * xx.v+pp6.v)  &
                               * xx.v+pp7.v)  &
                               * xx.v+pp8.v
                      sumq.v = (((((    &
                                 xx.v+qq1.v)  &
                               * xx.v+qq2.v)  &
                               * xx.v+qq3.v)  &
                               * xx.v+qq4.v)  &
                               * xx.v+qq5.v)  &
                               * xx.v+qq6.v
                      val.v  = sump.v/sumq.v
                      msk4.m = (xmax.v-one5.v<x.v)
                      where(msk4.m)
                            a.v = exp(x.v-frty.v)
                            b.v = exp40.v
                      else where 
                          
                            a.v = exp(x.v)
                            b.v = one.v
                      end where 
                      t0.v   = val.v*a.v+pbar.v*a.v
                      val.v  = (t0.v/sqrt(x.v))*b.v
            end where  
           
             msk5.m = (arg.v<zero.v)
             where(msk5.m) 
                  val.v = -val.v
             end where 
        end subroutine calci1_xmm2r8
        
      

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



      
         subroutine calck0_xmm2r8(arg,val)
              
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calck0_xmm2r8
              !dir$ attributes forceinline :: calck0_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_xmm2r8  
              type(XMM2r8_t),   intent(in)   :: arg
              type(XMM2r8_t),   intent(out)  :: val
                            
              type(XMM2r8_t),   parameter    :: zero   = XMM2r8_t(0.0_dp)
              type(XMM2r8_t),   parameter    :: one    = XMM2r8_t(1.0_dp)
              type(XMM2r8_t),   parameter    :: xsmall = XMM2r8_t(1.11e-16_dp)
              type(XMM2r8_t),   parameter    :: xinf   = XMM2r8_t(1.79e+308_dp)
              type(XMM2r8_t),   parameter    :: xmax   = XMM2r8_t(705.342e+00_dp)
              type(XMM2r8_t),   parameter    :: p1     = XMM2r8_t(5.8599221412826100000e-04_dp)        
	          type(XMM2r8_t),   parameter    :: p2     = XMM2r8_t(1.3166052564989571850e-01_dp)            
              type(XMM2r8_t),   parameter    :: p3     = XMM2r8_t(1.1999463724910714109e+01_dp)              
              type(XMM2r8_t),   parameter    :: p4     = XMM2r8_t(4.6850901201934832188e+02_dp)               
              type(XMM2r8_t),   parameter    :: p5     = XMM2r8_t(5.9169059852270512312e+03_dp)              
              type(XMM2r8_t),   parameter    :: p6     = XMM2r8_t(2.4708152720399552679e+03_dp)
              type(XMM2r8_t),   parameter    :: q1     = XMM2r8_t(-2.4994418972832303646e+02_dp)            
	          type(XMM2r8_t),   parameter    :: q2     = XMM2r8_t(2.1312714303849120380e+04_dp)
              type(XMM2r8_t),   parameter    :: f1     = XMM2r8_t(-1.6414452837299064100e+00_dp)
	          type(XMM2r8_t),   parameter    :: f2     = XMM2r8_t(-2.9601657892958843866e+02_dp)
              type(XMM2r8_t),   parameter    :: f3     = XMM2r8_t(-1.7733784684952985886e+04_dp)           
              type(XMM2r8_t),   parameter    :: f4     = XMM2r8_t(-4.0320340761145482298e+05_dp)
              type(XMM2r8_t),   parameter    :: g1     = XMM2r8_t(-2.5064972445877992730e+02_dp)
	          type(XMM2r8_t),   parameter    :: g2     = XMM2r8_t(2.9865713163054025489e+04_dp) 
              type(XMM2r8_t),   parameter    :: g3     = XMM2r8_t(-1.6128136304458193998e+06_dp)
              type(XMM2r8_t),   parameter    :: pp1    = XMM2r8_t(1.1394980557384778174e+02_dp)            
	          type(XMM2r8_t),   parameter    :: pp2    = XMM2r8_t(3.6832589957340267940e+03_dp)             
              type(XMM2r8_t),   parameter    :: pp3    = XMM2r8_t(3.1075408980684392399e+04_dp) 
              type(XMM2r8_t),   parameter    :: pp4    = XMM2r8_t(1.0577068948034021957e+05_dp) 
              type(XMM2r8_t),   parameter    :: pp5    = XMM2r8_t(1.7398867902565686251e+05_dp)
              type(XMM2r8_t),   parameter    :: pp6    = XMM2r8_t(1.5097646353289914539e+05_dp) 
              type(XMM2r8_t),   parameter    :: pp7    = XMM2r8_t(7.1557062783764037541e+04_dp) 
              type(XMM2r8_t),   parameter    :: pp8    = XMM2r8_t(1.8321525870183537725e+04_dp) 
              type(XMM2r8_t),   parameter    :: pp9    = XMM2r8_t(2.3444738764199315021e+03_dp) 
              type(XMM2r8_t),   parameter    :: pp10   = XMM2r8_t(1.1600249425076035558e+02_dp)
              type(XMM2r8_t),   parameter    :: qq1    = XMM2r8_t(2.0013443064949242491e+02_dp)
	          type(XMM2r8_t),   parameter    :: qq2    = XMM2r8_t(4.4329628889746408858e+03_dp)
              type(XMM2r8_t),   parameter    :: qq3    = XMM2r8_t(3.1474655750295278825e+04_dp)            
              type(XMM2r8_t),   parameter    :: qq4    = XMM2r8_t(9.7418829762268075784e+04_dp)            
              type(XMM2r8_t),   parameter    :: qq5    = XMM2r8_t(1.5144644673520157801e+05_dp)             
              type(XMM2r8_t),   parameter    :: qq6    = XMM2r8_t(1.2689839587977598727e+05_dp)             
              type(XMM2r8_t),   parameter    :: qq7    = XMM2r8_t(5.8824616785857027752e+04_dp)             
              type(XMM2r8_t),   parameter    :: qq8    = XMM2r8_t(1.4847228371802360957e+04_dp)             
              type(XMM2r8_t),   parameter    :: qq9    = XMM2r8_t(1.8821890840982713696e+03_dp)             
              type(XMM2r8_t),   parameter    :: qq10   = XMM2r8_t(9.2556599177304839811e+01_dp)
              type(XMM2r8_t),   automatic    :: sumf,sumg
              type(XMM2r8_t),   automatic    :: sump,sumq
              type(XMM2r8_t),   automatic    :: temp,x
              type(XMM2r8_t),   automatic    :: xx,t0,t1,t2
              type(Mask2_t),    automatic    :: msk1,msk2
              type(Mask2_t),    automatic    :: msk3,msk4
              !dir$ attributes align : 16 :: p1
              !dir$ attributes align : 16 :: p2 
              !dir$ attributes align : 16 :: p3 
              !dir$ attributes align : 16 :: p4 
              !dir$ attributes align : 16 :: p5 
              !dir$ attributes align : 16 :: p6 
              !dir$ attributes align : 16 :: q1 
              !dir$ attributes align : 16 :: q2 
              !dir$ attributes align : 16 :: f1 
              !dir$ attributes align : 16 :: f2 
              !dir$ attributes align : 16 :: f3 
              !dir$ attributes align : 16 :: f4 
              !dir$ attributes align : 16 :: g1 
              !dir$ attributes align : 16 :: g2 
              !dir$ attributes align : 16 :: g3 
              !dir$ attributes align : 16 :: pp1 
              !dir$ attributes align : 16 :: pp2 
              !dir$ attributes align : 16 :: pp3 
              !dir$ attributes align : 16 :: pp4 
              !dir$ attributes align : 16 :: pp5 
              !dir$ attributes align : 16 :: pp6
              !dir$ attributes align : 16 :: pp7 
              !dir$ attributes align : 16 :: pp8
              !dir$ attributes align : 16 :: pp9 
              !dir$ attributes align : 16 :: pp10
              !dir$ attributes align : 16 :: qq1
              !dir$ attributes align : 16 :: qq2
              !dir$ attributes align : 16 :: qq3
              !dir$ attributes align : 16 :: qq4
              !dir$ attributes align : 16 :: qq5
              !dir$ attributes align : 16 :: qq6
              !dir$ attributes align : 16 :: qq7
              !dir$ attributes align : 16 :: qq8
              !dir$ attributes align : 16 :: qq9
              !dir$ attributes align : 16 :: qq10
              !dir$ attributes align : 16 :: zero
              !dir$ attributes align : 16 :: one
              !dir$ attributes align : 16 :: xsmall
              !dir$ attributes align : 16 :: xinf
              !dir$ attributes align : 16 :: xmax
              !dir$ attributes align : 16 :: sumf
              !dir$ attributes align : 16 :: sumg
              !dir$ attributes align : 16 :: sump
              !dir$ attributes align : 16 :: sumq
              !dir$ attributes align : 16 :: temp
              !dir$ attributes align : 16 :: x
              !dir$ attributes align : 16 :: xx
              !dir$ attributes align : 16 :: t0
              !dir$ attributes align : 16 :: t1
              !dir$ attributes align : 16 :: t2
              x.v    = arg.v
              msk1.m = (zero.v<x.v)
              msk4.m = (xmax.v<x.v)
              where(msk1.m)
                    msk2.m = (x.v<=one.v)
                    where(msk2.m)
                        temp.v = log(x.v)
                        msk3.m = (x.v<=xsmall.v)
                        where(msk3.m)
                              val.v = p6.v/q2.v-temp.v 
                                 
                        else where 
                      
                              xx.v   = x.v*x.v
                              sump.v = ((((  &
                                        p1.v   &
                                 * xx.v+p2.v)  &
                                 * xx.v+p3.v)  &
                                 * xx.v+p4.v)  &
                                 * xx.v+p5.v)  &
                                 * xx.v+p6.v
                              sumq.v = (xx.v+q1.v) * &
                                   xx.v+q2.v
                              sumf.v = (((  &
                                         f1.v) &
                                  * xx.v+f2.v) &
                                  * xx.v+f3.v) &
                                  * xx.v+f4.v
                              sumg.v = ((xx.v+g1.v) * &
                                    xx.v+g2.v) * &
                                    xx.v+g3.v
                              !t0.v   = sump.v/sumq.v
                              !t1.v   = xx.v*sumf.v
                              !t2.v   = temp.v/sumg.v-temp.v
                              !val.v  = t0.v-t1.v*t2.v
                              val.v   = sump.v/sumq.v-xx.v*sumf.v*temp.v/sumg.v-temp.v 
                        end where    
                     
                    else where (msk4.m)
                         val.v = zero.v
                    else where 
                         xx.v  = one.v/x.v
                         t0.v  = sqrt(x.v)
                         sump.v= pp1.v
                         sump.v= sump.v*xx.v+pp2.v
                         sump.v= sump.v*xx.v+pp3.v
                         sump.v= sump.v*xx.v+pp4.v
                         sump.v= sump.v*xx.v+pp5.v
                         sump.v= sump.v*xx.v+pp6.v
                         sump.v= sump.v*xx.v+pp7.v
                         sump.v= sump.v*xx.v+pp8.v
                         sump.v= sump.v*xx.v+pp9.v
                         sump.v= sump.v*xx.v+pp10.v
                         sumq.v= xx.v
                         sumq.v= (sumq.v+qq1.v)*xx.v
                         sumq.v= (sumq.v+qq2.v)*xx.v
                         sumq.v= (sumq.v+qq3.v)*xx.v
                         sumq.v= (sumq.v+qq4.v)*xx.v
                         sumq.v= (sumq.v+qq5.v)*xx.v
                         sumq.v= (sumq.v+qq6.v)*xx.v
                         sumq.v= (sumq.v+qq7.v)*xx.v
                         sumq.v= (sumq.v+qq8.v)*xx.v
                         sumq.v= (sumq.v+qq9.v)*xx.v 
                         sumq.v= sumq.v+qq10.v
                         val.v = sump.v/sumq.v/t0.v
                         val.v = val.v*exp(-x.v)
                    end where 
            else where 
                 val.v = xinf.v
            end where 

         end subroutine calck0_xmm2r8
         
      

!
!! CALCK1 computes various K1 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the second kind
!    and order one, K1(X) and EXP(X)*K1(X), for real arguments X.
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
!    Input, real ( kind = 8 ) ARG, the argument.  XLEAST < ARG is
!    always required.  If JINT = 1, then the argument must also be
!    less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = K1(x);
!    2, RESULT = exp(x) * K1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, K1(x);
!    2, exp(x) * K1(x);  



       subroutine calck1_xmm2r8(arg,val)
              
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calck1_xmm2r8
              !dir$ attributes forceinline :: calck1_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_xmm2r8  
              type(XMM2r8_t),   intent(in)   :: arg
              type(XMM2r8_t),   intent(out)  :: val
              
              type(XMM2r8_t),   automatic    :: sumf,sumg
              type(XMM2r8_t),   automatic    :: sump,sumq
              type(XMM2r8_t),   automatic    :: x,xx
              type(XMM2r8_t),   automatic    :: t0,t1,t2
              type(Mask2_t),    automatic    :: msk1,msk2
              type(Mask2_t),    automatic    :: msk3,msk4  
              type(XMM2r8_t),   parameter    :: zero   = XMM2r8_t(0.0_dp)
              type(XMM2r8_t),   parameter    :: one    = XMM2r8_t(1.0_dp)
              type(XMM2r8_t),   parameter    :: xsmall = XMM2r8_t(1.11e-16_dp)
              type(XMM2r8_t),   parameter    :: xinf   = XMM2r8_t(1.79e+308_dp)
              type(XMM2r8_t),   parameter    :: xmax   = XMM2r8_t(705.342e+00_dp) 
              type(XMM2r8_t),   parameter    :: xleast = XMM2r8_t(2.23e-308_dp)
              type(XMM2r8_t),   parameter    :: p1     = XMM2r8_t(4.8127070456878442310e-1_dp)
              type(XMM2r8_t),   parameter    :: p2     = XMM2r8_t(9.9991373567429309922e+1_dp)
              type(XMM2r8_t),   parameter    :: p3     = XMM2r8_t(7.1885382604084798576e+3_dp)
              type(XMM2r8_t),   parameter    :: p4     = XMM2r8_t(1.7733324035147015630e+5_dp)
              type(XMM2r8_t),   parameter    :: p5     = XMM2r8_t(7.1938920065420586101e+5_dp)
              type(XMM2r8_t),   parameter    :: q1     = XMM2r8_t(-2.8143915754538725829e+2_dp)
              type(XMM2r8_t),   parameter    :: q2     = XMM2r8_t(3.7264298672067697862e+4_dp) 
              type(XMM2r8_t),   parameter    :: q3     = XMM2r8_t(-2.2149374878243304548e+6_dp)
              type(XMM2r8_t),   parameter    :: f1     = XMM2r8_t(-2.2795590826955002390e-1_dp)
              type(XMM2r8_t),   parameter    :: f2     = XMM2r8_t(-5.3103913335180275253e+1_dp) 
              type(XMM2r8_t),   parameter    :: f3     = XMM2r8_t(-4.5051623763436087023e+3_dp) 
              type(XMM2r8_t),   parameter    :: f4     = XMM2r8_t(-1.4758069205414222471e+5_dp) 
              type(XMM2r8_t),   parameter    :: f5     = XMM2r8_t(-1.3531161492785421328e+6_dp)
              type(XMM2r8_t),   parameter    :: g1     = XMM2r8_t(-3.0507151578787595807e+2_dp)
              type(XMM2r8_t),   parameter    :: g2     = XMM2r8_t(4.3117653211351080007e+4_dp)
              type(XMM2r8_t),   parameter    :: g3     = XMM2r8_t(-2.7062322985570842656e+6_dp)
              type(XMM2r8_t),   parameter    :: pp1    = XMM2r8_t(6.4257745859173138767e-2_dp)
              type(XMM2r8_t),   parameter    :: pp2    = XMM2r8_t(7.5584584631176030810e+0_dp)
              type(XMM2r8_t),   parameter    :: pp3    = XMM2r8_t(1.3182609918569941308e+2_dp)
              type(XMM2r8_t),   parameter    :: pp4    = XMM2r8_t(8.1094256146537402173e+2_dp)
              type(XMM2r8_t),   parameter    :: pp5    = XMM2r8_t(2.3123742209168871550e+3_dp)
              type(XMM2r8_t),   parameter    :: pp6    = XMM2r8_t(3.4540675585544584407e+3_dp)
              type(XMM2r8_t),   parameter    :: pp7    = XMM2r8_t(2.8590657697910288226e+3_dp)
              type(XMM2r8_t),   parameter    :: pp8    = XMM2r8_t(1.3319486433183221990e+3_dp)
              type(XMM2r8_t),   parameter    :: pp9    = XMM2r8_t(3.4122953486801312910e+2_dp)
              type(XMM2r8_t),   parameter    :: pp10   = XMM2r8_t(4.4137176114230414036e+1_dp)
              type(XMM2r8_t),   parameter    :: pp11   = XMM2r8_t(2.2196792496874548962e+0_dp) 
              type(XMM2r8_t),   parameter    :: qq1    = XMM2r8_t(3.6001069306861518855e+1_dp)
              type(XMM2r8_t),   parameter    :: qq2    = XMM2r8_t(3.3031020088765390854e+2_dp)
              type(XMM2r8_t),   parameter    :: qq3    = XMM2r8_t(1.2082692316002348638e+3_dp)
              type(XMM2r8_t),   parameter    :: qq4    = XMM2r8_t(2.1181000487171943810e+3_dp)
              type(XMM2r8_t),   parameter    :: qq5    = XMM2r8_t(1.9448440788918006154e+3_dp)
              type(XMM2r8_t),   parameter    :: qq6    = XMM2r8_t(9.6929165726802648634e+2_dp)
              type(XMM2r8_t),   parameter    :: qq7    = XMM2r8_t(2.5951223655579051357e+2_dp)
              type(XMM2r8_t),   parameter    :: qq8    = XMM2r8_t(3.4552228452758912848e+1_dp)
              type(XMM2r8_t),   parameter    :: qq9    = XMM2r8_t(1.7710478032601086579e+0_dp)
               
              !dir$ attributes align : 16 :: zero
              !dir$ attributes align : 16 :: one
              !dir$ attributes align : 16 :: xsmall
              !dir$ attributes align : 16 :: xinf
              !dir$ attributes align : 16 :: xmax
              !dir$ attributes align : 16 :: xleast 
              !dir$ attributes align : 16 :: sumf
              !dir$ attributes align : 16 :: sumg
              !dir$ attributes align : 16 :: sump
              !dir$ attributes align : 16 :: sumq
              !dir$ attributes align : 16 :: x
              !dir$ attributes align : 16 :: xx
              !dir$ attributes align : 16 :: t0
              !dir$ attributes align : 16 :: t1
              !dir$ attributes align : 16 :: t2
              !dir$ attributes align : 16 :: p1 
              !dir$ attributes align : 16 :: p2 
              !dir$ attributes align : 16 :: p3 
              !dir$ attributes align : 16 :: p4 
              !dir$ attributes align : 16 :: p5 
              !dir$ attributes align : 16 :: q1 
              !dir$ attributes align : 16 :: q2 
              !dir$ attributes align : 16 :: q3 
              !dir$ attributes align : 16 :: f1 
              !dir$ attributes align : 16 :: f2 
              !dir$ attributes align : 16 :: f3 
              !dir$ attributes align : 16 :: f4 
              !dir$ attributes align : 16 :: f5 
              !dir$ attributes align : 16 :: g1 
              !dir$ attributes align : 16 :: g2 
              !dir$ attributes align : 16 :: g3 
              !dir$ attributes align : 16 :: pp1 
              !dir$ attributes align : 16 :: pp2 
              !dir$ attributes align : 16 :: pp3 
              !dir$ attributes align : 16 :: pp4 
              !dir$ attributes align : 16 :: pp5 
              !dir$ attributes align : 16 :: pp6 
              !dir$ attributes align : 16 :: pp7 
              !dir$ attributes align : 16 :: pp8 
              !dir$ attributes align : 16 :: pp9 
              !dir$ attributes align : 16 :: pp10
              !dir$ attributes align : 16 :: pp11
              !dir$ attributes align : 16 :: qq1
              !dir$ attributes align : 16 :: qq2
              !dir$ attributes align : 16 :: qq3
              !dir$ attributes align : 16 :: qq4
              !dir$ attributes align : 16 :: qq5
              !dir$ attributes align : 16 :: qq6
              !dir$ attributes align : 16 :: qq7
              !dir$ attributes align : 16 :: qq8
              !dir$ attributes align : 16 :: qq9
              x.v    = arg.v
              msk1.m = (x.v<=xleast.v)
              msk2.m = (x.v<=one.v)
              msk4.m = (xmax.v<x.v)
              where(msk1.m)
                  val.v = xinf.v
              else where(msk2.m)
                  msk3.m = (x.v<xsmall.v)
                  where(msk3.m)
                      val.v = one.v/x.v
                  else where 
                      xx.v    = x.v*x.v
                      sump.v  = ((((    &
                                    p1.v       &
                               *    xx.v+p2.v) & 
                               *    xx.v+p3.v) &
                               *    xx.v+p4.v) &
                               *    xx.v+p5.v) &
                               *    xx.v+q3.v
                      sumq.v   = ((     &
                                    xx.v+q1.v) &
                                  * xx.v+q2.v) &
                                  * xx.v+q3.v
                      t1.v     = sump.v/sumq.v  
                      sumf.v   = (((    &
                                    f1.v       &
                               *    xx.v+f2.v) &
                               *    xx.v+f3.v) &
                               *    xx.v+f4.v) &
                               *    xx.v+f5.v
                      t2.v     = xx.v*log(x.v)
                      sumg.v   = ((    &
                                    xx.v+g1.v) &
                               *    xx.v+g2.v) &
                               *    xx.v+g3.v
                      t0.v     = sumf.v/sumg.v
                      val.v    = (t2.v*t0.v+t1.v)/x.v
                    
                  end where 
             else where(msk4.m)
                      val.v = zero.v
             else where 
                      xx.v  = one.v/x.v
                      sump.v= pp1.v
                      sump.v= sump.v*xx.v+pp2.v
                      sump.v= sump.v*xx.v+pp3.v
                      sump.v= sump.v*xx.v+pp4.v
                      sump.v= sump.v*xx.v+pp5.v
                      sump.v= sump.v*xx.v+pp6.v
                      sump.v= sump.v*xx.v+pp7.v
                      sump.v= sump.v*xx.v+pp8.v
                      sump.v= sump.v*xx.v+pp9.v
                      sump.v= sump.v*xx.v+pp10.v
                      sump.v= sump.v*xx.v+pp11.v
                      t0.v  = sqrt(x.v)
                      sumq.v= xx.v
                      sumq.v= (sumq.v+qq1.v)*xx.v
                      sumq.v= (sumq.v+qq2.v)*xx.v
                      sumq.v= (sumq.v+qq3.v)*xx.v
                      sumq.v= (sumq.v+qq4.v)*xx.v
                      sumq.v= (sumq.v+qq5.v)*xx.v
                      sumq.v= (sumq.v+qq6.v)*xx.v
                      sumq.v= (sumq.v+qq7.v)*xx.v
                      sumq.v= (sumq.v+qq8.v)*xx.v
                      sumq.v= sumq.v+qq9.v
                      val.v = sump.v/sumq.v/t0.v
                      val.v = val.v*exp(-x.v)
             end where 

        end subroutine calck1_xmm2r8
        
        

!
!! CALJY0 computes various J0 and Y0 Bessel functions.
!
!  Discussion:
!
!    This routine computes zero-order Bessel functions of the first and
!    second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
!    for Y0, and |X| <= XMAX for J0.
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
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 0, ARG
!    must satisfy
!     -XMAX < ARG < XMAX;
!    If JINT = 1, then ARG must satisfy
!      0 < ARG < XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    0, RESULT = J0(x);
!    1, RESULT = Y0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    0, J0(x);
!    1, Y0(x);  



        subroutine caljy0_xmm2r8(arg,val,jint)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: caljy0_xmm2r8
              !dir$ attributes forceinline :: caljy0_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_xmm2r8  
              
              type(XMM2r8_t),   intent(in)   :: arg
              type(XMM2r8_t),   intent(out)  :: val
              integer(kind=i4), intent(in)   :: jint 
             
              type(XMM2r8_t),   parameter :: zero  = XMM2r8_t(0.0e+0_dp)
              type(XMM2r8_t),   parameter :: one   = XMM2r8_t(1.0e+0_dp)
              type(XMM2r8_t),   parameter :: three = XMM2r8_t(3.0e+0_dp)
              type(XMM2r8_t),   parameter :: four  = XMM2r8_t(4.0e+0_dp)
              type(XMM2r8_t),   parameter :: eight = XMM2r8_t(8.0e+0_dp)
              type(XMM2r8_t),   parameter :: five5 = XMM2r8_t(5.5e+0_dp)
              type(XMM2r8_t),   parameter :: sixty4= XMM2r8_t(64.0e+0_dp)
              type(XMM2r8_t),   parameter :: oneov8= XMM2r8_t(0.125e+0_dp)
              type(XMM2r8_t),   parameter :: p17   = XMM2r8_t(1.716e-1_dp)
              type(XMM2r8_t),   parameter :: two56 = XMM2r8_t(256.0e+0_dp)
              type(XMM2r8_t),   parameter :: cons  = XMM2r8_t(-1.1593151565841244881e-1_dp)
              type(XMM2r8_t),   parameter :: pi2   = XMM2r8_t(6.3661977236758134308e-1_dp)
              type(XMM2r8_t),   parameter :: twopi = XMM2r8_t(6.2831853071795864769e+0_dp)
              type(XMM2r8_t),   parameter :: twopi1= XMM2r8_t(6.28125e+0_dp)
              type(XMM2r8_t),   parameter :: twopi2= XMM2r8_t(1.9353071795864769253e-3_dp)
              type(XMM2r8_t),   parameter :: xmax  = XMM2r8_t(1.07e+09_dp)
              type(XMM2r8_t),   parameter :: xsmall= XMM2r8_t(9.31e-10_dp)
              type(XMM2r8_t),   parameter :: xinf  = XMM2r8_t(1.7e+38_dp)
              type(XMM2r8_t),   parameter :: xj0   = XMM2r8_t(2.4048255576957727686e+0_dp)
              type(XMM2r8_t),   parameter :: xj1   = XMM2r8_t(5.5200781102863106496e+0_dp)
              type(XMM2r8_t),   parameter :: xy0   = XMM2r8_t(8.9357696627916752158e-1_dp)
              type(XMM2r8_t),   parameter :: xy1   = XMM2r8_t(3.9576784193148578684e+0_dp)
              type(XMM2r8_t),   parameter :: xy2   = XMM2r8_t(7.0860510603017726976e+0_dp)
              type(XMM2r8_t),   parameter :: xj01  = XMM2r8_t(616.0e+0_dp)
              type(XMM2r8_t),   parameter :: xj02  = XMM2r8_t(-1.4244423042272313784e-3_dp)
              type(XMM2r8_t),   parameter :: xj11  = XMM2r8_t(1413.0e+0_dp)
              type(XMM2r8_t),   parameter :: xj12  = XMM2r8_t(5.4686028631064959660e-4_dp)
              type(XMM2r8_t),   parameter :: xy01  = XMM2r8_t(228.0e+0_dp)
              type(XMM2r8_t),   parameter :: xy02  = XMM2r8_t(2.9519662791675215849e-3_dp)
              type(XMM2r8_t),   parameter :: xy11  = XMM2r8_t(1013.0e+0_dp)
              type(XMM2r8_t),   parameter :: xy12  = XMM2r8_t(6.4716931485786837568e-4_dp)
              type(XMM2r8_t),   parameter :: xy21  = XMM2r8_t(1814.0e+0_dp)
              type(XMM2r8_t),   parameter :: xy22  = XMM2r8_t(1.1356030177269762362e-4_dp)
              type(XMM2r8_t),   parameter :: plg1  = XMM2r8_t(-2.4562334077563243311e+1_dp)
	          type(XMM2r8_t),   parameter :: plg2  = XMM2r8_t(2.3642701335621505212e+2_dp)
              type(XMM2r8_t),   parameter :: plg3  = XMM2r8_t(-5.4989956895857911039e+2_dp)
              type(XMM2r8_t),   parameter :: plg4  = XMM2r8_t(3.5687548468071500413e+2_dp)
              type(XMM2r8_t),   parameter :: qlg1  = XMM2r8_t(-3.5553900764052419184e+1_dp)
	          type(XMM2r8_t),   parameter :: qlg2  = XMM2r8_t(1.9400230218539473193e+2_dp)
              type(XMM2r8_t),   parameter :: qlg3  = XMM2r8_t(-3.3442903192607538956e+2_dp)
              type(XMM2r8_t),   parameter :: qlg4  = XMM2r8_t(1.7843774234035750207e+2_dp)  
              type(XMM2r8_t),   parameter :: pj01  = XMM2r8_t(6.6302997904833794242e+6_dp)
	          type(XMM2r8_t),   parameter :: pj02  = XMM2r8_t(-6.2140700423540120665e+8_dp) 
              type(XMM2r8_t),   parameter :: pj03  = XMM2r8_t(2.7282507878605942706e+10_dp)
              type(XMM2r8_t),   parameter :: pj04  = XMM2r8_t(-4.1298668500990866786e+11_dp) 
              type(XMM2r8_t),   parameter :: pj05  = XMM2r8_t(-1.2117036164593528341e-1_dp)
              type(XMM2r8_t),   parameter :: pj06  = XMM2r8_t(1.0344222815443188943e+2_dp)
              type(XMM2r8_t),   parameter :: pj07  = XMM2r8_t(-3.6629814655107086448e+4_dp)
              type(XMM2r8_t),   parameter :: qj01  = XMM2r8_t(4.5612696224219938200e+5_dp)
	          type(XMM2r8_t),   parameter :: qj02  = XMM2r8_t(1.3985097372263433271e+8_dp) 
              type(XMM2r8_t),   parameter :: qj03  = XMM2r8_t(2.6328198300859648632e+10_dp) 
              type(XMM2r8_t),   parameter :: qj04  = XMM2r8_t(2.3883787996332290397e+12_dp) 
              type(XMM2r8_t),   parameter :: qj05  = XMM2r8_t(9.3614022392337710626e+2_dp)
              type(XMM2r8_t),   parameter :: pj11  = XMM2r8_t(4.4176707025325087628e+3_dp)
	          type(XMM2r8_t),   parameter :: pj12  = XMM2r8_t(1.1725046279757103576e+4_dp)
              type(XMM2r8_t),   parameter :: pj13  = XMM2r8_t(1.0341910641583726701e+4_dp)
              type(XMM2r8_t),   parameter :: pj14  = XMM2r8_t(-7.2879702464464618998e+3_dp) 
              type(XMM2r8_t),   parameter :: pj15  = XMM2r8_t(-1.2254078161378989535e+4_dp)
              type(XMM2r8_t),   parameter :: pj16  = XMM2r8_t(-1.8319397969392084011e+3_dp) 
              type(XMM2r8_t),   parameter :: pj17  = XMM2r8_t(4.8591703355916499363e+1_dp) 
              type(XMM2r8_t),   parameter :: pj18  = XMM2r8_t(7.4321196680624245801e+2_dp)
              type(XMM2r8_t),   parameter :: qj11  = XMM2r8_t(3.3307310774649071172e+2_dp)
	          type(XMM2r8_t),   parameter :: qj12  = XMM2r8_t(-2.9458766545509337327e+3_dp)
              type(XMM2r8_t),   parameter :: qj13  = XMM2r8_t(1.8680990008359188352e+4_dp)
              type(XMM2r8_t),   parameter :: qj14  = XMM2r8_t(-8.4055062591169562211e+4_dp) 
              type(XMM2r8_t),   parameter :: qj15  = XMM2r8_t(2.4599102262586308984e+5_dp)
              type(XMM2r8_t),   parameter :: qj16  = XMM2r8_t(-3.5783478026152301072e+5_dp) 
              type(XMM2r8_t),   parameter :: qj17  = XMM2r8_t(-2.5258076240801555057e+1_dp)
              type(XMM2r8_t),   parameter :: py01  = XMM2r8_t(1.0102532948020907590e+4_dp)
	          type(XMM2r8_t),   parameter :: py02  = XMM2r8_t(-2.1287548474401797963e+6_dp) 
              type(XMM2r8_t),   parameter :: py03  = XMM2r8_t(2.0422274357376619816e+8_dp)
              type(XMM2r8_t),   parameter :: py04  = XMM2r8_t(-8.3716255451260504098e+9_dp) 
              type(XMM2r8_t),   parameter :: py05  = XMM2r8_t(1.0723538782003176831e+11_dp)
              type(XMM2r8_t),   parameter :: py06  = XMM2r8_t(-1.8402381979244993524e+1_dp)
              type(XMM2r8_t),   parameter :: qy01  = XMM2r8_t(6.6475986689240190091e+2_dp)
	          type(XMM2r8_t),   parameter :: qy02  = XMM2r8_t(2.3889393209447253406e+5_dp) 
              type(XMM2r8_t),   parameter :: qy03  = XMM2r8_t(5.5662956624278251596e+7_dp) 
              type(XMM2r8_t),   parameter :: qy04  = XMM2r8_t(8.1617187777290363573e+9_dp) 
              type(XMM2r8_t),   parameter :: qy05  = XMM2r8_t(5.8873865738997033405e+11_dp)
              type(XMM2r8_t),   parameter :: py11  = XMM2r8_t(1.4566865832663635920e+4_dp) 
	          type(XMM2r8_t),   parameter :: py12  = XMM2r8_t(4.6905288611678631510e+6_dp) 
              type(XMM2r8_t),   parameter :: py13  = XMM2r8_t(-6.9590439394619619534e+8_dp)
              type(XMM2r8_t),   parameter :: py14  = XMM2r8_t(4.3600098638603061642e+10_dp)
              type(XMM2r8_t),   parameter :: py15  = XMM2r8_t(-5.5107435206722644429e+11_dp)
              type(XMM2r8_t),   parameter :: py16  = XMM2r8_t(-2.2213976967566192242e+13_dp) 
              type(XMM2r8_t),   parameter :: py17  = XMM2r8_t(1.7427031242901594547e+1_dp)
              type(XMM2r8_t),   parameter :: qy11  = XMM2r8_t(8.3030857612070288823e+2_dp)
	          type(XMM2r8_t),   parameter :: qy12  = XMM2r8_t(4.0669982352539552018e+5_dp) 
              type(XMM2r8_t),   parameter :: qy13  = XMM2r8_t(1.3960202770986831075e+8_dp) 
              type(XMM2r8_t),   parameter :: qy14  = XMM2r8_t(3.4015103849971240096e+10_dp)
              type(XMM2r8_t),   parameter :: qy15  = XMM2r8_t(5.4266824419412347550e+12_dp) 
              type(XMM2r8_t),   parameter :: qy16  = XMM2r8_t(4.3386146580707264428e+14_dp)
              type(XMM2r8_t),   parameter :: py21  = XMM2r8_t(2.1363534169313901632e+4_dp)
	          type(XMM2r8_t),   parameter :: py22  = XMM2r8_t(-1.0085539923498211426e+7_dp) 
              type(XMM2r8_t),   parameter :: py23  = XMM2r8_t(2.1958827170518100757e+9_dp)
              type(XMM2r8_t),   parameter :: py24  = XMM2r8_t(-1.9363051266772083678e+11_dp) 
              type(XMM2r8_t),   parameter :: py25  = XMM2r8_t(-1.2829912364088687306e+11_dp) 
              type(XMM2r8_t),   parameter :: py26  = XMM2r8_t(6.7016641869173237784e+14_dp) 
              type(XMM2r8_t),   parameter :: py27  = XMM2r8_t(-8.0728726905150210443e+15_dp)
              type(XMM2r8_t),   parameter :: py28  = XMM2r8_t(-1.7439661319197499338e+1_dp)
              type(XMM2r8_t),   parameter :: qy21  = XMM2r8_t(8.7903362168128450017e+2_dp)
	          type(XMM2r8_t),   parameter :: qy22  = XMM2r8_t(5.3924739209768057030e+5_dp)
              type(XMM2r8_t),   parameter :: qy23  = XMM2r8_t(2.4727219475672302327e+8_dp) 
              type(XMM2r8_t),   parameter :: qy24  = XMM2r8_t(8.6926121104209825246e+10_dp)
              type(XMM2r8_t),   parameter :: qy25  = XMM2r8_t(2.2598377924042897629e+13_dp) 
              type(XMM2r8_t),   parameter :: qy26  = XMM2r8_t(3.9272425569640309819e+15_dp)
              type(XMM2r8_t),   parameter :: qy27  = XMM2r8_t(3.4563724628846457519e+17_dp)
              type(XMM2r8_t),   parameter :: p01   = XMM2r8_t(3.4806486443249270347e+3_dp)
	          type(XMM2r8_t),   parameter :: p02   = XMM2r8_t(2.1170523380864944322e+4_dp) 
              type(XMM2r8_t),   parameter :: p03   = XMM2r8_t(4.1345386639580765797e+4_dp) 
              type(XMM2r8_t),   parameter :: p04   = XMM2r8_t(2.2779090197304684302e+4_dp)
              type(XMM2r8_t),   parameter :: p05   = XMM2r8_t(8.8961548424210455236e-1_dp) 
              type(XMM2r8_t),   parameter :: p06   = XMM2r8_t(1.5376201909008354296e+2_dp)
              type(XMM2r8_t),   parameter :: q01   = XMM2r8_t(3.5028735138235608207e+3_dp)
	          type(XMM2r8_t),   parameter :: q02   = XMM2r8_t(2.1215350561880115730e+4_dp) 
              type(XMM2r8_t),   parameter :: q03   = XMM2r8_t(4.1370412495510416640e+4_dp) 
              type(XMM2r8_t),   parameter :: q04   = XMM2r8_t(2.2779090197304684318e+4_dp)
              type(XMM2r8_t),   parameter :: q05   = XMM2r8_t(1.5711159858080893649e+2_dp)
              type(XMM2r8_t),   parameter :: p11   = XMM2r8_t(-2.2300261666214198472e+1_dp)
	          type(XMM2r8_t),   parameter :: p12   = XMM2r8_t(-1.1183429920482737611e+2_dp)
              type(XMM2r8_t),   parameter :: p13   = XMM2r8_t(-1.8591953644342993800e+2_dp)
              type(XMM2r8_t),   parameter :: p14   = XMM2r8_t(-8.9226600200800094098e+1_dp)
              type(XMM2r8_t),   parameter :: p15   = XMM2r8_t(-8.8033303048680751817e+3_dp)
              type(XMM2r8_t),   parameter :: p16   = XMM2r8_t(-1.2441026745835638459e+00_dp)
              type(XMM2r8_t),   parameter :: q11   = XMM2r8_t(1.4887231232283756582e+3_dp) 
	          type(XMM2r8_t),   parameter :: q12   = XMM2r8_t(7.2642780169211018836e+3_dp)
              type(XMM2r8_t),   parameter :: q13   = XMM2r8_t(1.1951131543434613647e+4_dp) 
              type(XMM2r8_t),   parameter :: q14   = XMM2r8_t(5.7105024128512061905e+3_dp)
              type(XMM2r8_t),   parameter :: q15   = XMM2r8_t(9.0593769594993125859e+1_dp)
               !dir$ attributes align : 16 :: zero
              !dir$ attributes align : 16 :: one
              !dir$ attributes align : 16 :: three
              !dir$ attributes align : 16 :: four
              !dir$ attributes align : 16 :: eight
              !dir$ attributes align : 16 :: five5
              !dir$ attributes align : 16 :: sixty4
              !dir$ attributes align : 16 :: oneov8
              !dir$ attributes align : 16 :: p17
              !dir$ attributes align : 16 :: two56
              !dir$ attributes align : 16 :: cons
              !dir$ attributes align : 16 :: pi2
              !dir$ attributes align : 16 :: twopi
              !dir$ attributes align : 16 :: twopi1
              !dir$ attributes align : 16 :: twopi2
              !dir$ attributes align : 16 :: xmax
              !dir$ attributes align : 16 :: xsmall
              !dir$ attributes align : 16 :: xinf
              !dir$ attributes align : 16 :: xj0
              !dir$ attributes align : 16 :: xj1
              !dir$ attributes align : 16 :: xy0
              !dir$ attributes align : 16 :: xy1
              !dir$ attributes align : 16 :: xy2
              !dir$ attributes align : 16 :: xj01
              !dir$ attributes align : 16 :: xj02
              !dir$ attributes align : 16 :: xj11
              !dir$ attributes align : 16 :: xj12
              !dir$ attributes align : 16 :: xy01
              !dir$ attributes align : 16 :: xy02
              !dir$ attributes align : 16 :: xy11
              !dir$ attributes align : 16 :: xy12
              !dir$ attributes align : 16 :: xy21
              !dir$ attributes align : 16 :: xy22
              !dir$ attributes align : 16 :: plg1
              !dir$ attributes align : 16 :: plg2
              !dir$ attributes align : 16 :: plg3
              !dir$ attributes align : 16 :: plg4
              !dir$ attributes align : 16 :: qlg1
              !dir$ attributes align : 16 :: qlg2
              !dir$ attributes align : 16 :: qlg3
              !dir$ attributes align : 16 :: qlg4
              !dir$ attributes align : 16 :: pj01
              !dir$ attributes align : 16 :: pj02
              !dir$ attributes align : 16 :: pj03
              !dir$ attributes align : 16 :: pj04
              !dir$ attributes align : 16 :: pj05
              !dir$ attributes align : 16 :: pj06
              !dir$ attributes align : 16 :: pj07
              !dir$ attributes align : 16 :: qj01
              !dir$ attributes align : 16 :: qj02
              !dir$ attributes align : 16 :: qj03
              !dir$ attributes align : 16 :: qj04
              !dir$ attributes align : 16 :: qj05
              !dir$ attributes align : 16 :: pj11
              !dir$ attributes align : 16 :: pj12
              !dir$ attributes align : 16 :: pj13
              !dir$ attributes align : 16 :: pj14
              !dir$ attributes align : 16 :: pj15
              !dir$ attributes align : 16 :: pj16
              !dir$ attributes align : 16 :: pj17
              !dir$ attributes align : 16 :: pj18
              !dir$ attributes align : 16 :: qj11
              !dir$ attributes align : 16 :: qj12
              !dir$ attributes align : 16 :: qj13
              !dir$ attributes align : 16 :: qj14
              !dir$ attributes align : 16 :: qj15
              !dir$ attributes align : 16 :: qj16
              !dir$ attributes align : 16 :: qj17
              !dir$ attributes align : 16 :: py01
              !dir$ attributes align : 16 :: py02
              !dir$ attributes align : 16 :: py03
              !dir$ attributes align : 16 :: py04
              !dir$ attributes align : 16 :: py05
              !dir$ attributes align : 16 :: py06
              !dir$ attributes align : 16 :: qy01
              !dir$ attributes align : 16 :: qy02
              !dir$ attributes align : 16 :: qy03
              !dir$ attributes align : 16 :: qy04
              !dir$ attributes align : 16 :: qy05
              !dir$ attributes align : 16 :: py11
              !dir$ attributes align : 16 :: py12
              !dir$ attributes align : 16 :: py13
              !dir$ attributes align : 16 :: py14
              !dir$ attributes align : 16 :: py15
              !dir$ attributes align : 16 :: py16
              !dir$ attributes align : 16 :: py17
              !dir$ attributes align : 16 :: qy11
              !dir$ attributes align : 16 :: qy12
              !dir$ attributes align : 16 :: qy13
              !dir$ attributes align : 16 :: qy14
              !dir$ attributes align : 16 :: qy15
              !dir$ attributes align : 16 :: qy16
              !dir$ attributes align : 16 :: py21
              !dir$ attributes align : 16 :: py22
              !dir$ attributes align : 16 :: py23
              !dir$ attributes align : 16 :: py24
              !dir$ attributes align : 16 :: py25
              !dir$ attributes align : 16 :: py26
              !dir$ attributes align : 16 :: py27
              !dir$ attributes align : 16 :: py28
              !dir$ attributes align : 16 :: qy21
              !dir$ attributes align : 16 :: qy22
              !dir$ attributes align : 16 :: qy23
              !dir$ attributes align : 16 :: qy24
              !dir$ attributes align : 16 :: qy25
              !dir$ attributes align : 16 :: qy26
              !dir$ attributes align : 16 :: qy27
              !dir$ attributes align : 16 :: p01
              !dir$ attributes align : 16 :: p02
              !dir$ attributes align : 16 :: p03
              !dir$ attributes align : 16 :: p04
              !dir$ attributes align : 16 :: p05
              !dir$ attributes align : 16 :: p06
              !dir$ attributes align : 16 :: q01
              !dir$ attributes align : 16 :: q02
              !dir$ attributes align : 16 :: q03
              !dir$ attributes align : 16 :: q04
              !dir$ attributes align : 16 :: q05
              !dir$ attributes align : 16 :: p11
              !dir$ attributes align : 16 :: p12
              !dir$ attributes align : 16 :: p13
              !dir$ attributes align : 16 :: p14
              !dir$ attributes align : 16 :: p15
              !dir$ attributes align : 16 :: p16
              !dir$ attributes align : 16 :: q11
              !dir$ attributes align : 16 :: q12
              !dir$ attributes align : 16 :: q13
              !dir$ attributes align : 16 :: q14
              !dir$ attributes align : 16 :: q15
            
              type(XMM2r8_t),   automatic :: ax,down
              type(XMM2r8_t),   automatic :: prod,resj
              type(XMM2r8_t),   automatic :: r0,r1
              type(XMM2r8_t),   automatic :: up,w
              type(XMM2r8_t),   automatic :: wsq,xden,xnum 
              type(XMM2r8_t),   automatic :: xy,z,zsq
              type(XMM2r8_t),   automatic :: t0,t1
              type(XMM2r8_t),   automatic :: pi2ax,t2
              type(XMM2r8_t),   automatic :: t3
              type(Mask2_t),    automatic :: m0,m1
              !dir$ attributes align : 16 :: ax
              !dir$ attributes align : 16 :: down
              !dir$ attributes align : 16 :: prod
              !dir$ attributes align : 16 :: resj
              !dir$ attributes align : 16 :: r0
              !dir$ attributes align : 16 :: r1
              !dir$ attributes align : 16 :: up
              !dir$ attributes align : 16 :: w
              !dir$ attributes align : 16 :: wsq
              !dir$ attributes align : 16 :: xden
              !dir$ attributes align : 16 :: xnum 
              !dir$ attributes align : 16 :: xy
              !dir$ attributes align : 16 :: z
              !dir$ attributes align : 16 :: zsq
              !dir$ attributes align : 16 :: t0
              !dir$ attributes align : 16 :: t1
              !dir$ attributes align : 16 :: pi2ax
              !dir$ attributes align : 16 :: t2
              !dir$ attributes align : 16 :: t3
              

              ax.v   = abs(arg.v)
              m0.m   = (arg.v<=zero.v)
              pi2ax.v= pi2.v/ax.v
              m1.m   = (xmax.v<ax.v)
              m0.m = (eight.v<ax.v)
              m1.m = (ax.v<=xsmall.v)
              if(all(m0.m)) goto 800
              

             
!               /*
!                            !  Calculate J0 for appropriate interval, preserving
!                            !  accuracy near the zero of J0.
!                        
! */                     
              m0.m  = (ax.v<=four.v)
              zsq.v = ax.v*ax.v
              where(m0.m)
                  xnum.v = pj05.v*zsq.v+ &
                           pj06.v*zsq.v+ &
                           pj07.v
                  xden.v = zsq.v+qj05.v
                  xnum.v = xnum.v*zsq.v+pj01.v
                  xden.v   = xden.v*zsq.v+qj01.v
                  xnum.v = xnum.v*zsq.v+pj02.v
                  xden.v   = xden.v*zsq.v+qj02.v
                  xnum.v = xnum.v*zsq.v+pj03.v
                  xden.v   = xden.v*zsq.v+qj03.v
                  xnum.v = xnum.v*zsq.v+pj04.v
                  xden.v   = xden.v*zsq.v+qj04.v
                  t0.v   = ax.v-xj01.v/two56.v
                  t1.v   = ax.v+xj0.v
                  prod.v = (t0.v-xj02.v)*t1.v
              else where 
                  wsq.v  = one.v-zsq.v/sixty4.v
                  xnum.v = pj17.v*wsq.v+ &
                           pj18.v
                  xden.v = wsq.v+qj17.v
                  xnum.v = xnum.v*wsq.v+pj11.v
                  xden.v = xden.v*wsq.v+qj11.v
                  xnum.v = xnum.v*wsq.v+pj12.v
                  xden.v = xden.v*wsq.v+qj12.v
                  xnum.v = xnum.v*wsq.v+pj13.v
                  xden.v = xden.v*wsq.v+qj13.v
                  xnum.v = xnum.v*wsq.v+pj14.v
                  xden.v = xden.v*wsq.v+qj14.v
                  xnum.v = xnum.v*wsq.v+pj15.v
                  xden.v = xden.v*wsq.v+qj15.v
                  xnum.v = xnum.v*wsq.v+pj16.v
                  xden.v = xden.v*wsq.v+qj16.v
                  t0.v   = ax.v-xj11.v/two56.v
                  t1.v   = ax.v+xj1.v
                  prod.v = (t0.v*t1.v)-xj12.v
              end where 
              val.v  = prod.v*xnum.v/xden.v
              if(jint==0) return
!    /*
!                          Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x),
!                          !  where xn is a zero of Y0.
!                       */      
              m0.m  = (ax.v<=three.v)       
              m1.m  = (ax.v<=five5.v)
              where(m0.m)
                  up.v = (ax.v-xy01.v/two56.v)-xy02.v
                  xy.v = xy0.v
              else where(m1.m)
                  up.v = (ax.v-xy11.v/two56.v)-xy12.v
                  xy.v = xy1.v
              else where 
                  up.v = (ax.v-xy21.v/two56.v)-xy22.v
                  xy.v = xy1.v
              end where 
              down.v   = ax.v*xy.v
              t0.v     = abs(up.v)
              t1.v     = p17.v*down.v
              m0.m     = (t0.v<t1.v)
              where(m0.m)
                  w.v   = up.v/down.v
                  wsq.v = w.v*w.v
                  xnum.v= plg1.v
                  xden.v= wsq.v+qlg1.v
                  xnum.v= xnum.v*wsq.v+plg2.v
                  xden.v= xden.v*wsq.v+qlg2.v
                  xnum.v= xnum.v*wsq.v+plg3.v
                  xden.v= xden.v*wsq.v+qlg3.v
                  xnum.v= xnum.v*wsq.v+plg4.v
                  xden.v= xden.v*wsq.v+qlg4.v
                  t0.v  = xnum.v/xden.v
                  t1.v  = pi2.v*val.v
                  resj.v= t1.v*w.v*t0.v
              else where 
                  t0.v  = ax.v/xy.v
                  t1.v  = pi2.v*val.v
                  resj.v= t1.v*log(t0.v)
              end where 
!               /*
!                           Now calculate Y0 for appropriate interval, preserving
!                           !  accuracy near the zero of Y0.
!                       */
              m0.m = (ax.v<=three.v)
              m1.m = (ax.v<=five5.v)
              where(m0.m)
                  xnum.v = py06.v*zsq.v+ &
                           py01.v
                  xden.v = zsq.v+qy01.v
                  xnum.v = xnum.v*zsq.v+py02.v
                  xden.v = xden.v*zsq.v+qy02.v
                  xnum.v = xnum.v*zsq.v+py03.v
                  xden.v = xden.v*zsq.v+qy03.v
                  xnum.v = xnum.v*zsq.v+py04.v
                  xden.v = xden.v*zsq.v+qy04.v
                  xnum.v = xnum.v*zsq.v+py05.v
                  xden.v = xden.v*zsq.v+qy05.v
              else where(m1.m)
                  xnum.v = py17.v*zsq.v+ &
                           py11.v
                  xden.v = zsq.v+qy11.v
                  xnum.v = xnum.v*zsq.v+py12.v
                  xden.v = xden.v*zsq.v+qy12.v
                  xnum.v = xnum.v*zsq.v+py13.v
                  xden.v = xden.v*zsq.v+qy13.v
                  xnum.v = xnum.v*zsq.v+py14.v
                  xden.v = xden.v*zsq.v+qy14.v
                  xnum.v = xnum.v*zsq.v+py15.v
                  xden.v = xden.v*zsq.v+qy15.v
                  xnum.v = xnum.v*zsq.v+py16.v
                  xden.v = xden.v*zsq.v+qy16.v
              else where 
                  xnum.v = py28.v*zsq.v+ &
                           py21.v
                  xden.v = zsq.v+qy21.v
                  xnum.v = xnum.v*zsq.v+py22.v
                  xden.v = xnum.v*zsq.v+qy22.v
                  xnum.v = xnum.v*zsq.v+py23.v
                  xden.v = xnum.v*zsq.v+qy23.v
                  xnum.v = xnum.v*zsq.v+py24.v
                  xden.v = xnum.v*zsq.v+qy24.v
                  xnum.v = xnum.v*zsq.v+py25.v
                  xden.v = xnum.v*zsq.v+qy25.v
                  xnum.v = xnum.v*zsq.v+py26.v
                  xden.v = xnum.v*zsq.v+qy26.v
                  xnum.v = xnum.v*zsq.v+py27.v
                  xden.v = xnum.v*zsq.v+qy27.v
              end where 
              t0.v  = xnum.v/xden.v
              t1.v  = up.v*down.v
              val.v = resj.v+t1.v*t1.v
              return
800           z.v   = eight.v/ax.v
              w.v   = ax.v/twopi.v
              t1.v  = int(w.v,kind=i8)
              w.v   = real(t1.v,kind=dp)+oneov8.v
              t0.v  = w.v*twopi2.v
              t1.v  = ax.v-w.v
              w.v   = t1.v*twopi1.v-t0.v
              zsq.v = z.v*z.v
              xnum.v= p05.v*zsq.v+ &
                      p06.v
              xden.v= zsq.v+q05.v
              up.v  = p15.v*zsq.v+ &
                      p16.v
              down.v= zsq.v+q15.v
              xnum.v= xnum.v*zsq.v+p01.v
              xden.v= xden.v*zsq.v+q01.v
              xnum.v= xnum.v*zsq.v+p02.v
              xden.v= xden.v*zsq.v+q02.v
              xnum.v= xnum.v*zsq.v+p03.v
              xden.v= xden.v*zsq.v+q03.v
              xnum.v= xnum.v*zsq.v+p04.v
              xden.v= xden.v*zsq.v+q04.v
              r0.v  = xnum.v/xden.v
              t1.v  = cos(w.v)
              r1.v  = up.v/down.v
              t0.v  = sqrt(pi2.v/ax.v)
              t2.v  = sin(w.v)
              t3.v  = z.v*r1.v
              if(jint==0) then
                  val.v = t0.v*r0.v*t1.v- &
                          t3.v*t2.v
              else
                  val.v = t0.v*r0.v*t2.v+ &
                          t3.v*t1.v
              end if
        end subroutine caljy0_xmm2r8
        
#if 0        

!
!! CALJY1 computes various J1 and Y1 Bessel functions.
!
!  Discussion:
!
!    This routine computes first-order Bessel functions of the first and
!    second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX
!    for Y1, and |X| <= XMAX for J1.
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
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 0, ARG
!    must satisfy
!     -XMAX < ARG < XMAX;
!    If JINT = 1, then ARG must satisfy
!      0 < ARG < XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    0, RESULT = J1(x);
!    1, RESULT = Y1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    0, J1(x);
!    1, Y1(x);  



          subroutine caljy1_xmm2r8(arg,val,jint)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: caljy1_xmm2r8
              !dir$ attributes forceinline :: caljy1_xmm2r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_xmm2r8  
              use mod_vectypes, only : XMM2i8_t
              type(XMM2r8_t),   intent(in)   :: arg
              type(XMM2r8_t),   intent(out)  :: val
              integer(kind=i4), intent(in)   :: jint 
              !dir$ attributes align : 16 :: eight
              !dir$ attributes align : 16 :: four
              !dir$ attributes align : 16 :: half
              !dir$ attributes align : 16 :: throv8
              !dir$ attributes align : 16 :: pi2
              !dir$ attributes align : 16 :: p17
              !dir$ attributes align : 16 :: twopi
              !dir$ attributes align : 16 :: zero
              !dir$ attributes align : 16 :: twopi1
              !dir$ attributes align : 16 :: twopi2
              !dir$ attributes align : 16 :: two56
              !dir$ attributes align : 16 :: rtpi2
              !dir$ attributes align : 16 :: xmax
              !dir$ attributes align : 16 :: xsmall
              !dir$ attributes align : 16 :: xinf
              !dir$ attributes align : 16 :: xj0
              !dir$ attributes align : 16 :: xj1
              !dir$ attributes align : 16 :: xy0
              !dir$ attributes align : 16 :: xy1
              !dir$ attributes align : 16 :: xj01
              !dir$ attributes align : 16 :: xj02
              !dir$ attributes align : 16 :: xj11
              !dir$ attributes align : 16 :: xj12
              !dir$ attributes align : 16 :: xy01
              !dir$ attributes align : 16 :: xy02
              !dir$ attributes align : 16 :: xy11
              !dir$ attributes align : 16 :: xy12
              !dir$ attributes align : 16 :: ax
              !dir$ attributes align : 16 :: down
              !dir$ attributes align : 16 :: prod
              !dir$ attributes align : 16 :: resj
              !dir$ attributes align : 16 :: r0
              !dir$ attributes align : 16 :: r1
              !dir$ attributes align : 16 :: up
              !dir$ attributes align : 16 :: w
              !dir$ attributes align : 16 :: wsq
              !dir$ attributes align : 16 :: xden
              !dir$ attributes align : 16 :: xnum
              !dir$ attributes align : 16 :: t0
              !dir$ attributes align : 16 :: t1
              !dir$ attributes align : 16 :: z
              !dir$ attributes align : 16 :: zsq
              !dir$ attributes align : 16 :: t2
              !dir$ attributes align : 16 :: t3
              type(XMM2r8_t),   parameter    :: eight = XMM2r8_t(8.0e+0_dp);
              type(XMM2r8_t),   parameter    :: four  = XMM2r8_t(4.0e+0_dp);
              type(XMM2r8_t),   parameter    :: half  = XMM2r8_t(0.5_dp);
              type(XMM2r8_t),   parameter    :: throv8= XMM2r8_t(0.375_dp);
              type(XMM2r8_t),   parameter    :: pi2   = XMM2r8_t(6.3661977236758134308e-1_dp);
              type(XMM2r8_t),   parameter    :: p17   = XMM2r8_t(1.716e-1_dp);
              type(XMM2r8_t),   parameter    :: twopi = XMM2r8_t(6.2831853071795864769e+0_dp);
              type(XMM2r8_t),   parameter    :: zero  = XMM2r8_t(0.0_dp);
              type(XMM2r8_t),   parameter    :: twopi1= XMM2r8_t(6.28125e+0_dp);
              type(XMM2r8_t),   parameter    :: twopi2= XMM2r8_t(1.9353071795864769253e-3_dp);
              type(XMM2r8_t),   parameter    :: two56 = XMM2r8_t(256.0e+0_dp);
              type(XMM2r8_t),   parameter    :: rtpi2 = XMM2r8_t(7.9788456080286535588e-1_dp);
              type(XMM2r8_t),   parameter    :: xmax  = XMM2r8_t(1.07e+9_dp);
              type(XMM2r8_t),   parameter    :: xsmall= XMM2r8_t(9.31e-10_dp);
              type(XMM2r8_t),   parameter    :: xinf  = XMM2r8_t(1.7e+38_dp);  
              type(XMM2r8_t),   parameter    :: xj0   = XMM2r8_t(3.8317059702075123156e+0_dp);
              type(XMM2r8_t),   parameter    :: xj1   = XMM2r8_t(7.0155866698156187535e+0_dp);
              type(XMM2r8_t),   parameter    :: xy0   = XMM2r8_t(2.1971413260310170351e+0_dp);
              type(XMM2r8_t),   parameter    :: xy1   = XMM2r8_t(5.4296810407941351328e+0_dp);
              type(XMM2r8_t),   parameter    :: xj01  = XMM2r8_t(981.0e+0_dp);
              type(XMM2r8_t),   parameter    :: xj02  = XMM2r8_t(-3.2527979248768438556e-4_dp);
              type(XMM2r8_t),   parameter    :: xj11  = XMM2r8_t(1796.0e+0_dp);
              type(XMM2r8_t),   parameter    :: xj12  = XMM2r8_t(-3.8330184381246462950e-5_dp);
              type(XMM2r8_t),   parameter    :: xy01  = XMM2r8_t(562.0e+0_dp);
              type(XMM2r8_t),   parameter    :: xy02  = XMM2r8_t(1.8288260310170351490e-3_dp);
              type(XMM2r8_t),   parameter    :: xy11  = XMM2r8_t(1390.0e+0_dp);
              type(XMM2r8_t),   parameter    :: xy12  = XMM2r8_t(-6.4592058648672279948e-6_dp);
              type(XMM2r8_t),   automatic    :: ax,down
              type(XMM2r8_t),   automatic    :: prod,resj
              type(XMM2r8_t),   automatic    :: r0,r1
              type(XMM2r8_t),   automatic    :: up,w
              type(XMM2r8_t),   automatic    :: wsq,xden
              type(XMM2r8_t),   automatic    :: xnum,t0
              type(XMM2r8_t),   automatic    :: t1,z
              type(XMM2r8_t),   automatic    :: zsq,t2
              type(XMM2r8_t),   automatic    :: t3
              type(XMM2i8_t),   automatic    :: ti
              type(Mask2_t),    automatic    :: m0,m1,m2,m3
#if (GMS_EXPLICIT_VECTORIZE) == 1
               integer(kind=i4) :: j
#endif   
              ax.v  = arg.v
              m3.m  = (xmax.v<ax.v)
              t0.v  = ax.v*xinf.v
              m0.m  = (arg.v<=zero.v)
              m1.m  = (arg.v<half.v)
              m2.m  = (t0.v<pi2.v)
              if(all(m0.m).or.                 &
                 (all(m1.m).and.all(m2.m))) then
                    val.v = -xinf.v
                    return
              else if(all(m3.m)) then
                    val.v = zero.v
                    return
              end if
              m0.m  = (eight.v<ax.v)
              m1.m  = (ax.v<=xsmall.v)
              if(all(m0.m)) then
                  goto 800
              else if(all(m1.m)) then
                   if(jint==0) then
                       val.v = arg.v*half.v
                       return
                   else
                       val.v = -pi2.v/ax.v 
                       return
                   end if
              end if 
              ! /*
              !                Calculate J1 for appropriate interval, preserving
              !                !  accuracy near the zero of J1.
              !           */
#if (GMS_EXPLICIT_VECTORIZE) == 1
           
               !dir$ loop_count(2)
               !dir$ vector aligned
               !dir$ vector vectorlength(8)
               !dir$ vector always
               do j=0,1
               
                   zsq.v(j) = ax.v(j)*ax.v(j)
                   m0.m(j)  = (ax.v(j)<=four.v(j))
                   if(all(m0.m(j))) then
                         xnum.v(j) = caljy1_pj0(6).v(j)*zsq.v(j)+ &
                                  caljy1_pj0(5).v(j)*zsq.v(j)+ &
                                  caljy1_pj0(4).v(j)
                         xden.v(j) = zsq.v(j)+caljy1_qj0(4).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj0(0).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj0(0).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj0(1).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj0(1).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj0(2).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj0(2).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj0(3).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj0(3).v(j)
                         t0.v(j)   = ax.v(j)-(xj01.v(j)/two56.v(j))
                         t1.v(j)   = ax.v(j)+xj0.v(j)
                         prod.v(j) = (t0.v(j)-xj02.v(j))*t1.v(j)
                   else
                         xnum.v(j) = caljy1_pj1(0).v(j)
                         xden.v(j) = (zsq.v(j)+caljy1_qj1(6).v(j))* &
                                     (zsq.v(j)+caljy1_qj1(0).v(j))
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj1(1).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj1(1).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj1(2).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj1(2).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj1(3).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj1(3).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj1(4).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj1(4).v(j)
                         xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_pj1(5).v(j)
                         xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qj1(5).v(j)
                         t0.v(j)   = xnum.v(j)*(ax.v(j)-eight.v(j))
                         t1.v(j)   = (ax.v(j)+eight.v(j))+caljy1_pj1(6).v(j)
                         xnum.v(j) = t0.v(j)*t1.v(j)
                         t0.v(j)   = xnum.v(j)*(ax.v(j)-four.v(j))
                         t1.v(j)   = (ax.v(j)+four.v(j))+caljy1_pj1(7).v(j)
                         xnum.v(j) = t0.v(j)*t1.v(j)
                         t0.v(j)   = ax.v(j)-(xj11.v(j)/two56.v(j))-xj12.v(j)
                         t1.v(j)   = ax.v(j)+xj1.v(j)
                         prod.v(j) = arg.v(j)*t0.v(j)*t1.v(j)
                  endif
                  val.v(j) = prod.v(j)*(xnum.v(j)/xden.v(j))
                  if(jint==0) return
              ! /*
              !               Calculate Y1.  First find RESJ = pi/2 ln(x/xn) J1(x),
              !               !  where xn is a zero of Y1.
              !           */
                  m0.m(j) = (ax.v(j)<=four.v(j))
                  if(all(m0.m(j))) then
                       t0.v(j) = ax.v(j)-(xy01.v(j)/two56.v(j))
                       up.v(j) = t0.v(j)-xy02.v(j)
                       xy.v(j) = xy0.v(j)
                  else
                       t0.v(j) = ax.v(j)-(xy11.v(j)/two56.v(j))
                       up.v(j) = t0.v(j)-xy12.v(j)
                       xy.v(j) = xy01.v(j)
                  endif
                  down.v(j)   = ax.v(j)+xy.v(j)
                  t1.v(j)     = p17.v(j)*down.v(j))
                  m0.m(j)     = (abs(up.v(j))<t1.v(j))
                  if(all(m0.m(j))) then
                      w.v(j)   = up.v(j)/down.v(j)
                      wsq.v(j) = w.v*w.v(j)
                      xnum.v(j)= caljy1_plg(0).v(j)
                      xden.v(j)= wsq.v(j)+caljy1_qlg(0).v(j)
                      xnum.v(j)= xnum.v(j)*wsq.v(j)+caljy1_plg(1).v(j)
                      xden.v(j)= xden.v(j)*wsq.v(j)+caljy1_qlg(1).v(j)
                      xnum.v(j)= xnum.v(j)*wsq.v(j)+caljy1_plg(2).v(j)
                      xden.v(j)= xden.v(j)*wsq.v(j)+caljy1_qlg(2).v(j)
                      xnum.v(j)= xnum.v(j)*wsq.v(j)+caljy1_plg(3).v(j)
                      xden.v(j)= xden.v(j)*wsq.v(j)+caljy1_qlg(3).v(j)
                      t0.v(j)  = w.v(j)*(xnum.v(j)/xden.v(j))
                      t1.v(j)  = pi2.v(j)*val.v(j)
                      resj.v(j)= t0.v(j)*t1.v(j)
                  else
                      t0.v(j)  = log(ax.v(j)/xy.v(j))
                      resj.v(j)= pi12.v(j)*val.v(j)*t0.v(j)
                  end if
              !  /*
              !               Now calculate Y1 for appropriate interval, preserving
              !               !  accuracy near the zero of Y1.
              !           */   
                  m0.m(j) = (ax.v(j)<=four.v(j))
                  if(all(m0.m(j))) then
                      xnum.v(j) = caljy1_py0(6).v(j)*zsq.v(j)+ &
                                  caljy1_py0(0).v(j)         
                      xden.v(j) = zsq.v(j)+caljy1_qy0(0).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py0(1).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy0(1).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py0(2).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy0(2).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py0(3).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy0(3).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py0(4).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy0(4).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py0(5).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy0(5).v(j)  
                  else
                      xnum.v(j) = caljy1_py1(8).v(j)*zsq.v(j)+ &
                                  caljy1_py1(0).v(j)
                      xden.v(j) = zsq.v(j)+caljy1_qy1(0).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(1).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(1).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(2).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(2).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(3).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(3).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(4).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(4).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(5).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(5).v(j)
                      xnum.v(j) = xnum.v(j)*zsq.v(j)+caljy1_py1(6).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(6).v(j)
                      xnum.v(j) = xnum.v*zsq.v+caljy1_py1(7).v(j)
                      xden.v(j) = xden.v(j)*zsq.v(j)+caljy1_qy1(7).v(j)
              end if
              t0.v(j) = xnum.v(j)/xden.v(j)
              t1.v(j) = up.v(j)*(down.v(j)/ax.v(j))
              val.v(j)= t0.v(j)*t1.v(j)+resj.v(j)
              return
800           z.v(j)  = eight.v(j)/ax.v(j)
              t0.v(j) = ax.v(j)/twopi.v(j)
              ti.v(j) = int(t0.v(j),kind=i8)
              w.v(j)  = real(ti.v(j),kind=dp)*throv8.v(j)
              t0.v(j) = ax.v(j)-w.v(j)
              w.v(j)  = t0.v(j)*twopi1.v(j)-w.v(j)*twopi2.v(j)
              zsq.v(j)= z.v(j)*z.v(j)
              xnum.v(j)=caljy1_p0(5).v(j)
              xden.v(j)=zsq.v(j)+caljy1_q0(5).v
              up.v(j)  = caljy1_p1(5).v(j)
              xnum.v(j)= xnum.v(j)*zsq.v(j)+caljy1_p0(0).v(j)
              t0.v(j)  = rtpi2.v(j)/sqrt(ax.v(j))
              xden.v(j)= xden.v(j)*zsq.v(j)+caljy1_q0(0).v(j)
              xnum.v(j)= xnum.v(j)*zsq.v(j)+caljy1_p0(1).v(j)
              xden.v(j)= xden.v(j)*zsq.v(j)+caljy1_q0(1).v(j)
              xnum.v(j)= xnum.v(j)*zsq.v(j)+caljy1_p0(2).v(j)
              xden.v(j)= xden.v(j)*zsq.v(j)+caljy1_q0(2).v(j)
              xnum.v(j)= xnum.v(j)*zsq.v(j)+caljy1_p0(3).v(j)
              xden.v(j)= xden.v(j)*zsq.v(j)+caljy1_q0(3).v(j)
              xnum.v(j)= xnum.v(j)*zsq.v(j)+caljy1_p0(4).v(j)
              t1.v(j)  = sin(w.v(j))
              xden.v(j)= xden.v(j)*zsq.v(j)+caljy1_q0(4).v(j)
              r0.v(j)  = xnum.v(j)/xden.v(j)
              r1.v(j)  = up.v(j)/down.v(j)
              t2.v(j)  = cos(w.v(j))
              t3.v(j)  = z.v(j)*r1.v(j)
              if(jint==1) then
                   val.v = t0.v(j)*r0.v(j)*t2.v(j)- &
                           t3.v(j)*t1.v(j)
              else
                   val.v(j) = t0.v(j)*r0.v(j)*t1.v(j)- &
                           t3.v(j)*t2.v(j)
              end if
              m0.m(j)  = (arg.v(j)<zero.v(j))
              if(jint==0.and.all(m0.m(j))) val.v(j) = -val.v(j)
           end do
#else                 
              zsq.v = ax.v*ax.v
              m0.m  = (ax.v<=four.v)
              if(all(m0.m)) then
                   xnum.v = caljy1_pj0(6).v*zsq.v+ &
                                  caljy1_pj0(5).v*zsq.v+ &
                                  caljy1_pj0(4).v
                   xden.v = zsq.v+caljy1_qj0(4).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj0(0).v
                   xden.v = xden.v*zsq.v+caljy1_qj0(0).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj0(1).v
                   xden.v = xden.v*zsq.v+caljy1_qj0(1).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj0(2).v
                   xden.v = xden.v*zsq.v+caljy1_qj0(2).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj0(3).v
                   xden.v = xden.v*zsq.v+caljy1_qj0(3).v
                   t0.v   = ax.v-(xj01.v/two56.v)
                   t1.v   = ax.v+xj0.v
                   prod.v = (t0.v-xj02.v)*t1.v
              else
                   xnum.v = caljy1_pj1(0).v
                   xden.v = (zsq.v+caljy1_qj1(6).v)* &
                            (zsq.v+caljy1_qj1(0).v)
                   xnum.v = xnum.v*zsq.v+caljy1_pj1(1).v
                   xden.v = xden.v*zsq.v+caljy1_qj1(1).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj1(2).v
                   xden.v = xden.v*zsq.v+caljy1_qj1(2).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj1(3).v
                   xden.v = xden.v*zsq.v+caljy1_qj1(3).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj1(4).v
                   xden.v = xden.v*zsq.v+caljy1_qj1(4).v
                   xnum.v = xnum.v*zsq.v+caljy1_pj1(5).v
                   xden.v = xden.v*zsq.v+caljy1_qj1(5).v
                   t0.v   = xnum.v*(ax.v-eight.v)
                   t1.v   = (ax.v+eight.v)+caljy1_pj1(6).v
                   xnum.v = t0.v*t1.v
                   t0.v   = xnum.v*(ax.v-four.v)
                   t1.v   = (ax.v+four.v)+caljy1_pj1(7).v
                   xnum.v = t0.v*t1.v
                   t0.v   = ax.v-(xj11.v/two56.v)-xj12.v
                   t1.v   = ax.v+xj1.v
                   prod.v = arg.v*t0.v*t1.v
              endif
              val.v = prod.v*(xnum.v/xden.v)
              if(jint==0) return
              ! /*
              !               Calculate Y1.  First find RESJ = pi/2 ln(x/xn) J1(x),
              !               !  where xn is a zero of Y1.
              !           */
              m0.m = (ax.v<=four.v)
              if(all(m0.m)) then
                  t0.v = ax.v-(xy01.v/two56.v)
                  up.v = t0.v-xy02.v
                  xy.v = xy0.v
              else
                  t0.v = ax.v-(xy11.v/two56.v)
                  up.v = t0.v-xy12.v
                  xy.v = xy01.v
              endif
              down.v   = ax.v+xy.v
              t1.v     = p17.v*down.v
              m0.m     = (abs(up.v)<t1.v)
              if(all(m0.m)) then
                  w.v   = up.v/down.v
                  wsq.v = w.v*w.v
                  xnum.v= caljy1_plg(0).v
                  xden.v= wsq.v+caljy1_qlg(0).v
                  xnum.v= xnum.v*wsq.v+caljy1_plg(1).v
                  xden.v= xden.v*wsq.v+caljy1_qlg(1).v
                  xnum.v= xnum.v*wsq.v+caljy1_plg(2).v
                  xden.v= xden.v*wsq.v+caljy1_qlg(2).v
                  xnum.v= xnum.v*wsq.v+caljy1_plg(3).v
                  xden.v= xden.v*wsq.v+caljy1_qlg(3).v
                  t0.v  = w.v*(xnum.v/xden.v)
                  t1.v  = pi2.v*val.v
                  resj.v= t0.v*t1.v
              else
                  t0.v  = log(ax.v/xy.v)
                  resj.v= pi12.v*val.v*t0.v
              end if
              !  /*
              !               Now calculate Y1 for appropriate interval, preserving
              !               !  accuracy near the zero of Y1.
              !           */   
              m0.m = (ax.v<=four.v)
              if(all(m0.m)) then
                   xnum.v = caljy1_py0(6).v*zsq.v+ &
                            caljy1_py0(0).v         
                   xden.v = zsq.v+caljy1_qy0(0).v
                   xnum.v = xnum.v*zsq.v+caljy1_py0(1).v
                   xden.v = xden.v*zsq.v+caljy1_qy0(1).v
                   xnum.v = xnum.v*zsq.v+caljy1_py0(2).v
                   xden.v = xden.v*zsq.v+caljy1_qy0(2).v
                   xnum.v = xnum.v*zsq.v+caljy1_py0(3).v
                   xden.v = xden.v*zsq.v+caljy1_qy0(3).v
                   xnum.v = xnum.v*zsq.v+caljy1_py0(4).v
                   xden.v = xden.v*zsq.v+caljy1_qy0(4).v
                   xnum.v = xnum.v*zsq.v+caljy1_py0(5).v
                   xden.v = xden.v*zsq.v+caljy1_qy0(5).v  
              else
                   xnum.v = caljy1_py1(8).v*zsq.v+ &
                            caljy1_py1(0).v
                   xden.v = zsq.v+caljy1_qy1(0).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(1).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(1).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(2).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(2).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(3).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(3).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(4).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(4).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(5).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(5).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(6).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(6).v
                   xnum.v = xnum.v*zsq.v+caljy1_py1(7).v
                   xden.v = xden.v*zsq.v+caljy1_qy1(7).v
              end if
              t0.v = xnum.v/xden.v
              t1.v = up.v*(down.v/ax.v)
              val.v= t0.v*t1.v+resj.v
              return
800           z.v  = eight.v/ax.v
              t0.v = ax.v/twopi.v
              ti.v = int(t0.v,kind=i8)
              w.v  = real(ti.v,kind=dp)*throv8.v
              t0.v = ax.v-w.v
              w.v  = t0.v*twopi1.v-w.v*twopi2.v
              zsq.v= z.v*z.v
              xnum.v=caljy1_p0(5).v
              xden.v=zsq.v+caljy1_q0(5).v
              up.v  = caljy1_p1(5).v
              xnum.v= xnum.v*zsq.v+caljy1_p0(0).v
              t0.v  = rtpi2.v/sqrt(ax.v)
              xden.v= xden.v*zsq.v+caljy1_q0(0).v
              xnum.v= xnum.v*zsq.v+caljy1_p0(1).v
              xden.v= xden.v*zsq.v+caljy1_q0(1).v
              xnum.v= xnum.v*zsq.v+caljy1_p0(2).v
              xden.v= xden.v*zsq.v+caljy1_q0(2).v
              xnum.v= xnum.v*zsq.v+caljy1_p0(3).v
              xden.v= xden.v*zsq.v+caljy1_q0(3).v
              xnum.v= xnum.v*zsq.v+caljy1_p0(4).v
              t1.v  = sin(w.v)
              xden.v= xden.v*zsq.v+caljy1_q0(4).v
              r0.v  = xnum.v/xden.v
              r1.v  = up.v/down.v
              t2.v  = cos(w.v)
              t3.v  = z.v*r1.v
              if(jint==1) then
                   val.v = t0.v*r0.v*t2.v- &
                           t3.v*t1.v
              else
                   val.v = t0.v*r0.v*t1.v- &
                           t3.v*t2.v
              end if
              m0.m  = (arg.v<zero.v)
              if(jint==0.and.all(m0.m)) val.v = -val.v
#endif      

       end subroutine caljy1_xmm2r8
       
    
#endif          
     
      


end module 