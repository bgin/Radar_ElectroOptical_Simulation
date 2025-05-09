
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

module unit_test_atmos_refraction

       use mod_kinds, only : i4, sp, dp 
       use atmos_refraction 
       
       ! Refractive indices short data [Temp (C), Rel Humiditiy (%), Pressure (kPa), Wavelength (nm), n-Cidor,n-Edlen, diff(Cidor-Edlen)]
       character(len=64),      parameter :: row1 =  "20,0,101.325,633,1.000271800,1.000271799,0.1"
	   character(len=64),      parameter :: row2 =  "20,0,60,633,1.000160924,1.000160920,0.4"
       character(len=64),      parameter :: row3 =  "20,0,120,633,1.000321916,1.000321918,-0.2"
       character(len=64),      parameter :: row4 =  "50,0,100,633,1.000243285,1.000243270,1.5"
       character(len=64),      parameter :: row5 =  "5,0,100,633,1.000282756,1.000282750,0.6"
       character(len=64),      parameter :: row6 =  "-40,0,100,633,1.000337580,1.000337471,10.9"
       character(len=64),      parameter :: row7 =  "50,100,120,633,1.000287924,1.000287864,6.0"


       character(len=1),        parameter, private :: dcol = ":"


       
       contains 

subroutine unit_test_n_refract_tht_f243_r4()
              character(len=128), automatic :: filename 
              real(kind=sp),      automatic :: n 
              real(kind=sp),      automatic :: n0 
              real(kind=sp),      automatic :: z 
              real(kind=sp),      automatic :: z0 
              real(kind=sp),      automatic :: r 
              real(kind=sp),      automatic :: R0  
              real(kind=sp),      automatic :: phi 
              real(kind=sp),      automatic :: phi0 
              real(kind=sp),      automatic :: n_over_tht 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)            :: t
              character(len=8)             :: d
              character(len=50), parameter :: header = "[TEST #1: n_refract_tht_f243_r4 -- START]"
              character(len=48), parameter :: footer = "[TEST #1: n_refract_tht_f243_r4 -- END]"
              ! Exec code ...
                          
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , " : " , t , filename , lstart  
              print*,   header
              n           = 1.000271800_sp 
              n0          = 1.000282756_sp 
              z           = 0.5_sp 
              z0          = 0.42_sp 
              r           = 25000.0_sp
              R0          = 15000.0_sp
              phi         = 0.35_sp 
              phi0        = 0.15_sp 
              n_over_tht  = n_refract_tht_f243_r4(n,n0,z,z0,r,R0,phi,phi0)
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , ":" , t , filename , lend
              print*,  footer
end subroutine unit_test_n_refract_tht_f243_r4

subroutine unit_test_n_refract_tht_f243_r8()
              character(len=128), automatic :: filename 
              real(kind=dp),      automatic :: n 
              real(kind=dp),      automatic :: n0 
              real(kind=dp),      automatic :: z 
              real(kind=dp),      automatic :: z0 
              real(kind=dp),      automatic :: r 
              real(kind=dp),      automatic :: R0  
              real(kind=dp),      automatic :: phi 
              real(kind=dp),      automatic :: phi0 
              real(kind=dp),      automatic :: n_over_tht 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)            :: t
              character(len=8)             :: d
              character(len=50), parameter :: header = "[TEST #2: n_refract_tht_f243_r8 -- START]"
              character(len=48), parameter :: footer = "[TEST #2: n_refract_tht_f243_r8 -- END]"
              ! Exec code ...
                          
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , " : " , t , filename , lstart
              print*,  header
              n           = 1.000271800_dp 
              n0          = 1.000282756_dp 
              z           = 0.5_dp 
              z0          = 0.42_dp 
              r           = 25000.0_dp
              R0          = 15000.0_dp
              phi         = 0.35_dp 
              phi0        = 0.15_dp 
              n_over_tht  = n_refract_tht_f243_r8(n,n0,z,z0,r,R0,phi,phi0)
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0=",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend 
              print*,  footer
end subroutine unit_test_n_refract_tht_f243_r8

subroutine unit_test_n_refract_phi_f243_r4()
              character(len=128), automatic :: filename 
              real(kind=sp),      automatic :: n 
              real(kind=sp),      automatic :: n0 
              real(kind=sp),      automatic :: z 
              real(kind=sp),      automatic :: z0 
              real(kind=sp),      automatic :: r 
              real(kind=sp),      automatic :: R0  
              real(kind=sp),      automatic :: phi 
              real(kind=sp),      automatic :: phi0 
              real(kind=sp),      automatic :: n_over_tht 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)            :: t
              character(len=8)             :: d
              character(len=50), parameter :: header = "[TEST #3: n_refract_phi_f243_r4 -- START]"
              character(len=48), parameter :: footer = "[TEST #3: n_refract_phi_f243_r4 -- END]"
              ! Exec code ...
                          
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              n           = 1.000271800_sp 
              n0          = 1.000282756_sp 
              z           = 0.5_sp 
              z0          = 0.42_sp 
              r           = 25000.0_sp
              R0          = 15000.0_sp
              phi         = 0.47_sp 
              phi0        = 0.25_sp 
              n_over_tht  = n_refract_phi_f243_r4(n,n0,z,z0,r,R0,phi,phi0)
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0=",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_n_refract_phi_f243_r4

subroutine unit_test_n_refract_phi_f243_r8
              character(len=128), automatic :: filename 
              real(kind=dp),      automatic :: n 
              real(kind=dp),      automatic :: n0 
              real(kind=dp),      automatic :: z 
              real(kind=dp),      automatic :: z0 
              real(kind=dp),      automatic :: r 
              real(kind=dp),      automatic :: R0  
              real(kind=dp),      automatic :: phi 
              real(kind=dp),      automatic :: phi0 
              real(kind=dp),      automatic :: n_over_tht 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)            :: t
              character(len=8)             :: d
              character(len=50), parameter :: header = "[TEST #4: n_refract_phi_f243_r8 -- START]"
              character(len=48), parameter :: footer = "[TEST #4: n_refract_phi_f243_r8 -- END]"
              ! Exec code ...
                          
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              n           = 1.000271800_dp 
              n0          = 1.000282756_dp 
              z           = 0.5_dp 
              z0          = 0.42_dp 
              r           = 25000.0_dp
              R0          = 15000.0_dp
              phi         = 0.47_dp 
              phi0        = 0.25_dp 
              n_over_tht  = n_refract_phi_f243_r8(n,n0,z,z0,r,R0,phi,phi0)
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0=",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_n_refract_phi_f243_r8





end module unit_test_atmos_refraction 



program main 
use unit_test_atmos_refraction
    write(*,*), "Using precomputed n-Cidor values."
    write(*,'(A60)'), row1 
    write(*,'(A60)'), row5  
    call unit_test_n_refract_tht_f243_r4()
    call unit_test_n_refract_tht_f243_r8() 
    call unit_test_n_refract_phi_f243_r4()
    call unit_test_n_refract_phi_f243_r8()
end program main 