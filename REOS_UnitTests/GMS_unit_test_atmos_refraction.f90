
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

       use mod_kinds, only : i1, i4, i8, sp, dp 
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

subroutine unit_test_rad_ray_curvature_f251_r4()
              character(len=128), automatic :: filename
              real(kind=sp),      automatic :: n 
              real(kind=sp),      automatic :: z
              real(kind=sp),      automatic :: dndr 
              real(kind=sp),      automatic :: rho 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #5: rad_ray_curvature_f251_r4 -- START]"
              character(len=48), parameter  :: footer = "[TEST #5: rad_ray_curvature_f251_r4 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              rho         = 0.0_sp
              n           = 1.000271800_sp 
              z           = 0.78_sp 
              dndr        = -0.00000004_sp 
              rho         = rad_ray_curvature_f251_r4(n,z,dndr)
              print*,"[Input]:  n=",n,"z=",z,"dndr=",dndr 
              print*,"[Output]: rho=",rho 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_rad_ray_curvature_f251_r4

subroutine unit_test_rad_ray_curvature_f251_r8()
              character(len=128), automatic :: filename
              real(kind=dp),      automatic :: n 
              real(kind=dp),      automatic :: z
              real(kind=dp),      automatic :: dndr 
              real(kind=dp),      automatic :: rho 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #6: rad_ray_curvature_f251_r8 -- START]"
              character(len=48), parameter  :: footer = "[TEST #6: rad_ray_curvature_f251_r8 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              rho         = 0.0_dp
              n           = 1.000271800_dp 
              z           = 0.78_dp 
              dndr        = -0.00000004_dp 
              rho         = rad_ray_curvature_f251_r8(n,z,dndr)
              print*,"[Input]:  n=",n,"z=",z,"dndr=",dndr 
              print*,"[Output]: rho=",rho 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_rad_ray_curvature_f251_r8

subroutine unit_test_k_relative_f254_r4()
              character(len=128), automatic :: filename
              real(kind=sp),      automatic :: n 
              real(kind=sp),      automatic :: z
              real(kind=sp),      automatic :: dndr 
              real(kind=sp),      automatic :: k_rel 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #7: k_relative_f254_r4 -- START]"
              character(len=48), parameter  :: footer = "[TEST #7: k_relative_f254_r4 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              k_rel       = 0.0_sp
              n           = 1.000271800_sp 
              z           = 0.78_sp 
              dndr        = -0.00000004_sp 
              k_rel       = k_relative_f254_r4(n,z,dndr)
              print*,"[Input]:  n=",n,"z=",z,"dndr=",dndr 
              print*,"[Output]: k_rel=",k_rel  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_k_relative_f254_r4

subroutine unit_test_k_relative_f254_r8()
              character(len=128), automatic :: filename
              real(kind=dp),      automatic :: n 
              real(kind=dp),      automatic :: z
              real(kind=dp),      automatic :: dndr 
              real(kind=dp),      automatic :: k_rel 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #8: k_relative_f254_r8 -- START]"
              character(len=48), parameter  :: footer = "[TEST #8: k_relative_f254_r8 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              k_rel       = 0.0_dp
              n           = 1.000271800_dp 
              z           = 0.78_dp 
              dndr        = -0.00000004_dp 
              k_rel       = k_relative_f254_r8(n,z,dndr)
              print*,"[Input]:  n=",n,"z=",z,"dndr=",dndr 
              print*,"[Output]: k_rel=",k_rel  
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_k_relative_f254_r8

subroutine unit_test_rho_to_a_f267_r4()
              character(len=128), automatic :: filename
              real(kind=sp),      automatic :: dndh
              real(kind=sp),      automatic :: R 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #9: rho_to_a_f267_r4 -- START]"
              character(len=48), parameter  :: footer = "[TEST #9: rho_to_a_f267_r4 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              dndh = -0.00000004_sp 
              R    = 0.0_sp 
              R    = rho_to_a_f267_r4(dndh)
              print*,"[Input]: dndh=",dndh 
              print*,"[Output]: R=",R 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_rho_to_a_f267_r4

subroutine unit_test_rho_to_a_f267_r8()

              use iso_c_binding, only : c_int 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              character(len=128), automatic :: filename
              real(kind=dp),      automatic :: dndh
              real(kind=dp),      automatic :: R 
              integer(kind=i4),   automatic :: lstart, lend 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #10: rho_to_a_f267_r8 -- START]"
              character(len=48), parameter  :: footer = "[TEST #10: rho_to_a_f267_r8 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              dndh = -0.00000004_dp 
              R    = 0.0_dp 
#if 0
              print*, raise(SIGTRAP)
#endif
              R    = rho_to_a_f267_r8(dndh)
              print*,"[Input]: dndh=",dndh 
              print*,"[Output]: R=",R 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_rho_to_a_f267_r8

subroutine unit_test_n_avg_h_f145_r4()

              use iso_c_binding, only : c_int 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              character(len=128), automatic :: filename
              real(kind=sp),      automatic :: dn0 
              real(kind=sp),      automatic :: beta 
              real(kind=sp),      automatic :: h 
              real(kind=sp),      automatic :: nah 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #11: n_avg_h_f145_r4 -- START]"
              character(len=48), parameter  :: footer = "[TEST #11: n_avg_h_f145_r4 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              nah  = 0.0_sp 
              dn0  = 0.000240_sp 
              beta = 0.11_sp 
              h    = 1505.0_sp 
#if 0
              print*, raise(SIGTRAP)
#endif
              nah  = n_avg_h_f145_r4(dn0,beta,h)
              print*,"[Input]:  dn0=",dn0,"beta=",beta,"h=",h 
              print*,"[Output]: nah=",nah 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_n_avg_h_f145_r4

subroutine unit_test_n_avg_h_f145_r8()

              use iso_c_binding, only : c_int 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              character(len=128), automatic :: filename
              real(kind=dp),      automatic :: dn0 
              real(kind=dp),      automatic :: beta 
              real(kind=dp),      automatic :: h 
              real(kind=dp),      automatic :: nah 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #12: n_avg_h_f145_r8 -- START]"
              character(len=48), parameter  :: footer = "[TEST #12: n_avg_h_f145_r8 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              nah  = 0.0_dp 
              dn0  = 0.000240_dp 
              beta = 0.11_dp 
              h    = 1505.0_dp 
#if 0
              print*, raise(SIGTRAP)
#endif
              nah  = n_avg_h_f145_r8(dn0,beta,h)
              print*,"[Input]:  dn0=",dn0,"beta=",beta,"h=",h 
              print*,"[Output]: nah=",nah 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_n_avg_h_f145_r8

subroutine unit_test_refraction_angle_f345_r4()

              use iso_c_binding, only : c_int 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              character(len=128), automatic :: filename
              real(kind=sp),      automatic :: n0 
              real(kind=sp),      automatic :: nh 
              real(kind=sp),      automatic :: z0 
              real(kind=sp),      automatic :: dn0 
              real(kind=sp),      automatic :: beta 
              real(kind=sp),      automatic :: H 
              real(kind=sp),      automatic :: alpha 
              integer(c_int),     parameter :: SIGTRAP = 5 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #13: refraction_angle_f345_r4 -- START]"
              character(len=48), parameter  :: footer = "[TEST #13: refraction_angle_f345_r4 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              alpha = 0.0_sp  
              dn0   = 0.000240_sp 
              beta  = 0.10_sp 
              H     = 1245.14_sp
              nh    = n_avg_h_f145_r4(dn0,beta,H)
              z0    = 0.745_sp 
              n0    = 1.000271800_sp 
#if 0
              print*, raise(SIGTRAP)
#endif
              alpha = refraction_angle_f345_r4(n0,nh,z0,dn0,beta,H)
              print*,"[Input]: dn0=",dn0,"beta=",beta,"H=",H,"nh=",nh,"z0=",z0,"n0=",n0 
              print*,"[Output]: alpha=",alpha 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_refraction_angle_f345_r4

subroutine unit_test_refraction_angle_f345_r8()

              use iso_c_binding, only : c_int 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              character(len=128), automatic :: filename
              real(kind=dp),      automatic :: n0 
              real(kind=dp),      automatic :: nh 
              real(kind=dp),      automatic :: z0 
              real(kind=dp),      automatic :: dn0 
              real(kind=dp),      automatic :: beta 
              real(kind=dp),      automatic :: H 
              real(kind=dp),      automatic :: alpha 
              integer(c_int),     parameter :: SIGTRAP = 5 
              character(len=10)             :: t
              character(len=8)              :: d
              character(len=50), parameter  :: header = "[TEST #14: refraction_angle_f345_r8 -- START]"
              character(len=48), parameter  :: footer = "[TEST #14: refraction_angle_f345_r8 -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              alpha = 0.0_dp  
              dn0   = 0.000240_dp 
              beta  = 0.10_dp 
              H     = 1245.14_dp
              nh    = n_avg_h_f145_r8(dn0,beta,H)
              z0    = 0.745_dp 
              n0    = 1.000271800_dp 
#if 0
              print*, raise(SIGTRAP)
#endif
              alpha = refraction_angle_f345_r8(n0,nh,z0,dn0,beta,H)
              print*,"[Input]: dn0=",dn0,"beta=",beta,"H=",H,"nh=",nh,"z0=",z0,"n0=",n0 
              print*,"[Output]: alpha=",alpha 
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
end subroutine unit_test_refraction_angle_f345_r8

subroutine unit_test_alloc_atmos_refraction_state_r4_omp()
                use iso_c_binding, only : c_int 
                use IFPORT
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              type(atmos_refraction_state_r4_t),    automatic :: atmos_refract_state 
              type(atmos_refraction_size_params_t), automatic :: size_params 
              character(len=128),                   automatic :: filename 
              integer(c_int),                       parameter :: SIGTRAP = 5 
              character(len=10)                               :: t
              character(len=8)                                :: d
              character(len=64),                    parameter  :: header = "[TEST #15: alloc_atmos_refraction_state_r4_omp -- START]"
              character(len=64),                    parameter  :: footer = "[TEST #15: alloc_atmos_refraction_state_r4_omp -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              size_params = atmos_refraction_size_params_t(set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size())
#if 0
              print*, raise(SIGTRAP)
#endif              
              call alloc_atmos_refraction_state_r4_omp(atmos_refract_state,size_params)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
        contains

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats
end subroutine unit_test_alloc_atmos_refraction_state_r4_omp

subroutine unit_test_alloc_atmos_refraction_state_r8_omp()
                use iso_c_binding, only : c_int 
                use IFPORT
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              type(atmos_refraction_state_r8_t),    automatic :: atmos_refract_state 
              type(atmos_refraction_size_params_t), automatic :: size_params 
              character(len=128),                   automatic :: filename 
              integer(c_int),                       parameter :: SIGTRAP = 5 
              character(len=10)                               :: t
              character(len=8)                                :: d
              character(len=64),                    parameter  :: header = "[TEST #16: alloc_atmos_refraction_state_r8_omp -- START]"
              character(len=64),                    parameter  :: footer = "[TEST #16: alloc_atmos_refraction_state_r8_omp -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , ":" , t , filename , lstart
              print*,  header
              size_params = atmos_refraction_size_params_t(set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size())
#if 0
              print*, raise(SIGTRAP)
#endif              
              call alloc_atmos_refraction_state_r8_omp(atmos_refract_state,size_params)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , " : " , t , filename , lend  
              print*,  footer
        contains

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats
end subroutine unit_test_alloc_atmos_refraction_state_r8_omp


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
    call unit_test_rad_ray_curvature_f251_r4()
    call unit_test_rad_ray_curvature_f251_r8()
    call unit_test_k_relative_f254_r4()
    call unit_test_k_relative_f254_r8()
    call unit_test_rho_to_a_f267_r4()
    call unit_test_rho_to_a_f267_r8()
    call unit_test_n_avg_h_f145_r4()
    call unit_test_n_avg_h_f145_r8()
    call unit_test_refraction_angle_f345_r4()
    call unit_test_refraction_angle_f345_r8()
    call unit_test_alloc_atmos_refraction_state_r4_omp()
    call unit_test_alloc_atmos_refraction_state_r8_omp()
end program main 