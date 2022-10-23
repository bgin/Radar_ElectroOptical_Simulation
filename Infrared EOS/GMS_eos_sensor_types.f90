

module eos_sensor_types



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         eos_sensor_types
 !          
 !          Purpose:
 !                        Derived data types for 'eos_sensor' module implementation.
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !          History:
 !                        Date: 20-10-2022
 !                        Time: 15:33 GMT+2
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

   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_FULLVER =   &
            1000*EOS_SENSOR_TYPES_MAJOR+100*EOS_SENSOR_TYPES_MINOR+10*EOS_SENSOR_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_TYPES_CREATE_DATE = "20-10-2022 15:34 +00200 (THR 20 OCT 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_TYPES_SYNOPSIS    = "EO Sensors derived data types."



     type, public :: param_gamma_r4_t
  
           integer(kind=i4)                         :: n
           real(kind=sp), dimension(:), allocatable ::  phi  ! input
           real(kind=sp), dimesnion(:), allocatable :: gamma ! output    
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma     
     end type param_gamma_r4_t


     type, public :: param_gamma_r8_t
  
           integer(kind=i4)                         :: n
           real(kind=dp), dimension(:), allocatable ::  phi  ! input
           real(kind=dp), dimesnion(:), allocatable :: gamma ! output    
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma     
     end type param_gamma_r8_t


     type, public :: SN_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: R
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: sn
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sn
     end type SN_r4_t


     type, public :: SN_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: R
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: sn
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sn
     end type SN_r8_t


     type, public :: SM_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: R
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: sm
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sm
     end type SM_r4_t


     type, public :: SM_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: R
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: sm
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sm
     end type SM_r8_t


     type, public :: ratio_FH_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp), dimension(:), allocatable :: psi
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: fh
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: fh
     end type ratio_FH_r4_t


     type, public :: ratio_FH_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp), dimension(:), allocatable :: psi
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: fh
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: fh
     end type ratio_FH_r8_t


     type, public :: scan_mirror_ang_r4_t
 
           integer(kind=i4)                         :: n
           character(len=3)                         :: dir
           real(kind=sp), dimension(:), allocatable :: gam0
           real(kind=sp), dimension(:), allocatable :: psi
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           !dir$ attributes align : 64 :: gam0
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
     end type scan_mirror_ang_r4_t


     type, public :: scan_mirror_ang_r8_t
 
           integer(kind=i4)                         :: n
           character(len=3)                         :: dir
           real(kind=dp), dimension(:), allocatable :: gam0
           real(kind=dp), dimension(:), allocatable :: psi
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           !dir$ attributes align : 64 :: gam0
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
     end type scan_mirror_ang_r8_t


     type, public :: Dmax_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: d_ob
           real(kind=sp), dimension(:), allocatable :: h
           real(kind=sp), dimension(:), allocatable :: delta
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: dmax
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: dmax
     end type Dmax_r4_t


     type, public :: Dmax_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: d_ob
           real(kind=dp), dimension(:), allocatable :: h
           real(kind=dp), dimension(:), allocatable :: delta
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: dmax
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: dmax
     end type Dmax_r8_t


     type, public :: Dmin_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: d_ob
           real(kind=sp), dimension(:), allocatable :: h
           real(kind=sp), dimension(:), allocatable :: delta
           real(kind=sp), dimension(:), allocatable :: dmin
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: dmin
     end type Dmin_r4_t


     type, public :: Dmin_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: d_ob
           real(kind=dp), dimension(:), allocatable :: h
           real(kind=dp), dimension(:), allocatable :: delta
           real(kind=dp), dimension(:), allocatable :: dmin
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: dmin
     end type Dmin_r8_t


     type, public :: defocus_cof_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: l2
           real(kind=sp)                            :: o
           logical(kind=i4)                         :: inf
           real(kind=sp), dimension(:), allocatable :: alpha
           real(kind=sp), dimension(:), allocatable :: dc
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: dc
     end type defocus_cof_r4_t


     type, public :: defocus_cof_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: l2
           real(kind=dp)                            :: o
           logical(kind=i4)                         :: inf
           real(kind=dp), dimension(:), allocatable :: alpha
           real(kind=dp), dimension(:), allocatable :: dc
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: dc
     end type defocus_cof_r8_t


     type, public :: circle_dispersion_r4_t

           integer(kind=i4)                          :: n
           real(kind=sp)                             :: d
           real(kind=sp)                             :: l1
           real(kind=sp)                             :: l2
           real(kind=sp)                             :: o
           logical(kind=i4)                          :: inf
           real(kind=sp), dimension(:), allocatable  :: alpha
           real(kind=sp), dimension(:), allocatable  :: rho
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: rho
     end type circle_dispersion_r4_t


     type, public :: circle_dispersion_r8_t

           integer(kind=i4)                          :: n
           real(kind=dp)                             :: d
           real(kind=dp)                             :: l1
           real(kind=dp)                             :: l2
           real(kind=dp)                             :: o
           logical(kind=i4)                          :: inf
           real(kind=dp), dimension(:), allocatable  :: alpha
           real(kind=dp), dimension(:), allocatable  :: rho
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: rho
     end type circle_dispersion_r8_t 


     type, public :: circ_dispers_diam_r4_t

           integer(kind=i4)                          :: n
           real(kind=sp)                             :: d
           real(kind=sp)                             :: l1
           real(kind=sp)                             :: l2
           real(kind=sp)                             :: o
           logical(kind=i4)                          :: inf
           real(kind=sp), dimension(:), allocatable  :: alpha
           real(kind=sp), dimension(:), allocatable  :: ratio
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: ratio
     end type circ_dispers_diam_r4_t


     type, public :: circ_dispers_diam_r8_t

           integer(kind=i4)                          :: n
           real(kind=dp)                             :: d
           real(kind=dp)                             :: l1
           real(kind=dp)                             :: l2
           real(kind=dp)                             :: o
           logical(kind=i4)                          :: inf
           real(kind=dp), dimension(:), allocatable  :: alpha
           real(kind=dp), dimension(:), allocatable  :: ratio
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: ratio
     end type circ_dispers_diam_r8_t 


     type, public :: defocus_small_ang_r4_t

           integer(kind=i4)                          :: n
           real(kind=sp)                             :: l2
           real(kind=sp)                             :: o
           real(kind=sp), dimension(:), allocatable  :: alpha
           real(kind=sp), dimension(:), allocatable  :: rho
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: rho
     end type defocus_small_ang_r4_t


     type, public :: defocus_small_ang_r8_t

           integer(kind=i4)                          :: n
           real(kind=dp)                             :: l2
           real(kind=dp)                             :: o
           real(kind=dp), dimension(:), allocatable  :: alpha
           real(kind=dp), dimension(:), allocatable  :: rho
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: rho
     end type defocus_small_ang_r8_t


     type, public :: traj_scan_dxdt_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp), dimension(:,:), allocatable :: dx
           real(kind=sp), dimension(:,:), allocatable :: dt
           real(kind=sp), dimension(:),   allocatable :: dxdt
           !dir$ attributes align : 64 :: dx
           !dir$ attributes align : 64 :: dt
           !dir$ attributes align : 64 :: dxdt
     end type traj_scan_dxdt_r4_t


     type, public :: traj_scan_dxdt_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp), dimension(:,:), allocatable :: dx
           real(kind=dp), dimension(:,:), allocatable :: dt
           real(kind=dp), dimension(:),   allocatable :: dxdt
           !dir$ attributes align : 64 :: dx
           !dir$ attributes align : 64 :: dt
           !dir$ attributes align : 64 :: dxdt
     end type traj_scan_dxdt_r8_t


     type, public :: traj_scan_dydt_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp), dimension(:,:), allocatable :: dy
           real(kind=sp), dimension(:,:), allocatable :: dt
           real(kind=sp), dimension(:),   allocatable :: dydt
           !dir$ attributes align : 64 :: dy
           !dir$ attributes align : 64 :: dt
           !dir$ attributes align : 64 :: dydt
     end type traj_scan_dxdt_r4_t


     type, public :: traj_scan_dydt_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp), dimension(:,:), allocatable :: dy
           real(kind=dp), dimension(:,:), allocatable :: dt
           real(kind=dp), dimension(:),   allocatable :: dydt
           !dir$ attributes align : 64 :: dy
           !dir$ attributes align : 64 :: dt
           !dir$ attributes align : 64 :: dydt
     end type traj_scan_dydt_r8_t


     type, public :: fov_x_axis_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: h
           real(kind=sp), dimension(:),   allocatable :: delta
           real(kind=sp), dimension(:),   allocatable :: gamma
           real(kind=sp), dimension(:),   allocatable :: ax
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: ax 
     end type fov_x_axis_r4_t


     type, public :: fov_x_axis_r8_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: h
           real(kind=dp), dimension(:),   allocatable :: delta
           real(kind=dp), dimension(:),   allocatable :: gamma
           real(kind=dp), dimension(:),   allocatable :: ax
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: ax 
     end type fov_x_axis_r8_t


     type, public :: fov_y_axis_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: h
           real(kind=sp), dimension(:),   allocatable :: delta
           real(kind=sp), dimension(:),   allocatable :: gamma
           real(kind=sp), dimension(:),   allocatable :: ay
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: ay 
     end type fov_y_axis_r4_t


     type, public :: fov_y_axis_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: h
           real(kind=dp), dimension(:),   allocatable :: delta
           real(kind=dp), dimension(:),   allocatable :: gamma
           real(kind=dp), dimension(:),   allocatable :: ay
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: ay
     end type fov_y_axis_r8_t


     type, public :: scan_width_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: h
           real(kind=sp), dimension(:),   allocatable :: delta
           real(kind=sp), dimension(:),   allocatable :: theta
           real(kind=sp), dimension(:),   allocatable :: b
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: theta
           !dir$ attributes align : 64 :: b
     end type scan_width_r4_t


     type, public :: scan_width_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: h
           real(kind=dp), dimension(:),   allocatable :: delta
           real(kind=dp), dimension(:),   allocatable :: theta
           real(kind=dp), dimension(:),   allocatable :: b
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: theta
           !dir$ attributes align : 64 :: b
     end type scan_width_r8_t


     type, public :: refract_shift_r4_t

           integer(kind=i4)                           :: len
           real(kind=sp)                              :: i1
           real(kind=sp)                              :: delta
           real(kind=sp), dimension(:),   allocatable :: alfa
           real(kind=sp), dimension(:),   allocatable :: gamma
           real(kind=sp), dimension(:),   allocatable :: n
           real(kind=sp), dimension(:),   allocatable :: l
           !dir$ attributes align : 64 :: alfa
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: n
           !dir$ attributes align : 64 :: l
     end type refract_shift_r4_t


     type, public :: refract_shift_r8_t

           integer(kind=i4)                           :: len
           real(kind=dp)                              :: i1
           real(kind=dp)                              :: delta
           real(kind=dp), dimension(:),   allocatable :: alfa
           real(kind=dp), dimension(:),   allocatable :: gamma
           real(kind=dp), dimension(:),   allocatable :: n
           real(kind=dp), dimension(:),   allocatable :: l
           !dir$ attributes align : 64 :: alfa
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: n
           !dir$ attributes align : 64 :: l
     end type refract_shift_r8_t


     type, public :: project_xy_axis_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: l
           real(kind=sp), dimension(:),   allocatable :: alpha
           real(kind=sp), dimension(:),   allocatable :: xl
           real(kind=sp), dimension(:),   allocatable :: yl
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: xl
           !dir$ attributes align : 64 :: yl
     end type project_xy_axis_r4_t


     type, public :: project_xy_axis_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: l
           real(kind=dp), dimension(:),   allocatable :: alpha
           real(kind=dp), dimension(:),   allocatable :: xl
           real(kind=dp), dimension(:),   allocatable :: yl
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: xl
           !dir$ attributes align : 64 :: yl
     end type project_xy_axis_r8_t


     type, public :: s_shift_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: l
           real(kind=sp), dimension(:),   allocatable :: alpha
           real(kind=sp), dimension(:),   allocatable :: gamma
           real(kind=sp), dimension(:),   allocatable :: s
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: s 
     end type s_shift_r4_t


     type, public :: s_shift_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: l
           real(kind=dp), dimension(:),   allocatable :: alpha
           real(kind=dp), dimension(:),   allocatable :: gamma
           real(kind=dp), dimension(:),   allocatable :: s
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: s 
     end type s_shift_r8_t


     type, public :: project_s_xy_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: s
           real(kind=sp), dimension(:),   allocatable :: gamma
           real(kind=sp), dimension(:),   allocatable :: xs
           real(kind=sp), dimension(:),   allocatable :: ys
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: xs
           !dir$ attributes align : 64 :: ys
     end type project_s_xy_r4_t


     type, public :: project_s_xy_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: s
           real(kind=dp), dimension(:),   allocatable :: gamma
           real(kind=dp), dimension(:),   allocatable :: xs
           real(kind=dp), dimension(:),   allocatable :: ys
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: xs
           !dir$ attributes align : 64 :: ys
     end type project_s_xy_r8_t


     type, public :: raster_flux_int_r4_t

           integer(kind=i4)                           :: n
           integer(kind=i4)                           :: t
           real(kind=sp)                              :: xlo
           real(kind=sp)                              :: xup
           real(kind=sp), dimension(:,:), allocatable :: rhoE
           real(kind=sp), dimension(:),   allocatable :: absc
           real(kind=sp), dimension(:),   allocatable :: Phit
           integer(kind=i4),dimension(:), allocatable :: ier
           !dir$ attributes align : 64 :: rhoE
           !dir$ attributes align : 64 :: absc
           !dir$ attributes align : 64 :: Phit
           !dir$ attributes align : 64 :: ier
     end type raster_flux_int_r4_t


     type, public :: raster_flux_int_r8_t

           integer(kind=i4)                           :: n
           integer(kind=i4)                           :: t
           real(kind=dp)                              :: xlo
           real(kind=dp)                              :: xup
           real(kind=dp), dimension(:,:), allocatable :: rhoE
           real(kind=dp), dimension(:),   allocatable :: absc
           real(kind=dp), dimension(:),   allocatable :: Phit
           integer(kind=i4),dimension(:), allocatable :: ier
           !dir$ attributes align : 64 :: rhoE
           !dir$ attributes align : 64 :: absc
           !dir$ attributes align : 64 :: Phit
           !dir$ attributes align : 64 :: ier
     end type raster_flux_int_r8_t


     type, public :: raster_opacity_int_r4_t

           integer(kind=i4)                           :: n
           integer(kind=i4)                           :: t
           real(kind=sp)                              :: invs
           real(kind=sp)                              :: xlo
           real(kind=sp)                              :: xup
           real(kind=sp), dimension(:,:), allocatable :: rhophi
           real(kind=sp), dimension(:),   allocatable :: absc
           real(kind=sp), dimension(:),   allocatable :: rhi
           integer(kind=i4),dimension(:), allocatable :: ier
           !dir$ attributes align : 64 :: rhophi
           !dir$ attributes align : 64 :: absc
           !dir$ attributes align : 64 :: rho
           !dir$ attributes align : 64 :: ier
     end type raster_opacity_int_r4_t


     type, public :: raster_opacity_int_r8_t

           integer(kind=i4)                           :: n
           integer(kind=i4)                           :: t
           real(kind=dp)                              :: invs
           real(kind=dp)                              :: xlo
           real(kind=dp)                              :: xup
           real(kind=dp), dimension(:,:), allocatable :: rhophi
           real(kind=dp), dimension(:),   allocatable :: absc
           real(kind=dp), dimension(:),   allocatable :: rhi
           integer(kind=i4),dimension(:), allocatable :: ier
           !dir$ attributes align : 64 :: rhophi
           !dir$ attributes align : 64 :: absc
           !dir$ attributes align : 64 :: rho
           !dir$ attributes align : 64 :: ier
     end type raster_opacity_int_r8_t


     type, public :: cos_series_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: om0
           real(kind=sp)                              :: k
           real(kind=sp), dimension(:),   allocatable :: coss
           !dir$ attributes align : 64 :: coss
     end type cos_series_r4_t


     type, public :: cos_series_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: om0
           real(kind=dp)                              :: k
           real(kind=dp), dimension(:),   allocatable :: coss
           !dir$ attributes align : 64 :: coss
     end type cos_series_r8_t


     type, public :: sin_series_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: om0
           real(kind=sp)                              :: k
           real(kind=sp), dimension(:),   allocatable :: sins
           !dir$ attributes align : 64 :: sins
     end type sin_series_r4_t


     type, public :: sin_series_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: om0
           real(kind=dp)                              :: k
           real(kind=dp), dimension(:),   allocatable :: sins
           !dir$ attributes align : 64 :: sins
     end type sins_series_r8_t


     type, public :: rad_flux_spectrum

           integer(kind=i4)                           :: dim_len
           integer(kind=i4)                           :: data_len
           integer(kind=i4)                           :: status
           real(kind=dp), dimension(:),   allocatable :: phi_in
           complex(kind=sp), dimension(:),allocatable :: phi_out
           !dir$ attributes align : 64 :: phi_in
           !dir$ attributes align : 64 :: phi_out
     end type rad_flux_spectrum


     type, public :: squared_cos_flux_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: tin
           real(kind=sp)                              :: phi0
           real(kind=sp), dimension(:),   allocatable :: phi0t
           !dir$ attributes align : 64 :: phi0t
     end type squared_cos_flux_r4_t


     type, public :: squared_cos_flux_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: tin
           real(kind=dp)                              :: phi0
           real(kind=dp), dimension(:),   allocatable :: phi0t
           !dir$ attributes align : 64 :: phi0t
     end type squared_cos_flux_r8_t


     type, public :: const_flux_spectr_r4_t

           integer(kind=i4)                           :: n
           real(kind=sp)                              :: T
           real(kind=sp)                              :: phi0
           real(kind=sp), dimension(:),   allocatable :: phi0f
           real(kind=sp), dimension(:),   allocatable :: freq
           !dir$ attributes align : 64 :: phi0f
           !dir$ attributes align : 64 :: freq
     end type const_flux_spectr_r4_t
     

     type, public :: const_flux_spectr_r8_t

           integer(kind=i4)                           :: n
           real(kind=dp)                              :: T
           real(kind=dp)                              :: phi0
           real(kind=dp), dimension(:),   allocatable :: phi0f
           real(kind=dp), dimension(:),   allocatable :: freq
           !dir$ attributes align : 64 :: phi0f
           !dir$ attributes align : 64 :: freq
     end type const_flux_spectr_r8_t


     type, public :: ideal_modulator_r4_t

           integer(kind=i4)                          :: n
           real(kind=sp)                             :: f0
           real(kind=sp)                             :: phi0
           real(kind=sp)                             :: rho0
           real(kind=sp)                             :: rho1
           real(kind=sp), dimension(:),  allocatable :: rhot_s
           real(kind=sp), dimension(:),  allocatable :: rhot_c
           !dir$ attributes align : 64 :: rhot_s
           !dir$ attributes align : 64 :: rhot_c
     end type ideal_modulator_r4_t


     type, public :: ideal_modulator_r8_t

           integer(kind=i4)                          :: n
           real(kind=dp)                             :: f0
           real(kind=dp)                             :: phi0
           real(kind=dp)                             :: rho0
           real(kind=dp)                             :: rho1
           real(kind=dp), dimension(:),  allocatable :: rhot_s
           real(kind=dp), dimension(:),  allocatable :: rhot_c
           !dir$ attributes align : 64 :: rhot_s
           !dir$ attributes align : 64 :: rhot_c
     end type ideal_modulator_r8_t

        
     type, public :: rect_pulse_flux_r4_t

           integer(kind=i4)                          :: n
           real(kind=sp)                             :: fk
           real(kind=sp)                             :: Tin
           real(kind=dp), dimension(:),  allocatable :: phik
           real(kind=dp), dimension(:),  allocatable :: phi0
           !dir$ attributes align : 64 :: phik
           !dir$ attributes align : 64 :: phi0
      end type rect_pulse_flux_r4_t

        
      type, public :: rect_pulse_flux_r8_t

           integer(kind=i4)                          :: n
           real(kind=dp)                             :: fk
           real(kind=dp)                             :: Tin
           real(kind=dp), dimension(:),  allocatable :: phik
           real(kind=dp), dimension(:),  allocatable :: phi0
           !dir$ attributes align : 64 :: phik
           !dir$ attributes align : 64 :: phi0
      end type rect_pulse_flux_r8_t


      type, public :: rect_pulse_amp_r4_t

            integer(kind=i4)                          :: n
            real(kind=sp)                             :: phi0
            real(kind=sp)                             :: tin
            real(kind=sp)                             :: t
            real(kind=sp), dimension(:),  allocatable :: ak
            real(kind=sp), dimension(:),  allocatable :: phik
            real(kind=sp), dimension(:),  allocatable :: k
            !dir$ attributes align : 64 :: ak
            !dir$ attributes align : 64 :: phik
            !dir$ attributes align : 64 :: k
       end type rect_pulse_amp_r4_t


       type, public :: rect_pulse_amp_r8_t

            integer(kind=i4)                          :: n
            real(kind=dp)                             :: phi0
            real(kind=dp)                             :: tin
            real(kind=dp)                             :: t
            real(kind=dp), dimension(:),  allocatable :: ak
            real(kind=dp), dimension(:),  allocatable :: phik
            real(kind=dp), dimension(:),  allocatable :: k
            !dir$ attributes align : 64 :: ak
            !dir$ attributes align : 64 :: phik
            !dir$ attributes align : 64 :: k
       end type rect_pulse_amp_r8_t
        
      
       type, public :: transmit_coeff_r4_t

            integer(kind=i4)                         :: n
            real(kind=sp)                            :: rho0
            real(kind=sp)                            :: rho1
            real(kind=sp)                            :: beta
            real(kind=sp)                            :: omega0
            real(kind=sp)                            :: om0
            real(kind=sp), dimension(:), allocatable :: rhot
            !dir$ attributes align : 64 :: rhot
       end type transmit_coeff_r4_t


       type, public :: transmit_coeff_r8_t

            integer(kind=i4)                         :: n
            real(kind=dp)                            :: rho0
            real(kind=dp)                            :: rho1
            real(kind=dp)                            :: beta
            real(kind=dp)                            :: omega0
            real(kind=dp)                            :: om0
            real(kind=dp), dimension(:), allocatable :: rhot
            !dir$ attributes align : 64 :: rhot
       end type transmit_coeff_r8_t


       type, public :: integrate_FT_r4_t

            integer(kind=i4)                         :: n
            integer(kind=i4)                         :: ier
            real(kind=sp)                            :: xup
            real(kind=sp)                            :: np
            real(kind=sp)                            :: gamt
            real(kind=sp), dimension(:), allocatable :: Ft
            real(kind=sp), dimension(:), allocatable :: absc
            !dir$ attributes align : 64 :: Ft
            !dir$ attributes align : 64 :: absc
       end type integrate_FT_r4_t


       type, public :: integrate_FT_r8_t

            integer(kind=i4)                         :: n
            integer(kind=i4)                         :: ier
            real(kind=dp)                            :: xup
            real(kind=dp)                            :: np
            real(kind=dp)                            :: gamt
            real(kind=dp), dimension(:), allocatable :: Ft
            real(kind=dp), dimension(:), allocatable :: absc
            !dir$ attributes align : 64 :: Ft
            !dir$ attributes align : 64 :: absc
       end type integrate_FT_r8_t


       type, public :: raster_transmittance_t4_t

            integer(kind=i4)                        :: n
            real(kind=sp)                           :: a
            real(kind=sp)                           :: r
            real(kind=sp)                           :: rho0
            real(kind=sp)                           :: Omega0
            real(kind=sp)                           :: Beta
            real(kind=sp)                           :: R0
            real(kind=sp)                           :: om0
            real(kind=sp), dimension(:),allocatable :: rhot
            !dir$ attributes align : 64 :: rhot
       end type raster_transmittance_t4_t


       type, public :: raster_transmittance_t8_t

            integer(kind=i4)                        :: n
            real(kind=dp)                           :: a
            real(kind=dp)                           :: r
            real(kind=dp)                           :: rho0
            real(kind=dp)                           :: Omega0
            real(kind=dp)                           :: Beta
            real(kind=dp)                           :: R0
            real(kind=dp)                           :: om0
            real(kind=dp), dimension(:),allocatable :: rhot
            !dir$ attributes align : 64 :: rhot
       end type raster_transmittance_t8_t  
       
  
       type, public :: transmit_freq_modul_r4_t

            integer(kind=i4)                        :: n
            real(kind=sp)                           :: Phi0
            real(kind=sp)                           :: rho0
            real(kind=sp)                           :: om0
            real(kind=sp)                           :: Beta
            real(kind=sp)                           :: Omega0
            real(kind=sp), dimension(:),allocatable :: Phiom
            !dir$ attributes align : 64 :: Phiom
       end type transmit_freq_modul_r4_t


       type, public :: transmit_freq_modul_r8_t

            integer(kind=i4)                        :: n
            real(kind=dp)                           :: Phi0
            real(kind=dp)                           :: rho0
            real(kind=dp)                           :: om0
            real(kind=dp)                           :: Beta
            real(kind=dp)                           :: Omega0
            real(kind=dp), dimension(:),allocatable :: Phiom
            !dir$ attributes align : 64 :: Phiom
       end type transmit_freq_modul_r8_t
  
       
       type, public :: transmitt_spectr_r4_t

            integer(kind=i4)                        :: len
            integer(kind=i4)                        :: n
            real(kind=sp)                           :: rho0
            real(kind=sp)                           :: Beta
            real(kind=sp)                           :: om0
            real(kind=sp)                           :: Omega0
            real(kind=sp), dimension(:),allocatable :: rhot
            real(kind=sp), dimension(:),allocatable :: bjb
            !dir$ attributes align : 64 :: rhot
            !dir$ attributes align : 64 :: bjb
        end type transmitt_spectr_r4_t


        type, public :: transmitt_spectr_r8_t

            integer(kind=i4)                        :: len
            integer(kind=i4)                        :: n
            real(kind=dp)                           :: rho0
            real(kind=dp)                           :: Beta
            real(kind=dp)                           :: om0
            real(kind=dp)                           :: Omega0
            real(kind=dp), dimension(:),allocatable :: rhot
            real(kind=dp), dimension(:),allocatable :: bjb
            !dir$ attributes align : 64 :: rhot
            !dir$ attributes align : 64 :: bjb
         end type transmitt_spectr_r8_t


         type, public :: raster_transmit_fft_rc_t

             integer(kind=i4)                          :: dim_len
             integer(kind=i4)                          :: data_len
             integer(kind=i4)                          :: status
             real(kind=sp), dimension(:),allocatable   :: rhophi_in
             complex(kind=dp),dimension(:),allocatable :: rhophi_out
             !dir$ attributes align : 64 :: rhophi_in
             !dir$ attributes align : 64 :: rhophi_out
         end type raster_transmit_fft_rc_t
         

         type, public :: fourier_coff_ak_r4_t

             integer(kind=i4)                          :: N
             real(kind=sp)                             :: k
             real(kind=sp)                             :: ak
             integer(kind=i4)                          :: ier
             real(kind=sp), dimension(1:128)           :: rhophi
             real(kind=sp), dimension(1:128)           :: cosphi
         end type fourier_coff_ak_r4_t


        type, public :: fourier_coff_ak_r8_t

             integer(kind=i4)                          :: N
             real(kind=dp)                             :: k
             real(kind=dp)                             :: ak
             integer(kind=i4)                          :: ier
             real(kind=dp), dimension(1:128)           :: rhophi
             real(kind=dp), dimension(1:128)           :: cosphi
        end type fourier_coff_ak_r8_t


        type, public :: fourier_coff_a0k_r4_t

             integer(kind=i4)                          :: N
             real(kind=sp)                             :: k
             real(kind=sp)                             :: a0k
             integer(kind=i4)                          :: ier
             real(kind=sp), dimension(1:128)           :: rhod
             real(kind=sp), dimension(1:128)           :: cosphi
            
         end type fourier_coff_a0k_r4_t


        type, public :: fourier_coff_a0k_r8_t

             integer(kind=i4)                          :: N
             real(kind=dp)                             :: k
             real(kind=dp)                             :: a0k
             integer(kind=i4)                          :: ier
             real(kind=dp), dimension(1:128)           :: rhod
             real(kind=dp), dimension(1:128)           :: cosphi
             
        end type fourier_coff_a0k_r8_t


        type, public :: fourier_coff_bk_r4_t

             integer(kind=i4)                          :: N
             real(kind=sp)                             :: k
             real(kind=sp)                             :: bk
             integer(kind=i4)                          :: ier
             real(kind=sp), dimension(1:128)           :: rhophi
             real(kind=sp), dimension(1:128)           :: cosphi
            
        end type fourier_coff_bk_r4_t


       type, public :: fourier_coff_bk_r8_t

             integer(kind=i4)                          :: N
             real(kind=dp)                             :: k
             real(kind=dp)                             :: bk
             integer(kind=i4)                          :: ier
             real(kind=dp), dimension(1:128)           :: rhophi
             real(kind=dp), dimension(1:128)           :: cosphi
            
       end type fourier_coff_bk_r8_t

          
       type, public :: fourier_coff_b0k_r4_t

             integer(kind=i4)                          :: N
             real(kind=sp)                             :: k
             real(kind=sp)                             :: b0k
             integer(kind=i4)                          :: ier
             real(kind=sp), dimension(1:128)           :: rhod
             real(kind=sp), dimension(1:128)           :: cosphi
            
       end type fourier_coff_b0k_r4_t


       type, public :: fourier_coff_b0k_r8_t

             integer(kind=i4)                          :: N
             real(kind=dp)                             :: k
             real(kind=dp)                             :: b0k
             integer(kind=i4)                          :: ier
             real(kind=dp), dimension(1:128)           :: rhod
             real(kind=dp), dimension(1:128)           :: cosphi
            
       end type fourier_coff_b0k_r8_t


       type, public :: fourier_coff_a0_r4_t

             real(kind=sp)                              :: phit
             real(kind=sp)                              :: xlo
             real(kind=sp)                              :: xup
             real(kind=sp)                              :: ans
             integer(kind=i4)                           :: ier
             real(kind=sp), dimension(1:128)            :: rhophi
             real(kind=sp), dimension(1:128)            :: absc
            
       end type fourier_coff_a0_r4_t


       type, public :: fourier_coff_a0_r8_t

             real(kind=dp)                              :: phit
             real(kind=dp)                              :: xlo
             real(kind=dp)                              :: xup
             real(kind=dp)                              :: ans
             integer(kind=i4)                           :: ier
             real(kind=dp), dimension(1:128)            :: rhophi
             real(kind=dp), dimension(1:128)            :: absc
            
       end type fourier_coff_a0_r8_t

       ! used with  raster_transmitt_fourier_phi_t_r4   
       type, public :: raster_transmit_phit_r4_t

             integer(kind=i4)                           :: len
             integer(kind=i4)                           :: N
             real(kind=sp)                              :: ans !a0
             integer(kind=i4)                           :: r
             integer(kind=i4)                           :: klim
             real(kind=sp)                              :: xlo
             real(kind=sp)                              :: xup
             real(kind=sp),  dimension(1:128)           :: rhophi
             real(kind=sp),  dimension(1:128)           :: cosphi
             real(kind=sp),  dimension(1:128)           :: absc
             real(kind=sp),  dimension(:),allocatable   :: rhophit
             !dir$ attributes align : 64 :: rhophit
        end type raster_transmit_phit_r4_t

          
        ! used with  raster_transmitt_fourier_phi_t_r4   
        type, public :: raster_transmit_phit_r8_t

             integer(kind=i4)                           :: len
             integer(kind=i4)                           :: N
             real(kind=dp)                              :: ans !a0
             integer(kind=i4)                           :: r
             integer(kind=i4)                           :: klim
             real(kind=dp)                              :: xlo
             real(kind=dp)                              :: xup
             real(kind=dp),  dimension(1:128)           :: rhophi
             real(kind=dp),  dimension(1:128)           :: cosphi
             real(kind=dp),  dimension(1:128)           :: absc
             real(kind=dp),  dimension(:),allocatable   :: rhophit
             !dir$ attributes align : 64 :: rhophit
         end type raster_transmit_phit_r8_t


         ! used with:  raster_transmitt_fourier_t_r4 
         type, public :: raster_transmitt_t_r4_t

             integer(kind=i4)                           :: len
             integer(kind=i4)                           :: N
             real(kind=sp)                              :: xlo
             real(kind=sp)                              :: xup
             integer(kind=i4)                           :: r   ! number of raster rotations
             integer(kind=i4)                           :: klim
             real(kind=sp)                              :: phi1
             real(kind=sp)                              :: phi2
             real(kind=sp),  dimension(1:128)           :: rhophi
             real(kind=sp),  dimension(1:128)           :: rhod
             real(kind=sp),  dimension(1:128)           :: cosphi1
             real(kind=sp),  dimension(1:128)           :: cosphi2
             real(kind=sp),  dimension(1:128)           :: absc
             real(kind=sp),  dimension(1:128)           :: absc2 ! abscissas for rhod integration
             real(kind=sp),  dimension(:),allocatable   :: rhod
             !dir$ attributes align : 64 :: rhod
         end type raster_transmitt_t_r4_t


         type, public :: raster_transmitt_t_r8_t

             integer(kind=i4)                           :: len
             integer(kind=i4)                           :: N
             real(kind=dp)                              :: xlo
             real(kind=dp)                              :: xup
             integer(kind=i4)                           :: r   ! number of raster rotations
             integer(kind=i4)                           :: klim
             real(kind=dp)                              :: phi1
             real(kind=dp)                              :: phi2
             real(kind=dp),  dimension(1:128)           :: rhophi
             real(kind=dp),  dimension(1:128)           :: rhod
             real(kind=dp),  dimension(1:128)           :: cosphi1
             real(kind=dp),  dimension(1:128)           :: cosphi2
             real(kind=dp),  dimension(1:128)           :: absc
             real(kind=dp),  dimension(1:128)           :: absc2 ! abscissas for rhod integration
             real(kind=dp),  dimension(:),allocatable   :: rhod
             !dir$ attributes align : 64 :: rhod
         end type raster_transmitt_t_r8_t


         type, public :: raster_flux_sinc_r4_t

              integer(kind=i4)                          :: htin
              real(kind=sp)                             :: f
              real(kind=sp)                             :: phi0
              real(kind=sp), dimension(:), allocatable  :: phi0f
              !dir$ attributes align : 64 :: phi0f
         end type raster_flux_sinc_r4_t


         type, public :: raster_flux_sinc_r8_t

              integer(kind=i4)                          :: htin
              real(kind=dp)                             :: f
              real(kind=dp)                             :: phi0
              real(kind=dp), dimension(:), allocatable  :: phi0f
              !dir$ attributes align : 64 :: phi0f
         end type raster_flux_sinc_r8_t


         type, public :: raster_flux_mod_sinc_r4_t

              integer(kind=i4)                          :: htin
              real(kind=sp)                             :: f0
              real(kind=sp)                             :: rho0
              real(kind=sp)                             :: phi0
              real(kind=sp), dimension(:), allocatable  :: phif
              !dir$ attributes align : 64 :: phif
         end type raster_flux_mod_sinc_r4_t
         
         
         type, public :: raster_flux_mod_sinc_r8_t

              integer(kind=i4)                          :: htin
              real(kind=dp)                             :: f0
              real(kind=dp)                             :: rho0
              real(kind=dp)                             :: phi0
              real(kind=dp), dimension(:), allocatable  :: phif
              !dir$ attributes align : 64 :: phif
         end type raster_flux_mod_sinc_r8_t


         type, public :: raster_mod_sinc_shifted_r4_t

               integer(kind=i4)                         :: htin !shall be divisable by 2
               real(kind=sp)                            :: f
               real(kind=sp)                            :: phi0
               real(kind=sp)                            :: fx
               real(kind=sp)                            :: rho0
               real(kind=sp)                            :: dt
               real(kind=sp), dimension(:), allocatable :: phif
              !dir$ attributes align : 64 :: phif
         end type raster_mod_sinc_shifted_r4_t


         type, public :: raster_mod_sinc_shifted_r8_t

               integer(kind=i4)                         :: htin !shall be divisable by 2
               real(kind=dp)                            :: f
               real(kind=dp)                            :: phi0
               real(kind=dp)                            :: fx
               real(kind=dp)                            :: rho0
               real(kind=dp)                            :: dt
               real(kind=dp), dimension(:), allocatable :: phif
              !dir$ attributes align : 64 :: phif
         end type raster_mod_sinc_shifted_r8_t   
         

         !Aggregation of above defeined types -- constitutes a EO sensor

         type, public :: eo_sensor_r4_t

               type(param_gamma_r4_t)     :: m_param_gamma
               type(SN_r4_t)              :: m_SN
               type(SM_r4_t)              :: m_SM
               type(ratio_FH_r4_t)        :: m_ratio_FH
               type(scan_mirror_ang_r4_t) :: m_scan_mirr_ang
               type(Dmax_r4_t)            :: m_Dmax
               type(Dmin_r4_t)            :: m_Dmin
               type(defocus_cof_r4_t)     :: m_defocus_cof
         end type eo_sensor_r4_t
         
           

end module eos_sensor_types
