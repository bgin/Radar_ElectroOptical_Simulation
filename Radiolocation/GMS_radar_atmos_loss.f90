
module radar_atmos_loss


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'radar_atmos_loss'
 !          
 !          Purpose:
 !
 !                         Implementation o_sp atmospheric loss o_sp radar signals propagation.
 !          History:
 !                        Date: 11-04-2022
 !                        Time: 14:08 GMT+2
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
 !          Re_sperences:
 !         
 !                   (Artech House Radar Library) Sergey A. Leonov, Alexander I. Leonov - Handbook o_sp Computer Simulation in Radio Engineering, Communications and Radar-Artech House (2001)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. dfinitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : i4,sp,dp
     
     implicit none
     public
     !=====================================================59
     !  File and module in_spormation:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),  parameter :: RADAR_ATMOS_LOSS_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RADAR_ATMOS_LOSS_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RADAR_ATMOS_LOSS_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RADAR_ATMOS_LOSS_FULLVER =   &
            1000*RADAR_ATMOS_LOSS_MAJOR+100*RADAR_ATMOS_LOSS_MINOR+10*RADAR_ATMOS_LOSS_MICRO
     ! Module creation date
     character(*),        parameter :: RADAR_ATMOS_LOSS_CREATE_DATE = "20-12-2021 15:54 +00200 (MON 20 DEC 2021 GMT+2)"
     ! Module build date
     character(*),        parameter :: RADAR_ATMOS_LOSS_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author in_spo
     character(*),        parameter :: RADAR_ATMOS_LOSS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RADAR_ATMOS_LOSS_SYNOPSIS    = "Radar atmospheric loss eqautions implementation"

     !==============================!
     !        Constant data         !
     !==============================!

     ! Oxygen resonance _sprequencies (Blake Model usage) 
     real(sp), dimension(48), parameter, public :: f_N_plus = [6.2648_sp,56.2648_sp,58.4466_sp,58.4466_sp,   &
	                                                       59.5910_sp,59.5910_sp,60.4348_sp,60.4348_sp,  &
							       61.1506_sp,61.1506_sp,61.8002_sp,61.8002_sp,  &
							       62.4212_sp,62.4212_sp,62.9980_sp,62.9980_sp,  &
							       63.5658_sp,63.5658_sp,64.1272_sp,64.1272_sp,  &
							       64.6779_sp,64.6779_sp,65.2240_sp,65.2240_sp,  &
							       65.7626_sp,65.7626_sp,66.2978_sp,66.2978_sp,  &
							       66.8313_sp,66.8313_sp,67.3627_sp,67.3627_sp,  &
							       67.8923_sp,67.8923_sp,68.4205_sp,68.4205_sp,  &
							       68.9478_sp,68.9478_sp,69.4741_sp,69.4741_sp,  &
							       70.0_sp,70.0_sp,70.5249_sp,70.55249_sp,       &
							       71.0497_sp,71.0497_sp,0.0_sp,0.0_sp,0.0_sp]

     real(sp), dimension(48), parameter, public :: f_N_minus = [118.7505_sp,118.7505_sp,62.4862_sp,62.4862_sp,  &
	                                                        60.3061_sp,60.3061_sp,59.1642_sp,59.1642_sp,    &
							        58.3239_sp,58.3239_sp,57.6125_sp,57.6125_sp,    &
							        56.9682_sp,56.9682_sp,56.3634_sp,56.3634_sp,    &
							        55.7839_sp,55.7839_sp,55.2214_sp,55.2214_sp,    &
							        54.6728_sp,54.6728_sp,54.1294_sp,54.1294_sp,    &
							        53.5960_sp,53.5960_sp,53.0695_sp,53.0695_sp,    &
							        52.5458_sp,52.5458_sp,52.0259_sp,52.0259_sp,    &
							        51.5091_sp,51.5091_sp,50.9949_sp,50.9949_sp,    &
							        50.4830_sp,50.4830_sp,49.9730_sp,49.9730_sp,    &
							        49.4648_sp,49.4648_sp,48.9582_sp,48.9582_sp,    &
							        48.4530_sp,48.4530_sp,0.0_sp,0.0_sp,0.0_sp]

     real(sp), dimension(48), parameter, public :: Z        =  [1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp, &
                                                                0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp, &
                                                                1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp, &
                                                                0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp, &
                                                                1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp, &
                                                                0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp,1.0_sp,0.0_sp, &
                                                                1.0_sp,0.0_sp,1.0_sp,0.0_sp,0.0_sp,0.0_sp]


      real(dp), dimension(48), parameter, public :: f_N_plusr8 = [6.2648_dp,56.2648_dp,58.4466_dp,58.4466_dp,   &
	                                                          59.5910_dp,59.5910_dp,60.4348_dp,60.4348_dp,  &
							          61.1506_dp,61.1506_dp,61.8002_dp,61.8002_dp,  &
							          62.4212_dp,62.4212_dp,62.9980_dp,62.9980_dp,  &
							          63.5658_dp,63.5658_dp,64.1272_dp,64.1272_dp,  &
							          64.6779_dp,64.6779_dp,65.2240_dp,65.2240_dp,  &
							          65.7626_dp,65.7626_dp,66.2978_dp,66.2978_dp,  &
							          66.8313_dp,66.8313_dp,67.3627_dp,67.3627_dp,  &
							          67.8923_dp,67.8923_dp,68.4205_dp,68.4205_dp,  &
							          68.9478_dp,68.9478_dp,69.4741_dp,69.4741_dp,  &
							          70.0_dp,70.0_dp,70.5249_dp,70.55249_dp,       &
							          71.0497_dp,71.0497_dp,0.0_dp,0.0_dp,0.0_dp]

     real(dp), dimension(48), parameter, public :: f_N_minusr8 = [118.7505_dp,118.7505_dp,62.4862_dp,62.4862_dp,  &
	                                                          60.3061_dp,60.3061_dp,59.1642_dp,59.1642_dp,    &
							          58.3239_dp,58.3239_dp,57.6125_dp,57.6125_dp,    &
							          56.9682_dp,56.9682_dp,56.3634_dp,56.3634_dp,    &
							          55.7839_dp,55.7839_dp,55.2214_dp,55.2214_dp,    &
							          54.6728_dp,54.6728_dp,54.1294_dp,54.1294_dp,    &
							          53.5960_dp,53.5960_dp,53.0695_dp,53.0695_dp,    &
							          52.5458_dp,52.5458_dp,52.0259_dp,52.0259_dp,    &
							          51.5091_dp,51.5091_dp,50.9949_dp,50.9949_dp,    &
							          50.4830_dp,50.4830_dp,49.9730_dp,49.9730_dp,    &
							          49.4648_dp,49.4648_dp,48.9582_dp,48.9582_dp,    &
							          48.4530_dp,48.4530_dp,0.0_dp,0.0_dp,0.0_dp]

     real(dp), dimension(48), parameter, public :: Zr8        =  [1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp, &
                                                                  0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp, &
                                                                  1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp, &
                                                                  0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp, &
                                                                  1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp, &
                                                                  0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp, &
                                                                  1.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp]


     

     


















end module radar_atmos_loss
