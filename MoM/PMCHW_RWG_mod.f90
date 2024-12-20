module PMCHW_RWG_mod
!!==============================================================================
! This module uses PMCHW (Poggio, Miller, Chang, Harrington, and Wu)
! formulation for combining EFIE and MFIE to simulate electromagnetic
! scattering on an arbitrary surface. The scattering problem is solved using
! boundary element method, BEM, (often called mathod of moments) on a
! RWG-basis (Rao, Wilson, and Glisson) using Gelerkin's method. Integrals are
! solved numerically using Gaussian quadrature formulas.
!
! The problem consists of two regions, inside and outside of the closed
! surface. The regions have different permeability and permitivity.
! 
!
! Abbreviations:
!  CS - Closed Surface
!  OS - Open Surface
!  GQ - Gaussian Quadrature
!  GQF - Gaussian Quadrature Formula
!  GLQF - Gauss-Legendre Quadrature Formula  
!  EFIE - Electric Field Integral Formulation
!  MFIE - Magnetic Field Integral Formulation
! 
! Last edited: March 7th 2021.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use iso_fortran_env  , only: real32, real64, real128
  use ieee_arithmetic  , only: ieee_is_finite
  use working_precision, only: wp
  use RWG_basis_mod    , only: RWG_basis_type
  use math_funcs_mod   , only: cross_prod_3D
  use math_funcs_mod   , only: plane_wave
  use constants_mod    , only: PI
  use constants_mod    , only: I_IMAG
  use constants_mod    , only: ZERO_CMPLX
  use constants_mod    , only: ZERO
  use constants_mod    , only: UNITY
  use constants_mod    , only: PI4_INV
  use is_close_mod     , only: is_close
  use io_mod           , only: r8mat_write
  use gauss_quad_formulas_mod, only: GQF_triangle_3pnt
  use gauss_quad_formulas_mod, only: GQF_Legendre_3pnt
  use gauss_quad_formulas_mod, only: GQF_Legendre_5pnt

  implicit none

  !!===================!!
  ! External procedures !
  !=====================!=======================================================
  external :: CGESV
  external :: ZGESV
  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: PMCHW_RWG_type ! Main type

  integer    , parameter, public :: NUM_REGIONS = 2
  integer    , parameter, public :: INC_FIELD_TYPE_PLANE_WAVE = 1
  integer    , parameter, public :: INC_FIELD_TYPE_SPHERICAL_WAVE = 2
  integer    , parameter, public :: OUTER_REGION_IDX = 1
  integer    , parameter, public :: INNER_REGION_IDX = 2
  integer    , parameter, public :: X_IDX = 1
  integer    , parameter, public :: Y_IDX = 2
  integer    , parameter, public :: Z_IDX = 3
  integer    , parameter, public :: SPATIAL_DIM = 3
  integer    , parameter, public :: NUM_FACE_VERTICES = 3
  integer    , parameter, public :: NUM_FACES_IN_BASIS = 2
  integer    , parameter, public :: GQF_WEIGHT_IDX = 1
  integer    , parameter, public :: GQF_XI_IDX = 2
  integer    , parameter, public :: GQF_ETA_IDX = 3
  integer    , parameter, public :: GQF_ZETA_IDX = 4
  integer    , parameter, public :: GQF_LEGENDRE_POINT_IDX = 2
  real(wp)   , parameter, public :: PROP_CONST_OBS_PNT_SRC_CLOSE = -1._wp!e-11
  logical    , parameter, public :: CAUCHY = .false.

  public :: eval_green_func_integrals
  public :: eval_outer_integrals
  public :: face_pair_integral_EFIE
  public :: face_pair_integral_MFIE
  public :: surface_intgr_solution
  public :: line_intgr_solution
  public :: inner_intgr_of_subtr_terms
  public :: green_func_smoothened
  public :: calc_edge_unit_normals
  public :: map_GLQF_pnt_to_triangle_edge
  public :: dbl_singularity_intgr
  public :: eval_subtracted_terms
  public :: calc_green_func
  public :: calc_grad_of_green_func
  public :: Cauchy_principal_value
  
  !====================================!
  ! Private types/procedures/constants !
  !====================================!========================================

  private
  !!------------------------!!
  ! Derived type definitions !
  !--------------------------!--------------------------------------------------


  !!---------!!
  ! Main type !
  !-----------!-----------------------------------------------------------------
  type PMCHW_RWG_type
     type (RWG_basis_type)                     :: RWG_basis
     complex(wp), dimension(NUM_REGIONS)       :: permeabilities
     complex(wp), dimension(NUM_REGIONS)       :: permitivities
     real(wp)                                  :: angular_frequency
     complex(wp), dimension(:, :), allocatable :: PMCHW_matrix
     complex(wp), dimension(:, :), allocatable :: q_vectors
     complex(wp), dimension(:, :), allocatable :: expansion_coeff_alpha
     complex(wp), dimension(:, :), allocatable :: expansion_coeff_beta
     complex(wp), dimension(:, :), allocatable :: inc_E_field_ampl
     complex(wp), dimension(:, :), allocatable :: inc_H_field_ampl
     real(wp)   , dimension(:, :), allocatable :: inc_wave_direction ! unit-
     integer    , dimension(:)   , allocatable :: inc_field_type
     integer                                   :: num_q_vectors
   contains
     ! Initialisers
     procedure, pass(this), public :: initialise
     ! Deallocation
     procedure, pass(this), public :: deallocate_attributes
     ! Get-functions
     procedure, pass(this), public :: get_permeability
     procedure, pass(this), public :: get_permitivity
     procedure, pass(this), public :: get_angular_frequency
     procedure, pass(this), public :: get_num_q_vectors
     procedure, pass(this), public :: get_PMCHW_matrix_size
     procedure, pass(this), public :: get_q_vectors_size
     procedure, pass(this), public :: get_q_vectors
     procedure, pass(this), public :: get_PMCHW_matrix
     procedure, pass(this), public :: get_solutions
     ! Calculations
     procedure, pass(this), public :: calc_q_vectors
     procedure, pass(this), public :: calc_q_vectors_BBB
     procedure, pass(this), public :: calc_PMCHW_matrix
     procedure, pass(this), public :: D_and_K_matrix_element_mn
     procedure, pass(this), public :: inc_E_and_H_field_at_obs_pnt
     procedure, pass(this), public :: solve_matrix_equation
     procedure, pass(this), public :: E_and_H_field_at_obs_pnt
     procedure, pass(this), public :: E_and_H_field_at_obs_pnt_BBB
     procedure, pass(this), public :: bistatic_scattering_cross_section
     procedure, pass(this), public :: face_centroid
     procedure, pass(this), public :: are_obs_pnt_and_src_close
     procedure, pass(this), public :: get_edge_lengths
     procedure, pass(this), public :: D_and_K_matrix_element_mn_BBB
     ! Writing data
     procedure, pass(this), public :: write_solutions

  end type PMCHW_RWG_type
     
  
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  
  !!=================!!
  ! Public procedures !
  !===================!=========================================================
  !!------------!!
  ! Constructors !
  !--------------!--------------------------------------------------------------


  !!==================================!!
  ! RWG_basis_type internal procedures !
  !====================================!========================================
  !!------------!!
  ! Initialisers !
  !--------------!--------------------------------------------------------------
  subroutine initialise(&
       this, &
       RWG_basis, &
       permeabilities, &
       permitivities, &
       angular_frequency) 
    class (PMCHW_RWG_type)             , intent(inout) :: this
    type (RWG_basis_type)              , intent(in)    :: RWG_basis
    complex(wp), dimension(NUM_REGIONS), intent(in)    :: permeabilities
    complex(wp), dimension(NUM_REGIONS), intent(in)    :: permitivities
    real(wp)                           , intent(in)    :: angular_frequency
    this%RWG_basis = RWG_basis
    this%permeabilities = permeabilities
    this%permitivities = permitivities
    this%angular_frequency = angular_frequency
    if (allocated(this%PMCHW_matrix)) deallocate(this%PMCHW_matrix)
    if (allocated(this%q_vectors)) deallocate(this%q_vectors)
    if (allocated(this%expansion_coeff_alpha)) &
         deallocate(this%expansion_coeff_alpha)
    if (allocated(this%expansion_coeff_beta)) &
         deallocate(this%expansion_coeff_beta)
       
    allocate(this%PMCHW_matrix(2*RWG_basis%num_bases, 2*RWG_basis%num_bases))
  end subroutine initialise
  
  !!----------------------------------------------------------------------------

  subroutine deallocate_attributes(this)
    class (PMCHW_RWG_type), intent(inout) :: this
    deallocate(this%PMCHW_matrix)
    deallocate(this%q_vectors)
    deallocate(this%expansion_coeff_alpha)
    deallocate(this%expansion_coeff_beta)
  end subroutine deallocate_attributes
  

  !!-------------!!
  ! Get-functions !
  !---------------!-------------------------------------------------------------

  function get_permeability(this, region) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    integer                            :: region
    complex(wp)                        :: return_value
    return_value = this%permeabilities(region)
  end function get_permeability

  !!----------------------------------------------------------------------------

  function get_permitivity(this, region) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    integer                            :: region
    complex(wp)                        :: return_value
    return_value = this%permitivities(region)
  end function get_permitivity
  
  !!----------------------------------------------------------------------------

  function get_angular_frequency(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    real(wp)                        :: return_value
    return_value = this%angular_frequency
  end function get_angular_frequency
  
  !!----------------------------------------------------------------------------
  
  function get_PMCHW_matrix_size(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    integer                            :: return_value
    return_value = size(this%PMCHW_matrix)
  end function get_PMCHW_matrix_size
  
  !!----------------------------------------------------------------------------

  function get_q_vectors_size(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    integer                            :: return_value
    return_value = size(this%q_vectors)
  end function get_q_vectors_size

  !!----------------------------------------------------------------------------

  function get_q_vectors(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    complex(wp), dimension(:, :), allocatable :: return_value
    allocate(return_value, source=this%q_vectors)
  end function get_q_vectors
  
  !!----------------------------------------------------------------------------

  function get_num_q_vectors(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    integer                            :: return_value
    return_value = this%num_q_vectors
  end function get_num_q_vectors
  
  !!----------------------------------------------------------------------------
  
  function get_PMCHW_matrix(this) result(return_value)
    class (PMCHW_RWG_type), intent(in) :: this
    complex(wp), dimension(:, :), allocatable :: return_value
    allocate(return_value, source=this%PMCHW_matrix)
  end function get_PMCHW_matrix

  !!----------------------------------------------------------------------------
  
  function get_solutions(this) result(res)
    class (PMCHW_RWG_type), intent(in)       :: this
    complex(wp), dimension(:, :), allocatable :: res
    ! Variables for internal use -----------------------------------------------
    integer :: b
    integer :: m
    
    if (.not. allocated(this%q_vectors) .or. &
         .not. allocated(this%expansion_coeff_alpha) .or. &
         .not. allocated(this%expansion_coeff_beta)) then
       print *, 'Error: PMCHW_RWG_mod: get_solutions:'
       print *, '       PMCHW_RWG_type must be initialised, PMCHW-matrix ', &
            'and q-vectors must be set, and matrix equation must be solved.'
       stop 2
    end if

    allocate(res, mold=this%q_vectors)
    do b = 1, this%num_q_vectors
       res(:this%RWG_basis%num_bases, b) = this%expansion_coeff_alpha(:, b)
       res(this%RWG_basis%num_bases + 1:, b) = this%expansion_coeff_beta(:, b)
    end do

  end function get_solutions
  

  !!-------------!!
  ! Calculations  !
  !---------------!-------------------------------------------------------------
  subroutine calc_q_vectors(&
       this, &
       gauss_quad_formula, &
       inc_E_field_ampl, &
       inc_H_field_ampl, &
       inc_wave_direction, &
       inc_field_type)
    class (PMCHW_RWG_type)         , intent(inout) :: this
    real(wp)   , dimension(:, :)   , intent(in)    :: gauss_quad_formula
    complex(wp), dimension(:, :)   , intent(in)    :: inc_E_field_ampl
    complex(wp), dimension(:, :)   , intent(in)    :: inc_H_field_ampl
    real(wp)   , dimension(:, :)   , intent(in)    :: inc_wave_direction 
    integer    , dimension(:)      , intent(in)    :: inc_field_type
    ! Variables for internal use -----------------------------------------------
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM)   :: face_coords
    integer    , dimension(NUM_FACES_IN_BASIS)               :: free_vertices
    integer    , dimension(NUM_FACES_IN_BASIS)               :: T_m
    real(wp)   , dimension(SPATIAL_DIM)                      :: r1_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: r2_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: r3_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: p_m
    real(wp)   , dimension(SPATIAL_DIM)                      :: observation_pnt
    complex(wp), dimension(SPATIAL_DIM)                      :: inc_E_field
    complex(wp), dimension(SPATIAL_DIM)                      :: inc_H_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_xi_E_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_eta_E_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_zeta_E_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_E_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_xi_H_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_eta_H_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_zeta_H_field
    complex(wp), dimension(:, :, :), allocatable             :: intgr_H_field
    real(wp)   , dimension(NUM_FACES_IN_BASIS)               :: signs
    real(wp)                                                 :: xi
    real(wp)                                                 :: eta
    real(wp)                                                 :: zeta
    real(wp)                                                 :: weight
    real(wp)                                                 :: prefactor
    integer                                                  :: m
    integer                                                  :: m_plus_N
    integer                                                  :: j
    integer                                                  :: p
    integer                                                  :: o
    integer                                                  :: b

    if (allocated(this%q_vectors)) deallocate(this%q_vectors)
    if (allocated(this%inc_E_field_ampl)) deallocate(this%inc_E_field_ampl)
    if (allocated(this%inc_H_field_ampl)) deallocate(this%inc_H_field_ampl)
    if (allocated(this%inc_wave_direction)) &
         deallocate(this%inc_wave_direction)
    if (allocated(this%inc_field_type)) deallocate(this%inc_field_type)
    
    this%num_q_vectors = size(inc_E_field_ampl, dim=2)
    ! Allocate type attributes
    allocate(this%q_vectors(2*this%RWG_basis%num_bases, this%num_q_vectors))
    allocate(this%inc_E_field_ampl(SPATIAL_DIM, this%num_q_vectors))
    allocate(this%inc_H_field_ampl, mold=this%inc_E_field_ampl)
    allocate(this%inc_wave_direction(SPATIAL_DIM, this%num_q_vectors))
    allocate(this%inc_field_type(this%num_q_vectors))
    ! Allocate internal variables
    allocate(intgr_xi_E_field(this%num_q_vectors, &
         this%RWG_basis%mesh%num_faces, SPATIAL_DIM))
    allocate(intgr_eta_E_field, mold=intgr_xi_E_field)
    allocate(intgr_zeta_E_field, mold=intgr_xi_E_field)
    allocate(intgr_E_field, mold=intgr_xi_E_field)
    allocate(intgr_xi_H_field, mold=intgr_xi_E_field)
    allocate(intgr_eta_H_field, mold=intgr_xi_E_field)
    allocate(intgr_zeta_H_field, mold=intgr_xi_E_field)
    allocate(intgr_H_field, mold=intgr_xi_E_field)


    do b = 1, this%num_q_vectors
       this%inc_E_field_ampl(:, b) = inc_E_field_ampl(:, b)
       this%inc_H_field_ampl(:, b) = inc_H_field_ampl(:, b)
       this%inc_wave_direction(:, b) = inc_wave_direction(:, b)
       this%inc_field_type(b) = inc_field_type(b)
       
       do p = 1, this%RWG_basis%mesh%num_faces
          face_coords = this%RWG_basis%mesh%get_face_coords(p)
          r1_obs = face_coords(1, :)
          r2_obs = face_coords(2, :)
          r3_obs = face_coords(3, :)
          intgr_xi_E_field(b, p, :) = ZERO_CMPLX
          intgr_eta_E_field(b, p, :) = ZERO_CMPLX
          intgr_E_field(b, p, :) = ZERO_CMPLX
          intgr_xi_H_field(b, p, :) = ZERO_CMPLX
          intgr_eta_H_field(b, p, :) = ZERO_CMPLX
          intgr_H_field(b, p, :) = ZERO_CMPLX
          do j = 1, size(gauss_quad_formula, dim=1)
             weight = gauss_quad_formula(j, 1)
             xi = gauss_quad_formula(j, 2)
             eta = gauss_quad_formula(j, 3)
             zeta = gauss_quad_formula(j, 4)
             observation_pnt = xi*r1_obs + eta*r2_obs + zeta*r3_obs
             call this%inc_E_and_H_field_at_obs_pnt(&
                  inc_E_field, &
                  inc_H_field, &
                  b          , &
                  observation_pnt)
             intgr_xi_E_field(b, p, :) = &
                  intgr_xi_E_field(b, p, :) + weight*xi*inc_E_field
             intgr_eta_E_field(b, p, :) = &
                  intgr_eta_E_field(b, p, :) + weight*eta*inc_E_field
             intgr_E_field(b, p, :) = &
                  intgr_E_field(b, p, :) + weight*inc_E_field
             intgr_xi_H_field(b, p, :) = &
                  intgr_xi_H_field(b, p, :) + weight*xi*inc_H_field
             intgr_eta_H_field(b, p, :) = &
                  intgr_eta_H_field(b, p, :) + weight*eta*inc_H_field
             intgr_H_field(b, p, :) = &
                  intgr_H_field(b, p, :) + weight*inc_H_field
          end do
          intgr_zeta_E_field(b, p, :) = &
               intgr_E_field(b, p, :) - intgr_xi_E_field(b, p, :) &
               -intgr_eta_E_field(b, p, :)
          intgr_zeta_H_field(b, p, :) = &
               intgr_H_field(b, p, :) - intgr_xi_H_field(b, p, :) &
               -intgr_eta_H_field(b, p, :)
       end do
       

       signs = [ 1._wp, -1._wp ]
       do m = 1, this%RWG_basis%num_bases
          m_plus_N = m + this%RWG_basis%num_bases
          this%q_vectors(m, b) = ZERO_CMPLX
          this%q_vectors(m_plus_N, b) = ZERO_CMPLX

          free_vertices = this%RWG_basis%get_free_vertices(m)
          T_m = this%RWG_basis%get_adjacent_faces(m)
          do p = 1, NUM_FACES_IN_BASIS
             face_coords = this%RWG_basis%mesh%get_face_coords(T_m(p))
             r1_obs = face_coords(1, :)
             r2_obs = face_coords(2, :)
             r3_obs = face_coords(3, :)
             p_m = this%RWG_basis%mesh%get_vertex_coords(free_vertices(p))
             this%q_vectors(m, b) = this%q_vectors(m, b)                    &
                  + signs(p)*(                                              &
                  + dot_product(r1_obs, intgr_xi_E_field(b, T_m(p), :))    &
                  + dot_product(r2_obs, intgr_eta_E_field(b, T_m(p), :))    &
                  + dot_product(r3_obs, intgr_zeta_E_field(b, T_m(p), :))   &
                  - dot_product(p_m, intgr_E_field(b, T_m(p), :)) )
             this%q_vectors(m_plus_N, b) = this%q_vectors(m_plus_N, b)      &
                  + signs(p)*(                                              &
                  + dot_product(r1_obs, intgr_xi_H_field(b, T_m(p), :))    &
                  + dot_product(r2_obs, intgr_eta_H_field(b, T_m(p), :))    &
                  + dot_product(r3_obs, intgr_zeta_H_field(b, T_m(p), :))   & 
                  - dot_product(p_m, intgr_H_field(b, T_m(p), :)) )
          end do ! p
          prefactor = this%RWG_basis%get_basis_edge_length(m)/2._wp
          this%q_vectors(m, b) = prefactor*this%q_vectors(m, b)
          this%q_vectors(m_plus_N, b) = prefactor*this%q_vectors(m_plus_N, b)
       end do ! m
    end do ! b
       

    ! Deallocate coefficient vectors since q_vectors is changed
    if (allocated(this%expansion_coeff_alpha)) then
       deallocate(this%expansion_coeff_alpha)
       deallocate(this%expansion_coeff_beta)
    end if

  end subroutine calc_q_vectors
         
  !!----------------------------------------------------------------------------

  subroutine calc_q_vectors_BBB(&
       this, &
       gauss_quad_formula, &
       inc_E_field_ampl, &
       inc_H_field_ampl, &
       inc_wave_direction, &
       inc_field_type)
    class (PMCHW_RWG_type)         , intent(inout) :: this
    real(wp)   , dimension(:, :)   , intent(in)    :: gauss_quad_formula
    complex(wp), dimension(:, :)   , intent(in)    :: inc_E_field_ampl
    complex(wp), dimension(:, :)   , intent(in)    :: inc_H_field_ampl
    real(wp)   , dimension(:, :)   , intent(in)    :: inc_wave_direction 
    integer    , dimension(:)      , intent(in)    :: inc_field_type
    ! Variables for internal use -----------------------------------------------
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM)   :: face_coords
    integer    , dimension(NUM_FACES_IN_BASIS)               :: free_vertices
    integer    , dimension(NUM_FACES_IN_BASIS)               :: T_m
    real(wp)   , dimension(SPATIAL_DIM)                      :: r1_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: r2_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: r3_obs
    real(wp)   , dimension(SPATIAL_DIM)                      :: p_m
    real(wp)   , dimension(SPATIAL_DIM)                      :: r_obs
    complex(wp), dimension(SPATIAL_DIM)                      :: inc_E_field
    complex(wp), dimension(SPATIAL_DIM)                      :: inc_H_field
    complex(wp)                                              :: sum_E_field
    complex(wp)                                              :: sum_H_field
    real(wp)                                                 :: xi
    real(wp)                                                 :: eta
    real(wp)                                                 :: zeta
    real(wp)                                                 :: weight
    real(wp)                                                 :: prefactor
    integer                                                  :: num_quad_pnts
    integer                                                  :: m
    integer                                                  :: m_plus_N
    integer                                                  :: j
    integer                                                  :: p
    integer                                                  :: b

    if (allocated(this%q_vectors)) deallocate(this%q_vectors)
    if (allocated(this%inc_E_field_ampl)) deallocate(this%inc_E_field_ampl)
    if (allocated(this%inc_H_field_ampl)) deallocate(this%inc_H_field_ampl)
    if (allocated(this%inc_wave_direction)) &
         deallocate(this%inc_wave_direction)
    if (allocated(this%inc_field_type)) deallocate(this%inc_field_type)
    this%num_q_vectors = size(inc_E_field_ampl, dim=2)
    ! Allocate type attributes
    allocate(this%q_vectors(2*this%RWG_basis%num_bases, this%num_q_vectors))
    allocate(this%inc_E_field_ampl(SPATIAL_DIM, this%num_q_vectors))
    allocate(this%inc_H_field_ampl, mold=this%inc_E_field_ampl)
    allocate(this%inc_wave_direction(SPATIAL_DIM, this%num_q_vectors))
    allocate(this%inc_field_type(this%num_q_vectors))

    num_quad_pnts = size(gauss_quad_formula, dim=1)

    do b = 1, this%num_q_vectors
       this%inc_E_field_ampl(:, b) = inc_E_field_ampl(:, b)
       this%inc_H_field_ampl(:, b) = inc_H_field_ampl(:, b)
       this%inc_wave_direction(:, b) = inc_wave_direction(:, b)
       this%inc_field_type(b) = inc_field_type(b)
       
       do m = 1, this%RWG_basis%num_bases
          m_plus_N = m + this%RWG_basis%num_bases
          this%q_vectors(m, b) = ZERO_CMPLX
          this%q_vectors(m_plus_N, b) = ZERO_CMPLX
          T_m = this%RWG_basis%get_adjacent_faces(m)
          free_vertices = this%RWG_basis%get_free_vertices(m)

          do p = 1, NUM_FACES_IN_BASIS
             face_coords = this%RWG_basis%mesh%get_face_coords(T_m(p))
             r1_obs = face_coords(1, :)
             r2_obs = face_coords(2, :)
             r3_obs = face_coords(3, :)
             p_m = this%RWG_basis%mesh%get_vertex_coords(free_vertices(p))

             sum_E_field = ZERO_CMPLX
             sum_H_field = ZERO_CMPLX
             do j = 1, num_quad_pnts
                weight = gauss_quad_formula(j, GQF_WEIGHT_IDX)
                xi = gauss_quad_formula(j, GQF_XI_IDX)
                eta = gauss_quad_formula(j, GQF_ETA_IDX)
                zeta = gauss_quad_formula(j, GQF_ZETA_IDX)
                r_obs = xi*r1_obs + eta*r2_obs + zeta*r3_obs

                call this%inc_E_and_H_field_at_obs_pnt(&
                     inc_E_field, &
                     inc_H_field, &
                     b          , &
                     r_obs)

                sum_E_field = sum_E_field &
                     + weight*dot_product(r_obs - p_m, inc_E_field)
                sum_H_field = sum_H_field &
                     + weight*dot_product(r_obs - p_m, inc_H_field)
             end do ! j

             if (p == 1) then
                prefactor = 1._wp
             else
                prefactor = -1._wp
             end if
             this%q_vectors(m, b) = this%q_vectors(m, b) + prefactor*sum_E_field
             this%q_vectors(m_plus_N, b) = this%q_vectors(m_plus_N, b) &
                  + prefactor*sum_H_field
          end do ! p
          prefactor = this%RWG_basis%get_basis_edge_length(m)/2._wp
          this%q_vectors(m, b) = prefactor*this%q_vectors(m, b)
          this%q_vectors(m_plus_N, b) = prefactor*this%q_vectors(m_plus_N, b)
       end do !i m
    end do ! b

    ! Deallocate coefficient vectors since q_vectors is changed
    if (allocated(this%expansion_coeff_alpha)) then
       deallocate(this%expansion_coeff_alpha)
       deallocate(this%expansion_coeff_beta)
    end if

  end subroutine calc_q_vectors_BBB
  
  !!----------------------------------------------------------------------------
  
  subroutine calc_PMCHW_matrix(&
       this              , &
       gauss_quad_formula, &
       BBB) 
    class (PMCHW_RWG_type)      , intent(inout)  :: this
    real(wp)   , dimension(:, :), intent(in)     :: gauss_quad_formula
    logical                     , intent(in)     :: BBB
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(:, :, :), allocatable :: EFIE_integrals
    complex(wp), dimension(:, :, :), allocatable :: MFIE_integrals
    complex(wp), dimension(:, :, :), allocatable :: D_matrices
    complex(wp), dimension(:, :, :), allocatable :: K_matrices
    real(wp)   , dimension(:, :, :), allocatable :: intgr_pnts
    complex(wp), dimension(2)                    :: wavenumbers
    complex(wp), dimension(2)                    :: Z
    complex(wp)                                  :: prefactor_1
    complex(wp)                                  :: prefactor_2
    complex(wp)                                  :: prefactor_3
    complex(wp)                                  :: prefactor_4
    real(wp)   , dimension(2, 3)                 :: r1_obs
    real(wp)   , dimension(2, 3)                 :: r2_obs
    real(wp)   , dimension(2, 3)                 :: r3_obs
    real(wp)   , dimension(2, 3)                 :: p_m
    real(wp)   , dimension(3, 3)                 :: face_coords
    integer    , dimension(2)                    :: free_vertices
    integer    , dimension(2)                    :: T_m
    integer , parameter                          :: NUM_EFIE_INTGR = 9
    integer , parameter                          :: NUM_MFIE_INTGR = 18
    integer , parameter                          :: NUM_REGIONS = 2
    real(wp)                                     :: centroid_separation_dist
    logical                                      :: faces_are_close
    integer                                      :: num_intgr_pnts
    integer                                      :: i
    integer                                      :: j
    integer                                      :: k
    integer                                      :: p
    integer                                      :: q
    integer                                      :: m
    integer                                      :: n
    integer                                      :: l

    if (.not. allocated(this%PMCHW_matrix)) then
       print *, 'Error: PMCHW_RWG_mod: calc_PMCHW_matrix'
       print *, '       q_vectors not allocated. PMCHW_RWG_type need to be' , &
            'initialised by calling internal procedure "initialise"'
       stop 1
    end if

    !----------------------------------------------------!
    ! Speed improvement possibilities                    !
    ! - Store vertex coordinates of faces                !
    ! - Store free vertices of faces                     !
    ! - Store Green's functions for all possible source  !
    !   and observation points. This allows one to reuse !
    !   them if necesarry                                !
    ! - Store faces_are_close variable                   !
    ! - Store face-coordinates in matrix so that         !
    ! - Store dot and cross product of face-pair         !
    !   vertices                                         !
    ! - Factor out 4PI from the Green's function         !
    ! - Calculate integrals over subtracted terms in a   !
    !   face-by-face approach                            !
    ! - Store edge lengths of faces                      !
    ! - Store unit normals of faces                      !
    !                                                    !
    ! Memory improvement possibilities                   !
    ! - Calculate integration points on the go           !
    ! - Calculate outer integrals on the go              !
    ! - Calculate inner (=all) integrals on the go       !
    ! - Find a way to avoid allocating memory for both a !
    !   CFIE matrix and the D^i and H^i matrices         !
    !----------------------------------------------------!
    
    ! Iterators:
    !  p - triangles
    !  q - triangles
    !  i - regions
    !  j - Outer quadrature points
    !  k - Inner quadrature points
    !  l - spatial dimensions
    !  m - bases
    !  n - bases
    !  o - other


    num_intgr_pnts = size(gauss_quad_formula, dim=1)
    allocate(D_matrices(NUM_REGIONS, this%RWG_basis%num_bases, &
         this%RWG_basis%num_bases))
    allocate(K_matrices(NUM_REGIONS, this%RWG_basis%num_bases, &
         this%RWG_basis%num_bases))
    if (.not. BBB) then
       allocate(intgr_pnts(this%RWG_basis%mesh%num_faces, num_intgr_pnts, &
            SPATIAL_DIM))
       allocate(EFIE_integrals(this%RWG_basis%mesh%num_faces, &
            this%RWG_basis%mesh%num_faces, &
            NUM_EFIE_INTGR))
       allocate(MFIE_integrals(this%RWG_basis%mesh%num_faces, &
            this%RWG_basis%mesh%num_faces, &
            NUM_MFIE_INTGR))
       
       ! Calculate integration points for each triangle (face)
       do p = 1, this%RWG_basis%mesh%num_faces
          face_coords = this%RWG_basis%mesh%get_face_coords(p)
          do j = 1, num_intgr_pnts
             intgr_pnts(p, j, :) = ZERO
             do l = 1, SPATIAL_DIM
                intgr_pnts(p, j, :) = intgr_pnts(p, j, :) &
                     + gauss_quad_formula(j, l + 1)*face_coords(l, :)
             end do
          end do
       end do
    end if

    ! Repeats over all regions
    do i = 1, NUM_REGIONS
       wavenumbers(i) = this%angular_frequency &
            *sqrt(this%permeabilities(i)*this%permitivities(i))
       if (.not. BBB) then
          do p = 1, this%RWG_basis%mesh%num_faces
             do q = 1, this%RWG_basis%mesh%num_faces 
                ! If centroid-centroid separation is large enough, use only
                ! centroid as quadrature point for outer integral. Else,
                ! extract singularity from Green's function.
                faces_are_close = this%are_obs_pnt_and_src_close(&
                     this%face_centroid(&
                     p,               &
                     num_intgr_pnts,  &
                     intgr_pnts(p, :, :)), &
                     this%face_centroid(&
                     q,               &
                     num_intgr_pnts,  &
                     intgr_pnts(q, :, :)), &
                     wavenumbers(i)        , &
                     p=p                   , &
                     q=q)
                call eval_outer_integrals(&
                     EFIE_integrals(p, q, :), &
                     MFIE_integrals(p, q, :), &
                     intgr_pnts(p, :, :)    , &
                     intgr_pnts(q, :, :)    , &
                     gauss_quad_formula     , &
                     faces_are_close        , &
                     p                      , &
                     q                      , &
                     wavenumbers(i))
                if (any(isnan(EFIE_integrals(p, q, :)%re))) then
                   print *, 'EFIE_integral%re is nan - ', p, q
                end if
                if (any(isnan(MFIE_integrals(p, q, :)%re))) then
                   print *, 'MFIE_integral%re is nan - ', p, q
                end if
                if (any(isnan(EFIE_integrals(p, q, :)%im))) then
                   print *, 'EFIE_integral%im is nan - ', p, q
                end if
                if (any(isnan(MFIE_integrals(p, q, :)%im))) then
                   print *, 'MFIE_integral%im is nan - ', p, q
                end if
             end do ! q
             if (modulo(p, 100) == 0) then
                print *, 'Region, p, num_faces:', i, p, this%RWG_basis%mesh%num_faces
          end if
       end do ! p
    end if
       
       ! Evaluate the EFIE matrix D^i and MFIE matrix K^i
       do m = 1, this%RWG_basis%num_bases
          if (modulo(m, 100) == 0) then
             print *, 'Region, m, num_bases:', i, m, this%RWG_basis%num_bases
          end if
          T_m = this%RWG_basis%get_adjacent_faces(m)
          free_vertices = this%RWG_basis%get_free_vertices(m)
          do p = 1, 2
             face_coords = this%RWG_basis%mesh%get_face_coords(T_m(p))
             r1_obs(p, :) = face_coords(1, :)
             r2_obs(p, :) = face_coords(2, :)
             r3_obs(p, :) = face_coords(3, :)
             p_m(p, :) = this%RWG_basis%mesh%get_vertex_coords(free_vertices(p))
          end do ! p
          do n = 1, this%RWG_basis%num_bases

             if (BBB) then
                call this%D_and_K_matrix_element_mn_BBB(&
                     D_matrices(i, m, n), &
                     K_matrices(i, m, n), &
                     m                  , &
                     n                  , &
                     r1_obs             , &
                     r2_obs             , &
                     r3_obs             , &
                     p_m                , &
                     wavenumbers(i)     , &
                     i                  , &
                     gauss_quad_formula)
             else
                call this%D_and_K_matrix_element_mn(&
                     D_matrices(i, m, n), &
                     K_matrices(i, m, n), &
                     m                  , &
                     n                  , &
                     r1_obs             , &
                     r2_obs             , &
                     r3_obs             , &
                     p_m                , &
                     EFIE_integrals     , &
                     MFIE_integrals     , &
                     wavenumbers(i)     , &
                     i                  , &
                     num_intgr_pnts     , &
                     intgr_pnts         , &
                     gauss_quad_formula)
             end if
          end do ! n
       end do ! m
    end do ! i


    ! At last, insert D and K matrices into CFIE matrix
    prefactor_1 = this%permeabilities(1)*this%angular_frequency/I_IMAG
    prefactor_2 = this%permeabilities(2)*this%angular_frequency/I_IMAG
    prefactor_3 = this%permitivities(1)*this%angular_frequency/I_IMAG
    prefactor_4 = this%permitivities(2)*this%angular_frequency/I_IMAG
    
    this%PMCHW_matrix(:this%RWG_basis%num_bases, :this%RWG_basis%num_bases) = &
         prefactor_1*D_matrices(1, :, :) + prefactor_2*D_matrices(2, :, :)
    this%PMCHW_matrix(:this%RWG_basis%num_bases, this%RWG_basis%num_bases + 1:)&
         = -K_matrices(1, :, :) - K_matrices(2, :, :)
    this%PMCHW_matrix(this%RWG_basis%num_bases + 1:, :this%RWG_basis%num_bases)&
         = K_matrices(1, :, :) + K_matrices(2, :, :)
    this%PMCHW_matrix(this%RWG_basis%num_bases + 1:, &
         this%RWG_basis%num_bases + 1:) = &
         prefactor_3*D_matrices(1, :, :) + prefactor_4*D_matrices(2, :, :)

    ! Deallocate coefficient vectors since PMCHW_matrix is changed
    if (allocated(this%expansion_coeff_alpha)) then
       deallocate(this%expansion_coeff_alpha)
       deallocate(this%expansion_coeff_beta)
    end if
  end subroutine calc_PMCHW_matrix
  
  !!----------------------------------------------------------------------------

  subroutine inc_E_and_H_field_at_obs_pnt(&
       this        , &
       E_field     , &
       H_field     , &
       q_vector_idx, &
       observation_pnt)
    class (PMCHW_RWG_type)             , intent(in)    :: this
    complex(wp), dimension(SPATIAL_DIM), intent(inout) :: E_field
    complex(wp), dimension(SPATIAL_DIM), intent(inout) :: H_field
    integer                            , intent(in)    :: q_vector_idx
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: observation_pnt
    ! Variables for internal use -----------------------------------------------
    complex(wp) :: wavenumber

    ! Works only for outer region observation points
    
    wavenumber = this%angular_frequency*sqrt(  &
         this%permeabilities(OUTER_REGION_IDX) &
         *this%permitivities(OUTER_REGION_IDX))
    
    if (this%inc_field_type(q_vector_idx) == INC_FIELD_TYPE_PLANE_WAVE) then
       E_Field = plane_wave(                        &
            observation_pnt                       , &
            wavenumber                            , &
            this%inc_E_field_ampl(:, q_vector_idx), &
            this%inc_wave_direction(:, q_vector_idx))
       H_Field = plane_wave(                        &
            observation_pnt                       , &
            wavenumber                            , &
            this%inc_H_field_ampl(:, q_vector_idx), &
            this%inc_wave_direction(:, q_vector_idx))
    else
       print *, 'Error: PMCHW_RWG_mod: calc_q_vectors:'
       print *, '       Only plane wave incoming fields implemented'
       stop 2
    end if

  end subroutine inc_E_and_H_field_at_obs_pnt
  
  !!----------------------------------------------------------------------------

  subroutine E_and_H_field_at_obs_pnt(&
       this              , &
       E_field           , &
       H_field           , &
       observation_pnt   , &
       gauss_quad_formula, &
       region            , &
       scattered         , &
       test)
    class (PMCHW_RWG_type)             , intent(in)    :: this
    complex(wp), dimension(:, :)       , intent(inout) :: E_field
    complex(wp), dimension(:, :)       , intent(inout) :: H_field
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: observation_pnt
    real(wp)   , dimension(:, :)       , intent(in)    :: gauss_quad_formula
    integer                            , intent(in)    :: region
    logical    , optional              , intent(in)    :: scattered
    logical    , optional                              :: test
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(:, :)   , allocatable            :: E_field_p
    complex(wp), dimension(:, :)   , allocatable            :: H_field_p
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: intgr_xi
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: intgr_eta
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: intgr_zeta
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: intgr
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: grad_intgr_xi
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: grad_intgr_eta
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: grad_intgr_zeta
    complex(wp), dimension(this%RWG_basis%mesh%num_faces)   :: grad_intgr
    complex(wp), dimension(SPATIAL_DIM)                     :: inc_E_field
    complex(wp), dimension(SPATIAL_DIM)                     :: inc_H_field
    complex(wp), dimension(SPATIAL_DIM)                     :: term1
    complex(wp), dimension(SPATIAL_DIM)                     :: term2
    complex(wp), dimension(SPATIAL_DIM)                     :: prod
    complex(wp), dimension(SPATIAL_DIM)                     :: cross_product
    complex(wp), dimension(SPATIAL_DIM)                     :: divergence
    complex(wp)                                             :: wavenumber
    complex(wp)                                             :: k2
    complex(wp)                                             :: green_func
    complex(wp)                                             :: grad_of_green_func
    real(wp)   , dimension(:, :)   , allocatable            :: quad_pnts
    real(wp)   , dimension(NUM_FACES_IN_BASIS)              :: signs
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM)  :: face_coords
    real(wp)   , dimension(SPATIAL_DIM)                     :: source_pnt
    real(wp)   , dimension(SPATIAL_DIM)                     :: r1_src
    real(wp)   , dimension(SPATIAL_DIM)                     :: r2_src
    real(wp)   , dimension(SPATIAL_DIM)                     :: r3_src
    real(wp)   , dimension(SPATIAL_DIM)                     :: p_n
    real(wp)                                        :: prefactor
    real(wp)                                        :: alpha_n
    real(wp)                                        :: beta_n
    integer    , dimension(NUM_FACES_IN_BASIS)      :: T_n
    integer    , dimension(NUM_FACES_IN_BASIS)      :: free_vertices
    integer                                         :: num_quad_pnts
    integer                                         :: q
    integer                                         :: n
    integer                                         :: j
    integer                                         :: l
    integer                                         :: b
    logical    , dimension(:)      , allocatable    :: near_singularity
    logical                                         :: obs_pnt_and_src_are_close
    logical                                         :: check_allocation
    logical                                         :: sca
    ! For exact evaluation of integrals over the subtracted termsm when --------
    ! near singularity ---------------------------------------------------------
    complex(wp)                                         :: half_of_k2
    complex(wp), dimension(SPATIAL_DIM)                 :: prod_subtr
    complex(wp), dimension(SPATIAL_DIM)                 :: div_subtr
    complex(wp), dimension(SPATIAL_DIM)                 :: cross_subtr
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: edge_unit_normals_q
    real(wp), dimension(NUM_FACE_VERTICES)              :: edge_lengths_q
    real(wp), dimension(SPATIAL_DIM)                    :: face_unit_normal_q
    real(wp)                                            :: face_area_q
    real(wp), dimension(SPATIAL_DIM)                    :: hnX1_minus_3
    real(wp), dimension(SPATIAL_DIM)                    :: X2_minus_1
    real(wp), dimension(SPATIAL_DIM)                    :: X2_plus_1
    real(wp), dimension(SPATIAL_DIM)                    :: X3_minus_1
    real(wp), dimension(SPATIAL_DIM)                    :: X3_plus_1
    real(wp), dimension(SPATIAL_DIM)                    :: X4_minus_1
    real(wp), dimension(SPATIAL_DIM)                    :: X4_plus_1
    real(wp)                                            :: X1_minus_1
    real(wp)                                            :: X1_plus_1
    !---------------------------------------------------------------------------

    
    check_allocation = .true.
    if (present(test)) then
       if (test) then
          check_allocation = .false.
       end if
    end if
    if (check_allocation) then
       if (.not. allocated(this%PMCHW_matrix) .or. &
            .not. allocated(this%q_vectors)) then
          print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
          print *, '       Object must be initialised, PMCHW_matrix and ', &
               ' q_vectors must be set, and matrix equation must be ', &
               ' solved.'
          stop 2
       else if (.not. allocated(this%expansion_coeff_alpha) .or. &
            .not. allocated(this%expansion_coeff_beta)) then
          print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
          print *, '       Matrix equation for current PMCHW_matrix and ', &
               'q_vectors has not been solved'
          stop 2
       end if
    end if
    
    if (present(scattered)) then
       sca = scattered
    else
       sca = .false.
    end if
    if (sca .and. region==INNER_REGION_IDX) then
       print *, 'Warning: Scattered field not aplicable to inner region.'
       print *, '         Calculating total field.'
    end if
       
    num_quad_pnts = size(gauss_quad_formula, dim=1)
    allocate(quad_pnts(num_quad_pnts, SPATIAL_DIM))
    allocate(E_field_p(SPATIAL_DIM, this%num_q_vectors))
    allocate(H_field_p, mold=E_field_p)
    allocate(near_singularity(this%RWG_basis%mesh%get_num_faces()))

    wavenumber = &
         this%angular_frequency*sqrt(&
         this%permeabilities(region)*this%permitivities(region))
    k2 = wavenumber**2
    half_of_k2 = k2/2._wp

    ! Calculate the integrals for each face/triangle
    do q = 1, this%RWG_basis%mesh%get_num_faces()
       ! Set observation point and source not close.
       ! Will be altered if one of the quadrature points are close to the
       ! observation point.
       near_singularity(q) = .false.

       face_coords = this%RWG_basis%mesh%get_face_coords(q)
       do j = 1, num_quad_pnts
          quad_pnts(j, :) = 0._wp
          do l = 1, SPATIAL_DIM
             quad_pnts(j, :) = quad_pnts(j, :) &
                  + gauss_quad_formula(j, l + 1)*face_coords(l, :)
          end do
          source_pnt = quad_pnts(j, :)
          obs_pnt_and_src_are_close = this%are_obs_pnt_and_src_close(&
               observation_pnt, &
               source_pnt   , &
               wavenumber)
          if (obs_pnt_and_src_are_close) then
             near_singularity(q) = .false.
          end if
       end do ! j
       !------ FOR TESTING PORPUSES -------!
       if (present(test)) then
          if (test) then
             wavenumber = ZERO_CMPLX
             obs_pnt_and_src_are_close = .false.
             do j = 1, num_quad_pnts
                quad_pnts(j, :) = observation_pnt - 1._wp/(sqrt(3._wp)*4._wp*PI)
             end do ! j
          end if
       end if
       !-----------------------------------!
       call eval_green_func_integrals( &
            intgr_xi(q)              , &
            intgr_eta(q)             , &
            intgr(q)                 , &
            grad_intgr_xi(q)         , &
            grad_intgr_eta(q)        , &
            grad_intgr(q)            , &
            observation_pnt          , &
            quad_pnts                , &
            gauss_quad_formula       , &
            wavenumber               , &
            near_singularity(q)      , &
            faces_are_in_same_plane=.false.)
       intgr_zeta(q) = intgr(q) - intgr_xi(q) - intgr_eta(q)
       grad_intgr_zeta(q) =     &
            + grad_intgr(q)     &
            - grad_intgr_xi(q) &
            - grad_intgr_eta(q)

       !------ FOR TESTING PORPUSES -------!
       if (present(test)) then
          if (test) then
             wavenumber = &
                  this%angular_frequency*sqrt(&
                  this%permeabilities(region)*this%permitivities(region))
          end if
       end if
       !-----------------------------------!
    end do ! q

    ! Calculate the E- and H-field by summing integrals over all bases
    E_field = ZERO_CMPLX
    H_field = ZERO_CMPLX
    signs = [ 1._wp, -1._wp ]
    do n = 1, this%RWG_basis%get_num_bases()
       T_n = this%RWG_basis%get_adjacent_faces(n)
       free_vertices = this%RWG_basis%get_free_vertices(n)
       prefactor = this%RWG_basis%get_basis_edge_length(n)/2._wp
       E_field_p = ZERO_CMPLX
       H_field_p = ZERO_CMPLX
       term1 = ZERO_CMPLX
       term2 = ZERO_CMPLX
       do q = 1, NUM_FACES_IN_BASIS
          face_coords = this%RWG_basis%mesh%get_face_coords(T_n(q))
          r1_src = face_coords(1, :)
          r2_src = face_coords(2, :)
          r3_src = face_coords(3, :)
          p_n = this%RWG_basis%mesh%get_vertex_coords(free_vertices(q))
          ! Calculate the integrals of
          !  1. the product of the Green's function and the basis function
          !  2. the cross product of the gradient of the Green's function and
          !     the basis function
          !  3. the gradient of the Green's function and the times the
          !     divergence of the basis function
          prod =                   &
               + r1_src*intgr_xi(T_n(q))  &
               + r2_src*intgr_eta(T_n(q))  &
               + r3_src*intgr_zeta(T_n(q)) &
               - p_n*intgr(T_n(q))
          divergence =                     &
               - r1_src*grad_intgr_xi(T_n(q))  &
               - r2_src*grad_intgr_eta(T_n(q))  &
               - r3_src*grad_intgr_zeta(T_n(q)) &
               + observation_pnt*grad_intgr(T_n(q))
          cross_product =                                 &
               ( cross_prod_3D(observation_pnt, r1_src)   &
               + cross_prod_3D(r1_src, p_n) )             &
               *grad_intgr_xi(T_n(q))                         &
               + ( cross_prod_3D(observation_pnt, r2_src) &
               + cross_prod_3D(r2_src, p_n) )             &
               *grad_intgr_eta(T_n(q))                         &
               + ( cross_prod_3D(observation_pnt, r3_src) &
               + cross_prod_3D(r3_src, p_n) )             &
               *grad_intgr_zeta(T_n(q))                        &
               - cross_prod_3D(observation_pnt, p_n)      &
               *grad_intgr(T_n(q))
          
          if (near_singularity(T_n(q))) then
             face_area_q = this%RWG_basis%mesh%face_area(T_n(q))
             face_unit_normal_q = this%RWG_basis%mesh%face_unit_normal(T_n(q))
             edge_lengths_q = this%get_edge_lengths(T_n(q))
             edge_unit_normals_q = calc_edge_unit_normals(&
                  edge_lengths_q    , &
                  face_unit_normal_q, &
                  r1_src            , &
                  r2_src            , &
                  r3_src)
             
             call inner_intgr_of_subtr_terms(&
                  hnX1_minus_3      , &
                  X1_minus_1        , &
                  X1_plus_1         , &
                  X2_minus_1        , &
                  X2_plus_1         , &
                  X3_minus_1        , &
                  X3_plus_1         , &
                  X4_minus_1        , &
                  X4_plus_1         , &
                  .false.           , &
                  observation_pnt   , &
                  r1_src            , &
                  r2_src            , &
                  r3_src            , &
                  p_n               , &
                  face_area_q       , &
                  face_unit_normal_q, &
                  edge_lengths_q    , &
                  edge_unit_normals_q)

             prod_subtr = PI4_inv/face_area_q*(X2_minus_1 - half_of_k2*X2_plus_1)
             div_subtr = PI4_inv/face_area_q*(X3_minus_1 - half_of_k2*X3_plus_1)
             cross_subtr = PI4_inv/face_area_q*(X4_minus_1 - half_of_k2*X4_plus_1)
          else
             prod_subtr = ZERO_CMPLX
             div_subtr = ZERO_CMPLX
             cross_subtr = ZERO_CMPLX
          end if
             
          ! Calculate the resulting feilds using the expansion coefficients.
          ! Iterate over all solutions.
          if (q == 1) then
             term1 = term1 + (prod + prod_subtr - 2._wp/k2*(divergence + div_subtr))
             term2 = term2 + (cross_product + cross_subtr)
          else
             term1 = term1 - (prod + prod_subtr - 2._wp/k2*(divergence + div_subtr))
             term2 = term2 - (cross_product + cross_subtr)
          end if
       end do ! q

       do b = 1, this%num_q_vectors
          !------ FOR TESTING PORPUSES -------!
          if (present(test)) then
             if (test) then
                alpha_n = 1._wp
                beta_n = 1._wp
             else
                alpha_n = this%expansion_coeff_alpha(n, b)
                beta_n = this%expansion_coeff_beta(n, b)
             end if
             !-----------------------------------!
          else
             alpha_n = this%expansion_coeff_alpha(n, b)
             beta_n = this%expansion_coeff_beta(n, b)
          end if
          E_field(:, b) = E_field(:, b) + prefactor*( &
               -alpha_n*this%permeabilities(region)*this%angular_frequency &
               /I_IMAG*term1 + beta_n*term2 )
          H_field(:, b) = H_field(:, b) + prefactor*( &
               -beta_n*this%permitivities(region)*this%angular_frequency &
               /I_IMAG*term1 - alpha_n*term2 )
       end do ! end b
    end do ! end n

    do b = 1, this%num_q_vectors
       if ( region == INNER_REGION_IDX ) then
          E_field(:, b) = -E_field(:, b) 
          H_field(:, b) = -H_field(:, b)
       else if (.not. sca) then
          call this%inc_E_and_H_field_at_obs_pnt(&
               inc_E_field, &
               inc_H_field, &
               b          , &
               observation_pnt)
          E_field(:, b) = E_field(:, b) + inc_E_field
          H_field(:, b) = H_field(:, b) + inc_H_field
       else
          continue
       end if
       
    end do ! b
  end subroutine E_and_H_field_at_obs_pnt
       
  !!----------------------------------------------------------------------------

  subroutine E_and_H_field_at_obs_pnt_BBB(&
       this              , &
       E_field           , &
       H_field           , &
       observation_pnt   , &
       gauss_quad_formula, &
       region            , &
       scattered         , &
       test)
    class (PMCHW_RWG_type)             , intent(in)    :: this
    complex(wp), dimension(:, :)       , intent(inout) :: E_field
    complex(wp), dimension(:, :)       , intent(inout) :: H_field
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: observation_pnt
    real(wp)   , dimension(:, :)       , intent(in)    :: gauss_quad_formula
    integer                            , intent(in)    :: region
    logical    , optional              , intent(in)    :: scattered
    logical    , optional                              :: test
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(SPATIAL_DIM)        :: inc_E_field
    complex(wp), dimension(SPATIAL_DIM)        :: inc_H_field
    complex(wp), dimension(SPATIAL_DIM)        :: term1
    complex(wp), dimension(SPATIAL_DIM)        :: term2
    complex(wp), dimension(SPATIAL_DIM)        :: inner_sum1
    complex(wp), dimension(SPATIAL_DIM)        :: inner_sum2
    complex(wp), dimension(SPATIAL_DIM)        :: inner_sum3
    complex(wp)                                :: wavenumber
    complex(wp)                                :: kR
    complex(wp)                                :: k2
    complex(wp)                                :: green_func
    complex(wp)                                :: grad_of_green_func
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM)  :: face_coords
    real(wp)   , dimension(SPATIAL_DIM)        :: r_src
    real(wp)   , dimension(SPATIAL_DIM)        :: r1_src
    real(wp)   , dimension(SPATIAL_DIM)        :: r2_src
    real(wp)   , dimension(SPATIAL_DIM)        :: r3_src
    real(wp)   , dimension(SPATIAL_DIM)        :: p_n
    real(wp)                                   :: R
    real(wp)                                   :: R_inv
    real(wp)                                   :: prefactor
    real(wp)                                   :: alpha_n
    real(wp)                                   :: beta_n
    real(wp)                                   :: weight
    real(wp)                                   :: xi
    real(wp)                                   :: eta
    real(wp)                                   :: zeta
    integer    , dimension(NUM_FACES_IN_BASIS) :: T_n
    integer    , dimension(NUM_FACES_IN_BASIS) :: free_vertices
    integer                                    :: num_quad_pnts
    integer                                    :: p
    integer                                    :: n
    integer                                    :: b
    integer                                    :: k
    logical                                    :: pnts_are_close
    logical                                    :: check_allocation
    !---------------------------------------------------------------------------

    check_allocation = .true.
    if (present(test)) then
       if (test) then
          check_allocation = .false.
       end if
    end if
    if (check_allocation) then
       if (.not. allocated(this%PMCHW_matrix) .or. &
            .not. allocated(this%q_vectors)) then
          print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
          print *, '       Object must be initialised, PMCHW_matrix and ', &
               ' q_vectors must be set, and matrix equation must be ', &
               ' solved.'
          stop 2
       else if (.not. allocated(this%expansion_coeff_alpha) .or. &
            .not. allocated(this%expansion_coeff_beta)) then
          print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
          print *, '       Matrix equation for current PMCHW_matrix and ', &
               'q_vectors has not been solved'
          stop 2
       end if
    end if
       
    num_quad_pnts = size(gauss_quad_formula, dim=1)

    wavenumber = &
         this%angular_frequency*sqrt(&
         this%permeabilities(region)*this%permitivities(region))
    k2 = wavenumber**2

    E_field = ZERO_CMPLX
    H_field = ZERO_CMPLX
    do n = 1, this%RWG_basis%num_bases
       T_n = this%RWG_basis%get_adjacent_faces(n)
       free_vertices = this%RWG_basis%get_free_vertices(n)
       prefactor = this%RWG_basis%get_basis_edge_length(n)/2._wp

       term1 = ZERO_CMPLX
       term2 = ZERO_CMPLX
       do p = 1, NUM_FACES_IN_BASIS
          face_coords = this%RWG_basis%mesh%get_face_coords(T_n(p))
          r1_src = face_coords(1, :)
          r2_src = face_coords(2, :)
          r3_src = face_coords(3, :)
          p_n = this%RWG_basis%mesh%get_vertex_coords(free_vertices(p))

          inner_sum1 = ZERO_CMPLX ! sum of G(r,r')f(r')
          inner_sum2 = ZERO_CMPLX ! sum of nabla*G(r,r')f(r')
          inner_sum3 = ZERO_CMPLX ! sum of nabla'*G(r,r') x f(r')
          do k = 1, num_quad_pnts
             weight = gauss_quad_formula(k, GQF_WEIGHT_IDX)
             xi = gauss_quad_formula(k, GQF_XI_IDX)
             eta = gauss_quad_formula(k, GQF_ETA_IDX)
             zeta = gauss_quad_formula(k, GQF_ZETA_IDX)
             r_src = xi*r1_src + eta*r2_src + zeta*r3_src

             R = norm2(observation_pnt - r_src)
             kR = wavenumber*R
             
             if ( this%are_obs_pnt_and_src_close(&
                  observation_pnt, &
                  r_src          , &
                  wavenumber) ) then
                pnts_are_close = .true.
                call green_func_smoothened(&
                     green_func        , &
                     grad_of_green_func, &
                     wavenumber        , &
                     kR                , &
                     R                 , &
                     .false.)
             else
                R_inv = 1._wp/R
                pnts_are_close = .false.
                green_func = exp(I_IMAG*kR)*PI4_inv*R_inv
                grad_of_green_func = R_inv*green_func &
                     *(R_inv - I_IMAG*wavenumber)
             end if

             inner_sum1 = inner_sum1 + weight*green_func*(r_src - p_n)
             inner_sum2 = inner_sum2 + weight*grad_of_green_func &
                  *(observation_pnt - r_src)
             inner_sum3 = inner_sum3 + weight*grad_of_green_func &
                  *cross_prod_3D(observation_pnt - r_src, r_src - p_n)
          end do ! k
          

          if (p == 1) then
             term1 = term1 + inner_sum1 - 2._wp/k2*inner_sum2
             term2 = term2 + inner_sum3
          else
             term1 = term1 - inner_sum1 + 2._wp/k2*inner_sum2
             term2 = term2 - inner_sum3
          end if
       end do ! p
       
       do b = 1, this%num_q_vectors
          alpha_n = this%expansion_coeff_alpha(n, b)
          beta_n = this%expansion_coeff_beta(n, b)

          E_field(:, b) = E_field(:, b) + prefactor*( &
               -alpha_n*this%permeabilities(region)*this%angular_frequency &
               /I_IMAG*term1 + beta_n*term2 )
          H_field(:, b) = H_field(:, b) + prefactor*( &
               -beta_n*this%permitivities(region)*this%angular_frequency &
               /I_IMAG*term1 - alpha_n*term2 )

       end do ! b
    end do ! n
       
    do b = 1, this%num_q_vectors
       if ( region == OUTER_REGION_IDX .and. &
            (.not. present(scattered) .or. &
            (present(scattered) .and. scattered .eqv. .false.)) ) then
          call this%inc_E_and_H_field_at_obs_pnt(&
               inc_E_field, &
               inc_H_field, &
               b          , &
               observation_pnt)
          E_field(:, b) = E_field(:, b) + inc_E_field
          H_field(:, b) = H_field(:, b) + inc_H_field
       else if ( region == OUTER_REGION_IDX .and. &
            (present(scattered) .and. scattered .eqv. .true.) ) then
          continue
       else if (region == INNER_REGION_IDX .and. &
            present(scattered) .and. scattered .eqv. .true. ) then
          print *, 'Warning: Scattered field not aplicable to inner region.'
       else
          E_field(:, b) = -E_field(:, b) 
          H_field(:, b) = -H_field(:, b)
       end if
    end do ! b
  end subroutine E_and_H_field_at_obs_pnt_BBB
  
  !!----------------------------------------------------------------------------

  subroutine D_and_K_matrix_element_mn(&
       this          , &
       D_mn          , &
       K_mn          , &
       m             , &
       n             , &
       r1_obs        , &
       r2_obs        , &
       r3_obs        , &
       p_m           , &
       EFIE_integrals, &
       MFIE_integrals, &
       wavenumber    , &
       region        , &
       num_intgr_pnts, &
       intgr_pnts    , &
       gauss_quad_formula)
    class (PMCHW_RWG_type)         , intent(in)    :: this
    complex(wp)                    , intent(inout) :: D_mn
    complex(wp)                    , intent(inout) :: K_mn
    integer                        , intent(in)    :: m
    integer                        , intent(in)    :: n
    real(wp)   , dimension(:, :)   , intent(in)    :: r1_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: r2_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: r3_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: p_m
    complex(wp), dimension(:, :, :), intent(in)    :: EFIE_integrals
    complex(wp), dimension(:, :, :), intent(in)    :: MFIE_integrals
    complex(wp)                    , intent(in)    :: wavenumber
    integer                        , intent(in)    :: region
    integer                        , intent(in)    :: num_intgr_pnts
    real(wp)   , dimension(:, :, :), intent(in)    :: intgr_pnts
    real(wp)   , dimension(:, :)   , intent(in)    :: gauss_quad_formula
    ! Variables for internal use -----------------------------------------------
    complex(wp)                                    :: D_mn_pq
    complex(wp)                                    :: K_mn_pq
    complex(wp)                                    :: subtr_terms_D_mn_pq
    complex(wp)                                    :: subtr_terms_K_mn_pq
    real(wp)   , dimension(SPATIAL_DIM)            :: r1_src
    real(wp)   , dimension(SPATIAL_DIM)            :: r2_src
    real(wp)   , dimension(SPATIAL_DIM)            :: r3_src
    real(wp)   , dimension(SPATIAL_DIM)            :: p_n
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: face_coords
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) ::edge_unit_normals_p
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) ::edge_unit_normals_q
    real(wp)   , dimension(NUM_FACE_VERTICES)      :: edge_lengths_p
    real(wp)   , dimension(NUM_FACE_VERTICES)      :: edge_lengths_q
    real(wp)   , dimension(SPATIAL_DIM)            :: face_unit_normal_p
    real(wp)   , dimension(SPATIAL_DIM)            :: face_unit_normal_q
    real(wp)                                       :: face_area_p
    real(wp)                                       :: face_area_q
    integer    , dimension(NUM_FACES_IN_BASIS)     :: free_vertices
    integer    , dimension(NUM_FACES_IN_BASIS)     :: T_m
    integer    , dimension(NUM_FACES_IN_BASIS)     :: T_n
    real(wp)                                       :: prefactor
    logical                                        :: faces_are_close
    logical                                        :: R_is_zero
    integer                                        :: p
    integer                                        :: q
    integer                                        :: l
    integer, dimension(48, 2) :: faces_in_plane
    integer :: i
          
    T_m = this%RWG_basis%get_adjacent_faces(m)
    T_n = this%RWG_basis%get_adjacent_faces(n)
    free_vertices = this%RWG_basis%get_free_vertices(n)
    
    D_mn = ZERO_CMPLX
    K_mn = ZERO_CMPLX
    do p = 1, NUM_FACES_IN_BASIS
       do q = 1, NUM_FACES_IN_BASIS

          ! Increase speed by storing r1_src, etc. for both values of q
          face_coords = this%RWG_basis%mesh%get_face_coords(T_n(q))
          r1_src = face_coords(1, :)
          r2_src = face_coords(2, :)
          r3_src = face_coords(3, :)
          p_n = this%RWG_basis%mesh%get_vertex_coords(free_vertices(q))
          ! EFIE
          D_mn_pq = face_pair_integral_EFIE(&
               r1_obs(p, :), &
               r2_obs(p, :), &
               r3_obs(p, :), &
               r1_src      , &
               r2_src      , &
               r3_src      , &
               p_m(p, :)   , &
               p_n         , &
               wavenumber  , &
               EFIE_integrals(T_m(p), T_n(q), :))
               
          ! MFIE
          if (T_m(p) == T_n(q)) then
             ! Faces are equal. The inner integral will then be orthogonal
             ! to the basis function f_m(r), and the double integral vanishes.
             K_mn_pq = ZERO_CMPLX
          else
             K_mn_pq = face_pair_integral_MFIE(&
                  r1_obs(p, :), &
                  r2_obs(p, :), &
                  r3_obs(p, :), &
                  r1_src      , &
                  r2_src      , &
                  r3_src      , &
                  p_m(p, :)   , &
                  p_n         , &
                  MFIE_integrals(T_m(p), T_n(q), :)) 
          end if
          if (isnan(D_mn_pq%re)) then
             print *, '(m, n)', [m, n], '(p, q)', [p, q], '(T^p, T^q)', &
                  [T_m(p), T_n(q)], 'D_mn_pq is NaN'
          end if
          if (isnan(K_mn_pq%re)) then
             print *, '(m, n)', [m, n], '(p, q)', [p, q], '(T^p, T^q)', &
                  [T_m(p), T_n(q)], 'K_mn_pq is NaN'
          end if

          
          ! If faces are close, add evaluation of integrals over
          ! subtracted terms
          faces_are_close = this%are_obs_pnt_and_src_close(&
               this%face_centroid(&
                 T_m(p)         , &
                 num_intgr_pnts , &    
                 intgr_pnts(T_m(p), :, :)), &
               this%face_centroid(&
                 T_n(q)         , &
                 num_intgr_pnts , &
                 intgr_pnts(T_n(q), :, :)), &
               wavenumber            , &
               p=T_m(p)              , &
               q=T_n(q))              
          if (faces_are_close .and. .not. CAUCHY) then
             face_area_q = this%RWG_basis%mesh%face_area(T_n(q))
             face_unit_normal_q = this%RWG_basis%mesh%face_unit_normal(T_n(q))
             edge_lengths_q = this%get_edge_lengths(T_n(q))
             edge_unit_normals_q = calc_edge_unit_normals(&
                  edge_lengths_q    , &
                  face_unit_normal_q, &
                  r1_src            , &
                  r2_src            , &
                  r3_src)
             face_area_p = this%RWG_basis%mesh%face_area(T_m(p))
             face_unit_normal_p = this%RWG_basis%mesh%face_unit_normal(T_m(p))
             edge_lengths_p = this%get_edge_lengths(T_m(p))
             edge_unit_normals_p = calc_edge_unit_normals(&
                  edge_lengths_p    , &
                  face_unit_normal_p, &
                  r1_obs(p, :)      , &
                  r2_obs(p, :)      , &
                  r3_obs(p, :))
             

             call eval_subtracted_terms(&
                  subtr_terms_D_mn_pq, &
                  subtr_terms_K_mn_pq, &
                  wavenumber         , &
                  r1_obs(p, :)       , &
                  r2_obs(p, :)       , &
                  r3_obs(p, :)       , &
                  r1_src             , &
                  r2_src             , &
                  r3_src             , &
                  p_m(p, :)          , &
                  p_n                , &
                  gauss_quad_formula , &
                  T_m(p)             , &
                  T_n(q)             , &
                  face_area_p        , &
                  face_area_q        , &
                  face_unit_normal_p , &
                  face_unit_normal_q , &
                  edge_lengths_p     , &
                  edge_lengths_q     , &
                  edge_unit_normals_p, &
                  edge_unit_normals_q)
             
             if (isnan(subtr_terms_D_mn_pq%re)) then
                print *, '(m, n)', [m, n], '(p, q)', [p, q], '(T^p, T^q)', &
                     [T_m(p), T_n(q)], 'subtr_terms_D_mn_pq is NaN for reals'
             end if
             if (isnan(subtr_terms_K_mn_pq%re)) then
                print *, '(m, n)', [m, n], '(p, q)', [p, q], '(T^p, T^q)', &
                     [T_m(p), T_n(q)], 'subtr_terms_K_mn_pq is NaN for reals'
             end if
             
             D_mn_pq = D_mn_pq + subtr_terms_D_mn_pq
             K_mn_pq = K_mn_pq + subtr_terms_K_mn_pq
          end if

          if (p + q == 3) then
             D_mn = D_mn - D_mn_pq
             K_mn = K_mn - K_mn_pq
          else
             D_mn = D_mn + D_mn_pq
             K_mn = K_mn + K_mn_pq
          end if
          R_is_zero = .false.
       end do ! q
    end do ! p
    ! Multiply by prefactor L_m*L_n/4 and add to matrices
    prefactor = this%RWG_basis%get_basis_edge_length(m) &
         *this%RWG_basis%get_basis_edge_length(n)       &
         /4.0_wp
    D_mn = D_mn*prefactor
    K_mn = K_mn*prefactor

  end subroutine D_and_K_matrix_element_mn
             
  !!----------------------------------------------------------------------------

  subroutine D_and_K_matrix_element_mn_BBB(&
       this          , &
       D_mn          , &
       K_mn          , &
       m             , &
       n             , &
       r1_obs        , &
       r2_obs        , &
       r3_obs        , &
       p_m           , &
       wavenumber    , &
       region        , &
       gauss_quad_formula)
    class (PMCHW_RWG_type)         , intent(in)    :: this
    complex(wp)                    , intent(inout) :: D_mn
    complex(wp)                    , intent(inout) :: K_mn
    integer                        , intent(in)    :: m
    integer                        , intent(in)    :: n
    real(wp)   , dimension(:, :)   , intent(in)    :: r1_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: r2_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: r3_obs
    real(wp)   , dimension(:, :)   , intent(in)    :: p_m
    complex(wp)                    , intent(in)    :: wavenumber
    integer                        , intent(in)    :: region
    real(wp)   , dimension(:, :)   , intent(in)    :: gauss_quad_formula
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(SPATIAL_DIM)            :: inner_sum
    complex(wp), dimension(SPATIAL_DIM)            :: inner_sum_gradient
    complex(wp)                                    :: D_mn_pq
    complex(wp)                                    :: K_mn_pq
    complex(wp)                                    :: test_term
    complex(wp)                                    :: main_term
    complex(wp)                           :: int_xi_I_zeta
    complex(wp)                           :: int_eta_I_zeta
    complex(wp)                           :: int_I_zeta
    complex(wp)                                    :: term2
    complex(wp)                                    :: E1
    complex(wp)                                    :: E2
    complex(wp)                                    :: E3
    complex(wp)                                    :: E4
    complex(wp)                                    :: E5
    complex(wp)                                    :: E6
    complex(wp)                                    :: E7
    complex(wp)                                    :: E8
    complex(wp)                                    :: E9
    complex(wp)                                    :: kR
    complex(wp)                                    :: subtr_terms_D_mn_pq
    complex(wp)                                    :: subtr_terms_K_mn_pq
    complex(wp)                                    :: green_func
    complex(wp)                                    :: grad_of_green_func
    complex(wp)                                    :: inner_sum2
    real(wp)   , dimension(SPATIAL_DIM)            :: r_obs
    real(wp)   , dimension(SPATIAL_DIM)            :: r_src
    real(wp)   , dimension(SPATIAL_DIM)            :: r1_src
    real(wp)   , dimension(SPATIAL_DIM)            :: r2_src
    real(wp)   , dimension(SPATIAL_DIM)            :: r3_src
    real(wp)   , dimension(SPATIAL_DIM)            :: p_n
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: face_coords
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) ::edge_unit_normals_p
    real(wp)   , dimension(NUM_FACE_VERTICES, SPATIAL_DIM) ::edge_unit_normals_q
    real(wp)   , dimension(NUM_FACE_VERTICES)      :: edge_lengths_p
    real(wp)   , dimension(NUM_FACE_VERTICES)      :: edge_lengths_q
    real(wp)   , dimension(SPATIAL_DIM)            :: face_unit_normal_p
    real(wp)   , dimension(SPATIAL_DIM)            :: face_unit_normal_q
    real(wp)                                       :: face_area_p
    real(wp)                                       :: face_area_q
    real(wp)                                       :: R
    real(wp)                                       :: R_inv
    real(wp)                                       :: weight_J
    real(wp)                                       :: weight_k
    real(wp)                                       :: alpha
    real(wp)                                       :: beta
    real(wp)                                       :: gamma
    real(wp)                                       :: xi
    real(wp)                                       :: eta
    real(wp)                                       :: zeta
    integer    , dimension(NUM_FACES_IN_BASIS)     :: free_vertices
    integer    , dimension(NUM_FACES_IN_BASIS)     :: T_m
    integer    , dimension(NUM_FACES_IN_BASIS)     :: T_n
    real(wp)                                       :: prefactor
    logical                                        :: faces_are_close
    logical                                        :: R_is_zero
    integer                                        :: num_quad_pnts
    integer                                        :: p
    integer                                        :: q
    integer                                        :: j
    integer                                        :: k
    integer, dimension(24, 2) :: faces_in_plane
    integer :: i
    
    num_quad_pnts = size(gauss_quad_formula, dim=1)
    T_m = this%RWG_basis%get_adjacent_faces(m)
    T_n = this%RWG_basis%get_adjacent_faces(n)
    free_vertices = this%RWG_basis%get_free_vertices(n)
    D_mn = ZERO_CMPLX
    K_mn = ZERO_CMPLX
    do p = 1, NUM_FACES_IN_BASIS
       do q = 1, NUM_FACES_IN_BASIS
          faces_are_close = .false.
          R_is_zero = .false.
          face_coords = this%RWG_basis%mesh%get_face_coords(T_n(q))
          ! More efficient to store rx_src for both q-values in beginning
          r1_src = face_coords(1, :)
          r2_src = face_coords(2, :)
          r3_src = face_coords(3, :)
          p_n = this%RWG_basis%mesh%get_vertex_coords(free_vertices(q))

          D_mn_pq = ZERO_CMPLX
          K_mn_pq = ZERO_CMPLX
          test_term = ZERO_CMPLX
          main_term = ZERO_CMPLX
          do j = 1, num_quad_pnts
             weight_j = gauss_quad_formula(j, GQF_WEIGHT_IDX)
             alpha = gauss_quad_formula(j, GQF_XI_IDX)
             beta = gauss_quad_formula(j, GQF_ETA_IDX)
             gamma = gauss_quad_formula(j, GQF_ZETA_IDX)
             ! More efficient to store r_obs
             r_obs = r1_obs(p, :)*alpha + r2_obs(p, :)*beta &
                  + r3_obs(p, :)*gamma

             inner_sum = ZERO_CMPLX
             inner_sum2 = ZERO_CMPLX
             inner_sum_gradient = ZERO_CMPLX
             do k = 1, num_quad_pnts
                weight_k = gauss_quad_formula(k, GQF_WEIGHT_IDX)
                xi = gauss_quad_formula(k, GQF_XI_IDX)
                eta = gauss_quad_formula(k, GQF_ETA_IDX)
                zeta = gauss_quad_formula(k, GQF_ZETA_IDX)
                ! More efficient to store r_src
                r_src = r1_src*xi + r2_src*eta + r3_src*zeta

                R = norm2(r_obs - r_src)
                kR = wavenumber*R
                
                ! This check could be done much more efficiently
                if ( this%are_obs_pnt_and_src_close(&
                     this%face_centroid( &
                     T_m(p)            , &
                     0                 , &    
                     gauss_quad_formula), &
                     this%face_centroid( &
                     T_n(q)            , &
                     0                 , &
                     gauss_quad_formula), &
                     wavenumber                   , &
                     p=T_m(p)                     , &
                     q=T_n(q)) ) then
                   faces_are_close = .true.
                   if (is_close(R, 0._wp)) then
                      R_is_zero = .true.
                   end if
                   if (CAUCHY) then
                      ! Cauchy Principal value for singularity in green's function
                      call Cauchy_principal_value(&
                           green_func        , &
                           grad_of_green_func, &
                           wavenumber        , &
                           kR                , &
                           R)
                   else
                      call green_func_smoothened(&
                           green_func        , &
                           grad_of_green_func, &
                           wavenumber        , &
                           kR                , &
                           R                 , &
                           T_m(p)==T_n(q))
                   end if
                else
                   ! This should never occur
                   if (is_close(R, 0._wp)) then
                      R_is_zero = .true.
                      print *, 'Error: R is zero, but faces are not close'
                      stop 2
                   else
                     R_inv = 1._wp/R
                     green_func = calc_green_func(wavenumber, kR, R_inv)
                     if (.not. T_m(p)==T_n(p)) then
                        ! No reason to waste computation time on integrals that will vanish
                        ! because the faces are on the same plane. Which they are if p and q
                        ! points are the same face.
                        grad_of_green_func = calc_grad_of_green_func(&
                             wavenumber, &
                             kR        , &
                             R_inv     , &
                             green_func=green_func)
                     end if
                   end if
                end if

                inner_sum = inner_sum + weight_k*green_func*(r_src - p_n)
                inner_sum2 = inner_sum2 + weight_k*green_func
                inner_sum_gradient = inner_sum_gradient &
                     + weight_k*grad_of_green_func &
                     *cross_prod_3D(r_obs - r_src, r_src - p_n)

             end do ! k

             D_mn_pq = D_mn_pq + weight_j*dot_product(r_obs - p_m(p, :), &
                  inner_sum)
             D_mn_pq = D_mn_pq - 4._wp/wavenumber**2*weight_j*inner_sum2
             K_mn_pq = K_mn_pq + weight_j*dot_product(r_obs - p_m(p, :), &
                  inner_sum_gradient)
             
          end do ! j

          if (faces_are_close .and. .not. CAUCHY) then
             face_area_q = this%RWG_basis%mesh%face_area(T_n(q))
             face_unit_normal_q = this%RWG_basis%mesh%face_unit_normal(T_n(q))
             edge_lengths_q = this%get_edge_lengths(T_n(q))
             edge_unit_normals_q = calc_edge_unit_normals(&
                  edge_lengths_q    , &
                  face_unit_normal_q, &
                  r1_src            , &
                  r2_src            , &
                  r3_src)
             face_area_p = this%RWG_basis%mesh%face_area(T_m(p))
             face_unit_normal_p = this%RWG_basis%mesh%face_unit_normal(T_m(p))
             edge_lengths_p = this%get_edge_lengths(T_m(p))
             edge_unit_normals_p = calc_edge_unit_normals(&
                  edge_lengths_p    , &
                  face_unit_normal_p, &
                  r1_obs(p, :)      , &
                  r2_obs(p, :)      , &
                  r3_obs(p, :))

             call eval_subtracted_terms(&
                  subtr_terms_D_mn_pq, &
                  subtr_terms_K_mn_pq, &
                  wavenumber         , &
                  r1_obs(p, :)       , &
                  r2_obs(p, :)       , &
                  r3_obs(p, :)       , &
                  r1_src             , &
                  r2_src             , &
                  r3_src             , &
                  p_m(p, :)          , &
                  p_n                , &
                  gauss_quad_formula , &
                  T_m(p)             , &
                  T_n(q)             , &
                  face_area_p        , &
                  face_area_q        , &
                  face_unit_normal_p , &
                  face_unit_normal_q , &
                  edge_lengths_p     , &
                  edge_lengths_q     , &
                  edge_unit_normals_p, &
                  edge_unit_normals_q)
             
             
             D_mn_pq = D_mn_pq + subtr_terms_D_mn_pq
             K_mn_pq = K_mn_pq + subtr_terms_K_mn_pq
          end if

          if (p + q == 3) then
             D_mn = D_mn - D_mn_pq
             K_mn = K_mn - K_mn_pq
          else
             D_mn = D_mn + D_mn_pq
             K_mn = K_mn + K_mn_pq
          end if
       end do ! q
    end do ! p
    ! Multiply by prefactor L_m*L_n/4 and add to matrices
    prefactor = this%RWG_basis%get_basis_edge_length(m) &
         *this%RWG_basis%get_basis_edge_length(n)       &
         /4.0_wp
    D_mn = D_mn*prefactor
    K_mn = K_mn*prefactor

  end subroutine D_and_K_matrix_element_mn_BBB
  
  !!----------------------------------------------------------------------------

  subroutine solve_matrix_equation(this, numerical_method_in)
    class (PMCHW_RWG_type), intent(inout) :: this
    integer, optional     , intent(in)    :: numerical_method_in
    ! Variables for internal use -----------------------------------------------
    integer                                   :: numerical_method
    complex(wp), dimension(:, :), allocatable :: A_matrix
    complex(wp), dimension(:, :), allocatable :: B_vectors
    integer    , dimension(:)   , allocatable :: IPIV
    integer                                   :: NN
    integer                                   :: NRHS
    integer                                   :: LDA
    integer                                   :: LDB
    integer                                   :: INFO
    integer                                   :: m
    integer                                   :: n
    integer                                   :: b

    if (.not. allocated(this%PMCHW_matrix) .or. &
         .not. allocated(this%q_vectors)) then
       print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
       print *, '       Object must be initialised, PMCHW_matrix and ', &
            ' q_vectors must be set. '
       stop 2
    end if

    allocate(this%expansion_coeff_alpha(&
         this%RWG_basis%num_bases, this%num_q_vectors))
    allocate(this%expansion_coeff_beta, mold=this%expansion_coeff_alpha)

    NN = 2*this%RWG_basis%num_bases
    NRHS = this%num_q_vectors
    LDA = NN
    LDB = NN
    allocate(IPIV(NN))
    allocate(B_vectors, source=this%q_vectors)
    allocate(A_matrix, source=this%PMCHW_matrix)

    ! Set default numerical method to LU decomposition
    if (.not. present(numerical_method_in)) then
       numerical_method = 1
    else
       numerical_method = numerical_method_in
    end if

    select case (numerical_method)
    case (1)
       ! Solve matrix equation using LU decomposition
       select case (wp)
          case (real32)
             call CGESV(NN, NRHS, this%PMCHW_matrix, LDA, IPIV, &
                  this%q_vectors, LDB, INFO)
          case (real64)
             call ZGESV(NN, NRHS, this%PMCHW_matrix, LDA, IPIV, &
                 this%q_vectors, LDB, INFO)
          case (real128)
             print *, 'Error: PMCHW_RWG_mod: solve_matrix_equation'
             print *, '       LU-decomposition not available for quadruple ', &
                  'precision.'
             stop 2
          case default
             print *, 'Error: PMCHW_RWG_mod: solve_matrix_equation'
             print *, '       LU-decomposition not available for current ', &
                  'data type precision.'
             stop 2
          end select
       !Check for the exact singularity.
       if( INFO.gt.0 ) then
          write(*,*)'The diagonal element of the triangular factor of A,'
          write(*,*)'U(',INFO,',',INFO,') is zero, so that'
          write(*,*)'A is singular; the solution could not be computed.'
          stop
       end if
    case (2)
       ! Solve matrix equation using some iterative mehtod. Not yet implemented
    end select

    do b = 1, this%num_q_vectors
       do m = 1, NN
          if (m <= this%RWG_basis%num_bases) then
             this%expansion_coeff_alpha(m, b) = this%q_vectors(m, b)
          else
             this%expansion_coeff_beta(m - this%RWG_basis%num_bases, b) = &
                  this%q_vectors(m, b)
          end if
       end do
    end do

  end subroutine solve_matrix_equation

  !!----------------------------------------------------------------------------

  function bistatic_scattering_cross_section(&
       this              , &
       scattering_angle  , &
       arc_radius        , &
       phi               , &
       q_vector_idx      , &
       gauss_quad_formula, &
       BBB            ) &
       result(res)
    class (PMCHW_RWG_type)   , intent(in) :: this
    real(wp)                 , intent(in) :: scattering_angle
    real(wp)                 , intent(in) :: arc_radius
    real(wp), dimension(:, :), intent(in) :: gauss_quad_formula
    integer                  , intent(in) :: phi
    integer                  , intent(in) :: q_vector_idx
    logical                  , intent(in) :: BBB
    ! Result variable to be returned -------------------------------------------
    real(wp)                              :: res
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(SPATIAL_DIM)       :: incoming_E_field
    complex(wp), dimension(SPATIAL_DIM)       :: incoming_H_field
    complex(wp), dimension(:, :), allocatable :: scattered_E_field
    complex(wp), dimension(:, :), allocatable :: scattered_H_field
    complex(wp)                               :: radial_comp
    real(wp)   , dimension(SPATIAL_DIM)       :: r
    real(wp)                                  :: scat_E_field_magn_sqrd
    real(wp)                                  :: inc_E_field_magn_sqrd
    real(wp)                                  :: r1
    real(wp)                                  :: r2
    real(wp)                                  :: r3
    integer                                   :: i
    integer                                   :: l

    !---------------------------------------------------------------------------
    ! Assuming scatterer is localised in origio and that the arc radius is large
    ! enough to cover the whole scatterer.
    !
    ! Incoming E-field must (with current implementation) be polarized in
    ! the positive x-direction and propagating in the positive z-direction.
    !---------------------------------------------------------------------------

    if (.not. allocated(this%expansion_coeff_alpha) .or. &
         .not. allocated(this%expansion_coeff_beta)) then
       print *, 'Error: PMCHW_RWG_mod: bistatic_scattering_cross_section:'
       print *, '       Matrix equation must be solved ...'
       stop 2
    end if

    if (allocated(this%inc_E_field_ampl)) then
       if (is_close(this%inc_E_field_ampl(Y_IDX, q_vector_idx)%re, ZERO) &
            .and. is_close(&
            this%inc_E_field_ampl(Z_IDX, q_vector_idx)%re, ZERO)) then
          continue
       else
          print *, 'Error: PMCHW_RWG_mod: bistatic_scattering_cross_section:'
          print *, '       Polarisation of incoming E-field should be along ', &
               'the positive x-axis'
          stop 2
       end if
    else
       print *, 'Error: PMCHW_RWG_mod: bistatic_scattering_cross_section:'
       print *, '       Matrix equation must be solved ...'
       stop 2
    end if
    if (allocated(this%inc_wave_direction)) then
       if (is_close(this%inc_wave_direction(X_IDX, q_vector_idx), ZERO) &
            .and. is_close(&
            this%inc_wave_direction(Y_IDX, q_vector_idx), ZERO)      &
            .and. is_close(&
            this%inc_wave_direction(Z_IDX, q_vector_idx), UNITY)) then
          continue
       else
          print *, 'Error: PMCHW_RWG_mod: bistatic_scattering_cross_section:'
          print *, '       Incoming E-field should propagate in the positive', &
               ' z-axis'
          stop 2
       end if
    else
       print *, 'Error: PMCHW_RWG_mod: bistatic_scattering_cross_section:'
       print *, '       Matrix equation must be solved ...'
       stop 2
    end if
    
    allocate(scattered_E_field(SPATIAL_DIM, this%num_q_vectors))
    allocate(scattered_H_field(SPATIAL_DIM, this%num_q_vectors))

    r1 = arc_radius*sin(scattering_angle)
    r2 = 0._wp
    r3 = arc_radius*cos(scattering_angle)

    select case (phi)
    case (1)
       ! Parallel scattering
       r = [r1, r2, r3]
    case (2)
       ! Orthogonal scattering
       r = [r2, r1, r3]
    end select

    if (BBB) then
       call this%E_and_H_field_at_obs_pnt_BBB(&
            scattered_E_field , &
            scattered_H_field , &
            r                 , &
            gauss_quad_formula, &
            OUTER_REGION_IDX  , &
            scattered=.true.)
    else
       call this%E_and_H_field_at_obs_pnt(&
            scattered_E_field , &
            scattered_H_field , &
            r                 , &
            gauss_quad_formula, &
            OUTER_REGION_IDX  , &
            scattered=.true.)
    end if
 
    call this%inc_E_and_H_field_at_obs_pnt(&
         incoming_E_field, &
         incoming_H_field, &
         q_vector_idx    , &
         r)

    
    scat_E_field_magn_sqrd = ZERO
    inc_E_field_magn_sqrd = ZERO
           
    do l = 1, SPATIAL_DIM
       inc_E_field_magn_sqrd  = inc_E_field_magn_sqrd &
            + incoming_E_field(l)%re**2 &
            + incoming_E_field(l)%im**2 
       scat_E_field_magn_sqrd = scat_E_field_magn_sqrd &
            + scattered_E_field(l, q_vector_idx)%re**2 &
            + scattered_E_field(l, q_vector_idx)%im**2 
    end do
    res = 4._wp*PI*arc_radius**2*scat_E_field_magn_sqrd/inc_E_field_magn_sqrd
  end function bistatic_scattering_cross_section

  !!----------------------------------------------------------------------------

  function face_centroid(&
       this    , &
       face_idx, &
       num_intgr_pnts, &
       intgr_pnts) &
       result(res)
    class (PMCHW_RWG_type)   , intent(in) :: this
    integer                  , intent(in) :: face_idx
    integer                  , intent(in) :: num_intgr_pnts
    real(wp), dimension(:, :), intent(in) :: intgr_pnts
    real(wp), dimension(SPATIAL_DIM)      :: res
    ! Variables for internal use -----------------------------------------------

    if (num_intgr_pnts == 4 .or. &
         num_intgr_pnts == 7 .or. &
         num_intgr_pnts == 13) then
       res = intgr_pnts(1, :)
    else
       res = this%RWG_basis%mesh%face_centroid(face_idx)
    end if
  end function face_centroid
 
  !!----------------------------------------------------------------------------
  
  subroutine write_solutions(this, q_vector_idx, filename)
    class (PMCHW_RWG_type), intent(in) :: this
    integer               , intent(in) :: q_vector_idx
    character(*)          , intent(in) :: filename
    ! Variables for internal use -----------------------------------------------
    integer , parameter            :: num_columns = 4
    real(wp), dimension(this%RWG_basis%num_bases, num_columns) :: table
    integer                        :: n

    if (.not. allocated(this%PMCHW_matrix) .or. &
         .not. allocated(this%q_vectors)) then
       print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
       print *, '       Object must be initialised, PMCHW_matrix and ', &
            ' q_vectors must be set, and matrix equation must be ', &
            ' solved.'
       stop 2
    else if (.not. allocated(this%expansion_coeff_alpha) .or. &
         .not. allocated(this%expansion_coeff_beta)) then
       print *, 'Error: PMCHW_RWG_mod: E_and_H_field_at_obs_pnt:'
       print *, '       Matrix equation for current PMCHW_matrix and ', &
            'q_vectors has not been solved'
       stop 2
    end if
    
    table(:, 1) = this%expansion_coeff_alpha(:, q_vector_idx)%re
    table(:, 2) = this%expansion_coeff_alpha(:, q_vector_idx)%im
    table(:, 3) = this%expansion_coeff_beta(:, q_vector_idx)%re
    table(:, 4) = this%expansion_coeff_beta(:, q_vector_idx)%im

    if (wp == real64) then
       call r8mat_write(filename, this%RWG_basis%num_bases, num_columns, table)
    else
       print *, 'No write routine for current precision.'
    end if
  end subroutine write_solutions
  
  !!==================!!
  ! Public  procedures !
  !====================!========================================================
             
  subroutine eval_outer_integrals(&
       EFIE_integrals    , &
       MFIE_integrals    , &
       outer_intgr_pnts  , &
       inner_intgr_pnts  , &
       gauss_quad_formula, &
       faces_are_close   , &
       p                 , &
       q                 , &
       wavenumber)
    complex(wp), dimension(:)   , intent(inout) :: EFIE_integrals
    complex(wp), dimension(:)   , intent(inout) :: MFIE_integrals
    real(wp)   , dimension(:, :), intent(in)    :: outer_intgr_pnts
    real(wp)   , dimension(:, :), intent(in)    :: inner_intgr_pnts
    real(wp)   , dimension(:, :), intent(in)    :: gauss_quad_formula
    logical                     , intent(in)    :: faces_are_close
    integer                     , intent(in)    :: p
    integer                     , intent(in)    :: q
    complex(wp)                 , intent(in)    :: wavenumber
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(:), allocatable      :: EFIE_inner_intgr_xi
    complex(wp), dimension(:), allocatable      :: EFIE_inner_intgr_eta
    complex(wp), dimension(:), allocatable      :: EFIE_inner_intgr_
    complex(wp), dimension(:), allocatable      :: MFIE_inner_intgr_xi
    complex(wp), dimension(:), allocatable      :: MFIE_inner_intgr_eta
    complex(wp), dimension(:), allocatable      :: MFIE_inner_intgr_
    real(wp)   , dimension(3)                   :: observation_pnt
    integer, parameter                          :: NUM_EFIE_INTEGRALS = 9
    integer, parameter                          :: NUM_MFIE_INTEGRALS = 18
    integer                                     :: num_inner_quad_pnts
    integer                                     :: num_outer_quad_pnts
    real(wp)                                    :: weight_j
    real(wp)                                    :: alpha_j
    real(wp)                                    :: beta_j
    integer                                     :: j
    integer                                     :: o

    if ((size(EFIE_integrals) /= NUM_EFIE_INTEGRALS) .or. &
         (size(MFIE_integrals) /= NUM_MFIE_INTEGRALS)) then
       print *, 'Error: RWG_basis_mod.f90: eval_outer_integrals:'
       print *, '  Size of inout-matrices incorrect ...'
       stop 1
    end if
    EFIE_integrals = ZERO_CMPLX
    MFIE_integrals = ZERO_CMPLX

    num_inner_quad_pnts = size(gauss_quad_formula, dim=1)
    if (faces_are_close) then
       num_outer_quad_pnts = num_inner_quad_pnts
    else
       ! Implement functionality that chooses 3-point quadrature with
       ! degree of precision 2 and does not re-calulate the quadrature
       ! points if gauss_quad_formula is already 3-point with degree 2.
       num_outer_quad_pnts = num_inner_quad_pnts
    end if

    ! As of now, there are no reason to store all inner intgrals per quad
    ! point. I.e. EFIE_inner_intgr_xi etc. could be scalars
    allocate(EFIE_inner_intgr_xi(num_outer_quad_pnts))
    allocate(EFIE_inner_intgr_eta(num_outer_quad_pnts))
    allocate(EFIE_inner_intgr_(num_outer_quad_pnts))
    allocate(MFIE_inner_intgr_xi(num_outer_quad_pnts))
    allocate(MFIE_inner_intgr_eta(num_outer_quad_pnts))
    allocate(MFIE_inner_intgr_(num_outer_quad_pnts))
        
    do j = 1, num_outer_quad_pnts
       EFIE_inner_intgr_xi(j) = ZERO_CMPLX
       EFIE_inner_intgr_eta(j) = ZERO_CMPLX
       EFIE_inner_intgr_(j) = ZERO_CMPLX
       MFIE_inner_intgr_xi(j) = ZERO_CMPLX
       MFIE_inner_intgr_eta(j) = ZERO_CMPLX
       MFIE_inner_intgr_(j) = ZERO_CMPLX
       ! Evaluate inner integrals
       if (faces_are_close) then
          observation_pnt = outer_intgr_pnts(j, :)
       else
          ! Implement functionality that chooses 3-point quadrature with
          ! degree of precision 2 and does not re-calulate the quadrature
          ! points if gauss_quad_formula is already 3-point with degree 2.
          observation_pnt = outer_intgr_pnts(j, :)

       end if
!!$       print *, '  j:', j
       call eval_green_func_integrals(&
            EFIE_inner_intgr_xi(j), &
            EFIE_inner_intgr_eta(j), &
            EFIE_inner_intgr_(j)   , &
            MFIE_inner_intgr_xi(j), &
            MFIE_inner_intgr_eta(j), &
            MFIE_inner_intgr_(j)   , &
            observation_pnt        , &
            inner_intgr_pnts       , &
            gauss_quad_formula     , &
            wavenumber             , &
            faces_are_close        , &
            faces_are_in_same_plane=(p==q))

       Weight_j = gauss_quad_formula(j, GQF_WEIGHT_IDX)
       alpha_j = gauss_quad_formula(j, GQF_XI_IDX)
       beta_j = gauss_quad_formula(j, GQF_ETA_IDX)
       ! Evaluate EFIE face-pair integrals
       EFIE_integrals(1) = EFIE_integrals(1) + weight_j*alpha_j &
            *EFIE_inner_intgr_xi(j)
       EFIE_integrals(2) = EFIE_integrals(2) + weight_j*alpha_j &
            *EFIE_inner_intgr_eta(j)
       EFIE_integrals(3) = EFIE_integrals(3) + weight_j*alpha_j &
            *EFIE_inner_intgr_(j)
       EFIE_integrals(4) = EFIE_integrals(4) + weight_j*beta_j &
            *EFIE_inner_intgr_xi(j)
       EFIE_integrals(5) = EFIE_integrals(5) + weight_j*beta_j &
            *EFIE_inner_intgr_eta(j)
       EFIE_integrals(6) = EFIE_integrals(6) + weight_j*beta_j &
            *EFIE_inner_intgr_(j)
       EFIE_integrals(7) = EFIE_integrals(7) + weight_j       &
            *EFIE_inner_intgr_xi(j)
       EFIE_integrals(8) = EFIE_integrals(8) + weight_j       &
            *EFIE_inner_intgr_eta(j)
       EFIE_integrals(9) = EFIE_integrals(9) + weight_j       &
            *EFIE_inner_intgr_(j)
       ! Evaluate MFIE face-pair integrals
       if (.not. p == q) then
          ! No reason to waste computation time on integrals that will vanish
          ! because the faces are on the same plane. Which they are if p and q
          ! points at the same face.
          MFIE_integrals(1) = MFIE_integrals(1) + weight_j*alpha_j &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(2) = MFIE_integrals(2) + weight_j*alpha_j &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(3) = MFIE_integrals(3) + weight_j*alpha_j &
               *MFIE_inner_intgr_(j)
          MFIE_integrals(4) = MFIE_integrals(4) + weight_j*beta_j &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(5) = MFIE_integrals(5) + weight_j*beta_j &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(6) = MFIE_integrals(6) + weight_j*beta_j &
               *MFIE_inner_intgr_(j)
          MFIE_integrals(7) = MFIE_integrals(7) + weight_j       &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(8) = MFIE_integrals(8) + weight_j       &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(9) = MFIE_integrals(9) + weight_j       &
               *MFIE_inner_intgr_(j)
          MFIE_integrals(10) = MFIE_integrals(10) + weight_j*alpha_j*alpha_j &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(11) = MFIE_integrals(11) + weight_j*alpha_j*alpha_j &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(12) = MFIE_integrals(12) + weight_j*alpha_j*alpha_j &
               *MFIE_inner_intgr_(j)
          MFIE_integrals(13) = MFIE_integrals(13) + weight_j*beta_j*beta_j &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(14) = MFIE_integrals(14) + weight_j*beta_j*beta_j &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(15) = MFIE_integrals(15) + weight_j*beta_j*beta_j &
               *MFIE_inner_intgr_(j)
          MFIE_integrals(16) = MFIE_integrals(16) + weight_j*alpha_j*beta_j &
               *MFIE_inner_intgr_xi(j)
          MFIE_integrals(17) = MFIE_integrals(17) + weight_j*alpha_j*beta_j &
               *MFIE_inner_intgr_eta(j)
          MFIE_integrals(18) = MFIE_integrals(18) + weight_j*alpha_j*beta_j &
               *MFIE_inner_intgr_(j)
       end if
    end do ! j
  end subroutine eval_outer_integrals
  
  !!----------------------------------------------------------------------------
  
  subroutine eval_green_func_integrals(&
       green_func_intgr_xi        , &
       green_func_intgr_eta        , &
       green_func_intgr_           , &
       grad_of_green_func_intgr_xi, &
       grad_of_green_func_intgr_eta, &
       grad_of_green_func_intgr_   , &
       observation_pnt             , &
       quad_pnts                   , &
       gauss_quad_formula          , &
       wavenumber                  , &
       obs_pnt_and_src_are_close   , &
       faces_are_in_same_plane)
    complex(wp)              , intent(inout) :: green_func_intgr_xi
    complex(wp)              , intent(inout) :: green_func_intgr_eta
    complex(wp)              , intent(inout) :: green_func_intgr_
    complex(wp)              , intent(inout) :: grad_of_green_func_intgr_xi
    complex(wp)              , intent(inout) :: grad_of_green_func_intgr_eta
    complex(wp)              , intent(inout) :: grad_of_green_func_intgr_
    real(wp), dimension(3)   , intent(in)    :: observation_pnt
    real(wp), dimension(:, :), intent(in)    :: quad_pnts
    real(wp), dimension(:, :), intent(in)    :: gauss_quad_formula
    complex(wp)              , intent(in)    :: wavenumber
    logical                  , intent(in)    :: obs_pnt_and_src_are_close
    logical                  , intent(in)    :: faces_are_in_same_plane
    ! Variables for internal use -----------------------------------------------
    integer,  parameter                      :: GQF_WEIGHT_IDX = 1
    integer,  parameter                      :: GQF_XI_IDX = 2
    integer,  parameter                      :: GQF_ETA_IDX = 3
    real(wp)                                 :: weight_k
    real(wp)                                 :: xi
    real(wp)                                 :: eta
    real(wp)                                 :: R
    real(wp)                                 :: R_inv
    complex(wp)                              :: green_func
    complex(wp)                              :: grad_of_green_func
    complex(wp)                              :: kR
    integer                                  :: num_quad_pnts
    integer                                  :: k

    ! Add error handling if quad_pnts and gauss_quad_formula is now same size
    num_quad_pnts = size(quad_pnts, dim=1)

    green_func_intgr_xi = ZERO_CMPLX
    green_func_intgr_eta = ZERO_CMPLX
    green_func_intgr_ = ZERO_CMPLX
    grad_of_green_func_intgr_xi = ZERO_CMPLX
    grad_of_green_func_intgr_eta = ZERO_CMPLX
    grad_of_green_func_intgr_ = ZERO_CMPLX
    do k = 1, num_quad_pnts
       R = norm2(observation_pnt &
            - quad_pnts(k, :))
       kR = wavenumber*R
       if (obs_pnt_and_src_are_close) then
          if (CAUCHY) then
             ! Cauchy Principal value for singularity in green's function
             call Cauchy_principal_value(&
                  green_func        , &
                  grad_of_green_func, &
                  wavenumber        , &
                  kR                , &
                  R)
             if (isnan(green_func%re)) then
                print *, 'Cauchy - green_func is nan'
             end if
             if (isnan(grad_of_green_func%re)) then
                print *, 'Cauchy - grad_of_green_func is nan'
             end if
          else
             ! Singularity abstraction
             call green_func_smoothened(&
                  green_func        , &
                  grad_of_green_func, &
                  wavenumber        , &
                  kR                , &
                  R                 , &
                  faces_are_in_same_plane)
          end if
       else
          R_inv = 1._wp/R
          green_func = calc_green_func(wavenumber, kR, R_inv)
          if (.not. faces_are_in_same_plane) then
             ! No reason to waste computation time on integrals that will vanish
             ! because the faces are on the same plane. Which they are if p and q
             ! points are the same face.
             grad_of_green_func = calc_grad_of_green_func(&
                  wavenumber, &
                  kR        , &
                  R_inv     , &
                  green_func=green_func)
          end if
       end if

       weight_k = gauss_quad_formula(k, GQF_WEIGHT_IDX)
       xi = gauss_quad_formula(k, GQF_XI_IDX)
       eta = gauss_quad_formula(k, GQF_ETA_IDX)
       ! EFIE
       green_func_intgr_xi = green_func_intgr_xi + weight_k*xi*green_func
       green_func_intgr_eta = green_func_intgr_eta + weight_k*eta*green_func
       green_func_intgr_ = green_func_intgr_ + weight_k*green_func
       ! MFIE
       if (.not. faces_are_in_same_plane) then
          ! No reason to waste computation time on integrals that will vanish
          ! because the faces are on the same plane. Which they are if p and q
          ! are equal
          grad_of_green_func_intgr_xi = grad_of_green_func_intgr_xi &
               + weight_k*xi*grad_of_green_func
          grad_of_green_func_intgr_eta = grad_of_green_func_intgr_eta &
               + weight_k*eta*grad_of_green_func
          grad_of_green_func_intgr_ = grad_of_green_func_intgr_       &
               + weight_k*grad_of_green_func
       end if
    end do ! k
  end subroutine eval_green_func_integrals
  
  !!----------------------------------------------------------------------------
  
  function face_pair_integral_EFIE(&
       r1_obs    , &
       r2_obs    , &
       r3_obs    , &
       r1_src    , &
       r2_src    , &
       r3_src    , &
       p_m       , &
       p_n       , &
       wavenumber, &
       EFIE_integrals) &
       result(return_value)
    real(wp)   , dimension(3), intent(in) :: r1_obs
    real(wp)   , dimension(3), intent(in) :: r2_obs
    real(wp)   , dimension(3), intent(in) :: r3_obs
    real(wp)   , dimension(3), intent(in) :: r1_src
    real(wp)   , dimension(3), intent(in) :: r2_src
    real(wp)   , dimension(3), intent(in) :: r3_src
    real(wp)   , dimension(3), intent(in) :: p_m
    real(wp)   , dimension(3), intent(in) :: p_n
    complex(wp)              , intent(in) :: wavenumber
    complex(wp), dimension(:), intent(in) :: EFIE_integrals
    complex(wp)                           :: return_value
    ! Variables for internal use -----------------------------------------------
    complex(wp)                           :: int_xi_I_zeta
    complex(wp)                           :: int_eta_I_zeta
    complex(wp)                           :: int_I_zeta
    complex(wp)                           :: term1
    complex(wp)                           :: term2

    ! Integral of divergence of the basis-function f(r)
    term1 = -4._wp/wavenumber**2*EFIE_integrals(9)

    ! Integral of basis-function
    int_xi_I_zeta =          &
         + EFIE_integrals(3) &
         - EFIE_integrals(1) &
         - EFIE_integrals(2)
    int_eta_I_zeta =         &
         + EFIE_integrals(6) &
         - EFIE_integrals(4) &
         - EFIE_integrals(5)
    int_I_zeta =             &
         + EFIE_integrals(9) &
         - EFIE_integrals(7) &
         - EFIE_integrals(8)
    
    term2 = &
         + dot_product(r1_obs, r1_src)*EFIE_integrals(1) &
         + dot_product(r1_obs, r2_src)*EFIE_integrals(2) &
         - dot_product(r1_obs, p_n)   *EFIE_integrals(3) &
         + dot_product(r2_obs, r1_src)*EFIE_integrals(4) &
         + dot_product(r2_obs, r2_src)*EFIE_integrals(5) &
         - dot_product(r2_obs, p_n)   *EFIE_integrals(6) &
         - dot_product(p_m   , r1_src)*EFIE_integrals(7) &
         - dot_product(p_m   , r2_src)*EFIE_integrals(8) &
         + dot_product(p_m   , p_n)   *EFIE_integrals(9) &
         + dot_product(r1_obs, r3_src)*int_xi_I_zeta     &
         + dot_product(r2_obs, r3_src)*int_eta_I_zeta    &
         + dot_product(r3_obs, r3_src)*(                 &
              + int_I_zeta                               &
              - int_eta_I_zeta                           &
              - int_xi_I_zeta )                          &
         - dot_product(p_m   , r3_src)*int_I_zeta        &
         + dot_product(r3_obs, r1_src)                   &
              *(EFIE_integrals(7)                        &
              - EFIE_integrals(1)                        &
              - EFIE_integrals(4))                       &
         + dot_product(r3_obs, r2_src)                   &
              *(EFIE_integrals(8)                        &
              - EFIE_integrals(2)                        &
              - EFIE_integrals(5))                       &
         - dot_product(r3_obs, p_n)                      &
              *(EFIE_integrals(9)                        &
              - EFIE_integrals(3)                        &
              - EFIE_integrals(6))
    return_value = term1 + term2
  end function face_pair_integral_EFIE

  !!----------------------------------------------------------------------------

  function face_pair_integral_MFIE(&
       r1_obs, &
       r2_obs, &
       r3_obs, &
       r1_src, &
       r2_src, &
       r3_src, &
       p_m   , &
       p_n   , &
       MFIE_integrals) &
       result(res)
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r1_obs
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r2_obs
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r3_obs
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r1_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r2_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: r3_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: p_m
    real(wp)   , dimension(SPATIAL_DIM), intent(in) :: p_n
    complex(wp), dimension(:)          , intent(in) :: MFIE_integrals
    complex(wp)                                     :: res
    ! Variables for internal use -----------------------------------------------
    complex(wp)                                     :: int_xi_I_zeta
    complex(wp)                                     :: int_eta_I_zeta
    complex(wp)                                     :: int_zeta_I_zeta
    complex(wp)                                     :: int_I_zeta
    complex(wp)                                     :: int_zeta_I_xi
    complex(wp)                                     :: int_zeta_I_eta
    complex(wp)                                     :: int_zeta_I_1
    complex(wp)                                     :: int_xi_zeta_I_xi
    complex(wp)                                     :: int_xi_zeta_I_eta
    complex(wp)                                     :: int_xi_zeta_I_zeta
    complex(wp)                                     :: int_xi_zeta_I_1
    complex(wp)                                     :: int_eta_zeta_I_xi
    complex(wp)                                     :: int_eta_zeta_I_eta
    complex(wp)                                     :: int_eta_zeta_I_zeta
    complex(wp)                                     :: int_eta_zeta_I_1
    complex(wp)                                     :: int_zeta_zeta_I_xi
    complex(wp)                                     :: int_zeta_zeta_I_eta
    complex(wp)                                     :: int_zeta_zeta_I_1
    complex(wp)                                     :: int_xi_xi_I_zeta
    complex(wp)                                     :: int_eta_eta_I_zeta
    complex(wp)                                     :: int_zeta_zeta_I_zeta
    complex(wp)                                     :: int_xi_eta_I_zeta

    ! Integral of the curl of the gradient of the Green's function
    ! and the basis function f(r)
    ! Firstly follows temporary variables to avoid repeating identical
    ! summations, and to make the code more readable for debugging the
    ! mathematical expressions.
    int_xi_I_zeta =           &
         + MFIE_integrals(3)  &
         - MFIE_integrals(1)  &
         - MFIE_integrals(2)
    int_eta_I_zeta =          &
         + MFIE_integrals(6)  &
         - MFIE_integrals(4)  &
         - MFIE_integrals(5)
    int_I_zeta =              &
         + MFIE_integrals(9)  &
         - MFIE_integrals(7)  &
         - MFIE_integrals(8)
    int_zeta_I_zeta =         &
         + int_I_zeta         &
         - int_xi_I_zeta      &
         - int_eta_I_zeta
    int_zeta_I_xi =           &
         + MFIE_integrals(7)  &
         - MFIE_integrals(1)  &
         - MFIE_integrals(4)
    int_zeta_I_eta =          &
         + MFIE_integrals(8)  &
         - MFIE_integrals(2)  &
         - MFIE_integrals(5)
    int_zeta_I_1 =            &
         + MFIE_integrals(9)  &
         - MFIE_integrals(3)  &
         - MFIE_integrals(6)
    int_xi_zeta_I_xi =        &
         + MFIE_integrals(1)  &
         - MFIE_integrals(10) &
         - MFIE_integrals(16)
    int_eta_zeta_I_xi =       &
         + MFIE_integrals(4)  &
         - MFIE_integrals(16) &
         - MFIE_integrals(13)
    int_zeta_zeta_I_xi =      &
         + int_zeta_I_xi      &
         - int_xi_zeta_I_xi   &
         - int_eta_zeta_I_xi
    int_xi_zeta_I_eta =       &
         + MFIE_integrals(2)  &
         - MFIE_integrals(11) &
         - MFIE_integrals(17)
    int_eta_zeta_I_eta =      &
         + MFIE_integrals(5)  &
         - MFIE_integrals(14) &
         - MFIE_integrals(17)
    int_zeta_zeta_I_eta =    &
         + int_zeta_I_eta    &
         - int_xi_zeta_I_eta &
         - int_eta_zeta_I_eta
    int_xi_xi_I_zeta =        &
         + MFIE_integrals(12) &
         - MFIE_integrals(10) &
         - MFIE_integrals(11)
    int_xi_eta_I_zeta =       &
         + MFIE_integrals(18) &
         - MFIE_integrals(16) &
         - MFIE_integrals(17)
    int_eta_eta_I_zeta =      &
         + MFIE_integrals(15) &
         - MFIE_integrals(14) &
         - MFIE_integrals(13)
    int_xi_zeta_I_zeta =    &
         + int_xi_I_zeta    &
         - int_xi_xi_I_zeta &
         - int_xi_eta_I_zeta
    int_eta_zeta_I_zeta =    &
         + int_eta_I_zeta    &
         - int_xi_eta_I_zeta &
         - int_eta_eta_I_zeta
    int_zeta_zeta_I_1 =     &
         + int_zeta_I_1     &
         - int_xi_zeta_I_1  &
         - int_eta_zeta_I_1 
    int_xi_zeta_I_1 =         &
         + MFIE_integrals(3)  &
         - MFIE_integrals(12) &
         - MFIE_integrals(18)
    int_eta_zeta_I_1 =        &
         + MFIE_integrals(6)  &
         - MFIE_integrals(15) &
         - MFIE_integrals(18)
    int_zeta_zeta_I_zeta =    &
         + int_zeta_zeta_I_1  &
         - int_zeta_zeta_I_xi &
         - int_zeta_zeta_I_eta

    if (.false.) then
       write (*,*)  int_xi_eta_I_zeta
    end if
    res = ZERO_CMPLX + &
         ! Cross product including p_n at RHS
         dot_product(cross_prod_3D(r1_src, p_n), (      &
              MFIE_integrals(1)*r1_obs                  &
            + MFIE_integrals(4)*r2_obs                  &
            + int_zeta_I_xi*r3_obs                      &
            - MFIE_integrals(7)*p_m ))                  &
         + dot_product(cross_prod_3D(r2_src, p_n), (    &
              MFIE_integrals(2)*r1_obs                  &
            + MFIE_integrals(5)*r2_obs                  &
            + int_zeta_I_eta*r3_obs                     &
            - MFIE_integrals(8)*p_m ))                  &
         + dot_product(cross_prod_3D(r3_src, p_n), (    &
              int_xi_I_zeta*r1_obs                      &
            + int_eta_I_zeta*r2_obs                     &
            + int_zeta_I_zeta*r3_obs                    &
            - int_I_zeta*p_m ))                         &
         ! Cross product including r1_src at RHS        
         + dot_product(cross_prod_3D(r1_obs, r1_src), ( &
              MFIE_integrals(10)*r1_obs                 &
            + MFIE_integrals(16)*r2_obs                 &
            + int_xi_zeta_I_xi*r3_obs                   &
            - MFIE_integrals(1)*p_m ))                  &
         + dot_product(cross_prod_3D(r2_obs, r1_src), ( &
              MFIE_integrals(16)*r1_obs                 &
            + MFIE_integrals(13)*r2_obs                 &
            + int_eta_zeta_I_xi*r3_obs                  &
            - MFIE_integrals(4)*p_m ))                  &
         + dot_product(cross_prod_3D(r3_obs, r1_src), ( &
              int_xi_zeta_I_xi*r1_obs                   &
            + int_eta_zeta_I_xi*r2_obs                  &
            - int_zeta_I_xi*p_m                         &
            + int_zeta_zeta_I_xi*r3_obs ))              &
         ! Cross product including r2_src at RHS       
         + dot_product(cross_prod_3D(r1_obs, r2_src), ( &
              MFIE_integrals(11)*r1_obs                 &
            + MFIE_integrals(17)*r2_obs                 &
            + int_xi_zeta_I_eta*r3_obs                  &
            - MFIE_integrals(2)*p_m ))                  &
         + dot_product(cross_prod_3D(r2_obs, r2_src), ( &
              MFIE_integrals(17)*r1_obs                 &
            + MFIE_integrals(14)*r2_obs                 &
            + int_eta_zeta_I_eta*r3_obs                 &
            - MFIE_integrals(5)*p_m ))                  &
         + dot_product(cross_prod_3D(r3_obs, r2_src), ( &
              int_xi_zeta_I_eta*r1_obs                  &
            + int_eta_zeta_I_eta*r2_obs                 &
            - int_zeta_I_eta*p_m                        &
            + int_zeta_zeta_I_eta*r3_obs ))             &                        
         ! Cross product including r3_src at RHS        
         + dot_product(cross_prod_3D(r1_obs, r3_src), ( &
              int_xi_xi_I_zeta*r1_obs                   &
            + int_xi_eta_I_zeta*r2_obs                  &
            + int_xi_zeta_I_zeta*r3_obs                 &
            - int_xi_I_zeta*p_m ))                      &
         + dot_product(cross_prod_3D(r2_obs, r3_src), ( &
              int_xi_eta_I_zeta*r1_obs                  &
            + int_eta_eta_I_zeta*r2_obs                 &
            + int_eta_zeta_I_zeta*r3_obs                &
            - int_eta_I_zeta*p_m ))                     &
         + dot_product(cross_prod_3D(r3_obs, r3_src), ( &
              int_xi_zeta_I_zeta*r1_obs                 &
            + int_eta_zeta_I_zeta*r2_obs                &
            + int_zeta_zeta_I_zeta*r3_obs               &
            - int_zeta_I_zeta*p_m ))                    &    
         ! Cross product including p_n at RHS        
         - dot_product(cross_prod_3D(r1_obs, p_n), (    &
              MFIE_integrals(12)*r1_obs                 &
            + MFIE_integrals(18)*r2_obs                 &
            + int_xi_zeta_I_1*r3_obs                    &
            - MFIE_integrals(3)*p_m ))                  &
         - dot_product(cross_prod_3D(r2_obs, p_n), (    &
              MFIE_integrals(18)*r1_obs                 &
            + MFIE_integrals(15)*r2_obs                 &
            + int_eta_zeta_I_1*r3_obs                   &
            - MFIE_integrals(6)*p_m ))                  &
         - dot_product(cross_prod_3D(r3_obs, p_n), (    &
              int_xi_zeta_I_1*r1_obs                    &
            + int_eta_zeta_I_1*r2_obs                   &
            + int_zeta_zeta_I_1*r3_obs                  &
            - int_zeta_I_1*p_m ))           

  end function face_pair_integral_MFIE
  
  !!----------------------------------------------------------------------------

  function are_obs_pnt_and_src_close(&
       this      , &
       obs_pnt   , &
       src       , &
       wavenumber, &
       p         , &
       q)          &
       result(res)
    class(PMCHW_RWG_type)           , intent(in) :: this
    real(wp), dimension(SPATIAL_DIM), intent(in) :: obs_pnt
    real(wp), dimension(SPATIAL_DIM), intent(in) :: src
    complex(wp)                     , intent(in) :: wavenumber
    integer , optional              , intent(in) :: p
    integer , optional              , intent(in) :: q
    logical                                      :: res
    ! Variables for internal use -----------------------------------------------
    real(wp)                                     :: seperation_dist
    integer, dimension(NUM_FACE_VERTICES)        :: vertices_of_p
    integer, dimension(NUM_FACE_VERTICES)        :: vertices_of_q
    seperation_dist = norm2(obs_pnt - src)
    if (present(p) .and. present(q)) then
       vertices_of_p = this%RWG_basis%mesh%get_vertices_of_face(p)
       vertices_of_q = this%RWG_basis%mesh%get_vertices_of_face(q)
       if (p == q) then
          res = .true.
       else if ( any(vertices_of_p == vertices_of_q(1)) .or. &
            any(vertices_of_p == vertices_of_q(2)) .or. &
            any(vertices_of_p == vertices_of_q(3)) ) then
          res = .true.
       else if (seperation_dist < &
            PROP_CONST_OBS_PNT_SRC_CLOSE*2*PI/wavenumber%re) then
          res = .true.
       else
          res = .false.
       end if
    else if (seperation_dist < &
         PROP_CONST_OBS_PNT_SRC_CLOSE*2*PI/wavenumber%re) then
       res = .true.
    else
       res = .false.
    end if
  end function are_obs_pnt_and_src_close
    
  !!----------------------------------------------------------------------------

  function dbl_singularity_intgr(&
       p                  , &
       q                  , &
       r1_obs             , &
       r2_obs             , &
       r3_obs             , &
       r1_src             , &
       r2_src             , &
       r3_src             , &
       p_m                , &
       p_n                , &
       face_area_p        , &
       face_area_q        , &
       face_unit_normal_p , &
       face_unit_normal_q , &
       edge_lengths_p     , &
       edge_lengths_q     , &
       edge_unit_normals_p, &
       edge_unit_normals_q, &
       free_vertice_diff)        &
       result(res)
    integer                               , intent(in) :: p
    integer                               , intent(in) :: q
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r1_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r2_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r3_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r1_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r2_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: r3_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: p_m
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: p_n
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) &
         , intent(in) :: edge_unit_normals_p
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) &
         , intent(in) :: edge_unit_normals_q
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: edge_lengths_p
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: edge_lengths_q
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: face_unit_normal_p
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: face_unit_normal_q
    real(wp), dimension(SPATIAL_DIM)      , intent(in) :: free_vertice_diff
    real(wp)                              , intent(in) :: face_area_p
    real(wp)                              , intent(in) :: face_area_q
    complex(wp)                                        :: res
    ! Variables for internal use -----------------------------------------------
    real(wp)                                  :: line_integral
    real(wp)                                  :: X1_minus_1
    real(wp)                                  :: X1_plus_1
    real(wp), dimension(:, :), allocatable  :: gauss_quad_formula
    real(wp)   , dimension(SPATIAL_DIM)       :: hnX1_minus_3
    real(wp)   , dimension(SPATIAL_DIM)       :: X2_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X2_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X3_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X3_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X4_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X4_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: edge1
    real(wp)   , dimension(SPATIAL_DIM)       :: edge2
    real(wp)   , dimension(SPATIAL_DIM)       :: edge3
    real(wp)   , dimension(SPATIAL_DIM)       :: temp1
    real(wp)   , dimension(SPATIAL_DIM)       :: source_pnt
    real(wp)                                  :: weight
    real(wp)                                  :: abscissa
    integer    , dimension(NUM_FACE_VERTICES) :: face_edges
    integer                                   :: num_quad_pnts
    integer                                   :: l
    integer                                   :: j
    logical    , parameter                    :: X2_minus_only = .true.

    ! Using Gauss-Legendre 5-point formula for line integrals over triangle
    ! edges.
    allocate(gauss_quad_formula, source=GQF_Legendre_5pnt)
    num_quad_pnts = size(GQF_Legendre_5pnt, dim=1)
    edge1 = r3_src - r2_src
    edge2 = r1_src - r3_src
    edge3 = r2_src - r1_src
    res = ZERO_CMPLX
    do l = 1, NUM_FACE_VERTICES
       line_integral = ZERO
       ! This quantity is only dependent on the edge, not the explicit value
       ! of the source point r'
       temp1 = cross_prod_3D(free_vertice_diff, edge_unit_normals_q(l, :))
       do j = 1, num_quad_pnts
          weight = gauss_quad_formula(j, GQF_WEIGHT_IDX)
          source_pnt = map_GLQF_pnt_to_triangle_edge(&
               gauss_quad_formula, &
               edge1            , &
               edge2            , &
               edge3            , &
               r1_src           , &
               r2_src           , &
               r3_src           , &
               j                , &
               l)
          ! Evaluate inner integral
          call inner_intgr_of_subtr_terms(&
               hnX1_minus_3      , &
               X1_minus_1        , &
               X1_plus_1         , &
               X2_minus_1        , &
               X2_plus_1         , &
               X3_minus_1        , &
               X3_plus_1         , &
               X4_minus_1        , &
               X4_plus_1         , &
               X2_minus_only     , &
               source_pnt        , &
               r1_obs            , &
               r2_obs            , &
               r3_obs            , &
               p_m               , &
               face_area_p       , &
               face_unit_normal_p, &
               edge_lengths_p    , &
               edge_unit_normals_p)
          ! Calculate the integrand of the outer test integral and use it in
          ! the quadrature sum
          line_integral = line_integral + weight*dot_product(temp1, X2_minus_1)
       end do ! j
       select case (l)
       case (1)
          res = res + 0.5_wp*norm2(edge1)*line_integral
       case (2)
          res = res + 0.5_wp*norm2(edge2)*line_integral
       case (3)
          res = res + 0.5_wp*norm2(edge3)*line_integral
       end select
    end do ! l
    res = res*0.5_wp*PI4_INV/(face_area_q*face_area_p)

  end function dbl_singularity_intgr
 
  !!----------------------------------------------------------------------------

  subroutine eval_subtracted_terms(&
       subtr_terms_D     , &
       subtr_terms_K     , &
       wavenumber        , &
       r1_obs            , &
       r2_obs            , &
       r3_obs            , &
       r1_src            , &
       r2_src            , &
       r3_src            , &
       p_m               , &
       p_n               , &
       gauss_quad_formula, &
       p                 , &
       q                 , &
       face_area_p        , &
       face_area_q        , &
       face_unit_normal_p , &
       face_unit_normal_q , &
       edge_lengths_p     , &
       edge_lengths_q     , &
       edge_unit_normals_p, &
       edge_unit_normals_q)
    complex(wp)                           , intent(inout) :: subtr_terms_D
    complex(wp)                           , intent(inout) :: subtr_terms_K
    complex(wp)                           , intent(in)    :: wavenumber
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r1_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r2_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r3_obs
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r1_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r2_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: r3_src
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: p_m
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: p_n
    real(wp), dimension(:, :)             , intent(in)    :: gauss_quad_formula
    integer                               , intent(in)    :: p
    integer                               , intent(in)    :: q
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) &
         , intent(in) :: edge_unit_normals_p
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) &
         , intent(in) :: edge_unit_normals_q
    real(wp), dimension(NUM_FACE_VERTICES), intent(in)    :: edge_lengths_p
    real(wp), dimension(NUM_FACE_VERTICES), intent(in)    :: edge_lengths_q
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: face_unit_normal_p
    real(wp), dimension(SPATIAL_DIM)      , intent(in)    :: face_unit_normal_q
    real(wp)                              , intent(in)    :: face_area_p
    real(wp)                              , intent(in)    :: face_area_q
    ! Variables for internal use -----------------------------------------------
    complex(wp), dimension(SPATIAL_DIM)       :: X2
    complex(wp), dimension(SPATIAL_DIM)       :: X4
    complex(wp)                               :: X1
    complex(wp)                               :: Q_hnX1_minus_3
    complex(wp)                               :: Q_X1
    complex(wp)                               :: Q_X2
    complex(wp)                               :: Q_X4
    complex(wp)                               :: k2
    complex(wp)                               :: half_of_k2
    real(wp)   , dimension(SPATIAL_DIM)       :: hnX1_minus_3
    real(wp)   , dimension(SPATIAL_DIM)       :: X2_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X2_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X3_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X3_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X4_minus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: X4_plus_1
    real(wp)   , dimension(SPATIAL_DIM)       :: observation_pnt
    real(wp)   , dimension(SPATIAL_DIM)       :: free_vertice_diff
    real(wp)   , dimension(SPATIAL_DIM)       :: temp_vec
    real(wp)                                  :: X1_minus_1
    real(wp)                                  :: X1_plus_1
    real(wp)                                  :: weight
    real(wp)                                  :: alpha
    real(wp)                                  :: beta
    real(wp)                                  :: gamma
    integer    , dimension(NUM_FACE_VERTICES) :: face_edges
    integer                                   :: num_intgr_pnts
    integer                                   :: k

    k2 = wavenumber**2
    half_of_k2 = k2*0.5_wp
    free_vertice_diff = p_m - p_n

    Q_hnX1_minus_3 = ZERO_CMPLX
    Q_X1 = ZERO_CMPLX
    Q_X2 = ZERO_CMPLX
    Q_X4 = ZERO_CMPLX

    num_intgr_pnts = size(gauss_quad_formula, dim=1)

    do k = 1, num_intgr_pnts
       weight = gauss_quad_formula(k, GQF_WEIGHT_IDX)
       alpha = gauss_quad_formula(k, GQF_XI_IDX)
       beta = gauss_quad_formula(k, GQF_ETA_IDX)
       gamma = gauss_quad_formula(k, GQF_ZETA_IDX)
       observation_pnt = alpha*r1_obs + beta*r2_obs + gamma*r3_obs
       !observation_pnt = outer_intgr_pnts(k, :)

       !
       ! X1, X2, and X4 symbolises the three variations of inner integrals
       ! necessary to evaluate emerging from having subtracted terms
       ! in the inner integrands
       !
       !   X1_minus_1: integral of 1/R times the divergence of f(r')
       !   X1_plus_1: integral of R times the divergence of f(r')
       !   X2_minus_1: integral of 1/R times f(r')
       !   X2_plus_1: integral of R times f(r')
       !   X4_minus_1: integral of grad 1/R crossed with f(r')
       !   X4_plus_1: integral of grad R crossed with f(r')
       !
       ! Subtracted terms:
       !  1/(4*PI)(1/R - k*R/2)
 
       call inner_intgr_of_subtr_terms(&
            hnX1_minus_3      , &
            X1_minus_1        , &
            X1_plus_1         , &
            X2_minus_1        , &
            X2_plus_1         , &
            X3_minus_1        , &
            X3_plus_1         , &
            X4_minus_1        , &
            X4_plus_1         , &
            .false.           , &
            observation_pnt   , &
            r1_src            , &
            r2_src            , &
            r3_src            , &
            p_n               , &
            face_area_q       , &
            face_unit_normal_q, &
            edge_lengths_q    , &
            edge_unit_normals_q)
       
       if (isnan(X1_minus_1)) then
          print *, 'X1_minus_1 is NaN'
       end if
       if (isnan(X1_plus_1)) then
          print *, 'X1_plus_1 is NaN'
       end if
       if (any(isnan(X2_minus_1))) then
          print *, 'X2_minus_1 is NaN'
       end if
       if (any(isnan(X2_plus_1))) then
          print *, 'X2_plus_1 is NaN'
       end if
       if (any(isnan(X4_minus_1))) then
          print *, 'X4_minus_1 is NaN'
       end if
       if (any(isnan(X4_plus_1))) then
          print *, 'X4_plus_1 is NaN'
       end if

       X1 = X1_minus_1 - half_of_k2*X1_plus_1
       X2 = X2_minus_1 - half_of_k2*X2_plus_1
       X4 = -half_of_k2*X4_plus_1  !+ X4_minus_1 

       temp_vec = observation_pnt - p_m
       Q_hnX1_minus_3 = Q_hnX1_minus_3 + weight*dot_product(temp_vec, &
            cross_prod_3D(free_vertice_diff, hnX1_minus_3))
       Q_X1 = Q_X1 + weight*X1
       Q_X2 = Q_X2 + weight*dot_product(temp_vec, X2) ! Be aware of 
       Q_X4 = Q_X4 + weight*dot_product(temp_vec, X4) ! cmplx conjugate
    end do ! k

    subtr_terms_D = PI4_inv/face_area_q*(-4._wp/k2*Q_X1 + Q_X2)
    subtr_terms_K = PI4_inv/face_area_q*( Q_X4   &
         - Q_hnX1_minus_3 )                    &
         - dbl_singularity_intgr(&
         p                  , &
         q                  , &
         r1_obs             , &
         r2_obs             , &
         r3_obs             , &
         r1_src             , &
         r2_src             , &
         r3_src             , &
         p_m                , &
         p_n                , &
         face_area_p        , &
         face_area_q        , &
         face_unit_normal_p , &
         face_unit_normal_q , &
         edge_lengths_p     , &
         edge_lengths_q     , &
         edge_unit_normals_p, &
         edge_unit_normals_q, &
         free_vertice_diff)

    
  end subroutine eval_subtracted_terms
  
  !!----------------------------------------------------------------------------
  
  subroutine green_func_smoothened(&
       G_subtracted        , &
       grad_of_G_subtracted, &
       wavenumber          , &
       kR                  , &
       R                   , &
       faces_are_on_same_plane)
    complex(wp), intent(inout) :: G_subtracted
    complex(wp), intent(inout) :: grad_of_G_subtracted
    complex(wp), intent(in)    :: wavenumber
    complex(wp), intent(in)    :: kR
    real(wp)   , intent(in)    :: R
    logical    , intent(in)    :: faces_are_on_same_plane
    ! Variables for internal use -----------------------------------------------
    complex(wp) :: exponential
    complex(wp) :: G
    complex(wp) :: k2
    complex(wp) :: k2R
    complex(wp) :: half_of_k2
    real(wp)    :: R_inv

    k2 = wavenumber**2
    
    if (is_close(R, ZERO)) then
       G_subtracted = I_IMAG*wavenumber*PI4_INV
       grad_of_G_subtracted = I_IMAG*k2*wavenumber*PI4_INV/3._wp
    else
       ! Helping variables defined to avoid calculating the same quantity twice
       R_inv = 1._wp/R
       k2R = k2*R
       half_of_k2 = k2/2._wp
       if (is_close(wavenumber%im, 0._wp)) then
          exponential = cmplx(cos(kR%re), sin(kR%re))
       else
          exponential = exp(I_IMAG*kR)
       end if
       G = exponential*R_inv
       
       G_subtracted = ( G - (R_inv - R*half_of_k2) )*PI4_INV
       
       if (.not. faces_are_on_same_plane) then
          ! No reason to waste computation time on integrals that will vanish
          ! because the faces are on the same plane. Which they are if p and q
          ! points at the same face.
          grad_of_G_subtracted = ( G*(R_inv - I_IMAG*wavenumber) &
               - R_inv**2 - half_of_k2 )*PI4_INV*R_inv
       end if
    end if

  end subroutine green_func_smoothened
       
  !!----------------------------------------------------------------------------

  subroutine inner_intgr_of_subtr_terms(&
       hnX1_minus_3    , &
       X1_minus_1      , &
       X1_plus_1       , &
       X2_minus_1      , &
       X2_plus_1       , &
       X3_minus_1      , &
       X3_plus_1       , &
       X4_minus_1      , &
       X4_plus_1       , &
       X2_minus_only   , &
       observation_pnt , &
       r1_src          , &
       r2_src          , &
       r3_src          , &
       p_vec           , &
       face_area       , &
       face_unit_normal, &
       edge_lengths    , &
       edge_unit_normals)
     
    ! X1, X2, and X3 symbolises the three variations of inner integrals
    ! necessary to evaluate emerging from having subtracted terms
    ! in the inner integrands
    !
    !   X1_minus_1: integral of 1/R times the divergence of f(r')
    !   X1_plus_1: integral of R times the divergence of f(r')
    !   X2_minus_1: integral of 1/R times f(r')
    !   X2_plus_1: integral of R times f(r')
    !   X3_minus_1: integral of grad 1/R 
    !   X3_plus_1: integral of grad R 
    !   X4_minus_1: integral of grad 1/R crossed with f(r')
    !   X4_plus_1: integral of grad R crossed with f(r')
    !
    ! Subtracted terms:
    !  1/(4*PI)(1/R - k*R/2)
    real(wp)                           , intent(inout) :: X1_minus_1
    real(wp)                           , intent(inout) :: X1_plus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: hnX1_minus_3
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X2_minus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X2_plus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X3_minus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X3_plus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X4_minus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(inout) :: X4_plus_1
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: observation_pnt
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: r1_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: r2_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: r3_src
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: p_vec
    real(wp)                           , intent(in)    :: face_area
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: face_unit_normal
    real(wp)   , dimension(SPATIAL_DIM), intent(in)    :: edge_lengths
    real(wp)   , dimension(:, :)       , intent(in)    :: edge_unit_normals
    logical                            , intent(in)    :: X2_minus_only
    ! Variables for internal use -----------------------------------------------
    real(wp), dimension(SPATIAL_DIM) :: temp1
    real(wp), dimension(SPATIAL_DIM) :: temp2
    real(wp), dimension(SPATIAL_DIM) :: temp3
    integer                          :: l
    ! Variables for representing triangle transformed to an orthogonal basis ---
    real(wp), dimension(NUM_FACE_VERTICES) :: R_plus
    real(wp), dimension(NUM_FACE_VERTICES) :: R_minus
    real(wp), dimension(NUM_FACE_VERTICES) :: R_0
    real(wp), dimension(NUM_FACE_VERTICES) :: s_plus
    real(wp), dimension(NUM_FACE_VERTICES) :: s_minus
    real(wp), dimension(NUM_FACE_VERTICES) :: t
    real(wp), dimension(SPATIAL_DIM)       :: rho
    real(wp), dimension(SPATIAL_DIM)       :: u
    real(wp), dimension(SPATIAL_DIM)       :: v
    real(wp)                               :: h
    real(wp)                               :: u_3
    real(wp)                               :: v_3
    real(wp)                               :: u_0
    real(wp)                               :: v_0
    ! Variables for storing part of solutions (iterative values) ---------------
    real(wp), dimension(NUM_FACE_VERTICES) :: I_l_minus_1
    real(wp), dimension(NUM_FACE_VERTICES) :: I_l_plus_1
    real(wp), dimension(NUM_FACE_VERTICES) :: I_l_plus_3
    real(wp)                               :: I_S_minus_3
    real(wp)                               :: I_S_minus_1
    real(wp)                               :: I_S_plus_1
    real(wp), dimension(SPATIAL_DIM)       :: hn
    real(wp), dimension(SPATIAL_DIM)       :: edge_unit_normal_sum

    
    temp1 = observation_pnt - r1_src
    temp3 = observation_pnt - p_vec
    
    ! Notations for analytical formulas of integrals on triangles
    u = (r2_src - r1_src)/edge_lengths(3)
    v = -edge_unit_normals(3, :)
    ! Error check
    if (.not. is_close(norm2(u), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The vector "u" should be a unit vector.'
       stop 2
    else if (.not. is_close(norm2(u), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The vector "u" should be a unit vector.'
       stop 2
    else if (.not. is_close(norm2(face_unit_normal), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The face_unit_normal should be a unit vector.'
       stop 2
    else if (.not. is_close(norm2(edge_unit_normals(1, :)), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The edge_unit_normals(1, :) should be a unit vector.'
       stop 2
    else if (.not. is_close(norm2(edge_unit_normals(2, :)), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The edge_unit_normals(2, :) should be a unit vector.'
       stop 2
    else if (.not. is_close(norm2(edge_unit_normals(3, :)), 1._wp)) then
       print *, 'Error: PMCHW_RWG_mod: inner_intgr_of_subtr_terms:'
       print *, '       The edge_unit_normals(3, :) should be a unit vector.'
       stop 2
    end if
    R_plus(1) = norm2(observation_pnt - r3_src)
    R_plus(2) = norm2(temp1)
    R_plus(3) = norm2(observation_pnt - r2_src)
    R_minus(2) = R_plus(1)
    R_minus(3) = R_plus(2)
    R_minus(1) = R_plus(3)
    h = dot_product(face_unit_normal, temp1)
    hn = h*face_unit_normal
    rho = observation_pnt - h*face_unit_normal
    u_3 = dot_product((r3_src - r1_src), u)
    v_3 = 2*face_area/edge_lengths(3)
    u_0 = dot_product(temp1, u)
    v_0 = dot_product(temp1, v)
    s_minus(1) = -( (edge_lengths(3) - u_0)*(edge_lengths(3) - u_3) + v_0*v_3 )&
         /edge_lengths(1)
    s_minus(2) = -( u_3*(u_3 - u_0) + v_3*(v_3 - v_0) )/edge_lengths(2)
    s_minus(3) = -u_0
    s_plus(1) = s_minus(1) + edge_lengths(1)
    s_plus(2) = s_minus(2) + edge_lengths(2)
    s_plus(3) = s_minus(3) + edge_lengths(3)
    t(1) = ( v_0*(u_3 - edge_lengths(3)) + v_3*(edge_lengths(3) - u_0) ) &
         /edge_lengths(1)
    t(2) = ( u_0*v_3 - v_0*u_3 )/edge_lengths(2)
    t(3) = v_0
    do l = 1, NUM_FACE_VERTICES
       R_0(l) = sqrt(t(l)**2 + h**2)
    end do
    
    ! Additional helping quantities
    temp2 = rho - p_vec

    ! Calculate I^l_-1, I^l_1, and I^l_3 prior to assignment of X1, X2, and X3
    ! If R_0 = 0, do not calculate I_minus_1, but set it equal to zero
    do l = 1, NUM_FACE_VERTICES
       if (is_close(R_0(l), 0._wp)) then
          I_l_minus_1(l) = 0._wp
          I_l_plus_1(l) = 0.5_wp*(s_plus(l)*R_plus(l) - s_minus(l)*R_minus(l))
       else
          I_l_minus_1(l) = line_intgr_solution(&
               R_plus(l) , &
               R_minus(l), &
               s_plus(l) , &
               s_minus(l))
          I_l_plus_1(l) = 0.5_wp*( &
               R_0(l)**2*I_l_minus_1(l) + s_plus(l)*R_plus(l) - &
               s_minus(l)*R_minus(l) )
    ! Variables for internal use -----------------------------------------------
       end if
       if (isnan(I_l_minus_1(l))) then
          print *, 'I_l_minus_1(l) is NaN: l, R_0', l, R_0(l)
       end if
       if (isnan(I_l_plus_1(l))) then
          print *, 'I_l_plus_1(l) is NaN', l
       end if
       if (.not. X2_minus_only) then
          I_l_plus_3(l) = 0.25_wp*( 3._wp*R_0(l)**2*I_l_plus_1(l) &
               + s_plus(l)*R_plus(l)**3 - s_minus(l)*R_minus(l)**3 )
       end if
       if (isnan(I_l_plus_3(l))) then
          print *, 'I_l_plus_3(l) is NaN', l
       end if
    end do
    ! Calculate I^S_-3, I^S_-1, and I^S_1 prior to assignment of X1, X2 and X3
    ! If h = 0 (i.e. is on the plane formed by the face) do not calculate
    ! I_(q-2), but set it equal to zero.
    if (is_close(h, 0._wp)) then
       I_S_minus_3 = 0._wp
       I_S_minus_1 = 0._wp
    else
       I_S_minus_3 = surface_intgr_solution(&
            h      , &
            t      , &
            R_0    , &
            R_plus , &
            R_minus, &
            s_plus , &
            s_minus)
       I_S_minus_1 = -h**2*I_S_minus_3
       if (isnan(I_S_minus_3)) then
          print *, 'I_S_minus_3 is NaN'
       end if
    end if
    do l = 1, NUM_FACE_VERTICES
       I_S_minus_1 = I_S_minus_1 + t(l)*I_l_minus_1(l)
    end do
    if (isnan(I_S_minus_1)) then
       print *, 'I_S_minus_1 is NaN'
       stop 2
    end if
    ! I^S_1
    if (.not. X2_minus_only) then
       I_S_plus_1 = h**2*I_S_minus_1
       do l = 1, NUM_FACE_VERTICES
          I_S_plus_1 = I_S_plus_1 + t(l)*I_l_plus_1(l)
       end do
       I_S_plus_1 = 1._wp/3._wp*I_S_plus_1
    end if
    ! Other helping quantities
    edge_unit_normal_sum = 0._wp
    do l = 1, NUM_FACE_VERTICES
       edge_unit_normal_sum = edge_unit_normal_sum &
            + edge_unit_normals(l, :)*I_l_plus_1(l)
    end do

    if (isnan(h)) then
       print *, 'h is NaN'
    end if
    if (any(isnan(t))) then
       print *, 't is NaN'
    end if
    if (any(isnan(R_0))) then
       print *, 'R_0 is NaN'
    end if
    if (any(isnan(R_plus))) then
       print *, 'R_plus is NaN'
    end if
    if (any(isnan(R_minus))) then
       print *, 'R_minus is NaN'
    end if
    if (any(isnan(s_plus))) then
       print *, 's_plus is NaN'
    end if
    if (any(isnan(s_minus))) then
       print *, 's_minus is NaN'
    end if
    
    !--------!
    !-- X1 --!
    !--------!
    if (.not. X2_minus_only) then
       hnX1_minus_3 = hn*I_S_minus_3 
       X1_minus_1 = I_S_minus_1
       X1_plus_1 = I_S_plus_1
    end if
    
    !--------!
    !-- X2 --!
    !--------!
    ! X2: 1/R
    X2_minus_1 = I_S_minus_1*temp2 + edge_unit_normal_sum
    ! X2: R
    if (.not. X2_minus_only) then
       X2_plus_1 = temp2*I_S_plus_1
       do l = 1, NUM_FACE_VERTICES
          X2_plus_1 = X2_plus_1 + 1._wp/3._wp*edge_unit_normals(l, :)*I_l_plus_3(l)
       end do
    
       !--------!
       !-- X3 --!
       !--------!
       X3_minus_1 = -hn*I_S_minus_3
       do l = 1, NUM_FACE_VERTICES
          X3_minus_1 = X3_minus_1 - edge_unit_normals(l, :)*I_l_minus_1(l)
       end do
       X3_plus_1 = edge_unit_normal_sum - hn*I_S_minus_1

       !--------!
       !-- X4 --!
       !--------!
       X4_minus_1 = -cross_prod_3D(temp3, X3_minus_1)
       X4_plus_1 = -cross_prod_3D(temp3, X3_plus_1)
    end if

  end subroutine inner_intgr_of_subtr_terms
  
  !!----------------------------------------------------------------------------

  function surface_intgr_solution(&
       h      , &
       t      , &
       R_0    , &
       R_plus , &
       R_minus, &
       s_plus , &
       s_minus) &
       result(res)
    real(wp)                              , intent(in) :: h
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: t
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: R_0
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: R_plus
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: R_minus
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: s_plus
    real(wp), dimension(NUM_FACE_VERTICES), intent(in) :: s_minus
    real(wp)                                           :: res
    ! Variables for internal use -----------------------------------------------
    real(wp) :: abs_h
    integer  :: l

    abs_h = abs(h)
    res = 0._wp
    do l = 1, NUM_FACE_VERTICES
       res = res + atan( t(l)*s_plus(l)/(R_0(l)**2 + abs_h*R_plus(l)) ) &
            - atan( t(l)*s_minus(l)/(R_0(l)**2 + abs_h*R_minus(l)) )
    end do
    res = res/abs_h
  end function surface_intgr_solution

  !!----------------------------------------------------------------------------

  function line_intgr_solution(&
       R_plus , &
       R_minus, &
       s_plus , &
       s_minus) &
       result(res)
    real(wp), intent(in) :: R_plus
    real(wp), intent(in) :: R_minus
    real(wp), intent(in) :: s_plus
    real(wp), intent(in) :: s_minus
    real(wp)             :: res
    ! Variables for internal use -----------------------------------------------
    real(wp)             :: denom1
    real(wp)             :: denom2
    denom1 = R_minus + s_minus
    denom2 = R_plus - s_plus
    ! For better numerical accuracy, use the denominator which has the greatest
    ! absolute value
    if (abs(denom1) >= abs(denom2)) then
       res = log( (R_plus + s_plus)/denom1 )
    else
       res = log( (R_minus - s_minus)/denom2 )
    end if
    if (isnan(res)) then
       print *, 'Error'
       print *, 'res is NaN : R+, R-, s+, s-:', R_plus, R_minus, s_plus, s_minus
    end if
  end function line_intgr_solution

  !!----------------------------------------------------------------------------
   
  function get_edge_lengths(&
       this    , &
       face_idx) &
       result(res)
    class (PMCHW_RWG_type), intent(in)     :: this
    integer                                :: face_idx
    real(wp), dimension(NUM_FACE_VERTICES) :: res
    ! Variables for internal use -----------------------------------------------
    integer, dimension(NUM_FACE_VERTICES) :: face_edges
    integer                               :: l

    face_edges = this%RWG_basis%mesh%get_edges_on_face(face_idx)
    ! Edge length nr. l needs to be opposite to vertex nr. l
    do l = 1, NUM_FACE_VERTICES
       if (l == NUM_FACE_VERTICES) then
          res(l) = this%RWG_basis%&
               mesh%edge_length(face_edges(1))
       else 
          res(l) = this%RWG_basis%&
               mesh%edge_length(face_edges(l + 1))
       end if
    end do 
  end function get_edge_lengths

  !!----------------------------------------------------------------------------

  Function calc_edge_unit_normals(&
       edge_lengths    , &
       face_unit_normal, &
       vertex1         , &
       vertex2         , &
       vertex3) &
       result(res)
    real(wp), dimension(NUM_FACE_VERTICES), intent(in)  :: edge_lengths
    real(wp), dimension(SPATIAL_DIM)      , intent(in)  :: face_unit_normal
    real(wp), dimension(SPATIAL_DIM)      , intent(in)  :: vertex1
    real(wp), dimension(SPATIAL_DIM)      , intent(in)  :: vertex2
    real(wp), dimension(SPATIAL_DIM)      , intent(in)  :: vertex3
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: res
    ! Variables for internal use -----------------------------------------------
    
    res(1, :) = cross_prod_3D(vertex3 - vertex2, &
         face_unit_normal)/edge_lengths(1)
    res(2, :) = cross_prod_3D(vertex1 - vertex3, &
         face_unit_normal)/edge_lengths(2)
    res(3, :) = cross_prod_3D(vertex2 - vertex1, &
         face_unit_normal)/edge_lengths(3)
  end function calc_edge_unit_normals
  
  !!----------------------------------------------------------------------------

  function map_GLQF_pnt_to_triangle_edge(&
       GLQF       , &
       edge1      , &
       edge2      , &
       edge3      , &
       vertex1    , &
       vertex2    , &
       vertex3    , &
       quad_pnt_nr, &
       edge_idx)    &
       result(res)
    real(wp), dimension(:, :)       , intent(in) :: GLQF
    real(wp), dimension(SPATIAL_DIM), intent(in) :: edge1
    real(wp), dimension(SPATIAL_DIM), intent(in) :: edge2
    real(wp), dimension(SPATIAL_DIM), intent(in) :: edge3
    real(wp), dimension(SPATIAL_DIM), intent(in) :: vertex1
    real(wp), dimension(SPATIAL_DIM), intent(in) :: vertex2
    real(wp), dimension(SPATIAL_DIM), intent(in) :: vertex3
    integer                         , intent(in) :: quad_pnt_nr
    integer                         , intent(in) :: edge_idx
    ! Variables to be returned -------------------------------------------------
    real(wp), dimension(SPATIAL_DIM)             :: res
    ! Variables for internal use -----------------------------------------------
    real(wp) :: variable_substitute
    real(wp) :: mapping_factor
    integer  :: l
    !__________________________________________________________________________!
    !/\_/\_/\_/\_/\_/\_/\_/\_/\_/\__DOCSTRING__/\_/\_/\_/\_/\_/\_/\_/\_/\_/\_/\!
    ! A Gauss-Legendre quadrature formula point is mapped from the interval
    ! (-1, 1) onto a specified edge on a triangle.
    ! Arguments:
    !     GLQF - The Gauss-Legendre quadrature formula as a matrix
    !     vertex1, vertex2, vertex3 - Cartesian coordinates to the vertices of
    !                                 the triangle to be mapped to.
    !     quad_pnt_nr - Index of the quadrature point to be mapped.
    !     edge_idx - Index specifying which edge to map to.
    ! Result:
    !     Cartesian coordinates of the point mapped onto the triangle edge.
    !__________________________________________________________________________!
   variable_substitute = 0.5_wp*GLQF(quad_pnt_nr, GQF_LEGENDRE_POINT_IDX)
    select case (edge_idx)
    case (1)
       res = variable_substitute*edge1 + 0.5_wp*(vertex3 + vertex2)
    case (2)
       res = variable_substitute*edge2 + 0.5_wp*(vertex1 + vertex3)
    case (3)
       res = variable_substitute*edge3 + 0.5_wp*(vertex2 + vertex1)
    end select
 
  end function map_GLQF_pnt_to_triangle_edge

  !!----------------------------------------------------------------------------

  function calc_green_func(wavenumber, kR, R_inv) result(res)
    complex(wp), intent(in) :: wavenumber
    complex(wp), intent(in) :: kR
    real(wp)   , intent(in) :: R_inv
    complex(wp)             :: res

    if (is_close(wavenumber%im, 0._wp)) then
       res = cmplx(cos(kR%re), sin(kR%re))
    else
       res = exp(I_IMAG*kR)
    end if
    res = res*R_inv*PI4_INV
  end function calc_green_func
  
  !!----------------------------------------------------------------------------
  
  function calc_grad_of_green_func(wavenumber, kR, R_inv, green_func) &
       result(res)
    complex(wp), intent(in)           :: wavenumber
    complex(wp), intent(in)           :: kR
    real(wp)   , intent(in)           :: R_inv
    complex(wp), optional, intent(in) :: green_func
    complex(wp)                       :: res
    ! Variables for internal use -----------------------------------------------
    complex(wp) :: exponential

    if (present(green_func)) then
       res = green_func*R_inv*(R_inv - I_IMAG*wavenumber)
    else
       if (is_close(wavenumber%im, 0._wp)) then
          exponential = cmplx(cos(kR%re), sin(kR%re))
       else
          exponential = exp(I_IMAG*kR)
       end if
       res = exponential*R_inv**2*PI4_INV*(R_inv - I_IMAG*wavenumber)
    end if
  end function calc_grad_of_green_func

  !!----------------------------------------------------------------------------
  
  subroutine Cauchy_principal_value(&
       green_func        , &
       grad_of_green_func, &
       wavenumber        , &
       kR                , &
       R)
    complex(wp), intent(inout) :: green_func
    complex(wp), intent(inout) :: grad_of_green_func
    complex(wp), intent(in)           :: wavenumber
    complex(wp), intent(in)           :: kR
    real(wp)   , intent(in)           :: R
    ! Variables for internal use -----------------------------------------------
    real(wp) :: R_inv

    if (is_close(R, ZERO)) then
!!$    if (.true.) then
       green_func = ZERO_CMPLX
       grad_of_green_func = ZERO_CMPLX
    else
       R_inv = 1._wp/R
       green_func = calc_green_func(wavenumber, kR, R_inv)
       grad_of_green_func = calc_grad_of_green_func(&
            wavenumber, &
            kR        , &
            R_inv     , &
            green_func=green_func)
    end if
  end subroutine Cauchy_principal_value
  
  !!----------------------------------------------------------------------------
  
  !=================================!==========================================!
end module PMCHW_RWG_mod
