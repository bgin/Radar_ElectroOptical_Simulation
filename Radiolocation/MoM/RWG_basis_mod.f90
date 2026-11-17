module RWG_basis_mod
!!==============================================================================
! This module defines the RWG basis type, which inherits the mesh_mod_type
! from mesh_mod.f90 and represents an RWG basis function mapping of a
! surface mesh.
!
! Abbreviations:
!  CS - Closed Surface
!  OS - Open Surface
!  GQ - Gaussian Quadrature
! 
! Last edited: March 7th 2021.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use iso_fortran_env  , only: real64
  use mesh_mod         , only: mesh_type
  use math_funcs_mod   , only: cross_prod_3D
  use is_close_mod     , only: is_close
  use constants_mod    , only: PI
  use io_mod           , only: r8mat_write

  implicit none

  !!===================!!
  ! External procedures !
  !=====================!=======================================================

  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: RWG_basis_type ! Main type

  ! Constants
  integer, parameter, public :: SPATIAL_DIM = 3
  integer, parameter, public :: NUM_FACES_IN_BASIS = 2
  integer, parameter, public :: NUM_FACE_VERTICES = 3
  character(*), parameter, public :: MODULE_NAME = 'RWG_basis_mod'

  
  !!==================================!!
  ! Private types/procedures/constants !
  !====================================!========================================
  private

  !!------------------------!!
  ! Derived type definitions !
  !--------------------------!--------------------------------------------------


  !!---------!!
  ! Main type !
  !-----------!-----------------------------------------------------------------
  type RWG_basis_type
     type (mesh_type)                          :: mesh
     integer                                   :: num_bases
     integer , dimension(:)      , allocatable :: basis_edges
     integer , dimension(:, :)   , allocatable :: adjacent_faces
     real(wp), dimension(:)      , allocatable :: basis_edge_length
   contains
     ! Initialisers
     procedure, pass(this), public :: initialise
     ! Deallocation
     procedure, pass(this), public :: deallocate_attributes
     ! Get-functions
     procedure, pass(this), public :: get_num_bases
     procedure, pass(this), public :: get_free_vertices
     procedure, pass(this), public :: get_basis_edge_coords
     procedure, pass(this), public :: get_basis_edge_length
     procedure, pass(this), public :: get_adjacent_faces
     ! Calculations
     procedure, pass(this), public :: integrate_tested_func 
     ! Validations
     procedure, pass(this), public :: validate_current_direction
     ! Write procedures
     procedure, pass(this), public :: write_RWG_basis

  end type RWG_basis_type
     
  
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!


  !!==================================!!
  ! RWG_basis_type internal procedures !
  !====================================!========================================
  !!------------!!
  ! Initialisers !
  !--------------!--------------------------------------------------------------
  subroutine initialise(this, mesh) 
    class (RWG_basis_type), intent(inout) :: this
    type (mesh_type), intent(in)          :: mesh
    ! Variables for internal use -----------------------------------------------
    character(:), allocatable             :: msg
    logical                               :: closed_surface
    integer                               :: edge_order
    integer                               :: num_face_vertices
    integer                               :: num_handles
    integer                               :: num_apertures
    integer                               :: num_boundary_edges
    integer                               :: num_faces
    integer                               :: num_edges
    integer                               :: num_vertices
    integer                               :: num_bases
    integer, dimension(2)                 :: adjacent_faces
    integer                               :: boundary_edges_count
    integer                               :: num_bases_created
    integer                               :: edge_occurence
    integer, dimension(3)                 :: vertices
    real(wp)                              :: area
    real(wp)                              :: length
    integer                               :: error_nr
    integer                               :: i
    integer                               :: j
    integer                               :: k

    call mesh%get_topology(&
         num_handles       , &
         num_apertures     , &
         num_boundary_edges, &
         num_faces         , &
         num_edges         , &
         num_vertices)

    edge_order = mesh%get_edge_order()
    num_face_vertices = mesh%get_face_order()
    if (num_face_vertices /= 3) then
       print *, 'Error: RWG_basis_mod.f90: RWG_basis_type_:'
       print *, '       RWG basis construction needs a triangulated surface.'
       stop 1
    end if
    num_bases = num_edges - num_boundary_edges
    this%mesh = mesh
    this%num_bases = num_bases
    allocate(this%basis_edges(num_bases))
    allocate(this%basis_edge_length(num_bases))
    allocate(this%adjacent_faces(num_bases, 2))

    ! Find basis edges and assign adjacent faces.
    ! A boundary edge has only one adjacent face
    boundary_edges_count = 0
    num_bases_created = 0
    do i = 1, num_edges
       edge_occurence = 0
       do j = 1, num_faces
          do k = 1, num_face_vertices
             if (mesh%faces(j)%edges(k) == i) then
                if (edge_occurence == 0) then
                   adjacent_faces(1) = j
                   edge_occurence = edge_occurence + 1
                else if (edge_occurence == 1) then
                   adjacent_faces(2) = j
                   edge_occurence = edge_occurence + 1
                end if
                exit
             end if
          end do
          if (edge_occurence == 2) then
             exit
          end if
       end do
       if (edge_occurence == 2) then
          num_bases_created = num_bases_created + 1
          this%basis_edges(num_bases_created) = i
          this%adjacent_faces(num_bases_created, :) &
               = adjacent_faces
       else if (edge_occurence == 1) then
          boundary_edges_count = boundary_edges_count + 1
       else
          print *, 'Error: RWG_basis_mod.f90: RWG_basis_type_:'
          print *, '       Edge has no adjacent faces.'
          print *, '         edge:', i
          stop 1
       end if
    end do

    if (boundary_edges_count /= num_boundary_edges) then
       print *, 'Error: RWG_basis_mod.f90: RWG_basis_type_:'
       print *, '       Counted boundary edges is not correct'
       print *, '         Count:', boundary_edges_count
       stop 1
    else if (num_bases_created /= num_bases) then
      print *, 'Error: RWG_basis_mod.f90: RWG_basis_type_:'
       print *, '       Created too few bases.'
       print *, '         Count:', num_bases_created
       stop 1
    end if
     
    ! Calculate basis constants
    do i = 1, num_bases 
       length = this%mesh%edge_length(&
            this%basis_edges(i))
       this%basis_edge_length(i) = length
    end do

    ! Validate the orientation of the basis edges with the orientation of
    ! the faces, i.e. validate the current orientation on the basis.
    call this%validate_current_direction(error_nr)
    if (error_nr /= 0) then
       print *, error_nr
       write (msg, "(A50, I2, A8)") 'Current direction validation basis nr. ' &
            , error_nr, ' failed.'
       print *, ''
       print *, 'Error: ', MODULE_NAME, ': ', 'initialise', ':'
       print *, '       ', msg
    end if
       

  end subroutine initialise
  
  !!----------------------------------------------------------------------------

  subroutine deallocate_attributes(this)
    class (RWG_basis_type), intent(inout) :: this
    deallocate(this%basis_edges)
    deallocate(this%basis_edge_length)
    deallocate(this%adjacent_faces)
  end subroutine deallocate_attributes
  

  !!-------------!!
  ! Get-functions !
  !---------------!-------------------------------------------------------------
  function get_num_bases(this) result(return_value)
    class (RWG_basis_type), intent(in) :: this
    integer                            :: return_value
    return_value = this%num_bases
  end function get_num_bases
  
  !!----------------------------------------------------------------------------
    
  function get_basis_edge_coords(this, basis_idx) result(return_value)
    class (RWG_basis_type), intent(in) :: this
    integer               , intent(in) :: basis_idx
    real(wp), dimension(2, this%mesh%spatial_dim) :: return_value
    return_value = this%mesh%get_edge_coords(basis_idx)
  end function get_basis_edge_coords

  !!----------------------------------------------------------------------------

  function get_basis_edge_length(this, basis_idx) result(return_value)
    class (RWG_basis_type), intent(in) :: this
    integer                            :: basis_idx
    real(wp)                           :: return_value
    return_value = this%basis_edge_length(basis_idx)
  end function get_basis_edge_length
  
  !!----------------------------------------------------------------------------
  function get_free_vertices(this, basis_idx) result(return_value)
    class (RWG_basis_type), intent(in)     :: this
    integer               , intent(in)     :: basis_idx
    integer, dimension(NUM_FACES_IN_BASIS) :: return_value
    ! Variables for internal use -----------------------------------------------
    integer, dimension(NUM_FACE_VERTICES) :: face_vertices
    integer                               :: i
    integer                               :: j

    do i = 1, NUM_FACES_IN_BASIS
       face_vertices = this%mesh%get_vertices_of_face(&
            this%adjacent_faces(basis_idx, i))
       do j = 1, 3
          if (.not. any(this%mesh%edges(basis_idx)%vertices &
               == face_vertices(j))) then
             return_value(i) = face_vertices(j)
             exit 
          end if
       end do
    end do
  end function get_free_vertices
  
  !!----------------------------------------------------------------------------
  
  function get_adjacent_faces(this, basis_idx) result(return_value)
    class (RWG_basis_type), intent(in) :: this
    integer                            :: basis_idx
    integer, dimension(2)              :: return_value
    return_value = this%adjacent_faces(basis_idx, :)
  end function get_adjacent_faces
  

  !!-------------!!
  ! Calculations  !
  !---------------!-------------------------------------------------------------
  function integrate_tested_func(this, gauss_quad_formula, func) &
       result(res)
    class (RWG_basis_type)   , intent(in) :: this
    real(wp), dimension(:, :), intent(in) :: gauss_quad_formula
    interface
       function func(pos_vec) result(func_value)
         use working_precision, only: wp
         implicit none
         real(wp)   , dimension(:), intent(in)  :: pos_vec
         complex(wp), dimension(:), allocatable :: func_value
       end function func
    end interface
    real(wp), dimension(this%num_bases) :: res
    ! Variables for internal use -----------------------------------------------
    real(wp), dimension(3, 3)                   :: triangle_coords
    integer,  dimension(2)                      :: free_vertices
    real(wp), dimension(2, 3)                   :: free_vertices_coords
    real(wp), dimension(3)                      :: r
    real(wp)                                    :: ksi
    real(wp)                                    :: eta
    real(wp)                                    :: zeta
    real(wp)                                    :: w
    complex(wp), dimension(this%mesh%num_faces, 3) :: I_ksi
    complex(wp), dimension(this%mesh%num_faces, 3) :: I_eta
    complex(wp), dimension(this%mesh%num_faces, 3) :: I_zeta
    complex(wp), dimension(this%mesh%num_faces, 3) :: I_
    integer                                     :: m
    integer                                     :: i
    integer                                     :: p
    integer                                     :: q

    do p = 1, this%mesh%num_faces
       triangle_coords = this%mesh%get_face_coords(p)
       I_ksi(p, :) = cmplx(0._wp, 0._wp)
       I_eta(p, :) = cmplx(0._wp, 0._wp)
       I_(p, :) = cmplx(0._wp, 0._wp)
       do i = 1, size(gauss_quad_formula, dim=1)
          w = gauss_quad_formula(i, 1)
          ksi = gauss_quad_formula(i, 2)
          eta = gauss_quad_formula(i, 3)
          zeta = gauss_quad_formula(i, 4)
          r = ksi*triangle_coords(1, :) + eta*triangle_coords(2, :) &
               + zeta*triangle_coords(3, :)
          I_ksi(p, :) = I_ksi(p, :) + w*ksi*func(r)
          I_eta(p, :) = I_eta(p, :) + w*eta*func(r)
          I_(p, :) = I_(p, :) + w*func(r)
       end do
       I_zeta(p, :) = I_(p, :) - I_ksi(p, :) - I_eta(p, :)
    end do

    do m = 1, this%num_bases
       p = this%adjacent_faces(m, 1)
       q = this%adjacent_faces(m, 2)
       free_vertices = this%get_free_vertices(m)
       free_vertices_coords(1, :) =this%mesh%get_vertex_coords(free_vertices(1))
       free_vertices_coords(2, :) =this%mesh%get_vertex_coords(free_vertices(2))
       triangle_coords = this%mesh%get_face_coords(p)
       res(m) =  &
            + dot_product(triangle_coords(1, :), I_ksi(p, :)) &
            + dot_product(triangle_coords(2, :), I_eta(p, :)) &
            + dot_product(triangle_coords(3, :), I_zeta(p, :)) &
            - dot_product(free_vertices_coords(1, :), I_(p, :))
       triangle_coords = this%mesh%get_face_coords(q)
       res(m) = res(m) &
            - dot_product(triangle_coords(1, :), I_ksi(q, :)) &
            - dot_product(triangle_coords(2, :), I_eta(q, :)) &
            - dot_product(triangle_coords(3, :), I_zeta(q, :)) &
            + dot_product(free_vertices_coords(2, :), I_(q, :))
       res(m) = res(m)*this%basis_edge_length(m)/2._wp
    end do
  end function integrate_tested_func
        
  !!----------------------------------------------------------------------------
  
  subroutine validate_current_direction(this, error_nr)
    class (RWG_basis_type), intent(in)    :: this
    integer               , intent(inout) :: error_nr
    ! Variables for internal use -----------------------------------------------
    integer     , dimension(NUM_FACE_VERTICES)  :: face_vertices
    integer     , dimension(NUM_FACE_VERTICES)  :: plus_face
    integer     , dimension(NUM_FACE_VERTICES)  :: minus_face
    integer     , dimension(NUM_FACES_IN_BASIS) :: T_n
    integer     , dimension(NUM_FACES_IN_BASIS) :: edge_vertices
    integer     , dimension(NUM_FACES_IN_BASIS) :: free_vertices
    integer                                     :: plus_idx
    integer                                     :: minus_idx
    integer                                     :: n
    integer                                     :: p
    integer                                     :: i
  
    error_nr = 0
    do n = 1, this%num_bases
       T_n = this%get_adjacent_faces(n)
       free_vertices = this%get_free_vertices(n)
       edge_vertices = this%mesh%get_vertices_of_edge(n)
       plus_face = this%mesh%get_vertices_of_face(T_n(1))
       minus_face = this%mesh%get_vertices_of_face(T_n(2))

       ! Check orientiation of plus face with respect to orientation of basis
       ! edge
       do i = 1, NUM_FACE_VERTICES
          if (free_vertices(1) == plus_face(i)) then
             plus_idx = i
          end if
       end do
       select case (plus_idx)
       case (1)
          if ( .not. all(edge_vertices == [ plus_face(2), plus_face(3)&
               ]) ) then
             error_nr = n
          end if
       case (2)
          if ( .not. all(edge_vertices == [ plus_face(3), plus_face(1)&
               ]) ) then
             error_nr = n
          end if
       case (3)
          if ( .not. all(edge_vertices == [ plus_face(1), plus_face(2)&
               ]) ) then
             error_nr = n
          end if
       end select
       ! Check orientiation of minus face with respect to orientation of basis
       ! edge
       do i = 1, NUM_FACE_VERTICES
          if (free_vertices(2) == minus_face(i)) then
             minus_idx = i
          end if
       end do
       select case (minus_idx)
       case (1)
          if ( .not. all(edge_vertices == [ minus_face(3), minus_face(2)&
               ]) ) then
             error_nr = n
          end if
       case (2)
          if ( .not. all(edge_vertices == [ minus_face(1), minus_face(3)&
               ]) ) then
             error_nr = n
          end if
       case (3)
          if ( .not. all(edge_vertices == [ minus_face(2), minus_face(1)&
               ]) ) then
             error_nr = n
          end if
       end select
    end do

  end subroutine validate_current_direction
 
  !!----------------------------------------------------------------------------

  subroutine write_RWG_basis(this, filename)
    class (RWG_basis_type), intent(in) :: this
    character(len=*) , intent(in)      :: filename
    ! Variables for internal use -----------------------------------------------
    real(wp), dimension(:, :), allocatable              :: table
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: plus_face_coords
    real(wp), dimension(NUM_FACE_VERTICES, SPATIAL_DIM) :: minus_face_coords
    real(wp), dimension(2, SPATIAL_DIM)                 :: edge_coords
    real(wp), dimension(2, SPATIAL_DIM)                 :: edge_coors
    integer , dimension(NUM_FACES_IN_BASIS)             :: T_n
    integer                                             :: num_cols
    integer                                             :: n
    integer                                             :: j
    integer                                             :: k

    num_cols = 2*SPATIAL_DIM*NUM_FACE_VERTICES + SPATIAL_DIM*2
    allocate(table(this%num_bases, num_cols))
    do n = 1, this%num_bases
       T_n = this%get_adjacent_faces(n)
       plus_face_coords = this%mesh%get_face_coords(T_n(1))
       minus_face_coords = this%mesh%get_face_coords(T_n(2))
       edge_coords = this%mesh%get_edge_coords(n)
       do j = 1, NUM_FACE_VERTICES
          do k = 1, SPATIAL_DIM
             table(n, (j - 1)*SPATIAL_DIM + k) = plus_face_coords(j, k)
             table(n, (j - 1)*SPATIAL_DIM + k &
                  + SPATIAL_DIM*NUM_FACE_VERTICES) = minus_face_coords(j, k)
          end do
       end do
       do j = 1, 2
          do k = 1, SPATIAL_DIM
             table(n, (j - 1)*SPATIAL_DIM + 2*SPATIAL_DIM*NUM_FACE_VERTICES+k)&
                  = edge_coords(j, k)
          end do
       end do
    end do
    if (wp == real64) then
       call r8mat_write(filename, this%num_bases, num_cols, table)
    else
       print *, 'No write routine for current precision.'
    end if
  end subroutine write_RWG_basis
  
  !=================================!==========================================!
end module RWG_basis_mod

