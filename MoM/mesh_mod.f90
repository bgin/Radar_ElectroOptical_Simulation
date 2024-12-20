module mesh_mod
!!==============================================================================
! This module represents a mesh in the hierarchical structure:
!    face --> edge --> vertex --> point
!
! A mesh_type is initialised by reading a .mesh-formated file.
!
! Abbreviations:
!  CS - Closed Surface
!  OS - Open Surface
!`
! Last edited: March 7th 2021.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use math_funcs_mod   , only: cross_prod_3D
  use io_mod           , only: open_read_gmsh2
  use io_mod           , only: r8mat_write
  use iso_fortran_env  , only: real64
  use is_close_mod     , only: is_close
  use constants_mod    , only: PI
  use constants_mod    , only: ZERO

  implicit none

  !!===================!!
  ! External procedures !
  !=====================!=======================================================
  external :: dnrm2 ! BLAS level 1: Euclidean norm (double)

  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: mesh_type  ! Main type
  public :: face_type
  public :: edge_type
  public :: vertex_type
  public :: node_type

  !!==================================!!
  ! Private types/procedures/constants !
  !====================================!========================================
  private :: eval_Euler_characteristic_CS
  private :: check_input_triangulated_surface
  private :: eval_topology_on_triangulated_surface

  ! From here on everything is by defualt decleared private
  private

  !!------------------------!!
  ! Derived type definitions !
  !--------------------------!--------------------------------------------------
  type face_type
     ! Type to store indices related to the edges forming a face on the mesh.
     integer, dimension(:), allocatable :: edges
  end type face_type

  type edge_type
     ! Type to store the indicies related to the vertices forming an edge.
     integer, dimension(2)              :: vertices
     ! Length of node_idx depends on edge order:
     !  linear:    len = 0
     !  quadratic: len = 1
     !  cubic:     len = 2
     integer, dimension(:), allocatable :: node_idx
   contains
     procedure, pass(this), public :: initialise_edge
  end type edge_type

  type vertex_type
     ! Type to store the index of the node at which the vertex is located.
     integer :: node_idx
  end type vertex_type

  type node_type
     ! Type to store the 3D coordinates of a node.
     real(wp), dimension(3) :: coords
  end type node_type

  !!---------!!
  ! Main type !
  !-----------!-----------------------------------------------------------------
  type mesh_type
     type (face_type)  , dimension(:), allocatable :: faces
     type (edge_type)  , dimension(:), allocatable :: edges
     type (vertex_type), dimension(:), allocatable :: vertices
     type (node_type)  , dimension(:), allocatable :: nodes
     integer                                       :: edge_order
     integer                                       :: spatial_dim
     integer                                       :: face_order
     integer                                       :: num_faces
     integer                                       :: num_edges
     integer                                       :: num_vertices
     integer                                       :: num_nodes
     integer                                       :: num_handles
     integer                                       :: num_apertures
     integer                                       :: num_boundary_edges
     logical                                       :: closed_surface
   contains
     ! Initialisers
     procedure, pass(this), public  :: initialise
     procedure, pass(this), public  :: initialise_tetrahedron
     ! Writing procedures
     procedure, pass(this), public  :: write_mesh
     ! Deallocation of attributes
     procedure, pass(this), public  :: deallocate_attributes
     ! Get-procedures
     procedure, pass(this), public  :: get_closed_surface
     procedure, pass(this), public  :: get_edge_order
     procedure, pass(this), public  :: get_spatial_dim
     procedure, pass(this), public  :: get_face_order
     procedure, pass(this), public  :: get_num_handles
     procedure, pass(this), public  :: get_num_faces
     procedure, pass(this), public  :: get_num_edges
     procedure, pass(this), public  :: get_num_vertices
     procedure, pass(this), public  :: get_num_nodes
     procedure, pass(this), public  :: get_topology
     procedure, pass(this), public  :: get_edges_on_face
     procedure, pass(this), public  :: get_vertices_of_edge
     procedure, pass(this), public  :: get_vertices_of_face
     procedure, pass(this), public  :: get_vertex_coords
     procedure, pass(this), public  :: get_edge_coords
     procedure, pass(this), public  :: get_face_coords
     ! Calculations
     procedure, pass(this), public  :: face_normal
     procedure, pass(this), public  :: face_unit_normal
     procedure, pass(this), public  :: face_area
     procedure, pass(this), public  :: face_centroid
     procedure, pass(this), public  :: edge_length
     procedure, pass(this), public  :: surface_area
     procedure, pass(this), public  :: volume
     ! Other routines
     procedure, pass(this), public  :: print
     procedure, pass(this), public  :: scale_nodes
     ! For determining whether a point is inside or outside
     ! of meshe_nodes
     procedure, pass(this), public  :: solid_angle_spanned_by_face
     procedure, pass(this), public  :: solid_angle_spanned_by_mesh
     procedure, pass(this), public  :: is_obs_pnt_inside_mesh
     ! Private procedures for internal use
     procedure, pass(this), private :: create_member_types_linear
     
     
  end type mesh_type


  !!==============================!!
  ! Overloaded operator interfaces !
  !================================!============================================
  ! Overloaded operator interfaces
  interface operator (==)
     module procedure is_edges_equal
  end interface operator (==)


  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  
  !!=================!!
  ! Public procedures !
  !===================!=========================================================
  !!------------!!
  ! Constructors !
  !--------------!--------------------------------------------------------------


  !!=============================!!
  ! mesh_type internal procedures !
  !===============================!=============================================
  !!------------!!
  ! Initialisers !
  !--------------!--------------------------------------------------------------
  subroutine initialise_edge(this, vertices, node_idx) 
    class (edge_type)              , intent(inout) :: this
    integer, dimension(2)          , intent(in)    :: vertices
    integer, dimension(:), optional, intent(in)    :: node_idx
    this%vertices = vertices
    if (present(node_idx)) then
       allocate(this%node_idx(size(node_idx)))
       this%node_idx = node_idx
    else
       allocate(this%node_idx(0))
    end if
  end subroutine initialise_edge

  
  subroutine initialise(this, &
       MSH_FILE,              &
       FILE_FORMAT,           &
       closed_surface,        &
       edge_order,            &
       spatial_dim,           &
       face_order,            &
       num_handles_in,        &
       num_apertures_in,      &
       num_boundary_edges_in)
    class (mesh_type), intent(inout)      :: this
    character(len=*) , intent(in)         :: MSH_FILE
    character(len=*) , intent(in)         :: FILE_FORMAT
    logical          , intent(in)         :: closed_surface
    integer          , intent(in)         :: edge_order
    integer          , intent(in)         :: spatial_dim
    integer          , intent(in)         :: face_order
    integer, optional, intent(in)         :: num_handles_in
    integer, optional, intent(in)         :: num_apertures_in
    integer, optional, intent(in)         :: num_boundary_edges_in
    ! Variables for internal use -----------------------------------------------
    real(wp),dimension(:, :), allocatable :: nodes
    integer, dimension(:, :), allocatable :: elements
    integer                               :: num_elements
    integer                               :: num_faces
    integer                               :: num_edges
    integer                               :: num_vertices
    integer                               :: num_nodes
    integer                               :: num_handles
    integer                               :: num_apertures
    integer                               :: num_boundary_edges

    !file_type = read_file_type(MSH_FILE)
    select case (FILE_FORMAT)
    case ('gmsh2')
       call open_read_gmsh2(MSH_FILE, spatial_dim, face_order, nodes, &
            elements)
    end select
    
    num_elements = size(elements,dim=1)
    num_nodes = size(nodes, dim=1)
    num_faces = num_elements ! Always the case for surface meshes

    if (face_order > 3) then
       print *, 'Error: mesh_mod.f90: mesh_type_initialise:'
       print *, '       Functionality for reading quadriliteral faces', &
            ' is not implemented..'
       stop 1
    else if (face_order < 3) then
       print *, 'Error: mesh_mod.f90: mesh_type_initialise:'
       print *, '       Invalid input: face_order'
       stop 1
    else
       call check_input_triangulated_surface(&
            closed_surface, &
            edge_order, &
            num_handles_in=num_handles_in, &
            num_apertures_in=num_apertures_in, &
            num_boundary_edges_in=num_boundary_edges_in)

       call eval_topology_on_triangulated_surface(&
            num_edges, &
            num_vertices, &
            num_handles, &
            num_apertures, &
            num_boundary_edges, &
            num_faces, &
            num_nodes, &
            closed_surface, &
            edge_order, &
            num_handles_in=num_handles_in, &
            num_apertures_in=num_apertures_in, &
            num_boundary_edges_in=num_boundary_edges_in)
    end if

    this%closed_surface = closed_surface
    this%edge_order = edge_order
    this%spatial_dim = spatial_dim
    this%face_order = face_order
    this%num_faces = num_faces
    this%num_edges = num_edges
    this%num_vertices = num_vertices
    this%num_nodes = num_nodes
    this%num_handles = num_handles
    this%num_apertures = num_apertures
    this%num_boundary_edges = num_boundary_edges
    allocate(this%faces(num_faces))
    allocate(this%edges(num_edges))
    allocate(this%vertices(num_vertices))
    allocate(this%nodes(num_nodes))
       
    select case (edge_order)
    case (0)
       call this%create_member_types_linear(nodes, elements)
!!$    case (1)
       ! Not yet implemented
!!$       call this%create_member_types_quadratic(nodes, elements)
!!$    case (2)
       ! Not yet implemented
!!$       call this%create_member_types_cubic(nodes, elements)
    end select
    
    ! Test if face orientation is correct
    if (closed_surface) then
       if (this%volume() < 0) then
          print *, 'Volume is negative:', this%volume()
          stop 1
          ! Not yet implemented
!!$          call this%flip_face_orientation()
       end if
    end if

    
  end subroutine initialise

  !!----------------------------------------------------------------------------
   
  subroutine initialise_tetrahedron(this) 
    class (mesh_type), intent(inout) :: this
    this%num_nodes = 4
    this%num_vertices = 4
    this%num_edges = 6
    this%num_faces = 4
    allocate (this%nodes(this%num_nodes))
    allocate (this%vertices(this%num_vertices))
    allocate (this%edges(this%num_edges))
    allocate (this%faces(this%num_faces))
    this%nodes(1) = node_type([ 1.0_wp, -1.0_wp, 1.0_wp ])
    this%nodes(2) = node_type([ -1.0_wp, 1.0_wp, 1.0_wp ])
    this%nodes(3) = node_type([ 1.0_wp, 1.0_wp, -1.0_wp ])
    this%nodes(4) = node_type([ -1.0_wp, -1.0_wp, -1.0_wp ])
    this%vertices(1) = vertex_type(1)
    this%vertices(2) = vertex_type(2)
    this%vertices(3) = vertex_type(3)
    this%vertices(4) = vertex_type(4)
    call this%edges(1)%initialise_edge([ 1, 3 ])
    call this%edges(2)%initialise_edge([ 3, 2 ])
    call this%edges(3)%initialise_edge([ 2, 1 ])
    call this%edges(4)%initialise_edge([ 2, 4 ])
    call this%edges(5)%initialise_edge([ 4, 1 ])
    call this%edges(6)%initialise_edge([ 4, 3 ])
    this%faces(1) = face_type([ 1, 2, 3 ])
    this%faces(2) = face_type([ 3, 4, 5 ])
    this%faces(3) = face_type([ 2, 6, 4 ])
    this%faces(4) = face_type([ 5, 6, 1 ])
    this%num_handles = eval_Euler_characteristic_CS( &
         num_faces=this%num_faces, &
         num_edges=this%num_edges, &
         num_vertices=this%num_vertices)
    this%closed_surface = .true.
    this%edge_order = 0
    this%spatial_dim = 3
    this%face_order = 3
  end subroutine initialise_tetrahedron
  
  !!----------------------------------------------------------------------------

  subroutine deallocate_attributes(this)
    class (mesh_type), intent(inout) :: this
    ! Variables for internal use -----------------------------------------------
    integer                          :: i

    do i = 1, this%num_faces
       deallocate(this%faces(i)%edges)
    end do
    do i = 1, this%num_edges
       deallocate(this%edges(i)%node_idx)
    end do
    deallocate(this%faces)
    deallocate(this%edges)
    deallocate(this%vertices)
    deallocate(this%nodes)
  end subroutine deallocate_attributes
 
  !!--------------!!
  ! Write routines !
  !----------------!------------------------------------------------------------
    
  subroutine write_mesh(this, filename)
    class (mesh_type), intent(in) :: this
    character(len=*) , intent(in) :: filename
    ! Variables for internal use -----------------------------------------------
    real(wp), dimension(this%num_faces,this%spatial_dim*this%face_order):: table
    real(wp), dimension(this%face_order, this%spatial_dim) :: face_coords
    integer                                                :: i
    integer                                                :: j
    integer                                                :: k

    do i = 1, this%num_faces
       face_coords = this%get_face_coords(i)
       do j = 1, this%face_order
          do k = 1, this%spatial_dim
             table(i, (j - 1)*this%spatial_dim + k) = face_coords(j, k)
          end do
       end do
    end do
    if (wp == real64) then
       call r8mat_write(filename, this%num_faces, &
            this%spatial_dim*this%face_order, table)
    else
       print *, 'No write routine for current precision.'
    end if
  end subroutine write_mesh
 
  !!-------------!!
  ! Get-functions !
  !---------------!-------------------------------------------------------------
  function get_closed_surface(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    logical                        :: return_value
    return_value = this%closed_surface
  end function get_closed_surface

  !!----------------------------------------------------------------------------

  function get_spatial_dim(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%spatial_dim
  end function get_spatial_dim
  
  !!----------------------------------------------------------------------------

  function get_face_order(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%face_order
  end function get_face_order
  
  !!----------------------------------------------------------------------------

  function get_edge_order(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%edge_order
  end function get_edge_order

  !!----------------------------------------------------------------------------
  
  function get_num_handles(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%num_handles
  end function get_num_handles
  
  !!----------------------------------------------------------------------------

  function get_num_faces(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%num_faces
  end function get_num_faces
  
  !!----------------------------------------------------------------------------

  function get_num_edges(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%num_edges
  end function get_num_edges

  !!----------------------------------------------------------------------------

  function get_num_vertices(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%num_vertices
  end function get_num_vertices

  !!----------------------------------------------------------------------------

  function get_num_nodes(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    integer                        :: return_value
    return_value = this%num_nodes
  end function get_num_nodes

  !!----------------------------------------------------------------------------

  subroutine get_topology(&
       this              , &
       num_handles       , &
       num_apertures     , &
       num_boundary_edges, &
       num_faces         , &
       num_edges         , &
       num_vertices)
    class (mesh_type), intent(in)    :: this
    integer          , intent(inout) :: num_handles
    integer          , intent(inout) :: num_apertures
    integer          , intent(inout) :: num_boundary_edges
    integer          , intent(inout) :: num_faces
    integer          , intent(inout) :: num_edges
    integer          , intent(inout) :: num_vertices
    num_handles = this%num_handles
    num_apertures = this%num_apertures
    num_boundary_edges = this%num_boundary_edges
    num_faces = this%num_faces
    num_edges = this%num_edges
    num_vertices = this%num_vertices
  end subroutine get_topology

  !!----------------------------------------------------------------------------

  function get_edges_on_face(this, face_idx) result(return_value)
    class (mesh_type)    , intent(in)  :: this
    integer              , intent(in)  :: face_idx
    integer, dimension(3)              :: return_value
    return_value = this%faces(face_idx)%edges
  end function get_edges_on_face

  !!----------------------------------------------------------------------------

  function get_vertices_of_edge(this, edge_idx) result(return_value)
    class (mesh_type), intent(in)      :: this
    integer          , intent(in)      :: edge_idx
    integer, dimension(2)              :: return_value
    return_value = this%edges(edge_idx)%vertices
  end function get_vertices_of_edge

  !!----------------------------------------------------------------------------

  function get_vertex_coords(this, vertex_idx) result(return_value)
    class (mesh_type)     , intent(in)    :: this
    integer               , intent(in)    :: vertex_idx
    real(wp), dimension(this%spatial_dim) :: return_value
    return_value = this%nodes(this%vertices(vertex_idx)%node_idx)%coords
  end function get_vertex_coords

  !!----------------------------------------------------------------------------

  function get_edge_coords(this, edge_idx) result(return_value)
    class (mesh_type)        , intent(in)    :: this
    integer                  , intent(in)    :: edge_idx
    real(wp), dimension(2, this%spatial_dim) :: return_value
    ! Variables for internal use -----------------------------------------------
    integer , dimension(2)                   :: vertices
    vertices = this%edges(edge_idx)%vertices
    return_value(1, :) = this%get_vertex_coords(vertices(1))
    return_value(2, :) = this%get_vertex_coords(vertices(2))
  end function get_edge_coords

  !!----------------------------------------------------------------------------

  function get_vertices_of_face(this, face_idx) result(return_value)
    class (mesh_type)        , intent(in)      :: this
    integer                  , intent(in)      :: face_idx
    integer , dimension(this%face_order)       :: return_value
    ! Variables for internal use -----------------------------------------------
    integer , dimension(this%face_order)        :: edges_on_face
    integer , dimension(2)                      :: vertices_of_edge1
    integer , dimension(2)                      :: vertices_of_edge2
    
    edges_on_face = this%faces(face_idx)%edges
    vertices_of_edge1 = this%edges(edges_on_face(1))%vertices
    vertices_of_edge2 = this%edges(edges_on_face(2))%vertices
    ! Find the three unique vertices of the face and arrange them in the
    ! array 'return_value' such that the orientation of the face is correct
    if (((vertices_of_edge1(1) == vertices_of_edge2(1)) .and. &
    (vertices_of_edge1(2) == vertices_of_edge2(2))) .or. &
    ((vertices_of_edge1(2) == vertices_of_edge2(1)) .and. &
    (vertices_of_edge1(1) == vertices_of_edge2(2)))) then
       ! If two edges represents the same vertices, exit with error.
       print *, 'Face has two identical edges.'
       print *, 'Edges on face: ', edges_on_face
       print *, 'Exiting..'
       stop 1
    else if (vertices_of_edge1(1) == vertices_of_edge2(1)) then
       return_value(1) = vertices_of_edge1(2)
       return_value(2) = vertices_of_edge1(1)
       return_value(3) = vertices_of_edge2(2)
    else if (vertices_of_edge1(1) == vertices_of_edge2(2)) then
       return_value(1) = vertices_of_edge1(2)
       return_value(2) = vertices_of_edge1(1)
       return_value(3) = vertices_of_edge2(1)
    else if (vertices_of_edge1(2) == vertices_of_edge2(1)) then
       return_value(1) = vertices_of_edge1(1)
       return_value(2) = vertices_of_edge1(2)
       return_value(3) = vertices_of_edge2(2)
    else if (vertices_of_edge1(2) == vertices_of_edge2(2)) then
       return_value(1) = vertices_of_edge1(1)
       return_value(2) = vertices_of_edge1(2)
       return_value(3) = vertices_of_edge2(1)
    else
       ! If the two edges does not have a common vertex, exit with error.
       print *, 'Could not find matching vertices on face edges.'
       print *, 'Edges on face: ', edges_on_face
       print *, 'Vertices of edge1: ', vertices_of_edge1
       print *, 'Vertices of edge2: ', vertices_of_edge2
       print *, 'Exiting..'
       stop 1
    end if
  end function get_vertices_of_face

  !!----------------------------------------------------------------------------
  
  function get_face_coords(this, face_idx) result(return_value)
    class (mesh_type)        , intent(in)       :: this
    integer                  , intent(in)       :: face_idx
    real(wp), dimension(this%face_order, this%spatial_dim) :: return_value
    ! Variables for internal use -----------------------------------------------
    integer , dimension(this%face_order) :: vertices_of_face
    integer                                     :: i
    vertices_of_face = this%get_vertices_of_face(face_idx)
    do i = 1, this%face_order
       return_value(i, :) = this%get_vertex_coords(vertices_of_face(i))
    end do
  end function get_face_coords
 

  !!------------!!
  ! Calculations !
  !--------------!--------------------------------------------------------------
  function face_normal(this, face_idx) result(return_value)
    class (mesh_type)     , intent(in)  :: this
    integer               , intent(in)  :: face_idx
    integer                             :: i
    real(wp), dimension(3)              :: return_value
    real(wp), dimension(3, 3)           :: face_coords
    real(wp), dimension(2, 3)           :: face_vectors
    face_coords = this%get_face_coords(face_idx)
    do i = 1, 3
       face_vectors(1, i) = face_coords(2, i) - face_coords(1, i)
       face_vectors(2, i) = face_coords(3, i) - face_coords(1, i)
    end do
    return_value = cross_prod_3D(face_vectors(1, :), face_vectors(2, :))
  end function face_normal
  

  function face_unit_normal(this, face_idx) result(return_value)
    class (mesh_type)     , intent(in)  :: this
    integer               , intent(in)  :: face_idx
    real(wp), dimension(3)              :: return_value
    real(wp)              , external    :: dnrm2
    real(wp)                            :: scale_factor
    return_value = this%face_normal(face_idx)
    scale_factor = 1/norm2(return_value)
    return_value = scale_factor*return_value
  end function face_unit_normal


  function face_area(this, face_idx) result(return_value)
    ! From BLAS level 1:
    ! dnrm2: Euclidean norm (double)
    class (mesh_type), intent(in)  :: this
    integer          , intent(in)  :: face_idx
    real(wp)                       :: return_value
    real(wp)         , external    :: dnrm2
    return_value = 0.5_wp*norm2(this%face_normal(face_idx))
  end function face_area
  
  
  function face_centroid(this, face_idx) result(return_value)
    class (mesh_type)     , intent(in)  :: this
    integer               , intent(in)  :: face_idx
    integer                             :: i
    real(wp), dimension(3)              :: return_value
    real(wp), dimension(3, 3)           :: face_coords
    face_coords = this%get_face_coords(face_idx)
    ! The centroid has coordinates equal to the mean x, y, z coordinates
    ! of the face's node coordinates
    do i = 1, 3
       return_value(i) = sum(face_coords(:, i))/3._wp
    end do
  end function face_centroid
  

  function surface_area(this) result(return_value)
    class (mesh_type), intent(in)  :: this
    real(wp)                       :: return_value
    integer                        :: i
    return_value = 0._wp
    do i = 1, this%num_faces
       return_value = return_value + this%face_area(i)
    end do
  end function surface_area
  

  function edge_length(this, edge_idx) result(return_value)
    class (mesh_type), intent(in) :: this
    integer          , intent(in) :: edge_idx
    real(wp)                      :: return_value
    ! Variables for internal use -----------------------------------------------
    real(wp)         , external   :: dnrm2
    real(wp), dimension(2, this%spatial_dim) :: edge_coords
    real(wp), dimension(this%spatial_dim) :: displacement_vector
    edge_coords = this%get_edge_coords(edge_idx)
    displacement_vector = edge_coords(2, :) - edge_coords(1, :)
    return_value = dnrm2(this%spatial_dim, displacement_vector, 1) 
  end function edge_length
  
       
  function volume(this) result(return_value)
    ! This function is not optimised because it calls on face_normal() two times
    ! and get_face_coords three times
    class (mesh_type), intent(in)  :: this
    real(wp)                       :: return_value
    real(wp)                       :: area
    integer                        :: i
    real(wp), dimension(3)         :: unit_normal, centroid
    return_value = 0._wp
    if (this%closed_surface) then
       do i = 1, this%num_faces
          unit_normal = this%face_unit_normal(i)
          area = this%face_area(i)
          centroid = this%face_centroid(i)
          return_value = return_value + unit_normal(1)*area*centroid(1)
       end do
    else
       print *, 'Error: mesh_mod.f90: volume:'
       print *, '       Cannot calculate volume of open surface meshes.'
       stop 1
    end if
  end function volume
  

  !!-----------------!!
  ! Printing routines !
  !-------------------!---------------------------------------------------------
  subroutine print(this)
    class (mesh_type), intent(in) :: this
    integer                       :: i

    print *, 'Vertices:'
    do i = 1, this%num_vertices
       print *, this%vertices(i)%node_idx
    end do
    print *, 'Edges:'
    do i = 1, this%num_edges
       print *, this%edges(i)%vertices
    end do
    print *, 'Faces:'
    do i = 1, this%num_faces
       print *, this%faces(i)%edges
    end do
  end subroutine print

  
  !!----------------------------------------------------------------------------

  subroutine scale_nodes(this, scale_factor)
    class (mesh_type), intent(inout) :: this
    real(wp)         , intent(in)    :: scale_factor
    ! Variables for internal use -----------------------------------------------
    integer :: i
    integer :: j
    do i = 1, this%num_nodes
       do j = 1, this%spatial_dim
          this%nodes(i)%coords(j) = this%nodes(i)%coords(j)*scale_factor
       end do
    end do
  end subroutine scale_nodes

  !!----------------------------------------------------------------------------
  
  function solid_angle_spanned_by_face(this, face_idx, obs_pnt) result(res)
    class (mesh_type), intent(in)      :: this
    integer          , intent(in)         :: face_idx
    real(wp), dimension(this%spatial_dim) :: obs_pnt
    real(wp)                              :: res
    ! Variables for internal use -----------------------------------------------
    real(wp), dimension(this%face_order, this%spatial_dim) :: face_coords
    real(wp), dimension(this%spatial_dim) :: a
    real(wp), dimension(this%spatial_dim) :: b
    real(wp), dimension(this%spatial_dim) :: c

    ! Currently only implemented for 3D surface triangulations
    if (this%spatial_dim /= 3 .or. this%face_order /= 3) then
       print *, 'Error: mesh_mod.f90: solid_angle_spanned_by_triangle'
       print *, '       Solid angle calculation only implemented for 3D', &
            'surface triangulations'
    end if

    face_coords = this%get_face_coords(face_idx)
    a = face_coords(1, :) - obs_pnt
    b = face_coords(2, :) - obs_pnt
    c = face_coords(3, :) - obs_pnt
    a = a/norm2(a)
    b = b/norm2(b)
    c = c/norm2(c)
    ! Observation point on face
    ! Solid angle calculated by area of the spherical triangle abc on the unit
    ! sphere
    res = abs(dot_product(a, cross_prod_3D(b, c))) &
         /(1._wp + dot_product(a, b) + dot_product(b, c) + dot_product(a, c))
    ! Set sign of solid angle depending on the orientation of the face
    res = 2*atan(res)
    res = sign(res, dot_product(this%face_normal(face_idx), a))
  end function solid_angle_spanned_by_face

  !!----------------------------------------------------------------------------

  function solid_angle_spanned_by_mesh(this, obs_pnt) result(res)
    class (mesh_type), intent(in)         :: this
    real(wp), dimension(this%spatial_dim) :: obs_pnt
    real(wp)                              :: res
    ! Variables for internal use -----------------------------------------------
    integer :: n
    
    res = 0._wp
    do n = 1, this%num_faces
       res = res + this%solid_angle_spanned_by_face(n, obs_pnt)
    end do
  end function solid_angle_spanned_by_mesh
  
  !!----------------------------------------------------------------------------
  
  function is_obs_pnt_inside_mesh(this, obs_pnt) result(res)
    class (mesh_type), intent(in)         :: this
    real(wp), dimension(this%spatial_dim) :: obs_pnt
    logical                               :: res
    ! Variables for internal use -----------------------------------------------
    real(wp) :: solid_angle

    solid_angle = this%solid_angle_spanned_by_mesh(obs_pnt)
    if (is_close(solid_angle, 4._wp*PI, rtol=1.E-2_wp)) then
       res = .true.
    else if (is_close(solid_angle, ZERO)) then 
       res = .false.
    else
       print *, 'Warning: mesh_mod: is_obs_pnt_inside_mesh:'
       print *, '       Observation point neither inside or outside mesh.', &
            ' Check mesh representation.'
       print *, '       Solid angle value:', solid_angle
       print *, '       Observation point:', obs_pnt
       print *, '       Distance from Ori:', norm2(obs_pnt)
       res = .true.
    end if
  end function is_obs_pnt_inside_mesh
  
  !!---------------------------!!
  ! Private internal procedures !
  !-----------------------------!----------------------------------------------
  subroutine create_member_types_linear(this, nodes, elements)
    class (mesh_type)        , intent(inout)   :: this
    real(wp), dimension(:, :), intent(in)      :: nodes
    integer , dimension(:, :), intent(in)      :: elements
    ! Variables for internal use ----------------------------------------------
    type (edge_type)                           :: edge
    integer                                    :: i
    integer                                    :: j
    integer                                    :: k
    integer                                    :: idx
    integer                                    :: edge_counter
    integer, dimension(this%face_order) :: edge_idxs
    logical                                    :: edge_exists
    
    ! Create vertices and nodes
    do i = 1, this%num_nodes
       this%nodes(i) = node_type(nodes(i, :))
       this%vertices(i) = vertex_type(i)
    end do

    allocate(edge%node_idx(0))
    ! Create faces and edges
    edge_counter = 0
    do i = 1, this%num_faces
       do j = 1, this%face_order
          if (j == this%face_order) then
             idx = 1
          else
             idx = j + 1
          end if
          edge%vertices = [ elements(i, j), elements(i, idx) ]
          edge_exists = .false.
          do k = 1, edge_counter
             if (edge == this%edges(k)) then
                edge_exists = .true.
                exit
             end if
          end do
          if (edge_exists) then
             edge_idxs(j) = k
          else
             edge_counter = edge_counter + 1
             allocate(this%edges(edge_counter)%node_idx(0))
             this%edges(edge_counter)%vertices = [ elements(i, j), &
                  elements(i, idx) ] 
             edge_idxs(j) = edge_counter
          end if
       end do
       allocate(this%faces(i)%edges(this%face_order))
       this%faces(i)%edges = edge_idxs
    end do
    
  end subroutine create_member_types_linear
 
  !!----------------------------------------------------------------------------
!!$ 
!!$  subroutine create_member_types_quadratic(this, nodes, elements)
!!$    class (mesh_type), intent(inout)      :: this
!!$    real(wp), dimension(:, :), intent(in) :: nodes
!!$    integer , dimension(:, :), intent(in) :: elements
!!$    ! Not implemented
!!$  end subroutine create_member_types_quadratic
!!$
!!$  !!----------------------------------------------------------------------------
!!$
!!$  subroutine create_member_types_cubic(this, nodes, elements)
!!$    class (mesh_type), intent(inout)      :: this
!!$    real(wp), dimension(:, :), intent(in) :: nodes
!!$    integer , dimension(:, :), intent(in) :: elements
!!$    ! Not implemented
!!$  end subroutine create_member_types_cubic
!!$
!!$  !!----------------------------------------------------------------------------
!!$
!!$  subroutine flip_face_orientation(this)
!!$    class (mesh_type), intent(inout) :: this
!!$    ! Not implemented
!!$  end subroutine flip_face_orientation
!!$  
  
  !!==================!!
  ! Private procedures !
  !====================!========================================================
  function eval_Euler_characteristic_CS(num_faces, num_edges, num_vertices, &
       num_handles) result (return_value)
    integer, optional, intent(in) :: num_faces
    integer, optional, intent(in) :: num_edges
    integer, optional, intent(in) :: num_vertices
    integer, optional, intent(in) :: num_handles
    integer                       :: return_value

    if (present(num_faces) .and. present(num_edges) .and. &
         present(num_vertices)) then
       return_value = (num_edges - num_faces - num_vertices + 2)/2 
    else if (present(num_handles) .and. present(num_edges) .and. &
         present(num_vertices)) then
       return_value = 2*(1 - num_handles) + num_edges - num_vertices
    else if (present(num_handles) .and. present(num_faces) .and. &
         present(num_vertices)) then
       return_value = -2*(1 - num_handles) + num_faces + num_vertices
    else if (present(num_handles) .and. present(num_faces) .and. &
         present(num_edges)) then
       return_value = 2*(1 - num_handles) - num_faces + num_edges
    else
       print *, 'Error: eval_Euler_characteristic_CS: invalid options'
       stop 1
    end if
  end function eval_Euler_characteristic_CS

  
  subroutine check_input_triangulated_surface(&
       closed_surface, &
       edge_order,     &
       num_handles_in,    &
       num_apertures_in,  &
       num_boundary_edges_in)
    logical          , intent(in) :: closed_surface
    integer          , intent(in) :: edge_order
    integer, optional, intent(in) :: num_handles_in
    integer, optional, intent(in) :: num_apertures_in
    integer, optional, intent(in) :: num_boundary_edges_in

    if (closed_surface) then
       if (edge_order > 0) then
          if (.not. present(num_handles_in)) then
             print *, 'Error: mesh_mod.f90: initialise: '
             print *, '       Missing optional parameter: num_handles. '
          end if
       end if
    else
       if (edge_order > 0) then
          if  (.not. present(num_handles_in) .or. &
               .not. present(num_apertures_in) .or. &
               .not. present(num_boundary_edges_in)) then
             print *, 'Error: mesh_mod.f90: initialise: '
             print *, '       Missing one or more of the following optional ', &
                  'parameters: '
             print *, '         num_handles, num_apertures, num_boundary_edges'
          end if
       else
          if (.not. present(num_boundary_edges_in)) then
             print *, 'Error: mesh_mod.f90: initialise: '
             print *, '       Missing optional parameter: num_bounadry_edges. '
          end if
       end if
    end if
  end subroutine check_input_triangulated_surface
                
 !------------------------------------------------------------------------------

  subroutine eval_topology_on_triangulated_surface(&
       num_edges,      &
       num_vertices,   &
       num_handles,    &
       num_apertures,  &
       num_boundary_edges, &
       num_faces,      &
       num_nodes,      &
       closed_surface, &
       edge_order,     &
       num_handles_in,    &
       num_apertures_in,  &
       num_boundary_edges_in)

    integer, intent(inout)        :: num_edges
    integer, intent(inout)        :: num_vertices
    integer, intent(inout)        :: num_handles
    integer, intent(inout)        :: num_apertures
    integer, intent(inout)        :: num_boundary_edges
    integer, intent(in)           :: num_faces
    integer, intent(in)           :: num_nodes
    logical, intent(in)           :: closed_surface
    integer, intent(in)           :: edge_order
    integer, optional, intent(in) :: num_handles_in
    integer, optional, intent(in) :: num_apertures_in
    integer, optional, intent(in) :: num_boundary_edges_in

    select case (closed_surface)
    case (.true.)
       if (edge_order == 0) then
          num_vertices = num_nodes
       else
          num_vertices = eval_Euler_characteristic_CS(&
               num_faces=num_faces, &
               num_edges=num_edges, &
               num_handles=num_handles_in)
       end if
       num_edges = 3*num_faces/2
       if (.not. present(num_handles_in)) then
          num_handles = eval_Euler_characteristic_CS(&
               num_faces=num_faces, &
               num_edges=num_edges, &
               num_vertices=num_vertices)
       else
          num_handles = num_edges - num_Faces - num_vertices + 2
          if (num_handles /= 2*num_handles_in) then
             print *, 'Warning: mesh_mod.f90: eval_topology_on_triangulated_', &
                  'surface:'
             print *, '         Evaluated topology parameters do not follow '
             print *, '         Euler characteristic. num_handles differ by:', &
                  (num_handles_in - 2*num_handles)/2 ! Might cause rounding err.
          end if
          num_handles = num_handles_in
       end if
       num_apertures = 0
       num_boundary_edges = 0
    case (.false.)
       if (edge_order == 0) then
          num_vertices = num_nodes
       else
          num_vertices = (num_faces - 2*num_apertures_in - 4*(num_handles_in-1)&
               + num_boundary_edges_in)/2
       end if
       num_edges = (3*num_faces + num_boundary_edges_in)/2
       if (present(num_apertures_in) .and. &
            .not. present(num_handles_in)) then
          num_apertures = num_apertures_in
          num_handles = (num_edges - 3*num_vertices - 3*num_apertures_in &
               + 6 + num_boundary_edges_in)/6
       else if (present(num_handles_in) .and. &
            .not. present(num_apertures_in)) then
          num_handles = num_handles_in
          num_apertures = (num_faces - 2*num_vertices - 4*(num_handles_in - 1) &
               + num_boundary_edges_in)/2
       else if (present(num_handles_in) .and. &
            present(num_apertures_in)) then
          num_handles = (num_faces - 2*num_vertices - 2*num_apertures + &
               num_boundary_edges_in + 4)/4
          num_apertures = num_apertures_in
          if (num_handles /= num_handles_in) then
             print *, 'Warning: mesh_mod.f90: eval_topology_on_triangulated_', &
                  'surface:'
             print *, '         Evaluated topology parameters do not follow '
             print *, '         Euler characteristic. num_handles differ by:', &
                  num_handles_in - num_handles
             num_handles = num_handles_in
          end if
       else
          print *, 'Warning: mesh_mod.f90: eval_topology_on_triangulated_', &
               'surface:'
          print *, '         mesh_type attributes not set:'
          print *, '           num_handles, num_apertures'
       end if
       num_boundary_edges = num_boundary_edges_in
       
    end select
  end subroutine eval_topology_on_triangulated_surface
  
       
  
  !!====================!!
  ! Overloaded operators !
  !======================!======================================================
  function is_edges_equal(edge_a, edge_b) result(return_value)
    type(edge_type), intent(in) :: edge_a
    type(edge_type), intent(in) :: edge_b
    logical                     :: return_value
    if ((edge_a%vertices(1) == edge_b%vertices(1) .and. &
         edge_a%vertices(2) == edge_b%vertices(2)) .or. (&
         edge_a%vertices(1) == edge_b%vertices(2) .and. &
         edge_a%vertices(2) == edge_b%vertices(1))) then
       return_value = .true.
    else
       return_value = .false.
    end if
  end function is_edges_equal
  
  !=============================================================================
end Module mesh_mod
