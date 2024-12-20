module test_utilities
!!==============================================================================
! This module defines som useful procedures for use by the testing programs.
!
!
! Last edited: October 21th 2020.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use mesh_mod         , only: mesh_type
  use RWG_basis_mod    , only: RWG_basis_type
  implicit none


  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: print_status_start
  public :: print_status_done
  public :: check_mesh_calculations
  public :: check_RWG_basis_initialisation

   
  !!=====================!!
  ! Procedure definitions !
  !=======================!=====================================================
 contains
   
  subroutine print_status_start(num_tests, test_nr, mod, proc)
    integer, intent(in)          :: num_tests
    integer, intent(in)          :: test_nr
    character(len=*), intent(in) :: mod
    character(len=*), intent(in) :: proc
    write (*, fmt="(a,a)", advance="no") 'Testing: ', mod
    write (*, fmt="(i3,a,i0,a)", advance="no") test_nr, '/', num_tests, ' >>> '
    write (*, fmt="(a,a)", advance="no") proc, ' ...'
  end subroutine print_status_start

  !!----------------------------------------------------------------------------

  subroutine print_status_done(test_nr)
    integer, intent(inout) :: test_nr
    write (*,*) 'done'
    test_nr = test_nr + 1
  end subroutine print_status_done

  !!----------------------------------------------------------------------------

  subroutine print_error_message(module_name, test_name, msg, degree)
    character(len=*) , intent(in) :: module_name
    character(len=*) , intent(in) :: test_name
    character(len=*) , intent(in) :: msg
    integer, optional, intent(in) :: degree
    print *, ''
    print *, 'Error: ', module_name, ': ', test_name, ':'
    print *, '       ', msg
    if (present(degree)) then
       stop degree
    else
       stop 2
    end if
  end subroutine print_error_message
  
  !!----------------------------------------------------------------------------
  
  subroutine check_mesh_calculations(&
       mesh                      , &
       correct_num_faces         , &
       correct_num_edges         , &
       correct_num_vertices      , &
       correct_num_handles       , &
       correct_num_apertures     , &
       correct_num_boundary_edges, &
       correct_face_area         , &
       correct_surface_area      , &
       correct_volume)
    type (mesh_type)  , intent(in) :: mesh
    integer           , intent(in) :: correct_num_faces
    integer           , intent(in) :: correct_num_edges
    integer           , intent(in) :: correct_num_vertices
    integer           , intent(in) :: correct_num_handles
    integer           , intent(in) :: correct_num_apertures
    integer           , intent(in) :: correct_num_boundary_edges
    real(wp)          , intent(in) :: correct_face_area
    real(wp)          , intent(in) :: correct_surface_area
    real(wp), optional, intent(in) :: correct_volume
    ! Variables for internal use -----------------------------------------------
    real(wp)                       :: area
    real(wp)                       :: surface_area
    real(wp)                       :: volume
    integer                        :: i

    if (mesh%num_faces /= correct_num_faces) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of faces: ', mesh%num_faces, &
            'Correct result: ', correct_num_faces
       stop 1
    end if
    if (mesh%num_edges /= correct_num_edges) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of edges: ', mesh%num_edges, &
            'Correct result: ', correct_num_edges
       stop 1
    end if
    if (mesh%num_vertices /= correct_num_vertices) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of vertices: ', mesh%num_vertices, &
            'Correct result: ', correct_num_vertices
       stop 1
    end if
    if (mesh%num_handles /= correct_num_handles) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of handles: ', mesh%num_handles, &
            'Correct result: ', correct_num_handles
       stop 1
    end if
    if (mesh%num_apertures /= correct_num_apertures) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of apertures: ', mesh%num_apertures, &
            'Correct result: ', correct_num_apertures
       stop 1
    end if
    if (mesh%num_boundary_edges /= correct_num_boundary_edges) then
       print *, ''
       print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
            ' failed..'
       print *, '  Estimated number of boundary_edges: ', &
            mesh%num_boundary_edges, &
            'Correct result: ', correct_num_boundary_edges
       stop 1
    end if

    do i = 1, mesh%num_faces
       area = mesh%face_area(i)
       if (area /= correct_face_area) then
          print *, ''
          print *, 'Error: test_initialise_tetrahedron in mesh_test.f90', &
               ' failed..'
          print *, '  Estimated face area: ', area, 'Correct result: ', &
               correct_face_area
          stop 1
       end if
    end do   

    surface_area = mesh%surface_area()
    if (surface_area /= correct_surface_area) then
       print *, ''
       print *, 'Error: test_surface_area in mesh_test.f90 failed..'
       print *, '  Estimated surface area: ', surface_area, 'Correct result: ', &
            correct_surface_area
       stop 1
    end if
    if (present(correct_volume)) then
       volume = mesh%volume()
       if (volume /= correct_volume) then
          print *, ''
          print *, 'Error: test_volume in mesh_test.f90 failed..'
         print *, '  Estimated volume: ', volume, 'Correct result: ', &
               correct_volume
          stop 1
       end if
    end if

  end subroutine check_mesh_calculations
     
  !!----------------------------------------------------------------------------

  subroutine check_RWG_basis_initialisation(&
       RWG_basis          , &
       correct_basis_edges, &
       correct_adjacencies, &
       correct_basis_edge_length)
    type (RWG_basis_type)    , intent(in) :: RWG_basis
    integer , dimension(:)   , intent(in) :: correct_basis_edges
    integer , dimension(:, :), intent(in) :: correct_adjacencies
    real(wp), dimension(:)   , intent(in) :: correct_basis_edge_length
    ! Variables for internal use -----------------------------------------------
    integer                               :: correct_num_bases
    integer                               :: i
    integer                               :: j

    correct_num_bases = size(correct_basis_edges)
    if (correct_num_bases /= RWG_basis%num_bases) then
       print *, 'Error: test_initialise in RWG_basis_test.f90 failed..'
       print *, 'Num bases:', RWG_basis%num_bases, 'Correct', correct_num_bases
       stop 2
    end if
    do i = 1, correct_num_bases
       if (RWG_basis%basis_edges(i) /= correct_basis_edges(i)) then
          print *, 'Error: test_initialise in RWG_basis_test.f90 failed..'
          print *, 'i: ', i, 'Edge_idx:', RWG_basis%basis_edges(i), &
               'Correct value:', correct_basis_edges(i)
          stop 2
       end if

       if (RWG_basis%basis_edge_length(i) /= correct_basis_edge_length(i)) then
          print *, 'Error: test_initialise in RWG_basis_test.f90 failed..'
          print *, 'i: ', i,  'edge length:', &
               RWG_basis%basis_edge_length(i), &
               'Correct value:', correct_basis_edge_length(i)
          stop 2
       end if

       do j = 1, 2
          if (RWG_basis%adjacent_faces(i, j) /= correct_adjacencies(i, j)) then
             print *, 'Error: test_initialise in RWG_basis_test.f90 failed..'
             print *, 'i: ', i, 'j:', j, 'face_idx:', &
                  RWG_basis%adjacent_faces(i, j), &
                  'Correct value:', correct_adjacencies(i, j)
             stop 2
          end if
       end do
    end do
  end subroutine check_RWG_basis_initialisation

!!------------------------------------------------------------------------------

  function linspace( &
       lower_limit , &
       upper_limit , &
       num_elements) &
       result(res)
    real(wp), intent(in) :: lower_limit
    real(wp), intent(in) :: upper_limit
    integer , intent(in) :: num_elements
    ! Result variable to be returned -------------------------------------------
    real(wp), dimension(num_elements) :: res
    ! Variables for internal use -----------------------------------------------
    real(wp) :: uniform_spacing
    integer  :: i

    uniform_spacing = (upper_limit - lower_limit)/real(num_elements - 1)
    do i = 1, num_elements
       res(i) = lower_limit + (i - 1)*uniform_spacing
    end do
  end function linspace
  
!!------------------------------------------------------------------------------

  subroutine meshgrid(&
       XX      , &
       YY      , &
       x_coords, &
       y_coords)
    real(wp), dimension(:, :),intent(inout) :: XX(:, :)
    real(wp), dimension(:, :), intent(inout) :: YY(:, :)
    real(wp), dimension(:)                , intent(in)    :: x_coords
    real(wp), dimension(:)                , intent(in)    :: y_coords
    ! Variables for internal use -----------------------------------------------
    integer :: num_x_coords
    integer :: num_y_coords
    integer :: i
    
    num_x_coords = size(x_coords)
    num_y_coords = size(y_coords)
!!$    allocate(XX(num_y_coords, num_x_coords))
!!$    allocate(YY(num_y_coords, num_y_coords))
    XX(:, :) = reshape(spread(x_coords, 1, num_y_coords), [num_y_coords, num_x_coords])
    YY(:, :) = reshape(spread(y_coords, 2, num_x_coords), [num_y_coords, num_x_coords])
  end subroutine meshgrid

!!------------------------------------------------------------------------------
end module test_utilities

