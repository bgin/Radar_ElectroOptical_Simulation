
module GMSH_Reader



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'GMSH_Reader'
 !          
 !          Purpose:
 !                      J. Burkardt 'gmsh_io' implementation wrapped in
 !                      a module.
 !          History:
 !                        
 !                        Date: 02-01-2022
 !                        Time: 09:57 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          Contact:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp,dp
     implicit none
     public

     !!=============================================!
     !      GMSH Mesh derived type
     !!=============================================!

     type, public :: GMSH_Mesh
           public
           character(len=128) :: input_fname
           integer(kind=i4)   :: node_dim !NODE_DIM, the spatial dimension.
           integer(kind=i4)   :: node_num !NODE_NUM, the number of nodes.
           integer(kind=i4)   :: element_order ! ELEMENT_ORDER, the order of the elements.
           integer(kind=i4)   :: element_num   ! ELEMENT_NUM, the number of elements.
#if defined(__INTEL_COMPILER) || defined(__ICC)
           real(kind=dp), allocatable, dimension(:,:) :: node_x ! NODE_X(NODE_DIM,NODE_NUM), the node coordinates.
           integer(kind=i4), allocatable, dimension(:,:) :: element_node !integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM) the nodes that make up each element.
           !DIR$ ATTRIBUTES ALIGN : 64 :: node_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: element_node
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:,:) :: node_x           !GCC$ ATTRIBUTES aligned(64) :: node_x
           integer(kind=i4), allocatable, dimension(:,:) :: element_node  !GCC$ ATTRIBUTES aligned(64) :: element_node
#endif
      end type GMSH_Mesh


    contains

      subroutine ch_cap ( ch )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, the character to capitalize.
!
  !implicit none

  character ch
  integer(kind=i4) :: itemp

  itemp = iachar ( ch )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    ch = achar ( itemp - 32 )
  end if

  return
end subroutine ch_cap

function ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  !implicit none

  logical ch_eqi
  character c1
  character c1_cap
  character c2
  character c2_cap

  c1_cap = c1
  c2_cap = c2

  call ch_cap ( c1_cap )
  call ch_cap ( c2_cap )

  if ( c1_cap == c2_cap ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

  return
end function ch_eqi

subroutine ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer DIGIT, the corresponding integer value.
!    If C was 'illegal', then DIGIT is -1.
!
  !implicit none

  character c
  integer(kind=i4) :: digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end subroutine ch_to_digit

subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT, the free unit number.
!
  !implicit none

  integer(kind=i4) :: i
  integer(kind=i4) :: ios
  integer(kind=i4) :: iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end subroutine get_unit

subroutine gmsh_data_read ( gmsh_filename, node_dim, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_DATA_READ reads data from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Input, integer NODE_DIM, the spatial dimension.
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Output, real ( kind = rk ) NODE_X(NODE_DIM,NODE_NUM), the node coordinates.
!
!    Input, integer ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ELEMENT_NUM, the number of elements.
!
!    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order
  integer(kind=i4) :: node_dim
  integer(kind=i4) :: node_num

  integer(kind=i4) :: element_node(element_order,element_num)
  character ( len = * ) gmsh_filename
  integer(kind=i4) :: i
  integer(kind=i4) :: i4_dummy
  integer(kind=i4) :: ierror
  integer(kind=i4) :: indx
  integer(kind=i4) :: input
  integer(kind=i4) :: input_stat
  integer(kind=i4) :: j
  integer(kind=i4) :: k
  integer(kind=i4) :: length
  integer(kind=i4) :: level
  real ( kind = dp ) node_x(node_dim,node_num)
  real ( kind = dp ), parameter :: r8_big = 1.0+30_dp
  logical s_begin
  character ( len = 255 ) text
  real ( kind = dp ) x

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking node coordinates.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        j = j + 1
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        do i = 1, node_dim
          call s_to_r8 ( text, x, ierror, length )
          text = text(length+1:)
          node_x(i,j) = x
        end do      
      end if
    end if

  end do
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking element connectivity.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        j = j + 1
        k = 0
        do k = 1, 5
          call s_to_i4 ( text, i4_dummy, ierror, length )
          text = text(length+1:)
        end do
        do i = 1, element_order
          call s_to_i4 ( text, k, ierror, length )
          text = text(length+1:)
          element_node(i,j) = k
        end do
      end if
    end if

  end do

  close ( unit = input )

  return
end subroutine gmsh_data_read

subroutine gmsh_size_read ( gmsh_filename, node_num, node_dim, element_num, &
  element_order )

!*****************************************************************************80
!
!! GMSH_SIZE_READ reads sizes from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Output, integer NODE_NUM, the number of nodes.
!
!    Output, integer NODE_DIM, the spatial dimension.
!
!    Output, integer ELEMENT_NUM, the number of elements.
!
!    Output, integer ELEMENT_ORDER, the order of the elements.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order
  character ( len = * ) gmsh_filename
  integer(kind=i4) :: ierror
  integer(kind=i4) :: indx
  integer(kind=i4) :: input
  integer(kind=i4) :: input_stat
  integer(kind=i4) :: k
  integer(kind=i4) :: length
  integer(kind=i4) :: level
  integer(kind=i4) :: node_dim
  integer(kind=i4) :: node_num
  real ( kind = dp ), parameter :: r8_big = 1.0+30_dp
  logical s_begin
  character ( len = 255 ) text
  real ( kind = dp ) x
  real ( kind = dp ) x_max
  real ( kind = dp ) x_min
  real ( kind = dp ) y
  real ( kind = dp ) y_max
  real ( kind = dp ) y_min
  real ( kind = dp ) z
  real ( kind = dp ) z_max
  real ( kind = dp ) z_min

  node_num = 0
  node_dim = 0

  x_max = - r8_big
  x_min = + r8_big
  y_max = - r8_big
  y_min = + r8_big
  z_max = - r8_big
  z_min = + r8_big

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_SIZE_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, node_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        call s_to_r8 ( text, x, ierror, length )
        x_min = min ( x_min, x )
        x_max = max ( x_max, x )
        text = text(length+1:)
!
!  Need to check that we actually were able to read an R8 here.
!
        call s_to_r8 ( text, y, ierror, length )
        y_min = min ( y_min, y )
        y_max = max ( y_max, y )
        text = text(length+1:)
        call s_to_r8 ( text, z, ierror, length )
        text = text(length+1:)
        z_min = min ( z_min, z )
        z_max = max ( z_max, z )
      end if
    end if

  end do
!
!  Make a very simple guess as to the dimensionality of the data.
!
  node_dim = 3
  if ( z_max == z_min ) then
    node_dim = 2
    if ( y_max == y_min ) then
      node_dim = 1
    end if
  end if
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, element_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        k = 0
        do 
          call s_to_i4 ( text, indx, ierror, length )
          text = text(length+1:)
          if ( ierror /= 0 ) then
            exit
          end if
          k = k + 1
        end do
        element_order = k - 5
        exit
      end if
    end if

  end do

  close ( unit = input )

  return
end subroutine gmsh_size_read

subroutine gmsh_mesh1d_write ( gmsh_filename, m, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_MESH1D_WRITE writes 1D mesh data as a Gmsh file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Christophe Geuzaine, Jean-Francois Remacle,
!    Gmsh: a three-dimensional finite element mesh generator with
!    built-in pre- and post-processing facilities,
!    International Journal for Numerical Methods in Engineering,
!    Volume 79, Number 11, pages 1309-1331, 2009.
!
!  Parameters:
!
!    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
!
!    Input, integer M, the spatial dimension.
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( kind = rk ) NODE_X(M,NODE_NUM), the node coordinates.
!
!    Input, integer ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ELEMENT_NUM, the number of elements.
!
!    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order
  integer(kind=i4) :: m
  integer(kind=i4) :: node_num

  integer(kind=i4) :: element
  integer(kind=i4) :: element_node(element_order,element_num)
  integer(kind=i4) :: element_type
  character * ( * ) gmsh_filename
  integer(kind=i4) :: gmsh_unit
  integer(kind=i4) :: node
  real ( kind = dp ) node_x(m,node_num)
  integer(kind=i4) :: tag_num
  integer(kind=i4) :: tag1
!
!  Enforce 1-based indexing of nodes.
!
  call mesh_base_one ( node_num, element_order, element_num, element_node )
!
!  Open the file.
!
  call get_unit ( gmsh_unit )

  open ( unit = gmsh_unit, file = gmsh_filename, status = 'replace' )
!
!  Write the data.
!
  write ( gmsh_unit, '(a)' ) '$MeshFormat'
  write ( gmsh_unit, '(a)' ) '2.2 0 8'
  write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

  write ( gmsh_unit, '(a)' ) '$Nodes'
  write ( gmsh_unit, '(i6)' ) node_num
  do node = 1, node_num
    write ( gmsh_unit, '(i6,2x,g14.6,a)' ) &
      node, node_x(1:m,node), '  0.0  0.0'
  end do
  write ( gmsh_unit, '(a)' ) '$EndNodes'

  element_type = 1

  tag_num = 2
  tag1 = 0
  write ( gmsh_unit, '(a)' ) '$Elements'
  write ( gmsh_unit, '(i6)' ) element_num
  do element = 1, element_num
    write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,2(2x,i6))' ) &
      element, element_type, tag_num, tag1, element, &
      element_node(1:element_order,element)
  end do
  write ( gmsh_unit, '(a)' ) '$EndElements'

  close ( unit = gmsh_unit )

  return
end subroutine gmsh_mesh1d_write

subroutine gmsh_mesh2d_element_data_example ( element_num, element_order, &
  element_node )

!*****************************************************************************80
!
!! GMSH_MESH2D_ELEMENT_DATA_EXAMPLE returns element data for the 2D example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ELEMENT_NUM, the number of elements.
!
!    Input, integer ELEMENT_ORDER, the order of the elements.
!
!    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the indices of the nodes that make up each element.
!
  !implicit none

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order

  integer(kind=i4) :: element_node(element_order,element_num)
  integer(kind=i4), dimension ( 3, 24 ) :: element_node_save = &
  reshape ( (/ &
    1,  2,  6, &
    7,  6,  2, &
    2,  3,  7, &
   8,  7,  3, &
    3,  4,  8, &
    9,  8,  4, &
    4,  5,  9, &
   10,  9,  5, &
    6,  7, 11, &
   12, 11,  7, &
    7,  8, 12, &
   13, 12,  8, &
    8,  9, 13, &
   14, 13,  9, &
    9, 10, 14, &
   15, 14, 10, &
   11, 12, 16, &
   17, 16, 12, &
   12, 13, 17, &
   18, 17, 13, &
   16, 17, 19, &
   20, 19, 17, &
   17, 18, 20, &
   21, 20, 18 /), (/ 3, 24 /) )

  call i4mat_copy ( element_order, element_num, element_node_save, &
    element_node )

  return
end subroutine gmsh_mesh2d_element_data_example

subroutine gmsh_mesh2d_element_size_example ( element_num, element_order )

!*****************************************************************************80
!
!! GMSH_MESH2D_ELEMENT_SIZE_EXAMPLE returns element sizes for the 2D example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ELEMENT_NUM, the number of elements.
!
!    Output, integer ELEMENT_ORDER, the order of the elements.
!
  !implicit none

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order

  element_num = 24
  element_order = 3

  return
end subroutine gmsh_mesh2d_element_size_example

subroutine gmsh_mesh2d_node_data_example ( node_num, node_dim, node_x )

!*****************************************************************************80
!
!! GMSH_MESH2D_NODE_DATA_EXAMPLE returns node data for the 2D example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, integer NODE_DIM, the spatial dimension.
!
!    Output, real ( kind = rk ) NODE_X(NODE_DIM,NODE_NUM), the nodal 
!    coordinates.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: node_dim
  integer(kind=i4) :: node_num

  real ( kind = dp ) node_x(node_dim,node_num)
  real ( kind = dp ), dimension ( 2, 21 ) :: node_x_save = reshape ( (/ &
    0.0, 0.0, &
    1.0, 0.0, &
    2.0, 0.0, &
    3.0, 0.0, &
    4.0, 0.0, &
    0.0, 1.0, &
    1.0, 1.0, &
    2.0, 1.0, &
    3.0, 1.0, &
    4.0, 1.0, &
    0.0, 2.0, &
    1.0, 2.0, &
    2.0, 2.0, &
    3.0, 2.0, &
    4.0, 2.0, &
    0.0, 3.0, &
    1.0, 3.0, &
    2.0, 3.0, &
    0.0, 4.0, &
    1.0, 4.0, &
    2.0, 4.0 /), (/ 2, 21 /) )

  call r8mat_copy ( node_dim, node_num, node_x_save, node_x )

  return
end subroutine gmsh_mesh2d_node_data_example

subroutine gmsh_mesh2d_node_size_example ( node_num, node_dim )

!*****************************************************************************80
!
!! GMSH_MESH2D_NODE_SIZE_EXAMPLE returns node sizes for the 2D example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer NODE_NUM, the number of nodes.
!
!    Output, integer NODE_DIM, the spatial dimension.
!
  !implicit none

  integer(kind=i4) :: node_dim
  integer(kind=i4) :: node_num

  node_num = 21
  node_dim = 2

  return
end subroutine gmsh_mesh2d_node_size_example

subroutine gmsh_mesh2d_write ( gmsh_filename, m, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_MESH2D_WRITE writes 2D mesh data as a Gmsh file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Christophe Geuzaine, Jean-Francois Remacle,
!    Gmsh: a three-dimensional finite element mesh generator with
!    built-in pre- and post-processing facilities,
!    International Journal for Numerical Methods in Engineering,
!    Volume 79, Number 11, pages 1309-1331, 2009.
!
!  Parameters:
!
!    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
!
!    Input, integer M, the spatial dimension.
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( kind = rk ) NODE_X(M,NODE_NUM), the node coordinates.
!
!    Input, integer ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ELEMENT_NUM, the number of elements.
!
!    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order
  integer(kind=i4) :: m
  integer(kind=i4) :: node_num

  integer(kind=i4) :: element
  integer(kind=i4) :: element_node(element_order,element_num)
  integer(kind=i4) :: element_type
  character * ( * ) gmsh_filename
  integer(kind=i4) :: gmsh_unit
  integer(kind=i4) :: node
  real ( kind = dp ) node_x(m,node_num)
  integer(kind=i4) :: tag_num
  integer(kind=i4) :: tag1
!
!  Enforce 1-based indexing of nodes.
!
  call mesh_base_one ( node_num, element_order, element_num, element_node )
!
!  Open the file.
!
  call get_unit ( gmsh_unit )

  open ( unit = gmsh_unit, file = gmsh_filename, status = 'replace' )
!
!  Write the data.
!
  write ( gmsh_unit, '(a)' ) '$MeshFormat'
  write ( gmsh_unit, '(a)' ) '2.2 0 8'
  write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

  write ( gmsh_unit, '(a)' ) '$Nodes'
  write ( gmsh_unit, '(i6)' ) node_num
  do node = 1, node_num
    write ( gmsh_unit, '(i6,2x,g14.6,2x,g14.6,a)' ) &
      node, node_x(1:m,node), '  0.0'
  end do
  write ( gmsh_unit, '(a)' ) '$EndNodes'

  if ( element_order == 3 ) then
    element_type = 2
  else if ( element_order == 6 ) then
    element_type = 9
  end if

  tag_num = 2
  tag1 = 0
  write ( gmsh_unit, '(a)' ) '$Elements'
  write ( gmsh_unit, '(i6)' ) element_num
  do element = 1, element_num
    write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' ) &
      element, element_type, tag_num, tag1, element, &
      element_node(1:element_order,element)
  end do
  write ( gmsh_unit, '(a)' ) '$EndElements'

  close ( unit = gmsh_unit )

  return
end subroutine gmsh_mesh2d_write

subroutine gmsh_mesh3d_write ( gmsh_filename, m, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_MESH3D_WRITE writes 3D mesh data as a Gmsh file.
!
!  Discussion:
!
!    The node ordering for the 20 node element is not standard.
!
!    Assuming the vertices are A, B, C and D, Gmsh uses the following ordering:
!
!    1:    a
!    2:        b
!    3:            c
!    4:                d
!    5: (2*a  +b        )/3
!    6: (  a+2*b        )/3
!    7: (    2*b+  c    )/3
!    8: (      b+2*c    )/3
!    9: (  a    +2*c    )/3
!   10: (2*a    +  c    )/3
!   11: (2*a        +  d)/3
!   12: (  a        +2*d)/3
!   13: (     b     +2*d)/3
!   14: (   2*b     +  d)/3
!   15: (       +  c+2*d)/3
!   16: (       +2*c+  d)/3
!   17: (  a+  b+  c    )/3
!   18: (  a+  b    +  d)/3
!   19: (      b+  c+  d)/3
!   20: (  a+      c+  d)/3
!
!    Leo Rebholz used the following ordering:
!
!    1:    a
!    2:        b
!    3:            c
!    4:                d
!    5: (2*a  +b        )/3
!    6: (2*a    +  c    )/3
!    7: (  a+2*b        )/3
!    8: (  a    +2*c    )/3
!    9: (  a+  b+  c    )/3
!   10: (    2*b+  c    )/3
!   11: (      b+2*c    )/3
!   12: (2*a        +  d)/3
!   13: (   2*b     +  d)/3
!   14: (       +2*c+  d)/3
!   15: (  a+  b    +  d)/3
!   16: (      b+  c+  d)/3
!   17: (  a+      c+  d)/3
!   18: (  a        +2*d)/3
!   19: (     b     +2*d)/3
!   20: (       +  c+2*d)/3
!
!    Since the only 20 node data we have is from Leo, we will assume that
!    all 20 node input data is in Leo's format, and needs to be converted
!    to the Gmsh convention.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Christophe Geuzaine, Jean-Francois Remacle,
!    Gmsh: a three-dimensional finite element mesh generator with
!    built-in pre- and post-processing facilities,
!    International Journal for Numerical Methods in Engineering,
!    Volume 79, Number 11, pages 1309-1331, 2009.
!
!  Parameters:
!
!    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
!
!    Input, integer M, the spatial dimension.
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( kind = rk ) NODE_X(M,NODE_NUM), the node coordinates.
!
!    Input, integer ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ELEMENT_NUM, the number of elements.
!
!    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order
  integer(kind=i4) :: m
  integer(kind=i4) :: node_num

  integer(kind=i4) :: element
  integer(kind=i4) :: element_node(element_order,element_num)
  integer(kind=i4) :: element_type
  character * ( * ) gmsh_filename
  integer(kind=i4) :: gmsh_unit
  integer(kind=i4), dimension ( 20 ) :: leo_to_gmsh = (/ &
     1,  2,  3,  4,  5, &
     7, 10, 11,  8,  6, &
    12, 18, 19, 13, 20, &
    14,  9, 15, 16, 17 /)
  integer(kind=i4) :: node
  real ( kind = dp ) node_x(3,node_num)
  integer(kind=i4) :: tag_num
  integer(kind=i4) :: tag1
!
!  Enforce 1-based indexing of nodes.
!
  call mesh_base_one ( node_num, element_order, element_num, element_node )
!
!  Open the file.
!
  call get_unit ( gmsh_unit )

  open ( unit = gmsh_unit, file = gmsh_filename, status = 'replace' )
!
!  Write the data.
!
  write ( gmsh_unit, '(a)' ) '$MeshFormat'
  write ( gmsh_unit, '(a)' ) '2.2 0 8'
  write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

  write ( gmsh_unit, '(a)' ) '$Nodes'
  write ( gmsh_unit, '(i6)' ) node_num
  do node = 1, node_num
    write ( gmsh_unit, '(i6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      node, node_x(1:m,node)
  end do
  write ( gmsh_unit, '(a)' ) '$EndNodes'

  if ( element_order == 4 ) then
    element_type = 4
  else if ( element_order == 10 ) then
    element_type = 11
  else if ( element_order == 20 ) then
    element_type = 29
  end if

  tag_num = 2
  tag1 = 0
  write ( gmsh_unit, '(a)' ) '$Elements'
  write ( gmsh_unit, '(i6)' ) element_num
  do element = 1, element_num
    if ( element_order == 20 ) then
      write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' ) &
        element, element_type, tag_num, tag1, element, &
        element_node(leo_to_gmsh(1:element_order),element)
    else
      write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' ) &
        element, element_type, tag_num, tag1, element, &
        element_node(1:element_order,element)
    end if
  end do
  write ( gmsh_unit, '(a)' ) '$EndElements'

  close ( unit = gmsh_unit )

  return
end subroutine gmsh_mesh3d_write

subroutine i4mat_copy ( m, n, a1, a2 )

!*****************************************************************************80
!
!! I4MAT_COPY copies an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A1(M,N), the matrix to be copied.
!
!    Output, integer A2(M,N), the copied matrix.
!
  !implicit none

  integer(kind=i4) :: m
  integer(kind=i4) :: n

  integer(kind=i4) :: a1(m,n)
  integer(kind=i4) :: a2(m,n)

  a2(1:m,1:n) = a1(1:m,1:n)

  return
end subroutine i4mat_copy

subroutine i4mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  !implicit none

  integer(kind=i4) :: m
  integer(kind=i4) :: n

  integer(kind=i4) :: a(m,n)
  character ( len = * ) title

  call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end subroutine i4mat_transpose_print

subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  !implicit none

  integer(kind=i4) ::, parameter :: incx = 10
  integer(kind=i4) :: m
  integer(kind=i4) :: n

  integer(kind=i4) :: a(m,n)
  character ( len = 8 ) ctemp(incx)
  integer(kind=i4) :: i
  integer(kind=i4) :: i2
  integer(kind=i4) :: i2hi
  integer(kind=i4) :: i2lo
  integer(kind=i4) :: ihi
  integer(kind=i4) :: ilo
  integer(kind=i4) :: inc
  integer(kind=i4) :: j
  integer(kind=i4) :: j2hi
  integer(kind=i4) :: j2lo
  integer(kind=i4) :: jhi
  integer(kind=i4) :: jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8)' ) i
    end do

    write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc

        i = i2lo - 1 + i2

        write ( ctemp(i2), '(i8)' ) a(i,j)

      end do

      write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end subroutine i4mat_transpose_print_some

subroutine mesh_base_one ( node_num, element_order, element_num, element_node )

!*****************************************************************************80
!
!! MESH_BASE_ONE ensures that the element definition is one-based.
!
!  Discussion:
!
!    The ELEMENT_NODE array contains nodes indices that form elements.
!    The convention for node indexing might start at 0 or at 1.
!    Since a FORTRAN90 program will naturally assume a 1-based indexing, it is
!    necessary to check a given element definition and, if it is actually
!    0-based, to convert it.
!
!    This function attempts to detect 9-based node indexing and correct it.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, int NODE_NUM, the number of nodes.
!
!    Input, int ELEMENT_ORDER, the order of the elements.
!
!    Input, int ELEMENT_NUM, the number of elements.
!
!    Input/output, int ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the element
!    definitions.
!
  !implicit none

  integer(kind=i4) :: element_num
  integer(kind=i4) :: element_order

  integer(kind=i4) :: element_node(element_order,element_num)
  integer(kind=i4) ::, parameter :: i4_huge = 2147483647
  integer(kind=i4) :: node_max
  integer(kind=i4) :: node_min
  integer(kind=i4) :: node_num

  node_min = + i4_huge
  node_max = - i4_huge

  node_min = minval ( element_node(1:element_order,1:element_num) )
  node_max = maxval ( element_node(1:element_order,1:element_num) )

  if ( node_min == 0 .and. node_max == node_num - 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' )'MESH_BASE_ONE:'
    write ( *, '(a)' )'  The element indexing appears to be 0-based!'
    write ( *, '(a)' )'  This will be converted to 1-based.'
    element_node(1:element_order,1:element_num) = &
      element_node(1:element_order,1:element_num) + 1
  else if ( node_min == 1 .and. node_max == node_num  ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' )'MESH_BASE_ONE:'
    write ( *, '(a)' )'  The element indexing appears to be 1-based!'
    write ( *, '(a)' )'  No conversion is necessary.'
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MESH_BASE_ONE - Warning!'
    write ( *, '(a)' ) '  The element indexing is not of a recognized type.'
    write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
    write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
    write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
  end if

  return
end subroutine mesh_base_one

subroutine r8mat_copy ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_COPY copies an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> (I+J*M).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the order of the matrix.
!
!    Input, real ( kind = rk ) A(M,N), the matrix to be copied.
!
!    Output, real ( kind = rk ) B(M,N), a copy of the matrix.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: m
  integer(kind=i4) :: n

  real ( kind = dp ) a(m,n)
  real ( kind = dp ) b(m,n)

  b(1:m,1:n) = a(1:m,1:n)

  return
end subroutine r8mat_copy

subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = rk ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) :: m
  integer(kind=i4) :: n

  real ( kind = dp ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end subroutine r8mat_transpose_print

subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = rk ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  integer(kind=i4) ::, parameter :: incx = 5
  integer(kind=i4) :: m
  integer(kind=i4) :: n

  real ( kind = dp ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer(kind=i4) :: i
  integer(kind=i4) :: i2
  integer(kind=i4) :: i2hi
  integer(kind=i4) :: i2lo
  integer(kind=i4) :: ihi
  integer(kind=i4) :: ilo
  integer(kind=i4) :: inc
  integer(kind=i4) :: j
  integer(kind=i4) :: j2hi
  integer(kind=i4) :: j2lo
  integer(kind=i4) :: jhi
  integer(kind=i4) :: jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end subroutine r8mat_transpose_print_some

function s_begin ( s1, s2 )

!*****************************************************************************80
!
!! S_BEGIN is TRUE if one string matches the beginning of the other.
!
!  Discussion:
!
!    The strings are compared, ignoring blanks, spaces and capitalization.
!
!  Example:
!
!     S1              S2      S_BEGIN
!
!    'Bob'          'BOB'     TRUE
!    '  B  o b '    ' bo b'   TRUE
!    'Bob'          'Bobby'   TRUE
!    'Bobo'         'Bobb'    FALSE
!    ' '            'Bob'     FALSE    (Do not allow a blank to match
!                                       anything but another blank string.)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Output, logical S_BEGIN, is TRUE if the strings match up to
!    the end of the shorter string, ignoring case.
!
  !implicit none

  logical ch_eqi
  integer(kind=i4) :: i1
  integer(kind=i4) :: i2
  logical s_begin
  character ( len = * ) s1
  integer(kind=i4) :: s1_length
  character ( len = * ) s2
  integer(kind=i4) :: s2_length

  s1_length = len_trim ( s1 )
  s2_length = len_trim ( s2 )
!
!  If either string is blank, then both must be blank to match.
!  Otherwise, a blank string matches anything, which is not
!  what most people want.
!
  if ( s1_length == 0 .or. s2_length == 0 ) then

    if ( s1_length == 0 .and. s2_length == 0 ) then
      s_begin = .true.
    else
      s_begin = .false.
    end if

    return

  end if

  i1 = 0
  i2 = 0
!
!  Find the next nonblank in S1.
!
  do

    do

      i1 = i1 + 1

      if ( s1_length < i1 ) then
        s_begin = .true.
        return
      end if

      if ( s1(i1:i1) /= ' ' ) then
        exit
      end if

    end do
!
!  Find the next nonblank in S2.
!
    do

      i2 = i2 + 1

      if ( s2_length < i2 ) then
        s_begin = .true.
        return
      end if

      if ( s2(i2:i2) /= ' ' ) then
        exit
      end if

    end do
!
!  If the characters match, get the next pair.
!
    if ( .not. ch_eqi ( s1(i1:i1), s2(i2:i2) ) ) then
      exit
    end if

  end do

  s_begin = .false.

  return
end function s_begin

subroutine s_to_i4 ( s, ival, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer LENGTH, the number of characters of S
!    used to make IVAL.
!
  !implicit none

  character c
  integer(kind=i4) :: i
  integer(kind=i4) :: ierror
  integer(kind=i4) :: isgn
  integer(kind=i4) :: istate
  integer(kind=i4) :: ival
  integer(kind=i4) :: length
  character ( len = * ) s

  ierror = 0
  istate = 0
  isgn = 1
  ival = 0

  do i = 1, len_trim ( s )

    c = s(i:i)
!
!  Haven't read anything.
!
    if ( istate == 0 ) then

      if ( c == ' ' ) then

      else if ( c == '-' ) then
        istate = 1
        isgn = -1
      else if ( c == '+' ) then
        istate = 1
        isgn = + 1
      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read the sign, expecting digits.
!
    else if ( istate == 1 ) then

      if ( c == ' ' ) then

      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read at least one digit, expecting more.
!
    else if ( istate == 2 ) then

      if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        ival = 10 * ival + ichar ( c ) - ichar ( '0' )
      else
        ival = isgn * ival
        length = i - 1
        return
      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( istate == 2 ) then
    ival = isgn * ival
    length = len_trim ( s )
  else
    ierror = 1
    length = 0
  end if

  return
end subroutine s_to_i4

subroutine s_to_r8 ( s, dval, ierror, length )

!*****************************************************************************80
!
!! S_TO_R8 reads an R8 from a string.
!
!  Discussion:
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = rk ) DVAL, the value read from the string.
!
!    Output, integer IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  !implicit none

  !integer, parameter :: rk = kind ( 1.0D+00 )

  character c
  logical ch_eqi
  real ( kind = dp ) dval
  integer(kind=i4) ::  ierror
  integer(kind=i4) :: ihave
  integer(kind=i4) :: isgn
  integer(kind=i4) :: iterm
  integer(kind=i4) :: jbot
  integer(kind=i4) :: jsgn
  integer(kind=i4) :: jtop
  integer(kind=i4) :: length
  integer(kind=i4) :: nchar
  integer(kind=i4) :: ndig
  real ( kind = dp ) rbot
  real ( kind = dp ) rexp
  real ( kind = dp ) rtop
  character ( len = * ) s

  nchar = len_trim ( s )

  ierror = 0
  dval = 0.0_dp
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    length = length + 1

    if ( nchar < length+1 ) then
      exit
    end if

    c = s(length+1:length+1)
!
!  Blank character.
!
    if ( c == ' ' ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( 1 < ihave ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        length = length + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = -1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( c == '+' ) then

      if ( ihave == 1 ) then
        ihave = 2
      else if ( ihave == 6 ) then
        ihave = 7
      else
        iterm = 1
      end if
!
!  Decimal point.
!
    else if ( c == '.' ) then

      if ( ihave < 4 ) then
        ihave = 4
      else if ( 6 <= ihave .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Scientific notation exponent marker.
!
    else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

      if ( ihave < 6 ) then
        ihave = 6
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if (  ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

      if ( ihave <= 2 ) then
        ihave = 3
      else if ( ihave == 4 ) then
        ihave = 5
      else if ( ihave == 6 .or. ihave == 7 ) then
        ihave = 8
      else if ( ihave == 9 ) then
        ihave = 10
      end if

      call ch_to_digit ( c, ndig )

      if ( ihave == 3 ) then
        rtop = 10.0_dp * rtop + real ( ndig, kind = dp )
      else if ( ihave == 5 ) then
        rtop = 10.0_dp * rtop + real ( ndig, kind = dp )
        rbot = 10.0_dp * rbot
      else if ( ihave == 8 ) then
        jtop = 10 * jtop + ndig
      else if ( ihave == 10 ) then
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      end if
!
!  Anything else is regarded as a terminator.
!
    else
      iterm = 1
    end if
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    if ( iterm == 1 ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to NCHAR.
!
  if ( iterm /= 1 .and. length + 1 == nchar ) then
    length = nchar
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then
    ierror = ihave
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
    write ( *, '(a)' ) '  Illegal or nonnumeric input:'
    write ( *, '(a)' ) '    ' // trim ( s )
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0_dp
  else
    if ( jbot == 1 ) then
      rexp = 10.0_dp ** ( jsgn * jtop )
    else
      rexp = 10.0_dp ** ( real ( jsgn * jtop, kind = dp ) &
        / real ( jbot, kind = rk ) )
    end if
  end if

  dval = real ( isgn, kind = dp ) * rexp * rtop / rbot

  return
end subroutine s_to_r8

#if 0
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  implicit none

  character ( len = 8 ) ampm
  integer d
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  integer values(8)
  integer y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end subroutine timestamp
#endif

     















end module GMSH_Reader
