

module ball_grid

!MIT License
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


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'ball_grid'
 !          
 !          Purpose:
 !                     This module contains a slightly modified implementation
 !                      of J. Burkardt "ball_grid" subroutines.
 !          History:
 !                        
 !                        Date: 17-06-2022
 !                        Time: 12:53 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Modified by:
 !                 
 !                   Bernard Gingold
 !
 !         Original author:
 !         
 !                   J. Burkartd
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


     interface ball_grid
        module procedure :: r8_ball_grid
        module procedure :: r4_ball_grid
     end interface ball_grid

     interface ball_grid_count
        module procedure :: r8_ball_grid_count
        module procedure :: r4_ball_grid_count
     end interface ball_grid_count

     private :: get_unit
     
   contains


   subroutine r8_ball_grid ( n, r, c, ng, bg )
         !dir$ attributes forceinline :: r8_circle_grid
         !dir$ attributes code_align : 32 :: r8_circle_grid
         !dir$ optimize : 3
         !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: r8_circle_grid
!*****************************************************************************80
!
!! BALL_GRID computes grid points inside a ball.
!
!  Discussion:
!
!    The grid is defined by specifying the radius and center of the ball,
!    and the number of subintervals N into which the horizontal radius
!    should be divided.  Thus, a value of N = 2 will result in 5 points
!    along that horizontal line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, real ( kind = 8 ) C(3), the coordinates of the center of the ball.
!
!    Input, integer ( kind = 4 ) NG, the number of grid points, as determined by
!    BALL_GRID_COUNT.
!
!    Output, real ( kind = 8 ) BG(3,NG), the grid points inside the ball.
!
  implicit none

  integer ( kind = i4 ) ng

  real ( kind = dp ) bg(3,ng)
  real ( kind = dp ) c(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  integer ( kind = i4 ) n
  integer ( kind = i4 ) p
  real ( kind = dp ) r
  real ( kind = dp ) x
  real ( kind = dp ) y
  real ( kind = dp ) z

  p = 0

  !dir$ assume_aligned bg:64
  do i = 0, n

    x = c(1) + r * real ( 2 * i, kind = dp ) / real ( 2 * n + 1, kind = dp )
    
    do j = 0, n

      y = c(2) + r * real ( 2 * j, kind = dp ) / real ( 2 * n + 1, kind = dp )

      do k = 0, n

        z = c(3) + r * real ( 2 * k, kind = dp ) / real ( 2 * n + 1, kind = dp )

        if ( r * r < ( x - c(1) )**2 &
                   + ( y - c(2) )**2 &
                   + ( z - c(3) )**2 ) then
          exit
        end if

        p = p + 1
        bg(1,p) = x
        bg(2,p) = y
        bg(3,p) = z

        if ( 0 < i ) then
          p = p + 1
          bg(1,p) = 2.0_dp * c(1) - x
          bg(2,p) = y
          bg(3,p) = z
        end if

        if ( 0 < j ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = 2.0_dp * c(2) - y
          bg(3,p) = z
        end if

        if ( 0 < k ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = y
          bg(3,p) = 2.0_dp * c(3) - z
        end if

        if ( 0 < i .and. 0 < j ) then
          p = p + 1
          bg(1,p) = 2.0_dp * c(1) - x
          bg(2,p) = 2.0_dp * c(2) - y
          bg(3,p) = z
        end if

        if ( 0 < i .and. 0 < k ) then
          p = p + 1
          bg(1,p) = 2.0_dp * c(1) - x
          bg(2,p) = y
          bg(3,p) = 2.0_dp * c(3) - z
        end if

        if ( 0 < j .and. 0 < k ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = 2.0_dp * c(2) - y
          bg(3,p) = 2.0_dp * c(3) - z
        end if

        if ( 0 < i .and. 0 < j .and. 0 < k ) then
          p = p + 1
          bg(1,p) = 2.0_dp * c(1) - x
          bg(2,p) = 2.0_dp * c(2) - y
          bg(3,p) = 2.0_dp * c(3) - z
        end if

      end do
    end do
  end do

 
  end subroutine


   subroutine r4_ball_grid ( n, r, c, ng, bg )
         !dir$ attributes forceinline :: r4_circle_grid
         !dir$ attributes code_align : 32 :: r4_circle_grid
         !dir$ optimize : 3
         !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: r4_circle_grid
!*****************************************************************************80
!
!! BALL_GRID computes grid points inside a ball.
!
!  Discussion:
!
!    The grid is defined by specifying the radius and center of the ball,
!    and the number of subintervals N into which the horizontal radius
!    should be divided.  Thus, a value of N = 2 will result in 5 points
!    along that horizontal line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, real ( kind = 8 ) C(3), the coordinates of the center of the ball.
!
!    Input, integer ( kind = 4 ) NG, the number of grid points, as determined by
!    BALL_GRID_COUNT.
!
!    Output, real ( kind = 8 ) BG(3,NG), the grid points inside the ball.
!
  implicit none

  integer ( kind = i4 ) ng

  real ( kind = sp ) bg(3,ng)
  real ( kind = sp ) c(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  integer ( kind = i4 ) n
  integer ( kind = i4 ) p
  real ( kind = sp ) r
  real ( kind = sp ) x
  real ( kind = sp ) y
  real ( kind = sp ) z

  p = 0

  !dir$ assume_aligned bg:64
  do i = 0, n

    x = c(1) + r * real ( 2 * i, kind = sp ) / real ( 2 * n + 1, kind = sp )
    
    do j = 0, n

      y = c(2) + r * real ( 2 * j, kind = sp ) / real ( 2 * n + 1, kind = sp )

      do k = 0, n

        z = c(3) + r * real ( 2 * k, kind = sp ) / real ( 2 * n + 1, kind = sp )

        if ( r * r < ( x - c(1) )**2 &
                   + ( y - c(2) )**2 &
                   + ( z - c(3) )**2 ) then
          exit
        end if

        p = p + 1
        bg(1,p) = x
        bg(2,p) = y
        bg(3,p) = z

        if ( 0 < i ) then
          p = p + 1
          bg(1,p) = 2.0_sp * c(1) - x
          bg(2,p) = y
          bg(3,p) = z
        end if

        if ( 0 < j ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = 2.0_sp * c(2) - y
          bg(3,p) = z
        end if

        if ( 0 < k ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = y
          bg(3,p) = 2.0_sp * c(3) - z
        end if

        if ( 0 < i .and. 0 < j ) then
          p = p + 1
          bg(1,p) = 2.0_sp * c(1) - x
          bg(2,p) = 2.0_sp * c(2) - y
          bg(3,p) = z
        end if

        if ( 0 < i .and. 0 < k ) then
          p = p + 1
          bg(1,p) = 2.0_sp * c(1) - x
          bg(2,p) = y
          bg(3,p) = 2.0_sp * c(3) - z
        end if

        if ( 0 < j .and. 0 < k ) then
          p = p + 1
          bg(1,p) = x
          bg(2,p) = 2.0_sp * c(2) - y
          bg(3,p) = 2.0_sp * c(3) - z
        end if

        if ( 0 < i .and. 0 < j .and. 0 < k ) then
          p = p + 1
          bg(1,p) = 2.0_sp * c(1) - x
          bg(2,p) = 2.0_sp * c(2) - y
          bg(3,p) = 2.0_sp * c(3) - z
        end if

      end do
    end do
  end do

 
  end subroutine


  subroutine r8_ball_grid_count ( n, r, c, ng )
         !dir$ attributes forceinline :: r8_ball_grid_count
         !dir$ attributes code_align : 32 :: r8_ball_grid_count
         !dir$ optimize : 3
!*****************************************************************************80
!
!! BALL_GRID computes grid points inside a ball.
!
!  Discussion:
!
!    The grid is defined by specifying the radius and center of the ball,
!    and the number of subintervals N into which the horizontal radius
!    should be divided.  Thus, a value of N = 2 will result in 5 points
!    along that horizontal line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, real ( kind = 8 ) C(3), the coordinates of the center of the ball.
!
!    Output, integer ( kind = 4 ) NG, the number of grid points inside the ball.
!
  implicit none

  real ( kind = dp ) c(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  integer ( kind = i4 ) n
  integer ( kind = i4 ) ng
  real ( kind = dp ) r
  real ( kind = dp ) x
  real ( kind = dp ) y
  real ( kind = dp ) z

  ng = 0

  do i = 0, n

    x = c(1) + r * real ( 2 * i, kind = dp ) / real ( 2 * n + 1, kind = dp )
    
    do j = 0, n

      y = c(2) + r * real ( 2 * j, kind = dp ) / real ( 2 * n + 1, kind = dp )

      do k = 0, n

        z = c(3) + r * real ( 2 * k, kind = dp ) / real ( 2 * n + 1, kind = dp )

        if ( r * r < ( x - c(1) )**2 &
                   + ( y - c(2) )**2 &
                   + ( z - c(3) )**2 ) then
          exit
        end if

        ng = ng + 1

        if ( 0 < i ) then
          ng = ng + 1
        end if

        if ( 0 < j ) then
          ng = ng + 1
        end if

        if ( 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < j ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < j .and. 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < j .and. 0 < k ) then
          ng = ng + 1
        end if

      end do
    end do
  end do

 
  end subroutine


  subroutine r4_ball_grid_count ( n, r, c, ng )
         !dir$ attributes forceinline :: r4_ball_grid_count
         !dir$ attributes code_align : 32 :: r4_ball_grid_count
         !dir$ optimize : 3
!*****************************************************************************80
!
!! BALL_GRID computes grid points inside a ball.
!
!  Discussion:
!
!    The grid is defined by specifying the radius and center of the ball,
!    and the number of subintervals N into which the horizontal radius
!    should be divided.  Thus, a value of N = 2 will result in 5 points
!    along that horizontal line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, real ( kind = 8 ) C(3), the coordinates of the center of the ball.
!
!    Output, integer ( kind = 4 ) NG, the number of grid points inside the ball.
!
  implicit none

  real ( kind = sp ) c(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  integer ( kind = i4 ) n
  integer ( kind = i4 ) ng
  real ( kind = sp ) r
  real ( kind = sp ) x
  real ( kind = sp ) y
  real ( kind = sp ) z

  ng = 0

  do i = 0, n

    x = c(1) + r * real ( 2 * i, kind = sp ) / real ( 2 * n + 1, kind = sp )
    
    do j = 0, n

      y = c(2) + r * real ( 2 * j, kind = sp ) / real ( 2 * n + 1, kind = sp )

      do k = 0, n

        z = c(3) + r * real ( 2 * k, kind = sp ) / real ( 2 * n + 1, kind = sp )

        if ( r * r < ( x - c(1) )**2 &
                   + ( y - c(2) )**2 &
                   + ( z - c(3) )**2 ) then
          exit
        end if

        ng = ng + 1

        if ( 0 < i ) then
          ng = ng + 1
        end if

        if ( 0 < j ) then
          ng = ng + 1
        end if

        if ( 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < j ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < j .and. 0 < k ) then
          ng = ng + 1
        end if

        if ( 0 < i .and. 0 < j .and. 0 < k ) then
          ng = ng + 1
        end if

      end do
    end do
  end do

 
  end subroutine


  subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = i4 ) i
  integer ( kind = i4 ) ios
  integer ( kind = i4 ) iunit
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

  
  end subroutine 

  subroutine ball_grid_print( n, a, max_print, title )

!*****************************************************************************80
!
!! R83VEC_PRINT_PART prints "part" of an R83VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(3,N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = i4 ) n

  real ( kind = dp ) a(3,n)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) max_print
  character ( len = * )  title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    write ( *, '(a)' ) &
      '  ........  ..............  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:3,i), &
      '...more entries...'

  end if

  
  end subroutine 

  subroutine ball_grid_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = i4 ) m
  integer ( kind = i4 ) n

  integer ( kind = i4 ) j
  character ( len = * ) output_filename
  integer ( kind = i4 ) output_status
  integer ( kind = i4 ) output_unit
  character ( len = 30 ) string
  real ( kind = dp ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    return
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

 
  end subroutine


  !=========================== Usage =================================!
  !program main

!*****************************************************************************80
!
!! BALL_GRID_TEST tests BALL_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
  !implicit none

  !call timestamp ( )
  !write ( *, '(a)' ) ' '
  !write ( *, '(a)' ) 'BALL_GRID_TEST:'
  !write ( *, '(a)' ) '  FORTRAN90 version'
  !write ( *, '(a)' ) '  Test the BALL_GRID library.'

  !call ball_grid_test01 ( )
!
!  Terminate.
!
  !write ( *, '(a)' ) ' '
  !write ( *, '(a)' ) 'BALL_GRID_TEST:'
  !write ( *, '(a)' ) '  Normal end of execution.'
  !write ( *, '(a)' ) ' '
  !call timestamp ( )

  !return
!end
!subroutine ball_grid_test01 ( )

!*****************************************************************************80
!
!! BALL_GRID_TEST01 tests BALL_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
  !implicit none

  !real ( kind = 8 ), allocatable :: bg(:,:)
  !real ( kind = 8 ) c(3)
  !character ( len = 80 ) filename
  !integer ( kind = 4 ) n
  !integer ( kind = 4 ) ng
  !real ( kind = 8 ) r

  !write ( *, '(a)' ) ' '
  !write ( *, '(a)' ) 'TEST01:'
  !write ( *, '(a)' ) '  BALL_GRID can define a grid of points'
  !write ( *, '(a)' ) '  with N+1 points on a horizontal or vertical radius,'
  !write ( *, '(a)' ) '  based on any ball.'

  !n = 10
  !r = 2.0D+00
  !c(1) = 1.0D+00
  !c(2) = 5.0D+00
  !c(3) = 2.0D+00

  !write ( *, '(a)' ) ' '
  !write ( *, '(a,i8)' ) '  We use N = ', n
  !write ( *, '(a,g14.6)' ) '  Radius R = ', r
  !write ( *, '(a,g14.6,a,g14.6,a,g14.6,a)' ) &
  !  '  Center C = (', c(1), ',', c(2), ',', c(3), ')'

  !call ball_grid_count ( n, r, c, ng )

  !write ( *, '(a)' ) ' '
  !write ( *, '(a,i8)' ) '  Number of grid points will be ', ng

  !allocate ( bg(1:3,1:ng) )

  !call ball_grid ( n, r, c, ng, bg )

  !call r83vec_print_part ( ng, bg, 20, '  Part of the grid point array:' )

  !filename = 'ball_grid_test01.xyz'

  !call r8mat_write ( filename, 3, ng, bg )

  !write ( *, '(a)' ) ' '
  !write ( *, '(a)' ) '  Data written to the file "' // trim ( filename ) // '".'

  !deallocate ( bg )

  !return
  !end



end module ball_grid
