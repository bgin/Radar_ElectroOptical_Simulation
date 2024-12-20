module io_mod
!!==============================================================================
! This module contains I/O procedures, and has a procedure
! specifically designed to read Gmsh2 files. To be used together with mesh_mod.
!
! Last edited: March 7th 2021.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use iso_fortran_env  , only: IOSTAT_END 
  use iso_fortran_env  , only: ERROR_UNIT
  use ieee_arithmetic  , only: ieee_is_finite
  use ieee_arithmetic  , only: ieee_is_nan

  implicit none
  

  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public  :: open_read_gmsh2
  public  :: string_to_int4
  public  :: string_to_real_wp
  public  :: read_nth_int4
  public  :: read_n_last_int4
  public  :: read_n_last_real_wp
  public  :: count_int4_on_string
  public  :: count_real_wp_on_string
  public  :: capitalise_char
  ! Procedures by John Burkardt for writing tables to file
  public  :: r8mat_write
  public  :: get_unit
  
  !!==================================!!
  ! Private types/procedures/constants !
  !====================================!========================================
  private :: check_ioerr_opening
  private :: check_ioerr_reading

  
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  
  !!=================!!
  ! Public procedures !
  !===================!=========================================================

  !!-------------------------------------------!!
  ! Specific file format open and read routines !
  !---------------------------------------------!-------------------------------
  subroutine open_read_gmsh2(FILENAME, spatial_dim, element_order, nodes, &
       elements)
    character(len=*)                      , intent(in)  :: FILENAME
    integer                               , intent(in)  :: spatial_dim
    integer                               , intent(in)  :: element_order
    real(wp), dimension(:, :), allocatable, intent(out) :: nodes
    integer , dimension(:, :), allocatable, intent(out) :: elements
    ! Variables for internal use ----------------------------------------------
    integer , parameter       :: BUFFER_LEN = 255
    integer,  parameter       :: IOMSG_LEN = 255
    character(len=BUFFER_LEN) :: line
    character(len=IOMSG_LEN)  :: iotxt
    integer                   :: line_nr
    integer                   :: num_nodes
    integer                   :: num_elements
    integer                   :: num_elements_tot
    integer                   :: element_line_start
    integer                   :: element_line_end
    integer                   :: unit_nr
    integer                   :: level
    integer                   :: int4_value
    real(wp)                  :: real_wp_value
    integer                   :: length
    integer                   :: ioerr
    integer                   :: error_state
    integer                   :: i, j, k
    !__________________________________________________________________________!
    !/\_/\_/\_/\_/\_/\_/\_/\_/\_/\__DOCSTRING__/\_/\_/\_/\_/\_/\_/\_/\_/\_/\_/\!
    ! This routine loads a Gmsh2 ASCII file given by a file name, the spatial
    ! order of the mesh, and the element order. It reads the file line by line
    ! and succesively progresses through levels, which are activated by
    ! keywords in the .msh-file.
    !
    ! Arguments:
    !     FILENAME - The path to the .msh
    !     spatial dim - The spatial order of the mesh
    !     element order - The order of the elements in the mesh.
    ! Result:
    !     nodes - A matrix contain the nodes of the mesh and their Cartesian
    !             coordinates.
    !     elements - A matrix containing the elements of the mesh, defined by
    !                the indices of the nodes it comprises.
    !__________________________________________________________________________!
    
    open (newunit=unit_nr, file=FILENAME, status='old', action='read', &
         iostat=ioerr, iomsg=iotxt)
    call check_ioerr_opening(ioerr, iotxt, IOMSG_LEN, FILENAME, 1, 'reading')

    ! Read file and interpret line by line
    level = 0
    num_elements = 0
    line_nr = 0
    do 
       length = 1
       line_nr = line_nr + 1
       
       read (unit_nr, '(a)', iostat=ioerr, iomsg=iotxt) line
       if (ioerr /= 0) then
          call check_ioerr_reading(ioerr, iotxt, IOMSG_LEN, FILENAME, 3)
          exit
       end if
       ! Read nodes
       if (level == 0) then
          if (line(1:6) == '$Nodes') then
             level = 1
          end if
       else if (level == 1) then
          call string_to_int4(line, length, int4_value, error_state)
          num_nodes = int4_value
          allocate(nodes(num_nodes, spatial_dim))
          j = 0
          level = 2
       else if (level == 2) then
          if (line(1:9) == '$EndNodes') then
             level = 3
          else
             j = j + 1
             call read_n_last_real_wp(line, spatial_dim, nodes(j, :), &
                  num_real_wp_in=(spatial_dim + 1))
          end if
          
       ! Read elements
       else if (level == 3) then
          if (line(1:9) == '$Elements') then
             level = 4
          end if
       else if (level == 4) then
          call string_to_int4(line, length, int4_value, error_state)
          num_elements_tot = int4_value
          level = 5
       else if (level == 5) then
          int4_value = read_nth_int4(line, 2)
          if (int4_value == 2) then
             element_line_start = line_nr
             num_elements = 1
             level = 6
          end if
       else if (level == 6) then
          if (line(1:12) == '$EndElements') then
             level = 7
             rewind(unit_nr)
          else
             int4_value = read_nth_int4(line, 2)
             if (int4_value /= 2) then
                level = 7
                rewind(unit_nr)
             end if
          end if
          if (level == 6) then
             num_elements = num_elements + 1
          end if
       else if (level == 7) then
          allocate(elements(num_elements, element_order))
          level = 8
          j = 0
          element_line_end = line_nr - 1
          line_nr = 1
       else if (level == 8) then
          if (line(1:12) == '$EndElements') then
             exit
          else if (j == num_elements) then
             exit
          else if (line_nr == element_line_end) then
             exit
          else if (line_nr >= element_line_start) then
             j = j + 1
             call read_n_last_int4(line, element_order, elements(j, :))
          end if
       end if
    end do

    close (unit_nr)

    call validate_nodes_and_elements(spatial_dim, element_order, num_nodes, &
         num_elements, nodes, elements)
    
  end subroutine open_read_gmsh2


  !!-----------------------------------------!!
  ! Routines converting strings to data types !
  !-------------------------------------------!---------------------------------
  subroutine string_to_int4(text, length, int4_value, error_state)
    character(len=*), intent(in)    :: text
    integer         , intent(inout) :: length
    integer         , intent(inout) :: int4_value
    integer         , intent(inout) :: error_state
    ! Variables for internal use -----------------------------------------------
    character(len=1)                :: ch
    logical                         :: terminate
    integer                         :: state
    integer                         :: int4_sign
    integer                         :: char_start
    integer                         :: char_end
    integer                         :: i
    
    state = 1
    terminate = .false.
    int4_sign = 1
    
    do i = 1, len_trim(text)
       ch = text(i:i)
       ! Blank
       if (ch == ' ') then
          if (state > 1) then
             terminate = .true.
          end if
       ! Plus or minus sign
       else if (ch == '+' .or. ch == '-') then
          if (state == 1) then
             state = 2
             char_start = i
          else
             terminate = .true.
          end if
       ! Digit
       else if (lle ('0', ch) .and. lle (ch, '9')) then
          if (state == 1) then
             char_start = i
          end if
          state = 3
          char_end = i
       ! Terminate on anything else
       else
          terminate = .true.
       end if
       
       if (terminate) then
          exit
       end if
    end do
    
    length = i - 1
    !  Number seems to have terminated.  Have we got a legal number?
    !  Not if we terminated right after a sign character or without
    !  reading a single digit. I.e. not if we terminated in
    !   states 1 or 2
    if (state == 1 .or. state == 2) then
      error_state = state
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRING_TO_INT4 - Serious error!'
      write ( *, '(a)' ) '  Illegal or nonnumeric input:'
      write ( *, '(a)' ) '    ' // trim (text)
    end if
    ! Determine value
    read(text(char_start:char_end), '(I10)') int4_value
    
  end subroutine string_to_int4
  
 !------------------------------------------------------------------------------

  subroutine string_to_real_wp(text, length, value_of_real, error_state)
    character(len=*), intent(in)    :: text
    integer         , intent(inout) :: length
    real(wp)        , intent(inout) :: value_of_real
    integer         , intent(inout) :: error_state
    ! Variables for internal use -----------------------------------------------
    character(len=1)                :: ch
    logical                         :: terminate
    integer                         :: state 
    integer                         :: sign_of_base
    integer                         :: sign_of_exponent
    real(wp)                        :: base_top
    real(wp)                        :: base_bot
    integer                         :: exponent_top
    integer                         :: exponent_bot
    real(wp)                        :: exponential_value
    integer                         :: i
    integer                         :: digit

    state = 1
    terminate = .false.
    sign_of_base = 1
    sign_of_exponent = 1
    base_top = 0._wp
    base_bot = 1._wp
    exponent_top = 0
    exponent_bot = 1
    error_state = 0

    do i = 1, len_trim(text)
       ch = text(i:i)
       ! The following code is a modification of code written by
       ! John Burkardt in 2000.
       !------------------------------------------------------------------------
       ! 
       ! Blanck character
       if (ch == ' ') then
          if (state == 2) then   
          else if (state == 6 .or. state == 7) then
             terminate = .true.
          else if (state > 1) then
             state = 11
          end if
       !
       ! Comma
       else if (ch == ',' .or. ch == ';') then
          if (state /= 1) then ! If read characters other than blanks.
             terminate = .true.
             state = 12
             !i = i + 1 ! Why increment?
          end if
       !
       ! Minus sign  
       else if (ch == '-') then
          if (state == 1) then ! If first non-blank character.
             state = 2
             sign_of_base = -1
          else if (state == 6) then
             state = 7
             sign_of_exponent = -1
          else
             terminate = .true.
          end if
       !
       ! Pluss sign
       else if (ch == '+') then
          if (state == 1) then ! If first non-blank character.
             state = 2
             sign_of_base = 1
          else if (state == 6) then
             state = 7
             sign_of_exponent = 1
          else
             terminate = .true.
          end if
       !
       ! Decimal point
       else if (ch == '.') then
          if (state < 4) then
             state = 4
          else if (state >= 6 .and. state <= 8) then
             state = 9
          else
             terminate = .true.
          end if
       !
       ! Scientific notation exponent marker.
       else if (capitalise_char(ch) == 'E' .or. capitalise_char(ch) == 'D') then
          if (state < 6) then
             state = 6
          else
             terminate = .true.
          end if
       !
       ! Digit
       else if (state < 11 .and. lle('0', ch) .and. lle(ch, '9')) then
          if (state <= 2) then ! If first digit
             state = 3
          else if (state == 4) then ! If first digit after decimal point
             state = 5
          else if (state == 6 .or. state == 7) then ! If first digit after
             ! exponent marker
             state = 8
          else if (state == 9) then ! If decimal point after exponent marker
             state = 10
          end if
          
          read(ch, '(I10)') digit

          if (state == 3) then
             base_top = 10._wp * base_top + real(digit, wp)
          else if (state == 5) then
             base_top = 10._wp * base_top + real(digit, wp)
             base_bot = 10._wp * base_bot
          else if (state == 8) then
             exponent_top = 10*exponent_top + digit
          else if (state == 10) then
             exponent_top = 10*exponent_top + digit
             exponent_bot = 10*exponent_bot
          end if
       !
       ! Termninate on anything else
       else
          terminate = .true.
       end if
       if (terminate) then
          exit
       end if
    end do

    length = i - 1
    !
    !  Number seems to have terminated.  Have we got a legal number?
    !  Not if we terminated right after a sign character, scientific
    !  marker, or without reading a single digit. I.e. not if we
    !  terminated in states 1, 2, 6 or 7!
    !
    if ( state == 1 .or. state == 2 .or. state == 6 .or. state == 7 ) then
      error_state = state
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRING_TO_REAL_WP - Serious error!'
      write ( *, '(a)' ) '  Illegal or nonnumeric input:'
      write ( *, '(a)' ) '    ' // trim (text)
    end if
  !
  !  Number OK. Form it.
    if ( exponent_top == 0 ) then ! if non-scientific form
      exponential_value = 1._wp
    else
      if ( exponent_bot == 1 ) then
        exponential_value = 10._wp**(sign_of_exponent*exponent_top)
      else
        exponential_value = 10._wp**(real(sign_of_exponent*exponent_top, wp) &
          /real(exponent_bot, wp))
      end if
    end if

    value_of_real = real(sign_of_base, wp)*exponential_value*base_top/base_bot

 end subroutine string_to_real_wp

 !-----------------------------------------------------------------------------

 function count_int4_on_string(input_text) result(return_value)
   character(len=*), intent(in) :: input_text
   integer                      :: return_value
   ! Variables for internal use -----------------------------------------------
   integer                      :: int4_value
   character(len=255)           :: text
   integer                      :: length
   integer                      :: i
   integer                      :: error_state

   error_state = 0
   text = trim(input_text)
   length = 1
   i = 0
   ! Count total number of data int4 on string
   do
      ! For optimalisation, a different, more efficient routine may be
      ! constructed
      call string_to_int4(text, length, int4_value, error_state)
      if (error_state /= 0) then
         exit
      end if
      i = i + 1
      if (length == len_trim(text)) then
         exit
      end if
      text = text(length + 1:)
   end do
   return_value = i
 end function count_int4_on_string
 
 !-----------------------------------------------------------------------------

 function count_real_wp_on_string(input_text) result(return_value)
   character(len=*)      , intent(in)    :: input_text
   integer                               :: return_value
   ! Variables for internal use -----------------------------------------------
   real(wp)                              :: real_wp_value
   character(len=255)                    :: text
   integer                               :: length
   integer                               :: i
   integer                               :: error_state

   error_state = 0
   text = trim(input_text)
   length = 1
   i = 0
   ! Count total number of data int4 on string
   do
      ! For optimalisation, a different, more efficient routine may be
      ! constructed
      call string_to_real_wp(text, length, real_wp_value, error_state)
      if (error_state /= 0) then
         exit
      end if
      i = i + 1
      if (length == len_trim(text)) then
         exit
      end if
      text = text(length + 1:)
   end do
   return_value = i
 end function count_real_wp_on_string
 
 !------------------------------------------------------------------------------

 function read_nth_int4(input_text, nth) result(return_value)
   character(len=*), intent(in) :: input_text
   integer         , intent(in) :: nth
   integer                      :: return_value
   ! Variables for internal use -----------------------------------------------
   integer                      :: int4_value
   character(len=255)           :: text
   integer                      :: length
   integer                      :: i
   integer                      :: error_state

   error_state = 0
   text = trim(input_text)
   length = 1
   do i = 1, nth
      call string_to_int4(text, length, int4_value, error_state)
      if (error_state /= 0) then
         exit
      end if
      text = text(length + 1:)
   end do
   return_value = int4_value
 end function read_nth_int4
 
 !------------------------------------------------------------------------------
 
 subroutine read_n_last_int4(input_text, n, n_last_int4, num_int4_in)
   character(len=*)      , intent(in)    :: input_text
   integer               , intent(in)    :: n
   integer, optional     , intent(in)    :: num_int4_in
   integer, dimension(n) , intent(inout) :: n_last_int4
   ! Variables for internal use -----------------------------------------------
   integer, dimension(:), allocatable    :: all_int4
   integer                               :: num_int4
   integer                               :: int4_value
   character(len=255)                    :: text
   integer                               :: length
   integer                               :: i
   integer                               :: error_state

   error_state = 0
   text = trim(input_text)
   if (.not. present(num_int4_in)) then
      num_int4 = count_int4_on_string(text)
   else
      num_int4 = num_int4_in
   end if
   
   if (num_int4 < n) then
      print *, 'READ_N_LAST_INT4 - Error:'
      print *, '  String has too few int4.'
      print *, '  n = ',  n, 'num_data_int4 = ', num_int4
      stop 1
   end if
   allocate(all_int4(num_int4))
   
   ! Read again, but save int4
   text = input_text
   length = 1
   do i = 1, num_int4
      call string_to_int4(text, length, int4_value, error_state)
      all_int4(i) = int4_value
      if (error_state /= 0) then
         exit
      end if
      text = text(length + 1:)
   end do

   n_last_int4 = all_int4(num_int4 - n + 1:)
      
 end subroutine read_n_last_int4

 !-----------------------------------------------------------------------------

 subroutine read_n_last_real_wp(input_text, n, n_last_real_wp, num_real_wp_in)
   character(len=*)       , intent(in)    :: input_text
   integer                , intent(in)    :: n
   integer, optional      , intent(in)    :: num_real_wp_in
   real(wp), dimension(n) , intent(inout) :: n_last_real_wp
   ! Variables for internal use -----------------------------------------------
   real(wp), dimension(:), allocatable    :: all_real_wp
   integer                                :: num_real_wp
   real(wp)                               :: real_wp_value
   character(len=255)                     :: text
   integer                                :: length
   integer                                :: i
   integer                                :: error_state

   error_state = 0
   text = trim(input_text)
   length = 1
   if (present(num_real_wp_in)) then
      num_real_wp = num_real_wp_in
   else
      num_real_wp = count_real_wp_on_string(text)
   end if
   
   if (num_real_wp < n) then
      print *, 'READ_N_LAST_REAL_WP - Error:'
      print *, '  String has too few real_wp.'
      print *, '  n = ',  n, 'num_data_real_wp = ', num_real_wp
      stop 1
   end if
   allocate(all_real_wp(num_real_wp))
   
   ! Read again, but save real_wp
   text = input_text
   length = 1
   do i = 1, num_real_wp
      call string_to_real_wp(text, length, real_wp_value, error_state)
      all_real_wp(i) = real_wp_value
      if (error_state /= 0) then
         exit
      end if
      text = text(length + 1:)
   end do

   n_last_real_wp = all_real_wp(num_real_wp - n + 1:)

 end subroutine read_n_last_real_wp
 
 
 !!===================!
 ! Private procedures !
 !====================!=========================================================

 !!--------------!!
 ! Error handling !
 !----------------!-------------------------------------------------------------
 subroutine check_ioerr_opening(ioerr, iotxt, IOMSG_LEN, FILENAME, &
       stop_unit, action)
    integer                 , intent(in) :: ioerr, stop_unit
    integer                 , intent(in) :: IOMSG_LEN
    character(len=IOMSG_LEN), intent(in) :: iotxt
    character(len=*)        , intent(in) :: FILENAME, action

    if (ioerr /= 0) then
       write (ERROR_UNIT, *) 'Problem while opening for ', &
            action, ': ', FILENAME
       write (ERROR_UNIT, *) 'Message              : ', trim (iotxt)
       stop stop_unit
    end if
  end subroutine check_ioerr_opening
  
  
  subroutine check_ioerr_reading(ioerr, iotxt, IOMSG_LEN, FILENAME, stop_unit)
    integer                 , intent(in) :: ioerr, stop_unit
    integer                 , intent(in) :: IOMSG_LEN
    character(len=IOMSG_LEN), intent(in) :: iotxt
    character(len=*)        , intent(in) :: FILENAME
    
    if (ioerr /= IOSTAT_END) then
       write (ERROR_UNIT, *) 'Problem while reading: ', FILENAME
       write (ERROR_UNIT, *) 'Message              : ', trim (iotxt)
       stop stop_unit
    end if
  end subroutine check_ioerr_reading


  subroutine validate_nodes_and_elements(spatial_dim, element_order, num_nodes,&
       num_elements, nodes, elements)
    integer                                       , intent(in) :: spatial_dim
    integer                                       , intent(in) :: element_order
    integer                                       , intent(in) :: num_nodes
    integer                                       , intent(in) :: num_elements
    real(wp), dimension(num_nodes, spatial_dim)   , intent(in) :: nodes
    integer , dimension(num_elements, spatial_dim), intent(in) :: elements
    ! Variables for internal use -----------------------------------------------
    integer                                                    :: i
    integer                                                    :: j
    integer                                                    :: k

!!$    print *, ''
!!$    write (*, fmt="(a)", advance="no") &
!!$         '         Validating nodes and elements... '

    do i = 1, num_nodes
       do j = 1, spatial_dim
          if (ieee_is_nan(nodes(i, j))) then
             print *, ''
             print *, 'Error: Node coordinate is NAN..'
             print *, '  i: ', i, 'j: ', j
             stop 1
          else if (.not. ieee_is_finite(nodes(i, j))) then
             print *, ''
             print *, 'Error: Node coordinate is infinite..'
             print *, '  i: ', i, 'j: ', j
             stop 1
          end if
       end do
       do k = 1, num_nodes
          if (k /= i .and. all(nodes(k, :) == nodes(i, :))) then
             print *, ''
             print *, 'Error: Two identical nodes..'
             print *, '  i: ', i, 'k: ', k
             stop 1
          end if
       end do
    end do

    do i = 1, num_elements
       do j = 1, element_order
          if (elements(i, j) < 1) then
             print *, ''
             print *, 'Error: Node-index is less than 1..'
             print *, '  i: ', i, 'j: ', j
             stop 1
          else if (elements(i, j) > size(nodes, dim=1)) then
             print *, ''
             print *, 'Error: Node-index is larger than number of nodes..'
             print *, '  i: ', i, 'j: ', j
             stop 1
          end if
          do k = 1, element_order
             if (j /= k) then
                if (elements(i, j) == elements(i, k)) then
                   print *, ''
                   print *, 'Error: Element has two equal vertices.'
                   print *, '  i: ', i, 'j: ', j
                   stop 1
                end if
             end if
          end do
       end do
       do k = 1, num_elements
          if (k /= i .and. all(elements(k, :) == elements(i, :))) then
             print *, ''
             print *, 'Error: Two identical elements..'
             print *, '  i: ', i, 'k: ', k
             print *, elements(i, :)
             print *, elements(k, :)
             stop 1
          end if
       end do
    end do
          
  end subroutine validate_nodes_and_elements

  
  !!----------------!!
  ! Other procedures !
  !------------------!----------------------------------------------------------
  function capitalise_char(ch) result(return_value)
    character, intent(in) :: ch
    character             :: return_value
    integer               :: ascii_code
    ascii_code = iachar(ch)
    if (ascii_code >= 97 .and. ascii_code <= 122) then
       return_value = achar(ascii_code - 32)
    else
       return_value = ch
    end if
  end function capitalise_char


  !!------------------------!!
  ! Writing matrices to file !
  !--------------------------!--------------------------------------------------
  subroutine r8mat_write ( output_filename, m, n, table )
    !*************************************************************************80
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
    
    integer ( kind = 4 ) m
    integer ( kind = 4 ) n
    
    integer ( kind = 4 ) j
    character ( len = * ) output_filename
    integer ( kind = 4 ) output_status
    integer ( kind = 4 ) output_unit
    character ( len = 30 ) string
    real ( kind = 8 ) table(m,n)
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
       stop 1
    end if
    !
    !  Create a format string.
    !
    !  For less precision in the output file, try:
    !
    !                                        &
    !         '(', m, 'g', 14, '.', 6, ')'
    !
    if ( 0 < m .and. 0 < n ) then
       
       write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) &
            '(', n, 'g', 24, '.', 16, ')'
       !
       !  Write the data.
       !
       do j = 1, m
          write ( output_unit, string ) table(j, 1:n)
       end do
       
    end if
    !
    !  Close the file.
    !
    close ( unit = output_unit )
    
    return
  end subroutine r8mat_write

  !!----------------------------------------------------------------------------

  subroutine get_unit ( iunit )
    !*************************************************************************80
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
    
    integer ( kind = 4 ) i
    integer ( kind = 4 ) ios
    integer ( kind = 4 ) iunit
    logical ( kind = 4 ) lopen
    
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
  
  !!----------------------------------------------------------------------------
  
end module io_mod
