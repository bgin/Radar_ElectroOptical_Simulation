!===============================================================================
!    Copyright (c) 2015 Thomas E. Dunn
!
!    Permission is hereby granted, free of charge, to any person obtaining a copy of this
!    software and associated documentation files (the "Software"), to deal in the Software
!    without restriction, including without limitation the rights to use, copy, modify, merge,
!    publish, distribute, sublicense, and/or sell copies of the Software, and to permit
!    persons to whom the Software is furnished to do so, subject to the following conditions:
!    
!    The above copyright notice and this permission notice shall be included in all copies or
!    substantial portions of the Software.
!    
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
!    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
!    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
!    OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
!    DEALINGS IN THE SOFTWARE.
!
!    Modified by Cory Goates 2021
!
!===============================================================================
module linked_list_mod
  implicit none

  private

  ! public types:
  public :: list
  public :: node

  ! public operators:
  public :: assignment(=)
  public :: operator(==)

  ! public subroutines:
  public :: append
  public :: get_item
  public :: get_node

  ! derived types:
  type :: node
    private
    type(node), pointer :: next => null()
    class(*), allocatable :: item
  contains
    final :: node_finalizer
  end type node

  type :: list
    private
    integer :: num_nodes = 0
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()
  contains
    final :: list_finalizer
    procedure :: len => list_length
    procedure :: append => list_append_item
    procedure :: delete => list_delete_integer
    procedure :: clear => list_clear
    procedure :: print => list_print_integer
    procedure :: get_item_character => list_get_item_character
    procedure :: get_item_complex => list_get_item_complex
    procedure :: get_item_integer => list_get_item_integer
    procedure :: get_item_logical => list_get_item_logical
    procedure :: get_item_real => list_get_item_real
    generic :: get => get_item_character, get_item_complex, get_item_integer, get_item_logical, get_item_real
    procedure :: is_in => list_is_in_integer
  end type list

  ! interfaces:
  interface list
    module procedure :: list_constructor
  end interface list

  interface node
    module procedure :: node_constructor
  end interface node

  interface append
    module procedure :: list_append_item
  end interface append

  interface get_item
    module procedure :: list_get_item_character
    module procedure :: list_get_item_complex
    module procedure :: list_get_item_integer
    module procedure :: list_get_item_logical
    module procedure :: list_get_item_real
    module procedure :: node_get_item_character
    module procedure :: node_get_item_complex
    module procedure :: node_get_item_integer
    module procedure :: node_get_item_logical
    module procedure :: node_get_item_real
  end interface get_item

  interface get_node
    module procedure :: list_get_node
  end interface get_node

  interface assignment(=)
    module procedure :: node_assign_node_to_node
  end interface assignment(=)

  interface operator(==)
    module procedure :: node_equality_character_node
    module procedure :: node_equality_complex_node
    module procedure :: node_equality_integer_node
    module procedure :: node_equality_logical_node
    module procedure :: node_equality_real_node
    module procedure :: node_equality_node_character
    module procedure :: node_equality_node_complex
    module procedure :: node_equality_node_integer
    module procedure :: node_equality_node_logical
    module procedure :: node_equality_node_real
  end interface operator(==)
contains

!===============================================================================
!  list_append_item:
!
!    Adds an item to the end of the list.
!
  pure subroutine list_append_item(this, item)
    class(list), intent(inout) :: this
    class(*), intent(in) :: item

    ! If list is currently empty, add at the head
    if (this%num_nodes == 0) then

      ! Allocate memory
      allocate(this%head, source=node(item))
      
      ! Associate the tail
      this%tail => this%head

    ! Add to end of list
    else

      ! Allocate memory
      allocate(this%tail%next, source=node(item))

      ! Associate the tail
      this%tail => this%tail%next

    end if

    ! Update number of nodes
    this%num_nodes = this%num_nodes + 1

  end subroutine list_append_item
!===============================================================================

!===============================================================================
!  list_constructor:
!
!    Returns an uninitialized list.
!
  pure function list_constructor( ) result( val )
    type(list) :: val
  end function list_constructor
!===============================================================================

!===============================================================================
!  list_finalizer:
!
!    Finalizes the components of the given list.
!
  elemental subroutine list_finalizer( this )
    type(list), intent(inout) :: this

    this%num_nodes = 0
    if (associated(this%head)) nullify(this%head)
  end subroutine list_finalizer


  function list_is_in_integer(this, item) result(is_in)
    ! Checks if the integer item is in the list

    class(list),intent(in) :: this
    integer,intent(in) :: item
    type(node),pointer :: curr_node
    integer :: i
    logical :: is_in

    ! Loop through items in list
    is_in = .false.
    curr_node => this%head
    i = 1
    do while(associated(curr_node))
      
      ! Compare
      if (curr_node == item) then
        is_in = .true.
        return
      end if

      ! Move pointer forward
      curr_node => curr_node%next
      i = i + 1

    end do

  end function list_is_in_integer
!===============================================================================

!===============================================================================
!  list_get_item_character:
!
!    Gets the i-th node from this list and returns item value if it is a
!    character.
!
!      STAT   ERRMSG
!        -1   item found but not of type character
!        -2   node found but item not allocated
!        -3   node not found (iNode exceeds list bounds)
!
  subroutine list_get_item_character( this, iNode, chVal, stat, errmsg )
    class(list), intent(in) :: this
    integer, intent(in) :: iNode
    character(*), intent(out) :: chVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    type(node) :: nVal
    integer    :: istat

    call get_node(this, iNode, nVal, stat=istat)
    if (istat == 0) then
      call get_item(nVal, chVal, stat=istat)
    else
      istat = -3
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'item found but not of type character'
      case (-2)
        errmsg = 'node found but item not allocated'
      case (-3)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_item_character
!===============================================================================

!===============================================================================
!  list_get_item_complex:
!
!    Gets the i-th node from this list and returns item value if it is a
!    complex.
!
!      STAT   ERRMSG
!        -1   item found but not of type complex
!        -2   node found but item not allocated
!        -3   node not found (iNode exceeds list bounds)
!
  subroutine list_get_item_complex( this, iNode, cVal, stat, errmsg )
    class(list), intent(in) :: this
    integer, intent(in) :: iNode
    complex, intent(out) :: cVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    type(node) :: nVal
    integer    :: istat

    call get_node(this, iNode, nVal, stat=istat)
    if (istat == 0) then
      call get_item(nVal, cVal, stat=istat)
    else
      istat = -3
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'item found but not of type character'
      case (-2)
        errmsg = 'node found but item not allocated'
      case (-3)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_item_complex
!===============================================================================

!===============================================================================
!  list_get_item_integer:
!
!    Gets the i-th node from this list and returns item value if it is an
!    integer.
!
!      STAT   ERRMSG
!        -1   item found but not of type integer
!        -2   node found but item not allocated
!        -3   node not found (iNode exceeds list bounds)
!
  subroutine list_get_item_integer( this, iNode, iVal, stat, errmsg )
    class(list), intent(in) :: this
    integer, intent(in) :: iNode
    integer, intent(out) :: iVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    type(node) :: nVal
    integer    :: istat

    call get_node(this, iNode, nVal, stat=istat)
    if (istat == 0) then
      call get_item(nVal, iVal, stat=istat)
    else
      istat = -3
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'item found but not of type character'
      case (-2)
        errmsg = 'node found but item not allocated'
      case (-3)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_item_integer
!===============================================================================

!===============================================================================
!  list_get_item_logical:
!
!    Gets the i-th node from this list and returns item value if it is a
!    logical.
!
!      STAT   ERRMSG
!        -1   item found but not of type logical
!        -2   node found but item not allocated
!        -3   node not found (iNode exceeds list bounds)
!
  subroutine list_get_item_logical( this, iNode, lVal, stat, errmsg )
    class(list), intent(in) :: this
    integer, intent(in) :: iNode
    logical, intent(out) :: lVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    type(node) :: nVal
    integer    :: istat

    call get_node(this, iNode, nVal, stat=istat)
    if (istat == 0) then
      call get_item(nVal, lVal, stat=istat)
    else
      istat = -3
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'item found but not of type character'
      case (-2)
        errmsg = 'node found but item not allocated'
      case (-3)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_item_logical
!===============================================================================

!===============================================================================
!  list_get_item_real:
!
!    gets the i-th node from this list and returns item value if it is a
!    real.
!
!      STAT   ERRMSG
!        -1   item found but not of type real
!        -2   node found but item not allocated
!        -3   node not found (inode exceeds list bounds)
!
  subroutine list_get_item_real( this, inode, rval, stat, errmsg )
    class(list), intent(in) :: this
    integer, intent(in) :: inode
    real, intent(out) :: rval
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    type(node) :: nval
    integer    :: istat

    call get_node(this, inode, nval, stat=istat)
    if (istat == 0) then
      call get_item(nval, rval, stat=istat)
    else
      istat = -3
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'item found but not of type character'
      case (-2)
        errmsg = 'node found but item not allocated'
      case (-3)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_item_real


  subroutine list_clear(this)
    ! Removes all elements from this list

    implicit none
    
    class(list), intent(inout) :: this

    integer :: i
  
    ! Delete every item
    do while (this%len() > 0) 
      call this%get(1, i)
      call this%delete(i)
    end do
    
  end subroutine list_clear


  subroutine list_delete_integer(this, int)
    ! Deletes any items in the list which equal int

    implicit none

    class(list), intent(inout) :: this
    integer,intent(in) :: int

    type(node),pointer :: curr_node, prev_node, del_node
    integer :: curr_val

    ! Check if it's in the list
    if (.not. this%is_in(int)) then
      return
    end if

    ! Check if this is the only element in the list
    if (this%len() == 1) then

      ! Deallocate
      deallocate(this%head%item)
      deallocate(this%head)

      ! Reassign pointers
      this%head => null()
      this%tail => null()

      ! Update length
      this%num_nodes = 0

    else

      ! Start at the head
      curr_node => this%head
      prev_node => null()

      ! Loop through list
      do while (associated(curr_node))

        ! Get the value at the current list node
        call get_item(curr_node, curr_val)

        ! Check if this node is the value
        if (curr_val == int) then

          ! If we're deleting the head node, move the head pointer
          if (associated(curr_node, this%head)) then
            this%head => curr_node%next

          ! Otherwise, move the previous node's pointer
          else
            prev_node%next => curr_node%next
          end if

          ! If we're deleting the tail node, move the tail pointer
          if (associated(curr_node, this%tail)) then
            this%tail => prev_node
          end if

          ! Update length
          this%num_nodes = this%num_nodes - 1

          ! Move pointers (prev_node doesn't move if curr_node is getting deleted)
          del_node => curr_node
          curr_node => curr_node%next

          ! Deallocate
          deallocate(del_node%item)
          deallocate(del_node)

        else

          ! Move pointers
          prev_node => curr_node
          curr_node => curr_node%next

        end if

      end do
    end if

  end subroutine list_delete_integer


  subroutine list_print_integer(this)

    implicit none

    class(list),intent(in) :: this

    type(node),pointer :: curr_node, prev_node
    integer :: curr_val

    ! Start at the head
    curr_node => this%head
    prev_node => null()

    ! Loop through list
    write(*,'(a)',advance='no') '['
    do while (associated(curr_node))

      ! Get the value at the current list node
      call get_item(curr_node, curr_val)

      ! Print out that value
      write(*,'(i10)',advance='no') curr_val

      ! Move pointers
      prev_node => curr_node
      curr_node => curr_node%next

    end do
    write(*,'(a)') ']'
  
  end subroutine list_print_integer
!===============================================================================

!===============================================================================
!  list_get_node:
!
!    Gets the i-th node from this list.
!
!      STAT   ERRMSG
!        -1   node not found (iNode exceeds list bounds)
!
  subroutine list_get_node( this, iNode, nVal, stat, errmsg )
    type(list), intent(in) :: this
    integer, intent(in) :: iNode
    type(node), intent(out) :: nVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: i, istat
    type(node), pointer :: current_node


    if (iNode < 1) then
      istat = -1
    else if (iNode > this%len()) then
      istat = -1
    else
      istat = 0

      current_node => this%head
      do i = 2, iNode
        current_node => current_node%next
      end do
      nVal = current_node
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node not found (iNode exceeds list bounds)'
      case default
        errmsg = ''
      end select
    end if
  end subroutine list_get_node
!===============================================================================

!===============================================================================
!  list_length:
!
!    Returns the number of nodes in the given list
!
  elemental function list_length( self ) result( val )
    class(list), intent(in) :: self
    integer :: val

    val = self%num_nodes
  end function list_length
!===============================================================================

!===============================================================================
!  node_assign_node_to_node:
!
!    Returns .TRUE. if the given node has an item of type complex with value
!    equal to the given complex value.
!
  elemental subroutine node_assign_node_to_node( LHS, RHS )
    type(node), intent(inout) :: LHS
    type(node), intent(in) :: RHS

    if (allocated(LHS%item)) deallocate(LHS%item)
   if (allocated(RHS%item)) allocate(LHS%item, source=RHS%item)
  end subroutine node_assign_node_to_node
!===============================================================================

!===============================================================================
!  node_constructor:
!
!    Returns a node constructed from the given item.
!
  pure function node_constructor( item ) result( val )
    class(*), intent(in), optional :: item
    type(node) :: val

   if (present(item)) allocate(val%item, source=item)
  end function node_constructor
!===============================================================================

!===============================================================================
!  node_equality_character_node:
!
!    Returns .TRUE. if the given node has an item of type character with value
!    equal to the given complex value.
!
  elemental function node_equality_character_node( chVal, nVal ) result( ans )
    character(*), intent(in) :: chVal
    type(node), intent(in) :: nVal
    logical :: ans

    select type(item => nVal%item)
    type is (character(*))
      ans = item == chVal
    class default
      ans = .false.
    end select
  end function node_equality_character_node
!===============================================================================

!===============================================================================
!  node_equality_complex_node:
!
!    Returns .TRUE. if the given node has an item of type complex with value
!    equal to the given complex value.
!
  elemental function node_equality_complex_node( cVal, nVal ) result( ans )
    complex, intent(in) :: cVal
    type(node), intent(in) :: nVal
    logical :: ans

    select type(item => nVal%item)
    type is (complex)
      ans = item == cVal
    class default
      ans = .false.
    end select
  end function node_equality_complex_node
!===============================================================================

!===============================================================================
!  node_equality_integer_node:
!
!    Returns .TRUE. if the given node has an item of type integer with value
!    equal to the given integer value.
!
  elemental function node_equality_integer_node( iVal, nVal ) result( ans )
    integer, intent(in) :: iVal
    type(node), intent(in) :: nVal
    logical :: ans

    select type(item => nVal%item)
    type is (integer)
      ans = item == iVal
    class default
      ans = .false.
    end select
  end function node_equality_integer_node
!===============================================================================

!===============================================================================
!  node_equality_logical_node:
!
!    Returns .TRUE. if the given node has an item of type logical with value
!    equal to the given logical value.
!
  elemental function node_equality_logical_node( lVal, nVal ) result( ans )
    logical, intent(in) :: lVal
    type(node), intent(in) :: nVal
    logical :: ans

    select type(item => nVal%item)
    type is (logical)
      ans = item .eqv. lVal
    class default
      ans = .false.
    end select
  end function node_equality_logical_node
!===============================================================================

!===============================================================================
!  node_equality_real_node:
!
!    Returns .TRUE. if the given node has an item of type real with value
!    equal to the given real value.
!
  elemental function node_equality_real_node( rVal, nVal ) result( ans )
    real, intent(in) :: rVal
    type(node), intent(in) :: nVal
    logical :: ans

    select type(item => nVal%item)
    type is (real)
      ans = item == rVal
    class default
      ans = .false.
    end select
  end function node_equality_real_node
!===============================================================================

!===============================================================================
!  node_equality_node_character:
!
!    Returns .TRUE. if the given node has an item of type character with value
!    equal to the given character value.
!
  elemental function node_equality_node_character( nVal, chVal ) result( ans )
    type(node), intent(in) :: nVal
    character(*), intent(in) :: chVal
    logical :: ans

    select type(item => nVal%item)
    type is (character(*))
      ans = item == chVal
    class default
      ans = .false.
    end select
  end function node_equality_node_character
!===============================================================================

!===============================================================================
!  node_equality_node_complex:
!
!    Returns .TRUE. if the given node has an item of type complex with value
!    equal to the given complex value.
!
  elemental function node_equality_node_complex( nVal, cVal ) result( ans )
    type(node), intent(in) :: nVal
    complex, intent(in) :: cVal
    logical :: ans

    select type(item => nVal%item)
    type is (complex)
      ans = item == cVal
    class default
      ans = .false.
    end select
  end function node_equality_node_complex
!===============================================================================

!===============================================================================
!  node_equality_node_integer:
!
!    Returns .TRUE. if the given node has an item of type integer with value
!    equal to the given integer value.
!
  elemental function node_equality_node_integer( nVal, iVal ) result( ans )
    type(node), intent(in) :: nVal
    integer, intent(in) :: iVal
    logical :: ans

    select type(item => nVal%item)
    type is (integer)
      ans = item == iVal
    class default
      ans = .false.
    end select
  end function node_equality_node_integer
!===============================================================================

!===============================================================================
!  node_equality_node_logical:
!
!    Returns .TRUE. if the given node has an item of type logical with value
!    equal to the given logical value.
!
  elemental function node_equality_node_logical( nVal, lVal ) result( ans )
    type(node), intent(in) :: nVal
    logical, intent(in) :: lVal
    logical :: ans

    select type(item => nVal%item)
    type is (logical)
      ans = item .eqv. lVal
    class default
      ans = .false.
    end select
  end function node_equality_node_logical
!===============================================================================

!===============================================================================
!  node_equality_node_real:
!
!    Returns .TRUE. if the given node has an item of type real with value
!    equal to the given real value.
!
  elemental function node_equality_node_real( nVal, rVal ) result( ans )
    type(node), intent(in) :: nVal
    real, intent(in) :: rVal
    logical :: ans

    select type(item => nVal%item)
    type is (real)
      ans = item == rVal
    class default
      ans = .false.
    end select
  end function node_equality_node_real
!===============================================================================

!===============================================================================
!  node_finalizer:
!
!    Finalizes the components of the given node.
!
  elemental subroutine node_finalizer( this )
    type(node), intent(inout) :: this

    if (associated(this%next)) nullify(this%next)
    if (allocated(this%item)) deallocate(this%item)
  end subroutine node_finalizer
!===============================================================================

!===============================================================================
!  node_get_item_character:
!
!    Returns .TRUE. if the given node has an item of type character with value
!    equal to the given character value.
!
!      STAT   ERRMSG
!        -1   node item not of type character
!        -2   node item not allocated
!
  elemental subroutine node_get_item_character( this, sVal, stat, errmsg )
    type(node), intent(in) :: this
    character(*), intent(out) :: sVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: istat

    if (allocated(this%item)) then
      select type (item => this%item)
      type is (character(*))
        sVal = item
        istat = 0
      class default
        istat = -1
      end select
    else
      istat = -2
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node item is not of type character'
      case (-2)
        errmsg = 'node item is not allocated'
      case default
        errmsg = ''
      end select
    end if
  end subroutine node_get_item_character
!===============================================================================

!===============================================================================
!  node_get_item_complex:
!
!    Returns .TRUE. if the given node has an item of type complex with value
!    equal to the given complex value.
!
!      STAT   ERRMSG
!        -1   node item not of type complex
!        -2   node item not allocated
!
  elemental subroutine node_get_item_complex( this, cVal, stat, errmsg )
    type(node), intent(in) :: this
    complex, intent(out) :: cVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: istat

    if (allocated(this%item)) then
      select type (item => this%item)
      type is (complex)
        cVal = item
        istat = 0
      class default
        istat = -1
      end select
    else
      istat = -2
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node item is not of type complex'
      case (-2)
        errmsg = 'node item is not allocated'
      case default
        errmsg = ''
      end select
    end if
  end subroutine node_get_item_complex
!===============================================================================

!===============================================================================
!  node_get_item_integer:
!
!    Returns .TRUE. if the given node has an item of type integer with value
!    equal to the given integer value.
!
!      STAT   ERRMSG
!        -1   node item not of type integer
!        -2   node item not allocated
!
  elemental subroutine node_get_item_integer( this, iVal, stat, errmsg )
    type(node), intent(in) :: this
    integer, intent(out) :: iVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: istat

    if (allocated(this%item)) then
      select type (item => this%item)
      type is (integer)
        iVal = item
        istat = 0
      class default
        istat = -1
      end select
    else
      istat = -2
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node item is not of type integer'
      case (-2)
        errmsg = 'node item is not allocated'
      case default
        errmsg = ''
      end select
    end if
  end subroutine node_get_item_integer
!===============================================================================

!===============================================================================
!  node_get_item_logical:
!
!    Returns .TRUE. if the given node has an item of type logical with value
!    equal to the given logical value.
!
!      STAT   ERRMSG
!        -1   node item not of type logical
!        -2   node item not allocated
!
  elemental subroutine node_get_item_logical( this, lVal, stat, errmsg )
    type(node), intent(in) :: this
    logical, intent(out) :: lVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: istat

    if (allocated(this%item)) then
      select type (item => this%item)
      type is (logical)
        lVal = item
        istat = 0
      class default
        istat = -1
      end select
    else
      istat = -2
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node item is not of type logical'
      case (-2)
        errmsg = 'node item is not allocated'
      case default
        errmsg = ''
      end select
    end if
  end subroutine node_get_item_logical
!===============================================================================

!===============================================================================
!  node_get_item_real:
!
!    Returns .TRUE. if the given node has an item of type real with value
!    equal to the given real value.
!
!      STAT   ERRMSG
!        -1   node item not of type real
!        -2   node item not allocated
!
  elemental subroutine node_get_item_real( this, rVal, stat, errmsg )
    type(node), intent(in) :: this
    real, intent(out) :: rVal
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! local variables:
    integer :: istat

    if (allocated(this%item)) then
      select type (item => this%item)
      type is (real)
        rVal = item
        istat = 0
      class default
        istat = -1
      end select
    else
      istat = -2
    end if

    if (present(stat)) stat = istat

    if (present(errmsg)) then
      select case (istat)
      case (-1)
        errmsg = 'node item is not of type real'
      case (-2)
        errmsg = 'node item is not allocated'
      case default
        errmsg = ''
      end select
    end if
  end subroutine node_get_item_real
!===============================================================================
end module linked_list_mod
!===============================================================================