! These various sort routines are provided as
! examples, rather than being fully optimised
! for the best possible performance. MJR 9/2019
!
! So far the collection is:
!
!  bubble_sort           (order N**2)
!  selection_sort        (order N**2)
!  inline_selection_sort (order N**2)
!  insertion_sort        (order N**2)
!  binary_insertion_sort (order N**2)
!  pair_insertion_sort   (order N**2)
!  double_insertion_sort (order N**2)
!  shell_sort            (order N**(4/3)) (possibly)
!  merge_sort            (order N log N)
!  alt_merge_sort        (order N log N)
!  heap_sort             (order N log N)
!  heap_sort_nr          (order N log N)
!  smoothsort            (order N log N)
!  quicksort             (order N log N) (usually)
!  quicksort_nr          (order N log N) (usually)
!  dpquicksort           (order N log N) (usually)
!
! 14/10/19 and 18/10/19 indexing errors in heap sorts corrected
!
! 4/4/20 smooth sort and non-recursive quicksort added, and some
!        minor code tidying
!
! Pulled from https://www.mjr19.org.uk/IT/sorts/ by Cory Goates 2/17/22
!
! What I need is a sorting routine which will, rather than sorting the array,
! give an array of indices which will sort the array. CDG


module sort_mod

  implicit none

  !integer, parameter :: prec=kind(1d0) ! I'd prefer to use a compiler option CDG
  integer,parameter :: i64=selected_int_kind(15)

  ! array L used by smoothsort

  integer, parameter :: L(0:43) = (/ 1, 1, 3, 5, 9, 15, 25, 41, 67, 109, &
       177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, &
       13529, 21891, 35421, 57313, 92735, 150049, &
       242785, 392835, 635621, 1028457, 1664079, 2692537, &
       4356617, 7049155, 11405773, 18454929, 29860703, &
       48315633, 78176337, 126491971, 204668309, 331160281, &
       535828591, 866988873, 1402817465 /)


  private :: binary_search, merge_arrays, heapify, heapify_nr, &
       smooth_heapify, smooth_extract, sift_in, interheap_sift, L

contains

  recursive subroutine quicksort(array)
    real, intent(inout)::array(:)
    real :: temp,pivot
    integer :: i,j,last,left,right

    last=size(array)

    if (last.lt.50) then ! use insertion sort on small arrays
       do i=2,last
          temp=array(i)
          do j=i-1,1,-1
             if (array(j).le.temp) exit
             array(j+1)=array(j)
          enddo
          array(j+1)=temp
       enddo
       return
    endif
    ! find median of three pivot
    ! and place sentinels at first and last elements
    temp=array(last/2)
    array(last/2)=array(2)
    if (temp.gt.array(last)) then
       array(2)=array(last)
       array(last)=temp
    else
       array(2)=temp
    endif
    if (array(1).gt.array(last)) then
       temp=array(1)
       array(1)=array(last)
       array(last)=temp
    endif
    if (array(1).gt.array(2)) then
       temp=array(1)
       array(1)=array(2)
       array(2)=temp
    endif
    pivot=array(2)

    left=3
    right=last-1
    do
       do while(array(left).lt.pivot)
          left=left+1
       enddo
       do while(array(right).gt.pivot)
          right=right-1
       enddo
       if (left.ge.right) exit
       temp=array(left)
       array(left)=array(right)
       array(right)=temp
       left=left+1
       right=right-1
    enddo
    if (left.eq.right) left=left+1
    call quicksort(array(1:left-1))
    call quicksort(array(left:))

  end subroutine quicksort

  ! This version maintains its own stack, to avoid needing to call
  ! itself recursively. By always pushing the larger "half" to the
  ! stack, and moving directly to calculate the smaller "half",
  ! it can guarantee that the stack needs no more than log_2(N)
  ! entries
  subroutine quicksort_nr(array)
    real, intent(inout)::array(:)
    real :: temp,pivot
    integer :: i,j,left,right,low,high
    ! If your compiler lacks storage_size(), replace
    ! storage_size(i) by 64
    integer :: stack(2,storage_size(i)),stack_ptr

    low=1
    high=size(array)
    stack_ptr=1

    do

       if (high-low.lt.50) then ! use insertion sort on small arrays
          do i=low+1,high
             temp=array(i)
             do j=i-1,low,-1
                if (array(j).le.temp) exit
                array(j+1)=array(j)
             enddo
             array(j+1)=temp
          enddo
          ! now pop from stack
          if (stack_ptr.eq.1) return
          stack_ptr=stack_ptr-1
          low=stack(1,stack_ptr)
          high=stack(2,stack_ptr)
          cycle
       endif

       ! find median of three pivot
       ! and place sentinels at first and last elements
       temp=array((low+high)/2)
       array((low+high)/2)=array(low+1)
       if (temp.gt.array(high)) then
          array(low+1)=array(high)
          array(high)=temp
       else
          array(low+1)=temp
       endif
       if (array(low).gt.array(high)) then
          temp=array(low)
          array(low)=array(high)
          array(high)=temp
       endif
       if (array(low).gt.array(low+1)) then
          temp=array(low)
          array(low)=array(low+1)
          array(low+1)=temp
       endif
       pivot=array(low+1)

       left=low+2
       right=high-1
       do
          do while(array(left).lt.pivot)
             left=left+1
          enddo
          do while(array(right).gt.pivot)
             right=right-1
          enddo
          if (left.ge.right) exit
          temp=array(left)
          array(left)=array(right)
          array(right)=temp
          left=left+1
          right=right-1
       enddo
       if (left.eq.right) left=left+1
       !          call quicksort(array(1:left-1))
       !          call quicksort(array(left:))
       if (left.lt.(low+high)/2) then
          stack(1,stack_ptr)=left
          stack(2,stack_ptr)=high
          stack_ptr=stack_ptr+1
          high=left-1
       else
          stack(1,stack_ptr)=low
          stack(2,stack_ptr)=left-1
          stack_ptr=stack_ptr+1
          low=left
       endif

    enddo
  end subroutine quicksort_nr


  ! dual pivot quicksort
  recursive subroutine dpquicksort(array)
    real, intent(inout)::array(:)
    real :: temp,p1,p2
    integer :: i,j,last,l,k,g

    last=size(array)

    if (last.lt.40) then ! use insertion sort on small arrays
       do i=2,last
          temp=array(i)
          do j=i-1,1,-1
             if (array(j).le.temp) exit
             array(j+1)=array(j)
          enddo
          array(j+1)=temp
       enddo
       return
    endif
    p1=array(last/3)
    p2=array(2*last/3)
    if (p2.lt.p1) then
       temp=p1
       p1=p2
       p2=temp
    endif
    array(last/3)=array(1)
    array(1)=p1
    array(2*last/3)=array(last)
    array(last)=p2

    g=last
    l=2
    do while (array(l).lt.p1)
       l=l+1
    enddo
    k=l

    do while(k.lt.g)
       temp=array(k)
       if (temp.lt.p1) then
          array(k)=array(l)
          array(l)=temp
          l=l+1
       else if (temp.gt.p2) then
          do while(array(g-1).gt.p2)
             g=g-1
          enddo
          if (k.ge.g) exit
          g=g-1
          if (array(g).lt.p1) then
             array(k)=array(l)
             array(l)=array(g)
             array(g)=temp
             l=l+1
          else
             array(k)=array(g)
             array(g)=temp
          endif
       endif
       k=k+1
    enddo
    if (l.gt.2) then
       array(1)=array(l-1)
       array(l-1)=p1
       call dpquicksort(array(1:l-2))
    endif
    call dpquicksort(array(l:g-1))
    if (g.lt.last) then
       array(last)=array(g)
       array(g)=p2
       call dpquicksort(array(g+1:last))
    endif

  end subroutine dpquicksort

  function binary_search(array, x, start, end)
    integer binary_search
    integer, intent(in) :: start,end
    real, intent(in) :: x,array(:)
    integer :: a,b,mid

    a=start
    b=end

    do
       if (a.eq.b) then
          if (array(a).gt.x) then
             binary_search=a
             return 
          else
             binary_search=a+1
             return
          endif
       endif
       if (a.gt.b) then
          binary_search=a
          return
       end if


       mid=(a+b)/2
       if (array(mid).lt.x) then
          a=mid+1
       else if (array(mid).gt.x) then
          b=mid-1
       else
          binary_search=mid
          return
       endif

    end do
  end function binary_search

  subroutine binary_insertion_sort(array)
    real, intent(inout) :: array(:)
    integer :: i,j,pos
    real :: x

    do i=2,size(array)
       x=array(i)
       pos=binary_search(array,x,1,i-1)
       do j=i,pos+1,-1
          array(j)=array(j-1)
       enddo
       array(pos)=x
    enddo
  end subroutine binary_insertion_sort


  subroutine insertion_sort(array)
    real,intent(inout) :: array(:)
    integer :: i,j
    real :: temp

    do i=2,size(array)
       temp = array(i)
       do j=i-1,1,-1
          if (array(j) <= temp) exit
          array(j+1)=array(j)
       end do
       array(j+1)=temp
    enddo
  end subroutine insertion_sort


  subroutine insertion_arg_sort(array, i_sorted)
   ! Returns a list of indices which will sort the given array

   implicit none

    real,intent(in) :: array(:)
    integer,dimension(:),allocatable,intent(out) :: i_sorted

    integer :: i, j, N, temp

    ! Allocate unsorted indices
    N = size(array)
    allocate(i_sorted(N))
    do i=1,N
      i_sorted(i) = i
    end do

    ! Sort
    do i=2,N
       temp = i_sorted(i)
       do j=i-1,1,-1
          if (array(i_sorted(j)) <= array(temp)) then
            exit
          else
            i_sorted(j+1) = i_sorted(j)
          end if
       end do
       i_sorted(j+1) = temp
    end do
  end subroutine insertion_arg_sort


  subroutine double_insertion_sort(array)
    real, intent(inout) :: array(:)
    integer :: i,j,left,right,last
    real :: temp,p,next

    last=size(array)

    p=0.5*(array(1)+array(last))
    if (array(1).gt.array(last)) then
       temp=array(last)
       array(last)=array(1)
       array(1)=temp
    endif

    left=1
    right=last
    temp=array(2)

    do i=2,last-1
       if (temp.lt.p) then
          do j=left,1,-1
             if (array(j).le.temp) exit
             array(j+1)=array(j)
          enddo
          array(j+1)=temp
          temp=array(left+2)
          left=left+1
       else
          next=array(right-1)
          do j=right,last
             if (array(j).ge.temp) exit
             array(j-1)=array(j)
          enddo
          array(j-1)=temp
          temp=next
          right=right-1
       endif
    enddo

  end subroutine double_insertion_sort


  subroutine pair_insertion_sort(array)
    real, intent(inout) :: array(:)
    integer :: i,j,last
    real :: t1,t2

    last=size(array)
    do i=2,last-1,2
       t1=min(array(i),array(i+1))
       t2=max(array(i),array(i+1))
       j=i-1
       do while((j.ge.1).and.(array(j).gt.t2))
          array(j+2)=array(j)
          j=j-1
       enddo
       array(j+2)=t2
       do while((j.ge.1).and.(array(j).gt.t1))
          array(j+1)=array(j)
          j=j-1
       enddo
       array(j+1)=t1
    end do

    if(mod(last,2).eq.0)then
       t1=array(last)
       do j=last-1,1,-1
          if (array(j).le.t1) exit
          array(j+1)=array(j)
       end do
       array(j+1)=t1
    endif

  end subroutine pair_insertion_sort


  subroutine merge_arrays(a,b,out)
    ! routine used by merge_sort()
    real, intent(in) :: a(:),b(:)
    real, intent(out) :: out(:)
    integer :: ai,bi,oi,i

    ai=1
    bi=1
    oi=1

    do
       if (ai.gt.size(a)) then
          do i=bi,size(b)
             out(oi)=b(i)
             oi=oi+1
          enddo
          return
       endif
       if (bi.gt.size(b)) then
          do i=ai,size(a)
             out(oi)=a(i)
             oi=oi+1
          enddo
          return
       endif

       if (a(ai).lt.b(bi)) then
          out(oi)=a(ai)
          ai=ai+1
       else
          out(oi)=b(bi)
          bi=bi+1
       endif
       oi=oi+1
    enddo
  end subroutine merge_arrays

  recursive subroutine merge_sort(array)
    ! With a little skill, the extra copy of "array=temp"
    ! can be avoided
    real, intent(inout) :: array(:)
    real, allocatable :: temp(:)
    integer :: last

    last=size(array)

    if (last.lt.40) then
       call insertion_sort(array)
       return
    endif

    call merge_sort(array(1:last/2))
    call merge_sort(array(last/2+1:last))
    allocate(temp(last))
    call merge_arrays(array(1:last/2),array(last/2+1:last),temp)
    array=temp
    deallocate(temp)
  end subroutine merge_sort

  recursive subroutine alt_merge_sort(array)
    ! Alternate merges between array and temp
    ! to avoid copy
    real, intent(inout) :: array(:)
    real, allocatable :: temp(:)
    integer :: last

    last=size(array)

    if (last.lt.40) then
       call insertion_sort(array)
       return
    endif

    call alt_merge_sort(array(1:last/4))
    call alt_merge_sort(array(last/4+1:last/2))
    call alt_merge_sort(array(last/2+1:last/2+last/4))
    call alt_merge_sort(array(last/2+last/4+1:last))

    allocate(temp(last))
    call merge_arrays(array(1:last/4),array(last/4+1:last/2),temp(1:last/2))
    call merge_arrays(array(last/2+1:last/2+last/4),  &
         array(last/2+last/4+1:last),temp(last/2+1:last))
    call merge_arrays(temp(1:last/2),temp(last/2+1:last),array)
    deallocate(temp)
  end subroutine alt_merge_sort

  subroutine selection_sort(array)
    ! This version uses Fortran's minloc intrinsic
    real, intent(inout) :: array(:)
    real :: temp
    integer :: i,j

    do i=1,size(array)-1
       j=minloc(array(i:),1)+i-1
       temp=array(i)
       array(i)=array(j)
       array(j)=temp
    enddo

  end subroutine selection_sort

  subroutine inline_selection_sort(array)
    ! This version does not use Fortran's minloc intrinsic
    real, intent(inout) :: array(:)
    real :: temp
    integer :: i,j,k

    do i=1,size(array)-1
       temp=array(i)
       j=i
       do k=i+1,size(array)
          if (array(k).lt.temp) then
             temp=array(k)
             j=k
          endif
       enddo
       array(j)=array(i)
       array(i)=temp
    enddo

  end subroutine inline_selection_sort

  subroutine shell_sort(array)
    ! There are many choices of intervals
    ! The sequence (3**k-1)/2 used below is given by Knuth
    ! The sort relies on the insertion sort of nearly sorted
    ! data being fast (i.e. not order N**2)
    real, intent(inout) :: array(:)
    integer :: i,interval,last

    interval=1
    last=size(array)
    do while(interval<last/3)
       interval=3*interval+1
    enddo

    do while(interval>0)
       do i=1,interval
          ! The following syntax passes a subarray with elements
          ! array(i),array(i+interval),array(i+2*interval),...
          call insertion_sort(array(i::interval))
       enddo
       interval=(interval-1)/3
    enddo

  end subroutine shell_sort

  subroutine bubble_sort(array)

    real, intent(inout) :: array(:)
    real :: temp
    integer :: i,j,last

    last=size(array)
    do i=last-1,1,-1
       do j=1,i
          if (array(j+1).lt.array(j)) then
             temp=array(j+1)
             array(j+1)=array(j)
             array(j)=temp
          endif
       enddo
    enddo

  end subroutine bubble_sort

  subroutine heap_sort(array)
    real, intent(inout) :: array(:)
    real :: temp
    integer :: i,last

    last=size(array)

    ! Build heap
    do i=last/2,1,-1
       call heapify(array,i)
    enddo

    ! Unpick heap
    do i=last,2,-1
       temp=array(1)
       array(1)=array(i)
       array(i)=temp
       call heapify(array(1:i-1),1)
    end do

  end subroutine heap_sort

  recursive subroutine heapify(array,root)
    real, intent(inout) :: array(:)
    real :: temp
    integer :: left,right,root,last,largest

    last=size(array)
    left=2*root
    right=left+1
    largest=root

    if (left.le.last) then
       if(array(left).gt.array(largest)) largest=left
    endif

    if (right.le.last) then
       if(array(right).gt.array(largest)) largest=right
    endif

    if (largest.ne.root) then
       temp=array(root)
       array(root)=array(largest)
       array(largest)=temp
       call heapify(array,largest)
    end if

  end subroutine heapify


  subroutine heap_sort_nr(array)
    ! The same but with a non-recursive heapify
    real, intent(inout) :: array(:)
    real :: temp
    integer :: i,last

    last=size(array)

    ! Build heap
    do i=last/2,1,-1
       call heapify_nr(array,i)
    enddo

    ! Unpick heap
    do i=last,2,-1
       temp=array(1)
       array(1)=array(i)
       array(i)=temp
       call heapify_nr(array(1:i-1),1)
    end do

  end subroutine heap_sort_nr

  subroutine heapify_nr(array,i)
    ! A non-recursive heapify
    real, intent(inout) :: array(:)
    real :: root_val
    integer :: i,left,right,root,last,largest

    last=size(array)
    root=i
    left=2*root
    root_val=array(root)
    largest=root

    do while(left.le.last)
       right=left+1

       if (left.le.last) then
          if(array(left).gt.array(largest)) largest=left
       endif

       if (right.le.last) then
          if(array(right).gt.array(largest)) largest=right
       endif

       if (largest.eq.root) exit

       array(root)=array(largest)
       array(largest)=root_val
       root=largest
       left=2*root
    enddo

    !    array(largest)=root_val

  end subroutine heapify_nr

  ! Smoothsort is introduced by Edsger Dijkstra
  ! https://doi.org/10.1016/0167-6423(82)90016-8
  !
  ! This Fortran version owes much to https://www.keithschwarz.com/smoothsort/
  ! and the C version given by Martin Knoblauch Revuelta at
  ! http://code.google.com/archive/p/combsortcs2p-and-other-sorting-algorithms

  subroutine smoothsort(array)
    real, intent(inout) :: array(:)
    integer(i64) :: mask
    integer :: offset

    if (size(array).le.1) return

    call smooth_heapify(array,mask,offset)

    call smooth_extract(array,mask,offset)
  end subroutine smoothsort

  subroutine smooth_heapify(array,mask,offset)
    real, intent(inout) :: array(0:)
    integer(i64), intent(out) :: mask
    integer, intent(out) :: offset
    integer :: i,last
    logical :: wbf

    mask=1
    offset=1
    last=size(array)

    do i=1,last-1
       if (iand(mask,2_i64).eq.2) then
          mask=ior(ishft(mask,-2),1_i64)
          offset=offset+2
       else if (offset.eq.1) then
          mask=ior(ishft(mask,1),1_i64)
          offset=0
       else
          mask=ior(ishft(mask,offset-1),1_i64)
          offset=1
       endif
       wbf=(((iand(mask,2_i64).eq.2).and.(i+1.lt.last)).or. &
            ((offset.gt.0).and.(1+i+L(offset-1).lt.last)))
       if (wbf) then
          call sift_in(array,i,offset)
       else
          call interheap_sift(array,i,mask,offset)
       endif
    enddo

  end subroutine smooth_heapify
  subroutine smooth_extract(array,mask,offset)
    real, intent(inout) :: array(0:)
    integer :: i,j,last,ch(2)
    integer(i64),value :: mask
    integer, value :: offset

    last=size(array)

    do i=last-1,2,-1
       if (offset.lt.2) then
          do
             mask=ishft(mask,-1)
             offset=offset+1
             if (iand(mask,1_i64).eq.1) exit
          enddo
       else
          ch(2)=i-1
          ch(1)=ch(2)-L(offset-2)
          mask=iand(mask,not(1_i64))
          do j=1,2
             mask=ior(ishft(mask,1),1_i64)
             offset=offset-1
             call interheap_sift(array,ch(j),mask,offset)
          end do
       end if
    end do


  end subroutine smooth_extract


  subroutine sift_in(array,root,sz)
    real, intent(inout) :: array(0:)
    real :: tmp
    integer :: left,right,next
    integer,value :: root,sz

    if (sz.lt.2) return

    tmp=array(root)

    do
       right=root-1
       left=right-L(sz-2)

       if (array(right).lt.array(left)) then
          next=left
          sz=sz-1
       else
          next=right
          sz=sz-2
       end if

       if (array(next).le.tmp) exit

       array(root)=array(next)

       root=next
       if (sz.le.1) exit
    end do

    array(root)=tmp

  end subroutine sift_in

  subroutine interheap_sift(array,root,mask,offset)
    real, intent(inout) :: array(0:)
    real :: tmp,mx
    integer :: left,right,next
    integer(i64),value :: mask
    integer, value :: root,offset

    tmp=array(root)

    do while (mask.ne.1)
       mx=tmp

       if (offset.gt.1) then
          right=root-1
          left=right-L(offset-2)
          mx=max(mx,array(left),array(right))
       end if

       next=root-L(offset)

       if (array(next).le.mx) exit

       array(root)=array(next)
       root=next

       do
          mask=ishft(mask,-1)
          offset=offset+1
          if (iand(mask,1_i64).eq.1) exit
       end do
    end do


    array(root)=tmp
    call sift_in(array,root,offset)
  end subroutine interheap_sift


end module sort_mod
