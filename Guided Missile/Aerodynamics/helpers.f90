
!Copyright (c) 2021 USU Aero Lab

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

module helpers_mod
     ! ************Slightly modified by Bernard Gingold, 30/03/2024, 10:20AM**************
    use mod_kinds, only : i4,sp
    implicit none

    logical :: run_checks, verbose

    type progress_bar
        ! NOT WORKING

        integer(kind=i4):: N_total, N_complete

        contains

            procedure :: init => progress_bar_init
            procedure :: advance => progress_bar_advance

    end type progress_bar
    
contains

    subroutine check_allocation(stat, what)

        implicit none

        integer(kind=i4),intent(in) :: stat
        character(len=*),intent(in),optional :: what

        ! Check if stat is nonzero
        if (stat /= 0) then
            if (present(what)) then
                write(*,*) "!!! Your computer has insufficient memory to allocate ", what, ". Quitting..."
            else
                write(*,*) "!!! Your computer has insufficient memory. Quitting..."
            end if
            stop
        end if
    
    end subroutine check_allocation


    function mirror_across_plane(vec, plane) result(mirrored_vec)
        ! Returns a version of the given vector mirrored about the given plane
        ! The plane number is the component index which is normal to the plane (i.e. 1: yz plane, etc.)

        implicit none

        real(kind=sp),dimension(3),intent(in) :: vec
        integer(kind=i4),intent(in) :: plane
        real(kind=sp),dimension(3) :: mirrored_vec

        mirrored_vec = vec
        mirrored_vec(plane) = -vec(plane)
        
    end function mirror_across_plane


    subroutine invert_permutation_vector(N, P, P_inv)
        ! Inverts the given permutation vector

        implicit none
        
        integer(kind=i4),intent(in) :: N
        integer(kind=i4),dimension(N),intent(in) :: P
        integer(kind=i4),dimension(:),allocatable,intent(out) :: P_inv

        integer(kind=i4):: i

        ! Invert
        allocate(P_inv(N))
        do i=1,N
            P_inv(P(i)) = i
        end do
        
    end subroutine invert_permutation_vector


    subroutine progress_bar_init(this, N_total)
        ! Sets up the progress bar

        implicit none

        class(progress_bar),intent(inout) :: this
        integer(kind=i4),intent(in) :: N_total

        ! Initialize
        this%N_total = N_total
        this%N_complete = 0

        ! Print out initial
        write(*,'(i3, a)',advance='no') 0, "% complete."
    
    end subroutine progress_bar_init


    subroutine progress_bar_advance(this)
        ! Advances the progress bar by one

        implicit none

        class(progress_bar),intent(inout) :: this

        ! Update number complete
        this%N_complete = this%N_complete + 1

        ! Print progress
        write(*,'(tl13, i3, a)',advance='no') 100.0*real(kind=sp)(this%N_complete)/real(kind=sp)(this%N_total), "% complete."

        ! Write newline
        if (this%N_complete == this%N_total) then
            write(*,*)
        end if
    
    end subroutine progress_bar_advance


    subroutine delete_file(filename)
        ! Deletes the given file if it exists

        implicit none

        character(len=:),allocatable,intent(in) :: filename

        integer(kind=i4):: stat

        open(unit=13, iostat=stat, file=filename)
        if (stat == 0) close(13, status='delete')
    
    end subroutine delete_file

    
end module helpers_mod
