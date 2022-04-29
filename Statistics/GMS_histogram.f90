MODULE histogram

  ! This module calculates the Histogram of data
  ! and is part of the JAMS Fortran library.
  !
  ! Written  Juliane Mai, Feb 2012
  ! @Modified by Bernard Gingold on 29-05-2022 10:58 GMT+2
  ! Minor changes added: Ifort directives, minor optimizations.

  ! License
  ! -------
  ! This file is part of the JAMS Fortran package, distributed under the MIT License.
  !
  ! Copyright (c) 2012 Juliane Mai
  !
  ! Permission is hereby granted, free of charge, to any person obtaining a copy
  ! of this software and associated documentation files (the "Software"), to deal
  ! in the Software without restriction, including without limitation the rights
  ! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  ! copies of the Software, and to permit persons to whom the Software is
  ! furnished to do so, subject to the following conditions:
  !
  ! The above copyright notice and this permission notice shall be included in all
  ! copies or substantial portions of the Software.
  !
  ! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  ! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  ! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  ! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  ! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  ! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  ! SOFTWARE.

  USE mod_kinds, ONLY: i4, sp, dp

  IMPLICIT NONE


  PUBLIC :: histo           ! Histogram of 1D data
  PUBLIC :: histo2d         ! Histogram of 2D data

  ! ------------------------------------------------------------------

  !     NAME
  !         histo

  !     PURPOSE
  !         Calculates the histogram of n data 1D points (Xi),i=1,n ,
  !         i.e. the n data points are sorted into k categories (bins).
  !         In case of multidimensional data points (Xij),j=1,n ,
  !         only the first coordinate (X1j) is taken for determination of the category,
  !         while the other coordinates are averged per bin.
  !
  !         With no optional arguments the number of bins k is the integer part of Sqrt(n)
  !         where n is the number of data points. E.g. n=15 will create k=3 bins.
  !         The bin width w is calculated depending on the minimal and maximal value of
  !         the first coordinate):
  !                     w = (maxval(X) - minval(X)) / k
  !
  !         Optionally, one can fix the number of bins k (integer i4)
  !         while the width of the bins is determined.
  !         Or, on can fix the binwidth w (float sp/dp) while k is determined automatically.
  !         If an optional mask is given, the histogram is taken into account only data points (Xij)
  !         which have a true value in the mask.
  !         (Xij) can be single or double precision. The result will have the same numerical precision.

  !     CALLING SEQUENCE
  !         call hist(x, binx, bincount, width)                          ! with no optional arguments
  !         call hist(x, binx, bincount, width, bins=k,     mask=maske)  ! to fix the number of bins k
  !         call hist(x, binx, bincount, width, binwidth=w, mask=maske)  ! to fix the bin width w

  !     INTENT(IN)
  !         real(sp/dp) :: x(:)     1D array of x values
  !       or
  !         real(sp/dp) :: x(:,:)   2D array of x values

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         real(sp/dp) :: binx(:)       x-values of the histogram (center of the bin)
  !       or
  !         real(sp/dp) :: binx(:,:)     center of the bins and the averaged 2nd, 3rd... coordinates
  !       and
  !         integer(i4) :: bincount(:)   number of values within each bin
  !         real(sp/dp) :: width         width of a bin

  !     INTENT(IN), OPTIONAL
  !         logical     :: mask(:)    1D-array of logical values with size(x,1).
  !                                   If present, only the data points in (Xij) corresponding
  !                                   to the true values in mask are used.
  !         integer(i4) :: bins       Number of bins to be generated.
  !                                   If present, the number of bins will be fixed to this value.
  !                                   Otherwise, is set to integer part of sqrt(size(x)) or
  !                                   using the optionally set binwidth.
  !         real(sp/dp) :: binwidth   Width of the bins to be generated.
  !                                   If present, the width of bins will be fixed to this value and
  !                                   the INTENT(OUT) width will be set to this value.
  !                                   Otherwise, will be determined using the number of bins.

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RESTRICTIONS
  !         Input values must be floating points.
  !         Use more than one data point: Size(x,1) > 1.
  !         If you fix the number of bins, it has to be larger than 1: bins > 1.
  !         If you fix the width of the bins, it has to be larger than 0: binwidth > 0.
  !         Either the number of bins or the width of the bins can be fixed, not both.
  !         Not all values of the first coordinate should be equal.
  !         Dimension of x and mask have to match: Size(x,1) = Size(mask).

  !     EXAMPLE
  !         x = (/3.0, 4.0, 6.0, 7.0/)
  !
  !         binx = (/4.0, 6.0/)
  !         bincount = (/2, 2/)
  !         width = 2.0
  !
  !         x = (/ (/3.0, 4.0, 6.0, 7.0/) , (/1.0, 2.0, 5.0, 2.0/) /)
  !
  !         binx = (/ (/4.0, 6.0/) , (/1.5, 3.5/) /)
  !         bincount = (/2, 2/)
  !         width = 2.0
  !
  !         -> see also example in test directory

  !     LITERATURE
  !

  !     HISTORY
  !         Written,  Juliane Mai, Feb 2012
  INTERFACE histo
     MODULE PROCEDURE histo_sp_1d, histo_dp_1d, histo_sp_2d, histo_dp_2d
  END INTERFACE histo

  ! ------------------------------------------------------------------

  !     NAME
  !         histo2d

  !     PURPOSE
  !         Calculates the 2D histogram of n data 2D points (Xi,Yi),i=1,n ,
  !         i.e. the n data points are sorted into k x k categories (bins).
  !
  !         With no optional arguments the number of bins k per dimension
  !         is the integer part of n**(1/4) where n is the number of data points.
  !         E.g. n=20 will create k=2 bins in each direction (= 4 bins in sum).
  !         The bin width w is calculated depending on the minimal and maximal value of
  !         the first coordinate):
  !                     w = (maxval(X) - minval(X)) / k
  !
  !         Optionally, one can fix the number of bins k (integer i4)
  !         while the width of the bins is determined.
  !         Or, on can fix the binwidth w (float sp/dp) while k is determined automatically.
  !         If an optional mask is given, the histogram is taken into account only data points
  !         (Xi, Yi) which have a true value in the mask.
  !         (Xi, Yi) can be single or double precision.
  !         The result will have the same numerical precision.

  !     CALLING SEQUENCE
  !     with no optional arguments
  !         call hist2d(x, binx, bincount, width)
  !     to fix the number of bins k
  !         call hist2d(x, binx, bincount, width, bins=k,     mask=maske)
  !     to fix the bin width w
  !         call hist2d(x, binx, bincount, width, binwidth=w, mask=maske)

  !     INTENT(IN)
  !         real(sp/dp) :: x(:,2)   2D array of (x,y) values

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         real(sp/dp) :: binx(:,2)     (x,y)-values of the histogram (center of the bin)
  !         integer(i4) :: bincount(:)   number of values within each bin
  !         real(sp/dp) :: width         width of a bin

  !     INTENT(IN), OPTIONAL
  !         logical     :: mask(:)      1D-array of logical values with size(x,1).
  !                                     If present, only the data points in (Xij) corresponding
  !                                     to the true values in mask are used.
  !         integer(i4) :: bins         Number of bins to be generated.
  !                                     If present, the number of bins will be fixed to this value.
  !                                     Otherwise, is set to integer part of sqrt(size(x)) or
  !                                     using the optionally set binwidth.
  !         real(sp/dp) :: binwidth(2)  Width of the bins to be generated.
  !                                     One value for each direction.
  !                                     If present, the width of bins will be fixed to this value
  !                                     and the INTENT(OUT) width will be set to this value.
  !                                     Otherwise, will be determined using the number of bins.

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RESTRICTIONS
  !         The number of bins is equal in each direction, i.e. x and y.
  !         Input values must be floating points.
  !         Use more than one data point: Size(x,1) > 1.
  !         If you fix the number of bins, it has to be larger than 1: bins > 1.
  !         If you fix the width of the bins, it has to be larger than 0: binwidth > 0.
  !         Either the number of bins or the width of the bins can be fixed, not both.
  !         Not all values of the first coordinate should be equal.
  !         Dimension of x and mask have to match: Size(x,1) = Size(mask).

  !     EXAMPLE
  !     REAL(DP),    DIMENSION(4,2)              :: x
  !     REAL(DP),    DIMENSION(:,:), ALLOCATABLE :: testbinx
  !     INTEGER(I4), DIMENSION(:),   ALLOCATABLE :: testbincount
  !     REAL(DP),    DIMENSION(2)                :: testwidth

  !     x(1,:) = (/3.0_dp, 1.0_dp/)
  !     x(2,:) = (/6.0_dp, 4.0_dp/)
  !     x(3,:) = (/2.0_dp, 3.0_dp/)
  !     x(4,:) = (/1.0_dp, 1.0_dp/)
  !
  !     call histo2d(x, testbinx, testbincount, testwidth)
  !
  !         ^
  !         |                                      x ... data points
  !       4 |  ********************x               * ... edge of bin
  !         |  *         *         *
  !       3 |  *   x     *         *
  !         |  *********************
  !       2 |  *         *         *
  !         |  *         *         *
  !       1 |  x*******x************
  !         |
  !         |- - - - - - - - - - - - -  >
  !            1   2   3   4   5   6
  !
  !     print*, 'testbinx(1)  = ',testbinx(1,:)    --> (/2.25, 1.75/)
  !     print*, 'testbinx(2)  = ',testbinx(2,:)    --> (/4.75, 1.75/)
  !     print*, 'testbinx(3)  = ',testbinx(3,:)    --> (/2.25, 3.25/)
  !     print*, 'testbinx(4)  = ',testbinx(4,:)    --> (/4.75, 3.25/)
  !     print*, 'testbincount = ',testbincount     --> (/ 2, 0, 1, 1/)
  !     print*, 'testwidth    = ',testwidth        --> (/ 2.5, 1.5 /)
  !
  !     LITERATURE
  !
  !     HISTORY
  !         Written,  Juliane Mai, Mar 2012
  INTERFACE histo2d
     MODULE PROCEDURE histo2d_sp, histo2d_dp
  END INTERFACE histo2d

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  SUBROUTINE histo_dp_1d(x, binx, bincount, width, mask, bins, binwidth)
      !dir$ attributes code_align : 32 :: histo_dp_1d
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: histo_dp_1d

    IMPLICIT NONE

    REAL(DP),    DIMENSION(:),                     INTENT(IN)  :: x
    REAL(DP),    DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(DP),                                      INTENT(OUT) :: width

    INTEGER(I4),            OPTIONAL,              INTENT(IN)  :: bins
    REAL(DP),               OPTIONAL,              INTENT(IN)  :: binwidth
    LOGICAL,  DIMENSION(:), OPTIONAL,              INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x))              :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(DP)                                 :: w        ! binwidth
    REAL(DP)                                 :: minimalvalue
    INTEGER(I4)                              :: i, binnr
    REAL(DP), DIMENSION(:),ALLOCATABLE       :: helpbinx
    INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount
    !dir$ attributes align : 64 :: helpbinx
    !dir$ attributes align : 64 :: helpbincount
    if (present(mask)) then
       if (size(mask) /= size(x)) stop 'Error histo_dp: size(mask) /= size(x)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x)
    endif

    if (n .le. (1.0_dp+tiny(1.0_dp))) then
       stop 'Error histo_dp: size(x) must be at least 2'
    end if
    if (maxval(x(:), mask=maske)-minval(x(:), mask=maske) <= tiny(1.0_dp)) then
       stop 'Error histo_dp: all entries of x(:) are equal'
    end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo_dp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo_dp: number of bins <= 1'
       k = bins
       w = (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / real(k,dp)
    else
       if (present(binwidth)) then
          if (binwidth .le. tiny(1.0_dp)) stop 'Error histo_dp: width of bins too small'
          w = binwidth
          k = Ceiling( (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / w )
       else
          k = Floor(Sqrt(real(n,dp)))
          w = (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / real(k,dp)
       end if
    endif

    ! Histogram

    allocate (binx(k),bincount(k))

    minimalvalue=minval(x(:), mask=maske)
    !dir$ assume_aligned binx:64
    !dir$ assume_aligned bincount:64
    !dir$ vector always
    !dir$ ivdep
    do i=1,k
       binx(i) = minimalvalue + real(2*i-1,dp)*0.5_dp*w
       bincount(i) = 0_i4
    end do
    
    do i=1,size(x)
       If (maske(i)) then
          binnr           = Floor((x(i)-minimalvalue)/w )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr           = binnr - Floor(real(binnr,dp)/real(k,dp)) + 1_i4
          bincount(binnr) = bincount(binnr) + 1_i4
       end if
    end do

    width = w

    if (count(bincount .gt. 0_i4) .lt. k) then
       print *, ' '
       print *, 'WARNING histo_dp: Empty bins have been deleted. size(binx) < bins'
       print *, ' '

       allocate(helpbinx(count(bincount .gt. 0_i4)))
       allocate(helpbincount(count(bincount .gt. 0_i4)))
       binnr=1_i4
       !dir$ assume_aligned helpbincount:64
       !dir$ assume_aligned helpbinx:64
       !dir$ ivdep
       !dir$ vector always
       do i=1,k
          if (bincount(i) .gt. 0_i4) then
             helpbinx(binnr) = binx(i)
             helpbincount(binnr) = bincount(i)
             binnr=binnr+1_i4
          end if
       end do
       deallocate(binx,bincount)
       allocate(binx(size(helpbinx)))
       allocate(bincount(size(helpbinx)))
       binx=helpbinx
       bincount=helpbincount
       deallocate(helpbinx,helpbincount)
    end if

  END SUBROUTINE histo_dp_1d


  SUBROUTINE histo_sp_1d(x, binx, bincount, width, mask, bins, binwidth)
      !dir$ attributes code_align : 32 :: histo_sp_1d
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: histo_sp_1d
    IMPLICIT NONE

    REAL(SP),    DIMENSION(:),                     INTENT(IN)  :: x
    REAL(SP),    DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(SP),                                      INTENT(OUT) :: width

    INTEGER(I4),            OPTIONAL,              INTENT(IN)  :: bins
    REAL(SP),               OPTIONAL,              INTENT(IN)  :: binwidth
    LOGICAL,  DIMENSION(:), OPTIONAL,              INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x))              :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(SP)                                 :: w        ! binwidth
    REAL(SP)                                 :: minimalvalue
    INTEGER(I4)                              :: i, binnr
    REAL(SP), DIMENSION(:),ALLOCATABLE       :: helpbinx
    INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount
    !dir$ attribute align : 64 :: helpbinx
    !dir$ attribute align : 64 :: helpbincount
    if (present(mask)) then
       if (size(mask) /= size(x)) stop 'Error histo_sp: size(mask) /= size(x)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x)
    endif

    if (n .le. (1.0_sp+tiny(1.0_sp))) then
       stop 'Error histo_sp: size(x) must be at least 2'
    end if
    if (maxval(x(:), mask=maske)-minval(x(:), mask=maske) <= tiny(1.0_sp)) then
       stop 'Error histo_sp: all entries of x(:) are equal'
    end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo_sp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo_sp: number of bins <= 1'
       k = bins
       w = (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / real(k,sp)
    else
       if (present(binwidth)) then
          if (binwidth .le. tiny(1.0_sp)) stop 'Error histo_sp: width of bins too small'
          w = binwidth
          k = Ceiling( (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / w )
       else
          k = Floor(Sqrt(real(n,sp)))
          w = (maxval(x(:), mask=maske)-minval(x(:), mask=maske)) / real(k,sp)
       end if
    endif

    ! Histogram

    allocate (binx(k),bincount(k))

    minimalvalue = minval(x(:), mask=maske)
    !dir$ assume_aligned binx:64
    !dir$ assume_aligned bincount:64
    !dir$ vector always
    !dir$ ivdep
    do i=1,k
       binx(i) = minimalvalue + real(2*i-1,sp)*0.5_sp*w
       bincount(i) = 0_i4
    end do
      !dir$ assume_aligned bincount:64
    do i=1,size(x)
       If (maske(i)) then
          binnr           = Floor((x(i)-minimalvalue)/w )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr           = binnr - Floor(real(binnr,sp)/real(k,sp)) + 1_i4
          bincount(binnr) = bincount(binnr) + 1_i4
       end if
    end do

    width = w

    if (count(bincount .gt. 0_i4) .lt. k) then
       print *, ' '
       print *, 'WARNING histo_sp: Empty bins have been deleted. size(binx) < bins'
       print *, ' '

       allocate(helpbinx(count(bincount .gt. 0_i4)))
       allocate(helpbincount(count(bincount .gt. 0_i4)))
       binnr=1_i4
        !dir$ assume_aligned binx:64
        !dir$ assume_aligned bincount:64
        !dir$ vector always
        !dir$ ivdep
       do i=1,k
          if (bincount(i) .gt. 0_i4) then
             helpbinx(binnr) = binx(i)
             helpbincount(binnr) = bincount(i)
             binnr=binnr+1_i4
          end if
       end do
       deallocate(binx,bincount)
       allocate(binx(size(helpbinx)))
       allocate(bincount(size(helpbinx)))
       binx=helpbinx
       bincount=helpbincount
       deallocate(helpbinx,helpbincount)
    end if

  END SUBROUTINE histo_sp_1d

  SUBROUTINE histo_dp_2d(x, binx, bincount, width, mask, bins, binwidth)
      !dir$ attributes code_align : 32 :: histo_dp_2d
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: histo_dp_2d
    IMPLICIT NONE

    REAL(DP),    DIMENSION(:,:),                   INTENT(IN)  :: x
    REAL(DP),    DIMENSION(:,:),      ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(DP),                                      INTENT(OUT) :: width

    INTEGER(I4),            OPTIONAL,              INTENT(IN)  :: bins
    REAL(DP),               OPTIONAL,              INTENT(IN)  :: binwidth
    LOGICAL,  DIMENSION(:), OPTIONAL,              INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x,1))            :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(DP)                                 :: w        ! binwidth
    REAL(DP)                                 :: minimalvalue
    INTEGER(I4)                              :: i, j, binnr
    REAL(DP), DIMENSION(:,:),ALLOCATABLE     :: helpbinx
    INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount
    !dir$ attribute align : 64 :: helpbinx
    !dir$ attribute align : 64 :: helpbincount
    if (present(mask)) then
       if (size(mask) /= size(x,1)) stop 'Error histo_dp: size(mask) /= size(x,1)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x,1)
    endif

    if (n .le. (1.0_dp+tiny(1.0_dp))) then
       stop 'Error histo_dp: size(x,1) must be at least 2'
    end if
    if (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske) <= tiny(1.0_dp)) then
       stop 'Error histo_dp: all entries of x(:,1) are equal'
    end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo_dp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo_dp: number of bins <= 1'
       k = bins
       w = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,dp)
    else
       if (present(binwidth)) then
          if (binwidth .le. tiny(1.0_dp)) stop 'Error histo_dp: width of bins too small'
          w = binwidth
          k = Ceiling( (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / w )
       else
          k = Floor(Sqrt(real(n,dp)))
          w = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,dp)
       end if
    endif

    ! Histogram

    allocate (binx(k,size(x,2)),bincount(k))

    minimalvalue = minval(x(:,1), mask=maske)

    binx = 0.0_dp
    bincount = 0_i4
    !dir$ assume_aligned binx:64
    !dir$ vector always
    !dir$ ivdep
    do i=1,k
       binx(i,1) = minimalvalue + real(2*i-1,dp)*0.5_dp*w
    end do

    do i=1,size(x,1)
       If (maske(i)) then
          binnr           = Floor((x(i,1)-minimalvalue)/w )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr           = binnr - Floor(real(binnr,dp)/real(k,dp)) + 1_i4
          bincount(binnr) = bincount(binnr) + 1_i4
          do j=2,size(x,2)
             binx(binnr,j)     = binx(binnr,j) + x(i,j)
          end do
       end if
    end do

    width = w

    if (count(bincount .gt. 0_i4) .lt. k) then
       print *, ' '
       print *, 'WARNING histo_dp: Empty bins have been deleted. size(binx) < bins'
       print *, ' '

       allocate(helpbinx(count(bincount .gt. 0_i4),size(x,2)))
       allocate(helpbincount(count(bincount .gt. 0_i4)))
       binnr=1_i4
       do i=1,k
          if (bincount(i) .gt. 0_i4) then
             helpbinx(binnr,1) = binx(i,1)
             do j=2,size(x,2)
                helpbinx(binnr,j) = binx(i,j)/real(bincount(i),dp)
             end do
             helpbincount(binnr) = bincount(i)
             binnr=binnr+1_i4
             !else
             !   print*, 'Delete binx = ', binx(i,1)
          end if
       end do
       deallocate(binx,bincount)
       allocate(binx(size(helpbinx,1),size(helpbinx,2)))
       allocate(bincount(size(helpbinx)))
       binx=helpbinx
       bincount=helpbincount
       deallocate(helpbinx,helpbincount)
    else
       do i=1,k
          do j=2,size(x,2)
             binx(i,j)     = binx(i,j) / real(bincount(i),dp)
          end do
       end do
    end if

  END SUBROUTINE histo_dp_2d


  SUBROUTINE histo_sp_2d(x, binx, bincount, width, mask, bins, binwidth)
      !dir$ attributes code_align : 32 :: histo_sp_2d
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: histo_sp_2d
    IMPLICIT NONE

    REAL(SP),    DIMENSION(:,:),                   INTENT(IN)  :: x
    REAL(SP),    DIMENSION(:,:),      ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(SP),                                      INTENT(OUT) :: width

    INTEGER(I4),            OPTIONAL,              INTENT(IN)  :: bins
    REAL(SP),               OPTIONAL,              INTENT(IN)  :: binwidth
    LOGICAL,  DIMENSION(:), OPTIONAL,              INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x,1))            :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(SP)                                 :: w        ! binwidth
    REAL(SP)                                 :: minimalvalue
    INTEGER(I4)                              :: i, j, binnr
    REAL(SP), DIMENSION(:,:),ALLOCATABLE     :: helpbinx
    INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount
    !dir$ attribute align : 64 :: helpbinx
    !dir$ attribute align : 64 :: helpbincount
    if (present(mask)) then
       if (size(mask) /= size(x,1)) stop 'Error histo_sp: size(mask) /= size(x,1)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x,1)
    endif

    if (n .le. (1.0_sp+tiny(1.0_sp))) then
       stop 'Error histo_sp: size(x,1) must be at least 2'
    end if
    if (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske) <= tiny(1.0_sp)) then
       stop 'Error histo_sp: all entries of x(:,1) are equal'
    end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo_sp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo_sp: number of bins <= 1'
       k = bins
       w = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,sp)
    else
       if (present(binwidth)) then
          if (binwidth .le. tiny(1.0_sp)) stop 'Error histo_sp: width of bins too small'
          w = binwidth
          k = Ceiling( (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / w )
       else
          k = Floor(Sqrt(real(n,sp)))
          w = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,sp)
       end if
    endif

    ! Histogram

    allocate (binx(k,size(x,2)),bincount(k))

    minimalvalue = minval(x(:,1), mask=maske)

    binx = 0.0_sp
    bincount = 0_i4
    !dir$ assume_aligned binx:64
    !dir$ vector always
    do i=1,k
       binx(i,1) = minimalvalue + real(2*i-1,sp)*0.5_sp*w
    end do

    do i=1,size(x,1)
       If (maske(i)) then
          binnr           = Floor((x(i,1)-minimalvalue)/w )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr           = binnr - Floor(real(binnr,sp)/real(k,sp)) + 1_i4
          bincount(binnr) = bincount(binnr) + 1_i4
          do j=2,size(x,2)
             binx(binnr,j)     = binx(binnr,j) + x(i,j)
          end do
       end if
    end do

    width = w

    if (count(bincount .gt. 0_i4) .lt. k) then
       print *, ' '
       print *, 'WARNING histo_sp: Empty bins have been deleted. size(binx) < bins'
       print *, ' '

       allocate(helpbinx(count(bincount .gt. 0_i4),size(x,2)))
       allocate(helpbincount(count(bincount .gt. 0_i4)))
       binnr=1_i4
       do i=1,k
          if (bincount(i) .gt. 0_i4) then
             helpbinx(binnr,1) = binx(i,1)
             do j=2,size(x,2)
                helpbinx(binnr,j) = binx(i,j)/real(bincount(i),sp)
             end do
             helpbincount(binnr) = bincount(i)
             binnr=binnr+1_i4
             !else
             !   print*, 'Delete binx = ', binx(i,1)
          end if
       end do
       deallocate(binx,bincount)
       allocate(binx(size(helpbinx,1),size(helpbinx,2)))
       allocate(bincount(size(helpbinx)))
       binx=helpbinx
       bincount=helpbincount
       deallocate(helpbinx,helpbincount)
    else
       do i=1,k
          do j=2,size(x,2)
             binx(i,j)     = binx(i,j) / real(bincount(i),sp)
          end do
       end do
    end if

  END SUBROUTINE histo_sp_2d

  ! ------------------------------------------------------------------

  SUBROUTINE histo2d_dp(x, binx, bincount, width, mask, bins, binwidth)
        !dir$ attributes code_align : 32 :: histo2d_dp
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: histo2d_dp
    IMPLICIT NONE

    REAL(DP),    DIMENSION(:,:),                   INTENT(IN)  :: x
    REAL(DP),    DIMENSION(:,:),      ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(DP),    DIMENSION(2),                     INTENT(OUT) :: width

    INTEGER(I4),               OPTIONAL,           INTENT(IN)  :: bins
    REAL(DP),    DIMENSION(2), OPTIONAL,           INTENT(IN)  :: binwidth
    LOGICAL,     DIMENSION(:), OPTIONAL,           INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x,1))            :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(DP), DIMENSION(2)                   :: w        ! binwidth
    REAL(DP), DIMENSION(2)                   :: minimalvalue
    INTEGER(I4)                              :: i, j
    INTEGER(I4)                              :: binnr, binnr1, binnr2
    !REAL(DP), DIMENSION(:),ALLOCATABLE       :: helpbinx
    !INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount

    if (present(mask)) then
       if (size(mask) /= size(x,1)) stop 'Error histo_dp: size(mask) /= size(x)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x,1)
    endif

    if (n .le. (1.0_dp+tiny(1.0_dp))) then
       stop 'Error histo2d_dp: size(x) must be at least 2'
    end if
    !if (maxval(x(:), mask=maske)-minval(x(:), mask=maske) <= tiny(1.0_dp)) then
    !   stop 'Error histo_dp: all entries of x(:) are equal'
    !end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo2d_dp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo2d_dp: number of bins <= 1'
       k = bins
       w(1) = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,dp)
       w(2) = (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / real(k,dp)
    else
       if (present(binwidth)) then
          if (any(binwidth .le. tiny(1.0_dp))) stop 'Error histo2d_dp: width of bins too small'
          if (size(binwidth,1) .ne. 2_i4) stop 'Error histo2d_dp: size(width) has to be 2'
          w(:) = binwidth(:)
          k = Max( Ceiling( (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / w(1) ), &
               Ceiling( (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / w(2) ) )
       else
          k = Floor(real(n,dp)**0.25_dp)
          w(1) = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,dp)
          w(2) = (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / real(k,dp)
       end if
    endif

    ! Histogram

    allocate (binx(k*k,2),bincount(k*k))

    minimalvalue(1)=minval(x(:,1), mask=maske)
    minimalvalue(2)=minval(x(:,2), mask=maske)

    do i=1,k
       do j=1,k
          binx((i-1)*k+j,1)   = minimalvalue(1) + real(2*j-1,dp)/2.0_dp*w(1)
          binx((i-1)*k+j,2)   = minimalvalue(2) + real(2*i-1,dp)/2.0_dp*w(2)
          bincount((i-1)*k+j) = 0_i4
       end do
    end do

    do i=1,size(x,1)
       If (maske(i)) then
          !print*, 'x(i) = ',x(i,:)
          binnr1 =   Floor((x(i,1)-minimalvalue(1))/w(1) )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr1 =   binnr1 - Floor(real(binnr1,dp)/real(k,dp)) + 1_i4
          !print*, 'i = ',i,'  binnr 1 = ',binnr1

          binnr2 =   Floor((x(i,2)-minimalvalue(2))/w(2) )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr2 =   binnr2 - Floor(real(binnr2,dp)/real(k,dp)) + 1_i4
          !print*, 'i = ',i,'  binnr 2 = ',binnr2

          binnr           = (binnr2-1_i4)*k + binnr1
          !print*, 'i = ',i,'  binnr 3 = ',binnr
          bincount(binnr) = bincount(binnr) + 1_i4
       end if
    end do

    width(:) = w(:)

  END SUBROUTINE histo2d_dp


  SUBROUTINE histo2d_sp(x, binx, bincount, width, mask, bins, binwidth)

    IMPLICIT NONE

    REAL(SP),    DIMENSION(:,:),                   INTENT(IN)  :: x
    REAL(SP),    DIMENSION(:,:),      ALLOCATABLE, INTENT(OUT) :: binx
    INTEGER(I4), DIMENSION(:),        ALLOCATABLE, INTENT(OUT) :: bincount
    REAL(SP),    DIMENSION(2),                     INTENT(OUT) :: width

    INTEGER(I4),               OPTIONAL,           INTENT(IN)  :: bins
    REAL(SP),    DIMENSION(2), OPTIONAL,           INTENT(IN)  :: binwidth
    LOGICAL,     DIMENSION(:), OPTIONAL,           INTENT(IN)  :: mask

    ! Local variables
    LOGICAL, DIMENSION(size(x,1))            :: maske
    INTEGER(I4)                              :: n        ! number of data points
    INTEGER(I4)                              :: k        ! number of bins
    REAL(SP), DIMENSION(2)                   :: w        ! binwidth
    REAL(SP), DIMENSION(2)                   :: minimalvalue
    INTEGER(I4)                              :: i, j
    INTEGER(I4)                              :: binnr, binnr1, binnr2
    !REAL(SP), DIMENSION(:),ALLOCATABLE       :: helpbinx
    !INTEGER(I4), DIMENSION(:),ALLOCATABLE    :: helpbincount

    if (present(mask)) then
       if (size(mask) /= size(x,1)) stop 'Error histo_sp: size(mask) /= size(x)'
       maske = mask
       n = count(maske)
    else
       maske(:) = .true.
       n = size(x,1)
    endif

    if (n .le. (1.0_sp+tiny(1.0_sp))) then
       stop 'Error histo2d_sp: size(x) must be at least 2'
    end if
    !if (maxval(x(:), mask=maske)-minval(x(:), mask=maske) <= tiny(1.0_sp)) then
    !   stop 'Error histo_sp: all entries of x(:) are equal'
    !end if

    if (present(bins) .and. present(binwidth)) then
       stop 'Error histo2d_sp: Either fix number of bins or binwidth, not both.'
    end if

    if (present(bins)) then
       if (bins .le. 1_i4) stop 'Error histo2d_sp: number of bins <= 1'
       k = bins
       w(1) = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,sp)
       w(2) = (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / real(k,sp)
    else
       if (present(binwidth)) then
          if (any(binwidth .le. tiny(1.0_sp))) stop 'Error histo2d_sp: width of bins too small'
          if (size(binwidth,1) .ne. 2_i4) stop 'Error histo2d_sp: size(width) has to be 2'
          w(:) = binwidth(:)
          k = Max( Ceiling( (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / w(1) ), &
               Ceiling( (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / w(2) ) )
       else
          k = Floor(Sqrt(real(n,sp)))
          w(1) = (maxval(x(:,1), mask=maske)-minval(x(:,1), mask=maske)) / real(k,sp)
          w(2) = (maxval(x(:,2), mask=maske)-minval(x(:,2), mask=maske)) / real(k,sp)
       end if
    endif

    ! Histogram

    allocate (binx(k*k,2),bincount(k*k))

    minimalvalue(1)=minval(x(:,1), mask=maske)
    minimalvalue(2)=minval(x(:,2), mask=maske)

    do i=1,k
       do j=1,k
          binx((i-1)*k+j,1)   = minimalvalue(1) + real(2*j-1,sp)/2.0_sp*w(1)
          binx((i-1)*k+j,2)   = minimalvalue(2) + real(2*i-1,sp)/2.0_sp*w(2)
          bincount((i-1)*k+j) = 0_i4
       end do
    end do

    do i=1,size(x,1)
       If (maske(i)) then
          !print*, 'x(i) = ',x(i,:)
          binnr1 =   Floor((x(i,1)-minimalvalue(1))/w(1) )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr1 =   binnr1 - Floor(real(binnr1,sp)/real(k,sp)) + 1_i4
          !print*, 'i = ',i,'  binnr 1 = ',binnr1

          binnr2 =   Floor((x(i,2)-minimalvalue(2))/w(2) )
          ! maxval(x) has to be assigned to last bin k not k+1
          binnr2 =   binnr2 - Floor(real(binnr2,sp)/real(k,sp)) + 1_i4
          !print*, 'i = ',i,'  binnr 2 = ',binnr2

          binnr           = (binnr2-1_i4)*k + binnr1
          !print*, 'i = ',i,'  binnr 3 = ',binnr
          bincount(binnr) = bincount(binnr) + 1_i4
       end if
    end do

    width(:) = w(:)

  END SUBROUTINE histo2d_sp


END MODULE histogram
