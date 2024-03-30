! Linear algebra subroutines
module linalg_mod

    implicit none
    
contains


subroutine matinv(n, a, ai)
      ! This sobroutine inverts a matrix "a" and returns the inverse in "ai"
      ! n  - Input by user, an integer specifying the size of the matrix to be inverted.
      ! a  - Input by user, an n by n real array containing the matrix to be inverted.
      ! ai - Returned by subroutine, an n by n real array containing the inverted matrix.
      ! d  - Work array, an n by 2n real array used by the subroutine.
      ! io - Work array, a 1-dimensional integer array of length n used by the subroutine.

      ! NEVER INVERT A MARTIX EXPLICITLY!
      ! Unless you know what you're doing. It's okay for small (N<10) matrices that are well-conditioned.

      implicit none

      integer :: n,i,j,k,m,itmp
      real :: a(n,n),ai(n,n),tmp,r
      real,allocatable,dimension(:,:) :: d
      integer,allocatable,dimension(:) :: io

      allocate(d(n,2*n))
      allocate(io(n))

      d(:,:) = 0.0
      io(:) = 0

!     Fill in the "io" and "d" matrix.
!     ********************************
      do i=1,n
         io(i)=i
      end do
      do i=1,n
         do j=1,n
            d(i,j)=a(i,j)
            if(i.eq.j)then
               d(i,n+j)=1.
            else
               d(i,n+j)=0.
            endif
         end do
      end do

!     Scaling
!     *******
      do i=1,n
         m=1
         do k=2,n
            if(abs(d(i,k)).gt.abs(d(i,m))) m=k
         end do
         tmp=d(i,m)
         do k=1,2*n
            d(i,k)=d(i,k)/tmp
         end do
      end do

!     Lower Elimination
!     *****************
      do i=1,n-1
!        Pivoting
!        ********
         m=i
         do j=i+1,n
            if(abs(d(io(j),i)).gt.abs(d(io(m),i))) m=j
         end do
         itmp=io(m)
         io(m)=io(i)
         io(i)=itmp
!        Scale the Pivot element to unity
!        ********************************
         r=d(io(i),i)
         do k=1,2*n
            d(io(i),k)=d(io(i),k)/r
         end do
!        ********************************
         do j=i+1,n
            r=d(io(j),i)
            do k=1,2*n
               d(io(j),k)=d(io(j),k)-r*d(io(i),k)
            end do
         end do
      end do

!     Upper Elimination
!     *****************
      r=d(io(n),n)
      do k=1,2*n
         d(io(n),k)=d(io(n),k)/r
      end do
      do i=n-1,1,-1
         do j=i+1,n
            r=d(io(i),j)
            do k=1,2*n
               d(io(i),k)=d(io(i),k)-r*d(io(j),k)
            end do
         end do
      end do

!     Fill Out "ai" matrix
      do i=1,n
         do j=1,n
            ai(i,j)=d(io(i),n+j)
         end do
      end do

      ! Cleanup
      deallocate(d)
      deallocate(io)

end subroutine matinv


subroutine lu_solve(N, A, b, x)
  ! Solves a general [A]x=b on an nxn matrix
  ! This replaces A (in place) with its LU decomposition (permuted row-wise)

    implicit none

    integer,intent(in) :: N
    real,dimension(N,N),intent(inout) :: A
    real,dimension(N),intent(in) :: b
    real,dimension(:),allocatable,intent(out) :: x

    integer,allocatable,dimension(:) :: indx
    integer :: D, info

    allocate(indx(N))

    ! Compute decomposition
    call lu_decomp(A, N, indx, D, info)

    ! if the matrix is nonsingular, then backsolve to find X
    if (info == 1) then
        write(*,*) 'Subroutine lu_decomp() failed. The given matrix is singular (i.e. no unique solution). Quitting...'
        stop
    else
        call lu_back_sub(A, N, indx, b, x)
    end if

    ! Cleanup
    deallocate(indx)

end subroutine lu_solve


!*******************************************************
!*    LU decomposition routines used by test_lu.f90    *
!*                                                     *
!*                 F90 version by J-P Moreau, Paris    *
!*    improved for F95 by Cory Goates, Logan, UT, USA  *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986" [BIBLI 08].                *
!*                                                     *
!*******************************************************


subroutine lu_decomp(A, N, indx, D, code)
  ! Given an N x N matrix A, this routine replaces it by the LU
  ! decomposition of a rowwise permutation of itself. A and N  
  ! are input. indx is an output vector which records the row  
  ! permutation effected by the partial pivoting; D is output  
  ! as -1 or 1, depending on whether the number of row inter-  
  ! changes was even or odd, respectively. This routine is used
  ! in combination with lu_back_sub to solve linear equations or to 
  ! invert a matrix. Return code is 1 if matrix is singular.  

  implicit none

  real,dimension(N,N),intent(inout) :: A
  integer,intent(in) :: N
  integer,dimension(N),intent(out) :: indx
  integer,intent(out) :: code, D

  real,dimension(N) :: vv
  real,parameter :: tiny=1.5e-20
  integer :: i, j, k, imax
  real :: amax, dum, s

  ! Initialize
  D = 1
  code = 0
  imax = 0

  ! Loop over rows to get implicit scaling information
  do i=1,N

    ! Get largest element in this row
    amax=0.0
    do j=1,N
      if (abs(A(i,j)) > amax) then
        amax = abs(A(i,j))
      end if
    end do

    ! Check the largest element in this row is nonzero
    if (amax <= tiny) then
      code = 1 ! Singular matrix
      return
    end if

    ! Store scaling
    vv(i) = 1.0 / amax

  end do

  ! Loop over columns of Crout's method
  do j=1,N

    do i=1,j-1
      
      s = A(i,j)

      do k=1,i-1
        s = s - A(i,k)*A(k,j)
      end do

      A(i,j) = s

    end do

    ! Initialize search for largest pivot element
    amax = 0.0
    do i=j,N
  
      s = A(i,j)
      do k=1,j-1
        s = s - A(i,k)*A(k,j)
      end do
      A(i,j) = s
  
      ! Determine figure of merit for the pivot
      dum = vv(i)*abs(s)
      if (dum >= amax) then
        imax = i
        amax = dum
      end if
  
    end do

    ! Figure out if we need to interchange rows
    if (j /= imax) then
  
      ! Perform interchange
      do k=1,N
        dum = A(imax,k)
        A(imax,k) = A(j,k)
        A(j,k) = dum
      end do
  
      ! Update the sign of D since a row interchange has occurred
      D = -D
  
      ! Interchange the implicit scaling factor
      vv(imax) = vv(j)
  
    end if

    ! Store pivoting
    indx(j) = imax

    ! Divide by pivot element
    if (j /= N) then
      dum = 1.0 / A(j,j)
      do i=j+1,N
        A(i,j) = A(i,j)*dum
      end do
    end if

  end do

end subroutine lu_decomp


subroutine lu_back_sub(A, N, indx, b, x)
  ! Solves the set of N linear equations Ax = b.  Here A is     
  ! input, not as the matrix A but rather as its LU decomposition, 
  ! determined by the routine LUDCMP. indx is input as the permuta-
  ! tion vector returned by LUDCMP. b is input as the right-hand   
  ! side vector b. The solution vector is x. A, N, b and
  ! indx are not modified by this routine and can be used for suc- 
  ! cessive calls with different right-hand sides. This routine is 
  ! also efficient for plain matrix inversion.                     

  implicit none

  integer,intent(in) :: N
  real,dimension(N,N),intent(in) :: A
  real,dimension(N),intent(in) :: b
  integer,dimension(N),intent(in) :: indx
  real,dimension(:),allocatable,intent(out) :: x

  real :: sum
  integer :: ii,i,j,ll

  ! Initialize solution
  allocate(x, source=b)

  ! Set tracker to ignore leading zeros in b
  ii = 0

  ! Forward substitution
  do i=1,N

    ! Untangle pivoting
    ll = indx(i)
    sum = x(ll)
    x(ll) = x(i)

    ! If a nonzero element of b has already been encountered
    if (ii /= 0) then
      do J=ii,i-1
        sum = sum - A(i,J)*x(J)
      end do

    ! Check for first nonzero element of b
    else if(sum /= 0.0) then
      ii = i
    end if

    x(i) = sum

  end do

  ! Back substitution
  do i=N,1,-1
    sum = x(i)
    do j=i+1,N
      sum = sum - A(i,j)*x(j)
    end do
    x(i) = sum / A(i,i)
  end do

end subroutine lu_back_sub


subroutine quadratic_fit(pts, a, b, c)
  ! Fits a parabola through three specified   
  ! points and returns the coefficients a, b, c defining this parabola 
  ! according to the equation y = a * x**2 + b * x + c                 
  !            pts = list of three (x, y) points                       
  !   Outputs: a, b, c = quadratic coefficients                        

  implicit none

  real,dimension(3, 2),intent(in) :: pts
  real,intent(out) :: a, b, c

  integer :: i
  real,dimension(3,3) :: m
  real,dimension(:),allocatable :: coeff

  do i = 1, 3
    m(i,1) = pts(i,1)**2
    m(i,2) = pts(i,1)
    m(i,3) = 1.0
  end do

  call lu_solve(3, m, pts(:,2), coeff)

  a = coeff(1)
  b = coeff(2)
  c = coeff(3)

end subroutine quadratic_fit


subroutine decompose_blocks(N, A, N_blocks, block_size, A_blocks, N_last, ind_P, i_start_block, i_end_block)
  ! Decomposes the diagonal blocks of A using LU decomposition
  ! N is the size of the system
  ! A is the system matrix
  ! block_size is the desired size of each block
  ! A_blocks is the block diagonals of A after LU decomposition
  ! N_last is the number of elements in the last block
  ! ind_P is a matrix of the block permutation indices
  ! i_start_block is a vector of the start indices for each block
  ! i_end_block is a vector of the end indices for each block

  implicit none

  integer,intent(in) :: N
  real,dimension(N,N),intent(in) :: A
  integer,intent(in) :: N_blocks, block_size
  real,dimension(:,:,:),allocatable,intent(out) :: A_blocks
  integer,intent(out) :: N_last
  integer,dimension(:,:),allocatable,intent(out) :: ind_P
  integer,dimension(:),allocatable,intent(out) :: i_start_block, i_end_block

  integer :: i, info, D

  ! Allocate start and end indices
  allocate(i_start_block(N_blocks))
  allocate(i_end_block(N_blocks))

  ! Allocate permutation incides
  allocate(ind_P(block_size, N_blocks))

  ! Allocate blocks
  allocate(A_blocks(block_size,block_size,N_blocks))

  ! Decompose blocks
  !$OMP parallel do private(info)
  do i=1,N_blocks

    ! Last block
    if (i == N_blocks) then

      ! Determine start and end indices of this block
      i_start_block(i) = (i-1)*block_size + 1
      i_end_block(i) = N
      N_last = i_end_block(i)-i_start_block(i)+1

      ! Store in block to be decomposed
      A_blocks(1:N_last,1:N_last,i) = A(i_start_block(i):N,i_start_block(i):N)

      ! Decompose
      call lu_decomp(A_blocks(1:N_last,1:N_last,i), N_last, ind_P(1:N_last,i), D, info)

      ! Check for singular submatrix
      if (info == 1) then
          write(*,*) 'Subroutine lu_decomp() failed on subblock ', i, '. Quitting...'
          stop
      end if

    ! Not last block
    else

      ! Determine start and end indices of this block
      i_start_block(i) = (i-1)*block_size + 1
      i_end_block(i) = i*block_size

      ! Store in block to be decomposed
      A_blocks(:,:,i) = A(i_start_block(i):i_end_block(i),i_start_block(i):i_end_block(i))

      ! Decompose
      call lu_decomp(A_blocks(:,:,i), block_size, ind_P(:,i), D, info)

      ! Check for singular submatrix
      if (info == 1) then
          write(*,*) 'Subroutine lu_decomp() failed on subblock ', i, '. Quitting...'
          stop
      end if

    end if

  end do

end subroutine decompose_blocks


subroutine block_ssor_solve(N, A, b, block_size, tol, rel, max_iter, output_file, total_iter, x)
  ! Iteratively solves the [A]x=b system using block symmetric successive overrelaxation
  ! N is the size of the system
  ! A is the system matrix; block diagonals will be replaced with their LU decompositions
  ! b is the RHS vector
  ! block_size is the desired size of each block
  ! tol is the convergence tolerance between iterations
  ! rel is a relaxation factor between 0 and 2
  ! max_iter
  ! verbose
  ! x is the solution

  implicit none

  integer,intent(in) :: N, block_size, max_iter
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  real,intent(inout) :: rel
  character(len=:),allocatable,intent(in) :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  real :: err, dx 
  real,dimension(N) :: x_new, vk
  real,dimension(:,:,:),allocatable :: A_blocks
  real,dimension(block_size) :: bi
  real,dimension(:),allocatable :: xi
  integer :: i, N_blocks, r, N_last, iteration, step, start, end, unit
  integer,dimension(:),allocatable :: i_start_block, i_end_block
  integer,dimension(:,:),allocatable :: ind_P
  logical :: verbose

  ! Check if we need to output
  verbose = output_file /= 'none'

  ! Give initial error estimate
  err = tol + 1.

  ! Initialize alterantion
  step = -1

  ! Initialize solution vector
  allocate(x(N), source=0.)

  ! Calculate number of blocks
  N_blocks = N/block_size
  r = modulo(N, block_size)
  if (r > 0) then
    N_blocks = N_blocks + 1
  end if

  ! Decompose blocks
  call decompose_blocks(N, A, N_blocks, block_size, A_blocks, N_last, ind_P, i_start_block, i_end_block)

  ! Write out header
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    write(unit,*) "BSSOR"
    write(unit,*) "iteration,||dx||,||err||,relaxation"
  end if

  ! Iterate
  iteration = 0
  do while(err >= tol .and. iteration < max_iter)

    ! Update iteration number
    iteration = iteration + 1

    ! Alternate direction
    if (step == 1) then
      start = N_blocks
      end = 1
      step = -1
    else
      start = 1
      end = N_blocks
      step = 1
    end if

    ! Perform iteration on blocks
    do i=start,end,step

      ! Last block
      if (i == N_blocks) then

        ! Calculate new RHS vector
        if (step == 1) then
          bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x_new(1:i_start_block(i)-1))
        else
          bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x(1:i_start_block(i)-1))
        end if

        ! Solve
        call lu_back_sub(A_blocks(1:N_last,1:N_last,i), N_last, ind_P(1:N_last,i), bi, xi)

        ! Store
        x_new(i_start_block(i):N) = (1.-rel)*x(i_start_block(i):N) + rel*xi

      ! Not last block
      else

        ! Calculate new RHS vector
        bi = b(i_start_block(i):i_end_block(i))
        if (step == 1) then
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x_new(1:i_start_block(i)-1))
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x(i_end_block(i)+1:N))
        else
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x(1:i_start_block(i)-1))
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x_new(i_end_block(i)+1:N))
        end if

        ! Solve
        call lu_back_sub(A_blocks(:,:,i), block_size, ind_P(:,i), bi, xi)

        ! Store
        x_new(i_start_block(i):i_end_block(i)) = (1.-rel)*x(i_start_block(i):i_end_block(i)) + rel*xi

      end if
    end do

    ! Calculate error
    vk = matmul(A, x_new)
    err = norm2(vk - b)
    dx = norm2(x - x_new)

    ! Output progress
    if (verbose) write(unit,'(i6, a, ES10.3, a, ES10.3, a, ES10.3)') iteration, ',', dx, ',', err, ',', rel

    ! Prepare for next iteration
    x = x_new

  end do

  close(unit)

  total_iter = iteration

end subroutine block_ssor_solve


subroutine block_jacobi_solve(N, A, b, block_size, tol, rel, max_iter, output_file, total_iter, x)
  ! Iteratively solves the [A]x=b system using the specified block Jacobi method
  ! Will alternate directions through the blocks on each iteration
  ! N is the size of the system
  ! A is the system matrix; block diagonals will be replaced with their LU decompositions
  ! b is the RHS vector
  ! block_size is the desired size of each block
  ! tol is the convergence tolerance between iterations
  ! rel is a relaxation factor between 0 and 2
  ! max_iter
  ! x is the solution

  implicit none

  integer,intent(in) :: N, block_size, max_iter
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  real,intent(inout) :: rel
  character(len=:),allocatable,intent(in) :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  real :: err, dx
  real,dimension(N) :: x_new, vk
  real,dimension(:,:,:),allocatable :: A_blocks
  real,dimension(block_size) :: bi
  real,dimension(:),allocatable :: xi
  integer :: i, N_blocks, r, N_last, iteration, step, unit
  integer,dimension(:),allocatable :: i_start_block, i_end_block
  integer,dimension(:,:),allocatable :: ind_P
  logical :: verbose

  ! Determine whether we need to output
  verbose = output_file /= 'none'

  ! Give initial error estimate
  err = tol + 1.

  ! Initialize alterantion
  step = -1

  ! Initialize solution vector
  allocate(x(N), source=0.)
  do i=1,N
    x(i) = b(i)/A(i,i)
  end do

  ! Calculate number of blocks
  N_blocks = N/block_size
  r = modulo(N, block_size)
  if (r > 0) then
    N_blocks = N_blocks + 1
  end if

  ! Decompose blocks
  call decompose_blocks(N, A, N_blocks, block_size, A_blocks, N_last, ind_P, i_start_block, i_end_block)

  ! Write out header
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    write(unit,*) "BJAC"
    write(unit,*) "N=", N
    write(unit,*) "iteration,||dx||,||err||,relaxation"
  end if

  ! Iterate
  iteration = 0
  do while(err >= tol .and. iteration < max_iter)

    ! Update iteration number
    iteration = iteration + 1

    ! Perform Jacobi iteration on blocks
    !$OMP parallel do private(bi, xi)
    do i=1,N_blocks

      ! Last block
      if (i == N_blocks) then

        ! Calculate new RHS vector
        bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x(1:i_start_block(i)-1))

        ! Solve
        call lu_back_sub(A_blocks(1:N_last,1:N_last,i), N_last, ind_P(1:N_last,i), bi, xi)

        ! Store
        x_new(i_start_block(i):N) = xi

      ! Not last block
      else

        ! Calculate new RHS vector
        bi = b(i_start_block(i):i_end_block(i))
        bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x(1:i_start_block(i)-1))
        bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x(i_end_block(i)+1:N))

        ! Solve
        call lu_back_sub(A_blocks(:,:,i), block_size, ind_P(:,i), bi, xi)

        ! Store
        x_new(i_start_block(i):i_end_block(i)) = xi

      end if
    end do

    ! Calculate new x
    x_new = (1.-rel)*x + rel*x_new

    ! Calculate error
    vk = matmul(A, x_new)
    err = norm2(vk - b)
    dx = norm2(x - x_new)

    ! Output progress
    if (verbose) write(unit,'(i6, a, ES10.3, a, ES10.3, a, ES10.3)') iteration, ',', dx, ',', err, ',', rel

    ! Prepare for next iteration
    x = x_new

  end do

  close(unit)

  total_iter = iteration

end subroutine block_jacobi_solve


subroutine purcell_solve(N, A, b, x)
  ! Solves the NxN linear system [A]x = b using Purcell's method ( [A | -b] [x | 1]^T = 0 )
  ! Taken from Chen, "Matrix Preconditioning Techniques and Applications", Cambridge Press, 2005, pp. 490-492.

  implicit none

  integer, intent(in) :: N
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(in) :: b
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, k, s
  integer,dimension(N) :: m
  real,dimension(N+1,N+1) :: V
  real,dimension(N+1) :: V_s, d
  real :: alpha, denom

  ! Initialize the solution space
  V(:,:) = 0.
  do i=1,N+1
    V(i,i) = 1.
  end do

  ! Loop through smaller and smaller solution spaces
  do i=N,1,-1

    ! Calculate inner products
    !$OMP parallel do
    do k=1,i+1
      d(k) = sum(A(N+1-i,:)*V(1:N,k)) - b(N+1-i)*V(N+1,k)
    end do

    ! Determine the pivot
    s = maxloc(abs(d(1:i+1)), 1)

    ! Get pivot vector
    V_s = V(:,s)

    ! Set elements of m(k) to skip the pivot element
    do k=1,i
      if (k < s) then
        m(k) = k
      else if (k >= s) then
        m(k) = k + 1
      end if
    end do

    ! Remove one dimension from the solution space
    denom = 1. / d(s)
    do k=1,i

      ! Calculate scale factor
      alpha = -d(m(k)) * denom

      ! Calculate new basis
      V(:,k) = alpha*V_s + V(:,m(k))

    end do
  end do

  ! Extract solution
  allocate(x, source=V(1:N,1) / V(N+1,1))
  
end subroutine purcell_solve


function get_lower_bandwidth(N, A) result(B_l)
  ! Calculates the lower bandwidth of the matrix A, which is assumed to be NxN

  implicit none
  
  integer,intent(in) :: N
  real,dimension(N,N),intent(in) :: A

  integer :: B_l

  integer :: i, j
  logical :: found_nonzero

  ! Initialize 
  B_l = 0

  ! Loop through rows, starting at the bottom
  !$OMP parallel do private(j, found_nonzero) reduction(max : B_l)
  do i=N,1,-1

    ! Initialize for this row
    found_nonzero = .false.

    ! Loop through columns starting at the left
    column_loop: do j=1,i-1

      ! Check this element
      found_nonzero = abs(A(i,j)) > 1.0e-12
      if (found_nonzero) exit column_loop

    end do column_loop

    ! We want a maximum bound on the bandwidth
    if (found_nonzero) then
      B_l = max(B_l, i-j)
    end if
  end do
  
end function get_lower_bandwidth


subroutine gen_givens_rot(x, y, c, s)
  ! Calculates the plane rotation from x and y
  ! Also applies the rotation to x and y

  implicit none
  
  real,intent(inout) :: x, y
  real,intent(out) :: c, s

  real :: d, t

  t = abs(x) + abs(y)
  d = t*sqrt((x/t)**2 + (y/t)**2)
  c = x/d
  s = y/d
  x = d
  y = 0.
  
end subroutine gen_givens_rot


subroutine apply_givens_row_rot(c, s, x, y, N)
  ! Applies the Givesn rotation to the two vectors x and y of length N

  implicit none
  
  real,intent(in) :: c, s
  real,dimension(N),intent(inout) :: x, y
  integer,intent(in) :: N

  real,dimension(:),allocatable :: t

  ! Apply to first row using temp storage
  allocate(t, source=c*x + s*y)

  ! Apply to second row
  y = c*y - s*x

  ! Move first row into place
  x = t
  
end subroutine apply_givens_row_rot


subroutine QR_givens_solve_UP(N, A, b, x)
  ! Solves the equation [A]x = b using the QR factorization via row-oriented Givens rotations
  ! Assumes A is upper-pentagonal

  implicit none
  
  integer,intent(in) :: N
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(inout) :: b
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, j, B_l
  real :: s, c

  ! Get lower bandwidth
  B_l = get_lower_bandwidth(N, A)

  ! Zero out lower triangle

  ! Loop through columns
  do j=1,N

    ! Loop through rows
    do i=min(j+B_l,N),j+1,-1

      ! Check nonzero
      if (A(i,j) /= 0.) then

        ! Generate Givens rotation
        call gen_givens_rot(A(j,j), A(i,j), c, s)

        ! Apply to rest of row
        call apply_givens_row_rot(c, s, A(j,j+1:), A(i,j+1:), N-j-1)

        ! Apply to b vector
        call apply_givens_row_rot(c, s, b(j), b(i), 1)

      end if

    end do
  end do

  ! Back substitution
  call upper_triangular_back_sub(N, A, b, x)
  
end subroutine QR_givens_solve_UP


subroutine upper_triangular_back_sub(N, R, b, x)
  ! Performs back substitution (not pivoted) on the system [R] x = b where [R] is an upper-triangular matrix

  implicit none
  
  integer,intent(in) :: N
  real,dimension(N,N),intent(in) :: R
  real,dimension(N),intent(in) :: b
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, j

  ! Initialize
  allocate(x(N))

  ! Back substitution
  do i=N,1,-1

    x(i) = b(i)

    ! Loop through known x values
    do j=i+1,N
      x(i) = x(i) - R(i,j)*x(j)
    end do

    ! Divide by diagonal coefficient
    if (R(i,i) /= 0.) then
      x(i) = x(i) / R(i,i)
    else
      write(*,*) "!!! Zero found on the diagonal of R. Upper-triangular back substitution failed. Quitting..."
      stop
    end if

  end do
  
end subroutine upper_triangular_back_sub


subroutine gen_fast_givens_rot(x, y, a, b, D1, D2, rot_type)
  ! Generates a fast Givens rotation

  implicit none
  
  real,intent(inout) :: x, y
  real,intent(out) :: a, b
  real,intent(inout) :: D1, D2
  integer,intent(out) :: rot_type

  real :: gamma, t, d, temp, ratio

  ! Preliminaries
  gamma = D1/D2
  if (x /= 0.) then
    ratio = (y*y)/(x*x)
  else
    ratio = gamma + 1.
  end if

  ! Determine case
  if (D1 >= D2) then
    if (ratio <= gamma) then

      rot_type = 1

      ! Calculate factors
      t = y/x
      b = t/gamma
      d = 1. + b*t
      a = t/d

      ! Scale diagonals
      D1 = D1/d
      D2 = D2*d

      ! Set first element
      x = x*d

    else

      rot_type = 3

      ! Calculate factors
      a = x/y
      t = a*gamma
      d = 1. + a*t
      b = t/d

      ! Scale diagonals
      temp = D2*d
      D2 = D1/d
      D1 = temp

      ! Set first element
      x = y

    end if

  else

    if (ratio <= gamma) then

      rot_type = 2

      ! Calculate factors
      a = y/x
      t = a/gamma
      d = 1. + a*t
      b = t/d

      ! Scale diagonals
      D1 = D1*d
      D2 = D2/d

      ! Set first element
      x = x

    else

      rot_type = 4

      ! Calculate factors
      t = x/y
      b = t*gamma
      d = 1. + b*t
      a = t/d

      ! Scale diagonals
      temp = D2/d
      D2 = D1*d
      D1 = temp

      ! Set first element
      x = y*d

    end if

  end if

  ! Zero second element
  y = 0.

end subroutine gen_fast_givens_rot


subroutine apply_fast_givens_rot(a, b, x, y, N, rot_type)
  ! Applies the fast Givens rotation to the vectors x and y

  implicit none
  
  real,intent(in) :: a, b
  real,dimension(N),intent(inout) :: x, y
  integer,intent(in) :: N, rot_type

  real,dimension(N) :: temp

  ! Select rotation type
  select case (rot_type)

  case (1)

    x = x + b*y
    y = y - a*x

  case (2)

    y = y - a*x
    x = x + b*y

  case (3)

    temp = y
    y = a*y - x
    x = temp - b*y

  case (4)

    temp = x
    x = b*x + y
    y = a*x - temp

  end select
  
end subroutine apply_fast_givens_rot


subroutine QR_fast_givens_solve_upper_pentagonal(N, A, b, x)
  ! Solves the equation [A]x = b using the QR factorization via fast Givens rotations
  ! Assumes A is upper-pentagonal

  implicit none
  
  integer,intent(in) :: N
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(inout) :: b
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, j, B_l, rot_type
  real :: alpha, beta
  real,dimension(N) :: D

  ! Initialize diagonals
  D = 1.

  ! Get lower bandwidth
  B_l = get_lower_bandwidth(N, A)

  ! Zero out lower triangle

  ! Loop through columns
  do j=1,N

    ! Loop through rows
    do i=min(j+B_l,N),j+1,-1

      ! Check nonzero
      if (A(i,j) /= 0.) then

        ! Generate Givens rotation
        call gen_fast_givens_rot(A(j,j), A(i,j), alpha, beta, D(j), D(i), rot_type)

        ! Apply to rest of row
        call apply_fast_givens_rot(alpha, beta, A(j,j+1:), A(i,j+1:), N-j-1, rot_type)

        ! Apply to b vector
        call apply_fast_givens_rot(alpha, beta, b(j), b(i), 1, rot_type)

      end if

    end do

  end do

  ! Back substitution
  call upper_triangular_back_sub(N, A, b, x)

end subroutine QR_fast_givens_solve_upper_pentagonal


subroutine arnoldi(N, A, k, q1, Q, H)
  ! Uses the Arnoldi algorithm to find an orthonormal basis for the k-th Krylov subspace of A

  implicit none
  
  integer,intent(in) :: N, k
  real,dimension(N,N) :: A
  real,dimension(N) :: q1
  real,dimension(:,:),allocatable,intent(out) :: Q, H

  integer :: i, j

  ! Allocate basis storage
  allocate(Q(N,k))
  allocate(H(k+1,k))

  ! Store first
  Q(:,1) = q1

  ! Assemble basis
  do j=1,k-1

    ! Add dimension to Krylov subspace
    Q(:,j+1) = matmul(A, Q(:,j))

    ! Orthogonalize
    do i=1,j
      H(i,j) = sum(Q(:,j+1)*Q(:,i))
      Q(:,j+1) = Q(:,j+1) - H(i,j)*Q(:,i)
    end do

    ! Normalize
    H(j+1,j) = norm2(Q(:,j+1))
    Q(:,j+1) = Q(:,j+1)/H(j+1,j)

  end do
  
end subroutine arnoldi


subroutine arnoldi_update(N, A, k, Q, H)
  ! Uses the Arnoldi algorithm to update the orthonormal basis

  implicit none
  
  integer,intent(in) :: N, k
  real,dimension(N,N),intent(in) :: A
  real,dimension(:,:),allocatable,intent(inout) :: Q, H

  integer :: i

  ! Add dimension to Krylov subspace
  Q(:,k+1) = matmul(A, Q(:,k))

  ! Orthogonalize
  do i=1,k
    H(i,k) = sum(Q(:,k+1)*Q(:,i))
    Q(:,k+1) = Q(:,k+1) - H(i,k)*Q(:,i)
  end do

  ! Normalize
  H(k+1,k) = norm2(Q(:,k+1))
  Q(:,k+1) = Q(:,k+1)/H(k+1,k)
  
end subroutine arnoldi_update


subroutine GMRES(N, A, b, tol, max_iter, output_file, total_iter, x)
  ! Uses the generalized minimum residual algorithm to estimate the solution to Ax=b
  ! Taken from "Handbook of Linear Algebra" Leslie Hogben ed.

  implicit none
  
  integer,intent(in) :: N, max_iter
  real,dimension(N,N),intent(in) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  character(len=:),allocatable :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, k, k_max, unit
  real,dimension(:,:),allocatable :: Q, H
  real,dimension(N) :: E, r0
  real :: beta, temp, err, d
  real,dimension(:),allocatable :: y, c, s
  logical :: verbose

  ! We cannot have more iterations than the size of the matrix
  k_max = min(N, max_iter)

  ! Allocate memory for storing the orthonormal basis and rotations
  allocate(Q(N,k_max), source=0.)
  allocate(H(k_max+1,k_max), source=0.)
  allocate(c(k_max), source=0.)
  allocate(s(k_max), source=0.)

  ! Initialize
  E(1) = 1.
  E(2:) = 0.
  r0 = b
  beta = norm2(r0)
  Q(:,1) = r0/beta

  ! Start output
  verbose = output_file /= 'none'
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    write(unit,*) "GMRES"
    write(unit,*) "N=", N
    write(unit,*) "iteration,||err||"
  end if

  ! Loop through subspaces
  k = 0
  err = tol + 1
  do while (err > tol .and. k < k_max-1)

    k = k + 1

    ! Update orthonormal basis using Arnoldi algorithm
    call arnoldi_update(N, A, k, Q, H)

    ! Apply rotations to new last column of H
    do i=1,k-1
      temp = c(i)*H(i,k) + s(i)*H(i+1,k)
      H(i+1,k) = -s(i)*h(i,k) + c(i)*H(i+1,k)
      H(i,k) = temp
    end do

    ! Calculate k-th rotation
    d = sqrt(H(k,k)**2 + H(k+1,k)**2)
    c(k) = abs(H(k,k)) / d
    s(k) = sign(1., H(k,k))*H(k+1,k) / d

    ! Apply to H
    H(k,k) = c(k)*H(k,k) + s(k)*H(k+1,k)
    H(k+1,k) = 0.

    ! Apply to E
    E(k+1) = -s(k)*E(k)
    E(k) = c(k)*E(k)

    ! Calculate residual norm estimate
    err = beta*abs(E(k+1))

    ! Output progress
    if (verbose) write(unit,'(i6, a, ES10.3)') k, ',', err

    ! Check termination
    if (err < tol) exit

  end do

  ! Solve upper triangular system
  call upper_triangular_back_sub(k, H(1:k,1:k), beta*E(1:k), y)

  ! Get estimate for x
  x = matmul(Q(:,1:k), y)

  ! Finish output
  if (verbose) close(unit)

  total_iter = k
  
end subroutine GMRES


subroutine restarted_GMRES(N, A, b, tol, max_iter, restart_iter, output_file, total_iter, x)
  ! Uses the generalized minimum residual algorithm to estimate the solution to Ax=b
  ! with restarts to improve performance
  ! Taken from "Handbook of Linear Algebra" Leslie Hogben ed.

  implicit none
  
  integer,intent(in) :: N, max_iter, restart_iter
  real,dimension(N,N),intent(in) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  character(len=:),allocatable :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  integer :: i, k, k_max, unit, outer_iter
  real,dimension(:,:),allocatable :: Q, H
  real,dimension(N) :: E, r0
  real :: beta, temp, err, d
  real,dimension(:),allocatable :: y, c, s
  logical :: verbose

  ! Intialize iterations
  total_iter = 0
  outer_iter = 0

  ! We cannot have more iterations than the size of the matrix
  k_max = min(restart_iter, N)

  ! Allocate memory for storing the orthonormal basis and rotations
  allocate(Q(N,k_max))
  allocate(H(k_max+1,k_max))
  allocate(c(k_max))
  allocate(s(k_max))

  ! Initial guess
  allocate(x(N), source=0.)

  ! Start output
  verbose = output_file /= 'none'
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    write(unit,*) "GMRES"
    write(unit,*) "N=", N
    write(unit,*) "iteration,outer iteration,inner iteration,||err||"
  end if

  ! Iterate through restarts
  err = tol + 1
  do while (err > tol .and. total_iter <= max_iter)

    outer_iter = outer_iter + 1

    ! Wipe previous iteration
    Q = 0.
    H = 0.
    c = 0.
    s = 0.

    ! Initialize
    E(1) = 1.
    E(2:) = 0.
    r0 = b - matmul(A, x)
    beta = norm2(r0)
    Q(:,1) = r0/beta

    ! Loop through subspaces
    k = 0
    do while (err > tol .and. k < k_max-1)

      k = k + 1
      total_iter = total_iter + 1

      ! Update orthonormal basis using Arnoldi algorithm
      call arnoldi_update(N, A, k, Q, H)

      ! Apply rotations to new last column of H
      do i=1,k-1
        temp = c(i)*H(i,k) + s(i)*H(i+1,k)
        H(i+1,k) = -s(i)*h(i,k) + c(i)*H(i+1,k)
        H(i,k) = temp
      end do

      ! Calculate k-th rotation
      d = sqrt(H(k,k)**2 + H(k+1,k)**2)
      c(k) = abs(H(k,k)) / d
      s(k) = sign(1., H(k,k))*H(k+1,k) / d

      ! Apply to H
      H(k,k) = c(k)*H(k,k) + s(k)*H(k+1,k)
      H(k+1,k) = 0.

      ! Apply to E
      E(k+1) = -s(k)*E(k)
      E(k) = c(k)*E(k)

      ! Calculate upper residual norm estimate
      err = beta*abs(E(k+1))

      ! Output progress
      if (verbose) write(unit,'(i6, a, i6, a, i6, a, ES10.3)') total_iter, ',', outer_iter, ',', k, ',', err

    end do

    ! Solve upper triangular system
    call upper_triangular_back_sub(k, H(1:k,1:k), beta*E(1:k), y)

    ! Update estimate for x
    x = x + matmul(Q(:,1:k), y)

  end do

  ! Finish output
  if (verbose) close(unit)

end subroutine restarted_GMRES

!!! --------------------------------------------------------------------------------------------------
!!! OBSOLETE FUNCTIONS
!!! --------------------------------------------------------------------------------------------------

subroutine block_ssor_solve_with_adaptation(N, A, b, block_size, tol, rel, max_iter, output_file, total_iter, x)
  ! Iteratively solves the [A]x=b system using block symmetric successive overrelaxation
  ! N is the size of the system
  ! A is the system matrix; block diagonals will be replaced with their LU decompositions
  ! b is the RHS vector
  ! block_size is the desired size of each block
  ! tol is the convergence tolerance between iterations
  ! rel is a relaxation factor
  ! max_iter
  ! verbose
  ! x is the solution

  ! rel between 0 and 2 specifies a constant relaxation factor
  ! rel = -1 specifies the relaxation factor is to be adaptively updated

  implicit none

  integer,intent(in) :: N, block_size, max_iter
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  real,intent(inout) :: rel
  character(len=:),allocatable,intent(in) :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  real :: err, dx 
  real,dimension(N) :: x_new, vk
  real,dimension(:,:,:),allocatable :: A_blocks
  real,dimension(block_size) :: bi
  real,dimension(:),allocatable :: xi
  integer :: i, N_blocks, r, N_last, iteration, step, start, end, unit
  integer,dimension(:),allocatable :: i_start_block, i_end_block
  integer,dimension(:,:),allocatable :: ind_P
  logical :: adaptive, verbose

  ! Check if we need to output
  verbose = output_file /= 'none'

  ! Give initial error estimate
  err = tol + 1.

  ! Initialize alterantion
  step = -1

  ! Check for adaptive relaxation factor
  if (rel < 0.) then
    adaptive = .true.
    rel = 0.5
  else
    adaptive = .false.
  end if

  ! Initialize solution vector
  allocate(x(N), source=0.)

  ! Calculate number of blocks
  N_blocks = N/block_size
  r = modulo(N, block_size)
  if (r > 0) then
    N_blocks = N_blocks + 1
  end if

  ! Decompose blocks
  call decompose_blocks(N, A, N_blocks, block_size, A_blocks, N_last, ind_P, i_start_block, i_end_block)

  ! Write out header
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    if (adaptive) then
      write(unit,*) "ABSSOR"
    else
      write(unit,*) "BSSOR"
    end if
    write(unit,*) "iteration,||dx||,||err||,relaxation"
  end if

  ! Iterate
  iteration = 0
  do while(err >= tol .and. iteration < max_iter)

    ! Update iteration number
    iteration = iteration + 1

    ! Alternate direction
    if (step == 1) then
      start = N_blocks
      end = 1
      step = -1
    else
      start = 1
      end = N_blocks
      step = 1
    end if

    ! Perform iteration on blocks
    do i=start,end,step

      ! Last block
      if (i == N_blocks) then

        ! Calculate new RHS vector
        if (step == 1) then
          bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x_new(1:i_start_block(i)-1))
        else
          bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x(1:i_start_block(i)-1))
        end if

        ! Solve
        call lu_back_sub(A_blocks(1:N_last,1:N_last,i), N_last, ind_P(1:N_last,i), bi, xi)

        ! Store
        x_new(i_start_block(i):N) = (1.-rel)*x(i_start_block(i):N) + rel*xi

      ! Not last block
      else

        ! Calculate new RHS vector
        bi = b(i_start_block(i):i_end_block(i))
        if (step == 1) then
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x_new(1:i_start_block(i)-1))
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x(i_end_block(i)+1:N))
        else
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x(1:i_start_block(i)-1))
          bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x_new(i_end_block(i)+1:N))
        end if

        ! Solve
        call lu_back_sub(A_blocks(:,:,i), block_size, ind_P(:,i), bi, xi)

        ! Store
        x_new(i_start_block(i):i_end_block(i)) = (1.-rel)*x(i_start_block(i):i_end_block(i)) + rel*xi

      end if
    end do

    ! Calculate error
    vk = matmul(A, x_new)
    err = norm2(vk - b)
    dx = norm2(x - x_new)

    ! Update relaxation for ABSOR method
    if (adaptive) then

      ! Aim for a step size that's of the same order of magnitude as the error
      rel = rel - 0.1*(log10(dx/err))

      ! Nowhere should we go above 2
      if (rel > 2.) rel = 2.

      ! Make sure the relaxation factor stays positive
      if (rel < 0.) rel = 0.01
    end if

    ! Output progress
    if (verbose) write(unit,'(i6, a, ES10.3, a, ES10.3, a, ES10.3)') iteration, ',', dx, ',', err, ',', rel

    ! Prepare for next iteration
    x = x_new

  end do

  close(unit)

  total_iter = iteration

end subroutine block_ssor_solve_with_adaptation


subroutine block_jacobi_solve_with_optimal(N, A, b, block_size, tol, rel, max_iter, output_file, total_iter, x)
  ! Iteratively solves the [A]x=b system using the specified block Jacobi method
  ! Will alternate directions through the blocks on each iteration
  ! N is the size of the system
  ! A is the system matrix; block diagonals will be replaced with their LU decompositions
  ! b is the RHS vector
  ! block_size is the desired size of each block
  ! tol is the convergence tolerance between iterations
  ! rel is a relaxation factor
  ! max_iter
  ! x is the solution

  ! rel between 0 and 2 will specify the relaxation factor should be constant
  ! rel = -1 specifies that the optimal relaxation factor is to be used at each iteration

  implicit none

  integer,intent(in) :: N, block_size, max_iter
  real,dimension(N,N),intent(inout) :: A
  real,dimension(N),intent(in) :: b
  real,intent(in) :: tol
  real,intent(inout) :: rel
  character(len=:),allocatable,intent(in) :: output_file
  integer,intent(out) :: total_iter
  real,dimension(:),allocatable,intent(out) :: x

  real :: err, dx, vkTvk, bTvk, vkp1Tvkp1, vkp1Tvk, bTvkp1
  real,dimension(N) :: x_new, vk, vkp1
  real,dimension(:,:,:),allocatable :: A_blocks
  real,dimension(block_size) :: bi
  real,dimension(:),allocatable :: xi
  integer :: i, N_blocks, r, N_last, iteration, step, unit
  integer,dimension(:),allocatable :: i_start_block, i_end_block
  integer,dimension(:,:),allocatable :: ind_P
  logical :: optimal_rel, verbose

  ! Determine whether we need to output
  verbose = output_file /= 'none'

  ! Give initial error estimate
  err = tol + 1.

  ! Initialize alterantion
  step = -1

  ! Initialize method
  if (rel < 0) then
    optimal_rel = .true.
    vk = 0.
  else
    optimal_rel = .false.
  end if

  ! Initialize solution vector
  allocate(x(N), source=0.)
  do i=1,N
    x(i) = b(i)/A(i,i)
  end do

  ! Calculate number of blocks
  N_blocks = N/block_size
  r = modulo(N, block_size)
  if (r > 0) then
    N_blocks = N_blocks + 1
  end if

  ! Decompose blocks
  call decompose_blocks(N, A, N_blocks, block_size, A_blocks, N_last, ind_P, i_start_block, i_end_block)

  ! Write out header
  if (verbose) then
    open(newunit=unit, file=output_file)
    write(unit,*) "method"
    if (optimal_rel) then
      write(unit,*) "ORBJ"
    else
      write(unit,*) "BJAC"
    end if
    write(unit,*) "N=", N
    write(unit,*) "iteration,||dx||,||err||,relaxation"
  end if

  ! Iterate
  iteration = 0
  do while(err >= tol .and. iteration < max_iter)

    ! Update iteration number
    iteration = iteration + 1

    ! Perform Jacobi iteration on blocks
    !$OMP parallel do private(bi, xi)
    do i=1,N_blocks

      ! Last block
      if (i == N_blocks) then

        ! Calculate new RHS vector
        bi(1:N_last) = b(i_start_block(i):N) - matmul(A(i_start_block(i):N,1:i_start_block(i)-1), x(1:i_start_block(i)-1))

        ! Solve
        call lu_back_sub(A_blocks(1:N_last,1:N_last,i), N_last, ind_P(1:N_last,i), bi, xi)

        ! Store
        x_new(i_start_block(i):N) = xi

      ! Not last block
      else

        ! Calculate new RHS vector
        bi = b(i_start_block(i):i_end_block(i))
        bi = bi - matmul(A(i_start_block(i):i_end_block(i),1:i_start_block(i)-1), x(1:i_start_block(i)-1))
        bi = bi - matmul(A(i_start_block(i):i_end_block(i),i_end_block(i)+1:N), x(i_end_block(i)+1:N))

        ! Solve
        call lu_back_sub(A_blocks(:,:,i), block_size, ind_P(:,i), bi, xi)

        ! Store
        x_new(i_start_block(i):i_end_block(i)) = xi

      end if
    end do

    ! Calculate optimal relaxation for the ORBJ method
    if (optimal_rel) then

      ! Preliminary quantities
      vkTvk = dot_product(vk, vk)
      bTvk = dot_product(b, vk)
      vkp1 = matmul(A, x_new)
      bTvkp1 = dot_product(b, vkp1)
      vkp1Tvk = dot_product(vkp1, vk)
      vkp1Tvkp1 = dot_product(vkp1, vkp1)

      ! Check for zero denominator
      if (vkTvk - 2.*vkp1Tvk + vkp1Tvkp1 /= 0.) then
        rel = (vkTvk - vkp1Tvk - bTvk + bTvkp1) / (vkTvk - 2.*vkp1Tvk + vkp1Tvkp1)
      else
        rel = 1.0
      end if

    end if

    ! Calculate new x
    x_new = (1.-rel)*x + rel*x_new

    ! Calculate error
    if (optimal_rel) then
      vk = (1.-rel)*vk + rel*vkp1
    else
      vk = matmul(A, x_new)
    end if
    err = norm2(vk - b)
    dx = norm2(x - x_new)

    ! Output progress
    if (verbose) write(unit,'(i6, a, ES10.3, a, ES10.3, a, ES10.3)') iteration, ',', dx, ',', err, ',', rel

    ! Prepare for next iteration
    x = x_new

  end do

  close(unit)

  total_iter = iteration

end subroutine block_jacobi_solve_with_optimal


subroutine diagonal_preconditioner(N, A, b, A_p, b_p)
    ! Applies the preconditioning M^-1 A x = M^-1 b where
    ! M is the diagonal elements of A

    implicit none

    integer,intent(in) :: N
    real,dimension(N,N),intent(in) :: A
    real,dimension(N),intent(in) :: b
    real,dimension(:,:),allocatable,intent(out) :: A_p
    real,dimension(:),allocatable,intent(out) :: b_p

    real,dimension(N) :: A_ii_inv
    integer :: i, j

    ! Get preconditioning matrix
    do i=1,N
        A_ii_inv = 1./A(i,i)
    end do

    ! Allocate
    allocate(A_p(N,N))
    allocate(b_p(N))

    ! Apply preconditioning
    do j=1,N
        do i=1,N
            A_p(i,j) = A_ii_inv(i)*A(i,j)
        end do

        b_p(j) = b(j)*A_ii_inv(j)
    end do
    
end subroutine diagonal_preconditioner


end module linalg_mod