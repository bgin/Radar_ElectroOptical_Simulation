c
c     file resc3.f
c
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c     *                                                               *
c     *                  copyright (c) 2008 by UCAR                   *
c     *                                                               *
c     *       University Corporation for Atmospheric Research         *
c     *                                                               *
c     *                      all rights reserved                      *
c     *                                                               *
c     *                     MUDPACK  version 5.0.1                    *
c     *                                                               *
c     *                 A Fortran Package of Multigrid                *
c     *                                                               *
c     *                Subroutines and Example Programs               *
c     *                                                               *
c     *      for Solving Elliptic Partial Differential Equations      *
c     *                                                               *
c     *                             by                                *
c     *                                                               *
c     *                         John Adams                            *
c     *                                                               *
c     *                             of                                *
c     *                                                               *
c     *         the National Center for Atmospheric Research          *
c     *                                                               *
c     *                Boulder, Colorado  (80307)  U.S.A.             *
c     *                                                               *
c     *                   which is sponsored by                       *
c     *                                                               *
c     *              the National Science Foundation                  *
c     *                                                               *
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c     subroutine resc3(nx,ny,nz,work,res)
c
c ... purpose
c
c
c     subroutine resc3 computes the fine grid residual in the nx by ny by nz
c     array res after calling cuh3 or cud3 or cud3sa.  if
c
c          L * p = f
c
c     is the n by n (n = nx*ny*nz) block tri-diagonal linear system resulting
c     from the pde discretization (done internally in cud3 or cuh3) and phi
c     is the approximation to p obtained by calling cud3, then resc3 computes
c     the nx by ny by nz residual array
c
c          res = f - L * phi.
c
c     one of the vector norms of res,
c
c          || res ||
c
c     can be computed as a "measure" of how well phi satisfies the
c     discretization equations.  for example, the following statements
c     will compute the location and size of the maximum residual in res
c     on cray computers:
c
c          ijk = isamax(nx*ny*nz,res,1)
c
c          kmax = (ijk-1)/(nx*ny) + 1
c
c          jmax = (ijk-(kmax-1)*nx*ny-1)/nx + 1
c
c          imax = ij - nx*((kmax-1)*ny-jmax+1)
c
c          resmax = abs(res(imax,jmax,kmax))
c
c ... see documentation and test files provided in this distribution
c
c ... notes
c
c     let pe be the exact continuous solution to the elliptic pde
c     evaluated on the nx by ny by nz discretization grid;
c
c     let p be the exact solution to the linear discretization;
c
c     let phi be the approximation to p generated by the mudpack solver;
c
c     then discretization level error is defined by the condition
c
c          || phi - p || < || p - pe ||.
c                        =
c
c     a common measure of multigrid efficieny is that discretization level
c     error is reached in one full multigrid cycle (see references [2,9] in
c     the mudpack file "readme").  this can happen before the residual is
c     reduced to the level of roundoff error.  consequently, || res || is
c     a conservative measure of accuracy which can be wasteful if multi-
c     grid cycles are executed until it reaches the level of roundoff error.
c
c     || res || can be used to estimate the convergence rate of multigrid
c     iteration.  let r(n) be the residual and e(n) be the error after
c     executing n cycles.  they are related by the residual equation
c
c          L * e(n) = r(n).
c
c     it follows that the ratio
c
c          || r(n+1) || / || r(n) ||
c
c     estimates
c
c          || e(n+1) || / || e(n) ||
c
c     which in turn estimates the convergence rate
c
c          c = max || e(k+1) || / || e(k) ||.
c               k
c
c     notice
c                         n
c          || e(n) || <  c  || e(0) ||.
c
c
c ... assumptions (see cud3 or cud3sa or cuh3 documentation)
c
c     (1) nx,ny,nz have the same values as iparm(14),iparm(15),iparm(16)
c         (used to set the fine grid resolution when calling cud3 or cuh3 or cud3sa)
c
c     (2) work,phi are the same parameters used in calling cud3 or cuh3 or cud3sa
c
c     (3) work,phi have not changed since the last call to cud3 or cud3sa or cuh3
c
c
c     (3) assures a copy of the last approximation phi is contained in work.
c     if these assumptions are not true then resc3 cannot compute the
c     residual in res.
c
      subroutine resc3(nx,ny,nz,work,res)
c
c     compute fine grid residual in res after calling cud3 or cud3sa or cuh3
c
      implicit none
      integer nx,ny,nz,ic
      complex work(*),res(nx,ny,nz)
c
c     set coefficient pointer
c
      ic = 1+(nx+2)*(ny+2)*(nz+2)
      ic = 1+(nx+2)*(ny+2)*(nz+2)
      call rec3(nx,ny,nz,work,work(ic),res)
      return
      end

      subroutine rec3(nx,ny,nz,phi,cof,res)
      implicit none
      integer nx,ny,nz,i,j,k
      complex cof(nx,ny,nz,8),phi(0:nx+1,0:ny+1,0:nz+1),res(nx,ny,nz)
      do k=1,nz
	do j=1,ny
	  do i=1,nx
	    res(i,j,k) =  cof(i,j,k,8) - (
     +                    cof(i,j,k,1)*phi(i-1,j,k)+
     +                    cof(i,j,k,2)*phi(i+1,j,k)+
     +                    cof(i,j,k,3)*phi(i,j-1,k)+
     +                    cof(i,j,k,4)*phi(i,j+1,k)+
     +                    cof(i,j,k,5)*phi(i,j,k-1)+
     +                    cof(i,j,k,6)*phi(i,j,k+1)+
     +                    cof(i,j,k,7)*phi(i,j,k) )
	  end do
	end do
      end do
      return
      end
