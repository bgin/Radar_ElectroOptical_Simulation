MODULE nrutil
	!USE nrtype
        USE mod_kinds, only : i4,sp,dp
        IMPLICIT NONE
       
	!TYPE sprs2_sp
	!	INTEGER(i4) :: n,len
	!	REAL(sp), DIMENSION(:), POINTER :: val
	!	INTEGER(i4), DIMENSION(:), POINTER :: irow
	!	INTEGER(i4), DIMENSION(:), POINTER :: jcol
	!END TYPE sprs2_sp
	!TYPE sprs2_dp
	!	INTEGER(i4) :: n,len
	!	REAL(dp), DIMENSION(:), POINTER :: val
	!	INTEGER(i4), DIMENSION(:), POINTER :: irow
	!	INTEGER(i4), DIMENSION(:), POINTER :: jcol
	!END TYPE sprs2_dp
	
        REAL(sp), PARAMETER :: PI=3.141592653589793238462643383279502884197_sp
	REAL(sp), PARAMETER :: PIO2=1.57079632679489661923132169163975144209858_sp
	REAL(sp), PARAMETER :: TWOPI=6.283185307179586476925286766559005768394_sp
	REAL(sp), PARAMETER :: SQRT2=1.41421356237309504880168872420969807856967_sp
	REAL(sp), PARAMETER :: EULER=0.5772156649015328606065120900824024310422_sp
	REAL(dp), PARAMETER :: PI_D=3.141592653589793238462643383279502884197_dp
	REAL(dp), PARAMETER :: PIO2_D=1.57079632679489661923132169163975144209858_dp
	REAL(dp), PARAMETER :: TWOPI_D=6.283185307179586476925286766559005768394_dp
	INTEGER(i4), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
	INTEGER(i4), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
	INTEGER(i4), PARAMETER :: NPAR_CUMSUM=16
	INTEGER(i4), PARAMETER :: NPAR_CUMPROD=8
	INTEGER(i4), PARAMETER :: NPAR_POLY=8
	INTEGER(i4), PARAMETER :: NPAR_POLYTERM=8
	INTERFACE array_copy
		MODULE PROCEDURE array_copy_r, array_copy_d, array_copy_i
	END INTERFACE
	INTERFACE swap
		MODULE PROCEDURE swap_i,swap_r,swap_rv,swap_c, &
			swap_cv,swap_cm,swap_z,swap_zv,swap_zm, &
			masked_swap_rs,masked_swap_rv,masked_swap_rm
	END INTERFACE
	INTERFACE reallocate
		MODULE PROCEDURE reallocate_rv,reallocate_rm,&
			reallocate_iv,reallocate_im,reallocate_hv
	END INTERFACE
	INTERFACE imaxloc
		MODULE PROCEDURE imaxloc_r,imaxloc_i
	END INTERFACE
	INTERFACE assert
		MODULE PROCEDURE assert1,assert2,assert3,assert4,assert_v
	END INTERFACE
	INTERFACE assert_eq
		MODULE PROCEDURE assert_eq2,assert_eq3,assert_eq4,assert_eqn
	END INTERFACE
	!INTERFACE arth
	!	MODULE PROCEDURE arth_r, arth_d, arth_i
	!END INTERFACE
	!INTERFACE geop
	!	MODULE PROCEDURE geop_r, geop_d, geop_i, geop_c, geop_dv
	!END INTERFACE
	!INTERFACE cumsum
	!	MODULE PROCEDURE cumsum_r,cumsum_i
	!END INTERFACE
	!INTERFACE poly
	!	MODULE PROCEDURE poly_rr,poly_rrv,poly_dd,poly_ddv,&
	!		poly_rc,poly_cc,poly_msk_rrv,poly_msk_ddv
	!END INTERFACE
	!INTERFACE poly_term
	!	MODULE PROCEDURE poly_term_rr,poly_term_cc
	!END INTERFACE
	!INTERFACE outerprod
	!	MODULE PROCEDURE outerprod_r,outerprod_d
	!END INTERFACE
	!INTERFACE outerdiff
	!	MODULE PROCEDURE outerdiff_r,outerdiff_d,outerdiff_i
	!END INTERFACE
	!INTERFACE scatter_add
	!	MODULE PROCEDURE scatter_add_r,scatter_add_d
	!END INTERFACE
	!INTERFACE scatter_max
	!	MODULE PROCEDURE scatter_max_r,scatter_max_d
	!END INTERFACE
	!INTERFACE diagadd
	!	MODULE PROCEDURE diagadd_rv,diagadd_r
	!END INTERFACE
	!INTERFACE diagmult
	!	MODULE PROCEDURE diagmult_rv,diagmult_r
	!END INTERFACE
	INTERFACE get_diag
		MODULE PROCEDURE get_diag_rv, get_diag_dv
	END INTERFACE
	INTERFACE put_diag
		MODULE PROCEDURE put_diag_rv, put_diag_r
	END INTERFACE
CONTAINS
!BL
	SUBROUTINE array_copy_r(src,dest,n_copied,n_not_copied)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: array_copy_r
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: array_copy_r
          !dir$ attributes optimization_parameter: 'g2s=on' :: array_copy_r
	REAL(sp), DIMENSION(:), INTENT(IN) :: src
	REAL(sp), DIMENSION(:), INTENT(OUT) :: dest
	INTEGER(i4), INTENT(OUT) :: n_copied, n_not_copied
	n_copied=min(size(src),size(dest))
	n_not_copied=size(src)-n_copied
        !dir$ assume_aligned dest:64,src:64
        !dir$ vector aligned
	dest(1:n_copied)=src(1:n_copied)
	END SUBROUTINE array_copy_r
!BL
	SUBROUTINE array_copy_d(src,dest,n_copied,n_not_copied)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: array_copy_d
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: array_copy_d
          !dir$ attributes optimization_parameter: 'g2s=on' :: array_copy_d
	REAL(dp), DIMENSION(:), INTENT(IN) :: src
	REAL(dp), DIMENSION(:), INTENT(OUT) :: dest
	INTEGER(i4), INTENT(OUT) :: n_copied, n_not_copied
	n_copied=min(size(src),size(dest))
	n_not_copied=size(src)-n_copied
        !dir$ assume_aligned dest:64,src:64
        !dir$ vector aligned
	dest(1:n_copied)=src(1:n_copied)
	END SUBROUTINE array_copy_d
!BL
	SUBROUTINE array_copy_i(src,dest,n_copied,n_not_copied)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: array_copy_i
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: array_copy_i
          !dir$ attributes optimization_parameter: 'g2s=on' :: array_copy_i
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: src
	INTEGER(i4), DIMENSION(:), INTENT(OUT) :: dest
	INTEGER(i4), INTENT(OUT) :: n_copied, n_not_copied
	n_copied=min(size(src),size(dest))
	n_not_copied=size(src)-n_copied
	dest(1:n_copied)=src(1:n_copied)
	END SUBROUTINE array_copy_i
!BL
!BL
	SUBROUTINE swap_i(a,b)
         !dir$ attributes forceinline :: swap_i
	INTEGER(i4), INTENT(INOUT) :: a,b
	INTEGER(i4) :: dum
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_i
!BL
	SUBROUTINE swap_r(a,b)
          !dir$ attributes forceinline :: swap_r
	REAL(sp), INTENT(INOUT) :: a,b
	REAL(sp) :: dum
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_r
!BL
	SUBROUTINE swap_rv(a,b)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: swap_rv
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: swap_rv
          !dir$ attributes optimization_parameter: 'g2s=on' :: swap_rv
	REAL(sp), DIMENSION(:), INTENT(INOUT) :: a,b
	REAL(sp), DIMENSION(SIZE(a)) :: dum
        !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_rv
!BL
	SUBROUTINE swap_c(a,b)
         !dir$ attributes forceinline :: swap_c
	COMPLEX(sp), INTENT(INOUT) :: a,b
	COMPLEX(sp) :: dum
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_c
!BL
	SUBROUTINE swap_cv(a,b)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: swap_cv
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: swap_cv
          !dir$ attributes optimization_parameter: 'g2s=on' :: swap_cv
	COMPLEX(sp), DIMENSION(:), INTENT(INOUT) :: a,b
	COMPLEX(sp), DIMENSION(SIZE(a)) :: dum
        !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_cv
!BL
	SUBROUTINE swap_cm(a,b)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: swap_cm
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: swap_cm
          !dir$ attributes optimization_parameter: 'g2s=on' :: swap_cm
	COMPLEX(sp), DIMENSION(:,:), INTENT(INOUT) :: a,b
	COMPLEX(sp), DIMENSION(size(a,1),size(a,2)) :: dum
         !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_cm
!BL
	SUBROUTINE swap_z(a,b)
         !dir$ attributes forceinline :: swap_z
	COMPLEX(dp), INTENT(INOUT) :: a,b
	COMPLEX(dp) :: dum
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_z
!BL
	SUBROUTINE swap_zv(a,b)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: swap_zv
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: swap_zv
          !dir$ attributes optimization_parameter: 'g2s=on' :: swap_zv
	COMPLEX(dp), DIMENSION(:), INTENT(INOUT) :: a,b
	COMPLEX(dp), DIMENSION(SIZE(a)) :: dum
         !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_zv
!BL
	SUBROUTINE swap_zm(a,b)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: swap_zm
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: swap_zm
          !dir$ attributes optimization_parameter: 'g2s=on' :: swap_zm
	COMPLEX(dp), DIMENSION(:,:), INTENT(INOUT) :: a,b
	COMPLEX(dp), DIMENSION(size(a,1),size(a,2)) :: dum
         !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_zm
!BL
	SUBROUTINE masked_swap_rs(a,b,mask)
         !dir$ attributes forceinline :: masked_swap
	REAL(sp), INTENT(INOUT) :: a,b
	LOGICAL(i4), INTENT(IN) :: mask
	REAL(sp) :: swp
	if (mask) then
		swp=a
		a=b
		b=swp
	end if
	END SUBROUTINE masked_swap_rs
!BL
	SUBROUTINE masked_swap_rv(a,b,mask)
            !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: masked_swap_rv
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: masked_swap_rv
          !dir$ attributes optimization_parameter: 'g2s=on' :: masked_swap_rv
	REAL(sp), DIMENSION(:), INTENT(INOUT) :: a,b
	LOGICAL(i4), DIMENSION(:), INTENT(IN) :: mask
	REAL(sp), DIMENSION(size(a)) :: swp
         !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	where (mask)
		swp=a
		a=b
		b=swp
	end where
	END SUBROUTINE masked_swap_rv
!BL
	SUBROUTINE masked_swap_rm(a,b,mask)
          !dir$ optimize:3 
          !dir$ attributes code_align : 32 :: masked_swap_rm
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: masked_swap_rm
          !dir$ attributes optimization_parameter: 'g2s=on' :: masked_swap_rm
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: a,b
	LOGICAL(i4), DIMENSION(:,:), INTENT(IN) :: mask
	REAL(sp), DIMENSION(size(a,1),size(a,2)) :: swp
         !dir$ assume_aligned a:64,b:64,dum:64
        !dir$ vector aligned
	where (mask)
		swp=a
		a=b
		b=swp
	end where
	END SUBROUTINE masked_swap_rm
!BL
!BL
	FUNCTION reallocate_rv(p,n)
	REAL(sp), DIMENSION(:), POINTER :: p, reallocate_rv
	INTEGER(i4), INTENT(IN) :: n
	INTEGER(i4) :: nold,ierr
	allocate(reallocate_rv(n),stat=ierr)
	if (ierr /= 0) call &
		nrerror('reallocate_rv: problem in attempt to allocate memory')
	if (.not. associated(p)) RETURN
	nold=size(p)
	reallocate_rv(1:min(nold,n))=p(1:min(nold,n))
	deallocate(p)
	END FUNCTION reallocate_rv
!BL
	FUNCTION reallocate_iv(p,n)
	INTEGER(i4), DIMENSION(:), POINTER :: p, reallocate_iv
	INTEGER(i4), INTENT(IN) :: n
	INTEGER(i4) :: nold,ierr
	allocate(reallocate_iv(n),stat=ierr)
	if (ierr /= 0) call &
		nrerror('reallocate_iv: problem in attempt to allocate memory')
	if (.not. associated(p)) RETURN
	nold=size(p)
	reallocate_iv(1:min(nold,n))=p(1:min(nold,n))
	deallocate(p)
	END FUNCTION reallocate_iv
!BL
	FUNCTION reallocate_hv(p,n)
	CHARACTER(1), DIMENSION(:), POINTER :: p, reallocate_hv
	INTEGER(i4), INTENT(IN) :: n
	INTEGER(i4) :: nold,ierr
	allocate(reallocate_hv(n),stat=ierr)
	if (ierr /= 0) call &
		nrerror('reallocate_hv: problem in attempt to allocate memory')
	if (.not. associated(p)) RETURN
	nold=size(p)
	reallocate_hv(1:min(nold,n))=p(1:min(nold,n))
	deallocate(p)
	END FUNCTION reallocate_hv
!BL
	FUNCTION reallocate_rm(p,n,m)
	REAL(sp), DIMENSION(:,:), POINTER :: p, reallocate_rm
	INTEGER(i4), INTENT(IN) :: n,m
	INTEGER(i4) :: nold,mold,ierr
	allocate(reallocate_rm(n,m),stat=ierr)
	if (ierr /= 0) call &
		nrerror('reallocate_rm: problem in attempt to allocate memory')
	if (.not. associated(p)) RETURN
	nold=size(p,1)
	mold=size(p,2)
	reallocate_rm(1:min(nold,n),1:min(mold,m))=&
		p(1:min(nold,n),1:min(mold,m))
	deallocate(p)
	END FUNCTION reallocate_rm
!BL
	FUNCTION reallocate_im(p,n,m)
	INTEGER(i4), DIMENSION(:,:), POINTER :: p, reallocate_im
	INTEGER(i4), INTENT(IN) :: n,m
	INTEGER(i4) :: nold,mold,ierr
	allocate(reallocate_im(n,m),stat=ierr)
	if (ierr /= 0) call &
		nrerror('reallocate_im: problem in attempt to allocate memory')
	if (.not. associated(p)) RETURN
	nold=size(p,1)
	mold=size(p,2)
	reallocate_im(1:min(nold,n),1:min(mold,m))=&
		p(1:min(nold,n),1:min(mold,m))
	deallocate(p)
	END FUNCTION reallocate_im
!BL
	FUNCTION ifirstloc(mask)
         !dir$ optimze:3
         !dir$ attributes forceinline :: ifirstloc
	LOGICAL(i4), DIMENSION(:), INTENT(IN) :: mask
	INTEGER(i4) :: ifirstloc
	INTEGER(i4), DIMENSION(1) :: loc
	loc=maxloc(merge(1,0,mask))
	ifirstloc=loc(1)
	if (.not. mask(ifirstloc)) ifirstloc=size(mask)+1
	END FUNCTION ifirstloc
!BL
	FUNCTION imaxloc_r(arr)
         !dir$ optimze:3
         !dir$ attributes forceinline :: imaxloc_r
	REAL(sp), DIMENSION(:), INTENT(IN) :: arr
	INTEGER(i4) :: imaxloc_r
	INTEGER(i4), DIMENSION(1) :: imax
	imax=maxloc(arr(:))
	imaxloc_r=imax(1)
	END FUNCTION imaxloc_r
!BL
	FUNCTION imaxloc_i(iarr)
          !dir$ optimze:3
         !dir$ attributes forceinline :: imaxloc_i
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: iarr
	INTEGER(i4), DIMENSION(1) :: imax
	INTEGER(i4) :: imaxloc_i
	imax=maxloc(iarr(:))
	imaxloc_i=imax(1)
	END FUNCTION imaxloc_i
!BL
	FUNCTION iminloc(arr)
          !dir$ optimze:3
         !dir$ attributes forceinline :: iminloc
	REAL(sp), DIMENSION(:), INTENT(IN) :: arr
	INTEGER(i4), DIMENSION(1) :: imin
	INTEGER(i4) :: iminloc
	imin=minloc(arr(:))
	iminloc=imin(1)
	END FUNCTION iminloc
!BL
	SUBROUTINE assert1(n1,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	LOGICAL, INTENT(IN) :: n1
	if (.not. n1) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		STOP 'program terminated by assert1'
	end if
	END SUBROUTINE assert1
!BL
	SUBROUTINE assert2(n1,n2,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	LOGICAL, INTENT(IN) :: n1,n2
	if (.not. (n1 .and. n2)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		STOP 'program terminated by assert2'
	end if
	END SUBROUTINE assert2
!BL
	SUBROUTINE assert3(n1,n2,n3,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	LOGICAL, INTENT(IN) :: n1,n2,n3
	if (.not. (n1 .and. n2 .and. n3)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		STOP 'program terminated by assert3'
	end if
	END SUBROUTINE assert3
!BL
	SUBROUTINE assert4(n1,n2,n3,n4,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	LOGICAL, INTENT(IN) :: n1,n2,n3,n4
	if (.not. (n1 .and. n2 .and. n3 .and. n4)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		STOP 'program terminated by assert4'
	end if
	END SUBROUTINE assert4
!BL
	SUBROUTINE assert_v(n,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	LOGICAL, DIMENSION(:), INTENT(IN) :: n
	if (.not. all(n)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		STOP 'program terminated by assert_v'
	end if
	END SUBROUTINE assert_v
!BL
	FUNCTION assert_eq2(n1,n2,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	INTEGER, INTENT(IN) :: n1,n2
	INTEGER :: assert_eq2
	if (n1 == n2) then
		assert_eq2=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		STOP 'program terminated by assert_eq2'
	end if
	END FUNCTION assert_eq2
!BL
	FUNCTION assert_eq3(n1,n2,n3,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	INTEGER, INTENT(IN) :: n1,n2,n3
	INTEGER :: assert_eq3
	if (n1 == n2 .and. n2 == n3) then
		assert_eq3=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		STOP 'program terminated by assert_eq3'
	end if
	END FUNCTION assert_eq3
!BL
	FUNCTION assert_eq4(n1,n2,n3,n4,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	INTEGER, INTENT(IN) :: n1,n2,n3,n4
	INTEGER :: assert_eq4
	if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
		assert_eq4=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		STOP 'program terminated by assert_eq4'
	end if
	END FUNCTION assert_eq4
!BL
	FUNCTION assert_eqn(nn,string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	INTEGER, DIMENSION(:), INTENT(IN) :: nn
	INTEGER :: assert_eqn
	if (all(nn(2:) == nn(1))) then
		assert_eqn=nn(1)
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		STOP 'program terminated by assert_eqn'
	end if
	END FUNCTION assert_eqn
!BL
	SUBROUTINE nrerror(string)
	CHARACTER(LEN=*), INTENT(IN) :: string
	write (*,*) 'nrerror: ',string
	STOP 'program terminated by nrerror'
	END SUBROUTINE nrerror
!BL
#if 0
	FUNCTION arth_r(first,increment,n)
	REAL(sp), INTENT(IN) :: first,increment
	INTEGER(i4), INTENT(IN) :: n
	REAL(sp), DIMENSION(n) :: arth_r
	INTEGER(i4) :: k,k2
	REAL(sp) :: temp
	if (n > 0) arth_r(1)=first
	if (n <= NPAR_ARTH) then
		do k=2,n
			arth_r(k)=arth_r(k-1)+increment
		end do
	else
		do k=2,NPAR2_ARTH
			arth_r(k)=arth_r(k-1)+increment
		end do
		temp=increment*NPAR2_ARTH
		k=NPAR2_ARTH
		do
			if (k >= n) exit
			k2=k+k
			arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	END FUNCTION arth_r
!BL
	FUNCTION arth_d(first,increment,n)
	REAL(dp), INTENT(IN) :: first,increment
	INTEGER(i4), INTENT(IN) :: n
	REAL(dp), DIMENSION(n) :: arth_d
	INTEGER(i4) :: k,k2
	REAL(dp) :: temp
	if (n > 0) arth_d(1)=first
	if (n <= NPAR_ARTH) then
		do k=2,n
			arth_d(k)=arth_d(k-1)+increment
		end do
	else
		do k=2,NPAR2_ARTH
			arth_d(k)=arth_d(k-1)+increment
		end do
		temp=increment*NPAR2_ARTH
		k=NPAR2_ARTH
		do
			if (k >= n) exit
			k2=k+k
			arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	END FUNCTION arth_d
!BL
	FUNCTION arth_i(first,increment,n)
	INTEGER(i4), INTENT(IN) :: first,increment,n
	INTEGER(i4), DIMENSION(n) :: arth_i
	INTEGER(i4) :: k,k2,temp
	if (n > 0) arth_i(1)=first
	if (n <= NPAR_ARTH) then
		do k=2,n
			arth_i(k)=arth_i(k-1)+increment
		end do
	else
		do k=2,NPAR2_ARTH
			arth_i(k)=arth_i(k-1)+increment
		end do
		temp=increment*NPAR2_ARTH
		k=NPAR2_ARTH
		do
			if (k >= n) exit
			k2=k+k
			arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	END FUNCTION arth_i
!BL
!BL
	FUNCTION geop_r(first,factor,n)
	REAL(sp), INTENT(IN) :: first,factor
	INTEGER(i4), INTENT(IN) :: n
	REAL(sp), DIMENSION(n) :: geop_r
	INTEGER(i4) :: k,k2
	REAL(sp) :: temp
	if (n > 0) geop_r(1)=first
	if (n <= NPAR_GEOP) then
		do k=2,n
			geop_r(k)=geop_r(k-1)*factor
		end do
	else
		do k=2,NPAR2_GEOP
			geop_r(k)=geop_r(k-1)*factor
		end do
		temp=factor**NPAR2_GEOP
		k=NPAR2_GEOP
		do
			if (k >= n) exit
			k2=k+k
			geop_r(k+1:min(k2,n))=temp*geop_r(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	END FUNCTION geop_r
!BL
	FUNCTION geop_d(first,factor,n)
	REAL(dp), INTENT(IN) :: first,factor
	INTEGER(i4), INTENT(IN) :: n
	REAL(dp), DIMENSION(n) :: geop_d
	INTEGER(i4) :: k,k2
	REAL(dp) :: temp
	if (n > 0) geop_d(1)=first
	if (n <= NPAR_GEOP) then
		do k=2,n
			geop_d(k)=geop_d(k-1)*factor
		end do
	else
		do k=2,NPAR2_GEOP
			geop_d(k)=geop_d(k-1)*factor
		end do
		temp=factor**NPAR2_GEOP
		k=NPAR2_GEOP
		do
			if (k >= n) exit
			k2=k+k
			geop_d(k+1:min(k2,n))=temp*geop_d(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	END FUNCTION geop_d
!BL
	FUNCTION geop_i(first,factor,n)
	INTEGER(i4), INTENT(IN) :: first,factor,n
	INTEGER(i4), DIMENSION(n) :: geop_i
	INTEGER(i4) :: k,k2,temp
	if (n > 0) geop_i(1)=first
	if (n <= NPAR_GEOP) then
		do k=2,n
			geop_i(k)=geop_i(k-1)*factor
		end do
	else
		do k=2,NPAR2_GEOP
			geop_i(k)=geop_i(k-1)*factor
		end do
		temp=factor**NPAR2_GEOP
		k=NPAR2_GEOP
		do
			if (k >= n) exit
			k2=k+k
			geop_i(k+1:min(k2,n))=temp*geop_i(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	END FUNCTION geop_i
!BL
	FUNCTION geop_c(first,factor,n)
	COMPLEX(sp), INTENT(IN) :: first,factor
	INTEGER(i4), INTENT(IN) :: n
	COMPLEX(sp), DIMENSION(n) :: geop_c
	INTEGER(i4) :: k,k2
	COMPLEX(sp) :: temp
	if (n > 0) geop_c(1)=first
	if (n <= NPAR_GEOP) then
		do k=2,n
			geop_c(k)=geop_c(k-1)*factor
		end do
	else
		do k=2,NPAR2_GEOP
			geop_c(k)=geop_c(k-1)*factor
		end do
		temp=factor**NPAR2_GEOP
		k=NPAR2_GEOP
		do
			if (k >= n) exit
			k2=k+k
			geop_c(k+1:min(k2,n))=temp*geop_c(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	END FUNCTION geop_c
!BL
	FUNCTION geop_dv(first,factor,n)
	REAL(dp), DIMENSION(:), INTENT(IN) :: first,factor
	INTEGER(i4), INTENT(IN) :: n
	REAL(dp), DIMENSION(size(first),n) :: geop_dv
	INTEGER(i4) :: k,k2
	REAL(dp), DIMENSION(size(first)) :: temp
	if (n > 0) geop_dv(:,1)=first(:)
	if (n <= NPAR_GEOP) then
		do k=2,n
			geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
		end do
	else
		do k=2,NPAR2_GEOP
			geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
		end do
		temp=factor**NPAR2_GEOP
		k=NPAR2_GEOP
		do
			if (k >= n) exit
			k2=k+k
			geop_dv(:,k+1:min(k2,n))=geop_dv(:,1:min(k,n-k))*&
				spread(temp,2,size(geop_dv(:,1:min(k,n-k)),2))
			temp=temp*temp
			k=k2
		end do
	end if
	END FUNCTION geop_dv
!BL
!BL
#endif
	RECURSIVE FUNCTION cumsum_r(arr,seed) RESULT(ans)
	REAL(sp), DIMENSION(:), INTENT(IN) :: arr
	REAL(sp), OPTIONAL, INTENT(IN) :: seed
	REAL(sp), DIMENSION(size(arr)) :: ans
	INTEGER(i4) :: n,j
	REAL(sp) :: sd
	n=size(arr)
	if (n == 0_i4b) RETURN
	sd=0.0_sp
	if (present(seed)) sd=seed
	ans(1)=arr(1)+sd
	if (n < NPAR_CUMSUM) then
		do j=2,n
			ans(j)=ans(j-1)+arr(j)
		end do
	else
		ans(2:n:2)=cumsum_r(arr(2:n:2)+arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
	end if
	END FUNCTION cumsum_r
!BL
	RECURSIVE FUNCTION cumsum_i(arr,seed) RESULT(ans)
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: arr
	INTEGER(i4), OPTIONAL, INTENT(IN) :: seed
	INTEGER(i4), DIMENSION(size(arr)) :: ans
	INTEGER(i4) :: n,j,sd
	n=size(arr)
	if (n == 0_i4b) RETURN
	sd=0_i4b
	if (present(seed)) sd=seed
	ans(1)=arr(1)+sd
	if (n < NPAR_CUMSUM) then
		do j=2,n
			ans(j)=ans(j-1)+arr(j)
		end do
	else
		ans(2:n:2)=cumsum_i(arr(2:n:2)+arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
	end if
	END FUNCTION cumsum_i
!BL
!BL
	RECURSIVE FUNCTION cumprod(arr,seed) RESULT(ans)
	REAL(sp), DIMENSION(:), INTENT(IN) :: arr
	REAL(sp), OPTIONAL, INTENT(IN) :: seed
	REAL(sp), DIMENSION(size(arr)) :: ans
	INTEGER(i4) :: n,j
	REAL(sp) :: sd
	n=size(arr)
	if (n == 0_i4b) RETURN
	sd=1.0_sp
	if (present(seed)) sd=seed
	ans(1)=arr(1)*sd
	if (n < NPAR_CUMPROD) then
		do j=2,n
			ans(j)=ans(j-1)*arr(j)
		end do
	else
		ans(2:n:2)=cumprod(arr(2:n:2)*arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)*arr(3:n:2)
	end if
	END FUNCTION cumprod
!BL
!BL
#if 0
	FUNCTION poly_rr(x,coeffs)
	REAL(sp), INTENT(IN) :: x
	REAL(sp), DIMENSION(:), INTENT(IN) :: coeffs
	REAL(sp) :: poly_rr
	REAL(sp) :: pow
	REAL(sp), DIMENSION(:), ALLOCATABLE :: vec
	INTEGER(i4) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_rr=0.0_sp
	else if (n < NPAR_POLY) then
		poly_rr=coeffs(n)
		do i=n-1,1,-1
			poly_rr=x*poly_rr+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_rr=vec(1)
		deallocate(vec)
	end if
	END FUNCTION poly_rr
!BL
	FUNCTION poly_dd(x,coeffs)
	REAL(dp), INTENT(IN) :: x
	REAL(dp), DIMENSION(:), INTENT(IN) :: coeffs
	REAL(dp) :: poly_dd
	REAL(dp) :: pow
	REAL(dp), DIMENSION(:), ALLOCATABLE :: vec
	INTEGER(i4) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_dd=0.0_dp
	else if (n < NPAR_POLY) then
		poly_dd=coeffs(n)
		do i=n-1,1,-1
			poly_dd=x*poly_dd+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_dp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_dd=vec(1)
		deallocate(vec)
	end if
	END FUNCTION poly_dd
!BL
	FUNCTION poly_rc(x,coeffs)
	COMPLEX(spC), INTENT(IN) :: x
	REAL(sp), DIMENSION(:), INTENT(IN) :: coeffs
	COMPLEX(spC) :: poly_rc
	COMPLEX(spC) :: pow
	COMPLEX(spC), DIMENSION(:), ALLOCATABLE :: vec
	INTEGER(i4) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_rc=0.0_sp
	else if (n < NPAR_POLY) then
		poly_rc=coeffs(n)
		do i=n-1,1,-1
			poly_rc=x*poly_rc+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_rc=vec(1)
		deallocate(vec)
	end if
	END FUNCTION poly_rc
!BL
	FUNCTION poly_cc(x,coeffs)
	COMPLEX(spC), INTENT(IN) :: x
	COMPLEX(spC), DIMENSION(:), INTENT(IN) :: coeffs
	COMPLEX(spC) :: poly_cc
	COMPLEX(spC) :: pow
	COMPLEX(spC), DIMENSION(:), ALLOCATABLE :: vec
	INTEGER(i4) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_cc=0.0_sp
	else if (n < NPAR_POLY) then
		poly_cc=coeffs(n)
		do i=n-1,1,-1
			poly_cc=x*poly_cc+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_cc=vec(1)
		deallocate(vec)
	end if
	END FUNCTION poly_cc
!BL
	FUNCTION poly_rrv(x,coeffs)
	REAL(sp), DIMENSION(:), INTENT(IN) :: coeffs,x
	REAL(sp), DIMENSION(size(x)) :: poly_rrv
	INTEGER(i4) :: i,n,m
	m=size(coeffs)
	n=size(x)
	if (m <= 0) then
		poly_rrv=0.0_sp
	else if (m < n .or. m < NPAR_POLY) then
		poly_rrv=coeffs(m)
		do i=m-1,1,-1
			poly_rrv=x*poly_rrv+coeffs(i)
		end do
	else
		do i=1,n
			poly_rrv(i)=poly_rr(x(i),coeffs)
		end do
	end if
	END FUNCTION poly_rrv
!BL
	FUNCTION poly_ddv(x,coeffs)
	REAL(dp), DIMENSION(:), INTENT(IN) :: coeffs,x
	REAL(dp), DIMENSION(size(x)) :: poly_ddv
	INTEGER(i4) :: i,n,m
	m=size(coeffs)
	n=size(x)
	if (m <= 0) then
		poly_ddv=0.0_dp
	else if (m < n .or. m < NPAR_POLY) then
		poly_ddv=coeffs(m)
		do i=m-1,1,-1
			poly_ddv=x*poly_ddv+coeffs(i)
		end do
	else
		do i=1,n
			poly_ddv(i)=poly_dd(x(i),coeffs)
		end do
	end if
	END FUNCTION poly_ddv
!BL
	FUNCTION poly_msk_rrv(x,coeffs,mask)
	REAL(sp), DIMENSION(:), INTENT(IN) :: coeffs,x
	LOGICAL(i4), DIMENSION(:), INTENT(IN) :: mask
	REAL(sp), DIMENSION(size(x)) :: poly_msk_rrv
	poly_msk_rrv=unpack(poly_rrv(pack(x,mask),coeffs),mask,0.0_sp)
	END FUNCTION poly_msk_rrv
!BL
	FUNCTION poly_msk_ddv(x,coeffs,mask)
	REAL(dp), DIMENSION(:), INTENT(IN) :: coeffs,x
	LOGICAL(i4), DIMENSION(:), INTENT(IN) :: mask
	REAL(dp), DIMENSION(size(x)) :: poly_msk_ddv
	poly_msk_ddv=unpack(poly_ddv(pack(x,mask),coeffs),mask,0.0_dp)
	END FUNCTION poly_msk_ddv
!BL
!BL
	RECURSIVE FUNCTION poly_term_rr(a,b) RESULT(u)
	REAL(sp), DIMENSION(:), INTENT(IN) :: a
	REAL(sp), INTENT(IN) :: b
	REAL(sp), DIMENSION(size(a)) :: u
	INTEGER(i4) :: n,j
	n=size(a)
	if (n <= 0) RETURN
	u(1)=a(1)
	if (n < NPAR_POLYTERM) then
		do j=2,n
			u(j)=a(j)+b*u(j-1)
		end do
	else
		u(2:n:2)=poly_term_rr(a(2:n:2)+a(1:n-1:2)*b,b*b)
		u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
	end if
	END FUNCTION poly_term_rr
!BL
	RECURSIVE FUNCTION poly_term_cc(a,b) RESULT(u)
	COMPLEX(spC), DIMENSION(:), INTENT(IN) :: a
	COMPLEX(spC), INTENT(IN) :: b
	COMPLEX(spC), DIMENSION(size(a)) :: u
	INTEGER(i4) :: n,j
	n=size(a)
	if (n <= 0) RETURN
	u(1)=a(1)
	if (n < NPAR_POLYTERM) then
		do j=2,n
			u(j)=a(j)+b*u(j-1)
		end do
	else
		u(2:n:2)=poly_term_cc(a(2:n:2)+a(1:n-1:2)*b,b*b)
		u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
	end if
	END FUNCTION poly_term_cc
!BL
!BL
#endif
	FUNCTION zroots_unity(n,nn)
	INTEGER(i4), INTENT(IN) :: n,nn
	COMPLEX(sp), DIMENSION(nn) :: zroots_unity
	INTEGER(i4) :: k
	REAL(sp) :: theta
	zroots_unity(1)=1.0
	theta=TWOPI/n
	k=1
	do
		if (k >= nn) exit
		zroots_unity(k+1)=cmplx(cos(k*theta),sin(k*theta),spC)
		zroots_unity(k+2:min(2*k,nn))=zroots_unity(k+1)*&
			zroots_unity(2:min(k,nn-k))
		k=2*k
	end do
	END FUNCTION zroots_unity
!BL
#if 0
	FUNCTION outerprod_r(a,b)
	REAL(sp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(sp), DIMENSION(size(a),size(b)) :: outerprod_r
	outerprod_r = spread(a,dim=2,ncopies=size(b)) * &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerprod_r
!BL
	FUNCTION outerprod_d(a,b)
	REAL(dp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(dp), DIMENSION(size(a),size(b)) :: outerprod_d
	outerprod_d = spread(a,dim=2,ncopies=size(b)) * &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerprod_d
!BL
	FUNCTION outerdiv(a,b)
	REAL(sp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(sp), DIMENSION(size(a),size(b)) :: outerdiv
	outerdiv = spread(a,dim=2,ncopies=size(b)) / &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerdiv
!BL
	FUNCTION outersum(a,b)
	REAL(sp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(sp), DIMENSION(size(a),size(b)) :: outersum
	outersum = spread(a,dim=2,ncopies=size(b)) + &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outersum
!BL
	FUNCTION outerdiff_r(a,b)
	REAL(sp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(sp), DIMENSION(size(a),size(b)) :: outerdiff_r
	outerdiff_r = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerdiff_r
!BL
	FUNCTION outerdiff_d(a,b)
	REAL(dp), DIMENSION(:), INTENT(IN) :: a,b
	REAL(dp), DIMENSION(size(a),size(b)) :: outerdiff_d
	outerdiff_d = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerdiff_d
!BL
	FUNCTION outerdiff_i(a,b)
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: a,b
	INTEGER(i4), DIMENSION(size(a),size(b)) :: outerdiff_i
	outerdiff_i = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerdiff_i
!BL
	FUNCTION outerand(a,b)
	LOGICAL(i4), DIMENSION(:), INTENT(IN) :: a,b
	LOGICAL(i4), DIMENSION(size(a),size(b)) :: outerand
	outerand = spread(a,dim=2,ncopies=size(b)) .and. &
		spread(b,dim=1,ncopies=size(a))
	END FUNCTION outerand
!BL
	SUBROUTINE scatter_add_r(dest,source,dest_index)
	REAL(sp), DIMENSION(:), INTENT(OUT) :: dest
	REAL(sp), DIMENSION(:), INTENT(IN) :: source
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: dest_index
	INTEGER(i4) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_add_r')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
	end do
	END SUBROUTINE scatter_add_r
	SUBROUTINE scatter_add_d(dest,source,dest_index)
	REAL(dp), DIMENSION(:), INTENT(OUT) :: dest
	REAL(dp), DIMENSION(:), INTENT(IN) :: source
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: dest_index
	INTEGER(i4) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_add_d')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
	end do
	END SUBROUTINE scatter_add_d
	SUBROUTINE scatter_max_r(dest,source,dest_index)
	REAL(sp), DIMENSION(:), INTENT(OUT) :: dest
	REAL(sp), DIMENSION(:), INTENT(IN) :: source
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: dest_index
	INTEGER(i4) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_max_r')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=max(dest(i),source(j))
	end do
	END SUBROUTINE scatter_max_r
	SUBROUTINE scatter_max_d(dest,source,dest_index)
	REAL(dp), DIMENSION(:), INTENT(OUT) :: dest
	REAL(dp), DIMENSION(:), INTENT(IN) :: source
	INTEGER(i4), DIMENSION(:), INTENT(IN) :: dest_index
	INTEGER(i4) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_max_d')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=max(dest(i),source(j))
	end do
	END SUBROUTINE scatter_max_d
!BL
#endif
#if 0
	SUBROUTINE diagadd_rv(mat,diag)

	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	REAL(sp), DIMENSION(:), INTENT(IN) :: diag
	INTEGER(i4) :: j,n
	n = assert_eq2(size(diag),min(size(mat,1),size(mat,2)),'diagadd_rv')
	do j=1,n
		mat(j,j)=mat(j,j)+diag(j)
	end do
	END SUBROUTINE diagadd_rv
!BL
	SUBROUTINE diagadd_r(mat,diag)
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	REAL(sp), INTENT(IN) :: diag
	INTEGER(i4) :: j,n
	n = min(size(mat,1),size(mat,2))
	do j=1,n
		mat(j,j)=mat(j,j)+diag
	end do
	END SUBROUTINE diagadd_r
!BL
	SUBROUTINE diagmult_rv(mat,diag)
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	REAL(sp), DIMENSION(:), INTENT(IN) :: diag
	INTEGER(i4) :: j,n
	n = assert_eq2(size(diag),min(size(mat,1),size(mat,2)),'diagmult_rv')
	do j=1,n
		mat(j,j)=mat(j,j)*diag(j)
	end do
	END SUBROUTINE diagmult_rv
!BL
	SUBROUTINE diagmult_r(mat,diag)
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	REAL(sp), INTENT(IN) :: diag
	INTEGER(i4) :: j,n
	n = min(size(mat,1),size(mat,2))
	do j=1,n
		mat(j,j)=mat(j,j)*diag
	end do
	END SUBROUTINE diagmult_r
!BL
	FUNCTION get_diag_rv(mat)
	REAL(sp), DIMENSION(:,:), INTENT(IN) :: mat
	REAL(sp), DIMENSION(size(mat,1)) :: get_diag_rv
	INTEGER(i4) :: j
	j=assert_eq2(size(mat,1),size(mat,2),'get_diag_rv')
	do j=1,size(mat,1)
		get_diag_rv(j)=mat(j,j)
	end do
	END FUNCTION get_diag_rv
!BL
	FUNCTION get_diag_dv(mat)
	REAL(dp), DIMENSION(:,:), INTENT(IN) :: mat
	REAL(dp), DIMENSION(size(mat,1)) :: get_diag_dv
	INTEGER(i4) :: j
	j=assert_eq2(size(mat,1),size(mat,2),'get_diag_dv')
	do j=1,size(mat,1)
		get_diag_dv(j)=mat(j,j)
	end do
	END FUNCTION get_diag_dv
!BL
#endif
	SUBROUTINE put_diag_rv(diagv,mat)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: put_diag_rv
          !dir$ attributes code_align : 32 :: put_diag_rv
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: put_diag_rv 
	REAL(sp), DIMENSION(:), INTENT(IN) :: diagv
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	INTEGER(i4) :: j,n
	n=assert_eq2(size(diagv),min(size(mat,1),size(mat,2)),'put_diag_rv')
        !dir$ assume_aligned mat:64
        !dir$ vector aligned
	do j=1,n
		mat(j,j)=diagv(j)
	end do
	END SUBROUTINE put_diag_rv
!BL
	SUBROUTINE put_diag_r(scal,mat)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: put_diag_r
          !dir$ attributes code_align : 32 :: put_diag_r
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: put_diag_r 
	REAL(sp), INTENT(IN) :: scal
	REAL(sp), DIMENSION(:,:), INTENT(INOUT) :: mat
	INTEGER(i4) :: j,n
	n = min(size(mat,1),size(mat,2))
        !dir$ assume_aligned mat:64
        !dir$ vector aligned
	do j=1,n
		mat(j,j)=scal
	end do
	END SUBROUTINE put_diag_r
!BL
	SUBROUTINE unit_matrix(mat)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: unit_matrix
          !dir$ attributes code_align : 32 :: unit_matrix
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: unit_matrix
	REAL(sp), DIMENSION(:,:), INTENT(OUT) :: mat
	INTEGER(i4) :: i,n
	n=min(size(mat,1),size(mat,2))
	mat(:,:)=0.0_sp
        !dir$ assume_aligned mat:64
        !dir$ vector aligned
	do i=1,n
		mat(i,i)=1.0_sp
	end do
	END SUBROUTINE unit_matrix
!BL
	FUNCTION upper_triangle(j,k,extra)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: upper_triangle
          !dir$ attributes code_align : 32 :: upper_triangle
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: upper_triangle
	INTEGER(i4), INTENT(IN) :: j,k
	INTEGER(i4), OPTIONAL, INTENT(IN) :: extra
	LOGICAL(i4), DIMENSION(j,k) :: upper_triangle
	INTEGER(i4) :: n
	n=0
	if (present(extra)) n=extra
	upper_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) < n)
	END FUNCTION upper_triangle
!BL
	FUNCTION lower_triangle(j,k,extra)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: lower_triangle
          !dir$ attributes code_align : 32 :: lower_triangle
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: lower_triangle
	INTEGER(i4), INTENT(IN) :: j,k
	INTEGER(i4), OPTIONAL, INTENT(IN) :: extra
	LOGICAL(i4), DIMENSION(j,k) :: lower_triangle
	INTEGER(i4) :: n
	n=0
	if (present(extra)) n=extra
	lower_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) > -n)
	END FUNCTION lower_triangle
!BL
	FUNCTION vabs(v)
          !dir$ optimize:3 
          !dir$ attributes forceinline :: vabs
          !dir$ attributes code_align : 32 :: vabs
          !dir$ attributes optimization_parameter: 'target_arch=skylake-avx512' :: vabs
	REAL(sp), DIMENSION(:), INTENT(IN) :: v
	REAL(sp) :: vabs
	vabs=sqrt(dot_product(v,v))
	END FUNCTION vabs
!BL
END MODULE nrutil
