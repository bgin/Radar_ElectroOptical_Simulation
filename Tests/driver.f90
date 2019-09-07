

program TMatrix_MPS_Test
     use mod_kinds, only : int4, dp
     use mod_tmatrix_mps, only : tmatrix_mps_driver,NANGMAX
     use IFPORT, only : dclock
     implicit none

     ! Arguments
     integer(kind=int4), parameter :: nLp = 15
     integer(kind=int4), parameter :: nL  = 10
     integer(kind=int4), parameter :: np  = 20
     integer(kind=int4), parameter :: MXINT = 200
     integer(kind=int4), parameter :: NADD  = 0
     integer(kind=int4), parameter :: idscmt = 1
     integer(kind=int4), parameter :: irat   = 1
     integer(kind=int4), parameter :: Mie    = 0
     real(kind=dp),      parameter :: small = 0.000001_dp
     real(kind=dp),      parameter :: sang  = 1.0_dp
     real(kind=dp),      parameter :: w     = 0.029979246_dp
     !real(kind=dp), dimension(9)     :: tmp
     real(kind=dp), dimension(3,nLp) :: shp
     real(kind=dp), dimension(9,nLp) :: r0
     integer(kind=int4), dimension(nLp) :: idshp 
     ! results
     real(kind=dp),   dimension(4,4,NANGMAX) :: mue
     !DIR$ ATTRIBUTES ALIGN : 64 :: mue
     real(kind=dp),   dimension(NANGMAX)     :: dang,inat,pol,i11,i21,i12,i22
     !DIR$ ATTRIBUTES ALIGN : 64 :: dang
     !DIR$ ATTRIBUTES ALIGN : 64 :: inat
     !DIR$ ATTRIBUTES ALIGN : 64 :: pol
     !DIR$ ATTRIBUTES ALIGN : 64 :: i11
     !DIR$ ATTRIBUTES ALIGN : 64 :: i21
     !DIR$ ATTRIBUTES ALIGN : 64 :: i12
     !DIR$ ATTRIBUTES ALIGN : 64 :: i22
     real(kind=dp),   dimension(nLp)         :: cexti,cabsi,cscai,assymi,cpri
     real(kind=dp) :: cext,cabs,csca,assym,cabsv,   &
                      cscav,cbakv,cprv,cexts,cabss, &
                      cscas,cbaks,cprs
     real(kind=dp) :: start,end,duration
     ! Initialization
     !
     idshp   = 0
     r0(1,:) = 0.5_dp
     r0(2,:) = 0.0_dp
     r0(3,:) = 0.0_dp
     r0(4,:) = 0.0_dp
     r0(5,:) = 0.0_dp
     r0(6,:) = 0.0_dp
     r0(7,:) = 3.0_dp
     r0(8,:) = 1.6_dp
     r0(9,:) = 0.02_dp
     shp    = 0.0_dp
     mue    = 0.000000001_dp
     dang   = 0.000000001_dp
     inat   = 0.000000001_dp
     pol    = 0.000000001_dp
     i11    = 0.000000001_dp
     i21    = 0.000000001_dp
     i12    = 0.000000001_dp
     i22    = 0.000000001_dp
     cexti  = 0.000000001_dp
     cabsi  = 0.000000001_dp
     cscai  = 0.000000001_dp
     assymi = 0.000000001_dp
     cpri   = 0.000000001_dp
     cext   = 0.000000001_dp
     cabs   = 0.000000001_dp
     csca   = 0.000000001_dp
     assym  = 0.000000001_dp
     cabsv  = 0.000000001_dp
     cscav  = 0.000000001_dp
     cbakv  = 0.000000001_dp
     cprv   = 0.000000001_dp
     cexts  = 0.000000001_dp
     cabss  = 0.000000001_dp
     cscas  = 0.000000001_dp
     cbaks  = 0.000000001_dp
     cprs   = 0.000000001_dp
     start  = 0.0_dp
     end    = 0.0_dp
     duration = 0.0_dp
     ! Crude time measurement
     start = dclock()
     call tmatrix_mps_driver(nLp,np,Mie,small,MXINT,NADD,idscmt,sang,w,irat,  &
                             nL,idshp,shp,r0,cext,cabs,csca,assym,cextv,cabsv, &
                             cscav,cbakv,cprv,cexts,cabss,cscas,cbaks,cprs,    &
                             dang,inat,pol,i11,i21,i12,i22,cexti,cabsi,cscai,  &
                             assymi,cpri,mue)
     end = dclock()
     duration = end - start
     print*, "tmatrix_mps_driver -- total execution time: ", duration, " microseconds."

     
end program TMatrix_MPS_Test
