


         ! Dynamic derived type for EMF modeling (complex-single) AoS-layout
          ! DC3D_aos_cmplx_t
           ! First dimension nth field, second dimension number of sample points
          ! Array holding a number of points for various antenna characteristics computation.
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        integer(kind=i4), parameter :: SC3D_caos_ne = 1     ! number of  EM fields (for phased array radiating element)
        integer(kind=i4), parameter :: SC3D_caos_nx = 1!x-dim
        integer(kind=i4), parameter :: SC3D_caos_ny = 1 !y-dim
        integer(kind=i4), parameter :: SC3D_caos_nz = 1 !z-dim
#else         
        integer(kind=i4), parameter :: SC3D_caos_ne = 1     ! number of  EM fields (for phased array radiating element)
        integer(kind=i4), parameter :: SC3D_caos_nx = 1!x-dim
        integer(kind=i4), parameter :: SC3D_caos_ny = 1 !y-dim
        integer(kind=i4), parameter :: SC3D_caos_nz = 1 !z-dim
#endif   

       ! Dynamic derived type for EMF modeling (complex-single) flat-layout
       ! DC3D_flat_cmplx_t
       ! First dimension nth field, second dimension number of sample points
       ! Array holding a number of points for various antenna characteristics computation. 
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        integer(kind=i4), parameter :: SC3D_cflat_ne = 1     ! number of  EM fields (for phased array radiating element)
        integer(kind=i4), parameter :: SC3D_cflat_nx = 1!x-dim
        integer(kind=i4), parameter :: SC3D_cflat_ny = 1 !y-dim
        integer(kind=i4), parameter :: SC3D_cflat_nz = 1 !z-dim
#else         
        integer(kind=i4), parameter :: SC3D_cflat_ne = 1     ! number of  EM fields (for phased array radiating element)
        integer(kind=i4), parameter :: SC3D_cflat_nx = 1!x-dim
        integer(kind=i4), parameter :: SC3D_cflat_ny = 1 !y-dim
        integer(kind=i4), parameter :: SC3D_cflat_nz = 1 !z-dim
#endif  

 
       ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) AoS-layout
       ! DC2D_aos_cmplx_t
       ! First dimension nth field, second dimension number of sample points    
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1        
       integer(kind=i4), parameter :: SC2D_caos_ne   = 1         ! number of  EM fields (for phased array radiating element)
       integer(kind=i4), parameter :: SC2D_caos_nth  = 1!th-dim
       integer(kind=i4), parameter :: SC2D_caos_nph  = 1!ph-dim
#else
       integer(kind=i4), parameter :: SC2D_caos_ne   = 1       ! number of  EM fields (for phased array radiating element)
       integer(kind=i4), parameter :: SC2D_caos_nth  = 1!th-dim
       integer(kind=i4), parameter :: SC2D_caos_nph  = 1!ph-dim
#endif  

       ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) flat-layout
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1        
       integer(kind=i4), parameter :: SC2D_cflat_ne   = 1         ! number of  EM fields (for phased array radiating element)
       integer(kind=i4), parameter :: SC2D_cflat_nth  = 1!th-dim
       integer(kind=i4), parameter :: SC2D_flat_nph   = 1!ph-dim
#else
       integer(kind=i4), parameter :: SC2D_cflat_ne   = 1       ! number of  EM fields (for phased array radiating element)
       integer(kind=i4), parameter :: SC2D_cflat_nth  = 1!th-dim
       integer(kind=i4), parameter :: SC2D_cflat_nph  = 1!ph-dim
#endif     

        ! Normalized function Psi (power of radiated field)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1 
       integer(kind=i4), parameter :: nth = 1  !theta-dim
       integer(kind=i4), parameter :: nph = 1 !phi-dim
#else       
       integer(kind=i4), parameter :: nth = 1  !theta-dim
       integer(kind=i4), parameter :: nph = 1 !phi-dim
#endif

        ! Elementary electric dipoles (radiation patterns)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1    
       integer(kind=i4), parameter :: nft240 = 1  ! number of values of elementary electric dipole (2.40)
       integer(kind=i4), parameter :: ndf240 = 1 ! number of elementary dipoles (2.40)
#else
       integer(kind=i4), parameter :: nft240 = 1  ! number of values of elementary electric dipole (2.40)
       integer(kind=i4), parameter :: ndf240 = 1 ! number of elementary dipoles (2.40)
#endif      


        ! Sinusoidal current distribution (2.43)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1    
      integer(kind=i4),  parameter :: nzf243  = 1 ! number of current distribution 'z' values (2.43)
#else
      integer(kind=i4),  parameter :: nzf243  = 1 ! number of current distribution 'z' values (2.43)
#endif      
      
      
       ! Radiation pattern of similiar EM radiators (2.96)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1       
       integer(kind=i4), parameter :: ntf296  = 1  ! number of theta values (2.96)
       integer(kind=i4), parameter :: npf296  = 1! number of phi values (2.96)
#else
       integer(kind=i4), parameter :: ntf296  = 1  ! number of theta values (2.96)
       integer(kind=i4), parameter :: npf296  = 1! number of phi values (2.96)
#endif      
      
      
       !Linear phase error values apperture edge (2.98)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1    
       integer(kind=i4), parameter :: npf298  = 1  ! number of linear phase error values apperture edge (2.98)
#else
       integer(kind=i4), parameter :: npf298  = 1 
#endif      
      
      
      
