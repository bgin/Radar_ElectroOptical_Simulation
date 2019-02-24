        MODULE SWAN_COMMON
      use mod_kinds, only : int4, sp, dp
      USE TIMECOMM                                                        !40.41
      USE OCPCOMM2                                                       ! 40.41
      USE OCPCOMM4                                                       ! 40.41
      USE SWCOMM1                                                         !40.41
      USE SWCOMM2                                                        ! 40.41
      USE SWCOMM3                                                        ! 40.41
      USE SWCOMM4                                                        ! 40.41
      USE OUTP_DATA                                                      ! 40.51
      USE M_GENARR                                                       ! 40.31
      USE M_PARALL                                                       ! 40.31
      USE PASS                                                            !fyshi
!
      IMPLICIT NONE

      INTEGER(int4) ::    IUNIT                                                     !34.01
      INTEGER(int4) ::    IOSTAT, IT0, IT, SAVITE, ILEN                             !40.30
      INTEGER(int4) ::    INERR                                                     !40.31
      INTEGER(int4) ::    IERR                                                     ! 40.31 40.00
      INTEGER(int4) ::    ISTAT, IF1, IL1                                          ! 40.41
!      CHARACTER PTYPE, PNAME *8, COMPUT *4, DTTIWR*18                     40.00
      CHARACTER*20 NUMSTR, CHARS(1)                                       !40.41
      CHARACTER*80 MSGSTR                                                 !40.41
      LOGICAL   LOPEN                                                     !34.01

      INTEGER(int4), ALLOCATABLE :: CROSS(:,:)                                 ! 40.31
      INTEGER(int4), ALLOCATABLE :: BGRIDP(:)                                  ! 40.31
      REAL(sp)   , ALLOCATABLE :: BSPECS(:,:,:,:)                            ! 40.31
      REAL(sp)   , ALLOCATABLE :: AC1(:,:,:), COMPDA(:,:)                    ! 40.31
!
      REAL(sp), ALLOCATABLE    :: BLKND(:), BLKNDC(:), OURQT(:)               !40.51 40.41
      LOGICAL STPNOW    

        END MODULE SWAN_COMMON
 
