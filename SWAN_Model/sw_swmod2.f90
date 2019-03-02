!                 ALLOCATABLE DATA RELATED MODULES, file 2 of 3
!
!     Contents of this file
!
!     M_WCAP             information for whitecapping
!     OUTP_DATA          information for output data
!     M_SNL4             information for quadruplets
!     M_BNDSPEC          information for boundary conditions
!     M_OBSTA            information for obstacles
!     M_GENARR           contains a number of general arrays
!     M_PARALL           information for parallelisation with MPI
!     M_DIFFR            information for diffraction
!
      MODULE M_WCAP
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.02: IJsbrand Haagsma
!
!  1. Updates
!
!     Sep. 00: New Module
!
!  2. Purpose
!
!     Create global variables used in whitecapping and integral parameter
!     subroutines
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     ACTOT  : Total action density per gridpoint
!     EDRKTOT: Zeroth moment of energy / SQRT(wavenumber)
!     EKTOT  : Zeroth moment of energy * wavenumber
!     ETOT1  : First moment of the energy density
!     ETOT2  : Second moment of the energy density
!     ETOT4  : Fourth moment of the energy density
!     KM_WAM : Mean average wavenumber according to the WAM-formulation
!     KM01   : Mean average wavenumber according to first order moment
!     SIGM_10: Mean frequency according to zeroth order moment
!     SIGM01 : Mean frequency according to first order moment
!
      REAL(sp), SAVE    :: ACTOT
      REAL(sp), SAVE    :: EDRKTOT
      REAL(sp), SAVE    :: EKTOT
      REAL(sp), SAVE    :: ETOT1
      REAL(sp), SAVE    :: ETOT2
      REAL(sp), SAVE    :: ETOT4
      REAL(sp), SAVE    :: KM_WAM
      REAL(sp), SAVE    :: KM01
      REAL(sp), SAVE    :: SIGM_10
      REAL(sp), SAVE    :: SIGM01
!
!$OMP THREADPRIVATE(ACTOT, EDRKTOT, EKTOT, ETOT1, ETOT2, ETOT4,KM_WAM, KM01, SIGM_10, SIGM01)
           
!
!     SIGPOW : contains powers of relative frequencies
!              second dimension indicates power of sigma
!
!DIR$ ATTRIBUTES ALIGN : 64 :: SIGPOW
      REAL(sp), SAVE, ALLOCATABLE :: SIGPOW(:,:)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SSFILL :
!     SINTGRL: Calculating integral paramters
!     WCAP   : Calculating whitecapping source term
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_WCAP

      MODULE OUTP_DATA
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.13: Nico Booij
!     40.31: Marcel Zijlema
!
!  1. Updates
!
!     40.13, July 01: New Module
!     40.13, Oct. 01: Longer filenames for output requests
!     40.31, Dec. 03: derive types OPSDAT, ORQDAT added
!
!  2. Purpose
!
!     Contains data needed during generation of output
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
      USE OCPCOMM2
      use mod_kinds, only : int4, sp
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables

      INTEGER(int4), PARAMETER :: MAX_OUTP_REQ = 250 ! max. number of output requests

      CHARACTER (LEN=1)  :: OUT_COMMENT = '%' ! comment sign for heading lines

      ! formats for output:
      CHARACTER (LEN=40) :: FLT_BLOCK = '(6E12.4)'       ! floating point block
      CHARACTER (LEN=40) :: FLT_TABLE = '(E11.4)'        ! floating point table
      CHARACTER (LEN=40) :: FIX_SPEC  = '(200(1X,I4))'   ! spectral output

!     format for block output per process in case of collecting data
      CHARACTER (LEN=40) :: FLT_BLKP = '(6E17.9)'

      INTEGER(int4) :: FLD_TABLE = 12       ! field length for fixed-point table
      INTEGER(int4) :: DEC_BLOCK =  4       ! number of decimals for fixed-point block
      INTEGER(int4) :: DEC_SPEC  =  4       ! number of decimals for spectral output

!     longer filenames for output requests
      CHARACTER (LEN=LENFNM) :: OUTP_FILES(1:MAX_OUTP_REQ)
      ! filenames for output; index is output request sequence number

      INTEGER, SAVE :: NREOQ = 0         ! actual number of requests saved

      TYPE OPSDAT
         CHARACTER (LEN=1)      :: PSTYPE                     ! type (F, C, P, ...)
         CHARACTER (LEN=8)      :: PSNAME                     ! name of point set
         INTEGER(int4)          :: OPI(2)                     ! integer coefficients
         REAL(sp)               :: OPR(5)                     ! real coefficients
         INTEGER(int4)          :: MIP                        ! number of points
         REAL(sp), POINTER      :: XP(:), YP(:), XQ(:), YQ(:) ! point coordinates
         TYPE(OPSDAT), POINTER :: NEXTOPS
      END TYPE OPSDAT

      TYPE(OPSDAT), SAVE, TARGET  :: FOPS
      TYPE(OPSDAT), SAVE, POINTER :: COPS
      LOGICAL, SAVE :: LOPS = .FALSE.

      TYPE ORQDAT
         CHARACTER (LEN=4)            :: RQTYPE   ! type (BLK, TAB, SPC ...)
         CHARACTER (LEN=8)            :: PSNAME   ! name of point set
         INTEGER(int4)                :: OQI(4)   ! integer coefficients
         REAL(sp)                     :: OQR(2)   ! real coefficients
         INTEGER(int4), POINTER       :: IVTYP(:) ! type of output variable
         REAL(int4), POINTER          :: FAC(:)   ! multiplication factor of block output
         TYPE(ORQDAT), POINTER  :: NEXTORQ
      END TYPE ORQDAT

      TYPE(ORQDAT), SAVE, TARGET  :: FORQ
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWREAD : reads data (command OUTPut OPTions)
!     SWBLOK : produces block output
!     SWTABP : produces table output
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE OUTP_DATA

      MODULE M_SNL4
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.17: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     Feb. 01: New Module
!     Aug. 04: added linked list for RIAM
!
!  2. Purpose
!
!     Create global variables used in quadruplet subroutines
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables

!     MDIA  : Number of quadruplets in the MDIA formulation

      INTEGER(int4), PUBLIC, SAVE           :: MDIA = 1

!     AF11  : Contains the scaling frequency for the DIA.
!     CNL4_1: Contains the values for C1 in the MDIA formulation.
!     CNL4_2: Contains the values for C2 in the MDIA formulation.
!     LAMBDA: Contains the values for lambda in the MDIA formulation.
!DIR$ ATTRIBUTES ALIGN : 64 :: AF11
      REAL(sp), PUBLIC, SAVE, ALLOCATABLE :: AF11(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: CNL4_1
      REAL(sp), PUBLIC, SAVE, ALLOCATABLE :: CNL4_1(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: CNL4_2
      REAL(sp), PUBLIC, SAVE, ALLOCATABLE :: CNL4_2(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: LAMBDA
      REAL(sp), PUBLIC, SAVE, ALLOCATABLE :: LAMBDA(:)

      TYPE RIAMDAT
         INTEGER(int4)                :: II(3), JJ(3)
         REAL(sp)                     :: SSS, DI(3), DJ(3)
         TYPE(RIAMDAT), POINTER :: NEXTRIAM
      END TYPE RIAMDAT

      TYPE(RIAMDAT), SAVE, TARGET  :: FRIAM
      TYPE(RIAMDAT), SAVE, POINTER :: CURRIAM

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_SNL4

      MODULE M_BNDSPEC
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: M. Zijlema                                    |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.31: Marcel Zijlema
!
!  1. Updates
!
!     Nov. 03: New Module
!
!  2. Purpose
!
!     Contains data with respect to specification
!     of boundary conditions
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     BFILED  : data concerning boundary condition files
!     BGP     : array containing data w.r.t. boundary grid points
!     BSPLOC  : place in array BSPECS where to store interpolated spectra
!     BSPDIR  : spectral directions of input spectrum
!     BSPFRQ  : spectral frequencies of input spectrum
!     CUBGP   : current item in list of boundary grid points
!     DSHAPE  : indicates option for computation of directional distribution
!               in the spectrum (boundary spectra etc.)
!               =1: directional spread in degrees is given
!               =2: power of COS is given
!     FBNDFIL : first boundary condition file in list of files
!     FBGP    : first item in list of boundary grid points
!     FBS     : first item in list of boundary spectrum parameters
!     FSHAPE  : indicates option for computation of frequency distribution
!               in the spectrum (boundary spectra etc.)
!               =1: Pierson-Moskowitz
!               =2: Jonswap
!               =3: bin
!               =4: Gaussian
!     NBS     : index of BSPECS
!     NEXTBGP : pointer to next item in list of boundary grid points
!     NEXTBS  : pointer to next item in list of boundary spectrum parameters
!     NEXTBSPC: pointer to next boundary condition file in list
!     SPPARM  : integral parameters used for computation of
!               incident spectrum. Meaning:
!               1: significant wave height
!               2: wave period (peak or mean)
!               3: average wave direction
!               4: directional distribution coefficient

      TYPE BSPCDAT
         INTEGER(int4)                   :: BFILED(20)
         INTEGER(int4), POINTER          :: BSPLOC(:)
         REAL(sp),    POINTER          :: BSPDIR(:), BSPFRQ(:)
         TYPE(BSPCDAT), POINTER :: NEXTBSPC
      END TYPE BSPCDAT

      TYPE(BSPCDAT), SAVE, TARGET :: FBNDFIL

      TYPE BSDAT
         INTEGER(int4)                :: NBS
         INTEGER(int4)                :: FSHAPE, DSHAPE
         REAL(sp)                     :: SPPARM(4)
         TYPE(BSDAT), POINTER   :: NEXTBS
      END TYPE BSDAT

      TYPE(BSDAT), SAVE, TARGET :: FBS

      TYPE BGPDAT
         INTEGER(int4)                :: BGP(6)
         TYPE(BGPDAT), POINTER  :: NEXTBGP
      END TYPE BGPDAT

      TYPE(BGPDAT), SAVE, TARGET  :: FBGP
      TYPE(BGPDAT), SAVE, POINTER :: CUBGP

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_BNDSPEC

      MODULE M_OBSTA
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: M. Zijlema                                    |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.31: Marcel Zijlema
!
!  1. Updates
!
!     Nov. 03: New Module
!
!  2. Purpose
!
!     Contains data with respect to obstacles
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     FOBSTAC : first obstacle in list of obstacles
!     NCRPTS  : number of corner points in obstacle
!     NEXTOBST: pointer to next obstacle in list
!     RFCOEF  : reflection coefficients
!     RFTYP1  : reflection type: standard (REFL)
!     RFTYP2  : reflection type: diffusive/specular (RDIFF/RSPEC)
!     RFTYP3  : reflection type: frequency-dependent (RFD)
!     TRCOEF  : transmission coefficients
!     TRTYPE  : transmission type
!     XCRP    : x-coordinate of corner point
!     YCRP    : y-coordinate of corner point

      TYPE OBSTDAT
         INTEGER(int4)                :: TRTYPE
         REAL(sp)                   :: TRCOEF(3)
         INTEGER(int4)                :: RFTYP1, RFTYP2, RFTYP3
         REAL(sp)                   :: RFCOEF(6)
         INTEGER(int4)                :: NCRPTS
         REAL(sp), POINTER          :: XCRP(:), YCRP(:)
         TYPE(OBSTDAT), POINTER :: NEXTOBST
      END TYPE OBSTDAT

      TYPE(OBSTDAT), SAVE, TARGET  :: FOBSTAC

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_OBSTA

      MODULE M_GENARR
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: M. Zijlema                                    |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.31: Marcel Zijlema
!
!  1. Updates
!
!     Oct. 03: New Module
!
!  2. Purpose
!
!     Create several allocatable arrays for SWAN computation
!
!  3. Method
!
!     The following arrays will be created:
!
!     KGRPNT, KGRBND
!     XYTST
!     AC2
!     XCGRID, YCGRID
!     SPCSIG, SPCDIR
!     DEPTH , FRIC
!     UXB   , UYB
!     WXI   , WYI
!     WLEVL , ASTDF
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     AC2   : Contains action density at present time step
!     ASTDF : input field of air-sea temperature difference
!     DEPTH : input field of depth
!     FRIC  : input field of friction
!     KGRBND: array containing all boundary points
!             (+ 2 extra zeros as area separator for all separated areas)
!     KGRPNT: array containing indirect addresses for grid points
!     SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
!     SPCSIG: Relative frequencies in computational domain in sigma-space
!     UXB   : input field of contravariant U-velocity
!     UYB   : input field of contravariant V-velocity
!     WLEVL : input field of water level
!     WXI   : input field of wind U-velocity (contravariant)
!     WYI   : input field of wind V-velocity (contravariant)
!     XCGRID: Coordinates of computational grid in x-direction
!     XYTST : Grid point indices of test points
!     YCGRID: Coordinates of computational grid in y-direction
!DIR$ ATTRIBUTES ALIGN : 64 :: KGRPNT, KGRBND
      INTEGER(int4) , SAVE, ALLOCATABLE :: KGRPNT(:,:), KGRBND(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: XYTST
      INTEGER(int4), SAVE, ALLOCATABLE :: XYTST(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: AC2
      REAL(sp)   , SAVE, ALLOCATABLE :: AC2(:,:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: XCGRID, YCGRID
      REAL(sp)   , SAVE, ALLOCATABLE :: XCGRID(:,:), YCGRID(:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: SPCSIG, SPCDIR
      REAL(sp)   , SAVE, ALLOCATABLE :: SPCSIG(:)  , SPCDIR(:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: DEPTH, FRIC
      REAL(sp)   , SAVE, ALLOCATABLE :: DEPTH(:,:) , FRIC(:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: UXB, UYB
      REAL(sp)   , SAVE, ALLOCATABLE :: UXB(:,:)   , UYB(:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: WXI, WYI
      REAL(sp)   , SAVE, ALLOCATABLE :: WXI(:,:)   , WYI(:,:)
!DIR$ ATTRIBUTES ALIGN : 64 :: WLEVL, ASTDF
      REAL(sp)   , SAVE, ALLOCATABLE :: WLEVL(:,:) , ASTDF(:,:)

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_GENARR

      MODULE M_PARALL
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: M. Zijlema                                    |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     Dec. 03: New Module
!     Jul. 04: introduction logicals
!
!  2. Purpose
!
!     Contains data with respect to parallel process
!     based on distributed-memory apprach using MPI
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     IHALOX  : width of halo area in x-direction
!     IHALOY  : width of halo area in y-direction
!     MASTER  : rank of master process
!
      INTEGER(int4) :: MASTER
      INTEGER(int4) :: IHALOX, IHALOY
      PARAMETER (MASTER = 1,IHALOX = 3, IHALOY = 3)
            
!
!  7. Local variables
!
!     *** variables for parallel process with MPI:
!
!     INODE   : rank of present node
!     NPROC   : number of nodes
!     PARLL   : flag to denote run as parallel (.TRUE.) or not (.FALSE.)
!     SWINT   : MPI datatype for integers
!     SWMAX   : MPI collective maximum operation
!     SWMIN   : MPI collective minimum operation
!     SWREAL  : MPI datatype for reals
!     SWSUM   : MPI collective summation
!
      INTEGER(int4) :: INODE, NPROC
      INTEGER(int4) :: SWINT, SWREAL
      INTEGER(int4) :: SWMAX, SWMIN, SWSUM
      LOGICAL(int4) :: PARLL
!
!     *** information related to global domain and subdomains
!
!     IBLKAD  : administration array for subdomain interfaces
!               contents:
!               pos. 1                     number of neighbouring subdomains
!                                          =m
!               pos. 3*i-1                 number of i-th neighbour
!               pos. 3*i                   position of i-th neighbour with
!                                          respect to present subdomain
!               pos. 3*i+1                 pointer of i-th neighbour in
!                                          last part of this array
!               pos. 3*m+2                 number of overlapping unknowns
!                                          on subdomain interface
!               pos. 3*m+3 ... 3*m+2+n     position of unknown in array
!                                          to be sent to neighbour
!               pos. 3*m+3+n ... 3*m+2*n+2 position of unknown in array
!                                          to be received from neighbour
!     IWEIG   : weights to determine load per part
!     KGRBGL  : array containing all boundary points in global domain
!               (+ 2 extra zeros as area separator for all separated areas)
!     KGRPGL  : indirect addressing for grid points in global domain
!               =1: not active point
!               >1: active point
!     LENSPO  : format length for spectral output
!     LMXF    : logical indicating whether first x-point of subdomain equals
!               first x-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMXL    : logical indicating whether last x-point of subdomain equals
!               last x-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMYF    : logical indicating whether first y-point of subdomain equals
!               first y-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMYL    : logical indicating whether last y-point of subdomain equals
!               last y-point of global domain (=.TRUE.) or not (=.FALSE.)
!     MCGRDGL : number of wet grid points in global computational grid
!     MXCGL   : number of grid points in x-direction in global
!               computational grid
!     MXF     : first index w.r.t. global grid in x-direction
!     MXL     : last index w.r.t. global grid in x-direction
!     MYCGL   : number of grid points in y-direction in global
!               computational grid
!     MYF     : first index w.r.t. global grid in y-direction
!     MYL     : last index w.r.t. global grid in y-direction
!     NBGGL   : number of grid points for which boundary condition holds
!               in global domain
!     NGRBGL  : number of boundary points in global domain
!     XCLMAX  : maximum x-coordinate in subdomain
!     XCLMIN  : minimum x-coordinate in subdomain
!     YCLMAX  : maximum y-coordinate in subdomain
!     YCLMIN  : minimum y-coordinate in subdomain
!     XGRDGL  : x-coordinate of computational grid in global domain
!     YGRDGL  : y-coordinate of computational grid in global domain
!
      INTEGER(int4) :: MCGRDGL, MXCGL, MYCGL
      INTEGER(int4) :: MXF, MXL, MYF, MYL
      INTEGER(int4) :: NGRBGL, NBGGL
      REAL(sp) ::    XCLMAX, XCLMIN, YCLMAX, YCLMIN

      INTEGER(int4) :: LENSPO = 1000

      LOGICAL(int4) :: LMXF, LMXL, LMYF, LMYL
!DIR$ ATTRBUTES ALIGN : 64 :: IBLKAD
      INTEGER(int4), SAVE, ALLOCATABLE :: IBLKAD(:)
!DIR$ ATTRBUTES ALIGN : 64 :: IWEIG      
      INTEGER(int4), SAVE, ALLOCATABLE :: IWEIG(:)
!DIR$ ATTRBUTES ALIGN : 64 :: KGRPGL, KGRBGL      
      INTEGER(int4), SAVE, ALLOCATABLE :: KGRPGL(:,:), KGRBGL(:)
!DIR$ ATTRBUTES ALIGN : 64 :: XGRDGL, YGRDGL      
      REAL(sp)   , SAVE, ALLOCATABLE :: XGRDGL(:,:), YGRDGL(:,:)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_PARALL

      MODULE M_DIFFR
      use mod_kinds, only : int4, sp
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     40.21: Agnieszka Herman, Nico Booij
!
!  1. Updates
!
!     Aug. 01: New Module
!
!  2. Purpose
!
!     Contains global variables used in diffraction procedure
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     DIFPARAM: contains diffraction parameter (second derivative of Hs)
!     DIFPARDX: derivative of DIFPARAM in x-direction
!     DIFPARDY: derivative of DIFPARAM in y-direction
!DIR$ ATTRIBUTES ALIGN : 64 :: DIFPARAM
      REAL(sp), SAVE, ALLOCATABLE :: DIFPARAM(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: DIFPARDX
      REAL(sp), SAVE, ALLOCATABLE :: DIFPARDX(:)
!DIR$ ATTRIBUTES ALIGN : 64 :: DIFPARDY
      REAL(sp), SAVE, ALLOCATABLE :: DIFPARDY(:)

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     DIFPAR :   calculates the above arrays DIFPARAM, DIFPARDX, DIFPARDY
!     SPROSD :   computes propagation velocity in (x,y,theta) based on
!                arrays DIFPARAM, DIFPARDX, DIFPARDY
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      END MODULE M_DIFFR
!/impi
!/impi      MODULE MPI
!/impi      INCLUDE 'mpif.h'
!/impi      END MODULE MPI
