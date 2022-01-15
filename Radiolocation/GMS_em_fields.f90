

module em_fields


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'em_fields_simd'
 !          
 !          Purpose:
  !                     This module contains various implementations of
  !                      Computational ElectroMagnetics related routines.
  !                     This module operates on SoA and AoS data types.
 !          History:
 !                        
 !                        Date: 15-01-2022
 !                        Time: 17:04 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp,dp
     use em_fields_types, only : H_X_C1_4,H_XY_C1_4,H_XYZ_C1_4, &
                                 H_XYZ_R1_4,H_XY_R1_4,H_X_R1_4, &
                                 B_X_C1_4,B_XY_C1_4,B_XYZ_C1_4, &
                                 B_XYZ_R1_4,B_XY_R1_4,B_X_R1_4, &
                                 Js_XYZ_C1_4,H_X_C1_8,H_XY_C1_8,  &
                                 H_XYZ_C1_8,H_XYZ_R1_8,H_XY_R1_8,  &
                                 H_X_R1_8,B_X_C1_8,B_XY_C1_8,      &
                                 B_XYZ_C1_8,B_XYZ_R1_8,B_XY_R1_8,  &
                                 B_X_R1_8,Js_XYZ_C1_8

     implicit none
     public
      !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: EM_FIELDS_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: EM_FIELDS_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: EM_FIELDS_MICRO = 0
     ! Full version
     integer(kind=i4), parameter :: EM_FIELDS_FULLVER = &
          1000*EM_FIELDS_MAJOR+100*EM_FIELDS_MINOR+10*EM_FIELDS_MICRO
     ! Module creation date
     character(*),       parameter :: EM_FIELDS_CREATION_DATE = "15-01-2022 17:04 +00200 (SAT 15 JAN 2022 GMT+2)"
     ! Module build date
     character(*),       parameter :: EM_FIELDS_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: EM_FIELDS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: EM_FIELDS_SYNOPSIS      = " Computational ElectroMagnetics related routines"


     complex(kind=sp),   parameter, private :: jc4    = cmplx(0.0_sp,1.0_sp)
     complex(kind=dp),   parameter, private :: jc8    = cmplx(0.0_dp,1.0_dp)
     real(kind=sp),      parameter, private :: mu0r4  = 0.0000012566370614359173_sp
     real(kind=dp),      parameter, private :: mu0r8  = 0.0000012566370614359173_dp
     real(kind=sp),      parameter, private :: cr4    = 299792458.0_sp
     real(kind=dp),      parameter, private :: cr8    = 299792458.0_dp



   contains

     subroutine init_H_X_XY_XYZ_C1_4(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_C1_4  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_C1_4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_C1_4
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_C1_4),      intent(in)       :: HX
         type(H_XY_C1_4),     intent(in)       :: HXY
         type(H_XYZ_C1_4),    intent(in)       :: HXYZ
         integer(kind=i4),    intent(inout)    :: n_pts1
         integer(kind=i4),    intent(inout)    :: n_pts2
         integer(kind=i4),    intent(inout)    :: n_pts3
         integer(kind=i4),    intent(inout)    :: error
         integer(kind=i4),    intent(in)       :: iounit
         logical(kind=i4),    intent(in)       :: verbose
         logical(kind=i4),    intent(in)       :: logging
         character(len=*),    intent(in)       :: filename
         logical(kind=i4),    intent(in)       :: append
         ! LOcals
         character(len=256), automatic :: emsg
         !integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         !rem = 0
         !rem = MOD(n_pts1,16)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,16)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,16)
         !if(rem) n_pts3 = n_pts3-rem
         HX.n_cells   = n_pts1
         HXY.n_cells  = n_pts2
         HXYZ.n_cells = n_pts3
         allocate(HX.H_x(HX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(HXY.H_x(HXY.n_cells),
                  HXY.H_y(HXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(HXYZ.H_x(HXYZ.n_cells),
                  HXYZ.H_y(HXYZ.n_cells),
                  HXYZ.H_z(HXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_C1_4", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_C1_4


     subroutine init_H_X_XY_XYZ_R1_4(HX,HXY,HXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_R1_4  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_R1_4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_R1_4
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_R1_4),       intent(in)       :: HX
         type(H_XY_R1_4),      intent(in)       :: HXY
         type(H_XYZ_R1_4),     intent(in)       :: HXYZ
         integer(kind=i4),     intent(inout)    :: n_pts1
         integer(kind=i4),     intent(inout)    :: n_pts2
         integer(kind=i4),     intent(inout)    :: n_pts3
         integer(kind=i4),     intent(inout)    :: error
         integer(kind=i4),     intent(in)       :: iounit
         logical(kind=i4),     intent(in)       :: verbose
         logical(kind=i4),     intent(in)       :: logging
         character(len=*),     intent(in)       :: filename
         logical(kind=i4),     intent(in)       :: append
         ! LOcals
         character(len=256), automatic :: emsg
         !integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         !rem = 0
         !rem = MOD(n_pts1,16)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,16)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,16)
         !if(rem) n_pts3 = n_pts3-rem
         HX.n_cells   = n_pts1
         HXY.n_cells  = n_pts2
         HXYZ.n_cells = n_pts3
         allocate(HX.H_x(HX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(HXY.H_x(HXY.n_cells),
                  HXY.H_y(HXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(HXYZ.H_x(HXYZ.n_cells),
                  HXYZ.H_y(HXYZ.n_cells),
                  HXYZ.H_z(HXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_R1_4", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_R1_4
   





end module em_fields
