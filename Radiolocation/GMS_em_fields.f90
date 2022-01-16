

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
   

        subroutine init_B_X_XY_XYZ_C1_4(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_C1_4  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_C1_4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_C1_4
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_C1_4),       intent(in)       :: BX
         type(B_XY_C1_4),      intent(in)       :: BXY
         type(B_XYZ_C1_4),     intent(in)       :: BXYZ
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
         !! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         !rem = 0
         !rem = MOD(n_pts1,16)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,16)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,16)
         !if(rem) n_pts3 = n_pts3-rem
         BX.n_cells   = n_pts1
         BXY.n_cells  = n_pts2
         BXYZ.n_cells = n_pts3
         allocate(BX.B_x(BX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXY.B_x(BXY.n_cells),
                  BXY.B_y(BXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXYZ.B_x(BXYZ.n_cells),
                  BXYZ.B_y(BXYZ.n_cells),
                  BXYZ.B_z(BXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_C1_4", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_C1_4


     subroutine init_B_X_XY_XYZ_R1_4(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_R1_4  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_R1_4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_R1_4
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_R1_4),       intent(in)       :: BX
         type(B_XY_R1_4),      intent(in)       :: BXY
         type(B_XYZ_R1_4),     intent(in)       :: BXYZ
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
         BX.n_cells   = n_pts1
         BXY.n_cells  = n_pts2
         BXYZ.n_cells = n_pts3
         allocate(BX.B_x(BX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXY.B_x(BXY.n_cells),
                  BXY.B_y(BXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXYZ.B_x(BXYZ.n_cells),
                  BXYZ.B_y(BXYZ.n_cells),
                  BXYZ.B_z(BXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_R1_4", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_R1_4
  
     
     subroutine init_Js_XYZ_C1_4(JsXYZ,n_pts1,     &
                                iounit,verbose,logging, &
                                filename,append) !GCC$ ATTRIBUTES cold :: init_Js_XYZ_C1_4  !GCC$ ATTRIBUTES aligned(32) :: init_Js_XYZ_C1_4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_Js_XYZ_C1_4
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         
         type(Js_XYZ_C1_4),    intent(in)       :: JsXYZ
         integer(kind=i4),    intent(inout)    :: n_pts1
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
         JsXYZ.n_cells = n_pts1
         allocate(JsXYZ.Js_x(JsXYZ.n_cells),
                  JsXYZ.Js_y(JsXYZ.n_cells),
                  JsXYZ.Js_z(JsXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_Js_XYZ_C1_4", &
               emsg,__LINE__)
     end subroutine init_Js_XYZ_C1_4

       
     subroutine init_H_X_XY_XYZ_C1_8(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_C1_8  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_C1_8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_C1_8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_C1_8),       intent(in)       :: HX
         type(H_XY_C1_8),      intent(in)       :: HXY
         type(H_XYZ_C1_8),     intent(in)       :: HXYZ
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
         ! Allow only multiplicity of 8 i.e. the value of arguments: n_ptsx
         !rem = 0
         !rem = MOD(n_pts1,8)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,8)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,8)
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_C1_8", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_C1_8


     subroutine init_H_X_XY_XYZ_R1_8(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_R1_8  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_R1_8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_R1_8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_R1_8),       intent(in)       :: HX
         type(H_XY_R1_8),      intent(in)       :: HXY
         type(H_XYZ_R1_8),     intent(in)       :: HXYZ
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
         ! Allow only multiplicity of 8 i.e. the value of arguments: n_ptsx
         !rem = 0
         !rem = MOD(n_pts1,8)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,8)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,8)
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_R1_8", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_R1_8


     subroutine init_B_X_XY_XYZ_C1_8(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_C1_8  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_C1_8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_C1_8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_C1_8),       intent(in)       :: BX
         type(B_XY_C1_8),      intent(in)       :: BXY
         type(B_XYZ_C1_8),     intent(in)       :: BXYZ
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
         !rem = MOD(n_pts1,8)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,8)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,8)
         !if(rem) n_pts3 = n_pts3-rem
         BX.n_cells   = n_pts1
         BXY.n_cells  = n_pts2
         BXYZ.n_cells = n_pts3
         allocate(BX.B_x(BX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXY.B_x(BXY.n_cells),
                  BXY.B_y(BXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXYZ.B_x(BXYZ.n_cells),
                  BXYZ.B_y(BXYZ.n_cells),
                  BXYZ.B_z(BXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_C1_8", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_C1_8


     subroutine init_B_X_XY_XYZ_R1_8(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_R1_8  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_R1_8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_R1_8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_R1_8),       intent(in)       :: BX
         type(B_XY_R1_8),      intent(in)       :: BXY
         type(B_XYZ_R1_8),     intent(in)       :: BXYZ
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
         !rem = MOD(n_pts1,8)
         !if(rem) n_pts1 = n_pts1-rem
         !rem = MOD(n_pts2,8)
         !if(rem) n_pts2 = n_pts2-rem
         !rem = MOD(n_pts3,8)
         !if(rem) n_pts3 = n_pts3-rem
         BX.n_cells   = n_pts1
         BXY.n_cells  = n_pts2
         BXYZ.n_cells = n_pts3
         allocate(BX.B_x(BX.n_cells),STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXY.B_x(BXY.n_cells),
                  BXY.B_y(BXY.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         allocate(BXYZ.B_x(BXYZ.n_cells),
                  BXYZ.B_y(BXYZ.n_cells),
                  BXYZ.B_z(BXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_R1_8", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_R1_8
  

     subroutine init_Js_XYZ_C1_8(JsXYZ,n_pts1,     &
                                iounit,verbose,logging, &
                                filename,append) !GCC$ ATTRIBUTES cold :: init_Js_XYZ_C1_8  !GCC$ ATTRIBUTES aligned(32) :: init_Js_XYZ_C1_8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_Js_XYZ_C1_8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         
         type(Js_XYZ_C1_8),    intent(in)       :: JsXYZ
         integer(kind=i4),    intent(inout)    :: n_pts1
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
         !rem = MOD(n_pts1,8)
         !if(rem) n_pts1 = n_pts1-rem
         JsXYZ.n_cells = n_pts1
         allocate(JsXYZ.Js_x(JsXYZ.n_cells),
                  JsXYZ.Js_y(JsXYZ.n_cells),
                  JsXYZ.Js_z(JsXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_Js_XYZ_C1_8", &
               emsg,__LINE__)
     end subroutine init_Js_XYZ_C1_8

     !=========================================================
     !      Computational helpers
     !=========================================================

     pure function dot_c1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_c1_8
        !DIR$ ATTRIBUTES VECTOR ::  dot_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_c1_8
#endif
       complex(kind=dp),  dimension(:), intent(in) :: x
       complex(kind=dp),  dimension(:), intent(in) :: y
       complex(kind=dp) :: z
       ! Exec code ....
       z=sum(x*y)
     end function dot_c1_8


     pure function dot_c1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_c1_4
        !DIR$ ATTRIBUTES VECTOR ::  dot_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_c1_4
#endif
       complex(kind=sp),  dimension(:), intent(in) :: x
       complex(kind=sp),  dimension(:), intent(in) :: y
       complex(kind=sp) :: z
       ! Exec code ....
       z=sum(x*y)
     end function dot_c1_4


     pure function dot_r1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_r1_8
        !DIR$ ATTRIBUTES VECTOR ::  dot_r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_r1_8
#endif
         real(kind=dp),   dimension(:), intent(in) :: x
         real(kind=dp),   dimension(:), intent(in) :: y
         real(kind=dp) :: z
         ! Exec code ...
         z=sum(x*y)
     end function dot_r1_8


     pure function dot_r1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_r1_4
        !DIR$ ATTRIBUTES VECTOR ::  dot_r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_r1_4
#endif
         real(kind=sp),   dimension(:), intent(in) :: x
         real(kind=sp),   dimension(:), intent(in) :: y
         real(kind=sp) :: z
         ! Exec code ...
         z=sum(x*y)
     end function dot_r1_4


     pure function dot_r1c1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_r1c1_8
        !DIR$ ATTRIBUTES VECTOR ::  dot_r1c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_r1c1_8
#endif
         real(kind=dp),     dimension(:), intent(in) :: x
         complex(kind=dp),  dimension(:), intent(in) :: y
         complex(kind=dp) :: z
         ! Exec code ....
         z=sum(x*y)
     end function dot_r1c1_8


     pure function dot_r1c1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dot_r1c1_4
        !DIR$ ATTRIBUTES VECTOR ::  dot_r1c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dot_r1c1_4
#endif
         real(kind=sp),     dimension(:), intent(in) :: x
         complex(kind=sp),  dimension(:), intent(in) :: y
         complex(kind=sp) :: z
         ! Exec code ....
         z=sum(x*y)
     end function dot_r1c1_4


     pure function norm_c1_8(x) result(y)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: norm_c1_8
        !DIR$ ATTRIBUTES VECTOR ::  norm_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm_c1_8
#endif
         complex(kind=dp),  dimension(:), intent(in) :: x
         real(kind=dp) :: y
         real(kind=dp), automatic :: t0
         ! Exec code ....
         t0=real(dot_c1_8(conjg(x),x),kind=dp)
         y=sqrt(t0)
     end function norm_c1_8
     

     pure function norm_c1_4(x) result(y)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: norm_c1_4
        !DIR$ ATTRIBUTES VECTOR ::  norm_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm_c1_4
#endif
         complex(kind=sp),  dimension(:), intent(in) :: x
         real(kind=sp) :: y
         real(kind=sp), automatic :: t0
         ! Exec code ....
         t0=real(dot_c1_4(conjg(x),x),kind=sp)
         y=sqrt(t0)
     end function norm_c1_4


     pure function norm_r1_8(x) result(y)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: norm_r1_8
        !DIR$ ATTRIBUTES VECTOR ::  norm_r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm_r1_8
#endif
        real(kind=dp),  dimension(:), intent(in) :: x
        real(kind=dp) :: y
        ! EXec code ...
        y=sum(x*x)
     end function norm_r1_8
     
       
     pure function norm_r1_4(x) result(y)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: norm_r1_4
        !DIR$ ATTRIBUTES VECTOR ::  norm_r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm_r1_4
#endif
        real(kind=sp),  dimension(:), intent(in) :: x
        real(kind=sp) :: y
        ! EXec code ...
        y=sum(x*x)
     end function norm_r1_4


    pure function cross_r1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_r1_8
        !DIR$ ATTRIBUTES VECTOR :: cross_r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_r1_8
#endif
        real(kind=dp), dimension(3), intent(in) :: x
        real(kind=dp), dimension(3), intent(in) :: y
        real(kind=dp), dimension(3) :: z
        ! Exec code ...
        z(1)=x(2)*y(3)-x(3)*y(2)
        z(2)=x(3)*y(1)-x(1)*y(3)
        z(3)=x(1)*y(2)-x(2)*y(1)
     end function cross_r1_8


     pure function cross_r1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_r1_4
        !DIR$ ATTRIBUTES VECTOR :: cross_r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_r1_4
#endif
        real(kind=sp), dimension(3), intent(in) :: x
        real(kind=sp), dimension(3), intent(in) :: y
        real(kind=sp), dimension(3) :: z
        ! Exec code ...
        z(1)=x(2)*y(3)-x(3)*y(2)
        z(2)=x(3)*y(1)-x(1)*y(3)
        z(3)=x(1)*y(2)-x(2)*y(1)
     end function cross_r1_4


     pure function cross_c1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_c1_8
        !DIR$ ATTRIBUTES VECTOR :: cross_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_c1_8
#endif
         complex(kind=dp),  dimension(3), intent(in) :: x
         complex(kind=dp),  dimension(3), intent(in) :: y
         complex(kind=dp), dimension(3) :: z
         ! Exec code ....
         z(1)=x(2)*y(3)-x(3)*y(2)
         z(2)=x(3)*y(1)-x(1)*y(3)
         z(3)=x(1)*y(2)-x(2)*y(1)
    end function cross_c1_8


     pure function cross_c1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_c1_4
        !DIR$ ATTRIBUTES VECTOR :: cross_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_c1_4
#endif
         complex(kind=sp),  dimension(3), intent(in) :: x
         complex(kind=sp),  dimension(3), intent(in) :: y
         complex(kind=sp),  dimension(3) :: z
         ! Exec code ....
         z(1)=x(2)*y(3)-x(3)*y(2)
         z(2)=x(3)*y(1)-x(1)*y(3)
         z(3)=x(1)*y(2)-x(2)*y(1)
      end function cross_c1_4


      pure function cross_c1r1_8(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_c1r1_8
        !DIR$ ATTRIBUTES VECTOR :: cross_c1r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_c1r1_8
#endif
          complex(kind=dp),   dimension(3),  intent(in) :: x
          real(kind=dp),      dimension(3),  intent(in) :: y
          complex(kind=dp) :: z
          ! Exec code ...
          z(1)=x(2)*y(3)-x(3)*y(2)
          z(2)=x(3)*y(1)-x(1)*y(3)
          z(3)=x(1)*y(2)-x(2)*y(1)
      end function cross_c1r1_8


      pure function cross_c1r1_4(x,y) result(z)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: cross_c1r1_4
        !DIR$ ATTRIBUTES VECTOR :: cross_c1r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cross_c1r1_4
#endif
          complex(kind=sp),   dimension(3),  intent(in) :: x
          real(kind=sp),      dimension(3),  intent(in) :: y
          complex(kind=sp) :: z
          ! Exec code ...
          z(1)=x(2)*y(3)-x(3)*y(2)
          z(2)=x(3)*y(1)-x(1)*y(3)
          z(3)=x(1)*y(2)-x(2)*y(1)
     end function cross_c1r1_4


    pure function dir_vec_r1_8(th,phi) result(vd)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dir_vec_r1_8
        !DIR$ ATTRIBUTES VECTOR :: dir_vec_r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vec_r1_8
#endif
         real(kind=dp),     intent(in) :: th
         real(kind=dp),     intent(in) :: phi
         real(kind=dp), dimension(3) :: vd
         real(kind=dp), automatic :: st
         ! Exec code ....
         st=sin(th)
         vd(1)=st*cos(phi)
         vd(2)=st*sin(phi)
         vd(3)=cos(th)
      end function dir_vec_r1_8

      
     pure function dir_vec_r1_4(th,phi) result(vd)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: dir_vec_r1_4
        !DIR$ ATTRIBUTES VECTOR :: dir_vec_r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vec_r1_4
#endif
         real(kind=sp),     intent(in) :: th
         real(kind=sp),     intent(in) :: phi
         real(kind=sp), dimension(3) :: vd
         real(kind=sp), automatic :: st
         ! Exec code ....
         st=sin(th)
         vd(1)=st*cos(phi)
         vd(2)=st*sin(phi)
         vd(3)=cos(th)
     end function dir_vec_r1_4


     pure function pol_vec_r1_8(th,phi,psi) result(vp)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: pol_vec_r1_8
        !DIR$ ATTRIBUTES VECTOR :: pol_vec_r1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vec_r1_4
#endif
       real(kind=dp),   intent(in) :: th
       real(kind=dp),   intent(in) :: phi
       real(kind=dp),   intent(in) :: psi
       real(kind=dp),  dimension(3) :: vp
       ! Locals
       real(kind=dp), automatic :: cphi,spsi,sphi,cth
       ! Exec code ....
       cphi=cos(phi)
       spsi=sin(psi)
       sphi=sin(phi)
       cth=cos(th)
       vp(1)=cos(psi)*sphi-spsi*cth
       vp(2)=-cos(psi)*cphi-spsi*cth*sphi
       vp(3)=spsi*sin(th)
     end function pol_vec_r1_8


     pure function pol_vec_r1_4(th,phi,psi) result(vp)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: pol_vec_r1_4
        !DIR$ ATTRIBUTES VECTOR :: pol_vec_r1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vec_r1_4
#endif
       real(kind=sp),   intent(in) :: th
       real(kind=sp),   intent(in) :: phi
       real(kind=sp),   intent(in) :: psi
       real(kind=sp),  dimension(3) :: vp
       ! Locals
       real(kind=sp), automatic :: cphi,spsi,sphi,cth
       ! Exec code ....
       cphi=cos(phi)
       spsi=sin(psi)
       sphi=sin(phi)
       cth=cos(th)
       vp(1)=cos(psi)*sphi-spsi*cth
       vp(2)=-cos(psi)*cphi-spsi*cth*sphi
       vp(3)=spsi*sin(th)
     end function pol_vec_r1_8


     
     
 
     
      

     pure function H_XYZ_P_c1_8(pol,d,k,r) result(Hxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: H_XYZ_P_c1_8
        !DIR$ ATTRIBUTES VECTOR :: H_XYZ_P_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_XYZ_P_c1_8
#endif
         real(kind=dp), dimension(3), intent(in) :: pol
         real(kind=dp), dimension(3), intent(in) :: d
         real(kind=dp), dimension(3), intent(in) :: k
         complex(kind=dp),            intent(in) :: r
         complex(kind=dp), dimension(3) :: Hxyz
         ! Exec code ...
         real(kind=dp), automatic :: arg
         arg=dot_r1_8(d,r)
         Hxyz=pol*exp(jc8*k*arg)
      end function H_XYZ_P_c1_8


      pure function H_XYZ_P_c1_4(pol,d,k,r) result(Hxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: H_XYZ_P_c1_4
        !DIR$ ATTRIBUTES VECTOR :: H_XYZ_P_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_XYZ_P_c1_4
#endif
         real(kind=sp), dimension(3), intent(in) :: pol
         real(kind=sp), dimension(3), intent(in) :: d
         real(kind=sp), dimension(3), intent(in) :: k
         complex(kind=sp),            intent(in) :: r
         complex(kind=sp), dimension(3) :: Hxyz
         ! Exec code ...
         real(kind=sp), automatic :: arg
         arg=dot_r1_4(d,r)
         Hxyz=pol*exp(jc4*k*arg)
      end function H_XYZ_P_c1_4


     pure function B_XYZ_P_c1_8(pol,d,k,om,r) result(Bxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: B_XYZ_P_c1_8
        !DIR$ ATTRIBUTES VECTOR :: B_XYZ_P_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_P_c1_8
#endif
         real(kind=dp), dimension(3), intent(in) :: pol
         real(kind=dp), dimension(3), intent(in) :: d
         real(kind=dp), dimension(3), intent(in) :: k
         real(kind=dp),               intent(in) :: om
         complex(kind=dp),            intent(in) :: r
         complex(kind=dp), dimension(3) :: Bxyz
         ! LOcals
         complex(kind=dp), dimension(3) :: Hxyz
         complex(kind=dp), automatic :: cd
         cd=cmplx(d,kind=dp)
         Hxyz=H_XYZ_P_c1_8(pol,d,k,r)
         Bxyz=k/(om*mu0r8)*cross_c1_8(cd,Hxyz)
      end function B_XYZ_P_c1_8


      pure function B_XYZ_P_c1_4(pol,d,k,om,r) result(Bxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: B_XYZ_P_c1_4
        !DIR$ ATTRIBUTES VECTOR :: B_XYZ_P_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_P_c1_4
#endif
         real(kind=sp), dimension(3), intent(in) :: pol
         real(kind=sp), dimension(3), intent(in) :: d
         real(kind=sp), dimension(3), intent(in) :: k
         real(kind=sp),               intent(in) :: om
         complex(kind=sp),            intent(in) :: r
         complex(kind=sp), dimension(3) :: Bxyz
         ! LOcals
         complex(kind=sp), dimension(3) :: Hxyz
         complex(kind=sp), automatic :: cd
         cd=cmplx(d,kind=sp)
         Hxyz=H_XYZ_P_c1_4(pol,d,k,r)
         Bxyz=k/(om*mu0r8)*cross_c1_4(cd,Hxyz)
      end function B_XYZ_P_c1_4


      subroutine H_B_XYZ_P_c1_8(th,phi,psi,om,perm,pol,Hxyz,Bxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: H_B_XYZ_P_c1_8
        !DIR$ ATTRIBUTES VECTOR :: H_B_XYZ_P_c1_8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_B_XYZ_P_c1_8
#endif
         real(kind=dp),               intent(in) :: th
         real(kind=dp),               intent(in) :: phi
         real(kind=dp),               intent(in) :: psi
         real(kind=dp),               intent(in) :: om
         complex(kind=dp),            intent(in) :: perm
         real(kind=dp), dimension(3), intent(in) :: pol
         complex(kind=dp), dimension(3) :: Hxyz
         complex(kind=dp), dimension(3) :: Bxyz
         ! Locals
         real(kind=dp), dimension(3), automatic :: vp
         real(kind=dp), dimension(3), automatic :: vd
         complex(kind=dp), automatic :: k
         ! Exec code ...
         k=perm*om/c1_8
         vd=dir_vec_r1_8(th,phi)
         vp=pol_vec_r1_8(th,phi,psi)
         Hxyz=H_XYZ_P_c1_8(vp,vd,k,pol)
         Bxyz=B_XYZ_P_C1_8(vp,vd,k,om,pol)
       end subroutine H_B_XYZ_P_c1_8


       subroutine H_B_XYZ_P_c1_4(th,phi,psi,om,perm,pol,Hxyz,Bxyz)
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES INLINE :: H_B_XYZ_P_c1_4
        !DIR$ ATTRIBUTES VECTOR :: H_B_XYZ_P_c1_4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_B_XYZ_P_c1_4
#endif
         real(kind=sp),               intent(in) :: th
         real(kind=sp),               intent(in) :: phi
         real(kind=sp),               intent(in) :: psi
         real(kind=sp),               intent(in) :: om
         complex(kind=sp),            intent(in) :: perm
         real(kind=sp), dimension(3), intent(in) :: pol
         complex(kind=sp), dimension(3) :: Hxyz
         complex(kind=sp), dimension(3) :: Bxyz
         ! Locals
         real(kind=sp), dimension(3), automatic :: vp
         real(kind=sp), dimension(3), automatic :: vd
         complex(kind=sp), automatic :: k
         ! Exec code ...
         k=perm*om/c1_4
         vd=dir_vec_r1_4(th,phi)
         vp=pol_vec_r1_4(th,phi,psi)
         Hxyz=H_XYZ_P_c1_4(vp,vd,k,pol)
         Bxyz=B_XYZ_P_C1_4(vp,vd,k,om,pol)
      end subroutine H_B_XYZ_P_c1_4
      
 
      
     
       
     
     
     
     

end module em_fields
