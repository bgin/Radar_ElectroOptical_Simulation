

module  em_fields_simd



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
 !          History:
 !                        
 !                        Date: 18-12-2021
 !                        Time: 15:56 GMT+2
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
     use em_fields_types, only : H_X_C16,H_XY_C16,H_XYZ_C16, &
                                 H_XYZ_R16,H_XY_R16,H_X_R16, &
                                 B_X_C16,B_XY_C16,B_XYZ_C16, &
                                 B_XYZ_R16,B_XY_R16,B_X_R16, &
                                 Js_XYZ_C16,H_X_C8,H_XY_C8,  &
                                 H_XYZ_C8,H_XYZ_R8,H_XY_R8,  &
                                 H_X_R8,B_X_C8,B_XY_C8,      &
                                 B_XYZ_C8,B_XYZ_R8,B_XY_R8,  &
                                 B_X_R8,Js_XYZ_C8
     use mod_avx512c16f32
     use mod_avx512c8f64
     use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: EM_FIELDS_SIMD_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: EM_FIELDS_SIMD_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: EM_FIELDS_SIMD_MICRO = 0
     ! Full version
     integer(kind=i4), parameter :: EM_FIELDS_SIMD_FULLVER = &
          1000*EM_FIELDS_SIMD_MAJOR+100*EM_FIELDS_SIMD_MINOR+10*EM_FIELDS_SIMD_MICRO
     ! Module creation date
     character(*),       parameter :: EM_FIELDS_SIMD_CREATION_DATE = "18-12-2021 15:57 +00200 (SAT 18 DEC 2021 GMT+2)"
     ! Module build date
     character(*),       parameter :: EM_FIELDS_SIMD_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: EM_FIELDS_SIMD_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: EM_FIELDS_SIMD_SYNOPSIS      = " Computational ElectroMagnetics related routines"

     type(ZMM16c4),   parameter, private :: jc4    = ZMM16c4((0.0_sp,1.0_sp))
     type(ZMM8c8),    parameter, private :: jc8    = ZMM8c8((0.0_dp,1.0_dp))
     type(ZMM16r4_t), parameter, private :: mu0r16 = ZMM16r4_t(0.0000012566370614359173_sp)
     type(ZMM8r4_t),  parameter, private :: mu0r8  = ZMM8r4_t(0.0000012566370614359173_dp)
     type(ZMM16r4_t), parameter, private :: cr4    = ZMM16r4_t(299792458.0_sp)
     type(ZMM8r4_t),  parameter, private :: cr8    = ZMM8r8_t(299792458.0_dp)

  
     
    

     

    

    
     
        
     
    contains

      
   subroutine init_H_X_XY_XYZ_C16(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_C16  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_C16
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_C16
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_C16),       intent(in)       :: HX
         type(H_XY_C16),      intent(in)       :: HXY
         type(H_XYZ_C16),     intent(in)       :: HXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,16)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,16)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,16)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_C16", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_C16    
   
      
    
     subroutine init_H_X_XY_XYZ_R16(HX,HXY,HXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_R16  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_R16
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_R16
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_R16),       intent(in)       :: HX
         type(H_XY_R16),      intent(in)       :: HXY
         type(H_XYZ_R16),     intent(in)       :: HXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,16)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,16)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,16)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_R16", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_R16

       
     subroutine init_B_X_XY_XYZ_C16(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_C16  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_C16
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_C16
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_C16),       intent(in)       :: BX
         type(B_XY_C16),      intent(in)       :: BXY
         type(B_XYZ_C16),     intent(in)       :: BXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,16)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,16)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,16)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_C16", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_C16


     subroutine init_B_X_XY_XYZ_R16(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_C16  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_C16
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_R16
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_R16),       intent(in)       :: BX
         type(B_XY_R16),      intent(in)       :: BXY
         type(B_XYZ_R16),     intent(in)       :: BXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,16)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,16)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,16)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_R16", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_R16
  
     
     subroutine init_Js_XYZ_C16(JsXYZ,n_pts1,     &
                                iounit,verbose,logging, &
                                filename,append) !GCC$ ATTRIBUTES cold :: init_Js_XYZ_C16  !GCC$ ATTRIBUTES aligned(32) :: init_Js_XYZ_C16
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_Js_XYZ_C16
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         
         type(Js_XYZ_C16),    intent(in)       :: JsXYZ
         integer(kind=i4),    intent(inout)    :: n_pts1
         integer(kind=i4),    intent(inout)    :: error
         integer(kind=i4),    intent(in)       :: iounit
         logical(kind=i4),    intent(in)       :: verbose
         logical(kind=i4),    intent(in)       :: logging
         character(len=*),    intent(in)       :: filename
         logical(kind=i4),    intent(in)       :: append
         ! LOcals
         character(len=256), automatic :: emsg
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,16)
         if(rem) n_pts1 = n_pts1-rem
         JsXYZ.n_cells = n_pts1
         allocate(JsXYZ.Js_x(JsXYZ.n_cells),
                  JsXYZ.Js_y(JsXYZ.n_cells),
                  JsXYZ.Js_z(JsXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_Js_XYZ_C16", &
               emsg,__LINE__)
     end subroutine init_Js_XYZ_C16

       
     subroutine init_H_X_XY_XYZ_C8(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_C8  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_C8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_C8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_C8),       intent(in)       :: HX
         type(H_XY_C8),      intent(in)       :: HXY
         type(H_XYZ_C8),     intent(in)       :: HXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 8 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,8)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,8)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,8)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_C8", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_C8


     subroutine init_H_X_XY_XYZ_R8(HX,HXY,HXYZ,n_pts1,     &
                                  n_pts2,n_pts3,error,    &
                                  iounit,verbose,logging, &
                                  filename,append) !GCC$ ATTRIBUTES cold :: init_H_X_XY_XYZ_R8  !GCC$ ATTRIBUTES aligned(32) :: init_H_X_XY_XYZ_R8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_H_X_XY_XYZ_R8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(H_X_R8),       intent(in)       :: HX
         type(H_XY_R8),      intent(in)       :: HXY
         type(H_XYZ_R8),     intent(in)       :: HXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 8 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,8)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,8)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,8)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_H_X_XY_XYZ_R8", &
               emsg,__LINE__)
     end subroutine init_H_X_XY_XYZ_R8


     subroutine init_B_X_XY_XYZ_C8(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_C8  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_C8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_C8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_C8),       intent(in)       :: BX
         type(B_XY_C8),      intent(in)       :: BXY
         type(B_XYZ_C8),     intent(in)       :: BXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,8)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,8)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,8)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_C8", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_C8


     subroutine init_B_X_XY_XYZ_R8(BX,BXY,BXYZ,n_pts1,     &
                                    n_pts2,n_pts3,error,    &
                                    iounit,verbose,logging, &
                                    filename,append) !GCC$ ATTRIBUTES cold :: init_B_X_XY_XYZ_R8  !GCC$ ATTRIBUTES aligned(32) :: init_B_X_XY_XYZ_R8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_B_X_XY_XYZ_R8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         type(B_X_R8),       intent(in)       :: BX
         type(B_XY_R8),      intent(in)       :: BXY
         type(B_XYZ_R8),     intent(in)       :: BXYZ
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
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,8)
         if(rem) n_pts1 = n_pts1-rem
         rem = MOD(n_pts2,8)
         if(rem) n_pts2 = n_pts2-rem
         rem = MOD(n_pts3,8)
         if(rem) n_pts3 = n_pts3-rem
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
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_B_X_XY_XYZ_R8", &
               emsg,__LINE__)
     end subroutine init_B_X_XY_XYZ_R8
  

     subroutine init_Js_XYZ_C8(JsXYZ,n_pts1,     &
                                iounit,verbose,logging, &
                                filename,append) !GCC$ ATTRIBUTES cold :: init_Js_XYZ_C8  !GCC$ ATTRIBUTES aligned(32) :: init_Js_XYZ_C8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: init_Js_XYZ_C8
       !DIR$ OPTIMIZE : 3
#endif
         use mod_print_error, only : handle_fatal_memory_error
         
         type(Js_XYZ_C8),    intent(in)       :: JsXYZ
         integer(kind=i4),    intent(inout)    :: n_pts1
         integer(kind=i4),    intent(inout)    :: error
         integer(kind=i4),    intent(in)       :: iounit
         logical(kind=i4),    intent(in)       :: verbose
         logical(kind=i4),    intent(in)       :: logging
         character(len=*),    intent(in)       :: filename
         logical(kind=i4),    intent(in)       :: append
         ! LOcals
         character(len=256), automatic :: emsg
         integer(kind=i4),   automatic :: rem
         ! EXec code ....
         ! Allow only multiplicity of 16 i.e. the value of arguments: n_ptsx
         rem = 0
         rem = MOD(n_pts1,8)
         if(rem) n_pts1 = n_pts1-rem
         JsXYZ.n_cells = n_pts1
         allocate(JsXYZ.Js_x(JsXYZ.n_cells),
                  JsXYZ.Js_y(JsXYZ.n_cells),
                  JsXYZ.Js_z(JsXYZ.n_cells),
                  STAT=error,ERRMSG=emsg)
         if(error/=0) goto 9999
         error = 0
         return
9999     call handle_fatal_memory_error(iounit,logging,verbose,append,fname, &
              "logger: "//__FILE__// "module: ElectroMagnetics, prog unit: init_Js_XYZ_C8", &
               emsg,__LINE__)
     end subroutine init_Js_XYZ_C8

       
       

       


       
      


     ! Helper functions

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function sdotv_zmm16r4(v1x,v1y,v1z, &
                                 v2x,v2y,v2z) result(res) !GCC$ ATTRIBUTES aligned(32) :: sdotv_zmm16r4 !GCC$ ATTRIBUTES inline :: sdotv_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: sdotv_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function sdotv_zmm16r4(v1x,v1y,v1z, &
                                 v2x,v2y,v2z) result(res)
        !DIR$ ATTRIBUTES INLINE :: sdotv_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: sdotv_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: sdotv_zmm16r4
#endif
        
         type(ZMM16r4_t),   intent(in) :: v1x
         type(ZMM16r4_t),   intent(in) :: v1y
         type(ZMM16r4_t),   intent(in) :: v1z
         type(ZMM16r4_t),   intent(in) :: v2x
         type(ZMM16r4_t),   intent(in) :: v2y
         type(ZMM16r4_t),   intent(in) :: v2z
         type(ZMM16r4_t) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: res
#endif
         !Locals
         type(ZMM16r4_t), automatic :: t0,t1,t2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
#endif
         
         t0.v   = v1x.v*v2x.v
         t1.v   = v1y.v*v2y.v
         t2.v   = v1z.v*v2z.v
         res.v = t0.v+t1.v+t2.v
     end function sdotv_zmm16r4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function cdotv_zmm16c4(v1x,v1y,v1z, &
                                 v2x,v2y,v2z) result(res)  !GCC$ ATTRIBUTES aligned(32) :: cdotv_zmm16c4 !GCC$ ATTRIBUTES inline :: cdotv_zmm16c4 !GCC$ ATTRIBUTES vectorcall :: cdotv_zmm16c4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function cdotv_zmm16c4(v1x,v1y,v1z, &
                                 v2x,v2y,v2z) result(res)
        !DIR$ ATTRIBUTES INLINE :: cdotv_zmm16c4
        !DIR$ ATTRIBUTES VECTOR :: cdotv_zmm16c4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdotv_zmm16c4
#endif
         type(ZMM16c4),   intent(in) :: v1x
         type(ZMM16c4),   intent(in) :: v1y
         type(ZMM16c4),   intent(in) :: v1z
         type(ZMM16c4),   intent(in) :: v2x
         type(ZMM16c4),   intent(in) :: v2y
         type(ZMM16c4),   intent(in) :: v2z
         
         !complex(kind=sp) :: res
         type(ZMM16c4) :: res
         type(ZMM16c4), automatic :: tx,ty,tz
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: tx
         !DIR$ ATTRIBUTES ALIGN : 64 :: ty
         !DIR$ ATTRIBUTES ALIGN : 64 :: tz
#endif
         !t0 = v1*v2
         !res = complex(sum(t0.re),sum(t0.im))
         tx = v1x*v2x
         ty = v1y*v2y
         tz = v1z*v2z
         !tx.re(0)+ty.re(0)+tz.re(0)
         res.re = tx.re+ty.re+tz.re
         res.im = tx.im+ty.im+tz.im
      end function cdotv_zmm16c4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      pure function cnorm_zmm16c4(vx,vy,vz) result(vs) !GCC$ ATTRIBUTES aligned(32) :: cnorm_zmm16c4 !GCC$ ATTRIBUTES inline :: cnorm_zmm16c4 !GCC$ ATTRIBUTES vectorcall :: cnorm_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      pure function cnorm_zmm16c4(vx,vy,vz) result(vs)
           !DIR$ ATTRIBUTES INLINE :: cnorm_zmm16c4
           !DIR$ ATTRIBUTES VECTOR :: cnorm_zmm16c4
           !DIR$ OPTIMIZE : 3
           !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cnorm_zmm16c4
#endif
           type(ZMM16c4),   intent(in) :: vx
           type(ZMM16c4),   intent(in) :: vy
           type(ZMM16c4),   intent(in) :: vz
           
           type(ZMM16r4_t) :: vs
           ! Local
           type(ZMM16c4), automatic :: cx,cy,cz
           type(ZMM16c4), automatic :: t
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: vs
           !DIR$ ATTRIBUTES ALIGN : 64 :: cx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cz
#endif
           cx = conjugate(vx)
           cy = conjugate(vy)
           cz = conjugate(vz)
           !s = sqrt(real(cdotv_zmm16c4(v,c),kind=sp))
           t = cdotv_zmm16c4(vx,vy,vz, &
                             cx,cy,cz)
           vs.v = sqrt(t.re)
       end function cnorm_zmm16c4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      pure function cnorm_zmm8c8(vx,vy,vz) result(vs) !GCC$ ATTRIBUTES aligned(32) :: cnorm_zmm8c8 !GCC$ ATTRIBUTES inline :: cnorm_zmm8c8 !GCC$ ATTRIBUTES vectorcall :: cnorm_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      pure function cnorm_zmm8c8(vx,vy,vz) result(vs)
           !DIR$ ATTRIBUTES INLINE :: cnorm_zmm8c8
           !DIR$ ATTRIBUTES VECTOR :: cnorm_zmm8c8
           !DIR$ OPTIMIZE : 3
           !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cnorm_zmm8c8
#endif
           type(ZMM8c8),   intent(in) :: vx
           type(ZMM8c8),   intent(in) :: vy
           type(ZMM8c8),   intent(in) :: vz
           
           type(ZMM8r8_t)  :: vs
           ! Local
           type(ZMM8c8), automatic :: cx,cy,cz
           type(ZMM8c8), automatic :: t
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: cx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cz
           
#endif
           !c = conjugate(v)
           !s = sqrt(real(cdotv_zmm8c8(v,c),kind=dp))
           cx = conjugate(vx)
           cy = conjugate(vy)
           cz = conjugate(vz)
           t = zdotv_zmm8c8(vx,vy,vz, &
                             cx,cy,cz)
           vs.v = sqrt(t.re)
      end function cnorm_zmm8c8         
        


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function zdotv_zmm8c8(v1x,v1y,v1z, &
                                v2x,v2y,v2z) result(res)  !GCC$ ATTRIBUTES aligned(32) :: zdotv_zmm8c8 !GCC$ ATTRIBUTES inline :: zdotv_zmm8c8 !GCC$ ATTRIBUTES vectorcall :: zdotv_zmm8c8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function zdotv_zmm8c8(v1x,v1y,v1z, &
                                v2x,v2y,v2z) result(res)
        !DIR$ ATTRIBUTES INLINE :: zdotv_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: zdotv_zmm8c8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: zdotv_zmm8c8
#endif
         type(ZMM8c8),   intent(in) :: v1x
         type(ZMM8c8),   intent(in) :: v1y
         type(ZMM8c8),   intent(in) :: v1z
         type(ZMM8c8),   intent(in) :: v2x
         type(ZMM8c8),   intent(in) :: v2y
         type(ZMM8c8),   intent(in) :: v2z
         
         !complex(kind=dp) :: res
         type(ZMM8c8) :: res
         type(ZMM8c8), automatic :: tx,ty,tz
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: res
         !DIR$ ATTRIBUTES ALIGN : 64 :: tx
         !DIR$ ATTRIBUTES ALIGN : 64 :: ty
         !DIR$ ATTRIBUTES ALIGN : 64 :: tz
#endif
         !t0 = v1*v2
         !res = complex(sum(t0.re),sum(t0.im))
         tx = v1x*v2x
         ty = v1y*v2y
         tz = v1z*v2z
         !tx.re(0)+ty.re(0)+tz.re(0)
         res.re = tx.re+ty.re+tz.re
         res.im = tx.im+ty.im+tz.im
     end function zdotv_zmm8c8


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine  scrossc_zmm16c4(vc1x,vc1y,vc1z, &
                                 vc2x,vc2y,vc2z, &
                                 resx,resy,resz)  !GCC$ ATTRIBUTES aligned(32) :: scrossc_zmm16c4 !GCC$ ATTRIBUTES inline :: scrossc_zmm16c4 !GCC$ ATTRIBUTES vectorcall :: scrossc_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine scrossc_zmm16c4(vc1x,vc1y,vc1z, &
                                 vc2x,vc2y,vc2z,&
                                 resx,resy,resz) 
        !DIR$ ATTRIBUTES INLINE :: scrossc_zmm16c4
        !DIR$ ATTRIBUTES VECTOR :: scrossc_zmm16c4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: scrossc_zmm16c4
#endif
         type(ZMM16c4), intent(in)  :: vc1x
         type(ZMM16c4), intent(in)  :: vc1y
         type(ZMM16c4), intent(in)  :: vc1z
         type(ZMM16c4), intent(in)  :: vc2x
         type(ZMM16c4), intent(in)  :: vc2y
         type(ZMM16c4), intent(in)  :: vc2z
         type(ZMM16c4), intent(out) :: resx
         type(ZMM16c4), intent(out) :: resy
         type(ZMM16c4), intent(out) :: resz
       
         !Exec code ...
         !resx = vc1(2)*vc2(3)-vc1(3)*vc2(2)
         !resy = vc1(3)*vc2(1)-vc1(1)*vc2(3)
         !resz = vc1(1)*vc2(2)-vc1(2)*vc2(1)
         resx = vc1y*vc2z-vc1z*vc2y  
         resy = vc1z*vc2x-vc1x*vc2z
         resz = vc1x*vc2y-vc1y*vc2x
     end subroutine  scrossc_zmm16c4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine  dcrossc_zmm8c8(vc1x,vc1y,vc1z, &
                                vc2x,vc2y,vc2z, &
                                resx,resy,resz)  !GCC$ ATTRIBUTES aligned(32) :: dcrossc_zmm8c8 !GCC$ ATTRIBUTES inline :: dcrossc_zmm8c8 !GCC$ ATTRIBUTES vectorcall :: dcrossc_zmm8c8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine dcrossc_zmm8c8(vc1x,vc1y,vc1z, &
                               vc2x,vc2y,vc2z, &
                               resx,resy,resz) 
        !DIR$ ATTRIBUTES INLINE :: dcrossc_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: dcrossc_zmm8c8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dcrossc_zmm8c8
#endif
         type(ZMM8c8), intent(in)  :: vc1x
         type(ZMM8c8), intent(in)  :: vc1y
         type(ZMM8c8), intent(in)  :: vc1z
         type(ZMM8c8), intent(in)  :: vc2x
         type(ZMM8c8), intent(in)  :: vc2y
         type(ZMM8c8), intent(in)  :: vc2z
         type(ZMM8c8), intent(out) :: resx
         type(ZMM8c8), intent(out) :: resy
         type(ZMM8c8), intent(out) :: resz
         !Exec code ...
         resx = vc1y*vc2z-vc1z*vc2y  
         resy = vc1z*vc2x-vc1x*vc2z
         resz = vc1x*vc2y-vc1y*vc2x
     end subroutine  dcrossc_zmm8c8       
       
       
       
       


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function ddotv_zmm8r8(v1x,v1y,v1z, &
                                v2x,v2y,v2z) result(res) !GCC$ ATTRIBUTES aligned(32) :: ddotv_zmm8r8 !GCC$ ATTRIBUTES inline :: ddotv_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: ddotv_zmm8r8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function ddotv_zmm8r8(v1,v2) result(res)
        !DIR$ ATTRIBUTES INLINE :: ddotv_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: ddotv_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ddotv_zmm8r8
#endif
         
         type(ZMM8r8_t),  intent(in) :: v1x
         type(ZMM8r8_t),  intent(in) :: v1y
         type(ZMM8r8_t),  intent(in) :: v1z
         type(ZMM8r8_t),  intent(in) :: v2x
         type(ZMM8r8_t),  intent(in) :: v2y
         type(ZMM8r8_t),  intent(in) :: v2z
         
         type(ZMM8r8_t) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: res
#endif
         ! Locals
         type(ZMM8r8_t), automatic :: t0,t1,t2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
#endif
         !t0.v  = v1(1).v*v2(1).v !x0...x7
         !t1.v  = v1(2).v*v2(2).v !y0...y7
         !t2.v  = v1(3).v*v2(3).v !z0...z7
         t0.v  = v1x.v*v2x.v
         t1.v  = v1y.v*v2y.v
         t2.v  = v1z.v*v2z.v
         res.v = t0.v+t1.v+t2.v
      end function ddotv_zmm8r8

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      subroutine scrossv_zmm16r4(v1x,v1y,v1z,&
                                 v2x,v2y,v2z, &
                                 vcx,vcy,vcz) !GCC$ ATTRIBUTES aligned(32) :: scrossv_zmm16r4 !GCC$ ATTRIBUTES inline :: scrossv_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: scrossv_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine scrossv_zmm16r4(v1x,v1y,v1z,&
                                 v2x,v2y,v2z,&
                                 vcx,vcy,vcz)
        !DIR$ ATTRIBUTES INLINE :: scrossv_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: scrossv_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: scrossv_zmm16r4
#endif
        type(ZMM16r4_t),  intent(in)  :: v1x
        type(ZMM16r4_t),  intent(in)  :: v1y
        type(ZMM16r4_t),  intent(in)  :: v1z
        type(ZMM16r4_t),  intent(in)  :: v2x
        type(ZMM16r4_t),  intent(in)  :: v2y
        type(ZMM16r4_t),  intent(in)  :: v2z
        type(ZMM16r4_t),  intent(out) :: vcx
        type(ZMM16r4_t),  intent(out) :: vcy
        type(ZMM16r4_t),  intent(out) :: vcz
        
        ! Exec code ....
        !vcx.v = v1(2).v*v2(3).v-v1(3).v*v2(2).v
        !vcy.v = v1(3).v*v2(1).v-v1(1).v*v2(3).v
        !vcz.v = v1(1).v*v2(2).v-v1(2).v*v2(1).v
        vcx.v = v1y.v*v2z.v-v1x.v-v2y.v
        vcy.v = v1z.v*v2x.v-v1x.v*v2z.v
        vcz.v = v1x.v*v2y.v-v1y.v*v2x.v
      end subroutine scrossv_zmm16r4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      subroutine dcrossv_zmm8r8(v1x,v1y,v1z,&
                                v2x,v2y,v2z,&
                                vcx,vcy,vcz) !GCC$ ATTRIBUTES aligned(32) :: dcrossv_zmm8r8 !GCC$ ATTRIBUTES inline :: dcrossv_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: dcrossv_zmm8r8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine dcrossv_zmm8r8(v1x,v1y,v1z,&
                                v2x,v2y,v2z,&
                                vcx,vcy,vcz)
        !DIR$ ATTRIBUTES INLINE :: dcrossv_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: dcrossv_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dcrossv_zmm8r8
#endif
        type(ZMM8r8_t),  intent(in)  :: v1x
        type(ZMM8r8_t),  intent(in)  :: v1y
        type(ZMM8r8_t),  intent(in)  :: v1z
        type(ZMM8r8_t),  intent(in)  :: v2x
        type(ZMM8r8_t),  intent(in)  :: v2y
        type(ZMM8r8_t),  intent(in)  :: v2z
        type(ZMM8r8_t),  intent(out) :: vcx
        type(ZMM8r8_t),  intent(out) :: vcy
        type(ZMM8r8_t),  intent(out) :: vcz
           
        ! Exec code ....
        vcx.v = v1y.v*v2z.v-v1x.v-v2y.v
        vcy.v = v1z.v*v2x.v-v1x.v*v2z.v
        vcz.v = v1x.v*v2y.v-v1y.v*v2x.v
    end subroutine dcrossv_zmm8r8

      ! Direction Vector spherical [theta,phi] (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))   
    subroutine dir_vector_zmm16r4(theta,phi,dvx,dvy,dvz) !GCC$ ATTRIBUTES aligned(32) :: dir_vector_zmm16r !GCC$ ATTRIBUTES inline :: dir_vector_zmm16r !GCC$ ATTRIBUTES vectorcall :: dir_vector_zmm16r
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine dir_vector_zmm16r4(theta,phi,dvx,dvy,dvz)
        !DIR$ ATTRIBUTES INLINE :: dir_vector_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: dir_vector_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vector_zmm16r4
#endif
         type(ZMM16r4_t),               intent(in)  :: theta
         type(ZMM16r4_t),               intent(in)  :: phi
         type(ZMM16r4_t),               intent(out) :: dvx
         type(ZMM16r4_t),               intent(out) :: dvy
         type(ZMM16r4_t),               intent(out) :: dvz

         ! Locals
         type(ZMM16r4_t), automatic :: sth
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: sth
#endif         
         ! Exec code ...
         sth.v = sin(theta.v)
         dvx.v = sth.v*cos(phi.v)
         dvy.v = sth.v*sin(phi.v)
         dvz.v = cos(theta.v)
     end subroutine dir_vector_zmm16r4


      ! Direction Vector spherical [theta,phi] (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))   
    subroutine dir_vector_zmm8r8(theta,phi,dvx,dvy,dvz) !GCC$ ATTRIBUTES aligned(32) :: dir_vector_zmm8r8 !GCC$ ATTRIBUTES inline :: dir_vector_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: dir_vector_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine dir_vector_zmm8r8(theta,phi,dvx,dvy,dvz)
        !DIR$ ATTRIBUTES INLINE :: dir_vector_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: dir_vector_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vector_zmm8r8
#endif
         type(ZMM8r8_t),               intent(in)  :: theta
         type(ZMM8r8_t),               intent(in)  :: phi
         type(ZMM8r8_t),               intent(out) :: dvx
         type(ZMM8r8_t),               intent(out) :: dvy
         type(ZMM8r8_t),               intent(out) :: dvz
   
         ! Locals
         type(ZMM8r8_t), automatic :: sth
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: sth
#endif
         ! Exec code ...
         sth.v = sin(theta.v)
         dvx.v = sth.v*cos(phi.v)
         dvy.v = sth.v*sin(phi.v)
         dvz.v = cos(theta.v)
    end subroutine dir_vector_zmm8r8     
      
    ! Polarization Vector of plane-wave propagating into direction computed by
    ! dir_vector_xmmxrx (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine pol_vector_zmm16r4(theta,phi,psi,pvx,pvy,pvz) !GCC$ ATTRIBUTES aligned(32) :: pol_vector_zmm16r4 !GCC$ ATTRIBUTES inline :: pol_vector_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: pol_vector_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine pol_vector_zmm16r4(theta,phi,psi,pvx,pvy,pvz)
        !DIR$ ATTRIBUTES INLINE :: pol_vector_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: pol_vector_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vector_zmm16r4
#endif
        type(ZMM16r4_t),                  intent(in)  :: theta
        type(ZMM16r4_t),                  intent(in)  :: phi
        type(ZMM16r4_t),                  intent(in)  :: psi
        type(ZMM16r4_t),                  intent(out) :: pvx
        type(ZMM16r4_t),                  intent(out) :: pvy
        type(ZMM16r4_t),                  intent(out) :: pvz
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED pv : 64
#endif
        ! Locals
        type(ZMM16r4_t), automatic :: cpsi
        type(ZMM16r4_t), automatic :: cphi
        type(ZMM16r4_t), automatic :: spsi
        type(ZMM16r4_t), automatic :: sphi
        type(ZMM16r4_t), automatic :: t0
       
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: cpsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: cphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: spsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: sphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
      
        
#endif
        ! Exec code ...
        cpsi.v  = cos(psi.v)
        cphi.v  = cos(phi.v)
        spsi.v  = sin(psi.v)
        sphi.v  = sin(phi.v)
        t0.v    = spsi.v*cos(theta.v)
        pvx.v = cpsi.v*sphi.v-t0.v*cphi.v
        pvy.v = -cpsi.v*cphi-t0.v*sphi.v
        pvz.v = spsi.v*sin(theta.v)
    end subroutine pol_vector_zmm16r4
    

  ! Polarization Vector of plane-wave propagating into direction computed by
    ! dir_vector_xmmxrx (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
  subroutine pol_vector_zmm8r8(theta,phi,psi,pvx,pvy,pvz) !GCC$ ATTRIBUTES aligned(32) :: pol_vector_zmm8r8 !GCC$ ATTRIBUTES inline :: pol_vector_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: pol_vector_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
  subroutine pol_vector_zmm8r8(theta,phi,psi,pvx,pvy,pvz)
        !DIR$ ATTRIBUTES INLINE :: pol_vector_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: pol_vector_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vector_zmm8r8
#endif
        type(ZMM8r8_t),                  intent(in)  :: theta
        type(ZMM8r8_t),                  intent(in)  :: phi
        type(ZMM8r8_t),                  intent(in)  :: psi
        type(ZMM8r8_t),                  intent(out) :: pvx
        type(ZMM8r8_t),                  intent(out) :: pvy
        type(ZMM8r8_t),                  intent(out) :: pvz

        ! Locals
        type(ZMM8r8_t), automatic :: cpsi
        type(ZMM8r8_t), automatic :: cphi
        type(ZMM8r8_t), automatic :: spsi
        type(ZMM8r8_t), automatic :: sphi
        type(ZMM8r8_t), automatic :: t0
       
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: cpsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: cphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: spsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: sphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
       
        
#endif
        ! Exec code ...
        cpsi.v  = cos(psi.v)
        cphi.v  = cos(phi.v)
        spsi.v  = sin(psi.v)
        sphi.v  = sin(phi.v)
        t0.v    = spsi.v*cos(theta.v)
        pvx.v = cpsi.v*sphi.v-t0.v*cphi.v
        pvy.v = -cpsi.v*cphi-t0.v*sphi.v
        pvz.v = spsi.v*sin(theta.v)
    end subroutine pol_vector_zmm8r8
    
          
        
       
          
       

       ! Vectorized Electric-field at 16 points 'R'
     ! vpol -- vector of vertical polarization at point 'R'
     ! vdir -- direction vector
     ! vr   -- vector radius r
     ! Exyz -- resulting electrical field (3D) at sixteen points 'R', i.e. R(xyz), x0-x15,y0-y15,z0-z15
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine H_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                              vdirx,vdiry,vdirz, &
                              vrx,vry,vrz,k,     &
                              H_x,H_y,H_z) !GCC$ ATTRIBUTES aligned(32) :: H_XYZ_VP_zmm16c4 !GCC$ ATTRIBUTES inline :: H_XYZ_VP_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine H_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                               vdirx,vdiry,vdirz, &
                               vrx,vry,vrz,k,     &
                               H_x,H_y,H_z )
         !DIR$ ATTRIBUTES INLINE :: H_XYZ_VP_zmm16c4
         !DIR$ ATTRIBUTES VECTOR :: H_XYZ_VP_zmm16c4
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_XYZ_VP_zmm16c4
#endif
        type(ZMM16r4_t),       intent(in)  :: vpolx
        type(ZMM16r4_t),       intent(in)  :: vpoly
        type(ZMM16r4_t),       intent(in)  :: vpolz
        type(ZMM16r4_t),       intent(in)  :: vdirx
        type(ZMM16r4_t),       intent(in)  :: vdiry
        type(ZMM16r4_t),       intent(in)  :: vdirz
        type(ZMM16r4_t),       intent(in)  :: vrx
        type(ZMM16r4_t),       intent(in)  :: vry
        type(ZMM16r4_t),       intent(in)  :: vrz
        type(ZMM16c4),         intent(in)  :: k
        type(ZMM16c4),         intent(out) :: H_x
        type(ZMM16c4),         intent(out) :: H_y
        type(ZMM16c4),         intent(out) :: H_z
        

        type(ZMM16c4),   automatic :: carg
        type(ZMM16r4_t), automatic :: dp
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: dp
        !DIR$ ATTRIBUTES ALIGN : 64 :: carg
#endif
        ! Exec code ...
        dp.v    = sdotv_zmm16r4(vdirx,cdiry,vdirz, &
                                vrx,vry,vrz)
        carg    = cexp_c16(jc4*k*dp)
        H_x     = vpolx.v*carg
        H_y     = vpoly.v*carg
        H_z     = vpolz.v*carg
     end subroutine H_XYZ_VP_zmm16c4


    ! Vectorized (SIMD data-types)  Electric-field at 8 points 'R'
     ! vpol -- vector of vertical polarization at point 'R'
     ! vdir -- direction vector
     ! vr   -- vector radius r
     ! Exyz -- resulting electrical field (3D) at sixteen points 'R', i.e. R(xyz), x0-x7,y0-y7,z0-z7
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine H_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz, &
                              vdirx,vdiry,vdirz, &
                              vrx,vry,vrz,k,     &
                              H_x,H_y,H_z) !GCC$ ATTRIBUTES aligned(32) :: H_XYZ_VP_zmm8c8 !GCC$ ATTRIBUTES inline :: H_XYZ_VP_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine H_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz, &
                              vdirx,vdiry,vdirz, &
                              vrx,vry,vrz,k,     &
                              H_x,H_y,H_z)
         !DIR$ ATTRIBUTES INLINE :: H_XYZ_VP_zmm8c8
         !DIR$ ATTRIBUTES VECTOR :: H_XYZ_VP_zmm8c8
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: H_XYZ_VP_zmm8c8
#endif
        type(ZMM8r8_t),       intent(in)  :: vpolx
        type(ZMM8r8_t),       intent(in)  :: vpoly
        type(ZMM8r8_t),       intent(in)  :: vpolz
        type(ZMM8r8_t),       intent(in)  :: vdirx
        type(ZMM8r8_t),       intent(in)  :: vdiry
        type(ZMM8r8_t),       intent(in)  :: vdirz
        type(ZMM8r8_t),       intent(in)  :: vrx
        type(ZMM8r8_t),       intent(in)  :: vry
        type(ZMM8r8_t),       intent(in)  :: vrz
        type(ZMM8c8),         intent(in)  :: k
        type(ZMM8c8),         intent(out) :: H_x
        type(ZMM8c8),         intent(out) :: H_y
        type(ZMM8c8),         intent(out) :: H_z
        type(ZMM8c8),   automatic :: carg
        type(ZMM8r8_t), automatic :: dp
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: dp
        !DIR$ ATTRIBUTES ALIGN : 64 :: carg
#endif
        ! Exec code ...
        dp.v    = ddotv_zmm8r8(vdirx,vdiry,vdirz, &
                               vrx,vry,vrz)
        carg    = cexp_c8(jc8*k*dp)
        H_x     = vpolx.v*carg
        H_y     = vpoly.v*carg
        H_z     = vpolz.v*carg
     end subroutine H_XYZ_VP_zmm8c8


     ! Magnetic Field (SIMD data-types) [plane-wave], polarization 'vpol' of
     !  wave-vector argument:  vdir*k at sixteen points 'r'.
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine B_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                                 vdirx,vdiry,vdirz, &
                                 k,omega,           &
                                 vrx,vry,vrz,       &
                                 B_x,B_y,B_z) !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_VP_zmm16c4 !GCC$ ATTRIBUTES inline :: B_XYZ_VP_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                                 vdirx,vdiry,vdirz, &
                                 k,omega,           &
                                 vrx,vry,vrz,       &
                                 B_x,B_y,B_z)
         !DIR$ ATTRIBUTES INLINE :: B_XYZ_VP_zmm16c4
         !DIR$ ATTRIBUTES VECTOR :: B_XYZ_VP_zmm16c4
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_VP_zmm16c4
#endif
         type(ZMM16r4_t),   intent(in)  :: vpolx
         type(ZMM16r4_t),   intent(in)  :: vpoly
         type(ZMM16r4_t),   intent(in)  :: vpolz
         type(ZMM16r4_t),   intent(in)  :: vdirx
         type(ZMM16r4_t),   intent(in)  :: vdiry
         type(ZMM16r4_t),   intent(in)  :: vdirz
         type(ZMM16c4),     intent(in)  :: k
         type(ZMM16r4_t),   intent(in)  :: omega
         type(ZMM16r4_t),   intent(in)  :: vrx
         type(ZMM16r4_t),   intent(in)  :: vry
         type(ZMM16r4_t),   intent(in)  :: vrz
         type(ZMM16c4),     intent(out) :: B_x
         type(ZMM16c4),     intent(out) :: B_y
         type(ZMM16c4),     intent(out) :: B_z
         ! Locals
         !type(ZMM16c4), automatic, dimension(3) :: cdir
         !type(ZMM16c4), automatic, dimension(3) :: Exyz
         !type(ZMM16c4), automatic, dimension(3) :: cp
         type(ZMM16c4), automatic :: cdirx
         type(ZMM16c4), automatic :: cdiry
         type(ZMM16c4), automatic :: cdirz
         type(ZMM16c4), automatic :: H_x
         type(ZMM16c4), automatic :: H_y
         type(ZMM16c4), automatic :: H_z
         type(ZMM16c4), automatic :: cpx
         type(ZMM16c4), automatic :: cpy
         type(ZMM16c4), automatic :: cpz
         type(ZMM16c4), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdirx
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdiry
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdirz
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpx
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpy
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpz
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         call H_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                               vdirx,vdiry,vdirz,  &
                               vrx,vry,vrz,k,      &
                               H_x,H_y,H_z)
         cdirx = zmm16r41x_init(vdirx)
         cdiry = zmm16r41x_init(vdiry)
         cdirz = zmm16r41x_init(vdirz)
         t0 = k/(omega.v*mu0r16.v)
         call scrossc_zmm16c4(cdirx,cdiry,cdirz, &
                              H_x,H_y,H_z,       &
                              cpx,cpy,cpz)
         B_x = t0*cpx
         B_y = t0*cpy
         B_z = t0*cpz
       end subroutine B_XYZ_VP_zmm16c4

     ! Magnetic Field (SIMD data-types) [plane-wave], polarization 'vpol' of
     !  wave-vector argument:  vdir*k at eight points 'r'.
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine B_XYZ_VP_zmm8c8( vpolx,vpoly,vpolz, &
                                 vdirx,vdiry,vdirz, &
                                 k,omega,           &
                                 vrx,vry,vrz,       &
                                 B_x,B_y,B_z) !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_VP_zmm8c8 !GCC$ ATTRIBUTES inline :: B_XYZ_VP_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_VP_zmm8c8( vpolx,vpoly,vpolz, &
                                 vdirx,vdiry,vdirz, &
                                 k,omega,           &
                                 vrx,vry,vrz,       &
                                 B_x,B_y,B_z)
         !DIR$ ATTRIBUTES INLINE :: B_XYZ_VP_zmm8c8
         !DIR$ ATTRIBUTES VECTOR :: B_XYZ_VP_zmm8c8
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_VP_zmm8c8
#endif
         type(ZMM8r8_t),   intent(in)  :: vpolx
         type(ZMM8r8_t),   intent(in)  :: vpoly
         type(ZMM8r8_t),   intent(in)  :: vpolz
         type(ZMM8r8_t),   intent(in)  :: vdirx
         type(ZMM8r8_t),   intent(in)  :: vdiry
         type(ZMM8r8_t),   intent(in)  :: vdirz
         type(ZMM8c8),     intent(in)  :: k
         type(ZMM8r8_t),   intent(in)  :: omega
         type(ZMM8r8_t),   intent(in)  :: vrx
         type(ZMM8r8_t),   intent(in)  :: vry
         type(ZMM8r8_t),   intent(in)  :: vrz
         type(ZMM8c8),     intent(out) :: B_x
         type(ZMM8c8),     intent(out) :: B_y
         type(ZMM8c8),     intent(out) :: B_z
         type(ZMM8c8), automatic :: cdirx
         type(ZMM8c8), automatic :: cdiry
         type(ZMM8c8), automatic :: cdirz
         type(ZMM8c8), automatic :: H_x
         type(ZMM8c8), automatic :: H_y
         type(ZMM8c8), automatic :: H_z
         type(ZMM8c8), automatic :: cpx
         type(ZMM8c8), automatic :: cpy
         type(ZMM8c8), automatic :: cpz
         type(ZMM8c8), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdirx
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdiry
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdirz
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
         !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpx
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpy
         !DIR$ ATTRIBUTES ALIGN : 64 :: cpz
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         call H_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz, &
                              vdirx,vdiry,vdirz, &
                              vrx,vry,vrz,       &
                              k,H_x,H_y,H_z)
         cdirx = zmm8r81x_init(vdirx)
         cdiry = zmm8r81x_init(vdiry)
         cdirz = zmm8841x_init(vdirz)
         t0 = k/(omega.v*mu0r16.v)
         call dcrossc_zmm8c8(cdirx,cdiry,cdirz, &
                             H_x,H_y,H_z,       &
                             cpx,cpy,cpz)
         B_x = t0*cpx
         B_y = t0*cpy
         B_z = t0*cpz
      end subroutine B_XYZ_VP_zmm8c8


       
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      subroutine B_XYZ_H_XYZ_P_zmm16c4(theta,phi,psi,omega,r, &
                                       px,py,pz,              &
                                       H_x,H_y,H_z,           &
                                       B_x,B_y,B_z) !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_H_XYZ_P_zmm16c4 !GCC$ ATTRIBUTES inline :: B_XYZ_H_XYZ_P_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_H_XYZ_P_zmm16c4(theta,phi,psi,omega,r, &
                                       px,py,pz,              &
                                       H_x,H_y,H_z,           &
                                       B_x,B_y,B_z)
         !DIR$ ATTRIBUTES INLINE :: B_XYZ_H_XYZ_P_zmm16c4
         !DIR$ ATTRIBUTES VECTOR :: B_XYZ_H_XYZ_P_zmm16c4
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_H_XYZ_P_zmm16c4
#endif
         type(ZMM16r4_t),               intent(in)    :: theta
         type(ZMM16r4_t),               intent(in)    :: phi
         type(ZMM16r4_t),               intent(in)    :: psi
         type(ZMM16r4_t),               intent(in)    :: omega
         type(ZMM16c4),                 intent(in)    :: r
         type(ZMM16r4_t),               intent(in)    :: px
         type(ZMM16r4_t),               intent(in)    :: py
         type(ZMM16r4_t),               intent(in)    :: pz
         type(ZMM16c4),                 intent(inout) :: H_x
         type(ZMM16c4),                 intent(inout) :: H_y
         type(ZMM16c4),                 intent(inout) :: H_z
         type(ZMM16c4),                 intent(inout) :: B_x
         type(ZMM16c4),                 intent(inout) :: B_y
         type(ZMM16c4),                 intent(inout) :: B_z
         
         ! Locals
         type(ZMM16r4_t), automatic :: vpolx
         type(ZMM16r4_t), automatic :: vpoly
         type(ZMM16r4_t), automatic :: vpolz
         type(ZMM16r4_t), automatic :: vdirx
         type(ZMM16r4_t), automatic :: vdiry
         type(ZMM16r4_t), automatic :: vdirz
         type(ZMM16c4),   automatic :: k
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpolx
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpoly
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpolz
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdirx
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdiry
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdirz
         !DIR$ ATTRIBUTES ALIGN : 64 :: k
#endif
         
         call dir_vector_zmm16r4(theta,phi, &
                                 vdirx,vdiry,vdirz)
         k = r*omega.v/cr4.v
         call pol_vector_zmm16r4(theta,phi,psi, &
                                 vpolx,vpoly,vpolz)
         call H_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz,    &
                               vdirx,vdiry,vdirz,k,  &
                               px,py,pz,H_x,H_y,H_z)
         call B_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz,    &
                               vdirx,vdiry,vdirz,    &
                               k,omega,px,py,pz      &
                               B_x,B_y,B_z)
    end subroutine B_XYZ_H_XYZ_P_zmm16c4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine B_XYZ_H_XYZ_P_zmm8c8(theta,phi,psi,omega,r, &
                                       px,py,pz,              &
                                       H_x,H_y,H_z,           &
                                       B_x,B_y,B_z) !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_H_XYZ_P_zmm8c8 !GCC$ ATTRIBUTES inline :: B_XYZ_H_XYZ_P_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_H_XYZ_P_zmm8c8(theta,phi,psi,omega,r, &
                                       px,py,pz,              &
                                       H_x,H_y,H_z,           &
                                       B_x,B_y,B_z)
         !DIR$ ATTRIBUTES INLINE :: B_XYZ_H_XYZ_P_zmm8c8
         !DIR$ ATTRIBUTES VECTOR :: B_XYZ_H_XYZ_P_zmm8c8
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_H_XYZ_P_zmm8c8
#endif
         type(ZMM8r8_t),               intent(in)    :: theta
         type(ZMM8r8_t),               intent(in)    :: phi
         type(ZMM8r8_t),               intent(in)    :: psi
         type(ZMM8r8_t),               intent(in)    :: omega
         type(ZMM8c8),                 intent(in)    :: r
         type(ZMM8r8_t),               intent(in)    :: px
         type(ZMM8r8_t),               intent(in)    :: py
         type(ZMM8r8_t),               intent(in)    :: pz
         type(ZMM8c8),                 intent(inout) :: H_x
         type(ZMM8c8),                 intent(inout) :: H_y
         type(ZMM8c8),                 intent(inout) :: H_z
         type(ZMM8c8),                 intent(inout) :: B_x
         type(ZMM8c8),                 intent(inout) :: B_y
         type(ZMM8c8),                 intent(inout) :: B_z
         
         ! Locals
         type(ZMM8r8_t), automatic :: vpolx
         type(ZMM8r8_t), automatic :: vpoly
         type(ZMM8r8_t), automatic :: vpolz
         type(ZMM8r8_t), automatic :: vdirx
         type(ZMM8r8_t), automatic :: vdiry
         type(ZMM8r8_t), automatic :: vdirz
         type(ZMM8c8),   automatic :: k
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpolx
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpoly
         !DIR$ ATTRIBUTES ALIGN : 64 :: vpolz
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdirx
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdiry
         !DIR$ ATTRIBUTES ALIGN : 64 :: vdirz
         !DIR$ ATTRIBUTES ALIGN : 64 :: k
#endif
         
         call dir_vector_zmm8r8(theta,phi, &
                                 vdirx,vdiry,vdirz)
         k = r*omega.v/cr4.v
         call pol_vector_zmm8r8(theta,phi,psi, &
                                 vpolx,vpoly,vpolz)
         call H_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz,     &
                               vdirx,vdiry,vdirz,k,  &
                               px,py,pz,H_x,H_y,H_z)
         call B_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz,     &
                               vdirx,vdiry,vdirz,    &
                               k,omega,px,py,pz      &
                               B_x,B_y,B_z)
     end subroutine B_XYZ_H_XYZ_P_zmm8c8    
       
      
     ! Electric and Magnetic Fields elliptically polarized
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine B_XYZ_H_XYZ_EP_zmm16c4(theta,phi,omega,phase,refi, &
                                       px,py,pz,H_x,H_y,H_z,       &
                                       B_x,B_y,B_z)  !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_H_XYZ_EP_zmm16c4 !GCC$ ATTRIBUTES inline :: B_XYZ_H_XYZ_EP_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_H_XYZ_EP_zmm16c4(theta,phi,omega,phase,refi, &
                                       px,py,pz,H_x,H_y,H_z,       &
                                       B_x,B_y,B_z)
              !DIR$ ATTRIBUTES INLINE :: B_XYZ_H_XYZ_EP_zmm16c4
              !DIR$ ATTRIBUTES VECTOR :: B_XYZ_H_XYZ_EP_zmm16c4
              !DIR$ OPTIMIZE : 3
              !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_H_XYZ_EP_zmm16c4
#endif
              type(ZMM16r4_t),        intent(in)    :: theta
              type(ZMM16r4_t),        intent(in)    :: phi
              type(ZMM16r4_t),        intent(in)    :: omega
              type(ZMM16c4),          intent(in)    :: phase
              type(ZMM16c4),          intent(in)    :: refi
              type(ZMM16c4),          intent(in)    :: px
              type(ZMM16c4),          intent(in)    :: py
              type(ZMM16c4),          intent(in)    :: pz
              type(ZMM16c4),          intent(inout) :: H_x
              type(ZMM16c4),          intent(inout) :: H_y
              type(ZMM16c4),          intent(inout) :: H_z
              type(ZMM16c4),          intent(inout) :: B_x
              type(ZMM16c4),          intent(inout) :: B_y
              type(ZMM16c4),          intent(inout) :: B_z
              ! Locals
              type(ZMM16r4_t), parameter :: psi_0 = ZMM16r4_t(0.0_sp)
              type(ZMM16c4),   automatic :: H_x_1
              type(ZMM16c4),   automatic :: H_y_1
              type(ZMM16c4),   automatic :: H_z_1
              type(ZMM16c4),   automatic :: H_x_2
              type(ZMM16c4),   automatic :: H_y_2
              type(ZMM16c4),   automatic :: H_z_2
              type(ZMM16c4),   automatic :: k
              type(ZMM16c4),   automatic :: t0
              type(ZMM16c4),   automatic :: cdirx
              type(ZMM16c4),   automatic :: cdiry
              type(ZMM16c4),   automatic :: cdirz
              type(ZMM16r4_t), automatic :: vpolx
              type(ZMM16r4_t), automatic :: vpoly
              type(ZMM16r4_t), automatic :: vpolz
              type(ZMM16r4_t), automatic :: vdirx
              type(ZMM16r4_t), automatic :: vdiry
              type(ZMM16r4_t), automatic :: vdirz
              type(ZMM16r4_t), automatic :: cn
#if defined(__INTEL_COMPILER) || defined(__ICC)
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_x_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_y_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_z_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_x_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_y_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_z_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: k
              !DIR$ ATTRIBUTES ALIGN : 64 :: t0
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpolx
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpoly
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpolz
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdirx
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdiry
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdirz
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdirx
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdiry
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdirz
              !DIR$ ATTRIBUTES ALIGN : 64 :: cn
#endif
              ! Exec code ...
              
              call dir_vector_zmm16r4(theta,phi, &
                                      vdirx,vdiry,vdirz)
              cdirx = zmm16r41x_init(vdirx)
              k = refi.v*omega.v/cr4
              cdiry = zmm16r41x(vdiry)
              call pol_vector_zmm16r4(theta,phi,psi_0, &
                   vpolx,vpoly,vpolz)
              cdirz = zmm16r41x(vdirz)
              call H_XYZ_VP_zmm16c4(vpolx,vpoly,vpolz, &
                                    vdirx,vdiry,vdirz, &
                                    px,py,pz,k,        &
                                    H_x_1,H_y_1,H_z_1)
              call scrossc_zmm16c4(cdirx,cdiry,cdirz, &
                                   H_x_1,H_y_1,H_z_1, &
                                   H_x_2,H_y_2,H_z_2)
              H_x = H_x_1+phase*H_x_2
              H_y = H_y_1+phase*H_y_2
              H_z = H_z_1+phase*H_z_2
              cn  = cnorm_zmm16c4(H_x,H_y,H_z)
              H_x = H_x/cn
              H_y = H_y/cn
              H_z = H_z/cn
              t0  = k/(omega.v*mu0r16.v)
              call scrossc_zmm16c4(cdirx,cdiry,cdirz, &
                                   H_x,H_y,H_z,       &
                                   H_x_2,H_y_2,H_z_2)
              B_x = t0*H_x_2
              B_y = t0*H_y_2
              B_z = t0*H_z_2
      end subroutine B_XYZ_H_XYZ_EP_zmm16c4
      

      ! Electric and Magnetic Fields elliptically polarized
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine B_XYZ_H_XYZ_EP_zmm8c8(theta,phi,omega,phase,refi, &
                                       px,py,pz,H_x,H_y,H_z,       &
                                       B_x,B_y,B_z)  !GCC$ ATTRIBUTES aligned(32) :: B_XYZ_H_XYZ_EP_zmm8c8 !GCC$ ATTRIBUTES inline :: B_XYZ_H_XYZ_EP_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine B_XYZ_H_XYZ_EP_zmm8c8(theta,phi,omega,phase,refi, &
                                       px,py,pz,H_x,H_y,H_z,       &
                                       B_x,B_y,B_z)
              !DIR$ ATTRIBUTES INLINE :: B_XYZ_H_XYZ_EP_zmm8c8
              !DIR$ ATTRIBUTES VECTOR :: B_XYZ_H_XYZ_EP_zmm8c8
              !DIR$ OPTIMIZE : 3
              !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: B_XYZ_H_XYZ_EP_zmm8c8
#endif
              type(ZMM8r8_t),        intent(in)    :: theta
              type(ZMM8r8_t),        intent(in)    :: phi
              type(ZMM8r8_t),        intent(in)    :: omega
              type(ZMM8c8),          intent(in)    :: phase
              type(ZMM8c8),          intent(in)    :: refi
              type(ZMM8c8),          intent(in)    :: px
              type(ZMM8c8),          intent(in)    :: py
              type(ZMM8c8),          intent(in)    :: pz
              type(ZMM8c8),          intent(inout) :: H_x
              type(ZMM8c8),          intent(inout) :: H_y
              type(ZMM8c8),          intent(inout) :: H_z
              type(ZMM8c8),          intent(inout) :: B_x
              type(ZMM8c8),          intent(inout) :: B_y
              type(ZMM8c8),          intent(inout) :: B_z
              ! Locals
              type(ZMM8r8_t), parameter :: psi_0 = ZMM8r8_t(0.0_dp)
              type(ZMM18c8),   automatic :: H_x_1
              type(ZMM8c8),   automatic :: H_y_1
              type(ZMM8c8),   automatic :: H_z_1
              type(ZMM8c8),   automatic :: H_x_2
              type(ZMM8c8),   automatic :: H_y_2
              type(ZMM8c8),   automatic :: H_z_2
              type(ZMM8c8),   automatic :: k
              type(ZMM8c8),   automatic :: t0
              type(ZMM8c8),   automatic :: cdirx
              type(ZMM8c8),   automatic :: cdiry
              type(ZMM8c8),   automatic :: cdirz
              type(ZMM8r8_t), automatic :: vpolx
              type(ZMM8r8_t), automatic :: vpoly
              type(ZMM8r8_t), automatic :: vpolz
              type(ZMM8r8_t), automatic :: vdirx
              type(ZMM8r8_t), automatic :: vdiry
              type(ZMM8r8_t), automatic :: vdirz
              type(ZMM8r8_t), automatic :: cn
#if defined(__INTEL_COMPILER) || defined(__ICC)
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_x_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_y_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_z_1
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_x_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_y_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: H_z_2
              !DIR$ ATTRIBUTES ALIGN : 64 :: k
              !DIR$ ATTRIBUTES ALIGN : 64 :: t0
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpolx
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpoly
              !DIR$ ATTRIBUTES ALIGN : 64 :: vpolz
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdirx
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdiry
              !DIR$ ATTRIBUTES ALIGN : 64 :: vdirz
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdirx
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdiry
              !DIR$ ATTRIBUTES ALIGN : 64 :: cdirz
              !DIR$ ATTRIBUTES ALIGN : 64 :: cn
#endif
              ! Exec code ...
              
              call dir_vector_zmm8r8(theta,phi, &
                                      vdirx,vdiry,vdirz)
              cdirx = zmm8r81x_init(vdirx)
              k = refi.v*omega.v/cr4
              cdiry = zmm8r81x(vdiry)
              call pol_vector_zmm8r8(theta,phi,psi_0, &
                   vpolx,vpoly,vpolz)
              cdirz = zmm8r81x(vdirz)
              call H_XYZ_VP_zmm8c8(vpolx,vpoly,vpolz, &
                                    vdirx,vdiry,vdirz, &
                                    px,py,pz,k,        &
                                    H_x_1,H_y_1,H_z_1)
              call scrossc_zmm8c8(cdirx,cdiry,cdirz, &
                                   H_x_1,H_y_1,H_z_1, &
                                   H_x_2,H_y_2,H_z_2)
              H_x = H_x_1+phase*H_x_2
              H_y = H_y_1+phase*H_y_2
              H_z = H_z_1+phase*H_z_2
              cn  = cnorm_zmm8c8(H_x,H_y,H_z)
              H_x = H_x/cn
              H_y = H_y/cn
              H_z = H_z/cn
              t0  = k/(omega.v*mu0r8.v)
              call scrossc_zmm8c8(cdirx,cdiry,cdirz, &
                                   H_x,H_y,H_z,       &
                                   H_x_2,H_y_2,H_z_2)
              B_x = t0*H_x_2
              B_y = t0*H_y_2
              B_z = t0*H_z_2
      end subroutine B_XYZ_H_XYZ_EP_zmm8c8
      
        
       
            

      
     



















end module em_fields_simd
