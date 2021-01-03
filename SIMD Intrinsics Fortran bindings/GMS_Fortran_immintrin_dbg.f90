

module mod_Fortran_immintrin_dbg


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_Fortran_immintrin_dgb'
 !          
 !          Purpose:
  !                     An attempt to provide auto-vectorized
  !                     version of some of Intel SIMD Compiler
 !                      Intrinsics.
 !                       
 !          History:
 !                        Date: 03-01-2021
 !                        Time: 16:19 GMT+2
  !                       
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Copyright (c) 2019, <alexander.komarov@intel.com>
 !                      All rights reserved.
 !
 !                      Redistribution and use in source and binary forms, with or without
 !                      modification, are permitted provided that the following conditions are met:
 !
 !                         * Redistributions of source code must retain the above copyright notice, this
 !                           list of conditions and the following disclaimer.
 !
 !                         * Redistributions in binary form must reproduce the above copyright notice,
 !                           this list of conditions and the following disclaimer in the documentation
 !                           and/or other materials provided with the distribution.
 !
 !                         * Neither the name of the copyright holder nor the names of its
 !                           contributors may be used to endorse or promote products derived from
 !                           this software without specific prior written permission.
 !
 !
 !          Modified:
 !                   Bernard Gingold on 03-01-2021
 !                 
 !          References:
 !         
 !                Intrinsics guide version: 3.5.4 
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85


     use mod_kinds, only : i4,i8,sp,dp
     use mod_vectypes
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     use omp_lib
#endif

     public 
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter   :: MOD_FORTRAN_IMMINTRIN_DBG_MAJOR = 1_i4
    
    ! Minor version
    integer(kind=i4), parameter   :: MOD_FORTRAN_IMMINTRIN_DBG_MINOR = 0_i4
    
    ! Micro version
    integer(kind=i4), parameter   :: MOD_FORTRAN_IMMINTRIN_DBG_MICRO = 0_i4
    
    ! Module full version
    integer(kind=i4), parameter   :: MOD_FORTRAN_IMMINTRIN_DBG_FULLVER = 1000_i4*MOD_FORTRAN_IMMINTRIN_DBG_MAJOR+100_i4*MOD_FORTRAN_IMMINTRIN_DBG_MINOR+ &
                                             10_i4*MOD_FORTRAN_IMMINTRIN_DBG_MICRO
    
    ! Module creation date
    character(*),       parameter  :: MOD_FORTRAN_IMMINTRIN_DBG_CREATE_DATE = "03-01-2021 16:31 +00200 (SUN 03 JAN 2021 GMT+2)"
    
    ! Module build date
    character(*),       parameter  :: MOD_FORTRAN_IMMINTRIN_DBG_BUILD_DATE = __DATE__ ":" __TIME__
    
    ! Module author info
    character(*),       parameter  :: MOD_FORTRAN_IMMINTRIN_DBG_AUTHOR = "Copyright (c) 2019, <alexander.komarov@intel.com>. Ported to Fortran by Bernard Gingold, contact: beniekg@gmail.com"
    

  contains

    
#if defined __AVX512F__

    
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
subroutine _mm512_storeu_si512_dbg(Dst,Src) !GCC$ ATTRIBUTES hot :: _mm512_storeu_si512_dbg !GCC$ ATTRIBUTES inline :: _mm512_storeu_si512_dbg !GCC$ ATTRIBUTES aligned(32) :: _mm512_storeu_si512_dbg
#elif defined __ICC || defined __INTEL_COMPILER
subroutine _mm512_storeu_si512_dbg(Dst,Src)
  !DIR$ ATTRIBUTES INLINE :: _mm512_storeu_si512_dbg
  !DIR$ ATTRIBUTES ALIGN : 32 :: _mm512_storeu_si512_dbg
  !DIR$ ATTRIBUTES VECTOR :: _mm512_storeu_si512_dbg
#endif
  type(ZMM8i8_t),  intent(out) :: Dst
  type(ZMM8i8_t),  intent(in)  :: Src
  ! Exec code
  Dst.v = Src.v
end subroutine _mm512_storeu_si512_dbg



#endif











end module mod_Fortran_immintrin_dbg
