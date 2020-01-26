
#include "GMS_config.fpp"


module mod_imminitrin


!===================================================================================85
!---------------------------- DESCRIPTION ------------------------------------------85
!
!
!
!          Module  name:
!                         'mod_immintrin'
!          
!          Purpose:
!                         This is Fortran 90 port of Intel 'immintrin_dbg'
!                         source code.
!          History:
!                        
!                         
!          Version:
!
!                      Major: 1
!                      Minor: 0
!                      Micro: 0
!
!          Author:
!
!              Copyright (c) 2019, <alexander.komarov@intel.com>
!              All rights reserved.
!
!                 Redistribution and use in source and binary forms, with or without
!                 modification, are permitted provided that the following conditions are met:
!
!                  * Redistributions of source code must retain the above copyright notice, this
!                    list of conditions and the following disclaimer.
!
!                  * Redistributions in binary form must reproduce the above copyright notice,
!                    this list of conditions and the following disclaimer in the documentation
!                    and/or other materials provided with the distribution.
!
!                  * Neither the name of the copyright holder nor the names of its
!                    contributors may be used to endorse or promote products derived from
!                    this software without specific prior written permission.
!
!                  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!                  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!                  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!                  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!                  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!                  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!                  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!                  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!                  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!                  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!                  
!          Ported to Fortran 90 by Bernard Gingold on 26-01-2020 04:39 PM +00200
!          Intrinsics guide version: 3.5.4
!          Parser version: 0.25
!         
!          
!         
!          E-mail:
!                  
!                      beniekg@gmail.com
!=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int1, int4, int8b, sp, dp
     implicit none
     public

     private :: Convert_FP64_To_FP32, &
                

     character(*),   parameter :: MOD_IMMINITRIN_VERSION_ID = &
          "$Id: GMS_mod_immintrin.f90 1000 2020-01-26 16:42 +00200 beniekg@gmail.com $"

      
   contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER     
     function Convert_FP64_To_FP32(control) result(ctrl) !GCC$ ATTRIBUTES cold :: Convert_FP64_To_FP32 !GCC$ ATTRIBUTES inline :: Convert_FP64_To_FP32
#elif defined __ICC || defined __INTEL_COMPILER
     function Convert_FP64_To_FP32(control) result(ctrl)
       !DIR$ ATTRIBUTES INLINE :: Convert_FP64_To_FP32
#endif
          real(kind=dp),     intent(in) :: control
          !Locals
          real(kind=sp) :: ctrl
          ctrl = real(control,kind=sp)
      end function Convert_FP64_To_FP32
   
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      function Convert_FP32_To_FP64(control) result(ctrl) !GCC$ ATTRIBUTES cold :: Convert_FP32_To_FP64 !GCC$ ATTRIBUTES inline :: Convert_FP64_to_FP32
#if defined __ICC || defined __INTEL_COMPILER
      function Convert_FP32_To_FP64(control) result(ctrl)
        !DIR$ ATTRIBUTES INLINE :: Convert_FP32_To_FP64
#endif
          real(kind=sp),     intent(in) :: control
          ! Locals
          real(kind=dp) :: ctrl
          ctrl = real(control,kind=dp)
      end function Convert_FP32_To_FP64

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      function Convert_FP32_To_UnsignedInt32(control) result(ctrl) !GCC$ ATTRIBUTES cold :: Convert_FP32_To_UnsignedInt32 !GCC$ ATTRIBUTES inline :: Convert_FP32_To_UnsignedInt32
#elif defined __ICC || defined __INTEL_COMPILER
      function Convert_FP32_To_UnsignedInt32(control) result(ctrl)
        !DIR$ ATTRIBUTES INLINE :: Convert_FP32_To_UnsignedInt32
#endif
           real(kind=sp),     intent(in) :: control
           ! LOcals
           integer(kind=int4) :: ctrl
           ctrl = int(control,kind=int4)
       end function Convert_FP32_To_UnsignedInt32

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
       function Convert_FP64_To_Int32_Truncate(control) result(ctrl) !GCC$ ATTRIBUTES cold :: Convert_FP64_To_Int32_Truncate !GCC$ ATTRIBUTES inline :: Convert_FP64_To_Int32_Truncate
#elif defined __ICC || defined __INTEL_COMPILER
       function Convert_FP64_To_Int32_Truncate(control) result(ctrl)
            !DIR$ ATTRIBUTES INLINE :: Convert_FP64_To_Int32_Truncate
#endif          
           real(kind=dp),   intent(in) :: control
           ! Locals
           integer(kind=int4) :: ctrl
           ctrl = int(control,kind=int4)
        end function Convert_FP64_To_Int32_Truncate

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
        function APPROXIMATE(control) result(ctrl) !GCC$ ATTRIBUTES cold :: APPROXIMATE !GCC$ ATTRIBUTES inline :: APPROXIMATE 
#elif defined __ICC || defined __INTEL_COMPILER
        function APPROXIMATE(control) result(ctrl)
             !DIR$ ATTRIBUTES INLINE :: APPROXIMATE
#endif          
           real(kind=dp),  intent(in) :: control
           ! Locals
           real(kind=sp) :: ctrl
           ctrl = control
         end function APPROXIMATE

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
         function Saturate_To_UnsignedInt16(control) result(val) !GCC$ ATTRIBUTES cold :: Saturate_To_UnsignedInt16 !GCC$ ATTRIBUTES inline :: Saturate_To_UnsignedInt16
#elif defined __ICC || defined __INTEL_COMPILER
         function Saturate_To_UnsignedInt16(control) result(val)
           !DIR$ ATTRIBUTES INLINE :: Saturate_To_UnsignedInt16
#endif
              integer(kind=int4),   intent(in) :: control
              ! Locals
              integer(kind=int4) :: val
              if(control > 65535) val = 65535
              val = control
         end function Saturate_To_UnsignedInt16
         
end module mod_immintrin
