

module cpuid_x86


  !===================================================!
  !  Fortran wrappers to cpuid_x86.c implementation
  !===================================================!
  use, intrinsic :: ISO_C_BINDING
  implicit none
  public

#if defined(__i386__) || defined(__x86_64__) || defined(_M_IX86) || defined(_M_X64)
#define INTEL_AMD
#else
#error "Unsupported CPU architecture detected!!"
#endif
  
#define VENDOR_INTEL      1
#define VENDOR_AMD        3
#define FAMILY_80486  4
#define FAMILY_P5     5
#define FAMILY_P6     6
  
#ifdef INTEL_AMD
#define GET_EXFAMILY  1
#define GET_EXMODEL   2
#define GET_TYPE      3
#define GET_FAMILY    4
#define GET_MODEL     5
#define GET_APICID    6
#define GET_LCOUNT    7
#define GET_CHUNKS    8
#define GET_STEPPING  9
#define GET_BLANDID  10
#define GET_FEATURE  11
#define GET_NUMSHARE 12
#define GET_NUMCORES 13
#endif

#define CORE_UNKNOWN     0
#define CORE_80486       1
#define CORE_P5          2
#define CORE_P6          3
#define CORE_KATMAI      4
#define CORE_COPPERMINE  5
#define CORE_NORTHWOOD   6
#define CORE_PRESCOTT    7
#define CORE_BANIAS      8
#define CORE_ATHLON      9
#define CORE_OPTERON    10
#define CORE_BARCELONA  11
#define CORE_VIAC3      12
#define CORE_YONAH	13
#define CORE_CORE2	14
#define CORE_PENRYN	15
#define CORE_DUNNINGTON	16
#define CORE_NEHALEM	17
#define CORE_ATOM	18
#define CORE_NANO	19
#define CORE_SANDYBRIDGE 20
#define CORE_BOBCAT      21
#define CORE_BULLDOZER   22
#define CORE_PILEDRIVER  23
#define CORE_HASWELL     24
#define CORE_STEAMROLLER 25
#define CORE_EXCAVATOR   26
#define CORE_ZEN         27
#define CORE_SKYLAKEX    28
#define CORE_DHYANA	 29
#define CORE_COOPERLAKE  30
#define CORE_ZEN2        31
#define CORE_ZEN3        32
#define CORE_ICELAKE     33

#define CPUTYPE_UNKNOWN			 0
#define CPUTYPE_INTEL_UNKNOWN		 1

#define CPUTYPE_AMD_UNKNOWN              3
#define CPUTYPE_80386			11
#define CPUTYPE_80486			12
#define CPUTYPE_PENTIUM			13
#define CPUTYPE_PENTIUM2		14
#define CPUTYPE_PENTIUM3		15
#define CPUTYPE_PENTIUMM		16
#define CPUTYPE_PENTIUM4		17
#define CPUTYPE_CORE2			18
#define CPUTYPE_PENRYN			19
#define CPUTYPE_DUNNINGTON		20
#define CPUTYPE_NEHALEM			21
#define CPUTYPE_ATOM			22

#define CPUTYPE_AMD5X86			25
#define CPUTYPE_AMDK6			26
#define CPUTYPE_ATHLON			27
#define CPUTYPE_DURON			28
#define CPUTYPE_OPTERON			29
#define CPUTYPE_BARCELONA		30
#define CPUTYPE_SHANGHAI		31
#define CPUTYPE_ISTANBUL		32

#define CPUTYPE_SANDYBRIDGE             44
#define CPUTYPE_BOBCAT                  45
#define CPUTYPE_BULLDOZER               46
#define CPUTYPE_PILEDRIVER              47
#define CPUTYPE_HASWELL 		48
#define CPUTYPE_STEAMROLLER 		49
#define CPUTYPE_EXCAVATOR 		50
#define CPUTYPE_ZEN 			51
#define CPUTYPE_SKYLAKEX		52
#define CPUTYPE_COOPERLAKE		54
#define CPUTYPE_ZEN2                    55
#define CPUTYPE_ZEN3                    56
#define CPUTYPE_ICELAKE                 57

#define HAVE_SSE         lshift(1,0)
#define HAVE_SSE2        lshift(1,1) 
#define HAVE_SSE3        lshift(1,2) 
#define HAVE_SSSE3       lshift(1,3)
#define HAVE_SSE4_1      lshift(1,4)
#define HAVE_SSE4_2      lshift(1,5)
#define HAVE_SSE4A       lshift(1,6)
#define HAVE_SSE5        lshift(1,7)
#define HAVE_MMX         lshift(1,8)
#define HAVE_3DNOW       lshift(1,9)
#define HAVE_3DNOWEX     lshift(1,10)
#define HAVE_CMOV        lshift(1,11)
#define HAVE_PSE         lshift(1,12)
#define HAVE_CFLUSH      lshift(1,13)
#define HAVE_HIT         lshift(1,14)
#define HAVE_MISALIGNSSE lshift(1,15)
#define HAVE_128BITFPU   lshift(1,16)
#define HAVE_FASTMOVU    lshift(1,17)
#define HAVE_AVX         lshift(1,18)
#define HAVE_FMA4        lshift(1,19)
#define HAVE_FMA3        lshift(1,20)
#define HAVE_AVX512VL    lshift(1,21)
#define HAVE_AVX2        lshift(1,22) 
#define HAVE_AVX512BF16  lshift(1,23) 

  type, bind(c), public :: cache_info_t
     integer(c_int) :: size
     integer(c_int) :: associative
     integer(c_int) :: linesize
     integer(c_int) :: shared
  end type cache_info_t


  interface
     function support_avx() result(val) &
          bind(c,name='support_avx')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function support_avx
     
  end interface

  
  interface
     function support_avx2() result(val) &
          bind(c,name='support_avx2')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function support_avx2
     
  end interface


  interface
     function support_avx512() result(val) &
          bind(c,name='support_avx512')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function support_avx512
     
  end interface


  interface
     function support_avx512_bf16() result(val) &
          bind(c,name='support_avx512_bf16')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function support_avx512_bf16
     
  end interface


  interface
     function get_vendor() result(val) &
          bind(c,name='get_vendor')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function get_vendor
        
  end interface

  interface
     function get_cputype(gettype) result(val) &
          bind(c,name='get_cputype')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int),   intent(in), value :: gettype
          integer(c_int) :: val
     end function get_cputype
        
  end interface
  

  interface
     function get_cecheinfo(type,cacheinfo) result(val) &
          bind(c,name='getcacheinfo')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int),  intent(in), value :: type
          type(c_ptr),     intent(inout)     :: cacheinfo
          integer(c_int) :: val
        end function get_cecheinfo
        
  end interface


  interface
     function get_cpuname() result(val) &
          bind(c,name='get_cpuname')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function get_cpuname
  
  end interface


   interface
     function get_coretype() result(val) &
          bind(c,name='get_coretype')
          use, intrinsic :: ISO_C_BINDINGS
          integer(c_int) :: val
     end function get_coretype
  
  end interface

  
  interface
     subroutine get_cpuconfig()  &
          bind(c,name='get_cpuconfig')
          use, intrinsic :: ISO_C_BINDINGS
     end subroutine get_cpuconfig
  
  end interface


  interface
     subroutine get_cpuconfig()  &
          bind(c,name='get_cpuconfig')
          use, intrinsic :: ISO_C_BINDINGS
     end subroutine get_cpuconfig
  
  end interface


  interface
     subroutine get_sse()  &
          bind(c,name='get_sse')
          use, intrinsic :: ISO_C_BINDINGS
     end subroutine get_sse
  
  end interface


end module cpuid_x86
