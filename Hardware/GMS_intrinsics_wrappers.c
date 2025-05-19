
#include <stdint.h>
#include <immintrin.h>



unsigned  __int64 rdtsc_wrap();


unsigned __int64 rdtsc_wrap()
{
         return __rdtsc();
}

void  mm_clflush_wrap(void const *);

void mm_clflush_wrap(void const * p)
{
         _mm_clflush(p);
}

void  mm_clflushopt_wrap(void const *);

void mm_clflushopt_wrap(void const * p)
{
          _mm_clflushopt(p);
}
/*
void  clui_wrap();

void  clui_wrap()
{
      _clui();
}
*/
void  mm_clwb_wrap(void const *);

void  mm_clwb_wrap(void const * p)
{
      _mm_clwb(p);
}

unsigned int MM_GET_EXCEPTION_MASK_WRAP();

unsigned int MM_GET_EXCEPTION_MASK_WRAP()
{
    return _MM_GET_EXCEPTION_MASK();
}

unsigned int MM_GET_EXCEPTION_STATE_WRAP();

unsigned int MM_GET_EXCEPTION_STATE_WRAP()
{
     return _MM_GET_EXCEPTION_STATE();
}

unsigned int MM_GET_FLUSH_ZERO_MODE_WRAP();

unsigned int MM_GET_FLUSH_ZERO_MODE_WRAP()
{
      return _MM_GET_FLUSH_ZERO_MODE();
}

unsigned int MM_GET_ROUNDING_MODE_WRAP();

unsigned int MM_GET_ROUNDING_MODE_WRAP()
{
      return _MM_GET_ROUNDING_MODE();
}

unsigned int mm_getcsr_wrap();

unsigned int mm_getcsr_wrap()
{
      return _mm_getcsr();
}
/*
void  hreset_wrap(int);

void hreset_wrap(int eax)
{
     _hreset(eax);
}
*/
void  mm_lfence_wrap();

void  mm_lfence_wrap()
{
      _mm_lfence();
}

int may_i_use_cpu_feature_wrap(unsigned __int64);

int may_i_use_cpu_feature_wrap(unsigned __int64 a)
{
     switch(a)
     {
        case _FEATURE_GENERIC_IA32 : 
        return _may_i_use_cpu_feature(_FEATURE_GENERIC_IA32);
        case _FEATURE_FPU       : 
        return _may_i_use_cpu_feature(_FEATURE_FPU);
        case _FEATURE_CMOV      : 
        return _may_i_use_cpu_feature(_FEATURE_CMOV);
        case _FEATURE_FXSAVE    : 
        return _may_i_use_cpu_feature(_FEATURE_FXSAVE);
        case _FEATURE_SSE       : 
        return _may_i_use_cpu_feature(_FEATURE_SSE);   
        case _FEATURE_SSE2      :
        return _may_i_use_cpu_feature(_FEATURE_SSE2);
        case _FEATURE_SSE3      :
        return _may_i_use_cpu_feature(_FEATURE_SSE3);
        case _FEATURE_SSSE3     :
        return _may_i_use_cpu_feature(_FEATURE_SSSE3);
        case _FEATURE_SSE4_1    :
        return _may_i_use_cpu_feature(_FEATURE_SSE4_1);
        case _FEATURE_SSE4_2    :
        return _may_i_use_cpu_feature(_FEATURE_SSE4_2);
        case _FEATURE_MOVBE     :
        return _may_i_use_cpu_feature(_FEATURE_MOVBE);
        case _FEATURE_POPCNT    :
        return _may_i_use_cpu_feature(_FEATURE_POPCNT);
        case _FEATURE_PCLMULQDQ : 
        return _may_i_use_cpu_feature(_FEATURE_PCLMULQDQ);
        case _FEATURE_AES       :
        return _may_i_use_cpu_feature(_FEATURE_AES);
        case _FEATURE_F16C      :
        return _may_i_use_cpu_feature(_FEATURE_F16C);
        case _FEATURE_AVX       :
        return _may_i_use_cpu_feature(_FEATURE_AVX);
        case _FEATURE_RDRND     :
        return _may_i_use_cpu_feature(_FEATURE_RDRND);
        case _FEATURE_FMA       :
        return _may_i_use_cpu_feature(_FEATURE_FMA);
        case _FEATURE_BMI       :
        return _may_i_use_cpu_feature(_FEATURE_BMI);
        case _FEATURE_LZCNT     :
        return _may_i_use_cpu_feature(_FEATURE_LZCNT);
        case _FEATURE_HLE       :
        return _may_i_use_cpu_feature(_FEATURE_HLE);
        case _FEATURE_RTM       : 
        return _may_i_use_cpu_feature(_FEATURE_RTM);
        case _FEATURE_AVX2      :
        return _may_i_use_cpu_feature(_FEATURE_AVX2);
        case _FEATURE_AVX512F   : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512F);
        case _FEATURE_ADX       : 
        return _may_i_use_cpu_feature(_FEATURE_ADX);
        case _FEATURE_RDSEED    : 
        return _may_i_use_cpu_feature(_FEATURE_RDSEED);
        case _FEATURE_AVX512ER  :
        return _may_i_use_cpu_feature(_FEATURE_AVX512ER);
        case _FEATURE_AVX512PF  : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512PF);
        case _FEATURE_AVX512CD  : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512CD);
        case _FEATURE_SHA       :
        return _may_i_use_cpu_feature(_FEATURE_SHA);
        case _FEATURE_MPX       :
        return _may_i_use_cpu_feature(_FEATURE_MPX);
        case _FEATURE_AVX512BW  :
        return _may_i_use_cpu_feature(_FEATURE_AVX512BW);
        case _FEATURE_AVX512VL  :
        return _may_i_use_cpu_feature(_FEATURE_AVX512VL);
        case _FEATURE_AVX512VBMI:
        return _may_i_use_cpu_feature(_FEATURE_AVX512VBMI);
        case _FEATURE_AVX512_4FMAPS :
        return _may_i_use_cpu_feature(_FEATURE_AVX512_4FMAPS);
        case _FEATURE_AVX512_4VNNIW :
        return _may_i_use_cpu_feature(_FEATURE_AVX512_4VNNIW);
        case _FEATURE_AVX512_VPOPCNTDQ : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512_VPOPCNTDQ);
        case _FEATURE_AVX512_BITALG : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512_BITALG);
        case _FEATURE_AVX512_VBMI2 : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512_VBMI2);
        case _FEATURE_GFNI : 
        return _may_i_use_cpu_feature(_FEATURE_GFNI);
        case _FEATURE_VAES : 
        return _may_i_use_cpu_feature(_FEATURE_VAES);
        case _FEATURE_VPCLMULQDQ : 
        return _may_i_use_cpu_feature(_FEATURE_VPCLMULQDQ);
        case _FEATURE_AVX512_VNNI : 
        return _may_i_use_cpu_feature(_FEATURE_AVX512_VNNI);
        case _FEATURE_CLWB : 
        return _may_i_use_cpu_feature(_FEATURE_CLWB);
        case _FEATURE_RDPID : 
        return _may_i_use_cpu_feature(_FEATURE_RDPID);
        case _FEATURE_IBT : 
        return _may_i_use_cpu_feature(_FEATURE_IBT);
        case _FEATURE_SHSTK : 
        return _may_i_use_cpu_feature(_FEATURE_SHSTK);
        case _FEATURE_SGX : 
        return _may_i_use_cpu_feature(_FEATURE_SGX);
        case _FEATURE_WBNOINVD : 
        return _may_i_use_cpu_feature(_FEATURE_WBNOINVD);
        case _FEATURE_PCONFIG : 
        return _may_i_use_cpu_feature(_FEATURE_PCONFIG);
        default : 
        return -1;
     }

}

void  mm_fence_wrap();

void  mm_fence_wrap()
{
      _mm_mfence();
}

void  mm_sfence_wrap();

void  mm_sfence_wrap()
{
      _mm_sfence();
}

void  mm_monitor_wrap(void const *,
                      unsigned int,
                      unsigned int);

void  mm_monitor_wrap(void const * p,
                      unsigned int extensions,
                      unsigned int hints)
{
      _mm_monitor(p,extensions,hints);
}
/*
void  mm_wait_wrap(unsigned int,
                   unsigned int);

void  mm_wait_wrap(unsigned int extensions,
                   unsigned int hints)
{
      _mm_wait(extensions,hints);
}
*/
void  mm_pause_wrap();

void  mm_pause_wrap()
{
      _mm_pause();
}
/*
void _mm_prefetch (char const* p, int i)
#include <immintrin.h>
Instruction: prefetchwt1 m8
             prefetchw m8
             prefetchnta m8
             prefetcht0 m8
             prefetcht1 m8
             prefetcht2 m8
CPUID Flags: PREFETCHWT1
*/
void  mm_prefetch_NTA(char const *);

void  mm_prefetch_NTA(char const * p)
{
      _mm_prefetch(p,_MM_HINT_NTA);
}

void  mm_prefetch_T2(char const *);

void  mm_prefetch_T2(char const * p)
{
      _mm_prefetch(p,_MM_HINT_T2);
}

void  mm_prefetch_T1(char const *);

void  mm_prefetch_T1(char const * p)
{
      _mm_prefetch(p,_MM_HINT_T1);
}

void  mm_prefetch_T0(char const *);

void  mm_prefetch_T0(char const * p)
{
      _mm_prefetch(p,_MM_HINT_T0);
}
/*
void  mm_prefetch_ET1(char const *);

void  mm_prefetch_ET1(char const * p)
{
      _mm_prefetch(p,_MM_HINT_ET1);
}
*/
void  mm_prefetch_ET0(char const *);

void  mm_prefetch_ET0(char const * p)
{
      _mm_prefetch(p,_MM_HINT_ET0);
}
      
/*
void  mm_prefetchit0_wrap(const void *);

void  mm_prefetchit0_wrap(const void * p)
{
      _mm_prefetchit0(p);
}

void  mm_prefetchit1_wrap(const void *);

void  mm_prefetchit1_wrap(const void * p)
{
      _mm_prefetchit1(p);
}
*/
unsigned int rdpid_u32_wrap();

unsigned int rdpid_u32_wrap()
{
     return _rdpid_u32();
}

__int64 rdpmc_wrap(int );

__int64 rdpmc_wrap(int a)
{
        return _rdpmc(a);
}

unsigned __int64 rdtscp_wrap(unsigned int *);

unsigned __int64 rdtscp_wrap(unsigned int * mem)
{
      return __rdtscp(mem);
}

unsigned int readfsbase_u32_wrap();

unsigned int readfsbase_u32_wrap()
{
     return _readfsbase_u32();
}

unsigned __int64 readfsbase_u64_wrap();

unsigned __int64 readfsbase_u64_wrap()
{
      return _readfsbase_u64();
}

unsigned int readgsbase_u32_wrap();

unsigned int readfgsbase_u32_wrap()
{
     return _readgsbase_u32();
}

unsigned __int64 readgsbase_u64_wrap();

unsigned __int64 readgsbase_u64_wrap()
{
      return _readgsbase_u64();
}
/*
void  senduipi_wrap(unsigned __int64);

void  senduipi_wrap(unsigned __int64 a)
{
      _senduipi(a);
}
*/
void  serialize_wrap();

void serialioze_wrap()
{
      _serialize();
}

void  MM_SET_EXCEPTION_MASK_WRAP(unsigned int);

void MM_SET_EXCEPTION_MASK_WRAP(unsigned int a)
{
      _MM_SET_EXCEPTION_MASK(a);
}

void MM_SET_EXCEPTION_STATE_WRAP(unsigned int);

void MM_SET_EXCEPTION_STATE_WRAP(unsigned int a)
{
      _MM_SET_EXCEPTION_STATE(a);
}

void MM_SET_FLUSH_ZERO_MODE_WRAP(unsigned int);

void MM_SET_FLUSH_ZERO_MODE_WRAP(unsigned int a)
{
     _MM_SET_FLUSH_ZERO_MODE(a);
}

void MM_SET_ROUNDING_MODE_WRAP(unsigned int);

void MM_SET_ROUNDING_MODE_WRAP(unsigned int a)
{
      _MM_SET_ROUNDING_MODE(a);
}
/*
void stui_wrap();

void stui_wrap()
{
     _stui();
}
*/
void  cldemote_wrap(void const *);

void  cldemote_wrap(void const * p)
{
     _mm_cldemote(p);
}

void  clrssbsy_wrap(void *);

void  clrssbsy_wrap(void * p)
{
      _clrssbsy(p);
}

__int32  get_ssp_wrap();

__int32  get_ssp_wrap()
{
       return _get_ssp();
}


__int64  get_ssp64_wrap();

__int64 get_ssp64_wrap()
{
       return _get_ssp();
}

void  umonitor_wrap(void *);

void  umonitor_wrap(void * p)
{
      _umonitor(p);
}

unsigned char umwait_wrap(unsigned int,
                          unsigned __int64);

unsigned char umwait_wrap(unsigned int ctrl,
                          unsigned __int64 counter)
{
      _umwait(ctrl,counter);
}

void  wbinvd_wrap();

void  wbinvd_wrap()
{
      _wbinvd();
}

void  wbnoinvd_wrap();

void  wbnoinvd_wrap()
{
      _wbnoinvd();
}

void  invpcid_wrap(unsigned int,
                   void *);

void  invpcid_wrap(unsigned int type,
                   void * descriptor)
{
      _invpcid(type,descriptor);
}

int   rdrand16_step_wrap(unsigned short *);

int   rdrand16_step_wrap(unsigned short * val)
{
      return _rdrand16_step(val);
}

int   rdrand32_step_wrap(unsigned int *);

int   rdrand32_step_wrap(unsigned int * val)
{
      return _rdrand32_step(val);
}

int   rdrand64_step_wrap(unsigned __int64 *);

int   rdrand64_step_wrap(unsigned __int64 * val)
{
      return _rdrand64_step(val);
}

int   rdseed16_step_wrap(unsigned short *);

int   rdseed16_step_wrap(unsigned short * val)
{
      return _rdseed16_step(val);
}

int   rdseed32_step_wrap(unsigned int *);

int   rdseed32_step_wrap(unsigned int * val)
{
      return _rdseed32_step(val);
}

int   rdseed64_step_wrap(unsigned __int64 *);

int   rdseed64_step_wrap(unsigned __int64 * val)
{
      return _rdseed64_step(val);
}





