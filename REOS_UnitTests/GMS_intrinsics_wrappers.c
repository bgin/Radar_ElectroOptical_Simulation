
#include <stdint.h>
#include <immintrin.h>



unsigned  __int64 rdtsc_wrap();


unsigned __int64 rdtsc_wrap()
{
         return __rdtsc();
}
