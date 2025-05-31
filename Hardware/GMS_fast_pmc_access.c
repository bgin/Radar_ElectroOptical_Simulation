
#include "GMS_fast_pmc_access.h"

unsigned long rdtsc() {
    unsigned long a,d;
    __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));
    return (a | (d << 32));
}

unsigned long rdtscp() {
    unsigned long a,d,c;
    __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
    return (a | (d << 32));
}

unsigned long full_rdtscp(int * chip, int * core) {
    unsigned long a,d,c;
    __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
	*chip = (c & 0xFFF000UL)>>12;
	*core = c & 0xFFFUL;
    return (a | (d << 32));
}

int get_core_number() {
     unsigned long a,d,c;
    __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
    return ( c & 0xFFFUL );  
}

int get_socket_number() {
     unsigned long a,d,c;
    __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
    return ( (c & 0xF000UL)>>12 );
}

unsigned long rdpmc_instructions() {
    unsigned long a,d,c;
    c = (1UL<<30);
   __asm__ volatile("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
   return (a | (d << 32));
}

unsigned long rdpmc_actual_cycles() {
    unsigned long a,d,c;
    c = (1UL<<30)+2;
    __asm__ volatile("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
    return (a | (d << 32));
}

unsigned long rdpmc_reference_cycles() {
    unsigned long a,d,c;
    c = (1UL<<30)+2;
    __asm__ volatile("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
    return (a | (d << 32));
}

unsigned long rdpmc(int c) {
    unsigned long a,d;
    __asm__ volatile("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
    return (a | (d << 32));
}


// core performance counter width varies by processor
// the width is contained in bits 23:16 of the EAX register
// after executing the CPUID instruction with an initial EAX
// argument of 0x0a (subleaf 0x0 in ECX).
int get_core_counter_width() {
    unsigned int eax,ebx,ecx,edx;
    unsigned int leaf,subleaf;
    leaf = 0x0000000a;
	subleaf = 0x0;
	__asm__ __volatile__ ("cpuid" : \
	  "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (leaf), "c" (subleaf));
	return((eax & 0x00ff0000) >> 16);
}

// fixed-function performance counter width varies by processor
// the width is contained in bits 12:5 of the EDX register
// after executing the CPUID instruction with an initial EAX
// argument of 0x0a (subleaf 0x0 in ECX).
int get_fixed_counter_width() {
    unsigned int eax,ebx,ecx,edx;
    unsigned int leaf,subleaf;
    leaf = 0x0000000a;
	subleaf = 0x0;
    __asm__ __volatile__ ("cpuid" : \
	  "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (leaf), "c" (subleaf));
	return((edx & 0x00001fe0) >> 5);
}

#include <stdio.h>
#include <stdlib.h>

// Utility routine to compute counter differences taking into account rollover
// when the performance counter width is not known at compile time.  
// Use the "get_counter_width()" function to get the counter width on the
// current system, then use that as the third argument to this function.
// 64-bit counters don't generally roll over, but I added a special case
// for this

unsigned long corrected_pmc_delta(unsigned long end,
                                  unsigned long start,
				  int pmc_width) {
      unsigned long error_return=0xffffffffffffffff;
      unsigned long result;
	// sanity checks
	if ((pmc_width <= 0) || (pmc_width > 64)) {
		fprintf(stderr,"ERROR: corrected_pmc_delta() called with illegal performance counter width %d\n",pmc_width);
		return(error_return);
	}
	// Due to the specifics of unsigned arithmetic, for pmc_width == sizeof(unsigned long),
	// the simple calculation (end-start) gives the correct delta even if the counter has
	// rolled (leaving end < start).
	if (pmc_width == 64) {
		return (end - start);
	} else {
		// for pmc_width < sizeof(unsigned long), rollover must be detected and corrected explicitly
		if (end >= start) {
			result = end - start;
		} else {
			// I think this works independent of ordering, but this makes the most intuitive sense
			result = (end + (1UL<<pmc_width)) - start;
		}
		return (result);
	}
}

// Ugly, ugly, ugly hack to get nominal frequency from CPUID Brand String
// on Intel processors.
// Converted from C++ to C.
// Only works for products that use "GHz" as the frequency designator,
// not "MHz" or "THz".  So far this works on all processors tested.
// Return value is frequency in Hz, so user will need to divide by 1e9
// if GHz is desired....
float get_TSC_frequency() {
        unsigned int eax, ebx, ecx, edx;
	unsigned int leaf, subleaf;
	unsigned int  intbuf[12];
	char *buffer;
	int i,j,k,base,start,stop,length;
	float freq_GHz;
	float frequency;

	subleaf=0;

	base = 0;
	for (leaf=0x80000002; leaf<0x80000005; leaf++) {
		// printf("DEBUG: leaf = %x\n",leaf);
		__asm__ __volatile__ ("cpuid" : \
		  "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (leaf), "c" (subleaf));

		// printf("leaf = %x, eax = %8.8x, ebx = %8.8x, ecx = %8.8x, edx = %8.8x\n",leaf, eax, ebx, ecx, edx);
		intbuf[base] = eax;
		intbuf[base+1] = ebx;
		intbuf[base+2] = ecx;
		intbuf[base+3] = edx;
		base += 4;
		// printf("  DEBUG: %.8s %.8s %.8s %.8s\n",eax,ebx,ecx,edx);
	}
	// for (base=0; base<12; base++) {
	// 	printf("base[%d] = %8.8x\n",base,intbuf[base]);
	// }
	// printf("444444443333333333222222222211111111110000000000\n");
	// printf("765432109876543210987654321098765432109876543210\n");
	// printf("%48.48s\n",(char *)&intbuf[0]);
	buffer = (char *) &intbuf[0];
	// for (base=0; base<48; base++) {
	// 	printf("%c",buffer[base]);
	// }
	// printf("\n");
	// printf("000000000011111111112222222222333333333344444444\n");
	// printf("012345678901234567890123456789012345678901234567\n");
	// printf("\n");
	// printf("\n");
	// printf("Scanning backwards to try to find the frequency digits....\n");
	for (base=47; base>0; base--){
		if (buffer[base] == 0x7a) {
			// printf("Found z at location %d\n",base);
			if (buffer[base-1] == 0x48) {
				// printf("Found H at location %d\n",base-1);
				if (buffer[base-2] == 0x47) {
					// printf("Found G at location %d\n",base-2);
					// printf(" -- need to extract string now\n");
					i = base-3;
					stop = base-3;
					// printf("begin reverse search at stop character location %d\n",i);
					while(buffer[i] != 0x20) {
						// printf("found a non-blank character %c (%x) at location %d\n",buffer[i],buffer[i],i);
						i--;
					}
					start = i+1;
					length = stop - start + 1;
					k = length+1;
					// for (j=stop; j<start; j--) {
						// printf("DEBUG: buffer[%d] = %c\n",j,buffer[j]);
						// k--;
					// }
					// printf("DEBUG: starting position of frequency string is %d\n",start);
					//
					// note that sscanf will automatically stop when the string changes from digits
					// to non-digits, so I don't need to NULL-terminate the string in the buffer.
					//
					sscanf((char *)&buffer[start],"%f",&freq_GHz);
					// printf("Frequency is %f GHz\n",freq_GHz);
					frequency = 1.0e9*freq_GHz;
					return (frequency);
				}
			}
		}
	}
	return(-1.0f);
}

unsigned long long approximate_cpuid_bias()
{
	 unsigned long long lat_cpuid_start;
	 unsigned long long lat_cpuid_end;
	 unsigned long long cpuid_bias;
	 unsigned int cyc1_lo, cyc1_hi;
	 unsigned int cyc2_lo, cyc2_hi;
	 unsigned int eax,ebx,ecx,edx;
	 
     // Dry run.
	  __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc1_hi),"=r" (cyc1_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
     __asm__ __volatile__ ("cpuid" : \
	              "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) );
     __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc2_hi),"=r" (cyc2_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );

	  //The main measurement run.

	   __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc1_hi),"=r" (cyc1_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
     __asm__ __volatile__ ("cpuid" : \
	              "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) );
     __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc2_hi),"=r" (cyc2_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
	  
	  lat_cpuid_start = (((unsigned long long)cyc1_hi << 32) | cyc1_lo);
	  lat_cpuid_end   = (((unsigned long long)cyc2_hi << 32) | cyc2_lo);
	  cpuid_bias      = lat_cpuid_end-lat_cpuid_start;
	  return cpuid_bias;
}

unsigned long long approximate_rdpmc_bias()
{
	unsigned long long lat_rdpmc_start;
	unsigned long long lat_rdpmc_end;
	unsigned long long rdpmc_bias;
	unsigned int a,d,c;
	unsigned int cyc3_hi, cyc3_lo;
	unsigned int cyc4_hi, cyc4_lo;

	c = (1ull<<30)+1;
    __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc3_hi),"=r" (cyc3_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
     __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c) );
     __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc4_hi),"=r" (cyc4_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );

	 __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc3_hi),"=r" (cyc3_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
     __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c) );
     __asm__ __volatile__ (
                 "CPUID\n\t"
                 "RDTSC\n\t"
                 "mov %%edx, %0\n\t"
                 "mov %%eax, %1\n\t" : "=r" (cyc4_hi),"=r" (cyc4_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
	 
	 lat_rdpmc_start = (((unsigned long long)cyc3_hi << 32) | cyc3_lo);
	 lat_rdpmc_end   = (((unsigned long long)cyc4_hi << 32) | cyc4_lo);
     rdpmc_bias      = lat_rdpmc_end-lat_rdpmc_start;
	 return rdpmc_bias;
}

void approx_cpuid_bias_samples(unsigned long long * __restrict__ cpuid_bias_start,
                               unsigned long long * __restrict__ cpuid_bias_end,
							   unsigned long long * __restrict__ cpuid_bias_delta,
							   const unsigned long long n_runs,
							   const unsigned long long n_samples)
{
	 
	   //Dry run
	   __attribute__((unused))
	   volatile unsigned long long result;
	   result = approximate_cpuid_bias();
       for(unsigned long long __i = 0ull; __i != n_runs; ++__i)
	   {   
		   	unsigned int cyc1_lo, cyc1_hi;
	        unsigned int cyc2_lo, cyc2_hi;
	        unsigned int eax,ebx,ecx,edx;

		   for(unsigned long long __j = 0ull; __j != n_samples; ++__j)
		   {
			     __asm__ __volatile__ ("lfence");
			     __asm__ __volatile__ (
                              "CPUID\n\t"
                              "RDTSC\n\t"
                              "mov %%edx, %0\n\t"
                              "mov %%eax, %1\n\t" : "=r" (cyc1_hi),"=r" (cyc1_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
                 __asm__ __volatile__ ("cpuid" : \
	                                   "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) );
                 __asm__ __volatile__ (
                              "CPUID\n\t"
                              "RDTSC\n\t"
                              "mov %%edx, %0\n\t"
                              "mov %%eax, %1\n\t" : "=r" (cyc2_hi),"=r" (cyc2_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                 );
				 __asm__ __volatile__ ("lfence");
                 unsigned long long start = (((unsigned long long)cyc1_hi << 32) | cyc1_lo);
				 cpuid_bias_start[__i*n_samples+__j] = start;
				 unsigned long long end   = (((unsigned long long)cyc2_hi << 32) | cyc2_lo);
				 cpuid_bias_end[__i*n_samples+__j]   = end;
				 cpuid_bias_delta[__i*n_samples+__j] = end-start;
		   }
	   }

}

void approx_rdpmc_bias_samples(unsigned long long * __restrict__ rdpmc_bias_start,
                               unsigned long long * __restrict__ rdpmc_bias_end,
							   unsigned long long * __restrict__ rdpmc_bias_delta,
							   const unsigned long long n_runs,
							   const unsigned long long n_samples)
{
	    // Dry run 
		__attribute__((unused))
		volatile unsigned long long result;
		result = approximate_rdpmc_bias();
		for(unsigned long long __i = 0ull; __i != n_runs; ++__i)
		{
			  unsigned int a,d,c;
	          unsigned int cyc3_hi, cyc3_lo;
	          unsigned int cyc4_hi, cyc4_lo;
			  c = (1ull<<30)+1;

			  for(unsigned long long __j = 0ull; __j != n_samples; ++__j)
			  {
				   __asm__ __volatile__ ("lfence");
				   __asm__ __volatile__ (
                                         "CPUID\n\t"
                                         "RDTSC\n\t"
                                         "mov %%edx, %0\n\t"
                                         "mov %%eax, %1\n\t" : "=r" (cyc3_hi),"=r" (cyc3_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                            );
                   __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c) );
                   __asm__ __volatile__ (
                                         "CPUID\n\t"
                                         "RDTSC\n\t"
                                         "mov %%edx, %0\n\t"
                                         "mov %%eax, %1\n\t" : "=r" (cyc4_hi),"=r" (cyc4_lo) :: "%rax", "%rbx", "%rcx", "%rdx" 
                            );
				    __asm__ __volatile__ ("lfence");
					unsigned long long start = (((unsigned long long)cyc3_hi << 32) | cyc3_lo);
					rdpmc_bias_start[__i*n_samples+__j] = start;
					unsigned long long end   = (((unsigned long long)cyc4_hi << 32) | cyc4_lo);
					rdpmc_bias_end[__i*n_samples+__j]   = end;
					rdpmc_bias_delta[__i*n_samples+__j] = end-start;
			  }
		}
}
