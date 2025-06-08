


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
#include <unistd.h>
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

#define CPUID_SIGNATURE_HASWELL 0x000306f0U
#define CPUID_SIGNATURE_SKX 0x00050650U
#define CPUID_SIGNATURE_ICX 0x000606a0U
#define CPUID_SIGNATURE_SPR 0x000806f0U
// Based on J. McCalpin code 

unsigned int cpuid_signature() {
    int cpuid_return[4];

    __cpuid(&cpuid_return[0], 1);

    unsigned int  ModelInfo = cpuid_return[0] & 0x0fff0ff0;  // mask out the reserved and "stepping" fields, leaving only the base and extended Family/Model fields

#ifdef 0
    if (ModelInfo == CPUID_SIGNATURE_HASWELL) {                   // expected values for Haswell EP
        printf("Haswell EP\n");
    }
    else if (ModelInfo == CPUID_SIGNATURE_SKX) {              // expected values for SKX/CLX
        printf("SKX/CLX\n");
    }
    else if (ModelInfo == CPUID_SIGNATURE_ICX) {              // expected values for Ice Lake Xeon
        printf("ICX\n");
    }
    else if (ModelInfo == CPUID_SIGNATURE_SPR) {              // expected values for Sapphire Rapids Xeon
        printf("SPR\n");
    } else {
        printf("Unknown processor 0x%x\n",ModelInfo);
    }
#endif

    return ModelInfo;
}

// Based on J. McCalpin code 
unsigned long read_CHA_counter(unsigned int cpuid_signature,
                               int socket, 
							   int cha_number, 
							   int counter,
                               int * msr_fd)
{
	unsigned long msr_val, msr_num, msr_base;
    unsigned long msr_stride;

    msr_val = 0;

    switch(cpuid_signature) {
        case CPUID_SIGNATURE_HASWELL:
        // ------------ Haswell EP -- Xeon E5-2xxx v3 --------------
            // printf("CPUID Signature 0x%x identified as Haswell EP\n",CurrentCPUIDSignature);
            break;
        // ------------ Skylake Xeon and Cascade Lake Xeon -- 1st and 2nd generation Xeon Scalable Processors ------------
        case CPUID_SIGNATURE_SKX:
            // printf("CPUID Signature 0x%x identified as Skylake Xeon/Cascade Lake Xeon\n",CurrentCPUIDSignature);
            msr_base = 0xe00;
            msr_stride = 0x10;              // specific to SKX/CLX
            msr_num = msr_base + msr_stride*cha_number + counter + 8;     // compute MSR number for count register for counter
            // printf("DEBUG: socket %d cha_number %d counter %d msr_num 0x%lx msr_val 0x%lx\n",socket,cha_number,counter,msr_num,msr_val);
            pread(msr_fd[socket],&msr_val,sizeof(msr_val),msr_num);
            return (msr_val);
            break;
        // ------------- Ice Lake Xeon -- 3rd generation Xeon Scalable Processors ------------
        case CPUID_SIGNATURE_ICX:
            // printf("CPUID Signature 0x%x identified as Ice Lake Xeon\n",CurrentCPUIDSignature);
            msr_stride = 0x0e;              // ICX-specific

            if (cha_number >= 34) {
                msr_base = 0x0e00 - 0x47c;       // ICX MSRs skip backwards for CHAs 34-39
            } else if (cha_number >= 18) {
                msr_base = 0x0e00 + 0x0e;        // ICX MSRs skip forward for CHAs 18-33
            } else {
                msr_base = 0x0e00;               // MSRs for the first 18 CHAs
            }
            msr_num = msr_base + msr_stride*cha_number + counter + 8;     // compute MSR number for count register for counter 
            pread(msr_fd[socket],&msr_val,sizeof(msr_val),msr_num);
            return(msr_val); 
            break;
        // ------------------ Sapphire Rapids -- 4th generation Xeon Scalable Processors and Xeon CPU Max Processors ------------
        case CPUID_SIGNATURE_SPR:
            // printf("CPUID Signature 0x%x identified as Sapphire Rapids Xeon\n",CurrentCPUIDSignature);
            msr_base = 0x2000;
            msr_stride = 0x10;
            msr_num = msr_base + msr_stride*cha_number + 0x8 + counter;
            pread(msr_fd[socket],&msr_val,sizeof(msr_val),msr_num);
            return(msr_val); 
            break;
        default:
            fprintf(stderr,"CHA counters not yet supported for CPUID Signature 0x%x\n",cpuid_signature);
            exit(1);
	}
}

// Based on J. McCalpin code (slightly adapted)
int program_CHA_counter(unsigned int cpuid_signature, 
                        int num_chas,
						unsigned long * cha_perfevtsel,
                        int num_counters, 
						int * msr_fd, 
						int num_sockets)
{
    int pkg,tile,counter;
    unsigned long msr_val, msr_num, msr_base;
    unsigned long msr_stride;

    switch(cpuid_signature) {
        case CPUID_SIGNATURE_HASWELL:
        // ------------ Haswell EP -- Xeon E5-2xxx v3 --------------
            printf("CPUID Signature 0x%x identified as Haswell EP\n",cpuid_signature);
            break;
        // ------------ Skylake Xeon and Cascade Lake Xeon -- 1st and 2nd generation Xeon Scalable Processors ------------
        case CPUID_SIGNATURE_SKX:
            printf("CPUID Signature 0x%x identified as Skylake Xeon/Cascade Lake Xeon\n",cpuid_signature);
            msr_base = 0xe00;
            msr_stride = 0x10;              // specific to SKX/CLX
            for (pkg=0; pkg<num_sockets; pkg++) {
                for (tile=0; tile<num_chas; tile++) {
                    for (counter=0; counter<num_counters; counter++) {
                        msr_num = msr_base + msr_stride*tile + counter + 1;     // ctl register for counter
                        msr_val = cha_perfevtsel[counter];
                        // printf("DEBUG: pkg %d tile %d counter %d msr_num 0x%lx msr_val 0x%lx\n",pkg,tile,counter,msr_num,msr_val);
                        pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);
                    }
                    // The CHA performance counters on SKX/CLX have two filter registers that are required for some events.
                    // PMON_BOX_FILTER0 (offset 0x5) controls LLC_LOOKUP state (bits 26:17) and optional TID (bits 8:0)
                    msr_num = msr_base + msr_stride*tile + 5;    // filter0
                    msr_val = 0x01e20000;              // bits 24:21,17 FMESI -- all LLC lookups, not not SF lookups
                    // printf("DEBUG: pkg %d tile %d counter %d msr_num 0x%lx msr_val 0x%lx\n",pkg,tile,counter,msr_num,msr_val);
                    pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);
                    // PMON_BOX_FILTER1 (offset 0x6) allows opcode filtering and local/remote filtering.
                    //    FILTER1 should be set to 0x03B for no filtering (all memory, all local/remote, all opcodes)
                    msr_num = msr_base + msr_stride*tile + 6;    // filter1
                    msr_val = 0x03b;                    // near&non-near memory, local&remote, all opcodes
                    // printf("DEBUG: pkg %d tile %d counter %d msr_num 0x%lx msr_val 0x%lx\n",pkg,tile,counter,msr_num,msr_val);
                    pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);
                }
            }
            break;
        // ------------- Ice Lake Xeon -- 3rd generation Xeon Scalable Processors ------------
        case CPUID_SIGNATURE_ICX:
            printf("CPUID Signature 0x%x identified as Ice Lake Xeon\n",cpuid_signature);
            msr_stride = 0x0e;              // ICX-specific
            for (pkg=0; pkg<num_sockets; pkg++) {
                for (tile=0; tile<num_chas; tile++) {
                    if (tile >= 34) {
                        msr_base = 0x0e00 - 0x47c;       // ICX MSRs skip backwards for CHAs 34-39
                    } else if (tile >= 18) {
                        msr_base = 0x0e00 + 0x0e;        // ICX MSRs skip forward for CHAs 18-33
                    } else {
                        msr_base = 0x0e00;               // MSRs for the first 18 CHAs
                    }

                    // unit control register -- optional write bit 1 (value 0x2) to clear counters
                    msr_num = msr_base + msr_stride*tile;
                    msr_val = 0x2;
                    pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);

                    // program the control registers for counters 0..num_counters-1
                    for (counter=0; counter<num_counters; counter++) {
                        msr_num = msr_base + msr_stride*tile + counter + 1;     // ctl register for counter
                        msr_val = cha_perfevtsel[counter];
                        pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);
                    }
                }
            }
            return(0); // no error checking yet -- if it dies, it dies....
            break;
        // ------------------ Sapphire Rapids -- 4th generation Xeon Scalable Processors and Xeon CPU Max Processors ------------
        case CPUID_SIGNATURE_SPR:
            printf("CPUID Signature 0x%x identified as Sapphire Rapids Xeon\n",cpuid_signature);
            msr_base = 0x2000;
            msr_stride = 0x10;
            for (pkg=0; pkg<num_sockets; pkg++) {
                for (tile=0; tile<num_chas; tile++) {
                    for (counter=0; counter<num_counters; counter++) {
                        msr_num = msr_base + msr_stride*tile + counter + 2;     // compute MSR number of control register for counter
                        msr_val = cha_perfevtsel[counter];
                        // printf("DEBUG: pkg %d tile %d counter %d msr_num 0x%lx msr_val 0x%lx\n",pkg,tile,counter,msr_num,msr_val);
                        pwrite(msr_fd[pkg],&msr_val,sizeof(msr_val),msr_num);
                    }
                }
            }
            return(0); // no error checking yet -- if it dies, it dies....
            break;
        default:
            fprintf(stderr,"CHA counters not yet supported for CPUID Signature 0x%x\n",cpuid_signature);
            exit(1); 
	}
}

// Convert PCI(bus:device.function,offset) to uint32_t array index
unsigned int  PCI_cfg_index(unsigned int Bus, unsigned int Device, unsigned int Function, unsigned int Offset)
{
    unsigned int byteaddress;
    unsigned int index;
    assert (Device >= 0);
    assert (Function >= 0);
    assert (Offset >= 0);
    assert (Device < (1<<5));
    assert (Function < (1<<3));
    assert (Offset < (1<<12));
    byteaddress = (Bus<<20) | (Device<<15) | (Function<<12) | Offset;
    index = byteaddress / 4;
    return ( index );
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
