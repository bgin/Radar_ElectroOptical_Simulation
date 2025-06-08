
#ifndef __GMS_FAST_PMC_ACCESS_H__
#define __GMS_FAST_PMC_ACCESS_H__


/*
    Based on John D. McCalpin 'low-overhead-timers'.
    Callable from Fortran.
    !! Compile this file as a pure 'C'.
    Original author copyright:
    BSD 3-Clause License
    Copyright (c) 2018, John D McCalpin and University of Texas at Austin
    All rights reserved.
*/

// Some very low-overhead timer/counter interfaces:
//
// rdtsc() returns the number of "nominal" processor cycles since the system booted in a 64-bit unsigned integer.
//       For all recent Intel processors, this counter increments at a fixed rate, independent of the actual
//       core clock speed or the energy-saving mode.
// rdtscp() is the same as rdtsc except that it is partially ordered -- it will not execute until all prior
//       instructions in program order have executed.  (See also full_rdtscp)
// full_rdtscp() returns the number of "nominal" processor cycles in a 64-bit unsigned integer and also 
//       modifies its two integer arguments to show the processor socket and processor core that were in use
//       when the call was made.  (Note: the various cores in a chip usually have very similar values for 
//       the TSC, but they are allowed to vary by processor.  This function guarantees that you know exactly
//       which processor the TSC reading came from.)
// get_core_number() uses the RDTSCP instruction, but returns only the core number in an integer variable.
// get_socket_number() uses the RDTSCP instruction, but returns only the socket number in an integer variable.
// rdpmc_instructions() uses a "fixed-function" performance counter to return the count of retired instructions on
//       the current core in the low-order 48 bits of an unsigned 64-bit integer.
// rdpmc_actual_cycles() uses a "fixed-function" performance counter to return the count of actual CPU core cycles
//       executed by the current core.  Core cycles are not accumulated while the processor is in the "HALT" state,
//       which is used when the operating system has no task(s) to run on a processor core.
// rdpmc_reference_cycles() uses a "fixed-function" performance counter to return the count of "reference" (or "nominal")
//       CPU core cycles executed by the current core.  This counts at the same rate as the TSC, but does not count
//       when the core is in the "HALT" state.  If a timed section of code shows a larger change in TSC than in
//       rdpmc_reference_cycles, the processor probably spent some time in a HALT state.
// rdpmc() reads the programmable core performance counter number specified in the input argument.
//		 No error or bounds checking is performed.
//
// get_TSC_frequency() parses the Brand Identification string from the CPUID instruction to get the "nominal"
//       frequency of the processor, which is also the invariant TSC frequency, and returned as a float value in Hz.
//       This can then be used to convert TSC cycles to seconds.
//

unsigned long rdtsc();

unsigned long rdtscp();

unsigned long full_rdtscp(int *, int *);

int get_core_number();

int get_socket_number();

unsigned long rdpmc_instructions();

unsigned long rdpmc_actual_cycles();

unsigned long rdpmc_reference_cycles();

unsigned long rdpmc(int);

int get_core_counter_width();

int get_fixed_counter_width();

unsigned long corrected_pmc_delta(unsigned long,
				  unsigned long,
				  int);

float get_TSC_frequency();

unsigned int cpuid_signature();

unsigned long read_CHA_counter(unsigned int,
                               int, int, int,
                               int *);

int program_CHA_counter(unsigned int, 
                        int, unsigned long *,
                        int, int *, int);


unsigned int  PCI_cfg_index(unsigned int, 
                            unsigned int, 
                            unsigned int, 
                            unsigned int);

unsigned long long approximate_cpuid_bias();

unsigned long long approximate_rdpmc_bias();

void approx_cpuid_bias_samples(unsigned long long * __restrict__ ,
                               unsigned long long * __restrict__ ,
                               unsigned long long * __restrict__ ,
                               const unsigned long long,
                               const unsigned long long);

void approx_rdpmc_bias_samples(unsigned long long * __restrict__ ,
                               unsigned long long * __restrict__ ,
                               unsigned long long * __restrict__ ,
                               const unsigned long long,
                               const unsigned long long);                             


#endif /*__GMS_FAST_PMC_ACCESS_H__*/
