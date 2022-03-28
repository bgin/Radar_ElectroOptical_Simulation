

module fast_pmc_access

    !=============================================================!
    ! Fortran wrappers to 'GMS_fast_pmc_access.h' header file     !
    !=============================================================!
    use, intrinsic :: ISO_C_BINDINGS
    implicit none
    public

#if 0

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


#endif
    

      interface
         function rdtsc() result(val) &
                     bind(c,name='rdtsc')
             use, intrinsic :: ISO_C_BINDING
             integer(c_long_long) :: val
           end function rdtsc
           
      end interface

          
      interface
         function rdtscp() result(val) &
                     bind(c,name='rdtscp')
             use, intrinsic :: ISO_C_BINDING
             integer(c_long_long) :: val
           end function rdtscp
           
      end interface


      interface
         function full_rdtscp(chip,core) result(val) &
                      bind(c,name='full_rdtscp')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int), intent(inout) :: chip
             integer(c_int), intent(inout) :: core
             integer(c_long_long) :: val
         end function full_rdtscp
         
      end interface


      interface
         function get_core_number() result(val) &
                  bind(c,name='get_core_number')
              use, intrinsic :: ISO_C_BINDING
              integer(c_int) :: val
            end function get_core_number
            
      end interface


      interface
          function get_socket_number() result(val) &
                   bind(c,name='get_socket_number')
              use, intrinsic :: ISO_C_BINDING
              integer(c_int) :: val
            end function get_socket_number
            
      end interface


      interface
          function rdpmc_instructions() result(val) &
                   bind(c,name='rdpmc_instructions')
                use, intrinsic :: ISO_C_BINDING
                integer(c_long_long) :: val
              end function rdpmc_instructions
              
      end interface


      interface
          function rdpmc_actual_cycles() result(val) &
                   bind(c,name='rdpmc_actual_cycles')
                use, intrinsic :: ISO_C_BINDING
                integer(c_long_long) :: val
              end function rdpmc_actual_cycles
              
      end interface

           
      interface
          function rdpmc_reference_cycles() result(val) &
                   bind(c,name='rdpmc_reference_cycles')
                use, intrinsic :: ISO_C_BINDING
                integer(c_long_long) :: val
              end function rdpmc_reference_cycles
              
      end interface

           
      interface
          function rdpmc(c) result(val) &
               bind(c,name='rdpmc')
            use, intrinsic :: ISO_C_BINDING
            integer(c_int),  intent(in) :: c
            integer(c_long_long) :: val
          end function rdpmc
          
     end interface


     interface
          function get_core_counter_width() result(val) &
                   bind(c,name='get_core_counter_width')
                use, intrinsic :: ISO_C_BINDING
                integer(c_int) :: val
              end function get_core_counter_width
              
     end interface


     interface
          function get_fixed_counter_width() result(val) &
                   bind(c,name='get_fixed_counter_width')
                use, intrinsic :: ISO_C_BINDING
                integer(c_int) :: val
              end function get_fixed_counter_width
              
     end interface


     interface
        function corrected_pmc_delta(end,start,pmc_width) result(val) &
                 bind(c,name='corrected_pmc_delta')
             use, intrinsic :: ISO_C_BINDING
             integer(c_long_long), intent(in), value :: end
             integer(c_long_long), intent(in), value :: start
             integer(c_int),       intent(in), value :: pmc_width
             integer(c_long_long) :: val
        end function corrected_pmc_delta
        
     end interface

     interface
        function get_TSC_frequency() result(val) &
                   bind(c,name='get_TSC_frequency')
              use, intrinsic :: ISO_C_BINDING
              real(c_float) :: val
        end function get_TSC_frequency
    

     end interface
      
end module fast_pmc_access
