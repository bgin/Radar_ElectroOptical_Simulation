

module cpufreq_power


 !========================================================!
  ! Fortran interfaces to some of the functionality
  ! as presented by the program 'cpupower'.
  ! Experimental implementation!!
  !========================================================!
  use, intrinsic :: ISO_C_BINDING
  implicit none
  public
  
  
  enum, bind(c) :: acpi_cppc_value
        enumerator :: HIGHEST_PERF
        enumerator :: LOWEST_PERF
        enumerator :: NOMINAL_PERF
        enumerator :: LOWEST_NONLINEAR_PERF
        enumerator :: LOWEST_FREQ
        enumerator :: NOMINAL_FREQ
        enumerator :: REFERENCE_PERF
        enumerator :: WRAPAROUND_TIME
        enumerator :: MAX_CPPC_VALUE_FILES
  end enum
   
   
  interface
      function cpufreq_get_freq_kernel(cpu) result(val) &
               bind(c,name='cpufreq_get_freq_kernel')
            use, intrinsic :: ISO_C_BINDING
            integer(c_int),   intent(in), value :: cpu
            integer(c_int) :: val    
      end function cpufreq_get_freq_kernel
  end interface
  
  interface
      function cpufreq_get_freq_hardware(cpu) result(val) &
               bind(c,name='cpufreq_get_freq_hardware')
            use, intrinsic :: ISO_C_BINDING
            integer(c_int),   intent(in), value :: cpu
            integer(c_int) :: val  
      end function cpufreq_get_freq_hardware
  end interface

  interface
      function cpufreq_get_transition_latenct(cpu) result(val) &
               bind(c,name='cpufreq_get_transition_latency')
            use, intrinsic :: ISO_C_BINDING
            integer(c_int),   intent(in), value :: cpu
            integer(c_int) :: val   
      end function cpufreq_get_transition_latency
  end interface

  interface
      function cpufreq_get_hardware_limits(cpu,min,max) result(error) &
               bind(c,name='cpufreq_get_hardware_limits')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(out)       :: min
             integer(c_int),   intent(out)       :: max
             integer(c_int) :: error 
      end function cpufreq_get_hardware_limits
  end interface
  
  interface
      function cpufreq_get_driver(cpu) result(driver)
               bind(c,name='cpufreq_get_driver')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             type(c_ptr) :: driver
      end function cpufreq_get_driver
  end interface
  
  interface
      function cpufreq_modify_policy_min(cpu,min_freq) result(val) &
               bind(c,name='cpufreq_modify_policy_min')
             use, intrinsic :: ISO_C_BINDING 
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: min_freq
             integer(c_int) :: val
      end function cpufreq_modify_policy_min
  end interface
  
  interface
      function cpufreq_modify_policy_max(cpu,max_freq) result(val) &
               bind(c,name='cpufreq_modify_policy_max')
             use, intrinsic :: ISO_C_BINDING 
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: max_freq
             integer(c_int) :: val
      end function cpufreq_modify_policy_max
  end interface
  
  interface
      function cpufreq_set_frequency(cpu,target_freq) result(val) &
               bind(c,name='cpufreq_set_frequency')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: target_freq
             integer(c_int) :: val
      end function cpufreq_set_frequency
  end interface
  
  interface
      function cpufreq_get_transitions(cpu) result(val) &
               bind(c,name='cpufreq_get_transitions')
              use, intrinsic :: ISO_C_BINDING
              integer(c_int),   intent(in), value :: cpu
              integer(c_int) :: val
      end function cpufreq_get_transitions
  end interface
  
  interface
      function acpi_cppc_get_data(cpu,which) result(val) &
               bind(c,name='acpi_cppc_get_data')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: which
             integer(c_int) :: val
      end function acpi_cppc_get_data
  end interface
  
  interface
      function cpuidle_is_state_disabled(cpu,idlestate) result(val) &
               bind(c,name='cpuidle_is_state_disabled')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: idlestate
             integer(c_int) :: val 
      end function cpuidle_is_state_disabled
  end interface
  
  interface
      function cpuidle_state_disable(cpu,idlestate, &
                                     disable) result(val) &
               bind(c,name='cpuidle_state_disable')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: idlestate
             integer(c_int),   intent(in), value :: disable
             integer(c_int) :: val 
      end function cpuidle_state_disable
  end interface
  
  interface
      function cpuidle_state_latency(cpu,idlestate) result(val) &
               bind(c,name='cpuidle_state_latency')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: idlestate
             integer(c_int) :: val 
      end function cpuidle_state_latency
  end interface
  
  interface
      function cpuidle_state_usage(cpu,idlestate) result(val) &
               bind(c,name='cpuidle_state_usage')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: idlestate
             integer(c_int) :: val 
      end function cpuidle_state_usage
  end interface
  
  interface
      function cpuidle_state_time(cpu,idlestate) result(val) &
               bind(c,name='cpuidle_state_time')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int),   intent(in), value :: idlestate
             integer(c_int) :: val 
      end function cpuidle_state_time
  end interface
  
  interface
      function cpuidle_state_count(cpu) result(val) &
               bind(c,name='cpuidle_state_count')
             use, intrinsic :: ISO_C_BINDING
             integer(c_int),   intent(in), value :: cpu
             integer(c_int) :: val 
      end function cpuidle_state_count
  end interface


end module cpufreq_power
