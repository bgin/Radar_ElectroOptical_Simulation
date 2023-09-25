

module cpufreq_interface


 !========================================================!
  ! Fortran interfaces to some of the functionality
  ! as presented by the program 'cpupower'.
  ! Experimental implementation!!
  !========================================================!
  use, intrinsic :: ISO_C_BINDING
  implicit none
  public
  
   
   
   
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


end module cpufreq_interface
