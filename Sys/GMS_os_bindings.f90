
module os_bindings


  !=====================================================================!
  ! Various Fortran wrappers to platform,resource and system functions
  !=====================================================================!

  use, intrinsic :: ISO_C_BINDING
  implicit none
  public


  !=======================================================!
  ! Interface to C wrapper function calling 'getrusage'   !
  !=======================================================!
  
  interface
     function c_getrusage(who,            &
                          ru_utime,       &
                          ru_stime,       &
                          ru_maxrss,      &
                          ru_ixrss,       &
                          ru_idrss,       &
                          ru_isrss,       &
                          ru_minflt,      &
                          ru_majflt,      &
                          ru_nswap,       &
                          ru_inblock,     &
                          ru_outblock,    &
                          ru_msgsnd,      &
                          ru_msgrcv,      &
                          ru_nsignals,    &
                          ru_nvcws,       &
                          ru_nivcws)  result(status) &
           bind(c,name='c_getrusage')
           use, intrinsic :: ISO_C_BINDING
           integer(c_int),      intent(in), value :: who
           integer(c_int),      intent(out)       :: ru_utime
           integer(c_int),      intent(out)       :: ru_stime
           integer(c_int),      intent(out)       :: ru_maxrss
           integer(c_int),      intent(out)       :: ru_ixrss
           integer(c_int),      intent(out)       :: ru_idrss
           integer(c_int),      intent(out)       :: ru_isrss
           integer(c_int),      intent(out)       :: ru_minflt
           integer(c_int),      intent(out)       :: ru_majflt
           integer(c_int),      intent(out)       :: ru_nswap
           integer(c_int),      intent(out)       :: ru_inblock
           integer(c_int),      intent(out)       :: ru_outblock
           integer(c_int),      intent(out)       :: ru_msgsnd
           integer(c_int),      intent(out)       :: ru_msgrcv
           integer(c_int),      intent(out)       :: ru_nsignals
           integer(c_int),      intent(out)       :: ru_nvcws
           integer(c_int),      intent(out)       :: ru_nivcws
           integer(c_int) :: status
     end function c_getrusage
     
  end interface

  !=======================================================!
  ! Interface to C wrapper function calling 'sysconf'     !
  !=======================================================!

  interface
     function c_sysconf(name) result(val) &
          bind(c,name='c_sysconf')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),     intent(in), value :: name
          integer(c_int) :: val
     end function c_sysconf
     
  end interface

  !=======================================================!
  ! Interface to C wrapper function calling 'cacheflush'  !
  !=======================================================!
  
  interface
     function c_cacheflush(addr,    &
                           nbytes,  &
                           cache) result(stat) &
             bind(c,name='c_cacheflush')
             use, intrinsic :: ISO_C_BINDING
             type(c_ptr),     intent(in), value :: addr
             integer(c_int),  intent(in), value :: nbytes
             integer(c_int),  intent(in), value :: cache
             integer(c_int) :: stat
     end function c_cacheflush
     
  end interface

  !=======================================================!
  ! Interface to C wrapper function calling 'abort'       !
  !=======================================================!

  interface
     subroutine c_abort() bind(c,name='c_abort')
                use, intrinsic :: ISO_C_BINDING
     end subroutine c_abort
  end interface

  !=====================================================================!
  ! Interface to C wrapper function calling 'clock_getcpuclockid'       !
  !=====================================================================!

  interface
     function c_clock_getcpuclockid(pid,clockid) result(stat) &
               bind(c,name='c_clock_getcpuclockid')
               use, intrinsic :: ISO_C_BINDING
               integer(c_int),     intent(in), value  :: pid
               type(c_ptr),        intent(out)        :: clockid
               integer(c_int) :: stat
     end function c_clock_getcpuclockid
     
  end interface

  !=========================================================!
  ! Interface to C wrapper function calling 'clock_getres'  !
  !=========================================================!

  interface
     function c_clock_getres(clock_id,nsec) result(stat) &
                   bind(c,name='c_clock_getres')
                use, intrinsic :: ISO_C_BINDING
                integer(c_int),        intent(in), value :: pid
                integer(c_long_long),  intent(out)       :: nsec
                integer(c_int) :: stat
     end function c_clock_getres
     
  end interface

  !=========================================================!
  ! Interface to C wrapper function calling 'kill'  !
  !=========================================================!
  
  interface
     function c_kill(pid,sig) result(stat) &
          bind(c,name='c_kill')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),        intent(in), value :: pid
          integer(c_int),        intent(in), value :: sig
          integer(c_int) :: stat
     end function c_kill
     
  end interface



















end module os_bindings
