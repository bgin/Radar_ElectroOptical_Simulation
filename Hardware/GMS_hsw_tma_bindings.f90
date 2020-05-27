

module mod_hsw_tma_bindings


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_hsw_tma_bindings'
 !          
 !          Purpose:
 !                    Fortran bindings Haswell client TMA Metrics (C)
 !                   
 !                     
 !          History:
 !                        Date: 26-05-2020
 !                        Time: 16:53 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds, only : int4
     use, intrinsic :: ISO_C_BINDING
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
   
    integer(kind=int4),   parameter :: MOD_HSW_TMA_BINDINGS_MAJOR = 1
    integer(kind=int4),   parameter :: MOD_HSW_TMA_BINDINGS_MINOR = 0
    integer(kind=int4),   parameter :: MOD_HSW_TMA_BINDINGS_MICRO = 0
    integer(kind=int4),   parameter :: MOD_HSW_TMA_BINDINGS_FULLVER =
        1000*MOD_HSW_TMA_BINDINGS_MAJOR+100*MOD_HSW_TMA_BINDINGS_MINOR+ &
                 10*MOD_HSW_TMA_BINDINGS_MICRO
    character(*),         parameter :: MOD_HSW_TMA_BINDINGS_CREATE_DATE = "26-05-2020 16:53 +00200 (TUE 26 MAY 2020 GMT+2)"
    character(*),         parameter :: MOD_HSW_TMA_BINDINGS_BUILD_DATE  = __DATE__ " " __TIME__
    character(*),         parameter :: MOD_HSW_TMA_BINDINGS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    character(*),         parameter :: MOD_HSW_TMA_BINDINGS_SYNOPSIS    = "Fortran bindings Haswell client TMA Metrics (C)"


    interface

       function hsw_fetched_uops(idq_dsb_uops,  &
                                 lsd_uops,      &
                                 idq_mite_uops, &
                                 idq_ms_uops) &
                                 bind(c,name="hsw_fetched_uops")
           integer(c_size_t) :: idq_dsb_uops
           integer(c_size_t) :: lsd_uops
           integer(c_size_t) :: idq_mite_uops
           integer(c_size_t) :: idq_ms_uops
           integer(c_size_t) :: hsw_fetched_uops
       end function hsw_fetched_uops
   
    end interface

    interface

       function hsw_recovery_cycles( int_misc_recovery_cycles_any,
                                     int_misc_recovery_cycles,
                                     is_ht_enabled) &
                                          bind(c,name="hsw_recovery_cycles")
            integer(c_size_t) :: int_misc_recovery_cycles_any
            integer(c_size_t) :: int_misc_recovery_cycles
            integer(c_bool)   :: is_ht_enabled
            integer(c_size_t) :: hsw_recovery_cycles
       end function hsw_recovery_cycles
       
    end interface

    interface

       function hsw_execute_cycles( uops_executed_core_c1, &
                                    is_ht_enabled) &
                                    bind(c,name="hsw_execute_cycles")
            integer(c_size_t)   :: uops_execute_core_c1
            integer(c_bool)     :: is_ht_enabled
            integer(c_size_t)   :: hsw_execute_cycles
       end function hsw_execute_cycles
       
    end interface

    interface

       function hsw_sq_full_cycles( offcore_requests_buffer_sq_full, &
                                    is_ht_enabled) &
                                    bind(c,name="hsw_sq_full_cycles")
            integer(c_size_t)  :: offcore_requests_buffer_sq_full
            integer(c_bool)    :: is_ht_enabled
            integer(c_size_t)  :: hsw_sq_full_cycles
     end function hsw_sq_full_cycles
       
    end interface

    interface

       function hsw_itlb_miss_cycles( itlb_misses_stlb_hit, &
                                      itlb_misses_walk_duration) &
                                      bind(c,name="hsw_itlb_miss_cycles")
             integer(c_size_t) :: itlb_misses_stlb_hit
             integer(c_size_t) :: itlb_misses_walk_duration
             integer(c_size_t) :: hsw_itlb_miss_cycles
       end function hsw_itlb_miss_cycles
       
    end interface


    interface

       function hsw_frontend_rs_empty_cycles( rs_event_empty_cycles, &
                                              frontend_latency) &
                                              bind(c,name="hsw_frontend_rs_empty_cycles")
                integer(c_size_t)   :: rs_event_empty_cycles
                real(c_float)       :: frontend_latency
                integer(c_size_t)   :: hsw_frontend_rs_empty_cycles
       end function hsw_frontend_rs_empty_cycles
       
    end interface


    interface

       function hsw_cycles_0_ports_utilized( uops_executed_core_i1_c1, &
                                             stalls_total,             &
                                             rs_event_empty_cycles,    &
                                             frontend_latency,         &
                                             is_ht_enabled  ) &
                                             bind(c,name="hsw_cycles_0_ports_utilized")
              integer(c_size_t)   :: uops_executed_core_i1_c1
              integer(c_size_t)   :: stalls_total
              integer(c_size_t)   :: rs_event_empty_cycles
              real(c_float)       :: frontend_latency
              integer(c_bool)     :: is_ht_enabled
              integer(c_size_t)   :: hsw_cycles_0_ports_utilized
            end function hsw_cycles_0_ports_utilized
            
    end interface
   
    interface

       function hsw_cycles_1_ports_utilized( uops_executed_core_c1, &
                                             uops_executed_core_c2, &
                                             is_ht_enabled  )       &
                                             bind(c,name="hsw_cycles_1_ports_utilized")
              integer(c_size_t) :: uops_executed_core_c1
              integer(c_size_t) :: uops_executed_core_c2
              integer(c_bool)   :: is_ht_enabled
              integer(c_size_t) :: hsw_cycles_1_ports_utlized
       end function hsw_cycles_1_ports_utilized

    end interface

    interface

       function hsw_cycles_2_ports_utilized( uops_executed_core_c2, &
                                             uops_executed_core_c3, &
                                             is_ht_enabled  )       &
                                             bind(c,name="hsw_cycles_2_ports_utilized")
              integer(c_size_t) :: uops_executed_core_c2
              integer(c_size_t) :: uops_executed_core_c3
              integer(c_bool)   :: is_ht_enabled
              integer(c_size_t) :: hsw_cycles_2_ports_utlized
       end function hsw_cycles_2_ports_utilized

    end interface

    interface

       function hsw_cycles_3_ports_utilized( uops_executed_core_c3, &
                                              is_ht_enabled  )       &
                                             bind(c,name="hsw_cycles_3_ports_utilized")
              integer(c_size_t) :: uops_executed_core_c3
              integer(c_bool)   :: is_ht_enabled
              integer(c_size_t) :: hsw_cycles_3_ports_utlized
       end function hsw_cycles_3_ports_utilized

    end interface


end module mod_hsw_tma_bindings
