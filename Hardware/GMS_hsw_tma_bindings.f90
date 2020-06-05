

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

    interface

       function hsw_frontend_latency_cycles( cpu_clk_unhalted_thread, &
                                             idq_uops_not_delivered_cycles_0_uops_deliv_core) &
                                             bind(c,name="hsw_frontend_latency_cycles")
               integer(c_size_t)  :: cpu_clk_unhalted_thread
               integer(c_size_t)  :: idq_uops_not_delivered_cycles_0_uops_deliv_core
               integer(c_size_t)  :: hsw_frontend_latency_cycles
       end function hsw_frontend_latency_cycles
       
    end interface

    interface

       function hsw_stalls_mem_any( cpu_clk_unhalted_thread, &
                                    cycles_activity_stalls_lm_pending) &
                                    bind(c,name="hsw_stalls_mem_any")
                integer(c_size_t)  :: cpu_clk_unhalted_thread
                integer(c_size_t)  :: cycles_activity_stalls_lm_pending
                integer(c_size_t)  :: hsw_stalls_mem_any
       end function hsw_stalls_mem_any
       
    end interface

    interface

       function hsw_stalls_total( cpu_clk_unhalted_thread, &
                                  cycles_activity_cycles_no_execute) &
                                  bind(c,name="hsw_stalls_total")
                integer(c_size_t)  :: cpu_clk_unhalted_thread
                integer(c_size_t)  :: cycles_activity_cycles_no_execute
                integer(c_size_t)  :: hsw_stalls_total
       end function hsw_stalls_total

    end interface

    interface

       function hsw_oro_drd_any_cycles( cpu_clk_unhalted_thread, &
                                        offcore_requests_oustanding_cycles_with_data_rd) &
                                        bind(c,name="hsw_oro_drd_any_cycles")
                integer(c_size_t) :: cpu_clk_unhalted_thread
                integer(c_size_t) :: offcore_requests_oustanding_cycles_with_data_rd
                integer(c_size_t) :: hsw_oro_drd_any_cycles
       end function hsw_oro_drd_any_cycles
  
    end interface

    interface

       function hsw_oro_drd_bw_cycles( cpu_clk_unhalted_thread, &
                                       offcore_requests_outstanding_all_data_rd_c6) &
                                       bind(c,name="hsw_oro_drd_bw_cycles")
                integer(c_size_t) ::  cpu_clk_unhalted_thread
                integer(c_size_t) ::  offcore_requests_outstanding_all_data_rd_c6
                integer(c_size_t) :: hsw_oro_drd_bw_cycles
       end function hsw_oro_drd_bw_cycles

    end interface

    interface

       function hsw_oro_demand_rfo_c1( cpu_clk_unhalted_thread, &
                                       offcore_requests_outstanding_cycles_with_demand_rfo) &
                                       bind(c,name="hsw_oro_demand_rfo_c1")
                integer(c_size_t)  :: cpu_clk_unhalted_thread
                integer(c_size_t)  :: offcore_requests_outstanding_cycles_with_demand_rfo
                integer(c_size_t)  :: hsw_oro_demand_rfo_c1
       end function hsw_oro_demand_rfo_c1

    end interface

    interface

       function hsw_store_l2_hit_cycles( l2_rqsts_rfo_hit, &
                                         mem_uops_retired_lock_loads, &
                                         mem_uops_retired_all_stores) &
                                         bind(c,name="hsw_store_l2_hit_cycles")
               integer(c_size_t) :: l2_rqsts_rfo_hit
               integer(c_size_t) :: mem_uops_retired_lock_loads
               integer(c_size_t) :: mem_uops_retired_all_stores
               real(c_float)     :: hsw_store_l2_hit_cycles
       end function hsw_store_l2_hit_cycles
       
    end interface

    interface

       function hsw_load_l1_miss( mem_load_uops_retired_l2_hit, &
                                  mem_load_uops_retired_l3_hit, &
                                  mem_load_uops_l3_hit_retired_xsnp_hit, &
                                  mem_load_uops_l3_hit_retired_xsnp_hitm, &
                                  mem_load_uops_l3_hit_retired_xsnp_miss) &
                                  bind(c,name="hsw_load_l1_miss")
               integer(c_size_t) :: mem_load_uops_retired_l2_hit
               integer(c_size_t) :: mem_load_uops_retired_l3_hit
               integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hit
               integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hitm
               integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_miss
               integer(c_size_t) :: hsw_load_l1_miss
       end function hsw_load_l1_miss
       
    end interface

    interface

       function hsw_load_l1_miss_net( mem_load_uops_retired_l3_miss, &
                                      mem_load_uops_retired_l2_hit,  &
                                      mem_load_uops_retired_l3_hit,  &
                                      mem_load_uops_l3_hit_retired_xsnp_hit, &
                                      mem_load_uops_l3_hit_retired_xsnp_hitm, &
                                      mem_load_uops_l3_hit_retired_xsnp_miss) &
                                      bind(c,name="hsw_load_l1_miss_net")
                integer(c_size_t) :: mem_load_uops_retired_l3_miss
                integer(c_size_t) :: mem_load_uops_retired_l2_hit
                integer(c_size_t) :: mem_load_uops_retired_l3_hit
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hit
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hitm
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_miss
                integer(c_size_t) :: hsw_load_l1_miss_net
       end function hsw_load_l1_miss_net
       
    end interface

    interface


       function hsw_load_l3_hit( mem_load_uops_retired_l3_hit,  &
                                 mem_load_uops_retired_hit_lfb, &
                                 mem_load_uops_retired_l2_hit,  &
                                 mem_load_uops_l3_hit_retired_xsnp_hit, &
                                 mem_load_uops_l3_hit_retired_xsnp_hitm, &
                                 mem_load_uops_l3_hit_retired_xsnp_miss) &
                                 bind(c,name="hsw_load_l3_hit")
                integer(c_size_t) :: mem_load_uops_retired_l3_hit
                integer(c_size_t) :: mem_load_uops_retired_hit_lfb
                integer(c_size_t) :: mem_load_uops_retired_l2_hit
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hit
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_hitm
                integer(c_size_t) :: mem_load_uops_l3_hit_retired_xsnp_miss
                real(c_float)     :: hsw_load_l3_hit
       end function hsw_load_l3_hit

    end interface

    interface

       function hsw_load_xsnp_hit( mem_load_uops_l3_hit_retired_xsnp_hit, &
                                   mem_load_uops_retired_hit_lfb,         &
			           mem_load_uops_retired_l2_hit,          &
			           mem_load_uops_retired_l3_hit,          &
			           mem_load_uops_l3_hit_retired_xsnp_hitm,&
			           mem_load_uops_l3_hit_retired_xsnp_miss) &
                                   bind(c,name="hsw_load_xsnp_hit")
                  integer(c_size_t) ::  mem_load_uops_l3_hit_retired_xsnp_hit
                  integer(c_size_t) ::  mem_load_uops_retired_hit_lfb        
		  integer(c_size_t) ::	mem_load_uops_retired_l2_hit          
		  integer(c_size_t) ::	mem_load_uops_retired_l3_hit          
		  integer(c_size_t) ::	mem_load_uops_l3_hit_retired_xsnp_hitm
                  integer(c_size_t) ::	mem_load_uops_l3_hit_retired_xsnp_miss
                  real(c_float)     ::  hsw_load_xsnp_hit
       end function hsw_load_xsnp_hit

    end interface

    interface

         function hsw_load_xsnp_hitm( mem_load_uops_retired_hit_lfb, &
			              mem_load_uops_retired_l2_hit,  &
	                              mem_load_uops_retired_l3_hit,  &
			              mem_load_uops_l3_hit_retired_xsnp_hit, &
			              mem_load_uops_l3_hit_retired_xsnp_hitm,&
                                      mem_load_uops_l3_hit_hit_retired_xsnp_miss) &
                                      bind(c,name="hsw_load_xsnp_hitm")
                 integer(c_size_t)  :: mem_load_uops_retired_hit_lfb,
		 integer(c_size_t)  :: mem_load_uops_retired_l2_hit,
	         integer(c_size_t)  :: mem_load_uops_retired_l3_hit,
		 integer(c_size_t)  :: mem_load_uops_l3_hit_retired_xsnp_hit,
		 integer(c_size_t)  :: mem_load_uops_l3_hit_retired_xsnp_hitm,
                 integer(c_size_t)  :: mem_load_uops_l3_hit_hit_retired_xsnp_miss
                 real(c_float)      :: hsw_load_xsnp_hitm 
          end function hsw_load_xsnp_hitm
                 

       end interface


      interface

          function hsw_load_xsnp_miss( mem_load_uops_retired_hit_lfb, &
			              mem_load_uops_retired_l2_hit,  &
	                              mem_load_uops_retired_l3_hit,  &
			              mem_load_uops_l3_hit_retired_xsnp_hit, &
			              mem_load_uops_l3_hit_retired_xsnp_hitm,&
                                      mem_load_uops_l3_hit_hit_retired_xsnp_miss) &
                                      bind(c,name="hsw_load_xsnp_miss")
                 integer(c_size_t)  :: mem_load_uops_retired_hit_lfb,
		 integer(c_size_t)  :: mem_load_uops_retired_l2_hit,
	         integer(c_size_t)  :: mem_load_uops_retired_l3_hit,
		 integer(c_size_t)  :: mem_load_uops_l3_hit_retired_xsnp_hit,
		 integer(c_size_t)  :: mem_load_uops_l3_hit_retired_xsnp_hitm,
                 integer(c_size_t)  :: mem_load_uops_l3_hit_hit_retired_xsnp_miss
                 real(c_float)      :: hsw_load_xsnp_miss
           end function hsw_load_xsnp_miss
          
       end interface

        
      interface

         function hsw_few_uops_executed_threshold( uops_executed_core_c2, &
                                                   uops_executed_core_c3) &
                                                   bind(c,name="hsw_few_uops_executed_threshold")
                 integer(c_size_t)  :: uops_executed_core_c2
                 integer(c_size_t)  :: uops_executed_core_c3
                 integer(c_size_t)  :: hsw_few_uops_executed_threshold
         end function hsw_few_uops_executed_threshold

      end interface

      interface

         function hsw_backend_bound_cycles( stalls_total,  &
                                            uops_executed_core_c1, &
                                            few_uops_executed_threshold, &
                                            frontend_rs_empty_cycles, &
                                            resource_stalls_sb, &
                                            is_ht_enabled) &
                                            bind(c,name="hsw_backend_bound_cycles")
               integer(c_size_t)  :: stalls_total
               integer(c_size_t)  :: uops_executed_core_c1
               integer(c_size_t)  :: few_uops_executed_threshold
               integer(c_size_t)  :: frontend_rs_empty_cycles
               integer(c_size_t)  :: resource_stalls_sb
               integer(c_bool)    :: is_ht_enabled
               real(c_float)      :: hsw_backend_bound_cycles
         end function hsw_backend_bound_cycles
 
      end interface

      interface

         function hsw_memory_bound_fraction( stalls_mem_any, &
                                             resource_stalls_sb, &
                                             backend_bound_cycles) &
                                             bind(c,name="hsw_memory_bound_cycles")
              integer(c_size_t)  :: stalls_mem_any
              integer(c_size_t)  :: resource_stalls_sb
              real(c_float)      :: backend_bound_cycles
              real(c_float)      :: hsw_memory_bound_fraction
              
         end function hsw_memory_bound_cycles

      end interface

     interface

        function hsw_memory_bound_fraction( stalls_mem_any,  &
                                            resource_stalls_any, &
                                            backend_bound_cycles) &
                                            bind(c,name="hsw_memory_bound_fraction")
             integer(c_size_t) :: stalls_mem_any
             integer(c_size_t) :: resource_stalls_any
             real(c_float)     :: backend_bound_cycles
             real(c_float)     :: hsw_memory_bound_fraction
        end function hsw_memory_bound_fraction
         
     end interface

     interface

         function hsw_mem_l3_hit_fraction( mem_load_uops_retired_l3_hit, &
                                               mem_load_uops_retired_l3_miss) &
                                           bind(c,name="hsw_mem_l3_hit_fraction")
              integer(c_size_t)  :: mem_load_uops_retired_l3_hit
              integer(c_size_t)  :: mem_load_uops_retired_l3_miss
              real(c_float)      :: hsw_mem_l3_hit_fraction
         end function hsw_mem_l3_hit_fraction
     
     end interface

     interface

        function hsw_mem_lock_st_fraction( mem_uops_retired_lock_loads, &
                                           mem_uops_retired_all_stores) &
                                           bind(c,name="hsw_mem_lock_st_fraction")
              integer(c_size_t)  :: mem_uops_retired_lock_loads
              integer(c_size_t)  :: mem_uops_retired_all_stores
              real(c_float)      :: hsw_mem_lock_st_fraction
        end function hsw_mem_lock_st_fraction
   
     end interface

     interface

        function hsw_mispred_clears_fraction( br_mispred_all_branches, &
                                              machines_clears_count)  &
                                              bind(c,name="hsw_mispred_clears_fraction")
                 integer(c_size_t)  :: br_mispred_all_branches
                 integer(c_size_t)  :: machines_clears_count
                 real(c_float)      :: hsw_mispred_clears_fraction
        end function hsw_mispred_clears_fraction
  
     end interface

     interface

        function hsw_retire_fraction( uops_retired_retire_slots, &
                                      uops_issued_any) &
                                      bind(c,name="hsw_retire_fraction")
                 integer(c_size_t)  :: uops_retired_retire_slots
                 integer(c_size_t)  :: uops_issued_any
                 real(c_float)      :: hsw_retire_fraction
        end function hsw_retire_fraction
   
     end interface

     interface

        function hsw_ipc( inst_retired_any, &
                          clks) &
                          bind(c,name="hsw_ipc")
             integer(c_size_t) :: inst_retired_any
             integer(c_size_t) :: clks
             real(c_float)     :: hsw_ipc
        end function hsw_ipc
        
     end interface


     interface

        function hsw_upi( uops_retired_retire_slots, &
                          uops_retired_any) &
                          bind(c,name="hsw_upi")
              integer(c_size_t) :: uops_retired_retire_slots
              integer(c_size_t) :: uops_retired_any
              real(c_float)     :: hsw_upi
        end function hsw_upi
        
     end interface

     interface

        function hsw_iptb( instr_retired_any,  &
                           br_instr_retired_near_taken) &
                           bind(c,name="hsw_iptb")
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) :: br_instr_retired_near_taken
              real(c_float)     :: hsw_iptb
        end function hsw_iptb
        
     end interface

     interface

        function hsw_cpi( instr_retired_any, &
                          clks) &
                          bind(c,name="hsw_cpi")
                integer(c_size_t) :: instr_retired_any
                integer(c_size_t) :: clks
                real(c_float)     :: hsw_cpi
        end function hsw_cpi
   
     end interface

     interface

        function hsw_issue_slots( core_clks) &
                     bind(c,name="hsw_issue_slots")
            integer(c_size_t) :: core_clks
            integer(c_size_t) :: hsw_issue_slots
        end function hsw_issue_slots
    
     end interface

     interface

        function hsw_ipload(instr_retired_any, &
                           mem_uops_retired_all_loads) &
                           bind(c,name="hsw_ipload")
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) ::  mem_uops_retired_all_loads
              real(c_float)     :: hsw_ipload
        end function hsw_ipload
        
     end interface

     interface

        function hsw_ipstore( instr_retired_any, &
                              mem_uops_retired_all_stores) &
                              bind(c,name="hsw_ipstore")
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) ::  mem_uops_retired_all_stores
              real(c_float)     :: hsw_ipstore
        end function hsw_ipstore

     end interface

     interface

        function hsw_ipbranch(instr_retired_any, &
                              br_instr_retired_all_branches) &
                              bind(c,name="hsw_ipbranch")
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) :: br_instr_retired_all_branches
              real(c_float)     :: hsw_ipbranch
        end function hsw_ipbranch
        
     end interface

     interface

        function hsw_ipcall( instr_retired_any,  &
                             br_instr_retired_near_call) &
                             bind(c,name="hsw_ipcall")
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) :: br_instr_retired_near_call
              real(c_float)     :: hsw_ipcall
        end function hsw_ipcall
        
     end interface

     interface

          function hsw_biptb( br_inst_retired_all_branches, &
                             br_inst_retired_near_taken) &
                             bind(c,name="hsw_biptb")
                integer(c_size_t) :: br_inst_retired_all_branches
                integer(c_size_t) :: br_instr_retired_near_taken
                real(c_float)     :: hsw_biptb
          end function hsw_biptb

     end interface

       
     interface

        function hsw_dsb_coverage( idq_dsb_uops, &
                                   fetched_uops) &
                                   bind(c,name="hsw_dsb_coverage")
               integer(c_size_t) :: idq_dsb_uops
               integer(c_size_t) :: fetched_uops
               real(c_float)     :: hsw_dsb_coverage
        end function hsw_dsb_coverage
        
     end interface

     interface

        function hsw_ipbaclear(instr_retired_any, &
                               baclears_any) &
                               bind(c,name="hsw_ipbaclear")
               integer(c_size_t) :: instr_retired_any
               integer(c_size_t) :: baclears_any
               real(c_float)     :: hsw_ipbaclear
        end function hsw_ipbaclear
        
     end interface

     interface

        function hsw_ipc_core(instr_retired_any, &
                              core_clks) &
                              bind(c,name="hsw_ipc_core")
                  integer(c_size_t) :: instr_retired_any
                  integer(c_size_t) :: core_clks
                  real(c_float)     :: hsw_ipc_core
        end function hsw_ipc_core

     end interface

     interface

        function hsw_ilp( uops_executed_core, &
                          execute_cycles, &
                          is_ht_enabled) &
                          bind(c,name="hsw_ilp")
              integer(c_size_t) :: uops_executed
              integer(c_size_t) :: execute_cycles
              integer(c_size_t) :: is_hit_enabled
              real(c_float)     :: hsw_ilp
        end function hsw_ilp
             
        
     end interface


     interface

        function hsw_ip_mispredict( instr_retired_any, &
                                    br_misp_retired_all_branches) &
                                    bind(c,name="hsw_ip_mispredict")
              integer(c_size_t) :: intr_retired_any
              integer(c_size_t) :: br_misp_retired_all_branches
              real(c_float)     :: hsw_ip_mispredict
        end function hsw_ip_mispedict
        
     end interface

     interface

         function hsw_core_clks(   cpu_clk_unhalted_thread, &
                                   cpu_clk_unhalted_one_thread_active, &
			           cpu_clk_unhalted_ref_xclk, &
			           cpu_clk_unhalted_thread_any, &
                                   ebs_mode) &
                                   bind(c,name="hsw_core_clks")
               integer(c_size_t) :: cpu_clk_unhalted_thread
               integer(c_size_t) :: cpu_clk_unhalted_one_thread_active
               integer(c_size_t) :: cpu_clk_unhalted_ref_xclk
               integer(c_size_t) :: cpu_clk_unhalted_thread_any
               integer(c_bool)   :: ebs_mode
               integer(c_size_t) :: hsw_core_clks
         end function hsw_core_clks
  
      end interface

    interface

       function hsw_load_miss_real_latency( l1d_pend_miss_pending, &
                                            mem_load_uops_retired_l1_miss, &
                                            mem_load_uops_retired_hit_lfb) &
                                            bind(c,name="hsw_load_miss_real_latency")
                integer(c_size_t) :: l1d_pend_miss_pending
                integer(c_size_t) :: mem_load_uops_retired_l1_miss
                integer(c_size_t) :: mem_load_uops_retired_hit_lfb
                real(c_float)     :: hsw_load_miss_real_latency
       end function hsw_load_miss_real_latency
       
    end interface

    interface

       function hsw_mem_level_parallelism( l1d_pend_miss_pending, &
                                          l1d_pend_miss_pending_cycles) &
                                          bind(c,name="hsw_mem_level_parallelism")
                 integer(c_size_t) :: l1d_pend_miss_pending
                 integer(c_size_t) ::  l1d_pend_miss_pending_cycles
                 real(c_float)     :: hsw_mem_level_parallelism
       end function hsw_mem_level_paralellism

    end interface

    interface

        function hsw_page_walk_util(   itlb_misses_walk_duration,   &
                                       dtlb_load_misses_walk_duration, &
			               dtlb_store_misses_walk_duration, &
			               core_clks) &
                                       bind(c,name="hsw_page_walk_util")
               integer(c_size_t) :: itlb_misses_walk_duration
               integer(c_size_t) :: dtlb_load_misses_walk_duration
               integer(c_size_t) :: dtlb_store_misses_walk_duration
               integer(c_size_t) :: core_clks
               real(c_float)     :: hsw_page_walk_util
        end function hsw_page_walk_util
        
     end interface

     interface

        function hsw_l1d_cache_fill_bw( l1d_replacement, &
                                        time_interval) &
                                        bind(c,name="hsw_l1d_cache_fill_bw")
                 integer(c_size_t) :: l1d_replacement
                 integer(c_size_t) :: time_interval
                 real(c_float)     :: hsw_l1d_cache_fill_bw
        end function hsw_l1d_cache_fill_bw
        
     end interface

     interface

        function hsw_l2_cache_fill_bw( l2_lines_in_all, &
                                       time_interval) &
                                       bind(c,name="hsw_l2_cache_fill_bw")
                 integer(c_size_t) :: l2_lines_in_all
                 integer(c_size_t) :: time_interval
                 real(c_float)     :: hsw_l2_cache_fill_bw
        end function hsw_l2_cache_fill_bw
        
     end interface

     interface

        function hsw_l3_cache_fill_bw( longest_lat_cache_miss, &
                                     time_interval) &
                                     bind(c,name="hsw_l3_cache_fill_bw")
                integer(c_size_t) :: longest_lat_cache_miss
                integer(c_size_t) :: time_interval
                real(c_float)     :: hsw_l3_cache_fill_bw
        end function hsw_l3_cache_fill_bw
        
     end interface

     interface

        function hsw_l1mpki( mem_load_uops_retired_l1_miss, &
                            instr_retired_any) &
                            bind(c,name="hsw_l1mpki")
              integer(c_size_t) :: mem_load_uops_retired_l1_miss
              integer(c_size_t) :: instr_retired_any
              real(c_float)     :: hsw_l1mpki
        end function hsw_l1mpki
        
     end interface

     interface

        function hsw_l2mpki( mem_load_uops_retired_l2_miss, &
                            instr_retired_any) &
                            bind(c,name="hsw_l2mpki")
              integer(c_size_t) :: mem_load_uops_retired_l2_miss
              integer(c_size_t) :: instr_retired_any
              real(c_float)     :: hsw_l2mpki
        end function hsw_l2mpki
        
     end interface


     interface

        function hsw_l2hpki( mem_load_uops_retired_l2_miss, &
                            instr_retired_any) &
                            bind(c,name="hsw_l2hpki")
              integer(c_size_t) :: mem_load_uops_retired_l2_miss
              integer(c_size_t) :: instr_retired_any
              real(c_float)     :: hsw_l2hpki
        end function hsw_l2hpki
        
     end interface


     interface

        function hsw_l3mpki( mem_load_uops_retired_l3_miss, &
                            instr_retired_any) &
                            bind(c,name="hsw_l3mpki")
              integer(c_size_t) :: mem_load_uops_retired_l3_miss
              integer(c_size_t) :: instr_retired_any
              real(c_float)     :: hsw_l3mpki
        end function hsw_l3mpki
        
     end interface

     interface

        function hsw_ipfarbr( instr_retired_any,  &
                              br_inst_retired_far_branch) &
                              bind(c,name="hsw_ipfarbr") 
              integer(c_size_t) :: instr_retired_any
              integer(c_size_t) :: br_inst_retired_far_branch
              real(c_float)     :: hsw_ipfarbr
        end function hsw_ipfarbr
        
     end interface

     interface

        function hsw_assist( other_assists_any_wb_assists, &
                             slots) &
                             bind(c,name="hsw_assist")
               integer(c_size_t) :: other_assists_any_wb_assists
               integer(c_size_t) :: slots
               real(c_float)     :: hsw_assist
        end function hsw_assist
          
     end interface

     interface


        function hsw_kernel_utlization( cpu_clk_unhalted_thread_sup, &
                                        cpu_clk_unhalted_ref_tsc ) &
                                        bind(c,name="hsw_kernel_utilization")
                 integer(c_float) :: cpu_clk_unhalted_thread_sup
                 integer(c_float) :: cpu_clk_unhalted_ref_tsc
                 real(c_float)    :: hsw_kernel_utilization
        end function hsw_kernel_utilization
        
     end interface

     interface

        function hsw_dram_bw_use( unc_arb_trk_requests_all, &
                                  unc_arb_coh_trk_requests_all) &
                                  bind(c,name="hsw_dram_bw_use")
                integer(c_size_t) :: unc_arb_trk_requests_all
                integer(c_size_t) ::  unc_arb_coh_trk_requests_all
                real(c_float)     :: hsw_dram_bw_use
        end function hsw_dram_bw_use
         
     end interface

     interface

        function hsw_mem_requests_latency(unc_arb_trk_occupancy_all, &
                                          unc_arb_trk_requests_all) &
                                          bind(c,name="hsw_mem_requests_latency")
              integer(c_size_t) :: unc_arb_trk_occupancy_all
              integer(c_size_t) :: unc_arb_trk_requests_all
              real(c_float)     :: hsw_mem_requests_latency
        end function hsw_mem_requests_latency
        
     end interface

     interface

        function hsw_mem_parallel_requests( unc_arb_trk_occupancy_all, &
                                           unc_arb_trk_occupancy_cycles_with_any_request) &
                                           bind(c,name="hsw_mem_parallel_requests")
              integer(c_size_t) :: unc_arb_trk_occupancy_all
              integer(c_size_t) ::  unc_arb_trk_occupancy_cycles_with_any_request
              real(c_float)     :: hsw_mem_parallel_requests
        end function hsw_mem_parallel_requests
        
     end interface

     interface

        function hsw_ht_utilization(  cpu_clk_thread_unhalted_one_thread_active, &
                                      cpu_clk_thread_unhalted_ref_xclk_any) &
                                      bind(c,name="hsw_ht_utilization")
               integer(c_size_t) :: cpu_clk_thread_unhalted_one_thread_active
               integer(c_size_t) :: cpu_clk_thread_unhalted_ref_xclk_any
               real(c_float)     :: hsw_ht_utilization
        end function hsw_ht_utilization
        
     end interface

     interface

        function hsw_frontend_bound( idq_uops_not_delivered_core, &
                                     slots) &
                                     bind(c,name="hsw_frontend_bound")
               integer(c_size_t) :: idq_uops_not_delivered_core
               integer(c_size_t) :: slots
               real(c_float)     :: hsw_frontend_bound
        end function hsw_frontend_bound
        
     end interface

     interface

        function hsw_frontend_latency( frontend_latency_cycles, &
                                       cycles) &
                                       bind(c,name="hsw_frontend_latency")
                integer(c_size_t) :: frontend_latency_cycles
                integer(c_size_t) :: cycles
                real(c_float)     :: hsw_frontend_latency
        end function hsw_frontend_latency     
        
     end interface

     interface

        function hsw_icache_misses(icache_ifdata_stall, &
                                   clks) &
                                   bind(c,name="hsw_icache_misses")
                integer(c_size_t) :: icache_ifdata_stall
                integer(c_size_t) :: clks
                real(c_float)     :: hsw_icache_misses
        end function hsw_icache_misses
        
     end interface

     interface

        function hsw_dsb_switches( dsb2mite_switches_penalty_cycles, &
                                   clks) &
                                   bind(c,name="hsw_dsb_switches")
                integer(c_size_t) :: dsb2mite_switches_penalty_cycles
                integer(c_size_t) :: clks
                real(c_float)     :: gsw_dsb_switches
        end function hsw_dsb_switches
        
     end interface

     interface


        function hsw_lcp( ild_stall_lcp, &
                          clks) &
                          bind(c,name="ild_stall_lcp")
               integer(c_size_t) :: ild_stall_lcp
               integer(c_size_t) :: clks
               real(c_float)     :: hsw_lcp
        end function hsw_lcp
        
     end interface


     interface

        function hsw_ms_switches( idq_ms_switches,  &
                                  clks) &
                                  bind(c,name="hsw_ms_switches")
               integer(c_size_t) :: idq_ms_switches
               integer(c_size_t) :: clks
               real(c_float)     :: hsw_ms_switches
        end function hsw_ms_switches
        
     end interface

     interface

        function hsw_branch_resteers(  br_misp_retired_all_branches, &
                                       machine_clears_count,   &
			               baclears_any,     &
			               clks) &
                                       bind(c,name="hsw_branch_resteers")
                integer(c_size_t) :: br_misp_retired_all_branches
                integer(c_size_t) :: machine_clears_count
                integer(c_size_t) :: baclears_any
                integer(c_size_t) :: clks
                real(c_float)     :: hsw_branch_resteers
        end function hsw_branch_resteers
        
     end interface

     interface

          function hsw_mite(  idq_all_mite_cycles_any_uops, &
                              idq_all_mite_cycles_4_uops,   &
                              core_clks) &
                              bind(c,name="hsw_mite")
                integer(c_size_t) :: idq_all_mite_cycles_any_uops
                integer(c_size_t) ::   idq_all_mite_cycles_4_uops
                integer(c_size_t) :: core_clks
                real(c_float)     :: hsw_mite
          end function hsw_mite
  
       end interface

     interface

         function hsw_dsb( idq_all_dsb_cycles_any_uops, &
                           idq_all_dsb_cycles_4_uops,   &
                           core_clks) &
                           bind(c,name="hsw_dsb")
              integer(c_size_t) :: idq_all_dsb_cycles_any_uops
              integer(c_size_t) :: idq_all_dsb_cycles_4_uops
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_dsb
         end function hsw_dsb
        
      end interface

      interface

         function hsw_l1_bound(  stalls_mem_any, &
                                 cycles_activity_stalls_l1d_pending, &
			         clks) &
                                 bind(c,name="hsw_l1_bound")
               integer(c_size_t) :: stalls_mem_any
               integer(c_size_t) :: cycles_activity_stalls_l1d_pending
               integer(c_size_t) :: clks
               real(c_float)     :: hsw_l1_bound
         end function hsw_l1_bound

      end interface

      interface

          function hsw_dtlb_load(  dtlb_load_misses_stlb_hit, &
                                   dtlb_load_misses_walk_duration, &
			           clks) &
                                   bind(c,name="hsw_dtlb_load")
            integer(c_size_t) :: dtlb_load_misses_stlb_hit
            integer(c_size_t) :: dtlb_load_misses_walk_duration
            integer(c_size_t) :: clks
            real(c_float)     :: hsw_dtlb_load
          end function hsw_dtlb_load

       end interface

       interface

          function hsw_store_fwd_blk( ld_blocks_store_forward, &
                                      clks) &
                                      bind(c,name="hsw_store_fwd_blk")
            integer(c_size_t) :: ld_blocks_store_forward
            integer(c_size_t) :: clks
            real(c_float)     :: hsw_store_fwd_blk
          end function hsw_store_fwd_blk
          
       end interface

       interface

           function hsw_split_loads(  load_miss_real_latency, &
                                      ld_blocks_no_sr, &
			              clks) &
                                      bind(c,name="hsw_split_loads")
                  integer(c_size_t) :: load_misses_real_latency
                  integer(c_size_t) :: ld_blocks_no_sr
                  integer(c_size_t) :: clks
                  real(c_float)     :: hsw_split_loads
           end function hsw_split_loads
           
        end interface

        interface

           function hsw_single_mul_clks( uops_issued_single_mul, &
                                         clks) &
                                         bind(c,name="hsw_single_mul_clks")
                    integer(c_size_t) :: uops_issued_single_mul
                    integer(c_size_t) :: clks
                    real(c_float)     :: hsw_single_mul_clks
            end function hsw_single_mul_clks
         end interface

           interface

           function hsw_single_mul_core_clks( uops_issued_single_mul, &
                                         core_clks) &
                                         bind(c,name="hsw_single_mul_core_clks")
                    integer(c_size_t) :: uops_issued_single_mul
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: hsw_single_mul_core_clks
            end function hsw_single_mul_core_clks
         end interface

           interface

           function hsw_single_mul_uops_any( uops_issued_single_mul, &
                                            inst_issued_any) &
                                         bind(c,name="hsw_single_mul_uops_any")
                    integer(c_size_t) :: uops_issued_single_uops_any
                    integer(c_size_t) :: inst_issued_any
                    real(c_float)     :: hsw_single_mul_uops_any
            end function hsw_single_mul_uops_any
         end interface

           interface

           function hsw_single_mul_uops_retired_any( uops_issued_single_mul, &
                                                     uops_retired_any) &
                                         bind(c,name="hsw_single_mul_uops_retired_any")
                    integer(c_size_t) :: uops_issued_single_mul
                    integer(c_size_t) :: uops_retired_any
                    real(c_float)     :: hsw_single_mul_uops_retired_any
            end function hsw_single_mul_uops_retired_any
         end interface


         interface

              function hsw_simd_move_elim_not_elim(  move_elimination_simd_eliminated,  &
                                                     move_elimination_simd_not_eliminated) &
                                                     bind(c,name="hsw_simd_move_elim_not_elim")
                      integer(c_size_t)  :: move_elimimination_simd_eliminated
                      integer(c_size_t)  :: move_elimimination_simd_not_eliminated
                      real(c_float)      :: hsw_simd_move_elim_not_elim
              end function hsw_simd_move_elim_not_elim

         end interface

           
         interface

              function hsw_int_move_elim_not_elim(  move_elimination_int_eliminated,  &
                                                     move_elimination_int_not_eliminated) &
                                                     bind(c,name="hsw_int_move_elim_not_elim")
                      integer(c_size_t)  :: move_elimimination_int_eliminated
                      integer(c_size_t)  :: move_elimimination_int_not_eliminated
                      real(c_float)      :: hsw_int_move_elim_not_elim
              end function hsw_int_move_elim_not_elim

         end interface


         interface

            function hsw_uops_issued_any_mite_uops( idq_mite_uops, &
                                                   uops_issued_any) &
                                                   bind(c,name="hsw_uops_issued_any_mite_uops")
                    integer(c_size_t) :: idq_mite_uops
                    integer(c_size_t) :: uops_issued_any
                    real(c_float)     :: hsw_uops_issued_any_mite_uops
            end function hsw_uops_issued_any_mite_uops
              
         end interface

         interface

            function hsw_single_mul_avx_inst_all( uops_issued_any, &
                                                  avx_inst_all) &
                                                  bind(c,name="hsw_single_mul_avx_inst_all")
                       integer(c_size_t) :: uops_issued_any
                       integer(c_size_t) :: avx_inst_all
                       real(c_float)     :: hsw_single_mul_avx_inst_all
            end function hsw_single_mul_avx_inst_all
            
        end interface


        interface

           function hsw_frontend_latency( frontend_latency_cycles, &
                                          slots) &
                                          bind(c,name="hsw_frontend_latency")
                   integer(c_size_t) :: frontend_latency_cycles
                   integer(c_size_t) :: slots
                   real(c_float)     :: hsw_frontend_latency
           end function hsw_frontend_latency
            
        end interface

        interface

            function hsw_branch_resteers(  br_misp_retired_all_branches, &
                                           machine_clears_count,&
			                   baclears_any,&
				           clks) &
                                           bind(c,name="nsw_branch_resteers")
                      integer(c_size_t) :: br_misp_retired_all_branches
                      integer(c_size_t) :: machine_clears_count
                      integer(c_size_t) :: baclears_any
                      integer(c_size_t) :: clks
                      real(c_float)     :: hsw_branch_resteers
             end function hsw_branch_resteers
           
       end interface


       interface

          function hsw_frontend_bw( frontend_bound, &
                                    frontend_latency) &
                                    bind(c,name="hsw_frontend_bw")
                    real(c_float) :: frontend_bound
                    real(c_float) :: frontend_latency
                    real(c_float) :: hsw_frontend_bw
          end function hsw_frontend_bw
          
       end interface


       interface

            function hsw_mite(  idq_all_mite_cycles_any_uops, &
                                idq_all_mite_cycles_4_uops, &
		                clks) &
                                bind(c,name="hsw_mite")
                   integer(c_size_t) :: idq_all_mite_cycles_any_uops
                   integer(c_size_t) :: idq_all_mite_cycles_4_uops
                   integer(c_size_t) :: clks
                   real(c_float)     :: hsw_mite
            end function hsw_mite
          
        end interface

         
        interface

           function hsw_store_fwd_blocked( ld_blocks_store_forward, &
                                           clks) &
                                           bind(c,name="hsw_store_fwd_blocked")
                  integer(c_size_t) :: ld_blocks_store_forward
                  integer(c_size_t) :: clks
                  real(c_float)     :: hsw_store_fwd_blocked
           end function hsw_store_fwd_blocked

        end interface


        interface

            function hsw_lock_latency(   mem_lock_st_fraction, &
                                         oro_demand_rfo_c1,&
			                 clks) &
                                         bind(c,name="hsw_lock_latency")
                  real(c_float) :: mem_lock_st_fraction
                  integer(c_size_t) :: oro_demand_rfo_c1
                  integer(c_size_t) :: clks
                  real(c_float)     :: hsw_lock_latency
            end function hsw_lock_latency
            
       end interface

       interface

            function hsw_split_loads(   load_miss_real_latency, &
                                        load_blocks_no_sr,&
                                        clks) &
                                        bind(c,name="hsw_split_loads")
                 real(c_float) :: load_miss_real_latency
                 integer(c_size_t) :: load_blocks_no_sr
                 integer(c_size_t) :: clks
                 real(c_float)     :: hsw_split_loads
            end function hsw_split_loads
          
       end interface


      interface

           function hsw_4k_aliasing(  ld_block_partial_address_alias, &
                                      clks) &
                                      bind(c,name="hsw_4k_aliasing")
                 integer(c_size_t) :: ld_block_partial_address_alias
                 integer(c_size_t) :: clks
                 real(c_float)     :: hsw_4k_aliasing
           end function hsw_4k_aliasing
         
      end interface

      interface

          function hsw_fb_full(  load_miss_real_latency, &
                                 l1d_pend_miss_request_fb_full_c1, &
			         clks) &
                                 bind(c,name="hsw_fb_full")
                  real(c_float) :: load_miss_real_latency
                  integer(c_size_t) :: l1d_pend_miss_request_fb_full_c1
                  integer(c_size_t) :: clks
                  real(c_float)     :: hsw_fb_full
          end function hsw_fb_full

      end interface

      interface

             function hsw_l2_bound( cycle_activity_stalls_l1d_pending, &
                                    cycle_activity_stalls_l2_pending,&
			            clks) &
                                    bind(c,name="hsw_l2_bound")
                  integer(c_size_t) :: cycle_activity_stalls_l1d_pending
                  integer(c_size_t) :: cycle_activity_stalls_l2_pending
                  integer(c_size_t) :: clks
                  real(c_float)     :: hsw_l2_bound
             end function hsw_l2_bound
             
      end interface

      interface

          function hsw_l3_bound(  mem_l3_hit_fraction,&
                                  cycle_activity_stalls_l2_pending,&
                                  clks) &
                                  bind(c,name="hsw_l3_bound")
                 real(c_float) :: mem_l3_hit_fraction
                 integer(c_size_t) :: cycle_activity_stalls_l2_pending
                 integer(c_size_t) :: clks
                 real(c_float)     :: hsw_l3_bound
          end function hsw_l3_bound
          
     end interface

     interface

         function hsw_port0_exec(  uops_dispatched_port_port0,&
                                  core_clks) &
                                  bind(c,name="hsw_port0_exec")
              integer(c_size_t) :: uops_dispatched_port_port0
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port0_exec
         end function hsw_port0_exec
         
     end interface


     interface

         function hsw_port1_exec(  uops_dispatched_port_port1,&
                                  core_clks) &
                                  bind(c,name="hsw_port1_exec")
              integer(c_size_t) :: uops_dispatched_port_port1
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port1_exec
         end function hsw_port1_exec
         
     end interface

     interface

         function hsw_port5_exec(  uops_dispatched_port_port5,&
                                  core_clks) &
                                  bind(c,name="hsw_port5_exec")
              integer(c_size_t) :: uops_dispatched_port_port5
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port5_exec
         end function hsw_port5_exec
         
     end interface

    interface

         function hsw_port6_exec(  uops_dispatched_port_port6,&
                                 core_clks) &
                                  bind(c,name="hsw_port6_exec")
              integer(c_size_t) :: uops_dispatched_port_port6
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port6_exec
         end function hsw_port6_exec
         
     end interface


     interface

         function hsw_port3_exec(  uops_dispatched_port_port3,&
                                  core_clks) &
                                  bind(c,name="hsw_port3_exec")
              integer(c_size_t) :: uops_dispatched_port_port3
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port3_exec
         end function hsw_port3_exec
         
     end interface

      
     interface

         function hsw_port4_exec(  uops_dispatched_port_port4,&
                                  core_clks) &
                                  bind(c,name="hsw_port4_exec")
              integer(c_size_t) :: uops_dispatched_port_port4
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port4_exec
         end function hsw_port4_exec
         
     end interface

     interface

         function hsw_port7_exec(  uops_dispatched_port_port7,&
                                  core_clks) &
                                  bind(c,name="hsw_port7_exec")
              integer(c_size_t) :: uops_dispatched_port_port7
              integer(c_size_t) :: core_clks
              real(c_float)     :: hsw_port7_exec
         end function hsw_port7_exec
         
     end interface 

 end module mod_hsw_tma_bindings
