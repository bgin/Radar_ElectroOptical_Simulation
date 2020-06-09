

module mod_skx_tma_bindings



 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_skx_tma_bindings'
 !          
 !          Purpose:
 !                    Fortran bindings Skylake server TMA Metrics (C)
 !                   
 !                     
 !          History:
 !                        Date: 07-06-2020
 !                        Time: 10:02 GMT+2
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
    
   
    integer(kind=int4),   parameter :: MOD_SKX_TMA_BINDINGS_MAJOR = 1
    integer(kind=int4),   parameter :: MOD_SKX_TMA_BINDINGS_MINOR = 0
    integer(kind=int4),   parameter :: MOD_SKX_TMA_BINDINGS_MICRO = 0
    integer(kind=int4),   parameter :: MOD_SKX_TMA_BINDINGS_FULLVER =
        1000*MOD_SKX_TMA_BINDINGS_MAJOR+100*MOD_SKX_TMA_BINDINGS_MINOR+ &
                 10*MOD_SKX_TMA_BINDINGS_MICRO
    character(*),         parameter :: MOD_SKX_TMA_BINDINGS_CREATE_DATE = "07-06-2020 10:02 +00200 (SUN 07 JUN 2020 GMT+2)"
    character(*),         parameter :: MOD_SKX_TMA_BINDINGS_BUILD_DATE  = __DATE__ " " __TIME__
    character(*),         parameter :: MOD_SKX_TMA_BINDINGS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    character(*),         parameter :: MOD_SKX_TMA_BINDINGS_SYNOPSIS    = "Fortran bindings Skylake server TMA Metrics (C)"

    interface

       function skx_uops_fetched( idq_dsb_uops, &
                                  idq_mite_uops, &
                                  idq_ms_uops) &
                                  bind(c,name="skx_uops_fetched")
               integer(c_size_t) :: idq_dsb_uops
               integer(c_size_t) :: idq_mite_uops
               integer(c_size_t) :: idq_ms_uops
               integer(c_size_t) :: skx_uops_fetched
       end function 

    end interface

    interface

         function skx_recovery_cycles( int_misc_recovery_cycles_any, &
                                       int_misc_recovery_cycles,&
				       is_ht_enabled) &
                                       bind(c,name="skx_recovery_cycles")
                 integer(c_size_t) :: int_misc_recovery_cycles_any
                 integer(c_size_t) :: int_misc_recovery_cycles
                 integer(c_bool)   :: is_ht_enabled
                 real(c_float)     :: skx_recovery_cycles
         end function 
       
    end interface

      
    interface

        function skx_executed_cycles( uops_executed_core_cycles_ge_1,&
                                      is_ht_enabled) &
                                      bind(c,name="skx_executed_cycles")
               integer(c_size_t) :: uops_executed_core_cycles_ge_1
               integer(c_bool)   :: is_ht_enabled
               real(c_float)     :: skx_executed_cycles
        end function skx_executed_cycles
        
    end interface

     
    interface

         function skx_sq_full_cycles( offcore_requests_buffer_sq_full, &
                                      is_ht_enabled) &
                                      bind(c,name="skx_sq_full_cycles")
                integer(c_size_t) :: offcore_requests_buffer_sq_full
                integer(c_bool)   :: is_hit_enabled
                real(c_float)     :: skx_sq_full_cycles
         end function skx_sq_full_cycles
         
        
     end interface

      
     interface

         function skx_cycles_0_ports_utilized(  uops_executed_core_cycles_none, &
                                                exe_activity_exe_bound_0_ports, &
				                is_ht_enabled) &
                                                bind(c,name="skx_cycles_0_ports_utilized")
                integer(c_size_t) ::  uops_executed_core_cycles_none
                integer(c_size_t) ::   exe_activity_exe_bound_0_ports
                integer(c_bool)   :: is_ht_enabled
                real(c_float)     :: skx_cycles_0_ports_utilized
         end function skx_cycles_0_ports_utilized
         

     end interface

      
     interface

           function skx_cycles_1_ports_utilized(  uops_executed_core_cycles_ge_1, &
                                                  uops_executed_core_cycles_ge_2,&
				                  exe_activity_ports_util,&
				                  is_ht_enabled) &
                                                  bind(c,name="skx_cycles_1_ports_utilized")
                   integer(c_size_t) :: uops_executed_core_cycles_ge_1
                   integer(c_size_t) :: uops_executed_core_cycles_ge_2
                   integer(c_size_t) :: exe_activity_ports_util
                   integer(c_bool)   :: is_ht_enabled
                   real(c_float)     :: skx_cycles_1_ports_utilized
           end function skx_cycles_1_ports_utilized
           
         
     end interface


     interface

           function skx_cycles_2_ports_utilized(  uops_executed_core_cycles_ge_2, &
                                                  uops_executed_core_cycles_ge_3,&
				                  exe_activity_ports_util,&
				                  is_ht_enabled) &
                                                  bind(c,name="skx_cycles_2_ports_utilized")
                   integer(c_size_t) :: uops_executed_core_cycles_ge_2
                   integer(c_size_t) :: uops_executed_core_cycles_ge_3
                   integer(c_size_t) :: exe_activity_ports_util
                   integer(c_bool)   :: is_ht_enabled
                   real(c_float)     :: skx_cycles_2_ports_utilized
           end function skx_cycles_2_ports_utilized
           
         
     end interface


     interface

        function skx_cycles_3m_ports_utilized( uops_executed_core_cycles_ge_3, &
                                               is_ht_enabled) &
                                               bind(c,name="skx_cycles_3m_ports_utilized")
                 integer(c_size_t) :: uops_executed_core_cycles_ge_3
                 integer(c_bool)   :: is_ht_enabled
                 real(c_float)     :: skx_cycles_3m_ports_utilized
        end function skx_cycles_3m_ports_utilized
        
        
     end interface

     interface

           function skx_oro_drd_any_cycles( cpu_clk_unhalted_thread, &
                                            offcore_requests_outstanding_cycles_with_data_rd) &
                                            bind(c,name="skx_oro_drd_any_cycles")
                      integer(c_size_t) :: cpu_clk_unhalted_thread
                      integer(c_size_t) :: offcore_requests_outstanding_cycles_with_data_rd
                      integer(c_size_t) :: skx_oro_drd_any_cycles
           end function skx_oro_drd_any_cycles
           
             
  
     end interface

     interface

          function skx_oro_drd_bw_cycles( cpu_clk_unhalted_thread, &
                                           offcore_requests_outstanding_all_data_rd_c4) &
                                           bind(c,name="skx_oro_drd_bw_cycles")
                    integer(c_size_t) :: cpu_clk_unhalted_thread
                    integer(c_size_t) ::  offcore_requests_outstanding_all_data_rd_c4
                    integer(c_size_t) :: skx_oro_drd_bw_cycles
          end function skx_oro_drd_bw_cycles
          

     end interface

     interface

          function skx_oro_demand_rfo_c1(  cpu_clk_unhalted_thread, &
                                         offcore_requests_outstanding_cycles_with_demand_rfo) &
                                         bind(c,name="skx_oro_demand_rfo_c1")
                    integer(c_size_t) :: cpu_clk_unhalted_thread
                    integer(c_size_t) ::  offcore_requests_outstanding_cycles_with_demand_rfo
                    integer(c_size_t) :: skx_oro_demand_rfo_c1
          end function skx_oro_demand_rfo_c1
        
      end interface

       
      interface

         function skx_store_l2_hit_cycles( l2_rqsts_rfo_hit, &
                                           mem_lock_st_fraction) &
                                           bind(c,name="skx_store_l2_hit_cycles")
                  integer(c_size_t) :: l2_rqsts_rfo_hit
                  real(c_float)     :: mem_lock_st_fraction
                  real(c_float)     :: skx_store_l2_hit_cycles
         end function skx_store_l2_hit_cycles
         
         
      end interface

      interface

           function skx_load_l2_hit( mem_load_retired_l2_hit, &
                                     mem_load_retired_fb_hit, &
			             mem_load_retired_l1_miss) &
                                     bind(c,name="skx_load_l2_hit")
                  integer(c_size_t) :: mem_load_retired_l2_hit
                  integer(c_size_t) :: mem_load_retired_fb_hit
                  integer(c_size_t) :: mem_load_retired_l1_miss
                  real(c_float)     :: skx_load_l2_hit
           end function skx_load_l2_hit
           

       end interface

        
       interface

           function skx_load_l3_hit(  mem_load_retired_l3_hit,&
                                      mem_load_retired_fb_hit,&
			              mem_load_retired_l1_miss) &
                                      bind(c,name="skx_load_l3_hit")
                  integer(c_size_t) :: mem_load_retired_l3_hit
                  integer(c_size_t) :: mem_load_retired_fb_hit
                  integer(c_size_t) :: mem_load_retired_l1_miss
                  real(c_float)     :: skx_load_l3_hit
           end function skx_load_l3_hit
           
          
        end interface


        interface

           function skx_load_xsnp_hit( mem_load_l3_hit_retired_xsnp_hit, &
                                       mem_load_l3_hit_retired_xsnp_hitm,&
			               mem_load_retired_fb_hit,&
			               mem_load_retired_l1_miss) &
                                       bind(c,name="skx_load_xsnp_hit")
                 integer(c_size_t) :: mem_load_l3_hit_retired_xsnp_hit
                 integer(c_size_t) :: mem_load_l3_hit_retired_xsnp_hitm
                 integer(c_size_t) :: mem_load_retired_fb_hit
                 integer(c_size_t) :: mem_load_retired_l1_miss
                 real(c_float)     :: skx_load_xsnp_hit
           end function skx_load_xsnp_hit
           
           
        end interface


       interface

             function skx_load_xsnp_hitm( mem_load_l3_hit_retired_xsnp_hitm, &
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss,&
			                  true_xsnp_hit_fraction) &
                                          bind(c,name="skx_load_xsnp_hitm")
                    integer(c_size_t) :: mem_load_l3_hit_retired_xsnp_hitm
                    integer(c_size_t) :: mem_load_retired_fb_hit
                    integer(c_size_t) :: mem_load_retired_l1_miss
                    real(c_float)     :: true_xsnp_hit_fraction
                    real(c_float)     :: skx_load_xsnp_hitm
             end function skx_load_xsnp_hitm
             
          
       end interface

          
       interface

             function skx_load_xsnp_miss( mem_load_l3_hit_retired_xsnp_hitm, &
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss)&
			                  bind(c,name="skx_load_xsnp_miss")
                    integer(c_size_t) :: mem_load_l3_hit_retired_xsnp_hitm
                    integer(c_size_t) :: mem_load_retired_fb_hit
                    integer(c_size_t) :: mem_load_retired_l1_miss
                    real(c_float)     :: skx_load_xsnp_miss
             end function skx_load_xsnp_miss
             
          
        end interface


        interface


           function skx_load_local_miss(  mem_load_l3_miss_retired_local_dram,&
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss) &
                                          bind(c,name="skx_load_local_miss")
                integer(c_size_t) ::  mem_load_l3_miss_retired_local_dram
                integer(c_size_t) :: mem_load_retired_fb_hit
                integer(c_size_t) :: mem_load_retired_l1_miss
                real(c_float)     :: skx_load_local_miss
           end function skx_load_local_miss
           
            

        end interface

 
        interface


           function skx_load_remote_miss(  mem_load_l3_miss_retired_remote_dram,&
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss) &
                                          bind(c,name="skx_load_remote_miss")
                integer(c_size_t) ::  mem_load_l3_miss_retired_remote_dram
                integer(c_size_t) :: mem_load_retired_fb_hit
                integer(c_size_t) :: mem_load_retired_l1_miss
                real(c_float)     :: skx_load_local_miss
           end function skx_load_remote_miss
           
            

        end interface

        interface


           function skx_load_remote_hitm(  mem_load_l3_miss_retired_remote_hitm,&
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss) &
                                          bind(c,name="skx_load_remote_hitm")
                integer(c_size_t) ::  mem_load_l3_miss_retired_remote_hitm
                integer(c_size_t) :: mem_load_retired_fb_hit
                integer(c_size_t) :: mem_load_retired_l1_miss
                real(c_float)     :: skx_load_remote_hitm
           end function skx_load_remote_hitm
           
            

        end interface

       interface


           function skx_load_remote_forward(  mem_load_l3_miss_retired_remote_fwd,&
                                          mem_load_retired_fb_hit,&
			                  mem_load_retired_l1_miss) &
                                          bind(c,name="skx_load_remote_forward")
                integer(c_size_t) ::  mem_load_l3_miss_retired_remote_fwd
                integer(c_size_t) :: mem_load_retired_fb_hit
                integer(c_size_t) :: mem_load_retired_l1_miss
                real(c_float)     :: skx_load_remote_forward
           end function skx_load_remote_forward
           
            

        end interface

        interface

           function skx_uops_executed_threshold( exe_activity_2_ports_util, &
                                                 upc) &
                                                 bind(c,name="skx_uops_executed_threshold")
                   integer(c_size_t) :: exe_activity_2_ports_util
                   real(c_float)     :: upc
                   real(c_float)     :: skx_uops_executed_threshold
           end function skx_uops_executed_threshold
           
        end interface

        interface

            function skx_core_bound_cycles(  exe_activity_exe_bound_0_ports, &
                                             exe_activity_1_ports_util,&
				             uops_executed_threshold) &
                                             bind(c,name="skx_core_bound_cycles")
                  integer(c_size_t) :: exe_activity_exe_bound_0_ports
                  integer(c_size_t) :: exe_activity_1_ports_util
                  real(c_float)     :: uops_executed_threshold
                  real(c_float)     :: skx_core_bound_cycles
            end function skx_core_bound_cycles

        end interface

       interface

          function skx_backend_bound_cycles( core_bound_cycles, &
                                             cycles_activity_stalls_mem_any, &
                                             exe_activity_bound_on_stores) &
                                             bind(c,name="skx_backend_bound_cycles")
                real(c_float)     :: core_bound_cycles
                integer(c_size_t) :: cycles_activity_stalls_mem_any
                integer(c_size_t) :: exe_activity_bound_on_stores
                real(c_float)     :: skx_backend_bound_cycles
          end function skx_backend_bound_cycles

       end interface

       interface

          function skx_memory_bound_fraction(  cycles_activity_stalls_mem_any, &
                                               exe_activity_bound_on_stores,&
				               backend_bound_cycles) &
                                               bind(c,name="skx_memory_bound_fraction")
                integer(c_size_t) ::  cycles_activity_stalls_mem_any
                integer(c_size_t) ::  exe_activity_bound_on_stores
                real(c_float)     ::  backend_bound_cycles
                real(c_float)     :: skx_memory_bound_fraction
          end function skx_memory_bound_fraction
          

       end interface

       interface

          function skx_l2_bound_ratio(  cycles_activity_stalls_l1d_miss,&
                                        cycles_activity_stalls_l2_miss,&
			                clks) &
                                        bind(c,name="skx_l2_bound_ratio")
                  integer(c_size_t) :: cycles_activity_stalls_l1d_miss
                  integer(c_size_t) :: cycles_activity_stalls_l2_miss
                  integer(c_size_t) :: clks
                  real(c_float)     :: skx_l2_bound_ratio
          end function skx_l2_bound_ratio
          

       end interface

       interface

           function skx_mem_bound_ratio(   cycles_activity_stalls_l3_miss,&
                                           clks,&
			                   l2_bound_ratio,&
			                   l2_bound) &
                                           bind(c,name="skx_mem_bound_ratio")
                   integer(c_size_t) :: cycles_activity_stalls_l3_miss
                   integer(c_size_t) :: clks
                   real(c_float)     :: l2_bound_ratio
                   real(c_float)     :: l2_bound
                   real(c_float)     :: skx_mem_bound_ratio
           end function skx_mem_bound_ratio
           
          
      end interface

      interface

         function skx_mem_lock_st_fraction(  mem_inst_retired_lock_loads, &
                                             mem_inst_retired_all_stores) &
                                             bind(c,name="skx_mem_lock_st_fraction")
                 integer(c_size_t) ::  mem_inst_retired_lock_loads
                 integer(c_size_t) ::  mem_inst_retired_all_stores
                 real(c_float)     :: skx_mem_lock_st_fraction
         end function skx_mem_lock_st_fraction
         
         
      end interface

      interface

          function skx_mispredict_clears( br_misp_retired_all_branches, &
                                          machine_clears_count) &
                                          bind(c,name="skx_mispredict_clears")
                  integer(c_size_t) :: br_misp_retired_all_branches
                  integer(c_size_t) :: machine_clears_count
                  real(c_float)     :: skx_mispredict_clears
          end function skx_mispredict_clears
          
         
     end interface

     interface

          function skx_retired_uops_fraction(uops_retired_retired_slots, &
                                             uops_issued_any) &
                                             bind(c,name="skx_retired_uops_fraction")
                   integer(c_size_t) :: uops_retired_retired_slots
                   integer(c_size_t) :: uops_issued_any
                   real(c_float)     :: skx_retired_uops_fraction
          end function skx_retired_uops_fraction
          

     end interface

       
     interface

         function skx_xsnp_hitm_fraction(  offcore_response_demand_data_rd_l3_hit_hitm_other,&
                                           offcore_response_demand_data_rd_l3_hit_snoop_hit_with_fwd) &
                                           bind(c,name="skx_xsnp_hitm_fraction")
                  integer(c_size_t) :: offcore_response_demand_data_rd_l3_hit_hitm_other
                  integer(c_size_t) ::  offcore_response_demand_data_rd_l3_hit_snoop_hit_with_fwd
                  real(c_float)     :: skx_xsnp_hitm_fraction
         end function skx_xsnp_hitm_fraction
        
     end interface

     interface

            function skx_all_rfo_l3_hit_snoop_hitm(  offcore_response_demand_rfo_l3_hit_hitm_other_core, &
                                                     offcore_response_pf_l2_rfo_l3_hit_hitm_other_core) &
                                                     bind(c,name="skx_all_rfo_l3_hit_snoop_hitm")
                   integer(c_size_t) ::  offcore_response_demand_rfo_l3_hit_hitm_other_core
                   integer(c_size_t) ::   offcore_response_pf_l2_rfo_l3_hit_hitm_other_core
                   real(c_float)     ::  skx_all_rfo_l3_hit_snoop_hitm
            end function skx_all_rfo_l3_hit_snoop_hitm
            

      end interface

      interface

         function skx_retired_uops_cycle( uops_retired_retired_slots, &
                                          clks) &
                                          bind(c,name="skx_retired_uops_cycle")
                  integer(c_size_t) :: uops_retired_retired_slots
                  integer(c_size_t) :: clks
                  real(c_float)     :: skx_retired_uops_cycle
         end function skx_retired_uops_cycle
         

      end interface


      interface

          function skx_uops_per_inst(  uops_retired_retired_slots, &
                                       inst_retired_any) &
                                       bind(c,name="skx_uops_per_inst")
                   integer(c_size_t) :: uops_retired_retired_slots
                   integer(c_size_t) :: inst_retired_any
                   real(c_float)     :: skx_uops_per_inst
          end function skx_uops_per_inst
          
       end interface

       interface

          function skx_instr_per_clock( inst_retired_any, &
                                        clks) &
                                        bind(c,name="skx_instr_per_clock")
                  integer(c_size_t) :: inst_retired_any
                  integer(c_size_t) :: clks
                  real(c_float)     :: skx_instr_per_clock
          end function skx_instr_per_clock
          

       end interface

end module mos_skx_tma_bindings
