

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

          function skx_instr_per_cycle( inst_retired_any, &
                                        clks) &
                                        bind(c,name="skx_instr_per_cycle")
                  integer(c_size_t) :: inst_retired_any
                  integer(c_size_t) :: clks
                  real(c_float)     :: skx_instr_per_cycle
          end function skx_instr_per_cycle
          

       end interface

       interface

           function skx_instr_branch_taken(inst_retired_any,&
                                          br_inst_retired_near_taken )&
                                          bind(c,name="skx_instr_branch_taken")
                 integer(c_size_t) :: inst_retired_any
                 integer(c_size_t) ::  br_inst_retired_near_taken
                 real(c_float)     :: skx_instr_branch_taken
           end function skx_instr_branch_taken

       end interface

       interface

          function skx_cycles_per_inst(ipc) &
               bind(c,name="skx_cycles_per_inst")
            real(c_float) :: ipc
            real(c_float) :: skx_cycles_per_inst
          end function
          
       end interface


       interface

              function skx_instr_per_load(inst_retired_any, &
                                          mem_inst_retired_all_loads) &
                                          bind(c,name="skx_instr_per_load")
                     integer(c_size_t) :: instr_retired_any
                     integer(c_size_t) ::  mem_inst_retired_all_loads
                     real(c_float)     :: skx_instr_per_load
              end function skx_instr_per_load
              
        end interface


      interface

           function skx_instr_per_store(inst_retired_any, &
                                        mem_inst_retired_all_stores) &
                                          bind(c,name="skx_instr_per_store")
                     integer(c_size_t) :: instr_retired_any
                     integer(c_size_t) ::  mem_inst_retired_all_stores
                     real(c_float)     :: skx_instr_per_store
              end function skx_instr_per_store
           
     end interface


     interface

         function skx_instr_per_branch( inst_retired_any, &
                                        br_inst_retired_all_branches) &
                                        bind(c,name="skx_instr_per_branch")
                  integer(c_size_t) :: inst_retired_any
                  integer(c_size_t) ::   br_inst_retired_all_branches
                  real(c_float)     :: skx_instr_per_branch
         end function skx_instr_per_branch
         

      end interface

      interface

         function skx_instr_per_call(  inst_retired_any, &
                                       br_inst_retired_near_call) &
                                       bind(c,name="skx_instr_per_call")
                  integer(c_size_t) :: inst_retired_any
                  integer(c_size_t) :: br_inst_retired_near_call
                  real(c_float)     :: skx_instr_per_call
         end function skx_instr_per_call
         

      end interface

      interface

          function skx_br_inst_per_taken_br( br_inst_retired_all_branches, &
                                             br_inst_retired_near_taken) &
                                             bind(c,name="skx_br_inst_per_taken")
                   integer(c_size_t) ::  br_inst_retired_all_branche
                   integer(c_size_t) ::  br_inst_retired_near_taken
                   real(c_float)     :: skx_br_inst_per_taken_br
          end function skx_br_inst_per_taken_br
          

       end interface

       interface

           function skx_flops(  fp_arith_inst_retired_scalar_single,&
                     fp_arith_inst_retired_scalar_double,&
		     fp_arith_inst_retired_128B_packed_double,&
		     fp_arith_inst_retired_128B_packed_single,&
		      fp_arith_inst_retired_256B_packed_double,&
		     fp_arith_inst_retired_256B_packed_single,&
		     fp_arith_inst_retired_512B_packed_double,&
		     fp_arith_inst_retired_512B_packed_single) &
                     bind(c,name="skx_flops")
              integer(c_size_t) :: fp_arith_inst_retired_scalar_single
              integer(c_size_t) :: fp_arith_inst_retired_scalar_double
              integer(c_size_t) ::  fp_arith_inst_retired_128B_packed_double
              integer(c_size_t) ::  fp_arith_inst_retired_128B_packed_single
              integer(c_size_t) ::  fp_arith_inst_retired_256B_packed_double
              integer(c_size_t) ::  fp_arith_inst_retired_256B_packed_single
              integer(c_size_t) ::  fp_arith_inst_retired_512B_packed_double
              integer(c_size_t) ::  fp_arith_inst_retired_512B_packed_single
              integer(c_size_t)     :: skx_flops
           end function skx_flops
           
        end interface

        interface

           function skx_instr_per_flop( instr_retired_any, &
                                       flops) &
                                       bind(c,name="skx_instr_per_flop")
                   integer(c_size_t) :: instr_retired_any
                   integer(c_size_t) :: flops
                   real(c_float)     :: skx_instr_per_flop
           end function skx_instr_per_flop
           

        end interface


        interface

            function skx_instr_per_scalar_fp_sp( instr_retired_any,&
                                                fp_arith_instr_retired_scalar_single) &
                                                bind(c,name="skx_instr_per_scalar_fp_sp")
                          integer(c_size_t) :: instr_retired_any
                          integer(c_size_t) :: fp_arith_instr_retired_scalar_single
                          real(c_float)     :: skx_instr_per_scalar_fp_sp
            end function skx_instr_per_scalar_fp_sp
            
    
         end interface

         interface

            function skx_instr_per_scalar_fp_dp( instr_retired_any,&
                                                fp_arith_instr_retired_scalar_double) &
                                                bind(c,name="skx_instr_per_scalar_fp_dp")
                          integer(c_size_t) :: instr_retired_any
                          integer(c_size_t) :: fp_arith_instr_retired_scalar_double
                          real(c_float)     :: skx_instr_per_scalar_fp_dp
            end function skx_instr_per_scalar_fp_dp
            
    
         end interface

         interface

              function skx_instr_per_avx128(  instr_retired_any,&
                                              fp_arith_instr_retired_128B_packed_single,&
			                      fp_arith_instr_retired_128B_packed_double) &
                                              bind(c,name="skx_instr_per_avx128")
                     integer(c_size_t) :: instr_retired_any
                     integer(c_size_t) ::  fp_arith_instr_retired_128B_packed_single
                     integer(c_size_t) ::  fp_arith_instr_retired_128B_packed_double
                     real(c_float)     :: skx_instr_per_avx128
              end function skx_instr_per_avx128
              

         end interface


         interface

              function skx_instr_per_avx256(  instr_retired_any,&
                                              fp_arith_instr_retired_256B_packed_single,&
			                      fp_arith_instr_retired_256B_packed_double) &
                                              bind(c,name="skx_instr_per_avx256")
                     integer(c_size_t) :: instr_retired_any
                     integer(c_size_t) ::  fp_arith_instr_retired_256B_packed_single
                     integer(c_size_t) ::  fp_arith_instr_retired_256B_packed_double
                     real(c_float)     :: skx_instr_per_avx256
              end function skx_instr_per_avx256
              

         end interface

           
        interface

              function skx_instr_per_avx512(  instr_retired_any,&
                                              fp_arith_instr_retired_512B_packed_single,&
			                      fp_arith_instr_retired_512B_packed_double) &
                                              bind(c,name="skx_instr_per_avx512")
                     integer(c_size_t) :: instr_retired_any
                     integer(c_size_t) ::  fp_arith_instr_retired_512B_packed_single
                     integer(c_size_t) ::  fp_arith_instr_retired_512B_packed_double
                     real(c_float)     :: skx_instr_per_avx512
              end function skx_instr_per_avx512
              

       end interface


       interface

          function skx_uops_by_dsb( idq_dsb_uops, &
                                    uops_delivered_total) &
                                    bind(c,name="skx_uops_by_dsb")
                    integer(c_size_t) :: idq_dsb_uops
                    integer(c_size_t) :: uops_delivered_total
                    real(c_float)     :: skx_uops_by_dsb
          end function skx_uops_by_dsb
          
       end interface

       interface

          function skx_instr_per_baclears( instr_retired_any, &
                                           baclears_any) &
                                           bind(c,name="skx_instr_per_baclears")
                  integer(c_size_t) :: instr_retired_any
                  integer(c_size_t) :: baclears_any
                  real(c_float)     :: skx_instr_per_baclears
          end function skx_instr_per_baclears
          

       end interface

       interface

          function skx_ports_util_0_clks( cycles_ports_0_util, &
                                          core_clks) &
                                          bind(c,name="skx_ports_util_0_clks")
                    integer(c_size_t) :: cycles_ports_0_util
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: skx_ports_util_0_clks
          end function skx_ports_util_0_clks
          

       end interface

       
       interface

          function skx_ports_util_1_clks( cycles_ports_1_util, &
                                          core_clks) &
                                          bind(c,name="skx_ports_util_1_clks")
                    integer(c_size_t) :: cycles_ports_1_util
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: skx_ports_util_1_clks
          end function skx_ports_util_1_clks
          

       end interface


       interface

          function skx_ports_util_2_clks( cycles_ports_2_util, &
                                          core_clks) &
                                          bind(c,name="skx_ports_util_2_clks")
                    integer(c_size_t) :: cycles_ports_2_util
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: skx_ports_util_2_clks
          end function skx_ports_util_2_clks
          

       end interface

       interface

          function skx_ports_util_3m_clks( cycles_ports_3m_util, &
                                          core_clks) &
                                          bind(c,name="skx_ports_util_3m_clks")
                    integer(c_size_t) :: cycles_ports_3m_util
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: skx_ports_util_3m_clks
          end function skx_ports_util_3m_clks
          

       end interface

       interface


            function skx_port_0_clks(UOPS_DISPATCHED_PORT_PORT_0, &
                                     core_clks) &
                                     bind(c,name="skx_port_0_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_0
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_0_clks
            end function skx_port_0_clks
            
          
         end interface

        interface


            function skx_port_1_clks(UOPS_DISPATCHED_PORT_PORT_1, &
                                     core_clks) &
                                     bind(c,name="skx_port_1_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_1
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_1_clks
            end function skx_port_1_clks
            
          
        end interface

         
        interface


            function skx_port_5_clks(UOPS_DISPATCHED_PORT_PORT_5, &
                                     core_clks) &
                                     bind(c,name="skx_port_5_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_5
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_5_clks
            end function skx_port_5_clks
            
          
       end interface

         
       interface


            function skx_port_6_clks(UOPS_DISPATCHED_PORT_PORT_6, &
                                     core_clks) &
                                     bind(c,name="skx_port_6_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_6
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_6_clks
            end function skx_port_6_clks
            
          
       end interface

      interface


            function skx_port_2_clks(UOPS_DISPATCHED_PORT_PORT_2, &
                                     core_clks) &
                                     bind(c,name="skx_port_2_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_2
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_2_clks
            end function skx_port_2_clks
            
          
       end interface

       interface


            function skx_port_3_clks(UOPS_DISPATCHED_PORT_PORT_3, &
                                     core_clks) &
                                     bind(c,name="skx_port_3_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_3
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_3_clks
            end function skx_port_3_clks
            
          
       end interface

       interface


            function skx_port_4_clks(UOPS_DISPATCHED_PORT_PORT_4, &
                                     core_clks) &
                                     bind(c,name="skx_port_4_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_4
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_4_clks
            end function skx_port_4_clks
            
          
       end interface

       interface


            function skx_port_7_clks(UOPS_DISPATCHED_PORT_PORT_7, &
                                     core_clks) &
                                     bind(c,name="skx_port_7_clks")
                 integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_7
                 integer(c_size_t) :: core_clks
                 real(c_float)     :: skx_port_7_clks
            end function skx_port_7_clks
            
          
       end interface

         
       interface

           function skx_instr_per_core( instr_retired_any,&
                                        core_clks) &
                                        bind(c,name="skx_instr_per_core")
                    integer(c_size_t) :: instr_retired_any
                    integer(c_size_t) :: core_clks
                    real(c_float)     :: skx_instr_per_core
           end function skx_instr_per_core
           
        end interface

        interface

              function skx_fp_scalar_retired( fp_arith_inst_retired_scalar_single,&
                                              fp_arith_inst_retired_scalar_double,&
                                              uops_retired_retired_slots) &
                                              bind(c,name="skx_fp_scalar_retired")
                     integer(c_size_t) ::  fp_arith_inst_retired_scalar_single
                     integer(c_size_t) ::  fp_arith_inst_retired_scalar_double
                     integer(c_size_t) ::  uops_retired_retired_slots
                     real(c_float)     ::  skx_fp_scalar_retired
              end function skx_fp_scalar_retired
              
        end interface

           
        interface

             function skx_fp_vector_retired(  fp_arith_inst_retired_128B_packed_double,&
                                  fp_arith_inst_retired_128B_packed_single,&
			          fp_arith_inst_retired_256B_packed_double,&
				 fp_arith_inst_retired_256B_packed_single,&
				  fp_arith_inst_retired_512B_packed_double,&
				 fp_arith_inst_retired_512B_packed_single,&
				 uops_retired_retired_slots) &
                                 bind(c,name="skx_fp_vector_retired")
                   integer(c_size_t) :: fp_arith_inst_retired_128B_packed_double
                   integer(c_size_t) :: fp_arith_inst_retired_128B_packed_single
                   integer(c_size_t) :: fp_arith_inst_retired_256B_packed_double
                   integer(c_size_t) :: fp_arith_inst_retired_256B_packed_single
                   integer(c_size_t) :: fp_arith_inst_retired_512B_packed_double
                   integer(c_size_t) :: fp_arith_inst_retired_512B_packed_single
                   real(c_float)     :: skx_fp_vector_retired
             end function skx_fp_vector_retired
             

        end interface

          
        interface

             function skx_fp_arith_util_core(  uops_retired_retired_slots,&
                                               scalar_ratio,&
					       vector_ratio,&
					       core_clks) &
                                               bind(c,name="skx_fp_arith_util_core")
                       integer(c_size_t) :: uops_retired_retired_slot
                       real(c_float)     :: scalar_ratio
                       real(c_float)     :: vector_ratio
                       integer(c_size_t) :: core_clks
                       real(c_float)     :: skx_fp_arith_util_core
             end function skx_fp_arith_util_core
             
        end interface

        interface

            function skx_ilp_ratio( uops_executed_thread,&
                                    execute_cycles) &
                                    bind(c,name="skx_ilp_ratio")
                  integer(c_size_t) :: uops_executed_thread
                  real(c_float)     :: execute_cycles
                  real(c_float)     :: skx_ilp_ratio
            end function skx_ilp_ratio
            
         end interface

         interface

               function skx_alu_util(  UOPS_DISPATCHED_PORT_PORT_0,&
                                       UOPS_DISPATCHED_PORT_PORT_1,&
			               UOPS_DISPATCHED_PORT_PORT_5,&
			               UOPS_DISPATCHED_PORT_PORT_6,&
                                       core_clks) &
                                       bind(c,name="skx_alu_util")
                    integer(c_size_t) ::  UOPS_DISPATCHED_PORT_PORT_0
                    integer(c_size_t) ::  UOPS_DISPATCHED_PORT_PORT_1
                    integer(c_size_t) ::  UOPS_DISPATCHED_PORT_PORT_5
                    integer(c_size_t) ::  UOPS_DISPATCHED_PORT_PORT_6
                    integer(c_size_t) ::  core_clks
                    real(c_float)     ::  skx_alu_util
               end function skx_alu_util
               

         end interface

         interface

            function skx_instr_per_mispredict( instr_retired_any, &
                                               br_misp_retired_all_branches) &
                                               bind(c,name="skx_instr_per_mispredict")
                   integer(c_size_t) :: instr_retired_any
                   integer(c_size_t) ::  br_misp_retired_all_branches
                   real(c_float)     ::  skx_instr_per_mispredict
            end function skx_instr_per_mispredict
            

         end interface

         interface

             function skx_load_miss_real_latency( l1d_pend_miss_pending,&
                                                  l1d_pend_miss_pending_cycles) &
                                                  bind(c,name="skx_load_miss_real_latency")
                     integer(c_size_t) :: l1d_pend_miss_pending
                     integer(c_size_t) ::  l1d_pend_miss_pending_cycles
                     real(c_float)     ::  skx_load_miss_real_latency
             end function skx_load_miss_real_latency
             

          end interface

          interface

               function skx_mem_level_parallelism( l1d_pend_miss_pending, &
                                                   l1d_pend_miss_pending_cycles) &
                                                   bind(c,name="skx_mem_level_parallelism")
                     integer(c_size_t) :: l1d_pend_miss_pending
                     integer(c_size_t) ::  l1d_pend_miss_pending_cycles
                     real(c_float)     ::  skx_mem_level_parallelism
               end function skx_mem_level_parallelism
               

          end interface

          interface

               function skx_page_walker_util( itlb_misses_walk_pending, &
                                              dtlb_load_misses_walk_pending,&
			                      dtlb_store_misses_walk_pending,&
			                      ept_walk_pending,&
			                      core_clks) &
                                              bind(c,name="skx_page_walker_util")
                         integer(c_size_t) :: itlb_misses_walk_pending
                         integer(c_size_t) :: dtlb_load_misses_walk_pending
                         integer(c_size_t) :: dtlb_store_misses_walk_pending
                         integer(c_size_t) :: ept_walk_pending
                         integer(c_size_t) :: core_clks
                         real(c_float)     :: skx_page_walker_util
               end function skx_page_walker_util
               

          end interface

          interface

               function skx_l1d_bw_cache_fill( l1d_replacement, &
                                               time_interval) &
                                               bind(c,name="skx_l1d_bw_cache_fill")
                         integer(c_size_t) :: l1d_replacement
                         integer(c_size_t) :: time_interval
                         real(c_float)     :: skx_l1d_bw_cache_fill
               end function skx_l1d_bw_cache_fill
               

          end interface

            
          interface

             function skx_l2_bw_cache_fill( l2_lines_in_all, &
                                            time_interval) &
                                            bind(c,name="skx_l2_bw_cache_fill")
                         integer(c_size_t) :: l2_lines_in_all
                         integer(c_size_t) :: time_interval
                         real(c_float)     :: skx_l2_bw_cache_fill
             end function skx_l2_bw_cache_fill
               
             
          end interface

          interface

              function skx_l3_bw_cache_fill( longest_lat_cache_miss, &
                                             time_interval) &
                                             bind(c,name="skx_l3_bw_cache_fill")
                       integer(c_size_t) :: longest_lat_cache_miss
                       integer(c_size_t) :: time_interval
                       real(c_float)     :: skx_l3_bw_cache_fill
              end function skx_l3_bw_cache_fill
             
          end interface

         
          interface

               function skx_l3_bw_cache_access( offcore_requests_all_requests, &
                                                time_interva) &
                                                bind(c,name="skx_l3_bw_cache_access")
                        integer(c_size_t) :: offcore_requests_all_requests
                        integer(c_size_t) :: time_interval
                        real(c_float)     :: skx_l3_bw_cache_access
               end function skx_l3_bw_cache_access
               
             
          end interface

          interface

             function skx_l1_miss_1000instr(  mem_load_retired_l1_miss,&
                                              instr_retired_any) &
                                              bind(c,name="skx_l1_miss_1000instr")
                         integer(c_size_t)  :: mem_load_retired_l1_miss
                         integer(c_size_t)  :: instr_retired_any
                         real(c_float)      :: skx_l1_miss_1000instr
             end function skx_l1_miss_1000instr
               
          end interface

          interface

             function skx_l2_miss_1000instr(  mem_load_retired_l2_miss,&
                                              instr_retired_any) &
                                              bind(c,name="skx_l2_miss_1000instr")
                         integer(c_size_t)  :: mem_load_retired_l2_miss
                         integer(c_size_t)  :: instr_retired_any
                         real(c_float)      :: skx_l2_miss_1000instr
             end function skx_l2_miss_1000instr
               
          end interface

          interface

             function skx_l2_all_miss_1000instr(l2_rqsts_miss, &
                                                instr_retired_any) &
                                                bind(c,name="skx_l2_all_miss_1000instr")
                       integer(c_size_t) :: l2_rqsts_miss
                       integer(c_size_t) :: instr_retired_any
                       real(c_float)     :: skx_l2_all_miss_1000instr
             end function skx_l2_all_miss_1000instr
             

          end interface

          interface

             function skx_l2_miss_loads_1000instr( l2_rqsts_demand_data_rd_miss,&
                                                   instr_retired_any) &
                                                   bind(c,name="skx_l2_miss_loads_1000instr")
                   integer(c_size_t) :: l2_rqsts_demand_data_rd_miss
                   integer(c_size_t) :: instr_retired_any
                   real(c_float)     :: skx_l2_miss_loads_1000instr
             end function skx_l2_miss_loads_1000instr
             
  
          end interface

          interface

             function skx_l2_all_hits_1000instr( l2_rqsts_references,&
                                                 l2_rqsts_miss,&
				                 instr_retired_any) &
                                                 bind(c,name="skx_l2_all_hits_1000instr")
                      integer(c_size_t) :: l2_rqsts_references
                      integer(c_size_t) :: l2_rqsts_miss
                      integer(c_size_t) :: instr_retired_any
                      real(c_float)     :: skx_l2_all_hits_1000instr
             end function skx_l2_all_hits_1000instr
             
             
          end interface

          interface

             function skx_l3_miss_1000instr( mem_load_retired_l3_miss, &
                                             instr_retired_any) &
                                             bind(c,name="skx_l3_miss_1000instr")
                    integer(c_size_t) :: mem_load_retired_l3_miss
                    integer(c_size_t) :: instr_retired_any
                    real(c_float)     :: skx_l3_miss_1000instr
             end function skx_l3_miss_1000instr
             

          end interface

          interface

              function skx_l2_silent_eviction_rate( l2_lines_out_silent,&
                                                    instr_retired_any) &
                                                    bind(c,name="skx_l2_silent_eviction_rate")
                         integer(c_size_t) :: l2_lines_out_silent
                         integer(c_size_t) :: instr_retired_any
                         real(c_float)     :: skx_l2_silent_eviction_rate
              end function skx_l2_silent_eviction_rate
              
           end interface

           interface

              function skx_l2_nonsilent_eviction_rate( l2_lines_out_non_silent,&
                                                    instr_retired_any) &
                                                    bind(c,name="skx_l2_nonsilent_eviction_rate")
                         integer(c_size_t) :: l2_lines_out_non_silent
                         integer(c_size_t) :: instr_retired_any
                         real(c_float)     :: skx_l2_nonsilent_eviction_rate
              end function skx_l2_nonsilent_eviction_rate
              
           end interface

           interface

              function skx_gflops_rate( flop_count, &
                                        time_interval) &
                                        bind(c,name="skx_gflops_rate")
                      integer(c_size_t) :: flop_count
                      integer(c_size_t) :: time_interval
                      real(c_float)     :: skx_gflops_rate
              end function skx_gflops_rate
              

           end interface

           interface

              function skx_clocks_to_ref_tsc( clks, &
                                              cpu_clk_unhalted_ref_tsc) &
                                              bind(c,name="skx_clocks_to_ref_tsc")
                      integer(c_size_t) :: clks
                      integer(c_size_t) :: cpu_clk_unhalted_ref_tsc
                      real(c_float)     :: skx_clocks_to_ref_tsc
              end function skx_clocks_to_ref_tsc
              

           end interface

           interface

              function skx_baseline_license0(  core_power_lvl0_turbo_license,&
                                               core_clks,&
				               is_ht_enabled) &
                                               bind(c,name="skx_baseline_license0")
                         integer(c_size_t) :: core_power_lvl0_turbo_license
                         integer(c_size_t) :: core_clks
                         integer(c_bool)   :: is_ht_enabled
                         real(c_float)     :: skx_baseline_license0
              end function skx_baseline_license0
              

           end interface

           interface

              function skx_baseline_license1(  core_power_lvl1_turbo_license,&
                                               core_clks,&
				               is_ht_enabled) &
                                               bind(c,name="skx_baseline_license1")
                         integer(c_size_t) :: core_power_lvl1_turbo_license
                         integer(c_size_t) :: core_clks
                         integer(c_bool)   :: is_ht_enabled
                         real(c_float)     :: skx_baseline_license1
              end function skx_baseline_license1
              

           end interface

          interface

              function skx_baseline_license2(  core_power_lvl2_turbo_license,&
                                               core_clks,&
				               is_ht_enabled) &
                                               bind(c,name="skx_baseline_license2")
                         integer(c_size_t) :: core_power_lvl2_turbo_license
                         integer(c_size_t) :: core_clks
                         integer(c_bool)   :: is_ht_enabled
                         real(c_float)     :: skx_baseline_license2
              end function skx_baseline_license2
              

           end interface

           interface

              function skx_ht_utilization( cpu_clk_thread_unhalted_one_thread_active,&
                                           cpu_clk_thread_unhalted_ref_xclk_any,&
			                   is_ht_enabled) &
                                           bind(c,name="skx_ht_utilization")
                        integer(c_size_t) :: cpu_clk_thread_unhalted_one_thread_active
                        integer(c_size_t) ::  cpu_clk_thread_unhalted_ref_xclk_any
                        integer(c_bool)   :: is_ht_enabled
                        real(c_float)     :: skx_ht_utilization
              end function skx_ht_utilization
              

           end interface

           interface

              function skx_kernel_time_fraction(  cpu_clk_unhalted_ref_tsc_sup, &
                                                  cpu_clk_unhalted_ref_tsc) &
                                                  bind(c,name="skx_kernel_time_fraction")
                       integer(c_size_t) :: cpu_clk_unhalted_ref_tsc_sup
                       integer(c_size_t) ::  cpu_clk_unhalted_ref_tsc
                       real(c_float)     :: skx_kernel_time_fraction
              end function skx_kernel_time_fraction
              

           end interface

           interface

                  function skx_dram_bw_used( unc_m_cas_count_rd,&
                                             unc_m_cas_count_wr,&
			                     time_interval)&
                                             bind(c,name="skx_dram_bw_used")
                           integer(c_size_t) :: unc_m_cas_count_rd
                           integer(c_size_t) :: unc_m_cas_count_wr
                           integer(c_size_t) :: time_interval
                           real(c_float)     :: skx_dram_bw_used
                  end function skx_dram_bw_used
                  

            end interface

               
            interface

                 function skx_mem_read_latency(  unc_cha_tor_occupancy_ia_miss_rd,&
                                                 unc_char_tor_inserts_ia_miss_drd,&
				                 unc_cha_clockticks_one_unit,&
				                 time_interval) &
                                                 bind(c,name="skx_mem_read_latency")
                          integer(c_size_t) ::   unc_cha_tor_occupancy_ia_miss_rd
                          integer(c_size_t) ::   unc_char_tor_inserts_ia_miss_drd
                          integer(c_size_t) ::   unc_cha_clockticks_one_unit
                          integer(c_size_t) ::   time_interval
                          real(c_float)     ::   skx_mem_read_latency
                 end function skx_mem_read_latency
                 
               
            end interface

              
            interface

                function skx_mem_parallel_reads(  unc_cha_tor_occupancy_ia_miss_drd,&
                                                  unc_cha_tor_occupancy_ia_miss_drd_c1) &
                                                  bind(c,name="skx_mem_parallel_reads")
                           integer(c_size_t) :: unc_cha_tor_occupancy_ia_miss_drd
                           integer(c_size_t) ::  unc_cha_tor_occupancy_ia_miss_drd_c1
                           real(c_float)     :: skx_mem_parallel_reads    
                end function skx_mem_parallel_reads
                  

             end interface

             interface

                 function skx_mem_dram_read_latency( unc_m_rpq_occupancy,&
                                                     unc_m_rpq_inserts,&
				                     unc_m_clockticks_one_unit) &
                                                     bind(c,name="skx_mem_dram_read_latency")
                             integer(c_size_t) :: unc_m_rpq_occupancy
                             integer(c_size_t) :: unc_m_rpq_inserts
                             integer(c_size_t) :: unc_m_clockticks_one_unit
                             real(c_float)     :: skx_mem_dram_read_latency
                 end function skx_mem_dram_read_latency
                 

              end interface

              interface

                  function skx_instr_per_farbr( instr_retired_any, &
                                                br_instr_retired_far_branch) &
                                                bind(c,name="skx_instr_per_farbr")
                            integer(c_size_t) :: instr_retired_any
                            integer(c_size_t) :: br_instr_retired_far_branch
                            real(c_float)     :: skx_instr_per_farbr
                  end function skx_instr_per_farbr
                  

               end interface

               interface

                      function skx_frontend_bound( idq_uops_not_delivered_core, &
                                                  slots) &
                                                  bind(c,name="skx_frontend_bound")
                                 integer(c_size_t) :: idq_uops_not_delivered_core
                                 integer(c_size_t) :: slots
                                 real(c_float)     :: skx_frontend_bound
                      end function skx_frontend_bound
                      
                        
 
               end interface

                   
               interface

                     function skx_frontend_latency(  idq_uops_not_delivered_cycles_0_uops_deliv_core,&
                                                     frontend_retired_latency_ge_1,&
				                     frontend_retired_latency_ge_2,&
				                     retire_fraction,&
                                                     slots) &
                                                     bind(c,name="skx_frontend_latency")
                              integer(c_size_t) :: idq_uops_not_delivered_cycles_0_uops_deliv_core
                              integer(c_size_t) ::  frontend_retired_latency_ge_1
                              integer(c_size_t) ::  frontend_retired_latency_ge_2
                              real(c_float)     ::  retire_fraction
                              integer(c_size_t) :: slots
                              real(c_float)     :: skx_frontend_latency
                     end function skx_frontend_latency
                     

              end interface

              interface

                   function skx_itlb_misses(  ICACHE_64B_IFTAG_STALL,&
                                             clks) &
                                             bind(c,name="skx_itlb_misses")
                              integer(c_size_t) :: ICACHE_64B_IFTAG_STALL
                              integer(c_size_t) :: clks
                              real(c_float)     :: skx_itlb_misses
                   end function skx_itlb_misses
                   

              end interface

              interface

                       function skx_icache_misses(ICACHE_16B_IFDATA_STALL,&
                                                  ICACHE_16B_IFDATA_STALL_c1_e1,&
                                                  clks) &
                                                  bind(c,name="skx_icache_misses")
                                integer(c_size_t) :: ICACHE_16B_IFDATA_STALL
                                integer(c_size_t) :: ICACHE_16B_IFDATA_STALL_c1_e1
                                integer(c_size_t) :: clks
                                real(c_float)     :: skx_icache_misses
                       end function skx_icache_misses
                       
              end interface

              interface

                  function skx_branch_resteers(INT_MISC_CLEAR_RESTEER_CYCLES,&
                                               BACLEARS_ANY,&
                                               clks) &
                                               bind(c,name="skx_branch_resteers")
                              integer(c_size_t) ::  INT_MISC_CLEAR_RESTEER_CYCLES
                              integer(c_size_t)  :: BACLEARS_ANY
                              integer(c_size_t) :: clks
                              real(c_float)     :: skx_branch_resteers
                  end function skx_branch_resteers
                  

               end interface

               interface

                    function skx_mispredict_resteers( mispred_clears,&
                                                      INT_MISC_CLEAR_RESTEER_CYCLES,&
				                      clks) &
                                                      bind(c,name="skx_mispredict_resteers")
                              real(c_float) :: mispred_clears
                              integer(c_size_t) ::   INT_MISC_CLEAR_RESTEER_CYCLES
                              integer(c_size_t) :: clks
                              real(c_float)   :: skx_mispredict_resteers
                    end function 

               end interface

               interface

                     function skx_clears_resteers( mispred_fraction,&
                                                   INT_MISC_CLEAR_RESTEER_CYCLES,&
			                           clks) &
                                                   bind(c,name="skx_clears_resteers")
                               real(c_float) :: mispred_fraction
                               integer(c_size_t) ::  INT_MISC_CLEAR_RESTEER_CYCLES
                               integer(c_size_t) :: clks
                               real(c_float) :: skx_clears_resteers
                      end function

               end interface


               interface

                    function skx_unknown_branches( branch_resteers,&
                                                   INT_MISC_CLEAR_RESTEER_CYCLES,&
			                           clks) &
                                                   bind(c,name="skx_unknown_branches")
                               real(c_float) :: branch_resteers
                               integer(c_size_t) ::  INT_MISC_CLEAR_RESTEER_CYCLES
                               integer(c_size_t) :: clks
                               real(c_float)     :: skx_unknown_branches
                    end function

                 end interface


                 interface

                        function skx_dsb_switches(  DSB2MITE_SWITCHES_PENALTY_CYCLES,&
                                                  clks) &
                                                  bind(c,name="skx_dsb_switches")
                               integer(c_size_t) ::  DSB2MITE_SWITCHES_PENALTY_CYCLES
                               integer(c_size_t) :: clks
                               real(c_float)     :: skx_dsb_switches
                        end function     

                 end interface

                     
                 interface

                    function skx_lcp( ILD_STALL_LCP, &
                                    clks) &
                                    bind(c,name="skx_lcp")
                      integer(c_size_t) :: ILD_STALL_LCP
                      integer(c_size_t) :: clks
                      real(c_float)     :: skx_lcp
                    end function

                 end interface

               interface

                  function skx_ms_switches( IDQ_MS_SWITCHES, &
                                           clks) &
                                           bind(c,name="skx_ms_switches")
                        integer(c_size_t) :: IDQ_MS_SWITCHES
                        integer(c_size_t) :: clks
                        real(c_float)     :: skx_ms_switches
                  end function

               end interface

               interface

                  function skx_frontend_bw( frontend_bound, &
                                            frontend_latency) &
                                            bind(c,name="skx_frontend_bw")
                           real(c_float) :: frontend_bound
                           real(c_float) :: frontend_latency
                           real(c_float) :: frontend_bw
                  end function skx_frontend_bw
                  

               end interface


               interface

                     function skx_mite(  IDQ_ALL_MITE_CYCLES_ANY_UOPS,&
                                         IDQ_ALL_MITE_CYCLES_4_UOPS,&
                                         core_clks) &
                                         bind(c,name="skx_mite")
                           integer(c_size_t) :: IDQ_ALL_MITE_CYCLES_ANY_UOPS
                           integer(c_size_t) ::  IDQ_ALL_MITE_CYCLES_4_UOPS
                           integer(c_size_t) :: core_clks
                           real(c_float)     :: skx_mite
                     end function skx_mite
                     

               end interface

              interface

                     function skx_dsb( IDQ_ALL_DSB_CYCLES_ANY_UOPS,&
                                       IDQ_ALL_DSB_CYCLES_4_UOPS,&
		                       core_clks) &
                                       bind(c,name="skx_dsb")
                            integer(c_size_t) :: IDQ_ALL_DSB_CYCLES_ANY_UOPS
                            integer(c_size_t) ::  IDQ_ALL_DSB_CYCLES_4_UOPS
                            integer(c_size_t) :: core_clks
                            real(c_float)     :: skx_dsb
                     end function skx_dsb
                     

              end interface


              interface

                  function skx_bad_speculation(UOPS_ISSUED_ANY,&
                                               retired_slots,&
			                       recovery_cycles,&
			                       slots) &
                                               bind(c,name="skx_bad_speculation")
                              integer(c_size_t) :: UOPS_ISSUED_ANY
                              integer(c_size_t) :: retired_slots
                              real(c_float)     :: recovery_cycles
                              integer(c_size_t) :: slots
                              real(c_float)     :: skx_bad_speculation
                  end function skx_bad_speculation
                  
               end interface

               interface
                  function skx_branch_mispredict(mispred_clears, &
                                                 bad_speculation) &
                                                 bind(c,name="skx_branch_mispredict")
                             real(c_float) :: mispred_clears
                             real(c_float) :: bad_speculation
                             real(c_float) :: skx_branch_mispredict
                  end function skx_branch_mispredict
                  

               end interface

               interface

                  function skx_machine_clears( bad_speculation, &
                                               branch_mispredict) &
                                               bind(c,name="skx_machine_clears")
                           real(c_float) :: bad_speculation
                           real(c_float) :: branch_mispredict
                           real(c_float) :: skx_machine_clears
                  end function skx_machine_clears
                  

               end interface

               interface

                  function skx_backend_bound( frontend_bound, &
                                              bad_speculation,&
                                              retiring) &
                                              bind(c,name="skx_backend_bound")
                           real(c_float) :: frontend_bound
                           real(c_float) :: bad_speculation
                           real(c_float) :: retiring
                           real(c_float) :: skx_backend_bound
                  end function

               end interface

               interface

                  function skx_memory_bound( memory_bound_frac, &
                                             backend_bound) &
                                             bind(c,name="skx_memory_bound")
                           real(c_float) :: memory_bound_frac
                           real(c_float) :: backend_bound
                           real(c_float) :: skx_memory_bound
                  end function skx_memory_bound
                  

               end interface

               interface

                     function skx_l1_bound( CYCLE_ACTIVITY_STALLS_MEM_ANY,&
                                            CYCLE_ACTIVITY_STALLS_L1D_MISS,&
                                            clks) &
                                            bind(c,name="skx_l1_bound")
                               integer(c_size_t) :: CYCLE_ACTIVITY_STALLS_MEM_ANY
                               integer(c_size_t) :: CYCLE_ACTIVITY_STALLS_L1D_MISS
                               integer(c_size_t) :: clks
                               real(c_float)     :: skx_l1_bound
                     end function skx_l1_bound
                     
               end interface

               interface

                     function skx_dtlb_load(  DTLB_LOAD_MISSES_STLB_HIT,&
                                              DTLB_LOAD_MISSES_WALK_ACTIVE,&
			                      clks) &
                                              bind(c,name="skx_dtlb_load")
                                integer(c_size_t) :: DTLB_LOAD_MISSES_STLB_HIT
                                integer(c_size_t) :: DTLB_LOAD_MISSES_WALK_ACTIVE
                                integer(c_size_t) :: clks
                                real(c_float)     :: skx_dtlb_load
                     end function skx_dtlb_load
                     

               end interface

               interface

                  function skx_load_stlb_hit( dtlb_load, &
                                             load_stlb_miss) &
                                             bind(c,name="skx_load_stlb_hit")
                             real(c_float) :: dtlb_load
                             real(c_float) :: load_stlb_miss
                             real(c_float) :: skx_load_stlb_hit
                  end function skx_load_stlb_hit
                  

               end interface

               interface

                    function skx_load_stlb_miss( DTLB_LOAD_MISSES_WALK_ACTIVE, &
                                                 clks) &
                                                 bind(c,name="skx_load_stlb_miss")
                      integer(c_size_t) :: DTLB_LOAD_MISSES_WALK_ACTIVE
                      integer(c_size_t) :: clks
                      real(c_float)     :: skx_load_stlb_miss
                    end function

               end interface

               interface

                  function skx_sores_fwd_blocked(LD_BLOCKS_STORE_FORWARD, &
                                                clks) &
                                                bind(c,name="skx_sores_fwd_blocked")
                             integer(c_size_t) :: LD_BLOCKS_STORE_FORWARD
                             integer(c_size_t) :: clks
                             real(c_float)     :: skx_sores_fwd_blocked
                  end function

               end interface

               interface

                  function skx_lock_latency(  mem_lock_st_fraction,&
                                              oro_demand_rfo_c1,&
                                              clks) &
                                              bind(c,name="skx_lock_latency")
                           real(c_float) :: mem_lock_st_fraction
                           real(c_float) :: oro_demand_rfo_c1
                           integer(c_size_t) :: clks
                           real(c_float)     :: skx_lock_latency
                  end function
               end interface

               interface
                  function skx_split_loads( load_miss_real_lat, &
                                            LD_BLOCKS_NO_SR, &
                                            clks) &
                                            bind(c,name="skx_split_loads")
                            real(c_float) :: load_miss_real_lat
                            integer(c_size_t) :: LD_BLOCKS_NO_SR
                            integer(c_size_t) :: clks
                            real(c_float)     :: skx_split_loads
                  end function skx_split_loads
                  

               end interface

               interface

                     function skx_4k_aliasing(LD_BLOCKS_PARTIAL_ADDRESS_ALIAS,&
                                              clks) &
                                              bind(c,name="skx_4k_aliasing")
                             integer(c_size_t) :: LD_BLOCKS_PARTIAL_ADDRESS_ALIAS
                             integer(c_size_t) :: clks
                             real(c_float)     :: skx_4k_aliasing
                     end function

              end interface

              interface

                  function skx_sb_full(load_miss_real_lat,&
                                       L1D_PEND_MISS_FB_FULL_c1,&
			               clks) &
                                       bind(c,name="skx_sb_full")
                    real(c_float) :: load_miss_real_lat
                    integer(c_size_t) :: L1D_PEND_MISS_FB_FULL_c1
                    integer(c_size_t) :: clks
                    real(c_float)  :: skx_sb_full
                  end function skx_sb_full

               end interface

               interface

                       function skx_l2_bound( load_l2_hit,&
                                              L1D_PEND_MISS_FB_FULL_c1,&
			                      l2_bound_ratio) &
                                              bind(c,name="skx_l2_bound")
                         real(c_float) :: load_l2_hit
                         integer(c_size_t) :: L1D_PEND_MISS_FB_FULL_c1
                         real(c_float) :: l2_bound_ratio
                         real(c_float) :: skx_l2_bound
                       end function skx_l2_bound
                       

               end interface


               interface

                     function skx_l3_bound(  CYCLE_ACTIVITY_STALLS_L2_MISS,&
                                             CYCLE_ACTIVITY_STALLS_L3_MISS,&
			                     clks) &
                                             bind(c,name="skx_l3_bound")
                             integer(c_size_t) :: CYCLE_ACTIVITY_STALLS_L2_MISS
                             integer(c_size_t) :: CYCLE_ACTIVITY_STALLS_L3_MISS
                             integer(c_size_t) :: clks
                             real(c_float) :: skx_l3_bound
                     end function skx_l3_bound
                     

              end interface

              interface

                 function skx_contested_accesses( load_xsnp_hitm, &
                                                  load_xsnp_miss,&
                                                  clks) &
                                                  bind(c,name="skx_contested_accesses")
                          real(c_float) :: load_xsnp_hitm
                          real(c_float) :: load_xsnp_miss
                          integer(c_size_t) :: clks
                          real(c_float)  :: skx_contested_accesses
                 end function skx_contested_accesses
                 

              end interface

              interface

                 function skx_data_sharing( load_xsnp_hit,&
                                            clks) &
                                            bind(c,name="skx_data_sharing")
                         real(c_float) :: load_xsnp_hit
                         integer(c_size_t) :: clks
                         real(c_float) :: skx_data_sharing
                 end function skx_data_sharing
                 

              end interface

              interface

                 function skx_l3_hit_latency( load_l3_hit, &
                                              clks) &
                                              bind(c,name="skx_l3_hit_latency")
                            real(c_float) :: load_l3_hit
                            integer(c_size_t) :: clks
                            real(c_float)  :: skx_l3_hit_latency
                 end function skx_l3_hit_latency
                 

              end interface

              interface

                 function skx_sq_full( sq_full_cycles, &
                                      clks) &
                                      bind(c,name="skx_sq_full")
                          real(c_float) :: sq_full_cycles
                          integer(c_size_t) :: clks
                          real(c_float) :: skx_sq_full
                 end function skx_sq_full
                 

              end interface

              interface

                  function skx_mem_bw(  oro_drd_bw_cycles,&
                                        clks) &
                                        bind(c,name="skx_mem_bw")
                         integer(c_size_t) :: oro_drd_bw_cycles
                         integer(c_size_t) :: clks
                         real(c_float) :: skx_mem_bw
                  end function
               end interface

               interface

                  function skx_local_dram( load_lcl_mem, &
                                            clks) &
                                            bind(c,name="skx_local_dram")
                       real(c_float) :: load_lcl_mem
                       integer(c_size_t) :: clks
                       real(c_float) :: skx_local_dram
                  end function skx_local_dram
                  

               end interface

               interface

                  function skx_remote_dram( load_rmt_mem, &
                                           clks) &
                                           bind(c,name="skx_remote_dram")
                             real(c_float) :: load_rmt_mem
                             integer(c_size_t) :: clks
                             real(c_float) :: skx_remote_dram
                  end function skx_remote_dram
                  
               end interface

               interface
                    function skx_remote_cache(load_rmt_hitm,&
                                              load_rmt_fwd,&
			                      clks) &
                                              bind(c,name="skx_remote_cache")
                            real(c_float) :: load_rmt_hitm
                            real(c_float) :: load_rmt_fwd
                            integer(c_size_t) :: clks
                            real(c_float) :: skx_remote_cache
                    end function skx_remote_cache
                    

                 end interface

                 interface

                      function skx_store_bound(EXE_ACTIVITY_BOUND_ON_STORES,&
                                               clks) &
                                               bind(c,name="skx_store_bound")
                                 integer(c_size_t) :: EXE_ACTIVITY_BOUND_ON_STORES,
                                 integer(c_size_t) :: clks
                                 real(c_float) :: skx_store_bound
                      end function skx_store_bound
                      

                 end interface

                   
                 interface

                       function skx_store_latency( store_l2_hit_cycles,&
                                                   mem_lock_st_fraction,&
			                           oro_demand_rfo_c1,&
                                                   clks) &
                                                   bind(c,name="skx_store_latency")
                               real(c_float) :: store_l2_hit_cycles
                               real(c_float) :: mem_lock_st_fraction
                               integer(c_size_t) :: oro_demand_rfo_c1
                               integer(c_size_t) :: clks
                               real(C_float) :: skx_store_latency
                       end function skx_store_latency
                       

               end interface


               interface

                     function skx_split_stores_clks( MEM_INST_RETIRED_SPLIT_STORES,&
                                                    clks) &
                                                    bind(c,name="skx_split_stores_clks")
                       integer(c_size_t) :: MEM_INST_RETIRED_SPLIT_STORES
                       integer(c_size_t) :: clks
                       real(c_float) :: skx_split_stores_clks
                     end function skx_split_stores_clks
                     

               end interface


               interface

                   function skx_dtlb_store_clks( DTLB_STORE_MISSES_STLB_HIT,&
                                                 DTLB_STORE_MISSES_WALK_ACTIVE,&
				                 core_clks) &
                                                 bind(c,name="skx_dtlb_store_clks")
                           integer(c_size_t) :: DTLB_STORE_MISSES_STLB_HIT
                           integer(c_size_t) ::  DTLB_STORE_MISSES_WALK_ACTIVE
                           integer(c_size_t) :: clks
                           real(c_float) :: skx_dtlb_store_clks
                   end function skx_dtlb_store_clks
                   

               end interface

               
               interface

                  function skx_stlb_hit_clks( dtlb_store_clks, &
                                               store_stlb_miss) &
                                               bind(c,name="skx_stlb_hit_clks")
                           real(c_float) :: dtlb_store_clks
                           real(c_float) :: store_stlb_miss
                           real(c_float) :: skx_stlb_hit_clks
                  end function

               end interface

               interface

                    function skx_store_stlb_miss_clks( DTLB_STORE_MISSES_WALK_ACTIVE,&
                                                       core_clks) &
                                                       bind(c,name="skx_store_stlb_miss_clks")
                              integer(c_size_t) :: DTLB_STORE_MISSES_WALK_ACTIVE
                              integer(c_size_t) :: core_clks
                              real(c_float)     :: skx_store_stlb_miss_clks
                    end function skx_store_stlb_miss_clks
                    

                end interface

               interface

                  function skx_core_bound_slots( backend_bound, &
                                                 mem_bound) &
                                                 bind(c,name="skx_core_bound_slots")
                    real(c_float) :: backend_bound
                    real(c_float) :: mem_bound
                    real(c_float) :: skx_core_bound_slots
                    
                  end function skx_core_bound_slots
                  
               end interface

               interface

                  function skx_divider_clks( ARITH_DIVIDER_ACTIVE, &
                                            clks) &
                                            bind(c,name="skx_divider_clks")
                          integer(c_size_t) :: ARITH_DIVIDER_ACTIVE
                          integer(c_size_t) :: clks
                          real(c_float)     :: skx_divider_clks
                  end function


               end interface

               interface

                   function skx_alu_util(  UOPS_DISPATCHED_PORT_PORT_0,&
                                           UOPS_DISPATCHED_PORT_PORT_1,&
			                   UOPS_DISPATCHED_PORT_PORT_5,&
			                   UOPS_DISPATCHED_PORT_PORT_6,&
			                   core_clks) &
                                           bind(c,name="skx_alu_util")
                      integer(c_size_t) :: UOPS_DISPATCH_PORT_PORT_0
                      integer(c_size_t) :: UOPS_DISPATCH_PORT_PORT_1
                      integer(c_size_t) :: UOPS_DISPATCH_PORT_PORT_5
                      integer(c_size_t) :: UOPS_DISPATCH_PORT_PORT_6
                      integer(c_size_t) :: core_clks
                      real(c_float) :: skx_alu_util
                   end function skx_alu_util
                   

                end interface

                interface

                   function skx_x87_uops( UOPS_EXECUTED_X87, &
                                          UOPS_EXECUTED_THREAD) &
                                          bind(c,name="skx_x87_uops")
                     integer(c_size_t) :: UOPS_EXECUTED_X87
                     integer(c_size_t) :: UOPS_EXECUTED_THREAD
                     real(c_float) :: skx_x87_uops
                   end function
                end interface

              interface

                    function skx_load_ops_util_clks( UOPS_DISPATCHED_PORT_PORT_2,&
                                                     UOPS_DISPATCHED_PORT_PORT_3,&
				                     UOPS_DISPATCHED_PORT_PORT_7,&
				                     UOPS_DISPATCHED_PORT_PORT_4,&
				                     core_clks) &
                                                     bind(c,name="skx_load_ops_util_clks")
                          integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_2
                          integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_3
                          integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_7
                          integer(c_size_t) :: UOPS_DISPATCHED_PORT_PORT_4
                          integer(c_size_t) :: core_clks
                          real(c_float) :: skx_load_ops_util_clks
                    end function skx_load_ops_util_clks
                    

              end interface     
                 
                  
                    

end module mos_skx_tma_bindings
