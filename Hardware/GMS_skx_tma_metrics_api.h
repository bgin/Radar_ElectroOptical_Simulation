

#ifndef __GMS_SKX_TMA_METRICS_API_H__
#define __GMS_SKX_TMA_METRICS_API_H__ 190520201815


// This implementation must be compiled as C version only.
// This is an API for Fortran callers.

// File info

  const unsigned int gGMS_SKX_TMA_METRICS_API_MAJOR = 1;
  const unsigned int gGMS_SKX_TMA_METRICS_API_METRICS_MINOR = 0;
  const unsigned int gGMS_SKX_TMA_METRICS_API_MICRO = 0;
  const unsigned int gGMS_SKX_TMA_METRICS_API_FULLVER =
      1000U*gGMS_SKX_TMA_METRICS_API_MAJOR+
      100U*gGMS_SKX_TMA_METRICS_API_MINOR+
      10U*gGMS_SKX_TMA_METRICS_API_MICRO;
  const char * const pgGMS_HSW_TMA_METRICS_API_CREATION_DATE = "19-05-2020 18:15 +00200 (TUE 19 MAY 2020 06:15PM GMT+2)";
  const char * const pgGMS_HSW_TMA_METRICS_API_BUILD_DATE    = __DATE__ ":" __TIME__;
  const char * const pgGMS_HSW_TMA_METRICS_API_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
  const char * const pgGMS_HSW_TMA_METRICS_API_SYNOPSIS      = "Skylake-X  performance metrics based on TMA-Metrics (4.0)";


#include <stdint.h>
#include <stdbool.h>

 static const uint32_t Issue_Width = 4U;
 static const uint32_t Avg_Assist_Cost = 100U;
 static const uint32_t MS_Switch_Cost  = 2U;
 static const uint32_t BAClear_Cost    = 9U;
 static const uint32_t Mem_Remote_Forward_Cost = 180U;
 static const uint32_t Mem_Remote_HitM_Cost = 200U;
 static const uint32_t Mem_Remote_DRAM_Cost = 310U;
 static const uint32_t Mem_Local_DRAM_Cost  = 200U;
 static const uint32_t Mem_XSNP_None_Cost  = 41U;
 static const uint32_t Mem_XSNP_Hit_Cost  = 43U;
 static const uint32_t Mem_XSNP_HitM_Cost = 60U;
 static const uint32_t Mem_STLB_Hit_Cost = 9U;
 static const uint32_t Mem_L2_Store_Cost = 11U;


   
    uint64_t skx_uops_fetched( const uint64_t idq_dsb_uops,
                               const uint64_t idq_mite_uops,
			       const uint64_t idq_ms_uops);

   
    float skx_recovery_cycles( const uint64_t int_misc_recovery_cycles_any,
                                  const uint64_t int_misc_recovery_cycles,
			       const bool is_ht_enabled);

  
    float  skx_executed_cycles( const uint64_t uops_executed_core_cycles_ge_1,
				const bool is_ht_enabled);

   
    float skx_sq_full_cycles( const uint64_t offcore_requests_buffer_sq_full,
                              const bool is_ht_enabled);

  
    float skx_cycles_0_ports_utilized( const uint64_t uops_executed_core_cycles_none,
                                       const uint64_t exe_activity_exe_bound_0_ports,
				       const bool is_ht_enabled);

   
    float skx_cycles_1_ports_utilized( const uint64_t uops_executed_core_cycles_ge_1,
                                       const uint64_t uops_executed_core_cycles_ge_2,
				       const uint64_t exe_activity_ports_util,
				       const bool is_ht_enabled);

   
    float skx_cycles_2_ports_utilized( const uint64_t uops_executed_core_cycles_ge_2,
                                       const uint64_t uops_executed_core_cycles_ge_3,
				       const uint64_t exe_activity_ports_util,
				       const bool is_ht_enabled);

   
    float skx_cycles_3m_ports_utilized( const uint64_t uops_executed_core_cycles_ge_3,
                                        const bool is_ht_enabled);


   
    uint64_t skx_oro_drd_any_cycles( const uint64_t cpu_clk_unhalted_thread,
                                     const uint64_t offcore_requests_outstanding_cycles_with_data_rd);

   
    uint64_t skx_oro_drd_bw_cycles( const uint64_t cpu_clk_unhalted_thread,
                                    const uint64_t offcore_requests_outstanding_all_data_rd_c4);

   
    uint64_t skx_oro_demand_rfo_c1( const uint64_t cpu_clk_unhalted_thread,
                                    const uint64_t offcore_requests_outstanding_cycles_with_demand_rfo);

   
    float skx_store_l2_hit_cycles( const uint64_t l2_rqsts_rfo_hit,
                                   const float mem_lock_st_fraction);

   
    float skx_load_l2_hit(const uint64_t mem_load_retired_l2_hit,
                          const uint64_t mem_load_retired_fb_hit,
			  const uint64_t mem_load_retired_l1_miss);

  
    float skx_load_l3_hit(const uint64_t mem_load_retired_l3_hit,
                          const uint64_t mem_load_retired_fb_hit,
			  const uint64_t mem_load_retired_l1_miss);

   
    float skx_load_xsnp_hit(const uint64_t mem_load_l3_hit_retired_xsnp_hit,
                            const uint64_t mem_load_l3_hit_retired_xsnp_hitm,
			    const uint64_t mem_load_retired_fb_hit,
			    const uint64_t mem_load_retired_l1_miss);

  
    float skx_load_xsnp_hitm( const uint64_t mem_load_l3_hit_retired_xsnp_hitm,
                              const uint64_t mem_load_retired_fb_hit,
			      const uint64_t mem_load_retired_l1_miss,
			      const float true_xsnp_hit_fraction);

   
    float skx_load_xsnp_miss( const uint64_t mem_load_l3_retired_xsnp_miss,
                              const uint64_t mem_load_retired_fb_hit,
			      const uint64_t mem_load_retired_l1_miss);

   
    float skx_load_local_miss( const uint64_t mem_load_l3_miss_retired_local_dram,
                             const uint64_t mem_load_retired_fb_hit,
			       const uint64_t mem_load_retired_l1_miss);

   
    float skx_load_remote_miss( const uint64_t mem_load_l3_miss_retired_remote_dram,
                             const uint64_t mem_load_retired_fb_hit,
				const uint64_t mem_load_retired_l1_miss);

  
    float skx_load_remote_hitm( const uint64_t mem_load_l3_miss_retired_remote_hitm,
                             const uint64_t mem_load_retired_fb_hit,
				const uint64_t mem_load_retired_l1_miss);

  
    float skx_load_remote_forward( const uint64_t mem_load_l3_miss_retired_remote_fwd,
                             const uint64_t mem_load_retired_fb_hit,
				   const uint64_t mem_load_retired_l1_miss);

  
    float skx_uops_executed_threshold( const uint64_t exe_activity_2_ports_util,
                                       const float upc);
  
    float skx_core_bound_cycles( const exe_activity_exe_bound_0_ports,
                                 const exe_activity_1_ports_util,
				 const float uops_executed_threshold);

   
    float skx_backend_bound_cycles( const float core_bound_cycles,
                                    const uint64_t cycles_activity_stalls_mem_any,
				    const uint64_t exe_activity_bound_on_stores);

   
    float skx_memory_bound_fraction( const uint64_t cycles_activity_stalls_mem_any,
                                     const uint64_t exe_activity_bound_on_stores,
				     const float backend_bound_cycles);

   
    float skx_l2_bound_ratio( const uint64_t cycles_activity_stalls_l1d_miss,
                              const uint64_t cycles_activity_stalls_l2_miss,
			      const uint64_t clks); 
   
    float skx_mem_bound_ratio( const uint64_t cycles_activity_stalls_l3_miss,
                               const  uint64_t clks,
			       const float l2_bound_ratio,
			       const float l2_bound);

   
    float skx_mem_lock_st_fraction( const uint64_t mem_inst_retired_lock_loads,
                                    const uint64_t mem_inst_retired_all_stores);

   
    float skx_mispredicted_clears( const uint64_t br_misp_retired_all_branches,
                                   const uint64_t machine_clears_count);

   
    float skx_retired_uops_fraction( const uint64_t uops_retired_retired_slots,
                                     const uint64_t uops_issued_any);
   
    float skx_xsnp_hitm_fraction( const uint64_t offcore_response_demand_data_rd_l3_hit_hitm_other,
                                  const uint64_t offcore_response_demand_data_rd_l3_hit_snoop_hit_with_fwd);

   
    uint64_t skx_all_rfo_l3_hit_snoop_hitm( const uint64_t offcore_response_demand_rfo_l3_hit_hitm_other_core,
					    const uint64_t offcore_response_pf_l2_rfo_l3_hit_hitm_other_core);
  
    float skx_retired_uops_cycle( const uint64_t uops_retired_retired_slots,
                                  const uint64_t clks);

  
    float skx_uops_per_inst( const uint64_t uops_retired_retired_slots,
                             const uint64_t inst_retired_any);

  
    float skx_instr_per_cycle( const uint64_t inst_retired_any,
                               const uint64_t clks);

  
    float skx_instr_branch_taken( const uint64_t inst_retired_any,
                                  const uint64_t br_inst_retired_near_taken);

   
    float skx_cycles_per_inst( const float ipc);

   
    float skx_instr_per_load( const uint64_t inst_retired_any,
                              const uint64_t mem_inst_retired_all_loads);

   
    float skx_instr_per_store( const uint64_t inst_retired_any,
                               const uint64_t mem_inst_retired_all_stores);

  
    float skx_instr_per_branch( const uint64_t inst_retired_any,
                                const br_inst_retired_all_branches);

   
    float skx_instr_per_call( const uint64_t inst_retired_any,
                              const uint64_t br_inst_retired_near_call);

   
    float skx_br_inst_per_taken_br( const uint64_t br_inst_retired_all_branches,
                                    const uint64_t br_inst_retired_near_taken);
  
    uint64_t skx_flops( const uint64_t fp_arith_inst_retired_scalar_single,
                     const uint64_t fp_arith_inst_retired_scalar_double,
		     const uint64_t fp_arith_inst_retired_128B_packed_double,
		     const uint64_t fp_arith_inst_retired_128B_packed_single,
		     const uint64_t fp_arith_inst_retired_256B_packed_double,
		     const uint64_t fp_arith_inst_retired_256B_packed_single,
		     const uint64_t fp_arith_inst_retired_512B_packed_double,
	          	const uint64_t fp_arith_inst_retired_512B_packed_single);

   
    float skx_instr_per_flops( const uint64_t instr_retired_any,
                               const uint64_t flops);
   
    float skx_instr_per_scalar_fp_sp( const uint64_t instr_retired_any,
                                      const uint64_t fp_arith_instr_retired_scalar_single);

  
    float skx_instr_per_scalar_dp( const uint64_t instr_retired_any,
                                   const uint64_t fp_arith_instr_retired_scalar_double);
  
    float skx_instr_per_avx128( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_128B_packed_single,
				const uint64_t fp_arith_instr_retired_128B_packed_double);

   
    float skx_instr_per_avx256( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_256B_packed_single,
				const uint64_t fp_arith_instr_retired_256B_packed_double);

   
    float skx_instr_per_avx512( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_512B_packed_single,
				const uint64_t fp_arith_instr_retired_512B_packed_double);

  
    float skx_uops_by_dsb( const uint64_t idq_dsb_uops,
                           const uint64_t uops_delivered_total);

  
    float skx_instr_per_baclears( const uint64_t instr_retired_any,
				  const uint64_t baclears_any);

   
    float skx_instr_per_core( const uint64_t instr_retired_any,
                              const uint64_t core_clks);

  
    float skx_flops_per_cycle( const uint64_t flops,
                               const uint64_t core_clks);

   
    float skx_fp_scalar_retired( const uint64_t fp_arith_inst_retired_scalar_single,
                                 const uint64_t fp_arith_inst_retired_scalar_double,
				 const uint64_t uops_retired_retired_slots);

   
    float skx_fp_vector_retired( const uint64_t fp_arith_inst_retired_128B_packed_double,
                                 const uint64_t fp_arith_inst_retired_128B_packed_single,
				 const uint64_t fp_arith_inst_retired_256B_packed_double,
				 const uint64_t fp_arith_inst_retired_256B_packed_single,
				 const uint64_t fp_arith_inst_retired_512B_packed_double,
				 const uint64_t fp_arith_inst_retired_512B_packed_single,
				 const uint64_t uops_retired_retired_slots);
				 
   
    float skx_fp_arith_util_core(         const uint64_t uops_retired_retired_slots,
                                          const float scalar_ratio,
					  const float vector_ratio,
					  const uint64_t core_clks);

   
    float skx_ilp_ratio( const uint64_t uops_executed_thread,
                         const float execute_cycles);
   
    float skx_instr_per_mispredict( const uint64_t instr_retired_any,
                                    const uint64_t br_misp_retired_all_branches);

   
    float skx_load_miss_real_latency( const uint64_t l1d_pend_miss_pending,
                                      const uint64_t l1d_pend_miss_pending_cycles);

   
    float skx_mem_level_parallelism( const uint64_t l1d_pend_miss_pending,
                                     const uint64_t l1d_pend_miss_pending_cycles);

   
    float skx_page_walker_util( const uint64_t itlb_misses_walk_pending,
                                const uint64_t dtlb_load_misses_walk_pending,
				const uint64_t dtlb_store_misses_walk_pending,
				const uint64_t ept_walk_pending,
				const uint64_t core_clks);

   
    float skx_l1d_bw_cache_fill( const uint64_t l1d_replacement,
                                 const uint64_t time_interval);
   
    float skx_l2_bw_cache_fill( const uint64_t l2_lines_in_all,
                                const uint64_t time_interval);
   
    float skx_l3_bw_cache_fill( const uint64_t longest_lat_cache_miss,
                                const uint64_t time_interval);

   
    float skx_l3_bw_cache_access( const uint64_t offcore_requests_all_requests,
                                  const uint64_t time_interval);

   
    float skx_l1_miss_1000instr( const uint64_t mem_load_retired_l1_miss,
                                 const uint64_t instr_retired_any);

    
     float skx_l2_miss_1000instr( const uint64_t mem_load_retired_l2_miss,
                                  const uint64_t instr_retired_any);

   
     float skx_l2_all_miss_1000instr( const uint64_t l2_rqsts_miss,
                                      const uint64_t instr_retired_any);

    
     float skx_l2_miss_loads_1000instr( const uint64_t l2_rqsts_demand_data_rd_miss,
                                        const uint64_t instr_retired_any);

   
     float skx_l2_all_hits_1000instr( const uint64_t l2_rqsts_references,
                                      const uint64_t l2_rqsts_miss,
				      const uint64_t instr_retired_any);

   
     float skx_l2_hits_load_1000instr( const uint64_t l2_rqsts_demand_data_rd_hit,
                                       const uint64_t instr_retired_any);

    
     float skx_l3_miss_1000instr( const uint64_t mem_load_retired_l3_miss,
                                  const uint64_t instr_retired_any);

    
     float skx_l2_silent_evictions_rate( const uint64_t l2_lines_out_silent,
                                         const uint64_t instr_retired_any);

    
     float skx_l2_nonsilent_evictions_rate( const uint64_t l2_lines_out_non_silent,
                                            const uint64_t instr_retired_any); 

    
     float skx_gflops_rate( const uint64_t flop_count,
                            const uint64_t time_interval);

    
     float skx_clocks_to_ref_tsc(const uint64_t clks,
                                 const uint64_t cpu_clk_unhalted_ref_tsc);
    
     float skx_baseline_license0( const uint64_t core_power_lvl0_turbo_license,
                                  const uint64_t core_clks,
				  const bool is_ht_enabled);

    
     float skx_baseline_license1( const uint64_t core_power_lvl1_turbo_license,
                                  const uint64_t core_clks,
				  const bool is_ht_enabled);
   
     float skx_baseline_license2(  const uint64_t core_power_lvl2_turbo_license,
                                   const uint64_t core_clks,
				   const bool is_ht_enabled);

    
     float skx_ht_utilization( const uint64_t cpu_clk_thread_unhalted_one_thread_active,
                               const uint64_t cpu_clk_thread_unhalted_ref_xclk_any,
			       const bool is_ht_enabled);

    
     float skx_kernel_time_fraction( const uint64_t cpu_clk_unhalted_ref_tsc_sup,
                                     const uint64_t cpu_clk_unhalted_ref_tsc);

    
     float skx_dram_bw_used( const uint64_t unc_m_cas_count_rd,
                             const uint64_t unc_m_cas_count_wr,
			     const uint64_t time_interval);

   
     float skx_mem_read_latency( const uint64_t unc_cha_tor_occupancy_ia_miss_rd,
                                 const uint64_t unc_char_tor_inserts_ia_miss_drd,
				 const uint64_t unc_cha_clockticks_one_unit,
				 const uint64_t time_interval);

   
     float skx_mem_parallel_reads( const uint64_t unc_cha_tor_occupancy_ia_miss_drd,
                                   const uint64_t unc_cha_tor_occupancy_ia_miss_drd_c1);

    
     float skx_mem_dram_read_latency( const uint64_t unc_m_rpq_occupancy,
                                      const uint64_t unc_m_rpq_inserts,
				      const uint64_t unc_m_clockticks_one_unit);
   
     float skx_instr_per_farbr( const uint64_t instr_retired_any,
                                const uint64_t br_instr_retired_far_branch);

   
     float skx_frontend_bound( const uint64_t idq_uops_not_delivered_core,
                               const uint64_t slots);

   
     float skx_frontend_latency( const uint64_t idq_uops_not_delivered_cycles_0_uops_deliv_core,
                                 const uint64_t frontend_retired_latency_ge_1,
				 const uint64_t frontend_retired_latency_ge_2,
				 const float retire_fraction,
				 const uint64_t slots);

    
     float skx_itlb_misses( const uint64_t ICACHE_64B_IFTAG_STALL,
                            const uint64_t clks);
			    
    
     float skx_icache_misses( const uint64_t ICACHE_16B_IFDATA_STALL,
                              const uint64_t ICACHE_16B_IFDATA_STALL_c1_e1,
			      const uint64_t clks);

    
     float skx_branch_resteers( const uint64_t INT_MISC_CLEAR_RESTEER_CYCLES,
                                const uint64_t BACLEARS_ANY,
				const uint64_t clks);

   
     float skx_mispredict_resteers( const float mispred_clears,
                                    const uint64_t INT_MISC_CLEAR_RESTEER_CYCLES,
				    const uint64_t clks);

    
     float skx_clears_resteers( const mispred_fraction,
                                const uint64_t INT_MISC_CLEAR_RESTEER_CYCLES,
				const uint64_t clks);

    
     float skx_unknown_branches( const float branch_resteers,
                                 const uint64_t INT_MISC_CLEAR_RESTEER_CYCLES,
				 const uint64_t clks);
   
     float skx_dsb_switches( const uint64_t DSB2MITE_SWITCHES_PENALTY_CYCLES,
                             const uint64_t clks);
   
     float skx_lcp( const uint64_t ILD_STALL_LCP,
                    const uint64_t clks);


  
     float skx_ms_switches( const uint64_t IDQ_MS_SWITCHES,
                            const uint64_t clks);

   
     float skx_frontend_bw( const float frontend_bound,
                            const float frontend_latency);

    
     float skx_mite( const uint64_t IDQ_ALL_MITE_CYCLES_ANY_UOPS,
                     const uint64_t IDQ_ALL_MITE_CYCLES_4_UOPS,
		     const uint64_t core_clks);
   
     float skx_dsb( const uint64_t IDQ_ALL_DSB_CYCLES_ANY_UOPS,
                    const  UINT64_T IDQ_ALL_DSB_CYCLES_4_UOPS,
		    const uint64_t core_clks);

   
     float skx_bad_speculation( const uint64_t UOPS_ISSUED_ANY,
                                const uint64_t retired_slots,
				const float recovery_cycles,
				const uint64_t slots);

    
      float skx_branch_mispredict( const float mispred_clears,
                                   const float bad_speculation);
    
      float skx_machine_clears( const float bad_speculation,
                                const float branch_mispredict);

   
      float skx_backend_bound( const float frontend_bound,
                               const float bad_speculation,
			       const float retiring );
    
      float skx_memory_bound( const float memory_bound_frac,
                              const float backend_bound);

    
      float skx_l1_bound( const uint64_t CYCLE_ACTIVITY_STALLS_MEM_ANY,
                          const uint64_t CYCLE_ACTIVITY_STALLS_L1D_MISS,
			  const uint64_t clks);

    
      float skx_dtlb_load( const uint64_t DTLB_LOAD_MISSES_STLB_HIT,
                           const uint64_t DTLB_LOAD_MISSES_WALK_ACTIVE,
			   const uint64_t clks);
			   
    
      float skx_load_stlb_hit( const float dtlb_load,
                               const float load_stlb_miss);

     
      float skx_load_stlb_miss( const uint64_t DTLB_LOAD_MISSES_WALK_ACTIVE,
                                const uint64_t clks);
				
    
      float skx_sores_fwd_blocked( const uint64_t LD_BLOCKS_STORE_FORWARD,
                                   const uint64_t clks);

    
      float skx_lock_latency( const float mem_lock_st_fraction,
                              const float oro_demand_rfo_c1,
			      const uint64_t clks);

   
      float skx_split_loads( const float load_miss_real_lat,
                             const float uint64_t LD_BLOCKS_NO_SR,
			     const uint64_t clks);

     
      float skx_4k_aliasing( const uint64_t LD_BLOCKS_PARTIAL_ADDRESS_ALIAS,
                             const uint64_t clks);

     
      float skx_fb_full( const float load_miss_real_lat,
                         const uint64_t L1D_PEND_MISS_FB_FULL_c1,
			 const uint64_t clks);

    
      float skx_l2_bound( const float load_l2_hit,
                          const uint64_t  L1D_PEND_MISS_FB_FULL_c1,
			  const float l2_bound_ratio);
      
     
      float skx_l3_bound( const uint64_t CYCLE_ACTIVITY_STALLS_L2_MISS,
                          const uint64_t CYCLE_ACTIVITY_STALLS_L3_MISS,
			  const uint64_t clks);

     
      float skx_contested_accesses( const float load_xsnp_hitm,
                                    const float load_xsnp_miss,
				    const uint64_t clks);

   
      float skx_data_sharing( const float load_xsnp_hit,
                              const uint64_t clks);
      
      float skx_l3_hit_latency( const float load_l3_hit,
                                const uint64_t clks);
     
      float skx_sq_full( const float sq_full_cycles,
                         const uint64_t clks);
     
      float skx_mem_bw( const uint64_t oro_drd_bw_cycles,
                        const uint64_t clks);

    
      float skx_local_dram( const float load_lcl_mem,
                            const uint64_t clks);
    
      float skx_remote_dram( const float load_rmt_mem,
                             const uint64_t clks);

     
      float skx_remote_cache( const float load_rmt_hitm,
                              const float load_rmt_fwd,
			      const uint64_t clks);

     
      float skx_store_bound( const uint64_t EXE_ACTIVITY_BOUND_ON_STORES,
                             const uint64_t clks);

    
      float skx_store_latency( const float store_l2_hit_cycles,
                               const float mem_lock_st_fraction,
			       const uint64_t oro_demand_rfo_c1,
			       const uint64_t clks);

     
      float skx_split_stores_clks( const uint64_t MEM_INST_RETIRED_SPLIT_STORES,
                              const uint64_t clks);

     
      float skx_dtlb_store_clks( const uint64_t DTLB_STORE_MISSES_STLB_HIT,
                                 const uint64_t DTLB_STORE_MISSES_WALK_ACTIVE,
				 const uint64_t core_clks);

    
      float skx_stlb_hit_clks( const float dtlb_store_clks,
                               const float store_stlb_miss);

    
      float skx_store_stlb_miss_clks( const uint64_t DTLB_STORE_MISSES_WALK_ACTIVE,
                                      const uint64_t core_clks);
    
      float skx_core_bound_slots( const float backend_bound,
                                  const float mem_bound);

   
      float skx_divider_clks( const uint64_t ARITH_DIVIDER_ACTIVE,
                              const uint64_t clks);

    
      float skx_ports_util_0_clks( const float cycles_ports_0_util,
                                   const uint64_t core_clks);

     
      float skx_serializations_clks( const uint64_t PARTIAL_RAT_STALLS_SCOREBOARD,
                                     const uint64_t clks);
   
      float skx_ports_util_1_clks( const float cycles_ports_1_util,
                                   const uint64_t core_clks);
    
      float skx_ports_util_2_clks( const float cycles_ports_2_util,
                                   const uint64_t core_clks);
     
      float skx_ports_util_3m_clks( const float cycles_ports_3m_util,
                                   const uint64_t core_clks);

     
      float skx_alu_util( const uint64_t UOPS_DISPATCHED_PORT_PORT_0,
                          const uint64_t UOPS_DISPATCHED_PORT_PORT_1,
			  const uint64_t UOPS_DISPATCHED_PORT_PORT_5,
			  const uint64_t UOPS_DISPATCHED_PORT_PORT_6,
			  const uint64_t core_clks);

      
      float skx_port_0_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_0,
                             const uint64_t core_clks);

     
      float skx_port_1_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_1,
                             const uint64_t core_clks);
     
      float skx_port_5_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_5,
                             const uint64_t core_clks);
     
      float skx_port_6_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_6,
                             const uint64_t core_clks);
   
      float skx_load_ops_util_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_2,
                                    const uint64_t UOPS_DISPATCHED_PORT_PORT_3,
				    const uint64_t UOPS_DISPATCHED_PORT_PORT_7,
				    const uint64_t UOPS_DISPATCHED_PORT_PORT_4,
				    const uint64_t core_clks);

     
      float skx_port_2_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_2,
                             const uint64_t core_clks);

     
      float skx_port_3_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_3,
                             const uint64_t core_clks);

     
      float skx_port_4_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_4,
                             const uint64_t core_clks);

    
      float skx_port_7_clks( const uint64_t UOPS_DISPATCHED_PORT_PORT_7,
                             const uint64_t core_clks);

     
      float skx_x87_uops( const uint64_t UOPS_EXECUTED_X87,
                          const uint64_t UOPS_EXECUTED_THREAD);





#endif /*__GMS_SKX_TMA_METRICS_API_H__*/
