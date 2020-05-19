

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



#endif /*__GMS_SKX_TMA_METRICS_API_H__*/
