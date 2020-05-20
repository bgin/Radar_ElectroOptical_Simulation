
#include "GMS_skx_tma_metrics_api.h"


// Used only with integral types
#define MAX(a,b) ((a) > (b) ? a : b)
#define MIN(a,b) ((a) < (b) ? a : b)

  
    uint64_t skx_uops_fetched( const uint64_t idq_dsb_uops,
                               const uint64_t idq_mite_uops,
			       const uint64_t idq_ms_uops) {
          return (idq_dsb_uops+idq_mite_uops+idq_ms_uops);
    }

  
    float skx_recovery_cycles( const uint64_t int_misc_recovery_cycles_any,
                                  const uint64_t int_misc_recovery_cycles,
				  const bool is_ht_enabled) {
          return (is_ht_enabled ? (float)int_misc_recovery_cycles_any/2ULL :
	                          (float)int_misc_recovery_cycles);
    }

   
    float  skx_executed_cycles( const uint64_t uops_executed_core_cycles_ge_1,
                                  const bool is_ht_enabled) {
          return (is_ht_enabled ? (float)uops_executed_core_cycles_ge_1/2ULL :
	                          (float)uops_executed_core_cycles_ge_1);
    }

   
    float skx_sq_full_cycles( const uint64_t offcore_requests_buffer_sq_full,
                              const bool is_ht_enabled) {
          return (is_ht_enabled ? (float)offcore_requests_buffer_sq_full/2ULL :
	                          (float)offcore_requests_buffer_sq_full);
    }

   
    float skx_cycles_0_ports_utilized( const uint64_t uops_executed_core_cycles_none,
                                       const uint64_t exe_activity_exe_bound_0_ports,
				       const bool is_ht_enabled) {
          return (is_ht_enabled ? (float)uops_executed_core_cycles_none/2ULL :
	                          (float)exe_activity_exe_bound_0_ports);
    }

   
    float skx_cycles_1_ports_utilized( const uint64_t uops_executed_core_cycles_ge_1,
                                       const uint64_t uops_executed_core_cycles_ge_2,
				       const uint64_t exe_activity_ports_util,
				       const bool is_ht_enabled) {
          return (is_ht_enabled ? (float) (uops_executed_core_cycles_ge_1-
	                                   uops_executed_core_cycles_ge_2)/2ULL :
				  (float) exe_activity_ports_util);
    }

   
    float skx_cycles_2_ports_utilized( const uint64_t uops_executed_core_cycles_ge_2,
                                       const uint64_t uops_executed_core_cycles_ge_3,
				       const uint64_t exe_activity_ports_util,
				       const bool is_ht_enabled) {
          return (is_ht_enabled ? (float) (uops_executed_core_cycles_ge_2-
	                                   uops_executed_core_cycles_ge_3)/2ULL :
				   (float)exe_activity_ports_util);
    }

   
    float skx_cycles_3m_ports_utilized( const uint64_t uops_executed_core_cycles_ge_3,
                                        const bool is_ht_enabled) {
           return (is_ht_enabled ? (float)uops_executed_core_cycles_ge_3/2ULL :
	                           (float)uops_executed_core_cycles_ge_3);
    }



   
    uint64_t skx_oro_drd_any_cycles( const uint64_t cpu_clk_unhalted_thread,
                                     const uint64_t offcore_requests_outstanding_cycles_with_data_rd) {
           return (MIN(cpu_clk_unhalted_thread,
	                    offcore_requests_outstanding_cycles_with_data_rd)); 
    }

   
    uint64_t skx_oro_drd_bw_cycles( const uint64_t cpu_clk_unhalted_thread,
                                    const uint64_t offcore_requests_outstanding_all_data_rd_c4) {
           return (MIN(cpu_clk_unhalted_thread,
	                    offcore_requests_outstanding_all_data_rd_c4));
    }

   
    uint64_t skx_oro_demand_rfo_c1( const uint64_t cpu_clk_unhalted_thread,
                                    const uint64_t offcore_requests_outstanding_cycles_with_demand_rfo) {
           return (MIN(cpu_clk_unhalted_thread,
	                    offcore_requests_outstanding_cycles_with_demand_rfo));
    }

  
    float skx_store_l2_hit_cycles( const uint64_t l2_rqsts_rfo_hit,
                                   const float mem_lock_st_fraction) {
           return ((float)l2_rqsts_rfo_hit*Mem_L2_Store_Cost*
	                          (1.0f-mem_lock_st_fraction));
    }

   
    float skx_load_l2_hit(const uint64_t mem_load_retired_l2_hit,
                          const uint64_t mem_load_retired_fb_hit,
			  const uint64_t mem_load_retired_l1_miss) {
           return ((float)mem_load_retired_l2_hit*
	                    (1ULL+mem_load_retired_fb_hit)/
			             mem_load_retired_l1_miss);
    }

    
    float skx_load_l3_hit(const uint64_t mem_load_retired_l3_hit,
                          const uint64_t mem_load_retired_fb_hit,
			  const uint64_t mem_load_retired_l1_miss) {
           return ((float)mem_load_retired_l3_hit*
	                    (1ULL+mem_load_retired_fb_hit)/
			             mem_load_retired_l1_miss);
    }

    
    float skx_load_xsnp_hit(const uint64_t mem_load_l3_hit_retired_xsnp_hit,
                            const uint64_t mem_load_l3_hit_retired_xsnp_hitm,
			    const uint64_t mem_load_retired_fb_hit,
			    const uint64_t mem_load_retired_l1_miss) {
           return ((float)(mem_load_l3_hit_retired_xsnp_hit+
	                       mem_load_l3_hit_retired_xsnp_hitm *
			          (1.0f-true_xsnp_hitm_fraction))*
			             (1ULL+mem_load_retired_fb_hit)/
			                     mem_load_retired_l1_miss);
    }

    
    float skx_load_xsnp_hitm( const uint64_t mem_load_l3_hit_retired_xsnp_hitm,
                              const uint64_t mem_load_retired_fb_hit,
			      const uint64_t mem_load_retired_l1_miss,
			      const float true_xsnp_hit_fraction) {
            return ((float)mem_load_l3_hit_retired_xsnp_hitm*
	                          ((1ULL+mem_load_retired_fb_hit)/
			                    mem_load_retired_l1_miss)*
					          true_xsnp_hit_fraction);
    }

   
    float skx_load_xsnp_miss( const uint64_t mem_load_l3_retired_xsnp_miss,
                              const uint64_t mem_load_retired_fb_hit,
			      const uint64_t mem_load_retired_l1_miss) {
            return ((float)mem_load_l3_retired_xsnp_miss *
	                        (1ULL+mem_load_retired_fb_hit)/
			                mem_load_retired_l1_miss);
    }

    
    float skx_load_local_miss( const uint64_t mem_load_l3_miss_retired_local_dram,
                             const uint64_t mem_load_retired_fb_hit,
			     const uint64_t mem_load_retired_l1_miss) {
            return ((float)mem_load_l3_miss_retired_local_dram*
	                          (1ULL+mem_load_retired_fb_hit)/
			                  mem_load_retired_l1_miss);
    }

    
    float skx_load_remote_miss( const uint64_t mem_load_l3_miss_retired_remote_dram,
                             const uint64_t mem_load_retired_fb_hit,
			     const uint64_t mem_load_retired_l1_miss) {
            return ((float)mem_load_l3_miss_retired_remote_dram*
	                          (1ULL+mem_load_retired_fb_hit)/
			                  mem_load_retired_l1_miss);
    }

   
    float skx_load_remote_hitm( const uint64_t mem_load_l3_miss_retired_remote_hitm,
                             const uint64_t mem_load_retired_fb_hit,
			     const uint64_t mem_load_retired_l1_miss) {
            return ((float)mem_load_l3_miss_retired_remote_hitm*
	                          (1ULL+mem_load_retired_fb_hit)/
			                  mem_load_retired_l1_miss);
    }

   
    float skx_load_remote_forward( const uint64_t mem_load_l3_miss_retired_remote_fwd,
                             const uint64_t mem_load_retired_fb_hit,
			     const uint64_t mem_load_retired_l1_miss) {
            return ((float)mem_load_l3_miss_retired_remote_fwd*
	                          (1ULL+mem_load_retired_fb_hit)/
			                  mem_load_retired_l1_miss);
    }

   
    float skx_uops_executed_threshold( const uint64_t exe_activity_2_ports_util,
                                       const float upc) {
            return ((float)(exe_activity_2_ports_util*upc)/5ULL;)
    }

   
    float skx_core_bound_cycles( const exe_activity_exe_bound_0_ports,
                                 const exe_activity_1_ports_util,
				 const float uops_executed_threshold) {
            return ((float)exe_activity_exe_bound_0_ports+
	                   exe_activity_1_ports_util+
			   uops_executed_threshold);
    }

   
    float skx_backend_bound_cycles( const float core_bound_cycles,
                                    const uint64_t cycles_activity_stalls_mem_any,
				    const uint64_t exe_activity_bound_on_stores) {
            return ((float)core_bound_cycles+
	                      cycles_activity_stalls_mem_any+
			          exe_activity_bound_on_stores);
    }

   
    float skx_memory_bound_fraction( const uint64_t cycles_activity_stalls_mem_any,
                                     const uint64_t exe_activity_bound_on_stores,
				     const float backend_bound_cycles) {
             return ((float)(cycles_activity_stalls_mem_any+
	                       exe_activity_bound_on_stores)/
			                  backend_bound_cycles);
    }

   
    float skx_l2_bound_ratio( const uint64_t cycles_activity_stalls_l1d_miss,
                              const uint64_t cycles_activity_stalls_l2_miss,
			      const uint64_t clks) {
              return ((float)(cycles_activity_stalls_l1d_miss -
	                        cycles_activity_stalls_l2_miss)/clks);
    }

   
    float skx_mem_bound_ratio( const uint64_t cycles_activity_stalls_l3_miss,
                               const  uint64_t clks,
			       const float l2_bound_ratio,
			       const float l2_bound) {
               return ((float)cycles_activity_stalls_l3_miss/clks+
	                      l2_bound_ratio+l2_bound);
    }

   
    float skx_mem_lock_st_fraction( const uint64_t mem_inst_retired_lock_loads,
                                    const uint64_t mem_inst_retired_all_stores) {
               return ((float)mem_inst_retired_lock_loads/
	                      mem_inst_retired_all_stores);
    }

   
    float skx_mispredicted_clears( const uint64_t br_misp_retired_all_branches,
                                   const uint64_t machine_clears_count) {
               return ((float)br_misp_retired_all_branches/
	                      br_misp_retired_all_branches+
			      machine_clears_count);
    }

   
    float skx_retired_uops_fraction( const uint64_t uops_retired_retired_slots,
                                     const uint64_t uops_issued_any) {
               return ((float)uops_retired_retired_slots/uops_issued_any);
    }

   
    float skx_xsnp_hitm_fraction( const uint64_t offcore_response_demand_data_rd_l3_hit_hitm_other,
                                  const uint64_t offcore_response_demand_data_rd_l3_hit_snoop_hit_with_fwd) {
               return ((float)offcore_response_demand_data_rd_l3_hit_hitm_other/
	                      (offcore_response_demand_data_rd_l3_hit_hitm_other+
			       offcore_response_demand_data_rd_l3_hit_snoop_hit_with_fwd));
    }

   
    uint64_t skx_all_rfo_l3_hit_snoop_hitm( const uint64_t offcore_response_demand_rfo_l3_hit_hitm_other_core,
                                         const uint64_t offcore_response_pf_l2_rfo_l3_hit_hitm_other_core) {
               return (offcore_response_demand_rfo_l3_hit_hitm_other_core+
	               offcore_response_pf_l2_rfo_l3_hit_hitm_other_core );
    }

   
    float skx_retired_uops_cycle( const uint64_t uops_retired_retired_slots,
                                  const uint64_t clks) {
                return ((float)uops_retired_retired_slots/clks);
    }

   
    float skx_uops_per_inst( const uint64_t uops_retired_retired_slots,
                             const uint64_t inst_retired_any) {
                return ((float)uops_retired_retired_slots/
		               inst_retired_any);
    }

   
    float skx_instr_per_cycle( const uint64_t inst_retired_any,
                               const uint64_t clks) {
                return ((float) inst_retired_any/clks);
    }

   
    float skx_instr_branch_taken( const uint64_t inst_retired_any,
                                  const uint64_t br_inst_retired_near_taken) {
                return ((float)inst_retired_any/
		               br_inst_retired_near_taken);
    }

   
    float skx_cycles_per_inst( const float ipc) {
               return (1.0f/ipc);
    }

   
    float skx_instr_per_load( const uint64_t inst_retired_any,
                              const uint64_t mem_inst_retired_all_loads) {
                return ((float)inst_retired_any/
		               mem_inst_retired_all_loads);
    }

   
    float skx_instr_per_store( const uint64_t inst_retired_any,
                               const uint64_t mem_inst_retired_all_stores) {
                return ((float)inst_retired_any/
		               mem_inst_retired_all_stores);
    }

   
    float skx_instr_per_branch( const uint64_t inst_retired_any,
                                const br_inst_retired_all_branches) {
                 return ((float)inst_retired_any/
		                br_inst_retired_all_branches);
    }

   
    float skx_instr_per_call( const uint64_t inst_retired_any,
                              const uint64_t br_inst_retired_near_call) {
                 return ((float)inst_retired_any/
		                br_inst_retired_near_call);
    }

   
    float skx_br_inst_per_taken_br( const uint64_t br_inst_retired_all_branches,
                                    const uint64_t br_inst_retired_near_taken) {
                 return ((float)br_inst_retired_all_branches/
		                br_inst_retired_near_taken);
    }

   
    uint64_t skx_flops( const uint64_t fp_arith_inst_retired_scalar_single,
                     const uint64_t fp_arith_inst_retired_scalar_double,
		     const uint64_t fp_arith_inst_retired_128B_packed_double,
		     const uint64_t fp_arith_inst_retired_128B_packed_single,
		     const uint64_t fp_arith_inst_retired_256B_packed_double,
		     const uint64_t fp_arith_inst_retired_256B_packed_single,
		     const uint64_t fp_arith_inst_retired_512B_packed_double,
		     const uint64_t fp_arith_inst_retired_512B_packed_single) {
              return ((1ULL*(fp_arith_inst_retired_scalar_single+
	                            fp_arith_inst_retired_scalar_double)+2ULL*
				    fp_arith_inst_retired_128B_packed_double+
				    4ULL*(fp_arith_inst_retired_128B_packed_single+
				          fp_arith_inst_retired_256B_packed_double)+
				    8ULL*(fp_arith_inst_retired_256B_packed_single+
				          fp_arith_inst_retired_512B_packed_double+
				    16ULL*fp_arith_inst_retired_512B_packed_single)));
				  
    }

   
    float skx_instr_per_flops( const uint64_t instr_retired_any,
                               const uint64_t flops) {
              return ((float)instr_retired_any/flops);
    }

   
    float skx_instr_per_scalar_fp_sp( const uint64_t instr_retired_any,
                                      const uint64_t fp_arith_instr_retired_scalar_single) {
              return ((float)instr_retired_any/
	                       fp_arith_instr_retired_scalar_single);
    }

   
    float skx_instr_per_scalar_dp( const uint64_t instr_retired_any,
                                   const uint64_t fp_arith_instr_retired_scalar_double) {
              return ((float)instr_retired_any/
	                        fp_arith_instr_retired_scalar_double);
    }

   
    float skx_instr_per_avx128( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_128B_packed_single,
				const uint64_t fp_arith_instr_retired_128B_packed_double) {
               return ((float)instr_retired_any/
	                       (fp_arith_instr_retired_128B_packed_single+
			           fp_arith_instr_retired_128B_packed_double));
    }

   
    float skx_instr_per_avx256( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_256B_packed_single,
				const uint64_t fp_arith_instr_retired_256B_packed_double) {
               return ((float)instr_retired_any/
	                       (fp_arith_instr_retired_256B_packed_single+
			           fp_arith_instr_retired_256B_packed_double));
    }

   
    float skx_instr_per_avx512( const uint64_t instr_retired_any,
                                const uint64_t fp_arith_instr_retired_512B_packed_single,
				const uint64_t fp_arith_instr_retired_512B_packed_double) {
               return ((float)instr_retired_any/
	                       (fp_arith_instr_retired_512B_packed_single+
			           fp_arith_instr_retired_512B_packed_double));
    }

  
    float skx_uops_by_dsb( const uint64_t idq_dsb_uops,
                           const uint64_t uops_delivered_total) {
               return ((float)idq_dsb_uops/
	                        uops_deliverd_total);
    }

   
    float skx_instr_per_baclears( const uint64_t instr_retired_any,
                                   const uint64_t baclears_any) {
                return ((float)instr_retired_any/baclears_any);
    }

   
    float skx_instr_per_core( const uint64_t instr_retired_any,
                              const uint64_t core_clks) {
                return ((float)instr_retired_any/core_clks);
    }

   
    float skx_flops_per_cycle( const uint64_t flops,
                               const uint64_t core_clks) {
                return ((float)flops/core_clks);
    }
