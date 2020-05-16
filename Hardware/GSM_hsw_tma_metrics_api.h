

#ifndef __GMS_HSW_TMA_METRICS_API_H__
#define __GMS_HSW_TMA_METRICS_API_H__ 100520200913


// This implementation must be compiled as C version only.
// This is an API for Fortran callers.

// File info

  const unsigned int gGMS_HSW_CLIENT_METRICS_MAJOR = 1;
  const unsigned int gGMS_HSW_CLIENT_METRICS_MINOR = 0;
  const unsigned int gGMS_HSW_CLIENT_METRICS_MICRO = 0;
  const unsigned int gGMS_HSW_CLIENT_METRICS_FULLVER =
      1000U*gGMS_HSW_CLIENT_METRICS_MAJOR+
      100U*gGMS_HSW_CLIENT_METRICS_MINOR+
      10U*gGMS_HSW_CLIENT_METRICS_MICRO;
  const char * const pgGMS_HSW_CLIENT_METRICS_CREATION_DATE = "10-05-2020 09:15 +00200 (SUN 10 MAY 2020 09:36AM GMT+2)";
  const char * const pgGMS_HSW_CLIENT_METRICS_BUILD_DATE    = __DATE__ ":" __TIME__;
  const char * const pgGMS_HSW_CLIENT_METRICS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
  const char * const pgGMS_HSW_CLIENT_METRICS_SYNOPSIS      = "Haswell client performance metrics based on TMA-Metrics (4.0)";


#include <stdint.h>

// Constants

   const static uint32_t Issue_Width = 4;
   const static uint32_t Mem_L2_Store_Cost = 9;
   const static uint32_t Mem_L3_Weight = 7;
   const static uint32_t Energy_Unit = 61;
   const static uint32_t BAClear_Cost = 12;
   const static uint32_t MS_Switch_Cost = 2;
   const static uint32_t Avg_Assist_Cost = 100;
   const static uint32_t Mem_L3_Weight = 7;
   const static uint32_t Mem_STLB_Hit_Cost = 8;
   const static uint32_t Mem_XSNP_HitM_Cost = 60;
   const static uint32_t Mem_XSNP_Hit_Cost = 43;
   const static uint32_t Mem_XSNP_None_Cost = 29;

    
    uint64_t hsw_fetched_uops(const uint64_t,
                              const uint64_t ,
			      const uint64_t ,
			      const uint64_t );
#include <stdbool.h>



    uint64_t hsw_recovery_cycles(const uint64_t ,
                                 const uint64_t ,
			         const bool);

    uint64_t hsw_execute_cycles(const uint64_t ,
                                const bool);

    uint64_t hsw_sq_full_cycles(const uint64_t,
                                const bool);

    uint64_t hsw_itlb_miss_cycles(const uint64_t,
                                  const uint64_t);

    uint64_t hsw_frontend_rs_empty_cycles(const uint64_t,
                                          const float);

    uint64_t hsw_cycles_0_ports_utilized(const uint64_t ,
                                         const uint64_t,
					 const uint64_t,
					 const float,
					 const bool);

    uint64_t hsw_cycles_1_ports_utilized(const uint64_t,
                                         const uint64_t ,
				         const bool);

    uint64_t hsw_cycles_2_ports_utilized(const uint64_t,
                                         const uint64_t,
					 const bool);

    uint64_t hsw_cycles_3_ports_utilized(const uint64_t ,
                                         const bool);

    uint64_t hsw_frontend_latency_cycles(const uint64_t,
                                         const uint64_t);

    uint64_t hsw_stalls_mem_any(const uint64_t,
                                const uint64_t);

    uint64_t hsw_stalls_total(const uint64_t,
                              const uint64_t);

    uint64_t  hsw_oro_drd_any_cycles(const uint64_t,
                                    const uint64_t);

    uint64_t hsw_oro_drd_bw_cycles(const uint64_t,
                                   const uint64_t);

    uint64_t hsw_oro_demand_rfo_c1(const uint64_t,
                                   const uint64_t);

    float hsw_store_l2_hit_cycles(const uint64_t,
                                  const uint64_t ,
				  const uint64_t);

    uint64_t hsw_load_l1_miss(const uint64_t,
                              const uint64_t,
			      const uint64_t,
			      const uint64_t,
			      const uint64_t);

    uint64_t hsw_load_l1_miss_net(const uint64_t,
                                  const uint64_t,
                                  const uint64_t,
			          const uint64_t,
			          const uint64_t,
			          const uint64_t);

    float hsw_load_l3_hit(const uint64_t,
                          const uint64_t,
			  const uint64_t,
			  const uint64_t,
			  const uint64_t,
			  const uint64_t);

    float hsw_load_xsnp_hit(const uint64_t,
                            const uint64_t,
			    const uint64_t,
			    const uint64_t,
			    const uint64_t,
			    const uint64_t);

    float hsw_load_xsnp_hitm(
                             const uint64_t,
			     const uint64_t,
	                     const uint64_t,
			     const uint64_t,
			     const uint64_t,
			     const uint64_t);

     float hsw_load_xsnp_miss( const uint64_t ,
			     const uint64_t,
	                     const uint64_t,
			     const uint64_t,
			     const uint64_t,
			     const uint64_t);

      uint64_t hsw_few_uops_executed_threshold(const uint64_t,
                                             const uint64_t,
					     const float);

      float hsw_backend_bound_cycles(const uint64_t,
                                    const uint64_t,
				    const uint64_t,
				    const uint64_t,
				    const uint64_t,
				    const bool);

       float hsw_memory_bound_fraction(const uint64_t,
                                       const uint64_t,
				       const float);

       float hsw_mem_l3_hit_fraction( const uint64_t,
                                      const uint64_t);

       float hsw_mem_lock_st_fraction( const uint64_t,
                                       const uint64_t);

       float hsw_mispred_clears_fraction( const uint64_t,
                                          const uint64_t);

       float hsw_retire_fraction( const uint64_t,
                                  const uint64_t);

       float hsw_ipc( const uint64_t,
                      const uint64_t);

       float hsw_upi( const uint64_t,
                      const uint64_t);

       float hsw_iptb( const uint64_t,
                       const uint64_t);

       float hsw_cpi( const uint64_t,
                      const uint64_t);

       uint64_t hsw_issue_slots( const uint64_t);

       float hsw_ipload( const uint64_t,
                       const uint64_t);

       float hsw_ipstore( const uint64_t,
                        const uint64_t);

       float hsw_ipbranch( const uint64_t,
                         const uint64_t);

       float hsw_ipcall( const uint64_t ,
                         const uint64_t);

       float hsw_biptb( const uint64_t,
                        const uint64_t);

       float hsw_dsb_coverage( const uint64_t,
                             const uint64_t);

       float hsw_ipbaclear( const uint64_t,
                            const uint64_t);

       float hsw_ipc_core( const uint64_t,
                         const uint64_t);

       float hsw_ilp( const uint64_t,
                      const uint64_t,
		      const bool);

       float hsw_ip_mispredict( const uint64_t,
                              const uint64_t);

       uint64_t hsw_core_clks( const uint64_t,
                          const uint64_t,
			  const uint64_t,
			  const uint64_t,
			  const bool);

       float hsw_load_miss_real_latency(const uint64_t,
                                      const uint64_t,
				      const uint64_t);

       float hsw_mem_level_parallelism( const uint64_t,
                                      const uint64_t);

       float hsw_page_walk_util( const uint64_t,
                               const uint64_t,
			       const uint64_t,
			       const uint64_t);

       float hsw_l1d_cache_fill_bw( const uint64_t,
                                  const uint64_t);

       float hsw_l2_cache_fill_bw( const uint64_t ,
                                 const uint64_t);

       float hsw_l3_cache_fill_bw( const uint64_t,
                                 const uint64_t);

       float hsw_l1mpki( const uint64_t,
                         const uint64_t);

       float hsw_l2mpki( const uint64_t,
                       const uint64_t);

       float hsw_l2hpki(  const uint64_t,
                          const uint64_t);

       float hsw_l3mpki( const uint64_t ,
                         const uint64_t);

       float hsw_ipfarbr( const uint64_t ,
                        const uint64_t);

       float hsw_assist( const uint64_t,
                         const uint64_t);

       float hsw_kernel_utilization(const uint64_t,
                                    const uint64_t);

       float hsw_dram_bw_use(const uint64_t,
                             const uint64_t,
			     const uint64_t);

       float hsw_mem_requests_latency( const uint64_t,
                                       const uint64_t);

       float hsw_mem_parallel_requests( const uint64_t,
                                        const uint64_t);

       float hsw_ht_utilization( const uint64_t,
                                 const uint64_t,
			         const bool);

       float hsw_frontend_bound( const uint64_t,
                                 const uint64_t);

       float hsw_frontend_latency( const uint64_t,
                                   const uint64_t);

       float hsw_icache_misses( const uint64_t,
                                const uint64_t);

       float hsw_dsb_switches(const uint64_t ,
                              const uint64_t);

       float hsw_lcp(const uint64_t ,
                     const uint64_t);

       float hsw_ms_switches( const uint64_t,
                              const uint64_t);

       float hsw_branch_resteers( const uint64_t,
                                  const uint64_t ,
				  const uint64_t,
				  const uint64_t);

       float hsw_mite( const uint64_t,
                       const uint64_t,
		       const uint64_t);

       float hsw_dsb( const uint64_t,
                    const uint64_t ,
		    const uint64_t);

       float hsw_l1_bound( const uint64_t,
                           const uint64_t ,
			   const uint64_t);

       float hsw_dtlb_load(const uint64_t ,
                         const uint64_t ,
			 const uint64_t);

       float hsw_store_fwd_blk( const uint64_t,
                              const uint64_t);

       float hsw_split_loads( const float ,
                              const uint64_t ,
			      const uint64_t);

       float hsw_single_mul_clks( const uint64_t,
                                   const uint64_t);

       float hsw_single_mul_core_clks( const uint64_t ,
                                     const uint64_t);

       float hsw_single_mul_uops_any( const uint64_t,
                                    const uint64_t);

       float hsw_single_mul_uops_retired_any( const uint64_t,
                                            const uint64_t);

       float hsw_simd_move_elim_not_elim( const uint64_t,
                                        const uint64_t);

       float hsw_int_move_elim_not_elim( const uint64_t ,
                                       const uint64_t);

       float  hsw_uops_issued_any_mite_uops( const uint64_t ,
                                      const uint64_t);

       float hsw_single_mul_avx_inst_all( const uint64_t,
                                        const uint64_t);

       float hsw_frontend_latency(   const uint64_t ,
				   const uint64_t);

       float hsw_branch_resteers( const uint64_t,
                                const uint64_t ,
				const uint64_t ,
				const uint64_t);

       float hsw_frontend_bw( const float ,
                            const float);

       float hsw_mite( const uint64_t ,
                     const uint64_t ,
		     const uint64_t);

       float hsw_store_fwd_blocked( const uint64_t ,
                                  const uint64_t);

       float hsw_lock_latency( const float ,
                             const uint64_t ,
			     const uint64_t);

       float hsw_split_loads( const float ,
                            const uint64_t,
			    const uint64_t);

       float hsw_4k_aliasing( const uint64_t,
                            const uint64_t);

        
       float hsw_fb_full( const float ,
                        const uint64_t ,
			const uint64_t);
               

    
     float hsw_l2_bound( const uint64_t ,
                         const uint64_t,
			 const uint64_t);

    
     float hsw_l3_bound( const float ,
                         const uint64_t ,
			 const uint64_t );

     float hsw_contested_accesses( const float,
                                   const float ,
				   const uint64_t);

   
     float hsw_data_sharing( const float,
                             const uint64_t); 

    
     float hsw_dram_bound( const float ,
                           const uint64_t ,
			   const uint64_t );

    
     float hsw_mem_bw( const uint64_t,
                       const uint64_t);

    
     float hsw_mem_latency( const uint64_t,
                            const uint64_t ,
			    const float); 

    
     float hsw_store_bound( const uint64_t ,
                            const uint64_t) ;

   
     float hsw_dtlb_bound( const uint64_t,
                           const uint64_t ,
			   const uint64_t) ;

    
     float hsw_l3_hit_latency( const float,
                               const uint64_t);

    
     float hsw_false_sharing( const uint64_t,
                              const uint64_t); 

    
     float hsw_split_stores( const uint64_t,
                             const uint64_t);

     
     float hsw_dtlb_store( const uint64_t,
                           const uint64_t ,
			   const uint64_t);

    
     float hsw_core_bound( const float,
                           const float); 

   
     float hsw_divider( const uint64_t ,
                        const uint64_t);
     
     float hsw_ports_utilization( const float ,
                                  const uint64_t ,
				  const uint64_t ,
				  const uint64_t) ;

   
     float hsw_ports_utilized_0( const uint64_t,
                                 const uint64_t);

   
     float hsw_ports_utilized_1( const uint64_t ,
                                 const uint64_t); 

    
     float hsw_ports_utilized_2( const uint64_t,
                                 const uint64_t);

    
     float hsw_ports_utilized_0( const uint64_t ,
                                 const uint64_t); 

    
     float hsw_alu_utilization( const uint64_t,
                                const uint64_t ,
				const uint64_t ,
				const uint64_t ,
				const uint64_t); 

    
     float hsw_port0_exec( const uint64_t ,
                           const uint64_t);

    
     float hsw_port1_exec( const uint64_t ,
                           const uint64_t);

   
     float hsw_port5_exec( const uint64_t,
                           const uint64_t);

    
     float hsw_port6_exec( const uint64_t,
                           const uint64_t );

   
     float hsw_load_ops_utilization( const uint64_t ,
                                     const uint64_t,
				     const uint64_t ,
				     const uint64_t,
				     const uint64_t); 

    
     float hsw_port3_exec( const uint64_t ,
                           const uint64_t );

    
     float hsw_port4_exec( const uint64_t uops_dispatched_port_port4,
                           const uint64_t core_clks);

    
     float hsw_port7_exec( const uint64_t,
                           const uint64_t);

    
     float hsw_ms_assists( const uint64_t ,
                           const uint64_t);

   
     float hsw_ms( const float,
                   const uint64_t ,
		   const uint64_t);

   
     float hsw_x87_usage( const uint64_t ,
                          const float ,
			  const uint64_t);
   
     float hsw_retired( const uint64_t,
                        const uint64_t);

    
     float hsw_ret_reg_uops( const float,
                             const float ); 


		       


#endif /*__GMS_HSW_TMA_METRICS_API_H__*/
