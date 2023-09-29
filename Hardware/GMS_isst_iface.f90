
module isst_iface


  !========================================================!
  ! Fortran interfaces to some of the functionality
  ! as presented by the program 'isst'.
  ! Experimental implementation!!
  !========================================================!
  use, intrinsic :: ISO_C_BINDING
  implicit none
  public

#define CONFIG_TDP				0x7f
#define CONFIG_TDP_GET_LEVELS_INFO		0x00
#define CONFIG_TDP_GET_TDP_CONTROL		0x01
#define CONFIG_TDP_SET_TDP_CONTROL		0x02
#define CONFIG_TDP_GET_TDP_INFO			0x03
#define CONFIG_TDP_GET_PWR_INFO			0x04
#define CONFIG_TDP_GET_TJMAX_INFO		0x05
#define CONFIG_TDP_GET_CORE_MASK		0x06
#define CONFIG_TDP_GET_TURBO_LIMIT_RATIOS	0x07
#define CONFIG_TDP_SET_LEVEL			0x08
#define CONFIG_TDP_GET_UNCORE_P0_P1_INFO	0X09
#define CONFIG_TDP_GET_P1_INFO			0x0a
#define CONFIG_TDP_GET_MEM_FREQ			0x0b
#define	CONFIG_TDP_GET_RATIO_INFO		0x0c

#define CONFIG_TDP_GET_FACT_HP_TURBO_LIMIT_NUMCORES	0x10
#define CONFIG_TDP_GET_FACT_HP_TURBO_LIMIT_RATIOS	0x11
#define CONFIG_TDP_GET_FACT_LP_CLIPPING_RATIO		0x12

#define CONFIG_TDP_PBF_GET_CORE_MASK_INFO	0x20
#define CONFIG_TDP_PBF_GET_P1HI_P1LO_INFO	0x21
#define CONFIG_TDP_PBF_GET_TJ_MAX_INFO		0x22
#define CONFIG_TDP_PBF_GET_TDP_INFO		0X23

#define CONFIG_CLOS				0xd0
#define CLOS_PQR_ASSOC				0x00
#define CLOS_PM_CLOS				0x01
#define CLOS_PM_QOS_CONFIG			0x02
#define CLOS_STATUS				0x03

#define MBOX_CMD_WRITE_BIT			0x08

#define PM_QOS_INFO_OFFSET			0x00
#define PM_QOS_CONFIG_OFFSET			0x04
#define PM_CLOS_OFFSET				0x08
#define PQR_ASSOC_OFFSET			0x20

#define READ_PM_CONFIG				0x94
#define WRITE_PM_CONFIG				0x95
#define PM_FEATURE				0x03

#define DISP_FREQ_MULTIPLIER 100

#define MAX_PACKAGE_COUNT	32
#define MAX_DIE_PER_PACKAGE	2
#define MAX_PUNIT_PER_DIE	8

#define TRL_MAX_BUCKETS	8
#define TRL_MAX_LEVELS  6

#define ISST_TRL_MAX_ACTIVE_CORES	8
#define ISST_FACT_MAX_BUCKETS		8

#define ISST_MAX_TDP_LEVELS	(4 + 1) 

         type, bind(c) :: isst_id
               integer(c_int) :: cpu
               integer(c_int) :: pkg
               integer(c_int) :: die
               integer(c_int) :: punit
         end type isst_id
         
         type, bind(c) :: isst_clos_config
               integer(c_int) :: clos_min
               integer(c_int) :: clos_max
               integer(c_int) :: epp
               integer(c_int) :: clos_prop_prio
               integer(c_int) :: clos_desired
         end type isst_clos_config
         
         type, bind(c) :: isst_fact_bucket_info
               integer(c_int) :: hp_cores
               integer(c_int) , dimension(TRL_MAX_LEVELS) :: hp_ratios
         end type isst_fact_bucket_info
         
         type, bind(c) :: isst_pbf_info
               integer(c_int)    :: pbf_activated
               integer(c_int)    :: pbf_available
               integer(c_size_t) :: core_cpumask_size
               type(c_ptr)       :: core_cpumask
               integer(c_int)    :: p1_high
               integer(c_int)    :: p1_low
               integer(c_int)    :: t_control
               integer(c_int)    :: t_prochot
               integer(c_int)    :: tdp
         end type isst_pbf_info
         
         type, bind(c) :: isst_fact_info
               integer(c_int), dimension(TRL_MAX_LEVELS) :: lp_ratios
               type(isst_fact_bucket_info), dimension(ISST_FACT_MAX_BUCKETS) :: bucket_info
         end type isst_fact_info
         
         type, bind(c) :: isst_pkg_ctdp_level_info
               integer(c_int)    ::  processed
	       integer(c_int)    ::  control_cpu
	       integer(c_int)    ::  pkg_id
	       integer(c_int)    ::  die_id
	       integer(c_int)    ::  level
	       integer(c_int)    ::  fact_support
	       integer(c_int)    ::  pbf_support;
	       integer(c_int)    ::  fact_enabled
	       integer(c_int)    ::  pbf_enabled
	       integer(c_int)    ::  sst_cp_support
	       integer(c_int)    ::  sst_cp_enabled
	       integer(c_int)    ::  tdp_ratio
	       integer(c_int)    ::  active
	       integer(c_int)    ::  tdp_control
	       integer(c_int)    ::  pkg_tdp
	       integer(c_int)    ::  pkg_min_power
	       integer(c_int)    ::  pkg_max_power
	       integer(c_int)    ::  fact
	       integer(c_int)    ::  t_proc_hot
	       integer(c_int)    ::  cooling_type
	       integer(c_int)    ::  uncore_p0
	       integer(c_int)    ::  uncore_p1
	       integer(c_int)    ::  uncore_pm
	       integer(c_int)    ::  sse_p1
	       integer(c_int)    ::  avx2_p1
	       integer(c_int)    ::  avx512_p1
	       integer(c_int)    ::  amx_p1
	       integer(c_int)    ::  mem_freq
	       integer(c_size_t) ::  core_cpumask_size
	       type(c_ptr)       ::  core_cpumask
	       integer(c_int)    ::  cpu_count
	       integer(c_size_t) :: trl_cores;	!/* Buckets info */
	       integer(c_int), dimension(TRL_MAX_LEVELS,ISST_TRL_MAX_ACTIVE_CORES) :: trl_ratios
	       integer(c_int)    :: kobj_bucket_index
	       integer(c_int)    :: active_bucket
	       integer(c_int)    :: fact_max_index
	       integer(c_int)    :: fact_max_config
	       integer(c_int)    :: pbf_found
	       integer(c_int)    :: pbf_active
	       type(isst_pbf_info)  ::  pbf_info
	       type(isst_fact_info) ::  fact_info
         end type isst_pkg_ctdp_level_info

         type, bind(c) :: isst_pkg_ctdp
               integer(c_int) ::  locked
	       integer(c_int) ::  version
	       integer(c_int) ::  processed
	       integer(c_int) ::  levels
	       integer(c_int) ::  current_level
	       integer(c_int) ::  enabled
         end type isst_pkg_ctdp
         
         type, bind(c) :: _cpu_map
               integer(c_short) :: core_id
               integer(c_short) :: pkg_id
               integer(c_short) :: die_id
               integer(c_short) :: punit_id
               integer(c_short) :: punit_cpu
               integer(c_short) :: punit_cpu_core
               integer(c_short) :: initialized 
         end type _cpu_map
         
         type(c_ptr), bind(c), public :: cpu_map = c_null_ptr
         
         type, bind(c) :: cpu_topology
               integer(c_short) :: cpu
               integer(c_short) :: core_id
               integer(c_short) :: pkg_id
               integer(c_short) :: die_id
         end type cpu_topology
         
         enum, bind(c)
             enumerator :: ISST_PARAM_MBOX_DELAY
	     enumerator :: ISST_PARAM_MBOX_RETRIES
         end enum
         
         
         interface
             function is_clx_n_platform() result(val) &
                            bind(c,name='is_clx_n_platform')
                      use, intrinsic :: ISO_C_BINDING
                      integer(c_int) :: val
             end function is_clx_n_platform
         end interface
         
         interface
             function is_skx_n_platform() result(val) &
                            bind(c,name='is_skx_n_platform')
                      use, intrinsic :: ISO_C_BINDING
                      integer(c_int) :: val
             end function is_skx_n_platform
         end interface
         
         interface
             function is_spr_n_platform() result(val) &
                            bind(c,name='is_spr_n_platform')
                      use, intrinsic :: ISO_C_BINDING
                      integer(c_int) :: val
             end function is_spr_n_platform
         end interface
         
         interface
             function is_emr_n_platform() result(val) &
                            bind(c,name='is_emr_n_platform')
                      use, intrinsic :: ISO_C_BINDING
                      integer(c_int) :: val
             end function is_emr_n_platform
         end interface
         
         interface
             function is_icx_n_platform() result(val) &
                            bind(c,name='is_icx_n_platform')
                      use, intrinsic :: ISO_C_BINDING
                      integer(c_int) :: val
             end function is_icx_n_platform
         end interface
         
         interface
             function cpufreq_sysfs_present() result(val) &
                            bind(c,name='cpufreq_sysfs_present')
                       use, intrinsic :: ISO_C_BINDING
                       integer(c_int) :: val
             end function cpufreq_sysfs_present
         end interface
         
         ! Defined as a static (in original file, i.e. isst-config.c), but in my version
         ! those functions (rather part of them) as redefined as a non static.
         
         ! static int get_stored_topology_info(int cpu, int *core_id, int *pkg_id, int *die_id)
         interface
             function get_stored_topology_info(cpu,core_id, &
                                               pkg_id,die_id) result(val) &
                            bind(c,name='get_stored_topology_info')
                       use, intrinsic :: ISO_C_BINDING
                       integer(c_int),   intent(in), value :: cpu
                       integer(c_int),   intent(out)       :: core_id
                       integer(c_int),   intent(out)       :: pkg_id
                       integer(c_int),   intent(out)       :: die_id
             end function get_stored_topology_info
         end interface
         
         
         interface 
             subroutine store_cpu_topology() 
                            bind(c,name='store_cpu_topology')
                        use, intrinsic :: ISO_C_BINDING
             end subroutine store_cpu_topology
         end interface
         
         interface
             function get_physical_package_id(cpu) result(val) &
                            bind(c,name='get_physical_package_id')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),   intent(in), value :: cpu
                        integer(c_int) :: val
             end function get_physical_package_id
         end interface
         
         interface
             function get_physical_core_id(cpu) result(val) &
                            bind(c,name='get_physical_core_id')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),   intent(in), value :: cpu
                        integer(c_int) :: val
             end function get_physical_core_id
         end interface
         
         interface
             function get_physical_die_id(cpu) result(val) &
                            bind(c,name='get_physical_die_id')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),   intent(in), value :: cpu
                        integer(c_int) :: val
             end function get_physical_die_id
         end interface
         
         interface
             function get_physical_punit_id(cpu) result(val) &
                            bind(c,name='get_physical_punit_id')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),   intent(in), value :: cpu
                        integer(c_int) :: val
             end function get_physical_punit_id
         end interface
         
         interface
             subroutine set_isst_id(id,cpu) &
                            bind(c,name='set_isst_id')
                        use, intrinsic :: ISO_C_BINDING
                        type(c_ptr),     intent(inout), value :: id
                        integer(c_int),  intent(in), value    :: cpu
             end subroutine set_isst_id
         end interface
         
         interface
             function is_cpu_in_power_domain(cpu,id) result(val) &
                             bind(c,name='is_cpu_in_power_domain')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),  intent(in), value    :: cpu
                        type(c_ptr),     intent(inout), value :: id
             end function is_cpu_in_power_domain
         end interface
         
         interface
             function get_cpufreq_base_freq(cpu) result(val) &
                             bind(c,name='get_cpufreq_base_freq')
                       use, intrinsic :: ISO_C_BINDING
                       integer(c_int),   intent(in), value :: cpu 
                       integer(c_int)  :: val
             end function get_cpufreq_base_freq
        end interface
        
        interface
             function get_topo_max_cpus() result(val) &
                              bind(c,name='get_topo_max_cpus')
                       use, intrinsic :: ISO_C_BINDING
                       integer(c_int) :: val
             end function get_topo_max_cpus
        end interface
        
        interface
              function is_cpu_online(cpu) result(val) &
                              bind(c,name='is_cpu_online')
                       use, intrinsic :: ISO_C_BINDING
                       integer(c_int),   intent(in), value :: cpu
                       integer(c_int) :: val
              end function is_cpu_online
        end interface
        
        interface
              function get_kernel_version(major,minor) result(val) &
                              bind(c,name='get_kernel_version')
                        use, intrinsic :: ISO_C_BINDING
                        integer(c_int),   intent(out) :: major
                        integer(c_int),   intent(out) :: minor
                        integer(c_int) :: val
              end function get_kernel_version 
        end interface
        
        interface
              subroutine set_cpu_online_offline(cpu,state) &
                               bind(c,name='set_cpu_online_offline')
                           use, intrinsic :: ISO_C_BINDING
                           integer(c_int),  intent(in), value :: cpu
                           integer(c_int),  intent(in), value :: state
              end subroutine set_cpu_online_offline
        end interface
        
        interface
              subroutine force_all_cpus_online() &
                                bind(c,name='force_all_cpus_online')
                             use, intrinsic :: ISO_C_BINDING
              end subroutine force_all_cpus_online
        end interface
        
        interface
              subroutine set_max_cpu_num() &
                                bind(c,name='set_max_cpu_num')
                              use, intrinsic :: ISO_C_BINDING
              end subroutine set_max_cpu_num
        end interface
        
        interface
              function get_max_punit_core_id(id) result(val) &
                                bind(c,name='get_max_punit_core_id')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id
                             integer(c_int)  :: val
              end function get_max_punit_core_id
        end interface
        
        interface
              function get_cpu_count(id) result(val) &
                                bind(c,name='get_cpu_count')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id
                             integer(c_int)  :: val
              end function get_cpu_count
        end interface
        
        interface
              subroutine create_cpu_map() & 
                                bind(c,name='create_cpu_map')
                              use, intrinsic :: ISO_C_BINDING
              end subroutine create_cpu_map
        end interface
        
        interface
              function find_phy_core_num(logical_cpu) result(val) &
                                bind(c,name='find_phy_core_num')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in), value :: logical_cpu
                              integer(c_int) :: val
              end function find_phy_core_num
        end interface
        
        interface
              function enable_cpuset_controller() result(val) &
                                bind(c,name='enable_cpuset_controller')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int) :: val
              end function enable_cpuset_controller
        end interface
        
        interface
              function isst_fill_platform_info() result(val) &
                                bind(c,name='isst_fill_platform_info')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int) :: val
              end function isst_fill_platform_info
        end interface
        
        interface
              subroutine isst_print_extended_platform_info() & 
                                bind(c,name='isst_print_extended_platform_info')
                              use, intrinsic :: ISO_C_BINDING
              end subroutine isst_print_extended_platform_info
        end interface
        
        interface
              subroutine isst_print_platform_information() & 
                                bind(c,name='isst_print_platform_information')
                              use, intrinsic :: ISO_C_BINDING
              end subroutine isst_print_platform_information
        end interface
        
        interface
              function clx_n_get_base_ratio() result(val) &
                                bind(c,name='clx_n_get_base_ratio')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int) :: val
              end function clx_n_get_base_ratio 
        end interface
        
        interface
              function clx_n_config(id) result(val) result(val) &
                                bind(c,name='clx_n_config')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              integer(c_int) :: val
              end function clx_n_config
        end interface
        
        interface
              subroutine dump_clx_n_config_for_cpu(id,arg1,arg2, &
                                                   arg3,arg4) &
                                bind(c,name='dump_clx_n_config_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4
              end subroutine dump_clx_n_config_for_cpu
        end interface
        
        interface
              subroutine dump_isst_config_for_cpu(id,arg1,arg2, &
                                                   arg3,arg4) &
                                bind(c,name='dump_isst_config_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4
              end subroutine dump_isst_config_for_cpu
        end interface
        
        interface
            subroutine dump_isst_config(arg) & 
                              bind(c,name='dump_isst_config')
                            use, intrinsic :: ISO_C_BINDING
                            integer(c_int),   intent(in) :: arg
            end subroutine dump_isst_config
        end interface
        
        interface
              subroutine set_tdp_level_for_cpu(id,arg1,arg2, &
                                                   arg3,arg4) &
                                bind(c,name='set_tdp_level_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4
              end subroutine set_tdp_level_for_cpu
        end interface
        
        interface
            subroutine set_tdp_level(arg)  &
                                bind(c,name='set_tdp_level')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in) :: arg
                            
            end subroutine set_tdp_level
        end interface
        
        interface
            subroutine set_tdp_level(arg)  &
                                bind(c,name='set_tdp_level')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in) :: arg
                            
            end subroutine set_tdp_level
        end interface
        
        interface
            function set_cpufreq_scaling_min_max(cpu,max,freq) result(val) &
                                  bind(c,name='set_cpufreq_scaling_min_max')
                                use, intrinsic :: ISO_C_BINDING
                                integer(c_int),   intent(in) :: cpu
                                integer(c_int),   intent(in) :: max
                                integer(c_int),   intent(in) :: freq
                                integer(c_int) :: val
            end function set_cpufreq_scaling_min_max
        end interface
        
        interface
            subroutine adjust_scaling_max_from_base_freq(cpu)  &
                                bind(c,name='adjust_scaling_max_from_base_freq')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in) :: cpu
            end subroutine adjust_scaling_max_from_base_freq
        end interface
        
        interface
            subroutine adjust_scaling_min_from_base_freq(cpu)  &
                                bind(c,name='adjust_scaling_min_from_base_freq')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in) :: cpu
            end subroutine adjust_scaling_min_from_base_freq
        end interface
        
        interface
            function set_cpufreq_scaling_min_max_from_cpuinfo(cpu,
                                                cpuinfo_max,scaling_max) & result(val) &
                                bind(c,name='set_cpufreq_scaling_min_max_from_cpuinfo')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in) :: cpu
                              integer(c_int),   intent(in) :: cpuinfo_max
                              integer(c_int),   intent(in) :: scaling_max
                              integer(c_int) :: val                         
            end function set_cpufreq_scaling_min_max_from_cpuinfo
                               
        end interface
        
        interface
             subroutine set_scaling_min_to_cpuinfo_max(id) &
                                bind(c,name='set_scaling_min_to_cpuinfo_max')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id     
             end subroutine set_scaling_min_to_cpuinfo_max
        end interface
        
        interface
             subroutine set_scaling_min_to_cpuinfo_min(id) &
                                bind(c,name='set_scaling_min_to_cpuinfo_min')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id     
             end subroutine set_scaling_min_to_cpuinfo_min
        end interface
        
        interface
             subroutine set_scaling_max_to_cpuinfo_max(id) &
                                bind(c,name='set_scaling_max_to_cpuinfo_max')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id     
             end subroutine set_scaling_max_to_cpuinfo_max
        end interface
        
        interface
            function set_core_priority_and_min(id,mask_size,cpu_mask, &
                                               min_high,min_low) result(val) &
                                bind(c,name='set_core_priority_and_min')
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id
                             integer(c_int),   intent(in),    value :: mask_size
                             type(c_ptr),      intent(inout), value :: cpu_mask
                             integer(c_int),   intent(in),    value :: min_high
                             integer(c_int),   intent(in),    value :: min_low
                             integer(c_int) :: val     
            end function set_core_priority_and_min
        end interface
        
        interface
             function set_pbf_core_power(id) result(val) &
                                bind(c,name='set_pbf_core_power') 
                             use, intrinsic :: ISO_C_BINDING
                             type(c_ptr),      intent(inout), value :: id
                             integer(c_int) :: val
             end function set_pbf_core_power
        end interface
        
        interface
             subroutine set_pbf_for_cpu(id,arg1,arg2,arg3,arg4) &
                                 bind(c,name='set_pbf_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4  
             end subroutine set_pbf_for_cpu
        end interface
        
        interface
             subroutine set_pbf_enable(arg) &
                                 bind(c,name='set_pbf_enable')
                              use, intrinsic :: ISO_C_BINDING
                              integer(c_int),   intent(in), value :: arg
             end subroutine set_pbf_enable
        end interface
        
        interface
             subroutine dump_fact_config_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='dump_fact_config_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine dump_fact_config_for_cpu
        end interface
        
        interface
             subroutine set_fact_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='set_fact_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine set_fact_for_cpu
        end interface
        
        interface
            subroutine set_fact_enable(arg) &
                                  bind(c,name='set_fact_enable')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine set_fact_enable 
        end interface
        
        interface
             subroutine enable_clos_qos_config(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='enable_clos_qos_config')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine enable_clos_qos_config
        end interface
        
         interface
            subroutine set_clos_enable(arg) &
                                  bind(c,name='set_clos_enable')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine set_clos_enable 
        end interface
        
        interface
             subroutine dump_clos_config_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='dump_clos_config_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine dump_clos_config_for_cpu
        end interface
        
        interface
            subroutine dump_clos_config(arg) &
                                  bind(c,name='dump_clos_config')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine dump_clos_config
        end interface
        
        interface
             subroutine get_clos_info_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='get_clos_info_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine get_clos_info_for_cpu
        end interface
        
        interface
            subroutine dump_clos_info(arg) &
                                  bind(c,name='dump_clos_info')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine dump_clos_info
        end interface
        
        interface
             subroutine set_clos_config_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='set_clos_config_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine set_clos_config_for_cpu
        end interface
        
        interface
            subroutine set_clos_config(arg) &
                                  bind(c,name='set_clos_config')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine set_clos_config
        end interface
        
        interface
             subroutine set_clos_assoc_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='set_clos_assoc_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine set_clos_assoc_for_cpu
        end interface
        
        interface
            subroutine set_clos_assoc(arg) &
                                  bind(c,name='set_clos_assoc')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine set_clos_assoc
        end interface
        
        interface
             subroutine get_clos_assoc_for_cpu(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='get_clos_assoc_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine get_clos_assoc_for_cpu
        end interface
        
        interface
            subroutine get_clos_assoc(arg) &
                                  bind(c,name='get_clos_assoc')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine get_clos_assoc
        end interface
        
        interface
             subroutine set_turbo_mode_for_cpu(id,status) &
                                  bind(c,name='set_turbo_mode_for_cpu')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              integer(c_int),intent(in),    value :: status     
             end subroutine set_turbo_mode_for_cpu
        end interface
        
        interface
            subroutine set_turbo_mode(arg) &
                                  bind(c,name='set_turbo_mode')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine set_turbo_mode
        end interface
        
        interface
             subroutine get_set_trl(id,arg1,arg2,arg3,arg4) &
                                  bind(c,name='get_set_trl')
                              use, intrinsic :: ISO_C_BINDING
                              type(c_ptr),   intent(inout), value :: id
                              type(c_ptr),   intent(inout), value :: arg1
                              type(c_ptr),   intent(inout), value :: arg2
                              type(c_ptr),   intent(inout), value :: arg3
                              type(c_ptr),   intent(inout), value :: arg4       
             end subroutine get_set_trl
        end interface
        
        interface
            subroutine process_trl(arg) &
                                  bind(c,name='process_trl')
                                use, intrinsic :: ISO_C_BINDING   
                                integer(c_int),   intent(in), value :: arg   
            end subroutine process_trl
        end interface


end module isst_iface
