
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
         
         enum, bind(c)
             enumerator :: ISST_PARAM_MBOX_DELAY
	     enumerator :: ISST_PARAM_MBOX_RETRIES
         end enum



end module isst_iface
