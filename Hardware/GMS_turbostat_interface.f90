

module turbostat_interface


  !========================================================!
  ! Fortran interfaces to some of the functionality
  ! as presented by the program 'turbostat'.
  ! Experimental implementation!!
  !========================================================!
  use, intrinsic :: ISO_C_BINDING
  implicit none
  public

#define RAPL_PKG		(lshift(1,0))
					! 0x610 MSR_PKG_POWER_LIMIT 
					! 0x611 MSR_PKG_ENERGY_STATUS 
#define RAPL_PKG_PERF_STATUS	(lshift(1,1))
					! 0x613 MSR_PKG_PERF_STATUS */
#define RAPL_PKG_POWER_INFO	(lshift(1,2))
					! 0x614 MSR_PKG_POWER_INFO */

#define RAPL_DRAM		(lshift(1,3))
					! 0x618 MSR_DRAM_POWER_LIMIT */
					! 0x619 MSR_DRAM_ENERGY_STATUS */
#define RAPL_DRAM_PERF_STATUS	(lshift(1,4))
					! 0x61b MSR_DRAM_PERF_STATUS */
#define RAPL_DRAM_POWER_INFO	(lshift(1,5))
					! 0x61c MSR_DRAM_POWER_INFO */

#define RAPL_CORES_POWER_LIMIT	(lshift(1,6))
					! 0x638 MSR_PP0_POWER_LIMIT */
#define RAPL_CORE_POLICY	(lshift((1,7))
					! 0x63a MSR_PP0_POLICY */

#define RAPL_GFX		(lshift(1,8))
					!/* 0x640 MSR_PP1_POWER_LIMIT */
					!/* 0x641 MSR_PP1_ENERGY_STATUS */
					!/* 0x642 MSR_PP1_POLICY */

#define RAPL_CORES_ENERGY_STATUS	(lshift(1,9))
					!/* 0x639 MSR_PP0_ENERGY_STATUS */
#define RAPL_PER_CORE_ENERGY	(lshift((1,10))
					!/* Indicates cores energy collection is per-core,
					! * not per-package. */
#define RAPL_AMD_F17H		(lshift(1,11))
					!/* 0xc0010299 MSR_RAPL_PWR_UNIT */
					!/* 0xc001029a MSR_CORE_ENERGY_STAT */
					!/* 0xc001029b MSR_PKG_ENERGY_STAT */
#define RAPL_CORES (RAPL_CORES_ENERGY_STATUS | RAPL_CORES_POWER_LIMIT)
#define	TJMAX_DEFAULT	100

!/* MSRs that are not yet in the kernel-provided header. */
#define MSR_RAPL_PWR_UNIT	0xc0010299
#define MSR_CORE_ENERGY_STAT	0xc001029a
#define MSR_PKG_ENERGY_STAT	0xc001029b

#define MAX_ADDED_COUNTERS 8
#define MAX_ADDED_THREAD_COUNTERS 24
#define BITMASK_SIZE 32

#define	NAME_BYTES 20
#define PATH_BYTES 128

  type, bind(c) :: core_data

     integer(c_size_t) :: c3
     integer(c_size_t) :: c6
     integer(c_size_t) :: c7
     integer(c_size_t) :: mc6_us
     integer(c_int)    :: core_temp_c
     integer(c_int)    :: core_energy
     integer(c_int)    :: core_id
     integer(c_size_t), dimension(MAX_ADDED_COUNTERS) :: counter
  end type core_data

  type, bind(c) :: pkg_data

     integer(c_size_t) :: pc2
     integer(c_size_t) :: pc3
     integer(c_size_t) :: pc6
     integer(c_size_t) :: pc7
     integer(c_size_t) :: pc8
     integer(c_size_t) :: pc9
     integer(c_size_t) :: pc10
     integer(c_size_t) :: cpu_lpi
     integer(c_size_t) :: sys_lpi
     integer(c_size_t) :: pkg_wtd_core_c0
     integer(c_size_t) :: pkg_any_core_c0
     integer(c_size_t) :: pkg_any_gfxe_c0
     integer(c_size_t) :: pkg_both_core_gfxe_c0
     integer(c_long_long) :: gfx_rc6_ms
     integer(c_int)    :: gfx_mhz
     integer(c_int)    :: gfx_act_mhz
     integer(c_int)    :: package_id
     integer(c_size_t) :: energy_pkg !MSR_PKG_ENERGY_STATUS
     integer(c_size_t) :: energy_dram !MSR_DRAM_ENERGY_STATUS
     integer(c_size_t) :: energy_cores ! MSR_PP0_ENERGY_STATUS
     integer(c_size_t) :: energy_gfx   !MSR_PP1_ENERGY_STATUS
     integer(c_size_t) :: rapl_pkg_perf_status
     integer(c_size_t) :: rapl_dram_perf_status
     integer(c_int)    :: pkg_temp_c
     integer(c_size_t), dimension(MAX_ADDED_COUNTERS) :: counter
  end type pkg_data

  type, bind(c) :: system_summary
     type(thread_data) :: threads
     type(core_data)   :: cores
     type(pkg_data)    :: packages
  end type system_summary

  type(system_summary) :: average
  

  enum, bind(c)
     enumerator :: SCOPE_CPU
     enumerator :: SCOPE_CORE
     enumerator :: SCOPE_PACKAGE
  end enum

  enum, bind(c)
     enumerator :: COUNTER_ITEMS
     enumerator :: COUNTER_CYCLES
     enumerator :: COUNTER_SECONDS
     enumerator :: COUNTER_USEC
  end enum

  enum, bind(c)
     enumerator :: FORMAT_RAW
     enumerator :: FORMAT_DELTA
     enumerator :: FORMAT_PERCENT
  end enum

  type(c_ptr), bind(c), public :: outp = c_null_ptr
  
  
  !type, bind(c) :: msr_counter
  !   integer(c_int) :: msr_num
  !   character(c_char), dimension(NAME_BYTES) :: name
 !    character(c_char), dimension(PATH_BYTES) :: path
  !   integer(c_int) :: width
 !    
 ! end type msr_counter

  ! off_t idx_to_offset(int idx)
  interface
     function idx_to_offset(idx) result(offset) &
          bind(c,name='idx_to_offset')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: idx
          integer(c_int) :: offset
     end function idx_to_offset
 
  end interface

  !int offset_to_idx(off_t offset)
  interface
     function offset_to_idx(offset) result(idx) &
          bind(c,name='offset_to_idx')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: offset
          integer(c_int) :: idx
     end function offset_to_idx
     
  end interface


  interface
     function idx_valid(idx) result(res) &
          bind(c,name='idx_valid')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: idx
          integer(c_int) :: res
     end function idx_valid

  end interface

     
  interface
     function cpu_is_not_present(cpu) result(res) &
          bind(c,name='cpu_is_not_present')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: cpu
          integer(c_int) :: res
        end function cpu_is_not_present
        
   end interface


   interface
      function cpu_migrate(cpu) result(res) &
           bind(c,name='cpu_migrate')
           use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: cpu
          integer(c_int) :: res
      end function cpu_migrate

   end interface


   interface
      function get_msr_fd(cpu) result(fd) &
           bind(c,name='get_msr_fd')
           use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: cpu
          integer(c_int) :: fd
      end function get_msr_fd
      
   end interface


   interface
      function get_instr_count_fd(cpu) result(count) &
           bind(c,name='get_instr_count_fd')
           use, intrinsic :: ISO_C_BINDING
          integer(c_int),   intent(in), value :: cpu
          integer(c_int) :: count
      end function get_instr_count_fd
      
   end interface


   interface
      function get_msr(cpu,offset,msr) result(retval) &
           bind(c,name='get_msr')
           use, intrinsic :: ISO_C_BINDING
           integer(c_int),    intent(in), value :: cpu
           integer(c_int),    intent(in), value :: offset
           integer(c_size_t), intent(inout)     :: msr
           integer(c_size_t) :: retval
      end function get_msr
      
   end interface


   interface
      function dump_counters(t,c,p) result(res) &
           bind(c,name='dump_counters')
           use, intrinsic :: ISO_C_BINDING
           type(c_ptr),     intent(inout), value :: t
           type(c_ptr),     intent(inout), value :: c
           type(c_ptr),     intent(inout), value :: p
           integer(c_int) :: res
      end function dump_counters
        
   end interface



end module turbostat_interface
