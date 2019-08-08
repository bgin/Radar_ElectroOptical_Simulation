

module mod_core_events

  ! Based on haswell_core_v28.json
  ! rNNNN -- as used by the perf -- UMASK+EVENT
  ! "EventName": "INST_RETIRED.ANY",
  !  "BriefDescription": "Instructions retired from execution.",
  character(*),     parameter, public :: INST_RETIRED_ANY = "r100"
  !  "EventName": "CPU_CLK_UNHALTED.THREAD",
  !  "BriefDescription": "Core cycles when the thread is not in halt state.",
  character(*),     parameter, public :: CPU_CLK_UNHALTED_THREAD = "r200"
  !  "EventName": "CPU_CLK_UNHALTED.THREAD_ANY",
  !  "BriefDescription": "Core cycles when at least one thread on the physical core is not in halt state.",
  character(*),     parameter, public :: CPU_CLK_UNHALTED_THREAD_ANY = "r200"
  !  "EventName": "CPU_CLK_UNHALTED.REF_TSC",
  !  "BriefDescription": "Reference cycles when the core is not in halt state.",
  character(*),     parameter, public :: CPU_CLK_UNHALTED_REF_TSC    = "r300"
  !  "EventName": "LD_BLOCKS.STORE_FORWARD",
  !  "BriefDescription": "loads blocked by overlapping with store buffer that cannot be forwarded",
  character(*),     parameter, public :: LD_BLOCKS_STORE_FORWARD     = "r203"
  !   "EventName": "LD_BLOCKS.NO_SR",
  !   "BriefDescription": "The number of times that split load operations are temporarily blocked because all resources for handling the split accesses are in use",
  character(*),     parameter, public :: LD_BLOCKS_NO_SR             = "r803"
  !    "EventName": "MISALIGN_MEM_REF.LOADS",
  !    "BriefDescription": "Speculative cache line split load uops dispatched to L1 cache",
  character(*),     parameter, public :: MISALIGN_MEM_REF_LOADS      = "r105"
  !   "EventName": "MISALIGN_MEM_REF.STORES",
  !   "BriefDescription": "Speculative cache line split STA uops dispatched to L1 cache",
  character(*),     parameter, public :: MISALIGN_MEM_REF_STORES     = "r205"
  !   "EventName": "LD_BLOCKS_PARTIAL.ADDRESS_ALIAS",
  !   "BriefDescription": "False dependencies in MOB due to partial compare on address.",
  !             "PublicDescription": "Aliasing occurs when a load is issued after a store and their memory addresses are offset by 4K.  This event counts the number of loads that aliased with a precedin  !  g store, resulting in an extended address check in the pipeline which can have a performance impact."
  character(*),     parameter, public :: LD_BLOCKS_PARTIAL_ADDRESS_ALIAS = "r107"
  !  "EventName": "DTLB_LOAD_MISSES.MISS_CAUSES_A_WALK",
  !  "BriefDescription": "Load misses in all DTLB levels that cause page walks",
  character(*),     parameter, public :: DTLB_LOAD_MISSES_MISS_CAUSES_A_WALK = "r108"
  !  "EventName": "DTLB_LOAD_MISSES.WALK_COMPLETED_4K",
  !  "BriefDescription": "Demand load Miss in all translation lookaside buffer (TLB) levels causes a page walk that completes (4K)."
  character(*),     parameter, public :: DTLB_LOAD_MISSES_WALK_COMPLETED_4K  = "r208"
  !   "EventName": "DTLB_LOAD_MISSES.WALK_COMPLETED_2M_4M",
  !   "BriefDescription": "Demand load Miss in all translation lookaside buffer (TLB) levels causes a page walk that completes (2M/4M)."
  character(*),     parameter, public :: DTLB_LOAD_MISSES_WALK_COMPLETED_2M_4M = "r408"
  !    "EventName": "DTLB_LOAD_MISSES.WALK_COMPLETED_1G",
  !    "BriefDescription": "Load miss in all TLB levels causes a page walk that completes. (1G)"
  character(*),     parameter, public :: DTLB_LOAD_MISSES_WALK_COMPLETED_1G    = "r808"
  !    "EventName": "DTLB_LOAD_MISSES.WALK_COMPLETED",
  !    "BriefDescription": "Demand load Miss in all translation lookaside buffer (TLB) levels causes a page walk that completes of any page size."
  character(*),     parameter, public :: DTLB_LOAD_MISSES_WALK_COMPLETED       = "re08"
  !    "EventName": "DTLB_LOAD_MISSES.WALK_DURATION",
  !    "BriefDescription": "Cycles when PMH is busy with page walks",
  !    "PublicDescription": "This event counts cycles when the  page miss handler (PMH) is servicing page walks caused by DTLB load misses.",
  character(*),     parameter, public :: DTLB_LOAD_MISSES_WALK_DURATION        = "r1008"
  !     "EventName": "DTLB_LOAD_MISSES.STLB_HIT_4K",
  !     "BriefDescription": "Load misses that miss the  DTLB and hit the STLB (4K)",
  !     "PublicDescription": "This event counts load operations from a 4K page that miss the first DTLB level but hit the second and do not cause page walks.",
  character(*),     parameter, public :: DTLB_LOAD_MISSES_STLB_HIT_4K          = "r2008"
  !     "EventName": "DTLB_LOAD_MISSES.STLB_HIT_2M",
  !     "BriefDescription": "Load misses that miss the  DTLB and hit the STLB (2M)",
  !     "PublicDescription": "This event counts load operations from a 2M page that miss the first DTLB level but hit the second and do not cause page walks.",
  character(*),     parameter, public :: DTLB_LOAD_MISSES_STLB_HIT_2M          = "r4008"
  !     "EventName": "DTLB_LOAD_MISSES.STLB_HIT",
  !     "BriefDescription": "Load operations that miss the first DTLB level but hit the second and do not cause page walks"
  character(*),     parameter, public :: DTLB_LOAD_MISSES_STLB_HIT             = "r6008"
  !      "EventName": "DTLB_LOAD_MISSES.PDE_CACHE_MISS",
  !      "BriefDescription": "DTLB demand load misses with low part of linear-to-physical address translation missed"
  character(*),     parameter, public :: DTLB_LOAD_MISSIES_PDE_CACHE_MISS      = "r8008"
  !      "EventName": "INT_MISC.RECOVERY_CYCLES",
  !  "BriefDescription": "Core cycles the allocator was stalled due to recovery from earlier clear event for this thread (e.g. misprediction or memory nuke)",
  !  "PublicDescription": "This event counts the number of cycles spent waiting for a recovery after an event such as a processor nuke, JEClear, assist, hle/rtm abort etc
  character(*),     parameter, public :: INT_MISC_RECOVERY_CYCLES              = "r30D"
  !     "EventName": "UOPS_ISSUED.ANY",
  !  "BriefDescription": "Uops that Resource Allocation Table (RAT) issues to Reservation Station (RS)",
  !  "PublicDescription": "This event counts the number of uops issued by the Front-end of the pipeline to the Back-end. This event is counted at the allocation stage and will count both retired and non  !   !-retired uops.",
  character(*),     parameter, public :: UOPS_ISSUED_ANY                       = "r100E"
  !   "EventName": "UOPS_ISSUED.STALL_CYCLES",
  !   "BriefDescription": "Cycles when Resource Allocation Table (RAT) does not issue Uops to Reservation Station (RS) for the thread.
  character(*),     parameter, public :: UOPS_ISSUED_STALL_CYCLES              = "r100E"
  !    "EventName": "UOPS_ISSUED.CORE_STALL_CYCLES",
  !    "BriefDescription": "Cycles when Resource Allocation Table (RAT) does not issue Uops to Reservation Station (RS) for all threads."
  character(*),     parameter, public :: UOPS_ISSUED_CORE_STALL_CYCLES         = "r1O0E"
  !   "EventName": "UOPS_ISSUED.FLAGS_MERGE",
  !   "BriefDescription": "Number of flags-merge uops being allocated. Such uops considered perf sensitive; added by GSR u-arch."
  character(*),     parameter, public :: UOPS_ISSUED_FLAGS_MERGE               = "r100E"
  !    "EventName": "UOPS_ISSUED.SLOW_LEA",
  !    "BriefDescription": "Number of slow LEA uops being allocated. A uop is generally considered SlowLea if it has 3 sources
  character(*),     parameter, public :: UOPS_ISSUED_SLOW_LEA                  = "r200E"
  !    "EventName": "UOPS_ISSUED.SINGLE_MUL",
  !    "BriefDescription": "Number of Multiply packed/scalar single precision uops allocated"
  character(*),     parameter, public :: UOPS_ISSUED_SINGLE_MUL                = "r400E"
  !    "EventName": "ARITH.DIVIDER_UOPS",
  !    "BriefDescription": "Any uop executed by the Divider. (This includes all divide uops, sqrt, ...)"
  character(*),     parameter, public :: ARITH_DIVIDER_UOPS                    = "r0214"
  !     "EventName": "L2_RQSTS.DEMAND_DATA_RD_MISS",
  !    "BriefDescription": "Demand Data Read miss L2, no rejects",
  character(*),     parameter, public :: L2_RQSTS_DEMAND_DATA_RD_MISS          = "r2124"
  !    "EventName": "L2_RQSTS.RFO_MISS",
  !  "BriefDescription": "RFO requests that miss L2 cache",
  !  "PublicDescription": "Counts the number of store RFO requests that miss the L2 cache."
  character(*),     parameter, public :: L2_RQSTS_RFO_MISS                     = "r2224"
  
end module mod_core_events





module mod_uncore_events



end module mod_uncore_events
