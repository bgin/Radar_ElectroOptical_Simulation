

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
  !  "PublicDescription": "This event counts the number of uops issued by the Front-end of the pipeline to the Back-end. This event is counted at the
   ! allocation stage and will count both retired and non  !   !-retired uops.",
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
  !  "EventName": "L2_RQSTS.CODE_RD_MISS",
  !  "BriefDescription": "L2 cache misses when fetching instructions",
  character(*),     parameter, public :: L2_RQSTS_CODE_RD_MISS                 = "r2424"
  !   "EventName": "L2_RQSTS.ALL_DEMAND_MISS",
  !   "BriefDescription": "Demand requests that miss L2 cache",
  character(*),     parameter, public :: L2_RQSTS_ALL_DEMAND_MISS              = "r2724"
  !   "EventName": "L2_RQSTS.L2_PF_MISS",
  !  "BriefDescription": "L2 prefetch requests that miss L2 cache",
  !  "PublicDescription": "Counts all L2 HW prefetcher requests that missed L2.
  character(*),     parameter, public :: L2_RQSTS_L2_PF_MISS                   = "r3024"
  !   "EventName": "L2_RQSTS.MISS",
  !   "BriefDescription": "All requests that miss L2 cache"
  character(*),     parameter, public :: L2_RQSTS_MISS                         = "r3F24"
  !   "EventName": "L2_RQSTS.DEMAND_DATA_RD_HIT",
  !  "BriefDescription": "Demand Data Read requests that hit L2 cache",
  !  "PublicDescription": "Counts the number of demand Data Read requests, initiated by load instructions, that hit L2 cache"
  character(*),     parameter, public :: L2_RQSTS_DEMAND_DATA_RD_HIT           = "rC124"
  !   "EventName": "L2_RQSTS.RFO_HIT",
  !  "BriefDescription": "RFO requests that hit L2 cache",
  !  "PublicDescription": "Counts the number of store RFO requests that hit the L2 cache."
  character(*),     parameter, public :: L2_RQSTS_RFO_HIT                      = "rC224"
  !  "EventName": "L2_RQSTS.CODE_RD_HIT",
  !  "BriefDescription": "L2 cache hits when fetching instructions, code reads.",
  character(*),     parameter, public :: L2_RQST_CODE_RD_HIT                   = "rC424"
  !   "EventName": "L2_RQSTS.L2_PF_HIT",
  !   "BriefDescription": "L2 prefetch requests that hit L2 cache",
  character(*),     parameter, public :: L2_RQSTS_L2_PF_HIT                    = "rD024"
  !    "EventName": "L2_RQSTS.ALL_DEMAND_DATA_RD",
  !  "BriefDescription": "Demand Data Read requests",
  !  "PublicDescription": "Counts any demand and L1 HW prefetch data load requests to L2."
  character(*),     parameter, public :: L2_RQSTS_ALL_DEMAND_DATA_RD           = "rE124"
  !   "EventName": "L2_RQSTS.ALL_RFO",
  !  "BriefDescription": "RFO requests to L2 cache"
  character(*),     parameter, public :: L2_RQSTS_ALL_RFO                      = "rE224"
  !   "EventName": "L2_RQSTS.ALL_CODE_RD",
  !   "BriefDescription": "L2 code requests
  character(*),     parameter, public :: L2_RQSTS_ALL_CODE_RD                  = "rE424"
  !   "EventName": "L2_RQSTS.ALL_DEMAND_REFERENCES",
  !   "BriefDescription": "Demand requests to L2 cache",
  character(*),     parameter, public :: L2_RQSTS_ALL_DEMAND_REFERENCES        = "rE724"
  !   "EventName": "L2_RQSTS.ALL_PF",
  !   "BriefDescription": "Requests from L2 hardware prefetchers",
  character(*),     parameter, public :: L2_RQSTS_ALL_PF                       = "rF824"
  !     "EventName": "L2_RQSTS.REFERENCES",
  !   "BriefDescription": "All L2 requests"
  character(*),     parameter, public :: L2_RQSTS_REFERENCES                   = "rFF24"
  !  "EventName": "L2_DEMAND_RQSTS.WB_HIT",
  !   "BriefDescription": "Not rejected writebacks that hit L2 cache"
  character(*),     parameter, public :: L2_DEMAND_RQSTS_WB_HIT                = "r5027"
  !    "EventName": "LONGEST_LAT_CACHE.MISS",
  !  "BriefDescription": "Core-originated cacheable demand requests missed L3"
  character(*),     parameter, public :: LONGEST_LAT_CACHE_MISS                = "r412E"
  !  "EventName": "LONGEST_LAT_CACHE.REFERENCE",
  !  "BriefDescription": "Core-originated cacheable demand requests that refer to L3"
  character(*),     parameter, public :: LONGEST_LAT_CACHE_REFERENCE           = "r4F2E"
  !  "EventName": "CPU_CLK_UNHALTED.THREAD_P",
  !  "BriefDescription": "Thread cycles when thread is not in halt state",
  !  "PublicDescription": "Counts the number of thread cycles while the thread is not in a halt state. 
  !   The thread enters the halt state when it is running the HLT instruction. The core frequency may change from time to time due to power or thermal throttling.",
  character(*),     parameter, public :: CPU_CLK_UNHALTED_THREAD_P             = "r003C"
  !   "EventName": "CPU_CLK_UNHALTED.THREAD_P_ANY",
  !   "BriefDescription": "Core cycles when at least one thread on the physical core is not in halt state.",
  character(*),     parameter, public :: CPU_CLK_UNHALTED_THREAD_P_ANY         = "r003C"
  !   "EventName": "CPU_CLK_THREAD_UNHALTED.REF_XCLK",
  !  "BriefDescription": "Reference cycles when the thread is unhalted (counts at 100 MHz rate)",
  !  "PublicDescription": "Increments at the frequency of XCLK (100 MHz) when not halted."
  character(*),     parameter, public :: CPU_CLK_THREAD_UNHALTED_REF_XCLK      = "r013C"
  !    "EventName": "CPU_CLK_THREAD_UNHALTED.REF_XCLK_ANY",
  !  "BriefDescription": "Reference cycles when the at least one thread on the physical core is unhalted (counts at 100 MHz rate)"
  character(*),     parameter, public :: CPU_CLK_THREAD_UNHALTED_REF_XCLK_ANY  = "r013C"
  !    "EventName": "CPU_CLK_UNHALTED.REF_XCLK",
  !   "BriefDescription": "Reference cycles when the thread is unhalted (counts at 100 MHz rate)"
  character(*),     parameter, public :: CPU_CLK_UNHALTED_REF_XCLK             = "r013C"
  !   "EventName": "CPU_CLK_UNHALTED.REF_XCLK_ANY",
  !  "BriefDescription": "Reference cycles when the at least one thread on the physical core is unhalted (counts at 100 MHz rate)"
  character(*),     parameter, public :: CPU_CLK_UNHALTED_REF_XCLK_ANY         = "r013C"
  !   "EventName": "CPU_CLK_THREAD_UNHALTED.ONE_THREAD_ACTIVE",
  !   "BriefDescription": "Count XClk pulses when this thread is unhalted and the other thread is halted."
  character(*),     parameter, public :: CPU_CLK_THREAD_ONE_THREAD_ACTIVE      = "r023C"
  !    "EventName": "CPU_CLK_UNHALTED.ONE_THREAD_ACTIVE",
  !   "BriefDescription": "Count XClk pulses when this thread is unhalted and the other thread is halted.
  character(*),     parameter, public :: CPU_CLK_UNHALTED_ONE_THREAD_ACTIVE    = "r023C"
  !   "EventName": "L1D_PEND_MISS.PENDING",
  !  "BriefDescription": "L1D miss oustandings duration in cycles",
  !  "PublicDescription": "Increments the number of outstanding L1D misses every cycle. Set Cmask = 1 and Edge =1 to count occurrences."
  character(*),     parameter, public :: L1D_MISS_PENDING                      = "r0148"
  !   "EventName": "L1D_PEND_MISS.PENDING_CYCLES",
  !   "BriefDescription": "Cycles with L1D load Misses outstanding.",
  character(*),     parameter, public :: L1_PEND_MISS_PENDING_CYCLES           = "r0148"
  !    "EventName": "L1D_PEND_MISS.PENDING_CYCLES_ANY",
  !    "BriefDescription": "Cycles with L1D load Misses outstanding from any thread on physical core.
  character(*),     parameter, public :: L1_PEND_MISS_PENDING_CYCLES_ANY       = "r0148"
  !   "EventName": "L1D_PEND_MISS.REQUEST_FB_FULL",
  !  "BriefDescription": "Number of times a request needed a FB entry but there was no entry available for it.
  !   That is the FB unavailability was dominant reason for blocking the request. A request includes cacheable/uncacheable demands that is load, store or SW prefetch. HWP are e.
  character(*),     parameter, public :: L1D_PEND_MISS_REQUEST_FB_FULL          = "r0248"
  !   "EventName": "L1D_PEND_MISS.FB_FULL",
  !   "BriefDescription": "Cycles a demand request was blocked due to Fill Buffers inavailability."
  character(*),     parameter, public :: L1D_PEND_MISS_FB_FULL                  = "r0248"
  !    "EventName": "DTLB_STORE_MISSES.MISS_CAUSES_A_WALK",
  !  "BriefDescription": "Store misses in all DTLB levels that cause page walks"
  character(*),     parameter, public :: DTLB_STORE_MISES_MISS_CAUSES_A_WALK    = "r0149"
  !   "EventName": "DTLB_STORE_MISSES.WALK_COMPLETED_4K",
  !  "BriefDescription": "Store miss in all TLB levels causes a page walk that completes. (4K)"
  character(*),     parameter, public :: DTLB_STORE_MISSES_WALK_COMPLETED_4K    = "r0249"
  !   "EventName": "DTLB_STORE_MISSES.WALK_COMPLETED_2M_4M",
  !  "BriefDescription": "Store misses in all DTLB levels that cause completed page walks (2M/4M)",
  character(*),     parameter, public :: DTLB_STORE_MISSES_WALK_COMPLETED_2M_4M = "r0449"
  !  "EventName": "DTLB_STORE_MISSES.WALK_COMPLETED_1G",
  !  "BriefDescription": "Store misses in all DTLB levels that cause completed page walks. (1G)
  character(*),     parameter, public :: DTLB_STORE_MISSES_WALK_COMPLETED_1G    = "r0849"
  !   "EventName": "DTLB_STORE_MISSES.WALK_COMPLETED",
  !   "BriefDescription": "Store misses in all DTLB levels that cause completed page walks",
  character(*),     parameter, public :: DTLB_STORE_MISSES_WALK_COMPLETED       = "r0e49"
  !  "EventName": "DTLB_STORE_MISSES.WALK_DURATION",
  !  "BriefDescription": "Cycles when PMH is busy with page walks",
  !  "PublicDescription": "This event counts cycles when the  page miss handler (PMH) is servicing page walks caused by DTLB store misses.
  character(*),     parameter, public :: DTLB_STORE_MISSES_WALK_DURATION        = "r1049"
  !   "EventName": "DTLB_STORE_MISSES.STLB_HIT_4K",
  !  "BriefDescription": "Store misses that miss the  DTLB and hit the STLB (4K)",
  character(*),     parameter, public :: DTLB_STORE_MISSES_STLB_HIT_4K          = "r2049"
  !  "EventName": "DTLB_STORE_MISSES.STLB_HIT_2M",
  !  "BriefDescription": "Store misses that miss the  DTLB and hit the STLB (2M)",
  !  "PublicDescription": "This event counts store operations from a 2M page that miss the first DTLB level but hit the second and do not cause page walks.",
  character(*),     parameter, public :: DTLB_STORE_MISSES_STLB_HIT_2M          = "r4049"
  !   "EventName": "DTLB_STORE_MISSES.STLB_HIT",
  !  "BriefDescription": "Store operations that miss the first TLB level but hit the second and do not cause page walks",
  character(*),     parameter, public :: DTLB_STORE_MISSES_STLB_HIT             = "r6049"
  !   "EventName": "DTLB_STORE_MISSES.PDE_CACHE_MISS",
  !  "BriefDescription": "DTLB store misses with low part of linear-to-physical address translation missed"
  character(*),     parameter, public :: DTLB_STORE_MISSES_PDE_CACHE_MISS       = "r8049"
  !   "EventName": "LOAD_HIT_PRE.SW_PF",
  !  "BriefDescription": "Not software-prefetch load dispatches that hit FB allocated for software prefetch"
  character(*),     parameter, public :: LOAD_HIT_PRE_SW_PF                     = "r014C"
  !   "EventName": "LOAD_HIT_PRE.HW_PF",
  !  "BriefDescription": "Not software-prefetch load dispatches that hit FB allocated for hardware prefetch"
  character(*),     parameter, public :: LOAD_HIT_PRE_HW_PF                     = "r024C"
  !   "EventName": "EPT.WALK_CYCLES",
  !   "BriefDescription": "Cycle count for an Extended Page table walk.
  character(*),     parameter, public :: EPT_WALK_CYCLES                        = "r104F"
  !  "EventName": "L1D.REPLACEMENT",
  !  "BriefDescription": "L1D data line replacements",
  character(*),     parameter, public :: L1D_REPLACEMENT                        = "r0151"
  !   "EventName": "TX_MEM.ABORT_CONFLICT",
  !  "BriefDescription": "Number of times a transactional abort was signaled due to a data conflict on a transactionally accessed address.
  character(*),     parameter, public :: TX_MEM_ABORT_CONFLICT                  = "r0154"
  !  "EventName": "TX_MEM.ABORT_CAPACITY_WRITE",
  !  "BriefDescription": "Number of times a transactional abort was signaled due to a data capacity limitation for transactional writes.
  character(*),     parameter, public :: TX_MEM_ABORT_CAPACITY_WRITE            = "r0254"
  !   "EventName": "TX_MEM.ABORT_HLE_STORE_TO_ELIDED_LOCK",
  !   "BriefDescription": "Number of times a HLE transactional region aborted due to a non XRELEASE
  !    prefixed instruction writing to an elided lock in the elision buffer.
  character(*),     parameter, public :: TX_MEM_ABORT_HLE_STORE_TO_ELIDED_LOCK  = "r0454"
  !     "EventName": "TX_MEM.ABORT_HLE_ELISION_BUFFER_NOT_EMPTY",
  !   "BriefDescription": "Number of times an HLE transactional execution aborted due to NoAllocatedElisionBuffer being non-zero
  character(*),     parameter, public :: TX_MEM_ABORT_HLE_ELISION_BUFFER_NOT_EMPTY = "r0854"
  !    "EventName": "TX_MEM.ABORT_HLE_ELISION_BUFFER_MISMATCH",
  !    "BriefDescription": "Number of times an HLE transactional execution aborted due to
  !    XRELEASE lock not satisfying the address and value requirements in the elision buffer.
  character(*),     parameter, public :: TX_MEM_ABORT_HLE_ELISION_BUFFER_MISMATCH  = "r1054"
  !    "EventName": "TX_MEM.ABORT_HLE_ELISION_BUFFER_UNSUPPORTED_ALIGNMENT",
  !    "BriefDescription": "Number of times an HLE transactional execution aborted due to an unsupported read alignment from the elision buffer.
  character(*),     parameter, public :: TX_MEM_ABORT_HLE_ELISION_BUFFER_UNSUPPORTED_ALIGNMENT = "r2054"
  !     "EventName": "TX_MEM.HLE_ELISION_BUFFER_FULL",
  !     "BriefDescription": "Number of times HLE lock could not be elided due to ElisionBufferAvailable being zero.
  character(*),     parameter, public :: TX_MEM_HLE_ELISION_BUFFER_FULL            = "r4054"
  !      "EventName": "MOVE_ELIMINATION.INT_ELIMINATED",
  !      "BriefDescription": "Number of integer Move Elimination candidate uops that were eliminated
  character(*),     parameter, public :: MOVE_ELIMINATION_INT_ELIMINATED           = "r0158"
  !    "EventName": "MOVE_ELIMINATION.SIMD_ELIMINATED",
  !    "BriefDescription": "Number of SIMD Move Elimination candidate uops that were eliminated."
  character(*),     parameter, public :: MOVE_ELIMINATION_SIMD_ELIMINATED          = "r0258"
  !     "EventName": "MOVE_ELIMINATION.INT_NOT_ELIMINATED",
  !     "BriefDescription": "Number of integer Move Elimination candidate uops that were not eliminated.
  character(*),     parameter, public :: MOVE_ELIMINATION_INT_NOT_ELIMINATED       = "r0458"
  !    "EventName": "MOVE_ELIMINATION.SIMD_NOT_ELIMINATED",
  !   "BriefDescription": "Number of SIMD Move Elimination candidate uops that were not eliminated
  character(*),     parameter, public :: MOVE_ELIMINATION_SIMD_NOT_ELIMINATED      = "r0858"
  !    "EventName": "CPL_CYCLES.RING0",
  !   "BriefDescription": "Unhalted core cycles when the thread is in ring 0
  character(*),     parameter, public :: CPL_CYCLES_RING0                          = "r015C"
  !  "EventName": "CPL_CYCLES.RING0_TRANS",
  !  "BriefDescription": "Number of intervals between processor halts while thread is in ring 0
  character(*),     parameter, public :: CPL_CYCLES_RING0_TRANS                    = "r015C"
  !    "EventName": "CPL_CYCLES.RING123",
  !   "BriefDescription": "Unhalted core cycles when thread is in rings 1, 2, or 3
  character(*),     parameter, public :: CPL_CYCLES_RING123                        = "r025C"
  !  "EventName": "TX_EXEC.MISC1",
  !  "BriefDescription": "Counts the number of times a class of instructions that may cause a transactional abort was executed." 
  !  Since this is the count of execution, it may not always cause a transactional abort.
  character(*),     parameter, public :: TX_EXEC_MISC1                             = "r015D"
  !  "EventName": "TX_EXEC.MISC2",
  !  "BriefDescription": "Counts the number of times a class of instructions (e.g., vzeroupper) that may cause a transactional abort was executed inside a transactional region.
  character(*),     parameter, public :: TX_EXEC_MISC2                             = "r025D"
  !   "EventName": "TX_EXEC.MISC3",
  !   "BriefDescription": "Counts the number of times an instruction execution caused the transactional nest count supported to be exceeded.
  character(*),     parameter, public :: TX_EXEC_MISC3                             = "r045D"
  !    "EventName": "TX_EXEC.MISC4",
  !  "BriefDescription": "Counts the number of times a XBEGIN instruction was executed inside an HLE transactional region."
  character(*),     parameter, public :: TX_EXEC_MISC4                             = "r085D"
  !   "EventName": "TX_EXEC.MISC5",
  !   "BriefDescription": "Counts the number of times an HLE XACQUIRE instruction was executed inside an RTM transactional region.
  character(*),     parameter, public :: TX_EXEC_MISC5                             = "r105D"
  
end module mod_core_events





module mod_uncore_events



end module mod_uncore_events
