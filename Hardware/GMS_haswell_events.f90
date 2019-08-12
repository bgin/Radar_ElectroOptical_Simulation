

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
  !    "EventName": "RS_EVENTS.EMPTY_CYCLES",
  !  "BriefDescription": "Cycles when Reservation Station (RS) is empty for the thread",
  !  "PublicDescription": "This event counts cycles when the Reservation Station ( RS ) is empty for the thread. The RS is a structure that buffers allocated micro-ops from the Front-end.
  !  If there are many cycles when the RS is empty, it may represent an underflow of instructions delivered from the Front-end.
  character(*),     parameter, public :: RS_EVENTS_EMPTY_CYCLES                    = "r015E"
  !  "EventName": "RS_EVENTS.EMPTY_END",
  !   "BriefDescription": "Counts end of periods where the Reservation Station (RS) was empty. Could be useful to precisely locate Frontend Latency Bound issues.
  character(*),     parameter, public :: RS_EVENTS_EMPTY_END                       = "r015E"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.DEMAND_DATA_RD",
  !  "BriefDescription": "Offcore outstanding Demand Data Read transactions in uncore queue.",
  !  "PublicDescription": "Offcore outstanding demand data read transactions in SQ to uncore. Set Cmask=1 to count cycles.
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING_DEMAND_DATA_RD = "r0160"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.CYCLES_WITH_DEMAND_DATA_RD",
  !  "BriefDescription": "Cycles when offcore outstanding Demand Data Read transactions are present in SuperQueue (SQ), queue to uncore."
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.CYCLES_WITH_DEMAND_DATA_RD = "r0160"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.DEMAND_DATA_RD_GE_6",
  !   "BriefDescription": "Cycles with at least 6 offcore outstanding Demand Data Read transactions in uncore queue.
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.DEMAND_DATA_RD_GE_6        = "r0160"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.DEMAND_CODE_RD",
  !  "BriefDescription": "Offcore outstanding code reads transactions in SuperQueue (SQ), queue to uncore, every cycle",
  !  "PublicDescription": "Offcore outstanding Demand code Read transactions in SQ to uncore. Set Cmask=1 to count cycles.
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.DEMAND_CODE_R              = "r0260"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.DEMAND_RFO",
  !  "BriefDescription": "Offcore outstanding RFO store transactions in SuperQueue (SQ), queue to uncore",
  !  "PublicDescription": "Offcore outstanding RFO store transactions in SQ to uncore. Set Cmask=1 to count cycles."
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.DEMAND_RFO                = "r0460"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.ALL_DATA_RD",
  !  "BriefDescription": "Offcore outstanding cacheable Core Data Read transactions in SuperQueue (SQ), queue to uncore",
  !  "PublicDescription": "Offcore outstanding cacheable data read transactions in SQ to uncore. Set Cmask=1 to count cycles."
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.ALL_DATA_RD               = "r0860"
  !   "EventName": "OFFCORE_REQUESTS_OUTSTANDING.CYCLES_WITH_DATA_RD",
  !  "BriefDescription": "Cycles when offcore outstanding cacheable Core Data Read transactions are present in SuperQueue (SQ), queue to uncore."
  character(*),     parameter, public :: OFFCORE_REQUESTS_OUTSTANDING.CYCLES_WITH_DATA_RD       = "r0860"
  !   "EventName": "LOCK_CYCLES.SPLIT_LOCK_UC_LOCK_DURATION",
  !  "BriefDescription": "Cycles when L1 and L2 are locked due to UC or split lock",
  !  "PublicDescription": "Cycles in which the L1D and L2 are locked, due to a UC lock or split lock.
  character(*),     parameter, public :: LOCK_CYCLES-SPLIT_LOCK_UC_LOCK_DURATION                = "r0163"
  !   "EventName": "LOCK_CYCLES.CACHE_LOCK_DURATION",
  !  "BriefDescription": "Cycles when L1D is locked",
  !  "PublicDescription": "Cycles in which the L1D is locked.
  character(*),     parameter, public :: LOCK_CYCLES_CACHE_LOCK_DURATION                        = "r0263"
  !   "EventName": "IDQ.EMPTY",
  !   "BriefDescription": "Instruction Decode Queue (IDQ) empty cycles"
  character(*),     parameter, public :: IDQ_EMPTY                                              = "r0279"
  !    "EventName": "IDQ.MITE_UOPS",
  !  "BriefDescription": "Uops delivered to Instruction Decode Queue (IDQ) from MITE path",
  !  "PublicDescription": "Increment each cycle # of uops delivered to IDQ from MITE path. Set Cmask = 1 to count cycles.
  character(*),     parameter, public :: IDQ_MITE_UOPS                                          = "r0479"
  !   "EventName": "IDQ.MITE_CYCLES",
  !  "BriefDescription": "Cycles when uops are being delivered to Instruction Decode Queue (IDQ) from MITE path.
  character(*),     parameter, public :: IDQ_MITE_CYCLES                                        = "r0479"
  !   "EventName": "IDQ.DSB_UOPS",
  !  "BriefDescription": "Uops delivered to Instruction Decode Queue (IDQ) from the Decode Stream Buffer (DSB) path",
  !  "PublicDescription": "Increment each cycle. # of uops delivered to IDQ from DSB path. Set Cmask = 1 to count cycles.
  character(*),     parameter, public :: IDQ_DSB_UOPS                                           = "r0879"
  !   "EventName": "IDQ.MS_DSB_UOPS",
  !  "BriefDescription": "Uops initiated by Decode Stream Buffer (DSB) that are being delivered to Instruction Decode Queue (IDQ) while Microcode Sequenser (MS) is busy",
  !  "PublicDescription": "Increment each cycle # of uops delivered to IDQ when MS_busy by DSB. Set Cmask = 1 to count cycles. Add Edge=1 to count # of delivery
  character(*),     parameter, public :: IDQ_MS_DSB_UOPS                                        = "r1079"
  !   "EventName": "IDQ.MS_DSB_CYCLES",
  !  "BriefDescription": "Cycles when uops initiated by Decode Stream Buffer (DSB) are being delivered to Instruction Decode Queue (IDQ) while Microcode Sequenser (MS) is busy
  character(*),     parameter, public :: IDQ_MS_DSB_CYCLES                                      = "r1079"
  !    "EventName": "IDQ.MS_DSB_OCCUR",
  !   "BriefDescription": "Deliveries to Instruction Decode Queue (IDQ) initiated by Decode Stream Buffer (DSB) while Microcode Sequenser (MS) is busy
  character(*),     parameter, public :: IDQ_MS_DBS_OCCUR                                       = "r1079"
  !    "EventName": "IDQ.ALL_DSB_CYCLES_4_UOPS",
  !  "BriefDescription": "Cycles Decode Stream Buffer (DSB) is delivering 4 Uops",
  !  "PublicDescription": "Counts cycles DSB is delivered four uops. Set Cmask = 4.
  character(*),     parameter, public :: IDQ_ALL_DBS_CYCLES_4_UOPS                              = "r1879"
  !   "EventName": "IDQ.ALL_DSB_CYCLES_ANY_UOPS",
  !  "BriefDescription": "Cycles Decode Stream Buffer (DSB) is delivering any Uop",
  !  "PublicDescription": "Counts cycles DSB is delivered at least one uops. Set Cmask = 1.",
  character(*),     parameter, public :: IDQ_ALL_DBS_CYCLES_ANY_UOPS                            = "r1879"
  !   "EventName": "IDQ.MS_MITE_UOPS",
  !  "BriefDescription": "Uops initiated by MITE and delivered to Instruction Decode Queue (IDQ) while Microcode Sequenser (MS) is busy",
  !  "PublicDescription": "Increment each cycle # of uops delivered to IDQ when MS_busy by MITE. Set Cmask = 1 to count cycles."
  character(*),     parameter, public :: IDQ_MS_MITE_UOPS                                       = "r2079"
  !    "EventName": "IDQ.ALL_MITE_CYCLES_4_UOPS",
  !  "BriefDescription": "Cycles MITE is delivering 4 Uops",
  !  "PublicDescription": "Counts cycles MITE is delivered four uops. Set Cmask = 4.
  character(*),     parameter, public :: IDQ_ALL_MITE_CYCLES_4_UOPS                             = "r2479"
  !   "EventName": "IDQ.ALL_MITE_CYCLES_ANY_UOPS",
  !  "BriefDescription": "Cycles MITE is delivering any Uop",
  !  "PublicDescription": "Counts cycles MITE is delivered at least one uop. Set Cmask = 1
  character(*),     parameter, public :: IDQ_ALL_MITE_CYCLES_ANY_UOPS                           = "r2479"
  !  "EventName": "IDQ.MS_UOPS",
  !  "BriefDescription": "Uops delivered to Instruction Decode Queue (IDQ) while Microcode Sequenser (MS) is busy",
  !   "PublicDescription": "This event counts uops delivered by the Front-end with the assistance of the microcode sequencer.
  !  Microcode assists are used for complex instructions or scenarios that can't    be handled by the standard decoder.  Using other instructions, if possible, will usually improve performance."
  character(*),     parameter, public :: IDQ_MS_UOPS                                            = "r3079"
  !  "EventName": "IDQ.MS_CYCLES",
  !  "BriefDescription": "Cycles when uops are being delivered to Instruction Decode Queue (IDQ) while Microcode Sequenser (MS) is busy",
  !  "PublicDescription": "This event counts cycles during which the microcode sequencer assisted the Front-end in delivering uops.
  !  Microcode assists are used for complex instructions or scenarios that !  can't be handled by the standard decoder.  Using other instructions, if possible, will usually improve performance.",
  !   "Counter": "0,1,2,3",
  character(*),     parameter, public :: IDQ_MS_CYCLES                                          = "r3079"
  !  "EventName": "IDQ.MS_SWITCHES",
  !  "BriefDescription": "Number of switches from DSB (Decode Stream Buffer) or MITE (legacy decode pipeline) to the Microcode Sequencer.",
  !  "PublicDescription": "Number of switches from DSB (Decode Stream Buffer) or MITE (legacy decode pipeline) to the Microcode Sequencer."
  character(*),     parameter, public :: IDQ_MS_SWITCHES                                        = "r3079"
  !  "EventName": "IDQ.MITE_ALL_UOPS",
  !   "BriefDescription": "Uops delivered to Instruction Decode Queue (IDQ) from MITE path",
  !  "PublicDescription": "Number of uops delivered to IDQ from any path.",
  character(*),     parameter, public :: IDQ_MITE_ALL_UOPS                                      = "r3c79"
  !  "EventName": "ICACHE.HIT",
  !  "BriefDescription": "Number of Instruction Cache, Streaming Buffer and Victim Cache Reads. both cacheable and noncacheable, including UC fetches.
  character(*),     parameter, public :: ICACHE_HIT                                             = "r0180"
  !   "EventName": "ICACHE.MISSES",
  !  "BriefDescription": "Number of Instruction Cache, Streaming Buffer and Victim Cache Misses. Includes Uncacheable accesses.",
  !  "PublicDescription": "This event counts Instruction Cache (ICACHE) misses.
  character(*),     parameter, public :: ICACHE_MISSES                                          = "r0280"
  !  "EventName": "ICACHE.IFETCH_STALL",
  !   "BriefDescription": "Cycles where a code fetch is stalled due to L1 instruction-cache miss.",
  !   "PublicDescription": "Cycles where a code fetch is stalled due to L1 instruction-cache miss."
  character(*),     parameter, public :: ICACHE_IFETCH_STALL                                    = "r0480"
  !   "EventName": "ICACHE.IFDATA_STALL",
  !  "BriefDescription": "Cycles where a code fetch is stalled due to L1 instruction-cache miss.
  character(*),     parameter, public :: ICACHE_IFDATA_STALL                                    = "r0480"
  !  "EventName": "ITLB_MISSES.MISS_CAUSES_A_WALK",
  !  "BriefDescription": "Misses at all ITLB levels that cause page walks",
  !  "PublicDescription": "Misses in ITLB that causes a page walk of any page size.
  character(*),     parameter, public :: ITLB_MISSES_MISS_CAUSES_A_WALK                         = "r0185"
  !   "EventName": "ITLB_MISSES.WALK_COMPLETED_4K",
  !  "BriefDescription": "Code miss in all TLB levels causes a page walk that completes. (4K)"
  character(*),     parameter, public :: ITLB_MISSES_WALK_COMPLETED_4K                          = "r0285"
  !   "EventName": "ITLB_MISSES.WALK_COMPLETED_2M_4M",
  !   "BriefDescription": "Code miss in all TLB levels causes a page walk that completes. (2M/4M)",
  character(*),     parameter, public :: ITLB_MISSES_WALK_COMPLETED_2M_4M                       = "r0485"
  !   "EventName": "ITLB_MISSES.WALK_COMPLETED_1G",
  !   "BriefDescription": "Store miss in all TLB levels causes a page walk that completes. (1G)
  character(*),     parameter, public :: ITLB_MISSES_WALK_COMPLETED_1G                          = "r0885"
  !   "EventName": "ITLB_MISSES.WALK_COMPLETED",
  !   "BriefDescription": "Misses in all ITLB levels that cause completed page walks
  character(*),     parameter, public :: ITLB_MISSES_WALK_COMPLETED                             = "r0e85"
  !    "EventName": "ITLB_MISSES.WALK_DURATION",
  !   "BriefDescription": "Cycles when PMH is busy with page walks
  character(*),     parameter, public :: ITLB_MISSES_WALK_DURATION                              = "r1085"
  !    "EventName": "ITLB_MISSES.STLB_HIT_4K",
  !    "BriefDescription": "Core misses that miss the  DTLB and hit the STLB (4K)"
  character(*),     parameter, public :: ITLB_MISSES_STLB_HIT_4K                                = "r2085"
  !     "EventName": "ITLB_MISSES.STLB_HIT_2M",
  !   "BriefDescription": "Code misses that miss the  DTLB and hit the STLB (2M)"
  character(*),     parameter, public :: ITLB_MISSES_STLB_HIT_2M                                = "r4085"
  !   "EventName": "ITLB_MISSES.STLB_HIT",
  !  "BriefDescription": "Operations that miss the first ITLB level but hit the second and do not cause any page walks"
  character(*),     parameter, public :: ITLB_MISSES_STLB_HIT                                   = "r6085"
  !   "EventName": "ILD_STALL.LCP",
  !  "BriefDescription": "Stalls caused by changing prefix length of the instruction.",
  !  "PublicDescription": "This event counts cycles where the decoder is stalled on an instruction with a length changing prefix (LCP).
  character(*),     parameter, public :: ILD_STALL_LCP                                          = "r0187"
  !   "EventName": "ILD_STALL.IQ_FULL",
  !  "BriefDescription": "Stall cycles because IQ is full",
  character(*),     parameter, public :: ILD_STALL_IQ_FULL                                      = "r0487"
  ! "EventName": "BR_INST_EXEC.NONTAKEN_CONDITIONAL",
  !  "BriefDescription": "Not taken macro-conditional branches."
  character(*),     parameter, public :: BR_INST_EXEC.NONTAKEN_CONDITIONAL                      = "r4188"
  !   "EventName": "BR_INST_EXEC.TAKEN_CONDITIONAL",
  !   "BriefDescription": "Taken speculative and retired macro-conditional branches.
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_CONDITIONAL                         = "r8188"
  !  "EventName": "BR_INST_EXEC.TAKEN_DIRECT_JUMP",
  !  "BriefDescription": "Taken speculative and retired macro-conditional branch instructions excluding calls and indirects.
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_DIRECT_JUMP                         = "r8288"
  !   "EventName": "BR_INST_EXEC.TAKEN_INDIRECT_JUMP_NON_CALL_RET",
  !  "BriefDescription": "Taken speculative and retired indirect branches excluding calls and returns."
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_INDIRECT_JUMP_NON_CALL_RET          = "r8488"
  ! "EventName": "BR_INST_EXEC.TAKEN_INDIRECT_NEAR_RETURN",
  !  "BriefDescription": "Taken speculative and retired indirect branches with return mnemonic."
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_INDIRECT_NEAR_RETURN                = "r8888"
  !  "EventName": "BR_INST_EXEC.TAKEN_DIRECT_NEAR_CALL",
  !  "BriefDescription": "Taken speculative and retired direct near calls."
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_DIRECT_NEAR_CALL                    = "r9088"
  !  "EventName": "BR_INST_EXEC.TAKEN_INDIRECT_NEAR_CALL",
  !   "BriefDescription": "Taken speculative and retired indirect calls.
  character(*),     parameter, public :: BR_INST_EXEC.TAKEN_INDIRECT_NEAR_CALL                  = "rA088"
  !  "EventName": "BR_INST_EXEC.ALL_CONDITIONAL",
  !  "BriefDescription": "Speculative and retired macro-conditional branches.",
  character(*),     parameter, public :: BR_INST_EXEC_ALL_CONDITIONAL                           = "rC188"
  !   "EventName": "BR_INST_EXEC.ALL_DIRECT_JMP",
  !  "BriefDescription": "Speculative and retired macro-unconditional branches excluding calls and indirects."
  character(*),     parameter, public :: BR_INST_EXEC.ALL_DIRECT_JMP                            = "rC288"
  !  "EventName": "BR_INST_EXEC.ALL_INDIRECT_JUMP_NON_CALL_RET",
  !  "BriefDescription": "Speculative and retired indirect branches excluding calls and returns.
  character(*),     parameter, public :: BR_INST_EXEC.ALL_INDIRECT_JUMP_NON_CALL_RET            = "rC488"
  !    "EventName": "BR_INST_EXEC.ALL_INDIRECT_NEAR_RETURN",
  !  "BriefDescription": "Speculative and retired indirect return branches."
  character(*),     parameter, public :: BR_INST_EXEC.ALL_INDIRECT_NEAR_RETURN                  = "rC888"
  !  "EventName": "BR_INST_EXEC.ALL_DIRECT_NEAR_CALL",
  !  "BriefDescription": "Speculative and retired direct near calls."
  character(*),     parameter, public :: BR_INST_EXEC.ALL_DIRECT_NEAR_CALL                      = "rD088"
  !  "EventName": "BR_INST_EXEC.ALL_BRANCHES",
  !   "BriefDescription": "Speculative and retired  branches",
  character(*),     parameter, public :: BR_INST_EXEC.ALL_BRANCHES                              = "rFF88"
  !  "EventName": "BR_MISP_EXEC.NONTAKEN_CONDITIONAL",
  !  "BriefDescription": "Not taken speculative and retired mispredicted macro conditional branches.
  character(*),     parameter, public :: BR_MISP_EXEC.NONTAKEN_CONDITIONAL                      = "r4189"
  !   "EventName": "BR_MISP_EXEC.TAKEN_CONDITIONAL",
  !   "BriefDescription": "Taken speculative and retired mispredicted macro conditional branches."
  character(*),     parameter, public :: BR_MISP_EXEC.TAKEN_CONDITIONAL                         = "r4189"
  !    "EventName": "BR_MISP_EXEC.TAKEN_INDIRECT_JUMP_NON_CALL_RET",
  !   "BriefDescription": "Taken speculative and retired mispredicted indirect branches excluding calls and returns.
  character(*),     parameter, public :: BR_MISP_EXEC.TAKEN_INDIRECT_JUMP_NON_CALL_RET          = "r8489"
  !   "EventName": "BR_MISP_EXEC.TAKEN_RETURN_NEAR",
  !   "BriefDescription": "Taken speculative and retired mispredicted indirect branches with return mnemonic.
  character(*),     parameter, public :: BR_MISP_EXEC_TAKEN_RETURN_NEAR                         = "r8889"
  ! "EventName": "BR_MISP_EXEC.TAKEN_INDIRECT_NEAR_CALL",
  !  "BriefDescription": "Taken speculative and retired mispredicted indirect calls.
  character(*),     parameter, public :: BR_MISP_EXEC.TAKEN_INDIRECT_NEAR_CALL                  = "rA089"
  !  "EventName": "BR_MISP_EXEC.ALL_CONDITIONAL",
  !  "BriefDescription": "Speculative and retired mispredicted macro conditional branches.
  character(*),     parameter, public :: BR_MISP_EXEC_ALL_CONDITIONAL                           = "rC189"
  !  "EventName": "BR_MISP_EXEC.ALL_INDIRECT_JUMP_NON_CALL_RET",
  !  "BriefDescription": "Mispredicted indirect branches excluding calls and returns.
  character(*),     parameter, public :: BR_MISP_EXEC.ALL_INDIRECT_JUMP_NON_CALL_RET            = "rC489"
  !   "EventName": "BR_MISP_EXEC.ALL_BRANCHES",
  !   "BriefDescription": "Speculative and retired mispredicted macro conditional branches"
  character(*),     parameter, public :: BR_MISP_EXEC_ALL_BRANCHES                              = "rFF89"
  !   "EventName": "IDQ_UOPS_NOT_DELIVERED.CORE",
  !  "BriefDescription": "Uops not delivered to Resource Allocation Table (RAT) per thread when backend of the machine is not stalled",
  !  "PublicDescription": "This event count the number of undelivered (unallocated) uops from the Front-end to the Resource Allocation Table (RAT) while the Back-end of the processor is not stalled.
  !  The  !Front-end can allocate up to 4 uops per cycle so this event can increment 0-4 times per cycle depending on the number of unallocated uops. This event is counted on a per-core basis."
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED_CORE                            = "r019C"
  !   "EventName": "IDQ_UOPS_NOT_DELIVERED.CYCLES_0_UOPS_DELIV.CORE",
  !  "BriefDescription": "Cycles per thread when 4 or more uops are not delivered to Resource Allocation Table (RAT) when backend of the machine is not stalled",
  !  "PublicDescription": "This event counts the number cycles during which the Front-end allocated exactly zero uops
  !  to the Resource Allocation Table (RAT) while the Back-end of the processor is not stalled.  This event is counted on a per-core basis.",
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED_CYCLES_0_UOPS_DELIV_CORE        = "r019C"
  !     "EventName": "IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_1_UOP_DELIV.CORE",
  !     "BriefDescription": "Cycles per thread when 3 or more uops are not delivered to
  !     Resource Allocation Table (RAT) when backend of the machine is not stalled.
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_1_UOP_DELIV.CORE      = "r019C"
  !   "EventName": "IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_2_UOP_DELIV.CORE",
  !  "BriefDescription": "Cycles with less than 2 uops delivered by the front end."
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_2_UOP_DELIV.CORE      = "r019C"
  !   "EventName": "IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_3_UOP_DELIV.CORE",
  !   "BriefDescription": "Cycles with less than 3 uops delivered by the front end.
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED.CYCLES_LE_3_UOP_DELIV.CORE      = "r019C"
  !   "EventName": "IDQ_UOPS_NOT_DELIVERED.CYCLES_FE_WAS_OK",
  !   "BriefDescription": "Counts cycles FE delivered 4 uops or Resource Allocation Table (RAT) was stalling FE.
  character(*),     parameter, public :: IDQ_UOPS_NOT_DELIVERED.CYCLES_FE_WAS_OK                = "r019C"
  !    "EventName": "UOPS_EXECUTED_PORT.PORT_0",
  !    "BriefDescription": "Cycles per thread when uops are executed in port 0
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_0                              = "r01A1"
  !    "EventName": "UOPS_EXECUTED_PORT.PORT_0_CORE",
  !    "BriefDescription": "Cycles per core when uops are executed in port 0.
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_0_CORE                         = "r01A1"
  !     "EventName": "UOPS_DISPATCHED_PORT.PORT_0",
  !     "BriefDescription": "Cycles per thread when uops are executed in port 0.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_0                            = "r01A1"
  !      "EventName": "UOPS_EXECUTED_PORT.PORT_1",
  !     "BriefDescription": "Cycles per thread when uops are executed in port 1"
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_1                              = "r02A1"
  !     "EventName": "UOPS_EXECUTED_PORT.PORT_1_CORE",
  !     "BriefDescription": "Cycles per core when uops are executed in port 1
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_1_CORE                         = "r02A1"
  !      "EventName": "UOPS_DISPATCHED_PORT.PORT_1",
  !      "BriefDescription": "Cycles per thread when uops are executed in port 1
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_1                            = "r02A1"
  !      "EventName": "UOPS_EXECUTED_PORT.PORT_2",
  !      "BriefDescription": "Cycles per thread when uops are executed in port 2
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_2                              = "r04A1"
  !       "EventName": "UOPS_EXECUTED_PORT.PORT_2_CORE",
  !       "BriefDescription": "Cycles per core when uops are dispatched to port 2.
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_2_CORE                         = "r04A1"
  !        "EventName": "UOPS_DISPATCHED_PORT.PORT_2",
  !        "BriefDescription": "Cycles per thread when uops are executed in port 2
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_2                            = "r04A1"
  !      "EventName": "UOPS_EXECUTED_PORT.PORT_3",
  !      "BriefDescription": "Cycles per thread when uops are executed in port 3"
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_3                              = "r08A1"
  !       "EventName": "UOPS_EXECUTED_PORT.PORT_3_CORE",
  !       "BriefDescription": "Cycles per core when uops are dispatched to port 3
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_3_CORE                         = "r08A1"
  !      "EventName": "UOPS_DISPATCHED_PORT.PORT_3",
  !      "BriefDescription": "Cycles per thread when uops are executed in port 3.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_3                            = "r08A1"
  !       "EventName": "UOPS_EXECUTED_PORT.PORT_4",
  !       "BriefDescription": "Cycles per thread when uops are executed in port 4"
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_4                              = "r10A1"
  !        "EventName": "UOPS_EXECUTED_PORT.PORT_4_CORE",
  !         "BriefDescription": "Cycles per core when uops are executed in port 4.
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_4_CORE                         = "r10A1"
  !         "EventName": "UOPS_DISPATCHED_PORT.PORT_4",
  !   "BriefDescription": "Cycles per thread when uops are executed in port 4.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_4                            = "r10A1"
  !    "EventName": "UOPS_EXECUTED_PORT.PORT_5",
  !    "BriefDescription": "Cycles per thread when uops are executed in port 5"
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_5                              = "r20A1"
  !      "EventName": "UOPS_EXECUTED_PORT.PORT_5_CORE",
  !      "BriefDescription": "Cycles per core when uops are executed in port 5."
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_5_CORE                         = "r20A1"
  !       "EventName": "UOPS_DISPATCHED_PORT.PORT_5",
  !       "BriefDescription": "Cycles per thread when uops are executed in port 5.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_5                            = "r20A1"
  !       "EventName": "UOPS_EXECUTED_PORT.PORT_6",
  !       "BriefDescription": "Cycles per thread when uops are executed in port 6
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_6                              = "r40A1"
  !    "EventName": "UOPS_EXECUTED_PORT.PORT_6_CORE",
  !    "BriefDescription": "Cycles per core when uops are executed in port 6.
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_6_CORE                         = "r40A1"
  !   "EventName": "UOPS_DISPATCHED_PORT.PORT_6",
  !    "BriefDescription": "Cycles per thread when uops are executed in port 6.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_6                            = "r40A1"
  !     "EventName": "UOPS_EXECUTED_PORT.PORT_7",
  !  "BriefDescription": "Cycles per thread when uops are executed in port 7"
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_7                              = "r80A1"
  !    "EventName": "UOPS_EXECUTED_PORT.PORT_7_CORE",
  !    "BriefDescription": "Cycles per core when uops are dispatched to port 7."
  character(*),     parameter, public :: UOPS_EXECUTED_PORT_PORT_7_CORE                         = "r80A1"
  !     "EventName": "UOPS_DISPATCHED_PORT.PORT_7",
  !    "BriefDescription": "Cycles per thread when uops are executed in port 7.
  character(*),     parameter, public :: UOPS_DISPATCHED_PORT_PORT_7                            = "r80A1"
  !   "EventName": "RESOURCE_STALLS.ANY",
  !   "BriefDescription": "Resource-related stall cycles"
  character(*),     parameter, public :: RESOURCES_STALLS_ANY                                   = "r01A2"
  !     "EventName": "RESOURCE_STALLS.RS",
  !  "BriefDescription": "Cycles stalled due to no eligible RS entry available."
  character(*),     parameter, public :: RESOURCE_STALLS_RS                                     = "r04A2"
  !   "EventName": "RESOURCE_STALLS.SB",
  !   "BriefDescription": "Cycles stalled due to no store buffers available. (not including draining form sync).",
  !   "PublicDescription": "This event counts cycles during which no instructions were allocated because no Store Buffers (SB) were available.
  character(*),     parameter, public :: RESOURCE_STALLS_SB                                     = "r08A2"
  !    "EventName": "RESOURCE_STALLS.ROB",
  !  "BriefDescription": "Cycles stalled due to re-order buffer full.",
  character(*),     parameter, public :: RESOURCE_STALLS_ROB                                    = "r10A2"
  !    "EventName": "CYCLE_ACTIVITY.CYCLES_L2_PENDING",
  !  "BriefDescription": "Cycles with pending L2 cache miss loads.",
  !  "PublicDescription": "Cycles with pending L2 miss loads. Set Cmask=2 to count cycle."
  character(*),     parameter, public :: CYCLE_ACTIVITIES_CYCLES_L2_PENDING                     = "r01A3"
  !   "EventName": "CYCLE_ACTIVITY.CYCLES_LDM_PENDING",
  !  "BriefDescription": "Cycles with pending memory loads.",
  !  "PublicDescription": "Cycles with pending memory loads. Set Cmask=2 to count cycle.
  character(*),     parameter, public :: CYCLES_ACTIVITIES_CYCLES_LDM_PENDING                   = "r02A3"
  !    "EventName": "CYCLE_ACTIVITY.CYCLES_NO_EXECUTE",
  !  "BriefDescription": "This event increments by 1 for every cycle where there was no execute for this thread.",
  !   "PublicDescription": "This event counts cycles during which no instructions were executed in the execution stage of the pipeline.
  character(*),     parameter, public :: CYCLE_ACTIVITY_CYCLES_NO_EXECUTE                       = "r04A3"
  !    "EventName": "CYCLE_ACTIVITY.STALLS_L2_PENDING",
  !    "BriefDescription": "Execution stalls due to L2 cache misses."
  character(*),     parameter, public :: CYCLE_ACTIVITY_STALLS_L2_PENDING                       = "r05A3"
  !     "EventName": "CYCLE_ACTIVITY.STALLS_LDM_PENDING",
  !  "BriefDescription": "Execution stalls due to memory subsystem.",
  !  "PublicDescription": "This event counts cycles during which no instructions were executed in the execution stage of
  !   the pipeline and there were memory instructions pending (waiting for data).
  character(*),     parameter, public :: CYCLE_ACTIVITY_STALLS_LDM_PENDING                      = "r06A3"
  !    "EventName": "CYCLE_ACTIVITY.CYCLES_L1D_PENDING",
  !   "BriefDescription": "Cycles with pending L1 cache miss loads.",
  !  "PublicDescription": "Cycles with pending L1 data cache miss loads. Set Cmask=8 to count cycle."
  character(*),     parameter, public :: CYCLE_ACTIVITY_CYCLES_L1D_PENDING                      = "r08A3"
  !    "EventName": "CYCLE_ACTIVITY.STALLS_L1D_PENDING",
  !  "BriefDescription": "Execution stalls due to L1 data cache misses",
  !  "PublicDescription": "Execution stalls due to L1 data cache miss loads. Set Cmask=0CH.
  character(*),     parameter, public :: CYCLE_ACTIVITY_STALLS_L1D_PENDING                      = "r0CA3"
  !    "EventName": "LSD.UOPS",
  !   "BriefDescription": "Number of Uops delivered by the LSD.",
  character(*),     parameter, public :: LSD_UOPS                                               = "r01A8"
  !     "EventName": "LSD.CYCLES_ACTIVE",
  !   "BriefDescription": "Cycles Uops delivered by the LSD, but didn't come from the decoder.",
  character(*),     parameter, public :: LSD_CYCLES_ACTIVE                                      = "r01A8"
  !    "EventName": "LSD.CYCLES_4_UOPS",
  !   "BriefDescription": "Cycles 4 Uops delivered by the LSD, but didn't come from the decoder.
  character(*),     parameter, public :: LSD_CYCLES_4_UOPS                                      = "r01A8"
  !    "EventName": "DSB2MITE_SWITCHES.PENALTY_CYCLES",
  !    "BriefDescription": "Decode Stream Buffer (DSB)-to-MITE switch true penalty cycles."
  character(*),     parameter, public :: DSB2MITE_SWITCHES.PENALTY_CYCLES                       = "r02AB"
  !     "EventName": "ITLB.ITLB_FLUSH",
  !     "BriefDescription": "Flushing of the Instruction TLB (ITLB) pages, includes 4k/2M/4M pages.
  character(*),     parameter, public :: ITLB_ITLB_FLUSH                                        = "r01AE"
  !      "EventName": "OFFCORE_REQUESTS.DEMAND_DATA_RD",
  !      "BriefDescription": "Demand Data Read requests sent to uncore",
  character(*),     parameter, public :: OFFCORE_REQUESTS_DEMAND_DATA_RD                        = "r01B0"
  !       "EventName": "OFFCORE_REQUESTS.DEMAND_CODE_RD",
  !       "BriefDescription": "Cacheable and noncachaeble code read requests",
  character(*),     parameter, public :: OFFCORE_REQUESTS_DEMAND_CODE_RD                        = "r02B0"
  !        "EventName": "OFFCORE_REQUESTS.DEMAND_RFO",
  !  "BriefDescription": "Demand RFO requests including regular RFOs, locks, ItoM",
  !  "PublicDescription": "Demand RFO read requests sent to uncore, including regular RFOs, locks, ItoM.",
  character(*),     parameter, public :: OFFCORE_REQUESTS_DEMAND_RFO                            = "r04B0"
  !    "EventName": "OFFCORE_REQUESTS.ALL_DATA_RD",
  !  "BriefDescription": "Demand and prefetch data reads",
  !  "PublicDescription": "Data read requests sent to uncore (demand and prefetch).
  character(*),     parameter, public :: OFFCORE_REQUESTS_ALL_DATA_RD                           = "r08B0"
  !   "EventName": "UOPS_EXECUTED.STALL_CYCLES",
  !  "BriefDescription": "Counts number of cycles no uops were dispatched to be executed on this thread."
  character(*),     parameter, public :: UOPS_EXECUTED_STALL_CYCLES                             = "r01B1"
  !   "EventName": "UOPS_EXECUTED.CYCLES_GE_1_UOP_EXEC",
  !  "BriefDescription": "Cycles where at least 1 uop was executed per-thread",
  !  "PublicDescription": "This events counts the cycles where at least one uop was executed. It is counted per thread.",
  character(*),     parameter, public :: UOPS_EXECUTED_CYCLES_GE_1_UOP_EXEC                     = "r01B1"
  !  "EventName": "UOPS_EXECUTED.CYCLES_GE_2_UOPS_EXEC",
  !  "BriefDescription": "Cycles where at least 2 uops were executed per-thread",
  !  "PublicDescription": "This events counts the cycles where at least two uop were executed. It is counted per thread.",
  character(*),     parameter, public :: UOPS_EXECUTED_CYCLES_GE_2_UOP_EXEC                     = "r01B1"
  !   "EventName": "UOPS_EXECUTED.CYCLES_GE_3_UOPS_EXEC",
  !  "BriefDescription": "Cycles where at least 3 uops were executed per-thread",
  !  "PublicDescription": "This events counts the cycles where at least three uop were executed. It is counted per thread.",
  character(*),    parameter, public :: UOPS_EXECUTED.CYCLES_GE_3_UOPS_EXEC                     = "r01B1"
  !   "EventName": "UOPS_EXECUTED.CYCLES_GE_4_UOPS_EXEC",
  !  "BriefDescription": "Cycles where at least 4 uops were executed per-thread.",
  !  "PublicDescription": "Cycles where at least 4 uops were executed per-thread.",
  character(*),    parameter, public :: UOPS_EXECUTED.CYCLES_GE_4_UOPS_EXEC                     = "r01B1"
  !   "EventName": "UOPS_EXECUTED.CORE",
  !  "BriefDescription": "Number of uops executed on the core.",
  !  "PublicDescription": "Counts total number of uops to be executed per-core each cycle.",
  character(*),    parameter, public :: UOPS_EXECUTED_CORE                                      = "r02B1"
  !   "EventName": "UOPS_EXECUTED.CORE_CYCLES_GE_1",
  !   "BriefDescription": "Cycles at least 1 micro-op is executed from any thread on physical core.",
  character(*),    parameter, public :: UOPS_EXECUTED_CORE_CYCLES_GE_1                          = "r02B1"
  !    "EventName": "UOPS_EXECUTED.CORE_CYCLES_GE_2",
  !    "BriefDescription": "Cycles at least 2 micro-op is executed from any thread on physical core.",
  character(*),    parameter, public :: UOPS_EXECUTED_CORE_CYCLES_GE_2                          = "r02B1"
  !     "EventName": "UOPS_EXECUTED.CORE_CYCLES_GE_3",
  !   "BriefDescription": "Cycles at least 3 micro-op is executed from any thread on physical core.",
  character(*),    parameter, public ::  UOPS_EXECUTED_CORE_CYCLES_GE_3                         = "r02B1"
  !    "EventName": "UOPS_EXECUTED.CORE_CYCLES_GE_4",
  !   "BriefDescription": "Cycles at least 4 micro-op is executed from any thread on physical core.",
  character(*),    parameter, public ::  UOPS_EXECUTED_CORE_CYCLES_GE_4                        = "r02B1"
  !    "EventName": "UOPS_EXECUTED.CORE_CYCLES_NONE",
  !    "BriefDescription": "Cycles with no micro-ops executed from any thread on physical core.",
  character(*),    parameter, public :: UOPS_EXECUTED_CORE_CYCLES_NONE                         = "r02B1"
  !    "EventName": "OFFCORE_REQUESTS_BUFFER.SQ_FULL",
  !   "BriefDescription": "Offcore requests buffer cannot take more entries for this thread core.",
  character(*),    parameter, public :: OFFCORE_REQUESTS_BUFFER_SQ_FULL                        = "r01b2"
  !   "EventName": "OFFCORE_RESPONSE",
  !   "BriefDescription": "Offcore response can be programmed only with a specific pair of event select and counter MSR, 
  !and with specific event codes and predefine mask bit value in a dedicated MSR to specify attributes of the offcore transaction.",
  character(*),    parameter, public :: OFFCORE_RESPONSE                                       = "r0B7"
  !  "EventName": "PAGE_WALKER_LOADS.DTLB_L1",
  ! "BriefDescription": "Number of DTLB page walker hits in the L1+FB"
  character(*),    parameter, public :: PAGE_WALKER_LOADS_DTLB_L1                              = "r11BC"
  !   "EventName": "PAGE_WALKER_LOADS.DTLB_L2",
  ! "BriefDescription": "Number of DTLB page walker hits in the L2",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_DTLB_L2                              = "r12BC"
  !    "EventName": "PAGE_WALKER_LOADS.DTLB_L3",
  !  BriefDescription": "Number of DTLB page walker hits in the L3 + XSNP",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_DTLB_L3                              = "r14BC"
  !   "EventName": "PAGE_WALKER_LOADS.DTLB_MEMORY",
  ! "BriefDescription": "Number of DTLB page walker hits in Memory",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_DTLB_MEMORY                          = "r18BC"
  !  "EventName": "PAGE_WALKER_LOADS.ITLB_L1",
  !  "BriefDescription": "Number of ITLB page walker hits in the L1+FB",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_ITLB_L1                              = "r21BC"
  !    "EventName": "PAGE_WALKER_LOADS.ITLB_L2",
  !  "BriefDescription": "Number of ITLB page walker hits in the L2",
  character(*),    parameter, public ::  PAGE_WALKER_LOADS_ITLB_L2                             = "r22BC"
  !    "EventName": "PAGE_WALKER_LOADS.ITLB_L3",
  !  "BriefDescription": "Number of ITLB page walker hits in the L3 + XSNP"
  character(*),    parameter, public :: PAGE_WALKER_LOADS_ITLB_L3                              = "r24BC"
  !  "EventName": "PAGE_WALKER_LOADS.ITLB_MEMORY",
  !  "BriefDescription": "Number of ITLB page walker hits in Memory",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_ITLB_MEMORY                          = "r28BC"
  !  "EventName": "PAGE_WALKER_LOADS.EPT_DTLB_L1",
  !  "BriefDescription": "Counts the number of Extended Page Table walks from
  !  the DTLB that hit in the L1 and FB."
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_DTLB_L1                          = "r41BC"
  !   "EventName": "PAGE_WALKER_LOADS.EPT_DTLB_L2",
  !  "BriefDescription": "Counts the number of Extended Page Table walks
  !  from the DTLB that hit in the L2.
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_DTLB_L2                          = "r42BC"
  !   "EventName": "PAGE_WALKER_LOADS.EPT_DTLB_L3",
  !   "BriefDescription": "Counts the number of Extended Page Table walks
  !   from the DTLB that hit in the L3.",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_DTLB_L3                          = "r44BC"
  !   "EventName": "PAGE_WALKER_LOADS.EPT_DTLB_MEMORY",
  !  "BriefDescription": "Counts the number of Extended Page Table walks
  !   from the DTLB that hit in memory.
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_DTLB_MEMORY                      = "r48BC"
  !    "EventName": "PAGE_WALKER_LOADS.EPT_ITLB_L1",
  !  BriefDescription": "Counts the number of Extended Page Table
  !waks from the ITLB that hit in the L1 and FB."
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_ITLB_L1                          = "r81BC"
  ! "EventName": "PAGE_WALKER_LOADS.EPT_ITLB_L2",
  !  "BriefDescription": "Counts the number of Extended Page Table walks
  !  from the ITLB that hit in the L2."
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_ITLB_L2                          = "r82BC"
  !  "EventName": "PAGE_WALKER_LOADS.EPT_ITLB_L3",
  !  "BriefDescription": "Counts the number of Extended Page Table walks
  !  from the ITLB that hit in the L2.",
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_ITLB_L3                          = "r84BC"
  !   "EventName": "PAGE_WALKER_LOADS.EPT_ITLB_MEMORY",
  !  "BriefDescription": "Counts the number of Extended Page Table walks
  !   from the ITLB that hit in memory."
  character(*),    parameter, public :: PAGE_WALKER_LOADS_EPT_ITLB_MEMORY                      = "r88BC"
  !   "EventName": "TLB_FLUSH.DTLB_THREAD",
  !  "BriefDescription": "DTLB flush attempts of the thread-specific entries",
  character(*),    parameter, public :: TLB_FLUSH_DTLB_THREAD                                  = "r01BD"
  !     "EventName": "TLB_FLUSH.STLB_ANY",
  !  "BriefDescription": "STLB flush attempts",
  character(*),    parameter, public :: TLB_FLUSH_STLB_ANY                                     = "r20BD"
  !  "EventName": "INST_RETIRED.ANY_P",
  !  "BriefDescription": "Number of instructions retired.
  ! General Counter   - architectural event",
  character(*),    parameter, public :: INST_RETIRED_ANY_P                                    = "r00C0"
  !   "EventName": "INST_RETIRED.PREC_DIST",
  !  "BriefDescription": "Precise instruction retired event with HW
  !  to reduce effect of PEBS shadow in IP distribution",
  character(*),    parameter, public :: INT_RETIRED_PREC_DIST                                 = "r01C0"
  !   "EventName": "INST_RETIRED.X87",
  !  "BriefDescription": "FP operations retired. X87 FP operations that have no exceptions: Counts also flows that have several X87 or flows that use X87 uops in the exception handling.",
  !  "PublicDescription": "This is a precise version (that is, uses PEBS) of the event that counts FP operations retired.
  !  For X87 FP operations that have no exceptions counting also includes flows that have several X87, or flows that use X87 uops in the exception handling.",
  character(*),    parameter, public :: INST_RETIRED_X87                                      = "r02C0"
  !   "EventName": "OTHER_ASSISTS.AVX_TO_SSE",
  !   "BriefDescription": "Number of transitions from AVX-256 to legacy SSE when penalty applicable"
  character(*),    parameter, public :: OTHER_ASSISTS_AVX_TO_SSE                              = "r08C1"
  !    "EventName": "OTHER_ASSISTS.SSE_TO_AVX",
  !  "BriefDescription": "Number of transitions from legacy SSE to AVX-256 when penalty applicable ",
  character(*),    parameter, public :: OTHER_ASSISTS_SSE_TO_AVX                              = "r10C1"
  !    "EventName": "OTHER_ASSISTS.ANY_WB_ASSIST"
  character(*),    parameter, public :: OTHER_ASSISTS_ANY_WB_ASSIST                           = "r40C1"
  !   "EventName": "UOPS_RETIRED.ALL",
  !  "BriefDescription": "Actually retired uops.
  character(*),    parameter, public :: UOPS_RETIRED_ALL                                      = "r01C2"
  !  "EventName": "UOPS_RETIRED.STALL_CYCLES",
  !   "BriefDescription": "Cycles no executable uops retired",
  character(*),    parameter, public :: UOPS_RETIRED_STALL_CYCLES                             = "r01C2"
  !    "EventName": "UOPS_RETIRED.TOTAL_CYCLES",
  !  "BriefDescription": "Number of cycles using always true condition applied to  PEBS uops retired event."
  character(*),    parameter, public :: UOPS_RETIRED_TOTAL_CYCLES                             = "r01C2"
  !   "EventName": "UOPS_RETIRED.CORE_STALL_CYCLES",
  !  "BriefDescription": "Cycles no executable uops retired on core",
  character(*),    parameter, public :: UOPS_RETIRED_CORE_STALL_CYLES                         = "r01C2"
  !   "EventName": "UOPS_RETIRED.RETIRE_SLOTS",
  !  "BriefDescription": "Retirement slots used.
  character(*),    parameter, public :: UOPS_RETIRED_RETIRE_SLOTS                             = "r02C2"
  !   "EventName": "MACHINE_CLEARS.CYCLES",
  !  "BriefDescription": "Cycles there was a Nuke. Account for both thread-specific and All Thread Nukes."
  character(*),    parameter, public :: MACHINE_CLEARS_CYCLES                                 = "r01C3"
  !   "EventName": "MACHINE_CLEARS.COUNT",
  !   "BriefDescription": "Number of machine clears (nukes) of any type."
  character(*),    parameter, public :: MACHINE_CLEARS_COUNT                                  = "r01C3"
  !    "EventName": "MACHINE_CLEARS.MEMORY_ORDERING",
  !  "BriefDescription": "Counts the number of machine clears due to memory order conflicts.",
  !  "PublicDescription": "This event counts the number of memory ordering machine clears detected.
  !  Memory ordering machine clears can result from memory address aliasing or snoops from another hardware thread or core to data inflight in the pipeline.
  !  Machine clears can have a significant performance impact if they are happening frequently.",
  character(*),    parameter, public :: MACHINE_CLEARS_MEMORY_ORDERING                        = "r02C3"
  !   "EventName": "MACHINE_CLEARS.SMC",
  !   "BriefDescription": "Self-modifying code (SMC) detected.
  character(*),    parameter, public :: MACHINE_CLEARS_SMC                                    = "r04C3"
  !    "EventName": "MACHINE_CLEARS.MASKMOV",
  !  "BriefDescription": "This event counts the number of executed Intel AVX masked load operations that refer to an
  !   illegal address range with the mask bits set to 0.
  character(*),    parameter, public :: MACHINE_CLEARS_MASKMOV                                = "r20C3"
  !    "EventName": "BR_INST_RETIRED.ALL_BRANCHES",
  !   "BriefDescription": "All (macro) branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_ALL_BRANCHES                          = "r00C4"
  !    "EventName": "BR_INST_RETIRED.CONDITIONAL",
  !   "BriefDescription": "Conditional branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_CONDITIONAL                           = "r01C4"
  !    "EventName": "BR_INST_RETIRED.NEAR_CALL",
  !   "BriefDescription": "Direct and indirect near call instructions retired.
  character(*),    parameter, public :: BR_INST_RETIRED_NEAR_CALL                             = "r02C4"
  !   "EventName": "BR_INST_RETIRED.NEAR_CALL_R3",
  !  "BriefDescription": "Direct and indirect macro near call instructions retired (captured in ring 3)
  character(*),    parameter, public :: BR_INST_RETIRED_NEAR_CALL_R3                          = "r02C4"
  !  "EventName": "BR_INST_RETIRED.ALL_BRANCHES_PEBS",
  !   "BriefDescription": "All (macro) branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_ALL_BRANCHES_PEBS                     = "r04C4"
  !   "EventName": "BR_INST_RETIRED.NEAR_RETURN",
  !  "BriefDescription": "Return instructions retired.
  character(*),    parameter, public :: BR_INST_RETIRED_NEAR_RETURN                           = "r08C4"
  !    "EventName": "BR_INST_RETIRED.NOT_TAKEN",
  !    "BriefDescription": "Counts all not taken macro branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_NOT_TAKEN                             = "r10C4"
  !     "EventName": "BR_INST_RETIRED.NEAR_TAKEN",
  !    "BriefDescription": "Taken branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_NEAR_TAKEN                            = "r20C4"
  !   "EventName": "BR_INST_RETIRED.FAR_BRANCH",
  !  "BriefDescription": "Counts the number of far branch instructions retired.",
  character(*),    parameter, public :: BR_INST_RETIRED_FAR_BRANCH                            = "r40C4"
  !   "EventName": "BR_MISP_RETIRED.ALL_BRANCHES",
  !  "BriefDescription": "All mispredicted macro branch instructions retired.
  character(*),    parameter, public :: BR_MISP_RETIRED_ALL_BRANCHES                          = "r00C5"
  !     "EventName": "BR_MISP_RETIRED.CONDITIONAL",
  !  "BriefDescription": "Mispredicted conditional branch instructions retired.
  character(*),    parameter, public :: BR_MISP_RETIRED_CONDITIONAL                           = "r01C5"
  !    "EventName": "BR_MISP_RETIRED.ALL_BRANCHES_PEBS",
  !  "BriefDescription": "Mispredicted macro branch instructions retired. ",
  character(*),    parameter, public :: BR_MISP_RETIRED_ALL_BRANCHES_PEBS                     = "r04C5"
  !   "EventName": "BR_MISP_RETIRED.NEAR_TAKEN",
  !  "BriefDescription": "number of near branch instructions retired that were mispredicted and taken.",
  character(*),    parameter, public :: BR_MISP_RETIRED_NEAR_TAKEN                            = "r20C5"
  !   "EventName": "AVX_INSTS.ALL",
  !  "BriefDescription": "Approximate counts of AVX & AVX2 256-bit instructions, including non-arithmetic instructions, loads, and stores.  
  ! May count non-AVX instructions that employ 256-bit operations, including (but not necessarily limited to) rep string instructions that use 256-bit loads and stores for optimized performance,
  ! XSAVE* and XRSTOR*, and operations that transition the x87 FPU data registers between x87 and MMX.",
  character(*),    parameter, public :: AVX_INSTS_ALL                                         = "r07C6"
  !     "EventName": "HLE_RETIRED.START",
  !  "BriefDescription": "Number of times an HLE execution started.",
  character(*),    parameter, public :: HLE_RETIRED_START                                     = "r01C8"
  !   "EventName": "HLE_RETIRED.COMMIT",
  !  "BriefDescription": "Number of times an HLE execution successfully committed.",
  character(*),    parameter, public :: HLE_RETIRED_COMMIT                                    = "r02C8"
  !  "EventName": "HLE_RETIRED.ABORTED",
  !  "BriefDescription": "Number of times an HLE execution aborted due to any reasons (multiple categories may count as one)."
  character(*),    parameter, public :: HLE_RETIRED_ABORTED                                   = "r04C8"
  !   "EventName": "HLE_RETIRED.ABORTED_MISC1",
  !  "BriefDescription": "Number of times an HLE execution aborted due to various memory events (e.g., read/write capacity and conflicts)
  character(*),    parameter, public :: HLE_RETIRED_ABORTED_MISC1                             = "r08C8"
  !  "EventName": "HLE_RETIRED.ABORTED_MISC2",
  !  "BriefDescription": "Number of times an HLE execution aborted due to uncommon conditions."
  character(*),    parameter, public :: HLE_RETIRED_ABORTED_MISC2                             = "r10C8"
  !   "EventName": "HLE_RETIRED.ABORTED_MISC3",
  !  "BriefDescription": "Number of times an HLE execution aborted due to HLE-unfriendly instructions.",
  character(*),    parameter, public :: HLE_RETIRED_ABORTED_MISC3                             = "r20C8"
  !    "EventName": "HLE_RETIRED.ABORTED_MISC4",
  !  "BriefDescription": "Number of times an HLE execution aborted due to incompatible memory type
  character(*),    parameter, public :: HLE_RETIRED_ABORTED_MISC4                             = "r40C8"
  !  "EventName": "FP_ASSIST.X87_OUTPUT",
  !  "BriefDescription": "output - Numeric Overflow, Numeric Underflow, Inexact Result ",
  character(*),    parameter, public :: FP_ASSIST_X87_OUTPUT                                  = "r02CA"
  !   "EventName": "FP_ASSIST.X87_INPUT",
  !  "BriefDescription": "input - Invalid Operation, Denormal Operand, SNaN Operand ",
  character(*),    parameter, public :: FP_ASSIST_X87_INPUT                                   = "r04CA"
  !    "EventName": "FP_ASSIST.SIMD_OUTPUT",
  !  "BriefDescription": "SSE* FP micro-code assist when output value is invalid.
  character(*),    parameter, public :: FP_ASSIST_SIMD_OUTPUT                                 = "r08CA"
  !    "EventName": "FP_ASSIST.SIMD_OUTPUT",
  !  "BriefDescription": "SSE* FP micro-code assist when output value is invalid. ",
  character(*),    parameter, public :: FP_ASSIST_SIMD_OUTPUT                                 = "r08CA"
  !    "EventName": "FP_ASSIST.SIMD_INPUT",
  !   "BriefDescription": "Any input SSE* FP Assist ",
  character(*),    parameter, public :: FP_ASSIST_SIMD_INPUT                                  = "r10CA"
  !    "EventName": "FP_ASSIST.ANY",
  !  "BriefDescription": "Counts any FP_ASSIST umask was incrementing ",
  character(*),    parameter, public :: FP_ASSIST_ANY                                         = "r1ECA"
  !     "EventName": "ROB_MISC_EVENTS.LBR_INSERTS",
  !   "BriefDescription": "Count cases of saving new LBR",
  character(*),    parameter, public :: ROB_MISC_EVENTS_LBR_INSERTS                           = "r20CC"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_4",
  !  "BriefDescription": "Randomly selected loads with latency value being above 4
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_4                   = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_8",
  !  "BriefDescription": "Randomly selected loads with latency value being above 8
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_8                   = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_16",
  !  "BriefDescription": "Randomly selected loads with latency value being above 16
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_16                   = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_32",
  !  "BriefDescription": "Randomly selected loads with latency value being above 32
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_32                   = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_64",
  !  "BriefDescription": "Randomly selected loads with latency value being above 64
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_64                  = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_128",
  !  "BriefDescription": "Randomly selected loads with latency value being above 128
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_128                  = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_256",
  !  "BriefDescription": "Randomly selected loads with latency value being above 256
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_256                  = "r01CD"
  !    "EventName": "MEM_TRANS_RETIRED.LOAD_LATENCY_GT_512",
  !  "BriefDescription": "Randomly selected loads with latency value being above 512
  character(*),    parameter, public :: MEM_TRANS_RETIRED.LOAD_LATENCY_GT_512                   = "r01CD"
end module mod_core_events







module mod_uncore_events



end module mod_uncore_events
