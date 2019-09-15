

perf_loop1778:  select case(pmc_loop1778)
                   case(1)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set1.txt -er100 -er203 -er105 -er108 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(2)
                       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set2.txt -er100e -er200e -er0214 -er2424 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(3)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set3.txt -er3024 -er3f24 -erc424 -erf824 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(4)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set4.txt -er003c -er0148 -er0248 -er024c &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(5)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set5.txt -er0151 -er015e -er0279 -er1879 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(6)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set6.txt -er0180 -er0280 -er0480 -er0185 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(7)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set7.txt -er4188 -er019c -er01a1 -er02a1 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(8)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set8.txt -er04a1 -er08a1 -er10a1 -er20a1 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(9)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set9.txt -er80a1 -er01a2 -er01a8 -er00c0 &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case(10)
		       result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1778_set10.txt -er01c2 -er074c6  &
                                        -C 7 -a sleep 0.001")
                       if(result == .false.) then
                           ret = GETLASTERRORQQ()
                           print,* "SYSTEMQQ: perf_loop1778 -- Failed to execute perf command -- reason: ", ret, "loop1778"
                       end if
		   case default
		       print,* "perf_loop1778: Invalid select case argument: ", pmc_loop1778
		   end select perf_loop1778