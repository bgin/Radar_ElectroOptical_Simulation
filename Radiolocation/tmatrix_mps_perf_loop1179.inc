

perf_loop1179:  select case(pmc_loop1179)
                   case(1)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set1.txt -er203 -er105 -er108 -er2008 &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179"
                        end if
		   case(2)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set2.txt -er100e -er200e -er0214 -er2424 &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179"
                        end if
	           case(3)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set3.txt -erc424 -er003c -er0148 -er0248 &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop1179"
                        end if
		   case(4)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set4.txt -er024c -er0151 -er015e -er0279  &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179"
                        end if
	           case(5)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set5.txt -er1879 -er0280 -er0480 -er0185   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
	           case(6)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set6.txt -er4188 -er019c -er01a1 -er02a1   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
		   case(7)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set7.txt -er04a1 -er08a1 -er10a1 -er20a1   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
		   case(8)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set8.txt -er40a1 -er80a1 -er01a2 -er02a3   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
		   case(9)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set9.txt -er08c1 -er10c1 -er01c2 -er02c2   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
		   case(10)
		        result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop1179_set10.txt -er07c6   &
                                        -C 7  -a sleep 1")
                        if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print,* "SYSTEMQQ: perf_loop1179 -- Failed to execute perf command -- reason: ", ret, "loop1179 "
                        end if
		   case default
		        print*, "perf_loop1179: Invalid select case argument: ", pmc_loop1179
	           end select perf_loop1179