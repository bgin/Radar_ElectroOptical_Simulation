

  perf_loop744:  select case(pmc_loop744)
                   case(1)
                         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set1.txt -er203 -er803 -er105 -er107 &
                                           -C 7  -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if   
                   case(2)
                         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set2.txt -er100E -er0214 -er3F24 -er003c &
                                          -C 7  -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	           case(3)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set3.txt -er0148 -er0248 -er0149 -er024c &
                                           -C 7 -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
                   case(4)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set4.txt -er0151 -er0279 -er0180 -er0280 &
                                           -C 7  -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	           case(5)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set5.txt -er0480 -er0185 -er4188 -er019c &
                                           -C 7  -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	           case(6)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set6.txt -er01a1 -er02a1 -er04a1 -er08a1 &
                                           -C 7 -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	           case(7)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set7.txt -er10a1 -er20a1 -er80a1 -er01a2 &
                                           -C 7 -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	           case(8)
		         result = SYSTEMQQ("perf stat -o tmatrix_mps_driver_loop744_set8.txt -er11bc -er01c2  &
                                           -C 7 -a sleep 0.007945060729980469")
                         if(result == .false.) then
                             ret = GETLASTERRORQQ()
                             print*, "SYSTEMQQ: Failed to execute perf command -- reason: ", ret, "loop744"
                         end if
	          case default
		         print*, "perf_loop744:  Invalid select case argument: ", pmc_loop744
	          end select perf_loop744		 
			 