
vtune_loop744:   select case(analysis_type)
                   case(1)
		         result = SYSTEMQQ("amplxe-cl -hotspots -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(2)
		          result = SYSTEMQQ("amplxe-cl -memory-consumption -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
	           case(3)
		          result = SYSTEMQQ("amplxe-cl -collect-with runsa -cpu-mask=0-7 -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(4)
		          result = SYSTEMQQ("amplxe-cl -uarch-exploration -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(5)
		          result = SYSTEMQQ("amplxe-cl -memory-access -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(6)
		          result = SYSTEMQQ("amplxe-cl -threading -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(7)
		          result = SYSTEMQQ("amplxe-cl -hpc-performance -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case(8)
		          result = SYSTEMQQ("amplxe-cl -io -no-allow-multiple-runs -call-stack-mode=user-only &
                                            data-limit=500 -d 1 -inline-mode=on -loop-mode=loop-and-function target-pid"//pid//" &
					    -v")
		          if(result == .false.) then
                            ret = GETLASTERRORQQ()
                            print*, "SYSTEMQQ: Failed to launch vtune profiler  -- reason: ", ret, "loop744"
                          end if
		   case default
		          print*, "vtune_loop744 -- Invalid select case argument: ", vtune_loop744
	    end select vtune_loop744