

    AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//init_valh
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       ier = -2
                       return
               end select AccessMSR 