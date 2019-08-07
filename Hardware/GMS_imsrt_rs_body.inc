

           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
