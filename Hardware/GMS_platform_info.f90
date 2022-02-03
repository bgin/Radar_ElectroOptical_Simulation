
#include "Config.fpp"

module  platform_info


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         patform_stats
 !          
 !          Purpose:
  !                         Linux and platform (HW) statistics
  !                         Fortran subroutine readers.
 !          History:
 !                        Date: 20-12-2019
 !                        Time: 10:13 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                   Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds, only : i4
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4),  parameter :: MOD_PLATFORM_STATS_MAJOR = 1 
    ! Minor version
    integer(kind=i4),  parameter :: MOD_PLATFORM_STATS_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: MOD_PLATFORM_STATS_MICRO = 0
    ! Module full version
    integer(kind=i4),  parameter :: MOD_PLATFORM_STATS_FULLVER = &
         1000*MOD_PLATFORM_STATS_MAJOR+100*MOD_PLATFORM_STATS_MINOR+10*MOD_PLATFORM_STATS_MINOR
    ! Module creation date
    character(*),        parameter :: MOD_PLATFORM_STATS_CREATION_DATE = "20-12-2019 10:20 +00200 (FRI 20 DEC 2019 GMT+2)"
    ! Module build date
    character(*),        parameter :: MOD_PLATFORM_STATS_BUILD_DATE    = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: MOD_PLATFORM_STATS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Module short info
    character(*),        parameter :: MOD_PLATFORM_STATS_SYNOPSIS      = "Linux and platform Hardware statistic."

  contains

#if defined __GFORTRAN__
    subroutine get_self_vmpeak(vmpeak) !GCC$ ATTRIBUTES cold :: get_self_vmpeak !GCC$ ATTRIBUTES aligned(32) :: get_self_vmpeak
#elif defined __INTEL_COMPILER
    subroutine get_self_vmpeak(vmpeak)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: get_self_vmpeak
#endif
      integer(kind=int4),       intent(inout) :: vmpeak
      ! Locals
      character(len=32),  automatic :: key,val
      integer(kind=int4), automatic :: iounit
      ! Exec code ....
      vmpeak = 0
      open(newunit=iounit,name="/proc/self/status",status='scratch',err=9999)
      do while(.true.)
         read(unit=iounit,fmt=*,err=8888) key,val
         if(key == "VmPeak:") then
            read(unit=val,fmt = '(I)') vmpeak
            exit
         end if
      end do
8888  close(unit=iounit)
      if(vmpeak == 0) goto 9999
      return
9999  print*, "vmpeak has unusual value=", vmpeak
      vmpeak = -9999
    end subroutine get_self_vmpeak

#if defined __GFORTRAN__
    subroutine get_anyproc_vmpeak(pid,vmpeak) !GCC$ ATTRIBUTES cold :: get_anyproc_vmpeak !GCC$ ATTRIBUTES aligned(32) :: get_anyproc_vmpeak
#elif defined __INTEL_COMPILER
    subroutine get_anyproc_vmpeak(pid,vmpeak)
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: get_anyproc_vmpeak
      character(len=24),     intent(in)    :: pid
      integer(kind=int4),    intent(inout) :: vmpeak
      ! Locals
      character(len=32), automatic :: key,val
      integer(kind=int4),automatic :: iounit
      ! Exec code ...
      vmpeak = 0
      open(newunit=iounit,name=pid,status='scratch',err=9999)
      do while(.true.)
         read(unit=iounit,fmt=*,err=8888) key,val
         if(key == "VmPeak:") then
            read(unit=val,fmt = '(I)') vmpeak
            exit
         else goto 8888
         end if
      end do
8888  close(unit=iounit)
      if(vmpeak == 0) goto 9999
      return
9999  print*, "vmpeak for process://pid has unusual value=", vmpeak
      vmpeak = -9999
    end subroutine get_anyproc_vmpeak
    
#if defined __GFORTRAN__
     subroutine get_self_vmsize(vmsize) !GCC$ ATTRIBUTES cold :: get_self_vmsize !GCC$ ATTRIBUTES aligned(32) :: get_self_vmsize
#elif defined __INTEL_COMPILER
     subroutine get_self_vmsize(vmsize)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: get_self_vmsize
#endif
        integer(kind=int4),       intent(inout) :: vmsize
        ! Locals
        character(len=32), automatic :: key,val
        integer(kind=int4),automatic :: iounit
        ! EXec code ...
        vmsize = 0
        open(newunit=iounit,name="/proc/self/status",status='scratch',err=9999)
        do while(.true.)
           read(unit=iounit,fmt=*,err=8888) key,val
           if(key == "VmSize:") then
              read(unit=val,fmt = '(I)') vmsize
              exit
           end if
        end do
8888    close(unit=iounit)
        if(vmsize == 0) goto 9999
        return
9999    print*, "vmsize for self  has an unusual value:", vmsize
        vmsize = -9999
    end subroutine get_self_vmsize

#if defined __GFORTRAN__
    subroutine get_cores_num(ncores)  !GCC$ ATTRIBUTES cold :: get_cores_num !GCC$ ATTRIBUTES aligned(32) :: get_cores_num
#elif defined __INTEL_COMPILER
    subroutine get_cores_num(ncores)
         !DIR$ ATTRIBUTES COLD_ALIGN : 32 :: get_cores_num
#endif  
         integer(kind=int4),      intent(inout) :: ncores
         ! Locals
         character(len=9),   automatic :: key
         integer(kind=int4), automatic :: iounit,stat
         ! Exec code ...
         ncores = 0
         open(newunit=iounit,name="/proc/cpuinfo",status='scratch',iostat=stat,err=9999)
         do while(EOF(iounit)) then
            read(unit=iounit,fmt=*,iostat=stat,err=9999) key
            if(key == "processor") then
               ncores = ncores+1
            end if
         end do
         close(unit=iounit)
         return
9999     close(unit=iounit)
         print*, "An I/O error has ocurred: ",stat
       end subroutine get_cores_num

!#if defined __GFORTRAN__
!       subroutine read_vmstat(datum,nlines,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_vmstat !GCC$ ATTRIBUTES aligned(32) :: read_vmstat
!#elif defined __INTEL_COMPILER
!       subroutine read_vmstat(datum,nlines,ioerr,errmsg)
!           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_vmstat
!#endif
!           
!           character(len=64),   dimension(:),      intent(inout) :: datum
!           integer(kind=int4),                     intent(inout) :: nlines
!           integer(kind=int4),                     intent(inout) :: ioerr
!           character(len=256),                     intent(inout) :: errmsg
!           ! Locals
!           integer(kind=int4), automatic :: i
!           integer(kind=int4), automatic :: iounit
!           ! Exec code....
!           open(newunit=iounit,FILE="/proc/vmstat",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
!           if(ioerr > 0) return
!           do
!              
!              read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
!              if(ioerr/=0) exit
!              i = i+1
!              
!             
!           end do
!           nlines = i
!           close(unit=iounit)
!          !
!       end subroutine read_vmstat

#if defined __GFORTRAN__
       subroutine get_nlines(filename,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: get_nlines !GCC$ ATTRIBUTES aligned(32) :: get_nlines
#elif defined __INTEL_COMPILER
      subroutine get_nlines(filename,nlines,ioerr,errmsg)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: get_nlines
#endif
         character(len=128),      intent(in)    :: filename
         integer(kind=int4),      intent(inout) :: nlines
         integer(kind=int4),      intent(inout) :: ioerr
         character(len=128),      intent(inout) :: errmsg
         ! Locals
         character(len=256), automatic :: line
         integer(kind=int4), automatic :: iounit
         ! Exec code ....
         line = " "
         open(newunit=iounit,FILE=trim(filename),ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
         if(ioerr > 0) return
         do
            read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) line
            if(ioerr/=0) exit
            nlines = nlines+1
         end do
         close(unit=iounit)
         !
      end subroutine get_nlines

!#if defined __GFORTRAN__
!      subroutine read_meminfo(datum,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_meminfo !GCC$ ATTRIBUTES aligned(32) :: read_meminfo
!#elif defined __INTEL_COMPILER
!      subroutine read_meminfo(datum,nlines,ioerr,errmsg)
!          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_info
!#endif
!          character(len=64),  contiguous, dimension(:),  intent(inout) :: datum
!          integer(kind=int4),                            intent(inout) :: nlines
!          integer(kind=int4),                            intent(inout) :: ioerr
!          character(len=128),                            intent(inout) :: errmsg
!          ! Locals
!          integer(kind=int4), automatic :: i
!          integer(kind=int4), automatic :: iounit
!          ! Exec code...
!          open(newunit=iounit,FILE="/proc/meminfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
!          if(ioerr > 0) return
!          i = 0
!          do
!             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
!             if(ioerr/=0) exit
!             i = i+1
!          end do
!          nlines = i
!          close(unit=iounit)
!          !
!!       end subroutine read_meminfo

!#if defined __GFORTRAN__        
!       subroutine read_cpuinfo(datum,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_cpuinfo !GCC$ ATTRIBUTES aligned(32) :: read_cpuinfo
!#elif defined __INTEL_COMPILER
!       subroutine read_cpuinfo(datum,nlines,ioerr,errmsg)
!         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_cpuinfo
!#endif
!          character(len=64),  contiguous, dimension(:),  intent(inout) :: datum
!          integer(kind=int4),                            intent(inout) :: nlines
!          integer(kind=int4),                            intent(inout) :: ioerr
!          character(len=128),                            intent(inout) :: errmsg
!          ! Locals
!          integer(kind=int4), automatic :: i
!          integer(kind=int4), automatic :: iounit
!          ! Exec code...
!          open(newunit=iounit,FILE="/proc/cpuinfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
!          if(ioerr > 0) return
!          i = 0
!          do
!             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
!             if(ioerr/=0) exit
!             i = i+1
!          end do
!          nlines = i
!          close(unit=iounit)
!          !
!      end subroutine read_cpuinfo
        

        

!!----------- Subroutines with run-time memory allocation ---------------!!

#if defined __GFORTRAN__
      subroutine read_cpuinfo(datum,llen,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_cpuinfo !GCC$ ATTRIBUTES aligned(32) :: read_cpuinfo
#elif defined __INTEL_COMPILER
      subroutine read_cpuinfo(datum,llen,nlines,ioerr,errmsg)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_cpuinfo
#endif
          character(len=:), allocatable,  dimension(:),  intent(inout) :: datum
          integer(kind=i4),                            intent(inout) :: llen
          integer(kind=i4),                            intent(inout) :: nlines
          integer(kind=i4),                            intent(inout) :: ioerr
          character(len=128),                            intent(inout) :: errmsg
          ! Locals
          character(len=256),  automatic :: string
          integer(kind=i4),  automatic :: i,n,m
          integer(kind=i4),  automatic :: iounit
          integer(kind=i4),  automatic :: aerr
          ! EXec code .....
          if(allocated(datum)) return
          open(newunit=iounit,FILE="/proc/cpuinfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
            close(iounit)
            return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          llen=n
          nlines=m
          close(iounit)
      end subroutine read_cpuinfo

        

#if defined __GFORTRAN__
      subroutine read_meminfo(datum,strlen,nstrings,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_meminfo !GCC$ ATTRIBUTES aligned(32) :: read_meminfo
#elif defined __INTEL_COMPILER
      subroutine read_meminfo(datum,strlen,nstrings,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_meminfo
          character(len=:), allocatable, dimension(:),  intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=256), automatic :: string
          integer(kind=i4), automatic :: i,n,m
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr
          ! EXec code ....
          if(allocated(datum)) return
          open(newunit=iounit,FILE="/proc/meminfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
         !
          allocate(character(len=n)::datum(m),STAT=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
          close(iounit)
      end subroutine read_meminfo

#if defined __GFORTRAN__        
      subroutine read_vmstat(datum,strlen,nstrings,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_vmstat !GCC$ ATTRIBUTES aligned(32) :: read_vmstat
#elif defined __INTEL_COMPILER 
      subroutine read_vmstat(datum,strlen,nstrings,ioerr,errmsg)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_vmstat
#endif
          character(len=:), allocatable, dimension(:),  intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=256), automatic :: string
          integer(kind=i4), automatic :: i,n,m
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr
          ! EXec code ....
          if(allocated(datum)) return
          open(newunit=iounit,FILE="/proc/vmstat",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),STAT=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
          close(iounit)
      end subroutine read_vmstat

#if defined __GFORTRAN__
      subroutine read_self_status(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_self_status !GCC$ ATTRIBUTES aligned(32) :: read_self_status
#elif defined __INTEL_COMPILER
      subroutine read_self_status(datum,strlen,nstrings,filesz,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_self_status
#endif
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=512), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/self/status",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/self/status",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("/proc/self/status",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code: ", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/self/status",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit)
     end subroutine read_self_status

#if defined __GFORTRAN__
     subroutine read_self_io(datum,strlen,nstrings,filesz,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_self_io !GCC$ ATTRIBUTES aligned(32) :: read_self_io
#elif defined __INTEL_COMPILER
     subroutine read_self_io(datum,strlen,nstrings,filesz,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_self_io
#endif
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=512), automatic :: string
#if defined __GFORTRAN__
          integer(kind=int4), dimension(13), automatic :: buf
          integer(kind=int4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize=0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/self/io",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/self/io",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("/proc/self/io",buf,gfstat)
          if(gfstat == 0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code: ",gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/self/io",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit)
     end subroutine read_self_io

#if defined __GFORTRAN__
     subroutine read_self_maps(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_self_maps !GCC$ ATTRIBUTES aligned(32) :: read_self_maps
#elif defined __INTEL_COMPILER
     subroutine read_self_maps(datum,strlen,nstrings,filesz,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_self_maps 
#endif
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=512), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/self/maps",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/self/maps",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/self/maps",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/self/maps",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit)
     end subroutine read_self_maps

#if defined __GFORTRAN__
     subroutine read_self_numa_maps(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_self_numa_maps !GCC$ ATTRIBUTES aligned(32) :: read_self_numa_maps
#elif defined __INTEL_COMPILER
     subroutine read_self_numa_maps(datum,strlen,nstrings,filesz,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_self_numa_maps
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=512), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/self/numa_maps",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/self/numa_maps",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/self/numa_maps",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/self/numa_maps",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit)
     end subroutine read_self_numa_maps

#if defined __GFORTRAN__        
     subroutine read_self_smaps_dalloc(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_self_smaps_dalloc !GCC$ ATTRIBUTES aligned(32) :: read_self_smaps_dalloc
#elif defined __INTEL_COMPILER
     subroutine read_self_smaps_dalloc(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_self_smaps_dalloc
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                           intent(inout) :: errmsg
          ! Locals
          character(len=512), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/self/smaps",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/self/smaps",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/self/smaps",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/self/smaps",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
      end subroutine read_self_smaps


#if defined __GFORTRAN__        
     subroutine read_devices(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_devices !GCC$ ATTRIBUTES aligned(32) :: read_devices
#elif defined __INTEL_COMPILER
     subroutine read_devices(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_devices
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                         intent(inout) :: errmsg
          ! Locals
          character(len=32), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/devices",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/devices",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/devices",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/devices",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
      end subroutine read_devices


#if defined __GFORTRAN__        
     subroutine read_timer_list(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_timer_list !GCC$ ATTRIBUTES aligned(32) :: read_timer_list
#elif defined __INTEL_COMPILER
     subroutine read_timer_list(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_timer_list
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                         intent(inout) :: errmsg
          ! Locals
          character(len=32), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/timer_list",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/timer_list",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/timer_list",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/timer_list",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
     end subroutine read_timer_list

#if defined __GFORTRAN__        
     subroutine read_softirqs(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_softirqs !GCC$ ATTRIBUTES aligned(32) :: read_softirqs
#elif defined __INTEL_COMPILER
     subroutine read_devices(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_softirqs
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=256),                         intent(inout) :: errmsg
          ! Locals
          character(len=32), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/sofirqs",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/softirqs",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/softirqs",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/softirqs",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),istat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
      end subroutine read_softirqs

        
#if defined __GFORTRAN__        
     subroutine read_interrupts(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_interrupts !GCC$ ATTRIBUTES aligned(32) :: read_interrupts
#elif defined __INTEL_COMPILER
     subroutine read_interrupts(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_interrupts
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=128),                         intent(inout) :: errmsg
          ! Locals
          character(len=32), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/interrupts",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/interrupts",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/interrupts",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/interrupts",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
      end subroutine read_interrupts

        
#if defined __GFORTRAN__        
     subroutine read_iomem(datum,strlen,nstrings,filesz,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_iomem !GCC$ ATTRIBUTES aligned(32) :: read_iomem
#elif defined __INTEL_COMPILER
     subroutine read_iomem(datum,strlen,nstrings,filesz,ioerr,errmsg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_iomem
#endif 
          character(len=:),  allocatable, dimension(:), intent(inout) :: datum
          integer(kind=i4),                           intent(inout) :: strlen
          integer(kind=i4),                           intent(inout) :: nstrings
          integer(kind=i4),                           intent(inout) :: filesz
          integer(kind=i4),                           intent(inout) :: ioerr
          character(len=64),                         intent(inout) :: errmsg
          ! Locals
          character(len=32), automatic :: string
#if defined __GFORTRAN__
          integer(kind=i4), dimension(13), automatic :: buf
          integer(kind=i4), automatic :: gfstat
#endif
          integer(kind=i4), automatic :: i,m,n
          integer(kind=i4), automatic :: iounit
          integer(kind=i4), automatic :: aerr,fsize
          logical(kind=i4), automatic :: is_present
          ! EXec code ....
          if(allocated(datum)) return
          fsize = 0
#if defined __INTEL_COMPILER
          inquire(FILE="/proc/iomem",EXIST=is_present,SIZE=fsize)
          if(.not.is_present .or. fsize == 0) return
#elif defined __GFORTRAN__
          inquire(FILE="/proc/iomem",EXIST=is_present)
          if(.not.is_present) return
          gfstat=0
          call stat("proc/iomem",buf,gfstat)
          if(gfstat==0) then
             if(buf(8)==0) return
          else
             print*, "stat failed with a status code:", gfstat
             return
          end if
#endif
          open(newunit=iounit,FILE="/proc/iomem",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          n = 0
          m = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             n = max(n,len_trim(string))
             if(ioerr/=0) exit
             m = m+1
          end do
          !
          allocate(character(len=n)::datum(m),stat=aerr)
          if(aerr/=0) then
             close(iounit)
             return
          end if
          rewind(iounit)
          do i=1,m
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) string
             if(ioerr/=0) exit
             datum(i) = string
          end do
          if(ioerr/=iostat_end) then
             deallocate(datum)
             close(iounit)
             return
          end if
          strlen = n
          nstrings = m
#if defined __INTEL_COMPILER
          filesz=fsize
#elif defined __GFORTRAN__
          filesz=buf(8)
#endif
          close(iounit) 
      end subroutine read_iomem


        

        
        

        
     
end module platform_info
