
#include "Config.fpp"

module  mod_platform_stats


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_patform_stats
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
     use mod_kinds, only : int4
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4),  parameter :: MOD_PLATFORM_STATS_MAJOR = 1 
    ! Minor version
    integer(kind=int4),  parameter :: MOD_PLATFORM_STATS_MINOR = 0
    ! Micro version
    integer(kind=int4),  parameter :: MOD_PLATFORM_STATS_MICRO = 0
    ! Module full version
    integer(kind=int4),  parameter :: MOD_PLATFORM_STATS_FULLVER = &
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

#if defined __GFORTRAN__
       subroutine read_vmstat(datum,nlines,ioerr,errmsg)  !GCC$ ATTRIBUTES cold :: read_vmstat !GCC$ ATTRIBUTES aligned(32) :: read_vmstat
#elif defined __INTEL_COMPILER
       subroutine read_vmstat(datum,nlines,ioerr,errmsg)
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_vmstat
#endif
           
           character(len=64),   dimension(:),      intent(inout) :: datum
           integer(kind=int4),                     intent(inout) :: nlines
           integer(kind=int4),                     intent(inout) :: ioerr
           character(len=256),                     intent(inout) :: errmsg
           ! Locals
           integer(kind=int4), automatic :: i
           integer(kind=int4), automatic :: iounit
           ! Exec code....
           open(newunit=iounit,FILE="/proc/vmstat",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
           if(ioerr > 0) return
           do
              
              read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
              if(ioerr/=0) exit
              i = i+1
              
             
           end do
           nlines = i
           close(unit=iounit)
          !
       end subroutine read_vmstat

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

#if defined __GFORTRAN__
      subroutine read_meminfo(datum,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_meminfo !GCC$ ATTRIBUTES aligned(32) :: read_meminfo
#elif defined __INTEL_COMPILER
      subroutine read_meminfo(datum,nlines,ioerr,errmsg)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_info
#endif
          character(len=64),  contiguous, dimension(:),  intent(inout) :: datum
          integer(kind=int4),                            intent(inout) :: nlines
          integer(kind=int4),                            intent(inout) :: ioerr
          character(len=128),                            intent(inout) :: errmsg
          ! Locals
          integer(kind=int4), automatic :: i
          integer(kind=int4), automatic :: iounit
          ! Exec code...
          open(newunit=iounit,FILE="proc/meminfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          i = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
             if(ioerr/=0) exit
             i = i+1
          end do
          nlines = i
          close(unit=iounit)
          !
       end subroutine read_meminfo

#if defined __GFORTRAN__        
       subroutine read_cpuinfo(datum,nlines,ioerr,errmsg) !GCC$ ATTRIBUTES cold :: read_cpuinfo !GCC$ ATTRIBUTES aligned(32) :: read_cpuinfo
#elif defined __INTEL_COMPILER
       subroutine read_cpuinfo(datum,nlines,ioerr,errmsg)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: read_cpuinfo
#endif
          character(len=64),  contiguous, dimension(:),  intent(inout) :: datum
          integer(kind=int4),                            intent(inout) :: nlines
          integer(kind=int4),                            intent(inout) :: ioerr
          character(len=128),                            intent(inout) :: errmsg
          ! Locals
          integer(kind=int4), automatic :: i
          integer(kind=int4), automatic :: iounit
          ! Exec code...
          open(newunit=iounit,FILE="proc/cpuinfo",ACTION="READ",STATUS="OLD",IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) return
          i = 0
          do
             read(iounit,'(A)',IOSTAT=ioerr,IOMSG=errmsg) datum(i)
             if(ioerr/=0) exit
             i = i+1
          end do
          nlines = i
          close(unit=iounit)
          !
       end subroutine read_cpuinfo
        
end module mod_platform_stats
