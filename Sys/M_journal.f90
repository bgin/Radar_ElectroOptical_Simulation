










!>
!!##NAME
!!     M_journal(3fm) - [M_journal::INTRO] write program messages to stdout and/or
!!     a log file
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     use, M_journal , only : journal
!!##DESCRIPTION
!!
!!    For interactive programs in particular it is useful if all messages
!!    go thru the JOURNAL(3f) routine. This makes it easy to write messages
!!    to a log file as well as standard output; to toggle time prefixes
!!    on and off; to turn on and off debug-mode messages; control output
!!    paging and create replayable input journals.
!!
!!    The primary use of JOURNAL(3f) is to create journal files for
!!    interactive programs that can be replayed and/or be used to verify
!!    program executions. Typically, you would echo what the user typed to
!!    the trail file as-is, and write output you write to stdout as comments
!!    to the trail file so that the trail file can easily be read back in
!!    (by ignoring comments). So usually things that are read from user
!!    input are using output with WHERE='T' and output that usually goes
!!    to stdout is written with WHERE='SC' in the JOURNAL(3f) call.
!!
!!     >      :
!!     >      :
!!     > character(len=256) userline, output
!!     > call journal('O','my_trail_file')  ! open trail file
!!     >      :
!!     >      :
!!     > do
!!     >    read(*,'(a)',iostat=ios) userline  ! read user input
!!     >    if(ios.ne.0)exit
!!     >    ! echo user input to trail file
!!     >    call journal('T',userline)
!!     >    ! assume user input causes values i1, i2, and i3 to be calculated
!!     >    write(output,'(i0,1x,i0,1x)')i1,i2,i3 ! build an output line
!!     >    ! write output to stdout and as comment to trail file
!!     >    call journal(output)
!!     >    !or you can specify the WHERE parameter and up to ten scalar values
!!     >    call journal('SC','i1=',i1,'i2=',i2,'i3=',i3)
!!     > enddo
!!
!!    In this example an output line was built with an internal write; but calls
!!    to journal(3f) with numeric values with and without advancing I/O turned on
!!    are often used for simpler output:
!!
!!       I=10
!!       R=20.3
!!       ! write to stdout and trail file without advancing I/O
!!       call journal('+SC','I=',i)
!!       call journal('SC','AND R=',r)
!!
!!    writes to the trail file are ignored unless a trail file was opened with
!!
!!       CALL JOURNAL('O',filename)
!!
!!
!!    So that routines that do their output via JOURNAL(3f) can be used with and
!!    without programs generating trail files. That is, destinations 'T' and 'C'
!!    are ignored unless a trail file has been requested.
!!
!!    With no parameters, the trail file is flushed.
!!
!!##EXAMPLES
!!
!!
!!    The man(1) page for journal(3f) describes all the options for the WHERE field.
!!    In addition to being used to generate a journal, the routine can be used for
!!    producing optional debug messages and timing information.
!!
!!    Sample program for debug messages:
!!
!!      program demo_journal
!!      !! showing creating debug messages
!!      use M_journal, only : journal
!!      implicit none
!!      !! produces no output because trail is not on
!!      call journal('D','*demo* DEBUG MESSAGE 001 IGNORED')
!!      !! turn on debug messages
!!      call journal('>','debug on')
!!      !! produces output on stdout because debug mode
!!      !! is on but no named trail file
!!      call journal('D','*demo* DEBUG MESSAGE 002 ON STDOUT')
!!      !! open trail file
!!      call journal('O','mytrail.txt')
!!      !! debug messages now go to the trail file
!!      call journal('D','*demo* DEBUG MESSAGE 003 TO TRAIL')
!!      !! close trail file so messages go to stdout again
!!      call journal('O','')
!!      !! debug on stdout now
!!      call journal('D','*demo* DEBUG MESSAGE 004 TO STDOUT')
!!      call journal('<','debug off')
!!      !! back to no output from the next message
!!      call journal('D','*demo* DEBUG MESSAGE 005 IGNORED')
!!      end program demo_journal
!!
!!   Sample program for trail messages with optional timing information:
!!
!!      program testit
!!      use M_journal,only : journal
!!      implicit none
!!      call journal('a single string A -should be on S')
!!
!!      ! add time prefix to output
!!      call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
!!      call journal('a single string B -should be on S with prefix')
!!      call journal('%','CPU_TIME: %c:CALLS: %C: %b')  ! change time prefix
!!      call journal('a single string B-1 -should be on S with prefix')
!!      call journal('a single string B-2 -should be on S with prefix')
!!      call journal('a single string B-3 -should be on S with prefix')
!!      !  Other useful time formats:
!!      !     %E -- Unix Epoch time
!!      !     %e -- integer value of Unix Epoch time
!!      !     %C -- number of times this format is used
!!      !     %c -- CPU_time(3f) output
!!      !     %S -- seconds since last use of this format
!!      !     %k -- CPU time in seconds from system_clock
!!      call journal('%','') ! turn off time prefix
!!      !
!!      call journal('a single string C -should be on S')
!!      !
!!      call journal('O','aaa.out') ! turn on trail file
!!      call journal('a single string D -should be on SC')
!!      call journal('a single string E -should be on SC')
!!      call journal('a single string F -should be on SC')
!!      call journal('O','') ! turn off trail file
!!      !
!!      call journal('a single string G -should be on S')
!!      call journal('a single string H -should be on S')
!!      call journal('a single string I -should be on S')
!!
!!      ! build one line of output with intrinsic scalar values added
!!      call journal('+sc','APPEND:')
!!      call journal('+sc',' integer',         1234)
!!      call journal('+sc',' and real',        1234.5678)
!!      call journal('+sc',' and double',1234567890.123456d0)
!!      call journal('+sc',' and logical',    .true.)
!!      call journal('sc','')
!!      !
!!      end program testit
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_journal
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
use :: M_msg,                      only : str
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_journal] provides public message routine, no paging or graphic mode change
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,],[VALUE(s)])
!!
!!     character(len=*),intent(in) :: where
!!     character(len=*)|real|integer|doubleprecision|complex,optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!
!!   WRITE MESSAGES
!!    basic messages
!!
!!       call journal(where,[VALUE(S)])
!!       call journal(message) # a shortcut for "call journal('sc',message)":
!!   OPEN OR CLOSE TRAIL FILE
!!    trail file
!!
!!       call journal('O',trailfile_name) # open trail file
!!       call journal('O','')             # close trail file
!!   SET OUTPUT TIME PREFIX
!!    set the function display format for timestamps. See the NOW(3f)
!!    procedure for allowable timestamp macros
!!
!!       call journal('%',time_stamp_prefix_specification)
!!
!!   MODES
!!
!!    Turn on/off writing DEBUG messages to trail file
!!
!!       call journal('>','debug on') # turn on debug mode
!!       call journal('<','debug off') # turn off debug mode
!!
!!   ASSIGN STDOUT TO AN ALTERNATE FILE
!!    change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!
!!       call journal(iunit,filename)
!!
!!    change stdout to iunit to use a file already open
!!
!!       call journal(iunit)
!!
!!##DESCRIPTION
!!
!!    If a user procedure is used for outputting messages instead of calling
!!    WRITE(3f) it is easy to provide control of when messages are printed
!!    (ie. a "verbose" mode, or "quite" mode), creating files to replay
!!    program execution, duplicating output, ...
!!
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the
!!          following characters can be used...
!!
!!      Usually one of these to write to the standard output files ...
!!
!!      S   write to stdout or iounit set with journal(unit) or
!!          journal(unit,filename).
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file
!!      defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#)
!!          character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with "DEBUG:" prefix in front
!!          of message (if file is open) if debug mode is on. Write to stdout
!!          if no trail file and debug mode is on.
!!
!!      Modifier for S|E|C|T|D specifiers
!!
!!      +   subsequent files are written to with advance='no'. Position is
!!          important. '+sc' does an advance='no' on both files, 's+c'
!!          only does the advance='no' for the trail file.
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file using value of "message" parameter or close
!!          trail file if no filename or a blank filename.
!!      A   Auxiliary programs that also want to write to the current log file
!!          (a2b, z2a, ...) call this routine to see if there is a trail file
!!          being generated and then add to it so that a program like ush(1f)
!!          can call the auxiliary programs and still just generate one log file,
!!          but if the auxiliary program is used as a stand-alone program no trail
!!          is generated.
!!
!!   VALUES(S)   message to write to stdout, stderr, and the trail file.
!!               a numeric or character value to optionally be appended
!!               to the message. Up to nine values are allowed. The WHERE
!!               field is required if values are added.
!!   FILENAME    when WHERE="O" to turn the trail file on or off, the "message"
!!               field becomes the trail filename to open. If blank, writing
!!               to the trail file is turned off.
!!   TFORMAT     when WHERE="%" the message is treated as a time format
!!               specification as described under now(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_journal
!!    use M_journal, only : journal
!!    !! BASIC USAGE
!!    call journal('write to standard output as-is, and trail file as a comment if open')
!!    ! since we have not opened a trail file yet, only stdout will display output
!!    call journal('c','ignored, as trail file is not open')
!!    ! now open trail file "trail"
!!    call journal('o','trail')
!!    call journal('sc','same thing except now trail file is open')
!!    ! only write to trail file if open
!!    call journal('c','not ignored, as trail file is open. Written with # suffix')
!!    call journal('t','not ignored, as trail file is open. Written as-is')
!!    ! turn off trail file
!!    call journal('o','')
!!    end program demo_journal
!!
!!   Adding intrinsic scalar values to the message:
!!
!!    program test_journal
!!    use M_journal, only: journal
!!    implicit none
!!       call journal('S','This is a test with no optional value')
!!       call journal('S','This is a test with a logical value',.true.)
!!       call journal('S','This is a test with a double value',1234567890.123456789d0)
!!       call journal('S','This is a test with a real value',1234567890.123456789)
!!       call journal('S','This is a test with an integer value',1234567890)
!!       call journal('STDC','This is a test using STDC',1234567890)
!!       call journal('stdc','This is a test using stdc',1234567890)
!!       call journal('o','journal.txt')                        ! open trail file
!!       call journal('S',1,12.34,56789.111111111d0,.false.,'a bunch of values')
!!       ! the combinations that make sense
!!       call journal('st','stdout and trail')
!!       call journal('s' ,'stdout only')
!!       call journal('t' ,'trail only')
!!       call journal('sc','stdout and trail_comment')
!!       call journal('c' ,'trail_comment only ')
!!       call journal('d' ,'debug only')
!!       call journal('e' ,'stderr only')
!!       call journal('o' ,' ') ! closing trail file
!!    end program test_journal
!!
!!    program testit
!!    ! this is a utility program that calls the module routines. It is typically built using ccall(1).
!!    use M_journal, only : journal
!!       character(len=:),allocatable :: time_stamp_prefix
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES')
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() but did not generate a log file')
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH LOG FILE')
!!       call journal('o','journal.txt')                        ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generated log file journal.txt')
!!       call journal('','journal.txt')                         ! close trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH TIMING INFORMATION')
!!       time_stamp_prefix='CPU_TIME=%c:CALLS=%C:SINCE=%S:%b'  ! change time prefix
!!       call journal('%',time_stamp_prefix) ! set a time prefix in front of messages
!!       call journal('o','timed.txt')                          ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generate log file timed.txt')
!!       call journal('','timed.txt')                           ! close trail file
!!       call journal('%','')                                   ! turn off time prefix
!!       call journal('o','timed.txt')                          ! open trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!
!!    contains
!!
!!       subroutine two()
!!          call journal('Entered subroutine two')
!!          call journal('Exited subroutine two')
!!       end subroutine two
!!
!!       subroutine one()
!!          call journal('Entered subroutine one')
!!          sum=-HUGE(1.0)
!!          do i=1,10000000
!!            sum=sum+sqrt(real(i))
!!          enddo
!!          write(*,*)'SUM=',sum
!!          call journal('Exited subroutine one')
!!       end subroutine one
!!
!!    end program testit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
public journal

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

! ident_1="@(#)M_journal::journal(3fg): provides public message routine, no paging or graphic mode change"

! global variables

!integer,parameter,private  :: stdin=INPUT_UNIT
integer,save,private       :: my_stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

interface
   function now_ex(format)
      character(len=*),intent(in),optional :: format
      character(len=:),allocatable         :: now_ex
   end function now_ex
end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

! ident_2="@(#)M_journal::where_write_message(3fp): basic message routine used for journal files"

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn on debug messages (change mode), which are ones with WHERE='D'
!     where=< turn off debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to my_stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
character(len=:),allocatable       :: prefix            ! the prefix string to add to output
logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times.eq.0)then
         !!   write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('>'); debug=.true.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('<'); debug=.false.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for my_stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios.eq.0)then
               my_stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            my_stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times.eq.0)then
            !! write(my_stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times.eq.0)then
               write(my_stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(my_stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()

! ident_3="@(#)M_journal::flush_trail(3fp): flush trail file"

call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)

! ident_4="@(#)M_journal::set_stdout_lun(3fp): change I/O logical unit value for standard writes"

integer,intent(in)                   :: iounit
   my_stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    where_write_message_all(3f) - [M_journal] converts any standard scalar type to a string and calls journal(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine where_write_message_all(where,g0,g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!     character(len=*),intent(in)   :: where
!!     class(*),intent(in)           :: g0
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     character,intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    where_write_message_all(3f) builds and writes a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!
!!    where    string designating where to write message, as with journal(3f)
!!    g0       value to print. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!    g[1-9]   optional additional values to print the value of after g0.
!!    sep      separator to add between values. Default is a space
!!##RETURNS
!!    where_write_message_all  description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wm_all
!!    use M_journal, only : where_write_message_all
!!    implicit none
!!    end program program demo_wm_all
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine where_write_message_all(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, sep)
implicit none

! ident_5="@(#)M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
character,intent(in),optional :: sep
call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,sep))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

! ident_6="@(#)M_journal::write_message_only(3fp): calls JOURNAL('sc',message)"

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
