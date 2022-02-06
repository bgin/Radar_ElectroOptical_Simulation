module M_msg
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------
! USED SO FREQUENTLY IN OTHER MODULES PUT IN THIS ONE WITH NO DEPENDENCIES TO PREVENT CIRCULAR DEPENDENCY
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_1="@(#)M_msg::str(3f): {msg_scalar,msg_one}"

public str
public stderr
public wrt
public fmt
!!public :: a,i,f,g

interface str
   module procedure msg_scalar, msg_one
end interface str

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    str(3f) - [M_msg] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!      & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!      class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!      character(len=*),intent(in),optional :: sep
!!      character,len=(:),allocatable :: str
!!
!!##DESCRIPTION
!!    str(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator string used between values. Defaults to a space.
!!
!!##RETURNS
!!    str     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_msg, only : str
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!    pr=str('HUGE(3f) integers',huge(0),&
!!    &'and real',huge(0.0),'and double',huge(0.0d0))
!!    write(*,'(a)')pr
!!    pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    write(*,'(a)')pr
!!    pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    write(*,'(a)')pr
!!    pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    write(*,'(a)')pr
!!
!!    ! create a format on the fly
!!    biggest=huge(0)
!!    frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!    write(*,*)'format=',frmt
!!
!!    ! although it will often work, using str(3f)
!!    ! in an I/O statement is not recommended
!!    ! because if an error occurs str(3f) will try
!!    ! to write while part of an I/O statement
!!    ! which not all compilers can handle and is currently non-standard
!!    write(*,*)str('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_msg::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,&
               & generica,genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj,&
               & sep)
implicit none

! ident_3="@(#)M_msg::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
class(*),intent(in),optional  :: generica(:), genericb(:), genericc(:), genericd(:), generice(:)
class(*),intent(in),optional  :: genericf(:), genericg(:), generich(:), generici(:), genericj(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=1+len(sep)
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   istart=len_trim(line)+increment+1
   line=trim(line)//']'//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_msg] convert any intrinsic to a string using specified format
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function fmt(value,format) result(string)
!!
!!     class(*),intent(in),optional :: value
!!     character(len=*),intent(in),optional  :: format
!!     character(len=:),allocatable :: string
!!##DESCRIPTION
!!    FMT(3f) converts any standard intrinsic value to a string using the specified
!!    format.
!!##OPTIONS
!!    value    value to print the value of. May be of type INTEGER, LOGICAL,
!!             REAL, DOUBLEPRECISION, COMPLEX, or CHARACTER.
!!    format   format to use to print value. It is up to the user to use an
!!             appropriate format. The format does not require being
!!             surrounded by parenthesis. If not present a default is selected
!!             similar to what would be produced with free format.
!!##RETURNS
!!    string   A string value
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_fmt
!!     use :: M_msg, only : fmt
!!     implicit none
!!     character(len=:),allocatable :: output
!!
!!        output=fmt(10,"'[',i0,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(10.0/3.0,"'[',g0.5,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(.true.,"'The final answer is [',g0,']'")
!!        write(*,*)'result is ',output
!!
!!     end program demo_fmt
!!
!!   Results:
!!
!!     result is [10]
!!     result is [3.3333]
!!     result is The final answer is [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive function fmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_4="@(#)M_msg::fmt(3f): convert any intrinsic to a string using specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in),optional  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
integer                      :: ios
character(len=255)           :: msg
character(len=1),parameter   :: null=char(0)
integer                      :: ilen
   if(present(format))then
      fmt_local=format
   else
      fmt_local=''
   endif
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   if(fmt_local.eq.'')then
      select type(generic)
         type is (integer(kind=int8));     fmt_local='(i0,a)'
         type is (integer(kind=int16));    fmt_local='(i0,a)'
         type is (integer(kind=int32));    fmt_local='(i0,a)'
         type is (integer(kind=int64));    fmt_local='(i0,a)'
         type is (real(kind=real32));      fmt_local='(1pg0,a)'
         type is (real(kind=real64));      fmt_local='(1pg0,a)'
         type is (real(kind=real128));     fmt_local='(1pg0,a)'
         type is (logical);                fmt_local='(l1,a)'
         type is (character(len=*));       fmt_local='(a,a)'
         type is (complex);                fmt_local='("(",1pg0,",",1pg0,")",a)'
      end select
   else
      if(format(1:1).eq.'(')then
         fmt_local=format(:len_trim(format)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   allocate(character(len=256) :: line) ! cannot currently write into allocatable variable
   ios=0
   select type(generic)
      type is (integer(kind=int8));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int16));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int32));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int64));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real32));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real64));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real128));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (logical);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (character(len=*));       write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (complex);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
   end select
   if(ios.ne.0)then
      line='<ERROR>'//trim(msg)
   else
      ilen=index(line,null,back=.true.)
      if(ilen.eq.0)ilen=len(line)
      line=line(:ilen-1)
   endif
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stderr(3f) - [M_msg] write message to stderr
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine stderr(msg,[generic])
!!
!!     class(*),intent(in),optional :: msg
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!##DESCRIPTION
!!    STDERR(3f) writes a message to standard error using a standard f2003 method.
!!    Up to ten generic options are available.
!!##OPTIONS
!!    msg           - description to print
!!    generic[0-9]  - optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_stderr
!!    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    use,intrinsic :: iso_fortran_env, only : real32, real64, real128
!!    use,intrinsic :: iso_fortran_env, only : real=> real32, integer=> int32
!!    use M_msg, only: stderr
!!    implicit none
!!
!!    call stderr('A simple message')
!!    call stderr('error: RVALUE=',3.0/4.0)
!!    call stderr('error: IVALUE=',123456789)
!!    call stderr('error: LVALUE=',.true.)
!!
!!    SEVERAL: block
!!    integer :: least=10, most=999, ival=-10
!!    call stderr('error: value',ival,'should be between',least,'and',most)
!!    endblock SEVERAL
!!
!!    call stderr('real32  :',huge(0.0_real32),0.0_real32,12345.6789_real32,tiny(0.0_real32))
!!    call stderr('real64  :',huge(0.0_real64),0.0_real64,12345.6789_real64,tiny(0.0_real64))
!!    call stderr('real128 :',huge(0.0_real128),0.0_real128,12345.6789_real128,tiny(0.0_real128))
!!    call stderr('complex :',cmplx(huge(0.0_real),tiny(0.0_real)))
!!
!!    call stderr('error: program will now stop')
!!    stop 1
!!
!!    end program demo_stderr
!!
!!   Results:
!!     A simple message
!!     error: RVALUE= 0.750000000
!!     error: IVALUE= 123456789
!!     error: LVALUE= T
!!     error: value -10 should be between 10 and 999
!!     real32  : 3.40282347E+38 ...
!!               0.00000000 ...
!!               12345.6787 ...
!!               1.17549435E-38
!!     real64  : 1.7976931348623157E+308 ...
!!               0.0000000000000000 ...
!!               12345.678900000001 ...
!!               2.2250738585072014E-308
!!     real128 : 1.18973149535723176508575932662800702E+4932 ...
!!               0.00000000000000000000000000000000000  ...
!!               12345.6789000000000000000000000000002 ...
!!               3.36210314311209350626267781732175260E-4932
!!     complex : (3.40282347E+38,1.17549435E-38)
!!     error: program will now stop
!!     STOP 1
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine stderr(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
implicit none

! ident_5="@(#)M_msg::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer                      :: ios
   write(error_unit,'(a)',iostat=ios) str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wrt(3f) - [M_msg] write multiple scalar values to any number of files
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine wrt(luns,generic(s),iostat)
!!
!!     integer,intent(in)           :: luns(:)
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!     class(*),intent(in),optional :: generica,genericb,genericc,genericd,generice
!!     class(*),intent(in),optional :: genericf,genericg,generich,generici,genericj
!!     integer,intent(out),optional :: ios
!!##DESCRIPTION
!!    WRT(3f) writes a list of scalar values  to the list of unit numbers in LUNS(:).
!!##OPTIONS
!!    LUNS            Unit numbers to write to. If of size zero no output is generated
!!    generic[1-20]   optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##RETURNS
!!    IOSTAT          The value of the last non-zero IOSTAT value. Returns zero if
!!                    no errors occurred.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wrt
!!    use, intrinsic :: iso_fortran_env, only : &
!!     & stdin=>input_unit, &
!!     & stdout=>output_unit, &
!!     & stderr=>error_unit
!!    use M_msg, only: wrt, fmt
!!    implicit none
!!    integer,allocatable :: luns(:)
!!    integer :: iostat=0
!!    integer,parameter :: ints(3)=[1,2,3]
!!
!!    ! a null list allows for turning off verbose or debug mode output
!!    luns=[integer ::]
!!    call wrt(luns,'NULL LIST:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! multiple files can be used to create a log file, for example
!!    luns=[stderr,stdout]
!!    call wrt(luns,'TWO FILES:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! using fmt
!!    call wrt([stdout,stdout,stdout],'USING FMT :', &
!!     & huge(0),'PI=',asin(1.0d0)*2.0d0,fmt(ints(2),'i0.4'),iostat=iostat)
!!
!!
!!    end program demo_wrt
!!
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!  IOSTAT=           0
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!  IOSTAT=           0
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine wrt(luns,g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj,iostat)
implicit none

! ident_6="@(#)M_msg::write(3f): writes a message to any number of open files with any scalar values"

integer,intent(in)           :: luns(:)
class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer,intent(out),optional :: iostat
integer                      :: i
character(len=256)           :: msg
   do i=1,size(luns)
      write(luns(i),'(a)',iostat=iostat,iomsg=msg)str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
      if(iostat.ne.0)then
         call stderr('<ERROR>*write*:',msg)
      endif
   enddo
end subroutine wrt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
