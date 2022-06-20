! unix_syslog.F90
module unix_syslog
    use, intrinsic :: iso_c_binding
    implicit none
    private

    integer(kind=c_int), parameter, public :: LOG_EMERG   = 0 ! system is unusable
    integer(kind=c_int), parameter, public :: LOG_ALERT   = 1 ! action must be taken immediately
    integer(kind=c_int), parameter, public :: LOG_CRIT    = 2 ! critical conditions
    integer(kind=c_int), parameter, public :: LOG_ERR     = 3 ! error conditions
    integer(kind=c_int), parameter, public :: LOG_WARNING = 4 ! warning conditions
    integer(kind=c_int), parameter, public :: LOG_NOTICE  = 5 ! normal but significant condition
    integer(kind=c_int), parameter, public :: LOG_INFO    = 6 ! informational
    integer(kind=c_int), parameter, public :: LOG_DEBUG   = 7 ! debug-level messages

    integer(kind=c_int), parameter, public :: LOG_KERN     = shiftl( 0, 3) ! kernel messages
    integer(kind=c_int), parameter, public :: LOG_USER     = shiftl( 1, 3) ! random user-level messages
    integer(kind=c_int), parameter, public :: LOG_MAIL     = shiftl( 2, 3) ! mail system
    integer(kind=c_int), parameter, public :: LOG_DAEMON   = shiftl( 3, 3) ! system daemons
    integer(kind=c_int), parameter, public :: LOG_AUTH     = shiftl( 4, 3) ! security/authorization messages
    integer(kind=c_int), parameter, public :: LOG_SYSLOG   = shiftl( 5, 3) ! messages generated internally by syslogd
    integer(kind=c_int), parameter, public :: LOG_LPR      = shiftl( 6, 3) ! line printer subsystem
    integer(kind=c_int), parameter, public :: LOG_NEWS     = shiftl( 7, 3) ! network news subsystem
    integer(kind=c_int), parameter, public :: LOG_UUCP     = shiftl( 8, 3) ! UUCP subsystem
    integer(kind=c_int), parameter, public :: LOG_CRON     = shiftl( 9, 3) ! clock daemon
    integer(kind=c_int), parameter, public :: LOG_AUTHPRIV = shiftl(10, 3) ! security/authorization messages (private)
    integer(kind=c_int), parameter, public :: LOG_FTP      = shiftl(11, 3) ! ftp daemon

#if defined (__FreeBSD__)

    integer(kind=c_int), parameter, public :: LOG_NTP      = shiftl(12, 3) ! NTP subsystem
    integer(kind=c_int), parameter, public :: LOG_SECURITY = shiftl(13, 3) ! security subsystems (firewalling, etc.)
    integer(kind=c_int), parameter, public :: LOG_CONSOLE  = shiftl(14, 3) ! /dev/console output

#endif

    integer(kind=c_int), parameter, public :: LOG_PID    = int(z'01') ! log the pid with each message
    integer(kind=c_int), parameter, public :: LOG_CONS   = int(z'02') ! log on the console if errors in sending
    integer(kind=c_int), parameter, public :: LOG_ODELAY = int(z'04') ! delay open until first syslog() (default)
    integer(kind=c_int), parameter, public :: LOG_NDELAY = int(z'08') ! don't delay open
    integer(kind=c_int), parameter, public :: LOG_NOWAIT = int(z'10') ! don't wait for console forks: DEPRECATED
    integer(kind=c_int), parameter, public :: LOG_PERROR = int(z'20') ! log to stderr as well

    public :: c_closelog
    public :: c_openlog
    public :: c_syslog

    interface
       ! void closelog(void)
       subroutine c_closelog() bind(c, name='closelog')
       end subroutine c_closelog

       ! void openlog(const char *ident, int option, int facility)
       subroutine c_openlog(ident, option, facility) bind(c, name='openlog')
           import :: c_char, c_int
           implicit none
           character(kind=c_char), intent(in)        :: ident
           integer(kind=c_int),    intent(in), value :: option
           integer(kind=c_int),    intent(in), value :: facility
       end subroutine c_openlog

       ! void syslog(int priority, const char *format, ...)
       subroutine c_syslog(priority, format) bind(c, name='syslog')
           import :: c_char, c_int
           implicit none
           integer(kind=c_int),    intent(in), value :: priority
           character(kind=c_char), intent(in)        :: format
       end subroutine c_syslog
    end interface
end module unix_syslog
