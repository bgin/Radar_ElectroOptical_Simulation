
module mod_test_cuda_header


     !!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use, intrinsic :: ISO_C_BINDING
    use mod_cuda_header
    use mod_kinds, only : int4
    implicit none
    public
    
    character(len=1),        parameter, private :: dcol = ":"
    integer(c_int),          parameter, private :: intInvalid = -999999
    contains
    
    subroutine test_cuInit()
          character(len=256) :: filename 
          character(len=35), parameter :: header = "[TEST #1: test_cuInit -- START]"
          character(len=32), parameter :: footer = "[TEST #1: test_cuInit -- END]"
          character(len=10)  :: t
          character(len=8)   :: d
          integer(kind=int4) :: lstart,lend
          integer(c_int), parameter :: Flags = 0
          integer(kind(CUDA_SUCCESS)) :: status
          ! Exec code ....
          filename = __FILE__
          lstart   = __LINE__
          call DATE_AND_TIME(DATE=d,TIME=t)
          print*, d , ":" , t , filename , lstart ,  header
          print*, "Calling Fortran-to-CUDA interface -- cuInit"
          print*, "Argument to: cuInit -- Flags:", Flags
          status = cuInit(Flags)
          !print*, "cuInit returned following status code: ", status
          if(status == 0) then
              print*, "TEST PASSED -- status= ", status
          else
              print*, "TEST FAILED -- status= ", status
          end if
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer    
    end subroutine test_cuInit
    
    subroutine test_cuDriverGetVersion()
          character(len=256) :: filename
          character(len=47),   parameter :: header = "[TEST #2: test_cuDriverGetVersion -- START]"
          character(len=44),   parameter :: footer = "[TEST #2: test_cuDriverGetVersion -- END]"
          character(len=10)  :: t
          character(len=8)   :: d
          integer(kind=int4) :: lstart = 0_int4,lend = 0_int4
          integer(c_int)     :: driverVersion = intInvalid
          integer(kind(CUDA_SUCCESS)) :: status
          ! Exec code ....
          filename = __FILE__
          lstart   = __LINE__
          call DATE_AND_TIME(DATE=d,TIME=t)
          print*, d , ":" , t , filename , lstart ,  header
          print*,"Calling Fortran-to-CUDA Interface -- cuDriverGetVersion"
          status = cuDriverGetVersion(driverVersion)
          if(status == 0 .and. driverVersion /= intInvalid) then
              print*,"TEST PASSED -- status=", status, "Driver Version=", driverVersion
          else
              print*,"TEST FAILED -- status=", status, "Driver Version=", driverVersion
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer     
    end subroutine test_cuDriverGetVersion
    
    subroutine test_cuDeviceGetCount()
          character(len=256) :: filename
          character(len=42),   parameter :: header = "[TEST #3: test_cuDeviceGetCount -- START]"
          character(len=40),   parameter :: footer = "[TEST #3: test_cuDeviceGetCount -- END]"
          character(len=10)  :: t = ""
          character(len=8)   :: d = ""
          integer(kind=int4) :: lstart = 0_int4,lend = 0_int4
          integer(c_int)     :: count = intInvalid
          integer(kind(CUDA_SUCCESS)) :: status
          ! Exec code ...
          filename = __FILE__
          lstart   = __LINE__
          call DATE_AND_TIME(DATE=d,TIME=t)
          print*, d , ":" , t , filename , lstart ,  header
          print*,"Calling Fortran-to-CUDA Interface -- cuDeviceGetCount"
          status = cuDeviceGetCount(count)
          if(status == 0 .and. count > 0) then
              print*,"TEST PASSED -- status= ", status, "Count of GPGPU= ", count
          else
              print*,"TEST FAILED -- status= ", status, "Count of GPGPU= ", count
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer  
    end subroutine test_cuDeviceGetCount
    
    subroutine test_cuDeviceGet()
          character(len=256) :: filename
          character(len=37),   parameter :: header = "[TEST #4: test_cuDeviceGet -- START]"
          character(len=35),   parameter :: footer = "[TEST #4: test_cuDeviceGet -- END]"
          character(len=10)  :: t = ""
          character(len=8)   :: d = ""
          integer(kind=int4) :: lstart = 0_int4,lend = 0_int4
          integer(c_int)     :: device = intInvalid
          integer(c_int)     :: ordinal = 0
          integer(kind(CUDA_SUCCESS)) :: status
          ! Exec code ....
          filename = __FILE__
          lstart   = __LINE__
          call DATE_AND_TIME(DATE=d,TIME=t)
          print*, d , ":" , t , filename , lstart ,  header
          print*,"Calling Fortran-to-CUDA Interface -- cuDeviceGet"
          status = cuDeviceGet(device,ordinal)
          if(status == 0 .and. device /= intInvalid) then
                print*,"TEST PASSED -- status= ", status, "GPGPU Handle= ", device
          else
                print*,"TEST FAILED -- status= ", status, "GPGPU Handle= ", device
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer  
    end subroutine test_cuDeviceGet
    
    subroutine test_cuda_header_api()
          call test_cuInit()
          call test_cuDriverGetVersion()
          call test_cuDeviceGetCount()
          call test_cuDeviceGet()
    end subroutine test_cuda_header_api
    
    
end module mod_test_cuda_header