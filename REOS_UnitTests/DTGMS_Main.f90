
    
    
    
    
Program DTGMS_Main

      !use mod_test_quaternion
      !use mod_test_avx_bindings
       !use mod_test_sse_matrix
       use mod_test_cuda_header
      !call test_quaternion_ctors()
      !call test_quaternion_to_r3_conversions()
      ! call test_quaternion_to_R4_rotation()
      !call test_quaternion_procedures()
      !call test_alloc_quaternion_arrays()
      !call test_vec4f64_xxx_vec4f64()
      ! call test_M4x4_constructors()
      call test_cuda_header_api() 
End Program DTGMS_Main