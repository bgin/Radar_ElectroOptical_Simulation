

module platform_errors_kernels_iface

!==========================================================!
!      Interfaces to CUDA platform errors kernels.         !
!==========================================================! 

use, intrinsic :: ISO_C_BINDING
implicit none
public


#if 0
void platform_orient_err_cuda(const float * __restrict__ psi,
                              const float * __restrict__ theta,
                              const float * __restrict__ da1, 
                              const float * __restrict__ da2,
                              const float * __restrict__ da3,
                              float * __restrict__ dpsi1,//ang,min, azimuth measurement error
                              float * __restrict__ dthi1, //ang,min, elevation measurement error
                              const uint32_t n_threads,
                              const uint32_t type,
                              const uint32_t n)

#endif

interface

   subroutine platform_orient_err_cuda(psi,theta,da1,da2,da3, &
                                       dpsi1,dthi1,n_threads,type,n) &
                          bind(c,name='platform_orient_err_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float), dimension(*),       intent(in)   :: psi
              real(c_float), dimension(*),       intent(in)   :: theta
              real(c_float), dimension(*),       intent(in)   :: da1
              real(c_float), dimension(*),       intent(in)   :: da2
              real(c_float), dimension(*),       intent(in)   :: da3
              real(c_float), dimension(*),       intent(out)  :: dpsi1
              real(c_float), dimension(*),       intent(out)  :: dthi1
              integer(c_int),                    intent(in), value :: n_threads
              integer(c_int),                    intent(in), value :: n
              integer(c_int),                    intent(in), value :: type
   end subroutine

end interface


#if 0
void platform_pos_err_cuda(const float * __restrict__ R,
                           const float * __restrict__ psi,
                           const float * __restrict__ theta,
                           const float * __restrict__ dx1,
                           const float * __restrict__ dx2,
                           const float * __restrict__ dx3,
                           float * __restrict__ dpsi2, //min, azimuth error
                           float * __restrict__ dth2,  //min, elevation error
                           float * __restrict__ dR,    //m,   range error
                           const uint32_t n_threads,
                           const uint32_t type,
                           const uint32_t n)

#endif

interface

     subroutine platform_pos_err_cuda(R,psi,theta,dx1,dx2,dx3, &
                                      dpsi2,dth2,dR,n_threads,type,n) &
                          bind(c,name='platform_pos_err_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float), dimension(*),       intent(in)   :: R
              real(c_float), dimension(*),       intent(in)   :: psi
              real(c_float), dimension(*),       intent(in)   :: theta
              real(c_float), dimension(*),       intent(in)   :: dx1
              real(c_float), dimension(*),       intent(in)   :: dx2
              real(c_float), dimension(*),       intent(in)   :: dx3
              real(c_float), dimension(*),       intent(out)  :: dpsi2
              real(c_float), dimension(*),       intent(out)  :: dth2
              real(c_float), dimension(*),       intent(out)  :: dR
              integer(c_int),                    intent(in), value :: n_threads
              integer(c_int),                    intent(in), value :: n
              integer(c_int),                    intent(in), value :: type
   end subroutine
   
end interface

end module platform_errors_kernels_iface
