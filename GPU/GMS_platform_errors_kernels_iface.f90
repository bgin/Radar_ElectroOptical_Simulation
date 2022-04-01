

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

#if 0
void elev_mpath_err_cuda(   const float * __restrict__ R,
                            const float * __restrict__ ha,
                            const float * __restrict__ ht,
                            const float psi0,
                            const float psiv,
                            const float psis,
                            const float * __restrict__ th3,
                            const float * __restrict__ thmax,
                            const float * __restrict__ kme,
                            float * __restrict__ Sigem, // deg, elevation multipath error
                            const uint32_t n_threads,
                            const uint32_t type,
                            const uint32_t n)
#endif

interface

   subroutine elev_mpath_err_cuda(R,ha,ht,psi0,psiv,psis, &
                                  th3,thmax,kme,Sigem,    &
                                  n_threads,type,n) &
               bind(c,name='elev_mpath_err_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),       intent(in)        :: R
               real(c_float), dimension(*),       intent(in)        :: ha
               real(c_float), dimension(*),       intent(in)        :: ht
               real(c_float),                     intent(in), value :: psi0
               real(c_float),                     intent(in), value :: psiv
               real(c_float),                     intent(in), value :: psis
               real(c_float), dimension(*),       intent(in)        :: th3
               real(c_float), dimension(*),       intent(in)        :: thmax
               real(c_float), dimension(*),       intent(in)        :: kme
               real(c_float), dimension(*),       intent(out)       :: Sigem
               integer(c_int),                    intent(in), value :: n_threads
               integer(c_int),                    intent(in), value :: n
               integer(c_int),                    intent(in), value :: type
   end subroutine

end interface

#if 0
void elev_refract_err_cuda(     const float * __restrict__ R,
                                const float * __restrict__ ha,
                                const float * __restrict__ ht,
                                const float psi0,
                                const float psiv,
                                const float psis,
                                const float * __restrict__ th3,
                                const float * __restrict__ thmax,
                                const float * __restrict__ kme, 
                                const float Ns,
                                const int32_t flucts,
                                float * __restrict__ Siger, //deg, rms error of elevation measurement
                                const uint32_t n_threads,
                                const uint32_t type,
                                const uint32_t n)

#endif

interface

   subroutine elev_refract_err_cuda(R,ha,ht,psi0,psiv,psis, &
                                    th3,thmax,kme,Ns,flucts,&
                                    Siger,n_threads,type,n) &
               bind(c,name='elev_refract_err_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),       intent(in)        :: R
               real(c_float), dimension(*),       intent(in)        :: ha
               real(c_float), dimension(*),       intent(in)        :: ht
               real(c_float),                     intent(in), value :: psi0
               real(c_float),                     intent(in), value :: psiv
               real(c_float),                     intent(in), value :: psis
               real(c_float), dimension(*),       intent(in)        :: th3
               real(c_float), dimension(*),       intent(in)        :: thmax
               real(c_float), dimension(*),       intent(in)        :: kme
               real(c_float),                     intent(in), value :: Ns
               integer(c_int),                    intent(in), value :: flucts
               real(c_float), dimension(*),       intent(out)       :: Siger
               integer(c_int),                    intent(in), value :: n_threads
               integer(c_int),                    intent(in), value :: n
               integer(c_int),                    intent(in), value :: type
   end subroutine

end interface



end module platform_errors_kernels_iface
