

module rcs_kernels_iface


!==================================================!
! Interfaces to cuda radar cross section kernels   !
!==================================================!


use, intrinsic :: ISO_C_BINDING
implicit none
public


#if 0
extern "C"
void empirical_K_cuda(const float * __restrict__,
                      const float * __restrict__,
                      const float,
                      float * __restrict__,
                      const uint32_t,
                      const uint32_t,
                      const uint32_t);

#endif

interface

   subroutine empirical_K_cuda(rcs,A,gam,K, &
                               n_threads,n,type) &
                               bind(c,name='empirical_K_cuda')
            use, intrinsic :: ISO_C_BINDING
            real(c_float),  dimension(*), intent(in)        :: rcs
            real(c_float),  dimension(*), intent(in)        :: A
            real(c_float),                intent(in), value :: gam
            real(c_float),  dimension(*), intent(out)       :: K
            integer(c_int),               intent(in), value :: n_threads
            integer(c_int),               intent(in), value :: n
            integer(c_int),               intent(in), value :: type
   end subroutine

end interface


#if 0
void effective_rcs_cuda(const float gamma
                        const float * __restrict__ R, 
                        const float h_a, 
                        const float h_t, 
                        float * __restrict__ rcs_eff,
                        float * __restrict__ rcs_eff_db,
                        const uint32_t n_threads,
                        const uint32_t n,
                        const uint32_t type) 

#endif

interface

   subroutine effective_rcs_cuda(gamma,R,h_a,h_t,  &
                                 rcs_eff,rcs_eff_db, &
                                 n_threads,n,type) &
                                 bind(c,name='effective_rcs_cuda')
           use, intrinsic :: ISO_C_BINDING
           real(c_float),                  intent(in), value :: gamma
           real(c_float), dimension(*),    intent(in)        :: R
           real(c_float),                  intent(in), value :: h_a
           real(c_float),                  intent(in), value :: h_t
           real(c_float), dimension(*),    intent(out)       :: rcs_eff
           real(c_float), dimension(*),    intent(out)       :: rcs_eff_db
           integer(c_int),                 intent(in), value :: n_threads
           integer(c_int),                 intent(in), value :: n
           integer(c_int),                 intent(in), value :: type
   end subroutine

end interface


#if 0
void bistatic_target_rcs_cuda(const float sig0, 
		              const float K, 
	                      const float * __restrict__ Beta,
		              const float * __restrict__ R1,
			      const float * __restrict__ R2,
			      const float B,   
			      const int32_t type, 
			      float * __restrict sigma,               
                              float * __restrict sigma_db,
                              const uint32_t n_threads,
                              const uint32_t kernel_type,
                              const uint32_t n)

#endif

interface

   subroutine bistatic_target_rcs_cuda(sig0,K,Beta,R1,R2, &
                                       B,type,sigma,sigma_db, &
                                       n_threads,kernel_type,n) &
                                       bind(c,name='bistatic_target_rcs_cuda')
            use, intrinsic :: ISO_C_BINDING
            real(c_float),                   intent(in), value :: sig0
            real(c_float),                   intent(in), value :: K
            real(c_float), dimension(*),     intent(in)        :: Beta
            real(c_float), dimension(*),     intent(in)        :: R1
            real(c_float), dimension(*),     intent(in)        :: R2
            real(c_float),                   intent(in), value :: B
            integer(c_int),                  intent(in), value :: type
            real(c_float), dimension(*),     intent(out)       :: sigma
            real(c_float), dimension(*),     intent(out)       :: sigma_db
            integer(c_int),                  intent(in), value :: n_threads
            integer(c_int),                  intent(in), value :: kernel_type
            integer(c_int),                  intent(in), value :: n
   end subroutine 
   
end interface

#if 0
void antenna_rcs_cuda(   const float * __restrict__ Ae,
                         const float gamma,            
                         const float G,                
                         float * __restrict sigma,   
                         float * __restrict__ sigma_db,   
                         const uint32_t n_threads,
                         const uint32_t type,
                         const uint32_t n)

#endif

interface

   subroutine antenna_rcs_cuda(Ae,gamma,G,sigma,  &
                               sigma_db,n_threads, &
                               type,n) &
                               bind(c,name='antenna_rcs_cuda')
           use, intrinsic :: ISO_C_BINDING
           real(c_float), dimension(*),      intent(in)        :: Ae
           real(c_float),                    intent(in), value :: gamma
           real(c_float),                    intent(in), value :: G
           real(c_float), dimension(*),      intent(out)       :: sigma
           real(c_float), dimension(*),      intent(out)       :: sigma_db
           integer(c_int),                   intent(in), value :: n_threads
           integer(c_int),                   intent(in), value :: kernel_type
           integer(c_int),                   intent(in), value :: n 
   end subroutine
                            
end interface


#if 0
void bird_insect_rcs_cuda(const float * __restrict__ W,
                          float * __restrict__ rcs,
                          const uint32_t n_threads)

#endif

interface

   subroutine bird_insect_rcs_cuda(W,rcs,n_threads) &
                           bind(c,name='bird_insect_rcs_cuda')
            use, intrinsic :: ISO_C_BINDING
            real(c_float), dimension(*),     intent(in)        :: W
            real(c_float), dimension(*),     intent(out)       :: rcs
            integer(c_int),                  intent(in), value :: n_threads
   end subroutine
   
end interface

#if 0
void cone_ogive_rcs_cuda(const float gamma, 
                         const float * __restrict__ cha,
                         float * __restrict__ sig1,
                         const uint32_t n_threads,
                         const uint32_t type,
                         const uint32_t n)

#endif

interface

   subroutine cone_ogive_rcs_cuda(gamma,cha,sig1,  &
                                  n_threads,type,n) &
                            bind(c,name='cone_ogive_rcs_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),                  intent(in), value :: gamma
              real(c_float), dimension(*),    intent(in)        :: cha
              real(c_float), dimension(*),    intent(out)       :: sig1
              integer(c_int),                 intent(in), value :: n_threads
              integer(c_int),                 intent(in), value :: type
              integer(c_int),                 intent(in), value :: n 
   end subroutine
                            
end interface

#if 0
void cylinder_rcs_cuda(const float * __restrict__ Rcyl,
                       const float * __restrict__ Lcyl,
                       const float gamma,              
                       const float theta,            
                       float * __restrict__ sig,
                       const uint32_t n_threads,
                       const uint32_t type,
                       const uint32_t n) 

#endif
                       
interface

   subroutine cylinder_rcs_cuda(Rcyl,Lcyl,gamma,theta, &
                                sig,n_threads,type,n) &
                           bind(c,name='cylinder_rcs_cuda')
                use, intrinsic :: ISO_C_BINDING
                real(c_float), dimension(*),    intent(in)        :: Rcyl
                real(c_float), dimension(*),    intent(in)        :: Lcyl
                real(c_float),                  intent(in), value :: gamma
                real(c_float),                  intent(in), value :: theta
                real(c_float), dimension(*),    intent(out)       :: sig
                integer(c_int),                 intent(in), value :: n_threads
                integer(c_int),                 intent(in), value :: type
                integer(c_int),                 intent(in), value :: n 
   end subroutine
   
end interface

#if 0
void disk_rcs_cuda(const float * __restrict__ Rd, 
                   const float * __restrict__ theta, 
                   const float gamma,            
                   const int32_t type, 
                   float * __restrict__ sig,
                   const uint32_t n_threads,
                   const uint32_t kernel_type,
                   const uint32_t n)

#endif

interface

   subroutine disk_rcs_cuda(Rd,theta,gamma,type, &
                            sig,n_threads,kernel_type,n) &
                       bind(c,name='disk_rcs_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float), dimension(*),    intent(in)        :: Rd
              real(c_float), dimension(*),    intent(in)        :: theta
              real(c_float),                  intent(in), value :: gamma
              integer(c_int),                 intent(in), value :: type
              real(c_float), dimension(*),    intent(out)       :: sig
              integer(c_int),                 intent(in), value :: n_threads
              integer(c_int),                 intent(in), value :: kernel_type
              integer(c_int),                 intent(in), value :: n 
   end subroutine

end interface


#if 0
void curved_edge_rcs_cuda(const float * __restrict__ Redg,
		          const float gamma,
                          float * __restrict__ rcs,
                          const uint32_t n_threads)

#endif

interface

   subroutine curved_edge_rcs_cuda(Redg,gamma,rcs,n_threads) &
                              bind(c,name='curved_edge_rcs_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float), dimension(*),    intent(in)        :: Redg
              real(c_float),                  intent(in), value :: gamma
              real(c_float), dimension(*),    intent(out)       :: rcs
              integer(c_int),                 intent(in), value :: n_threads
   end subroutine 
   
end interface

#if 0
void plate_rcs_cuda(   const float * __restrict__ x, 
                       const float * __restrict__ y, 
                       const float theta,           
                       const float phi,             
	               const float gamma,          
                       float * __restrict__ sig,
                       const uint32_t n_threads,
                       const uint32_t type,
                       const uint32_t n)
#endif

interface

   subroutine plate_rcs_cuda(x,y,theta,phi,gamma, &
                             sig,n_threads,type,n) &
                         bind(c,name='plate_rcs_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),    intent(in)        :: x
               real(c_float), dimension(*),    intent(in)        :: y
               real(c_float),                  intent(in), value :: theta
               real(c_float),                  intent(in), value :: phi
               real(c_float),                  intent(in), value :: gamma
               real(c_float), dimension(*),    intent(out)       :: sig
               integer(c_int),                 intent(in), value :: n_threads
               integer(c_int),                 intent(in), value :: type
               integer(c_int),                 intent(in), value :: n 
   end subroutine
   
end interface



end module rcs_kernels_iface
