
module radar_jamming_kernels_iface



!==========================================================!
!      Interfaces to CUDA radar jamming  kernels.          !
!==========================================================! 

use, intrinsic :: ISO_C_BINDING
implicit none
public



#if 0

void therm_noise_range1_cuda( const float Frdr,
                              const float Kth,
                              const float rho,
                              const float tf,
                              const float tr,
	                      const float th,
		              const float * __restrict d_Pt,
		              const float gamm,
			      const float * __restrict d_w,
			      const float * __restrict d_h,
			      const float Ln,
			      const float * __restrict d_Ts,
			      const float sig,
			      const float F,
			      const float Fp,
		              const float Flens,
			      const float Dx,
			      const float * __restrict d_Lt,
			      const float * __restrict d_La,
			      float * __restrict d_Rm,
			      const uint32_t n_threads)

#endif


interface

   subroutine therm_noise_range1_cuda(Frdr,Kth,rho,tf,tr,th,d_Pt, &
                                      gamm,d_w,d_h,Ln,d_Ts,sig,F, &
                                      Fp,Flens,Dx,d_Lt,d_La,d_Rm, &
                                      n_threads)  &
                         bind(c,name='therm_noise_range1_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),               intent(in), value :: Frdr
              real(c_float),               intent(in), value :: Kth
              real(c_float),               intent(in), value :: rho
              real(c_float),               intent(in), value :: tf
              real(c_float),               intent(in), value :: tr
              real(c_float),               intent(in), value :: th
              real(c_float), dimension(*), intent(in)        :: d_Pt
              real(c_float),               intent(in), value :: gamm
              real(c_float), dimension(*), intent(in)        :: d_w
              real(c_float), dimension(*), intent(in)        :: d_h
              real(c_float),               intent(in), value :: Ln
              real(c_float), dimension(*), intent(in)        :: d_Ts
              real(c_float),               intent(in), value :: sig
              real(c_float),               intent(in), value :: F
              real(c_float),               intent(in), value :: Fp
              real(c_float),               intent(in), value :: Flens
              real(c_float),               intent(in), value :: Dx
              real(c_float), dimension(*), intent(in)        :: d_Lt
              real(c_float), dimension(*), intent(in)        :: d_La
              real(c_float), dimension(*), intent(out)       :: d_Rm
              integer(c_int),              intent(in), value :: n_threads
   end subroutine

end interface


#if 0

void therm_noise_range2_cuda( const float Frdr,
                              const float Kth,
                              const float rho,
                              const float tf,
                              const float tr,
	                      const float th,
		              const float * __restrict d_Pt,
		              const float gamm,
			      const float * __restrict d_w,
			      const float * __restrict d_h,
			      const float Ln,
			      const float * __restrict d_Ts,
			      const float sig,
			      const float F,
			      const float Fp,
		              const float Flens,
			      const float Dx,
			      const float * __restrict d_Lt,
			      const float * __restrict d_La,
			      float * __restrict d_Rm,
			      const uint32_t n)

#endif


interface

   subroutine therm_noise_range2_cuda(Frdr,Kth,rho,tf,tr,th,d_Pt, &
                                      gamm,d_w,d_h,Ln,d_Ts,sig,F, &
                                      Fp,Flens,Dx,d_Lt,d_La,d_Rm, &
                                      n)  &
                         bind(c,name='therm_noise_range2_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),               intent(in), value :: Frdr
              real(c_float),               intent(in), value :: Kth
              real(c_float),               intent(in), value :: rho
              real(c_float),               intent(in), value :: tf
              real(c_float),               intent(in), value :: tr
              real(c_float),               intent(in), value :: th
              real(c_float), dimension(*), intent(in)        :: d_Pt
              real(c_float),               intent(in), value :: gamm
              real(c_float), dimension(*), intent(in)        :: d_w
              real(c_float), dimension(*), intent(in)        :: d_h
              real(c_float),               intent(in), value :: Ln
              real(c_float), dimension(*), intent(in)        :: d_Ts
              real(c_float),               intent(in), value :: sig
              real(c_float),               intent(in), value :: F
              real(c_float),               intent(in), value :: Fp
              real(c_float),               intent(in), value :: Flens
              real(c_float),               intent(in), value :: Dx
              real(c_float), dimension(*), intent(in)        :: d_Lt
              real(c_float), dimension(*), intent(in)        :: d_La
              real(c_float), dimension(*), intent(out)       :: d_Rm
              integer(c_int),              intent(in), value :: n
   end subroutine

end interface

#if 0
void jammer_req_temp_cuda(    const float Frdr,
                              const float Kth,
                              const float rho,
                              const float tf,
                              const float tr,
                              const float th,
                              const float * __restrict__ d_Pt,
                              const float gamm,
                              const float * __restrict__ d_w,
                              const float * __restrict__ d_h,
                              const float Ln,
                              const float * __restrict__ d_Ts,
                              const float sig,
                              const float F,
                              const float Fp,
                              const float Flens,
                              const float Dx,
                              const float * __restrict__ d_Lt,
                              const float * __restrict__ d_Rm,
                              const float Rmj,
                              const float La,
                              const float Flen,
                              float * __restrict__ rt,
                              const uint32_t n_threads,
                              const uint32_t type,
                              const uint32_t n

#endif


interface

   subroutine jammer_req_temp_cuda(Frdr,Kth,rho,tf,tr,th,d_Pt, &
                                   gamm,d_w,d_h,Ln,d_Ts,sig,F, &
                                   Fp,Flens,Dx,d_Lt,d_La,d_Rm, &
                                   Rmj,La,Flen,rt,n_threads,type,n)  &
                    bind(c,name='jammer_req_temp_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),               intent(in), value :: Frdr
              real(c_float),               intent(in), value :: Kth
              real(c_float),               intent(in), value :: rho
              real(c_float),               intent(in), value :: tf
              real(c_float),               intent(in), value :: tr
              real(c_float),               intent(in), value :: th
              real(c_float), dimension(*), intent(in)        :: d_Pt
              real(c_float),               intent(in), value :: gamm
              real(c_float), dimension(*), intent(in)        :: d_w
              real(c_float), dimension(*), intent(in)        :: d_h
              real(c_float),               intent(in), value :: Ln
              real(c_float), dimension(*), intent(in)        :: d_Ts
              real(c_float),               intent(in), value :: sig
              real(c_float),               intent(in), value :: F
              real(c_float),               intent(in), value :: Fp
              real(c_float),               intent(in), value :: Flens
              real(c_float),               intent(in), value :: Dx
              real(c_float), dimension(*), intent(in)        :: d_Lt
              real(c_float), dimension(*), intent(in)        :: d_La
              real(c_float), dimension(*), intent(in)        :: d_Rm
              real(c_float),               intent(in), value :: Rmj
              real(c_float),               intent(in), value :: La
              real(c_float),               intent(in), value :: Flen
              real(c_float), dimension(*), intent(out)       :: rt
              integer(c_int),              intent(in), value :: n_threads
              integer(c_int),              intent(in), value :: n
              integer(c_int),              intent(in), value :: type
   end subroutine
   
end interface

#if 0
void
 therm_noise_range_cuda(           const float Frdr,
                                   const float Kth,
                                   const float rho,
                                   const float tf,
                                   const float tr,
				   const float th,
				   const float * __restrict d_Pt,
				   const float gamm,
				   const float * __restrict d_w,
				   const float * __restrict d_h,
				   const float Ln,
				   const float * __restrict d_Ts,
				   const float sig,
				   const float F,
				   const float Fp,
				   const float Flens,
				   const float Dx,
				   const float * __restrict d_Lt,
				   const float * __restrict d_La,
				   float * __restrict d_Rm,
				   const uint32_t n_threads,
                                   const uint32_t type,
                                   const uint32_t n)

#endif

interface

   subroutine therm_noise_range_cuda( Frdr,Kth,rho,tf,tr,th,d_Pt, &
                                      gamm,d_w,d_h,Ln,d_Ts,sig,F, &
                                      Fp,Flens,Dx,d_Lt,d_La,d_Rm, &
                                      n_threads,type,n) &
                          bind(c,name='therm_noise_range_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),               intent(in), value :: Frdr
              real(c_float),               intent(in), value :: Kth
              real(c_float),               intent(in), value :: rho
              real(c_float),               intent(in), value :: tf
              real(c_float),               intent(in), value :: tr
              real(c_float),               intent(in), value :: th
              real(c_float), dimension(*), intent(in)        :: d_Pt
              real(c_float),               intent(in), value :: gamm
              real(c_float), dimension(*), intent(in)        :: d_w
              real(c_float), dimension(*), intent(in)        :: d_h
              real(c_float),               intent(in), value :: Ln
              real(c_float), dimension(*), intent(in)        :: d_Ts
              real(c_float),               intent(in), value :: sig
              real(c_float),               intent(in), value :: F
              real(c_float),               intent(in), value :: Fp
              real(c_float),               intent(in), value :: Flens
              real(c_float),               intent(in), value :: Dx
              real(c_float), dimension(*), intent(in)        :: d_Lt
              real(c_float), dimension(*), intent(in)        :: d_La
              real(c_float), dimension(*), intent(out)       :: d_Rm
              integer(c_int),              intent(in), value :: n_threads
              integer(c_int),              intent(in), value :: n
              integer(c_int),              intent(in), value :: type
   end subroutine

end interface


#if 0
void
 tropo_range_loss_cuda(            const float Frdr,
                                   const float Kth,
                                   const float rho,
                                   const float tf
                                   const float tr,
				   const float th,
				   const float * __restrict d_Pt,
				   const float * __restrict d_Rmj,
				   const float gamm,
				   const float * __restrict d_w,
				   const float * __restrict d_h,
				   const float Ln,
				   const float * __restrict d_Ts,
				   const float sig,
				   const float F,
				   const float Fp,
				   const float Flens,
				   const float Dx,
				   const float * __restrict d_Lt,
				   const float * __restrict d_La,
				   float * __restrict d_Rm,
				   float * __restrict d_La1,
				   const uint32_t n_threads,
                                   const uint32_t type,
                                   const uint32_t n)

#endif


interface

   subroutine tropo_range_loss_cuda(Frdr,Kth,rho,tf,tr,th,d_Pt,  &
                                    d_Rmj,gamm,d_w,d_h,Ln,d_Ts,  &
                                    sig,F,Fp,Flens,Dx,d_Lt,d_La, &
                                    d_Rm,d_La1,n_threads,type,n) &
                         bind(c,name='tropo_range_loss_cuda')
              use, intrinsic :: ISO_C_BINDING
              real(c_float),               intent(in), value :: Frdr
              real(c_float),               intent(in), value :: Kth
              real(c_float),               intent(in), value :: rho
              real(c_float),               intent(in), value :: tf
              real(c_float),               intent(in), value :: tr
              real(c_float),               intent(in), value :: th
              real(c_float), dimension(*), intent(in)        :: d_Pt
              real(c_float), dimension(*), intent(in)        :: d_Rmj
              real(c_float),               intent(in), value :: gamm
              real(c_float), dimension(*), intent(in)        :: d_w
              real(c_float), dimension(*), intent(in)        :: d_h
              real(c_float),               intent(in), value :: Ln
              real(c_float), dimension(*), intent(in)        :: d_Ts
              real(c_float),               intent(in), value :: sig
              real(c_float),               intent(in), value :: F
              real(c_float),               intent(in), value :: Fp
              real(c_float),               intent(in), value :: Flens
              real(c_float),               intent(in), value :: Dx
              real(c_float), dimension(*), intent(in)        :: d_Lt
              real(c_float), dimension(*), intent(in)        :: d_La
              real(c_float), dimension(*), intent(out)       :: d_Rm
              real(c_float), dimension(*), intent(in)        :: d_La1
              integer(c_int),              intent(in), value :: n_threads
              integer(c_int),              intent(in), value :: n
              integer(c_int),              intent(in), value :: type
   end subroutine

end interface

#if 0
void jammer_erp_cuda(const float * __restrict__ Pj,
                     const float Gj,
                     const float Ltj,
                     float * __restrict__ erp,
                     const uint32_t n_threads,
                     const uint32_t type,
                     const uint32_t n) 

#endif

interface

   subroutine jammer_erp_cuda(Pj,Gj,Ltj,erp,n_threads,type,n) &
                              bind(c,name='jammer_erp_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),  intent(in)        :: Pj
               real(c_float),                intent(in), value :: Gj
               real(c_float),                intent(in), value :: Ltj
               real(c_float), dimension(*),  intent(out)       :: erp
               integer(c_int),               intent(in), value :: n_threads
               integer(c_int),               intent(in), value :: n
               integer(c_int),               intent(in), value :: type
   end subroutine

end interface

#if 0
void jammer_ernp_cuda(const float * __restrict__ Pj,
                      const float Qj,
                      const float Gj,
                      const float Fpj,
                      const float Ltj,
                      float * __restrict__ ernp,
                      const uint32_t n_threads,
                      const uint32_t type,
                      const uint32_t n)

#endif

interface

   subroutine jammer_ernp_cuda(Pj,Qj,Gj,Fpj,Ltj,ernp,     &
                               n_threads,type,n)  &
                    bind(c,name='jammer_ernp_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),  intent(in)        :: Pj
               real(c_float),                intent(in), value :: Qj
               real(c_float),                intent(in), value :: Gj
               real(c_float),                intent(in), value :: Fpj
               real(c_float),                intent(in), value :: Ltj
               real(c_float), dimension(*),  intent(out)       :: ernp
               integer(c_int),               intent(in), value :: n_threads
               integer(c_int),               intent(in), value :: n
               integer(c_int),               intent(in), value :: type
   end subroutine
   
end interface


#if 0
void jammer_spectr_dens_cuda(   const float * __restrict__ gamma,
                                const float Kth,
                                const float Ln,
                                const float h,
                                const float w,
                                const float Qj,
                                const float * __restrict__ Pj,
                                const float Gj,
                                const float Fpj,
                                const float Flnsj,
                                const float Fj,
                                const float Rj,
                                const float Bj,
                                const float Ltj,
                                const float Laj,
                                float * __restrict__ sd,
                                const uint32_t n_threads,
                                const uint32_t type,
                                const uint32_t n)

#endif

interface

   subroutine jammer_spectr_dens_cuda(gamma,Kth,Ln,h,w,Qj,Pj,Gj, &
                                      Fpj,Flnsj,Fj,Rj,Bj,Ltj,Laj, &
                                      sd,n_threads,type,n) &
                        bind(c,name='jammer_spectr_dens_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),   intent(in)        :: gamma
               real(c_float),                 intent(in), value :: Kth
               real(c_float),                 intent(in), value :: Ln
               real(c_float),                 intent(in), value :: h
               real(c_float),                 intent(in), value :: w
               real(c_float),                 intent(in), value :: Qj
               real(c_float), dimension(*),   intent(in)        :: Pj
               real(c_float),                 intent(in), value :: Gj
               real(c_float),                 intent(in), value :: Fpj
               real(c_float),                 intent(in), value :: Flnsj
               real(c_float),                 intent(in), value :: Fj
               real(c_float),                 intent(in), value :: Rj
               real(c_float),                 intent(in), value :: Bj
               real(c_float),                 intent(in), value :: Ltj
               real(c_float),                 intent(in), value :: Laj
               real(c_float), dimension(*),   intent(out)       :: sd
               integer(c_int),               intent(in), value :: n_threads
               integer(c_int),               intent(in), value :: n
               integer(c_int),               intent(in), value :: type
   end subroutine

end interface


#if 0
void single_jammer_temp_cuda(   const float * __restrict__ gamma,
                                const float Kth,
                                const float Ln,
                                const float h,
                                const float w,
                                const float Qj,
                                const float * __restrict__ Pj,
                                const float Gj,
                                const float Fpj,
                                const float Flnsj,
                                const float Fj,
                                const float Rj,
                                const float Bj,
                                const float Ltj,
                                const float Laj,
                                float * __restrict__ sd,
                                const uint32_t n_threads,
                                const uint32_t type,
                                const uint32_t n)

#endif

interface

   subroutine single_jammer_temp_cuda(gamma,Kth,Ln,h,w,Qj,Pj,Gj, &
                                      Fpj,Flnsj,Fj,Rj,Bj,Ltj,Laj, &
                                      sd,n_threads,type,n) &
                        bind(c,name='single_jammer_temp_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float), dimension(*),   intent(in)        :: gamma
               real(c_float),                 intent(in), value :: Kth
               real(c_float),                 intent(in), value :: Ln
               real(c_float),                 intent(in), value :: h
               real(c_float),                 intent(in), value :: w
               real(c_float),                 intent(in), value :: Qj
               real(c_float), dimension(*),   intent(in)        :: Pj
               real(c_float),                 intent(in), value :: Gj
               real(c_float),                 intent(in), value :: Fpj
               real(c_float),                 intent(in), value :: Flnsj
               real(c_float),                 intent(in), value :: Fj
               real(c_float),                 intent(in), value :: Rj
               real(c_float),                 intent(in), value :: Bj
               real(c_float),                 intent(in), value :: Ltj
               real(c_float),                 intent(in), value :: Laj
               real(c_float), dimension(*),   intent(out)       :: sd
               integer(c_int),                intent(in), value :: n_threads
               integer(c_int),                intent(in), value :: n
               integer(c_int),                intent(in), value :: type
   end subroutine

end interface

#if 0
void n_jammers_range_cuda(   const float Kth,
                             const float gamma,
                             const float * __restrict__ h,
                             const float * __restrict__ w,
                             const float Ln,
                             const float rho,
                             const float tf,
                             const float tr,
                             const float * __restrict__ Pt,
                             const float Frdr,
                             const float Fp,
                             const float F,
                             const float Flen,
                             const float * __restrict__ sig,
                             const float Ts,
                             const float Dx,
                             const float Lt,
                             const float La,
                             float * __restrict Rnj,
                             const uint32_t n_threads,
                             const uint32_t type,
                             const uint32_t n)
#endif

interface

   subroutine n_jammers_range_cuda(Kth,gamma,h,w,Ln,rho,tf,tr,Pt,Frdr, &
                                   Fp,F,Flen,sig,Ts,Dx,Lt,La,Rnj,      &
                                   n_threads,type,n) &
                      bind(c,name='n_jammers_range_cuda')
               use, intrinsic :: ISO_C_BINDING
               real(c_float),                 intent(in), value :: Kth
               real(c_float),                 intent(in), value :: gamma
               real(c_float), dimension(*),   intent(in)        :: h
               real(c_float), dimension(*),   intent(in)        :: w
               real(c_float),                 intent(in), value :: Ln
               real(c_float),                 intent(in), value :: rho
               real(c_float),                 intent(in), value :: tf
               real(c_float),                 intent(in), value :: tr
               real(c_float), dimension(*),   intent(in)        :: Pt
               real(c_float),                 intent(in), value :: Frdr
               real(c_float),                 intent(in), value :: Fp
               real(c_float),                 intent(in), value :: F
               real(c_float),                 intent(in), value :: Flen
               real(c_float), dimension(*),   intent(in)        :: sig
               real(c_float),                 intent(in), value :: Ts
               real(c_float),                 intent(in), value :: Dx
               real(c_float),                 intent(in), value :: Lt
               real(c_float),                 intent(in), value :: La
               real(c_float), dimension(*),   intent(out)        :: Rnj
               integer(c_int),                intent(in), value :: n_threads
               integer(c_int),                intent(in), value :: n
               integer(c_int),                intent(in), value :: type
   end subroutine
                                
end interface

end module radar_jamming_kernels_iface
