#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 2
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 2
                          t1 = sm2x2avg_t3(l,k,1)+orient_distr*scat2x2m(l,k)
                          sm2x2avg_t3(l,k,1) = t1
                       end do
                    end do
#else
                    t1 = sm2x2avg_t3(1,1,1)+orient_distr*scat2x2m(1,1)
                    sm2x2avg_t3(1,1,1) = t1
                    t2 = sm2x2avg_t3(1,2,1)+orient_distr*scat2x2m(1,2)
                    sm2x2avg_t3(1,2,1) = t2
                    t3 = sm2x2avg_t3(2,1,1)+orient_distr*scat2x2m(2,1)
                    sm2x2avg_t3(2,1,1) = t3
                    t4 = sm2x2avg_t3(2,2,1)+orient_distr*scat2x2m(2,2)
                    sm2x2avg_t3(2,2,1) = t4
#endif
