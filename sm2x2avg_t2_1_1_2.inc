#if (LEAF_PHASE_MATRIX_AUTOVECTORIZE) == 1

                    do k=1, 2
#if defined __INTEL_COMPILER
                       !DIR$ VECTOR ALWAYS
                       !DIR$ CODE_ALIGN : 64
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                       !GCC$ VECTOR
#endif
                       do l=1, 2
                          t1 = sm2x2avg_t2(l,k,2)+orient_distr*scat2x2m(l,k)
                          sm2x2avg_t2(l,k,2) = t1
                       end do
                    end do
#else
                    t1 = sm2x2avg_t2(1,1,2)+orient_distr*scat2x2m(1,1)
                    sm2x2avg_t2(1,1,2) = t1
                    t2 = sm2x2avg_t2(1,2,2)+orient_distr*scat2x2m(1,2)
                    sm2x2avg_t2(1,2,2) = t2
                    t3 = sm2x2avg_t2(2,1,2)+orient_distr*scat2x2m(2,1)
                    sm2x2avg_t2(2,1,2) = t3
                    t4 = sm2x2avg_t2(2,2,2)+orient_distr*scat2x2m(2,2)
                    sm2x2avg_t2(2,2,2) = t4
#endif
