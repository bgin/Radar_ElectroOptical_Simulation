program Tmatrix_Test
  use mod_tmatrix  
  implicit none

    
 


 
 
 
 
 ! Arguments to TMatrix_driver subroutine
 real(R64P) :: RAT,AXMAX,B,GAM,EPS,LAM,MRR,MRI,DDELT
 integer(I32P) :: NDISTR,NPNAX,NKMAX,NP,NPNA,NDGS,i,j
 
 ! Output arguments
 real(R64P) :: CEXT,CSCA,W,COSPH,REFF,VEFF
 real(R64P), dimension(NPL) :: ALPHA1,ALPHA2,ALPHA3,ALPHA4,BETA1,BETA2
 !DIR$ ATTRIBUTES ALIGN : 64 :: ALPHA1
 !DIR$ ATTRIBUTES ALIGN : 64 :: ALPHA2
 !DIR$ ATTRIBUTES ALIGN : 64 :: ALPHA3
 !DIR$ ATTRIBUTES ALIGN : 64 :: ALPHA4
 !DIR$ ATTRIBUTES ALIGN : 64 :: BETA1
 !DIR$ ATTRIBUTES ALIGN : 64 :: BETA2
 real(R64P), dimension(6,19) :: SCATMAT
 !DIR$ ATTRIBUTES ALIGN : 64 :: SCATMAT
 
 ! Initialziation
 RAT = 1._R64P
 NDISTR = 3_I32P
 AXMAX  = 0.3_R64P
 NPNAX  = 1_I32P
 B = 0.1_R64P
 GAM = 0.5_R64P
 NKMAX = 51_I32P
 EPS = 2._R64P
 NP = -1_I32P
 LAM = 0.5_R64P
 MRR = 1.53_R64P
 MRI = 0.008_R64P
 DDELT = 0.001_R64P
 NPNA = 19_I32P
 NDGS = 2_I32P
 
 ! Output variables
 ! Arrays
 ALPHA1 = 0.0_R64P
 ALPHA2 = 0.0_R64P
 ALPHA3 = 0.0_R64P
 ALPHA4 = 0.0_R64P
 BETA1 = 0.0_R64P
 BETA2 = 0.0_R64P
 SCATMAT = 0.0_R64P
 ! Scakalrs
 CEXT = 0._R64P
 CSCA = 0._R64P
 W = 0._R64P
 COSPH = 0._R64P
 REFF = 0._R64P
 VEFF = 0._R64P
 
    call TMATRIX_Driver(RAT,NDISTR,AXMAX,NPNAX,B,GAM,NKMAX,EPS,  &
                        NP,LAM,MRR,MRI,DDELT,NPNA,NDGS,CEXT,CSCA, &
                        W,COSPH,REFF,VEFF,ALPHA1,ALPHA2,ALPHA3,   &
                        ALPHA4,BETA1,BETA2,SCATMAT           )
    
    
    print*, " Dumping output values from the T-MATRIX model "
    print*, CEXT,CSCA,W,COSPH,REFF,VEFF
    print*, SCATMAT
!    write(6,10)  CEXT
!10  format("CEXT=",F10.15)
!    write(6,20)  CSCA
!20  format("CSCA=",F10.15)
!    write(6,30)  W
!30  format("W=",F10.15)
!    write(6,40)  COSPH
!40  format("COSPH=",F10.15)
!    write(6,50)  REFF
!50  format("REFF=",F10.15)
!    write(6,60)  VEFF
!60  format("VEFF=",F10.15)
    
   ! do i = 1, 6
   !     do j = 1, 19
   !         write(6,70) SCATMAT(i,j)
!70          format("Complex Amplitude values: ",F10.15)
!        end do
!    end do
    print*, " End of model dump "
 









end program   Tmatrix_Test