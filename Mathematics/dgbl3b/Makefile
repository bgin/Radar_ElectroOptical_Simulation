### GEMM-Based Level 3 BLAS ############################################

GBLIB =	 ../libgbl3b.a

########################################################################

DGBFLG = -fast

DAXFLG = -fast

AXFLG =	 -fast

########################################################################

AUXS =	 lsame.f xerbla.f

AUX =	 lsame.o xerbla.o

DGBS =	 dsymm.f dsyrk.f dsyr2k.f dtrmm.f dtrsm.f

DGB =	 dsymm.o dsyrk.o dsyr2k.o dtrmm.o dtrsm.o

DAUXS =	 dbigp.f dcld.f

DAUX =	 dbigp.o dcld.o

########################################################################

libgbl3b:	$(DGB) $(DAUX) $(AUX)
	ar cr $(GBLIB) $(DGB) $(DAUX) $(AUX)
	ranlib $(GBLIB)

dsgpm:		dsgpm.f getwrd.f eoln.f
	$(FC) -o dsgpm dsgpm.f getwrd.f eoln.f

$(DGB):		$(DGBS)
	$(FC) -c $(DGBFLG) $(DGBS)

$(DAUX):	$(DAUXS)
	$(FC) -c $(DAXFLG) $(DAUXS)

$(AUX):		$(AUXS)
	$(FC) -c $(AXFLG) $(AUXS)
