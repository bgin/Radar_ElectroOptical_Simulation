### GEMM-Based Level 3 BLAS ############################################

GBLIB =	 ../libgbl3b.a

########################################################################

SGBFLG = -fast

SAXFLG = -fast

AXFLG =	 -fast

########################################################################

AUXS =	 lsame.f xerbla.f

AUX =	 lsame.o xerbla.o

SGBS =	 ssymm.f ssyrk.f ssyr2k.f strmm.f strsm.f

SGB =	 ssymm.o ssyrk.o ssyr2k.o strmm.o strsm.o

SAUXS =	 sbigp.f scld.f

SAUX =	 sbigp.o scld.o

########################################################################

libgbl3b:	$(SGB) $(SAUX) $(AUX)
	ar cr $(GBLIB) $(SGB) $(SAUX) $(AUX)
	ranlib $(GBLIB)

ssgpm:		ssgpm.f getwrd.f eoln.f
	$(FC) -o ssgpm ssgpm.f getwrd.f eoln.f

$(SGB):		$(SGBS)
	$(FC) -c $(SGBFLG) $(SGBS)

$(SAUX):	$(SAUXS)
	$(FC) -c $(SAXFLG) $(SAUXS)

$(AUX):		$(AUXS)
	$(FC) -c $(AXFLG) $(AUXS)
