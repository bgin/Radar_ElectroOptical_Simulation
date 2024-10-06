### GEMM-Based Level 3 BLAS ############################################

GBLIB =	 ../libgbl3b.a

########################################################################

CGBFLG =

CAXFLG =

AXFLG =

########################################################################

CGBS =	 csymm.f chemm.f csyrk.f cherk.f csyr2k.f cher2k.f \
	 ctrmm.f ctrsm.f

CGB =    csymm.o chemm.o csyrk.o cherk.o csyr2k.o cher2k.o \
	 ctrmm.o ctrsm.o

CAUXS =  cbigp.f ccld.f

CAUX =   cbigp.o ccld.o

AUXS =	 lsame.f xerbla.f

AUX =	 lsame.o xerbla.o

########################################################################

libgbl3b:	$(CGB) $(CAUX) $(AUX)
	ar ur $(GBLIB) $(CGB) $(CAUX) $(AUX)
	ranlib $(GBLIB)

csgpm:		csgpm.f getwrd.f eoln.f
	$(FC) -o csgpm csgpm.f getwrd.f eoln.f

$(CGB):		$(CGBS)
	$(FC) -c $(CGBFLG) $(CGBS)

$(CAUX):        $(CAUXS)
	$(FC) -c $(CAXFLG) $(CAUXS)

$(AUX):		$(AUXS)
	$(FC) -c $(AXFLG) $(AUXS)
