TARGET	=	go
OBJECTS	=	main.o	element.o	datain5.o	stiff5.o	check_stiff.o	check_solution3.o	bound2.o	check_matrix2.o	gauss_ver4u.o	
F90	=	gfortran
FFLAGS	=	-O	-fdefault-real-8 -fbounds-check
COMMON_MOD	=

.SUFFIXES	:
.SUFFIXES	:	.o	.f90
.f90.o:
	${F90}	-c	$<	${FFLAGS}

${TARGET}	:	${OBJECTS}
	${F90}	-o	$@	${OBJECTS}

${OBJECTS}	:	${COMMON_MOD}
