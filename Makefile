TARGET	=	go
OBJECTS	=	main.o	stiff2.o	datain3.o	check_stiff.o	bc2.o	check_matrix2.o	check_solution3.o	gauss_ver4u.o	bound2.o
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
