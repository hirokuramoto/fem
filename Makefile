TARGET	=	go
OBJECTS	=	main.o	stiff.o	ex13.o	ex23.o	check_stiff.o	check_matrix2.o	check_solution.o	gauss_ver4u.o	bound1.o	bound2.o
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
