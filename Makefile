TARGET	=	go
OBJECTS	=	main.o	datain7.o	element.o	amerge.o	stiff7.o	check_stiff.o	check_solution6.o	bound2.o	check_matrix2.o	gauss_ver4u.o	
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
