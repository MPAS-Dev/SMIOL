default:
	@echo "Please provide a compiler name (llvm, gnu, intel, pgi, xi, nag)"
	exit 1

llvm:
	( $(MAKE) smiol \
	 "CC = clang" \
	 "CFLAGS = -g -Weverything" \
	 "FC = flang" \
	 "FFLAGS = -g -Mbounds -Mchkptr -Mstandard" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

gnu:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = gfortran"\
	 "FFLAGS = -g -Wall -fcheck=all -pedantic -std=f2003 -fbacktrace" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

intel:
	( $(MAKE) smiol \
	 "CC = icc" \
	 "CFLAGS = -g -Wall -traceback" \
	 "FC = ifort " \
	 "FFLAGS = -g -warn all -check all -traceback" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

pgi:
	( $(MAKE) smiol \
	 "CC = pgcc" \
	 "CFLAGS = -g -traceback" \
	 "FC = pgfortran" \
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

xl:
	( $(MAKE) smiol \
	 "CC = xlc_r" \
	 "CFLAGS = -g" \
	 "FC = xlf2003_r" \
	 "FFLAGS = -g -C" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

nag:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = nagfor" \
	 "FFLAGS = -f2003 -g -C=all" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )


ifneq "$(PNETCDF)" ""
CPPINCLUDES = -DSMIOL_PNETCDF -I${PNETCDF}/include
LIBS = -L${PNETCDF}/lib -lpnetcdf
endif

smiol:

	$(MAKE) -C ./src CC=$(CC_PARALLEL) FC=$(FC_PARALLEL) CPPINCLUDES="$(CPPINCLUDES)"
	$(CC_PARALLEL) -I./src/ $(CPPINCLUDES) $(CFLAGS) -L./ $(LDFLAGS) -o smiol_runner_c smiol_runner.c -lsmiol $(LIBS)
	$(FC_PARALLEL) -I./src/ $(CPPINCLUDES) $(FFLAGS) -L./ $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 -lsmiolf -lsmiol $(LIBS)


test:




clean:
	$(RM) -f smiol_runner_c smiol_runner_f
	$(MAKE) -C ./src clean 
