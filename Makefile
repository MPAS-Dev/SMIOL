default:
	@echo "Please provide a compiler name (llvm, gnu, intel, pgi, xl, nag, cray)"
	exit 1

llvm:
	( $(MAKE) smiol \
	 "CC = clang" \
	 "CFLAGS = -g -Weverything" \
	 "FC = flang" \
	 "FFLAGS = -g -Mbounds -Mchkptr -Mstandard" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

gnu:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = gfortran"\
	 "FFLAGS = -g -Wall -fcheck=all -pedantic -std=f2003 -fbacktrace" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

intel:
	( $(MAKE) smiol \
	 "CC = icc" \
	 "CFLAGS = -g -Wall -traceback" \
	 "FC = ifort " \
	 "FFLAGS = -g -warn all -check all -traceback" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

pgi:
	( $(MAKE) smiol \
	 "CC = pgcc" \
	 "CFLAGS = -g -traceback" \
	 "FC = pgfortran" \
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

xl:
	( $(MAKE) smiol \
	 "CC = xlc_r" \
	 "CFLAGS = -g" \
	 "FC = xlf2003_r" \
	 "FFLAGS = -g -C" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

nag:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = nagfor" \
	 "FFLAGS = -f2003 -g -C=all" \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

cray:
	( $(MAKE) smiol \
	 "CC = cc" \
	 "CFLAGS = -G 0 -h conform -h nomessage=193 -h msglevel_0 -h bounds -h dir_check" \
	 "FC = ftn" \
	 "FFLAGS = -G 0 -m 0 -h dir_check -R bps -O 0 -e nI -N 132" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = cc" \
	 "FC_PARALLEL = ftn" )


ifneq "$(PNETCDF)" ""
CPPINCLUDES = -DSMIOL_PNETCDF -I${PNETCDF}/include
LIBS = -L${PNETCDF}/lib -lpnetcdf
endif

smiol:

	$(MAKE) -C ./src CC=$(CC_PARALLEL) FC=$(FC_PARALLEL) CPPINCLUDES="$(CPPINCLUDES)"
	$(CC_PARALLEL) -I./src/ $(CPPINCLUDES) $(CFLAGS) -L./ -o smiol_runner_c smiol_runner.c -lsmiol $(LIBS)
	$(FC_PARALLEL) -I./src/ $(CPPINCLUDES) $(FFLAGS) -L./ -o smiol_runner_f smiol_runner.F90 -lsmiolf -lsmiol $(LIBS)


test:




clean:
	$(RM) -f smiol_runner_c smiol_runner_f
	$(MAKE) -C ./src clean 
