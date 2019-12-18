default:
	@echo "Please provide a compiler name (llvm, gnu, intel, pgi, xi, nag)"
	exit 1

llvm:
	( $(MAKE) smiol \
	 "CC = clang" \
	 "CFLAGS = -g -Weverything" \
	 "CPPINCLUDES = " \
	 "FC = flang" \
	 "FFLAGS = -g -Mbounds -Mchkptr -Mstandard" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

gnu:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "CPPINCLUDES = " \
	 "FC = gfortran"\
	 "FFLAGS = -g -Wall -fcheck=all -pedantic -std=f2003" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

intel:
	( $(MAKE) smiol \
	 "CC = icc" \
	 "CFLAGS = -g -Wall" \
	 "CPPINCLUDES = " \
	 "FC = ifort " \
	 "FFLAGS = -g -warn all -check all" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

pgi:
	( $(MAKE) smiol \
	 "CC = pgcc" \
	 "CFLAGS = -g -traceback" \
	 "CPPINCLUDES = " \
	 "FC = pgfortran" \
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )

xl:
	( $(MAKE) smiol \
	 "CC = xlc_r" \
	 "CFLAGS = -g" \
	 "CPPINCLUDES = " \
	 "FC = xlf2003_r" \
	 "FFLAGS = -g -C" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

nag:
	( $(MAKE) smiol \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "CPPINCLUDES = " \
	 "FC = nagfor" \
	 "FFLAGS = -f2003 -g -C=all" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpifort" )

smiol:
	$(MAKE) -C ./src
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) -L./ $(LDFLAGS) -o smiol_runner_c smiol_runner.c -lsmiol
	$(FC) -I./src/ $(CPPINCLUDES) $(FFLAGS) -L./ $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 -lsmiolf -lsmiol


test:




clean:
	$(RM) -f smiol.a smiolf.a smiol_runner_c smiol_runner_f smiol_runner.o
	$(MAKE) -C ./src clean 
