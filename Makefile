default:
	@echo "Please provide a compiler name (clang, gnu, intel, pgi, xi, nag)"
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
	 "CFLAGS = -g -Wall" \
	 "CPPINCLUDES = " \
	 "FC = gfortran" \
	 "FFLAGS = -g -Wall -fcheck=all" \
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

xi:

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
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) -L`pwd` $(LDFLAGS) -o smiol_runner_c smiol_runner.c -lsmiol
	$(FC) -I./src/ $(CPPINCLUDES) $(FFLAGS) -L`pwd` $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 -lsmiolf -lsmiol


test:




clean:
	$(RM) -f smiol.a smiolf.a smiol_runner_c smiol_runner_f smiol_runner.o
	$(MAKE) -C ./src clean 
