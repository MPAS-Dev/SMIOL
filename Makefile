default:
	@echo "Please provide a compiler name (clang, gnu, intel, pgi, xi)"
	exit 1

clang:

gnu:
	( $(MAKE) -C ./src \
	 "CC = gcc" \
	 "CFLAGS = -g -Wall" \
	 "CPPINCLUDES = " \
	 "FC = gfortran"\
	 "FFLAGS = -g -Wall -fcheck=all" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner_c smiol_runner.c smiol.a
	$(FC) -I./src/ $(CPPINCLUDES) $(FFLAGS) $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 smiolf.a smiol.a

intel:
	( $(MAKE) -C ./src \
	 "CC = icc" \
	 "CFLAGS = -g -Wall" \
	 "CPPINCLUDES = " \
	 "FC = ifort "\
	 "FFLAGS = -g -warn all -check all" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner_c smiol_runner.c smiol.a
	$(FC) -I./src/ $(CPPINCLUDES) $(FFLAGS) $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 smiolf.a smiol.a

pgi:
	( $(MAKE) -C ./src \
	 "CC = pgcc" \
	 "CFLAGS = -g -traceback" \
	 "CPPINCLUDES = " \
	 "FC = pgfortran"\
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" \
	 "FCINCLUDES = " \
	 "CC_PARALLEL = mpicc" \
	 "FC_PARALLEL = mpif90" )
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner_c smiol_runner.c smiol.a
	$(FC) -I./src/ $(CPPINCLUDES) $(FFLAGS) $(LDFLAGS) -o smiol_runner_f smiol_runner.F90 smiolf.a smiol.a

xi:


test:




clean:
	$(RM) -f smiol.a smiolf.a smiol_runner_c smiol_runner_f smiol_runner.o
	$(MAKE) -C ./src clean 
