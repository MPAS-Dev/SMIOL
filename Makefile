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
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner smiol_runner.c smiol.a 

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
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner smiol_runner.c smiol.a 

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
	$(CC) -I./src/ $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o smiol_runner smiol_runner.c smiol.a 

xi:


test:




clean:
	rm -f smiol.a smiol_runner
	$(MAKE) -C ./src clean 
