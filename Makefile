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

pgi:

xi:

test:


clean:
	rm -f HelloWorld HelloWorld_Fortran
	$(MAKE) -C ./src clean
