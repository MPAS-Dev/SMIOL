all: smiol

smiol:
	$(CC) $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -c smiol.c
	$(FC) $(CPPINCLUDES) $(FFLAGS) $(LDFLAGS) -c smiolf.F90
	ar cr ../smiol.a smiol.o
	ar cr ../smiolf.a smiolf.o

clean:
	$(RM) -f smiol.o ../smiol.a
	$(RM) -f smiof.o smiolf.mod ../smiolf.a
