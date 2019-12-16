all: smiol

smiol:
	$(CC) $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -c smiol.c
	$(FC) $(CPPINCLUDES) $(FFLAGS) $(LDFLAGS) -c smiolf.F90
	ar cr ../libsmiol.a smiol.o
	ar cr ../libsmiolf.a smiolf.o

clean:
	$(RM) -f smiol.o ../libsmiol.a
	$(RM) -f smiof.o smiolf.mod ../libsmiolf.a
