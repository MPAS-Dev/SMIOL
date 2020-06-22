all: smiol

smiol:
	$(CC) $(CPPINCLUDES) $(CFLAGS) -c smiol_utils.c
	$(CC) $(CPPINCLUDES) $(CFLAGS) -c smiol.c
	./gen_put_get_var_interfaces.sh
	$(FC) $(CPPINCLUDES) $(FFLAGS) -c smiolf.F90
	ar cr ../libsmiol.a smiol.o smiol_utils.o
	ar cr ../libsmiolf.a smiolf.o

clean:
	$(RM) -f smiol.o smiol_utils.o ../libsmiol.a
	$(RM) -f smiolf.o smiolf.mod ../libsmiolf.a
	$(RM) -f smiolf_put_var_interface_headers.inc smiolf_put_var_interfaces.inc
	$(RM) -f smiolf_get_var_interface_headers.inc smiolf_get_var_interfaces.inc
