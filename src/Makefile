all: smiol

smiol:
	$(CC) $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -c smiol.c
	ar cr ../smiol.a smiol.o

clean:
	rm -f smiol.o ../smiol.a
