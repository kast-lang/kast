libkast.a:
	cc -I include -lbacktrace -lunwind src/*.c -c
	ar rc libkast.a *.o

install: libkast.a
	echo ${PREFIX}
	exit 1
	install -d $(DESTDIR)$(PREFIX)/lib/
	install -m 644 libkast.a $(DESTDIR)$(PREFIX)/lib/

