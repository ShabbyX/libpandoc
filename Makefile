PREFIX = /usr
NAME   = pandoc
LIB    = lib$(NAME).so
LIBDIR = $(PREFIX)/lib
INCDIR = $(PREFIX)/include
H      = ghc
F      = -O2 -optl-s

all: $(LIB)
	rm -f *.o *stub* *.hi

clean:
	rm -f *.so *.o *stub* *.hi test/test test/*.o

install: $(LIB)
	cp $(LIB) $(LIBDIR)/
	cp pandoc.h $(INCDIR)/
	ldconfig

test:
	$H -lpandoc test/test.c -o test/test
	test/test

%.so: %.hs lib$(NAME)-so.c
	$H $F --make -no-hs-main -optl-shared -o $@ $< lib$(NAME)-so.c

.PHONY: test clean