PREFIX = /usr
NAME   = libpandoc
LIB    = $(NAME).so
LIBDIR = $(PREFIX)/lib
H      = ghc
F      = -O2 -optl-s

all: $(LIB)
	rm -f *.o *stub* *.hi

clean:
	rm -rf *.so *.o *stub* *.hi

install: $(LIB)
	cp $(LIB) $(LIBDIR)/
	ldconfig

test:
	mzscheme test/test.ss test/README

%.so: %.hs $(NAME)-so.c
	$H $F --make -no-hs-main -optl-shared -o $@ $< $(NAME)-so.c

.PHONY: test clean