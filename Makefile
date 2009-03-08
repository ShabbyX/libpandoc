H = ghc
L = -O2 -optl-s

all: libpandoc.so
	rm -f *.so *.o *stub* *.hi

clean:
	rm -rf *.so *.o *stub* *.hi

%.so: %.hs %-so.c
	$H $F --make -no-hs-main -optl-shared -o $@ $<