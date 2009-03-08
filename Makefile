H = ghc
L = -O2 -optl-s

all: libpandoc.so

clean:
	rm -rf *.so

%.so: %.hs %-so.c
	$H $F --make -no-hs-main -optl-shared -o $@ $<