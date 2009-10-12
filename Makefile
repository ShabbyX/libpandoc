Lib = LibPandoc
lib = pandoc

all: $(lib).so

$(lib).so: $(lib).o $(Lib).o
	ghc --make -no-hs-main -optl-shared -o $@ $(lib).c $(Lib).hs

$(lib).o: $(Lib).o

clean:
	rm -rf *stub* *.so *.o *.hi *.hs

%.hs: %.hsc
	hsc2hs -I. $<

%.o: %.hs
	ghc -c $<

%.o: %.c
	ghc -c $<
