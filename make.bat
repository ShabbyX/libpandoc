ghc --make -no-hs-main -optl-shared -o libpandoc.dll libpandoc.hs libpandoc-dll.c
del *.hi *.o *stub* *.manifest