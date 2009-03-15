all: Pandoc.dll

Pandoc.dll: Pandoc.cs
	mcs -target:library -out:$@ $< 

clean: 	
	rm -f Pandoc.dll


test: Pandoc.dll
	fsi --exec --quiet pandoc.fsx