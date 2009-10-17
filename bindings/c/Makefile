test: client.exe
	cat ../../README.markdown | ./client.exe

client.exe: client.c
	cc -lpandoc -Xpreprocessor -I../.. $< -o $@

clean:
	rm -rf client.exe

