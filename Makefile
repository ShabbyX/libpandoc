all:	build

configure:
	./Setup.lhs configure

build:
	./Setup.lhs build

install: 
	./Setup.lhs install

clean:
	./Setup.lhs clean

