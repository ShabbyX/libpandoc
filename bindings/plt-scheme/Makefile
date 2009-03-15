plt=pandoc
own=toyvo
maj=1
min=2

all: $(plt).plt

$(plt).plt: $(plt)
	planet create $(plt)

install: $(plt).plt
	planet fileinject $(own) $(plt).plt $(maj) $(min)

uninstall:
	planet remove $(own) $(plt).plt $(maj) $(min)

clean:
	rm -f $(plt).plt