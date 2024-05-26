PREFIX=/usr/local

.PHONY: clean install-deps

anicca-subscribe: main.rkt
	raco exe -o anicca-subscribe main.rkt

install: all
	install -Dvm755 ./anicca-subscribe $(PREFIX)/bin/anicca-subscribe

install-deps:
	raco pkg install --skip-installed http-easy-lib text-table

clean:
	rm -f anicca-subscribe
