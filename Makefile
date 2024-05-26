PREFIX=/usr/local

.PHONY: clean install uninstall install-deps

anicca-subscribe: main.rkt
	raco exe -o anicca-subscribe main.rkt

install: anicca-subscribe
	install -Dvm755 ./anicca-subscribe $(PREFIX)/bin/anicca-subscribe

uninstall:
	rm -vf $(PREFIX)/bin/anicca-subscribe

install-deps:
	raco pkg install --skip-installed http-easy-lib text-table

clean:
	rm -f anicca-subscribe
