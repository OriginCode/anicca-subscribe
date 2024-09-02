STATIC=1
DESTDIR=
PREFIX=/usr/local

.PHONY: clean install uninstall install-deps

anicca-subscribe: main.rkt
ifeq ($(STATIC), 1)
	raco exe --vv --orig-exe -o anicca-subscribe main.rkt
else
	raco exe --vv -o anicca-subscribe main.rkt
endif

install: anicca-subscribe
	raco distribute -v $(DESTDIR)$(PREFIX) anicca-subscribe

uninstall:
	rm -vf $(DESTDIR)$(PREFIX)/bin/anicca-subscribe

install-deps:
	raco pkg install --skip-installed http-easy-lib text-table

clean:
	rm -f anicca-subscribe
