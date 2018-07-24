OPTIONS := -t "$(shell echo `pwd`/../Style/trailer.html)"
include ../Makefile.include
index:
debian:
	$(MAKE) -C GNU-Linux/Debian
index-local:
	index-new-html -l $(OPTIONS)
index: debian index-local
	cd Common && index-new-html $(OPTIONS)
	cd FreeBSD && index-new-html $(OPTIONS)
	cd GNU-Linux && index-new-html $(OPTIONS)
	cd MacOSX && index-new-html $(OPTIONS)
	cd Microsoft-Windows && index-new-html $(OPTIONS)
	cd Copyright && make && index-new-html $(OPTIONS)
	cd OtherSourceCode && index-new-html $(OPTIONS)
	cd SourceCode && index-new-html $(OPTIONS)

