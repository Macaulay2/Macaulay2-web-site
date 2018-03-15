include ../Makefile.include
all: debian index-new
debian-ubuntu: debian
	cd GNU-Linux/Ubuntu && index-new-html $(OPTIONS)
	cd GNU-Linux/Debian && index-new-html $(OPTIONS)
debian:
	$(MAKE) -C GNU-Linux/Debian
#links:
#	find . -type d | while read x ; do [ -a "$$x"/index.css ] || ln -v ../index.css "$$x" ; done
#	find . -type d | while read x ; do [ -a "$$x"/icon.gif ] || ln -v ../icon.gif "$$x" ; done

OPTIONS := -t "$(shell echo `pwd`/../Style/trailer.html)"
index-new-local:
	index-new-html -l $(OPTIONS)
index-new: index-new-local
	cd Common && index-new-html $(OPTIONS)
	cd FreeBSD && index-new-html $(OPTIONS)
	cd GNU-Linux && index-new-html $(OPTIONS)
	cd MacOSX && index-new-html $(OPTIONS)
	cd Microsoft-Windows && index-new-html $(OPTIONS)
	cd Copyright && make && index-new-html $(OPTIONS)
	cd OtherSourceCode && index-new-html $(OPTIONS)
	cd SourceCode && index-new-html $(OPTIONS)

