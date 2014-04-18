VERSION = 1.4
RELEASE = 12617
URL = -r $(RELEASE) svn://svn.macaulay2.com/Macaulay2/release-branches/$(VERSION)
TARFILE = Macaulay2-$(VERSION)-r$(RELEASE)-src.tar.bz2
DIRNAME = Macaulay2-$(VERSION)-r$(RELEASE)
include ../../Makefile.include
index-new:; index-new-html -t ~/to/Macaulay2/Style/trailer.html -l
tarfile: $(TARFILE) index-new
$(TARFILE):
	rm -rf tmp
	mkdir tmp
	cd tmp && svn export -q $(URL) $(DIRNAME)
	tar cfj tmp/$(TARFILE) -C tmp $(DIRNAME)
	ln tmp/$(TARFILE) $(TARFILE)
	rm -rf tmp
