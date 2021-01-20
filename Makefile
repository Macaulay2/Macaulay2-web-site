include Makefile.include
index:
mirror:; rsync -a --delete . root@www2.macaulay2.com:/var/www/www2.macaulay2.com/Macaulay2/.

DIRS = \
	Book \
	Citing \
	Downloads \
	Events \
	Funding \
	GettingStarted \
	Jobs \
	Libraries \
	PublicKeys \
	Publications \
	Resources \
	Scripts \
	Style \
	Questions \
	Repositories \
	Screenshots \
	TryItOut \
	dev

index: index-local
index:; for dir in $(DIRS) ; do $(MAKE) -C $$dir $@ || exit 1 ; done
index-local:; index-new-html -l -t Style/trailer.html
404.html: 404-pre.html; index-new-html -f "File not found" -t Style/trailer.html <$< >$@.tmp && mv $@.tmp $@
.PHONY : mirror
# Local Variables:
# compile-command: "make "
# End:
