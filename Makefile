include Makefile.include

DIRS = \
	Citing \
	Contributors \
	Downloads \
	Events \
	Funding \
	GettingStarted \
	Jobs \
	Libraries \
	Packages \
	PublicKeys \
	Publications \
	Style \
	Questions \
	Repositories \
	Screenshots \
	TryItOut \
	dev

all:
all index-new: index-new-local
all index index-new:
	for dir in $(DIRS) ; do $(MAKE) -C $$dir $@ || exit 1 ; done
local index-new-local:; index-new-html -l -t Style/trailer.html

find-new:; find $(DIRS) -name icon.gif -ls
rm-new:; find $(DIRS) -name icon.gif | xargs rm -v

VERSION = 0.9.8
DIR = $(HOME)/local.Linux/encap/Macaulay2-$(VERSION)/
EXC =   --exclude="*.m2" \
	--exclude=".\#*" \
	--exclude=".linkdir" \
	--exclude="*.out"
PATHS = share/Macaulay2 share/doc/Macaulay2
TARDIR = doc-$(VERSION)
listdoc :; tar chf - -C $(DIR) $(EXC) $(PATHS) | tar tfv - -C $(TARDIR)
copydoc : $(TARDIR); tar chf - -C $(DIR) $(EXC) $(PATHS) | tar xf - -C $(TARDIR)
$(TARDIR) :; mkdir "$@"
find-html:; find . -name doc -prune -false -o -not -name README.html -not -name index.html -not -name index-new.html -not -name README-*.html -not -name FOOTER.html  -name \*.html

404.html: 404-pre.html; index-new-html -f "File not found" -t Style/trailer.html <$< >$@.tmp && mv $@.tmp $@
all: mirror
.PHONY : mirror
mirror:
	rsync -av . root@www2.macaulay2.com:/var/www/www2.macaulay2.com
# Local Variables:
# compile-command: "make "
# End:
