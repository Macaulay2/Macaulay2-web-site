# To publish a package, copy NAME-VERSION.tar or NAME-VERSION.el here
# and run "make" to regenerate archive-contents.

EMACS = emacs
PACKAGES = $(wildcard *.tar) $(filter-out make-archive-contents.el,$(wildcard *.el))

all: archive-contents

# always regenerated, since adding or removing packages with old
# timestamps wouldn't otherwise trigger a rebuild
archive-contents:
	$(EMACS) --batch -l ./make-archive-contents.el $@ $(PACKAGES)

index:
	~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html

.PHONY: all archive-contents index
