include ../../../Makefile.include
OPTIONS := -t "$(shell echo `pwd`/../../../Style/trailer.html)"
all: README.html FOOTER.html
	index-new-html -l $(OPTIONS)
index: README.html FOOTER.html
	index-html
README.html : ../Ubuntu/README0.html Makefile
	rm -f $@
	sed 's/Ubuntu/Raspbian/g' <$< >$@
	chmod a-w $@
FOOTER.html : ../Ubuntu/FOOTER0.html Makefile
	rm -f $@
	sed 's/Ubuntu/Raspbian/g' <$< | egrep -v '<!--[^R]-->' >$@
	chmod a-w $@
