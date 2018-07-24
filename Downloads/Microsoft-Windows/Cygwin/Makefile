include ../../../Makefile.include
index-new:setup.html ; index-new-html -l -t ../../../Style/trailer.html
setup.html: setup-pre.html; index-new-html -f Setup -t ../../../Style/trailer.html <$< >$@.tmp && mv $@.tmp $@
