include ../../../Makefile.include
all : index-new 
index-new:; index-new-html -l -t ../../../Style/trailer.html
setup.html: setup-pre.html; index-new-html -f Setup -t ../../../Style/trailer.html <$< >$@.tmp && mv $@.tmp $@
