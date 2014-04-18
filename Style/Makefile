include ../Makefile.include
all: index.css index-new
index.css: index.css.sh; mv -f $@ $@.bak && ./$<>$@ && chmod a-w $@
index-new index:; index-new-html -t trailer.html
