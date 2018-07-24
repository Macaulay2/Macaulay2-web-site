include ../Makefile.include
index:index.css; index-new-html -t trailer.html
index.css: index.css.sh; mv -f $@ $@.bak && ./$<>$@ && chmod a-w $@
