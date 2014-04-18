include ../Makefile.include
all: build index-new
build:; $(MAKE) -C projects
index-new:; index-new-html -s -t `pwd`/../Style/trailer.html
