# use this with gnu make
export TEXINPUTS := ../inputs:$(TEXINPUTS)
LINEWIDTH := 99
TEX := latex
FILES := Makefile example.tex merge.c process.c
CFLAGS := -Wall -g
CC := gcc
.SUFFIXES: -m2.tex .aux .out .m2
.PRECIOUS: example.m2 example.out example-m2.tex

all : merge process demo words
clean :; rm -rf merge process *.log *.aux *.dvi *-m2.tex example.m2 *.old *.out
distclean : clean
demo : example-m2.dvi
example-m2.dvi : ../inputs/merge.tex
run : process
shar :
	shar $(FILES) >util.shar

%.out : %.m2
	@ echo '=================================================================='
	@ echo '==                       making $@, perhaps'
	@ echo '=================================================================='
	@if [ -f $*.out ] && cmp -s $*.m2 $*.old ; \
	 then touch $*.out ; \
	 else echo "running M2 on $*.m2"; \
	   echo M2 -x -s -q "<$*.m2" ">$*.out.tmp"; \
	   M2 -x -s -q <$*.m2 >$*.out.tmp && \
	   mv $*.out.tmp $*.out && \
	   cp $*.m2 $*.old ; \
	 fi

%-m2.tex : %.tex %.out merge
	@ echo '=================================================================='
	@ echo '==                       making $@'
	@ echo '=================================================================='
	./merge -w $(LINEWIDTH) $*.tex $*.out >$*-m2.tex.tmp
	mv $*-m2.tex.tmp $*-m2.tex

%.m2 : %.tex process
	@ echo '=================================================================='
	@ echo '==                       making $@'
	@ echo '=================================================================='
	./process $< >$@.tmp
	mv $@.tmp $@
