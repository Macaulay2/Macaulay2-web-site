CHAPTERS :=     \
		preface/chapter \
		varieties/chapter \
		geometry/chapter \
		programming/chapter \
		schemes/chapter \
		monomialIdeals/chapter \
		solving/solving \
		completeIntersections/ci \
		toricHilbertScheme/chapter \
		exterior-algebra/chapter \
		constructions/chapter \
		d-modules/chapter 
CHAPTERFILES := $(foreach c,$(CHAPTERS), ../chapters/$(c)-m2.tex ../chapters/$(c)-wrapper.bbl)

all : book.dvi book.pdf book.ps book-reduced.ps book.shar book.words.ps
	@echo done

once: book.tex; latex '\scrollmode \input book'

book.pdf book.dvi : book.tex ../inputs/lncse.cls ../inputs/merge.tex
book.tex : ../inputs/book-macros.tex $(CHAPTERFILES) book.sh addresses.tex
	./book.sh >$@

# DVIPSFLAGS = -z

# see share/texmf/metafont/misc/modes.mf for metafont modes like this one:
DVIPSFLAGS += -D 1200 -mode ljfzzz

CC := gcc
TEX = latex
export TEXINPUTS := ../inputs:../chapters/d-modules:../chapters/solving:../chapters/monomialIdeals:$(TEXINPUTS)

.SUFFIXES: .aux
.PHONY : all bak shar
.PRECIOUS : %.dvi

# compressing a file
%.gz : %
	gzip <$< >$@

# making indices
%.ind : %.idx
	@if [ -f $@ ] && cmp -s $< $*-old.idx ; \
	 then : ; \
	 else echo '==================================================================' ; \
	      echo '==                       making $@' ; \
	      echo "makeindex -s ../lncse-macros/sprmindx.sty -o $*.tmp $* && mv $*.tmp $@ && cp $< $*-old.idx" ; \
	            makeindex -s ../lncse-macros/sprmindx.sty -o $*.tmp $* && mv $*.tmp $@ && cp $< $*-old.idx ; \
	      echo '==================================================================' ; \
	 fi

# running tex
%.dvi : %.tex %.ind
	@ echo '=================================================================='
	@ echo '==                       making $@'
	if [ -f $*.idx ] ; then cp $*.idx $*-old.idx; else touch $*-old.idx; fi
	$(TEX) '\scrollmode \input $*'
	@ echo '=================================================================='
	@if cmp -s $*.idx $*-old.idx ; \
	then : ; \
	else echo '=================================================================='; \
	     echo '==                       making $*.ind and $@ again'; \
	     echo "makeindex -s ../lncse-macros/sprmindx.sty -o $*.tmp $* && mv $*.tmp $*.ind" ; \
	           makeindex -s ../lncse-macros/sprmindx.sty -o $*.tmp $* && mv $*.tmp $*.ind ; \
	     echo "$(TEX) '\scrollmode \input $*'"; \
	           $(TEX) '\scrollmode \input $*'; \
	     echo '=================================================================='; \
	fi
	@if true || grep -s "Rerun to get cross-references right." $*.log; \
	then echo '=================================================================='; \
	     echo '==                       making $@ again'; \
	     echo "$(TEX) '\scrollmode \input $*'"; \
	           $(TEX) '\scrollmode \input $*'; \
	     echo '=================================================================='; \
	fi

# postscript
%.ps : %.dvi
	@ echo '=================================================================='
	@ echo '==                       making $@'
	dvips $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	rm -f body.tmp head.tmp
	@ echo '=================================================================='

# acrobat
%.pdf : %.tex
	@ echo '=================================================================='
	@ echo '==                       making $@'
	pdf$(TEX) '\scrollmode \input $*'
	pdf$(TEX) '\scrollmode \input $*'
	@ echo '=================================================================='

# %.pdf : %.ps
# 	@ echo '=================================================================='
# 	@ echo '==                       making $@'
#	ps2pdf $<
# 	@ echo '=================================================================='

# dvired
%-reduced-3.ps : %.dvi
	@ echo '=================================================================='
	@ echo '==                       making $@'
	dvired $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	@ echo '=================================================================='

# dvired
%-reduced-2.ps : %.ps
	@ echo '=================================================================='
	@ echo '==                       making $@'
	mpage -2 $< >$@
	@ echo '=================================================================='

# pstops reduction
%-reduced.ps : %.ps Makefile
	@ echo '=================================================================='
	@ echo '==                       making $@'
	pstops '2:0L@.9(9.45in,-.6in)+1L@.9(9.45in,4.1in)' $< >$@
	@ echo '=================================================================='


# compression
%.gz : %
	gzip <$< >$@

# miscellaneous
print : book.ps
	lpr $<

clean :
	@ echo '=================================================================='
	@ echo '==                       making $@'
	rm -rf *.dvi *.numtex *.nums *.blg *.tmp *.ps *.pdf *.tgz *.shar *.old *.*pk \
		*.idx *.toc *.log *.xyc *.aux *.bbl *.lof *.lot *.glo *.ind *.ilg *.out \
		*.pdf.gz *.ps.gz tmp
	@ echo '=================================================================='

distclean : clean

include ../Makefile.common

book.shar : book.mst book.tex ../inputs/lncse.cls ../lncse-macros/sprmindx.sty \
		../inputs/xispace.sty README book.ind book.aux book.toc \
		../inputs/diagrams.tex ../inputs/springer.bst \
		../chapters/solving/Y25.eps \
		../chapters/monomialIdeals/octahedron.eps \
		../chapters/monomialIdeals/standardpairs-fixed.eps \
		../inputs/merge.tex
	mkdir /tmp/book-$$$$ ; \
	cp $^ /tmp/book-$$$$ ; \
	(cd /tmp/book-$$$$ ; shar *) >$@ ; \
	rm -rf /tmp/book-$$$$

test-shar : book.shar
	rm -rf tmp
	mkdir -p tmp && cd tmp && sh ../book.shar && sh -x README

CODEFILES := $(foreach c,$(CHAPTERS), ../chapters/$(c).m2)
book.words : $(CODEFILES) ../../emacs/M2-symbols
	@ echo '=================================================================='
	@ echo '==                       making $@'
	cat $(CODEFILES) | \
		../util/words | \
		sort | uniq | \
		join - ../../emacs/M2-symbols >$@
	wc -l book.words
	@ echo '=================================================================='

book.words.ps : book.words
	@ echo '=================================================================='
	@ echo '==                       making $@'
	enscript --copies=3 --columns=4 -o $@ $<
	@ echo '=================================================================='

test-print.ps : book.dvi
	dvips $(DVIPSFLAGS) -o $@ $^ -n 4 -p 82
