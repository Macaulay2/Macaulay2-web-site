TEX = latex

%.pdf : %.tex
	@ echo '=================================================================='
	@ echo '==                       making $@'
	@ echo '=================================================================='
	pdf$(TEX) $<
%.ps : %.dvi 
	@ echo '=================================================================='
	@ echo '==                       making $@'
	@ echo '=================================================================='
	dvips $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	rm -f body.tmp head.tmp
%.gz : %
	gzip <$< >$@

all : lncse-doc.dvi lncse-dem.dvi lncse-doc.ps.gz

lncse-doc.dvi : lncse-doc.tex
	$(TEX) lncse-doc.tex
	$(TEX) lncse-doc.tex

lncse-dem.dvi : lncse-dem.tex
	$(TEX) lncse-dem.tex
	$(TEX) lncse-dem.tex

clean :
	rm -f *.aux *.dvi *.log *.idx *.toc *.*pk
