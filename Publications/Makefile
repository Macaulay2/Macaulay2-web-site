include ../Makefile.include
all: index-new
index-new: README.html; index-new-html -t ../Style/trailer.html
README.html: README-top.html README-bottom.html papers.txt Makefile
	(\
		cat README-top.html ;\
		<papers.txt awk '{printf("<li>%s</li>\n",$$0);}' ;\
		cat README-bottom.html ;\
	)>$@
papers.txt: joined-references
	awk -F "	" '{print $$5}' <$< >$@
	$(MAKE) -C ../Scripts citation-table.js
