include ../Makefile.include

all: citation-table.js randomize documentation-table.js
citation-table.js: ../Publications/papers.txt Makefile citation.awk
	grep href <$< |sed -e "s/\\\\' *e/\\&eacute;/" -e "s/'/\\\\'/g" | awk -f citation.awk >$@
randomize: documentation-table.js
documentation-table.js: always
	time ((cd ../..;\
		find Macaulay2/doc/Macaulay2-$(DOCVERSION)/share/doc/Macaulay2/Macaulay2Doc/html -name \*.html | \
		while read x ; do echo `rand`:$$x ; done \
		) | sort | head -100 | sed 's/.*://' | \
		while read f ; do echo "$$f:`<../../$$f egrep '<title>.*</title>'`" ; done | \
		sed -e 's=:.*<title>\(.*\)</title>=:\1=' -e "s/'/\\\\'/g" -e "s/(/ (/g" -e "s/,/, /g" | \
		awk -F : -f documentation.awk >$@.tmp)
	mv $@.tmp $@
always:

index:
	index-new-html -t ../Style/trailer.html
