include ../../Makefile.include
index:;	index-new-html -l -s -t ../../Style/trailer.html
all: update comments index-new
update:;	svn update
ci:;		svn ci -m 'update projects'
comments:;	egrep -l '^Title:' [a-z]* | while read f ; do egrep '^Title:' $$f | sed 's/^Title: *//' > $$f.comment ; done
