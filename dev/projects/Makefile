include ../../Makefile.include
all: update comments index-new
update:;	svn update
ci:;		svn ci -m 'update projects'
comments:;	egrep -l '^Title:' [a-z]* | while read f ; do egrep '^Title:' $$f | sed 's/^Title: *//' > $$f.comment ; done
index-new:;	index-new-html -l -s
