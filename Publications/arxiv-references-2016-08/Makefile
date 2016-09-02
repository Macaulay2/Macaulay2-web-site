# create "old-references" by copying old "joined-references"
# create "search-results-url" by runnning harvest.m2 and putting the URLs into the file
#	note: we can only get 200 results that way!
# copy file "false-references"
# create empty file "additional-results"

all : joined-references

new-references: search-results-key get-all-xml extract-xml.m2 Makefile
	- mv new-references new-references.bak
	M2 --stop extract-xml.m2 -e 'exit 0'
	- diff -u new-references.bak new-references

get-all-xml : search-results-key get-xml
	./get-xml <$< >$@
	touch $@

search-results-key : search-results-url-fixed url-to-key.sed
	sed -f url-to-key.sed <$< | sort | uniq >$@ 

search-results-url:
	: I make this file manually using harvest.m2
	false

# additional-results contains papers where the author has told me it uses Macaulay2, but the full text search doesn't find it
#   for example, it might refer to Macaulay instead (e.g., 0902.4065)
#   or the full text search may not work (e.g., 0803.0341)

search-results-url-fixed : search-results-url false-references additional-results
	wc -l search-results-url
	(join -v 1 search-results-url false-references ; cat additional-results ) | sort | uniq >$@
	wc -l $@

joined-references : old-references.keyed new-references.keyed 
	( \
		join -v 1 -t "	" -o '1.2	1.3	1.4	1.5	1.6' $^ ;\
		join -a 2 -t "	" -o '2.2	2.3	2.4	2.5	2.6' $^ )\
		| sort >$@
	wc -l $@


%.keyed : %
	<$< egrep -v '^#' | awk -F '	' '{if (length($$5)==0) exit(1) ; printf "%s+%s+%s+%s	%s	%s	%s	%s	%s\n", $$1,$$2,$$3,$$4, $$1, $$2, $$3, $$4, $$5}'>$@.tmp
	sort <$@.tmp >$@
	rm $@.tmp
bak:; rsync -av --delete --delete-excluded --exclude html --exclude xml --exclude 'tmp.*' ../arxiv-references/. u00:tmp/backups/arxiv-references/.
install:; rsync -av --delete --delete-excluded --exclude html --exclude xml --exclude 'tmp.*' ../arxiv-references/. u00:to/Macaulay2/Publications/arxiv-references/.
clean:; rm -f .log
