DIRS := util inputs chapters/* book proposals
default : all
all clean distclean::
	for i in $(DIRS); \
	do if [ "$$i" != chapters/CVS -a -d "$$i" ] ; \
	   then $(MAKE) -C $$i $@ || exit 1 ; \
	   fi ; \
	done
run:
	for i in util chapters/*; \
	do if [ -d "$$i" -a "$$i" != chapters/CVS ] ;\
	   then make -C "$$i" $@ || exit 1 ;\
	   fi ;\
	done
diffs log:
	for i in chapters/*; \
	do if [ -d "$$i" -a "$$i" != chapters/CVS ] ;\
	   then make -C "$$i" $@ || exit 1 ;\
	   fi ;\
	done
TAGS : always
	etags book/*.tex inputs/*.tex chapters/*/*.tex
clean ::; rm -f TAGS
always :
