include ../Makefile.include

I = ../doc/Macaulay2-$(DOCVERSION)/share/doc/Macaulay2/*

index:

packages: .
	@ umask 022; find . -maxdepth 1 -mindepth 1 -type d | sed 's=^\./==' | while read i ; \
	  do echo $$i >&3 ;\
	     echo $$i: >&2 ;\
	     $(MAKE) -f ../Makefile.pkg -C $$i ;\
	  done 3>$@

# fill a file with lines like this:
#     <li><a href="../doc/Macaulay2-1.6/share/doc/Macaulay2/AdjointIdeal/html/index.html" title=", Adjoint ideal of a plane curve and related computations">AdjointIdeal</a>, Adjoint ideal of a plane curve and related computations</li>
documented-packages: $I Makefile
	for h in $I ;\
	do if [ -f $$h/html/.Headline ] ;\
	   then echo -n '    <li>';\
		if [ -f $$h/html/.Certification ] ;\
		then echo -n '<img src="../Style/GoldStar.png" alt="a gold star"/> ' ;\
		fi ;\
		echo "<a href=\"$$h/html/\" title=\"`cat $$h/html/.Headline`\">`basename $$h`</a>, `cat $$h/html/.Headline`</li>" ;\
	   fi ;\
	done >$@

index index-local: README.html

index index-local: packages README.html

README.html: README.html.tmp
	mv $< $@

README.html.tmp: documented-packages README-2.html Makefile ../Makefile.include INTRO.html
	(exec >$@ ;\
	 cat INTRO.html ;\
	 echo '<h2>Packages included with <i>Macaulay2</i> version $(DOCVERSION)</h2>' ;\
	 echo ' <ul>' ;\
	 cat documented-packages ;\
	 echo ' </ul>' ;\
	 cat README-2.html ;\
	)

index:; index-new-html -t `pwd`/../Style/trailer.html

index-local:; index-new-html -l -t `pwd`/../Style/trailer.html




