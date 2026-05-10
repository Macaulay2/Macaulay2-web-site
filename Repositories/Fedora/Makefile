make-repos:
	for i in 42 43;					\
	do ( set -x ;						\
	     cd "$$i" ;						\
	     find . -name \*.rpm > packages.tmp ;		\
	     createrepo --verbose --pkglist packages.tmp . ;	\
	     rm packages.tmp ;					\
	     ) ;						\
	done
index:
	~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html
