make-repos:
	for i in 6 7 ;						\
	do ( set -x ;						\
	     cd "$$i" ;						\
	     find . -name \*.rpm > packages.tmp ;		\
	     createrepo --verbose --pkglist packages.tmp . ;	\
	     rm packages.tmp ;					\
	     ) ;						\
	done
