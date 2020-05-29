# I'm manually hard linking the appropriate rpm files into 6/ or 7/ before running this.
# Also, use rpmsign --add-sign *.rpm to add signatures to the rpm files

make-repos:
	for i in 6 7 8 ;					\
	do ( set -x ;						\
	     cd "$$i" ;						\
	     find . -name \*.rpm > packages.tmp ;		\
	     createrepo --verbose --pkglist packages.tmp . ;	\
	     rm packages.tmp ;					\
	     ) ;						\
	done
index:
	~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html
