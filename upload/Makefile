include ../Makefile.include

index : ; index-new-html -l -t ../Style/trailer.html

install:
	sign-files *.rpm *.deb *.tar.gz *.dmg
	find . -name Macaulay2\*common\* -exec mv -iv '{}' ../Downloads/Common/. ';'
	find . -name Macaulay2\*Linux-Generic\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Generic/. ';'
	find . -name Macaulay2\*macOS\* -exec mv -iv '{}' ../Downloads/MacOSX/. ';'
	find . -name Macaulay2\*Ubuntu\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Ubuntu/. ';'
	find . -name Macaulay2\*Debian\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Debian/. ';'
	find . -name Macaulay2\*FreeBSD\* -exec mv -iv '{}' ../Downloads/FreeBSD/. ';'
	find . -name Macaulay2\*Darwin\* -exec mv -iv '{}' ../Downloads/MacOSX/. ';'
	find . -name Macaulay2\*[fF]edora\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Fedora/. ';'
	find . -name Macaulay2\*-Linux-CentOS-\* -exec mv -iv '{}' ../Downloads/GNU-Linux/"Red Hat Enterprise, CentOS, Scientific Linux"/. ';'
	find . -name Macaulay2\*-Linux-scientific-\* -exec mv -iv '{}' ../Downloads/GNU-Linux/"Red Hat Enterprise, CentOS, Scientific Linux"/. ';'
	find . -name Macaulay2\*-Linux-rhel-\* -exec mv -iv '{}' ../Downloads/GNU-Linux/"Red Hat Enterprise, CentOS, Scientific Linux"/. ';'
	find . -name Macaulay2\*opensuse\* -exec mv -iv '{}' ../Downloads/GNU-Linux/openSUSE/. ';'
	find . -name Macaulay2\*-ArchLinux\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Arch/. ';'
	$(MAKE) -C ../Downloads index
	$(MAKE) -C ../Downloads/Microsoft-Windows index-local
	$(MAKE) -C ../Downloads/Microsoft-Windows/Cygwin index
	$(MAKE) -C ../Downloads/GNU-Linux/Debian all
	$(MAKE) -C ../Downloads/MacOSX index-local
	$(MAKE) index
