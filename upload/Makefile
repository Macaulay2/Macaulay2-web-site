a:
	: chmod g+w Macaulay2*
	find . -name Macaulay2\*common\* -exec mv -iv '{}' ../Downloads/Common/. ';'
	find . -name Macaulay2\*Linux-Generic\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Generic/. ';'
	find . -name Macaulay2\*Ubuntu\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Ubuntu/. ';'
	find . -name Macaulay2\*Debian\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Debian/. ';'
	find . -name Macaulay2\*FreeBSD\* -exec mv -iv '{}' ../Downloads/FreeBSD/. ';'
	find . -name Macaulay2\*MacOS\* -exec mv -iv '{}' ../Downloads/MacOSX/. ';'
	find . -name Macaulay2\*Fedora\* -exec mv -iv '{}' ../Downloads/GNU-Linux/Fedora/. ';'
	find . -name Macaulay2\*ScientificLinux\* -exec mv -iv '{}' ../Downloads/GNU-Linux/ScientificLinux/. ';'
	$(MAKE) -C ../Downloads index-new
	$(MAKE) -C ../Downloads/Microsoft-Windows index-new-local
	$(MAKE) -C ../Downloads/Microsoft-Windows/Cygwin index-new
	$(MAKE) -C ../Downloads/GNU-Linux/Debian all
	$(MAKE) -C ../Downloads/MacOSX index-new-local
