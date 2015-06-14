# To handle all the signatures without typing the gpg key passphrase more than once, I find
# it necessary to start "gpg-agent" with this command:
#   eval `gpg-agent --daemon --pinentry-program /usr/bin/pinentry-curses`
# and then to run "make" in a terminal window rather than in an emacs buffer.

.PHONY : debian cygwin always all index
DOWNLOADDIR = ../Downloads/GNU-Linux
FLAVORS = Ubuntu Debian Raspbian
UBUNTU_RELEASES = maverick precise quantal raring trusty utopic
DEBIAN_RELEASES = etch lenny squeeze wheezy jessie
RASPBIAN_RELEASES = wheezy
SUFFIX =

# Debian repository is at http://security.debian.org/dists/../
#  note: the symbolic link Debian/dists/stable tracks the latest stable release
#        and should be maintained to be the same as the link "stable" at http://security.debian.org/dists/
# Ubuntu repository is at http://us.archive.ubuntu.com/ubuntu/

# SUFFIX=-beta
all : index-new
test-signature :
	rm -f test-sig.*
	echo hi there > test-sig.txt
	sign-files test-sig.txt
	rm -f test-sig.*
index-repositories : cygwin debian debian-signature 
# links:
# 	find . -type d | while read x ; do [ -a "$$x"/index.css ] || ln -v ../index.css "$$x" ; done
# 	find . -type d | while read x ; do [ -a "$$x"/icon.gif ] || ln -v ../icon.gif "$$x" ; done
debian-only : test-signature debian debian-signature
index-new:
	~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html
#	~/to/Macaulay2/Scripts/index-new-html --shadow

index-new-cygwin:
	cd cygwin && ~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html

always : 
cygwin : always ; $(MAKE) -C cygwin

debian-signature : always
	 @ for dist in $(FLAVORS) ; \
	 do set -e ;\
	    echo distribution: $$dist >&2 ;\
	    case $$dist in \
	       Debian) releases="$(DEBIAN_RELEASES)" ;;\
	       Ubuntu) releases="$(UBUNTU_RELEASES)" ;;\
	       Raspbian) releases="$(RASPBIAN_RELEASES)" ;;\
	       *) echo unrecognized distribution: $$dist >&2 ; exit 1 ;;\
	    esac ;\
	    for release in $$releases ; \
	    do echo "  ==== release: $$release" >&2 ;\
	       ( mkdir -p $$dist$(SUFFIX)/dists/$$release;\
		 cd $$dist$(SUFFIX)/dists/$$release ;\
		 ( echo Origin: $$dist ;\
		   echo Label: $$dist ;\
		   echo Suite: $$release ;\
		   echo Date: `date` ;\
		   echo Codename: $$release ;\
		   echo Architectures: amd64 i386 powerpc sparc ;\
		   echo Components: main ;\
		   echo Description: Macaulay2 ;\
		   echo MD5Sum: ;\
		   find * -name Packages\* -o -name Sources\* | while read filename ;\
			do echo " `md5sum -b < $$filename | sed 's/ .*//'` `wc -c < $$filename` $$filename" ;\
			done ;\
		   echo SHA1: ;\
		   find * -name Packages\* -o -name Sources\* | while read filename ;\
			do echo " `sha1sum -b < $$filename | sed 's/ .*//'` `wc -c < $$filename` $$filename" ;\
			done ;\
		   echo SHA256: ;\
		   find * -name Packages\* -o -name Sources\* | while read filename ;\
			do echo " `sha256sum -b < $$filename | sed 's/ .*//'` `wc -c < $$filename` $$filename" ;\
			done ;\
		 )  > Release;\
		 rm -f Release.gpg ;\
	         gpg --armor --detach-sign -u Macaulay2 --output=Release.gpg Release ;\
	       )\
            done ;\
	done

debian : always
	 @for dist in $(FLAVORS) ; \
	 do echo ======== distribution: $$dist >&2 ;\
	    case $$dist in \
	       Debian) releases="$(DEBIAN_RELEASES)" ;;\
	       Ubuntu) releases="$(UBUNTU_RELEASES)" ;;\
	       Raspbian) releases="$(RASPBIAN_RELEASES)" ;;\
	       *) echo unrecognized distribution: $$dist >&2 ; exit 1 ;;\
	    esac ;\
	    for release in $$releases ; \
	    do echo "  ==== release: $$release" >&2 ;\
	       case $$dist-$$release in \
		    Ubuntu-maverick) match="Ubuntu-10.10" ;;\
		    Ubuntu-precise) match="Ubuntu-12.04" ;;\
		    Ubuntu-quantal) match="Ubuntu-12.10" ;;\
		    Ubuntu-raring) match="Ubuntu-13.04" ;;\
		    Ubuntu-trusty) match="Ubuntu-14.04" ;;\
		    Ubuntu-utopic) match="Ubuntu-14.10" ;;\
		    Debian-etch) match="Debian-4.0" ;;\
		    Debian-lenny) match="Debian-5.0" ;;\
		    Debian-squeeze) match="Debian-6.0" ;;\
		    Debian-wheezy) match="Debian-7.0" ;;\
		    Debian-jessie) match="Debian-8.1" ;;\
		    Raspbian-wheezy) match="Raspbian-7" ;;\
		    *) echo unrecognized release: $$release >&2 ; exit 1 ;;\
	       esac ;\
	       for ARCH in i386 amd64 armhf powerpc sparc source ; do ( \
		  if [ $$ARCH = source ]; \
		  then ARCHDIR=source;        ITEM=Sources ; \
		  else ARCHDIR=binary-$$ARCH; ITEM=Packages; \
		  fi ; \
		  basename=dists/$$release/main/$$ARCHDIR ;\
		  dirname=$$dist$(SUFFIX)/$$basename ;\
		  mkdir -p $$dirname && cd $$dirname ; DOTS=../../../../.. ;\
		  rm -f Packages* Sources* *.deb *.deb.sig ;\
		  touch $$ITEM ;\
		  : echo "    pwd: `pwd`" >&2 ;\
		  absdir=`cd $$DOTS/$(DOWNLOADDIR)/$$dist && pwd` ;\
		  : echo "    found in $$absdir: " $$DOTS/$(DOWNLOADDIR)/$$dist/*-$$ARCH-*$$match.deb >&2 ;\
		  for p in $$DOTS/$(DOWNLOADDIR)/$$dist/*-$$ARCH-*$$match.deb ;\
		  do if [ -f "$$p" ] ;\
		     then \
			  echo "    ====" archiitecture: $$ARCH >&2 ;\
			  echo "    pwd: `pwd`" >&2 ;\
			  if ! [ -f $$p.sig ] ;\
			  then echo "missing signature file: $$p.sig" >&2 ;\
			       exit 1 ;\
			  fi ;\
			  echo "    gpg --verify --quiet $$p.sig" >&2 ;\
			  gpg --verify --quiet $$p.sig 2>&1 | sed 's/^/    /' >&2 || exit 1 ;\
			  ln -sv $$p $$p.sig . 2>&1 | sed 's/^/    /' >&2 || exit 1;\
			  ar x $$p control.tar.gz ;\
			  tar xzf control.tar.gz control ;\
			  ( cat control ; \
			    echo Filename: $$basename/`basename $$p` ; \
			    echo MD5sum: `md5sum $$p | sed 's/ .*//'` ; \
			    echo SHA1: `sha1sum $$p | sed 's/ .*//'` ; \
			    echo SHA256: `sha256sum $$p | sed 's/ .*//'` ; \
			    echo Size: `wc -c $$p | sed 's/ .*//'` ; \
			    echo ) >> $$ITEM ;\
			  rm control control.tar.gz ;\
			  p=`echo $$p | sed -e "s=/$$dist/=/Common/=" -e "s=-$$ARCH.*=-common.deb="` ;\
			  : echo $$dist $$release $$ARCH : `basename $$p` >&2 ;\
			  if ! [ -f $$p ] ;\
			  then echo "missing file: $$p" >&2 ;\
			       exit 1;\
			  fi ;\
			  echo "    gpg --verify --quiet $$p.sig" >&2 ;\
			  gpg --verify --quiet $$p.sig 2>&1 | sed 's/^/    /' >&2 || exit 1 ;\
			  ln -sv $$p $$p.sig . 2>&1 | sed 's/^/    /' >&2 || exit 1 ;\
			  ar x $$p control.tar.gz ;\
			  tar xzf control.tar.gz control ;\
			  ( cat control ; \
			    echo Filename: $$basename/`basename $$p` ; \
			    echo MD5sum: `md5sum $$p | sed 's/ .*//'` ; \
			    echo SHA1: `sha1sum $$p | sed 's/ .*//'` ; \
			    echo SHA256: `sha256sum $$p | sed 's/ .*//'` ; \
			    echo Size: `wc -c $$p | sed 's/ .*//'` ; \
			    echo ) >> $$ITEM ;\
			  rm control control.tar.gz ;\
		     fi ;\
		  done ;\
		  gzip  <$$ITEM >$$ITEM.gz  ;\
		  bzip2 <$$ITEM >$$ITEM.bz2 ;\
		  ) || exit 1 ;\
	     done ; done ;\
	done