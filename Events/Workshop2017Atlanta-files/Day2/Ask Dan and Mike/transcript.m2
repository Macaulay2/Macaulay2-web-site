
+ /Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production/M2 --no-readline -e 'run "stty -echo -onlcr"' -q --print-width 264
Macaulay2, version 1.10.0.1
with packages: ConwayPolynomials, Elimination, IntegralClosure, InverseSystems, LLLBases, PrimaryDecomposition, ReesAlgebra, TangentCone

i1 : 

Process M2 hi finished

+ /Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production/M2 --no-readline -e 'run "stty -echo -onlcr"' -q --print-width 111
Macaulay2, version 1.10.0.1
with packages: ConwayPolynomials, Elimination, IntegralClosure, InverseSystems, LLLBases, PrimaryDecomposition,
               ReesAlgebra, TangentCone

i1 : 1..100

o1 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
     ----------------------------------------------------------------------------------------------------------
     29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
     ----------------------------------------------------------------------------------------------------------
     55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
     ----------------------------------------------------------------------------------------------------------
     81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100)

o1 : Sequence

i2 : QQ[x]

o2 = QQ[x]

o2 : PolynomialRing

i3 : I = ideal(x,x^2)

                2
o3 = ideal (x, x )

o3 : Ideal of QQ[x]

i4 : peek I

o4 = Ideal{cache => CacheTable{} }
           generators => | x x2 |
           ring => QQ[x]

i5 : I.cache#"difficulty" = "very hard"

o5 = very hard

i6 : peek I.cache

o6 = CacheTable{difficulty => very hard}

i7 : new HashTable from {
       a => 1,
       b => 2,
       cache => new CacheTable
     }

o7 = HashTable{a => 1               }
               b => 2
               cache => CacheTable{}

o7 : HashTable

i8 : p = oo

o8 = HashTable{a => 1               }
               b => 2
               cache => CacheTable{}

o8 : HashTable

i9 : p.cache#"difficulty" = "very hard"

o9 = very hard

i10 : peek p

o10 = HashTable{a => 1                      }
                b => 2
                cache => CacheTable{...1...}

i11 : peek p.cache

o11 = CacheTable{difficulty => very hard}

i12 : help "GC garbage collector"

o12 = GC garbage collector
      ********************

      Macaulay2 uses the excellent garbage collector GC, version 7.6.0, written by Hans-J. Boehm and Alan J.
      Demers and generously licensed to the public.  It is available at
      http://www.hpl.hp.com/personal/Hans_Boehm/gc/.

      Some environment variables can be set by the user to tune garbage collector performance:

        * GC_INITIAL_HEAP_SIZE -- initial heap size in bytes
        * GC_MAXIMUM_HEAP_SIZE -- maximum collected heap size
        * GC_FREE_SPACE_DIVISOR -- if set to a number D, then we try to make sure that we allocate at least N/D
          bytes between collections, where N is twice the number of traced bytes, plus the number of untraced
          bytes, plus a rough estimate of the root set size.  Increasing its value will use less space but more
          collection time.  Decreasing it will appreciably decrease collection time at the expense of space.
          Macaulay2 sets the initial default value to 12.
        * GC_PRINT_STATS -- whether to turn on logging
        * GC_PRINT_VERBOSE_STATS -- whether to turn on even more logging
        * GC_DUMP_REGULARLY -- whether to generate a debugging dump on startup and during every collection;
          very verbose
        * GC_NPROCS -- the number of processors to use (for Linux only)
        * GC_MARKERS -- the number of marker threads.  This is normally set to the number of processors.  It is
          safer to adjust GC_MARKERS than GC_NPROCS, since GC_MARKERS has no impact on the lock implementation

      The full list is found in the source code for gc in the file doc/README.environment.

      Here are some error messages you may see from it when it aborts the program, due to lack of memory or
      related problems.  Typically, the only recourse for the user is to increase the memory available to the
      program.

        * Insufficient space for initial table allocation
        * No space for lwp data structures
        * Out of memory
        * Too many exclusions
        * Too many heap sections
        * Too many heap sections: Increase MAXHINCR or MAX_HEAP_SECTS
        * Too many root sets

      See also
      ========

        * "collectGarbage" -- collect the garbage in memory

o12 : DIV

i13 : 

Process M2 hi finished

+ FOO=bar
+ GC_INITIAL_HEAP_SIZE=5G
+ /Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production/M2 --no-readline -e 'run "stty -echo -onlcr"' -q --print-width 264
Macaulay2, version 1.10.0.1
with packages: ConwayPolynomials, Elimination, IntegralClosure, InverseSystems, LLLBases, PrimaryDecomposition, ReesAlgebra, TangentCone

i1 : getenv "FOO"

o1 = bar

i2 : getenv "GC_INITIAL_HEAP_SIZE"

o2 = 5G

i3 : GCstats()

o3 = HashTable{don't expand => false           }
               finalize on demand => false
               full freq => 19
               GC_ALL_INTERIOR_POINTERS => 
               GC_all_interior_pointers => 1
               GC_BACKTRACES => 
               GC_DISABLE_INCREMENTAL => 
               GC_DONT_GC => 
               GC_DUMP_REGULARLY => 
               GC_ENABLE_INCREMENTAL => 
               GC_FIND_LEAK => 
               GC_FORCE_UNMAP_ON_GCOLLECT => 
               GC_FREE_SPACE_DIVISOR => 
               GC_free_space_divisor => 12
               GC_FULL_FREQUENCY => 
               GC_IGNORE_GCJ_INFO => 
               GC_INITIAL_HEAP_SIZE => 5G
               GC_LARGE_ALLOC_WARN_INTERVAL => 
               GC_LOG_FILE => 
               GC_LOOP_ON_ABORT => 
               GC_MARKERS => 
               GC_MAXIMUM_HEAP_SIZE => 
               GC_NO_BLACKLIST_WARNING => 
               GC_NPROCS => 
               GC_PAUSE_TIME_TARGET => 
               GC_PRINT_ADDRESS_MAP => 
               GC_PRINT_BACK_HEIGHT => 
               GC_PRINT_STATS => 
               GC_PRINT_VERBOSE_STATS => 
               GC_RETRY_SIGNALS, => 
               GC_TRACE => 
               GC_UNMAP_THRESHOLD => 
               GC_USE_GETWRITEWATCH => 
               heap size => 1073741824
               java finalization => true
               max retries => 0
               number of collections => 1
               parallel => true
               time limit => 999999

o3 : HashTable

i4 : peek commandLine

o4 = {"/Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production/usr-dist/x86_64-Darwin-MacOS-10.12.4/bin/M2-binary", "--no-readline", "-e", "run \"stty -echo -onlcr\"", "-q", "--print-width", "264"}

i5 : run "printenv"
CAML_LD_LIBRARY_PATH=/Users/dan/.opam/4.02.1/lib/stublibs
GC_INITIAL_HEAP_SIZE=5G
NNTPSERVER=netnews.insightbb.com
SHELLUSED=-bash
MANPATH=/usr/local/MacGPG2/share/man:/opt/X11/share/man:/Users/dan/local/share/man:/Users/dan/local/man:/Users/dan/local/share/man:/Users/dan/local/man:/Applications/DjView.app/Contents/share/man:/opt/chef/embedded/lib/ruby/gems/1.9.1/gems/chef-11.8.2/distro/common/man:/usr/local/share/man:/usr/local/man:/usr/share/man:/opt/X11/share/man:
rvm_bin_path=/Users/dan/.rvm/bin
TERM_PROGRAM=Apple_Terminal
M2=/Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production/M2
MIZFILES=/capybara/share/mizar
TERM=dumb
SHELL=/bin/bash
TMPDIR=/var/folders/46/9b86vqxj4hjcngvy7kd7sb140000gn/T/
FOO=bar
PERL5LIB=/Users/dan/.opam/4.02.1/lib/perl5:/usr/local/lib/perl5/site_perl/:
Apple_PubSub_Socket_Render=/private/tmp/com.apple.launchd.yxAPtWsyJg/Render
CVSROOT=:ext:dan@ssh.math.uiuc.edu:/home/users/dan/local/cvs
TERM_PROGRAM_VERSION=388.1
WINDOWID=1
LD_LIBRARY_PATH_SCREEN=/Users/dan/local/lib:/Users/dan/home/lib:/Users/dan/local/lib:/Applications/DjView.app/Contents/lib:/Users/dan/Library/Haskell/ghc-7.6.3/lib/Agda-2.3.2.2/lib:/usr/local/lib:/usr/lib:/opt/X11/lib:
TERM_SESSION_ID=DA878B1F-52A3-4C10-8755-8747BD3852A1
LC_ALL=C
DOT_PROFILE=yes
OCAML_TOPLEVEL_PATH=/Users/dan/.opam/4.02.1/lib/toplevel
HISTFILESIZE=30
USER=dan
DEVEL_DAN=yes
_system_type=Darwin
AUTOCLEAN=no
rvm_path=/Users/dan/.rvm
ENCAPCONTACT=dan@math.uiuc.edu
TERMCAP=
PH_SERVER=ns.uiuc.edu:105
SSH_AUTH_SOCK=/private/tmp/com.apple.launchd.yoiXzRqqIt/Listeners
__CF_USER_TEXT_ENCODING=0x1F5:0x0:0x0
OLDMANPATH=/Applications/Macaulay2-1.10/share/man:
COLUMNS=265
PAGER=sed 's/.//g'
NEWMANPATH=/usr/local/MacGPG2/share/man:/opt/X11/share/man:/Users/dan/local/share/man:/Users/dan/local/man:/Users/dan/local/share/man:/Users/dan/local/man:/Applications/DjView.app/Contents/share/man:/opt/chef/embedded/lib/ruby/gems/1.9.1/gems/chef-11.8.2/distro/common/man:/usr/local/share/man:/usr/local/man:/usr/share/man:/opt/X11/share/man:
SYSTEM=Darwin
rvm_prefix=/Users/dan
OPAMUTF8MSGS=1
PATH=/Users/dan/.opam/4.02.1/bin:/usr/local/Cellar/coreutils/8.24/libexec/gnubin:/Users/dan/src/HoTT/UniMath/sub/coq/bin:/Users/dan/src/HoTT/UniMath/sub/coq/bin/dev:/Applications/Macaulay2-1.10/bin:/usr/local/MacGPG2/bin:/opt/X11/bin:/Users/dan/src/ocaml/camlp5.git:/Users/dan/src/ocaml/camlp5.git/etc:/Users/dan/src/ocaml/camlp5.git/main:/Users/dan/src/ocaml/ocaml.git:/System/Library/PrivateFrameworks/Apple80211.framework/Resources:/Users/dan/bin:/Users/dan/local/bin:/Users/dan/home/bin:/Users/dan/bin:/Users/dan/local/bin:/Applications/DjView.app/Contents/bin:/Users/dan/Library/Haskell/ghc-7.6.3/lib/Agda-2.3.2.2/bin:/Users/dan/Library/Haskell/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/X11/bin:/nowhere:/command:/Users/dan/.rvm/bin
PWD=/Users/dan/src/M2/M2-Macaulay2/M2/Macaulay2/packages
INPUTRC=/Users/dan/.inputrc
HOMEBREW_GITHUB_API_TOKEN=db1c36c3f5260be54c7676970c42d002fad7e715
EDITOR=emacsclient
LANG=en_US.UTF-8
_system_arch=x86_64
XPC_FLAGS=0x0
_system_version=10.12
XPC_SERVICE_NAME=0
rvm_version=1.25.27 (stable)
M2BUILDDIR=/Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/mac64-master-clang-production
EMACSLOADPATH=/Users/dan/src/ocaml/ocaml.git/emacs:/Users/dan/local/share/emacs/site-lisp:/Users/dan/local/share/emacs/site-lisp:/usr/local/share/emacs/site-lisp:/usr/share/emacs/site-lisp:
GPG_TTY=/dev/ttys000
SHLVL=3
HOME=/Users/dan
ODBCINI=/Users/dan/home/var/odbc.ini
rvm_ruby_string=system
PYTHONPATH=/Users/dan/local.Linux/lib/python2.2/site-packages
LOGNAME=dan
VISUAL=emacsclient
CVS_RSH=ssh
PATHS=set
PKG_CONFIG_PATH=/usr/local/Library/ENV/pkgconfig/10.9:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig:/opt/X11/lib/pkgconfig:
INFOPATH=/Applications/Macaulay2-1.10/share/info:/Users/dan/local/share/info:/Users/dan/local/info:/Users/dan/local/share/info:/Users/dan/local/info:/usr/local/share/info:/usr/local/info:/usr/share/info:
rvm_delete_flag=0
DISPLAY=/private/tmp/com.apple.launchd.xI0ubqWDAm/org.macosforge.xquartz:0
INSIDE_EMACS=25.2.1,comint
NO_AT_BRIDGE=1
RSYNC_RSH=ssh -x -a
_system_name=OSX
_=/usr/bin/printenv

o5 = 0

i6 : SimpleDoc

o6 = SimpleDoc

o6 : Package

i7 : viewHelp oo

i8 : print docTemplate 

doc ///
   Key
   Headline
   Usage
   Inputs
   Outputs
   Consequences
    Item
   Description
    Text
    Code
    Pre
    Example
    CannedExample
   Subnodes
   Caveat
   SeeAlso
///


i9 : viewHelp installPackage 

i10 : help sin

o10 = sin -- compute the sine
      ***********************

      Synopsis
      ========

        * Usage:sin x
        * Inputs:
            * x, a real number
        * Outputs:
            * a real number, the sine of x

      Description
      ===========

      +-------------------------+
      |i1 : sin (pi/2)          |
      |                         |
      |o1 = 1                   |
      |                         |
      |o1 : RR (of precision 53)|
      +-------------------------+

      Ways to use sin :
      =================

        * sin(CC)
        * sin(QQ)
        * sin(RR)
        * sin(ZZ)

o10 : DIV

i11 : applicationDirectory

o11 = applicationDirectory

o11 : FunctionClosure

i12 : applicationDirectory()

o12 = /Users/dan/Library/Application Support/Macaulay2/

i13 : code oo
stdio:13:1:(3): error: no method found for applying code to:
     argument   :  "/Users/dan/Library/Application Support/M. (of class String)

i14 : code o11

o14 = ../d/startup.m2.in:255:32-260:16: --source code:
           applicationDirectory = () -> (
                getenv "HOME" | "/" |
                if instance(applicationDirectorySuffix, Function)
                then applicationDirectorySuffix()
                else applicationDirectorySuffix
                );

i15 : "
      

i15 : code o11

o15 = ../d/startup.m2.in:255:32-260:16: --source code:
           applicationDirectory = () -> (
                getenv "HOME" | "/" |
                if instance(applicationDirectorySuffix, Function)
                then applicationDirectorySuffix()
                else applicationDirectorySuffix
                );

i16 : f=method()

o16 = f

o16 : MethodFunction

i17 : f():= ()->111
stdio:19:4:(3): error: expected 1, 2, 3, or 4 parameter types

i18 : installMethod ( f,  ()->111 )

o18 = {*Function[stdio:20:23-20:25]*}

o18 : FunctionClosure

i19 : f()

o19 = 111

i20 : 