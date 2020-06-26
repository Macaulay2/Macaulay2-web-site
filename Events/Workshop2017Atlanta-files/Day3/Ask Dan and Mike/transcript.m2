el
+ FOO=bar
+ GC_INITIAL_HEAP_SIZE=5G
+ /Users/dan/src/M2/M2-Macaulay2/M2/BUILD/dan/builds.tmp/einsteinium-master/M2 --no-readline -e 'run "stty -echo -onlcr"' --print-width 111
Macaulay2, version 1.10.0.1
with packages: ConwayPolynomials, Elimination, IntegralClosure, InverseSystems, LLLBases, PrimaryDecomposition,
               ReesAlgebra, TangentCone

i1 : 1..111

o1 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
     ----------------------------------------------------------------------------------------------------------
     29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
     ----------------------------------------------------------------------------------------------------------
     55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
     ----------------------------------------------------------------------------------------------------------
     81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
     ----------------------------------------------------------------------------------------------------------
     106, 107, 108, 109, 110, 111)

o1 : Sequence

i2 : help benchmark 

o2 = benchmark -- accurate timing of execution
     *****************************************

     Synopsis
     ========

       * Usage:benchmark s
       * Inputs:
           * s, a string, a string containing Macaulay2 code
       * Outputs:
           * a real number, the number of seconds it takes to evaluate the code in s

     Description
     ===========

     Produces an accurate timing for the code contained in the string s.  The value returned is the number of
     seconds.

     +------------------------------+
     |i1 : benchmark "sqrt 2p100000"|
     |                              |
     |o1 = .000876663035513209      |
     |                              |
     |o1 : RR (of precision 53)     |
     +------------------------------+

     The snippet of code provided will be run enough times to register meaningfully on the clock, and the
     garbage collector will be called beforehand.

o2 : DIV

i3 : help "timing"

o3 = timing -- time a computation
     ****************************

     Description
     ===========

     timing e evaluates e and returns a list of type "Time" of the form {t,v}, where t is the number of seconds
     of cpu timing used, and v is the value of the the expression.


     The default method for printing such timing results is to display the timing separately in a comment below
     the computed value.

     +--------------------------------------+
     |i1 : timing 3^30                      |
     |                                      |
     |o1 = 205891132094649                  |
     |     -- .000003331 seconds            |
     |                                      |
     |o1 : Time                             |
     +--------------------------------------+
     |i2 : peek oo                          |
     |                                      |
     |o2 = Time{.000003331, 205891132094649}|
     +--------------------------------------+

     See also
     ========

       * "Time" -- the class of all timing results
       * "time" -- time a computation
       * "cpuTime" -- seconds of cpu time used since Macaulay2 began
       * "elapsedTiming" -- time a computation using time elapsed
       * "elapsedTime" -- time a computation using time elapsed

     For the programmer
     ==================

     The object "timing" is a keyword.

o3 : DIV

i4 : {-1}

o4 = {-1}

o4 : List

i5 : adsf " asdf
     

i5 : asdf

o5 = asdf

o5 : Symbol

i6 : "
     

i6 : -- "asdf"
     
     viewHelp sin

i7 : 