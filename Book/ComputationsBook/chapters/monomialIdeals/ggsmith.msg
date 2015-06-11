Date: Fri, 26 Jan 2001 13:33:29 -0800 (PST)
From: Greg Smith <ggsmith@math.berkeley.edu>
To: <dan@rhenium.math.uiuc.edu>
cc: Dan Grayson <dan@math.uiuc.edu>, Serkan Hosten <serkan@math.berkeley.edu>
Subject: Changes to monomial ideals.
In-Reply-To: <20010126210545.2479.qmail@rhenium.home>

  This message is in MIME format.  The first part should be readable text,
  while the remaining parts are likely unreadable without MIME-aware tools.
  Send mail to mime@docserver.cac.washington.edu for more info.

---559023410-851401618-980544809=:23853
Content-Type: TEXT/PLAIN; charset=US-ASCII


Here is the list of changes for the monomial ideals
chapter.  Note that the diff -u command doesn't
work here so the line number below are approximate.
I've also attached the new chapter.tex file.

Cheers,
  Greg.


@@ line 11
- \newtheorem*{sproof}{Proof of
- Proposition~\ref{pro:complexity}}{\itshape}{\rmfamily}
+ \newtheorem*{sproof}{Proof of Proposition}{\itshape}{\rmfamily}

@@ line 52
- ideals in \Mtwo and demonstrate a wide range of applications.
+ ideals in \Mtwo and demonstrate a range of applications.

@@ line 56-57
- Algorithms are presented as \Mtwo functions.  We demonstrate these
- techniques by solving problems from different areas of mathematics.
+ Algorithms are presented as \Mtwo functions.  We illustrate these
+ methods by solving problems from various areas of mathematics.

@@ line 60
- functions on monomial ideals in \Mtwo.  To illustrate these functions,
+ functions on monomial ideals in \Mtwo.  To demonstrate these functions,

@@ line 90
- <<<monomialIdeal( a^2+a*b, a*b+3, b^2+d)>>>
+ <<<monomialIdeal(a^2+a*b, a*b+3, b^2+d)>>>

@@ line 92
- There are also several different methods of associating a monomial
+ There are also several methods of associating a monomial

@@ line 99
- <<<K = ideal( a^2, b^2, a*b+b*c)>>>
+ <<<K = ideal(a^2, b^2, a*b+b*c)>>>

@@ line 150
- To prove this theorem, we work with the fine grading and then
+ Following Stanley~\cite{MR98h:05001}, we work with the fine
+ grading and then

@@ line 183-188
-     h := apply(reverse toList(0..d), i ->
-          if first exponents leadMonomial N === i then (
-               c := leadCoefficient N;
-               N = N - leadTerm N;
-               c)
-          else 0);
+     h := apply(reverse toList(0..d), i -> N_(t^i));

@@ line 122
- heavily of these methods from commutative algebra.
+ heavily on these methods from commutative algebra.

@@ line 246-247
- ideal $I$ is called primary\index{ideal!primary} if $r_{1}r_{2} \in I$
- implies either $r_{1} \in I$ or $r_{2}^{\ell} \in I$ for some $\ell >
+ ideal $J$ is called primary\index{ideal!primary} if $r_{1}r_{2} \in J$
+ implies either $r_{1} \in J$ or $r_{2}^{\ell} \in J$ for some $\ell >

@@ line 318
- definition appears in Miller \cite{M}.  If $I \subseteq \bbbq[x_{1},
+ definition appears in Miller~\cite{M},~\cite{MR1779598}.  If
+ $I \subseteq \bbbq[x_{1},

@@ line 325-326
- x_{1}^{\beta_{1}}, \dotsc, x_{n}^{\beta_{n}}\rangle$ is an
- irredundant} \\ \text{irreducible component of $I$} \end{tabular}
+ x_{i}^{\beta_{i}} : \beta_{i} \geq 1\rangle$ is an
+ irredundant} \\ \text{irreducible component of $I$} \end{tabular}

@@ line 396
- $\langle x_{i} : x_{i} \not\in F^{c} \rangle$.
+ $\langle x_{i} : x_{i} \not\in F^{c} \rangle =
+ \langle x_{i} : x_{i} \in F \rangle$.

@@ line 486
- <<<clearAll;>>>
+ <<<erase symbol x>>>

@@ line 518
- $(0,1,1)$, and $(vi)$ the monomial $y^{2}z^{2}$.
+ $(0,2,1)$, and $(vi)$ the monomial $y^{2}z^{2}$.

@@ line 538
- (1, \{x,y\}), (1, \{x,z\}), (y,\{x,z\}), (z, \{y\}), (yz, \{x\}),
+ (1, \{x,y\}), (1, \{x,z\}), (y,\{x,z\}), (z, \{y\}), (y^{2}z, \{x\}),

@@ line 571-572
- The \Mtwo version of this algorithms takes the following form:
- <<<supp = r -> select(gens ring r, e -> r % e == 0);>>>
+ The \Mtwo version of this algorithm takes the following form:

@@ line 605
- <<<scan(stdPairs(I), P -> << endl << P << endl);>>>
+ <<<scan(time stdPairs I, P -> << endl << P << endl);>>>

@@ line 637
+ <<<time standardPairs I;>>>

@@ line 652
- <<<clearAll;>>>
+ <<<erase symbol x>>>
+ <<<erase symbol z>>>

@@ line 674
- \in \bbbn^{n}$ and $A \gamma = \delta$.  We write $\initial_{\,
+ \in \bbbn^{n}$ and $A \gamma = A \delta$.  We write $\initial_{\,

@@ line 686
- Sturmfels~\cite{MR97b:13034}
+ Sturmfels~\cite{MR97b:13034}.

@@ line 705
- \omega}(I_{A})$ covers all optimal solutions to all integer programs
+ \omega}(I_{A})$ cover all optimal solutions to all integer programs

@@ line 762
- <<<clearAll;>>>

@@ line 768-769
- Gr\"{o}bner basis calculations and initial ideals depend heavily on
- the choice of variables.  By making a generic change of coordinates
+ Gr\"{o}bner basis calculations and initial ideals depend heavily on
+ the given coordinate system.  By making a generic change of coordinates

@@ line 796
- In \Mtwo, the function {\tt isBorel} tests if a monomial ideal is
+ In \Mtwo, the function {\tt isBorel} tests whether a monomial ideal is

@@ line 815
- initial ideal}\index{gin}\index{ideal!generic initial} of $I$.
+ initial ideal}\index{ideal!generic initial} of $I$.

@@ line 829
+ This routine assumes that the random function generates a matrix in
+ the Zariski open subset $U$.  Since we are working over a field of
+ characteristic zero this occurs with probability one.

@@ line 984
- Finally, we use the distraction to connect the different classes.
+ Finally, we use the distraction to connect the classes.

@@ line 989
- <<<clearAll;>>>

@@ line 1038
- following function determines if a monomial ideal has the chain
+ following function determines whether a monomial ideal has the chain

@@ line 1040-1047
- <<<hasChainProperty = I -> (
-     L := ass(I);
-     radI := radical(I);
-     all(L, P -> (radI : (radI : P)) == P or (
-               gensP := first entries gens P;
-               all(gensP, r -> (
-                         Q := monomialIdeal delete(r, gensP);
-                         (I : (I : Q)) == Q)))));>>>
+ <<<hasChainProperty = I -> (
+     L := ass I;
+     radI := radical I;
+     all(L, P -> (radI : (radI : P)) == P or (
+               gensP := first entries gens P;
+               all(gensP, r -> (
+                         Q := monomialIdeal delete(r, gensP);
+                         I : (I : Q) == Q)))));>>>

@@ line 1052
- {\em An Initial ideal of a toric ideal.}  As mention above, Ho\c{s}ten
+ {\em An initial ideal of a toric ideal.}  As mentioned above, Ho\c{s}ten

@@ line 1053
- and Thomas proved that abt initial ideal of a toric ideal satisfies
+ and Thomas proved that any initial ideal of a toric ideal satisfies

@@ line 1054-1058
- phenomena.  We first include our function for generating a toric
- ideal (see the section on integer programming problems).
- <<<toBinomial = (b, S) -> (pos := 1_S; neg := 1_S; scan(#b, i -> if b_i
> 0 then pos = pos*S_i^(b_i) else if b_i < 0 then neg = neg*S_i^(-b_i));
pos - neg);>>>
-<<<toricIdeal = (A, w) -> (n := rank source A; S = QQ[x_1..x_n, Weights
=> w, MonomialSize => 16]; B := transpose matrix syz A; J := ideal
apply(entries B, b -> toBinomial(b, S)); scan(gens S, r -> J = saturate(J,
r)); J);>>>
-Next, consider the matrix $A$:
+ phenomena.  Consider the matrix $A$:

@@ line 1061
- {\em An Initial ideal of a prime ideal.}  Since toric ideals are
+ {\em An initial ideal of a prime ideal.}  Since toric ideals are

@@ line 1092
- <<<clearAll;>>>

@@ line 1095
- positive.  Hence, we use
+ positive.  Hence, we append to $A$ the sum of its rows to get a
matrix whose columns will serve as the degrees of the variables.

@@ line 1113
- <<<hasChainProperty = I -> (L := ass(I); radI := radical(I); all(L, P ->
(radI : (radI : P)) == P or (gensP := first entries gens P; all(gensP, r
-> (Q := monomialIdeal delete(r, gensP); (I : (I : Q)) == Q)))));>>>

Add to papers.bib:
+ @article {MR1779598,
+    AUTHOR = {Miller, Ezra},
+     TITLE = {The {A}lexander duality functors and local duality with
+              monomial support},
+   JOURNAL = {J. Algebra},
+  FJOURNAL = {Journal of Algebra},
+    VOLUME = {231},
+      YEAR = {2000},
+    NUMBER = {1},
+     PAGES = {180--234},
+      ISSN = {0021-8693},
+     CODEN = {JALGA4},
+   MRCLASS = {13Dxx},
+  MRNUMBER = {1 779 598},
+ }

