
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%   SHEAFHOM   Version of August 4, 1998   %%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- %%%%%%%%%%%%%%%%%% Homological Algebra %%%%%%%%%%%%%%%%%%%

-- All modules will be modules over the following fixed ring R.
-- Eventually, R should be ZZ.

R = QQ

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Here are some small patches.

ChainComplexMap // ChainComplexMap := (f,g) -> (
     map(g.source, f.source, k -> (f_k // g_k)))

isIsomorphism ChainComplexMap := f -> (
  ans := true;
  scan(keys f, i -> if class i === ZZ then (
    if not(isIsomorphism f_i) then ans = false));
  ans)

-- To avoid GradedModuleMap problems, I'll redefine homology to
-- produce a chain complex.  The following is cribbed from Dan's source
-- code, with the words after "new" changed.

spots := C -> select(keys C, i -> class i === ZZ)

homology(ChainComplex) := (C,opts) -> (
     H := new ChainComplex;
     H.ring = ring C;
     complete C;
     scan(spots C, i -> H#i = homology(i,C));
     H)
document { (homology,ChainComplex),
     TT "HH C", " -- produces the homology modules of the
     chain complex ", TT "C", " as a chain complex with trivial
     differential.  If you have not loaded Mark McConnell's Sheafhom file,
     then ", TT "HH C", " will be a graded module, which is the default in Macaulay 2.",
     SEEALSO {"GradedModule", "HH"}
     }

homology(ChainComplexMap) := (f,opts) -> (
     g := new ChainComplexMap;
     g.degree = f.degree;
     g.source = HH f.source;
     g.target = HH f.target;
     scan(spots f, i -> g#i = homology(i,f));
     g)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- For future reference:
-- coimageMap ChainComplexMap := f -> map(target(f), coimage(f), k -> f_k)

-- Here is the dual of the // operator.  It solves f = hg for h, as
-- long as the row-image of f is contained in the row-image of g.

--         *
--       ^ ^
--    f /  . h
--     /   .
--    /    .
--  * ---> *
--      g

coquotient := method()

coquotient (Matrix, Matrix) := (f,g) -> (
  transpose( transpose(f) // transpose(g)))

coquotient (ChainComplexMap, ChainComplexMap) := (f,g) -> (
  map(target f, target g, k -> coquotient(f_k,g_k)))

myInverse := method()

myInverse ChainComplexMap := f -> (
  -- Warning: if the i'th object in source f or target f is zero,
  -- but f_i is not an isomorphism, then the current code will not
  -- notice this fact, and the i'th entry in the inverse will be blank.
  map(source f, target f, i -> (
    -- assert(isIsomorphism f_i);
    (f_i)^(-1))))

-- ***** totalExteriorPower *****

totalExteriorPower = method()

document { quote totalExteriorPower,
     TT "totalExteriorPower(v)", " -- for a module ", TT "v",
     ", returns the chain complex
     formed by all its exterior powers.",
     PARA,
     TT "totalExteriorPower(f)", " -- for a matrix ", TT "f",
     ", returns the chain map
     formed by all its exterior powers.",
     PARA,
     "The total exterior power of ", TT "v", " is the chain complex ",
     TT "E", 
     " with
     ", TT "exteriorPower(i,v)", " in degree ", TT "-i", " for ",
     TT "i", " from ", TT "0", " to ", TT "rank v", ".  We
     use the negative index so we can think of ", TT "E", " as a cochain complex.
     The value of ", TT
     "E.comesFrom", " will be ", TT "v", ".",
     PARA,
     "The value of ", TT "totalExteriorPower(f)", " is the chain map
     from ", TT "totalExteriorPower(source f)", " to ", TT
     "totalExteriorPower(target f)", "
     that extends ", TT "f", ".",
     SEEALSO {"exteriorPower"}
     }

totalExteriorPower (Module) := v -> (
  if v.?totalExteriorPower then v.totalExteriorPower
    else v.totalExteriorPower = (
     ans := new ChainComplex;
     ans.ring = ring v;
     ans.comesFrom = v;
     scan(0..rank v, i -> ans#(-i) = exteriorPower(i,v));
     ans))

totalExteriorPower (Matrix) := f -> (
  if f.?totalExteriorPower then f.totalExteriorPower
    else f.totalExteriorPower = (
      map(totalExteriorPower target f,
	  totalExteriorPower source f,
	  i -> exteriorPower(-i,f))))

-- ***** DirectSum *****

-- The preferred way to make direct sums with user-installed indices
-- is something like directSum { myindex1 => A, myindex2 => B, ... }.
-- Or use directSum {A, B, ...} if you want the indices to be the
-- default 0, 1, ...

-- The following is a helper function which makes an interface with my
-- older method, where the arguments are two lists {A,B,...},
-- {myindex1, myindex2, ...}

makeDirectSumObject := method()

makeDirectSumObject (List, List) := (summands, indices) -> (
  directSum apply(summands, indices, (su, i) -> (i => su)))

makeDirectSumMorphism := method()

makeDirectSumMorphism (Module, Module, Function) := (ta, so, f) -> (
  map(ta, so, matrix table(indices ta, indices so, f)))

-- Here is another version of the previous function.  It worked
-- simultaneously for modules and complexes.  But it has two problems.
-- (1) We should define fmem := memoize f and call fmem(i,j) instead
-- of f(i,j).  (2) It uses inclusions and projections onto the
-- summands.  Replacing this code with the above version gave us a
-- speed-up, though not such a significant one.
--
--  sum flatten table(indices ta, indices so, (i,j) -> (
--    ta_[i] * f(i,j) * so^[j])))

makeDirectSumMorphism (ChainComplex, ChainComplex, Function) :=
  (ta, so, f) -> (
    f = memoize f;
    map(ta, so, k -> map(ta_k, so_k, matrix table(indices ta, indices so,
	                                          (i,j) -> (f(i,j))_k))))

makeDirectSumMorphismToSingleton := method()

makeDirectSumMorphismToSingleton (Module, Module, Function) := (ta, so, f) -> (
  map(ta, so, matrix {apply(indices so, f)}))

makeDirectSumMorphismToSingleton (ChainComplex, ChainComplex, Function) :=
  (ta, so, f) -> (
    f = memoize f;
    map(ta, so, k -> map(ta_k, so_k, matrix {apply(indices so, j -> (f(j))_k)})))

makeDirectSumMorphismFromSingleton := method()

makeDirectSumMorphismFromSingleton (Module, Module, Function) := (ta, so, f) -> (
     map(ta, so, matrix apply(indices ta, i -> {f(i)})))

makeDirectSumMorphismFromSingleton (ChainComplex, ChainComplex, Function) := 
  (ta, so, f) -> (
    f = memoize f;
    map(ta, so, k -> map(ta_k, so_k, matrix apply(indices ta, i -> {(f(i))_k}))))

-- ***** Truncation *****

-- When the ground ring is a field, flabbyTruncationMap automatically
-- uses the split version, where all you add on is the homology
-- groups.

chainToHomologyMap := method()

chainToHomologyMap (ChainComplex, ZZ) := (X, i) -> (
  -- Assumes X is a chain complex over a field, with every entry
  -- pruned.  Produces a map from X_i to HH_i(X) which extends the
  -- natural map on cycles Z_i.  Prunes its result.
  assert (isField ring X);
  cycleInclusion := prune map(X_i, kernel X.dd_i);
  retraction := cycleInclusion^(-1);
  assert ((retraction * cycleInclusion) == id_(source cycleInclusion));
  ZiToHi := prune map(HH_i(X), kernel X.dd_i);
  ZiToHi * retraction)

flabbyTruncationMapSplit := method()

flabbyTruncationMapSplit (ChainComplex, ZZ) := (x,k) -> (
  -- x is a chainComplex whose base ring is a field.  This finds a
  -- truncation where the cohomology in degrees > k is killed.  This
  -- really means the homology in degrees < -k is killed.  The
  -- truncation is performed by mapping the complex to a second
  -- complex, rh, containing the cohomology of x in (cohomology)
  -- degrees > k.  We take advantage of the fact that the base ring is
  -- a field to split the cocycle-into-cochain inclusion map and get a
  -- map from the cochain group x_-i to the cohomology HH^i(x).  We
  -- form the associated single complex t for this map. The value
  -- returned by the function is the canonical chainComplexMap t -> x.
  -- Using cohomology notation, this chainComplexMap is an isomorphism
  -- in degrees <= k and is the map 0 -> (...) in degrees > k.
  rh := new ChainComplex;
  rh.ring = ring x;
  xToRh := new ChainComplexMap;
  xToRh.degree = 0;
  xToRh.source = x;
  xToRh.target = rh;
  scan(keys x, i -> (
   if class i === ZZ then (
      if i < -k then (
        xToRh#i = chainToHomologyMap(x, i);
	rh#i = target xToRh_i))));
  scan(keys rh, i -> (
    if class i === ZZ then rh.dd#i = map(rh_(i-1), rh_i, 0)));
  trunc := new ChainComplex;
  trunc.ring = ring x;
  scan(keys x, i -> (
    if class i === ZZ then (
      trunc#i = x_i ++ rh_(i+1))));
  minI := min rh;
  if minI != infinity then (trunc#(minI-1) = x_(minI-1) ++ rh_minI);
  scan(keys x, l -> (
    if class l === ZZ then (
      trunc.dd#l = makeDirectSumMorphism(trunc_(l-1), trunc_l, (i,j) -> (
	if (j == 0) and (i == 0) then x.dd_l
	else if (j == 1) and (i == 1) then -rh.dd_(l+1)  -- a zero map
	else if (j == 0) and (i == 1) then xToRh_l
	else map(x_(l-1), rh_(l+1), 0))))));
  assert (trunc.dd^2 == 0);
  -- The answer:
  map(x, trunc, l -> trunc_l^[0]))

flabbyTruncationMap = method()

document { quote flabbyTruncationMap,
     TT "flabbyTruncationMap(C,k)", " -- truncates the chain complex
     ", TT "C", " so its cohomology is zero in cohomological degree 
     greater than ", TT "k", ".",
     PARA,
     "In terms of homological degree, the homology is zero in degree
     less than ", TT "-k", ".",
     PARA,
     "The truncation is performed by mapping the complex to a second
     complex, ", TT "rh", ", matching the original in degrees beyond ",
     TT "-k", " (with
     ", TT "image(C.dd_(-k))", " in the ", TT "(-k)", "th position).  We form the associated
     single complex ", TT "t", " for this map.  The value returned by the function
     is the canonical chain complex map from ", TT "t", " to ", TT
     "C", ".  Using cohomology
     notation, this chain complex map induces an isomorphism on cohomology in
     degrees less than or equal to ", TT "k", " and is the map
     ", TT "0 -> (...)", " in degrees greater than ", TT "k", ".",
     PARA,
     "When ", TT "C", " is defined over a field, we use a simpler
     definition of ", TT "rh", ": it is just the homology of ", TT
     "C", "
     in degrees less than ", TT "-k", ", and zero in the other degrees.
     The map from ", TT "C", " to ", TT "rh", " is given by extending the
     canonical map from cycles to homology."
     }

flabbyTruncationMap (ChainComplex, ZZ) := (x,k) -> (
 if isField(ring x) then flabbyTruncationMapSplit(x,k)
 else (
  rh := new ChainComplex;
  rh.ring = ring x;
  xToRh := new ChainComplexMap;
  xToRh.degree = 0;
  xToRh.source = x;
  xToRh.target = rh;
  rh#-k = image x.dd_-k;
  rh.dd#-k = prune map(x_(-k-1), rh_-k);
  xToRh#-k = prune map(rh_-k, x_-k, 1);
  rh#-k = prune rh_-k;
  -- assert (rh.dd#-k * xToRh#-k == x.dd_-k);
  scan(keys x, i -> (
   if class i === ZZ then (
     if i < -k then rh#i = x_i)));
  scan(keys rh, i -> (
    if class i === ZZ then (
      if i < -k then (rh.dd#i = x.dd_i;
	              xToRh#i = map(rh_i, x_i, 1)))));
  trunc := new ChainComplex;
  trunc.ring = ring x;
  scan(keys x, i -> (
    if class i === ZZ then (
      trunc#i = x_i ++ rh_(i+1))));
  minI := min rh;
  if minI != infinity then (trunc#(minI-1) = x_(minI-1) ++ rh_minI);
  scan(keys x, l -> (
    if class l === ZZ then (
      trunc.dd#l = makeDirectSumMorphism(trunc_(l-1), trunc_l, (i,j) -> (
	if (j == 0) and (i == 0) then x.dd_l
	else if (j == 1) and (i == 1) then -rh.dd_(l+1)
	else if (j == 0) and (i == 1) then xToRh_l
	else map(x_(l-1), rh_(l+1), 0))))));
  assert (trunc.dd^2 == 0);
  -- The answer:
  map(x, trunc, l -> trunc_l^[0])))

-- ***** Tensors *****

makeTensorObject = method()

document { quote makeTensorObject,
     TT "makeTensorObject(A,B)", " -- makes the tensor product of ", TT "A", "
     and ", TT "B", ", storing pointers to ", TT "A", " and ", TT "B", " inside the product.",
     PARA,
     "The functions sets ", TT "ans", " equal to ", TT "A ** B", ", sets ", TT "ans.from1", " equal to ", TT "A", "
     and ", TT "ans.from2", " equal to ", TT "B", ", and returns ", TT "ans", ".",
     SEEALSO{quote **}
     }

makeTensorObject (Thing, Thing) := (a,b) -> (
  ans := a ** b;
  ans.from1 = a;
  ans.from2 = b;
  ans)

extractFFromIdTensorF := method()

extractFFromIdTensorF ChainComplexMap := g -> (
    -- g is a chainComplexMap (A tensor B) --> (A tensor C).  
    -- The function assumes that g is the tensor product of two maps,
    -- the first being the identity A -> A and the second a map
    -- f : B --> C.  We also assume the degree-0 term of A has
    -- rank 1.  The function returns f.  The values of .from1 and
    -- .from2 for both the source and target of g should be set
    -- correctly.
    a := (source g).from1;
    b := (source g).from2;
    c := (target g).from2;
    assert (a === (target g).from1);
    assert (rank a_0 == 1);
    assert (a.dd_0 == 0);
    a0 := a_0[0];
    a0In := map(a, a0, k -> 1);
    a0Out := map(a0, a, k -> 1);
    a0b := a0 ** b;
    a0c := a0 ** c;
    -- The next two really(!) assume the map on a_0 is the identity.
    bA0b := map(a0b, b, i -> map(a0b_i, b_i, 1));
    a0cC := map(c, a0c, i -> map(c_i, a0c_i, 1));
    -- The answer:
    a0cC * (a0Out ** id_c) * g * (a0In ** id_b) * bA0b)

distributeTensorOverExteriorAlgebraOfDirectSum = method()

document { quote distributeTensorOverExteriorAlgebraOfDirectSum,
     TT "distributeTensorOverExteriorAlgebraOfDirectSum(X,Y)",
     PARA,
     "The first argument is ", TT "totalExteriorPower(A ++ B)", " for some
     modules ", TT "A", " and ", TT "B", ", and the second is
     ", TT "makeTensorObject(totalExteriorPower(A), totalExteriorPower(B))", ".
     The function outputs the canonical isomorphism from the second
     argument to the first.",
     SEEALSO{"totalExteriorPower", "++", "makeTensorObject"}
     }

distributeTensorOverExteriorAlgebraOfDirectSum (ChainComplex, ChainComplex) :=
  (wab, waTenswb) -> (
    ab := wab.comesFrom;
    assert (ab.components#0 == waTenswb.from1.comesFrom);
    assert (ab.components#1 == waTenswb.from2.comesFrom);
    map(wab, waTenswb, k -> matrix {toList apply(waTenswb_k.indices,
		                  (i,j) -> (wedgeProduct(-i,-j,ab) * (
		                  exteriorPower(-i,ab_[0]) **
				  exteriorPower(-j,ab_[1]))))}
	      ))

flabbyInclusionOfSubspaces := method()

flabbyInclusionOfSubspaces (ChainComplexMap, ChainComplexMap) := (f,g) -> (
  -- Solves for h in f // g = h, in a setting where g is not
  -- injective, but where h is uniquely determined up to q.i.(?) by the
  -- extra requirement that it be a chain map.  Let A = source(f), B =
  -- source(g), C = target(f) = target(g).  We must set h_i,
  -- as i runs from high to low, equal to the downward-slanting
  -- diagonal arrow in
  --
  --                            dA_(i+1)
  --                   A_(i+1) ----------> A_i
  --                     |               /  |
  -- dB_(i+1) * h_(i+1)  |         /        | f_i
  --                     v   /              v
  --                    B_i -------------> C_i
  --                              g_i
  --
  -- with the requirement that both triangles commute.
  A := source f;
  B := source g;
  C := target f;
  h := new ChainComplexMap;
  h.degree = 0;
  h.ring = ring A;
  h.source = A;
  h.target = B;
  assert (C == target g);
  i := min(max A, max B);
  bottom := max(min A, min B);
  while i >= bottom do (
    approx1 := f_i // g_i;
    newLeft := ((B.dd)_(i+1) * h_(i+1)) - (approx1 * (A.dd)_(i+1));
    approx2 := coquotient(newLeft, (A.dd)_(i+1));
    -- The answer for this pass of the loop:
    ans := approx1 + approx2;
    assert(f_i == g_i * ans);  -- does solve h = f // g
    assert(((B.dd)_(i+1) * h_(i+1)) == (ans * (A.dd)_(i+1))); -- is coch. map
    h#i = ans;
    -- Loop back.
    i = i - 1);
  -- The final answer:
  h)

kunneth = method()

document { quote kunneth,
     TT "kunneth(AB)", " -- the Kunneth isomorphism",
     PARA,
     TT "AB", " is a chain complex which is a tensor product of two chain
     complexes ", TT "A", ", ", TT "B", ", which are accessible as ", TT "AB.from1", " and ", TT "AB.from2", ".
     (To arrange this, define ", TT "AB", " to be ", TT "makeTensorObject(A,B)", ".)
     The function returns the Kunneth map from ", TT "HH(A) ** HH(B)", " to
     ", TT "HH(AB)", ".  This map is always injective.  It is an isomorphism iff
     ", TT "Tor_1(HH_i(A), HH_j(B))", " vanishes for all ", TT "i", " and ", TT "j", ".  The present
     version of the function actually assumes more: it assumes the map from
     ", TT "kernel A.dd", " to ", TT "HH(A)", " can be split back, and ditto for ", TT "B", ".
     For instance, the present version works correctly when the ground ring
     is a field.",
     SEEALSO {"makeTensorObject", "**", "HH"}
     }

kunneth ChainComplex := AB -> (
  A := AB.from1;
  B := AB.from2;
  ZHAmap := prune map(HH(A), kernel A.dd);
  ZHBmap := prune map(HH(B), kernel B.dd);
  ZAZBmap := prune(map(A, kernel A.dd)) ** prune(map(B, kernel B.dd));
  assert(0 == AB.dd * ZAZBmap);
  ZAZBtoZAB := ZAZBmap // prune(map(AB, kernel AB.dd));
  ans := prune(map(HH(AB), kernel AB.dd)) *
	 ZAZBtoZAB *
	 (map(source ZHAmap, target ZHAmap, k -> (ZHAmap_k)^-1) ** 
	  map(source ZHBmap, target ZHBmap, k -> (ZHBmap_k)^-1));
  ans.source = makeTensorObject(prune HH(A), prune HH(B));
  if isField(AB.ring) then assert(isIsomorphism ans)
    else assert(isInjective ans);
  ans)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%% SHEAVES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- RANKED POSETS ------------------------------------------------

Poset = new Type of Set

document { Poset,
     TT "Poset", " -- the class of all partially ordered sets.",
     PARA,
     "Let ", TT "C", " be a class of objects.  A poset ",
     TT "P", " of
     elements of type ", TT "C", " is just a set of
     elements of type ", TT "C", "; the set can be created with the ",
     TO "poset", "
     function.  The user should make sure the binary operator ", TO "?", " 
     is defined for elements of class ", TT "C", ".  Then the
     operators ", TO "<", ",", TO "<=", ",", TO ">", ",", " and ", TO ">=", " will
     compute the partial order relation for elements of ", TT "P", ".",
     SEEALSO {"openStar", "puncturedStar", "RankedPoset", "doVertices"}
     }

-- Constructor

poset = method()

document { quote poset,
     TT "poset x", " -- creates an element of the class Poset from a
     set or list ", TT "x", ".",
     SEEALSO {"Poset", "RankedPoset"}
     }

poset Set := x -> new Poset from x

poset List := x -> poset set x

-- Stars

openStar = method()

document { quote openStar,
     TT "openStar(c,P)", " -- finds the open star of ", TT "c", " in
     the poset ", TT "P", ".",
     PARA,
     "When ", TT "P", " is a member of the class ", TO "Poset", ", and ", TT "c", " is an element of ", TT "P", ",
     this returns the poset of all elements ", TT "c1", " of ", TT "P", " satisfying ", TT "c <= c1", ".  
     If ", TT "P", " is in the class ", TO "RankedPoset", " or ", TO "Fan", ", the open star will be in
     that class also.",
     SEEALSO {"puncturedStar"}
     }

openStar (Thing,Poset) := (c,P) -> poset select(toList P, elt -> c <= elt)

puncturedStar = method()

document { quote puncturedStar,
     TT "puncturedStar(c,P)", " -- finds the punctured open star of ", TT "c", " in
     the poset ", TT "P", ".",
     PARA,
     "When ", TT "P", " is a member of the class ", TO "Poset", ", and ", TT "c", " is an element of ", TT "P", ",
     this returns the poset of all elements ", TT "c1", " of ", TT "P", " satisfying ", TT "c < c1", ".  
     If ", TT "P", " is in the class ", TO "RankedPoset", " or ", TO "Fan", ", the
     punctured star will be in that class also.",
     SEEALSO {"openStar"}
     }

puncturedStar (Thing,Poset) := (c,P) -> poset select(toList P, elt -> c < elt)

RankedPoset = new Type of Poset

document { RankedPoset,
     TT "RankedPoset", " -- the class of all ranked posets.",
     PARA,
     TT "RankedPoset", " is a subtype of ", TT "Poset", ".  If ", TT "P", " is in the class
     ", TT "RankedPoset", ", then for every ", TT "c0", " and ", TT "cn", " in ", TT "P", ", any maximal chain
     ", TT "c0 < c1 < ... < cn", " must have the same number of elements, say
     ", TT "n+1", ".  The user should make sure the function ", TT "rank(c,P)", " is defined
     so that for the element ", TT "cn", " above, ", TT "rank(cn,P)", " is ", TT "n", ".",
     SEEALSO {"rankedPoset", "Poset", "(rank,Thing,RankedPoset)", "openStar", "puncturedStar", "doVertices", "doEdges"}
     }

rankedPoset = method()

document { quote rankedPoset,
     TT "rankedPoset x", " -- creates an element of the class
     RankedPoset from a set or list ", TT "x", ".",
     SEEALSO {"RankedPoset", "Poset"}
     }

rankedPoset Set := x -> new RankedPoset from x

rankedPoset List := x -> rankedPoset set x

openStar (Thing, RankedPoset) := (c,P) -> (
  rankedPoset select(toList P, elt -> c <= elt))

puncturedStar (Thing, RankedPoset) := (c,P) -> (
  rankedPoset select(toList P, elt -> c < elt))

lub = method()

doVertices = method()

document { quote doVertices,
     TT "doVertices(P, f)", " -- call ", TT "f", " on every element of the poset ", TT "P", ".",
     PARA,
     TT "P", " is in the class ", TO "Poset", ".  For each
     element ", TT "c", " of ", TT "P", ", the value ", TT "f(c)",
     " is computed for its side effects.
     ", TT "doVertices", " returns an unspecified value.",
     SEEALSO {"doEdges"}
     }

doVertices (Poset, Thing) := (U,f) -> scan(toList U, f)

doEdges = method()

document { quote doEdges,
     TT "doEdges(P, f)", " -- call ", TT "f", " on every edge of the ranked poset ", TT "P", ".",
     PARA,
     TT "P", " is in the class ", TO "RankedPoset", ".  By an edge, we
     mean a pair ", TT "(a,b)", " of elements of ", TT "P", " with ", TT "b < a", " and
     ", TT "rank(a,P) == rank(b,P) + 1", ".  For each edge, the value
     ", TT "f(a,b)", " is computed for its side effects.
     ", TT "doEdges", " returns an unspecified value.",
     SEEALSO {"doVertices", "(rank,Thing,RankedPoset)"}
     }

doEdges (RankedPoset, Thing) := (U,f) -> (
  scan(toList U, c -> (
    scan(select(toList U, c1 -> (c1 < c and
                                rank(c1,U) + 1 == rank(c,U))),
         c1 -> f(c,c1)))))

-- COMBINATORIAL SHEAVES ----------------------------------------

CombinatorialSheaf = new Type of MutableHashTable

document { CombinatorialSheaf,
     TT "CombinatorialSheaf", " -- the class of combinatorial sheaves.",
     PARA,
"A combinatorial sheaf ", TT "F", " is an object of class ", TO "MutableHashTable", " with
at least the following three slots:
   (.) ", TT "F.rp", ", which is of class ", TO "RankedPoset", ";
   (.) ", TT "F.verts", ", a mutable hash table keyed by the elements in ", TT "F.rp", ";
   (.) ", TT "F.edges", ", a mutable hash table keyed by the pairs ", TT "(tau,sigma)", "
       where ", TT "tau", " and ", TT "sigma", " are elements in ", TT "F.rp", " with ", TT "sigma < tau", "
       and consecutive rank.",
       PARA,
TT "F.rp", " is thought of as having the topology generated by the basis
", TT "openStar(sigma,F.rp)", ", for all ", TT "sigma", " in ", TT "F.rp", ".  The space of sections of
the sheaf over ", TT "U = openStar(sigma,F.rp)", " is the same as the stalk at
", TT "sigma", ", since ", TT "U", " is the smallest open set containing ", TT "sigma", ".  This stalk
is computed by ", TT "stalk(sigma,F)", ", and is stored in ", TT "(F.verts)#sigma", ".  If
", TT "sigma < tau", ", then ", TT "openStar(sigma,F.rp)", " contains ", TT "openStar(tau,F.rp)", ",
so there is a restriction map in the sheaf, and hence a map from
", TT "stalk(sigma,F)", " to ", TT "stalk(tau,F)", ".  The latter maps are also computed by
", TT "stalk(sigma,F)", ", and are stored in ", TT "(F.edges)#(tau,sigma)", ".",
       PARA,
"It is assumed that the whole arrangement is functorial: given any two
chains in the poset with the same initial and terminal points, the
compositions of the morphisms along the chains should be equal.
(These compositions are found using ", TO "internalMap", ".)  It is taken for
granted that ", TT "F", " is a sheaf rather than just a presheaf.",
     PARA,
"WARNING: the current code assumes that all the minimal elements of ", TT "F.rp", "
have the same rank (namely, 0).  For toric varieties, for example, this
condition will be met if we use complete fans, or if the union of
the interiors of the codimension-0 and codimension-1 cones in the 
fan is connected.",
     PARA,
"A complex of sheaves is constructed as a sheaf of complexes: every element
of ", TT "F.verts", " is of class ", TO "ChainComplex", ", and every element of ", TT "F.edges", " is
of class ", TO "ChainComplexMap", ".",
     PARA,
"There will also be a slot ", TT "F.globalSectionsWithEdgeMorphisms", ",
after this is requested the first time by the functions
", TO "globalSectionsWithEdgeMorphisms", " or ", TO "globalSections", ".",
     SEEALSO {"CombinatorialSheafMap", "hypercohomology", "stalkCohomology", "kernelMap", "cokernelMap", "stalk", "(rank,Thing,RankedPoset)", "openStar"}
     }

stalk = method()

document { quote stalk,
     TT "stalk(c,S)", " -- the stalk of ", TT "S", " at ", TT "c", ".",
     PARA,
     TT "S", " is of class ", TO "CombinatorialSheaf", ", and ", TT "c", " is an element of the
     underlying ranked poset ", TT "S.rp", ".  The function returns the stalk
     of ", TT "S", " at ", TT "c", ".",
     PARA,
     "If the stalk is not yet known, the function should
     compute and store it, doing something recursive with the part of the
     sheaf lying over ", TT "puncturedStar(c, S.rp)", ".  It should also
     compute and store the values of ", TT "(S.edges)#(d,c)", " for all ", TT "d", "
     with ", TT "c < d", " and of consecutive rank.",
     SEEALSO{"RankedPoset", "puncturedStar"}
     }
  
stalk (Thing, CombinatorialSheaf) := (c,F) -> (F.verts)#c

globalSections = method()

document { quote globalSections,
     TT "globalSections(x)", " -- the global sections functor for items
of class ", TO "CombinatorialSheaf", " or ", TO "CombinatorialSheafMap", ".",
     PARA,
"If ", TT "x", " is a combinatorial sheaf, the function returns its space of global
sections.  If ", TT "x", " is a morphism of combinatorial sheaves, the function
returns the induced morphism on global sections.",
     SEEALSO{"globalSectionsWithEdgeMorphisms"}
     }

globalSections CombinatorialSheaf := F -> (
  (globalSectionsWithEdgeMorphisms F)#0)

globalSectionsWithEdgeMorphisms = method()

document { quote globalSectionsWithEdgeMorphisms,
     TT "globalSectionsWithEdgeMorphisms(F)", " -- returns the essential
data for a global sections computation.",
     PARA,
"This is the main workhorse for the function ", TO "globalSections", ".
", TT "F", " is of class ", TO "CombinatorialSheaf", ".  The function computes ", TT "A", ", the
space of global sections of ", TT "F", ".  Let ", TT "tau1", ", ", TT "tau2", ", ... be the elements of
minimal rank in the underlying ranked poset ", TT "F.rp", ", and let
", TT "ftau1", ", ", TT "ftau2", ", ... be the appropriate maps from ", TT "A", " to
", TT "stalk(tau1,F)", ", ", TT "stalk(tau2,F)", ", etc.  The function returns the list
", TT "{A, (tau1, ftau1), (tau2, ftau2), ...}", ".",
     SEEALSO{"RankedPoset", "stalk"}
     }

globalSectionsWithEdgeMorphisms CombinatorialSheaf := F -> (
 if F.?globalSectionsWithEdgeMorphisms then F.globalSectionsWithEdgeMorphisms
 else F.globalSectionsWithEdgeMorphisms = (
  if #F.rp == 1 then (
      c := (toList F.rp)#0;
      A := stalk(c,F);
      {A, (c, id_A)} )
    else (
    UMinimal := select(toList F.rp, c1 -> (rank(c1, F.rp) == 0));
    UTriples := {};
    -- The double loop is over all pairs c1, c2 of distinct elts in
    -- UMinimal.  It appends (c1, c2, lub(c1,c2)) onto UTriples
    -- iff the intersection has rank 1.
    i := 0;
    while i < #UMinimal do (
      j := i+1;
      while j < #UMinimal do (
        c3 := lub(UMinimal#i, UMinimal#j);
        if rank(c3, F.rp) == 1 then
            UTriples = append(UTriples, (UMinimal#i, UMinimal#j, c3));
        j = j+1);
      i = i+1);
    so := makeDirectSumObject(apply(UMinimal, c1 -> stalk(c1, F)), UMinimal);
    ta := makeDirectSumObject(apply(UTriples, tri -> stalk(tri#2, F)),
                              UTriples);
    bigMap := makeDirectSumMorphism(ta, so,
	        (tri,j) -> (
		  if j === tri#0 then internalMap(tri#2, tri#0, F)
		  else if j === tri#1 then -internalMap(tri#2, tri#1, F)
                  else map(ta.components#(ta.indexComponents#tri),
		           so.components#(so.indexComponents#j),
			   k -> 0)));
    globalSection := kernel bigMap;
    globalSectionInclusion := prune map(bigMap.source, globalSection);
    globalSection = source globalSectionInclusion;
    edgeMorphisms := apply(UMinimal, tau -> (
	                    (tau, (so^[tau] * globalSectionInclusion))));
    prepend(globalSection, edgeMorphisms))))

internalMap = method()

document { quote internalMap,
     TT "internalMap(tau, sigma, F)", " -- restriction maps within
     the combinatorial sheaf ", TT "F", ".",
     PARA,
"See the documentation on the class ", TO "CombinatorialSheaf", " for
why the section of ", TT "F", " over ", TT "openStar(c,F.rp)", "
is the stalk of ", TT "F", " at ", TT "c", ".  The function ", TT "internalMap", " assumes
that ", TT "sigma <= tau", " in ", TT "F.rp", ", and returns the map in the sheaf
from ", TT "stalk(sigma,F)", " to ", TT "stalk(tau,F)", ".",
     SEEALSO{"stalk", "openStar", "RankedPoset"}
     }

internalMap (Thing, Thing, CombinatorialSheaf) := (tau, sigma, F) -> (
  assert (sigma <= tau);
  if tau === sigma then id_(stalk(tau,F))
  else (
    rs1 := rank(sigma, F.rp) + 1;
    if rank(tau, F.rp) == rs1 then F.edges#(tau,sigma)
      else (
	lyst := select(1, toList puncturedStar(sigma, F.rp), up -> (
		  up < tau and rank(up, F.rp) == rs1));
        assert (#lyst == 1);
	upsilon := lyst#0;
	internalMap(tau, upsilon, F) * F.edges#(upsilon, sigma))))

hypercoh = method()

document { quote hypercoh,
     TT "hypercoh x", " -- the hypercohomology functor for items
     of class ", TO "CombinatorialSheaf", " or ", TO "CombinatorialSheafMap", ".",
     PARA,
"If ", TT "F", " is a combinatorial sheaf, ", TT "hypercoh F", " returns the cohomology of
the global sections of ", TT "F", ".  If ", TT "f", " is a morphism of combinatorial sheaves,
", TT "hypercoh f", " returns the induced morphism on cohomology of global
sections.",
     PARA,
"The system assumes that the combinatorial sheaves involved are
complexes of Gamma-acyclic sheaves, such as injective, flabby, or soft
sheaves.  For example, all the sheaves produced by the current toric
variety code are injective when the ground ring is the rationals, and
flabby when it's the integers.",
     PARA,
     "An equivalent name for this function is ", TT "hypercohomology", ".",
     SEEALSO{"globalSections"}
     }

hypercoh CombinatorialSheaf := F -> prune HH globalSections F

hypercohomology = method()

hypercohomology Thing := x -> hypercoh x

document { quote hypercohomology,
     TT "hypercohomology", " -- another name for ", TO "hypercoh"
     }

CombinatorialSheaf ** CombinatorialSheaf := (A,B) -> (
  if not(A.rp === B.rp) then
    error "Can form tensor product of sheaves only on the same ranked poset.";
  ans := new CombinatorialSheaf;
  ans.rp = A.rp;
  ans.verts = new MutableHashTable;
  ans.edges = new MutableHashTable;
  doVertices(ans.rp, c -> (
	      (ans.verts)#c = stalk(c,A) ** stalk(c,B)));
  doEdges(ans.rp, (c,c1) -> (
	    (ans.edges)#(c,c1) = (A.edges)#(c,c1) ** (B.edges)#(c,c1)));
  ans)

makeTensorObject (CombinatorialSheaf, CombinatorialSheaf) := (A,B) -> (
  if not(A.rp === B.rp) then
    error "Can form tensor product of sheaves only on the same ranked poset.";
  ans := new CombinatorialSheaf;
  ans.rp = A.rp;
  ans.verts = new MutableHashTable;
  ans.edges = new MutableHashTable;
  doVertices(ans.rp, c -> (
	      (ans.verts)#c = makeTensorObject(stalk(c,A), stalk(c,B))));
  doEdges(ans.rp, (c,c1) -> (
	    (ans.edges)#(c,c1) = (A.edges)#(c,c1) ** (B.edges)#(c,c1)));
  ans.from1 = A;
  ans.from2 = B;
  ans)

stalkCohomology = method()

document { quote stalkCohomology,
     TT "stalkCohomology x", " -- the stalk cohomology functor for items
     of class ", TO "CombinatorialSheaf", " or ", TO "CombinatorialSheafMap", ".",
     PARA,
"If ", TT "F", " is a combinatorial sheaf, ", TT "stalkCohomology F", " is the combinatorial sheaf
obtained by taking cohomology in each stalk of ", TT "F", ".  If ", TT "f", " is a morphism
of combinatorial sheaves, ", TT "stalkCohomology f", " is the corresponding
morphism."
     }

stalkCohomology CombinatorialSheaf := F -> (
  HF := new CombinatorialSheaf;
  HF.rp = F.rp;
  HF.verts = new MutableHashTable;
  HF.edges = new MutableHashTable;
  doVertices(HF.rp, c -> (
    (HF.verts)#c = prune HH stalk(c,F)));
  doEdges(HF.rp, (c,c1) -> (
    (HF.edges)#(c,c1) = prune HH (F.edges)#(c,c1)));
  HF)

-- COMBINATORIAL SHEAF MAPS ----------------------------------------

CombinatorialSheafMap = new Type of MutableHashTable

document { CombinatorialSheafMap,
     TT "CombinatorialSheafMap", " -- the class of morphisms of
     combinatorial sheaves.",
     PARA,
"A combinatorial sheaf map ", TT "rho", " is a mutable hash table with at least the
following three slots:
   (.) ", TT "rho.source", " and
   (.) ", TT "rho.target", ", which are of class ", TT "CombinatorialSheaf", " (call them ", TT "S", ",
	", TT "T", ", resp.) and have the same underlying ranked poset ", TT "P", "; and
   (.) ", TT "rho.stalkMaps", ", a mutable hash table keyed by the elements of ", TT "P", ".",
   PARA,
"The value of ", TT "(rho.stalkMaps)#c", " is a morphism from ", TT "stalk(c,S)", "
to ", TT "stalk(c,T)", ".  It is assumed these stalk maps commute with the
maps in ", TT "S.edges", " and ", TT "T.edges", ".  The stalk maps are computed and stored by
", TT "stalkMap(c,rho)", ".",
   PARA,
"Maps between combinatorial sheaves with different underlying ranked
posets are currently not supported.",
   PARA,
"After it is first requested, there will also be a slot
", TT "rho.globalSections", " holding the map from ", TT "globalSections S", "
to ", TT "globalSections T", ".",
     SEEALSO{"CombinatorialSheaf", "RankedPoset", "stalkMap", "globalSections", "stalk"}
     }

stalkMap = method()

document { quote stalkMap,
     TT "stalkMap(c,rho)", " -- the map on stalks at ", TT "c", ", for
     combinatorial sheaf maps rho.",
     PARA,
TT "rho", " is of class ", TO "CombinatorialSheafMap", ", with source ", TT "S", " and target ", TT "T", ",
and ", TT "c", " is an element of the underlying ranked poset ", TT "S.rp", " (same as ", TT "T.rp", ").
The function returns the map from ", TT "stalk(c,S)", " to ", TT "stalk(c,T)", " that ", TT "rho", "
determines.  If this map is not yet known, the function computes and
stores it.",
     SEEALSO{"CombinatorialSheaf", "stalk"}
     }
  
globalSections CombinatorialSheafMap := rho -> (
 if rho.?globalSections then rho.globalSections
 else rho.globalSections = (
  S := rho.source;
  T := rho.target;
  assert (S.rp === T.rp);
  Slyst := globalSectionsWithEdgeMorphisms S;
  Tlyst := globalSectionsWithEdgeMorphisms T;
  Slyst1 := drop(Slyst,1);
  Tlyst1 := drop(Tlyst,1);
  sumS := makeDirectSumObject(apply(Slyst1, pair -> (pair#1).target),
			      apply(Slyst1, pair -> pair#0));
  sumT := makeDirectSumObject(apply(Tlyst1, pair -> (pair#1).target),
			      apply(Tlyst1, pair -> pair#0));
  ff := makeDirectSumMorphism(sumT, sumS, (i,j) -> (
	    if i === j then stalkMap(i,rho)
	    else map(stalk(i,T), stalk(j,S), k -> 0)));
  Smap := makeDirectSumMorphismFromSingleton(sumS, Slyst#0, i -> (
	    -- slow list technique!
	    a := select(1, Slyst1, pair -> (pair#0 === i));
	    (a#0)#1));
  Tmap := makeDirectSumMorphismFromSingleton(sumT, Tlyst#0, i -> (
	    -- slow list technique!
	    a := select(1, Tlyst1, pair -> (pair#0 === i));
	    (a#0)#1));
  ffSmap := ff * Smap;
  ans := ffSmap // Tmap;
  assert (ffSmap == Tmap * ans);
  ans))

hypercoh CombinatorialSheafMap := f -> prune HH globalSections f

kernelMap = method()

kernelMap CombinatorialSheafMap := f -> (
  F := source f;
  G := target f;
  S := F.rp;
  ansObj := new CombinatorialSheaf;
  ansObj.rp = S;
  ansObj.verts = new MutableHashTable;
  ansObj.edges = new MutableHashTable;
  ansMap := new CombinatorialSheafMap;
  ansMap.source = ansObj;
  ansMap.target = F;
  ansMap.stalkMaps = new MutableHashTable;
  doVertices(S, c -> (
    g := prune map(stalk(c,F), kernel stalkMap(c,f));
    (ansObj.verts)#c = source g;
    (ansMap.stalkMaps)#c = g));
  doEdges(S, (c,c1) -> (
    (ansObj.edges)#(c,c1) =
      ((F.edges)#(c,c1) * (ansMap.stalkMaps)#c1) //
        (ansMap.stalkMaps)#c));
  ansMap)

kernel CombinatorialSheafMap := (f, options) -> source kernelMap f

cokernelMap = method()

cokernelMap CombinatorialSheafMap := f -> (
  G := source f;
  F := target f;
  S := F.rp;
  ansObj := new CombinatorialSheaf;
  ansObj.rp = S;
  ansObj.verts = new MutableHashTable;
  ansObj.edges = new MutableHashTable;
  ansMap := new CombinatorialSheafMap;
  ansMap.source = F;
  ansMap.target = ansObj;
  ansMap.stalkMaps = new MutableHashTable;
  doVertices(S, c -> (
    g := prune map(cokernel stalkMap(c,f), stalk(c,F));
    (ansObj.verts)#c = target g;
    (ansMap.stalkMaps)#c = g));
  doEdges(S, (c1,c) -> (
    (ansObj.edges)#(c1,c) =
      coquotient((ansMap.stalkMaps)#c1 * (F.edges)#(c1,c),
	         (ansMap.stalkMaps)#c)));
  ansMap)

cokernel CombinatorialSheafMap := f -> target cokernelMap f

stalkCohomology CombinatorialSheafMap := f -> (
     ans := new CombinatorialSheafMap;
     ans.source = stalkCohomology source f;
     ans.target = stalkCohomology target f;
     ans.stalkMaps = new MutableHashTable;
     doVertices((source f).rp, c -> (
       (ans.stalkMaps)#c = prune HH stalkMap(c,f)));
     ans)

-- PERVERSITIES -------------------------------------------------
     
zeroPerversity = k -> 0

middlePerversity = k -> max(0, k//2 - 1)    -- lower middle

topPerversity = k -> max(0, k-2)

sublogPerversity = k -> (if k == 2 then 0 else k//2 - 2)

logPerversity = k -> (if k == 2 then 0 else k//2)

-- %%%%%%%%%%%%%%%%%%%%%% TORIC VARIETIES %%%%%%%%%%%%%%%%%%%%%%%%

-- CONES -----------------------------------------------------------

-- Currently, we assume that the ground ring for all computation with
-- cones and fans is ZZ.

-- Dan says that vectors aren't reliable.  One should use a list of
-- elements of ZZ in preference to a vector in ZZ^n.  Hence, a cone is
-- a set of n-element lists of elements of ZZ.

-- All cones must contain the zero vector in ZZ^n.  This is a kludge
-- to ensure that the zero cone is not the empty set, so that it
-- always knows the rank of the ZZ^n it's the zero vector of.

Cone = new Type of Set

document { Cone,
     TT "Cone", " -- the class of polyhedral cones.",
     PARA,
"For any ring ", TT "R", ", we represent vectors in ", TT "R^n", " by lists of ", TT "n", " elements of
", TT "R", ".  A cone is just a set of vectors that is of class ", TT "Cone", ".  If ", TT "R", " is ", TT "QQ", "
or ", TT "RR", ", we imagine that the set spans a polyhedral cone in ", TT "R^n", ".",
     PARA,
"Construct a cone with ", TT "cone", ".",
     PARA,
"The operator ", TO "?", " has been overloaded so that cones
are partially ordered by reverse inclusion.  This sets up the 
operators ", TO "<", ",", TO "<=", ",", TO ">", ",", " and ", TO ">=", " so
that, for instance, ", TT "x <= y", " iff every vector in ", TT "y", " is also in ", TT "x", ".  No
attempt is made to check if ", TT "y", " is contained in the convex hull of ", TT "x", ".",
     EXAMPLE{ "x = cone{{1,0}, {0,1}}",
	      "y = cone{{1,0}}",
	      "x <= y"},
     SEEALSO{"Fan", "(cone,List)", "(cone,Set)"}
     }    

cone Set := x -> new Cone from x

document { (cone,Set),
     TT "cone x", " -- turns the set ", TT "x", " into an object of class ", TO "Cone", "."
     }

cone List := x -> cone set x

document { (cone,List),
     TT "cone x", " -- turns the list ", TT "x", " into an object of class ", TO "Cone", ".",
     PARA,
     "Turns ", TT "x", " into a set, and that into a cone."
     }

-- We now set up the partial order on cones.

Cone ? Cone := (x,y) -> (
     if x === y then quote ==
         -- === of lists checks element by element.  Currently, === of
	 -- matrices may not return true, even when the matrices are
	 -- equal element-by-element.
     else if isSubset(x,y) then quote >
         -- as preceq in Shh is backwards!
     else if isSubset(y,x) then quote <
     else incomparable) -- now a reserved word in M2

-- The span of a cone is its image I in R^n, as R-module.  If we ever
-- needed the inclusion map i from this image into R^n, we would have
-- to create i before pruning I, prune i, and then set I = prune I.

span = method()

span Cone := memoize (c -> (
	  if (isField(R) or isField(coefficientRing R)) then (
	       prune image transpose matrix(R, toList c))
	  else error "Need to saturate the image in span(Cone)."))

perpInclusion = method()

perpInclusion Cone := memoize (c -> (
  ff := matrix(R, toList c);
  prune map(source ff, kernel ff)))

perp = method()

perp Cone := c -> source perpInclusion c

perpAlgebra = method()

perpAlgebra Cone := memoize (c -> totalExteriorPower perp c)

-- The rank of a cone is the rank of its linear span.  The rank of a
-- cone in a fan is different--see below.

rank Cone := memoize (c -> rank span c)

document { (rank,Cone),
     TT "rank x", " -- the rank of the span of ", TT "x", " in ", TT "R^n", ".",
     PARA,
     "Here x is of class Cone.",
     SEEALSO{"Cone"}
     }

lub (Cone, Cone) := (a,b) -> cone(a * b)

-- FANS ---------------------------------------------------

-- A fan is a RankedPoset of cones.  Thus it is a partially ordered
-- set S whose elements happen to be cones, whose partial order is
-- computable using < and friends, and where rank(c,S) computes the
-- rank function.

Fan = new Type of RankedPoset

document { Fan,
     TT "Fan", " -- the class of fans.",
     PARA,
"A fan is a ranked poset of strictly convex polyhedral cones.  Thus it
is an object of class ", TT "RankedPoset", " whose elements are of class ", TT "Cone", ".
The cones are partially ordered by inclusion; the partial order
is computed by ", TO "<", ", ", TO "<=", ", etc.",
     PARA,
"To construct a fan, use ", TO "fan", ".",
     SEEALSO{"RankedPoset", "IctvSheaf", "Cone"}
     }

fan = method(Options => {AllCones => false})

fan List := (lyst, options) -> (
  if options.AllCones then new Fan from set lyst
  else (
    -- lyst is a list of cones, all in R^n.  Take all i-fold
    -- intersections of elements of lyst for i from 1 to n, add on an
    -- empty cone, remove duplicates, add the n-vector 0 to each cone,
    -- and form the new Fan from that.
    --    This method assumes that the multi-intersections of the cones
    -- in lyst will determine all the cones of the fan.  For instance,
    -- this is true if the fan is complete and lyst contains all the
    -- n-dim'l cones.  If you want to use just one n-dim'l cone,
    -- give it together with all its (n-1)-dim'l faces.
    n := #((toList(lyst#0))#0);
    allIntersects := apply(flatten(apply(toList(1..n), i -> subsets(lyst,i))),
	                   x -> cone fold(x, (c1,c2) -> c1 * c2));
    allIntersects = append(allIntersects, cone{});
    noDups := {};
    scan(allIntersects, c -> (
	      if not(member(c,noDups)) then noDups = append(noDups,c)));
    zeroV := toList(n:0);
    new Fan from set apply(noDups, c -> (
	      cone append(toList c, zeroV)))))

openStar (Thing, Fan) := (c,P) -> fan select(toList P, elt -> c <= elt)

puncturedStar (Thing, Fan) := (c,P) -> fan select(toList P, elt -> c < elt)

-- The next two functions define the rank function for this type of poset.

maxRank := method()

maxRank (Fan) := memoize (S -> max(apply(toList S, rank)))

rank (Cone, Fan) := memoize ((c,S) -> maxRank(S) - rank(c))

zeroCone := method()

zeroCone Fan := memoize (S -> (
  lyst := select(toList S, c -> (rank c == 0));
  assert (#lyst == 1);
  lyst#0))

-- TORIC VARIETY METHODS ---------------------------------------

-- Question: how to integrate this with Macaulay 2's variety classes,
-- e.g., with a ToricVariety class that may be created someday and
-- have both coherentSheaves and combinatorialSheaves "on" it.

-- An ictvSheaf [this name is provisional!] is a combinatorialSheaf F
-- which is intended to hold (the image under a natural pushforward
-- Rf_* of) the complex of sheaves of intersection cochains on a toric
-- variety.  An ictvSheaf has at least the following five slots.
--    (.) F.rp, which is a Fan;
--    (.) F.verts, a mutableHashTable keyed by the elts in F.rp;
--    (.) F.edges, a mutableHashTable keyed by the pairs (tau,sigma)
--        where these are elts in F.rp with sigma < tau and
--        consecutive rank;
--    (.) F.Perversity, a perversity function;
--    (.) F.ur, the free R-module that's the groundwork of the
--        entire sheaf; every cone has its image in this module.  We
--        store it so we can get at its rank n conveniently.
-- The value in F.edges for (tau,sigma) is a morphism from
-- stalk(sigma,F) to stalk(tau,F).  It is assumed that the whole
-- arrangement is functorial: given any two chains in the poset with
-- the same initial and terminal points, the compositions of the
-- morphisms along the chains should be equal.

IctvSheaf = new Type of CombinatorialSheaf

ictvSheaf = method(Options => {Perversity => topPerversity})

ictvSheaf Fan := (S, options) -> (
  -- The optional second argument is a perversity function; if not
  -- supplied, its value will be topPerversity.
  o := zeroCone S;
  ans := new IctvSheaf from {
       rp => S,
       verts => new MutableHashTable,
       edges => new MutableHashTable,
       Perversity => options.Perversity,
       ur => perp o};
  -- Set the stalk at the zero cone to be the exterior algebra module
  -- on the ur space, tensor with the normal complex {R}.
  nor := new ChainComplex;
  nor.ring = R;
  nor#0 = R^1;
  ans.verts#o = makeTensorObject(perpAlgebra o, nor);
  ans)

adaptToTprAux := method()

adaptToTprAux (Cone, Cone, Fan) := memoize((c1, c, Sigma) -> (
  -- A helper for adaptToTpr.  Input: same as adaptToTpr, but you only
  -- pass the fan underlying the sheaf F, so the memoizer can
  -- recognize it.  Output: the pair (source NprToT, wedgeIsom).
  TprToT := perpInclusion c // perpInclusion c1;
  NprToT := (prune map(coker TprToT, target TprToT))^(-1);
  splitSum := source TprToT ++ source NprToT;
  -- assert (isFreeModule splitSum);
  splitSumToT := makeDirectSumMorphismToSingleton(
                   perp c1, splitSum, j -> (
		     if j == 0 then TprToT else NprToT));
  -- assert (isFreeModule target splitSumToT);
  -- assert (isIsomorphism splitSumToT);
  wedgeIsom := (totalExteriorPower splitSumToT) *
               (distributeTensorOverExteriorAlgebraOfDirectSum(
		 totalExteriorPower splitSum,
		 makeTensorObject((perpAlgebra c),
		                  (totalExteriorPower source NprToT))));
  -- Output:
  (source NprToT, wedgeIsom)))

adaptToTpr := method()

adaptToTpr (Cone, Cone, IctvSheaf) := memoize ((c1,c,F) -> (
  -- F.rp is the fan for a toric variety of complex dimension n, and
  -- c1 and c are cones in F.rp , with c < c1.  Inside F.ur = R^n =
  -- the lattice M in toric-variety-speak, let T' = Tpr be the perp
  -- space for c, and T that for c1.  Then T' is a saturated submodule
  -- of T.  The idea is that c is for a small face of the moment map
  -- polytope, and c1 for a larger face having the face for c on its
  -- boundary.
  --    Let W be the exterior algebra module for T, and W' for T'
  -- [obtained by perpAlgebra(c1), perpAlgebra(c), resp.]   So W'
  -- naturally injects into W.  Let T = T' ++ N' be some splitting of
  -- T.  Say stalk(c,F) is 
  --                ta = W ** B
  -- for some complex B.  The function adaptToTpr outputs the
  -- isomorphism
  --              W' ** (Z' ** B) --> W ** B
  -- where Z' is the algebra on N'.
  aux := adaptToTprAux (c1, c, F.rp);
  norCx := (stalk(c1,F)).from2;
  -- answer:
  ans := (aux#1 ** id_norCx) *
         tensorAssociativity(perpAlgebra c,
                             totalExteriorPower aux#0,
                             norCx);
  ans.source.from1 = perpAlgebra c;
  ans.source.from2 = makeTensorObject(totalExteriorPower aux#0, norCx);
  ans))

stalk (Cone, IctvSheaf) := (c,F) -> (
  if F.verts#?c then F.verts#c
  else F.verts#c = (
    U := puncturedStar(c, F.rp);
    normalSheaf := ictvSheaf U;
    doVertices(U, c1 -> (
      normalSheaf.verts#c1 = (source adaptToTpr(c1,c,F)).from2));
    doEdges(U, (tau,sigma) -> (
      normalSheaf.edges#(tau,sigma) =
        extractFFromIdTensorF(myInverse(adaptToTpr(tau,c,F)) *
                              F.edges#(tau,sigma) *
                              adaptToTpr(sigma,c,F))));
    globalNormalData := globalSectionsWithEdgeMorphisms normalSheaf ;
    tdeg := (F.Perversity)(2 * rank c);
    truncNormalMap := flabbyTruncationMap(globalNormalData#0, tdeg);
    truncCx := makeTensorObject(perpAlgebra(c), (source truncNormalMap));
    scan(drop(globalNormalData,1), pair -> (
      tau := pair#0;
      ff := pair#1;
      F.edges#(tau,c) =
        adaptToTpr(tau,c,F) *
                (id_(perpAlgebra c) ** (ff * truncNormalMap))));
    -- Value returned is the new stalk itself.
    truncCx))

-- MAPS AMONG IH GROUPS OF TORIC VARIETIES -------------------------

perversityMap = method();

perversityMap (IctvSheaf, IctvSheaf) := (T, S) -> (
  assert(S.rp === T.rp);
  assert(rank S.ur == rank T.ur);
  k := 2;
  while k <= 2 * rank S.ur do (
    if (S.Perversity)(k) > (T.Perversity)(k) then
      error "Source's perversity must be <= target's perversity.";
    k = k + 2);
  ans := new CombinatorialSheafMap;
  ans.source = S;
  ans.target = T;
  ans.stalkMaps = new MutableHashTable;
  o := zeroCone S.rp;
  (ans.stalkMaps)#o = id_(stalk(o,S));
  ans)

stalkMap (Thing, CombinatorialSheafMap) := (c, rho) -> (
  -- A default method for use with sheaves of chain complexes, when we
  -- think that flabbyInclusionOfSubspaces will produce the
  -- unique-up-to-q.i. correct answer.
  if (rho.stalkMaps)#?c then (rho.stalkMaps)#c
  else (rho.stalkMaps)#c = (
    S := rho.source;
    T := rho.target;
    Sigma := S.rp;
    tauLyst := {};
    doVertices(Sigma, tau -> (
	               if (c < tau
			   and rank(tau, Sigma) == rank(c, Sigma) + 1)
                         then tauLyst = append(tauLyst, tau)));
    -- Now tauLyst holds the list of all elements tau in Sigma with
    -- c < tau and consecutive rank.
    sumS := makeDirectSumObject(apply(tauLyst,
	                              tau -> stalk(tau, S)), 
                                tauLyst);
    sumT := makeDirectSumObject(apply(tauLyst,
	                              tau -> stalk(tau, T)), 
                                tauLyst);
    ff := makeDirectSumMorphism(sumT, sumS, (i,j) -> (
	      if i === j then stalkMap(i,rho)
	      else map(stalk(i,T), stalk(j,S), k -> 0)));
    Smap := makeDirectSumMorphismFromSingleton(sumS, stalk(c,S),
	      i -> (S.edges#(i,c)));
    Tmap := makeDirectSumMorphismFromSingleton(sumT, stalk(c,T),
	      i -> (T.edges#(i,c)));
    ffSmap := ff * Smap;
    ans := flabbyInclusionOfSubspaces(ffSmap, Tmap);
    assert (ffSmap == Tmap * ans);
    ans))

pairingIH = method()

pairingIH (IctvSheaf, IctvSheaf, IctvSheaf) := (C, A, B) -> (
  -- Assumes A, B, C are intersection cochain sheaves on the same
  -- space X.  Assumes the perversities are p, q, r, resp., with
  -- p + q <= r.  The function computes the canonical pairing from
  -- A ** B to C at the sheaf level, and outputs the induced pairing
  -- from IH_p^*(X) ** IH_q^*(X) to IH_r^*(X).
  assert(A.rp === B.rp);
  assert(B.rp === C.rp);
  k := 2;
  while k <= 2 * rank A.ur do (
    if (A.Perversity)(k) + (B.Perversity)(k) > (C.Perversity)(k) then
      error "Perversities must satisfy p + q <= r.";
    k = k + 2);
  AB := makeTensorObject(A,B);
  glEA := globalSectionsWithEdgeMorphisms(A);
  glEB := globalSectionsWithEdgeMorphisms(B);
  AB.globalSectionsWithEdgeMorphisms =
    prepend(makeTensorObject(glEA#0, glEB#0),
	    apply(toList(1..#glEA-1), j -> ((glEA#j)#0,
		                      (glEA#j)#1 ** (glEB#j)#1)));
  ABtoC := new CombinatorialSheafMap;
  ABtoC.source = AB;
  ABtoC.target = C;
  ABtoC.stalkMaps = new MutableHashTable;
  o := zeroCone A.rp;
  ABo := (AB.verts)#o;
  Co := (C.verts)#o;
  (ABtoC.stalkMaps)#o =
    map(Co, ABo, k -> makeDirectSumMorphismToSingleton(
	                Co_k, ABo_k, ij -> (
		          wedgeProduct(-(ij#0), -(ij#1), C.ur))));
  -- The rest of ABtoC.stalkMaps will be created, when globalSections
  -- calls for them, by the default method for stalkMap, which in turn
  -- uses flabbyInclusionOfSubspaces.
  --
  -- The next two lines do the real work and give the answer.
  glMap := globalSections ABtoC;
  (prune HH glMap) * (kunneth source glMap))

pairing = method()

pairing (ZZ, ZZ, ChainComplexMap) := (i,j,P) -> (
  -- Consider cochain complexes A, B, and C with a map P from
  -- A ** B to C.  (Example: pairingIH produces such a P.)  This
  -- function returns the map from A^i ** B^j to C^(i+j).
  k := -(i+j);
  P_k * (source(P)_k)_[(-i,-j)])

pairing (ChainComplexMap, ZZ, ZZ) := (P,i,j) -> (
  -- Consider cochain complexes A, B, and C with a map P from
  -- A ** B to C.  (Example: pairingIH produces such a P.)  This
  -- function returns the map from A^i ** B^j to C^(i+j).
  k := -(i+j);
  P_k * (source(P)_k)_[(-i,-j)])

quotientMapFromPairing = method()

quotientMapFromPairing (ChainComplexMap, ChainComplexMap) := (P, i) -> (
  -- For some chain complexes A and B accessible by (source P).from1
  -- and (source P).from2, P is a map A ** B ---> B, and
  -- i : Omega --> A.  This function computes the map
  -- L = P * (i ** id_B) into B, and returns the cokernel map for L.
  B := (source P).from2;
  L := P * (i ** id_B);
  prune map(coker L, target P))

pairingQuotientMap = method()

pairingQuotientMap (ChainComplexMap, List) := (P, lyst) -> (
  -- P is a pairing A ** B --> C.  lyst is a list of three quotient
  -- maps, {C --> C0, A --> A0, B --> B0}.  The function returns the
  -- appropriate pairing A0 ** B0 --> C0.
  coquotient(lyst#0 * P, lyst#1 ** lyst #2))

-- %%%%%%%%%%%%%%%%%%%% END OF FILE shh.m2 %%%%%%%%%%%%%%%%%%%%

