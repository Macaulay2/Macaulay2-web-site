restart
R = QQ[s11, s22, s33, s12, s13, s23]

-- Conditional independence statements of the contraction property for
-- A = 1, B = 2, C = \emptyset, D = 3, analyzed with primary
-- decomposition.

-- 1 ind 2 | 3 -> rk \Sigma_{13,23} = 1
I1 = ideal (s12*s33-s13*s23)

-- 1 \ind 3 
I2 = ideal s13

I = I1 + I2

radical I == I
decompose I  -- equivalent to "minimalPrimes I".

--> Back to slides for now



--------------------------
-- Let's do the example from above again:
needsPackage "GraphicalModels"
R = gaussianRing 3 -- variables are entries of 4x4 covariance matrices
gens R
statements = { {{1},{2},{3}}, {{1},{3},{}} }
I = conditionalIndependenceIdeal (R, statements)
needsPackage "Binomials"
binomialMinimalPrimes I

---------------------------

-- Create a ring with variables for entries of covariance matrices
-- (5x5 in this case)
R = gaussianRing 5

-- make an undirected graph by listing edges:
--  1 -- 2 -- 3 -- 4 -- 5    
--  \------------------/     (a 5-cycle)
G = graph({{1,2},{2,3},{3,4},{4,5},{5,1}})

-- create sets of conditional independence statements:
pS = pairMarkov G
gS  = globalMarkov G

Ipair = conditionalIndependenceIdeal(R, pS)
Iglobal = conditionalIndependenceIdeal(R, gS)

Iglobal == Ipair  -- Didn't I say that the Markov conditions are
		  -- equivalent?

-- primaryDecompositions of these are out of reach, and so is the
-- vanishingIdeal


-- make an acyclic directed graph:  1 -> 2 -> 3 -> 4
--                                  \--------------^
D = digraph {{1,{2,4}},{2,{3}},{3,{4}},{4,{}}}
-- For this to work it seems necessary to create the Gaussian ring
-- from the digraph.  This makes sure that vertex labels and variable
-- indices are the same objects internally.  This is what the next
-- line does:
R = gaussianRing D
gens R
I1 = conditionalIndependenceIdeal(R, globalMarkov D) -- d-separation
I2 = trekIdeal (R, D)
I3 = gaussianVanishingIdeal R
-- As claimed on the slides: The vanishing ideal for this graph on 4
-- vertices equals the conditional independence and trek ideal:
I1 == I3
I2 == I3

-- For those of you who know discrete variables, those also work:
-- E.g. create an ambient ring for 4 binary random variables.
R = markovRing (2,2,2,2)
gens R

-- conditional Independence Ideal from d-separation
I1 = conditionalIndependenceIdeal(R, globalMarkov D)

-- the vanishingIdeal of the image of a parametrization (by recursive
-- factorization)
I2 = discreteVanishingIdeal (R,D)

-- A theorem says that the positive real parts of the varieties of
-- these two ideals coincide.  Although this condition is
-- semi-algebraic, it can be checked with M2:
pd = primaryDecomposition I1
#pd
pd#0 == I2 
pd#1 -- contains a sum of two variables, so no positive distributions.
-- the remaining components are similar.
