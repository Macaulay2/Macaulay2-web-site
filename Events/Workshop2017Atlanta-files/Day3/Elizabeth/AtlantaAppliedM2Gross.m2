restart

path = prepend ("/Applications/Macaulay2-1.6/share/Macaulay2",path) --thing Liz has to do since her M2 is configured weird

loadPackage "PhylogeneticTrees"

-- construct a tree

n=4 -- number of leaves
T = {{0,1}} -- tree specified in terms of splits

T= leafTree(n,T)

-- we can also use a graph to specify a tree
G = graph{ {0,4}, {1,4}, {2,5}, {3,5}, {4,5} }
leafTree(G)

-- there are four built in models (or you can specify your favorite model)
-- CFNmodel, JCmodel, K2Pmodel, K3Pmodel
M = K3Pmodel
M = K2Pmodel
M = CFNmodel

-- we can obtain generators by calling 4ti2
phyloToric42 (T,M)

-- or we can obtain generators using an implementation of
-- the toric fiber product described in Sturmfels-Sullivant 2005

phyloToricFP (T,M)

-- while both these methods will construct a ring, it is
-- helpful to have your own ring on hand

--construct ring in terms of Fourier coordinates

R = qRing(n,M)
vars R

phyloToric42(n,T,M,QRing=>R) == phyloToricFP(n,T,M,QRing=>R)

-- can generate a random phylogenetic invariant using
-- the toric fiber product

phyloToricRandom(T, M)

-- can also just generate quadratic invariants

phyloToricQuads(T, K3Pmodel)

-- now let's explore mixture models

I = phyloToric42(T,JCmodel);

T = leafTree(6,{{0,1}, {0,1,2}, {0,1,2,3}})
phyloToric42(T, JCmodel)

-- secant and join in PhylogeneticTrees.m2 can be used for
-- *any* homogeneous ideals

SecI3 = secant(I,2);
for i in flatten entries mingens SecI3 
list (if (degree i)#0 == 1 then continue; i)

--dim(SecI3)
--isPrime SecI3
toricSecantDim(phyloToricAMatrix(T,JCmodel),2)

loadPackage "ReactionNetworks"

-- the package includes preloaded motifs (see Feliu and Wiuf 2013)

A = oneSiteModificationA()

-- a reaction network can also be entered as a string

A = reactionNetwork({"S_0+E<-->X", "X-->E+S_1", "S_1+F<-->Y", "Y-->S_0+F"})

-- create ring

R = createRing(A,QQ)
vars R

-- contruct equations

S = steadyStateEquations A

C = conservationEquations A

-- Random values for the parameters may be chosen
-- or specific values may be entered by the user

subRandomInitVals A
subRandomReactionRates A

F = join(subRandomInitVals A, subRandomReactionRates A)

I = ideal F

ring I
vars oo

S=QQ[A.ConcentrationRates]
J = sub(I, S)

-- now the ideal belongs to a ring whose indeterminates
-- are only x's

dim J
degree J

loadPackage "Bertini"

equilibria = bertiniZeroDimSolve(flatten entries gens J)

-- some other helpful tools

A2 = substitute(A, {"S_0"=>"S_1", "S_1"=>"S_2"})

B = glue(oneSiteModificationA(), A2)
R = createRing(B,QQ)
F = join(subRandomInitVals B, subRandomReactionRates B)
I = ideal F
S=QQ[B.ConcentrationRates]
J = sub(I, S)
dim J
degree J



