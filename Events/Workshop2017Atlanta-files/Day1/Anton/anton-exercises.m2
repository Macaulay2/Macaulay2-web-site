---------------------------
-- "starter" for problem (1)
---------------------------
restart
needsPackage "NumericalAlgebraicGeometry"
F = CC -- sometimes it helps to experiment with F=QQ to gain intuition
R = F[la,x_1,x_2]
A = random(F^2,F^2)
X = genericMatrix (R,x_1,2,1)
I = ideal(A*X - la*X)
polySystem I
--...

---------------------------
-- "starter" for problem (2)
---------------------------
restart 
--pick one
F = ZZ/101
F = QQ
F = CC

R = F[a..i]
E = genericMatrix (R,3,3)
P1 = E * transpose E * E - (1/2) * trace(E * transpose E) * E

n = 3 -- perhaps 3 views is not enough...
B = apply(n,i->random(F^3,F^1))
C = apply(n,i->random(F^3,F^1))
P2 = apply(n, i->transpose B#i * E * C#i)  

I = ideal P1 + ideal P2

-- brute force numerics
needsPackage  "NumericalAlgebraicGeometry"
help solveSystem -- 0-dim solver
help numericalIrreducibleDecomposition -- arbitrary-dim solver
needsPackage "MonodromySolver"
help solveFamily
--...

