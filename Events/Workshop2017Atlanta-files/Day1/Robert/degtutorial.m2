-- degree of O(n)
restart
K = QQ
n = 3
--build the polynomial ring over the matrix entries
L = toList apply((0,0)..(n-1,n-1), (i,j)->"x"|toString i|toString j)
R = K[L]
--the defining relations for O(n)
Q = genericMatrix(R,n,n)
B = Q*(transpose Q) - id_(R^n)
I = flatten for i from 0 to n-1 list for j from i to n-1 list B_(i,j)

--symbolic approach
degree ideal I

S = K[R_*|{y}]
Ihom = homogenize(sub(ideal I,S),y);
f = hilbertPolynomial(Ihom, Projective=>false)
(leadCoefficient f)*(first degree f)!
fP = hilbertPolynomial(Ihom)
degree fP

--numerical approach
loadPackage "NumericalAlgebraicGeometry"
RC = CC[R_*]
I = apply(I,f->sub(f,RC));
lins = apply(binomial(n,2), i->random(1,RC)-random(CC));
S = solveSystem(I|lins);  --Default Software is M2engine
S = solveSystem(I|lins, Software=>BERTINI);
#S
first S
