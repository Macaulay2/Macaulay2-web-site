R = QQ[a..d]
I = ideal(a*c^2-b^2*d,b^3-a^2*c)

codim I
dim I
degree I
hilbertSeries I
hilbertPolynomial(I, Projective=>false)
hilbertFunction(12, I)

minI = minimalPrimes I;
netList minI
pd = primaryDecomposition I;
netList pd
for I in minI list degree I
for I in pd list degree I
minI/degree
pd/degree
radical I
minimalPrimes radical I

-- important notion: ideal quotients ("colon" ideals)
-- and saturation.
I1 = I : (a*c)
Isat = saturate(I,c*a)
I1 == Isat
Isat == I : (a*c)^2

#minI
minI_0
netList minI

IC = minI_0

C = res IC
betti C
regularity(R^1/IC)
regularity IC
C.dd
C.dd^2
