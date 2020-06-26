-----------------------------------
-- Complex numbers... 
-----------------------------------
a = 2.1 + 3.7*ii
b = 1.9 - 0.3*ii
imaginaryPart(a+b)
-- ... do they form a field?
c = a/b 
b*c == a
b*c - a
-- Does higher precision help?
a = 2.1p2017 + 3.7p2017*ii
b = random CC_2017
c = a/b 
b*c == a
b*c - a

needsPackage "NumericalAlgebraicGeometry"
----------------------------------------------
-- Newton's method
----------------------------------------------
-- Applied to a starting point sufficiently close to a {\em regular}
-- solution of a square system Newton's method converges to that
-- solution quadratically.
CC[x,y,z]; F = polySystem {x^2+y^2+z^2-5,x+y-3,x^2+z-1} 
P = point {{1.2,2.+0.2*ii,0.1*ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print (coordinates P, P.ErrorBoundEstimate);
    )

--------------------------------------------------------------------
-- If the solution is singular, the method converges slower and the
-- number of correct digits obtained at a fixed machine precision is
-- smaller.
CC[x,y,z]; F = polySystem {x^2+y^2+z^2-5,x+y-3,x^2+z^3-1} 
P = point {{1.2,2.+0.2*ii,0.1*ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print (coordinates P, P.ErrorBoundEstimate);
    )
jacobian F
J = evaluate(jacobian F, P)
rank J
numericalRank J

---------------------------------------------
-- Blackboxes
---------------------------------------------
-- One may attempt solving a square polynomial system using the blackbox solver.
R = CC[x,y];
F = {x^2+y^2-1, x*y};
sols = solveSystem F 
-- Some information describing a quality of solution is stored in the {\tt Point} object,
s = first sols; peek s
-- where some fields can be accessed by shortcut functions:
status s
coordinates s
-- Many tricks can be used to format the output:
VerticalList sols
clean_0.0001 matrix(sols/coordinates)

-----------------------------------------------------------------
-- How does it work?
-- The blackbox solver employs a randomized algorithm that returns
-- {\em all} solutions of a square system with probability one
-- provided the system is 0-dimensional.
--
-- The {\em total-degree homotopy} is at the heart of the
-- computation. Obtaining the start system and start solutions,
(G,solsG) = totalDegreeStartSystem F
-- the solver launches the segment homotopy tracker for 
-- $$H_t = (1-t) G + \gamma t F,  t\in [0,1].$$ 
track(G,F,solsG,gamma=>random CC)

-- Some divergent paths as well as most of the paths ending in
-- singular (multiplicity>1) or near-singular (clustered) solutions
-- are marked with "I" (status is set to "Infinity") 
-- or "M" (the minimal t-step value is reached; status="MinStepFailure").
F = {x^2+y^2-1, (x-y)^2};
sols = track(G,F,solsG) 
sols / coordinates

--------------------------------------------------
-- Try something new...
restart
needsPackage "MonodromySolver";
R = CC[a,b,c,d,e,f][x,y];
q  = a*x^2+b*y+c;
l = d*x+e*y+f;
-- ... to solve a generic system in a family
(sys, sols) = solveFamily polySystem {q,l}
-----------------------------------------------------------
-- Suppose we really need solutions to a particular system...
wanted = {2*x^2+3*y+5, 7*x+9*y+11}
-- ... get them from a generic one.
wanted'sols = track(sys,wanted,sols)
sols/(s->evaluate(polySystem sys, s))
wanted'sols/(s->norm evaluate(polySystem wanted, s))

-----------------------------------------
-- Interfaces to BERTINI and PHCPACK
track(sys,wanted,sols,Software=>BERTINI)
setDefault(Software=>PHCPACK)
solveSystem wanted

------------------------------------------
-- Positive-dimensional solutions
------------------------------------------      
-- "no" to ideals, "yes" to witness sets! 
R = CC[x,y,z];
sph = x^2+y^2+z^2-1; 
I = ideal {x*sph*(y-x^2), sph*(z-x^3)};
V := numericalIrreducibleDecomposition I
C = components V
degree C#0
C / dim
S = first V#2 -- first (and only) component of dim = 2
-- What is "inside" a component?
peek S
equations S
slice S
points S
-- Membership test for a point...
p1 := point {{2/3,2/3,1/3}} 
isOn(p1,S)
p2 := point (2*(matrix p)) 
isOn(p2,S)
-- ... and containment for components
circle = first components numericalIrreducibleDecomposition ideal {x^2+y^2-1,z}
isSubset(circle,S)

----------------------------------------
-- Under the carpet...
----------------------------------------
options track
getDefault \ keys options track
