
-- Model parametrized by twisted cubic
R = QQ[s,t,mu]
-- mu is the Lagrange multiplier in the critical equations
u0 =3; u1=7; u2=5; u3=9; 
-- u is the data observed
phi = (s,t) -> (s,s*t,s*t^2,s*t^3) 
-- parametrization map of the model
L= ({mu*s*(1+t+t^2+t^3) - (u0 + u1 + u2+ u3), 
	mu*t*(s+2*s*t+3*s*t^2)- (u1 +2*u2 +3*u3),
	s+s*t+s*t^2+s*t^3-1});
--system of critical equations
netList L

I = ideal L
dim I 
degree I --ML degree (for generic u)

--now we want to actually compute the MLE
--numerical computations
loadPackage "NumericalAlgebraicGeometry"

--change to CC
R = CC[s,t,mu]

--sub the system to the new ring
L = apply(L, a-> sub(a,R))

--solve with M2
sols1 = solveSystem L
#sols1
solutionsWithMultiplicity sols1

--solve with Bertini
sols2 = solveSystem(L, Software=> BERTINI)
#sols2
theta2 = first sols2
phi(theta2#Coordinates_0,theta2#Coordinates_1) --MLE

--solve with PHCPack
sols3 = solveSystem(L, Software=> PHCPACK)
theta3 = last sols3
phi(theta3#Coordinates_0,theta3#Coordinates_1) --MLE