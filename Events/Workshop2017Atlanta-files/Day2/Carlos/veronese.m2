
--Model parametrized by Veronese embedding
R = QQ[s,x,y];
--Data u
u = matrix{{1,3,5,7,9,2}};
N = 27

--standard scaling
c0=1; c1=1; c2=1; c3=1; c4=1; c5=1;

--another scaling
c0=1; c1=2; c3=1; c4=1; c5=2; c6=1;

c = matrix{{c0,c1,c2,c3,c4,c5}};

--v in the image of the parametrization
v = matrix{{c0*s,c1*s*x,c2*s*x^2,c3*s*y,c4*s*x*y,c5*s*y^2}};

--matrix defining the model
A = matrix{{1,1,1,1,1,1},  
           {0,1,2,0,1,0},
	   {0,0,0,1,1,2}};
--note that we are adding the sum to 1 restriction already

A = sub(A,R);

--Birch's Theorem system
I = ideal(A*(N*transpose v - transpose u));

J = saturate(I,s*x*y); 
degree J  --ML degree

--find MLE
loadPackage "Bertini"
L = flatten entries (A*(N*transpose v - transpose u))
bertiniZeroDimSolve(L)

