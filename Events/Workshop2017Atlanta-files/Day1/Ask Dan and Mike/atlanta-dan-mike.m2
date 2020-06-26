R = ZZ/32003[x,y,z]
F = x^3+y^2*z+x*y*z+z^3+y+1
J = ideal F + ideal jacobian ideal F
gens gb J
netList J_*
C = 1 // gens J
gens J * C
Z = syz gens J
C % Z
F = x^3+y^2*z
J = ideal F + ideal jacobian ideal F
gens gb J
netList J_*
C = 1 // gens J
1_R % gens J
gens J * C

