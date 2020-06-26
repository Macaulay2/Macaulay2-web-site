restart
-- Circle of Apollonius
-- vertices of the right triangle:
-- (0,0), (u_1,0), (0,u_2)
-- midpoint of hypotenuse: 1/2 (u_1,u_2)
-- (x_1,x_2) is the location on the hypotenuse which is the altitude of (0,0).
-- (x_3,x_4) is the origin of the circle going through
--   the three midpoints
-- claim: this circle goes through the origin
--   and the altitude (x_1,x_2)
R = QQ[u_1,u_2,x_1,x_2,x_3,x_4]
-- condition #1: (0,0)-(x1,x2) is perp to (u1,0)-(0,u2)
-- condition #2: (x1,x2) lies on line connecting (u1,0) to (0,u2)
-- condition #3: the distance from (x3,x4) to (0,1/2 u2) 
--   is the same as that from (x3,x4) to (1/2 u1, 1/2 u2)
-- condition #4: these distances are the same as from (x3,x4) to (1/2 u1, 0)
-- i.e. the two midpoints and the one vertex are equidistant to (x3,x4) 

I = ideal(
    x_1*u_1 - x_2*u_2,
    u_2*x_1 + x_2*u_1 - u_1*u_2,
    x_3^2 + (x_4 - 1/2 * u_2)^2 - (1/2 * u_1 - x_3)^2 - (1/2 * u_2 - x_4)^2,
    x_3^2 + (x_4 - 1/2 * u_2)^2 - x_4^2 - (x_3 - 1/2*u_1)^2
    )
comps = decompose I;
netList comps
J = saturate(I, u_1*u_2)
-- F says: distance of (x_1,x_2) to (x_3,x_4) is same as radius of circle
-- G says: distance of (0,0) to (x_3,x_4) is same as radius of circle
F = (x_1-x_3)^2 + (x_2-x_4)^2 - x_3^2 - (x_4 - 1/2 * u_2)^2
G = (x_1-x_3)^2 + (x_2-x_4)^2  - x_3^2 - x_4^2 
F % J
G % J -- A lies on this circle too!

kk = frac(QQ[u_1, u_2])
R1 = kk[x_1..x_4]
J1 = sub(J, R1)
G = ideal groebnerBasis J1
netList G_*
