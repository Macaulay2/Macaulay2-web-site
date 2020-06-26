restart
loadPackage "MinimalPrimes"

S = QQ[a,b,c,d,e,f,g,
       u1,u2,u3,
       MonomialOrder=>Lex]

-- 9 point cicle:
-- vertices of triangle: (0,0), (2*u1,0), (2*u2,2*u3)
A = {0,0}, B = {2*u1,0}, C = {2*u2,2*u3} -- vertices
D = {c,d}, E = {a,b}, F = {2*u2,0} -- altitudes
P = {2*u2,e} -- orthocenter
Q = {f,g} -- center of circle
M1 = {u1,0}, M2 = {u2,u3}, M3 = {u1+u2,u3} -- midpoints
N1 = {u2,1/2*e}
N2 = {u1+u2, 1/2*e}
N3 = {2*u2, u3 + 1/2*e}
-- The 9 point circle theorem:
-- the points: D,E,F,M1,M2,M3,N1,N2,N3 lie on a circle.

pt = (p1, p2, p3, p4, q) -> (
    -- p1,p2 form one line
    -- p3,p4 form another line
    -- q is the point of intersection of these two lines
    a1 := p1-q;
    a2 := q-p2;
    b1 := p3-q;
    b2 := q-p4;
    ideal(a1_1 * a2_0 - a1_0 * a2_1, 
        b1_1 * b2_0 - b1_0 * b2_1)
    );

collinear = (p1,p2,p3) -> (
    ideal det (matrix{{1},{1},{1}} |matrix{p1,p2,p3})
    );

-- the condition that line(p1,p2) is perpendicular to line(q1,q2)
perp = (p1,p2,q1,q2) -> (
    v := p1-p2;
    w := q1-q2;
    ideal(v_0*w_0 + v_1*w_1)
    )

-- squared distance
dist2 = (p1,p2) -> (
    v := p1-p2;
    v_0^2 + v_1^2
    )

-- the condition that the distance from p1 to q is same as distance from p2 to q
len = (p1,p2,q) -> ideal(dist2(p1,q) - dist2(p2,q))

I = collinear(A,E,C) +
    collinear(B,C,D) +
    perp(A,C,E,B) +
    perp(B,C,A,D) +
    collinear(E,P,B) +
    collinear(A,P,D) +
    len(M1,M2,Q) +
    len(M1,M3,Q) +
    pt(A,D,B,E,P) + 
    pt(A,D,C,F,P)

I = trim I
netList I_*
G = ideal groebnerBasis I
netList G_*

-- we wish to show: all 9 points are equidistant to Q
-- 3 points are same distance to Q (M1,M2,M3), by construction
-- need 6 more points to be this same distance
rad = dist2(M1,Q)
rad2 = dist2(M2,Q)
rad3 = dist2(M3,Q)
(rad-rad2)%I
(rad-rad3)%I

goals = matrix{{
    dist2(D,Q) - rad,
    dist2(E,Q) - rad,
    dist2(F,Q) - rad,
    dist2(N1,Q) - rad,
    dist2(N2,Q) - rad,
    dist2(N3,Q) - rad
    }}
goals % I -- not zero yet...

elapsedTime comps = minprimes I;
#comps -- 7 components
for i from 0 to #comps-1 do (<< "ideal " << i << "  " << netList (comps_i)_* << endl << endl;);
netList for P in comps list goals % P

-- note that u1 cannot be 0, and that u3 cannot be 0.  (u2 could be 0)
Isat = saturate(I, u1*u3)
isPrime Isat
goals % Isat -- theorem is true!

-- Can we solve for all of these points?
kk = frac(QQ[u1,u2,u3])
S1 = kk[a,b,c,d,e,f,g,
       MonomialOrder=>Lex]
Isat1 = sub(Isat,S1)
G = ideal groebnerBasis  Isat1
netList G_*
sub(rad,S1) % G
4*u3^2*oo
sub(oo,S)
factor oo
