-- recall Pappus' theorem:
-- given (distinct) points A, B, C on a line in the plane
-- given (distinct) points A', B', C' on another line
-- let P be the intersection pt of lines AB', A'B.
-- similarly for Q (lines AC', A'C)
-- and R (lines BC', B'C)
-- then PQR are collinear.

-- The goal here is to use Macaulay2 to translate this to algebra
-- then to analyze the algebra, proving the theorem.
-- (Realistically: the proof itself is much simpler, but this is
-- useful to see how to construct and analyze ideals in M2)
restart
loadPackage "MinimalPrimes"
installMinprimes()

S = QQ[a1,a2,a3,
       b1,b2,b3,
       u1,u2,
       x1,y1,x2,y2,x3,y3,
       MonomialOrder=>Lex]
-- the 9 points in question: WLOG we take the first line to be x=0
A = {0,0}
B = {0,u1}
C = {0,u2}
A' = {x1,y1}
B' = {x2,y2}
C' = {x3,y3}
P = {a1,b1}
Q = {a2,b2}
R = {a3,b3}

-- Form the equations that the point q is the intersection of the lines
-- p1, p2, and p3, p4.
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
pt({2,0},{0,2},{0,0},{3,3},{1,1})
pt({2,0},{0,2},{0,0},{3,3},{1,2})
I1 = pt(A,B',A',B,P)
I2 = pt(A,C',C,A',Q)
I3 = pt(B,C',C,B',R)

collinear = (p1,p2,p3) -> (
    det (matrix{{1},{1},{1}} |matrix{p1,p2,p3})
    );

collinear(A,B,C)
I4 = collinear(A',B',C')
I = I1 + I2 + I3 + I4;
netList I_*

codim I
degree I

goal = collinear(P,Q,R)
goal % I -- non-zero, so not easily OK!
goal^5 % I -- no such luck!
comps = minprimes I;
#comps -- 12 components
for i from 0 to #comps-1 do (
    << "ideal " << i << "  " 
    << netList (comps_i)_* 
    << endl << endl;
    );

-- Which components do we care about?
netList for L in comps list (goal%L) -- only follows for 3 of the 12 components!

-- constraint: all 6 input points should be distinct.
allpts = {
    A,B,C,A',B',C'
    }
allpairs = for x in subsets(allpts, 2) list trim ideal(x_0 - x_1)
for i from 0 to #comps-1 list positions(allpairs, J -> isSubset(J,comps_i));
netList oo
comps_0 -- this component is when all of the 6 input points lie on one line.
goal % comps_11
goal % comps_0

-- Saturate out all 15 bad cases:
-- Note: this is often much faster than first doing 'decompose'
I1 = I
for J in allpairs do I1 = saturate(I1,J)
I1 == intersect(comps_0, comps_-1)
goal % I1 -- theorem holds if all 6 points are distinct.
