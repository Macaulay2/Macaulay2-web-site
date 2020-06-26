—- M2@Atlanta, July 27, 2017, Stillman
-- 4 colorings of a graph

-- One puzzle on my ipad: given a map of regions, some of them colored
-- already (read, orange, green or purple) color the rest so that no
-- two regions next to each other have the same color.  In order to
-- touch they must share a line segment, not just a point

-- Our goal: find a Groebner basis method to solve this problem.
-- Step 1: encode the color into numbers.
--  For example: 
--    red: -1
--    orange: 0
--    green: 1
--    purple: 2

-- Part 1: For each variable, find a polynomial

--    which says that the region can only be colored certain colors.
--    For instance: region "a" cannot be red, orange, so must b green
--    or purple.

-- Part 2: For two adjacent regions, the colors cannot be the same.
--    Find an ideal in QQ[a,b] whose solutions are the points
--    a=0,1,2,3, b=0,1,2,3, BUT a and b cannot be the same.  What is
--    the lexicographic Groebner basis of this ideal?  (hint: it can
--    be done so the GB has 2 generators!).

-- Part 3: Find an ideal whose zero set is the set of all solutions to this
--    puzzle.  How many solutions are there?

restart
-- Representing colors as numbers:
-- red: -1
-- orange: 0
-- green: 1
-- purple: 2
col = (i) -> (
    if i == -1 then "red" 
    else if i == 0 then "orange" 
    else if i == 1 then "green" 
    else if i == 2 then "purple" 
    else error "unknown color"
    )
showcolor = (v,i) -> ("color of region "|toString v|" is "|col i)
F = (x) -> (x+1)*x*(x-1)*(x-2)
G = (x,y) -> (F x - F y)//(x-y)

-- Warmup example 1
R = QQ[a,b, MonomialOrder=>Lex]
F a
F b
G(a,b)
I = ideal(F a, F b, G(a,b))
netList decompose I
I == radical I

-- Warmup example 2
-- For our example, we will have about 48 variables, and alot of 
-- adjacent edges.  Let's make a function to help us with this
R = QQ[x_0,x_1,x_2,x_3,x_4]

-- this is a complete graph on 5 vertices:
edges = {
    (0,{1,2,3,4}),
    (1,{2,3,4}),
    (2,{3,4}),
    (3,{4})
    }

makeEdges = (edges) -> (
    -- edges is a list of 
    -- (vertex, {list of later vertices connected to this one})
    ideal flatten for e in edges list (
      for j in e_1 list G(x_(e_0), x_j)
    ))

see makeEdges { (0,{1,2}) }

I1 = ideal for x in gens R list F(x)
I2 = ideal(0_R);
I3 = makeEdges edges
I = I1 + I2 + I3
groebnerBasis I

-- Warmup example 2B: change edge set a bit, set color for  x_0
edges = {
    (0,{1,3,4}),
    (1,{2,3}),
    (2,{3,4}),
    (3,{4})
    }
I1 = ideal for x in gens R list F(x)
I2 = ideal(0_R)
I2 = ideal(x_0, x_1-1, x_2-2);
I3 = makeEdges edges
I = I1 + I2 + I3
J = ideal groebnerBasis I
decompose oo
#oo -- 72 4-colorings

-- Challenge#2:

R = QQ[x_0..x_47]
edges = {
    (0, {1,4,7,9,30,47}),
    (1, {4,7,10,11,5,31}),
    (2, {31,3,6,32,33}),
    (3, {31,5,6}),
    (4, {7}),
    (5, {6,11,12,31}),
    (6, {8,12,33}),
    (7, {9,10,13}),
    (8, {12,14,15,17,33,36}),
    (9, {13,10,16,18,39,37}),
    (10, {11,34,35,16,13}),
    (11, {12,14,34}),
    (12, {14}),
    (14, {17,19,35,34}),
    (15, {17,36}),
    (16, {35,21,18}),
    (17, {36,20,38,19}),
    (18, {21,27,22,40}),
    (19, {38, 25, 35}),
    (20, {42,26,44,25,38}),
    (21, {35,23,27}),
    (22, {27,45,39,40}),
    (23, {35,24,41,43,27}),
    (24, {25,41,35}),
    (25, {38,46,29,43,41,35}),
    (26, {42,44}),
    (27, {43, 28, 29}),
    (28, {29, 43}),
    (29, {43, 46})
    }
I1 = ideal for x in gens R list F(x)
I2 = ideal(
    -- red:
    x_30+1, x_32+1, x_35+1, x_38+1, x_43 + 1,
    -- orange:
    x_47, x_34, x_39, x_44,
    -- green: 
    x_36-1, x_37-1, x_40-1, x_42-1,
    -- purple:
    x_31-2, x_33-2, x_41-2, x_45-2, x_46-2
    )
I3 = makeEdges edges
I = I1 + I2 + I3
G = ideal groebnerBasis I
netList pack(4,G_*)
for i from 0 to 47 do print showcolor(i, x_i % I)
