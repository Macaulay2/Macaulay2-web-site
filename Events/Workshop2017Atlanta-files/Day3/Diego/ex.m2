--------- Ideal of graph q-colorings ---------

-- Here we perform chordal elimination on the ideal of q-colorings of a cycle graph.

restart
-- configure JpgViewer in package "Graphs" (optional)
needsPackage("Graphs",Configuration=>{"JpgViewer"=>"/usr/bin/qlmanage -p"})
needsPackage("Binomials")
needsPackage("Chordal")

-------------------------------
---------- METHODS ------------
-------------------------------

gbList = I -> flatten entries gens gb I

toLex = I -> (
    R := ring I;
    kk := coefficientRing R;
    S := kk[gens R, MonomialOrder=>Lex];
    phi = map(S,R);
    return phi(I);
)

-- GRAPHS

-- simple 10 vertex graph
graph10 = () -> (
    return graph(toList(0..9),{
        {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
        {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} });
)

-- graph of nested triangles
graphNestedTriangles = n -> (
    E := {};
    for i to n-1 do (
        E = join(E,{{3*i,3*i+1},{3*i,3*i+2},{3*i+1,3*i+2}}););
    for i to 3*n-4 do (
        E = append(E,{i,i+3}););
    return graph(toList(0..<3*n),E);
)

-- IDEALS

-- simple 4 variables 
ideal4Vars = () -> (
    R = QQ[x0,x1,x2,x3];
    F = ideal {x0^2*x1*x2 +2*x1 +1, x1^2 +x2, x1 +x2, x2*x3}
)

-- ideal of q-colorings
chromaticIdeal = (G,q,kk) -> (
    n := #vertices G;
    R := kk[x_0..x_(n-1), MonomialOrder=>GRevLex];
    F := for E in edges G list(
        (i,j):= toSequence elements E;
        sum for d to q-1 list R_i^d*R_j^(q-1-d)
    );
    F = F | for i to n-1 list (R_i)^q-1;
    return ideal (F)
)

-- ideal of colorings of the cycle
colorIdealCycle = (n,q) -> (
    G := cycleGraph n;
    kk := ZZ/10007;
    I := chromaticIdeal(G,q,kk); --grevlex
    I1 := toLex I;
    N := chordalNet I1;
    return (I,G,N);
)

-- edge ideal of the graph of nested triangles
edgeIdealTriangles = (n) -> (
    G := graphNestedTriangles n;
    I := edgeIdeal(G); --grevlex
    (o,phi) = suggestVariableOrder I;
    I1 := phi(I); --lex
    N := chordalNet I1;
    chordalTria N;
    topComponents N;
    return (I,G,N);
)

-- ideals of adjacent minors of a matrix
adjMinorsIdeal = (k,n) -> (
    kk := ZZ/10007; --generator a
    R := kk[vars(0..<k*n)]; --grevlex
    M := genericMatrix(R,k,n);
    F := for i to n-k list ( 
        L := toList(i..i+k-1);
        minors(k,M_L) 
    );
    I := ideal F;
    G := constraintGraph I;
    I1 := toLex I;
    N := chordalNet(I1);
    chordalTria N;
    topComponents N;
    return (I,G,N);
)

binomialEdgeIdeal = (G,kk) -> (
    n:= #vertices G;
    X:= flatten for i to n-1 list {(symbol x)_i,(symbol y)_i};
    R:= kk[X, MonomialOrder=>Lex];
    F:= for e in edges G list(
        E:=elements e; x_(E#0)*y_(E#1) - x_(E#1)*y_(E#0) );
    return ideal F;
)

binomialEdgeIdealCycle = n -> (
    kk := ZZ/10007;
    G := cycleGraph n;
    I := binomialEdgeIdeal (G,kk);
    N:= chordalNet I;
    chordalTria N;
    reduceDimension(N,2);
    return (I,G,N);
)

-- Knapsack problem:
-- xi^2 = 1 for i=0..n-1
-- x_0+...+x_(n-1) = 0
idealKnapsack = n -> (
    R := QQ[x_0..x_(n-1)];
    F := for i to n-1 list x_i^2 - 1;
    I := ideal(F) + sum gens R;
    G := constraintGraph I;
    I1 := toLex I;
    N := chordalNet(I1);
    chordalTria N;
    return (I,G,N);
)

-- Sparsified Knapsack
-- y_i = x_0+...+x_i
idealKnapsackSequential = n -> (
    X := flatten for i to n-1 list {x_i,y_i};
    R := QQ[X];
    F := for i to n-1 list x_i^2 - 1;
    F' := for i in 1..n-1 list y_i - x_i - y_(i-1);
    I := ideal(F | F' | {y_0-x_0, y_(n-1)} );
    G := constraintGraph I;
    I1 := toLex I;
    N := chordalNet(I1);
    chordalTria N;
    return (I,G,N);
)

-- Ideal of symmetric products
idealSymm = (n,k) -> (
    R := QQ[vars(0..<n)];
    I := ideal (subsets(gens R, k) / (S -> product(S)));
    G := constraintGraph I;
    I1 := toLex I;
    N := chordalNet(I1);
    chordalTria N;
    return (I,G,N);
)

-------------------------------
--------- Examples ------------
-------------------------------



-- Example
R = QQ[x_0..x_3]
I = ideal {x_0^2*x_1*x_2 +2*x_1 +1, x_1^2 +x_2, x_1 +x_2, x_2*x_3}
G = constraintGraph I

---- Example
---- Chordal completion
--G = graph10()
--Gc = chordalGraph(G)
--T = elimTree Gc

-- Example
-- Colorings of a cycle
--(I,G,N) = colorIdealCycle(10,3)
--gbList I
--chordalElim N
--chordalTria N

---- Example
---- Edge ideal
--(I,G,N) = edgeIdealTriangles(5)

---- Example
---- Ideal of adjacent minors
--(I,G,N) = adjMinorsIdeal(2,6)
--displayGraph constraintGraph I

---- Example
--(I,G,N) = binomialEdgeIdealCycle(15)
--dim N
--rootCount N
--topComponents N
--F = sum gbList I
--F % N
--codimCount N
--binomialCellularDecomposition I

---- Example
---- Knapsack problem
--(I,G,N) = idealKnapsack(8)
--rootCount N

