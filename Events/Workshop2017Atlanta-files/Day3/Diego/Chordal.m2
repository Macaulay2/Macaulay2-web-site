
newPackage(
    "Chordal",
    Version => "0.1", 
    Date => "25 May 2016",
    Authors => {
      {Name => "Diego Cifuentes",
       Email => "diegcif@mit.edu",
       HomePage => "http://www.mit.edu/~diegcif"},
      {Name => "Pablo Parrilo", 
       Email => "parrilo@mit.edu",
       HomePage => "http://www.mit.edu/~parrilo/"}
    },
    Headline => "A package that exploits chordal structure",
    --Configuration => { 
    --  "path" => "",
    --  "PHCexe"=>"phc", 
    --  "keep files" => true
    --},
    DebuggingMode => true,
    AuxiliaryFiles => true,
    PackageImports => {"SimpleDoc","Binomials","MapleInterface"},
    PackageExports => {"Graphs"}
)

--Copyright 2017 Diego Cifuentes, Pablo Parrilo.
--  You may redistribute this file under the terms of the GNU General
--  Public License as published by the Free Software Foundation,
--  either version 2 of the License, or any later version.

export { 
--Only for Debugging
    "inconsistentArc",
    "nodePairs",
    "NoRoot",
    "nodesRank",
    "netArcs",
    "nodeChildren",
    "treeDescendants",
    "GetTable",
    "triaSystem",
    "printState",
--Types
    "ChordalNetNode",
    "ChordalNetRank",
    "ChordalGraph",
    "ElimTree",
    "ChordalNet",
    "ChordalNetChain",
    "TriaSystem",
--Methods/Functions
    "chordalGraph",
    "treewidth",
    "elimTree",
    "constraintGraph",
    "suggestVariableOrder",
    "chordalNet",
    "displayNet",
    "chordalElim",
    "chordalTria",
    "triangularize",
    "mvar",
    "initial",
    "prem",
    "isPrimeSimple",
    "isStronglyNormalized",
    "codimCount",
    "rootCount",
    "nextChain",
    "nextOrderedPartition",
    "reduceNet",
    "reduceDiamonds",
    "reduceDimension",
    "reduceContained",
--Method options
--Symbols
    "isTriangular",
    "ineqs",
    "nodes",
    "Roots",
    "Cliques",
    "Structure",
    "TriangularDecompAlgorithm"
}

protect NoRoot, protect Reversed, protect counter

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

--PHCDBG = 0; -- debug level (10=keep temp files)
--path'PHC = (options PHCpack).Configuration#"path";
--PHCexe=path'PHC|(options PHCpack).Configuration#"PHCexe"; 
-- this is the executable string that make sures that calls to PHCpack run:
-- NOTE: the absolute path should be put into the init-PHCpack.m2 file 


--##########################################################################--
-- DEBUGGING METHODS
--##########################################################################--

-- verifies if for any outgoing arc there is an incoming arc
inconsistentArc = N -> (
    tree := N.elimTree;
    for ip in nodePairs(tree,NoRoot=>true) do (
        (i,p):= ip;
        for Ni in nodesRank(N,i) do (
            for Np in Ni.parents do
                if not member(Ni,nodeChildren(i,Np)) then return (Ni,Np);
            for Nc in Ni.children do
                if not member(Ni,Nc.parents) then return (Nc,Ni);
            );
    );
    return false;
)

--##########################################################################--
-- INTERNAL METHODS
--##########################################################################--

-- list of generators
gensL = I -> I_*;
gensGB = I -> flatten entries gens gb I

supportF = F -> if #F==0 then F else
    rsort toList sum for f in F list set support f;

uniqueFast = x -> keys set x;

--##########################################################################--
-- EXPORTED METHODS
--
-- NOTE:
-- trackPaths and refineSolutions are methods adapted 
-- from NumericalAlgebraicGEometry/PHCpack.interface.m2
--##########################################################################--

--###################################
-- Type definitions
--###################################

-- type representing chordal graphs
ChordalGraph = new Type of Digraph

-- type representing the elimination tree of a chordal graph
ElimTree = new Type of HashTable

-- type representing chordal networks
ChordalNet = new Type of MutableHashTable

-- type representing a rank of a chordal network
ChordalNetRank = new Type of MutableHashTable

-- TODO: this should be immutable
-- type representing a node of a chordal network
ChordalNetNode = new Type of MutableHashTable

-- type representing a chain of a chordal network
ChordalNetChain = new Type of HashTable

-- type representing a triangular system
TriaSystem = new Type of HashTable

--###################################
-- Chordal graphs
--###################################

-- ChordalGraph constructor
-- finds a chordal completion of a graph
chordalGraph = method()
chordalGraph (Digraph) := DG -> (
    (cG,tw) := chordalClosure(DG);
    new ChordalGraph from cG
)
-- completes the graph using a given ordering
chordalGraph (Graph,List) := (G,ordering) -> (
    if ordering!={} then G = reorderGraph(G,ordering);
    DG := directEdges(G);
    return chordalGraph(DG);
)
-- completes the graph, searching for a good ordering
chordalGraph (Graph) := G -> (
    (o,tw) := minDegOrder(G);
    return chordalGraph(G,o);
)
-- chordal graph given by an elimination tree
chordalGraph (ElimTree) := tree -> (
    DG:= digraph applyPairs(tree.Cliques, (i,C) -> (i,delete(i,C)));
    DG = reorderGraph(DG, tree.nodes);
    return chordalGraph DG;
)

-- completes a digraph using the ordering from G.vertexSet
chordalClosure = DG -> (
    A := mutableMatrix DG.adjacencyMatrix;
    tw := 0;
    for i to numRows(A)-1 do (
        degi := fillMat(A,i,false);
        tw = max(tw,degi);
    );
    Gc := graph(DG.vertexSet, matrix A);
    return (Gc,tw);
)

fillMat = (A,i,sym) -> (
    I := neighborsAdjMat(A,i);
    for e in subsets(I,2) do (
        A_(e_0,e_1) = 1;
        if sym then A_(e_1,e_0) = 1;
    );
    return #I
)
neighborsAdjMat = (A,i) -> (
    Ai := first entries A^{i};
    I := positions (Ai, j-> j!=0);
    return I;
)

-- reorders the vertices of a graph
reorderGraph = (G,ordering) -> graph(ordering, edges G)

-- directs the edges of a graph according to some given order
directEdges = method()
directEdges(Graph,List) := (G,o) -> (
    V := G.vertexSet;
    A := G.adjacencyMatrix;
    I := toList(0..<numRows(A));
    U := matrix table(I,I, (i,j)-> if i<=j then A_(o_i,o_j) else 0);
    return digraph(V_o,U);
)
directEdges(Graph) := G -> (
    return directEdges(G,toList(0..<#G.vertexSet));
)

-- minimum degree ordering
minDegOrder = (G) -> (
    A := mutableMatrix G.adjacencyMatrix;
    V := G.vertexSet;
    S := 0..<numRows(A);
    tw := 0;
    ordering := while #S>0 list (
        i := minDeg(A);
        Vi := V_(S_i);
        degi:= fillMat(A,i,true);
        tw = max(tw,degi);
        J := {0..<i}|{i+1..<#S};
        A = submatrix (A,J,J);
        S = S_J;
        Vi
    );
    return (ordering,tw);
)
minDeg = (A) -> (
    mdeg := infinity;
    vert := 0;
    for i to numRows(A)-1 do (
        I := neighborsAdjMat(A,i);
        degi := #I;
        simplicial := sum(flatten entries A^I_I)==#I*(#I-1);
        if simplicial then return i;
        if degi < mdeg then (
            mdeg = degi;
            vert = i;
        );
    );
    return vert;
)

--###################################
-- Custom methods for chordal graphs
--###################################

-- TODO: independenceNumber

isChordal(ChordalGraph):= G -> true

isPerfect(ChordalGraph):= G -> true

treewidth = method()
treewidth(ChordalGraph):= G -> 
    max(for v in G.vertexSet list #children(G,v))
treewidth(ElimTree):= tree -> 
    max(for v in tree.nodes list #tree.Cliques#v)-1

cliqueNumber(ChordalGraph) := G -> treewidth(G)+1

chromaticNumber(ChordalGraph):= G -> cliqueNumber G

inducedSubgraph(ChordalGraph):= G -> chordalGraph inducedSubgraph G

--###################################
-- Elimination tree
--###################################

-- ElimTree constructor
elimTree = method()
elimTree (List,HashTable,HashTable) := (sortednodes,parnt,clique) -> (
    roots := {};
    child := new MutableHashTable from sortednodes / (i->(i,{})); 
    for i in sortednodes do (
        p := parnt#i;
        if p===null then roots = append(roots,i)
        else child#p = append(child#p,i);
    );
    child#null = roots;
    child = new HashTable from child;
    new ElimTree from {
        symbol nodes => sortednodes,
        symbol parents => parnt, 
        symbol Cliques => clique,
        symbol children => child,
        symbol Roots => roots}
)
elimTree (ChordalGraph) := G -> (
    nodes := G.vertexSet;
    I := toList (0..<#nodes);
    posit := hashTable( I / (i-> (nodes#i,i)) );
    smallest := L -> (
        idx := min( L / (j->posit#j) );
        if idx != infinity then nodes#idx );
    parnt := hashTable(
        for i in nodes list (i,smallest toList children(G,i)) );
    sortverts := V -> for i in nodes list (
        if member(i,V) then i else continue);
    cliques := hashTable(
        for i in nodes list (i,prepend(i,sortverts children(G,i))) );
    return elimTree(nodes,parnt,cliques);
)
elimTree (ChordalNet) := N -> N.elimTree

cliqueIsMaximal = (j,tree) -> (
    C := tree.children#j;
    K := tree.Cliques;
    contained := any( C, c -> isSubset(K#j, K#c) );
    return not contained;
)

-- is clique Xj contained in a unique Xc?
cliqueNotContained = (j,tree) -> (
    C := tree.children#j;
    K := tree.Cliques;
    if #C!=1 then return true;
    c := first C;
    contained := isSubset(K#j, K#c); 
    return not contained;
)


-- enumerate the pairs (j,p) where p is parent of j
nodePairs = method(Options => {NoRoot=>false, Reversed=>false})
nodePairs (ElimTree) := opts -> tree -> (
    nodes := tree.nodes;
    if opts.Reversed then nodes = reverse(nodes);
    npairs := for j in nodes list (
        p := tree.parents#j;
        if opts.NoRoot and p===null then continue;
        (j,p) );
    return npairs;
)

treeDescendants = (tree,j) -> (
    nodes := {j};
    for c in tree.children#j do
        nodes = nodes | treeDescendants(tree,c);
    return nodes;
)

leaves(ElimTree) := tree -> select(tree.nodes, i->tree.children#i=={})

--###################################
-- Constraint graph of an ideal
--###################################

-- constructs the constraint graph of a polynomial set
constraintGraph = method()
constraintGraph (List,Ring) := (F,R) -> (
    E := set();
    for f in F do (
        Vf := support f;
        Ef := set subsets(Vf, 2);
        E = E + Ef;
    );
    return graph(gens R, toList E);
)
constraintGraph (Ideal) := I -> (
    F := gensL I;
    return constraintGraph(F, ring I);
)

-- suggests a good variable ordering
suggestVariableOrder = method()
suggestVariableOrder (Ideal):= I -> (
    g := constraintGraph(I);
    (X,tw) := minDegOrder(g);
    R := ring I;
    S := changeRing(R,X,);
    phi := map(S,R);
    return (X,phi);
)
changeRing = (R,X,mOrder) -> (
    Q := coefficientRing R;
    if X===null then X = gens R;
    if mOrder===null then mOrder = Lex;
    return Q[X, MonomialOrder=>mOrder];
)

-- verifies if the ordering is lexicographic
hasLexOrder = method()
hasLexOrder(Ring) := R -> (
    opts := (options R).MonomialOrder;
    haslex := false;
    badkeys := (Weights,GRevLex,RevLex,GroupLex,GroupRevLex);
    for o in opts do (
        if member(o#0,badkeys) then return false;
        if o#0==Lex then haslex = true;
    );
    return haslex;
)
hasLexOrder(Ideal) := I -> hasLexOrder ring I

--###################################
-- Chordal networks
--###################################

-- ChordalNetRank constructor
chordalNetRank = method()
chordalNetRank (ZZ,Thing,VisibleList) := (k,i,nods) -> (
    new ChordalNetRank from {
        symbol nodes => nods,
        symbol counter => k,
        symbol rank => i}
)
chordalNetRank (Thing,VisibleList) := (i,nods) -> (
    return chordalNetRank(0,i,nods);
)
chordalNetRank (Thing) := i -> (
    return chordalNetRank(i,{});
)



-- ChordalNetNode constructor 
chordalNetNode = method()
chordalNetNode(String,Thing,VisibleList,VisibleList) := (nname,i,F,H) -> (
    --TODO preprocess generators (reduction)
    H = select(H, h -> not isConstant h);
    new ChordalNetNode from {
        symbol label => nname,
        symbol gens => reduceEqs F,
        symbol ineqs => rsort H,
        symbol rank => i,
        symbol parents => {},
        symbol children => {}}
)
chordalNetNode(ChordalNetRank,Thing,VisibleList,VisibleList) := (NRi,i,F,H) -> (
    nname := getNodeLabel(NRi);
    return chordalNetNode(nname,i,F,H);
)

-- TODO: sorting necessary?
reduceEqs = F -> flatten entries mingens ideal F

ideal(ChordalNetNode) := Ni -> ideal Ni.gens


getNodeLabel = NRi -> (
    k := NRi.counter;
    NRi.counter = k + 1;
    label := toString(NRi.rank) | "_" | toString(k);
    return label;
)

nodeName = i -> (
     j := i % 52;
     str := if j < 26 then ascii(97 + j)
         else ascii(65 + j - 26);
     if i < 52 then str
     else nodeName(i//52)| str 
)

-- ChordalNet constructor
chordalNet = method(Options=> {Monomial=>false,Binomial=>false})
chordalNet(Ideal,ElimTree) := opts -> (I,tree) -> (
    if not hasLexOrder I then error("Monomial order must be Lex");
    F := gensL I;
    struct:= if opts.Monomial then "Monomial"
        else if opts.Binomial then "Binomial"
        else getStruct(F);
    N := new MutableHashTable from 
        for i in tree.nodes list 
            i => chordalNetRank(i);
    initializeNet(N,tree,F);
    CN := new ChordalNet from {
        symbol elimTree => tree,
        symbol ring => ring I,
        symbol Structure => struct,
        symbol isTriangular => false,
        symbol net => N};
    return CN;
)
chordalNet(MonomialIdeal,ElimTree) := opts -> (I,tree) -> (
    return chordalNet(ideal I,tree,Monomial=>true)
)
chordalNet(Ideal) := opts -> I -> (
    g := constraintGraph(I);
    gc := chordalGraph(g,{});
    tree := elimTree(gc);
    return chordalNet(I,tree);
)

initializeNet = (Nnet,tree,F) -> (
    FF := distributePolys(tree,F);
    for i in tree.nodes do
        Nnet#i.nodes = { chordalNetNode(Nnet#i,i,FF#i,{}) };
    for jp in nodePairs(tree,NoRoot=>true) do (
        (j,p):= jp;
        Nj:= Nnet#j.nodes#0;
        Np:= Nnet#p.nodes#0;
        Nj.parents = {Np};
        addChildren(Np, Nj);
    );
);

-- distribute polynomials on the tree
distributePolys = (tree,F) -> (
    cliques := applyPairs(tree.Cliques ,(i,C)->(i,set C));
    placePoly := f -> (
        Xf := support f;
        queue := {mvar f};
        minranks := {};
        while #queue>0 do(
            i := first queue;
            queue = drop(queue,1);
            C := tree.children#i;
            R := for c in C list (
                if isSubset(Xf,cliques#c) then c else continue);
            queue = queue | R;
            if #R==0 then
                minranks = append(minranks,i);
        );
        return minranks;
    );
    FF := new MutableHashTable from 
        for i in tree.nodes list i => {};
    for f in F do(
        ranks := placePoly f;
        for i in ranks do
            FF#i = append(FF#i, f);
    );
    return FF;
)

getStruct = (F) -> (
    nterms := max(F / (f-> #terms f));
    if nterms==1 then return "Monomial"
    else if nterms==2 then return "Binomial"
    else return "None";
)

--###################################
-- Access chordal network nodes/arcs
--###################################

-- enumerate pairs (i,Ni) where Ni is a rank i node
nodePairs (ChordalNet) := opts -> N -> (
    ranks := N.elimTree.nodes;
    if opts.Reversed then ranks = reverse(ranks);
    npairs := flatten for i in ranks list (
        for node in nodesRank(N,i) list (
            (i,node) ));
    return npairs;
)

nodesRank = (N,i) -> (N.net#i).nodes

-- enumerate arcs of the network
netArcs = N -> (
    tree := N.elimTree;
    E := for ip in nodePairs(tree,NoRoot=>true) list (
        (i,p):= ip;
        for Ni in nodesRank(N,i) list 
            for Np in Ni.parents list (Ni,Np)
    );
    return flatten flatten E;
)

-- children of a node
nodeChildren = (rk, Ni) -> select( Ni.children, Nc->Nc.rank==rk )

--###################################
-- Representation and Visualization
--###################################

-- printing custom types
net(ElimTree) := Net => tree -> (
    x := tree.parents;
    horizontalJoin flatten (
        net class tree,
        "{",
        stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
        "}"
    )
)
net(ChordalNetNode) := Net => Ni -> (
    F := Ni.gens;
    netgens := if #F==0 then " "
        else if #F==1 then net first F
        else net F;
    H := Ni.ineqs;
    netineqs := if #H==0 then ""
        else if #H==1 then " / " | net first H
        else " / " | net H;
    return netgens | netineqs;
)
net(ChordalNetRank) := Net => Nrk -> net Nrk.nodes
net(ChordalNet) := Net => N -> (
    x := N.net;
    horizontalJoin flatten (
        net class N,
        "{ ",
        stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
        " }"
    )
)
net(TriaSystem) := Net => T -> (
    F := T.gens;
    netgens := if #F==0 then " "
        else if #F==1 then net first F
        else net F;
    H := T.ineqs;
    netineqs := if #H==0 then ""
        else if #H==1 then " / " | net first H
        else " / " | net H;
    return netgens | netineqs;
)

size(ChordalNet) := N -> (values N.net) / (NR -> #NR.nodes)

-- this function should belong to the Graphs package
writeDotFile (String, Digraph) := (filename, G) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    V := vertexSet G;
    scan(#V, i -> fil << "\t" | toString i | " [label=\""|toString V_i|"\"];" << endl);
    A := adjacencyMatrix G;
    E := flatten for i from 0 to #V - 1 list for j from i+1 to #V - 1 list if A_(i,j) == 1 then {i, j} else continue;
    scan(E, e -> fil << "\t" | toString e_0 | " -> " | toString e_1 | ";" << endl);
    fil << "}" << endl << close;
)

-- display an elimination tree
displayGraph (String, String, ElimTree) := (dotfilename, jpgfilename, tree) -> (
    writeDotFile(dotfilename, tree);
    displayDotFile(dotfilename, jpgfilename);
)
displayGraph (String, ElimTree) := (dotfilename, tree) -> 
    displayGraph(dotfilename, temporaryFileName()|".jpg", tree);
displayGraph (ElimTree) := (tree) -> 
    displayGraph(temporaryFileName()|".dot", tree);

writeDotFile (String, ElimTree) := (filename, tree) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    for i in tree.nodes do
        fil << "\t" | toString i | " ;" << endl;
    for ip in nodePairs(tree,NoRoot=>true) do(
        (i,p):= ip;
        Ci := tree.Cliques#i;
        Ci = delete(p,delete(i,Ci));
        fil << "\t" | toString i | " -> " | toString p | ";" << endl;
        attr := "[dir=none,style=dotted]";
        fil << "\t" | toString i | " -> " | toString Ci | attr | ";" << endl;
    );
    fil << "}" << endl << close;
)

-- display a chordal network
displayNet = method()
displayNet (Function,String,String,ChordalNet) := (fun, dotfilename, jpgfilename, N) -> (
    writeDotFile(fun,dotfilename, N);
    displayDotFile(dotfilename, jpgfilename);
)
displayNet (Function,String,ChordalNet) := (fun,dotfilename, N) -> 
    displayNet(fun,dotfilename, temporaryFileName()|".jpg", N);
displayNet (Function,ChordalNet) := (fun,N) -> 
    displayNet(fun, temporaryFileName()|".dot", N);
displayNet (ChordalNet) := (N) -> 
    displayNet(net, N);

writeDotFile (Function,String, ChordalNet) := (fun,filename, N) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    for iNR in pairs(N.net) do(
        (i,NRi):= iNR;
        fil << "\tsubgraph cluster_" | toString i | " {" << endl;
        for Ni in NRi.nodes do
            fil << "\t\t" | toString Ni.label | " [label=\""|toString fun(Ni)|"\"];" << endl;
        fil << "\t}" << endl;
    );
    E := netArcs N;
    for e in E do(
        (Ni,Np):= e;
        fil << "\t" | toString Ni.label | " -> " | toString Np.label | ";" << endl;
    );
    fil << "}" << endl << close;
)

displayDotFile = (dotfilename,jpgfilename) -> (
    dotBinary := ((options Graphs).Configuration)#"DotBinary";
    jpgViewer := ((options Graphs).Configuration)#"JpgViewer";
    runcmd(dotBinary  | " -Tjpg " | dotfilename | " -o " | jpgfilename);
    runcmd(jpgViewer  | " " | jpgfilename|" &");
)
runcmd = cmd -> (
    stderr << "-- running: " << cmd << endl;
    r := run cmd;
    if r != 0 then error("-- command failed, error return code ", r);
)

-- digraph associated to a chordal network
digraph(ChordalNet) := opts -> N -> (
    tree := N.elimTree;
    nodes := {};
    edges := {};
    alias := Ni -> Ni.label;
    for ip in nodePairs(tree) do(
        (i,p):= ip;
        NRi := nodesRank(N,i);
        nodes = nodes | (NRi/alias);
        if p===null then continue;
        Er := for Ni in NRi list(
            {alias(Ni),(Ni.parents)/alias} );
        edges = edges | Er;
    );
    G := digraph (nodes,edges);
    return G;
)

--###################################
-- Modifying chordal networks 
--###################################

-- modify parents/children of a node
addChildren = method()
addChildren(ChordalNetNode,ChordalNetNode) := ( Ni, Nc ) -> (
    Ni.children = uniqueFast append(Ni.children, Nc);
)
addChildren(ChordalNetNode,VisibleList) := ( Ni, childn ) -> (
    Ni.children = uniqueFast join(Ni.children, childn);
)
addParents = method()
addParents(ChordalNetNode,ChordalNetNode) := ( Ni, Np ) -> (
    Ni.parents = uniqueFast append(Ni.parents, Np);
)
addParents(ChordalNetNode,VisibleList) := ( Ni, parnts ) -> (
    Ni.parents = uniqueFast join(Ni.parents, parnts);
)
delChildren = method()
delChildren(ChordalNetNode,ChordalNetNode) := ( Ni, Nc ) -> (
    Ni.children = delete(Nc,Ni.children);
)
delChildren(ChordalNetNode) := Ni -> (
    Ni.children = {};
)
delParents = method()
delParents(ChordalNetNode,ChordalNetNode) := ( Ni, Np ) -> (
    Ni.parents = delete(Np,Ni.parents);
)
delParents(ChordalNetNode) := Ni -> (
    Ni.parents = {};
)

-- delete node
deleteNode = (NRj,Nj) -> (
    disconectNode(Nj);
    NRj.nodes = delete(Nj, NRj.nodes);
);
-- create/remove arcs between nodes
createArc = ( Ni, Np ) -> (
    addParents(Ni, Np);
    addChildren(Np, Ni);
)
removeArc = ( Ni, Np ) -> (
    delChildren(Np, Ni);
    delParents(Ni, Np);
)
disconectNode = ( Ni ) -> (
    for Np in Ni.parents do 
        delChildren(Np, Ni);
    for Nc in Ni.children do 
        delParents(Nc, Ni);
    delParents(Ni);
    delChildren(Ni);
)
copyAllArcs = ( Ni, Ni2 ) -> (
    copyOutArcs(Ni,Ni2);
    copyInArcs(Ni,Ni2,);
)
copyOutArcs = ( Ni, Ni2 ) -> (
    NP:= Ni.parents;
    for Np in NP do 
        addChildren(Np, Ni2);
    addParents(Ni2, copy(NP));
)
copyInArcs = ( Ni, Ni2, filter ) -> (
    NC:= Ni.children;
    if filter=!=null then NC=select(NC,filter);
    for Nc in NC do 
        addParents(Nc, Ni2);
    addChildren(Ni2, copy(NC));
)
-- copy node Ni, ignoring the arcs from rank rk
copyNodeExcept = (NRi,Ni,rk) -> (
    Ni2 := chordalNetNode(NRi,Ni.rank,{},{});
    copyOutArcs(Ni,Ni2);
    copyInArcs(Ni,Ni2, Nc -> Nc.rank=!=rk );
    return Ni2;
)

--###################################
-- Chordal elimination
--###################################

-- Grobner operations
netNodeGb = method();
netNodeGb(ChordalNetNode) := Ni -> (
    I := ideal Ni;
    Ni.gens = gensGB I;
)

-- Elimination operations
-- TODO: merge after elimination can be done more efficiently
-- if two nodes Ni, Ni2 agree on the last elements, then it suffices to eliminate only one of them.
netNodeElim = method();
netNodeElim(Thing,ChordalNetRank,ChordalNetNode) := (p,NRp,Ni) -> (
    i := Ni.rank;
    NP := Ni.parents;
    if #NP==0 then error("node has no parent");
    hasi := f -> (x:= mvar f; x=!=null and x>p);
    F := partition(f -> hasi(f), Ni.gens, {true,false});
    H := partition(f -> hasi(f), Ni.ineqs, {true,false});
    Ni.gens = F#true;
    Ni.ineqs = H#true;
    NP2 := for Np in NP list (
        Np2 := copyNodeExcept(NRp,Np,i);
        Np2.gens = join(Np.gens, F#false);
        Np2.ineqs = join(Np.ineqs, H#false);
        createArc(Ni,Np2);
        removeArc(Ni,Np);
        if not any(Np.children, Nj->Nj.rank==i) then
            disconectNode(Np);
        Np2
    );
    return NP2;
)
netRankElim = method();
netRankElim(Thing,ElimTree,ChordalNetRank,ChordalNetRank) := (p,tree,NRp, NRi) -> (
    elim := Ni -> netNodeElim(p,NRp,Ni);
    NRp.nodes = mergeOut(NRp.rank,tree,NRi.nodes,elim);
    NRi.nodes = mergeIn(NRi.nodes,Ni->{Ni});
)

-- Chordal elimination
chordalElim = method();
chordalElim(ChordalNet) := N -> (
    if N.isTriangular then error("input is triangularized");
    goodInitial := (i,Ni) -> (
        J := ideal for f in Ni.gens list initial(i,f);
        return J==1;
    );
    tree := N.elimTree;
    guaranteed := true;
    for ip in nodePairs(tree) do (
        (i,p):= ip;
        if cliqueNotContained(i,tree) then
            for Ni in nodesRank(N,i) do(
                if guaranteed then
                    guaranteed = goodInitial(i,Ni);
                netNodeGb Ni;
            );
        if p===null then continue;
        netRankElim(p,tree,N.net#p,N.net#i);
    );
    return guaranteed;
)

--###################################
-- Chordal triangularization
--###################################

-- Triangulation operations
netNodeTria = method();
netNodeTria (List,ChordalNetRank,ChordalNetNode) := (TT,NRi,Ni) -> (
    i := Ni.rank;
    TN := for Tj in TT list (
        Fj := Tj.gens;
        Hj := Tj.ineqs;
        Nj := chordalNetNode(NRi,i,Fj,Hj);
        copyAllArcs( Ni, Nj );
        Nj
    );
    disconectNode( Ni );
    return TN;
)
netNodeTria (Ring,String,ChordalNetRank,ChordalNetNode) := (R,alg,NRi,Ni) -> (
    F := Ni.gens;
    H := Ni.ineqs;
    TT := triangularize(R,F,H,TriangularDecompAlgorithm=>alg);
    return netNodeTria(TT,NRi,Ni);
)
netRankTria = method();
netRankTria (Ring,String,ElimTree,ChordalNetRank) := (R,alg,tree,NRi) -> (
    tria := Ni -> netNodeTria(R,alg,NRi,Ni);
    if alg=="Maple" or alg=="Epsilon" then(
        TT := triangularizeBatch(alg,R,NRi.nodes);
        tria = Ni -> netNodeTria(TT#Ni,NRi,Ni);
    );
    NRi.nodes = mergeOut(NRi.rank,tree,NRi.nodes,tria);
)

netRankDelIneqs = NRi -> for Ni in NRi.nodes do Ni.ineqs = {};

-- Merge operations
mergeOut = (i,tree,nodes,lists) -> (
    Ci := tree.children#i;
    cmpChild := (Ni,Nj) -> (
        badC := 0;
        for c in Ci do 
            if set(nodeChildren(c,Ni))=!=set(nodeChildren(c,Nj)) then(
                badC = badC+1;
                if badC==2 then return false; );
        true );
    cmp := (Ni,Nj) -> if #Ci<=1 then true else cmpChild(Ni,Nj);
    key := Ni -> (Ni.gens,Ni.ineqs,set Ni.parents);
    collide := (Nj,Ncopy) -> (copyInArcs(Ncopy,Nj,); disconectNode(Ncopy););
    return toList uniqueObjects(nodes, lists, key, cmp, collide);
)
mergeIn = (nodes,lists) -> (
    key := Ni -> (Ni.gens,set Ni.children);
    collide := (Nj,Ncopy) -> (copyOutArcs(Ncopy,Nj); disconectNode(Ncopy););
    return toList uniqueObjects(nodes, lists, key, , collide);
)

-- Looks for non equivalent objects in some lists
-- Two objects l,j are equivalent if: key(l)=key(j) and cmp(l,k)
uniqueObjects = (domain, lists, key, cmp, collide) -> (
    searchj := (L,j,seenkj) -> 
        for i in seenkj do 
            if cmp===null or cmp(L#i,j) then return L#i; 
    L := new MutableList;
    seen := new MutableHashTable;
    cnt := 0;
    for i in domain do(
        J := lists(i);
        for j in J do(
            kj := key(j);
            if seen#?kj then (
                l := searchj(L,j,seen#kj);
                if l=!=null then (
                    if collide=!=null then collide(l,j);
                    continue; );
                seen#kj = append(seen#kj,cnt); )
            else 
                seen#kj = {cnt};
            L#cnt = j;
            cnt = cnt+1;
        );
    );
    return L;
)

-- Chordal Triangularization
-- TODO: simplify network by prunning unneeded arcs
-- TODO: allow inputs with size > 1
chordalTria = method(Options=> {TriangularDecompAlgorithm=>null});
chordalTria(ChordalNet) := opts -> N -> (
    if N.isTriangular then return;
    if max size N > 1 then error("network size > 1");
    tree := N.elimTree;
    badC := not testContainedCliques N;
    R := N.ring;
    alg := selectTriaAlg(opts.TriangularDecompAlgorithm, N.Structure,R, );
    ranks := tree.nodes;
    for ip in nodePairs(tree) do (
        (i,p):= ip;
        if badC or cliqueNotContained(i,tree) then
            netRankTria(N.ring,alg,tree,N.net#i);
        if p===null then(
            netRankDelIneqs(N.net#i);
            continue;
        );
        netRankElim(p,tree,N.net#p,N.net#i);
        netRankDelIneqs(N.net#i);
    );
    checkConsistency N;
    relabelNet N;
    N.isTriangular = true;
)

testContainedCliques = N -> (
    tree := N.elimTree;
    for i in tree.nodes do
        if not cliqueNotContained(i,tree) then(
            Ni:= first nodesRank(N,i);
            if #Ni.gens>0 then return false;
        );
    return true;
)

selectTriaAlg = (alg,struct,R,F) -> (
    if alg=!=null then return alg;
    if struct=!=null and struct=!="None" then return struct;
    if F=!=null then(
        struct = getStruct F;
        if struct!="None" then return struct;
    );
    if not testMaple() then
        error "MapleInterface failed. Maple is needed unless ideal is binomial."; 
    if char R==0 and testEpsilon() then return "Epsilon";
    return "Maple";
);

testMaple = () -> try callMaple("returnvalue:=1:") == 1 else 0

testEpsilon = () -> 
    callMaple("returnvalue:=SearchText('epsilon',cat(libname)):") > 0

relabelNet = N -> (
    for iNR in pairs(N.net) do(
        (i,NRi):= iNR;
        NRi.counter = 0;
        for Ni in NRi.nodes do
            Ni.label = getNodeLabel(NRi);
    );
)

--###################################
-- Triangular systems
--###################################

triaSystem = method()
triaSystem(String,Ring,VisibleList,VisibleList) := (struct,R,F,H) -> (
    H = uniqueFast select(H, h -> h!=1);
    new TriaSystem from {
        symbol gens => rsort F,
        symbol ineqs => rsort H,
        symbol ring => R,
        symbol Structure => struct }
)
triaSystem(Ring,VisibleList,VisibleList) := (R,F,H) -> 
    triaSystem("None",R,F,H)
triaSystem(ChordalNet,ChordalNetChain) := (N,C) -> (
    F := flatten for Ni in values C list Ni.gens;
    F = reduceEqs F;
    H := initial F;
    return triaSystem(N.Structure,N.ring,F,H);
)


-- main variable of a polynomial
mvar = method()
mvar (RingElement) := f -> (
    I:= indices f;
    if #I==0 then return null;
    return (ring f)_(first I);
)

-- initial of a polynomial
-- output is monic
initial = method()
initial(RingElement,RingElement) := (x,f) -> (
    if isConstant f then return 1_(ring f);
    d := degree(x,f);
    c := leadCoefficient f;
    return f//(c*x^d);
)
initial(RingElement) := f -> initial(mvar f, f)
initial(List) := T -> for t in T list initial t
initial(TriaSystem) := T -> initial T.gens

-- pseudo remainder of f by g
prem = method()
prem(RingElement,RingElement,RingElement) := (x,f,g) -> (
    d1 := degree(x,f);
    d2 := degree(x,g);
    if d1<d2 then return f;
    h := initial(x,g);
    if not isConstant h then
        f = f * h^(d1-d2+1);
    return f % g;
)
prem(RingElement,TriaSystem) := (f,T) -> (
    if isConstant f then return f;
    for t in T.gens do(
        if f==0 then return f;
        x := mvar t;
        if mvar f < x then continue;
        f = prem(x,f,t);
    );
    return f;
)

resultant(RingElement,TriaSystem) := (f,T) -> (
    if isConstant f then return f;
    for t in T.gens do(
        if f==0 then return f;
        x := mvar t;
        if mvar f < x then continue;
        f = resultant(f,t,x);
    );
    return f;
)

freeVars = T -> (
    F:= T.gens;
    X:= gens T.ring;
    if #F==0 then return X;
    Xalg := for f in F list mvar f, m->m!=1;
    return select(X, x-> not member(x,Xalg));
)


saturate(TriaSystem) := opts -> T -> (
    F:= T.gens;
    H:= T.ineqs;
    --H = select(H, h -> h!=1);
    I:= ideal F;
    for h in H do I = saturate(I,h,opts);
    return I;
)

isRegularChain = T -> (
    for h in initial T do
        if resultant(h,T)==0 then return false;
    return true;
)

isStronglyNormalized = method()
isStronglyNormalized(TriaSystem) := T -> (
    F := T.gens;
    Xalg := select(for f in F list mvar f, m->m!=1);
    Xh := supportF initial F;
    if #(set Xh * set Xalg)>0 then return false;
    return true;
)

-- simple test to determine primality
isPrimeSimple = method()
isPrimeSimple(TriaSystem) := T -> (
    F:= T.gens;
    if not isPrime last F then return false;
    for i to #F-2 do
        if degree(mvar F_i, F_i)>1 then return false;
    return true;
)

--###################################
-- Triangular decompositions
--###################################

-- triangular decomposition
-- algorithms: Monomial, Binomial, Epsilon, Maple
triangularize = method(Options=> {TriangularDecompAlgorithm=>null});
triangularize(List) := opts -> F -> (
    if #F==0 then return {F};
    R:= ring first F;
    alg:= selectTriaAlg(opts.TriangularDecompAlgorithm, ,R,F);
    TT:= if alg=="Monomial" then 
            triangularizeMonomial(F,false)
        else if alg=="Binomial" then
            reduceDecomposition triangularizeBinomial(F,{},R,false)
        else if alg=="Epsilon" then
            triangularizeEpsilon(F,{},R,false)
        else if alg=="Maple" then
            triangularizeMaple(F,{},R,false)
        else
            error("unknown algorithm");
    return TT;
)
triangularize(Ideal) := opts -> I -> 
    triangularize(gensL I,opts)
triangularize(MonomialIdeal) := opts -> I -> 
    triangularizeMonomial I

-- triangular decomposition inequations
triangularize(Ring,List,List) := opts -> (R,F,H) -> (
    if #F==0 then return {triaSystem("Binomial",R,F,H)};
    alg:= selectTriaAlg(opts.TriangularDecompAlgorithm, ,R,F);
    TT:= if alg=="Monomial" then 
            triangularizeMonomial(F,true)
        else if alg=="Binomial" then
            reduceDecomposition triangularizeBinomial(F,H,R,true)
        else if alg=="Epsilon" then
            triangularizeEpsilon(F,H,R,true)
        else if alg=="Maple" then
            triangularizeMaple(F,H,R,true)
        else
            error("unknown algorithm");
    return TT;
)

-- triangularize all nodes simultaneously
triangularizeBatch = (alg,R,nodes) -> (
    P := partition(Ni->#Ni.gens==0, nodes, {true,false});
    N0 := P#true;
    TT0:= hashTable for Ni in N0 list 
        Ni => {triaSystem("Binomial",R,Ni.gens,Ni.ineqs)};
    N1 := P#false;
    F := for Ni in N1 list Ni.gens;
    H := for Ni in N1 list Ni.ineqs;
    TT:= if alg=="Epsilon" then
            triangularizeEpsilonBatch(F,H,R,true)
        else if alg=="Maple" then
            triangularizeMapleBatch(F,H,R,true)
        else
            error("unknown algorithm");
    TT1:= hashTable for k to #N1-1 list N1#k => TT#k;
    return merge(TT0, TT1, (i,j)-> error "unexpected");
)

triangularizeMonomial = method();
triangularizeMonomial(MonomialIdeal,Boolean) := (I,getineqs) -> (
    TT := decompose I;
    return if getineqs then TT / ( T-> triaSystem("Monomial",ring I,gensL T,{}) )
        else TT / ( T-> gensL T );
)
triangularizeMonomial(List,Boolean) := (F,getineqs) -> 
    triangularizeMonomial(monomialIdeal F, getineqs)

triangularizeMaple = (F,H,R,getineqs) -> 
    first triangularizeMapleBatch({F},{H},R,getineqs)

triangularizeMapleBatch = (F,H,R,getineqs) -> (
    getOptions := (lazard,radicl,normalized) -> (
        sLaz:= if lazard then ",'output'='lazard'" else "";
        sRad:= if radicl then ",'radical'='yes'" else "";
        sNor:= if normalized===false then "" else ",'normalized'="|normalized;
        return sLaz | sNor | sRad;
    );
    getRing:= R -> (
        Q:= coefficientRing R;
        if char Q > 0 then
            if not Q#?order or Q.order!=char Q then 
                error("Field not implemented");
        sVars:= toString gens R;
        scharR:= toString char R;
        return "R:=PolynomialRing("|sVars|","|scharR|"): ";
    );
    getTria:= (i,Fi,Hi,sOpt) -> (
        sF:= toString Fi;
        sH:= if #H>0 then "," | toString Hi else "";
        sTria:= "dec:=Triangularize("|sF|sH|",R"|sOpt|"): ";
        sInfo:= "dec" | toString i | ":=[Info(dec,R)]: ";
        return sTria | sInfo;
    );
    getPrint:= (n) -> (
        sDec:= toString for i to n-1 list "dec"|toString i;
        return "returnvalue:=" | sDec | ": ";
    );
    processOutput:= (Hi,TTi) -> (
        if #Hi==0 then( --or not lazard
            TTi = first TTi; 
            if getineqs then TTi = TTi/(T -> triaSystem("None",R,T,initial T) ) ;
        )
        else 
            TTi = TTi / ( T -> triaSystem("None",R,T#0, (T#1|initial T#0)) );
        return TTi;
    );
    n:= #F;
    sPackage:= "with(RegularChains): ";
    sRing:= getRing R;
    sOpt:= getOptions(true,false,false);
    sTria:= concatenate for i to n-1 list getTria(i,F#i,H#i,sOpt);
    sPrint:= getPrint(n);
    program:= sPackage | sRing | sTria | sPrint;
    use R;
    TT:= callMaple(program,"","placeholder1");
    TT = for i to n-1 list processOutput(H#i,TT#i);
    return TT;
)

triangularizeEpsilon = (F,H,R,getineqs) -> 
    first triangularizeEpsilonBatch({F},{H},R,getineqs)

triangularizeEpsilonBatch = (F,H,R,getineqs) -> (
    getRing:= R -> (
        if char R > 0 then
            error("Epsilon does not support positive characteristic");
        sVars:= toString sort gens R;
        return "X:="|sVars|": ";
    );
    getTria:= (i,Fi,Hi) -> (
        if #Fi==0 then error("F must be nonempty");
        if #H>0 then Fi = {Fi,Hi};
        sF:= toString Fi;
        sTria:= "dec:=epsilon[RegSer]("|sF|",X): ";
        sInfo:= "dec" | toString i | ":=[dec]: ";
        return sTria | sInfo;
    );
    getPrint:= (n) -> (
        sDec:= toString for i to n-1 list "dec"|toString i;
        return "returnvalue:=" | sDec | ": ";
    );
    processOutput:= (Hi,TTi) -> (
        if #TTi==1 and #TTi#0==0 then TTi = {};
        if #Hi==0 and not getineqs then
            TTi = TTi / ( TH -> first TH )
        else 
            TTi = TTi / ( T -> triaSystem("None",R,T#0, T#1) );
        return TTi;
    );
    n:= #F;
    sRing:= getRing R;
    sTria:= concatenate for i to n-1 list getTria(i,F#i,H#i);
    sPrint:= getPrint(n);
    program:= sRing | sTria | sPrint;
    use R;
    TT:= callMaple(program,"","placeholder1");
    TT = for i to n-1 list processOutput(H#i,TT#i);
    return TT;
)

-- exhaustive enumeration over subsets of X
triangularizeBinomialNaive = (F,R) -> (
    X := support matrix {F};
    SS := toList(0..<#X);
    TT := {};
    for S in subsets(SS) do (
        Xi := toList(set(SS)-set(S)) / ( i -> X_i );
        J := ideal (F | Xi);
        b := gensGB J; --grevlex
        b1 := select(b, f-> #(terms f)==1);
        if #b1!=#Xi then continue; 
        b2 := select(b, f-> #(terms f)==2);
        T := copy(Xi);
        if #S>0 and #b2>0 then (
            T0 := triangularizeBinomialTorus(b2,X_S,R,true,false);
            T = rsort (T | T0);
        );
        TT = append(TT,T);
    );
    return reduceDecomposition TT;
)

-- returns a Kalkbrener decomposition (not Larzard)
triangularizeBinomialBCD = (F,NZ,R,getineqs) -> (
    I:= ideal F;
    bcd:= binomialCellularDecomposition( I, ReturnCellVars=>true);
    TT:= {};
    for c in bcd do(
        Ic:= c#0;
        NZc:= c#1;
        if not isSubset(NZ,NZc) then continue;
        Zc:= toList (set gens R - NZc);
        Ic = (Ic + ideal Zc); --radical is too expensive
        Fc:= rsort gensGB Ic;
        P:= partition(f-> #(terms f), Fc, {1,2});
        F':= P#2;
        T:= P#1;
        if #F'>0 then(
            T0:= triangularizeBinomialTorus(F',NZc,R,false,false);
            T = T | T0;
        );
        if getineqs then( 
            H:= (NZ | initialBinomial T0);
            T = triaSystem("Binomial",R,T,H);
        );
        TT = append(TT, T);
    );
    return TT;
)

-- NZ: set of nonzero variables
-- TODO: handle the case that T0 = {1}
triangularizeBinomial = (F,NZ,R,getineqs) -> (
    divideByVars := (f,X) -> product(set(support f) - X);
    triangularizeBinomial0 := (F,sF,Z,NZ) -> (
        --assume sF,Z are disjoint
        (T0,NZ'):= triangularizeBinomialTorus(F,sF,R,true,true);
        T:= rsort (Z | T0);
        if not getineqs then return (T, NZ');
        H:= (NZ | initialBinomial T0);
        return (triaSystem("Binomial",R,T,H), NZ');
    );
    queue := {(F,NZ)};
    TT := new MutableList;
    while #queue>0 do(
        (F,NZ) = first queue;
        queue = drop(queue,1);
        F = rsort gensGB ideal F; --grevlex
        P:= partition(f-> #(terms f), F, {1,2});
        F':= P#2; Z:= P#1;
        sF:= support matrix(R, {F'});
        Z = Z / (f-> divideByVars(f,NZ));
        if member(1,Z) then continue;
        if #Z>0 then(
            D:= triangularizeMonomial(Z, false); 
            if #F'==0 then(
                if getineqs then 
                    D = for T in D list triaSystem("Monomial",R,T,NZ);
                TT = join(TT, D );
                continue;
            );
            Z = D#0;
            needReduce:= #(set sF * set Z) > 0;
            if #D>1 or needReduce then (
                queue = queue | for T in D list (F'|T,NZ);
                continue;
            );
        );
        (T,NZ'):= triangularizeBinomial0(F',sF,Z,NZ);
        TT#(#TT) = T;
        queue = queue | for Xi in toList(NZ' - NZ) list (append(F,Xi), NZ);
    );
    return toList TT;
)

-- keeps only minimal sets
reduceDecomposition = TT -> (
    if #TT==0 then return TT;
    isList:= class TT#0 === List;
    len:= if isList then (T -> #T)
        else (T -> #T.gens);
    contained:= if isList then isSublist
        else (S,T) -> isSublist(S.gens,T.gens) and isSublist(S.ineqs,T.ineqs);
    cmp:= (T,T') -> (
        n:=len T; n':=len T';
        if n==n' then return 0;
        if n>n' then
            return if contained(T',T) then 1 else 0;
        return if contained(T,T') then -1 else 0;
    );
    TT = uniqueFast TT;
    minl:= minimalObjs(TT,cmp);
    return select(TT, T->minl#T);
)

-- determines containment of sequences
-- maybe its easier to use isSubset(set S, set T)
isSublist = (S,T) -> (
    n:=#S; N:=#T;
    if n==0 then return true;
    if n>N then return false;
    i:= position( take(T,{0,N-n}), t -> t===S#0 );
    if i===null then return false;
    return isSublist(drop(S,1),drop(T,i+1));
)

-- initials of binomials (on the torus)
initialBinomial = F -> (
    initsupp := f -> drop(support leadMonomial f, 1);
    return flatten ( F / (f-> initsupp f) );
)

-- TODO: handle the case that T = {1}
-- triangularize F in K[X] on the torus
-- NZ: variables that we assume that are nonzero
triangularizeBinomialTorus = (F,X,R,normalized,getNZ) -> (
    initsupp := f -> drop(support leadMonomial f, 1);
    if #F==0 then return {};
    S := indices sum X;
    (A,c,s) := binomial2matrix(F,S);
    NZ := (gens R)_s;
    H:=null; o:=null;
    (H,c,o) = myHermite(X,A, c, normalized);
    NZ = NZ | X_o;
    H = entries H;
    T0 := for i to #H-1 list (
        vec2binomial(X, H_i, c_i) );
    if any(T0,f->f==1) then
        return if getNZ then ({1_R},{}) else {1_R};
    T := select(T0,f->f!=0);
    if not getNZ then return T;
    NZ = set NZ + set initialBinomial T;
    return (T, NZ);
)

-- matrix of exponents of a binomial system
binomial2matrix = (F,S) -> (
    nonzero := new MutableHashTable from 
        hashTable for j in S list (j,false);
    m := #F;
    c := new MutableList from 0..m-1;
    A := matrix for i in 0..m-1 list(
        (mons,coeffs) := coefficients F#i;
        coeffs = flatten entries coeffs;
        if #coeffs!=2 then error("not binomial");
        c#i = -coeffs_1//coeffs_0;
        mons = first entries mons;
        e0 := first exponents mons#0;
        e1 := first exponents mons#1;
        (e0-e1)_S
    );
    nonzero = select(S, j->nonzero#j);
    return (A, toList c, nonzero);
)
-- f = x^e1 - c x^e2
vec2binomial = (x,e,c) -> (
    m1 := product for i to #x-1 list (x_i)^(max(e_i,0));
    m2 := product for i to #x-1 list (x_i)^(max(-e_i,0));
    f := m1 - c*m2;
    if isUnit f then f = 1_(ring f);
    return f;
);

-- reduces rows to echelon form (lower triangular)
-- only for debugging
myhermite = (A) -> (
    A = transpose A;
    Irows := reverse toList (0..<numRows(A));
    Icols := reverse toList (0..<numColumns(A));
    return transpose (hermite(A)^Irows)_Icols^Irows;
)

-- reduces rows to echelon form (nonreduced)
-- negativepivot: whether the entries above pivots must be negative
-- keeps track of columns where cancellations occur
myHermite = (X,A,c,negativepivot) -> (
    m:= numRows A; I:= toList(0..<m);
    n:= numColumns A; J:= toList(0..<n);
    A = new MutableList from entries A;
    c = new MutableList from c;
    badcol := new MutableHashTable from 
        hashTable for j in J list (j,false);
    column := j -> for i in I list (A#i)_j;
    assign := (i,Ai,ci,bcol) -> ( 
        A#i = Ai; 
        c#i = ci; 
        for j in bcol do badcol#j=true;
    );
    scaleRow := (i,j) -> 
        if (A#i)_j<0 then 
            assign(i, -(A#i), 1//c#i, {});
    swapRows := (i1,i2) -> (
        if i1!=i2 then(
            assign(i1, A#i2, c#i2, {});
            assign(i2, A#i1, c#i1, {});
        );
    );
    addRows := (i1,r1,i2,r2,j0) -> (
        A1 := r1*A#i1; 
        A2 := r2*A#i2;
        c12 := (c#i1)^r1*(c#i2)^r2;
        bcol:= select( drop(J,j0), 
            j -> not badcol#j and A1_j*A2_j<0 );
        return (A1+A2, c12, bcol);
    );
    reduceRows := (i1,i2,j) -> (
        x:=A#i1_j; y:=A#i2_j;
        A2:=null; c2:=null; b2:=null;
        if y%x==0 then(
            (A2,c2,b2)=addRows(i2,1,i1,-(y//x),j);
            assign(i2,A2,c2,b2);
        )else if x%y==0 then( 
            (A2,c2,b2)=addRows(i1,1,i2,-(x//y),j);
            assign(i1,A#i2,c#i2,{});
            assign(i2,A2,c2,b2);
        )else(
            D := gcdCoefficients(x,y);
            d:=D#0; r:=D#1; s:=D#2;
            (A1,c1,b1):=addRows(i1,r,i2,s,j);
            (A2,c2,b2)=addRows(i1,y//d,i2,-x//d,j);
            assign(i1,A1,c1,b1);
            assign(i2,A2,c2,b2);
        );
    );
    reducePivot := (i1,i2,j) -> (
        x:=A#i1_j; y:=A#i2_j;
        if y==0 then return;
        q:= -(y//x);
        if y%x!=0 then q = q-1;
        (A2,c2,bcol):= addRows(i1,q,i2,1,j);
        if #bcol==0 or (y>0 and negativepivot) then
            assign(i2,A2,c2,bcol);
    );
    p := 0; --pivot
    Jp := {};
    for j in J do(
        aj := column j;
        Ij := select(drop(I,p), i-> aj_i!=0);
        if #Ij==0 then continue;
        Jp = append(Jp,j);
        swapRows(p,first Ij);
        for i in drop(Ij,1) do
            reduceRows(p,i,j);
        scaleRow(p,j);
        p = p + 1;
    );
    for p to #Jp-1 do
        for i in take(I,{0,p-1}) do
            if false then reducePivot(p,i,Jp#p);
    A = matrix toList A;
    badcol = select(J, j->badcol#j);
    return (A,toList c,badcol);
)

--###################################
-- Reduction
--###################################

reduceNet = method()
reduceNet(ChordalNet) := N -> (
    reduceDiamonds N;
)

-- remove disconnected nodes
checkConsistency = N -> (
    tree:= N.elimTree;
    for jNj in nodePairs(N,Reversed=>true) do (
        (j,Nj):= jNj;
        if tree.parents#j=!=null and 
            #Nj.parents==0 then deleteNode(N.net#j,Nj);
    );
)

-- reduces diamonds of the following form
--   Ni
-- Np  Np'    with Np contained in Np'
--   Nk
reduceDiamonds = N -> (
    keepMaximal := (nodes,cmp,del) -> (
        P:= partition(Ni->Ni.gens, nodes);
        C0:= if P#?{} then P#{} else {};
        for fC in pairs(P) do(
            (f,C):= fC;
            C1:= if f==={} then C else C|C0;
            if #C1<=1 then continue;
            maxl := maximalObjs(C1,cmp);
            badN:= select(C, Np->not maxl#Np);
            for Np in badN do del(Np);
        );
    );
    tree:= N.elimTree;
    cmp0:= (S,T) -> if S>=T then 1 else if S<=T then -1 else 0;
    for jp in nodePairs(tree,NoRoot=>true) do (
        (j,p):= jp;
        child:= Ni -> select(Ni.children, Nc -> Nc.rank=!=j );
        cmp1:= (Ni,Nk) -> cmp0( set(Ni.parents|child(Ni)), set(Nk.parents|child(Nk)) );
        for Nj in nodesRank(N,j) do 
            keepMaximal(Nj.parents,cmp1,Np->removeArc(Nj,Np));
    );
    cmp2:= (Ni,Nk) -> cmp0( set(Ni.children), set(Nk.children) );
    for jp in nodePairs(tree,Reversed=>true,NoRoot=>true) do (
        (j,p):= jp;
        --if #tree.children#p!=1 then continue; --seems unnecessary
        for Np in nodesRank(N,p) do 
            keepMaximal(nodeChildren(j,Np),cmp2,Nj->removeArc(Nj,Np));
    );
)

-- should not be needed, diamonds better
reduceContained = N -> (
    keepZero:= (nodes,del) -> (
        P:= partition(Nj->Nj.gens=={}, nodes, {true,false});
        if #P#true>0 then
            for Nj in P#false do del(Nj);
    );
    tree:= N.elimTree;
    for j in leaves(tree) do(
        p:= tree.parents#j;
        if p===null then continue; 
        for Np in nodesRank(N,p) do
            keepZero(nodeChildren(j,Np), Nj->removeArc(Nj,Np));
    );
    for p in tree.Roots do(
        C:= tree.children#p;
        if #C!=1 then continue;
        j:= C#0;
        for Nj in nodesRank(N,j) do
            keepZero(Nj.parents, Np->removeArc(Nj,Np));
    );
)

-- minimial elements of a partial order
-- cmp(a,b) = 1 if a>=b
-- cmp(a,b) = -1 if a<b
-- cmp(a,b) = 0 if incomparable 
minimalObjs = (Os,cmp) -> (
    l := #Os;
    I := new MutableList from (0..<l);
    i := 0;
    while i<l do (
        j := i+1;
        if j>=l then break;
        while j<l do (
            rel := cmp(Os#(I#i),Os#(I#j));
            if rel==1 then (
                l = l-1;
                Ii:=I#i; I#i = I#l; I#l = Ii;
                break; )
            else if rel==-1 then (
                l = l-1;
                Ij:=I#j; I#j = I#l; I#l = Ij; )
            else(
                j = j+1;
                if j==l then i= i+1;
            );
        );
    );
    Il := toList take(I,l);
    return hashTable for i in 0..#I-1 list (Os#i,member(i,Il));
)
maximalObjs = (Os,cmp) -> (
    cmp2 := (a,b) -> cmp(b,a);
    return minimalObjs(Os,cmp2);
)

--###################################
-- Chains of a chordal network
--###################################

chordalNetChain = method()
chordalNetChain(HashTable) := chain -> (
    if mutable chain then 
        chain = new HashTable from chain;
    new ChordalNetChain from chain
)

codim(ChordalNetChain) := optsC -> (
    -- ignore first argument (options)
    chain:= optsC_1;
    cdim:= sum for Nj in values chain list #Nj.gens;
    return cdim;
)

-- iterates over the chains of a chordal network
nextChain = method();
nextChain(ChordalNet) := (N) -> (
    tree := N.elimTree;
    C := new MutableHashTable from 
        for j in tree.nodes list (j,null);
    rnodes := reverse(tree.nodes);
    success:= fillChain(N,C,rnodes,0,);
    if not success then return null;
    return chordalNetChain(C);
)
nextChain(ChordalNetChain,ChordalNet) := (chain,N) -> (
    tree:= N.elimTree;
    nodes := tree.nodes;
    rnodes := reverse nodes;
    C:= new MutableHashTable from chain;
    succ:= false;
    for i to #nodes-1 when not succ do (
        j := nodes_i;
        ir := #nodes-i-1;
        succ= fillChain(N,C,rnodes,ir,C#j);
    );
    if not succ then return null;
    return chordalNetChain(C); 
)

fillChain = (N,C,nodes,i,Nj0) -> (
    tree := N.elimTree;
    j:= nodes#i;
    succj := nextNj(N,C,tree,j,Nj0,);
    if not succj then return false
    else if i==#nodes-1 then return true;
    succ:= fillChain(N,C,nodes,i+1,);
    return succ;
)
nextNj = (N,C,tree,j,Nj0,filter) -> (
    p:= tree.parents#j;
    NRj:= if p===null then nodesRank(N,j)
        else nodeChildren(j,C#p);
    if filter=!=null then 
        NRj = select(NRj, Nj -> filter(Nj) );
    k:= if Nj0===null then 0
        else 1+position(NRj, Nj->Nj===C#j);
    if k==#NRj then return false;
    C#j = NRj_k;
    return true;
)

-- iterates over the chains of a given codimension
-- TODO: use codimension counts to make it more efficient
nextChain(ZZ,ChordalNet) := (cdim,N) -> (
    cdimTab := getCdimTable(N);
    if not member(cdim,cdimTab#0#null) then return (null,cdimTab);
    tree := N.elimTree;
    rnodes := reverse(tree.nodes);
    C := new MutableHashTable from 
        for j in tree.nodes list (j,null);
    Cdim := new MutableHashTable from C;
    roots := tree.Roots;
    succ := fillChainRoots(N,C,rnodes,cdimTab,0,Cdim,roots,cdim,);
    if not succ then return null;
    return (chordalNetChain(C),cdimTab);
)
nextChain(ChordalNetChain,Sequence,ZZ,ChordalNet) := (chain,cdimTab,cdim,N) -> (
    if codim chain =!= cdim then error "bad codimension of initial chain";
    tree:= N.elimTree;
    C:= new MutableHashTable from chain;
    Cdim:= new MutableHashTable from 
        for j in tree.nodes list (j,null);
    succ:= false;
    nodes:= append(tree.nodes,null);
    rnodes:= reverse nodes;
    for i to #nodes-1 when not succ do (
        j := nodes_i;
        child:= tree.children#j;
        P:= for c in child list Cdim#c;
        cdimj:= sum P;
        if j=!=null then Cdim#j = cdimj + #(C#j.gens);
        ir := #nodes-i-1;
        if #child>0 then
            succ= fillChainRoots(N,C,rnodes,cdimTab,ir+1,Cdim,child,cdimj,P);
        if not succ and j=!=null then
            succ= fillChainI(N,C,rnodes,cdimTab,ir,Cdim,C#j);
    );
    if not succ then return null;
    return chordalNetChain(C);
)

getCdimTable = N -> (
    tree := N.elimTree;
    nodeTab := codimCount(N, GetTable=>true);
    nodeTab = applyPairs(nodeTab, (Nj,f)-> (Nj,flatten exponents f));
    rankTab := hashTable for j in tree.nodes list(
        dimsj:= for Nj in nodesRank(N,j) list nodeTab#Nj;
        (j,unique flatten dimsj) 
    );
    return (nodeTab,rankTab);
)

fillChainRoots = (N,C,nodes,cdimTab,i,Cdim,roots,cdim,P0) -> (
    L:= for c in roots list cdimTab#1#c;
    P:= if P0===null then nextOrderedPartition(cdim,L)
        else nextOrderedPartition(P0,cdim,L);
    while P=!=null do(
        for i to #roots-1 do Cdim#(roots_i) = P_i;
        success:= fillChainI(N,C,nodes,cdimTab,i,Cdim,);
        if success then return true;
        P = nextOrderedPartition(P,cdim,L);
    );
    return false;
)
fillChainI = (N,C,nodes,cdimTab,i,Cdim,Nj0) -> (
    tree := N.elimTree;
    j:= nodes#i;
    filter := Nj -> member(Cdim#j,cdimTab#0#Nj);
    succj := nextNj(N,C,tree,j,Nj0,filter);
    if not succj then return false
    else if i==#nodes-1 then return true;
    child := tree.children#j;
    cdim := Cdim#j - #(C#j.gens);
    succ:= if #child>0 then
            fillChainRoots(N,C,nodes,cdimTab,i+1,Cdim,child,cdim,)
        else fillChainI(N,C,nodes,cdimTab,i+1,Cdim,);
    return succ;
)

-- given lists L1..Lk of distinct nonnegative integers, 
-- iterate over all tuples (l1,..,lk) with sum equal to S
nextOrderedPartition = method()
nextOrderedPartition(ZZ,List) := (S,Lists) -> (
    k:=#Lists;
    if k==1 then return if member(S,Lists_0) then {S} else null;
    P:= new MutableList from (0..<#Lists);
    Lmin := Lists / min;
    success:= fillPartitionI(Lists,Lmin,P,0,S);
    if success then return toList P;
    return null;
)
nextOrderedPartition(List,ZZ,List) := (partn,S,Lists) -> (
    k:=#Lists;
    if k==1 then return null;
    L:= new MutableList from Lists;
    Lmin := Lists / min;
    P:= new MutableList from partn;
    for i in reverse(0..<k) do(
        Li:= select(Lists_i, l -> l<=S-sum(drop(Lmin,i+1)));
        j:= position(Li,l-> l==P#i);
        L#i = drop(Li,j+1);
        Si:= S - sum drop(partn,{i,k-1});
        success:= fillPartitionI(L,Lmin,P,i,Si);
        if success then return toList P;
        L#i = Li;
    );
    return null;
)
fillPartitionI = (L,Lmin,P,i,S) -> (
    if i==#P-1 then(
        if not member(S,L#i) then return false;
        P#i = S;
        return true;
    );
    Li:= select(L#i, l -> l<=S-sum(drop(Lmin,i+1)));
    for l in Li do(
        P#i = l;
        succ:= fillPartitionI(L,Lmin,P,i+1,S-l);
        if succ then return true;
    );
    return false;
)


--###################################
-- Methods for chordal networks
--###################################

ring(ChordalNet) := N -> N.ring

checkTriangular = N -> 
    if not N.isTriangular then error "Network must be triangular";

codim(ChordalNet) := optsN -> (
    -- ignore first argument (options)
    N := optsN_1;
    checkTriangular N;
    initState := Nj -> if Nj=!=null then #Nj.gens else 0;
    initMess := Np -> infinity;
    message := (cdimNj,messg) -> min(cdimNj,messg);
    update := (cdimNp,messg) -> cdimNp+messg;
    Cdims := messagePassing(N,initState,initMess,message,update);
    return Cdims#null;
)

dim(ChordalNet) := N -> numgens(N.ring) - codim N

codimCount = method(Options => {GetTable=>false})
codimCount(ChordalNet) := opts -> N -> (
    checkTriangular N;
    tree := N.elimTree;
    nodes := nodePairs(N) / (jNj -> jNj_1);
    S := ZZ(monoid[getSymbol "t"]);
    t := first gens S;
    initState := Nj -> if Nj=!=null then t^(#Nj.gens) else 1_S;
    initMess := Np -> 0_S;
    message := (cdimNj,messg) -> cdimNj+messg;
    update := (cdimNp,messg) -> cdimNp*messg;
    Cdims := messagePassing(N,initState,initMess,message,update);
    return if opts.GetTable then Cdims else Cdims#null;
)

rootCount = method()
rootCount(ChordalNet) := N -> (
    checkTriangular N;
    initState := Nj -> 
        if Nj===null then 1
        else if #Nj.gens==0 then 0
        else (f:=first Nj.gens; degree(Nj.rank,f));
    initMess := Np -> 0;
    message := (rootsNj,messg) -> rootsNj+messg;
    update := (rootsNp,messg) -> rootsNp*messg;
    Nroots := messagePassing(N,initState,initMess,message,update);
    return Nroots#null;
)

-- message passing algorithm on the nodes of a chordal network
-- top-down pass over the tree
messagePassing = (N,initState,initMess,message,update) -> (
    tree := N.elimTree;
    nodes := append(nodePairs(N) / (jNj -> jNj_1), null);
    States := new MutableHashTable from
        for Nj in nodes list Nj=>initState(Nj);
    for jp in nodePairs(tree) do (
        (j,p):= jp;
        nodesj := nodesRank(N,j);
        nodesp := if p===null then {null}
            else nodesRank(N,p);
        messg := new MutableHashTable from
            for Np in nodesp list Np=>initMess(Np);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            for Nj in NJ do
                messg#Np = message(States#Nj,messg#Np);
        );
        for Np in nodesp do
            States#Np = update(States#Np,messg#Np);
    );
    return new HashTable from States;
)

-- only for debugging
printState = (N,States) -> (
    for j in N.elimTree.nodes do(
        print ("Rank: " | toString j);
        print for Nj in nodesRank(N,j) list States#Nj;
    );
    print ("Rank: null");
    print {States#null};
);

-- radical membership
-- top-down pass over the tree
prem(RingElement,ChordalNet) := (f,N) -> (
    initializeTop := (nodeP,R,f) -> (
        x0 := mvar f;
        for jNj in nodeP list(
            (j,Nj):=jNj; 
            if j===x0 then Nj=>f else Nj=>0_R )
    );
    initializeSplit := (nodeP,R,f) -> (
        P := partition(m -> mvar m, terms f, (gens R)|{null});
        P = applyPairs ( P , (j,Fj) -> (j, sum Fj) );
        assert( sum values P == f );
        for jNj in nodeP list(
            (j,Nj):=jNj;
            Nj => P#j )
    );
    premEval := (Dict, dict, f, Np) -> (
        if Np===null or f==0 then return f;
        x := Np.rank;
        Fp := Np.gens;
        if #Fp>0 then f = prem(x, f, Fp#0);
        f = dict#x(f);
        return f;
    );
    affineCoeffs := (Q,S) -> (
        r := 0_Q;
        hashTable for i to #S-1 list( 
            ri := random Q;
            if i<#S-1 then r = r + ri
            else ri = 1_Q - r;
            S#i=>ri )
    );
    checkTriangular N;
    if ring N =!= ring f then error("Rings don't match");
    R := N.ring;
    Q := coefficientRing R;
    dict := hashTable for x in gens R list 
        (x, map(R,R,{x=>random Q}));
    Dict := product values dict;
    tree := N.elimTree;
    nodeP := append(nodePairs N, (null,null) );
    F := new MutableHashTable from 
        if hasInitials N then initializeTop(nodeP,R,f)
        else initializeSplit(nodeP,R,f);
    for jp in nodePairs(tree) do (
        (j,p):= jp;
        nodesj := nodesRank(N,j);
        nodesp := if p===null then {null}
            else nodesRank(N,p);
        for Nj in nodesj do
            F#Nj = premEval(Dict,dict,F#Nj,Nj);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            C:= affineCoeffs(Q,NJ);
            for Nj in NJ do
                F#Np = C#Nj * F#Nj + F#Np;
        );
    );
    return F#null;
)

RingElement % ChordalNet := (f,N) -> prem(f,N)

hasInitials = N -> (
    for jNj in nodePairs N list (
        Fj:= (jNj#1).gens;
        if #Fj>0 then 
            if #(indices leadTerm Fj#0) > 1 then
                return true;
    );
    return false;
);

-- simple test to check if all chains are prime
isPrimeSimple(ChordalNet) := N -> (
    (tailAny,nonTailAll) := tailNodes N;
    for Nj in tailAny do
        if not isPrime Nj.gens#0 then return false;
    for Nj in nonTailAll do
        if degree(Nj.rank,Nj.gens#0)>1 then return false;
    return true;
)

-- tailAny: nodes that are the tail of some chain
-- tailAll: nodes that are the tail of all chains
-- nonTailAll: complement of tailAll
-- bottom-up pass over the tree
tailNodes = N -> (
    isZero := Nj -> #Nj.gens==0;
    checkTriangular N;
    tree := N.elimTree;
    r := min tree.Roots;
    nodes := nodePairs(N) / (jNj -> jNj_1);
    nonzero := select(nodes, Nj -> not isZero Nj);
    tailAny := nonzero;
    nonTailAll := select(nonzero, Nj -> Nj.rank=!=r);
    if #leaves(tree)>1 then -- TODO: handle more leaves
        return (tailAny,nonTailAll);
    -- any path consisting of zeros
    anyZeroPath := new MutableHashTable from
        for Nj in nodes list Nj=>(Nj.rank==r);
    -- all paths consist of zeros
    allZeroPath := copy anyZeroPath;
    anyP := Np -> isZero Np and anyZeroPath#Np;
    allP := Np -> isZero Np and allZeroPath#Np;
    for jp in nodePairs(tree,Reversed=>true,NoRoot=>true) do (
        (j,p):= jp;
        nodesj := nodesRank(N,j);
        for Nj in nodesj do(
            NP:= Nj.parents;
            anyZeroPath#Nj = any(NP,anyP);
            allZeroPath#Nj = all(NP,allP);
        );
    );
    tailAny = select(nonzero, Nj -> anyZeroPath#Nj);
    nonTailAll = select(nonzero, Nj -> not allZeroPath#Nj);
    return (tailAny,nonTailAll);
)

-- isolates the top dimensional components of a chordal network
-- bottom-up pass over the tree
topComponents(ChordalNet) := N -> (
    tree:= N.elimTree;
    (nodeTab,rankTab) := getCdimTable(N);
    nodeTab = applyPairs(nodeTab, (Nj,cdims)->(Nj,min(cdims)));
    rankTab = applyPairs(rankTab, (j,cdims)->(j,min(cdims)));
    for jNj in nodePairs(N,Reversed=>true) do (
        (j,Nj):= jNj;
        cdimNj:= nodeTab#Nj;
        if tree.parents#j=!=null then (
            for Np in Nj.parents do(
                cdimNp:= min(nodeChildren(j,Np) / (Nk->nodeTab#Nk) );
                if cdimNp<cdimNj then removeArc(Nj,Np);
            );
            if #Nj.parents==0 then deleteNode(N.net#j,Nj);
        )else
            if rankTab#j<cdimNj then deleteNode(N.net#j,Nj);
    );
)

-- eliminates arcs/nodes absent in the top k dimensions
-- bottom-up pass over the tree
reduceDimension = (N,k) -> (
    tree:= N.elimTree;
    (nodeTab,rankTab) := getCdimTable(N);
    nodeTab = applyPairs(nodeTab, (Nj,cdims)->(Nj,min(cdims)));
    nodes := append(nodePairs(N) / (jNj -> jNj_1), null);
    regret := new MutableHashTable from for Nj in nodes list 
            if Nj===null then Nj=>0 else Nj=>infinity;
    for jp in nodePairs(tree,Reversed=>true) do (
        (j,p):= jp;
        nodesj := nodesRank(N,j);
        nodesp := if p===null then {null}
            else nodesRank(N,p);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            cdim := min(NJ / (Nj->nodeTab#Nj));
            for Nj in NJ do(
                regretjp := regret#Np + (nodeTab#Nj - cdim);
                if regretjp >= k then(
                    if Np=!=null then removeArc(Nj,Np);
                    if #Nj.parents==0 then deleteNode(N.net#j,Nj);
                );
                regret#Nj = min(regretjp,regret#Nj);
            );
        );
    );
    checkConsistency N;
)

-- Components of the network (assuming they are prime)
-- brute force enumeration
components(ChordalNet,ZZ) := (N,topk) -> (
    R := N.ring;
    Rd := changeRing(R,,GRevLex);
    phi := map(Rd,R);
    psi := map(R,Rd);
    isMinimalEval := (dict,T,S0) -> (
        F := T.gens;
        X := freeVars T;
        subsX:= if #X==0 then id_R
            else product for x in X list dict#x;
        I := subsX(ideal F);
        return not any(S0, I0 -> isSubset(subsX(I0),I));
    );
    isMinimalPrem := (T,S0) -> (
        return not any(S0, I0 -> isContainedIn(I0,T));
    );
    componentsCdim := (dict,S,cdim) -> (
        L := new MutableList;
        (C,data) := nextChain(cdim,N);
        S0 := flatten values S;
        while C=!=null do(
            T := triaSystem(N,C);
            if isMinimalPrem(T,S0) then
                L#(#L) = psi saturate phi T;
            C = nextChain(C,data,cdim,N);
        );
        S#cdim = unique toList L;
    );
    Cdims := sort flatten exponents codimCount N;
    if topk>0 then Cdims = take(Cdims,{0,topk-1});
    Q := coefficientRing R;
    dict := hashTable for x in gens R list 
        (x, map(R,R,{x=>random Q}));
    S := new MutableHashTable;
    for cdim in Cdims do(
        componentsCdim(dict,S,cdim)
    );
    return new HashTable from S;
)
components(ChordalNet) := (N) -> components(N,0)

RingMap TriaSystem := (phi,T) -> (
    F := for f in T.gens list phi(f);
    H := for f in T.ineqs list phi(f);
    triaSystem(T.Structure, phi.target, F, H)
)

isContainedIn = method()
-- is I contained in sat(T)?
isContainedIn(Ideal,TriaSystem) := (I,T) -> 
    all(gensL I, f -> prem(f,T)==0)
-- is V(I) contained in Z(T)?
-- this implies that sat(T) \subset I
isContainedIn(TriaSystem,Ideal) := (T,I) -> (
    F:= T.gens;
    H:= T.ineqs;
    return all(F, f -> f%I==0) and (I+ideal H)==1;
)

--##########################################################################--
-- Documentation and Tests
--##########################################################################--

beginDocumentation()

load "./Chordal/ChordalDoc.m2";

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

