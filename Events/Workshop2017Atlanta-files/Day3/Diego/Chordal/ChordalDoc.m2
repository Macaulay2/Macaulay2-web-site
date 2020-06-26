--###################################
-- Types
--###################################

doc /// --Chordal
    Key
        Chordal
    Headline
        exploiting chordal structure in polynomial ideals
    Description
      Text
        This package provides several specialized routines for structured polynomial ideals.
        The sparsity structure of a polynomial set can be encoded in terms of graph.
        By exploiting some suitable "chordal completion" of this graph, it is possible to develop more efficient algorithms for several problems in computational algebraic geometry.

        The examples below illustrate how to use this package to compute the following properties of a structured ideal: compute elimination ideals, count the number of zeros, determine the dimension, decompose the variety.
      Example
        1+1
    --Caveat
    SeeAlso
///

doc /// --ChordalGraph
    Key
        ChordalGraph
    Headline
        a chordal graph
    Description
      Text
        This type represents a chordal graph G, together with a perfect elimination ordering.

        An ordering of the vertices is a perfect elimination ordering if for each vertex $v$ the vertices $N(v)$ form a clique, where $N(v)$ consists of the adjacent nodes that occur after $v$ in the ordering.
        A graph is chordal if and only if it has a perfect elimination ordering.
        For notational convenience, ChordalGraph orients the edges of the graph according to such ordering.

      Example
        1+1
    --Caveat
    SeeAlso
///

doc /// --ElimTree
    Key
        ElimTree
        (net,ElimTree)
        nodes
        Roots
        Cliques
    Headline
        the elimination tree of a chordal graph
    Description
      Text
        This type represents the elimination tree of a chordal graph.

        The arcs of a chordal graph can be directed according to a perfect elimination ordering.
        The elimination tree of the graph is the transitive closure of such directed acyclic graph.

      Example
        1+1
    --Caveat
    SeeAlso
///

doc /// --ChordalNet
    Key
        ChordalNet
        (net,ChordalNet)
    Headline
        a chordal network
    Description
      Text
        This type describes a chordal network representation of a polynomial ideal.
        A chordal network is a representation of a polynomial ideal whose sparsity structure is given by a chordal graph.
        Chordal networks can be effectively used to compute several properties of the underlying variety, such as elimination ideals, cardinality, dimension and radical ideal membership.

      Example
        1+1
    --Caveat
    SeeAlso
        chordalNet
        displayNet
        chordalElim
        chordalTria
///

doc /// --ChordalNetChain
    Key
        ChordalNetChain
    Headline
        a chain of a chordal network
    Description
      Text
        This type represents a chain of a chordal network.

      Example
        1+1
    --Caveat
    SeeAlso
        nextChain
        codimCount
///

--###################################
-- Methods/Functions
--###################################

doc /// --chordalGraph
    Key
        chordalGraph
        (chordalGraph,Graph)
        (chordalGraph,Graph,List)
        (chordalGraph,Digraph)
    Headline
        chordal completion of a graph
    Usage
        chordalGraph(G)
        chordalGraph(G,ordering)
    Inputs
        G:Graph
        ordering:List
          (optional)
    Outputs
        :ChordalGraph
          chordal graph cG that contains G as a subgraph
    Consequences
    Description
      Text
        This method finds a simple chordal completion of a given graph G.
        A chordal completion is a supergraph of G that is chordal.
        If a vertex ordering is given, it completes the graph using this ordering; otherwise it finds one using a minimum degree ordering heuristic.
      Example
        G = wheelGraph(6)
        chordalGraph G
      Example
        G = graph(toList(0..9),{
            {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
            {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} });
        chordalGraph G
      Code
      Pre
    Caveat
        If the input is a digraph, it must be topologically ordered; no check is made.
    SeeAlso
        ChordalGraph
///

doc /// --elimTree
    Key
        elimTree
        (elimTree,ChordalGraph)
    Headline
        elimination tree of a chordal graph
    Usage
        elimTree G
    Inputs
        G:ChordalGraph
    Outputs
        :ElimTree
          elimination tree of a chordal graph
    Consequences
    Description
      Text
        This method computes the elimination tree of some given chordal graph.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
///;

doc /// --treewidth
    Key
        treewidth
        (treewidth,ChordalGraph)
        (treewidth,ElimTree)
    Headline
        treewidth of a graph
    Usage
        treewidth G
    Inputs
        G:ChordalGraph
    Outputs
        k:
          treewidth
    Consequences
    Description
      Text
        This method computes the treewidth of a chordal graph.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
///;

doc /// --constraintGraph
    Key
        constraintGraph
        (constraintGraph,Ideal)
    Headline
        constraint graph of a polynomial set
    Usage
        constraintGraph I
    Inputs
        I:Ideal
    Outputs
        :Graph
          constraint graph of the generators of the ideal
    Consequences
    Description
      Text
        This method constructs the constraint graph associated to a polynomial set $F = \{f_1,\dots,f_m\}$.
        The vertices of this graph are the variables, and there is an edge connecting two variables iff there is a polynomial that uses both of them.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --suggestVariableOrder
    Key
        suggestVariableOrder
        (suggestVariableOrder,Ideal)
    Headline
        suggests a good variable ordering
    Usage
        suggestVariableOrder I
    Inputs
        I:Ideal
    Outputs
        :List
          variable ordering
        :RingMap
          ring map to convert to such ordering
    Consequences
    Description
      Text
        This method suggests a good variable ordering, and constructs the ring map to obtain such ordering.
        The ordering is chosen by finding a good chordal completion of its constraint graph.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        constraintGraph
        chordalGraph
///

doc /// --chordalNet
    Key
        chordalNet
        (chordalNet,Ideal)
        (chordalNet,Ideal,ElimTree)
    Headline
        constructs a chordal network from a polynomial set
    Usage
        chordalNet I
        chordalNet (I, tree)
    Inputs
        I:Ideal
        tree:ElimTree 
          (optional)
    Outputs
        :ChordalGraph
          chordal network constructed from the generators of the ideal
    Consequences
    Description
      Text
        This method constructs a chordal network from a given polynomial set F.
        This chordal network is a directed graph, whose nodes define a partition of F.
        The arcs of the directed graph are given by the elimination tree of the associated constraint graph.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        ChordalNet
///

doc /// --displayNet
    Key
        displayNet
        (displayNet,ChordalNet)
        (displayNet,Function,ChordalNet)
        (displayNet,Function,String,ChordalNet)
        (displayNet,Function,String,String,ChordalNet)
    Headline
        displays a chordal network using Graphviz
    Usage
        displayNet N
        displayNet (dotFileName, N)
        displayNet (dotFileName, jpgFileName, N)
    Inputs
        N:ChordalNet
        dotFileName:String
          (optional)
        jpgFileName:String
          (optional)
    --Outputs
    Consequences
    Description
      Text
        Displays a chordal network using Graphviz.
      Code
      Pre
    --Caveat
    SeeAlso
        (digraph,ChordalNet)
        displayGraph
///

doc /// --digraph
    Key
        (digraph,ChordalNet)
    Headline
        digraph associated to a chordal network
    Usage
        digraph N
    Inputs
        N:ChordalNet
    Outputs
        G:Digraph
    Consequences
    Description
      Text
        Returns the digraph associated to a chordal network.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        displayNet
        digraph
///

doc /// --chordalElim
    Key
        chordalElim
        (chordalElim,ChordalNet)
    Headline
        performs elimination on the chordal network
    Usage
        chordalElim N
    Inputs
        N:ChordalNet
    Outputs
        guaranteed:Boolean
          whether the output is guaranteed to be the elimination ideals
        :
          no output; the input chordal network is modified
    Consequences
    Description
      Text
        This method performs successive elimination on a given chordal network.
        Under suitable conditions this procedure computes the elimination ideals.

        Let $I\subseteq k[x_0,\dots,x_{n-1}]$ be the input ideal.
        The "approximate" $j$-th elimination ideal $I_j$ consists of the polynomials in the output network with main variable at most $x_j$.
        The containment $I_j \subseteq I\cap k[x_{j},\dots,x_{n-1}]$ always holds.
        If guaranteed=true, then $I_j$ provably agrees with $I\cap k[x_j,\dots,x_{n-1}]$ (up to radical).

        Example 3.1 of [Cifuentes-Parrilo'16]
      Text
        (chordalElim computes the elimination ideals of I)
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^4-1, x_0^2+x_2, x_1^2+x_2, x_2^2+x_3};
        N = chordalNet I;
        chordalElim N
        N
        flatten entries gens gb I
      Text
        Example 3.2 of [Cifuentes-Parrilo'16]
      Text
        (chordalElim does not compute the elimination ideals of I)
      Example
        R = QQ[x_0..x_2, MonomialOrder=>Lex];
        I = ideal {x_0*x_1+1, x_1+x_2, x_1*x_2};
        N = chordalNet I;
        chordalElim N
        N
        flatten entries gens gb I
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --chordalTria
    Key
        chordalTria
        (chordalTria,ChordalNet)
    Headline
        makes a chordal network triangular
    Usage
        chordalTria N
    Inputs
        N:ChordalNet
    Outputs
        :
          no output; the input chordal network is modified
    Consequences
    Description
      Text
        This method puts a chordal network into triangular form.
        A triangular chordal network can be effectively used to compute several properties of the underlying variety.

        Example 3.1 of [Cifuentes-Parrilo'17]
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I
        chordalTria N;
        N
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --triangularize
    Key
        triangularize
        (triangularize,Ideal)
        (triangularize,MonomialIdeal)
        (triangularize,List)
    Headline
        triangular decomposition of an ideal
    Usage
        triangularize I
    Inputs
        I:Ideal
    Outputs
        :List
          a list of polynomial sets, each of them a regular chain
    Consequences
    Description
      Text
        Computes a triangular decomposition of an ideal.
        The package implements algorithms for monomial and binomial ideals.
        For arbitrary ideals we interface to Maple.

        A triangular decomposition of an ideal $I$ is a collection of "simpler" sets $T_1,\ldots,T_k$ that decompose the variety of $I$.
        These simpler sets, called regular chains, have very nice algorithmic properties.
        Each regular chain $T_i$ has an associated "zero set" $W(T_i)$, such that
        $V(I) = W(T_1)\cup\cdots\cup W(T_k)$.

        A polynomial set $T = \{t_1,\ldots,t_m\}$ is triangular if it consists of polynomials with distinct main variables.
        A regular chain is a triangular set that satisfies additional regularity properties.
        The zero set a regular chain $T$ is the quasi-component
        $W(T) = V(T) \setminus V(h)$,
        where $h$ is the product of the initials of $T$.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        (triangularize,Ring,List,List)
///

doc /// --triangularize TriaSystem
    Key
        (triangularize,Ring,List,List)
    Headline
        triangular decomposition of a polynomial system
    Usage
        triangularize(R,F,H)
    Inputs
        R:Ring
        F:List
          a list of polynomials
        H:List
          a list of polynomials
    Outputs
        :List
          a list of triangular systems
    Consequences
    Description
      Text
        Computes a triangular decomposition of a polynomial system.
        The package implements algorithms for monomial and binomial sets.
        For arbitrary systems we interface to Maple.
        
        A polynomial system is a pair $(F,H)$ where $F,H$ are polynomial sets.
        The zero set of the pair is 
        $Z(F,H) = \{x : f(x)= 0 for f\in F, h(x)\neq 0 for h\in H\}$.
        A triangular decomposition of $(F,H)$ is a collection of "simpler" polynomial systems $(T_1,U_1),\ldots,(T_k,U_k)$ such that
        $Z(F,H) = Z(T_1,U_1)\cup\cdots\cup Z(T_k,U_k)$.
        These simpler sets, called triangular systems, have very nice algorithmic properties.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        triangularize
///

doc /// --nextChain
    Key
        nextChain
        (nextChain,ChordalNet)
        (nextChain,ChordalNetChain,ChordalNet)
        (nextChain,ZZ,ChordalNet)
        (nextChain,ChordalNetChain,Sequence,ZZ,ChordalNet)
    Headline
        iterates over the chains of a chordal network
    Usage
        C = nextChain(N)
        C' = nextChain(C,N)
        (C,data) = nextChain(cdim,N)
        C' = nextChain(C,data,cdim,N)
    Inputs
        N:ChordalNet
        C:ChordalNetChain
          an initial chain (optional)
        cdim:ZZ
          codimension of the chain (optional)
        data:Sequence
          cached data from previous computations (if codimension is specified)
    Outputs
        C':ChordalNetChain
          the next chain of the chordal network, if any
        data:Sequence
          cached data for future compuations (if codimension is specified)
    Consequences
    Description
      Text
        This method produces the chains of a chordal network one at a time.
        It can also iterate only over chains of a specified codimension.
        
        Returns "null" if none.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        ChordalNetChain
        codimCount
///

doc /// --nextOrderedPartition
    Key
        nextOrderedPartition
        (nextOrderedPartition,ZZ,List)
        (nextOrderedPartition,List,ZZ,List)
    Headline
        iterates over ordered partitions of a number
    Usage
        P = nextOrderedPartition(n,Lists)
        P' = nextOrderedPartition(P,n,Lists)
    Inputs
        n:ZZ
        Lists:List
        P:List
          an initial partition (optional)
    Outputs
        P':List
          the next ordered partition, if any
    Consequences
    Description
      Text
        Given an integer $n$ and lists $L_1,\ldots,L_k$ of distinct nonnegative integers, this method iterates over all tuples $(l_1,\ldots,l_k)$ such that $\sum_i l_i = n$ and $l_i\in L_i$.
        The tuples are produced one at a time.

        Returns "null" if none.
      Example
        L = {{0,1},{0,1,2},{2,3}};
        P = nextOrderedPartition (5,L)
        P = nextOrderedPartition (P,5,L) 
        P = nextOrderedPartition (P,5,L) 
        assert(nextOrderedPartition (P,5,L) === null)
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --dimension
    Key
        (dim,ChordalNet)
        (codim,ChordalNet)
    Headline
        dimension of a chordal network
    Usage
        dim N
        codim N
    Inputs
        N:ChordalNet
    Outputs
        d:
          dimension of the associated variety
    Consequences
    Description
      Text
        This method computes the dimension of the variety associated to a chordal network.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        codimCount
///

doc /// --codimCount
    Key
        codimCount
        (codimCount,ChordalNet)
    Headline
        codimension counts of the chains of a chordal network
    Usage
        codimCount N
    Inputs
        N:ChordalNet
    Outputs
        gf:RingElement
          generating function of the codimensions of the chains
    Consequences
    Description
      Text
        This method classifies the number of chains of the network according to its codimension.
        The output is the generating function of such counts.
        For instance, if there are ten chains of codimension 4 and one chain of codimension 6 the output is $t^6+10t^4$.
      Example
        1+1
      Code
      Pre
    Caveat
    SeeAlso
        ChordalNetChain
        nextChain
        (codim,ChordalNet)
        rootCount
///

doc /// --rootCount
    Key
        rootCount
        (rootCount,ChordalNet)
    Headline
        counts the number of roots of a chordal network
    Usage
        rootCount N
    Inputs
        N:ChordalNet
    Outputs
        nroots:ZZ
          a bound on the number of isolated roots
    Consequences
    Description
      Text
        This method counts the number of points (including multiplicities) in the variety of a chordal network, in case such variety is finite.
        If the variety is positive dimensional, then the method returns an upper bound on the number of isolated points.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        (dim,ChordalNet)
        codimCount
///

doc /// --reduceNet
    Key
        reduceNet
        (reduceNet,ChordalNet)
    Headline
        reduces a chordal network
    Usage
        reduceNet N
    Inputs
        N:ChordalNet
    Outputs
        :
          no output; the input chordal network is modified
    Consequences
    Description
      Text
        Simplifies a chordal network by applying a sequence of reduction rules.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --topComponents
    Key
        (topComponents,ChordalNet)
    Headline
        top dimension of a chordal network
    Usage
        topComponents N
    Inputs
        N:ChordalNet
    Outputs
        :
          no output; the input chordal network is modified
    Consequences
    Description
      Text
        Computes the top dimensional part of a chordal network.
      Example
        R = QQ[a,b,c,d,e,f,MonomialOrder=>Lex]
        I = ideal(subsets(gens R, 2) / (S -> product(S)))
        N = chordalNet(I)
        chordalTria N
        codimCount N
        topComponents N
        codimCount N
      Code
      Pre
    --Caveat
    SeeAlso
///

doc /// --components
    Key
        (components,ChordalNet)
        (components,ChordalNet,ZZ)
    Headline
        components of a chordal network
    Usage
        components(N)
        components(N,k)
    Inputs
        N:ChordalNet
    Outputs
        L:
          list of components of the network
    Consequences
    Description
      Text
        Enumerates the components of a chordal network.
        If an optional argument k is given, then only the components in the top k dimensions are computed.
      Example
        1+1
      Code
      Pre
    Caveat
        It is assumed that the chains of the network define prime ideals.
    SeeAlso
        codimCount
        nextChain
///

doc /// --mvar
    Key
        mvar
        (mvar,RingElement)
    Headline
        main variable of a polynomial
    Usage
        mvar f
    Inputs
        f:RingElement
    Outputs
        x:
          main variable of f
    Consequences
    Description
      Text
        Returns the main variable of a polynomial
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        initial
///

doc /// --initial
    Key
        initial
        (initial,RingElement)
    Headline
        initial of a polynomial
    Usage
        initial f
    Inputs
        f:RingElement
    Outputs
        h:
          initial of f
    Consequences
    Description
      Text
        Returns the initial of a polynomial.
        The initial of a polynomial $f$ is the the leading coefficient of $f$ viewed as a univariate polynomial in its main variable.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        mvar
        prem
///

doc /// --saturate
    Key
        (saturate,TriaSystem)
    Headline
        saturated ideal of a triangular set
    Usage
        saturate T
    Inputs
        T:TriaSystem
    Outputs
        I:Ideal
          saturated ideal of T
    Consequences
    Description
      Text
        Returns the saturated ideal of a triangular system.

        Let $T = (F,H)$ be a triangular system.
        Denoting $J=ideal(F)$, the saturated ideal of $T$ is
        $$sat(T) = J : h^\infty = \{ f: h^m f \in J for some m \}$$
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        (prem,RingElement,TriaSystem)
///

doc /// --prem
    Key
        prem
        (prem,RingElement,RingElement,RingElement)
    Headline
        pseudo-remainder
    Usage
        prem(x,f,g)
    Inputs
        x:RingElement
        f:RingElement
        g:RingElement
    Outputs
        r:
          pseudo-remainder of f by g with respect to x
    Consequences
    Description
      Text
        Returns the pseudo-remainder of $f$ by $g$, viewed as univariate polynomials in x.

        Let 
        $f = a_d x^d + \cdots + a_0$
        and
        $g = b_e x^e + \cdots + b_0$.
        If $d<e$ then
        $prem(f,g) = f$.
        Otherwise,
        $prem(f,g) = (b_e)^{d-e+1}f$ mod $g$.
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        mvar
        initial
        (prem,RingElement,TriaSystem)
///

doc /// --prem TriaSystem
    Key
        (prem,RingElement,TriaSystem)
    Headline
        pseudo-remainder by a triangular set
    Usage
        prem(f,T)
    Inputs
        f:RingElement
        T:TriaSystem
    Outputs
        r:
          pseudo-remainder of f by T
    Consequences
    Description
      Text
        Returns the pseudo-remainder of $f$ by a triangular set $T$.
        If $T$ is a triangular set, then $f\in sat T$ if and only if $prem(f,T)=0$.

        Let $T = (t_1,t_2,\cdots,t_k)$ where $mvar(t_1)>\cdots>mvar(t_k)$.
        The pseudo-remainder of $f$ by $T$ is
        $$prem(f,T) = prem(\cdots(prem(prem(f,t_1),t_2)\cdots,t_k)$$
      Example
        1+1
      Code
      Pre
    --Caveat
    SeeAlso
        prem
        (saturate,TriaSystem)
///

doc /// --prem ChordalNet
    Key
        (prem,RingElement,ChordalNet)
    Headline
        ideal membership
    Usage
        prem(f,N)
    Inputs
        f:RingElement
        N:ChordalNet
    Outputs
        r:
          random linear combination of the pseudo-remainder of f by the chains of N
    Consequences
    Description
      Text
        This method gives a randomized algorithm for ideal membership.
        If $f$ lies in the saturated ideal of each of the chains of the network, then the output is always zero.
        Otherwise, it returns a nonzero element with high probability.
      Example
        1+1
      Code
      Pre
    Caveat
        It is assumed that the base field has sufficiently many elements.
    SeeAlso
        prem
        (prem,RingElement,TriaSystem)
        (saturate,TriaSystem)
///

--###################################
-- Symbols
--###################################

doc /// --TriaAlgorithm
    Key
        TriangularDecompAlgorithm
        [triangularize,TriangularDecompAlgorithm]
        [chordalTria,TriangularDecompAlgorithm]
    Headline
        possible values: "Monomial", "Binomial", "Maple", "Epsilon"
    Description
      Text
        The package implements triangular decompositions in two restricted cases:

        (1) "Monomial" =>
        M2 implementation for monomial ideals

        (2) "Binomial" => 
        M2 implementation for binomial ideals

        For arbitrary ideals we interface to Maple:

        (3) "Maple" =>
        uses library RegularChains (included in Maple)

        (4) "Epsilon" =>
        uses Maple's package Epsilon (download from \url{http://www-polsys.lip6.fr/~wang/epsilon/})
    SeeAlso
        triangularize
///

