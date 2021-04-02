--###################################
-- Types
--###################################

refChordalNet := "D. Cifuentes, P.A. Parrilo (2017), \"Chordal networks of polynomial ideals\", in \"SIAM J. Appl. Algebra Geometry\", 1(1):73-–170" 
refChordalElim := "D. Cifuentes, P.A. Parrilo (2016), \"Exploiting chordal structure in polynomial ideals: a Groebner basis approach\", in \"SIAM J. Discrete Math\", 30(3):1534-–1570" 
refPermanents := "D. Cifuentes, P.A. Parrilo (2016), \"An efficient tree decomposition method for permanents and mixed discriminants\", in \"Linear Algebra and its Applications\", 493:45-–81" 

document { --Chordal
    Key => Chordal,
    Headline => "exploiting chordal structure in polynomial ideals",

    PARA {
        "This package provides several specialized routines for structured polynomial ideals. ",
        "The sparsity structure of a polynomial set can be described with a graph. ",
        "By exploiting some suitable \"chordal completion\" of this graph, it is possible to develop more efficient algorithms for several problems in computational algebraic geometry. ",
    },

    "See ",
    TO "installation and configuration",
    " for instructions on how to install this package. ",

    PARA {
        "The examples below illustrate how to use this package to get the following properties of a structured ideal: compute elimination ideals, count the number of zeros, determine the dimension, decompose the variety. ",
    },

    HEADER5 "graphical structure of an ideal",

    "We can abstract the sparsity structure of a polynomial system in a graph. ",
    "By exploiting the chordal completions of this graph more efficient algorithms can be developed. ",
    EXAMPLE lines ///
        R = QQ[a,b,c,d];
        I = ideal {a^2-1, a^2+a*b+1, a^3+c^2, b*d + d, c^3+c*d};
        G = constraintGraph I
        Gc = chordalGraph G
    ///,

    HEADER5 "chordal elimination",

    "Consider the ",
    TO2 {chromaticIdeal,"3-chromatic ideal"},
    " of the cycle graph. ",
    "Its elimination ideals have have a simple generating set. ",
    EXAMPLE lines ///
        I = chromaticIdeal(QQ, cycleGraph 10, 3);
        N = chordalNet I;
        chordalElim N;
        N
    ///,
    "On the other hand, its Groebner basis is complicated. ",
    EXAMPLE lines ///
        sum for f in gbList I list #terms f
    ///,

    HEADER5 "chordal networks",

    "Consider the ",
    TO2 {adjacentMinorsIdeal,"ideal of adjacent minors"},
    " of a 2xn matrix. ",
    "This ideal decomposes into ",
    "F", SUB "n",
    " components, where ",
    "F", SUB "n",
    " is the Fibonacci number. ",
    "These (exponentially many) components can be represented compactly with a chordal network. ",
    EXAMPLE lines ///
        I = adjacentMinorsIdeal(QQ,2,10);
        N = chordalNet I;
        chordalTria N;
        N
    ///,

    "Several properties of an ideal can be computed efficiently given a chordal representation; for instance, its dimension. ",
    EXAMPLE lines ///
        dim N
    ///,
    "We can also extract the top dimensional part, and count the number of components. ",
    EXAMPLE lines ///
        topComponents N
        codimCount N
    ///,
    BR{},
    "For further example see",
    UL{
        TO "chordal networks examples",
    },
     
    HEADER2 "Overview of methods",

    "Graphical structure of a polynomial ideal:",
    UL{
        TO constraintGraph,
        TO chordalGraph,
        TO chordalNet,
    },

    "Elimination routines:",
    UL{
        TO chordalElim,
        TO chordalTria,
	},

    "Methods for triangular chordal networks:",
    UL{
        TO rootCount,
        TO (dim,ChordalNet),
        TO (codim,ChordalNet),
        TO codimCount,
        TO (topComponents,ChordalNet),
        TO (symbol %,RingElement,ChordalNet),
        TO nextChain,
        TO (components,ChordalNet),
	},

    HEADER2 {"References"},
    UL{
        refChordalElim,
        refChordalNet
    }
}

doc /// --ChordalGraph
    Key
        ChordalGraph
        (net,ChordalGraph)
        (isChordal,ChordalGraph)
        (isPerfect,ChordalGraph)
        (chromaticNumber,ChordalGraph)
        (cliqueNumber,ChordalGraph)
        (inducedSubgraph,ChordalGraph)
    Headline
        a chordal graph
    Description
      Text
        This type represents a chordal graph G, together with a perfect elimination ordering.

        An ordering of the vertices is a perfect elimination ordering if for each vertex $v$ the vertices $N(v)$ form a clique, where $N(v)$ consists of the adjacent nodes that occur after $v$ in the ordering.
        A graph is chordal if and only if it has a perfect elimination ordering.
        For notational convenience, ChordalGraph orients the edges of the graph according to such ordering.

        The constructor of this type is @TO chordalGraph@.
        Chordal graphs can be visualized with @TO displayGraph@.

        Several combinatorial problems can be solved efficiently on chordal graphs.
        In particular, @TO chromaticNumber@, @TO cliqueNumber@.

    SeeAlso
///

doc /// --ElimTree
    Key
        ElimTree
        (net,ElimTree)
        (chordalGraph,ElimTree)
        (elimTree,ChordalNet)
        cliques
    Headline
        the elimination tree of a chordal graph
    Description
      Text
        This type represents the elimination tree of a @TO2 {ChordalGraph,"chordal graph"}@.

        The arcs of a chordal graph can be directed according to a perfect elimination ordering.
        The elimination tree of the graph is the transitive closure of such directed acyclic graph.

        The constructor of this type is @TO elimTree@.
        An elimination tree can be visualized with @TO (displayGraph,ElimTree)@.
    SeeAlso
///

document { --ChordalNet
    Key => {ChordalNet, (net,ChordalNet),
        (ring,ChordalNet),structure},
    Headline => "a chordal network",

    "This type describes a chordal network representation of a polynomial ideal. ",
    "The constructor of this type is ",
    TO "chordalNet",

    HEADER3 "Examples",
    UL{
        TO "chordal networks examples",
    },

    HEADER3 "Overview of chordal networks methods",

    "Methods to visualize a chordal network:",
    UL{
        TO displayNet,
        (TO2 {(net,ChordalNet),"net"}, " -- print a chordal network"),
        TO (digraph,ChordalNet),
    },

    "Methods to access properties of a chordal network:",
    UL{
        TO nodes,
        (TO2 {(elimTree,ChordalNet),"elimTree"}, " -- get the underlying elimination tree"),
        (TO2 {(ring,ChordalNet),"ring"}, " -- get the underlying ring"),
        TO isTriangular,
        (TO2 {structure,"structure"}, " -- either \"Monomial\", \"Binomial\" or \"None\" "),
    },

    "Elimination routines using chordal structure:",
    UL{
        TO chordalElim,
        TO chordalTria,
    },

    "Methods for triangular chordal networks:",
    UL{
        TO rootCount,
        TO (dim,ChordalNet),
        TO (codim,ChordalNet),
        TO codimCount,
        TO (topComponents,ChordalNet),
        TO (symbol %,RingElement,ChordalNet),
        TO nextChain,
        TO (components,ChordalNet),
	},
}

doc /// --examples
    Key
        "chordal networks examples"
    Headline
        a new representation of polynomial ideals
    Description
      Text
        A @TO2 {ChordalNet,"chordal network"}@ is a data structure that represents polynomial ideals in terms of the paths of a certain directed graph.
        Remarkably, several polynomial ideals with exponentially many components admit compact chordal network representations.
        Moreover, chordal networks can be efficiently post-processed to compute several properties of the underlying variety, such as cardinality, dimension, components, elimination ideals, and radical ideal membership.

        We now present some examples.

      Text
        {\bf Ideal of adjacent minors: }
        Consider the @TO2 {adjacentMinorsIdeal,"ideal of adjacent minors"}@ of a $2\times n$ matrix .
        This ideal decomposes into $F_n$ components, where $F_n$ is the Fibonacci number.
        These (exponentially many) components can be represented compactly with a chordal network.
      Example
        I = adjacentMinorsIdeal(QQ,2,10);
        N = chordalNet I;
        chordalTria N;
        topComponents N;
        N
      Text
        Once we have the chordal network, one may verify that the variety has codimension 9, and that it has $F_{10}=55$ components.
      Example
        codimCount N

      Text
        {\bf Edge ideals: }
        The @TO2 {edgeIdeal,"edge ideal"}@ of a graph $G=(V,E)$ is generated by the monomials $x_ix_j$ for $ij\in E$.
        Edge ideals have a very nice combinatorial structure, but they often have an exponential number of components.
        Chordal networks might be used to efficiently describe these large decompositions.
        The following code computes a chordal network representation for edge ideal of the product graph $C_3\times P_n$.
      Example
        G = cartesianProduct(cycleGraph 3, pathGraph 5);
        I = edgeIdeal G;
        N = chordalNet(I,"SuggestOrder");
        chordalTria N;
        topComponents N;
        N
      Text
        This variety has codimension 10, and has $48=3\times 2^{5-1}$ components.
      Example
        codimCount N

      Text
        {\bf Chromatic ideal of a cycle: }
        Coloring graphs is a classical NP-hard problem, but it is tractable for certain families of graphs.
        In particular, coloring the cycle graph $C_n$ is trivial.
        However, solving the problem algebraically (see @TO chromaticIdeal@) can be quite challenging using Gr\"obner bases.
        On the other hand, this chromatic ideal has a chordal network representation with less than $3n$ nodes [CP2017].
        This network can be found with the command {\tt chordalTria(N)}, but the calculation requires Maple (see @TO TriangularDecompAlgorithm@).
    SeeAlso
        ChordalNet
        displayNet
///

doc /// --ChordalNetChain
    Key
        ChordalNetChain
        (triaSystem,ChordalNet,ChordalNetChain)
        (dim,ChordalNetChain)
        (codim,ChordalNetChain)
    Headline
        a chain of a chordal network
    Description
      Text
        This type represents a chain of a chordal network.

        Let $N$ be a chordal network in variables $x_1,\dots,x_n$.
        A chain is a tuple $C = (N_1,...,N_n)$ of nodes of the network such that $rank N_i = x_i$, and the nodes are connected in the network.

        The constructor of this type is @TO nextChain@.

        Any chain has an associated triangular system, given by the union of the equations and inequations in each of the nodes.
        Such triangular system can be obtained with {\tt triaSystem(N,C)}.
        The command {\tt dim C} computes the dimension of this system.
    SeeAlso
        nextChain
        codimCount
        TriaSystem
///

document { --ChordalNetNode
    Key => {ChordalNetNode, (net,ChordalNetNode),
        (rank,ChordalNetNode),(gens,ChordalNetNode),(ineqs,ChordalNetNode),
        (label,ChordalNetNode),(parents,ChordalNetNode),(children,ChordalNetNode)},
    Headline => "a node of a chordal network",

    "This type represents a node of a chordal network.",
    BR{},
    BR{},

    "Each node of the network has the following properties:",
    UL{
        (EM "equations:", " a set of polynomials F"),
        (EM "inequations:", " a set of polynomials H"),
        (EM "rank:", " a variable of the polynomial ring"),
        (EM "label:", " a string that uniquely identifies the node (only needed for visualization)"),
    },
    "These properties can be accessed with the functions: ",
    TO2 {(gens,ChordalNetNode),"gens"},
    ", ",
    TO2 {(ineqs,ChordalNetNode),"ineqs"},
    ", ",
    TO2 {(rank,ChordalNetNode),"rank"},
    ", ",
    TO2 {(label,ChordalNetNode),"label"},
    BR{},
    BR{},

    "The incoming/outgoing arcs of a node can be obtained as ",
    UL{
        (TO2 {(parents,ChordalNetNode),"parents"}, " -- get the outgoing arcs"),
        (TO2 {(children,ChordalNetNode),"children"}, " -- get the incoming arcs"),
    },

    SeeAlso => {nodes, ChordalNet},
}

doc /// --ChordalNetRank
    Key
        ChordalNetRank
        (net,ChordalNetRank)
    Headline
        a rank of a chordal network
    Description
      Text
        This type represents the set of nodes of a fixed rank in a chordal network.
    SeeAlso
        ChordalNet
        ChordalNetNode
        (nodes,ChordalNet,RingElement)
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
          chordal graph that contains G as a subgraph
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
          the elimination tree of G
    Consequences
    Description
      Text
        This method computes the elimination tree of some given chordal graph.
      Example
        G = graph(toList(0..9),{
            {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
            {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} });
        Gc = chordalGraph G
        elimTree Gc
      Code
      Pre
    SeeAlso
///

doc /// --leaves elimTree
    Key
        (leaves,ElimTree)
    Headline
        leaves of an elimination tree
    Usage
        leaves tree
    Inputs
        tree:ElimTree
    Outputs
        :List
          of leaves of the tree
    Consequences
    Description
      Example
        G = graph(toList(0..9),{
            {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
            {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} });
        Gc = chordalGraph G
        tree = elimTree Gc
        leaves tree
      Code
      Pre
    SeeAlso
        ElimTree
        leaves
///

doc /// --displayGraph elimTree
    Key
        (displayGraph,String,String,ElimTree)
        (displayGraph,ElimTree)
    Headline
        displays an elimination tree using Graphviz
    Usage
        displayGraph (dotFileName, jpgFileName, tree)
        displayGraph tree 
    Inputs
        dotFileName:String
          (optional)
        jpgFileName:String
          (optional)
        tree:ElimTree
    --Outputs
    Consequences
    Description
      Text
        Displays an elimination tree using Graphviz.
      Code
      Pre
    Caveat
        If the function does not work, it might be the case that Graphviz is not installed, or that the package Graphs is not configured.
        See @TO "installation and configuration"@.
    SeeAlso
        displayGraph
        displayNet
///

doc /// --writeDotFile
    Key
        (writeDotFile,String,Function,ChordalNet)
        (writeDotFile,String,ElimTree)
    Headline
        writes a chordal network to a dot file
    Usage
        writeDotFile(fileName,label,N)
        writeDotFile(fileName,tree)
    Inputs
        fileName:String
        label:Function
        N:ChordalNet
    Consequences
    Description
      Text
        Writes the code for an inputted chordal network (or elimination tree) to be constructed in Graphviz with specified file name
      Code
      Pre
    SeeAlso
        writeDotFile
        displayNet
        (displayGraph,ElimTree)
///

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
        :ZZ
          treewidth of G
    Consequences
    Description
      Text
        This method computes the treewidth of a chordal graph.
      Example
        G = graph(toList(0..9),{
            {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
            {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} });
        Gc = chordalGraph G
        treewidth Gc
      Code
      Pre
    SeeAlso
///;

doc /// --constraintGraph
    Key
        constraintGraph
        (constraintGraph,Ideal)
        (constraintGraph,Ring,List)
    Headline
        constraint graph of a polynomial set
    Usage
        constraintGraph I
    Inputs
        I:Ideal
    Outputs
        :Graph
          constraint graph of the generators of I
    Consequences
    Description
      Text
        This method constructs the constraint graph associated to a polynomial set $F = \{f_1,\dots,f_m\}$.
        The vertices of this graph are the variables, and there is an edge connecting two variables iff there is a polynomial that uses both of them.
      Example
        R = QQ[x_0..x_3];
        I = ideal {x_0^2*x_1*x_2 +2*x_1 +1, x_1^2 +x_2, x_1 +x_2, x_2*x_3};
        constraintGraph I
      Code
      Pre
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
    Consequences
    Description
      Text
        This method suggests a good variable ordering.
        The ordering is chosen by finding a good chordal completion of its @TO2 {constraintGraph,"constraint graph"}@.
      Example
        G = cartesianProduct(cycleGraph 3, pathGraph 3);
        I = edgeIdeal G
        X = suggestVariableOrder I
      Code
      Pre
    SeeAlso
        constraintGraph
        chordalGraph
///

doc /// --chordalNet
    Key
        chordalNet
        (chordalNet,Ideal)
        (chordalNet,Ideal,List)
        (chordalNet,Ideal,String)
    Headline
        constructs a chordal network from a polynomial set
    Usage
        chordalNet I
        chordalNet (I, X)
        chordalNet (I, "SuggestOrder")
    Inputs
        I:Ideal
        X:List
          of variables (optional)
        str:String
          (optional)
    Outputs
        :ChordalNet
          chordal network constructed from {\tt I}
    Consequences
    Description
      Text
        This method constructs a chordal network from a given polynomial set F.
        This chordal network is a directed graph, whose nodes define a partition of F.
        The arcs of the directed graph are given by the @TO2 {elimTree,"elimination tree"}@ of the associated @TO2 {constraintGraph,"constraint graph"}@.

      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I
      Text
        Selecting a good variable ordering is very important for the complexity of chordal networks methods.
        The variable ordering can be specified in the input.
        Alternatively, the string "SuggestOrder" can be used.
      Example
        G = cartesianProduct(cycleGraph 3, pathGraph 3);
        I = edgeIdeal G
        N = chordalNet(I,"SuggestOrder")
      Code
      Pre
    SeeAlso
        ChordalNet
        suggestVariableOrder
///

document { --displayNet
    Key => {displayNet, (displayNet,ChordalNet), (displayNet,Function,ChordalNet), (displayNet,String,String,Function,ChordalNet) },
    Headline => "displays a chordal network using Graphivz", 
    Inputs => {
        "N"=> {ofClass ChordalNet},
        "label"=> {ofClass Function, " (optional)"},
        "dotFileName"=> {ofClass String, " (optional)"},
        "jpgFileName"=> {ofClass String, " (optional)"},
    },
    Usage => "displayNet N\ndisplayNet (label, N)\ndisplayNet (dotFileName, jpgFileName, label, N)",

    "Displays a chordal network using Graphviz.",
    BR{},
    BR{},

    "The text displayed in each of the nodes can be provided by the optional input ",
    TT "label. ",
    "The default is ",
    TT "label:= net. ",
    BR{},
    BR{},

    HEADER3 "Example",
    "Consider the following chordal network. ",
    EXAMPLE lines ///
        R = QQ[a..e,MonomialOrder=>Lex];
        I = ideal {a*b, b*c, c*d, a*e, d*e};
        N = chordalNet I;
        chordalTria N;
        topComponents N;
        N
    ///,
    "The command ",
    TT "displayNet N  ",
    "outputs the following figure. ",
    BR{},

    PARA IMG {"src" => replace ("PKG", "Chordal", currentLayout#"package") | "network.jpg", "alt" => "chordal network"},
    Caveat => {
        "If the function does not work, it might be the case that Graphviz is not installed, or that the package Graphs is not configured. See ",
        TO "installation and configuration"
    },
    SeeAlso => {(digraph,ChordalNet), displayGraph, (displayGraph,ElimTree)}
}

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
        :Digraph
    Consequences
    Description
      Text
        Returns the digraph associated to a chordal network.
        The vertices of this digraph are given by the @TO2 {(label,ChordalNetNode),"labels"}@ of the nodes.
      Example
        R = QQ[a,b,c,d,MonomialOrder=>Lex];
        I = ideal {a*b, a*c, b*c, c*d};
        N = chordalNet I;
        chordalTria N;
        reduceNet N;
        N
        nodes N / (Ni -> label Ni)
        digraph N
      Code
      Pre
    SeeAlso
        displayNet
        (chordalNet,HashTable,HashTable,ElimTree,Digraph)
        digraph
///

doc /// --chordalNet digraph
    Key
        (chordalNet,HashTable,HashTable,ElimTree,Digraph)
    Headline
        construct chordal network from a digraph
    Usage
        chordalNet(eqs,ranks,tree,DG)
    Inputs
        eqs:HashTable
          indicating the equations/inequations of each node
        ranks:HashTable
          indicating the rank of each node
        tree:ElimTree
        DG:Digraph
          whose vertices are labels (strings) of the nodes
    Outputs
        :ChordalNet
    Consequences
    Description
      Text
        This method constructs a chordal network from the digraph of its nodes.
      Example
        R = QQ[a,b,c,d,MonomialOrder=>Lex];
        DG = digraph {{"a0","b0"}, {"a0","b1"}, {"a1","b2"}, {"b0","c1"}, {"b1","c0"}, {"b2","c0"}, {"c0","d0"}, {"c1","d1"}}
        G = chordalGraph digraph hashTable{a=>{b,c},b=>{c},c=>{d},d=>{}};
        tree = elimTree G
        rnk = hashTable{"a0"=>a, "a1"=>a, "b0"=>b, "b1"=>b, "b2"=>b, 
                        "c0"=>c, "d0"=>d, "c1"=>c, "d1"=>d};
        eqs = hashTable{"a0" => ({a},{}), "a1" => ({},{}),
                        "b0" => ({b},{}), "b1" => ({},{}), "b2" => ({b},{}), 
                        "c0" => ({c},{}), "c1" => ({},{}), 
                        "d0" => ({},{}), "d1" => ({d},{}) };
        chordalNet(eqs,rnk,tree,DG)
      Code
      Pre
    SeeAlso
        displayNet
        (digraph,ChordalNet)
///

doc /// --size
    Key
        (size,ChordalNet)
    Headline
        size of a chordal network
    Usage
        size N
    Inputs
        N:ChordalNet
    Outputs
        :List
          with the number of nodes in each rank
    Consequences
    Description
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I;
        chordalTria N;
        size N
        N
      Code
      Pre
    SeeAlso
        ChordalNet
        nodes
///

doc /// --nodes
    Key
        nodes
        (nodes,ChordalNet)
        (nodes,ChordalNet,RingElement)
    Headline
        list of nodes of a chordal network
    Usage
        nodes N
        nodes(N,i)
    Inputs
        N:ChordalNet
        i:RingElement
          a variable (optional)
    Outputs
        :List
          consisting of @TO2 {ChordalNetNode,"chordal network nodes"}@
    Consequences
    Description
      Text
        This method returns the list of nodes of the chordal network.
        If the optional argument {\tt i} is given, then only the nodes of rank {\tt i} are returned.
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I;
        chordalTria N;
        N
        nodes N
        nodes(N,x_0)
      Code
      Pre
    SeeAlso
        ChordalNet
        (size,ChordalNet)
///

doc /// --isTriangular
    Key
        isTriangular
        (isTriangular,ChordalNet)
    Headline
        whether a chordal network is triangular
    Usage
        isTriangular N
    Inputs
        N:ChordalNet
    Outputs
        :Boolean
          true if N is triangular, and false otherwise
    Consequences
    Description
      Example
        R = QQ[a..e,MonomialOrder=>Lex];
        I = ideal {a*b, b*c, c*d, a*e, d*e};
        N = chordalNet I;
        isTriangular N
        chordalTria N;
        isTriangular N
      Code
      Pre
    SeeAlso
        ChordalNet
        nodes
///

doc /// --RingMap
    Key
        (symbol SPACE,RingMap,ChordalNet)
        (symbol SPACE,RingMap,ElimTree)
    Headline
        apply ring map to a chordal network
    Usage
        f N
        f tree
    Inputs
        f:RingMap
        N:ChordalNet
    Outputs
        :ChordalNet
          image of N under the ring map f
    Consequences
    Description
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I
        S = QQ[y_0..y_3, MonomialOrder=>Lex];
        f = map(S,R,gens S) 
        f N
      Code
      Pre
    SeeAlso
        ChordalNet
        (symbol SPACE,RingMap,RingElement)
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
    Consequences
      Item 
        the input chordal network is modified
    Description
      Text
        This method performs successive elimination on a given chordal network.
        Under suitable conditions this procedure computes the elimination ideals.

        Let $I\subseteq k[x_0,\dots,x_{n-1}]$ be the input ideal.
        The "approximate" $j$-th elimination ideal $I_j$ consists of the polynomials in the output network with main variable at most $x_j$.
        The containment $I_j \subseteq I\cap k[x_{j},\dots,x_{n-1}]$ always holds.
        If {\tt guaranteed=true}, then $I_j$ provably agrees with $I\cap k[x_j,\dots,x_{n-1}]$ (up to radical).

        {\bf Example 3.1 of [CP'16]}
      Text
        (chordalElim succeeds in computing the elimination ideals)
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^4-1, x_0^2+x_2, x_1^2+x_2, x_2^2+x_3};
        N = chordalNet I;
        chordalElim N
        N
        gbList I
      Text
        {\bf Example 3.2 of [CP'16]}
      Text
        (chordalElim does not compute the elimination ideals)
      Example
        R = QQ[x_0..x_2, MonomialOrder=>Lex];
        I = ideal {x_0*x_1+1, x_1+x_2, x_1*x_2};
        N = chordalNet I;
        chordalElim N
        N
        gbList I
      Text
        {\bf Example: } 3-chromatic ideal of the cycle graph
      Text
        (chordalElim succeeds)
      Example
        I = chromaticIdeal(QQ, cycleGraph 10, 3);
        N = chordalNet I;
        chordalElim N
        N
      Code
      Pre
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
          no output
    Consequences
      Item 
        the input chordal network is modified
    Description
      Text
        This method puts a chordal network into triangular form.
        A triangular chordal network can be effectively used to compute several properties of the underlying variety.

        Example 3.1 of [CP'17]
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
        N = chordalNet I;
        chordalTria N;
        N
      Text
        Example 1.3 of [CP'17]:
        ideal of adjacent minors of a 2xn matrix
      Example
        I = adjacentMinorsIdeal(QQ,2,10);
        N = chordalNet I;
        chordalTria N;
        N
      Code
      Pre
    Caveat
        This function calls @TO triangularize@, which is only implemented in Macaulay2 for binomial ideals.
        For arbitrary ideals we need to interface to Maple.
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
        This method produces the @TO2 {ChordalNetChain,"chains"}@ of a chordal network one at a time.
        It can also iterate only over chains of a specified codimension.
        
        Returns "null" if none.
      Example
        I = toLex edgeIdeal cycleGraph 9;
        N = chordalNet I;
        chordalTria N;
        codimCount N
        nC = 0;
        C = nextChain N;
        while C=!=null do (C=nextChain(C,N); nC=nC+1;)
        nC
      Text
        We can specify the codimension of the chains.
      Example
        nC = 0;
        (C,data) = nextChain(5,N);
        while C=!=null do (C=nextChain(C,data,5,N); nC=nC+1;)
        nC
      Code
      Pre
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
        :ZZ
          dimension of the associated variety
    Consequences
    Description
      Text
        This method computes the dimension of the variety associated to a chordal network.
      Example
        I = adjacentMinorsIdeal(QQ,2,10);
        N = chordalNet I;
        chordalTria N;
        dim N
      Code
      Pre
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
        :RingElement
          generating function of the codimensions of the chains
    Consequences
    Description
      Text
        This method classifies the number of @TO2 {ChordalNetChain,"chains"}@ of the network according to its codimension.
        The output is the generating function of such counts.
        For instance, if there are ten chains of codimension 4 and one chain of codimension 6 the output is $t^6+10t^4$.
      Example
        I = adjacentMinorsIdeal(QQ,2,5);
        N = chordalNet I;
        chordalTria N;
        codimCount N
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
        :ZZ
          an upper bound on the number of isolated roots
    Consequences
    Description
      Text
        This method counts the number of points (including multiplicities) in the variety of a chordal network, in case such variety is finite.
        If the variety is positive dimensional, then the method returns an upper bound on the number of isolated points.
      Example
        I = chromaticIdeal(QQ, cycleGraph 10, 2);
        N = chordalNet I;
        chordalTria N;
        rootCount N
      Code
      Pre
    SeeAlso
        (dim,ChordalNet)
        codimCount
///

-- TODO: the example might stop working if chordalTria changes
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
          no output
    Consequences
      Item 
        the input chordal network is modified
    Description
      Text
        Simplifies a chordal network by applying a sequence of reduction rules (while preserving the underlying variety).
      Example
        I = adjacentMinorsIdeal(QQ,2,4);
        N = chordalNet I;
        chordalTria N;
        codimCount N
        reduceNet N
        codimCount N
      Code
      Pre
    SeeAlso
        reduceDimension
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
          no output
    Consequences
      Item 
        the input chordal network is modified
    Description
      Text
        Computes the top dimensional part of a chordal network.
      Example
        I = toLex edgeIdeal cycleGraph 9
        N = chordalNet I;
        chordalTria N;
        codimCount N
        topComponents N
        codimCount N
      Code
      Pre
    SeeAlso
        (dim,ChordalNet)
        codimCount
///

doc /// --reduceDimension
    Key
        reduceDimension
    Headline
        removes arcs of a chordal network of small dimension
    Usage
        reduceDimension N
    Inputs
        N:ChordalNet
        k:ZZ
    Outputs
        :
          no output
    Consequences
      Item 
        the input chordal network is modified
    Description
      Text
        Reduces a chordal network, by prunning the arcs that do not belong to the top $k$ dimensional parts of the variety.
      Example
        R = QQ[a..j,MonomialOrder=>Lex];
        I = ideal {a*d - b*c, c*f - d*e, e*h - f*g, g*j - h*i, a*j - b*i};
        N = chordalNet I;
        chordalTria N;
        codimCount N
        reduceDimension(N,2);
        codimCount N
        reduceDimension(N,1);
        codimCount N
      Text
        The method @TO (topComponents,ChordalNet)@ is equivalent to {\tt reduceDimension(N,1)}.
      Code
      Pre
    Caveat
        An arc is removed only if all the @TO2 {ChordalNetChain,"chains"}@ through it have small dimension.
        If an arc belongs to a chain of large dimenson and a chain of small dimension, then the arc will not be removed.
    SeeAlso
        codimCount
        topComponents
///

doc /// --components
    Key
        (components,ChordalNet,ZZ)
        (components,ChordalNet)
    Headline
        components of a chordal network
    Usage
        components(N)
        components(N,k)
    Inputs
        N:ChordalNet
        k:ZZ
          (optional)
    Outputs
        :List
          of components of the network
    Consequences
    Description
      Text
        Enumerates the (maximal) components of a chordal network.
        If the optional argument $k$ is given, then only the components in the top $k$ dimensions are computed.
      Example
        I = toLex edgeIdeal cycleGraph 8
        N = chordalNet I;
        chordalTria N;
        codimCount N
        components(N,1)
        components(N)
      Code
      Pre
    Caveat
        It is assumed that the @TO2 {ChordalNetChain,"chains"}@ of the network define prime ideals.
    SeeAlso
        codimCount
        nextChain
        (isPrimeSimple,ChordalNet)
///

doc /// --prem ChordalNet
    Key
        (symbol %,RingElement,ChordalNet)
        (pseudoRemainder,RingElement,ChordalNet)
    Headline
        ideal membership test
    Usage
        f % N
        pseudoRemainder(f,N)
    Inputs
        f:RingElement
        N:ChordalNet
    Outputs
        :RingElement
          random linear combination of the pseudo-remainder of f by the chains of N
    Consequences
    Description
      Text
        This method gives a randomized algorithm for ideal membership.
        If $f$ lies in the @TO2 {(saturate,TriaSystem),"saturated ideal"}@ of each of the @TO2 {ChordalNetChain,"chains"}@ of the network, then the output is always zero.
        Otherwise, it returns a nonzero element with high probability.

        As an example, consider the ideal of cyclically adjacent minors.
      Example
        I = adjacentMinorsIdeal(QQ,2,6);
        X = gens ring I;
        J = I + (X_0 * X_(-1) - X_1*X_(-2));
        f = sum gbList J;
        N = chordalNet J;
        chordalTria N;
        f % N == 0
      Code
      Pre
    Caveat
        It is assumed that the base field has sufficiently many elements.
        For small finite fields one must work over a suitable field extension.
    SeeAlso
        (pseudoRemainder,RingElement,RingElement,RingElement)
        (pseudoRemainder,RingElement,TriaSystem)
        (saturate,TriaSystem)
///

doc /// --isPrimeSimple
    Key
        (isPrimeSimple,ChordalNet)
    Headline
        simple primality test of a chordal network
    Usage
        isPrimeSimple N
    Inputs
        N:ChordalNet
    Outputs
        :Boolean
          if the test is positive then each chain of N is prime; otherwise, they may or may not be prime
    Description
      Text
        Performs a @TO2 {isPrimeSimple,"simple test"}@ to determine whether each of the @TO2 {ChordalNetChain,"chains"}@ of the network defines a prime ideal.
      Example
        I = adjacentMinorsIdeal(QQ,2,5)
        N = chordalNet I;
        chordalTria N;
        topComponents N;
        N
        isPrimeSimple N
        C = nextChain N
        isPrimeSimple triaSystem(N,C)
      Code
      Pre
    SeeAlso
        isPrimeSimple
///

doc /// --adjacentMinorsIdeal
    Key
        adjacentMinorsIdeal
    Headline
        ideal of adjacent minors
    Usage
        adjacentMinorsIdeal(kk,m,n)
    Inputs
        kk:Ring
        m:ZZ
        n:ZZ
    Outputs
        :Ideal
    Consequences
    Description
      Text
        Returns the ideal of maximal adjacent minors of a $m\times n$ matrix.
      Example
        I = adjacentMinorsIdeal(QQ,2,6)
      Code
      Pre
    SeeAlso
///

doc /// --chromaticIdeal
    Key
        chromaticIdeal
    Headline
        chromatic ideal of a graph
    Usage
        chromaticIdeal(kk,G,q)
    Inputs
        kk:Ring
        G:Graph
        q:ZZ
    Outputs
        :Ideal
    Consequences
    Description
      Text
        Returns the ideal of proper $q$-colorings of a graph $G=(V,E)$, given by the equations
        $x_i^q -1, i \in V$
        and 
        $(x_i^q-x_j^q)/(x_i-x_j), ij\in E$.
      Example
        G = cycleGraph 4;
        I = chromaticIdeal(QQ, G, 3)
      Code
      Pre
    SeeAlso
///

doc /// --subsetsProductsIdeal
    Key
        subsetsProductsIdeal
    Headline
        ideal of subset products
    Usage
        subsetsProductsIdeal(kk,n,k)
    Inputs
        kk:Ring
        n:ZZ
        k:ZZ
    Outputs
        :Ideal
    Consequences
    Description
      Text
        Returns the monomial ideal generated by the products of $k$ distinct variables.
      Example
        I = subsetsProductsIdeal(QQ, 5, 3)
      Code
      Pre
    SeeAlso
///

doc /// --toLex
    Key
        toLex
    Headline
        change monomial order to lexicographic
    Usage
        toLex I
    Inputs
        I:Ideal
    Outputs
        :Ideal
    Consequences
    Description
      Example
        R = QQ[x,y,z,MonomialOrder=>GRevLex];
        I = ideal (x,y);
        J = toLex I
        describe ring J
      Code
      Pre
    SeeAlso
///

doc /// --setDefaultConfiguration
    Key
        setDefaultConfiguration
        (setDefaultConfiguration,Package,String,String)
        (setDefaultConfiguration,String,String,String)
    Headline
        default configuration of a package
    Usage
        setDefaultConfiguration(packge,key,val)
    Inputs
        packge:Package
        key:String
        val:String
    Consequences
    Description
      Text
        This function modifies the init file of a package by setting the default value of {\tt key}.

        The package @TO Chordal@ does not have any configuration options,
        but its dependencies @TO Graphs@ and @TO MapleInterface@ do.
        For instance, the command 

        @TT "setDefaultConfiguration(\"Graphs\",\"DotBinary\",\"circo\")"@

        sets the Graphviz binary used for visualizing graphs and chordal networks.
      Code
      Pre
    Caveat
        This method assumes that the package was installed in the @TO applicationDirectory@.
    SeeAlso
        "installation and configuration"
///

document { --configuration
    Key => "installation and configuration",
    Headline => "of the Chordal package",

    HEADER2 "Installation",

    "To install ",
    TO Chordal,
    " run the following commands in a Macaulay2 terminal:",
    UL {
        TT "installPackage \"Graphs\" ", 
        TT "installPackage \"MapleInterface\" ",
        TT "installPackage \"TriangularSets\" ",
        TT "installPackage \"Chordal\" ",
    },
    "To verify that the installation was correct, restart and execute the command ",
    TT "check \"Chordal\"",
    ".",

    HEADER2 "External Software",

    "The basic functionality of ",
    TO Chordal,
    " should work without any additional programs. ",
    "However, some of the functions do require external software. ",
    BR{},
    BR{},

    EM "Graphviz (optional): ",
    "The visualization of chordal networks depends on Graphviz, which can be (freely) download from ",
    HREF "http://www.graphviz.org/Download..php",
    BR{},
    BR{},

    EM "Maple (optional): ",
    "The command ",
    TO chordalTria,
    " requires Maple when the ideal is not binomial. ",

    HEADER2 "Configuration",

    "Many of the functions of ",
    TO Chordal,
    " will work without any configuration. ",
    "But some functions depend on the configuration of the dependencies ",
    TO Graphs,
    " and ",
    TO MapleInterface,
    ". ",
    BR{},
    BR{},

    EM "Graphs: ",
    "Visualizing chordal networks requires configuring the Graphviz executable. ",
    "The default Graphviz executable \"dot\" should work, but it if not it can be changed with:",
    BR{},
    TT "setDefaultConfiguration(\"Graphs\",\"DotBinary\",\"myexecutable\")",
    BR{},
    BR{},

    EM "MapleInterface: ",

    "The default executable \"maple\" should work, but if not it can be changed with: ",
    BR{},
    TT "setDefaultConfiguration(\"MapleInterface\",\"MapleCommand\",\"mymaplecommand\")",

    SeeAlso => {setDefaultConfiguration,Graphs,MapleInterface},
}

--###################################
-- Symbols
--###################################

doc /// --TriaAlgorithm
    Key
        GetTable
        [codimCount,GetTable]
    Headline
        get dynamic programming table
    Description
      Text
        An option that can be used to obtain the full dynamic programming table.
    SeeAlso
///
