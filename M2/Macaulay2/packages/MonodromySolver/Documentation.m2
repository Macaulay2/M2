-- undocumented methods and symbols (for each, consider... does it really need to be exported? should it be documented?)
undocumented {
    Vertices, 
    (pointArray,List), 
    saturateEdges,  (saturateEdges,HomotopyGraph), (makeRandomizedSelect,RR), (makeBatchPotential,ZZ), (dynamicFlowerSolve,Matrix,Point,List), RandomPointFunction, 
     Correspondence21, Edges, Correspondence12, Potential21, Potential12, Family, gamma1, gamma2, Graph, Node1,  Node2, HomotopyEdge, makeRandomizedSelect,
     isAffineLinearFunction, (symbol <<, File, PointArray), (position, Point, PointArray, FunctionClosure), (toExternalString, PointArray), PartialSolBins, LinearSegment, FirstDirectedEdge
     }
    --, SpecializedSystem,  HomotopyNode, HomotopyGraph,PartialSols}
-- undocument tags
undocumented {(symbol _,PointArray,List), (symbol _,PointArray,ZZ), (points,PointArray), (position,Point,PointArray), 1:(homotopyGraph), [dynamicFlowerSolve,RandomPointFunction], [homotopyGraph,Potential], [dynamicFlowerSolve,TargetSolutionCount],FilterFailure}

doc ///
    Key
        MonodromySolver 
    Headline
        solve polynomial systems via homotopy continuation and monodromy
    Description
        Text
	    This packages provides randomized methods for numerically solving polynomial systems of equations that occur in parametric families, by exploiting the transitive action of an associated monodromy group.
            The package implements the graph-based framework described in the third reference below.
	    There are three main functions that may be used to solve a system of a family of systems:
        Code
                UL {
		    TO solveFamily,
                    TO sparseMonodromySolve,
                    TO monodromySolve
		    } 
        Text
            @TO monodromySolve@ is the core function, whose input may be @ofClass PolySystem@ or $ofClass GateSystem$.
	    As an additional input, a seed pair consisting of initial parameter and solution values may be provided.
	    @TO solveFamily@ is a wrapper function that returns specific solutions and parameter values.
	    @TO sparseMonodromySolve@ is a blackbox solver for systems without parameters, that calls the core functio
	    These functions have many options in common, which are summarized in @TO MonodromySolverOptions@.
	    Here is an example illustrating how to solve a parametric family for specific parameter values.
        Example
            setRandomSeed 0;
            declareVariable \ {A,B,C,D,X,Y};
            F = gateSystem(matrix{{A,B,C,D}},matrix{{X,Y}},matrix{{A*(X-1)^2-B}, {C*(Y+2)^2+D}});
            p0 = point{{1,1,1,1}};
            sols = solveFamily(p0, F, NumberOfNodes=>3);
            for i from 0 to 3 list norm(evaluate(F, p0, sols#i))
        Text
            Each solver works by assembling randomly generated systems within a @TO HomotopyGraph@ and tracking paths between
            them. They are also equipped with a number of options, which may be useful for speeding up computation or 
            increasing the probability of success.
        Text
            In the example above, the system is linear in parameters, allowing for the seed pair to be computed automatically. 
	    The current seeding implementation will report failure in other cases.
	    Depending on the problem of interest, there may still be a natural way to generate the seed pair, as in @TO (monodromySolve, System, Point, List) @.
	Text
            Some references for numerical monodromy methods:
    	    
	    Sommese, Andrew J., Jan Verschelde, and Charles W. Wampler. "Numerical decomposition of the solution sets of polynomial systems into irreducible components." {\it SIAM Journal on Numerical Analysis} 38.6 (2001): 2022-2046.
	    
	    del Campo, Abraham Martin, and Jose Israel Rodriguez. "Critical points via monodromy and local methods." {\it Journal of Symbolic Computation} 79 (2017): 559-574.
	    
	    Duff, Timothy, Cvetelina Hill, Anders Jensen, Kisun Lee, Anton Leykin, and Jeff Sommars. "Solving polynomial systems via homotopy continuation and monodromy." {\it IMA Journal of Numerical Analysis} 39.3 (2019): 1421-1446.
    ///

doc ///
    Key
        sparseMonodromySolve
        (sparseMonodromySolve, PolySystem)
    Headline
        an "out of the box" polynomial system solver
    Usage
        sols = sparseMonodromySolve PS
    Inputs 
        PS:System
            whose coefficients are complex numbers
    Outputs
        sols:List
            containing solutions to sys, each represented as a @TO Point @.
    Description
        Text
            Blackbox monodromy solver for a square polynomial system without parameters.
            The example below finds all six intersection of a generic cubic F with its quadratic polar curve P.
        Text
    
        Example
            setRandomSeed 2020;
            R=CC[x,y,z];
            F=random(3,R);
            P=sum apply(gens R,g->diff(g,F)*random CC);
            PS = polySystem {F,P,random(1,R)-1};
            sols = sparseMonodromySolve PS;
            for i from 0 to 5 list norm evaluate(PS, sols#i)
        Text
            For systems with dense support such as the above, the total number of paths tracked is generally not optimal, though timings may 
            be comparable.
        ///

doc ///
    Key
        solveFamily
        (solveFamily,System)
        (solveFamily,Point,System)
    Headline
        a solver for parametric families with simple output
    Usage
        (p, sols) = solveFamily PS
        (p, sols) = solveFamily(P, p)
    Inputs 
        PS:System
           : a parametric polynomial system, represented as either a @TO PolySystem@ whose underlying coefficient ring itself a polynomial ring in the parameters, or a @TO GateSystem@ with parameters.
        p:Point
           consisting of target parameter values (optional.)
    Outputs
        p:Point
            parameter values. If not part of the input, they are chosen uniformly as complex numbers w/ modulus 1.
        sols:PointArray
            containing solutions to PS specialized at p.
    Description
        Text
            The output of @TO monodromySolve @ is opaque. This method is intended for users uninterested in the underlying
            @TO HomotopyGraph @ and its satellite data. When the number of solutions is small, it can help to increase options like @TO NumberOfNodes@ or @TO NumberOfEdges@ above their default value, as shown here:
        Example
            setRandomSeed 0
            R = CC[a,b,c,d,e,f][x,y];
            q  = a*x^2+b*y+c;
            l = d*x+e*y+f;
            (sys, sols) = solveFamily(polySystem{q,l}, NumberOfNodes=>3)
    ///

doc ///
    Key
        monodromySolve
    Headline
        the main function of the MonodromySolver package
    Description
        Code
	    HEADER3 "Ways to use:",
	    UL {
		TO (monodromySolve, System, Point, List),             
                TO (monodromySolve, System)
                }
       ///       

doc ///
    Key
        (monodromySolve, System, Point, List)
    Usage
            (N, npaths) = monodromySolve(PS,p0,L)
    Inputs
        PS:System
	    with parametric coefficients
        p0:Point
            representing a parametrized system
        L:List
            containing solutions associated to p0, each represented as a @TO Point @.
    Outputs
        N:HomotopyNode
        npaths:ZZ
            reporting the number of paths tracked.
    Description
        Text
	    Most solvers rely on the manual seeding function @TO createSeedPair @. The example below demonstrates how one might
	    seed manually when some equations don't have parameters---ie. the projection map onto the variables is non-dominant.
        Example
                setRandomSeed 0;
            S = CC[a,b,c];
            R = S[x,w];
            (h, f) = (a*x+b*w+c, 3*x^2 - w + 1);
            x0 = point {{ii_CC,-2}}; -- clearly a zero of f
            l = apply(2,i->random CC);
            p0 = point({append(l,- sum apply(l, x0.Coordinates,(i,x)->i*x))});
            (N, npaths) = monodromySolve(polySystem {h,f},p0,{x0},NumberOfNodes=>3);        
        ///        

doc ///
    Key
        (monodromySolve, System)
    Description
        Text
	    This blackbox solver is similar in usage to @TO sparseMonodromySolve @, but with "technical" output.
        Example
            R = CC[a,b,c,d][A,B]
            polys = polySystem {A^2*a+B^2*b,A*B*c+d}  
                setRandomSeed 0;
                        (V,npaths) = monodromySolve(polys, NumberOfNodes => 3);
            peek V
    /// 
    
doc ///
    Key
        specializeSystem
	(specializeSystem, Point, PolySystem)
	(specializeSystem, Point, Matrix)
    Usage
        equations = specializeSystem(p0, PS)
    Inputs
        p0:Point
	    in parameter space
        PS:System
	    with parameters as coefficients
    Outputs
        equations:List
            containing equtaions of system with parameters specialized at p0
    Headline
            specialize parametric system at a point in the parameter space.///
    
doc ///
    Key
        createSeedPair
        (createSeedPair, System)
        (createSeedPair, System, Point)
    Headline
        create initial seed for the homotopy continuation
    Usage
        (p0,x0) = createSeedPair PS
	(p0, x0) = createSeedPair(PS,x0)
    Inputs
        PS:System
	    a parametric polynomial system
	x0:Point
	    an initial solution for some system
    Outputs
        p0:Point
            representing a random system
        x0:Point
            representing a solution to the system defined by p0
    Description
        Text
	    The "seed pair" appearing in the output of this function is used to initialize the @TO HomotopyGraph @ in blackbox
            solvers. To diagnose failures for the parametric solvers, it may be helpful to check that seeding worked
            as in the example provided below.
        Example
	    setRandomSeed 0
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            (p0,x0) := createSeedPair polys;
            polys0=specializeSystem(p0,polys);
            -- line below checks that the system defined by p0 nearly vanishes at x0
            apply(polys0,p->sub(p,{x=>first x0.Coordinates,y=>last x0.Coordinates}))
   SeeAlso
       (monodromySolve,System,Point,List)
    ///

doc ///
    Key
        selectRandomEdgeAndDirection
    Headline
        random selection of edge and direction for homotopy 
    Description
        Text
	    This is the random edge-selection strategy for solvers. An edge is chosen uniformly at random from all edges in the
            underlying @TO HomotopyGraph @, then its direction is chosen randomly. This is currently the default behavior for
            all solvers.
    SeeAlso
        "selectBestEdgeAndDirection"
        "HomotopyEdge"
        "MonodromySolverOptions"
    /// 

doc ///
    Key
        selectBestEdgeAndDirection
    Headline
        selects edge and direction based on highest potential for obtaining new information
    Description
        Text
	    By default, the solver option SelectEdgeAndDirection is set to selectRandomEdgeAndDirection; it may be changed to 
            selectBestEdgeAndDirection as shown below. This will return an error unless the option @TO Potential @ is also set.
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            (V,npaths) = monodromySolve(polys,SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
    SeeAlso
        "selectRandomEdgeAndDirection"
        "potentialLowerBound"
        "potentialE"
        "HomotopyEdge"
        "MonodromySolverOptions"
    ///

doc ///
    Key
        MonodromySolverOptions
        AugmentEdgeCount
        [monodromySolve,AugmentEdgeCount]
        [solveFamily,AugmentEdgeCount]
        [sparseMonodromySolve,AugmentEdgeCount]
        AugmentGraphFunction
        [monodromySolve,AugmentGraphFunction]
        [solveFamily,AugmentGraphFunction]
        [sparseMonodromySolve,AugmentGraphFunction]
        AugmentNumberOfRepeats
        [monodromySolve,AugmentNumberOfRepeats]
        [solveFamily,AugmentNumberOfRepeats]
        [sparseMonodromySolve,AugmentNumberOfRepeats]
        AugmentNodeCount
        [monodromySolve,AugmentNodeCount]
        [solveFamily,AugmentNodeCount]
        [sparseMonodromySolve,AugmentNodeCount]
        BatchSize
        [monodromySolve,BatchSize]
        [solveFamily,BatchSize]
        [sparseMonodromySolve,BatchSize]
        EdgesSaturated
        [monodromySolve,EdgesSaturated]
        [solveFamily,EdgesSaturated]
        [sparseMonodromySolve,EdgesSaturated]
        GraphInitFunction
        [monodromySolve,GraphInitFunction]
        [solveFamily,GraphInitFunction]
        [sparseMonodromySolve,GraphInitFunction]
	NumberOfEdges
        [monodromySolve,NumberOfEdges]
        [solveFamily,NumberOfEdges]
        [sparseMonodromySolve,NumberOfEdges]
        NumberOfNodes
        [monodromySolve,NumberOfNodes]
        [solveFamily,NumberOfNodes]
        [sparseMonodromySolve,NumberOfNodes]
        NumberOfRepeats
        [monodromySolve,NumberOfRepeats]
        [solveFamily,NumberOfRepeats]
        [sparseMonodromySolve,NumberOfRepeats]
	PointArrayTol
        Potential
        [monodromySolve,Potential]
        [solveFamily,Potential]
        [sparseMonodromySolve,Potential]
        SelectEdgeAndDirection
        [monodromySolve,SelectEdgeAndDirection]
        [solveFamily,SelectEdgeAndDirection]
        [sparseMonodromySolve,SelectEdgeAndDirection]
        StoppingCriterion
	[dynamicFlowerSolve,StoppingCriterion]        
        [monodromySolve,StoppingCriterion]
	[solveFamily,StoppingCriterion]
	[sparseMonodromySolve,StoppingCriterion]
        TargetSolutionCount
        [monodromySolve,TargetSolutionCount]
        [solveFamily,TargetSolutionCount]
        [sparseMonodromySolve,TargetSolutionCount]
        [monodromySolve,Verbose]
        [solveFamily,Verbose]
        [sparseMonodromySolve,Verbose]
	Randomizer
        [solveFamily,Randomizer]
        [sparseMonodromySolve,Randomizer]
        Equivalencer
        [solveFamily,Equivalencer]
        [sparseMonodromySolve,Equivalencer]
        FilterCondition
        [solveFamily,FilterCondition]
        [sparseMonodromySolve,FilterCondition]
    Description
        Text
            Here are some options for the solvers. The current defaults for a given solver may be accessed like so:
        Example
                options monodromySolve
        Code
                HEADER3 "List of Options",
                UL {
                {BOLD "Augmentation Options (trigger dynamic strategies)", 
                UL {
                "AugmentEdgeCount: number of edges added in augmentation step",
                {"AugmentGraphFunction: see ", TO flowerGraphAugment, " and ", TO completeGraphAugment},
                "AugmentNodeCount: number of nodes in augmentation step (dynamic strategy triggered if >0)",
                "AugmentNumberOfRepeats: max number of augmentation steps should be set to ensure termination"                    
                    }},
                "BatchSize: maximum number of solutions tracked across an edge",
                "EdgesSaturated: fills correspondence tables after stopping criteria satisfied",
		"Equivalencer: a numeric function of solutions (default (x -> x) that divides partial solutions into equivalence classes",
		"FilterCondition: boolean function (default (p, x) -> false)) that evaluates true when we don't want to collect x (useful when x is an extraneous solution produced by path-jumping)",
                {"GraphInitFunction: the underlying graph topology, see ", TO completeGraphInit, " and ", TO flowerGraphInit},
                "NumberOfEdges: number of edges in underlying graph",
                "NumberOfNodes: number of nodes in underlying graph",
                "NumberOfRepeats: argument for StoppingCriterion",
		{"PointArrayTol: tolerance used for comparing elements contained in", ofClass PointArray},
                {"Potential: a function that assigns a number to each edge in each iteration, indicating its
                potential for producing new solutions. Current supported potential functions are ", TO potentialE , " and ", 
                TO potentialLowerBound},
	        "Randomizer: a function of the parameters that randomizes the homotopies' endpoints. Defaults to the", TO gamma," trick when linear in parameters, otherwise no randomization (p -> p)",
                {"SelectEdgeAndDirection: accepts either ", TO selectBestEdgeAndDirection, " or ",
                 TO selectRandomEdgeAndDirection, ". The former also requires setting a potential. Default is an internal function that selects the first available edge." },
                "StoppingCriterion: eg. stop if no progress has been made",
                "TargetSolutionCount: expected/desired number of solutions (overrides StoppingCriterion)",
                "Verbose: reports progress in each iteration"
                }
    ///

doc ///
    Key
        HomotopyNode
        PartialSols
        SpecializedSystem
    Description
        Text
	    Object associated to a polynomial system.
        Code 
	    HEADER3 "Keys associated with HomotopyNode",
	        UL {
		    "BasePoint: the parameter point defining some system",
		    "Edges: a mutable list of edges",
		    "Graph: a HomotopyGraph containing the node",
		    {"PartialSols: ", TO PointArray, " of solutions for SpecializedSystem"},
		    "SpecializedSystem: list of equations for system specialized at the BasePoint"
		    }        

        ///
    
doc ///
    Key
        HomotopyGraph
    Description
        Text
            The graph of systems connected by homotopies. Inherits from
            @TO MutableHashTable @.
        Code 
	    HEADER3 "Keys associated with HomotopyGraph",
            UL {
            "Edges: a mutable list of edges",
            "Family: the parametric system (or null)",
            "Potential: the potential function used (or null)",
            "Vertices: a mutable list of nodes"
            }        
    SeeAlso
            "HomotopyNode"
        ///
    
doc ///
    Key
        potentialLowerBound
    Headline
        the potential which is equal to the minimal number of new points guaranteed to be discovered
    Description
        Text
            This is an option for the Potential option for @TO "monodromySolve" @ when we use
            @TO "selectBestEdgeAndDirection" @ option to select edge and direction. This option
            observes discovered and undiscovered points first, and then follows the homotopy which
            has the minimal number of new points quaranteed to be found.
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            (V, npaths) = monodromySolve(polys,SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
            G = V.Graph;
            apply(toList G.Edges,e->e.Potential12)--potentials for all edges of a given direction
    SeeAlso
        "selectBestEdgeAndDirection"
        "potentialE"
        "HomotopyEdge"
    ///

doc ///
    Key
        flowerGraphInit
    Headline
        solve via monodromy by using flower shaped graph
    Description
        Text
            This is an option for the function monodromySolve which
            uses a flower graph. That is, the seed system is the central
            vertex of the graph; it is connected to all other vertices by two
            paths, thus creating a "petal" for each additional vertex.  
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            monodromySolve(polys,GraphInitFunction => flowerGraphInit)    
    /// 
    
    
doc ///
    Key
        computeMixedVolume
        (computeMixedVolume, List)
    Headline
        compute mixed volume via Gfan
    Usage
        d = computeMixedVolume polys
    Inputs
        polys:List
            containing polynomials    
    Outputs
        d:ZZ
    Description
        Text
            Computes mixed volume of a polynomial system using the package {TO gfanInterface}. For generic systems of a given support set, this is the generic
            root count.
        Example
	   setRandomSeed 0;
            R = CC[x,y];
            polys = {x+y^2,x*y+1};
            mixedVol = computeMixedVolume polys;
            sparseMonodromySolve(polySystem polys,TargetSolutionCount=>mixedVol,NumberOfNodes=>3)
    ///


doc ///
    Key
        flowerGraphAugment
    Headline
        augment graph with the flower graph structure
    Description
        Text
            This is a possible value of the option @TO AugmentGraphFunction @ of the
            function monodromySolve. It will augment the graph while respecting the flower
            graph structure, so it should be used in conjunction with @TO flowerGraphInit @. 
            To use it, it is necessary to specify a number of nodes or
            edges to augment by using AugmentNodeCount or AugmentEdgeCount.
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>flowerGraphAugment,AugmentNodeCount=>1, AugmentNumberOfRepeats=>3)
    ///
    
    
doc ///
    Key
        completeGraphAugment
    Headline
        augment graph with the complete graph structure
    Description
        Text
            This is a possible value of the option AugmentGraphFunction of the
            function monodromySolve. It will augment the graph while respecting the complete
            graph structure, so it should be used in conjunction with flowerGraphInit. 
            To use it, it is necessary to specify a number of nodes or
            edges to augment by using AugmentNodeCount or AugmentEdgeCount.
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentNodeCount=>1, AugmentNumberOfRepeats=>3)
    ///    

doc ///
    Key
        makeBatchPotential      
    Headline
        batch sensitive potentialE
    Description 
        Text
            This is a more general @TO potentialE @, which accounts for the fact that the expected number of points discovered along an edge 
            depends on the @TO BatchSize @.
    SeeAlso
        "potentialE"
        "MonodromySolverOptions"
    ///

doc ///
    Key
        homotopyGraph
    Headline
        HomotopyGraph Constructor
    Usage
        G = homotopyGraph PS
    Inputs
        PS:System
    Outputs
        G:HomotopyGraph
    ///

doc ///
    Key
        getTrackTime
        (getTrackTime, HomotopyGraph)
    Headline
        elapsed time taken by solver
    Usage
        t = getTrackTime G
    Inputs
        G:HomotopyGraph
    Outputs
        t:RR
    Description
        Text
            This only works on graphs which have been modified by a solver.
    SeeAlso    
        "elapsedTime"
    ///
    
doc ///
    Key
        dynamicFlowerSolve
    Headline
        a naive dynamic strategy
    Usage
        (L, npaths) = dynamicFlowerSolve(M,p0,L)
    Inputs
        M:Matrix
            defining polynomial system
        p0:Point
            associated to a specialized system
        L:List
            containing partial solutions associated to p0
    Outputs
        L:List
        npaths:ZZ
    Description
        Text
            Output is verbose. For other dynamic strategies, see 
            @TO MonodromySolverOptions @.
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            (p0, x0) = createSeedPair polys;
            (L, npaths) = dynamicFlowerSolve(polys.PolyMap,p0,{x0})
    ///   

doc ///
    Key
        appendPoint
        (appendPoint, PointArray, Point)
    Headline
        append a point at the end of a PointArray
    Usage
        appendPoint(A,p)
    Inputs
        A:PointArray
        p:Point
    ///

doc ///
    Key
        appendPoints
        (appendPoints, PointArray, List)
    Headline
        append a list of points at the end of a PointArray
    Usage
        appendPoints(A,L)
    Inputs
        A:PointArray
        L:List
    ///    
 
doc ///
    Key
        completeGraphInit
    Headline
        initialize the topology of a complete graph
    Description
        Text
            This is an option for the function monodromySolve which uses a complete graph.
            For instance, the user can choose to have 4 vertices and 2 edges connecting
            each pair of vertices. Then the homotopy will run on the complete graph
            on 4 vertices, where each edge is doubled. 
        Example
            R = CC[a,b,c,d][x,y];
            polys = polySystem {a*x+b*y^2,c*x*y+d};
            monodromySolve(polys,GraphInitFunction => completeGraphInit)    
                ///

doc ///
    Key
        potentialE
    Headline
        the "expected" potential of an edge
    Description
        Text
            This is an option for the Potential option for @TO "monodromySolve" @ when we use
            @TO "selectBestEdgeAndDirection" @ option to select edge and direction. This option
            computes the expected number of new points obtained by tracking points (under suitable randomness asumptions
            about the permutations generated by the underlying graph.) The expected value is computed by the ratio of unmatched
	    points and the difference between the total solution count and the number of the known points.
        Example
	    R = CC[a,b,c,d,e,f,g,h][x,y,z];
            polys = polySystem {a*x+b*y+c*z,d*x*y+e*x*z+f*y*z,g*x*y*z+h};
        Text
            In here, we need the target number of solutions, and we will use the mixed volume for that.
        Example
	    (p0,x0) := createSeedPair polys
        Text
	    We will comput the mixed volume to find the number of solution counts.
        Example
            mixedVolume = computeMixedVolume specializeSystem(p0,polys)
            monodromySolve(polys,p0,{x0},SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE, TargetSolutionCount=>mixedVolume)
    SeeAlso
        "selectBestEdgeAndDirection"
        "potentialLowerBound"
        "computeMixedVolume"
        "Potential"
    ///  

doc ///
    Key
        pointArray
    Headline
        constructor for PointArray
    Usage
        A = pointArray L
    Inputs
        L:List
            containing objects of type @TO Point@
    Outputs
        A:PointArray
    ///

doc ///
    Key
        PointArray
    Headline
            a container for solutions
    Description
        Text
            PointArray is a data structure that organizes the solutions found
            by a solver. Each @TO HomotopyNode @ object V has an assocaciated
            PointArray accessed via V.PartialSols. A "fingerprinting"
            scheme allows for equality of points to be checked quickly.        
    ///

doc ///
    Key
        (indices,PointArray)
    Headline
        returns indices of a PointArray
    Usage
        indices A
    Inputs
        A:PointArray
    SeeAlso
        "indices"
    ///


doc ///
    Key
        (length, PointArray)
    Headline
        number of items stored in a PointArray
    Usage
        length A
    Inputs
        A:PointArray
    SeeAlso
        "length"
    ///

doc ///
    Key
        (net, PointArray)
    Headline
        pretty printing
    Usage
        net A 
    Inputs
        A:PointArray
    SeeAlso
        "Net"
    ///

doc ///
    Key
        (member, Point, PointArray)
    Headline
        test Point membership in a PointArray
    Usage
        member(p,A)
    Inputs
        p:Point
        A:PointArray
    SeeAlso
        "member"
    ///


doc ///
    Key
        monodromyGroup
        (monodromyGroup, System)
        (monodromyGroup, System, Point, List)
    Headline
        compute the group of permutations implicitly defined by a homotopy graph
    Usage
        monodromyGroup S
        monodromyGroup(S, p0, x0s)
    Inputs
        S:System
        p0:Point
            a basepoint for the monodromy group in the parameter space for S
        x0s:List
            points in the fiber over S        
        FileName=>String
            a name for an output file suitable for reading by GAP. Default value is null.
    Description
        Text
            If the monodromy group is full symmetric and the degree is large, then the default settings have a good chance of generating the whole group. However, you will need to use a bigger graph than the default settings to fully generate imprimitive groups, as in the following example of a Euclidean distance degree calculation.
        Example
            setRandomSeed 100;
            declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3};
            paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}};
            varMatrix = gateMatrix{{t_1,t_2}};
            phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}};
            loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2;
            dLoss = diff(varMatrix, gateMatrix{{loss}});
            G = gateSystem(paramMatrix,varMatrix,transpose dLoss);
            monodromyGroup(G,"msOptions" => {NumberOfEdges=>10})
    Caveat
        This is still somewhat experimental.
    ///
    
end
restart
needs "ed-gal.m2"
monodromyGroup(G,FileName=>"eddGG","msOptions" => {Verbose=>true,NumberOfEdges=>10})

    SeeAlso
        "member"
    ///



-*
doc ///
    Key
        seedTest
	(seedTest, System, Point, Point)
    Headline
        test initial seed
    Usage
        H = seedTest(P,p0,L)
    Inputs
        P:System
        p0:Point
	L:List
    Outputs
        H:MutableHashTable
    Description
        Text
            The success of MonodromySolver depends in part on the quality of an initial seed.
	    The function @TO createSeedPair @ assumes very little of its input; seedTest performs
	    some basic tests on the seed data:
    ///
*-
    
-* doc ///
    Key
        "Example 1"
    Headline

    Description
        Text
    Example

    /// *-
