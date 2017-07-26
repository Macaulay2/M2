-- undocumented methods and symbols (for each, consider... does it really need to be exported? should it be documented?)
undocumented {randomWeights, Vertices, (pointArray,List), saturateEdges,  (saturateEdges,HomotopyGraph), (makeRandomizedSelect,RR), (makeBatchPotential,ZZ), (dynamicFlowerSolve,Matrix,Point,List), RandomPointFunction, 
     Correspondence21, Edges, Correspondence12, Potential21, Potential12, Family, gamma1, gamma2, Graph, Node1,  Node2, HomotopyEdge, makeRandomizedSelect}
    --, SpecializedSystem,  HomotopyNode, HomotopyGraph,PartialSols}
-- undocument tags
undocumented {(symbol _,PointArray,List), (symbol _,PointArray,ZZ), (points,PointArray), (position,Point,PointArray), (positions,PointArray,Function),  1:(homotopyGraph), [dynamicFlowerSolve,RandomPointFunction], [homotopyGraph,Potential], [dynamicFlowerSolve,TargetSolutionCount], [homotopyGraph,Family]}

doc ///
	Key
		MonodromySolver 
	Headline
		solve polynomial systems via homotopy continuation and monodromy
	Description
		Text
		    	This package provides randomized numerical methods for finding complex solutions to systems of polynomial equations.
			The main methods implemented in this package find solutions to polynomial systems of equations over the 
			complex numbers. As described in @HREF("https://arxiv.org/abs/1609.08722","\"Solving polynomial systems
			via homotopy continuation and monodromy\" (2016)")@, these methods pair numerical homotopy continuation with the 
			transitive monodromy action of a suitable covering map.
		Text
		    	It includes several blackbox functions based on these methods.
		Code
		        UL {
	   		    TO sparseMonodromySolve,
			    TO solveFamily,
	   		    TO monodromySolve,
			    TO dynamicFlowerSolve
	   	   	    } 
		Text
			The most basic interface is provided by the method @TO "sparseMonodromySolve" @. More advanced solvers can be applied to
			linearly parametrized families. The family in the example below is the 5-variable Reimer system from the Posso test
			suite: the generic solution count is 144, while the Bezout number and mixed volume are both 720.
		Text
		
		Example
    	    	    	setRandomSeed 0
		    	R = CC[a_1..a_5,b_1..b_5][x,y,z,t,u]   
			P = polySystem {-a_1+b_1*(x^2-y^2+z^2+u^2-t^2),-a_2+b_2*(x^3-y^3+z^3+u^3-t^3),-a_3+b_3*(x^4-y^4+z^4+u^4-t^4),-a_4+b_4*(x^5-y^5+z^5+u^5-t^5), -a_5+b_5*(x^6-y^6+z^6+u^6-t^6)}  				
		    	(N,npaths) = monodromySolve P				
			first N.SpecializedSystem -- (the first polynomial of) a randomly generated system in the family defined by P
			first N.PartialSols -- a solution to N.SpecializedSystem
			npaths -- total number of paths tracked in call to monodromySolve
		Text

		Text
		    	Each solver works by assembling randomly generated systems within a @TO HomotopyGraph@ and tracking paths between
			them. They are also equipped with a number of options, which may be useful for speeding up computation or 
			increasing the probability of success.
		Text
		    	In the example above, the underlying graph is "seeded" automatically. The current seeding implementation will fail,
			for instance, in cases where there are equations without parameters. In such a case, the user may find a seed pair
			themselves (see @TO (monodromySolve, PolySystem, Point, List) @ for an example.)
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
    	PS:PolySystem
	   whose coefficients are complex numbers
    Outputs
	sols:List
	    containing solutions to sys, each represented as a @TO Point @.
    Description
    	Text
	    Blackbox monodromy solver for a square polynomial system without parameters.
	    The example below finds all six intersection of a generic cubic with its quadratic polar curve.
	Text
	
	Example
        	setRandomSeed 0;
		R=CC[x,y,z];
		F=random(3,R);
		P=sum apply(gens R,g->diff(g,F)*random CC);
		sparseMonodromySolve polySystem {F,P,random(1,R)-1}
	Text
	    For systems with dense support such as the above, the number of paths tracked is generally not optimal, though timings may 
	    be comparable.
        ///

doc ///
    Key
        solveFamily
    	(solveFamily,PolySystem)
	(solveFamily,PolySystem,Point,List)
    Headline
    	a solver for parametric families with simple output
    Usage
    	(sys,sols) = solveFamily PS
    Inputs 
    	PS:PolySystem
	   whose underlying coefficient ring is defined by parameters.
    Outputs
    	sys:List
	    containing the equations of a random specialization of PS.
	sols:List
	    containing solutions to sys, each represented as a @TO Point @.
    Description
    	Text
	    The output of @TO monodromySolve @ is "technical." This method is intended for users uninterested in the underlying
	    @TO HomotopyGraph @ and its satellite data.
	Example
	    R = CC[a,b,c,d,e,f][x,y];
	    q  = a*x^2+b*y+c;
	    l = d*x+e*y+f;
    	    (sys, sols) = solveFamily polySystem {q,l}
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
    	    	    	    TO (monodromySolve, PolySystem, Point, List),			 
			    TO (monodromySolve, PolySystem)
			    }
       ///	   

doc ///
	Key
		(monodromySolve, PolySystem, Point, List)
		"(monodromySolve, PolySystem, Point, List)"
	Usage
	    	(N, npaths) = monodromySolve(PS,p0,L)
	Inputs
	    	PS:PolySystem
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
		(monodromySolve, PolySystem)
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
	        PS:PolySystem
		    with parameters as coefficients
	Outputs
	    	equations:List
		    containing equtaions of system with parameters specialized at p0
	Headline
	    	specialize parametric system at a point in the parameter space.///
    
doc ///
	Key
		createSeedPair
		(createSeedPair, PolySystem)
		(createSeedPair, PolySystem, List)
	Headline
		create initial seed for the homotopy continuation
	Usage
	    	(p0,x0) = createSeedPair PS
	Inputs
	    	PS:PolySystem
		    a parametric polynomial system
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
               (monodromySolve,PolySystem,Point,List)
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
			    {"GraphInitFunction: the underlying graph topology, see ", TO completeGraphInit, " and ", TO flowerGraphInit},
			    "NumberOfEdges: number of edges in underlying graph",
			    "NumberOfNodes: number of nodes in underlying graph",
			    "NumberOfRepeats: argument for StoppingCriterion",
			    {"Potential: a function that assigns a number to each edge in each iteration, indicating its
				potential for producing new solutions. Current supported potential functions are ", TO potentialE , " and ", 
				TO potentialLowerBound},
			    {"SelectEdgeAndDirection: currently accepts either ", TO selectBestEdgeAndDirection, " or ",
				 TO selectRandomEdgeAndDirection, ". Note that the former also requires setting " },
			    "StoppingCriterion: eg. stop if no progress has been made",
			    "TargetSolutionCount: expected/desired number of solutions (overrides StoppingCriterion)",
			    "Verbose: reports progress in each iteration"
			    }
	///

{* ---------- GHOSTS?

	
doc ///
	Key
	    	HomotopyEdge
		Correspondence12
		Correspondence21
		gamma1
		gamma2
		Node1
		Node2
		Potential12
		Potential21
    	///		
*}
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
		    	      "BasePoint: not exported",
		    	      "Edges: a mutable list of edges",
		    	      "Graph: a HomotopyGraph containing the node",
		    	      {"PartialSols: a ", TO PointArray, " of associated solutions"},
		    	      "SpecializedSystem: a list of equations associated to the node"
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
		compute mixed volume via PHCpack
	Usage
	    	d = computeMixedVolume polys
	Inputs
	    	polys:List
		    containing polynomials
		    
	Outputs
	    	d:ZZ
	Description
		Text
			Computes mixed volume of a polynomial system. For generic systems of a given support set, this is the generic
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
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>flowerGraphAugment,AugmentNodeCount=>1)
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
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentNodeCount=>1)
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
    	PS:PolySystem
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
	    assoiated to a specialized system
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
    Usage
    	appendPoint(A,p)
    Inputs
    	A:PointArray
    	p:Point
    Headline
    	append a point at the end of a PointArray///

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
    
    
{* doc ///
    Key
    	"Example 1"
    Headline

    Description
    	Text
	Example

    /// *}
