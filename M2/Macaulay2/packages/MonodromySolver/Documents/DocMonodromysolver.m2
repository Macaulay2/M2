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
    Headline
    	a solver for parametric families with simple ouput
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
		Example    
		    	setRandomSeed 0;
    	    	    	(V,npaths) = monodromySolve(polys, NumberOfNodes => 3);
	SeeAlso
	    	"MonodromySolverOptions"
		"(monodromySolve, PolySystem, Point, List)"
		"flowerGraphInit"
		"completeGraphInit"
	/// 
    
doc ///
    	Key
	    	specializeSystem
		(specializeSystem, Point, PolySystem)
		(specializeSystem, Point, Matrix)
	Headline
	    	speliaze system at a point in the parameter space.///
		
doc ///
    	Key
	    	randomWeights
		(randomWeights, ZZ)
	Headline
	    	Generates a random complex vector.///		

    
doc ///
	Key
		createSeedPair
		(createSeedPair, PolySystem)
		(createSeedPair, PolySystem, List)
	Headline
		create initial seed for the homotopy continuation
	Description
		Text
			This function creates a seed by selecting generic parameter. The
			argument p0 below is the tuple corresponding to the parameters, and 
			x0 is the seed solution to the system with those parameters.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			(p0,x0) := createSeedPair polys
	///

doc ///
	Key
		selectRandomEdgeAndDirection
	Headline
		random selection of edge and direction for homotopy 
	Description
		Text
			This is an option for the function @TO "monodromySolve" @. By default, the option
			SelectEdgeAndDirection is set to selectRandomEdgeAndDirection.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,SelectEdgeAndDirection=>selectRandomEdgeAndDirection)
		Text
			Note that in this example we have not specified the type of selection for edge and direction for the homotopy;
			this implies that selectRandomEdgeAndDirection is used. 
	SeeAlso
		"selectBestEdgeAndDirection"
	/// 

doc ///
	Key
		selectBestEdgeAndDirection
	Headline
		selects edge and direction based on highest potential for obtaining new information
	Description
		Text
			This is an option for the function @TO "monodromySolve" @. By default, the option
			SelectEdgeAndDirection is set to selectRandomEdgeAndDirection; it can be changed to 
			selectBestEdgeAndDirection as shown below. The use of selectBestEdgeAndDirection requires
			the choice of a potential function as well. 
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
	SeeAlso
		"selectRandomEdgeAndDirection"
		"potentialLowerBound"
		"potentialE"
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
		    	UL {
			    "BatchSize: maximum number of solutions tracked across an edge",
			    "EdgesSaturated: fills correspondence tables after stopping criteria satisfied",
			    {"GraphInitFunction: underlying graph topology (eg. complete, flower)", TO completeGraphInit},
			    "NumberOfEdges: number of edges in underlying graph",
			    "NumberOfNodes: number of nodes in underlying graph",
			    "NumberOfRepeats: argument for StoppingCriterion",
			    {"Potential: a function that assigns a number to a ", TO HomotopyEdge, " in each iteration, indicating its
				potential for producing new solutions. Current supported potential functions are ", TO potentialE , " and ", 
				TO potentialLowerBound},
			    {"SelectEdgeAndDirection: currently accepts either ", TO selectBestEdgeAndDirection, " or ",
				 TO selectRandomEdgeAndDirection, ". Note that the former also requires setting " },
			    "StoppingCriterion: eg. stop if no progress has been made",
			    "TargetSolutionCount: expected/desired number of solutions (overrides StoppingCriterion)",
			    "Verbose: reports progress in each iteration"
			    }
	///

doc ///
	Key
    	    	HomotopyNode
		Edges
		PartialSols
		SpecializedSystem
    	///
	

doc ///
	Key
    	    	HomotopyGraph
		Family
		Graph
		Potential
    	///
	
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
			monodromySolve(polys,SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
	SeeAlso
		"selectBestEdgeAndDirection"
		"potentialE"
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
	Description
		Text
			Computes mixed volume of a polynomial system, which is a sharp bound on the
			number of roots.
		Example
			R = CC[x,y]
			polys = {x+y^2,x*y+1}
			mixedVol = computeMixedVolume polys
	///


doc ///
	Key
		flowerGraphAugment
	Headline
		augment graph with the flower graph structure
	Description
		Text
			This is a possible value of the option AugmentGraphFunction of the
			function monodromySolve. It will augment the graph while respecting the flower
			graph structure, so it should be used in conjunction with flowerGraphInit. 
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
		AugmentNodeCount
	Headline
		number of nodes by which to augment graph
	Description
		Text
			This is an option of monodromySolve which tells the AugmentGraphFunction
			how many nodes should be used to augment the graph. This will not do
			anything if AugmentGraphFunction is not set.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentNodeCount=>1)
	///

doc ///
    Key
    	pointArray
    Headline
    	an array of labelled points, to which one may append new elements///

doc ///
    Key
    	makeRandomizedSelect
    Headline
    	randomly chooses SelectEdgeAndDirection option///	

doc ///
    Key
    	makeBatchPotential	  
    Headline
	expected number of new points based on batch size///

doc ///
    Key
    	homotopyGraph
    Headline
    	creates HomotopyGraph of a polynomial system///

doc ///
    Key
    	getTrackTime
	(getTrackTime, HomotopyGraph)
    Headline
    	tracks time///
	
doc ///
    Key
    	dynamicFlowerSolve
    Headline
    	a naive dynamic strategy///	

doc ///
    Key
    	appendPoint
	(appendPoint, PointArray, Point)
    Headline
    	append a point at the end of a PointArray///

doc ///
    Key
    	appendPoints
	(appendPoints, PointArray, List)
    Headline
    	append a list of points at the end of a PointArray///	

doc ///
    Key
    	PointArray
    	(indices, PointArray)
	(length, PointArray)
	(member, Point, PointArray)
	(net, PointArray)
    Headline
    	an array of points (labelled with 0,1,...) to which one may append new elements
///

 
doc ///
	Key
		completeGraphInit
	Headline
		solve via monodromy by using complete graph.
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
		the potential which is equal to the expected number of new points obtained by tracking one point from the other
	Description
		Text
			This is an option for the Potential option for @TO "monodromySolve" @ when we use
			@TO "selectBestEdgeAndDirection" @ option to select edge and direction. This option
			computes the expected number of new points obtained by tracking points. The expected value
			is computed by the ratio of unmatched points and the difference between the total solution count and
			the number of the known points.
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
	///  
    	
{* doc ///
    Key
    	"Example 1"
    Headline

    Description
    	Text
	Example

    /// *}
