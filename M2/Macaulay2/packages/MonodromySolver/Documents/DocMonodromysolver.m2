doc ///
	Key
		MonodromySolver 
	Headline
		solve polynomial systems via homotopy continuation and monodromy
	Description
		Text
			This package provides tools to find all solutions of a generic system
			in a family of polynomial systems with parametric coefficients using
			numerical homotopy continuation and the action of the monodromy group. 
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			(V, npaths) = monodromySolve polys; 
			<< "tracked " << npaths << " homotopy paths";
			V.BasePoint 
			points V.PartialSols
    	    	Text
			The algorithm is introduced in 
			@HREF("https://arxiv.org/abs/1609.08722","\"Solving polynomial systems
			via homotopy continuation and monodromy\" (2016)")@.
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
    [monodromySolve,Verbose]
  Headline
    If the value is true, reports progress./// 
    
doc ///
  Key 		
    [monodromySolve,TargetSolutionCount]
  Headline
    Number of solutions to the polynomial system to be found./// 
    
doc ///
  Key 		
    [monodromySolve,StoppingCriterion]
  Headline
    Stopping criterion for the algorithm./// 
    
doc ///
  Key 		
    [monodromySolve,SelectEdgeAndDirection]
  Headline
    Select edge and direction for homotopy./// 

doc ///
  Key 		
    [monodromySolve,Potential]
  Headline
    Specify type of potential function for selection of edge in homotopy./// 
    
doc ///
  Key 		
    [monodromySolve,NumberOfEdges]
  Headline
    Specify the number of edges in the HomotopyGraph./// 

doc ///
  Key 		
    [monodromySolve,NumberOfNodes]
  Headline
    Specify the number of nodes in the HomotopyGraph./// 
    
doc ///
  Key 		
    [monodromySolve,NumberOfRepeats]
  Headline
    Number of repeats for the output before termination./// 
    
doc ///
  Key 		
    [monodromySolve,BatchSize]
  Headline
    Changes the number of paths tracked at once.///   
    
doc ///
    Key 
    	[monodromySolve, GraphInitFunction]
    Headline
    	Type of initial graph: flowerGraphInit or completeGraphInit.///  
    
doc ///
    Key 
    	[monodromySolve, AugmentGraphFunction]
    Headline
    	Specify a function to be used to augment the HomotopyGraph./// 
	
doc ///
    Key 
    	[monodromySolve, AugmentNodeCount]
    Headline
    	Number of nodes by which to augment graph./// 	 
	
doc ///
    Key 
    	[monodromySolve, AugmentEdgeCount]
    Headline
    	Number of edges by which to augment graph./// 
	
doc ///
    Key 
    	[monodromySolve, AugmentNumberOfRepeats]
    Headline
    	Number of times to augment the graph before termination./// 	

doc ///
	Key
		monodromySolve
		BatchSize
		GraphInitFunction
		SelectEdgeAndDirection
		StoppingCriterion
		TargetSolutionCount
		NumberOfEdges
		NumberOfNodes
		NumberOfRepeats
		(monodromySolve, PolySystem)
		(monodromySolve, PolySystem, Point, List)
	Headline
		solving system of equations by using monodromy loops
	Description
		Text
			This is the main function used to solve the polynomial system
			by using homotopy continuation and monodromy loops. 
			--For examples on how to use the options see @TO "Examples" @.
		Text
			Set the polynomial ring and the system of polynomials. 
		Example
			R = CC[a,b,c,d][A,B]
			polys = polySystem {A^2*a+B^2*b,A*B*c+d}  
		Text
			Create the "seed" used to initiate the homotopy process.
			This function uses a seed solution to the generic system, which is then 
			traced via linear homotopies to obtain all solutions of the system.  
		Example
			(p0,x0) := createSeedPair polys
		Text
			Below is the output of the main function, which displays the number of
			paths followed to obtain the solutions. To view the solutions
			use the last command displayed. 
		Example    
			(V,npaths) = monodromySolve(polys,p0,{x0})
			points V.PartialSols
	SeeAlso
		"createSeedPair"
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
		AugmentGraphFunction
	Headline
		specify a function to be used to augment the HomotopyGraph
	Description
		Text
			This is an option of the function monodromySolve.
			It is possible that static graph strategies will fail to find all 
			solutions to a polynomial system. In that eventuality, it is convenient to
			be able to augment the graph and have the process try again. If set, this
			function tells MonodromySolve in what way to modify the HomotopyGraph.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentNodeCount=>1)
	///

doc ///
	Key
		AugmentNumberOfRepeats
	Headline
		number of times to augment the graph before termination
	Description
		Text
			This optional input to monodromySolve guarantees that when a 
			HomotopyGraph is repeatedly augmented, the process eventually terminates.
			It is possible that after multiple augmentations and reattempts that all
			solutions could still not have been found. In that case, 
			AugmentNumberOfRepeats can be set to stop the number of times that the
			process will iterate.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentNodeCount=>1,AugmentNumberOfRepeats=>10)
	///

doc ///
	Key
		AugmentEdgeCount
	Headline
		number of edges by which to augment graph
	Description
		Text
			This is an option of monodromySolve which tells the AugmentGraphFunction
			how many edges should be used to augment the graph. This will not do
			anything if AugmentGraphFunction is not set.
		Example
			R = CC[a,b,c,d][x,y];
			polys = polySystem {a*x+b*y^2,c*x*y+d};
			monodromySolve(polys,GraphInitFunction => flowerGraphInit, AugmentGraphFunction=>completeGraphAugment,AugmentEdgeCount=>1)
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
    	HomotopyGraph
	Vertices
	Edges
	Family
    Headline
        a graph organizing homotopies for monodromy computation
///
 
doc ///
    Key
    	HomotopyEdge
	gamma1
	gamma2
	Correspondence12
	Correspondence21
    Headline
    	stores gamma values for the homotopy and correspondences between two nodes
///   

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
    	
{* doc ///
    Key
    	"Example 1"
    Headline

    Description
    	Text
	Example

    /// *}
