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
	
	   This package implements an algorithm introduced in 
	   @HREF("https://arxiv.org/abs/1609.08722","Solving polynomial systems
	       via homotopy continuation and monodromy")@.
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
	    R = CC[a,b,c,d][A,B]
            polys = polySystem {A*a+B*b,A*B*c+d}
	    (p0,x0) := createSeedPair polys
            monodromySolve(polys,p0,{x0},SelectEdgeAndDirection=>selectRandomEdgeAndDirection)
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
	    R = CC[a,b,c,d][A,B]
            polys = polySystem {A*a+B*b,A*B*c+d}
	    (p0,x0) = createSeedPair polys
            monodromySolve(polys,p0,{x0}, SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
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
	    R = CC[a,b,c,d][A,B]
            polys = polySystem {A*a+B*b,A*B*c+d}
	    (p0,x0) = createSeedPair polys
            monodromySolve(polys,p0,{x0}, SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound)
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
	    R = CC[a,b,c,d][A,B]
            polys = polySystem {A*a+B*b,A*B*c+d}
	    (p0,x0) = createSeedPair polys
	Text
	    In here, we need the target number of solutions, and we will use the mixed volume for that.
	Example    
	    mixedVolume = computeMixedVolume specializeSystem (p0,polys)
            monodromySolve(polys,p0,{x0}, SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE, TargetSolutionCount=>mixedVolume)
    SeeAlso
         "selectBestEdgeAndDirection"
	 "potentialLowerBound"
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
            R = CC[a,b,c,d][A,B]
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}
	    (p0,x0) := createSeedPair polys
            monodromySolve(polys,p0,{x0},GraphInitFunction => completeGraphInit)	
    /// 


doc ///
    Key
        monodromySolve
	(monodromySolve, Matrix, Point, List)
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
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}  
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
            R = CC[a,b,c,d][A,B]
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}
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
            R = CC[a,b,c,d][A,B]
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}
	    (p0,x0) := createSeedPair polys
            monodromySolve(polys,p0,{x0},GraphInitFunction => flowerGraphInit)	
    /// 
    
    
doc ///
    Key
    	computeMixedVolume
	(computeMixedVolume, List)
    Headline
    	compute mixed volume via PHCpack
    Description
    /// 

{* doc ///
    Key
    	"Example 1"
    Headline

    Description
    	Text
	Example

    /// *}