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
    /// 



doc ///
    Key
    	completeGraphInit

    Headline
        solve via monodromy by using complete graph.
    Description

    	Text
            This is an option for the function monodromySolve which makes one to use the complete graph.
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
        Solving system of equations by using monodromy loops
    Description
    	Text
            This function is the main function to solve the system of equations by using monodromy loops.
    	Example
            R = CC[a,b,c,d][A,B]
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}
	Text
	    Set the polynomial ring and the system of polynomials.    
        Example
	    (p0,x0) := createSeedPair polys
	Text 
	    Set nodes by using createSeedPair function.
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
        Taking random points for polynomial system.
    Description
    	Text
            This function is used to take points for polynomial system randomly.
    	Example
            R = CC[a,b,c,d][A,B]
            polys = polySystem matrix{{A*a+B*b},{A*B*c+d}}
	    (p0,x0) := createSeedPair polys
    /// 
    
    
    
doc ///
    Key
    	flowerGraphInit

    Headline
        solve via monodromy by using flower shaped graph.
    Description
    	Text
            This is an option for the function monodromySolve which makes one to use the flower shaped graph.
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
