beginDocumentation()
doc ///
    Key
        selectRandomEdgeAndDirection
	
    Headline

    Description
    	Text

    	Example

	Text
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
	    (p0,x0) = createSeedPair polys
            monodromySolve(polys,p0,{x0},GraphInitFunction => completeGraphInit)	
    /// 


doc ///
    Key
        selectBestEdgeAndDirection

    Headline

    Description
    	Text

    	Example

	Text
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
	    (p0,x0) = createSeedPair polys
	Text 
	    Set nodes by using createSeedPair function.
	Example    
            monodromySolve(polys,p0,{x0})
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
	    (p0,x0) = createSeedPair polys
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
	    (p0,x0) = createSeedPair polys
            monodromySolve(polys,p0,{x0},GraphInitFunction => flowerGraphInit)	
    /// 
    
    
doc ///
    Key
    	computeMixedVolume
	(computeMixedVolume, List)
    Headline

    Description
    	Text

    	Example

	Text
    /// 