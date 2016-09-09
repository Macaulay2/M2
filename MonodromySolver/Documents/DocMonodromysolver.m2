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

    Description
    	Text

    	Example

	Text
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
	    nedges = 4
	    setRandomSeed 0
	    polys = parametrizedCyclic 5
	    (p0,x0) = createSeedPair polySystem polys
	Text
	    Setting up the polynomial system to solve
	Example
	    mixedVolume = computeMixedVolume specializeSystem (p0,polys)
	Text
	    The mixed volume represents the maximum number of solutions.
	Example
	    monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)
	Text
	    Function takes polynomial system and its starting point as inputs.
    SeeAlso
        "potentialE"
	
    /// 
    


    
doc ///
    Key
    	dynamicFlowerSolve
	(dynamicFlowerSolve, Matrix, Point, List)
    Headline

    Description
    	Text

    	Example

	Text
    /// 
    
    
    
doc ///
    Key
    	flowerGraphInit

    Headline

    Description
    	Text

    	Example

	Text
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