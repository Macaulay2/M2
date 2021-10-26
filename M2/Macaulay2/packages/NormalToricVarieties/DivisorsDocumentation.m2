------------------------------------------------------------------------------
-- Toric Divisors and Related Groups  (Documentation)
------------------------------------------------------------------------------
doc ///
    Key
        "working with divisors"
    Headline
        information about toric divisors and their related groups
    Description
        Text
            The following methods allows one to make and manipulate
            torus-invariant Weil divisors on a normal toric variety.
    	Text
	    @SUBSECTION "Operations on toric divisors"@
	Text
            @UL {
    	        TO ToricDivisor,	
        	TO (toricDivisor, List, NormalToricVariety),
    		TO (toricDivisor, NormalToricVariety),
		TO (toricDivisor, Polyhedron),
		TO (smallAmpleToricDivisor, ZZ, ZZ),
    		TO (symbol _, NormalToricVariety, ZZ),
    		TO (normalToricVariety, ToricDivisor),
    		TO (expression, ToricDivisor),	
    		TO (support, ToricDivisor),
    		TO (entries, ToricDivisor),
    		TO (symbol +, ToricDivisor, ToricDivisor),
    		TO (symbol SPACE, OO, ToricDivisor),
    		TO (isEffective, ToricDivisor),
    		TO (isCartier, ToricDivisor),    
    		TO (isQQCartier, ToricDivisor), 
    		TO (isNef, ToricDivisor),  
		TO (nefGenerators, NormalToricVariety),     
    		TO (isAmple, ToricDivisor),
    		TO (isVeryAmple, ToricDivisor),
    		TO (vertices, ToricDivisor),
    		TO (latticePoints, ToricDivisor),
		TO (monomials, ToricDivisor),
    		TO (polytope, ToricDivisor)
	    }@
	Text
            One can also work with the various groups arising from
            torus-invariant and the canonical maps between them.
    	Text
	    @SUBSECTION "Associated groups and maps"@
	Text
            @UL {	    
    		TO (weilDivisorGroup, NormalToricVariety),
    		TO (fromWDivToCl, NormalToricVariety),
    		TO (classGroup, NormalToricVariety),
    		TO (cartierDivisorGroup, NormalToricVariety),
    		TO (fromCDivToWDiv, NormalToricVariety),
    		TO (fromCDivToPic, NormalToricVariety),
    		TO (picardGroup, NormalToricVariety),
    		TO (fromPicToCl, NormalToricVariety)
	    }@
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
        "resolving singularities"
	"working with toric maps"	
        "working with sheaves"	
///


doc ///
    Key 
        (classGroup, NormalToricVariety)
        classGroup	
    Headline 
        make the class group
    Usage 
        classGroup X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Module 
	    a finitely generated abelian group
    Description
        Text
	    The class group of a variety is the group of Weil divisors divided
            by the subgroup of principal divisors.  For a normal toric
            variety, the class group has a presentation defined by the map
            from the group of torus-characters to group of torus-invariant
            Weil divisors induced by minimal nonzero lattice points on the
            rays of the associated fan.  For more information, see Theorem
            4.1.3 in Cox-Little-Schenck's {\em Toric Varieties}.
    	Text  
            The following examples illustrate some possible class groups.
	Example
    	    classGroup toricProjectiveSpace 1
    	    classGroup hirzebruchSurface 7
	    classGroup affineSpace 3	    
            classGroup normalToricVariety ({{4,-1},{0,1}},{{0,1}})
	    classGroup normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3))
    	Text
            The @TO2((ring, NormalToricVariety), "total coordinate ring")@ of
            a toric variety is graded by its class group.
	Example
	    degrees ring toricProjectiveSpace 1
	    degrees ring hirzebruchSurface 7	    
	    degrees ring affineSpace 3
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.	    
    SeeAlso
        "working with divisors"
        (rays, NormalToricVariety)
        (weilDivisorGroup, NormalToricVariety)
    	(ring, NormalToricVariety)
     	(fromPicToCl, NormalToricVariety)
    	(fromWDivToCl, NormalToricVariety)
	(classGroup, ToricMap)
///	


doc ///
    Key 
        (fromWDivToCl, NormalToricVariety)
        fromWDivToCl	
    Headline 
        get the map from the group of Weil divisors to the class group	
    Usage 
        fromWDivToCl X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Matrix 
	    defining the surjection from the torus-invariant Weil divisors to
     	    the class group
    Description
        Text
	    For a normal toric variety, the class group has a presentation
            defined by the map from the group of torus-characters to group of
            torus-invariant Weil divisors induced by minimal nonzero lattice
            points on the rays of the associated fan.  Hence, there is a
            surjective map from the group of torus-invariant Weil divisors to
            the class group.  This method returns a matrix representing this
            map.  Since the ordering on the rays of the toric variety
            determines a basis for the group of torus-invariant Weil divisors,
            this matrix is determined by a choice of basis for the class
            group.  For more information, see Theorem 4.1.3 in
            Cox-Little-Schenck's {\em Toric Varieties}.
    	Text
            The examples illustrate some of the possible maps from the 
	    @TO2(weilDivisorGroup, "group of torus-invariant Weil divisors")@
	    to the @TO2(classGroup, "class group")@.
	Example
    	    PP2 = toricProjectiveSpace 2;
    	    A1 = fromWDivToCl PP2
    	    assert ( (target A1, source A1) === (classGroup PP2, weilDivisorGroup PP2) )
	    assert ( A1 * matrix rays PP2 == 0)
    	Example
    	    X = weightedProjectiveSpace {1,2,2,3,4};
    	    A2 = fromWDivToCl X
    	    assert ( (target A2, source A2) === (classGroup X, weilDivisorGroup X) )	    
	    assert ( A2 * matrix rays X == 0)
    	Example
    	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
    	    A3 = fromWDivToCl Y
	    classGroup Y
    	    assert ( (target A3, source A3) === (classGroup Y, weilDivisorGroup Y) )	    
	    assert ( A3 * matrix rays Y == 0)	    
    	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    A4 = fromWDivToCl U
    	    classGroup U
    	    assert ( (target A4, source A4) === (classGroup U, weilDivisorGroup U) )	    
	    assert ( A4 * matrix rays U == 0)	  	    
    	Text
  	    This matrix also induces the grading on the total coordinate ring
            of toric variety.
    	Example  
    	    assert ( transpose matrix degrees ring PP2 === fromWDivToCl PP2)
    	    assert ( transpose matrix degrees ring X === fromWDivToCl X)
    	Text
	    The optional argument @TO WeilToClass@ for the constructor 
	    @TO normalToricVariety@ allows one to specify a basis of the class
	    group.
	Text
	    This map is computed and cached when the class group is first
	    constructed.
    SeeAlso
        "working with divisors"
        "making normal toric varieties"
        (weilDivisorGroup, NormalToricVariety)
    	(classGroup, NormalToricVariety)
    	(ring, NormalToricVariety)
///	


doc ///
    Key
        (weilDivisorGroup, NormalToricVariety)
        weilDivisorGroup	
    Headline 
        make the group of torus-invariant Weil divisors
    Usage 
        weilDivisorGroup X
    Inputs 
        X : NormalToricVariety 
    Outputs 
        : Module 
	    a finitely generated free abelian group
    Description
        Text
	    The group of torus-invariant Weil divisors on a normal toric
            variety is the free abelian group generated by the torus-invariant
            irreducible divisors.  The irreducible divisors correspond
            bijectively to rays in the associated fan.  Since the rays are
            indexed in this package by $0, 1, \dots, n-1$ the group of
            torus-invariant Weil divisors is canonically isomorphic to
            $\ZZ^n$.  For more information, see Theorem 4.1.3 in
            Cox-Little-Schenck's {\em Toric Varieties}.
    	Text
  	    The examples illustrate various possible Weil groups.
    	Example
     	    PP2 = toricProjectiveSpace 2;
    	    # rays PP2
    	    weilDivisorGroup PP2
    	Example
    	    FF7 = hirzebruchSurface 7;
    	    # rays FF7
    	    weilDivisorGroup FF7
    	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    # rays U
    	    weilDivisorGroup U
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.	    
    SeeAlso
        "working with divisors"
	(symbol _, NormalToricVariety, ZZ)
        (fromCDivToWDiv, NormalToricVariety)
    	(fromWDivToCl, NormalToricVariety)
    	ToricDivisor
	(weilDivisorGroup, ToricMap)
///


doc ///
    Key 
        (cartierDivisorGroup, NormalToricVariety)
        cartierDivisorGroup	
    Headline 
        compute the group of torus-invariant Cartier divisors
    Usage 
        cartierDivisorGroup X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Module
	    a finitely generated abelian group
    Description
        Text	    
	    The group of torus-invariant Cartier divisors on $X$ is the
            subgroup of all locally principal torus-invarient Weil divisors.
            On a normal toric variety, the group of torus-invariant Cartier
            divisors can be computed as an inverse limit.  More precisely, if
            $M$ denotes the lattice of characters on $X$ and the maximal cones
            in the fan of $X$ are $sigma_0, sigma_1, \dots, sigma_{r-1}$, then
            we have $CDiv(X) = ker( \oplus_{i} M/M(sigma_i{}) \to{}
            \oplus_{i<j} M/M(sigma_i \cap sigma_j{})$.  For more information,
            see Theorem 4.2.8 in Cox-Little-Schenck's {\em Toric Varieties}.
	Text
            When $X$ is smooth, every torus-invariant Weil divisor is Cartier.
      	Example
    	    PP2 = toricProjectiveSpace 2;
	    cartierDivisorGroup PP2
    	    assert (isSmooth PP2 and weilDivisorGroup PP2 === cartierDivisorGroup PP2)
    	    assert (id_(cartierDivisorGroup PP2) == fromCDivToWDiv PP2)
    	Example
    	    FF7 = hirzebruchSurface 7;
    	    cartierDivisorGroup FF7
   	    assert (isSmooth FF7 and weilDivisorGroup FF7 === cartierDivisorGroup FF7)
    	    assert (id_(cartierDivisorGroup FF7) == fromCDivToWDiv FF7)	    
    	Text
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier; every torus-invariant Weil divisor has a
            positive integer multiple that is Cartier.
    	Example  
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
	    assert (isSimplicial U and not isSmooth U and not isComplete U)
    	    cartierDivisorGroup U
	    weilDivisorGroup U
	    prune coker fromCDivToWDiv U
    	    assert ( (coker fromCDivToWDiv U) ** QQ == 0)
	Example
	    X = weightedProjectiveSpace {1,2,2,3,4};
	    assert (isSimplicial X and not isSmooth X and isComplete X)
   	    cartierDivisorGroup X
	    weilDivisorGroup X
	    prune coker fromCDivToWDiv X
    	    assert (rank coker fromCDivToWDiv X === 0)
	Text	    
            In general, the Cartier divisors are only a subgroup of the Weil divisors.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    cartierDivisorGroup Q
    	    weilDivisorGroup Q
	    prune coker fromCDivToWDiv Q
	    assert (rank coker fromCDivToWDiv Q === 1)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isComplete Y)
    	    cartierDivisorGroup Y
    	    weilDivisorGroup Y
    	    prune cokernel fromCDivToWDiv Y
	    assert (rank coker fromCDivToWDiv Y === 4)
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.	    
    SeeAlso
        "working with divisors"
        (fromCDivToWDiv,NormalToricVariety)
    	(fromCDivToPic,NormalToricVariety)
    	(isCartier,ToricDivisor)
	(cartierDivisorGroup, ToricMap)
///
	

doc ///
    Key
        (fromCDivToWDiv, NormalToricVariety)
        fromCDivToWDiv	
    Headline 
        get the map from Cartier divisors to Weil divisors
    Usage 
        fromCDivToWDiv X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix 
	    representing the inclusion map from the group of torus-invariant
	    Cartier divisors to the group of torus-invariant Weil divisors
    Description
        Text
	    The 
	    @TO2(cartierDivisorGroup, "group of torus-invariant Cartier divisors")@ 
	    is the subgroup of all locally principal torus-invariant
            @TO2(weilDivisorGroup, "Weil divisors")@.  This function produces
            the inclusion map with respect to the chosen bases for the two
            finitely-generated abelian groups.  For more information, see
            Theorem 4.2.1 in Cox-Little-Schenck's {\em Toric Varieties}.
    	Text  
            On a smooth normal toric variety, every torus-invariant Weil
            divisor is Cartier, so the inclusion map is simply the identity
            map.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
	    assert (isSmooth PP2 and isProjective PP2)
    	    fromCDivToWDiv PP2
    	    assert (fromCDivToWDiv PP2 === id_(weilDivisorGroup PP2))
    	Example
	    X = smoothFanoToricVariety (4,20);
	    assert (isSmooth X and isProjective X and isFano X)
	    fromCDivToWDiv X
	    assert (fromCDivToWDiv X === id_(weilDivisorGroup X))
	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)
    	    fromCDivToWDiv U
	    assert (fromCDivToWDiv U === id_(weilDivisorGroup U))	    
    	Text
            On a @TO2(isSimplicial, "simplicial")@ normal toric variety,
            every torus-invariant Weil divisor is $\QQ$-Cartier; every
            torus-invariant Weil divisor has a positive integer multiple that
            is Cartier.
    	Example  
    	    C = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    fromCDivToWDiv C
    	    prune cokernel fromCDivToWDiv C
    	    assert (rank cokernel fromCDivToWDiv C === 0)
    	Text
	    In general, the Cartier divisors are only a subgroup of the Weil
	    divisors.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    fromCDivToWDiv Q
	    prune coker fromCDivToWDiv Q
	    assert (rank coker fromCDivToWDiv Q === 1)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isComplete Y)
    	    fromCDivToWDiv Y
    	    prune cokernel fromCDivToWDiv Y
	    assert (rank coker fromCDivToWDiv Y === 4)	    
	Text
	    This map is computed and cached when the Cartier divisor group is
	    first constructed.
    SeeAlso
        "working with divisors"
        (weilDivisorGroup ,NormalToricVariety)
        (cartierDivisorGroup, NormalToricVariety)
        (isCartier, ToricDivisor)
///


doc ///
    Key 
    	(picardGroup, NormalToricVariety)
	picardGroup	
    Headline 
        make the Picard group
    Usage 
        picardGroup X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Module 
	    a finitely generated abelian group
    Description
        Text
	    The Picard group of a variety is the group of Cartier divisors
            divided by the subgroup of principal divisors.  For a normal toric
            variety, the Picard group has a presentation defined by the map
            from the group of torus-characters to the group of torus-invariant
            Cartier divisors.  For more information, see Theorem 4.2.1 in
            Cox-Little-Schenck's {\em Toric Varieties}.
    	Text
            When the normal toric variety is @TO2(isSmooth, "smooth")@, the
            Picard group is isomorphic to the 
	    @TO2(classGroup, "class group")@.
      	Example
    	    PP3 = toricProjectiveSpace 3;
	    assert (isSmooth PP3 and isProjective PP3)
    	    picardGroup PP3
    	    assert (picardGroup PP3 === classGroup PP3 and isFreeModule picardGroup PP3)
    	Example
	    X = smoothFanoToricVariety (4,90);
	    assert (isSmooth X and isProjective X and isFano X)
	    picardGroup X
	    assert (fromCDivToPic X === fromWDivToCl X and isFreeModule picardGroup X)	
   	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U and # max U =!= 1)
    	    picardGroup U
    	    assert (classGroup U	=== picardGroup U and not isFreeModule picardGroup U)
    	Text
            For an affine toric variety, the Picard group is trivial.
	Example
	    AA3 = affineSpace 3
	    assert (isSimplicial AA3 and isSmooth AA3 and # max AA3 === 1)
	    picardGroup AA3
	    assert (picardGroup AA3 == 0 and isFreeModule picardGroup AA3)
    	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q and # max Q === 1)
    	    picardGroup Q
	    assert (picardGroup Q == 0 and isFreeModule picardGroup Q)
	Text
	    If the fan associated to $X$ contains a cone of dimension
	    $dim(X)$, then the Picard group is free.
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    picardGroup Y
	    assert (rank picardGroup Y === 1 and isFreeModule picardGroup Y)
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.	    
    SeeAlso
        "working with divisors"
        (classGroup, NormalToricVariety)
        (cartierDivisorGroup, NormalToricVariety)
        (fromCDivToPic, NormalToricVariety)
        (fromPicToCl, NormalToricVariety)
	(picardGroup, ToricMap)
///


doc ///
    Key
        (fromPicToCl, NormalToricVariety)
        fromPicToCl	
    Headline 
        get the map from Picard group to class group
    Usage 
        fromPicToCl X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix 
	    representing the inclusion map from the Picard group to the class
	    group
    Description
        Text			
	    The @TO2(picardGroup, "Picard group")@ of a normal toric variety
            is a subgroup of the @TO2(classGroup, "class group")@.  This
            function returns a matrix representing this map with respect to
            the chosen bases.  
    	Text
            On a @TO2(isSmooth, "smooth")@ normal toric variety, the Picard
            group is isomorphic to the class group, so the inclusion map is
            the identity.
    	Example  
    	    PP3 = toricProjectiveSpace 3;
	    assert (isSmooth PP3 and isProjective PP3)
    	    fromPicToCl PP3
	    assert (fromPicToCl PP3 === id_(classGroup PP3))
    	Example
	    X = smoothFanoToricVariety (4,90);
	    assert (isSmooth X and isProjective X and isFano X)
    	    fromPicToCl X
	    assert (fromPicToCl X === id_(classGroup X))	    
   	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U and # max U =!= 1)
	    fromPicToCl U
	    assert (fromPicToCl U === id_(classGroup U))	   	    
    	Text
            For @TO2(weightedProjectiveSpace, "weighted projective space")@,
	    the inclusion corresponds to $l \ZZ$ in $\ZZ$ where 
	    $l = lcm(q_0, q_1, \dots, q_d {})$.
    	Example
    	    P123 = weightedProjectiveSpace {1,2,3};
	    assert (isSimplicial P123 and isProjective P123)
	    fromPicToCl P123
	    assert (fromPicToCl P123 === lcm (1,2,3) * id_(classGroup P123))	   	    
    	Example
    	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
	    assert (isSimplicial P12234 and isProjective P12234)
	    fromPicToCl P12234
	    assert (fromPicToCl P12234 === lcm (1,2,2,3,4) * id_(classGroup P12234))	
	Text
            The following examples illustrate some other possibilities.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q and # max Q === 1)
	    fromPicToCl Q
    	    assert (fromPicToCl Q == 0)
    	Example
   	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    fromPicToCl Y
	Text
	    This map is computed and cached when the Picard group is first
	    constructed.
    SeeAlso
        "working with divisors"
        (picardGroup, NormalToricVariety)
    	(classGroup, NormalToricVariety)
	(isSmooth, NormalToricVariety)
///		
	

doc ///
    Key 
    	(fromCDivToPic, NormalToricVariety)
        fromCDivToPic	
    Headline 
        get the map from Cartier divisors to the Picard group
    Usage 
        fromCDivToPic X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix
	    representing the surjective map from the group of torus-invariant
            Cartier divisors to the Picard group
    Description
        Text
	    The @TO2(picardGroup, "Picard group")@ of a variety is the
	    @TO2(cartierDivisorGroup, "group of Cartier divisors")@ divided by
	    the subgroup of principal divisors.  For a normal toric variety ,
	    the Picard group has a presentation defined by the map from the
	    group of torus-characters to the group of torus-invariant Cartier
	    divisors.  Hence, there is a surjective map from the group of
	    torus-invariant Cartier divisors to the Picard group.  This
	    function returns a matrix representing this map with respect to
	    the chosen bases.  For more information, see Theorem 4.2.1 in
	    Cox-Little-Schenck's {\em Toric Varieties}.
	Text
            On a @TO2(isSmooth, "smooth")@ normal toric variety, the map from
            the torus-invariant Cartier divisors to the Picard group is the
            same as the map from the @TO2(weilDivisorGroup, "Weil divisors")@
            to the @TO2(classGroup, "class group")@.
    	Example
    	    PP2 = toricProjectiveSpace 2;
	    assert (isSmooth PP2 and isProjective PP2)
    	    fromCDivToPic PP2
    	    assert (fromCDivToPic PP2 === fromWDivToCl PP2)
    	Example
	    X = smoothFanoToricVariety (4,20);
	    assert (isSmooth X and isProjective X and isFano X)
	    fromCDivToPic X
	    assert (fromCDivToPic X === fromWDivToCl X)
	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)
    	    fromCDivToPic U
	    assert (fromCDivToPic U === fromWDivToCl U)
    	Text	    
            In general, there is a commutative diagram relating the map from
            the group of torus-invariant Cartier divisors to the Picard group
            and the map from the group of torus-invariant Weil divisors to the
            class group.
      	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    fromCDivToPic Q
    	    assert (fromWDivToCl Q * fromCDivToWDiv Q == fromPicToCl Q * fromCDivToPic Q)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    fromCDivToPic Y
	    fromPicToCl Y
	    fromPicToCl Y * fromCDivToPic Y
	    fromCDivToWDiv Y
	    fromWDivToCl Y
    	    assert (fromWDivToCl Y * fromCDivToWDiv Y == fromPicToCl Y * fromCDivToPic Y)
	Text
	    This map is computed and cached when the Picard group is first
	    constructed.	    
    SeeAlso
        "working with divisors"
        (cartierDivisorGroup, NormalToricVariety)
        (picardGroup, NormalToricVariety)
        (fromWDivToCl, NormalToricVariety)
///	


doc ///
    Key
	(nefGenerators, NormalToricVariety)
        nefGenerators	
    Headline
        compute generators of the nef cone
    Usage
        nefGenerators X
    Inputs
        X : NormalToricVariety
    Outputs
        : Matrix
	    whose columns generate the nef cone of {\tt X} as a convex cone in
	    the Picard group
    Description
        Text
	    The nef cone of a variety is the cone generated by classes of nef
	    Cartier divisors in vector space of Cartier divisors modulo
	    numerical equivalence.  On a normal toric variety, numerical
	    equivalence and linear equivalence coincide, so the nef cone lies
	    in the Picard group.  Assume that the normal toric variety is
	    non-degenerate, its nef cone is a rational polyhedral cone in the
	    Picard group; see Theorem 6.3.20 in Cox-Little-Schenck's {\em
	    Toric Varieties}.  This function calculates generators for the
	    rays of this cone, and returns a matrix whose columns correspond
	    to these generates (expressed as vectors in the chosen basis for
	    the Picard group).
	Text
	    For some of our favourite normal toric varieties, we choose a
	    basis for the Picard group which makes the nef cone into the
	    positive orthant.
	Example
	    nefGenerators toricProjectiveSpace 1
	    nefGenerators toricProjectiveSpace 3	    
	    nefGenerators normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3))
	    nefGenerators hirzebruchSurface 7
    	    nefGenerators kleinschmidt (3,{0,1})	    
	    nefGenerators smoothFanoToricVariety (2,3)
	    nefGenerators smoothFanoToricVariety (3,12)
	    nefGenerators smoothFanoToricVariety (4,90)
	Text
	    In general, the nef cone need not even be 
	    @TO2(isSimplicial, "simplicial")@.
	Example
	    nefGenerators smoothFanoToricVariety (2,4)	    
	    nefGenerators smoothFanoToricVariety (3,16)	    	    
	    nefGenerators smoothFanoToricVariety (4,120)
	Text
	    There are @TO2(isSmooth, "smooth")@ @TO2(isComplete, "complete")@
            normal toric varieties with no nontrivial nef divisors.
    	Example    
            X = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
	    assert (isComplete X and not isProjective X and isSmooth X)
	    picardGroup X
	    assert (nefGenerators X == 0)
    SeeAlso
        "working with divisors"
	(isNef, ToricDivisor)
///	        


------------------------------------------------------------------------------
-- toric divisors
------------------------------------------------------------------------------
doc ///
    Key
        ToricDivisor
    Headline 
        the class of all torus-invariant Weil divisors
    Description
        Text
            A torus-invariant Weil divisor on a normal toric variety is an
            integral linear combination of the irreducible torus-invariant
            divisors.  The irreducible torus-invariant divisors correspond to
            the rays.  In this package, the rays are ordered and indexed by
            the nonnegative integers.
    	Text  
            The first examples illustrates some torus-invariant Weil divisors
            on projective $2$-space.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
            D1 = toricDivisor ({2,-7,3}, PP2) 
            D2 = 2*PP2_0 + 4*PP2_2
            D1+D2
            D1-D2
            K = toricDivisor PP2  
	Text
            One can easily extract individual coefficients or the list of
            coefficients.
    	Example  
    	    D1#0
            D1#1
            D1#2
            entries D1
            entries K
    SeeAlso
        "working with divisors"
        (weilDivisorGroup, NormalToricVariety)
        (toricDivisor, List, NormalToricVariety)
        (toricDivisor, NormalToricVariety)
        (symbol _, NormalToricVariety, ZZ)
        (expression, ToricDivisor)
        (normalToricVariety, ToricDivisor)
        (support, ToricDivisor)
        (symbol +, ToricDivisor, ToricDivisor)
///


doc ///
    Key
        (expression, ToricDivisor)
    Headline 
        get the expression used to format for printing
    Usage 
        expression D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Expression
	    used to format {\tt D} for printing
    Description
        Text	    
            This method is the primary function called upon by @TO(symbol <<)@
            to format for printing.  It assumes that {\tt D} is well-defined.
      	Text
	    When the underlying normal toric variety has not been assigned a
            global variable, the $i$-th irreducible torus-invariant Weil
            divisor is displayed as $D_i$.  However, if the underlying normal
            toric variety has been assigned a global variable $X$, the $i$-th
            irreducible torus-invariant Weil divisor is displayed as $X_i$.
            In either case, an arbitrary torus-invariant Weil divisor is
            displayed as an integral linear combination of these
            expressions.
    	Example
	    toricDivisor({2,-7,3}, toricProjectiveSpace 2)  
    	Example	    
	    toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
    	Example	    
	    PP2 = toricProjectiveSpace 2;
	    D1 = toricDivisor({2,-7,3}, PP2)  
	    D2 = 2 * PP2_0 - 7 * PP2_1 + 3 * PP2_2
	    assert(D1 == D1)
    SeeAlso
        "working with divisors"
	(isWellDefined, ToricDivisor)
        (symbol _, NormalToricVariety, ZZ)
///

undocumented { (net,ToricDivisor) }


doc ///
    Key 
        (variety, ToricDivisor)
        (normalToricVariety, ToricDivisor)
    Headline 
        get the underlying normal toric variety
    Usage 
        variety D 
	normalToricVariety D
    Inputs 
        D : ToricDivisor
    Outputs 
        : NormalToricVariety
	    namely the underlying variety for {\tt D}
    Description
        Text	    
            This function allows one to easily access the normal toric variety
            over which the torus-invariant Weil divisor is defined.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
    	    variety D1
    	    normalToricVariety D1
	    assert(variety D1 === PP2 and normalToricVariety D1 === PP2)
    	Example
    	    X = normalToricVariety(id_(ZZ^3) | - id_(ZZ^3));
    	    D2 = X_0 - 5 * X_3
	    variety D2
    	    assert(X === variety D2 and X === normalToricVariety D2)
	Text
	    Since the underlying normal toric variety is a defining attribute
	    of a toric divisor, this method does not computation.
    SeeAlso
        "working with divisors"
	NormalToricVariety
///	


doc ///
    Key
        (entries, ToricDivisor)
    Headline
        get the list of coefficients 
    Usage
        entries D
    Inputs
        D : ToricDivisor
    Outputs
        : List
	    of @TO2 (ZZ, "integers")@ that are the coefficients of the corresponding
	    irreducible torus-invariant divisors
    Description
        Text	   
            This function returns the @TO List@ whose $i$-th entry is the
            coefficient of $i$-th irreducible torus-invariant divisor.  The
            indexing of the irreducible torus-invariant divisors is inherited
            from the indexing of the rays in the associated fan.  This list
            can be viewed as an element of the group of torus-invariant Weil
            divisors.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
	    entries D1
	    assert( D1 == toricDivisor(entries D1, variety D1) )
	    assert all(entries toricDivisor PP2, i -> i === -1)
	Example
	    D2 = toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
	    entries D2
    	    assert all(entries D2, i -> i === 1)
    SeeAlso
        "working with divisors"
	(rays, NormalToricVariety)		
	(toricDivisor, List, NormalToricVariety)
	(toricDivisor, Polyhedron)
///


doc ///
    Key 
        (vector, ToricDivisor)
    Headline 
        make the vector of coefficients
    Usage 
        vector D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Vector 
	    whose entries are the coefficients of {\tt D}
    Description
        Text	  
            This function returns the @TO Vector@ whose $i$-th entry is the
            coefficient of $i$-th irreducible torus-invariant divisor.  The
            indexing of the irreducible torus-invariant divisors is inherited
            from the indexing of the rays in the associated fan.  This list
            can be viewed as an element of the group of torus-invariant Weil
            divisors.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
	    vector D1
	    assert(entries vector D1 === entries D1)
	Example
	    D2 = toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
	    vector D2
	    assert(entries vector D2 === entries D2)
    SeeAlso
        "working with divisors"
	ToricDivisor
	(entries, ToricDivisor)
	(rays, NormalToricVariety)
///


doc ///     	    
    Key 
        (support, ToricDivisor)
    Headline 
        make the list of irreducible divisors with nonzero coefficients
    Usage 
        support D
    Inputs 
        D : ToricDivisor
    Outputs 
        : List 
	    indexing the irreducible torus-invariant divisors whose
            coefficient in {\tt D} are nonzero
    Description
        Text			
            The support of a torus-invariant Weil divisor is the set of
            irreducible torus-invariant divisors which appear with nonzero
            coefficients in the unique expression for this divisor.  In this
            package, we encode this information by indexing the irreducible
            torus-invariantdivisors that appear with a nonzero coefficient.
            The indexing of the irreducible torus-invariant divisors is
            inherited from the indexing of the rays in the associated fan.
    	Example	    
    	    PP2 = toricProjectiveSpace 2;
            D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2    
    	    support D1
    	    D2 = PP2_0-5*PP2_2
    	    support D2
    	    support (6*PP2_1)
    SeeAlso
        "working with divisors"
        (rays,NormalToricVariety)
	(entries, ToricDivisor)
///


doc ///
    Key
        (degree, ToricDivisor)
    Headline
        make the degree of the associated rank-one reflexive sheaf
    Usage
        degree D
    Inputs
        D : ToricDivisor
    Outputs
        : List	
	    of @TO2 (ZZ, "integers")@ corresponding to the degree of the
	    rank-one reflexive sheaf
    Description
        Text	   
            This function returns the @TO List@ representing an element of the
	    Picard group corresponding to the associated rank-one reflexive
	    sheaf.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = PP2_0
	    degree D1
	    OO D1
	    D2 = 3*PP2_1
	    degree D2
	    OO D2
	Example
	    FF2 = hirzebruchSurface 2;
	    D3 = -1*FF2_2 + 3*FF2_3
	    degree D3
    	    OO D3	    
    SeeAlso
        "working with divisors"
	(symbol SPACE, OO, ToricDivisor)
	(picardGroup, NormalToricVariety)
///


doc ///
    Key
        (monomials, ToricDivisor)
    Headline
       list the monomials that span the linear series
    Usage
        monomials D
    Inputs
        D : ToricDivisor
    Outputs
        : List
	    of torus-invariant @TO2 (RingElement, "RingElements")@ that appear
	    in the complete linear system of the given divisor
    Description
        Text
	    By identifying the coefficients of an effective irreducible
	    torus-invariant divisors with exponents of the generators of the
	    @TO2( (ring, NormalToricVariety), "total coordinate ring")@, each
	    toric divisor on a @TO NormalToricVariety@ corresponds to a
	    monomial.  This method function returns all of the monomials
	    corresponding to linear equivalent toric divisors.
	Text
	    This method function assumes that the underlying toric variety is
	    projective.	    
	Text
	    Projective space is especially simple.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 5*PP2_0
	    M1 = elapsedTime monomials D1
	    elapsedTime assert (set M1 === set first entries basis(degree D1, ring variety D1))
	Text
	    Toric varieties of Picard-rank 2 are slightly more interesting.
	Example
	    FF2 = hirzebruchSurface 2;
	    D2 = 2*FF2_0 + 3 * FF2_1
	    M2 = elapsedTime monomials D2
	    elapsedTime assert (set M2 === set first entries basis (degree D2, ring variety D2))	
	    X = kleinschmidt (5, {1,2,3});     
	    D3 = 3*X_0 + 5*X_1
	    m3 = elapsedTime # monomials D3
	    elapsedTime assert (m3 === #first entries basis (degree D3, ring variety D3))
	Text
	    By exploiting @TO "Polyhedra::latticePoints"@, this method function
	    avoids using the @TO basis@ function.	    
    SeeAlso
        "working with divisors"
	(ring, NormalToricVariety)    
	(vector, ToricDivisor)
///


doc ///
    Key
        (toricDivisor, List, NormalToricVariety)
        toricDivisor	
    Headline 
        make a torus-invariant Weil divisor
    Usage 
        toricDivisor(L, X)
    Inputs
        L : List
   	    of integers coefficients for the irreducible torus-invariant
	    divisors on {\tt X}
	X : NormalToricVariety
    Outputs 
        : ToricDivisor
    Description
        Text
            Given a list of integers and a normal toric variety, this method
            returns the torus-invariant Weil divisor such the coefficient of
            the $i$-th irreducible torus-invariant divisor is the $i$-th entry
            in the list.  The indexing of the irreducible torus-invariant
            divisors is inherited from the indexing of the rays in the
            associated fan.  In this package, the rays are ordered and indexed
            by the nonnegative integers.
	Example
            PP2 = toricProjectiveSpace 2;
    	    D = toricDivisor({2,-7,3},PP2)
            assert(D == 2* PP2_0 - 7*PP2_1 + 3*PP2_2)
            assert(D == toricDivisor(entries D, variety D))
    	Text
            Although this is a general method for making a torus-invariant
            Weil divisor, it is typically more convenient to simple enter the
            appropriate linear combination of torus-invariant Weil divisors.
    SeeAlso
        "working with divisors"
        (toricDivisor, NormalToricVariety)
        (symbol _, NormalToricVariety, ZZ)
///	


doc ///
    Key 
        (toricDivisor, NormalToricVariety)
    Headline
        make the canonical divisor
    Usage 
        toricDivisor X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : ToricDivisor
	    specifically minus the sum of all the irreducible torus-invariant
            divisors
    Description
        Text			       
            On a smooth normal toric variety, the canonical divisor equals
            minus the sum of all the torus-invariant irreducible divisors.
            For a singular toric variety, this divisor may not be Cartier or
            even $\QQ$-Cartier.  Nevertheless, the associated coherent sheaf,
            whose local sections are rational functions with at least simple
            zeros along the irreducible divisors, is the dualizing sheaf.
	Text
	    The first example illustrates the canonical divisor on projective
	    space.
    	Example	    
    	    PP3 = toricProjectiveSpace 3;
	    assert(isSmooth PP3 and isProjective PP3)
    	    K = toricDivisor PP3
	    assert(all(entries K, i -> i === -1) and isWellDefined K)
    	    omega = OO K
    	    assert(HH^3(PP3, OO_PP3(-7) ** omega) === HH^0(PP3, OO_PP3(7)))
	Text
            The second example illustrates that duality also holds on complete
            singular nonprojective toric varieties.
	Example 
            X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert(isComplete X and not isProjective X and not isSmooth X)	
    	    KX = toricDivisor X	
	    assert(all(entries KX, i -> i === -1) and isWellDefined KX)
	    isCartier KX
	    omegaX = OO KX
	    assert( HH^0(X, OO_X(1,2,5)) === HH^3(X, OO_X(-1,-2,-5) ** omegaX) )
    SeeAlso
        "working with divisors"
    	toricDivisor
    	(symbol SPACE, OO, ToricDivisor)
    	(cohomology, ZZ, NormalToricVariety, CoherentSheaf)
///	


doc ///
    Key
        (toricDivisor, Polyhedron)
	[toricDivisor, CoefficientRing]
	[toricDivisor, Variable]
	[toricDivisor, WeilToClass]
    Headline
        make the toric divisor associated to a polyhedron
    Usage
        toricDivisor P
    Inputs
        P:Polyhedron
	    whose vertices are lattice points
        CoefficientRing => Ring
	    that determines the coefficient ring of the total coordinate ring
	    of the underlying normal toric variety
        Variable => Symbol
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring of the underlying normal toric variety	    
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group of the underlying normal toric variety
    Outputs
       :ToricDivisor
           corresponding to the given convex lattice polytope on the normal
	   toric variety defined by the inner normal fan
    Description
        Text
	    A convex lattice polytope corresponds to a pair: the normal toric
	    variety determined by its normal fan and toric divisor.  The
	    coefficient of the $i$-th irreducible torus-invariant divisor is
	    determined by the supporting hyperplane to the polytope whose
	    normal vector is the minimal lattice point on the $i$-th ray.
	Text
	   Our example demonstrates how different triangles correspond
	   to toric divisors on the projective plane.
    	Example
	    P1 = convexHull matrix{{0,1,0},{0,0,1}};
	    D1 = toricDivisor P1
	    X = variety D1;
	    D1
	    P2 = convexHull matrix{{-1,0,-1},{0,0,1}};
	    D2 = toricDivisor P2
	    P3 = convexHull matrix{{0,1,0},{-1,-1,0}};
	    D3 = toricDivisor P3
	    P4 = convexHull matrix{{-1,2,-1},{-1,-1,2}};
	    D4 = toricDivisor(P4, CoefficientRing => ZZ/2)
	    ring variety D4
	Text
	    This method function creates both the toric divisor and the
	    underlying normal toric variety.
    SeeAlso
        "working with divisors"
	(normalToricVariety, Polyhedron)
///	   


doc ///
    Key 
        (isWellDefined, ToricDivisor)
    Headline 
        whether a toric divisor is well-defined
    Usage
        isWellDefined D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if underlying data structure is correctly
	    formatted
    Description
        Text			  
            This function checks that the following aspects of the data
            structure:
        Text	    
            @UL {
		{"the underlying ", TO HashTable, " has the expected keys,
	          namely the integers from ", TT "0", " to ", TT "n-1", "
	          where ", TT "n = # rays variety D", ", ", TT "variety", ",
	          and ", TT "cache", ","},
	        {"the value of each integer key is an ", TO ZZ, ","},
		{"the value of the ", TT "variety", " key is a ", 
	    	  TO NormalToricVariety, ","},
	        {"the value of the ", TT "cache", " key is a ", 
		  TO CacheTable, "."}
	    }@
        Example
      	    PP2 = toricProjectiveSpace 2
	    D1 = toricDivisor ({2,-7,3}, PP2)
	    assert isWellDefined D1
        Example	    
    	    debugLevel = 1;
    	    D2 = new ToricDivisor from hashTable { 0 => 2, symbol variety => PP2, symbol cache => new CacheTable};
	    assert not isWellDefined D2
	    D3 = new ToricDivisor from hashTable { 0 => 2, 1 => x, 2 => 3, symbol variety => PP2, symbol cache => new CacheTable};
    	    assert not isWellDefined D3	
	    D4 = new ToricDivisor from hashTable { 0 => 2, 1 => -7, 2 => 3, symbol variety => 7, symbol cache => new CacheTable};	
	    assert not isWellDefined D4	
        Text	
            The function @TO(expression, ToricDivisor)@ assumes that the input
            toric divisor is well-defined.
    SeeAlso
	"working with divisors"
        (toricDivisor, List, NormalToricVariety)
	"debugLevel"
///	   	

	   
doc ///
    Key 
        (symbol _, NormalToricVariety, ZZ)
    Headline 
        make an irreducible torus-invariant divisor
    Usage 
        X_i
    Inputs
        X : NormalToricVariety
        i : ZZ
	    indexing a ray in the fan associated to {\tt X}
    Outputs 
        : ToricDivisor 
	    namely the irreducible torus-invariant divisor associated to the
	    {\tt i}-th ray in the fan of {\tt X}
    Description
        Text	    
            The irreducible torus-invariant divisors on a normal toric variety
            correspond to the rays in the associated fan.  In this package,
            the rays are ordered and indexed by the nonnegative integers.
            Given a normal toric variety and nonnegative integer, this method
            returns the corresponding irreducible torus-invariant divisor.
            The most convenient way to make a general torus-invariant Weil
            divisor is to simply write the appropriate linear combination of
            these torus-invariant Weil divisors.
	Text
	    There are three irreducible torus-invariant divisors on the
	    projective plane.
    	Example	    
    	    PP2 = toricProjectiveSpace 2;
    	    PP2_0
    	    PP2_1
    	    PP2_2
    	    assert (- PP2_0 - PP2_1 - PP2_2 === toricDivisor PP2)
    	Text
	    A torus-invariant Weil divisor is irreducible if and only if its
            support has a single element.
    	Example  
     	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    X_0
    	    support X_0
	    assert( # support X_0 === 1)
    	    K = toricDivisor X	    
    	    support K
    SeeAlso
        "working with divisors"
	(support, ToricDivisor)
    	(variety, ToricDivisor)
///


------------------------------------------------------------------------------
-- Database of some ample divisors
------------------------------------------------------------------------------
doc /// 
    Key 
        (smallAmpleToricDivisor, ZZ, ZZ)    
        smallAmpleToricDivisor	    
    	[smallAmpleToricDivisor, CoefficientRing]
    	[smallAmpleToricDivisor, Variable]
	[smallAmpleToricDivisor, WeilToClass]
    Headline 
        get a very ample toric divisor from the database
    Usage 
        smallAmpleToricDivisor (d, i)
    Inputs
        d : ZZ 
	    equal to dimension of the underlying toric variety
        i : ZZ 
	    indexing the toric divisor in the database
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group of the underlying normal toric variety
    Outputs
       : ToricDivisor
           representing a very ample divisor that embeds the underlying smooth
           toric variety into a low-dimensional projective space
    Description
        Text
            This method function accesses a database of equivalence classes of very
            ample divisors that embed their underlying smooth toric varieties
            into low-dimensional projective spaces.  
	Text
	    The enumeration of the $41$ smooth projective toric surfaces
            embedding into at most projective $11$-space follows the
            classification in [Tristram Bogart, Christian Haase, Milena Hering,
            Benjamin Lorenz, Benjamin Nill Andreas Paffenholz, Günter Rote,
            Francisco Santos, and Hal Schenck,
            @HREF("https://dx.doi.org/10.1007/s11856-015-1175-7", "Finitely
            many smooth d-polytopes with n lattice points")@, {\em Israel
            J. Math.}, {\bf 207} (2015) 301-329].
	Text
	    The enumeration of the $103$ smooth projective toric threefolds
	    embedding into at most projective $15$-space follows [Anders
	    Lundman,
	    @HREF("https://dx.doi.org/10.1007/10.1007/s10801-012-0363-3", "A
	    classification of smooth convex 3-polytopes with at most 16
	    lattice points")@, {\em J. Algebr. Comb.}, {\bf 37} (2013) 139-165].
	Text
    	    The first $2$ toric divisors over a surface lie over a product of
    	    projective lines.
      	Example
	    D1 = smallAmpleToricDivisor(2,0)
	    assert isVeryAmple D1
	    X1 = variety D1;
	    assert (isSmooth X1 and isProjective X1)
	    rays X1
	    D1
	    latticePoints D1
	    D2 = smallAmpleToricDivisor (2,1);
	    assert isVeryAmple D2
	    X2 = variety D2;
	    assert (isSmooth X2 and isProjective X2)
	    rays X2
	    D2
	    latticePoints D2
	Text
	    The $15$-th toric divisors on a surface lies over normal toric
	    varieties with $8$ irreducible torus-invariant divisors.
	Example
	    D3 = smallAmpleToricDivisor (2,15);
	    assert isVeryAmple D3
	    X3 = variety D3;
	    assert (isSmooth X3 and isProjective X3)
	    rays X3
	    D3
	    latticePoints D3
	Text
	    Last, $25$ toric divisors on a surface lies over Hirzebruch
	    surfaces.
	Example
	    D4 = smallAmpleToricDivisor (2,30);
	    assert isVeryAmple D4
	    X4 = variety D4;
	    assert (isSmooth X4 and isProjective X4)
	    rays X4
	    D4
	    latticePoints D4
	Text  
	    The first $99$ toric divisors on a threefold embed a projective
	    bundle into projective space.
	Example
	    D5 = smallAmpleToricDivisor(3,75);
	    assert isVeryAmple D5
	    X5 = variety D5;
	    assert (isSmooth X5 and isProjective X5)
	    assert (# rays X5 === 8)
	    D5
	    latticePoints D5	
	Text
	    The last $4$ toric divisors on a threefold embed a blow-up of a
	    projective bundle at few points into projective space.
	Example
	    D6 = smallAmpleToricDivisor (3,102);
	    assert(isVeryAmple D6)
	    X6 = variety D6;
	    assert (isSmooth X6 and isProjective X6)
	    assert (# rays X6 === 7)
	    D6
	    latticePoints D6	    
	Text
	    @SUBSECTION "Acknowledgements"@
    	Text
            We thank @HREF("http://www.maths.ed.ac.uk/~mhering/", "Milena
            Hering")@ for her help creating the database.
    SeeAlso
        "working with divisors"
	(toricDivisor, List, NormalToricVariety)
    	(variety, ToricDivisor)
///	



------------------------------------------------------------------------------
-- Arithmetic of toric divisors
------------------------------------------------------------------------------
doc ///
    Key
        (symbol ==, ToricDivisor, ToricDivisor)
	(symbol ==, ToricDivisor, ZZ)
	(symbol ==, ZZ, ToricDivisor)
    Headline
        equality of toric divisors
    Usage
        D1 == D2
    Inputs
        D1 : ToricDivisor
	D2 : ToricDivisor
    Outputs
        : Boolean
	    that is @TO true@ if the underlying varieties are equal and lists
	    of coefficients are equal
    Description
        Text
	    Two torus-invariant Weil divisors are equal when their underlying
	    normal toric varieties are equal and, for each irreducible
	    torus-invariant divisor, the corresponding coefficients are equal.
	Example
	    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
	    D1 = toricDivisor({2,-7,3,0,7,5,8,-8}, X)
	    D2 = 2 * X_0 - 7 * X_1 + 3 * X_2 + 7 * X_4 + 5 * X_5 + 8 * X_6 - 8 * X_7
	    D1 == D2
	    D1 == - D2
	    assert (D1 == D2 and D2 == D1 and D1 =!= - D2)
	Text
	    Since the group of torus-equivariant Weil divisors form an abelian
	    group, it also makes sense to compare a toric divisor with the
	    zero integer (which we identify with the toric divisor whose
	    coefficients are equal to zero).
	Example
	    D1 == 0
    	    0*D1 == 0	
	    assert (D1 =!= 0 and 0*D1 == 0 and 0 == 0*D2)
    SeeAlso
        "working with divisors"
	(symbol +, ToricDivisor, ToricDivisor)    
///


doc ///
    Key 
    	"divisor arithmetic"
        (symbol +, ToricDivisor, ToricDivisor)
    	(symbol -, ToricDivisor, ToricDivisor)
    	(symbol -, ToricDivisor)
    	(symbol *, ZZ, ToricDivisor)
    Headline 
        perform arithmetic on toric divisors
    Usage 
        D1 + D2
	D1 - D2
	- D2
	m * D1
    Inputs 
        D1 : ToricDivisor
        D2 : ToricDivisor
	m : ZZ
    Outputs 
        : ToricDivisor
	    that is obtained via the specific operation
    Description
        Text	    
            The set of torus-invariant Weil divisors forms an abelian group
            under addition.  The basic operations arising from this structure,
            including addition, subtraction, negation, and scalar
            multiplication by integers, are available.
	Text
	    We illustrate a few of the possibilities on one variety.
      	Example
    	    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    	    # rays X
    	    D = toricDivisor({2,-7,3,0,7,5,8,-8}, X)
    	    K = toricDivisor X
    	    D + K
	    assert(D + K == K + D)
    	    D - K
    	    assert(D - K == -(K-D))
    	    - K
    	    assert(-K == (-1)*K)
    	    7*D
    	    assert(7*D == (3+4)*D)
	    assert(7*D == 3*D + 4*D)
    	    -3*D + 7*K    
    	    assert(-3*D+7*K == (-2*D+8*K) + (-D-K))
    SeeAlso
        "working with divisors"
	(symbol ==, ToricDivisor, ToricDivisor)
///

	
------------------------------------------------------------------------------
-- Line bundles
------------------------------------------------------------------------------
doc ///
    Key 
        (symbol SPACE, OO, ToricDivisor)
    Headline 
        make the associated rank-one reflexive sheaf
    Usage 
        OO D
    Inputs 
        D : ToricDivisor
    Outputs 
        : CoherentSheaf 
	    the associated rank-one reflexive sheaf
    Description
        Text
            For a Weil divisor $D$ on a normal variety $X$ the associated
  	    sheaf ${\cal O}_X(D)$ is defined by 
	    $H^0(U, {\cal O}_X(D)) = \{ f \in {\mathbb C}(X)^* | (div(f)+D)|_U \geq 0 \} \cup \{0\}$.
            The sheaf associated to a Weil divisor is reflexive; it is equal
  	    to its bidual.  A divisor is Cartier if and only if the associated
  	    sheaf is a line bundle
	Text
            The first examples show that the associated sheaves are reflexive.
	Example
            PP3 = toricProjectiveSpace 3;
            K = toricDivisor PP3
            omega = OO K
            omegaVee = prune sheafHom (omega, OO_PP3)
            omega === prune sheafHom (omegaVee, OO_PP3)
        Example
            X = hirzebruchSurface 2;
            D = X_0 + X_1
            L = OO D
            LVee = prune sheafHom (L, OO_X)
            L === prune sheafHom (LVee, OO_X)
	Example
            rayList = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
            coneList = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
            Y = normalToricVariety(rayList,coneList);
    	    isSmooth Y
    	    isProjective Y
    	    E = Y_0 + Y_2 + Y_4
    	    isCartier E
    	    F = OO E
    	    FVee = prune sheafHom(F, OO_Y)
    	    F === prune sheafHom(FVee, OO_Y)
        Text
            Two Weil divisors $D$ and $E$ are linearly equivalent if 
	    $D = E + div(f)$, for some $f \in {\mathbb C}(X)^*$. Linearly
            equivalent divisors produce isomorphic sheaves.
	Example
       	    PP3 = toricProjectiveSpace 3;
    	    D1 = PP3_0
    	    E1 = PP3_1
    	    OO D1 === OO E1
    	    X = hirzebruchSurface 2;
    	    D2 = X_2 + X_3    
    	    E2 = 3*X_0 + X_1
    	    OO D2 === OO E2
    SeeAlso 
        "working with divisors"
        "working with sheaves"
///


------------------------------------------------------------------------------
-- Properties of toric divisors
------------------------------------------------------------------------------
doc ///
    Key 
	(isEffective, ToricDivisor)
        isEffective 	
    Headline 
        whether a torus-invariant Weil divisor is effective
    Usage 
        isEffective D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if all the coefficients of the irreducible
            torus-invariant divisors are nonnegative
    Description
        Text    			  
            A torus-invariant Weil divisor is effective if all the
            coefficients of the torus-invariant irreducible divisors are
            nonnegative.
        Text
	    The canonical divisor is not effective, but the anticanonical
	    divisor is.
        Example	    
      	    PP3 = toricProjectiveSpace 3;
    	    K = toricDivisor PP3
    	    isEffective K
    	    isEffective (-K)
        Text
            The torus-invariant irreducible divisors generate the cone of
            effective divisors.
    SeeAlso
        "working with divisors"
	(symbol _, NormalToricVariety, ZZ)
///  


doc ///
    Key 
	(isCartier, ToricDivisor)
        isCartier 	
    Headline 
        whether a torus-invariant Weil divisor is Cartier
    Usage 
        isCartier D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is Cartier
    Description
        Text
            A torus-invariant Weil divisor $D$ on a normal toric variety $X$
            is Cartier if it is locally principal, meaning that $X$ has an
            open cover $\{U_i\}$ such that $D|_{U_i}$ is principal in $U_i$
            for every $i$.
        Text  
            On a smooth variety, every Weil divisor is Cartier.
	Example
      	    PP3 = toricProjectiveSpace 3;
    	    assert all (3, i -> isCartier PP3_i)
        Text
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier, which means that every torus-invariant Weil
            divisor has a positive integer multiple that is Cartier.
        Example
            W = weightedProjectiveSpace {2,5,7};
    	    assert isSimplicial W
    	    assert not isCartier W_0    
    	    assert isQQCartier W_0
    	    assert isCartier (35*W_0)      
        Text
  	    In general, the Cartier divisors are only a subgroup of the Weil
            divisors.
        Example  
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isCartier X_0
    	    assert not isQQCartier X_0
    	    K = toricDivisor X;
    	    assert isCartier K
    SeeAlso
        "working with divisors"
        (cartierDivisorGroup,NormalToricVariety)
        (isSimplicial,NormalToricVariety)
    	(isQQCartier,ToricDivisor)
///	


doc ///
    Key 
	(isQQCartier,ToricDivisor)
        isQQCartier 	
    Headline 
        whether a torus-invariant Weil divisor is QQ-Cartier
    Usage 
        isCartier D
    Inputs 
        D : ToricDivisor
    Outputs 
       : Boolean 
           that is @TO true@ if the divisor is $\QQ$-Cartier
    Description
        Text
            A Weil divisor is $\QQ$-Cartier if some positive integer multiple
            is Cartier.
        Text	    
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier.
        Example  
            W = weightedProjectiveSpace {2,5,7};
    	    assert isSimplicial W
            assert not isCartier W_0    
            assert isQQCartier W_0
            assert isCartier (35*W_0)    
        Text	    	      
            In general, the $\QQ$-Cartier divisors form a proper subgroup of
            the Weil divisors.
        Example 	    
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isCartier X_0
            assert not isQQCartier X_0
            K = toricDivisor X;
    	    assert isCartier K
    SeeAlso
        "working with divisors"
        (cartierDivisorGroup,NormalToricVariety)
        (isSimplicial,NormalToricVariety)
        (isCartier,ToricDivisor)
///	


doc ///
    Key 
        (isNef, ToricDivisor)
        isNef
    Headline 
        whether a torus-invariant Weil divisor is nef
    Usage 
        isNef D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is nef
    Description
        Text	    
            A $\QQ$-Cartier divisor is nef (short for numerically
            effective or numerically eventually free) if the
            intersection product of the divisor with every complete
            irreducible curve is nonnegative.  The definition depends only on
            the numerical equivalence class of the divisor.  For a
            torus-invariant $\QQ$-Cartier divisor on a complete normal
            toric variety, the following are equivalent:
        Text    
            @UL { 
        	{"the divisor is nef;"},
		{"some positive integer multiply of the divisor is Cartier and
		  basepoint free;"},
		{"the real piecewise linear support function associated to 
		  the divisor is convex."}
	    }@
        Text
            A torus-invariant Cartier divisor is nef if and only if it is
            basepoint free; in other words, the associated line bundle is
            generated by its global sections.
        Text		    
            On a Hirzebruch surface, three of the four torus-invariant
    	    irreducible divisors are nef, and none are ample.
        Example
            X1 = hirzebruchSurface 2;
            assert (isNef X1_0 and not isAmple X1_0)
            assert not isNef X1_1
            assert (isNef X1_2 and not isAmple X1_2)
            assert (isNef X1_3 and not isAmple X1_3)
        Text
            Not every $\QQ$-Cartier nef divisor is basepoint free.
        Example	    
            X2 = weightedProjectiveSpace {2,3,5};
            D = X2_1 - X2_0
            assert (isNef D and HH^0(X2, OO D) == 0)
    	    assert all (dim X2, i -> HH^i(X2, OO D) == 0)
    	    assert not isCartier D    
            assert isCartier (30*D)
            HH^0 (X2, OO (30*D))
            assert all (dim X2 -1, i -> HH^(i+1)(X2, OO (30*D)) == 0)
        Text	
            There are smooth complete normal toric varieties with no
    	    nontrivial nef divisors.
        Example	     
            X3 = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
    	    assert (isComplete X3 and not isProjective X3)
            assert not any (#rays X3, i -> isNef X3_i)
            assert isNef (0*X3_1)    
	    assert (nefGenerators X3 == 0)
        Text
            The most basic vanishing theorem for normal toric varieties states
            that the higher cohomology of coherent sheaf associated to a nef
            divisor is zero.
        Example	       
            X4 = kleinschmidt (9,{1,2,3});
            assert (isNef X4_0 and not isAmple X4_0)
	    assert all (dim X4 - 1, i -> HH^(i+1)(X4, OO X4_0) == 0)
    	    D = X4_0 + X4_4
	    assert (isNef D and isAmple D)
    	    assert all (dim X4 - 1, i -> HH^(i+1)(X4, OO D) == 0)
    SeeAlso
        "working with divisors"
	(nefGenerators, NormalToricVariety)
        (isComplete, NormalToricVariety)
        (kleinschmidt, ZZ, List)
        (isQQCartier, ToricDivisor)
        (isAmple, ToricDivisor)
///	


doc ///
    Key 
	(isAmple, ToricDivisor)
        isAmple	
    Headline 
        whether a torus-invariant Weil divisor is ample
    Usage 
        isAmple D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is ample
    Description
        Text		    
            A Cartier divisor is very ample when it is basepoint free and the
            map arising from its complete linear series is a closed embedding.
            A Cartier divisor is ample when some positive integer multiple is
            very ample.  For a torus-invariant Cartier divisor on a complete
            normal toric variety, the following conditions are equivalent:
        Text	  
	    @UL {
		{"the divisor is ample;"},
		{"the real piecewise linear support function associated to
    		  the divisor is strictly convex;"},
    		{"the lattice polytope corresponding to the divisor is
    		  full-dimensional and its normal fan equals the fan
    	          associated to the underlying toric variety;"},
                {"the intersection product of the divisor with every
      		  torus-invariant irreducible curve is positive."}
      	    }@
        Text      
            On projective space, every torus-invariant irreducible divisor is
            ample.
        Example	   
            PP3 = toricProjectiveSpace 3;
    	    assert all (# rays PP3, i -> isAmple PP3_i)
        Text  
            On a Hirzebruch surface, none of the torus-invariant irreducible
   	    divisors are ample.
        Example	  
            X1 = hirzebruchSurface 2;
    	    assert not any (# rays X1, i -> isAmple X1_i)
    	    D = X1_2 + X1_3
    	    assert isAmple D  
    	    assert isProjective X1  
        Text 	       
	    A normal toric variety is Fano if and only if its anticanonical
  	    divisors, namely minus the sum of its torus-invariant irreducible
  	    divisors, is ample.
        Example	    
    	    X2 = smoothFanoToricVariety (3,5);
    	    K = toricDivisor X2
    	    assert isAmple (- K)
    	    X3 = kleinschmidt (9,{1,2,3});
    	    K = toricDivisor X3
    	    assert isAmple (-K)  
    SeeAlso 
        "working with divisors"
	(isVeryAmple, ToricDivisor)
    	(isProjective, NormalToricVariety)
    	(isNef, ToricDivisor)
    	(isFano, NormalToricVariety)
///	


doc ///
    Key 
        (isVeryAmple, ToricDivisor)
    Headline 
        whether a torus-invariant Weil divisor is very ample
    Usage 
        isVeryAmple D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is very ample
    Description
        Text
            A Cartier divisor is very ample when it is basepoint free and the
            map arising from its complete linear series is a closed embedding.
            On a normal toric variety, the following are equivalent:
        Text
            @UL {
    		{"the divisor is a very ample divisor;"},
    		{"for every vertex of the associated lattice polytope
    		  associated to the divsor, the corresponding semigroup is
    		  saturated in the group characters."}
	    }@
        Text    
            On a smooth normal toric variety every ample divisor is very ample.
        Example		    
            PP3 = toricProjectiveSpace 3;
    	    assert isAmple PP3_0
    	    assert isVeryAmple PP3_0
    	    FF2 = hirzebruchSurface 2;
    	    assert isAmple (FF2_2 + FF2_3)
    	    assert isVeryAmple (FF2_2 + FF2_3)
        Text
            A Cartier divisor is ample when some positive integer multiple is
            very ample.  On a normal toric variety of dimension $d$, the
            $(d-1)$ multiple of any ample divisor is always very ample.
        Example	  
    	    X = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    assert (dim X === 4)
    	    D = 3*X_0
    	    assert isAmple D
    	    assert not isVeryAmple D
    	    assert not isVeryAmple (2*D)
    	    assert isVeryAmple (3*D)    
    SeeAlso
        "working with divisors"
        (isProjective,NormalToricVariety)
        (isAmple,ToricDivisor)
///	




doc ///
    Key 
	(isFano, NormalToricVariety)
        isFano 	
    Headline 
        whether a normal toric variety is Fano
    Usage 
        isFano X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean 
	    that is @TO true@ if the normal toric variety is Fano
    Description
        Text
            A normal toric variety is Fano if its anticanonical divisor,
            namely the sum of all the torus-invariant irreducible divisors, is
            ample.  This is equivalent to saying that the polyhedron
            associated to the anticanonical divisor is a reflexive polytope.
        Text
            Projective space is Fano.
    	Example
    	    PP3 = toricProjectiveSpace 3;
            assert isFano PP3
    	    K = toricDivisor PP3
            isAmple (-K)
            assert all (5, d -> isFano toricProjectiveSpace (d+1))
	Text
    	    There are eighteen smooth Fano toric threefolds.
    	Example
    	    assert all (18, i -> (X := smoothFanoToricVariety (3,i); isSmooth X and isFano X))
    	Text
            There are also many singular Fano toric varieties.
    	Example
            X = normalToricVariety matrix {{1,0,-1},{0,1,-1}};
            assert (not isSmooth X and isFano X)
            Y = normalToricVariety matrix {{1,1,-1,-1},{0,1,1,-1}};
    	    assert (not isSmooth Y and isFano Y)
    	    Z = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert (not isSmooth Z and isFano Z)
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.	    
    SeeAlso
        "finding attributes and properties"   
        (toricDivisor, NormalToricVariety)
        (isAmple, ToricDivisor)
        (smoothFanoToricVariety, ZZ, ZZ)
///


------------------------------------------------------------------------------
-- Polyhedral features of a toric divisor
------------------------------------------------------------------------------
doc ///
    Key 
        (vertices, ToricDivisor)
    Headline 
        compute the vertices of the associated polytope
    Usage 
        vertices D
    Inputs
        D : ToricDivisor
    Outputs 
        : Matrix 
	    whose columns are the vertices of the associated polytope
    Description
        Text	    
            On a complete normal toric variety, the polyhedron associated to a
            Cartier divisor is a lattice polytope.  Given a torus-invariant
            Cartier divisor on a normal toric variety, this method returns an
            integer matrix whose columns correspond to the vertices of the
            associated lattice polytope.  For a non-effective Cartier divisor,
            this methods returns @TO null@.  When the divisor is ample,
            the normal fan the corresponding polytope equals the fan
            associated to the normal toric variety.
        Text
            On the projective plane, the associate polytope is either empty,
	    a point, or a triangle.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    assert (null === vertices (-PP2_0))
    	    vertices (0*PP2_0)
    	    assert isAmple PP2_0
    	    V1 = vertices (PP2_0)
    	    X1 = normalToricVariety V1;
    	    assert (set rays X1 === set rays PP2 and max X1 === max PP2)
    	    assert isAmple (2*PP2_0)
    	    V2 = vertices (2*PP2_0)
    	    X2 = normalToricVariety V2;
    	    assert (rays X2 === rays X1 and max X2 === max X1)
        Text
            On a Hirzebruch surface, the polytopes associated to non-ample
  	    Cartier divisors give rise to other normal toric varieties.
        Example   
    	    FF2 = hirzebruchSurface 2;
    	    assert not isAmple FF2_2
    	    V3 = vertices FF2_2
    	    normalToricVariety V3  -- a degenerated version of the projective line
    	    assert isDegenerate normalToricVariety V3  
    	    assert not isAmple FF2_3
    	    V4 = vertices FF2_3
    	    normalToricVariety V4 -- a weighted projective space
    	    vertices FF2_1
    	    assert isAmple (FF2_2 + FF2_3)
    	    V5 = vertices (FF2_2 + FF2_3)
    	    X3 = normalToricVariety V5 -- isomorphic Hirzebruch surface
	    assert (set rays X3 === set rays FF2)
    SeeAlso
        "working with divisors"
    	(isComplete, NormalToricVariety)
    	(isCartier, ToricDivisor)
    	(isEffective, ToricDivisor)
    	(normalToricVariety, Matrix)
    	(latticePoints, ToricDivisor)
///	


doc ///
    Key 
        (latticePoints, ToricDivisor)
    Headline 
        compute the lattice points in the associated polytope
    Usage 
        latticePoints D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Matrix 
	    whose columns are the lattice points in the associated polytope
    Description
        Text	  
            On a complete normal toric variety, the polyhedron associated to a
  	    Cartier divisor is a lattice polytope.  Given a torus-invariant
  	    Cartier divisor on a normal toric variety, this method returns an
  	    integer matrix whose columns correspond to the lattices points
  	    contained in the associated polytope.  For a non-effective Cartier
  	    divisor, this method returns @TO null@.
        Text  
            On the projective plane, the associate polytope is either empty, a
   	    point, or a triangle.
        Example
            PP2 = toricProjectiveSpace 2;
    	    assert (null === vertices (-PP2_0))
    	    latticePoints (0*PP2_0)
    	    assert isAmple PP2_0
    	    V1 = latticePoints (PP2_0)
    	    X1 = normalToricVariety V1;
    	    assert (set rays X1 === set rays PP2 and  max X1 === max PP2)
    	    assert isAmple (2*PP2_0)
    	    V2 = latticePoints (2*PP2_0)
    	    X2 = normalToricVariety(V2, MinimalGenerators => true);
    	    assert (rays X2 === rays X1 and max X2 === max X1)
        Text
            In this singular example, we see that all the lattice points in
  	    the polytope arising from a divisor $2D$ do not come from the
  	    lattice points in the polytope arising from $D$.
        Example  
            Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    D = 3*Y_0;
    	    latticePoints D
    	    latticePoints (2*D)
    SeeAlso
        "working with divisors"
    	(normalToricVariety, Matrix)
    	(vertices, ToricDivisor)
///

doc ///	
    Key
        (polytope, ToricDivisor)
    Headline 
        makes the associated 'Polyhedra' polyhedron
    Usage 
        polytope D
    Inputs
        D : ToricDivisor
    Outputs 
        : Polyhedron
    Description
        Text
            For a torus-invariant Weil divisors $D = \sum_i a_i D_i$ the
            associated polyhedron is 
	    $\{ m \in M : (m, v_i) \geq -a_i \forall i \}$.	    
            Given a torus-invariant Weil divisor, this methods makes the
  	    associated polyhedra as an object in @TO Polyhedra@.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    P0 = polytope (-PP2_0)
	    assert (dim P0 === -1)
    	    P1 = polytope (0*PP2_0)
	    assert (dim P1 == 0)
	    assert (vertices P1 == 0)
    	    P2 = polytope (PP2_0)
    	    vertices P2
	    halfspaces P2
        Text	    
            This method works with $\QQ$-Cartier divisors.
        Example  	    
    	    Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    assert not isCartier Y_0
    	    assert isQQCartier Y_0
    	    P3 = polytope Y_0;
	    vertices P3
    	    vertices polytope Y_0
	    halfspaces P3
        Text	    
  	    It also works divisors on non-complete toric varieties.
       Example	    
           Z = normalToricVariety ({{1,0},{1,1},{0,1}}, {{0,1},{1,2}});
    	   assert not isComplete Z
    	   D = - toricDivisor Z
    	   P4 = polytope D;
	   rays P4
    	   vertices P4
	   halfspaces P4
    SeeAlso
        "working with divisors"
    	(normalToricVariety, Matrix)
    	(vertices, ToricDivisor)
///	
