------------------------------------------------------------------------------
-- Total Coordinate Rings and Coherent Sheaves (Documentation)
------------------------------------------------------------------------------
doc ///
    Key 
        "working with sheaves"
    Headline
        information about coherent sheaves and total coordinate rings (a.k.a. Cox rings)
    Description
        Text	
            @HREF("http://www3.amherst.edu/~dacox/", "David A. Cox")@
  	    introduced the total coordinate ring $S$ of a normal toric variety
  	    $X$ and the irrelevant ideal $B$.  The polynomial ring $S$ has one
  	    variable for each ray in the associated fan and a natural grading
  	    by the class group.  The monomial ideal $B$ encodes the maximal
  	    cones.  The following results of Cox indicate the significance of
  	    the pair $(S,B)$.
        Text	  
  	    @UL {
    		{"The variety ", EM "X", " is a good categorial ",
    		 "quotient of Spec(", EM "S", ") -  V(", EM "B",
		 ") by a suitable group action."},
                {"The category of coherent sheaves on ", EM "X", " is
    		   equivalent to the quotient of the category of finitely
    		   generated graded ", EM "S", "-modules by the full
    		   subcategory of ", EM "B", "-torsion modules."}
	    }@
        Text 
            In particular, we may represent any coherent sheaf on $X$ by
  	    giving a finitely generated graded $S$-module.
        Text   
            The following methods allow one to make and manipulate coherent
  	    sheaves on normal toric varieties.
        Text     
            @SUBSECTION "Sheaf-theoretic methods"@  
        Text     
  	    @UL {
    		TO (ring, NormalToricVariety),
    		TO (ideal, NormalToricVariety),
    		TO (sheaf, NormalToricVariety, Ring),	  	  
    		TO (sheaf ,NormalToricVariety, Module),
    		TO (symbol SPACE, OO, ToricDivisor),
    		TO (cotangentSheaf, NormalToricVariety),	  
    		TO (cohomology, ZZ, NormalToricVariety, CoherentSheaf),
		TO (intersectionRing, NormalToricVariety),
		TO (chern, CoherentSheaf),
		TO (ctop, CoherentSheaf),		
		TO (ch, CoherentSheaf),		
		TO (chi, CoherentSheaf),			       		
		TO (todd, CoherentSheaf),			       				
		TO (hilbertPolynomial, NormalToricVariety, CoherentSheaf)
	    }@
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
        "resolving singularities"
	"working with toric maps"	
        "working with divisors"
///	


doc ///
    Key 
        (ring, NormalToricVariety) 
	"Cox ring"
    Headline
        make the total coordinate ring (a.k.a. Cox ring)
    Usage 
        ring X
    Inputs 
        X : NormalToricVariety
    Outputs
        : PolynomialRing
	    the total coordinate ring
    Description
        Text
	    The total coordinate ring, which is also known as the Cox ring, of
  	    a normal toric variety is a polynomial ring in which the variables
  	    correspond to the rays in the fan.  The map from the group of
  	    torus-invarient Weil divisors to the class group endows this ring
  	    with a grading by the @TO2(classGroup,"class group")@.  For more
  	    information, see Subsection 5.2 in Cox-Little-Schenck's 
	    {\em Toric Varieties}.
        Text	    	
            The total coordinate ring for 
	    @TO2(toricProjectiveSpace, "projective space")@ is the standard
  	    graded polynomial ring.	    
        Example	      
            PP3 = toricProjectiveSpace 3;
    	    S = ring PP3;
    	    assert (isPolynomialRing S and isCommutative S)
    	    gens S
    	    degrees S
    	    assert (numgens S == #rays PP3)
    	    coefficientRing S
        Text	    
             For a @TO2((symbol **, NormalToricVariety, NormalToricVariety),
	    "product")@ of projective spaces, the total coordinate ring has a
	    bigrading.
        Example	    
     	    X = toricProjectiveSpace(2) ** toricProjectiveSpace(3);
    	    gens ring X
    	    degrees ring X
        Text
  	    A @TO2(hirzebruchSurface, "Hirzebruch surface")@ also has a
  	    $\ZZ^2$-grading.	    
        Example		    
    	    FF3 = hirzebruchSurface 3;
    	    gens ring FF3
    	    degrees ring FF3
	Text
	    To avoid duplicate computations, the attribute is cached in the
	    normal toric variety.  The variety is also cached in the ring.	    
    Caveat     
        The total coordinate ring is not yet implemented when the toric
	variety is degenerate or the class group has torsion.
    SeeAlso
        "working with sheaves"
    	(rays, NormalToricVariety)
    	classGroup
    	WeilToClass
    	(fromWDivToCl, NormalToricVariety)
    	(ideal, NormalToricVariety)
    	(sheaf, NormalToricVariety, Module)
///	


doc ///
    Key
        (normalToricVariety, Ring)
    Headline 
        get the associated normal toric variety
    Usage
        normalToricVariety S
    Inputs
        S : Ring
	CoefficientRing => Ring
            not used
        MinimalGenerators => Boolean 
            not used
        Variable => Symbol
	    not used	    
        WeilToClass => Matrix 
            not used   
    Outputs 
        : NormalToricVariety
    Description
        Text
            If a polynomial ring is constructed as the 	    
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@ of
  	    normal toric variety, then this method returns the associated
  	    variety.  
        Example	  
    	    PP3 = toricProjectiveSpace 3;
    	    S = ring PP3
    	    gens S
    	    degrees S
    	    normalToricVariety S
	    assert (PP3 === normalToricVariety S)	    
    	    variety S
	    assert (PP3 === variety S)
        Text
            If the polynomial ring is not constructed from a variety, then
  	    this method produces an error: "no variety associated with ring".
        Example  
     	   S = QQ[x_0..x_2];
    	   gens S
    	   degrees S
    	   assert (try (normalToricVariety S; false) else true)
    	   assert (try (variety S; false) else true)
    Caveat
    	This methods does {\em not} determine if a ring could be realized as
	the total coordinate ring of a normal toric variety.
    SeeAlso
        "working with sheaves"
       	(ring, NormalToricVariety)
///	


doc ///
    Key 
        (ideal, NormalToricVariety)
    	(monomialIdeal, NormalToricVariety)
    Headline
        make the irrelevant ideal
    Usage 
        ideal X
	monomialIdeal X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Ideal 
	    that is homogeneous in the total coordinate ring of $X$
    Description
        Text	    
	    The irrelevant ideal is a reduced monomial ideal in the
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@ that
	    encodes the combinatorics of the fan.  For each maximal cone in
	    the fan, it has a minimal generator, namely the product of the
	    variables not indexed by elements of the list corresponding to the
	    maximal cone.  For more information, see Subsection 5.3 in
	    Cox-Little-Schenck's {\em Toric Varieties}.
        Text
            For @TO2(toricProjectiveSpace, "projective space")@, the
            irrelevant ideal is generated by the variables.
        Example 	    
    	    PP4 = toricProjectiveSpace 4;
    	    B = ideal PP4
    	    assert (isMonomialIdeal B and B == radical B)
	    monomialIdeal PP4
	    assert (B == monomialIdeal PP4)
        Text
  	    For an affine toric variety, the irrelevant ideal is the unit
  	    ideal.
        Example 	    
    	    C = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}}, {{0,1,2,3}});
    	    ideal C
	    assert (monomialIdeal C == 1)
	    monomialIdeal affineSpace 3
	    assert (ideal affineSpace 3 == 1)
        Text	    
            The irrelevant ideal for a @TO2((symbol **, NormalToricVariety,
	    NormalToricVariety), "product")@ of toric varieties is
	    intersection of the irrelevant ideal of the factors.
        Example 	    
    	    X = toricProjectiveSpace (2) ** toricProjectiveSpace (3);
    	    S = ring X;
    	    B = ideal X
    	    primaryDecomposition B
    	    dual monomialIdeal B
        Text            
	    For a @TO2(isComplete, "complete")@ 
	    @TO2(isSimplicial, "simplicial")@ toric variety, the irrelevant
  	    ideal is the Alexander dual of the Stanley-Reisner ideal of the
  	    fan.
        Example   
    	    Y = smoothFanoToricVariety (2,3);
    	    dual monomialIdeal Y
    	    sort apply (max Y, s -> select (# rays Y, i -> not member (i,s)))	    
	    primaryDecomposition dual monomialIdeal Y
        Text
            Since the irrelevent ideal is a monomial ideal, the command 
	    @TO monomialIdeal@ also produces the irrelevant ideal.
        Example  
    	    code (monomialIdeal, NormalToricVariety)
    SeeAlso
        "working with sheaves"
    	(max, NormalToricVariety)
    	(ring, NormalToricVariety)
///	


------------------------------------------------------------------------------
-- sheaves
------------------------------------------------------------------------------
doc ///
    Key
        (sheaf, NormalToricVariety, Module)
    Headline 
        make a coherent sheaf
    Usage
        sheaf (X, M)
    Inputs 
        X : NormalToricVariety
        M : Module
	    a graded module over the total coordinate ring
    Outputs 
        : CoherentSheaf
	    the coherent sheaf on {\tt X} corresponding to {\tt M}
    Description
        Text	   	    
	    The category of coherent sheaves on a normal toric variety is
  	    equivalent to the quotient category of finitely generated modules
  	    over the @TO2((ring, NormalToricVariety), "total coordinate ring")@
  	    by the full subcategory of torsion modules with respect to the
	    @TO2((ideal, NormalToricVariety), "irrelevant ideal")@.  In
  	    particular, each finitely generated module over the total
  	    coordinate ring corresponds to coherent sheaf on the normal toric
  	    variety and every coherent sheaf arises in this manner.  For more
  	    information, see Subsection 5.3 in Cox-Little-Schenck's 
	    {\em Toric Varieties}.
        Text  
            Free modules correspond to reflexive sheaves.
        Example  	    
     	    PP3 = toricProjectiveSpace 3;
            F = sheaf (PP3, (ring PP3)^{{1},{2},{3}})
    	    FF7 = hirzebruchSurface 7;
    	    G = sheaf (FF7, (ring FF7)^{{1,0},{0,1}})
    SeeAlso
        "working with sheaves"
    	(ring, NormalToricVariety)
    	(ideal, NormalToricVariety)
    	(sheaf, NormalToricVariety)
///	


doc ///
    Key 
        (sheaf, NormalToricVariety, Ring)
     	(symbol _, OO, NormalToricVariety)
    	(sheaf, NormalToricVariety)
    Headline
        make a coherent sheaf of rings
    Usage
        sheaf (X, S)
    Inputs 
        X : NormalToricVariety
        S : Ring 
	    the total coordinate ring of {\tt X}
    Outputs
        : SheafOfRings
	    the structure sheaf on {\tt X}
    Description
        Text
	    The category of coherent sheaves on a normal toric variety is
  	    equivalent to the quotient category of finitely generated modules
  	    over the total coordinate ring by the full subcategory of torsion
  	    modules with respect to the irrelevant ideal.  In particular, the
  	    total coordinate ring corresponds to the structure sheaf. For more
  	    information, see Subsection 5.3 in Cox-Little-Schenck's 
	    {\em Toric Varieties}.
        Text  
            On @TO2(toricProjectiveSpace, "projective space")@, we can make
            the structure sheaf in a few ways.
        Example  
            PP3 = toricProjectiveSpace 3;
    	    F = sheaf (PP3, ring PP3)
    	    G = sheaf PP3
    	    assert (F === G)
    	    H = OO_PP3
    	    assert (F === H)
    SeeAlso
        "working with sheaves"
    	(ring, NormalToricVariety)
    	(sheaf, NormalToricVariety, Module)
///

doc ///	
    Key 
        (cotangentSheaf, NormalToricVariety)
	(cotangentSheaf, ZZ, NormalToricVariety)
    Headline
        make the sheaf of Zariski 1-forms
    Usage 
        cotangentSheaf X
    Inputs
        X : NormalToricVariety
        Minimize => Boolean 	
	    that specifies whether to apply @TO minimalPresentation@ to the
	    result before returning it
    Outputs
        : CoherentSheaf 
	    the sheaf of Zariski 1-forms on {\tt X}
    Description
        Text	    
	    For a normal variety, the sheaf of Zariski 1-forms is defined to
  	    be the double dual of the cotangent bundle or equivalently the
  	    extension of the sheaf of 1-forms on the smooth locus to the
  	    entire variety (the complement of the smooth locus has codimension
  	    at least two because the variety is normal).  By construction,
  	    this sheaf is reflexive with rank equal to the dimension of the
  	    variety.  When the underlying variety is smooth, this is simple
  	    the sheaf of 1-forms or the cotangent bundle.  For more
  	    information, see Theorem 8.1.5 in Cox-Little-Schenck's 
	    {\em Toric Varieties}.
        Text	    
            On a non-degenerate normal toric variety, the sheaf of Zariski
  	    1-forms is associated to the kernel of a map from the character
	    lattice tensor the total coordinate ring to the direct sum over
	    the rays of the quotient of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@ by the
	    ideal generated by the corresponding variable.
        Example  
    	    PP3 = toricProjectiveSpace 3;
    	    OmegaPP3 = cotangentSheaf PP3
	    assert (prune cotangentSheaf PP3 === cotangentSheaf (PP3, Minimize => true))	    
    	    L = prune exteriorPower (3, OmegaPP3)
    	    assert (L === OO toricDivisor PP3)
	    assert (L === prune cotangentSheaf (dim PP3, PP3))
        Example	    
    	    X = hirzebruchSurface 2;
    	    OmegaX = cotangentSheaf X
    	    L = prune exteriorPower(dim X, OmegaX)
    	    assert (L === OO toricDivisor X)
	    assert (L === prune cotangentSheaf(dim X, X))
        Example
    	    Y = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}}, {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
    	    assert (not isSmooth Y and not isProjective Y)
    	    OmegaY = cotangentSheaf Y
    	    prune exteriorPower(dim Y, OmegaY)
    	    assert (prune exteriorPower(dim Y, OmegaY) === OO toricDivisor Y)
    SeeAlso
        "working with sheaves"
    	(sheaf, NormalToricVariety, Module) 
///	   


doc ///
    Key 
        (cohomology, ZZ, NormalToricVariety, CoherentSheaf)
     	(cohomology, ZZ, NormalToricVariety, SheafOfRings)
    Headline 
        compute the cohomology of a coherent sheaf
    Usage
        HH^i (X, F)
    Inputs
        i : ZZ
        X : NormalToricVariety
        F : CoherentSheaf
	    on {\tt X}
    Outputs 
        : Module
            the {\tt i}-th cohomology group of {\tt F}
    Description
        Text	 	    
            The cohomology functor $HH^i (X,-)$ from the category of sheaves
  	    of abelian groups to the category of abelian groups is the right
  	    derived functor of the global sections functor.
        Text
            As a simple example, we compute the dimensions of the cohomology
  	    groups for some line bundles on the projective plane.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    HH^0 (PP2, OO_PP2(1))
	    matrix table (reverse toList (0..2), toList (-10..5),  (i,j) -> rank HH^i (PP2, OO_PP2(j-i)))
        Text
            For a second example, we compute the dimensions of the cohomology
  	    groups for some line bundles on a Hirzebruch surface.
        Example  
  	    FF2 = hirzebruchSurface 2;
	    HH^0 (FF2, OO_FF2(1,1))
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^0 (FF2, OO_FF2(j,i)))
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^1 (FF2, OO_FF2(j,i)))	    
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^2 (FF2, OO_FF2(j,i)))	    
        Text
            When {\it F} is free, the algorithm based on [Diane Maclagan and
  	    Gregory G. Smith, 
	    @HREF("http://arxiv.org/abs/math.AC/0305214", "Multigraded Castelnuovo-Mumford regularity")@, 
	    {\it J. Reine Angew. Math.} {\bf 571} (2004), 179-212].  The
  	    general case uses the methods described in [David Eisenbud, Mircea
  	    Mustata, and Mike Stillman, 
  	    @HREF("http://arxiv.org/abs/math.AG/0001159", "Cohomology on toric varieties and local cohomology with monomial supports")@,
	    {\it J. Symbolic Comput.} {\bf 29} (2000), 583-600].
    SeeAlso
        "working with sheaves"
    	(sheaf, NormalToricVariety, Module)
    	(sheaf, NormalToricVariety, Ring)
///
