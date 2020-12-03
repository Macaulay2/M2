------------------------------------------------------------------------------
-- Toric Maps (Documentation)
------------------------------------------------------------------------------
doc ///
    Key 
        "working with toric maps"
    Headline
        information about toric maps and the induced operations
    Description
	Text
	    A toric map is a morphism $f : X \to Y$ between normal toric
	    varieties that induces a morphism of algebraic groups $g : T_X \to
	    T_Y$ such that $f$ is $T_X$-equivariant with respect to the
	    $T_X$-action on $Y$ induced by $g$.  Every toric map $f : X \to Y$
	    corresponds to a unique map $f_N : N_X \to N_Y$ between the
	    underlying lattices.
        Text
            Although the primary method for creating a toric map is
	    @TO (map, NormalToricVariety, NormalToricVariety, Matrix)@, there
	    are a few other constructors.
    	Text
    	    @SUBSECTION "Making toric maps"@
	Text
    	    @UL {
                TO (map, NormalToricVariety, NormalToricVariety, Matrix),
        	TO (id, NormalToricVariety),
        	TO (symbol ^, NormalToricVariety, Array),		
        	TO (symbol _, NormalToricVariety, Array),
		TO diagonalToricMap,				
        	TO ToricMap,
        	TO (isWellDefined, ToricMap),
    	    }@
	Text
	    Having made a @TO2(ToricMap, "toric map")@, one can access its
	    basic invariants or test for some elementary properties by using
	    the following methods.
    	Text
    	    @SUBSECTION "Determining attributes and properties of toric maps"@
	Text
    	    @UL {
        	TO (source, ToricMap),		
        	TO (target, ToricMap),				
        	TO (matrix, ToricMap),			       		
		TO (symbol *, ToricMap, ToricMap),
		TO (isProper, ToricMap),	 		
		TO (isFibration, ToricMap),		
		TO (isDominant, ToricMap),	 		
		TO (isSurjective, ToricMap),	
    	    }@	
	Text	    
	    Several functorial aspects of normal toric varieties are also available.
    	Text
    	    @SUBSECTION "Functorial aspects of toric maps"@
	Text
    	    @UL {
		TO (weilDivisorGroup, ToricMap),			
		TO (classGroup, ToricMap),
		TO (cartierDivisorGroup, ToricMap),			
		TO (picardGroup, ToricMap),							
		TO (pullback, ToricMap, ToricDivisor),
		TO (pullback, ToricMap, CoherentSheaf),
		TO (inducedMap, ToricMap),
		TO (ideal, ToricMap)
    	    }@		
    SeeAlso
        "finding attributes and properties"
        "working with divisors"
        "working with sheaves"
        "resolving singularities"
///


doc ///
    Key
        ToricMap
    Headline
        the class of all torus-equivariant maps between normal toric varieties
    Description
        Text
            Let $X$ and $Y$ be normal toric varieties whose underlying
	    lattices are $N_X$ and $N_Y$ respectively.  A toric map is a
	    morphism $f : X \to Y$ that induces a morphism of algebraic groups
	    $g : T_X \to T_Y$ such that $f$ is $T_X$-equivariant with respect
	    to the $T_X$-action on $Y$ induced by $g$.  Every toric map 
	    $f : X \to Y$ corresponds to a unique map $f_N : N_X \to N_Y$
	    between the underlying lattices such that, for every cone $\sigma$
	    in the fan of $X$, there is a cone in the fan of $Y$ that contains
	    the image $f_N(\sigma)$. For details see Theorem 3.3.4 in
	    Cox-Little-Schenck.
        Text
	    To specify a map of normal toric varieties, the target and source
	    normal toric varieties need to be specificied as well as a matrix
	    which maps from $N_X$ to $N_Y$.
	Text
	    The primary constructor of a toric map is
	    @TO (map, NormalToricVariety, NormalToricVariety, Matrix)@.
    SeeAlso
    	"working with toric maps"
        NormalToricVariety
	(id, NormalToricVariety)
	(isWellDefined, ToricMap)
///


doc ///
    Key
        (source, ToricMap)
    Headline
        get the source of the map
    Usage
    	X = source f
    Inputs
    	f : ToricMap
    Outputs
    	X : NormalToricVariety
    	    that is the source of the map f
    Description
        Text
	    Given a toric map $f : X \to Y$, this method returns the normal
	    toric variety $X$.
       	Text
	    We illustrate how to access this defining feature of a toric map
	    with the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.
    	Example  
	    X = hirzebruchSurface 2;
            Y = toricProjectiveSpace 1;
            f = map(Y, X, matrix {{1, 0}})
     	    source f
	    assert (isWellDefined f and source f === X)   
	Text
	    Any normal toric variety is the source of its 
	    @TO2(diagonalToricMap, "diagonal map")@.
	Example
	    delta = diagonalToricMap X;
	    source delta
	    assert (isWellDefined delta and source delta === X)
	Text
	    In a well-defined toric map, the number of columns in the
	    underlying matrix equals the dimension of the source.	    
	Example
	    assert (numColumns matrix delta == dim X)
	Text
	    Since this is a defining attribute of a toric map, no computation
	    is required.
    SeeAlso
        "working with toric maps"
        (target, ToricMap)    
        (matrix, ToricMap)    		
	(isWellDefined, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, Matrix)
	(symbol **, NormalToricVariety, NormalToricVariety)	
///


doc ///
    Key
	(target, ToricMap)
    Headline 
    	get the target of the map
    Usage
    	Y = target f
    Inputs
    	f : ToricMap
    Outputs
    	Y : NormalToricVariety
    	    that is the target of the map f	
    Description	    
        Text
	    Given a toric map $f : X \to Y$, this method returns the normal
	    toric variety $Y$.
       	Text
	    We illustrate how to access this defining feature of a toric map
	    with the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	
    	Example  
	    X = hirzebruchSurface 2;
            Y = toricProjectiveSpace 1;
            f = map(Y, X, matrix {{1, 0}})
     	    target f
	    assert (isWellDefined f and target f === Y)   
	Text
	    Any normal toric variety is the target of the 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@ 
	    onto a factor of its Cartesian square.
	Example
	    X2 = X ** X
	    pi0 = X2^[0]
	    target pi0
	    assert (isWellDefined pi0 and target pi0 === X)
	    pi1 = X2^[1]
	    target pi1
	    assert (isWellDefined pi1 and target pi1 === X)	    	    	    
	Text
	    In a well-defined toric map, the number of rows in the
	    underlying matrix equals the dimension of the target.	    
	Example
	    assert (numRows matrix f == dim Y)
	Text
	    Since this is a defining attribute of a toric map, no computation
	    is required.	    
    SeeAlso
        "working with toric maps"    
        (source, ToricMap)    
        (matrix, ToricMap)    		
	(isWellDefined, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, Matrix)    		    	
	(symbol **, NormalToricVariety, NormalToricVariety)	
	(symbol ^, NormalToricVariety, Array)
///	  

doc ///
    Key
	(matrix, ToricMap)
    Headline 
    	get the underlying map of lattices
    Usage
    	g = matrix f
    Inputs
    	f : ToricMap
	Degree =>
	    unused
    Outputs
    	g : Matrix
    	    over the @TO2 (ZZ, "integers")@
    Description	    
        Text
	    Every toric map $f : X \to Y$ corresponds to a unique map 
	    $g : N_X \to N_Y$ of lattices such that, for every cone $\sigma$
	    in the fan of $X$, there is a cone in the fan of $Y$ that contains
	    the image $g(\sigma)$.  For more information on this
	    correspondence, see Theorem 3.3.4 in Cox-Little-Schenck's 
	    {\em Toric Varieties}. This method returns an integer matrix
	    representing $g$.
       	Text
	    We illustrate how to access this defining feature of a toric map
	    with the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.		
    	Example  
	    X = hirzebruchSurface 2;
            Y = toricProjectiveSpace 1;
            f = map(Y, X, matrix {{1, 0}})
     	    g = matrix f
	    assert (isWellDefined f and ring g === ZZ)
	Text
    	    An @TO2((symbol _, NormalToricVariety, Array), "inclusion map")@
    	    into the Cartesian square of a normal toric variety corresponds to
    	    matrix having identity and zero blocks.
	Example
	    X2 = X ** X;
	    iota0 = X2_[0]
	    assert (isWellDefined iota0 and source iota0 === X)
	    iota1 = X2_[1]
	    assert (isWellDefined iota1 and source iota1 === X)	    
	Text
	    In a well-defined toric map, the number of rows in the underlying
	    matrix must equal the dimension of the target and the number of
	    columns must equal the dimension of the source.	    
	Example
	    assert (numColumns g == dim X)
	    assert (numRows g == dim Y)	    
	Text
	    The output display for toric maps is inherited the underlying map
	    of lattices.
	Example
	    code (net, ToricMap)
	Text
	    Since this is a defining attribute of a toric map, no computation
	    is required.	    
    SeeAlso
        "working with toric maps"    
        (source, ToricMap)    
        (target, ToricMap)    		
	(isWellDefined, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, Matrix)    		    	
	(symbol **, NormalToricVariety, NormalToricVariety)
	(symbol _, NormalToricVariety, Array)
///	         


undocumented { (net,ToricMap) }


doc ///
    Key
        (map, NormalToricVariety, NormalToricVariety, Matrix)
    Headline 
    	make a torus-equivariant map between normal toric varieties
    Usage 
        f = map(Y, X, g)
    Inputs 
        Y : NormalToricVariety
	    the target of the map
	X : NormalToricVariety
	    the source of the map
	g : Matrix
	    over the integers
	Degree => 
	    used
	DegreeLift =>   
	    used
	DegreeMap =>
	    used
    Outputs 
        f : ToricMap
    Description
        Text
	    Let $X$ and $Y$ be normal toric varieties whose underlying
	    lattices are $N_X$ and $N_Y$ respectively.  Every toric map 
	    $f : X \to Y$ corresponds to a unique map $g : N_X \to N_Y$ of
	    lattices such that, for any cone $\sigma$ in the fan of $X$, there
	    is a cone in the fan of $Y$ that contains the image $g(\sigma)$.
	    For more information on this correspondence, see Theorem 3.3.4 in
	    Cox-Little-Schenck's {\em Toric Varieties}.  Given the target, the
	    source, and the matrix representing lattice map, this basic
	    constructor creates the corresponding toric map.
    	Text
	    We illustrate how to construct the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	
    	Example  
	   X = hirzebruchSurface 2;
           Y = toricProjectiveSpace 1;
           f = map (Y, X, matrix {{1, 0}})
	   assert (isWellDefined f and source f === X and 
	       target f === Y and matrix f === matrix {{1, 0}})
	Text
	    Our second example makes the 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@ from
	    the @TO2(toricBlowup, "blow-up")@ of the origin in the affine
	    plane to the @TO2(affineSpace, "affine plane")@ is
	    proper.
	Example
	    A = affineSpace 2;
	    max A
	    B = toricBlowup({0, 1}, A);
	    g = B^[]
	    isProper g
	    assert (isWellDefined g and g == map(A,B,1) and
	        matrix g === id_(ZZ^2) and isProper g)
    Caveat
        This method does not check that the given matrix determines a map of
        toric varieties. In particular, it assumes that the image of each cone
	in the source is contained in a cone in the target. One can verify this
	by using @TO (isWellDefined, ToricMap)@.
    SeeAlso
        "working with toric maps"    
    	(source, ToricMap)
	(target, ToricMap)
	(matrix, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, ZZ)
	(isProper, ToricMap)
/// 


doc ///
    Key
        (map, NormalToricVariety, NormalToricVariety, ZZ)
    Headline 
    	make a torus-equivariant map between normal toric varieties
    Usage 
        f = map(Y, X, m)
    Inputs 
        Y : NormalToricVariety
	    the target of the map
	X : NormalToricVariety
	    the source of the map
	m : ZZ
	Degree => 
	    used
	DegreeLift =>   
	    used
	DegreeMap =>
	    used
    Outputs 
        f : ToricMap
    Description
        Text
	    Let $X$ and $Y$ be normal toric varieties whose underlying
	    lattices are $N_X$ and $N_Y$ respectively.  Every toric map 
	    $f : X \to Y$ corresponds to a unique map $g : N_X \to N_Y$ of
	    lattices such that, for any cone $\sigma$ in the fan of $X$, there
	    is a cone in the fan of $Y$ that contains the image $g(\sigma)$.
	    For more information on this correspondence, see Theorem 3.3.4 in
	    Cox-Little-Schenck's {\em Toric Varieties}.  Given the target, the
	    source, and the matrix representing lattice map, this basic
	    constructor creates the corresponding toric map; the integer
	    determines the lattice map in two distinct ways.
	Text	    
	    When the integer equals zero, the underlying map of lattices is
	    represented by the zero matrix.
	Example
	    X = hirzebruchSurface 2;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, 0)
	    assert (isWellDefined f and source f === X and 
		target f === Y and matrix f === map(ZZ^(dim Y), ZZ^(dim X), 0))
    	Text	    	
	    If the integer $m$ is nonzero, then the underlying map of lattices
	    is represented by multiplying the identity matrix by the given
	    integer $m$.  Hence, this second case requires that the dimension
	    of the source and target be equal.
	Example
	    Z = normalToricVariety ({{1,0},{-1,2},{0,-1}}, {{0,1},{0,2},{1,2}});
	    assert (isWellDefined Z and not isSmooth Z)
	    g = map(Z, X, 2)
	    assert (isWellDefined g and source g === X and
		target g === Z and matrix g === 2*id_(ZZ^(dim X)))	    
    	Text
	    Setting {\tt m = 1} is a easy way to construct the canoncal
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@
	    associated to a @TO2(toricBlowup, "blow-up")@ or the 
	    @TO2((id, NormalToricVariety), "identity map")@.	    
	Example
	    A = affineSpace 2;
	    B = toricBlowup ({0, 1}, A);
	    h = map(A, B, 1)
	    assert (isWellDefined h and h == B^[])	 	    
	    i = map(A, A, 1)
	    i == id_A
	    assert (isWellDefined i and source i === A and
		target i === A and matrix i === id_(ZZ^2))
    Caveat
        This method does not check that the given matrix determines a map of
        toric varieties. In particular, it assumes that the image of each cone
	in the source is contained in a cone in the target. One can verify this
	by using @TO (isWellDefined, ToricMap)@.
    SeeAlso
        "working with toric maps"    
    	(source, ToricMap)
	(target, ToricMap)
	(matrix, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, Matrix)
/// 


doc ///
    Key
        (id, NormalToricVariety)
    Headline
    	make the identity map from a NormalToricVariety to itself
    Usage 
        id_X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : ToricMap
    Description
        Text	    
	    For the identity map on a normal toric variety, the underlying map
	    of lattices is given by the identity matrix. For more information
	    on this correspondence, see Theorem 3.3.4 in Cox-Little-Schenck's
	    {\em Toric Varieties}.
	Example
	    X = hirzebruchSurface 2;
	    f = id_X
	    assert (isWellDefined f and source f === X and
		target f === X and matrix f === id_(ZZ^(dim X)))
	Text
	    Identity maps also arise as edge cases of the canonical 
	    @TO2((symbol ^, NormalToricVariety, Array), "projections")@ and
	    @TO2((symbol _, NormalToricVariety, Array), "inclusions")@
	    associated to Cartesian products.
	Example
	    X2 = X ** X;
	    X2^[0,1]
	    X2_[0,1]
	    assert (X2^[0,1] == id_X2 and X2_[0,1] == id_X2)
    SeeAlso
        "working with toric maps"    
        (map, NormalToricVariety, NormalToricVariety, ZZ)
        (map, NormalToricVariety, NormalToricVariety, Matrix)	
	id 
///    


doc ///
    Key
        (isWellDefined, ToricMap)
    Headline 
        whether a toric map is well defined 
    Usage 
        isWellDefined f
    Inputs 
        f : ToricMap
    Outputs 
        : Boolean 
	    that is true if the underlying linear map determines a toric map
    Description
        Text	
	    Let $X$ and $Y$ be normal toric varieties whose underlying
	    lattices are $N_X$ and $N_Y$ respectively.  Every toric map 
	    $f : X \to Y$ corresponds to a unique map $g : N_X \to N_Y$ of
	    lattices such that, for any cone $\sigma$ in the fan of $X$, there
	    is a cone in the fan of $Y$ that contains the image $g(\sigma)$.
	    For more information on this correspondence, see Theorem 3.3.4 in
	    Cox-Little-Schenck's {\em Toric Varieties}.  This method
	    determines whether the underlying map of lattices defines a toric
	    map.
    	Text
	    We illustrate this test with the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.		
    	Example  
	    X = hirzebruchSurface 2;
            Y = toricProjectiveSpace 1;
            f = map (Y, X, matrix {{1, 0}})
	    source f
	    target f
	    matrix f
    	    assert (isWellDefined f and source f === X and
		target f === Y and matrix f === matrix {{1, 0}})
	Text
	    The second example illustrates two attempts to define a toric map
	    from the projective plane to a 
	    @TO2(weightedProjectiveSpace, "weighted projective space")@. The
	    first, corresponding to the identity on the lattices, is not
	    well-defined.  The second, corresponding to a stretch in the
	    lattices, is well-defined.  By making the current debugging level
	    greater than one, one gets some addition information about the
	    nature of the failure.
	Example
	    debugLevel = 1;	
	    Z = toricProjectiveSpace 2;
	    W = weightedProjectiveSpace {1, 1, 2};
	    g = map (W, Z, 1)
	    assert not isWellDefined g 
	    h = map (W, Z, matrix {{1, 0}, {0, 2}})
            assert isWellDefined h
    	Text
            This method also checks the following aspects of the data
            structure:	    
	Text
    	    @UL {
	        {"the underlying ", TO HashTable, " has the expected keys,
	    	    namely ", TT "source", ", ", TT "target", ", ", 
		    TT "matrix", ", and ", TT "cache", ","},
       	        {"the value of the ", TT "source", " key is a ", 
		    TO NormalToricVariety, ","},
       	        {"the value of the ", TT "target", " key is a ", 
		    TO NormalToricVariety, ","},
       	        {"the value of the ", TT "matrix", " key is a ", 
		    TO Matrix, ","},
       	        {"the underling ring of the ", TT "matrix", " is ", 
		    TO ZZ, ","},
       	        {"the rank of the source of the ", TT "matrix", " equal
		    dimension of the ", TT "source", " variety,"},
       	        {"the rank of the target of the ", TT "matrix", " equal
		    dimension of the ", TT "target", " variety,"},		    
                {"the value of the ", TT "cache", " key is a ", 
		    TO CacheTable, "."}
	    }@	    
    SeeAlso
        "working with toric maps"    
    	(map, NormalToricVariety, NormalToricVariety, Matrix)
    	(hirzebruchSurface, ZZ)
        (toricProjectiveSpace, ZZ)
        (weightedProjectiveSpace, List)
///


doc ///
    Key
    	(symbol *, ToricMap, ToricMap)
    Headline
    	make the composition of two toric maps
    Usage
    	g * f
    Inputs
    	f : ToricMap
	    a toric map between toric varieties
	g : ToricMap
	    a toric map between toric varieties
    Outputs
    	: ToricMap
	    the composition g * f from source f to target g
    Description
    	Text
	    Given two toric maps with the target of first equal to the source
	    of second, this method returns the composite map from source of
	    the first to target of the second.
    	Text
	    We illustrate this construction with the projection from a
	    @TO2(smoothFanoToricVariety, "smooth Fano Toric threefold")@
	    to the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ and a projection
	    from this Hirzebruch surface to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	    
	Example
	    X = smoothFanoToricVariety(3,14);
 	    Y = hirzebruchSurface 1;	    
	    f = map(Y, X, matrix{{1,0,0},{0,1,0}})
    	    assert (isWellDefined f and source f === X and target f === Y)	
	    Z = toricProjectiveSpace 1;	   
	    g = map(Z, Y, matrix{{1, 0}})	
	    assert (isWellDefined g and source g === Y and target g === Z)	        
	    h = g * f
	    assert (isWellDefined h and source h === X and target h === Z)	
	    X = hirzebruchSurface 1;
    	Text
	    Composing @TO2(diagonalToricMap, "diagonal maps")@ and canonical
	    @TO2((symbol ^, NormalToricVariety, Array), "projections")@ yields 
	    @TO2((id, NormalToricVariety), "identity maps")@.
	Example
	    X2 = X ** X;
	    delta = diagonalToricMap X
	    assert (X2^[0] * delta == id_X and X2^[1] * delta == id_X)	    
    SeeAlso
        "working with toric maps"    
    	(map, NormalToricVariety, NormalToricVariety, Matrix)
	(symbol **, NormalToricVariety, NormalToricVariety)
///	


doc ///
    Key
	(symbol ==, ToricMap, ToricMap)
    Headline 
    	whether to toric maps are equal
    Usage
    	f == g
    Inputs
    	f : ToricMap
    	g : ToricMap	
    Outputs
    	: Boolean
    	    that is @TO true@ if the toric maps have the same source, same
    	    target, and same underlying map of lattices
    Description	    
        Text
	    Two toric maps are equal if their three defining attributes
	    (namely source, target, and underlying matrix) are the same.
       	Text
	    We illustrate this test with the 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@ from a
	    @TO2(toricBlowup, "blow-up")@ at a point in the projective plane
	    to the @TO2(toricProjectiveSpace, "projective plane")@ and various
	    identity maps.
    	Example  
            Y = toricProjectiveSpace 2;	    	
	    X = toricBlowup({0, 2}, Y);	 
	    f = X^[]   
	    assert (isWellDefined f and f == map(Y, X, 1))
	    g = id_X 
	    assert (g == map(X, X, 1))
	    assert (f != g)
	    assert (isWellDefined g and source g === X and target g === X)
	    assert (matrix f == matrix g and source f === source g and 
		target f =!= target g)	   
	Text
	    The second example shows that we can have more than one well-defined
	    toric map with the same source and target.
	Example
	    Z = toricProjectiveSpace 1;
	    pi1 = map(Z, X, matrix{{0, 1}}) 
	    assert (isWellDefined pi1 and source pi1 === X and target pi1 === Z)
	    pi2 = map(Z, X, matrix{{0, 2}}) 
	    assert (isWellDefined pi2 and source pi2 === X and target pi2 === Z)	    
    	    assert (pi1 != pi2)
    SeeAlso
        "working with toric maps"    
	(isWellDefined, ToricMap)
        (map, NormalToricVariety, NormalToricVariety, Matrix)    
        (map, NormalToricVariety, NormalToricVariety, ZZ)    			    
        (id, NormalToricVariety)    			    	
///


doc ///
    Key
	(symbol ^, NormalToricVariety, Array)
    Headline 
    	make a canonical projection map 
    Usage
    	X ^ A
    Inputs
    	X : NormalToricVariety
	    that is a constructed as a product or blow-up
    	A : Array
	    whose entries index factors in the product construction of {\tt X}
	    or is empty if {\tt X} is a blow-up
    Outputs
    	: ToricMap
    	    that projects from {\tt X} onto the product of the factors indexed
    	    by {\tt A} or the normal toric variety that was blown-up
    Description	    
        Text
	    A product of varieties is equipped with canonical projection maps
	    onto it factors.  Given a product of normal toric varieties and a
	    nonempty array, this methods provides a concise way to make these
	    toric maps.
       	Text
	    The product of two normal toric varieties has projections onto
	    each factor.
    	Example  
	    Y0 = toricProjectiveSpace 1;
	    Y1 = hirzebruchSurface 3;
	    X = Y0 ** Y1;	    
	    X^[0]
	    assert isWellDefined X^[0]
	    assert (source X^[0] === X)
	    assert (target X^[0] === Y0)
    	    X^[1]
	    assert isWellDefined X^[1]
	    assert (source X^[1] === X)
	    assert (target X^[1] === Y1)
	Text
	    If {\tt A} indexes all the factors, then we simply obtain the
	    identity map on {\tt X}.
	Example
	    X^[0,1]
	    assert (X^[0,1] == id_X)
    	Text
	    When there are more than two factors, we also obtain projections
	    onto any subset of the factors.
	Example
	    Z = Y0 ^** 3;
    	    Z^[0]
	    Z^[1]
	    Z^[2]
	    assert all (3, i -> isWellDefined Z^[i] and source Z^[i] === Z and target Z^[i] === Y0)
	    Z^[0,1]
	    Z^[0,2]
	    Z^[1,2]
	    assert (isWellDefined Z^[1,2] and target Z^[1,2] === Y0 ** Y0)
    	    Z^[0,1,2]
	    assert (Z^[0,1,2] == id_Z)
	Text
	    When the normal toric variety is not constructed as a product,
	    this method only reproduces the identity map.
	Example
	    components Y1    
	    Y1^[0]
	    assert (Y1^[0] == id_Y1)
	Text
	    When the normal toric variety {\tt X} is a blow-up and the array
	    {\tt A} is empty, one obtains the canonical projection.
	Example
	    A = affineSpace 2;
	    B = toricBlowup({0,1}, A);
	    B^[]
	    assert (isWellDefined B^[] and source B^[] === B and target B^[] === A)
    SeeAlso
        "working with toric maps"    
	(symbol _, NormalToricVariety, Array) 	
        (symbol **, NormalToricVariety, NormalToricVariety)    
        (cartesianProduct, Sequence)  
	components  	
	toricBlowup		    
        (symbol ^, Module, Array)    			    	
///	    


doc ///
    Key
	(symbol _, NormalToricVariety, Array)
    Headline 
    	make a canonical inclusion into a product
    Usage
    	X _ A
    Inputs
    	X : NormalToricVariety
	    that is a constructed as a product
    	A : Array
	    whose entries index factors in the product construction of {\tt X}
    Outputs
    	: ToricMap
    	    that is a canonical inclusion from the product of the factors
    	    indexed by {\tt A} into the product {\tt X}	    
    Description	    
        Text
	    A product of varieties is equipped with canonical inclusion maps
	    from a product of any subset of its factors.  Given a product of
	    normal toric varieties and a nonempty array, this methods provides
	    a concise way to make these toric maps.
       	Text
	    The product of two normal toric varieties has inclusions from
	    each factor.
    	Example  
	    Y0 = toricProjectiveSpace 1;
	    Y1 = hirzebruchSurface 3;
	    X = Y0 ** Y1;	    
	    X_[0]
	    assert (isWellDefined X_[0] and source X_[0] === Y0 and target X_[0] === X)
    	    X_[1]
	    assert (isWellDefined X_[1] and source X_[1] === Y1 and target X_[1] === X)
	Text
	    The canonical inclusions interact with the canonical 
	    @TO2((symbol ^, NormalToricVariety, Array), "projections")@ in
	    the expected way.
	Example
	    assert (X^[0] * X_[0] == id_Y0 and X^[1] * X_[1] == id_Y1)
	    assert (X^[1] * X_[0] == map(Y1, Y0, 0) and X^[0] * X_[1] == map(Y0, Y1, 0))
	Text
	    If {\tt A} indexes all the factors, then we simply obtain the
	    identity map on {\tt X}.
	Example
	    X_[0,1]
	    assert (X_[0,1] == id_X)
    	Text
	    When there are more than two factors, we also obtain inclusions
	    from any subset of the factors.
	Example
	    Z = Y0 ^** 3;
    	    Z_[0]
	    Z_[1]
	    Z_[2]
	    assert all (3, i -> isWellDefined Z_[i] and source Z_[i] === Y0 and target Z_[i] === Z)
	    Z_[0,1]
	    Z_[0,2]
	    Z_[1,2]
	    assert (isWellDefined Z_[1,2] and source Z_[1,2] === Y0 ** Y0)
    	    Z_[0,1,2]
	    assert (Z_[0,1,2] == id_Z)
	Text
	    When the normal toric variety is not constructed as a product,
	    this method only reproduces the identity map.
	Example
	    components Y1    
	    Y1_[0]
	    assert (Y1_[0] == id_Y1)
    SeeAlso
        "working with toric maps"    
	(symbol _, NormalToricVariety, Array) 	
        (symbol **, NormalToricVariety, NormalToricVariety)    
        (cartesianProduct, Sequence)  
	components  			    
        (symbol ^, Module, Array)    			    	
///	       


doc ///
    Key
    	diagonalToricMap
	(diagonalToricMap, NormalToricVariety)
	(diagonalToricMap, NormalToricVariety, ZZ)
	(diagonalToricMap, NormalToricVariety, ZZ, Array)		
    Headline 
    	make a diagonal map into a Cartesian product
    Usage
    	diagonalToricMap X
    	diagonalToricMap(X,m)
    	diagonalToricMap(X,m,A)		
	
    Inputs
    	X : NormalToricVariety
	m : ZZ
	    that is positive
    	A : Array
	    whose entries index factors in the {\tt m}-ary product of {\tt X}
    Outputs
    	: ToricMap
    	    that is a diagonal map from {\tt X} into the {\tt m}-ary Cartesian
    	    product of {\tt X} with itself; the optional argument {\tt A}
    	    indexes factors in the product
    Description	    
        Text
	    Given a positive integer {\tt m} and a normal toric variety {\tt
	    X}, the diagonal morphism is the toric map from {\tt X} to the
	    {\tt m}-ary Cartersion product of {\tt X} such that it composes to
	    the identity with the {\tt i}-th 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection map")@, 
	    for all {\tt i} in {\tt A}, and compose to the zero map with the
	    {\tt i}-th projection maps for all {\tt i} not in {\tt A}.
       	Text
	    The most important example arises when {\tt m = 2}.  For this
	    case, one may omit both {\tt m} and {\tt A}.
    	Example  
  	    X = hirzebruchSurface 1;
  	    delta = diagonalToricMap X
  	    assert (isWellDefined delta and source delta === X and target delta === X ^** 2)
  	    S = ring target delta;
  	    I = ideal delta
  	    assert (codim I === dim X)
  	    X2 = target delta;
  	    assert (X2^[0] * delta == id_X and X2^[1] * delta == id_X)
	    assert (delta == diagonalToricMap(X,2) and delta == diagonalToricMap(X,2,[0,1]))	    
	Text
	    We may also recover the canonical 
	    @TO2((symbol _, NormalToricVariety, Array), "inclusions")@.
	Example
	    X2 = target delta;
	    assert (X2_[0] == diagonalToricMap(X,2,[0]))
	    assert (X2_[1] == diagonalToricMap(X,2,[1]))	    
        Text
	    When there are more than to factors, a diagonal can map to any
	    subset of the factors.  By omitting {\tt A}, we obtain the large
	    diagonal.
	Example
	    m = 3;
            largeD = diagonalToricMap(X, m)
  	    assert (isWellDefined largeD and source largeD === X and target largeD === X ^** m)
  	    assert (codim ideal largeD === (m-1) * dim X)
	    assert (largeD == diagonalToricMap(X, m, [0,1,2]))
	Text
	    By using the array to specify a proper subset of the factors, we
	    obtain a small diagonal.
	Example
  	    smallD = diagonalToricMap (X, 3, [0,2])
  	    assert (isWellDefined smallD and source smallD === X and target smallD === X ^** m)
  	    assert (codim ideal smallD === (m-1) * dim X)
  	    X3 = target smallD;
  	    assert (X3^[0] * smallD == id_X and X3^[1] * smallD == map(X,X,0) and
  	        X3^[2] * smallD == id_X)
    SeeAlso
        "working with toric maps"    
	(symbol ^, NormalToricVariety, Array) 	
	(symbol ^, NormalToricVariety, Array) 		
        (symbol ^**, NormalToricVariety, ZZ)    
///	       


------------------------------------------------------------------------------
-- properties of toric maps
------------------------------------------------------------------------------
doc ///
    Key
        (isProper, ToricMap)
    	isProper	
    Headline 
        whether a toric map is proper
    Usage 
        isProper f
    Inputs 
        f : ToricMap
    Outputs 
        : Boolean 
	    that is @TO true@ if the map is proper
    Description
        Text
	    A morphism of varieties is proper if it is universally closed.
	    For a toric map $f : X \to Y$ corresponding to the map 
	    $g : N_X \to N_Y$ of lattices, this is equivalent to the preimage
	    of the support of the target fan under $g$ being equal to the
	    support of the source fan. For more information about this
	    equivalence, see Theorem 3.4.11 in Cox-Little-Schenck's
	    {\em Toric Varieties}.	    
    	Text
	    We illustrate this method on the projection from the second 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.		
    	Example  
	    X = hirzebruchSurface 2;
            Y = toricProjectiveSpace 1;
            f = map(Y, X, matrix {{1,0}})
	    isProper f
	    assert (isWellDefined f and source f === X and 
		target f === Y and isProper f)
	Text
	    The second example shows that the 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@ from
	    the @TO2(toricBlowup, "blow-up")@ of the origin in the affine
	    plane to @TO2(affineSpace, "affine plane")@ is proper.	    
	Example
	    A = affineSpace 2;
	    B = toricBlowup({0,1}, A);
	    g = B^[]
	    isProper g
	    assert(isWellDefined g and g == map(A, B, 1) and isProper g)
	Text
	    The natural inclusion of the @TO2(affineSpace, "affine plane")@
	    into the @TO2(toricProjectiveSpace, "projective plane")@ is not
	    proper.
	Example
	    A = affineSpace 2;
	    P = toricProjectiveSpace 2;
	    f = map(P, A, 1)
	    isProper f
	    isDominant f
	    assert (isWellDefined f and not isProper f and isDominant f)
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the toric map.
    SeeAlso
        "working with toric maps"    
    	(map, NormalToricVariety, NormalToricVariety, Matrix)
    	(map, NormalToricVariety, NormalToricVariety, ZZ)	
        (isComplete, NormalToricVariety)
/// 


doc ///
    Key
        (isFibration, ToricMap)
	isFibration
    Headline 
        whether a toric map is a fibration
    Usage 
        isFibration f
    Inputs 
        f:ToricMap
    Outputs 
        :Boolean 
	    that is @TO true@ if the map is a fibration
    Description
        Text
	    A proper morphism $f : X \to Y$ is a fibration if 
	    $f_*(OO_X) = OO_Y$.  A proper toric map is a fibration if and only
	    if the underlying map of lattices is a surjection. For more
	    information, see Proposition 2.1 in deCataldo-Migliorini-Mustata,
	    "The combinatorics and topology of proper toric maps"
	    @HREF("https://arxiv.org/abs/1407.3497", "arXiv:1407.3497")@.
	Text
	    We illustrate this method on the projection from the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.		
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, matrix{{1 ,0}})
	    isFibration f
	    assert (isWellDefined f and isFibration f)
	Text
	    Here is an example of a proper map that is not a fibration.
	Example
	    Z = weightedProjectiveSpace {1, 1, 2};
	    g = map(Z, X, matrix{{1, 0}, {0, -2}})
	    isFibration g
	    assert (isWellDefined g and isProper g and not isFibration g)
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the toric map.	    	    
    SeeAlso
        "working with toric maps"    
        (isProper, ToricMap)
/// 


doc ///
    Key
        (isDominant, ToricMap)
	isDominant
    Headline
        whether a toric map is dominant
    Usage
        isDominant f
    Inputs
        f : ToricMap
    Outputs
        : Boolean
	    that is @TO true@ if the image is dense
    Description
        Text
	    A morphism of varieties is dominant if the image is dense. For a
	    toric map, it suffices to check that the dimension of the image
	    is the dimension of the target.
	Text
	    We demonstrate that the natural inclusion from the
	    @TO2(affineSpace, "affine plane")@ into the
	    @TO2(toricProjectiveSpace, "projective plane")@ is a dominant, but
	    not surjective
	Example
	    A = affineSpace 2;
	    P = toricProjectiveSpace 2;
	    f = map(P, A, 1)
	    isDominant f
    	    isSurjective f
	    assert (isWellDefined f and isDominant f and not isSurjective f)
	Text
	    A toric map from the projective line to the projective plane is
	    not dominant.
	Example
	    X = toricProjectiveSpace 1;
	    g = map(P, X, matrix{{2}, {1}})
	    isDominant g
    	    I = ideal g
	    assert (isWellDefined g and not isDominant g and codim I === 1)	    
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the toric map.	    
    SeeAlso
        "working with toric maps"    
	(map, NormalToricVariety, NormalToricVariety, Matrix)
	(map, NormalToricVariety, NormalToricVariety, ZZ)
	(isComplete, NormalToricVariety)
///


doc ///
    Key
        (isSurjective, ToricMap)
    Headline 
        whether a toric map is surjective
    Usage 
        isSurjective f
    Inputs 
        f : ToricMap
    Outputs 
        : Boolean 
	    that is @TO true@ if the map is surjective
    Description
        Text
	    A morphism $f : X \to Y$ is surjective if $f(X) = Y$ as sets.  To
	    be surjective toric map, the dimension of $X$ must be greater than
	    or equal to $Y$ and the image of the algebraic torus in $X$ must
	    be equal to the algebraic torus in $Y$.  Since $f$ is
	    torus-equivariant, it follows that $f$ is surjective if and only
	    if its image contains a point in each torus orbit in $Y$.  This
	    method checks whether all of the cones in the target fan contain a
	    point from the relative interior of a cone in the source fan.
	Text
	    The canonical 
	    @TO2((symbol ^, NormalToricVariety, Array), "projections")@ from a
	    product to the factors are surjective.	    
	Example
	    X = toricProjectiveSpace 2;
	    Y = hirzebruchSurface 2;
	    XY = X ** Y;
	    pi0 = XY^[0]
	    isSurjective pi0
	    assert (isWellDefined pi0 and isSurjective pi0)
	    pi1 = XY^[1]
	    isSurjective pi1
	    assert (isWellDefined pi1 and isSurjective pi1)	    
	Text
	    We demonstrate that the natural inclusion from the
	    @TO2(affineSpace, "affine plane")@ into the
	    @TO2(toricProjectiveSpace, "projective plane")@ is a dominant, but
	    not surjective
	Example
	    A = affineSpace 2;
	    f = map(X, A, 1)
	    isDominant f
    	    isSurjective f
	    assert (isWellDefined f and isDominant f and not isSurjective f)
	Text
	    For a toric map to be surjective, the underlying map of fans need
	    not be surjective.  
	Example
	    Y = (toricProjectiveSpace 1) ** (toricProjectiveSpace 1);
	    X = normalToricVariety(
		{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}},
		{{0},{1},{2},{3},{4},{5},{6},{7}});
    	    g = map(Y,X,1)
	    isSurjective g
	    isComplete X
    	    assert (isWellDefined g and isSurjective g and not isComplete X)	
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the toric map.	    
    SeeAlso
        "working with toric maps"    
        (isDominant, ToricMap)
/// 


------------------------------------------------------------------------------
-- making various associated groups into functors
------------------------------------------------------------------------------
doc ///
    Key
        (weilDivisorGroup, ToricMap)
    Headline
        make the induced map between groups of Weil divisors
    Usage
        weilDivisorGroup f
    Inputs
        f : ToricMap
	    with a smooth target
    Outputs
        : Matrix
	    representing the map of abelian groups between the corresponding
	    groups of torus-invariant Weil divisors
    Description
        Text
	    Given a toric map $f : X \to Y$ where $Y$ a smooth toric variety,
            this method returns the induced map of abelian groups from the
            group of torus-invariant Weil divisors on $Y$ to the group of
            torus-invariant Weil divisors on $X$.  For arbitary normal toric
            varieties, the @TO weilDivisorGroup@ is not a functor.  However,
            @TO weilDivisorGroup@ is a contravariant functor on the category
            of @TO2(isSmooth, "smooth")@ normal toric varieties.
	Text
	    We illustrate this method on the projection from the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.		    
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, matrix {{1, 0}})
	    f' = weilDivisorGroup f
	    assert (isWellDefined f and source f' == weilDivisorGroup Y and
		target f' == weilDivisorGroup X)
	Text
	    The next example gives the induced map from the group of
            torus-invariant Weil divisors on the projective plane to the group
            of torus-invariant Weil divisors on the first Hirzebruch surface.
	Example
	    Z = toricProjectiveSpace 2;
	    g = map(Z, X, matrix {{1, 0}, {0, -1}})
	    g' = weilDivisorGroup g
	    assert (isWellDefined g and source g' == weilDivisorGroup Z and
		target g' == weilDivisorGroup X)
        Text
            The induced map between the groups of torus-invariant Weil
            divisors is compatible with the induced map between the 
	    @TO2(classGroup, "class groups")@.
        Example
            g'' = classGroup g
            assert(g'' * fromWDivToCl Z  == fromWDivToCl X  * g')
    SeeAlso
        "working with toric maps"    
        (weilDivisorGroup, NormalToricVariety)
        (classGroup, ToricMap)
    	(cartierDivisorGroup, NormalToricVariety)
        (pullback, ToricMap, ToricDivisor)
///

   
doc ///
    Key
        (classGroup, ToricMap)
    Headline 
        make the induced map between class groups
    Usage 
        classGroup f
    Inputs 
        f : ToricMap
	    with a smooth target 
    Outputs 
        : Matrix 
	    representing the map of abelian groups between the corresponding
	    class groups
    Description
        Text
	    Given a toric map $f : X \to Y$ where $Y$ a smooth toric variety,
            this method returns the induced map of abelian groups from the
            class group of $Y$ to the class group of $X$.  For arbitary normal
            toric varieties, the @TO classGroup@ is not a functor.  However,
            @TO classGroup@ is a contravariant functor on the category of
            @TO2(isSmooth, "smooth")@ normal toric varieties.
	Text
	    We illustrate this method on the projection from the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	    
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, matrix {{1, 0}})
	    f' = classGroup f
	    assert (isWellDefined f and source f' == classGroup Y and
		target f' == classGroup X) 
        Text
	    The induced map between the class groups is compatible with the
	    induced map between the 
	    @TO2(weilDivisorGroup, "groups of torus-invariant Weil divisors")@.
        Example
            f'' = weilDivisorGroup f
            assert(f' * fromWDivToCl Y  == fromWDivToCl X  * f'')	    
	Text
	    The source of the toric map need not be 
	    @TO2(isSmooth, "smooth")@.	    
	Example
	    Z = toricBlowup({0, 1}, X, {1,2});
    	    assert (isWellDefined Z and not isSmooth Z)	    
	    g = map(Y, Z, matrix{{1, 0}})
	    g' = classGroup g
	    g'' = weilDivisorGroup g
	    assert(g' * fromWDivToCl Y == fromWDivToCl Z  * g'')
	    assert (isWellDefined g and source g' == classGroup Y and
		target g' == classGroup Z) 
    SeeAlso
        "working with toric maps"    
        (classGroup, NormalToricVariety)
        (weilDivisorGroup, ToricMap)
        (picardGroup, ToricMap)
        (pullback, ToricMap, ToricDivisor)
///


doc ///
    Key
        (picardGroup, ToricMap)
    Headline
        make the induced map between Picard groups
    Usage
        picardGroup f
    Inputs
        f : ToricMap
    Outputs
        : Matrix
	    representing the map of abelian groups between the corresponding
	    Picard groups
    Description
        Text
	    Given a toric map $f : X \to Y$, this method returns the induced
	    map of abelian groups from the Picard group of $Y$ to the Picard
	    group of $X$.  In other words, @TO picardGroup@ is a contravariant
	    functor on the category of normal toric varieties.
	Text
	    We illustrate this method on the projection from the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	  	
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, matrix {{1, 0}})
	    f' = picardGroup f
	    assert (isWellDefined f and source f' == picardGroup Y and
		target f' == picardGroup X)
        Text
	    The induced map between the Picard groups is compatible with the
	    induced map between the 
	    @TO2(cartierDivisorGroup, "groups of torus-invariant Cartier divisors")@.
        Example
            f'' = cartierDivisorGroup f
            assert(f' * fromCDivToPic Y  == fromCDivToPic X  * f'')	    
	Text
	    Neither the source nor the target of the toric map needs to be 
	    @TO2(isSmooth, "smooth")@.	    
	Example  
	    W = weightedProjectiveSpace {1, 1, 2};	     
	    Z = toricBlowup({0, 1, 4}, (W ** toricProjectiveSpace 1), {0, -2, 1});
	    assert (not isSmooth W and not isSmooth Z)
    	    g = map(W, Z, matrix{{1,0,0},{0,1,0}})
	    g' = picardGroup g
	    assert (isWellDefined g and source g' == picardGroup W and
		target g' == picardGroup Z)
            g'' = cartierDivisorGroup g
            assert(g' * fromCDivToPic W  == fromCDivToPic Z  * g'')		    
    SeeAlso
        "working with toric maps"    
        (picardGroup, NormalToricVariety)
        (cartierDivisorGroup, ToricMap)
        (classGroup, ToricMap)	
        (pullback, ToricMap, ToricDivisor)
///


doc ///
    Key
        (cartierDivisorGroup, ToricMap)
    Headline
        make the induced map between groups of Cartier divisors.
    Usage
        cartierDivisorGroup f
    Inputs
        f : ToricMap
    Outputs
        : Matrix
	    representing the map of abelian groups between the corresponding
	    groups of torus-invariant Cartier divisors
    Description
        Text
	    Given a toric map $f : X \to Y$, this method returns the induced
	    map of abelian groups from the group of torus-invariant Cartier
	    divisors on $Y$ to the group of torus-invariant Cartier divisors
	    on $X$. In other words, @TO cartierDivisorGroup@ is a
	    contravariant functor on the category of normal toric varieties.
	Text
	    We illustrate this method on the projection from the first 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.	  	
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    f = map(Y, X, matrix {{1, 0}})
	    f' = cartierDivisorGroup f
	    assert (isWellDefined f and source f' == cartierDivisorGroup Y and
		target f' == cartierDivisorGroup X)
        Text
	    The induced map between the Picard groups is compatible with the
	    induced map between the 
	    @TO2(cartierDivisorGroup, "groups of torus-invariant Cartier divisors")@.
        Example
            f'' = picardGroup f
            assert(f'' * fromCDivToPic Y == fromCDivToPic X  * f')
	Text
	    Neither the source nor the target of the toric map needs to be 
	    @TO2(isSmooth, "smooth")@.	    
	Example  
	    W = weightedProjectiveSpace {1, 1, 2};	     
	    Z = toricBlowup({0, 1, 4}, (W ** toricProjectiveSpace 1), {0, -2, 1});
	    assert (not isSmooth W and not isSmooth Z)
    	    g = map(W, Z, matrix{{1,0,0},{0,1,0}})
	    g' = picardGroup g
	    assert (isWellDefined g and source g' == picardGroup W and
		target g' == picardGroup Z)
            g'' = cartierDivisorGroup g
            assert(g' * fromCDivToPic W  == fromCDivToPic Z  * g'')	    
    SeeAlso
        "working with toric maps"    
        (cartierDivisorGroup, NormalToricVariety)
        (picardGroup, ToricMap)
        (pullback, ToricMap, ToricDivisor)
///


------------------------------------------------------------------------------
-- operations related to toric divisors and sheaves
------------------------------------------------------------------------------
doc ///
    Key
        pullback
    Headline
        make the pullback along a toric map
    Description
        Text
	    A toric map induces pullback operations in several different
	    situations.  Those currently implemented in this package are
	    listed below.	    
///
    
    
doc ///
    Key
        (pullback, ToricMap, ToricDivisor)
    Headline
        make the pullback of a Cartier divisor under a toric map
    Usage
        pullback(f, D)
    Inputs
        f : ToricMap
	D : ToricDivisor
	    on the target of {\tt f} that is Cartier
    Outputs
        : ToricDivisor
	    the pullback of the divisor {\tt D} under the map {\tt f}
    Description
        Text
	    Torus-invariant Cartier divisors pullback under a toric map by
	    composing the toric map with the support function of the divisor.
	    For more information, see Proposition 6.2.7 in
	    Cox-Little-Schenck's {\em Toric Varieties}.
    	Text
	    As a first example, we consider the 
	    @TO2((symbol ^, NormalToricVariety, Array), "projection")@ from a
	    @TO2((symbol **, NormalToricVariety, NormalToricVariety), "product")@ 
	    of two @TO2(toricProjectiveSpace, "projective lines")@ onto the
	    first factor.  The pullback of a point is just a fibre in the
	    product.
	Example
            P = toricProjectiveSpace 1;
            X = P ** P;
            f = X^[0]
	    pullback(f, P_0)
	    pullback(f, 2*P_0 - 6*P_1)
	    assert (isWellDefined f and f == map(P, X, matrix {{1,0}}))	    
	Text
	    The next example illustrates that the pullback of a line through
	    the origin in @TO2(affineSpace, "affine plane")@ under the 
	    @TO2(toricBlowup, "blowup map")@ is a line together with the
	    exceptional divisor.  
	Example
	    A = affineSpace 2, max A
	    B = toricBlowup({0,1}, A);
	    g = B^[]
    	    pullback(g, A_0)
 	    pullback(g, -3*A_0 + 7*A_1)	    
    SeeAlso
        "working with toric maps"    
        (isCartier, ToricDivisor)
        (pullback, ToricMap, CoherentSheaf)
///


doc ///
    Key
        (pullback, ToricMap, CoherentSheaf)
        (pullback, ToricMap, Module)	
    Headline
        make the pullback of a coherent sheaf under a toric map
    Usage
        F' = pullback(f, F)    
        M' = pullback(f, M)
    Inputs
        f : ToricMap
    	    where the target is smooth
	F : CoherentSheaf
	    on the target of {\tt f} or module representing such a coherent sheaf
    Outputs
        F' : CoherentSheaf
	    the pullback of {\tt F} under {\tt f}, or 
	    the module {\tt M'} representing such a sheaf
    Description
        Text
	    The category of coherent 
	    @TO2((sheaf, NormalToricVariety, Module), "sheaves")@ on a normal
	    toric variety is equivalent to the quotient category of finitely
	    generated modules over the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@ by the
	    full subcategory of torsion modules with respect to the
	    @TO2((ideal, NormalToricVariety), "irrelevant ideal")@. In
	    particular, each finitely generated module over the total
	    coordinate ring corresponds to coherent sheaf on the normal toric
	    variety and every coherent sheaf arises in this manner. For more
	    information, see Subsection 5.3 in Cox-Little-Schenck.
	Text
	    A toric morphism $f : X \to Y$ where $Y$ is smooth
	    @TO2((inducedMap, ToricMap), "induces")@ a map $g : S \to R$ from
	    total coordinate ring $S$ of $Y$ to the total coordinate ring $R$
	    of $X$.  Given a coherent sheaf $F$ on $Y$ corresponding to the
	    $S$-module $M$, the pullback of $F$ along $f$ is the coherent sheaf
	    corresponding to the $R$-module $M ** g$.  For more information,
	    see Tomasz Mańdziuk's "Cox rings and algebraic maps",
	    {\em Mathematische Nachrichten} {\bf 292} (2019) 389-401,	    
	    @HREF("https://arxiv.org/abs/1703.04794","arXiv:1703.04794")@.
	    This method returns the sheaf (or module) corresponding to the
	    pullback along the toric map.
	Text
	    As first example, we compute the pullback of the 
	    @TO2(cotangentSheaf, "cotangent sheaf")@ on the
	    @TO2(toricProjectiveSpace, "projective plane")@ to the first
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@.  
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 2;
	    f = map(Y, X, matrix{{1,0},{0,-1}})
	    Omega = cotangentSheaf Y
	    F = pullback(f, Omega)
    	    (R = ring X, S = ring Y);
	    inducedMap f
	    presentation module Omega	    
	    presentation module F	    
	    assert (isWellDefined f and isHomogeneous module F)
    	Text
	    For toric divisors, the operations of {\tt pullback} and taking
	    the associated sheaf {\tt OO} commute.
	Example	    
	    D = Y_0 + 2*Y_1 + 3*Y_2
	    L = pullback(f, OO D)
	    D' = pullback(f, D)
	    OO D'
    	    assert (isWellDefined f and L === OO pullback(f, D))
    Caveat
    	This method assumes that the target is smooth. One may verify this by
	using @TO (isSmooth, NormalToricVariety)@.	    
    SeeAlso
        "working with toric maps"
        "working with sheaves"    
	(inducedMap, ToricMap)
	(symbol SPACE, OO, ToricDivisor)
        (pullback, ToricMap, ToricDivisor)
///


doc ///
    Key
        (inducedMap, ToricMap)
    Headline
        make the induced map between total coordinate rings (a.k.a. Cox rings)
    Usage
        inducedMap f
    Inputs
        f : ToricMap
	    with a smooth target
	Degree =>
	    unused
	Verify =>
	    unused
    Outputs
        : RingMap
	    between the total homogeneous coordinate rings (aka Cox rings)
    Description
        Text
	    Any morphism of varieties whose target is a smooth normal toric
	    variety is determined by a collection of lines bundles together
	    with a section of each line bundle.  This data defines a ring map
	    whose source is the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
	    (a.k.a. Cox ring) of target variety.  For more information, see
	    David A. Cox, "The Functor of a Smooth Toric Variety", {\em The
	    Tohoku Mathematical Journal, Second Series}, {\bf 47} (1995) 251-262, 
	    @HREF("https://arxiv.org/abs/alg-geom/9312001", "arXiv:alg-geom/9312001v2")@.
	Text
	    Given a toric map $f : X \to Y$ where $Y$ is smooth, this method
	    returns the induced map from the total coordinate ring $S$ of $Y$
	    to the total coordinate ring $R$ of $X$.  Since $f$ is
	    torus-equivarient, each variable in the polynomial ring $S$ maps
	    to a monomial in $R$.  
	Text
	    As a first example, we compute the map on the total coordinate
	    rings induced by the natural inclusion of the 
	    @TO2(affineSpace, "affine plane")@ into 
	    @TO2(toricProjectiveSpace, "projective plane")@.
	Example
	    A = affineSpace 2;
	    P = toricProjectiveSpace 2;
	    f = map(P, A, 1)
	    (R = ring A, S = ring P);
	    f' = inducedMap f
	    f' vars S
	    ideal f == 0 
	    degrees source f'
	    degrees target f'
	    assert (isWellDefined f and isHomogeneous f')
	Text
	    The second example considers the projection from the third 
	    @TO2(hirzebruchSurface, "Hirzebruch surface")@ to the 
	    @TO2(toricProjectiveSpace, "projective line")@.
	Example
	    X = hirzebruchSurface 3;
	    Y = toricProjectiveSpace 1;
	    g = map(Y, X, matrix {{1, 0}})
    	    (R = ring Y, S = ring X);
	    g' = inducedMap g
    	    degrees source g'
	    degrees target g'
    	    assert (isWellDefined g and isHomogeneous g')
	Text
	    In the third example, we consider a third Veronese embedding of
	    the projective line into projective $3$-space.
	Example
	    Z = toricProjectiveSpace 3;
	    h = map(Z, Y, matrix {{1}, {2}, {3}})
    	    (R = ring Y, S = ring X);
	    h' = inducedMap h
    	    degrees source g'
	    degrees target g'
	    ideal h
    	    assert (isWellDefined h and isHomogeneous h' and ideal h == ker h')	    
	Text
	    To ensure that the induced map is homogeneous, the optional
	    argument @TO DegreeMap@ is used to record the degree of the
	    monomials in the target ring $R$.
	Example
	    code (inducedMap, ToricMap)
    Caveat
    	This method assumes that the target is smooth. One may verify this by
	using @TO (isSmooth, NormalToricVariety)@.	
    SeeAlso
        "working with toric maps"    
    	"working with sheaves"
	(ring, NormalToricVariety)
        (classGroup, ToricMap)
	(pullback, ToricMap, ToricDivisor)
	(ideal, ToricMap)
///


doc ///
    Key
    	(ideal, ToricMap)
    Headline
    	make the ideal defining the closure of the image 
    Usage
    	ideal f
    Inputs
    	f : ToricMap
    Outputs
    	: Ideal
	    in the homogeneous coordinate ring (a.k.a. Cox ring) of the target
    Description
    	Text
	    The closure of image of a morphism $f : X \to Y$ is a closed
	    subscheme in $Y$.  All closed subschemes in normal toric variety
	    $Y$ correspond to a saturated homogeneous ideal in the
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
	    (a.k.a. Cox ring) of $Y$.  For more information, see Proposition
	    5.2.4 in Cox-Little-Schenck's {\em Toric Varieties}.  This method
	    returns the saturated homogeneous ideal corresponding to the
	    closure of the image $f$.
	Text
	    The closure of a distinguished @TO2(affineSpace, "affine")@ open
	    set in the @TO2(toricProjectiveSpace, "projective space")@ is the
	    entire space.
	Example
	    A = affineSpace 4;
	    P = toricProjectiveSpace 4;
	    iota = map(P, A, 1)
	    ideal iota
	    assert (isWellDefined iota and ideal iota == 0)
	Text
	    The twisted cubic curve is the image of a map from the projective
	    line to the projective $3$-space.
	Example
	    X = toricProjectiveSpace 1;
	    Y = toricProjectiveSpace 3;
	    f = map(Y, X, matrix{{1}, {2}, {3}})
	    S = ring Y;
	    I = ideal f
	    assert (isWellDefined f and isHomogeneous I and 
		I == saturate(I, ideal Y) and I == ker inducedMap f and 
	    	I == minors(2, matrix{{S_0, S_1, S_2}, {S_1, S_2, S_3}}))
	Text
	    Thirdly, we have the image of diagonal embedding of the projective
	    $4$-space.
	Example
    	    (Y2 = Y ** Y, R = ring Y2);
	    g = diagonalToricMap(Y, 2);
	    J = ideal g
	    assert (isWellDefined g and isHomogeneous J and
		J == saturate(J, ideal Y2) and 
		J == minors(2, matrix{{R_0,R_1,R_2,R_3},{R_4,R_5,R_6,R_7}}))
	Text
	    The algorithm used is a minor variant of Algorithm 12.3 in Bernd
	    Sturmfels {\em Gröbner basis and convex polytopes}, University Lecture
	    Series 8. American Mathematical Society, Providence, RI, 1996.
    SeeAlso
        "working with toric maps"    
    	(inducedMap, ToricMap)
        (ring, NormalToricVariety)
///
