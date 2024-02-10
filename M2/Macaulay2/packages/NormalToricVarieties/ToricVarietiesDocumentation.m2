------------------------------------------------------------------------------
-- Normal Toric Varieties (Documentation)
------------------------------------------------------------------------------
undocumented {
    (describe, NormalToricVariety)    
    }

doc ///
    Key
        NormalToricVarieties
    Headline
        working with normal toric varieties and related objects
    Description
    	Text
            A toric variety is an integral scheme such that an algebraic torus
            forms a Zariski open subscheme and the natural action this torus
            on itself extends to an action on the entire scheme.  Normal toric
            varieties correspond to strongly convex rational polyhedral fans.
            This makes the theory of normal toric varieties very explicit and
            computable.
    	Text
            This {\em Macaulay2} package is designed to manipulate normal
            toric varieties and related geometric objects.  An introduction to
            the theory of normal toric varieties can be found in the following
            textbooks:
    	Text   
    	    @UL { 
                {"David A. Cox, John B. Little, Hal Schenck, ",
	            HREF("https://dacox.people.amherst.edu/toric.html", 
			EM "Toric varieties"), ", Graduate Studies in
		    Mathematics 124. American Mathematical Society,
		    Providence RI, 2011.  ISBN: 978-0-8218-4817-7"},
                {"Günter Ewald, ", EM "Combinatorial convexity and algebraic
                    geometry", ", Graduate Texts in Mathematics 168.
                    Springer-Verlag, New York, 1996.  ISBN: 0-387-94755-8" },
                {"William Fulton, ", EM "Introduction to toric varieties", ",
                    Annals of Mathematics Studies 131, Princeton University
                    Press, Princeton, NJ, 1993. ISBN: 0-691-00049-2" },
                {"Tadao Oda, ", EM "Convex bodies and algebraic geometry, an
                    introduction to the theory of toric varieties", ",
                    Ergebnisse der Mathematik und ihrer Grenzgebiete (3) 15,
                    Springer-Verlag, Berlin, 1988. ISBN: 3-540-17600-4" },	 
    	    }@
	Text
            @SUBSECTION "Contributors"@
	Text
	    The following people have generously contributed code, improved existing code, or 
	    enhanced the documentation:
	    @HREF("http://www.math.duke.edu/~psa/","Paul Aspinwall")@,
	    @HREF("http://www-users.math.umn.edu/~cberkesc/","Christine Berkesch")@,
	    @HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm","René Birkner")@,
	    @HREF("http://people.math.gatech.edu/~jchen646/","Justin Chen")@,		    
	    @HREF("https://math.berkeley.edu/~ceur/index.html", "Chris Eur")@,
	    @HREF("https://www.math.tamu.edu/~mfaust/", "Matthew Faust")@,
	    @HREF("http://www.coreyharris.name", "Corey Harris")@,		
	    @HREF("http://www-users.math.umn.edu/~loper012/", "Michael Loper")@,
	    @HREF("http://www.warwick.ac.uk/staff/D.Maclagan/","Diane Maclagan")@,
	    @HREF("https://www.math.mcmaster.ca/people/grad-students.html","Maryam Nowroozi")@,
	    @HREF("https://sites.google.com/view/erikapirnes", "Erika Pirnes")@,		
	    @HREF("https://math.berkeley.edu/~ritvik/index.html", "Ritvik Ramkumar")@,		
	    @HREF("https://sites.google.com/site/jranamath", "Julie Rana")@,	
	    @HREF("https://math.umn.edu/~mahrud/", "Mahrud Sayrafi")@,				
	    @HREF("http://www.math.unl.edu/~aseceleanu2/","Alexandra Seceleanu")@,                
	    @HREF("http://www.math.cornell.edu/~mike/","Mike Stillman")@,
	    @HREF("https://www.math.princeton.edu/people/sameera-vemulapalli", "Sameera Vemulapalli")@,			
	    @HREF("https://www.math.tamu.edu/~walkere", "Elise Walker")@,					
	    @HREF("https://wangweikun.com", "Weikun Wang")@,					
	    @HREF("https://sites.google.com/view/rachel-webb", "Rachel Webb")@,	
	    @HREF("https://www.math.tamu.edu/~thomasjyahl", "Thomas Yahl")@, and	    						
	    @HREF("https://www-users.math.umn.edu/~jkyang/", "Jay Yang")@.
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
        "resolving singularities"
	"working with toric maps"	
        "working with divisors"
        "working with sheaves"	
///	


------------------------------------------------------------------------------
-- Basic features of the normal toric variety datatype
------------------------------------------------------------------------------
doc ///
    Key 
        "making normal toric varieties"
    Headline
        information about the basic constructors
    Description
        Text
	    A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.
            The fan is encoded by the minimal nonzero lattice points on its
            rays and the set of rays defining the maximal cones (a maximal
            cone is not properly contained in another cone in the fan).  More
            information about the correspondence between normal toric varieties
            and strongly convex rational polyhedral fans appears in Subsection
            3.1 of Cox-Little-Schenck.
    	Text
    	    The general method for creating @TO2(NormalToricVariety, "normal
	    toric variety")@ is @TO normalToricVariety@.  However, there are
	    many additional methods for constructing other specific types of
	    normal toric varieties.
    	Text
    	    @SUBSECTION "Constructors for normal toric varieties"@
	Text
    	    @UL {
                TO (normalToricVariety, List, List),
        	TO (normalToricVariety, Matrix),
        	TO NormalToricVariety,
        	TO (isWellDefined, NormalToricVariety),
        	TO (affineSpace, ZZ),
		TO "projective space",
        	TO (toricProjectiveSpace, ZZ),
        	TO (weightedProjectiveSpace, List),
        	TO (hirzebruchSurface, ZZ),
        	TO (kleinschmidt, ZZ, List),
        	TO (symbol **, NormalToricVariety, NormalToricVariety),
        	TO (symbol ^**, NormalToricVariety, ZZ),
        	TO (smoothFanoToricVariety, ZZ, ZZ),
        	TO (normalToricVariety, Fan),
        	TO (normalToricVariety, Polyhedron)
    	    }@
	Text
	    Several methods for making new normal toric varieties from old
            ones are listed in the section on resolution of singularities.
    SeeAlso
        "finding attributes and properties"
        "resolving singularities"
	"working with toric maps"	
        "working with divisors"
        "working with sheaves"
///


doc /// 
    Key
        NormalToricVariety
    Headline 
        the class of all normal toric varieties
    Description
        Text  
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan.  In this package, the fan associated to a normal
            $d$-dimensional toric variety lies in the rational vector space
            $\QQ^d$ with underlying lattice $N = \ZZ^d$.  The fan is encoded
            by the minimal nonzero lattice points on its rays and the set of
            rays defining the maximal cones (a maximal cone is not properly
            contained in another cone in the fan).  More information about the
            correspondence between normal toric varieties and strongly convex
            rational polyhedral fans appears in Subsection 3.1 of
            Cox-Little-Schenck.
    Caveat 
	To sidestep edge case issues, this package excludes some normal toric
        varieties.  To avoid the $0$-dimensional vector space, all normal
        toric varieties in this package have positive dimension.  Similarly,
        to circumvent the empty set of rays, all normal toric varieties in
        this package have an orbit other than the dense algebraic 
    SeeAlso
        "making normal toric varieties"
        normalToricVariety
        (rays, NormalToricVariety)
        (max, NormalToricVariety)
        (expression, NormalToricVariety)
///


doc ///
    Key
        (expression, NormalToricVariety)
    Headline 
        get the expression used to format for printing
    Usage 
        expression X
    Inputs
        X:NormalToricVariety
    Outputs 
        : Expression
	    used to format {\tt X} for printing
    Description
        Text	    
            This function is primarily called by 
	    @TO2((symbol <<, Thing), "<<")@ to format printing.  It
	    displays the minimal nonzero lattice points on each ray and the
	    subsets of rays which determine the maximal cones in the fan.
    	Example
    	    toricProjectiveSpace 3
	    expression toricProjectiveSpace 3
    	    rays toricProjectiveSpace 3
    	    max toricProjectiveSpace 3
	Example
    	    hirzebruchSurface 7
	    expression hirzebruchSurface 7
    	    rays hirzebruchSurface 7
    	    max hirzebruchSurface 7
    	Text
  	    After assignment to a global variable {\em Macaulay2} knows the
            toric variety's name, and this name is used when printing.
    	Example  
    	    PP2 = toricProjectiveSpace 3
    	    expression PP2	    
    	    FF7 = hirzebruchSurface 7
	    expression FF7
    SeeAlso
        "finding attributes and properties"
    	(rays, NormalToricVariety)
    	(max, NormalToricVariety)
///		


doc ///
    Key
        (normalToricVariety, List, List)
        normalToricVariety 	
        [normalToricVariety, CoefficientRing]
        [normalToricVariety, Variable]
        [normalToricVariety, WeilToClass]
        WeilToClass
    Headline 
        make a normal toric variety
    Usage 
        normalToricVariety (rayList, coneList)
    Inputs
        rayList : List
            of lists of integers; each entry is the minimal nonzero lattice
            point on a ray in the fan
	coneList : List	     
     	    of lists of nonnegative integers; each entry indexes the rays
            defining a maximal cone in the fan
        CoefficientRing => Ring
            that determines the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        MinimalGenerators => Boolean 
            unused
        Variable => Symbol
	    that specifies the @TO2(baseName, "base name")@ for the indexed
	    variables in the total coordinate ring
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group	    
    Outputs
        : NormalToricVariety 
	    the normal toric variety determined by the fan
    Description
        Text	
            This is the main method for constructing a normal toric
            variety.  Almost all other constructors invoke it.
        Text
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.
            The fan is encoded by the minimal nonzero lattice points on its
            rays and the set of rays defining the maximal cones (meaning cones
            that are not proper subsets of another cone in the fan).  More
            precisely, {\tt rayList} lists the minimal nonzero lattice points
            on each ray (a.k.a. one-dimensional cone) in the fan. Each lattice
            point is a @TO2(List,"list")@ of @TO2(ZZ,"integers")@. The rays
            are ordered and indexed by nonnegative integers: $0,1,\dots,n$.
            Using this indexing, a maximal cone in the fan corresponds to a
            sublist of $\{0,1,\dots,n\}$.  All maximal cones are listed in
            {\tt coneList}.  More information explaining the correspondence
            between normal toric varieties and strongly convex rational
            polyhedral fans appears in Subsection 3.1 of Cox-Little-Schenck.
        Text
            The first example is projective plane blown up at two points.
        Example
            rayList = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}}
            coneList = {{0,1},{1,2},{2,3},{3,4},{0,4}}
    	    X = normalToricVariety (rayList, coneList)
            rays X
            max X
            dim X
	    assert (isWellDefined X and isProjective X and isSmooth X)
        Text
            The second example illustrates the data defining projective
            $4$-space.
        Example	    
            PP4 = toricProjectiveSpace 4;
            rays PP4
            max PP4
            dim PP4
            ring PP4
            PP4' = normalToricVariety (rays PP4, max PP4, CoefficientRing => ZZ/32003, Variable => y)
            ring PP4'
	    assert (isWellDefined PP4 and isProjective PP4 and isSmooth PP4)
        Text
            The optional argument {\tt WeilToClass} allows one to specify the
            map from the @TO2(weilDivisorGroup, "group of torus-invariant Weil
            divisors")@ to the @TO2(classGroup, "class group")@.  In
            particular, this allows the user to choose her favourite basis for
            the class group.  This map also determines the grading on the
            @TO2((ring, NormalToricVariety), "total coordinate ring")@ of the
            normal toric variety.  For example, we can choose the opposite
            generator for the class group of projective space as follows.
        Example  
            PP2 = toricProjectiveSpace 2;
            A = fromWDivToCl PP2
            source A == weilDivisorGroup PP2
            target A == classGroup PP2
            degrees ring PP2
            deg = matrix {toList (3:-1)}
            X = normalToricVariety (rays PP2, max PP2, WeilToClass => deg);
            A' = fromWDivToCl X
            source A' == weilDivisorGroup X
            target A' == classGroup X	  
            degrees ring X
            (matrix A') * (matrix rays X)
	    assert (isWellDefined X and isProjective X and isSmooth X)
        Text
            The integer matrix {\tt A} should span the kernel of the matrix
            whose columns are the minimal nonzero lattice points on the rays
            of the fan.

            We can also choose a basis for the class group of a blow-up of the
            projective plane such that the nef cone is the positive quadrant.
        Example  
            rayList = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}};
            coneList = {{0,1},{1,2},{2,3},{3,4},{0,4}};
            Y = normalToricVariety (rayList, coneList);
            fromWDivToCl Y
            nefGenerators Y
            deg = matrix{{1,-1,1,0,0},{0,1,-1,1,0},{0,0,1,-1,1}}
            Y' = normalToricVariety (rays Y, max Y, WeilToClass => deg);	  
            fromWDivToCl Y'
            nefGenerators Y'
	    assert (isWellDefined Y and isWellDefined Y')
    Caveat
        This method assumes that the lists {\tt rayList} and {\tt coneList}
        correctly encode a strongly convex rational polyhedral fan.  One can
        verify this by using @TO(isWellDefined, NormalToricVariety)@.
    SeeAlso 
        "making normal toric varieties"
        (rays, NormalToricVariety)
        (max ,NormalToricVariety)
        (isWellDefined, NormalToricVariety)
///


doc ///
    Key 
        (normalToricVariety, Matrix)
    	[normalToricVariety, MinimalGenerators]
    Headline
        make a normal toric variety from a polytope
    Usage 
        normalToricVariety VertMat
    Inputs
        VertMat : Matrix
	    of integers; each column is the lattice vertex of the polytope		     
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        MinimalGenerators => Boolean 
	    that specifies whether to compute minimal generators
    	Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
	    variables in the total coordinate ring
        WeilToClass => Matrix 
	    that specifies the map from the group of torus-invariant Weil
            divisors to the class group
    Outputs 
        : NormalToricVariety
            the normal toric variety determined by the polytope
    Description
        Text
            This method makes a @TO2(NormalToricVariety, "normal toric
            variety")@ from the polytope with vertices corresponding to the
            columns of the matrix {\tt VertMat}.  In particular, the
            associated fan is the INNER normal fan of the polytope.	    
    	Text  
            The first example shows how projective plane is obtained from
            a triangle.
        Example
            PP2 = normalToricVariety matrix {{0,1,0},{0,0,1}};
            rays PP2
            max PP2
            PP2' = toricProjectiveSpace 2;
            set rays PP2 === set rays PP2'
            max PP2 === max PP2'
	    assert (isWellDefined PP2 and isWellDefined PP2')
    	Text
            The second example makes the toric variety associated to the
            hypercube in $3$-space.
    	Example  
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
            transpose matrix rays X 
	    max X
	    assert (isWellDefined X and not isSimplicial X)
        Text
            The optional argument {\tt MinimalGenerators} specifics whether to
            compute the vertices of the polytope defined as the convex hull of
            the columns of the matrix {\tt VertMat}.
    	Example  	    
            FF1 = normalToricVariety matrix {{0,1,0,2},{0,0,1,1}};
	    assert isWellDefined FF1
            rays FF1
            max FF1 
            FF1' = hirzebruchSurface 1;
            assert (rays FF1 === rays FF1' and max FF1 === max FF1')
    	    VertMat = matrix {{0,0,1,1,2},{0,1,0,1,1}}
    	    notFF1 = normalToricVariety VertMat;
    	    max notFF1
    	    isWellDefined notFF1
    	    FF1'' = normalToricVariety (VertMat, MinimalGenerators => true);
    	    assert (rays FF1'' == rays FF1 and max FF1'' == max FF1)
	    assert isWellDefined FF1''
    SeeAlso
        "making normal toric varieties"
        (rays, NormalToricVariety)
        (max, NormalToricVariety)
        (isWellDefined, NormalToricVariety)
        (vertices, ToricDivisor)
        (latticePoints, ToricDivisor)
///


doc ///
    Key 
        (isWellDefined, NormalToricVariety)
    Headline 
        whether a toric variety is well-defined
    Usage
        isWellDefined X
    Inputs
        X : NormalToricVariety
    Outputs
        : Boolean
	    that is @TO true@ if the lists of rays and maximal cones
	    associated to {\tt X} determine a strongly convex rational
	    polyhedral fan
    Description
        Text
            A pair {\tt (rayList, coneList)} of lists correspond to a
            well-defined normal toric variety if the following conditions
            hold:
    	Text
    	    @UL {
                {"the union of the elements of ", TT "coneList", " equals the
          	    set of indices of elements of ", TT "rayList", ","},
                {"no element of ", TT "coneList", " is properly contained in
         	    another element of ", TT "coneList", ","},
                {"the rays indexed by an element of ", TT "coneList", "
         	    generate a strongly convex cone,"},
                {"the rays indexed by an element of ", TT "coneList", " are
         	    the unique minimal lattice points for the cone they
         	    generate,"},
                {"the intersection of the cones associated to two elements of ", 
		    TT "coneList", " is a face of each cone."}
    	    }@
	Text
            The first examples illustrate that small projective spaces are
            well-defined.
    	Example    
            assert all (5, d -> isWellDefined toricProjectiveSpace (d+1))
    	Text
            The second examples show that a randomly selected Kleinschmidt
            toric variety and a weighted projective space are also
            well-defined.
    	Example    
            setRandomSeed (currentTime ());
            a = sort apply (3, i -> random (7))
            assert isWellDefined kleinschmidt (4,a)
	Example
            q = sort apply (5, j -> random (1,9));
            while not all (subsets (q,#q-1), s -> gcd s === 1) do q = sort apply (5, j -> random (1,9));
            q
            assert isWellDefined weightedProjectiveSpace q
    	Text
            The next ten examples illustrate various ways that two lists can
            fail to define a normal toric variety.  By making the current
            debugging level greater than one, one gets some addition
            information about the nature of the failure.
    	Example    
	    X = new MutableHashTable;
            coneList = max toricProjectiveSpace 2;
            X#1 = normalToricVariety ({{-1,-1},{1,0},{0,1},{-1,0}}, coneList);
            isWellDefined X#1
            debugLevel = 1;
            isWellDefined X#1	  	  
    	    X#2 = normalToricVariety ({},{});
	    isWellDefined X#2
    	    X#3 = normalToricVariety ({{}},{});
	    isWellDefined X#3	    	    
    	    X#4 = normalToricVariety ({{}},{{}});
	    isWellDefined X#4
            coneList' = {{0,1},{0,3},{1,2},{2,3},{3}};
            X#5 = normalToricVariety ({{-1,0},{0,-1},{1,-1},{0,1}}, coneList');
            isWellDefined X#5
            X#6 = normalToricVariety ({{-1,-1},{1,0},{0,1,1}},coneList);
            isWellDefined X#6
            X#7 = normalToricVariety ({{-1,-1/1},{1,0},{0,1}},coneList);
            isWellDefined X#7
            X#8 = normalToricVariety ({{1,0},{0,1},{-1,0}},{{0,1,2}});
            isWellDefined X#8
            X#9 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,2}},{{0,1,2}});
            isWellDefined X#9
            X#10 = normalToricVariety ({{1,0},{0,1},{1,1}},{{0,1},{1,2}});
            isWellDefined X#10
	    debugLevel = 0;
	    assert all (keys X, k -> not isWellDefined X#k)
    	Text
            This method also checks that the following aspects of the data structure:
	Text
    	    @UL {
	        {"the underlying ", TO HashTable, " has the expected keys,
	    	    namely ", TT "rays", ", ", TT "max", ", and ", TT "cache",
	    	    ","},
       	        {"the value of the ", TT "rays", " key is a ", TO List, ","},
	        {"each entry in the ", TT "rays", " list is a ", TO List,
	            ","},
	        {"each entry in an entry of the ", TT "rays", " list is an ",
	            TO ZZ, ","},
                {"each entry in the ", TT "rays", " list as the same number of
                    entries,"},
	        {"the value of the ", TT "max", " key is a ", TO List, ","},	
                {"each entry in the ", TT "max", " list is a ", TO List, ","},
                {"each entry in an entry of the ", TT "max", " list is an ",
                    TO ZZ, ","},
                {"each entry in an entry of the ", TT "max", " list
                    corresponds to a ray,"},
                {"the value of the ", TT "cache", " key is a ", TO CacheTable,
                    "."}
	    }@
    SeeAlso
        "making normal toric varieties"
        (normalToricVariety, List, List)
	"debugLevel"
///


------------------------------------------------------------------------------
-- More advanced constructors
------------------------------------------------------------------------------
doc ///
    Key
        (affineSpace, ZZ)
	affineSpace
        [affineSpace,CoefficientRing]
        [affineSpace,Variable]
    Headline 
        make an affine space as a normal toric variety
    Usage 
        affineSpace d
    Inputs 
        d : ZZ
	    a positive integer
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring
    Outputs 
        : NormalToricVariety 
	    that is affine $d$-space
    Description
        Text
            Affine $d$-space is a smooth normal toric variety.  The rays are
            generated by the standard basis $e_1, e_2, \dots, e_d$ of $\ZZ^d$,
            and the maximal cone in the fan correspond to the $d$-element
            subsets of $\{ 0, 1, \dots, d-1 \}$.
    	Text
            The examples illustrate the affine line and affine $3$-space.
    	Example  
    	    AA1 = affineSpace 1;
    	    rays AA1
    	    max AA1
    	    dim AA1
    	    assert (isWellDefined AA1 and not isComplete AA1 and isSmooth AA1)
	Example
            AA3 = affineSpace (3, CoefficientRing => ZZ/32003, Variable => y);
    	    rays AA3
    	    max AA3
    	    dim AA3
    	    ring AA3
    	    assert (isWellDefined AA3 and not isComplete AA3 and isSmooth AA3)
    SeeAlso
        "making normal toric varieties"
        (isSmooth, NormalToricVariety)
        (isComplete, NormalToricVariety)
        (makeSmooth, NormalToricVariety)
///   


doc ///
    Key
        "projective space"
    Headline 
        information about various constructions of projective space
    Description
        Text
	    There are several different methods for creating projective
	    $n$-space in {\it Macaulay2}.   
    	Text
	    To generate projective space as a @TO ProjectiveVariety@,
	    we use the Proj-construction.
	Example
	    X0 = Proj (QQ[x_0..x_3])
	    assert (3 === dim X0)
	    ring X0
	    hilbertPolynomial(X0, Projective => false)
	    for d to 10 list rank HH^0 (OO_X0(d))
    	Text
	    To work with projective space as a @TO NormalToricVariety@,
	    we simply use the method @TO toricProjectiveSpace@.
	Example
	    X1 = toricProjectiveSpace 3
	    assert (3 === dim X1)
	    rays X1
	    max X1
	    ring X1
	    intersectionRing X1
	    hilbertPolynomial (X1)
	    for d to 10 list rank HH^0 (X1, OO_X1(d)) 
	    assert (X0 =!= X1)
    	Text
	    To manipulate projective space as an @TO AbstractVariety@,
	    we employ the method @TO abstractProjectiveSpace@.
	Example
	    X2 = abstractProjectiveSpace (3, base(symbol i))
	    assert (3 === dim X2)
    	    intersectionRing X2
	    chi (OO_X2(i))
	    assert (X2 =!= X0)
	    assert (X2 =!= X1)	    	    
	Text	    
	    If you prefer a shorter name for your favourite method of
	    constructing projective space, then make one.
	Example
	    projectiveSpace = n -> Proj (QQ[x_0..x_n]);
	    projectiveSpace 2
	Example	    
	    PP = toricProjectiveSpace;	    
	    PP 2
    SeeAlso
        "making normal toric varieties"
        Proj
        toricProjectiveSpace
        abstractProjectiveSpace	
///


doc ///
    Key 
        (toricProjectiveSpace, ZZ)
	toricProjectiveSpace
        [toricProjectiveSpace,CoefficientRing]
        [toricProjectiveSpace,Variable]
    Headline 
        make a projective space as a normal toric variety
    Usage 
        toricProjectiveSpace d
    Inputs 
        d : ZZ
	    a positive integer
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring
    Outputs 
        : NormalToricVariety 	    
            that is projective $d$-space
    Description
        Text	    
            Projective $d$-space is a smooth complete normal toric variety.
            The rays are generated by the standard basis $e_1, e_2, \dots,e_d$
            of $\ZZ^d$ together with vector $-e_1-e_2-\dots-e_d$.  The maximal
            cones in the fan correspond to the $d$-element subsets of $\{ 0,1,
            \dots,d\}$.
    	Text
  	    The examples illustrate the projective line and projective $3$-space.
	Example
    	    PP1 = toricProjectiveSpace 1;
    	    rays PP1
    	    max PP1
    	    dim PP1
    	    ring PP1
    	    ideal PP1
    	    assert (isWellDefined PP1 and isSmooth PP1 and isComplete PP1)
	Example	    
    	    PP3 = toricProjectiveSpace (3, CoefficientRing => ZZ/32003, Variable => y);
    	    rays PP3
    	    max PP3
    	    dim PP3
    	    ring PP3
    	    ideal PP3
    	    assert (isWellDefined PP3 and isSmooth PP3 and isComplete PP3)    
    SeeAlso
        "making normal toric varieties"
    	(isComplete, NormalToricVariety)
        (isSmooth, NormalToricVariety)
        (ring, NormalToricVariety)
        (ideal, NormalToricVariety)
///


doc ///
    Key
        (weightedProjectiveSpace, List)
	weightedProjectiveSpace 
        [weightedProjectiveSpace,CoefficientRing]
        [weightedProjectiveSpace,Variable]
    Headline 
        make a weighted projective space
    Usage 
        weightedProjectiveSpace q
    Inputs 
        q:List
            of relatively prime positive integers
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring	
    Outputs 
        : NormalToricVariety 
	    that is a weighted projective space
    Description
        Text
	    The weighted projective space associated to a list $\{ q_0, q_1,
            \dots, q_d \}$, where no $d$-element subset of $q_0, q_1, \dots,
            q_d$ has a nontrivial common factor, is a projective simplicial
            normal toric variety built from a fan in $N = \ZZ^{d+1}/\ZZ(q_0,
            q_1, \dots,q_d)$.  The rays are generated by the images of the
            standard basis for $\ZZ^{d+1}$, and the maximal cones in the fan
            correspond to the $d$-element subsets of $\{ 0, 1, ..., d \}$.
	    A weighted projective space is typically not smooth.
    	Text  
  	    The first examples illustrate the defining data for three
            different weighted projective spaces.
    	Example
            PP4 = weightedProjectiveSpace {1,1,1,1};
            rays PP4
            max PP4
            dim PP4
	    assert (isWellDefined PP4 and isProjective PP4 and isSmooth PP4)
	Example
            X = weightedProjectiveSpace {1,2,3};
            rays X
            max X
            dim X
            ring X
	    assert (isWellDefined X and isProjective X and isSimplicial X and not isSmooth X)
	Example
            Y = weightedProjectiveSpace ({1,2,2,3,4}, CoefficientRing => ZZ/32003, Variable => y);
            rays Y
            max Y
            dim Y
            ring Y
	    assert (isWellDefined Y and isProjective Y and isSimplicial Y and not isSmooth Y)
    	Text
            The grading of the total coordinate ring for weighted projective
            space is determined by the weights.  In particular, the class
            group is $\ZZ$.
	Example
            classGroup PP4
            degrees ring PP4
            classGroup X
            degrees ring X
            classGroup Y
            degrees ring Y
    SeeAlso
        "making normal toric varieties"
        (toricProjectiveSpace, ZZ)
        (ring, NormalToricVariety)
        (classGroup, NormalToricVariety)
        (isSimplicial, NormalToricVariety)
        (isSmooth, NormalToricVariety)
///	
	

doc ///
    Key
        (hirzebruchSurface,ZZ)
	hirzebruchSurface
    	[hirzebruchSurface,CoefficientRing]
    	[hirzebruchSurface,Variable]
    Headline
        make any Hirzebruch surface
    Usage 
        hirzebruchSurface a
    Inputs
        a:ZZ
	    that determines which Hirzebruch surface
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring
    Outputs 
        : NormalToricVariety 
	    that is a Hirzebruch surface
    Description
        Text
            The $a$-{th} Hirzebruch surface is a smooth projective normal toric
            variety.  It can be defined as the $\PP^1$-bundle over $X = \PP^1$
            associated to the sheaf ${\mathcal O}_X(0) \oplus {\mathcal
            O}_X(a)$.  It is also the quotient of affine $4$-space by a rank
            two torus.  
	Example
            FF3 = hirzebruchSurface 3;
            rays FF3
            max FF3
            dim FF3
            ring FF3
            degrees ring FF3
            ideal FF3
	    assert (isWellDefined FF3 and isProjective FF3 and isSmooth FF3)
    	Text
            When $a = 0$, we obtain $\PP^1 \times \PP^1$.
      	Example
            FF0 = hirzebruchSurface (0, CoefficientRing => ZZ/32003, Variable => y);
            rays FF0
            max FF0
            ring FF0
            degrees ring FF0
            I = ideal FF0
            decompose I
	    assert (isWellDefined FF0 and isProjective FF3 and isSmooth FF3)	    
    	Text
            The map from the @TO2(weilDivisorGroup, "group of torus-invariant
            Weil divisors")@ to the @TO2(classGroup, "class group")@ is chosen
            so that the positive orthant corresponds to the cone of nef line
            bundles.
	Example
	    nefGenerators FF3
    	    nefGenerators FF0	    
    SeeAlso
        "making normal toric varieties"
    	(ring,NormalToricVariety)
	nefGenerators
///	     


doc ///
    Key 
    	(kleinschmidt, ZZ, List)
        kleinschmidt	
    	[kleinschmidt,CoefficientRing]
    	[kleinschmidt,Variable]
    Headline 
        make any smooth normal toric variety having Picard rank two
    Usage 
        kleinschmidt (d, a)
    Inputs
        d : ZZ 
	    that specifies the dimension of toric variety
        a : List
	    that is nondecreasing and consists of at most $d-1$ nonnegative integers
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the 
	     @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring		
    Outputs 
        : NormalToricVariety 
	    a smooth toric variety with Picard rank two
    Description
        Text
            Peter Kleinschmidt constructs (up to isomorphism) all smooth
            normal toric varieties with dimension $d$ and $d+2$ rays; see
            Kleinschmidt's "A classification of toric varieties with few
            generators" {\em Aequationes Mathematicae}, {\bf 35} (1998)
            254-266.
    	Text   
            When $d = 2$, we obtain a variety isomorphic to a Hirzebruch
            surface.  By permuting the indexing of the rays and taking
	    an automorphism of the lattice, we produce an explicit 
	    isomorphism.
    	Example  
            X = kleinschmidt (2,{3});
            rays X
            max X
            FF3 = hirzebruchSurface 3;
            rays FF3
            max FF3
    	    permutingRays = matrix {{0,0,0,1},{0,1,0,0},{1,0,0,0},{0,0,1,0}}
	    latticeAutomorphism = matrix {{0,1},{1,0}}
	    assert (latticeAutomorphism * (matrix transpose rays X) * permutingRays == matrix transpose rays FF3)
    	Text
            The normal toric variety associated to the pair $(d,a)$ is Fano if
            and only if $\sum_{i=0}^{r-1} a_i < d-r+1$.
    	Example  
    	    X1 = kleinschmidt (3, {0,1});	  
    	    isFano X1
    	    X2 = kleinschmidt (4, {0,0});	  
    	    isFano X2
    	    ring X2
    	    X3 = kleinschmidt (9, {1,2,3}, CoefficientRing => ZZ/32003, Variable => y);
    	    isFano X3
    	    ring X3
    	Text
            The map from the @TO2(weilDivisorGroup, "group of torus-invariant
            Weil divisors")@ to the @TO2(classGroup, "class group")@ is chosen
            so that the positive orthant corresponds to the cone of nef line
            bundles.
	Example
	    nefGenerators X
    	    nefGenerators X1
    	    nefGenerators X2
    	    nefGenerators X3	    	    
    SeeAlso
        "making normal toric varieties"
        normalToricVariety
	hirzebruchSurface
///	


------------------------------------------------------------------------------
-- Products of normal toric varieties
------------------------------------------------------------------------------
doc ///
    Key
        (components, NormalToricVariety)
    Headline 
        list the factors in a product
    Usage 
        components X
    Inputs 
        X : NormalToricVariety
    Outputs
        : List
	    of the factors from which the product was form or just {\tt \{X\}}
	    if {\tt X} was not formed by a Cartesian product
    Description
        Text	    
            The Cartesian product of varieties $X_0, X_1, X_2, ...$, all
            defined over the same ground field $k$, is the fiber product 
	    $X_0 \times_k X_1 \times_k X_2 \times_k ...$.  For normal toric
            varieties, the fan of the product is given by the Cartesian
            product of the underlying fans of the factors.
    	Example 
	    X = toricProjectiveSpace 1;
	    Y = toricProjectiveSpace 2;
	    Z = toricProjectiveSpace 3;	    	    
	    Seq = (X, Y, Z);
	    P = cartesianProduct Seq;
	    dim P
	    assert (dim P == 1+2+3)
	Text
	    The factors are cached and can be accessed with @TO components@.
	Example
    	    factors = components P
	    # factors
    	    assert (factors#0 === X and factors#1 === Y and factors#2 === Z)	    
    SeeAlso
	(cartesianProduct, Sequence)
        (symbol **, NormalToricVariety, NormalToricVariety)
	components
///	


doc ///
    Key
        (cartesianProduct, Sequence)
	(cartesianProduct, NormalToricVariety)
	cartesianProduct
    Headline 
        make the Cartesian product of normal toric varieties
    Usage 
        cartesianProduct Seq
    Inputs 
        Seq : Sequence
	    all of whose elements are normal toric varieties; a single normal
	    toric variety is treated as a sequence with just one element
    Outputs
        : NormalToricVariety
	    the product of elements
    Description
        Text	    
            The Cartesian product of varieties $X_0, X_1, X_2, \dots$, all
            defined over the same ground field $k$, is the fiber product 
	    $X_0 \times_k X_1 \times_k X_2 \times_k \dots$.  For normal toric
            varieties, the fan of the product is given by the Cartesian
            product of the underlying fans of the factors.
    	Example 
	    X = toricProjectiveSpace 1;
	    Y = toricProjectiveSpace 2;
	    Z = toricProjectiveSpace 3;	    	    
	    Seq = (X, Y, Z);
	    P = cartesianProduct Seq;
	    dim P
	    assert (dim P == 1+2+3)
    	    factors = components P
	    # factors
    	    assert (factors#0 === X and factors#1 === Y and factors#2 === Z)		  
	Text
	    This general method for constructing products is invoked by all 
	    of the other product constructors.	    	    
    SeeAlso
        "making normal toric varieties"
	(symbol **, NormalToricVariety, NormalToricVariety)
        (symbol ^**, NormalToricVariety, ZZ)
        normalToricVariety
///	


doc ///
    Key
        (symbol **, NormalToricVariety, NormalToricVariety)
    Headline 
        make the Cartesian product of two normal toric varieties
    Usage 
        X ** Y
    Inputs 
        X : NormalToricVariety
	Y : NormalToricVariety
    Outputs
        : NormalToricVariety
	    the product of {\tt X} and {\tt Y}
    Description
        Text	    
            The Cartesian product of two varieties $X$ and $Y$, both defined
            over the same ground field $k$, is the fiber product $X \times_k
            Y$.  For normal toric varieties, the fan of the product is given
            by the Cartesian product of each pair of cones in the fans of the
            factors.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
            FF2 = hirzebruchSurface 2;
            X = FF2 ** PP2;
            assert (# rays X == # rays FF2 + # rays PP2)
            assert (matrix rays X == matrix rays FF2 ++ matrix rays PP2)
            primaryDecomposition ideal X 
	    flatten (primaryDecomposition \ {ideal FF2,ideal PP2})
    	Text
  	    The map from the torus-invariant Weil divisors to the class group
            is the direct sum of the maps for the factors.
      	Example
    	    assert (fromWDivToCl FF2 ++ fromWDivToCl PP2 == fromWDivToCl X)
	Text
	    The factors are cached and can be accessed with @TO components@.
	Example	    	    
	    factors = components X
	    assert (# factors === 2)
	    assert (factors#0 === FF2)
	    assert (factors#1 === PP2)
    SeeAlso
        "making normal toric varieties"
	(cartesianProduct, Sequence)
	(components, NormalToricVariety)
        (symbol ^**, NormalToricVariety, ZZ)
        normalToricVariety
///	


doc ///
    Key 
        (symbol ^**, NormalToricVariety, ZZ)
    Headline
        make the Cartesian power of a normal toric variety
    Usage 
        X ^** i
    Inputs
        X : NormalToricVariety
        i : ZZ
	    that is positive
    Outputs 
        : NormalToricVariety
	    the {\tt i}-ary Cartesian product of {\tt X} with itself
    Description
        Text
            The $i$-ary Cartesian product of the variety $X$, defined over the
            ground field $k$, is the $i$-ary fiber product of $X$ with itself
            over $k$.  For a normal toric variety, the fan of the $i$-ary
            Cartesian product is given by the $i$-ary Cartesian product of the
            cones.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    X = PP2 ^** 4;
    	    fromWDivToCl X
	Text
	    The factors are cached and can be accessed with @TO components@.
	Example	 
	    factors = components X
    	    assert (# factors === 4)
	    assert all (4, i -> factors#i === PP2)
	Example
    	    FF2 = hirzebruchSurface (2);
    	    Y = FF2 ^** 3;
    	    fromWDivToCl Y
	Example
    	    X' = PP2 ** PP2;
            X'' = PP2 ^** 2;
            assert (rays X' == rays X'' and  max X' == max X'')
    SeeAlso
        "making normal toric varieties" 
        (symbol **, NormalToricVariety, NormalToricVariety)
	(components, NormalToricVariety)
        normalToricVariety
///	


------------------------------------------------------------------------------
-- Databases of some normal toric varieties
------------------------------------------------------------------------------
doc /// 
    Key 
        (smoothFanoToricVariety, ZZ, ZZ)
        smoothFanoToricVariety	
    	[smoothFanoToricVariety, CoefficientRing]
    	[smoothFanoToricVariety, Variable]
    Headline 
        get a smooth Fano toric variety from database
    Usage 
        smoothFanoToricVariety (d,i)
    Inputs
        d : ZZ 
	    equal to dimension of toric variety
        i : ZZ 
	    indexing a normal toric variety in database
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the 
	     @TO2((ring, NormalToricVariety), "total coordinate ring")@
        Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
            variables in the total coordinate ring	    
    Outputs 
        : NormalToricVariety 
	    a smooth Fano toric variety
    Description
        Text

            This function accesses a database of all smooth Fano toric
            varieties of dimension at most $6$.  The enumeration of the toric
            varieties follows Victor V. Batyrev's classification ( "On the
            classification of toric Fano", {\em Journal of Mathematical
            Sciences (New York)}, {\bf 94} (1999) 1021-1050,
            @HREF("http://arxiv.org/abs/math/9801107", "arXiv:math/9801107v2")@ 
	    and Hiroshi Sato's "Toward the classification of
	    higher-dimensional toric Fano varieties", {\em The Tohoku
	    Mathematical Journal. Second Series}, {\bf 52} (2000) 383-413,
	    @HREF("http://arxiv.org/abs/math/9911022", "arXiv:math/9011022")@)
            for dimension at most $4$ and Mikkel Øbro's classification ( "An
            algorithm for the classification of smooth Fano polytopes"
	    @HREF("http://arxiv.org/abs/0704.0049", "arXiv:math/0704.0049v1")@)
            for dimensions $5$ and $6$.  
	Text
	    There is a unique smooth Fano toric curve, five smooth Fano toric
            surfaces, eighteen smooth Fano toric threefolds, $124$ smooth Fano
            toric fourfolds, $866$ smooth Fano toric fivefolds, and $7622$
            smooth Fano toric sixfolds.
        Text
            For all $d$, {\tt smoothFanoToricVariety (d,0)} yields projective
            $d$-space.	    
      	Example
      	    PP1 = smoothFanoToricVariety (1,0);
            assert (rays PP1 === rays toricProjectiveSpace 1)
            assert (max PP1 === max toricProjectiveSpace 1)
    	    PP4 = smoothFanoToricVariety (4,0, CoefficientRing => ZZ/32003, Variable => y);
            assert (rays PP4 === rays toricProjectiveSpace 4)
            assert (max PP4 === max toricProjectiveSpace 4)	    
	Text
            The following example was missing from Batyrev's table.
	Example
            W = smoothFanoToricVariety (4,123);
            rays W
            max W
	Text
	    @SUBSECTION "Acknowledgements"@
    	Text
            We thank @HREF("http://homepages.warwick.ac.uk/staff/G.Brown/",
            "Gavin Brown")@ and
            @HREF("http://magma.maths.usyd.edu.au/users/kasprzyk/","Alexander
            Kasprzyk")@ for their help extracting the data for the smooth Fano
            toric five and sixfolds from their @HREF("http://www.grdb.co.uk",
            "Graded Rings Database")@.
    SeeAlso
        "making normal toric varieties"
        normalToricVariety
        (isFano,NormalToricVariety)
///	


------------------------------------------------------------------------------
-- Methods that interface with the 'Polyhedra' package 
------------------------------------------------------------------------------
doc ///
    Key
        (normalToricVariety, Fan)
    Headline 
        make a normal toric variety from a 'Polyhedra' fan
    Usage 
        normalToricVariety F
    Inputs
        F : Fan
        CoefficientRing => Ring
            that determines the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        MinimalGenerators => Boolean 
            unused
        Variable => Symbol
	    that specifies the @TO2(baseName, "base name")@ for the indexed
	    variables in the total coordinate ring
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group		
    Outputs
        : NormalToricVariety
	    determined by the rational strongly convex polyhedral fan
    Description
        Text	    
            This method makes a @TO NormalToricVariety@ from a 
	    @TO "Polyhedra::Fan"@ as implemented in the 
	    @TO "Polyhedra::Polyhedra"@ package.
    	Example  
            F = faceFan convexHull (id_(ZZ^3) | -id_(ZZ^3))
    	    rays F
	    maxCones F
    	    X = normalToricVariety F;
    	    assert (transpose matrix rays X == rays F and max X == sort maxCones F)
    	Text
  	    The recommended method for creating a @TO NormalToricVariety@ from
            a fan is @TO (normalToricVariety,List,List)@.  In fact, this
            package avoids using objects from the @TO "Polyhedra::Polyhedra"@
            package whenever possible.   Here is a trivial example, namely
            projective 2-space, illustrating the substantial increase in time
            resulting from the use of a @TO "Polyhedra::Polyhedra"@ fan.
      	Example
            X1 = time normalToricVariety ({{-1,-1},{1,0},{0,1}}, {{0,1},{1,2},{0,2}})
            X2 = time normalToricVariety fan {posHull matrix {{-1,1},{-1,0}}, posHull matrix {{1,0},{0,1}}, posHull matrix{{-1,0},{-1,1}}};
	    assert (sort rays X1 == sort rays X2 and max X1 == max X2)
    SeeAlso
        "making normal toric varieties"
        normalToricVariety
///	


doc ///
    Key 
        (normalToricVariety, Polyhedron)
    Headline 
        make a normal toric variety from a 'Polyhedra' polyhedron
    Usage 
        normalToricVariety P
    Inputs
        P : Polyhedron
	    whose vertices are lattice points
        CoefficientRing => Ring
            that determines the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
        MinimalGenerators => Boolean 
            unused
        Variable => Symbol
	    that specifies the @TO2(baseName, "base name")@ for the indexed
	    variables in the total coordinate ring
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group		    
    Outputs
        : NormalToricVariety
	    determined by the lattice polytope
    Description
        Text
            This method makes a @TO NormalToricVariety@ from a @TO
            "Polyhedra::Polyhedron"@ as implemented in the @TO
            "Polyhedra::Polyhedra"@ package.  In particular, the associated
            fan is inner normal fan to the polyhedron.
       	Example
            P = convexHull (id_(ZZ^3) | -id_(ZZ^3));
	    fVector P
	    vertices P
            X = normalToricVariety P;
            rays X
            max X
	    picardGroup X
	Text
	    When the polyhedron is not full-dimensional, restricting to the
	    smallest linear subspace that contains the polyhedron guarantees
	    that normal fan is strongly convex.
	Example
	    P = convexHull transpose matrix unique permutations {1,1,0,0};
	    assert not isFullDimensional P
	    fVector P
	    X = normalToricVariety P;
	    assert (dim P === dim X)
	    rays X
	    max X
	    assert (8 === #rays X)
    	    assert (6 === #max X)
	    picardGroup X
    	Text
            The recommended method for creating a @TO NormalToricVariety@ from
            a polytope is @TO (normalToricVariety, Matrix)@.  In fact, this
            package avoids using objects from the @TO "Polyhedra::Polyhedra"@
            whenever possible.  Here is a trivial example, namely projective
            2-space, illustrating the increase in time resulting from the use
            of a @TO "Polyhedra::Polyhedra"@ polyhedron.	    
	Example
    	    vertMatrix = matrix {{0,1,0},{0,0,1}}
            X1 = time normalToricVariety convexHull (vertMatrix);
            X2 = time normalToricVariety vertMatrix;
	    assert (set rays X2 === set rays X1 and max X1 === max X2)
    SeeAlso
        "making normal toric varieties"
        (normalToricVariety, Matrix)
///

     
doc ///     
    Key 
        (fan, NormalToricVariety)
    Headline
        make the 'Polyhedra' fan associated to the normal toric variety
    Usage 
        fan X
    Inputs 
         X : NormalToricVariety
    Outputs
        : Fan
	    the underlying fan of the normal toric variety
    Description
        Text
            This methods returns the @TO Polyhedra@ fan associated to a normal
            toric variety.  
    	Example  
    	    PP3 = toricProjectiveSpace 3;
    	    F1 = fan PP3
    	    rays F1
    	    maxCones F1
    	    assert (set rays PP3 === set rays normalToricVariety F1 and max PP3 === max normalToricVariety F1)
    	Example
    	    F2 = fan hirzebruchSurface 3;
    	    rays F2
    	    maxCones F2
    SeeAlso
        "finding attributes and properties"
	(normalToricVariety, Fan)
///		


------------------------------------------------------------------------------
-- basic properties and invariants
------------------------------------------------------------------------------
doc ///
    Key 
        "finding attributes and properties"
    Headline
        information about accessing features of a normal toric variety
    Description
        Text
            Having made a @TO NormalToricVariety@, one can access its basic
            invariants or test for some elementary properties by using the
            following methods:
    	Text
	    @SUBSECTION "Determining attributes and properties of normal toric varieties"@
	Text
            @UL {
        	TO (rays, NormalToricVariety),
        	TO (max, NormalToricVariety),    
        	TO (expression, NormalToricVariety),    
    		TO (dim, NormalToricVariety),
    		TO (orbits, NormalToricVariety, ZZ),
    		TO (isDegenerate, NormalToricVariety),
    		TO (isSimplicial, NormalToricVariety),
    		TO (isSmooth, NormalToricVariety),
    		TO (isComplete, NormalToricVariety),
    		TO (isProjective, NormalToricVariety),
    		TO (isFano, NormalToricVariety),
    		TO (fan, NormalToricVariety)
	    }@
    SeeAlso
        "making normal toric varieties"
        "resolving singularities"
	"working with toric maps"	
        "working with divisors"
        "working with sheaves"	
///


doc ///
    Key 
        (rays, NormalToricVariety)
    Headline 
        get the rays of the associated fan
    Usage 
        rays X
    Inputs 
        X:NormalToricVariety
    Outputs
        :List
	    of lists of integers; each entry corresponds to a minimal nonzero
            lattice point on the ray in the underlying fan
    Description
        Text
	    A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N =
            {\ZZ}^d$.  As a result, each ray in the fan is determined by the
            minimal nonzero lattice point it contains.  Each such lattice
            point is given as a @TO2(List, "list")@ of $d$ @TO2(ZZ,
            "integers")@.  
	Text
            The examples show the rays for the projective plane, 
 	    @TO2(toricProjectiveSpace, "projective $3$-space")@, a
            @TO2(hirzebruchSurface, "Hirzebruch surface")@, and a 
	    @TO2(weightedProjectiveSpace, "weighted projective space")@.	    
            There is a canonical bijection between the rays and
            torus-invariant Weil divisor on the toric variety.
	Example
    	    PP2 = toricProjectiveSpace 2;
            rays PP2
            dim PP2
            weilDivisorGroup PP2
	    PP2_0
	Example
            PP3 = toricProjectiveSpace 3;
            rays PP3
            dim PP3
            weilDivisorGroup PP3
	Example
            FF7 = hirzebruchSurface 7;
            rays FF7
            dim FF7
            weilDivisorGroup FF7
	Example
            X = weightedProjectiveSpace {1,2,3};
            rays X
            weilDivisorGroup X
    	Text
             When the normal toric variety is nondegenerate, the number of
             rays equals the number of variables in the total coordinate ring.
    	Example  
             #rays X == numgens ring X
    	Text
            In this package, an ordered list of the minimal nonzero lattice
            points on the rays in the fan is part of the defining data of a
            toric variety, so this method does no computation.
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
        (max, NormalToricVariety)
        (ring, NormalToricVariety)
///	


doc ///
    Key 
        (max, NormalToricVariety)
    Headline 
        get the maximal cones in the associated fan
    Usage 
        max X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : List
	    of lists of nonnegative integers; each entry indexes the rays
            that generate a maximal cone in the fan
    Description
        Text
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.
            The fan is encoded by the minimal nonzero lattice points on its
            rays and the set of rays defining the maximal cones (where a
            maximal cone is not properly contained in another cone in the
            fan).  The rays are ordered and indexed by nonnegative integers:
            $0, 1, \dots, n-1$.  Using this indexing, a maximal cone in the fan
            corresponds to a sublist of $\{ 0, 1, \dots, n-1 \}$; the entries
            index the rays that generate the cone.
    	Text
            The examples show the maximal cones for the projective line,
            @TO2(toricProjectiveSpace, "projective $3$-space")@, a
            @TO2(hirzebruchSurface, "Hirzebruch surface")@, and a 
	    @TO2(weightedProjectiveSpace, "weighted projective space")@.
    	Example  
    	    PP1 = toricProjectiveSpace 1;
            # rays PP1
            max PP1
    	Example
            PP3 = toricProjectiveSpace 3;
            # rays PP3
            max PP3
    	Example
            FF7 = hirzebruchSurface 7;
            # rays FF7
            max FF7
    	Example
            X = weightedProjectiveSpace {1,2,3};
            # rays X
            max X
    	Text
  	    In this package, a list corresponding to the maximal cones in the
            fan is part of the defining data of a normal toric variety, so
            this method does no computation.
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
        (rays, NormalToricVariety)
///	


doc ///
    Key 
        (dim, NormalToricVariety)
    Headline 
        get the dimension of a normal toric variety
    Usage 
        dim X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : ZZ 
	    the dimension of the normal toric variety
    Description
        Text
            The dimension of a normal toric variety equals the dimension of
            its dense algebraic torus.  In this package, the fan associated to
            a normal $d$-dimensional toric variety lies in the rational vector
            space $\QQ^d$ with underlying lattice $N = \ZZ^d$.  Hence, the
            dimension simply equals the number of entries in a minimal nonzero
            lattice point on a ray.
        Text
            The following examples illustrate normal toric varieties of
            various dimensions.
    	Example  
            dim toricProjectiveSpace 1
	    dim affineSpace 2
            dim toricProjectiveSpace 5
            dim hirzebruchSurface 7
            dim weightedProjectiveSpace {1,2,2,3,4}
            X = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}})
            dim X
            isDegenerate X
    	Text
	    In this package, number of entries in any ray equals the dimension
	    of both the underlying lattice and the normal toric variety, so
	    this method does essentially no computation.
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
///	
 
 
doc ///
    Key
        (isDegenerate, NormalToricVariety)
        isDegenerate	
    Headline 
        whether a toric variety is degenerate
    Usage 
        isDegenerate X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Boolean 
 	    that is @TO true@ if the fan of {\tt X} is contained in a proper
	    linear subspace of its ambient space
    Description
        Text	
            A $d$-dimensional normal toric variety is degenerate if its rays
            do not span $\QQ^d$.  For example, projective spaces and
            Hirzebruch surfaces are not degenerate.
    	Example  
            assert not isDegenerate toricProjectiveSpace 3
            assert not isDegenerate hirzebruchSurface 7
    	Text
            Although one typically works with non-degenerate toric varieties,
            not all normal toric varieties are non-degenerate.
    	Example  
            U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
            isDegenerate U
    Caveat
        Many routines in this package, such as the
	@TO2((ring, NormalToricVariety), "total coordinate ring")@,
	require the normal toric variety to be non-degenerate.
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
///


doc ///
    Key
        (isSimplicial, NormalToricVariety)
    Headline
        whether a normal toric variety is simplicial
    Usage
        isSimplicial X
    Inputs
        X : NormalToricVariety
    Outputs
        :Boolean 
	    that is @TO true@ if the minimal nonzero lattice points on the
            rays in each maximal cone in the associated fan of form part of a
            $\QQ$-basis
    Description
    	Text		
            A normal toric variety is simplicial if every cone in its fan is
            simplicial and a cone is simplicial if its minimal generators are
            linearly independent over $\QQ$.  In fact, the following
            conditions on a normal toric variety $X$ are equivalent:
    	Text
    	    @UL {
        	{EM "X", " is simplicial,"},
        	{"every torus-invariant Weil divisor on ", EM "X",
         	    " has a positive integer multiple that is Cartier,"},
        	{"the Picard group of ", EM "X", " has finite index in
         	    the class group of ", EM "X", ","},
                {EM "X", " has only finite quotient singularities."}
	    }@
	Text
	    For more information, see Proposition 4.2.7 in
	    Cox-Little-Schenck's {\em Toric Varieties}.	    
	Text	
            @TO2(toricProjectiveSpace, "Projective spaces")@, 
	    @TO2(weightedProjectiveSpace, "weighted projective spaces")@, and 
	    @TO2(hirzebruchSurface, "Hirzebruch surfaces")@ are simplicial.
    	Example    
	    PP1 = toricProjectiveSpace 1;
            assert (isSimplicial PP1 and isProjective PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isSimplicial FF7 and isProjective FF7)
	    AA3 = affineSpace 3;
	    assert (isSimplicial AA3 and not isComplete AA3 and # max AA3 === 1)	
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (isSimplicial P12234 and isProjective P12234)
            U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});	
	    assert (isSimplicial U and not isSmooth U)
    	Text
            However, not all normal toric varieties are simplicial.
    	Example
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSmooth Q and not isSimplicial Q and not isComplete Q)	    
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)	
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the normal toric variety.	    
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
     	(max, NormalToricVariety) 
    	(isSmooth, NormalToricVariety)
    	(makeSimplicial, NormalToricVariety)
///


doc ///
    Key 
        (isSmooth, NormalToricVariety)
    Headline 
        whether a normal toric variety is smooth
    Usage
        isSmooth X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean 
	    that is @TO true@ if the minimal nonzero lattice points on the
            rays in each maximal cone in the associated fan of form part of a
            $\ZZ$-basis
    Description
    	Text		
            A normal toric variety is smooth if every cone in its fan is
            smooth and a cone is smooth if its minimal generators are linearly
            independent over $\ZZ$.  In fact, the following conditions on a
            normal toric variety $X$ are equivalent:
    	Text
	    @UL {
                {EM "X", " is smooth,"},
                {"every torus-invariant Weil divisor on ", EM "X", " is
                    Cartier,"},
                {"the Picard group of ", EM "X", " equals the class group of ",
		    EM "X", ","},
                {EM "X", " has no singularities."}
	    }@
	Text
	    For more information, see Proposition 4.2.6 in
	    Cox-Little-Schenck's {\em Toric Varieties}.
	Text
            Many of our favourite normal toric varieties are smooth.
    	Example
	    PP1 = toricProjectiveSpace 1;
            assert (isSmooth PP1 and isProjective PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isSmooth FF7 and isProjective FF7)
	    AA3 = affineSpace 3;
	    assert (isSmooth AA3 and not isComplete AA3 and # max AA3 === 1)
	    X = smoothFanoToricVariety (4,120);
	    assert (isSmooth X and isProjective X and isFano X)
	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)	    
    	Text
            However, not all normal toric varieties are smooth.
    	Example
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (not isSmooth P12234 and isSimplicial P12234 and isProjective P12234)
            C = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    assert (not isSmooth C and isSimplicial C and # max C === 1) 
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSmooth Q and not isSimplicial Q and not isComplete Q)
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (not isSmooth Y and not isSimplicial Y and isProjective Y)
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the normal toric variety.		    
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
        (max, NormalToricVariety)
        (isSimplicial, NormalToricVariety)
///



doc ///
    Key 
        (isComplete, NormalToricVariety)
    Headline 
        whether a toric variety is complete
    Usage 
        isComplete X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean
	    that is @TO true@ if the normal toric variety is complete
    Description
        Text
            A normal toric variety is complete if any of the following
            equivalent conditions hold:
    	Text    
    	    @UL {
	        {"the associated complex variety is compact in its classical
	 	    topology,"},
                {"the constant map from the normal toric variety to space
         	    consisting of a single point is proper,"},
                {"every one-parameter subgroup of the torus has a limit in the
        	    toric variety,"},
       	        {"the union of all the cones in the associated fan equals the
        	    entire vector space containing it,"},
    	        {"every torus-invariant curve lying in the normal toric
        	    variety is projective."}
	    }@
	Text
	    For more information, see Theorem 3.4.1 in Cox-Little-Schenck's
	    {\em Toric Varieties}.
	Text
            Affine varieties are not complete.
	Example
	    AA1 = affineSpace 1
	    assert (not isComplete AA1 and isSmooth AA1 and # max AA1 === 1)
	    AA3 = affineSpace 3
	    assert (not isComplete AA3 and isSmooth AA3 and # max AA3 === 1)
      	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
            assert (not isComplete U and isDegenerate U and # max U === 1)
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}})
	    assert (not isComplete Q and not isSmooth Q and # max Q === 1)	    	    
    	Text
    	    @TO2(toricProjectiveSpace, "Projective varieties")@ are complete.
    	Example
	    PP1 = toricProjectiveSpace 1;
            assert (isComplete PP1 and isProjective PP1 and isSmooth PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isComplete FF7 and isProjective FF7 and isSmooth FF7 and not isFano FF7)	    
	    X = smoothFanoToricVariety (4,120);
            assert (isComplete X and isProjective X and isSmooth X and isFano X)	    	    
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (isComplete P12234 and isProjective P12234 and not isSmooth P12234 and isSimplicial P12234)
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (isComplete Y and isProjective Y and not isSmooth Y and not isSimplicial Y)
    	Text
            There are also complete non-projective normal toric varieties.
    	Example
            X1 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert (isComplete X1 and not isProjective X1 and not isSmooth X1 and isWellDefined X1)
            X2 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
            assert (isComplete X2 and not isProjective X2 and isSmooth X2 and isWellDefined X2)
            X3 = normalToricVariety ({{-1,2,0},{0,-1,0},{1,-1,0},{-1,0,-1},{0,0,-1},{0,1,0},{0,0,1},{1,0,-2}},{{0,1,3},{1,2,3},{2,3,4},{3,4,5},{0,3,5},{0,5,6},{0,1,6},{1,2,6},{2,4,7},{4,5,7},{2,6,7},{5,6,7}});    
            assert (isComplete X3 and not isProjective X3 and isSmooth X3 and isWellDefined X3)
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the normal toric variety.	    
	Text
	    The nonprojective examples are taken from Osamu Fujino and Sam
            Payne's "Smooth complete toric threefolds with no nontrivial nef
            line bundles",
            {\em Japan Academy. Proceedings, Series A, Mathematical Sciences},
	    {\bf 81} (2005) 174-179,
    	    @HREF("https://arxiv.org/abs/math/0510679","arXiv:math/0510679")@.	    
    SeeAlso    
        "finding attributes and properties"
        (isProjective, NormalToricVariety)
///	
  
  
doc ///
    Key 
        (isProjective, NormalToricVariety)
    Headline 
        whether a toric variety is projective
    Usage 
        isProjective X
    Inputs
        X : NormalToricVariety
    Outputs
        : Boolean 
	    that is @TO true@ if {\tt X} is a projective variety
    Description
        Text
	    A variety is projective if it can be realized as a closed
            subvariety of some projective space.  For an normal toric variety,
            this is equivalent to saying that the associated fan is the normal
            fan of a polytope.
    	Text
            Nontrivial affine varieties are not projective.
    	Example
    	    assert not isProjective affineSpace 1
    	    assert not isProjective affineSpace 3
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert (not isProjective U and isDegenerate U)
    	Text
  	    Many of our favour toric varieties are projective.
    	Example
    	    assert isProjective toricProjectiveSpace 1
    	    assert isProjective toricProjectiveSpace 3
    	    assert isProjective hirzebruchSurface 7
    	    assert isProjective smoothFanoToricVariety (3,3)
    	    assert isProjective normalToricVariety (id_(ZZ^3) | -id_(ZZ^3))
    	Text
  	    There are complete non-projective normal toric varieties.
  	Example
            X1 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert (isComplete X1 and not isProjective X1 and not isSmooth X1)
    	Example
            X2 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
            assert (isComplete X2 and not isProjective X2 and isSmooth X2)
	Text
            To determine if a normal toric variety is projective, we use the
            Gale dual vector configuration associated to the rays; see Theorem
            V.4.8 in Ewald's {\em Combinatorial convexity and algebraic
            geometry} for more information.  
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the normal toric variety.		
    SeeAlso
        "finding attributes and properties"
        (isComplete, NormalToricVariety)
        (isAmple,ToricDivisor)
///
  

doc ///
    Key
        (orbits, NormalToricVariety)
        orbits	
    Headline 
        make a hashtable indexing the torus orbits (a.k.a. cones in the fan)
    Usage 
        orbits X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : HashTable 
	    whose keys are the dimensions of the torus orbits in {\tt X} and
            whose values are lists of lists of integers indexing the torus
            orbits
    Description
        Text			   
            A normal toric variety is a disjoint union of its orbits under the
            action of its algebraic torus.  These orbits are in bijection with
            the cones in the associated fan.  Each cone is determined by the
            rays it contains.  In this package, the rays are ordered and
            indexed by nonnegative integers: $0, 1, \dots,n$.  Using this
            indexing, an orbit or cone corresponds to a sublist of
            $\{0,\dots,n\}$; the entries index the rays that generate the
            cone.
        Text	    
            The projective plane has three fixed points and three fixed curves
            (under the action of its torus), and projective $3$-space has four
            fixed points, six fixed curves, and four divisors.  More
            generally, the orbits of projective $(n-1)$-space are enumerated
            by the $n$-th row of Pascal's triangle.
	Example
    	    O2 = orbits toricProjectiveSpace 2
            (#O2#0, #O2#1, #O2#2)
    	    O3 = orbits toricProjectiveSpace 3     
            apply (4, k -> #O3#k)
            apply (5, k -> # (orbits toricProjectiveSpace 4)#k)
            apply (6, k -> # (orbits toricProjectiveSpace 5)#k)    
    	Text
  	    Here is a non-simplicial example.
    	Example
      	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isSimplicial X
    	    OX = orbits X
	    apply (1+dim X, k -> #OX#k)
    	Text
  	    The following degenerate example has no fixed points.
    	Example
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert isDegenerate U
    	    OU = orbits U
	    apply (4, k -> #OU#k)
	    assert (#OU#0 == 0)
	Text
	    To avoid repeating a computation, the package caches the result in
	    the @TO CacheTable@ of the normal toric variety.		    
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
        (orbits, NormalToricVariety,ZZ)
///	

doc ///
    Key 
        (orbits, NormalToricVariety, ZZ)
    Headline 
        get a list of the torus orbits (a.k.a. cones in the fan) of a given dimension
    Usage 
        orbits (X, i)
    Inputs
        X : NormalToricVariety
        i : ZZ
	    determining the dimension of the orbits
    Outputs 
        : List
	    of lists of integers indexing the torus orbits
    Description
        Text			   
            A normal toric variety is a disjoint union of its orbits under the
            action of its algebraic torus.  These orbits are in bijection with
            the cones in the associated fan.  Each cone is determined by the
            rays it contains.  In this package, the rays are ordered and
            indexed by nonnegative integers: $0, 1, \dots, n$.  Using this
            indexing, an orbit or cone corresponds to a sublist of
            $\{ 0, 1, \dots, n \}$; the entries index the rays that generate
            the cone.
        Text	    
            The projective plane has three fixed points and three fixed curves
            (under the action of its torus), and projective $3$-space has four
            fixed points, six fixed curves, and four divisors.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    orbits (PP2,0)
    	    orbits (PP2,1)
	    orbits (PP2,2)
    	    PP3 = toricProjectiveSpace 3;
    	    orbits (PP3,0)
    	    orbits (PP3,1)
    	    orbits (PP3,2)
    	    orbits (PP3,3)	    
    	Text
	    Here is a non-simplicial example.  Since it is nondegenerate, the
            fixed points correspond to the maximal cones in the fan.  The rays
            always correspond to the divisors.
      	Example
    	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    orbits (X,0)
            assert (orbits (X,0) === max X)
    	    orbits (X,1)
	    orbits (X,2)
    	    assert (orbits (X,2) === apply (#rays X, i -> {i}))
	    orbits (X,3)
	    assert (orbits (X,3) === {{}})
    	Text
  	    The following degenerate example has no fixed points.
    	Example
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert isDegenerate U
    	    orbits (U,0)
    	    orbits (U,1)
    	    orbits (U,2)
	    orbits (U,3)
    	    dim U
	Text
	    To routine extracts the requested list from hashTable returned by
	    @TO(orbits, NormalToricVariety)@.
    SeeAlso
        "finding attributes and properties"
        (rays, NormalToricVariety)
        (orbits, NormalToricVariety)
///
     

------------------------------------------------------------------------------
-- Resolution of singularities    
------------------------------------------------------------------------------
doc ///
    Key
        "resolving singularities"
    Headline
        information about find a smooth proper birational surjection
    Description
        Text	
            A variety $X$ has a resolution of singularities if one can find a
  	    nonsingular variety $Y$ and a proper birational map from $Y$ to
  	    $X$.  Every normal toric variety has a resolution of singularities
  	    given by another normal toric variety.
        Text	  
            The following methods related to resolutions of singularities are
  	    currently available in this package.
        Text  
  	    @SUBSECTION "Methods for resolving singularities"@
        Text	    
  	    @UL {
    		TO (makeSmooth, NormalToricVariety),
    		TO (makeSimplicial, NormalToricVariety),
    		TO (toricBlowup, List, NormalToricVariety)
	    }@
    SeeAlso
        "making normal toric varieties"
        "finding attributes and properties"
	"working with toric maps"	
        "working with divisors"
        "working with sheaves"	
///	


doc ///
    Key 
    	(makeSimplicial, NormalToricVariety)    
        makeSimplicial
    	[makeSimplicial,Strategy]
    Headline 
        make a birational simplicial toric variety
    Usage 
        makeSimplicial X
    Inputs
        X : NormalToricVariety
        Strategy => ZZ
	    either {\tt 0} or {\tt 1}
    Outputs 
        : NormalToricVariety 
	    that is simplicial
    Description
        Text
            A normal toric variety is simplicial if every cone in its fan is
  	    simplicial and a cone is simplicial if its minimal generators are
  	    linearly independent over $\QQ$.  In fact, the following
  	    conditions on a normal toric variety $X$ are equivalent:
        Text	    
            @UL{
                {EM "X", " is simplicial;"},
                {"every Weil divisor on ", EM "X", " has a positive integer
     		  multiple that is Cartier;"},
                {"the Picard group of ", EM "X", " has finite index in the
     		  class group of ", EM "X", ";"},
                {EM "X", " has only finite quotient singularities."}
            }@
	Text
	    For more information, see Proposition 4.2.7 in
	    Cox-Little-Schenck's {\em Toric Varieties}.
        Text	
            Given a normal toric variety, this method makes a simplicial toric
  	    variety with the same rays by triangulating the non-simplicial
  	    maximal cones.  For the {\tt 0} strategy, the triangulation is
  	    constructed by repeated regular subdivisions using random integral
  	    weight vectors.  For the {\tt 1} strategy, the triangulation is
  	    constructed by repeated pushing subdivisions (i.e. toricBlowups at a
  	    given ray).
        Example  
            X = normalToricVariety (id_(ZZ^3) | - id_(ZZ^3));
    	    assert not isSimplicial X
    	    Y1 = makeSimplicial X;
    	    assert isSimplicial Y1
    	    assert (rays Y1 === rays X)
    	    max Y1 
	    max X
    	    Y2 = makeSimplicial(X, Strategy => 1);
    	    assert isSimplicial Y2
    	    assert (rays Y2 === rays X)
    	    max Y2
	    max Y1 == max Y2
        Text
            If the initial toric variety is simplicial, then this method
  	    simply returns it.
        Example	    
    	    PP3 = toricProjectiveSpace 3;
    	    assert isSimplicial PP3
    	    Z = makeSimplicial PP3;
    	    assert (rays Z === rays PP3 and max Z === max PP3)
    SeeAlso
        "resolving singularities"
        (isSimplicial, NormalToricVariety)
///	


doc ///
    Key 
    	(toricBlowup, List, NormalToricVariety, List)    
    	(toricBlowup, List, NormalToricVariety)    
        toricBlowup
    Headline 
        makes the toricBlowup of a normal toric variety along a torus orbit closure	
    Usage 
        toricBlowup (s, X, v)
    Inputs
        s : List 
	    of integers indexing a proper torus orbit
        X : NormalToricVariety
        v : List 
	    of integers giving a vector in the relative interior of the cone
	    corresponding to {\tt s} (optional)		    
    Outputs 
        : NormalToricVariety 
	    obtained by blowing up {\tt X} along the torus orbit indexed by
	    {\tt s}
    Description
        Text	    
            Roughly speaking, the toricBlowup replaces a subspace of a given space
  	    with all the directions pointing out of that subspace.  The
  	    metaphor is inflation of a balloon rather than an explosion.  A
  	    toricBlowup is the universal way to turn a subvariety into a Cartier
  	    divisor.
        Text  
            The toricBlowup of a normal toric variety along a torus orbit closure
  	    is also a normal toric variety.  The fan associated to the toricBlowup
  	    is star subdivision or stellar subdivision of the fan of the
  	    original toric variety.  More precisely, we throw out the star of
  	    the cone corresponding to {\tt s} and join a vector {\tt v} lying
  	    the relative interior to the boundary of the star.  When the
  	    vector {\tt v} is not specified, the ray corresponding to the sum
  	    of all rays in the cone corresponding to {\tt s} is used.
        Text  
            The simplest example is toricBlowup of the origin in the affine plane.
  	    Note that the new ray has the largest index.
        Example    
            AA2 = affineSpace 2;
    	    rays AA2
    	    max AA2
    	    Bl0 = toricBlowup ({0,1}, AA2);
    	    rays Bl0
    	    max Bl0
        Text 
  	    Here are a few different toricBlowups of a non-simplicial affine toric
  	    variety
        Example 	    
    	    C = normalToricVariety ({{1,0,0},{1,1,0},{1,0,1},{1,1,1}}, {{0,1,2,3}});
    	    assert not isSimplicial C
    	    Bl1 = toricBlowup ({0,1,2,3}, C);
    	    rays Bl1
    	    max Bl1
	    assert isSimplicial Bl1
    	    Bl2 = toricBlowup ({0,1}, C);
    	    rays Bl2
    	    max Bl2
	    assert isSimplicial Bl2
	    assert (rays Bl1 =!= rays Bl2 and max Bl1 =!= max Bl2)
    	    Bl3 = toricBlowup ({0,1,2,3}, C, {5,3,4});
    	    rays Bl3
    	    max Bl3
	    assert isSimplicial Bl3	    
    	    Bl4 = toricBlowup ({0}, C);
    	    rays Bl4
    	    max Bl4
    	    assert isSimplicial Bl4	    
        Text 
            The third collection of examples illustrate some toricBlowups of a
  	    non-simplicial projective toric variety.
        Example 	    
    	    X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
    	    rays X
    	    max X
    	    assert (not isSimplicial X and isProjective X)
    	    orbits (X,1)
    	    Bl5 = toricBlowup ({0,2}, X);
    	    Bl6 = toricBlowup ({6,7}, Bl5);
    	    Bl7 = toricBlowup ({1,5}, Bl6);
    	    rays Bl7
    	    max Bl7
    	    assert (isSimplicial Bl7 and isProjective Bl7)
    	    Bl8 = toricBlowup ({0}, X);
    	    Bl9 = toricBlowup ({7}, Bl8);
    	    assert (rays Bl9 === rays X)
    	    assert (isSimplicial Bl9 and isProjective Bl9)
    Caveat 
        The method assumes that the list {\tt v} corresponds to a primitive
  	vector.  In other words, the greatest common divisor of its entries is
  	one.  The method also assumes that {\tt v} lies in the relative
  	interior of the cone corresponding to {\tt s}.  If either of these
  	conditions fail, then the output will not necessarily be a
  	well-defined normal toric variety.
    SeeAlso
        "resolving singularities"
    	(orbits, NormalToricVariety)
    	(isWellDefined, NormalToricVariety)
    	(makeSmooth, NormalToricVariety)
///	


doc ///
    Key 
    	(makeSmooth, NormalToricVariety)
        makeSmooth 	
    	[makeSmooth, Strategy]
    Headline 
        make a birational smooth toric variety 
    Usage 
        makeSmooth X
    Inputs 
        X : NormalToricVariety
        Strategy => ZZ
	    either {\tt 0} or {\tt 1}	
    Outputs 
        : NormalToricVariety
	    that is smooth and birational to {\tt X} 
    Description
        Text	
            Every normal toric variety has a resolution of singularities given
  	    by another normal toric variety.  Given a normal toric variety $X$
  	    this method makes a new smooth toric variety $Y$ which has a
  	    proper birational map to $X$.  The normal toric variety $Y$ is
  	    obtained from $X$ by repeatedly blowing up appropriate torus orbit
  	    closures (if necessary the @TO makeSimplicial@ method is also used
  	    with the specified strategy).  A minimal number of blow-ups are
  	    used.
        Text
            As a simple example, we can resolve a simplicial affine
            singularity.
        Example 	    
    	    U = normalToricVariety ({{4,-1},{0,1}}, {{0,1}});
    	    assert not isSmooth U
    	    V = makeSmooth U;
    	    assert isSmooth V
    	    rays V, max V
    	    toList (set rays V - set rays U)
        Text 
            There is one additional rays, so only one toricBlowup was needed.
        Text 
  	    To resolve the singularities of this simplicial projective
  	    fourfold, we need eleven toricBlowups.
        Example  
    	    W = weightedProjectiveSpace {1,2,3,4,5};
    	    assert (dim W === 4)
    	    assert (isSimplicial W and not isSmooth W)
    	    W' = makeSmooth W;
    	    assert isSmooth W'
    	    # (set rays W' - set rays W)
        Text
            If the initial toric variety is smooth, then this method simply
  	    returns it.
        Example   
    	    AA1 = affineSpace 1;
    	    assert (AA1 === makeSmooth AA1)
    	    PP2 = toricProjectiveSpace 2;
    	    assert (PP2 === makeSmooth PP2)
        Text
            In the next example, we resolve the singularities of a
  	    non-simplicial projective threefold.
        Example 	    
    	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert (not isSimplicial X and not isSmooth X)
    	    X' = makeSmooth X;
    	    assert isSmooth X'
    	    # (set rays X' - set rays X)
        Text	    
  	    We also demonstrate this method on a complete simplicial
  	    non-projective threefold.
        Example  
    	    Z = normalToricVariety ({{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},{0,0,-1}}, {{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},{1,5,6},{2,3,4},{2,4,5}});
    	    assert (isSimplicial Z and not isSmooth Z)
	    assert (isComplete Z and not isProjective Z)
    	    Z' = makeSmooth Z;
    	    assert isSmooth Z'
    	    # (set rays Z' - set rays Z)
        Text
  	    We end with a degenerate example.
        Example	    
            Y = normalToricVariety ({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}}, {{0,1,2,3},{0,4}});
    	    assert (isDegenerate Y and not isSimplicial Y and not isComplete Y)
    	    Y' = makeSmooth Y;
    	    assert isSmooth Y'
	    # (set rays Y' - set rays Y)
    Caveat 
        A singular normal toric variety almost never has a unique minimal
	resolution.  This method returns only of one of the many minimal
	resolutions.
    SeeAlso
        "resolving singularities"
    	(isSmooth, NormalToricVariety)
    	(makeSimplicial, NormalToricVariety)
///	
