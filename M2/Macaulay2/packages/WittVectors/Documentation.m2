-------------------------
--- witt method
-------------------------


doc ///
    Key
	    (witt, ZZ, RingMap)
    Headline
        The map induced on Witt rings by a RingMap.
    Usage
	    witt(n,f)
    Inputs
	    n: ZZ
	    f: RingMap
    Outputs
	    Wf: WittRingMap
    Description
	    Text
	        Given f: R -> S a RingMap between rings  of positive characteristic and an integer
	        $n \geq 1$, witt(n, f) returns the WittRingMap $W_n(f): W_n(R) \to W_n(S)$.
	    Example
	        R = (ZZ/5)[x,y];
                S = (ZZ/5)[a,b,c,d];
	        f = map(S, R, {a*b, c*d});
	        witt(2, f)
///



doc ///
    Key
	(witt, ZZ, ZZ, RingMap)
    Headline
        The map induced on Witt rings by a RingMap.
    Usage
	witt(n, m, f)
    Inputs
	n: ZZ
	m: ZZ
	f: RingMap
    Outputs
        Wf: WittRingMap
    Description
	Text
	    Given f: R -> S a RingMap between rings  of positive characteristic and an integer
	    $n \geq 1$, witt(n, f) returns the WittRingMap $W_n(f): W_n(R) \to W_n(S)$.
	    If two integers $m \geq n \geq 1$ are passed, witt(n, m, f) returns the WittRingMap
	    given by the composition $W_m(R) \to W_m(S) \to W_n(S)$, where the last map is
	    truncation.
	Example
	    R = (ZZ/5)[x,y];
	    S = (ZZ/5)[a,b,c,d];
	    f = map(S, R, {a*b, c*d});
	    witt(2, 3, f)
///



doc ///
    Key
	(witt, ZZ, PolynomialRing)
    Headline
        Builds a Witt ring.
    Usage
	witt(n, R)
    Inputs
	n: ZZ
	R: PolynomialRing
    Outputs
	:WittPolynomialRing
    Description
	Text
	    Given an integer $n \geq 1$ and a polynomial ring R of positive prime characteristic,
	    this produces the n-th witt ring $W_n(R)$ of R.
	Example
	    R = (ZZ/3)[x,y,z];
	    WR = witt(2, R)
///

doc ///
    Key
	WittRingMap
    Headline
	The class of maps between Witt rings.
    Description
	Text
            In our implementation, a map of WittRings is always induced by a map between the underlying rings. WittRingMaps can operate on WittRingElements.
	Example
            R = (ZZ/5)[x,y];
            S = (ZZ/5)[a,b,c,d];
            f = map(S, R, {a*b, c*d});
            Wf = witt(2, f);
///




doc ///
    Key
        (baseMap, WittRingMap)
    Headline
        Returns the underlying RingMap of a WittRingMap
    Usage
        f = baseMap Wf
    Inputs
        Wf: WittRingMap
    Outputs
        f: RingMap
    Description
        Text
            Given a WittRingMap $W(f): W_n(R) \to W_n(S)$ induced by f:R-> S, this returns the underlying RingMap f: R->S.
        Example
            R = (ZZ/5)[x,y];
            S = (ZZ/5)[a,b,c,d];
            f = map(S, R, {a*b, c*d});
            Wf = witt(2, f);
            f === baseMap Wf
///

doc ///
    Key
	(witt, ZZ, QuotientRing)
    Headline
        Forms the n-th Witt ring of a quotient ring
    Usage
	witt(n, R)
    Inputs
	n: ZZ
	R: QuotientRing
    Outputs
	:WittQuotientRing
    Description
	Text
	    Given an integer $n \geq 1$ and a quotient ring R of positive prime characteristic,
	    this produces the n-th witt ring $W_n(R)$ of R.
	Text
	    Note that for this method to work, the ambient ring of R must be a polynomial ring.
	    If this is not the case, consider flattening before applying witt.
	Example
	    R = (ZZ/3)[x,y,z] / ideal(x^2, y^2, z^2);
	    WR = witt(2, R)
	Example
	    S = (ZZ/2)[x,y,z] / ideal(x^2);
	    R = S / ideal(y^2, z^2);
	    WR = witt(2, (flattenRing R)#0)

///
	
doc ///
    Key
     (truncate,ZZ,WittQuotientRing)
    Headline
        Crop Witt Quotient ring to the ring of Witt vectors of a given length
    Usage
        V=truncate(n,W)
    Inputs
        n: ZZ
        W: WittQuotientRing
    Outputs
        V: WittQuotientRing
    Description
        Text
            This crops W to have length n if n is less than or equal than the length of W.
        Example
            S = (ZZ/5)[x,y,z] / ideal(x^2 + y^2 + z^2)
            W=witt(3,S)
            truncate(2,W)
        Text
            This should give V a WittQuotientRing with WittLength 2
        Text
            We get an error if we try to truncate to something longer. For instance,  truncate(4,W) above would return an error.
///



doc ///
    Key
	(witt, List)
    Headline
        Forms a WittRingElement from a list of ring elements
    Usage
	witt L
    Inputs
	L: List
	    a list of elements of the same ring of positive characteristic
    Outputs
	:WittRingElement
    Description
	Text
	    Given a list  $L = \{x_1, \dots , x_n\}$ of elements of the same
	    ring R of positive prime characteristic, this produces the WittRingElement
	    $(x_1, \dots , x_n) \in W_n(R)$.
	Example
            R = ZZ/5[x,y,z]/(x^3 + y^3 + z^3);
            x + y
///


-------------------------
--- wittOverring
-------------------------


doc ///
    Key
	wittOverring
	(wittOverring, ZZ, Ring)
	(wittOverring, WittPolynomialRing)
	(wittOverring, WittQuotientRing)
    Headline
        Returns the n-th WittOverring of a ring R, or the overring of a witt ring.
    Usage
	wittOverring(n, R)
	wittOverring(WR)
    Inputs
	n: ZZ
	R: Ring
	WR:WittPolynomialRing
	WR:WittQuotientRing
    Outputs
	:Ring
    Description
	Text
	    Given a polynomial ring R = (ZZ/p) [x_1,..., x_n] over a
	    finite prime field ZZ/p, and an integer $n \geq 1$, it returns
	    and appropriately caches the polynomial ring $(\mathbb{Z}/ p^n)[T_1, \dots , T_n]$, which
	    we call the wittOverring. The reason is that the n-th Witt ring of R is a subring
	    of this wittOverring.
            Note: given a quotient ring R = S/I, where S is a polynomial ring over a finite prime field,
	    it returns the wittOverring of S. That is to say, the Witt overring does not keep track
	    of relations in the ring. As a consequence, for a prime p  wittOverring will return different answers
	    for witt(n, (ZZ/p)[x_1, .. ,x_n]) and witt(n, GF(p)[x_1 .. x_n]); the latter will have one more variable.
	    
	Example
	    R = (ZZ/2)[x,y];
	    wittOverring(3, R)
///


-------------------------
--- Random operations 
-------------------------


doc ///
    Key
	(toList, WittRingElement)
    Headline
        Converts a WittRingElement into a List
    Usage
	toList(w)
    Inputs
	w: WittRingElement
    Outputs
	:List
    Description
	Text
	    Turns a WittRingElement back into a List.
	Example
	    R = (ZZ/5)[x,y];
	    w = witt{x,y, x + y};
	    toList(w)
///

doc ///
    Key
	(ring, WittRingElement)
    Headline
        Returns the Witt ring that a WittRingElement belongs to
    Usage
	R = ring(w)
    Inputs
	w: WittRingElement
    Outputs
        R:Ring
    Description
	Text
	    Returns the WittPolynomialRing or WittQuotientRing that the input belongs to
	Example
	    R = (ZZ/5)[x,y]
	    w = witt{x, x+y}
	    ring(w)
	Example
	    R = (ZZ/5)[x,y,z] / ideal(x^2 + y^2 + z^2)
	    w = witt{x, y, z}
	    ring(w)
///

doc ///
    Key
        wittLength
        (wittLength, WittPolynomialRing)
	(wittLength, WittQuotientRing)
    Headline
        Returns the length of the Witt vectors in a given Witt ring
	
    Usage
        n = wittLength(WR)
    Inputs
        WR:WittPolynomialRing
	WR:WittQuotientRing
    Outputs
        n:ZZ
    Description
	Text
	    If $WR = W_n(R)$ for some polynomial or quotient ring R, and some integer $n \geq 1$,
	    wittLength(WR) returns the integer n.
	Example
	    R = GF(9)[x,y];
	    WR = witt(5, R);
	    wittLength(WR)	    
///
	   
	   
--------------------------------
--------- WittRingElement
--------------------------------

doc ///
    Key
	WittRingElement
    Headline
	The Type for elements of WittPolynomialRing and WittQuotientRing.
    Description
	Text
	    Instances of WittRingElement can be built by using the witt method.
	Example
	    R = (ZZ/3)[x,y];
	    w = witt{x^2 + y^2, x}
///


--------------------------------
--------- fSplittingHeight
--------------------------------

doc ///
	Key
	 fSplittingHeight
	 [fSplittingHeight, MaxHeight]
	Headline
	 Finds the quasi-F-split height ht(S/I) of the quotient of the polynomial ring S=(ZZ/p)[x1,...,xn] by an ideal I generated by a regular sequence (f1,...,fm).
	Usage
	 fSplittingHeight I
	Inputs
	 I: Ideal
	Outputs
	 r: Number
	Description
	 Text
	    This gives the quasi-F-Splitting height r of S/I
	 Example
	    S = ZZ/3[x,y,z, w]
	   	    I = ideal(x^4 + y^4 + z^4 + w^4 + x^2*z^2 + x*y*z^2)
	    fSplittingHeight I
	 Text
	    This should give 4. 
	 Text
	    One can change the max F-splitting height that the algorithm tests
	 Example
	    S = ZZ/3[x,y,z, w]
	   	I = ideal(x^4 + y^4 + z^4 + w^4 + x^2*z^2 + x*y*z^2)
	    fSplittingHeight (I,MaxHeight=>2)
	 Text
	    This should not return anything
	 Example
		S = ZZ/3[x,y,z,w]
	    I = ideal(x^4 + y^4 + z^4 + w^4)
	    fSplittingHeight I
	 Text
	    This should give infinity. 
	 Example
	    S = ZZ/3[x,y,z]
	   	I = ideal(x,y)
	 Text
	    This should give one. 
	 Example 
		S = ZZ/5[x,y,z]
		I = ideal(y*x,z*x)
	 	fSplittingHeight I
	 Text
	    We get an error since the ideal is not generated by a regular sequence
	 Example
		S = GF(4)[x,y,z]
		I=ideal(x)
		fSplittingHeight I
	 Text
	    We get an error since S is not a polynomial ring over ZZ/p.
///

--------------------------------
--------- WittRingElement operations
--------------------------------

doc///
	Key
	 (truncate,ZZ,WittRingElement)
	Headline 
	 Crop Witt Vector to have a given length.
	Usage 
	 truncate(i, w)
	Inputs
	 i: ZZ
	 w: WittRingElement
	Outputs
	 v: WittRingElement
	Description
         Text
	    This crops w to have length i if i is less than or equal than the length of w.
	  Example
	    S=ZZ/3[x,y]
	    w=witt{x,y}
	    truncate(1,w)
	 Text
	  This should give {x}, a WittRingElement
	 Text 
	  We get an error if we try to truncate to something longer. For instance,  truncate(3,w) above would return an error.
///




doc ///
    Key
	(wittFrobenius, WittPolynomialRing)
	(wittFrobenius, WittQuotientRing)
    Headline
        The (Witt) Frobenius map of a Witt ring
    Usage
    	phi = wittFrobenius W
    Inputs
	W:{WittPolynomialRing, WittQuotientRing}
	   The Witt ring of a ring R of positive characteristic
    Outputs
	phi:WittRingMap
            the Frobenius map on W
    Description
	Text
            This gives the Frobenius map on the Witt ring W (which in coordinates is just the entry-wise Frobenius)
	Example
            R = ZZ/5[x]
            W = witt(2,R);
            phi = wittFrobenius W;
///

doc ///
    Key
	(wittFrobenius, ZZ, Ring)
    Headline
	The (Witt) Frobenius map on the Witt vectors of a ring
    Usage
	phi = wittFrobenius(n, R)
    Inputs
        n:ZZ
           a positive integer, the length of the Witt vectors to consider
        R:Ring
            a ring of positive characteristic
    Outputs
	phi:WittRingMap
            the Frobenius map on witt(n,R)
    Description
	Text
            given a ring R and an integer n this gives the Frobenius map on W_n(R) (which in coordinates is just the entry-wise Frobenius)
	Example
            R = ZZ/5[x]
            phi = wittFrobenius(2,R)
///

doc ///
    Key
	(wittFrobenius, WittRingElement)
    Headline
	The (Witt) Frobenius map on an element
    Usage
	Fw = wittFrobenius w
    Inputs
	w:WittRingElement
           an element of a Witt ring
    Outputs
        Fw:WittRingElement
            the image of w under the Frobenius map
    Description
	Text
            Given a Witt vector w, this gives the image w under the Frobenius map (which in coordinates is just the entry-wise Frobenius)
	Example
            R = ZZ/5[x,y,z]
            w = witt{x,y,z}
            wittFrobenius(w) -- same as (wittFrobenius(R))(w)
///

doc ///
    Key
	(verschiebung, WittRingElement)
    Headline
	The Verschiebung map on an element
    Usage
	Vw = verschiebung w
    Inputs
	w:WittRingElement
           an element of a Witt ring
    Outputs
        Vw:WittRingElement
            the image of w under the Verschiebung map
    Description
	Text
            Given a Witt vector w, this gives the image w under the Verschiebung map (which in coordinates simply prepends a zero)
	Example
            R = ZZ/5[x,y]
            w = witt{x,y}
            verschiebung(w) -- same as wittFrobenius(w)
///

doc ///
    Key
	(symbol +, WittRingElement, WittRingElement)
    Headline
        Addition of WittRingElements.
    Usage
	w = w1+w2
    Inputs
	w1:WittRingElement
	w2:WittRingElement
           elements of a Witt ring
    Outputs
        w:WittRingElement
            the sum of w1 and w2
    Description
	Text
            Given Witt vectors w1 and w2, this computes their sum (corresponding to the addition operation inherited via the ghost maps)
	Example
            R = ZZ/5[x,y,z,w]
            w1 = witt{x,y}
            w2 = witt{z,w}
            w1+w2
///

doc ///
    Key
	(symbol *, WittRingElement, WittRingElement)
    Headline
        Multiplication of WittRingElements.
    Usage
	w = w1*w2
    Inputs
	w1:WittRingElement
	w2:WittRingElement
           elements of a Witt ring
    Outputs
        w:WittRingElement
            the product of w1 and w2
    Description
	Text
            Given Witt vectors w1 and w2, this computes their sum (corresponding to the multiplication operation inherited via the ghost maps)
	Example
            R = ZZ/5[x,y,z,w]
            w1 = witt{x,y}
            w2 = witt{z,w}
            w1*w2
///


--------------------------------
--------- Conversions
--------------------------------

doc ///
    Key
	wittRingToTuple
    Headline
	Converts an element of the explicit presentation of a Witt ring into a tuple.
    Usage
	w = wittRingToTuple r
    Inputs
	r:RingElement
           an element of a ring that is of the form explicit(W) for W a Witt ring
    Outputs
        w:WittRingElement
    Description
	Text
            Given an element r of a ring WR, which must be obtained as R = explicit(W) for a WittPolynomialRing or WittQuotientRing, this returns the corresponding tuple of W under the isomorphism between W and WR
	Example
            R = ZZ/5[x]
            W = witt(2,R)
            WR = explicit W
            w = WR_0+WR_1+WR_2+WR_3
            wittRingToTuple(w)
///

doc ///
    Key
	wittOverringToTuple
    Headline
	Converts an element of the explicit overring of a Witt ring into a tuple.
    Usage
	w = wittOverringToTuple r
    Inputs
	r:RingElement
           an element of a ring that is of the form wittOverring(n,R) a ring R
    Outputs
        w:WittRingElement
    Description
	Text
            Given an element r of a ring OR, which must be obtained as OR = wittOverring(n,R), this returns the corresponding tuple of W 
	Example
            R = ZZ/5[x]
            W = witt(2,R)
            OR = wittOverring(2,R)
            w = OR_0^5
            wittOverringToTuple(w)
///

doc ///
    Key
	wittTupleToRing
    Headline
	Converts an element of a Witt ring to an element of its explicit presentation.
    Usage
	r = wittTupleToRing w
    Inputs
        w:WittRingElement
    Outputs
	r:RingElement
           an element of a ring that is of the form explicit(W) for W a Witt ring
    Description
	Text
            Given a WittRingElement w of W (i.e., a tuple) this returns the corresponding element of the explicit presentation of W (i.e., an element of explicit(W))
	Example
            R = ZZ/5[x]
            w = witt{x,x^5+x^4}
            wittTupleToRing w
///

doc ///
    Key
	wittTupleToOverring 
    Headline
	Converts an element of a Witt ring to an element of the overring of its explicit presentation.
    Usage
	r = wittTupleToOverring w
    Inputs
        w:WittRingElement
    Outputs
	r:RingElement
           an element of a Witt overring
    Description
	Text
            Given a WittRingElement w of W = witt(n,R) (i.e., a tuple) this returns the corresponding element of the overring of the explicit presentation of W (i.e., an element of wittOverring(n,R)))
	Example
            R = ZZ/5[x]
            w = witt{x,x^5+x^4}
            wittTupleToOverring w
///


--------------------------------
--------- Conversions for ideals
--------------------------------


doc ///
    Key
	wittOverringIdeal
	(wittOverringIdeal, ZZ, Ideal)
    Headline
	The expansion of the witt ideal to the witt overring.
    Usage
	WOI = wittOverringIdeal(n, I)
    Inputs
	n: ZZ
	I: Ideal
	    an ideal in a polynomial ring R of positive characteristic.
    Outputs
	WOI: Ideal
	    an ideal in the nth witt overring of R
    Description
	Text
	    If R is a polynomial ring of positive characteristic, I is an ideal of R, and
	    $n \geq 1$ is an integer, one has an ideal W_n(I) of W_n(R) given as the kernel
	    of $W_n(R) \to W_n(R / I)$. The method wittOverringIdeal(n, I) returns the expansion
	    of W_n(I) to the n-th witt overring of R.
	Example
	    R = (ZZ / 3)[x,y,z];
	    I = ideal(x^2, y^2, z^2);
	    wittOverringIdeal(2, I)
///

doc ///
    Key
	wittRingIdeal
        (wittRingIdeal, ZZ, Ideal)
    Headline
	Returns the n-th witt ideal in explicit form.
    Usage
	WI = witt(n, I)
    Inputs
	n: ZZ
	I: Ideal
	    an ideal in a polynomial ring R of positive characteristic.
    Outputs
	WI: Ideal
	    the n-th witt of I, in explicit form.
    Description
	Text
	    If R is a polynomial ring of positive characteristic, I is an ideal of R, and
	    $n \geq 1$ is an integer, one has an ideal W_n(I) of W_n(R) given as the kernel
	    of $W_n(R) \to W_n(R / I)$. The method wittRingIdeal(n, I) returns the ideal $W_n(I)$
	    in explicit form. Note that these ideals tend to have many more generators than I.
	Example
	    R = (ZZ / 3)[x,y,z];
	    I = ideal(x^2, y^2, z^2);
	    WI = wittRingIdeal(2, I);
///


--------------------------------
--------- explicit method
--------------------------------



doc ///
    Key
        (explicit, WittPolynomialRing)

    Headline
        Expresses a WittPolynomialRing as a finitely generated algebra over the integers.
	
    Usage
        E = explicit(WR)
	
    Inputs
        WR:WittPolynomialRing
        
    Outputs
        E:Ring
	
    Description
	Text
	    If R = (ZZ/p)[x_1,..., x_d] is a polynomial ring over a prime field
	    ZZ/p, and $n \geq 1$ is an integer, then $W_n(R)$ can be identified
	    with a certain $\mathbb{Z} / p^n$ subalgebra of a polynomial ring in d variables
	    over $\mathbb{Z} / p^n$. This method finds the relations between these generators
	    in order to express $W_n(R)$ as a finitely generated ZZ-algebra. Note that
	    the number of generators and relations grows very fast with p and d.

	    When the method is applied when R is a polynomial ring over a finite
	    but not prime field, the package essentially treats R as a quotient of a
	    polynomial ring over its prime subfield. 
	    
	Example
	    R = (ZZ/2)[x];
	    WR = witt(2, R);
	    explicit(WR)
        Example
	    R = GF(4)[x];
	    WR = witt(2, R);
	    explicit(WR);
///

doc ///
    Key
        (explicit, WittQuotientRing)

    Headline
        Expresses a WittQuotientRing as a finitely generated algebra over the integers.
       
    Usage
        E = explicit(WR)
	
    Inputs
        WR:WittQuotientRing
	
    Outputs
        E:Ring
	
    Description
	Text
            If R is a finitely generated algebra over a prime field ZZ/p, and
	    WR = witt(n, R) for some $n \geq 1$, then explicit(WR) returns the ring WR
            as a finitely generated ZZ-algebra.

	    When the base field of R is a finite field, but not prime, the package essentially first
            writes R as a finitely generated algebra over its prime subfield, and then applies
	    the method. 
	    
	Example
	    R = (ZZ/2)[x] / ideal(x^2);
	    WR = witt(2, R);
	    explicit(WR)	    
///



doc ///
    Key
        (explicit, WittIdeal)

    Headline
        Obtains the explicit version of a WittIdeal.
	
    Usage
        J := explicit(WI)
	
    Inputs
	WI:WittIdeal
    Outputs
        J:Ideal
    Description
	Text
	    If WR is a WittPolynomialRing or WittQuotientRing, and WI is an WittIdeal in WR,
	    explicit(WI) gives the ideal corresponding to WI in explicit(WR)
	Example
	    R := (ZZ/2)[x,y];
	    w1 := witt{x,y};
	    w2 := witt{x^2, y^2};
	    I := wittIdeal(w1, w2)
///




	    

-------------------------------------------------
--------- WittPolynomialRing and operation on it
-------------------------------------------------

doc ///
    Key
	WittPolynomialRing
    Headline
	The class of the n-th Witt ring of a polynomial ring.
    Description
	Text
	    Can be built by using the witt method.
	Example
	    R = (ZZ/3)[x,y,z];
	    WR = witt(2, R)
///




doc ///
    Key
     (truncate,ZZ,WittPolynomialRing)
    Headline 
        Crop Witt ring to the ring of Witt vectors of a given length
    Usage 
        V=truncate(n,W)
    Inputs
        n: ZZ
        W: WittPolynomialRing
    Outputs
        V: WittPolynomialRing
    Description
        Text
            This crops W to have length n if n is less than or equal than the length of W.
        Example
            S=ZZ/3[x,y]
            W=witt(3,S)
            truncate(2,W)
        Text
            This should give V a Witt ring with WittLength 2
        Text 
            We get an error if we try to truncate to something longer. For instance,  truncate(4,W) above would return an error.
///


--------------------------------
--------- WittQuotientRing
--------------------------------

doc ///
    Key
	WittQuotientRing
    Headline
	The class of the n-th Witt ring of a quotient of a polynomial ring.
    Description
	Text
	    Can be built by using the witt method.
	Example
	    R = (ZZ/3)[x,y,z] / ideal(x^2  + y^2 + z^2);
	    WR = witt(4, R)
///

--------------------------------
--------- WittIdeal
--------------------------------

doc ///
    Key
        WittIdeal

    Headline
        Ideals in Witt rings.
	
    Description
	Text
	    A class for ideals in WittPolynomialRing and WittQuotientRing.
	    It can be built using the wittIdeal method.
	    
	Example
	    R = (ZZ/5)[x,y];
	    WR = witt(2, R);
	    w1 = witt{x,y};
	    w2 = witt{x^2, y^2};
	    w3 = witt{x^3, y^3};
	    WI = wittIdeal(w1, w2, w3)
    SeeAlso
        wittIdeal
	    
///

doc ///
    Key
        wittIdeal
	(wittIdeal, List)
	(wittIdeal, Sequence)
	(wittIdeal, WittRingElement)
    Headline
        Make an ideal in a Witt ring.
	
    Usage
        WI = wittIdeal(L)
	WI = wittIdeal(S)
	WI = wittIdeal(w)
	
    Inputs
        L:List
	    of WittRingElements.
	S:Sequence
	    of WittRingElements.
	w:WittRingElement
	
    Outputs
        WI:WittIdeal
	
    Description
	Text
	    This method builds a wittIdeal from a list or sequence of WittRingElements.
	    Passing a WittRingElement alone gives the principal ideal generated
	    by that WittRingElement.
	    
	Example
	    R = (ZZ/5)[x,y];
	    WR = witt(2, R);
	    w1 = witt{x,y};
	    w2 = witt{x^2, y^2};
	    w3 = witt{x^3, y^3};
	    WI = wittIdeal(w1, w2, w3)
///

doc ///
    Key
        (generators, WittIdeal)
    Headline
        Extract the generators of a WittIdeal.
    Usage
        L = generators(WI)
    Inputs
        WI:WittIdeal
    Outputs
        L:List
    Description
	Text
	    Returns the generators of the WittIdeal. Please note these are returned as a list, as
	    opposed to a matrix.
	Example
	    R = GF(3)[x,y];
	    WR = witt(2, R);
	    w1 = witt{x,y};
	    w2 = witt{x^2,y^2};
	    WI = wittIdeal(w1, w2);
	    generators(WI)
///





--------------------------------
--------- unWitt
--------------------------------

doc ///
    Key
	unWitt
	(unWitt, WittPolynomialRing)
	(unWitt, WittQuotientRing)
    Headline
        Returns the underlying ring R of a Witt ring W_n(R)
    Usage
	R = unWitt(WR)
    Inputs
	WR: WittPolynomialRing
	WR: WittQuotientRing
    Outputs
	R: Ring
    Description
	Text
	    If WR is one of the witt rings of R, unWitt(WR) returns the ring R.
	Example
	    R = (ZZ/3)[x,y,z] / ideal(x^2 + y^2 + z^2);
	    WR = witt(2, R);
	    unWitt(WR)
///

--------------------------------
--------- Frobenius Lifts
--------------------------------

doc ///
    Key
        findFrobeniusLiftConstraints
        [findFrobeniusLiftConstraints, PerturbationTerm]
        [findFrobeniusLiftConstraints, Homogeneous]
    Headline
        Finds the equations satisfied by a delta structure on a ring R
    Usage
        J = findFrobeniusLiftConstraints I
        J = findFrobeniusLiftConstraints R
    Inputs
        I: Ideal
        f: RingElement
        R: QuotientRing
    Outputs
        J: Ideal
    Description
        Text
            Given an ideal I in a characteristic-p polynomial ring S = (ZZ/p)[x_1..x_n], or a quotient ring R = S/I or a generator f of I, this method returns an ideal J in (S/I)[aa_1,...,aa_n]. The generators for J give the equations satisfied by the values of delta(x_i)=aa_i for the resulting Frobenius lift to descend from W_2(k)[x_1..x_n] to W_2(k)[x_1..x_n]/I. 
        Example
            S = (ZZ/2)[x,y]
            I = ideal(x*y)
            J = findFrobeniusLiftConstraints I
            J = findFrobeniusLiftConstraints (S/I)
        Text
            If the user wants to lift the Frobenius to a different lifting of I to W_2(k)[x_1..x_n], one can use the PerturbationTerm option to specify the coefficients of p in the lift of the defining equations.
        Example
            S = (ZZ/2)[x,y,z]
            I = ideal(x*y,z)
            J = findFrobeniusLiftConstraints(I,PerturbationTerm=>{1,0})
///


doc ///
    Key
        createEquations
        [createEquations, PerturbationTerm]
        [createEquations, Homogeneous]
    Headline
        Finds explicit equations satisfied by the parameters of a Frobenius lift up to a given degree
    Usage
        J = createEquations(d, I)
        J = createEquations(d, f)
        J = createEquations(d, R)
    Inputs
        d: ZZ
        I: Ideal
        f: RingElement
        R: QuotientRing
    Outputs
        J: Ideal
    Description
        Text
            If R = S/I and one represents the choice of a Frobenius lift on W_2(k)(S) by polynomials of degree at most d, this method returns the equations that the coefficients of those polynomials have to satisfy for the resulting Frobenius lift to descend to W_2(k)(S/I).
            The output is an ideal J in variables c_{{a_1..a_n},j}, where c_{{a_1..a_n},j} is the coefficient of x_1^{a_1}..x_n^{a_n} in the polynomial that gives the image of x_j under the Frobenius lift; the exponents a_i satisfy sum a_i \leq d.
        Example
            S = (ZZ/2)[x,y]
            I = ideal(x*y)
            J = createEquations(2, I)
        Text
            If one wants only to find the equations for homogeneous polynomials, one can use the Homogeneous option. One can specify a different lifting of I to W_2(k)[x_1..x_n] by using the PerturbationTerm option to specify the coefficients of p in the lift of the defining equations.
        Example
            S = (ZZ/2)[x,y]
            I = ideal(x*y)
            J = createEquations(2, I,Homogeneous=>true,PerturbationTerm=>{0})
            J = createEquations(2, I,Homogeneous=>true,PerturbationTerm=>{1}) -- no solutions!
///




doc ///
    Key
       findFrobeniusLift
       [findFrobeniusLift, Homogeneous]
       [findFrobeniusLift, PerturbationTerm] 
       [findFrobeniusLift, Nontrivial] 
       [findFrobeniusLift, Verbose]
    Headline
        Finds a random lift of the Frobenius
    Usage
        L = findFrobeniusLift(d, I)
        L = findFrobeniusLift(d, R)
    Inputs
        d: ZZ
        I: Ideal
        R: QuotientRing
    Outputs
        L: List
    Description
        Text
            This methods tries random polynomials of the given degree and checks if they give Frobenius lifts. 
 	    Example 
 	        S = (ZZ/2)[x,y]
 	        I = ideal(x^3+y^5)
            findFrobeniusLift(2,I,Verbose=>true)
        Text 
                This can give a couple of values like (x^2,0) or (0,y^2). Time and number of tries will vary since the polynomials the algorithm tries are random.
        Example 
            S = (ZZ/2)[x,y]
            I = ideal(x^3+y^5)
            findFrobeniusLift(2,I,Nontrivial=>true)
        Text 
                This forces the lift to be nontrivial. Here, it can give a couple of values like (x^2,0) or (0,y^2). 
        Text 
                If there is no Frobenius lift, the algorithm will run without ending.  For example, if S/I is an elliptic curve, by Serre--Tate theory, there is only one (canonical) lifting of S/I that has a Frobenius morphism compatible with that of S/I; if one chooses the "wrong" lift of the equation, there will be no Frobenius lift.
         Text 
                One can also specify a different lift than the default one (which simply lifts the coefficients naively to W_2(k)) by using the PerturbationTerm option, which specifies coefficients of p in the lift of the defining equations
        Example
                S=(ZZ/3)[x,y]
                I=ideal(x*y)
                findFrobeniusLift(2,I, PerturbationTerm=>{x}) -- this gives a lift to the ring W_2(k)[x,y]/(x*y + p*x)
///

end
