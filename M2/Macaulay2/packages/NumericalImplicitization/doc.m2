
--Documention--
--<<docTemplate
doc ///
    Key
    	NumericalImplicitization
    Headline
    	implicitization using numerical algebraic geometry
    Description
    	Text
	    This package supports user-friendly calculation of basic invariants of the image 
            of a polynomial map. The computational techniques (interpolation, homotopy 
            continuation and monodromy) come from numerical algebraic geometry.

	    Many varieties of interest in algebraic geometry and its applications are usefully 
            described as images of polynomial maps, via a parametrization. Implicitization is the
            process of converting a parametric description of a variety into an intrinsic, or implicit,
            description. Classically, implicitization refers to the procedure of computing the defining
            equations of a parametrized variety, and in theory this is accomplished by finding the
            kernel of a ring homomorphism, via Gr&ouml;bner bases. In practice however, 
            symbolic Gr&ouml;bner basis computations are often time consuming, even for 
            medium scale problems, and do not scale well with respect to the size of the input.

	    Despite this, one would often like to know basic information about a parametrized 
            variety, even when symbolic methods are prohibitively expensive. Examples of 
	    such information are discrete invariants such as the 
            @TO2{numericalImageDim, "dimension"}@, the 
	    @TO2{pseudoWitnessSet, "degree"}@, or 
            @TO2{numericalHilbertFunction, "Hilbert function"}@ 
	    values. Other examples include Boolean tests, for example whether a particular point 
	    @TO2{isOnImage, "lies on"}@ a parametrized variety. The goal of this package is to
            provide such information; in other words to numerically implicitize a parametrized variety.
    
	    {\em NumericalImplicitization} builds on existing numerical algebraic geometry software: 
	    @TO2{NumericalAlgebraicGeometry,"NAG4M2"}@, @TO Bertini@ and 
            @TO PHCpack@. The user may specify any of these to use for path tracking and 
            point sampling; by default, the native software NAG4M2 is used. Currently, all methods 
            are implemented for reduced and irreducible varieties.
    
	    {\bf Reference:} 
            
            [1] A.J. Sommese and C.W. Wampler, 
            The numerical solution of systems of polynomials.
            {\it World Scientific Publishing} (2005).
///

doc ///
    Key
    	numericalSourceSample
	(numericalSourceSample, Ideal, Thing, ZZ)
        (numericalSourceSample, Ideal, WitnessSet)
        (numericalSourceSample, Ideal, Point)
	(numericalSourceSample, Ideal, ZZ)
        (numericalSourceSample, Ideal)
    Headline
    	samples a general point on a variety
    Usage
        numericalSourceSample(I, W, s)
        numericalSourceSample(I, p, s)
        numericalSourceSample(I, W)
        numericalSourceSample(I, p)
    	numericalSourceSample(I, s)
	numericalSourceSample(I)
    Inputs
	I:Ideal
	    which is prime, specifying a variety $V(I)$
	W:WitnessSet
            a witness set for $V(I)$
        p:Point
            a point on the source $V(I)$
        s:ZZ
	    the number of points to sample on the source $V(I)$
    Outputs
    	:List
	    of sample points on the source $V(I)$
    Consequences
        Item
            If $I$ is not the zero ideal, and a sampling function is not specified via {\tt Software}, 
	    then a numerical irreducible decomposition of $I$ is performed, and cached under 
	    {\tt I.cache.WitnessSet}.
    Description
	Text
	    This method computes a list of sample points on a variety numerically. If $I$ is the 
            zero ideal in a polynomial ring of dimension $n$, then an $n$-tuple of random 
            elements in the ground field is returned. Otherwise, a 
            @TO2{numericalIrreducibleDecomposition, "numerical irreducible decomposition"}@ 
            of $I$ is computed, which is then used to sample points.

	    If the number of points $s$ is unspecified, then it is assumed that $s = 1$.
            
            One can provide a witness set for $V(I)$ if a witness set is already known.

	    In the example below, we sample a point from $A^3$ and then $3$ points from
	    $V(x^2 + y^2 + z^2 - 1)$ in $A^3$.
            
        Example
            R = CC[x,y,z];
            samp = numericalSourceSample(ideal 0_R)
            samp#0
            I = ideal(x^2 + y^2 + z^2 - 1);
            numericalSourceSample(I, 3)
	Text
            
            As of version 2.2.0 (Nov 2020), it is also possible to specify a custom sampling 
	    function: namely, one can specify the value of the option {\tt Software} to be a
	    @TO2{FunctionClosure, "function"}@ which takes in the ideal $I$ and returns
	    a point.
	    
	    The following example shows how to sample a point from SO(5, $\mathbb{R}$).

        Example
            n = 5
            R = RR[a_(1,1)..a_(n,n)]
            A = genericMatrix(R,n,n);
            I = ideal(A*transpose A - id_(R^n));	
	    q = first numericalSourceSample(I, Software => I -> realPoint(I, Iterations => 100))
	    matrix pack(n, q#Coordinates)
	    norm evaluate(gens I, q)
    Caveat
	Since numerical irreducible decompositions are done over @TO CC@, if $I$ is 
	not the zero ideal, then by default the output will be a point in complex space 
	(regardless of the ground field of the ring of $I$).
    SeeAlso
        numericalImageSample
	realPoint
///

doc ///
    Key
	realPoint
	(realPoint, Ideal)
	[realPoint, Tolerance]
	[realPoint, Iterations]
	[realPoint, Initial]
	optimizeNelderMead
	(optimizeNelderMead, FunctionClosure, List)
	(optimizeNelderMead, Ideal)
	[optimizeNelderMead, Tolerance]
	[optimizeNelderMead, Iterations]
	[optimizeNelderMead, Initial]
	lineSearch
	(lineSearch, Ideal, Matrix)
	[lineSearch, Tolerance]
	[lineSearch, Iterations]
	Initial
    Headline
	samples a real point on a variety
    Usage
	realPoint I
    Inputs
	I:Ideal
	    specifying a source variety $V(I)$
    Outputs
    	:Point
	    a sample real point on $V(I)$
    Description
	Text
	    This method samples a real point on a variety numerically, 
	    using a combination of the 
	    @HREF{"https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method", "Nelder-Mead simplex method"}@
	    and @HREF{"https://en.wikipedia.org/wiki/Line_search", "line search"}@
	    with gradient descent.
	    This can be much quicker than performing a numerical irreducible decomposition.
            
            The option {\tt Tolerance} specifies a requested error tolerance for the point, 
	    with respect to the generating set of the ideal.
	    
	    The option {\tt Iterations} specifies an upper limit on the number of iterations
	    to run the approximation algorithms. If this value is too low, then the method
	    will return a point which may not be within the specified error tolerance.

	    The following example shows how to sample a point from the 4 x 5 funtf variety.
            
        Example
            (n,r) = (4,5)
            R = RR[x_(1,1)..x_(n,r)]
            A = transpose genericMatrix(R,r,n)
            I1 = ideal(A*transpose A - (r/n)*id_(R^n));
            I2 = ideal apply(entries transpose A, row -> sum(row, v -> v^2) - 1);
            I = I1 + I2;
            elapsedTime p = realPoint(I, Iterations => 100)
	    matrix pack(5, p#Coordinates)
            norm evaluate(gens I, p)
    SeeAlso
        numericalSourceSample
///

doc ///
    Key
    	numericalImageSample
        (numericalImageSample, Matrix, Ideal, List, ZZ)
	(numericalImageSample, Matrix, Ideal, ZZ)
	(numericalImageSample, Matrix, Ideal)
        (numericalImageSample, List, Ideal, List, ZZ)
        (numericalImageSample, List, Ideal, ZZ)
	(numericalImageSample, List, Ideal)
        (numericalImageSample, RingMap, Ideal, List, ZZ)
        (numericalImageSample, RingMap, Ideal, ZZ)
	(numericalImageSample, RingMap, Ideal)
    Headline
    	samples general points on the image of a variety
    Usage
    	numericalImageSample(F, I, P, s)
        numericalImageSample(F, I, s)
	numericalImageSample(F, I)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	P:List
            of points on $F(V(I))$
        s:ZZ
	    the number of points to sample in $F(V(I))$
    Outputs
    	:List
	    of sample points on $F(V(I)))$
    Description
	Text
	    This method computes a list of sample points on the image of a variety 
            numerically, by calling @TO numericalSourceSample@.

	    If the number of points $s$ is unspecified, then it is assumed that $s = 1$.
            
            One can optionally provide an initial list of points $P$ on $F(V(I))$, which 
            will then be completed to a list of $s$ points on $F(V(I))$.

	    The following example samples a point from the twisted cubic. We then 
            independently verify that this point does lie on the twisted cubic.
            
        Example
            R = CC[s,t];
            F = {s^3,s^2*t,s*t^2,t^3};
            p = first numericalImageSample(F, ideal 0_R)
            A = matrix{p#Coordinates_{0,1,2}, p#Coordinates_{1,2,3}};
	    numericalNullity A == 2
    	Text
        
	    Here is how to sample a point from the Grassmannian $Gr(2,4)$ of 
	    $P^1$'s in $P^3$, under its Pl&uuml;cker embedding in $P^5$.
            We take maximal minors of a $2 x 4$ matrix, whose row span
            gives a $P^1$ in $P^3$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            numericalImageSample(F, ideal 0_R)
    SeeAlso
        numericalSourceSample
///

doc ///
    Key
    	numericalImageDim
	(numericalImageDim, Matrix, Ideal, Point)
	(numericalImageDim, Matrix, Ideal)
        (numericalImageDim, List, Ideal, Point)
	(numericalImageDim, List, Ideal)
        (numericalImageDim, RingMap, Ideal, Point)
	(numericalImageDim, RingMap, Ideal)
    Headline
    	computes the dimension of the image of a variety
    Usage
    	numericalImageDim(F, I, p)
	numericalImageDim(F, I)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a sample point on the source $V(I)$
    Outputs
    	:ZZ
	    the dimension of $F(V(I)))$
    Description
	Text
	    The method computes the dimension of the image of a variety numerically. 
	    Even if the source variety and map are projective, the affine (Krull) 
            dimension is returned. This ensures consistency with @TO dim@.

	    The following example computes the affine dimension of the Grassmannian 
            $Gr(2,4)$ of $P^1$'s in $P^3$, under its Pl&uuml;cker embedding in $P^5$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            numericalImageDim(F, ideal 0_R)
        Text
        
            For comparison, here is how to do the same computation symbolically.
            
        Example
            R = QQ[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            dim ker map(R,QQ[y_0..y_(#F-1)],F)
        Text
        
            Next is an example where direct symbolic computation fails to terminate quickly. 
	    Part of the Alexander-Hirschowitz theorem states that the $14$th secant 
	    variety of the $4$th Veronese of $P^4$ has affine dimension $69$, rather than 
	    the expected $14*4 + 13 + 1 = 70$. See J. Alexander, A. Hirschowitz, $Polynomial
            interpolation in several variables$, J. Alg. Geom. 4(2) (1995), 201-222. We 
            numerically verify this below.
            
        Example
            R = CC[a_(1,1)..a_(14,5)];
            F = sum(1..14, i -> basis(4, R, Variables=>toList(a_(i,1)..a_(i,5))));
            time numericalImageDim(F, ideal 0_R)
///

doc ///
    Key
        numericalNullity
        (numericalNullity, Matrix)
        (numericalNullity, Matrix, Boolean)
        (numericalNullity, List, Boolean)
        Precondition
	[numericalHilbertFunction, Precondition]
        [numericalNullity, Precondition]
	SVDGap
	[numericalHilbertFunction, SVDGap]
        [numericalNullity, SVDGap]
    Headline
        computes numerical kernel dimension of a matrix
    Usage
        numericalNullity M
    Inputs
        M:Matrix
            with real or complex entries
    Outputs
        :ZZ
            dimension of the kernel of M
    Description
        Text
            This method computes the dimension of the kernel of a matrix 
            with real or complex entries numerically, via singular value 
            decomposition (see @TO SVD@). 
            
            If $\sigma_1 \ge \ldots \ge \sigma_n$ are the singular values of 
            $M$, then to establish the nullity numerically we look for the 
	    largest "significant" gap between two consecutive singular values, where 
            the gap between $\sigma_i$ and $\sigma_{i+1}$ is "significant" if the ratio 
	    $\sigma_i / \sigma_{i+1}$ exceeds the value of {\tt SVDGap}.
	    If a gap is found which is greater than this threshold, then all singular values 
            after this gap are considered as numerically zero; if all gaps are 
            less than this threshold, then the matrix is considered numerically full rank.
	    The default value of {\tt SVDGap} is $1e5$.
            
            The option {\tt Precondition} specifies whether the rows of 
	    M will be normalized to have norm $1$ before computing the SVD.
            This helps reveal nullity if the matrix is dense (e.g. for a generic 
            interpolation matrix), but not if the matrix is sparse (e.g. diagonal).
	    The default value is @TO false@.
            
        Example
            numericalNullity(matrix{{2, 1}, {0, 1e-5}}, Precondition => false)
            numericalNullity(map(CC^2,CC^2,0))    
    Caveat
        The option {\tt SVDGap} may require tuning by the user.
    SeeAlso
        SVD
        numericalRank
///

doc ///
    Key
    	numericalHilbertFunction
	(numericalHilbertFunction, Matrix, Ideal, List, ZZ)
	(numericalHilbertFunction, Matrix, Ideal, ZZ)
        (numericalHilbertFunction, List, Ideal, List, ZZ)
	(numericalHilbertFunction, List, Ideal, ZZ)
        (numericalHilbertFunction, RingMap, Ideal, List, ZZ)
	(numericalHilbertFunction, RingMap, Ideal, ZZ)
        UseSLP
        [numericalHilbertFunction, UseSLP]
    Headline
    	computes the values of the Hilbert function for the image of a variety
    Usage
    	numericalHilbertFunction(F, I, S, d)
	numericalHilbertFunction(F, I, d)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	S:List
	    of general points on $F(V(I))$
    	d:ZZ
	    the argument of the Hilbert function of $F(V(I))$
    Outputs
    	:NumericalInterpolationTable
	    containing the number of linearly independent degree $d$ 
            forms in the ideal of the projective closure of $F(V(I))$, 
            along with approximations of those forms
    Description
	Text
	    This method computes values of the Hilbert function of the 
            image of a variety, by numerical interpolation. In more detail, 
            given a list $S$ of general points on $F(V(I))$ and a degree 
            $d$, the method forms a matrix whose entries are the 
            evaluations of monomials of degree $d$ at points in $S$. 
            The kernel of this interpolation matrix gives degree $d$ 
            equations of the image (provided the number of points in $S$
            is at least the number of degree $d$ monomials). This 
            technique circumvents the calculation of the kernel of the 
            associated ring map.

            In order to speed up computation, the list $S$ of points 
            can be precomputed (see @TO numericalImageSample@). 
            This list of points can then be re-used in multiple 
            interpolation computations (which can yield a large 
            speedup over performing separate sampling instances, 
            if the ideal $I$ is not the zero ideal).
            
            For a further speedup, the option {\tt UseSLP} allows for 
            the usage of @TO2{SLPexpressions, "straight-line programs"}@
            in creating the interpolation matrix.

            In the following, we compute the dimension of the space of 
            quartics in the ideal of the twisted cubic and obtain the expected 
            answer, $22$. Note that one can verify this by dimension counting:
            quartics in the coordinate ring pull back to forms of degree 
            $12$ on $P^1$, of which there is a $13$-dimensional
            space; thus the space of quartics in the 
            defining ideal has dimension $35 - 13 = 22$.
            
        Example
            R = CC[s,t]
            F = basis(3, R)
            numericalHilbertFunction(F, ideal 0_R, 4)
        Text
        
            The following example computes the dimension of Pl&uuml;cker quadrics in 
            the defining ideal of the Grassmannian $Gr(2,4)$ of $P^1$'s in $P^3$,
	    in the ambient space $P^5$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            S = numericalImageSample(F, ideal 0_R, 60);
            numericalHilbertFunction(F, ideal 0_R, S, 2, UseSLP => true)
    SeeAlso
    	NumericalInterpolationTable
        extractImageEquations
///

doc ///
    Key
    	NumericalInterpolationTable
        (net, NumericalInterpolationTable)
        hilbertFunctionArgument
        hilbertFunctionValue
        imagePoints
	interpolationBasis
        interpolationSVD
        interpolationMatrix
    Headline
    	the class of all NumericalInterpolationTables
    Description
	Text
    	    This is a type of hash table storing the output of a 
            polynomial interpolation computation, with the following keys: 
        Code
            UL {
                TEX "\\bf hilbertFunctionArgument: the argument, $d$, to the Hilbert function",
                TEX "\\bf hilbertFunctionValue: the value of the Hilbert function at $d$",
                TEX "\\bf imagePoints: a (vertical) list of sample points on the image",
		TEX "\\bf interpolationBasis: a matrix consisting of the degree $d$ monomials",
                TEX "\\bf interpolationSVD: the singular value decomposition of the interpolation matrix",
                TEX "\\bf interpolationMatrix: the matrix obtained by evaluating degree $d$ monomials at the sample points",
		TEX "\\bf map: the map $F$, of which the image is under consideration"
                }
        Example
            R = CC[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            T = numericalHilbertFunction(F, ideal 0_R, 2, Verbose => false)
            (T.hilbertFunctionArgument, T.hilbertFunctionValue)
    SeeAlso
    	numericalHilbertFunction
///

doc ///
    Key
    	extractImageEquations
        (extractImageEquations, Matrix, Ideal, ZZ)
        (extractImageEquations, List, Ideal, ZZ)
        (extractImageEquations, RingMap, Ideal, ZZ)
	(extractImageEquations, NumericalInterpolationTable)
        [extractImageEquations, Threshold]
        AttemptZZ
        [extractImageEquations, AttemptZZ]
    Headline
    	finds implicit equations in a fixed degree for the image of a variety
    Usage
        extractImageEquations(F, I, d)
    	extractImageEquations T
    Inputs
        T:NumericalInterpolationTable
            a numerical interpolation table for $F(V(I))$ of degree $d$
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    	d:ZZ
	    the argument of the Hilbert function of $F(V(I))$
    Outputs
    	:Matrix
	    of implicit degree d equations for $F(V(I))$
    Description
	Text
	    This method finds (approximate) implicit degree $d$ equations for the image 
            of a variety, by @TO2{numericalHilbertFunction, "numerical interpolation"}@. 
            The option {\tt AttemptZZ} specifies whether to use the @TO LLL@ algorithm
            to compute "short" equations over @TO ZZ@. The default value is @TO false@.

	    If a numerical interpolation table has already been computed, then 
            to avoid repetitive calculation one may run this function with the interpolation 
            table as input.

            For example, we determine the defining quadrics of the twisted cubic, as follows.
            
        Example
            R = CC[s,t]
            F = basis(3, R)
            extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
        Text
        
            Here is how to do the same computation symbolically.
            
        Example
            gens ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
        
	    We determine the $5$ Pl&uuml;cker quadrics defining the Grassmannian 
            $Gr(3,5)$ of $P^2$'s in $P^4$, in the ambient space $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
	    T = numericalHilbertFunction(F, I, 2, Verbose => false);
	    extractImageEquations(T, AttemptZZ => true)
        Text
        
    	    The option {\tt Threshold} sets the threshold for rounding the interpolation matrix. 
            If this option has value $n$, then the interpolation matrix will be rounded
            to $n$ decimal digits, after which LLL will be performed. The default value is $5$.
    SeeAlso
    	numericalHilbertFunction
        NumericalInterpolationTable
///

doc ///
    Key
    	numericalImageDegree
	(numericalImageDegree, PseudoWitnessSet)
	(numericalImageDegree, Matrix, Ideal)
        (numericalImageDegree, List, Ideal)
        (numericalImageDegree, RingMap, Ideal)
    Headline
    	computes the degree of the image of a variety
    Usage
    	numericalImageDegree W
	numericalImageDegree(F, I)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for $F(V(I))$
        F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    Outputs
    	:ZZ
	    the degree of $F(V(I))$
    Description
	Text
	    This method computes the degree of the image of a variety, 
	    by computing a pseudo-witness set for the image (cf.
	    @TO2{pseudoWitnessSet, "pseudo-witness set"}@ for more on the
	    techniques and options used).

            If a pseudo-witness set has already been computed, then 
            to avoid repetitive calculation one may run this function with the 
            pseudo-witness set as input.

            The following example determines the degree of the
            Grassmannian $Gr(2,4)$ of $P^1$'s in $P^3$, 
	    under its Pl&uuml;cker embedding in $P^5$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)]; I = ideal 0_R;
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
	    numericalImageDegree(F, I, Repeats => 2, Verbose => false)
    SeeAlso
    	pseudoWitnessSet
	PseudoWitnessSet
///

doc ///
    Key
    	pseudoWitnessSet
        (pseudoWitnessSet, Matrix, Ideal)
	(pseudoWitnessSet, Matrix, Ideal, Point)
	(pseudoWitnessSet, Matrix, Ideal, List, Thing)
        (pseudoWitnessSet, List, Ideal)
	(pseudoWitnessSet, List, Ideal, Point)
	(pseudoWitnessSet, List, Ideal, List, Thing)
        (pseudoWitnessSet, RingMap, Ideal)
	(pseudoWitnessSet, RingMap, Ideal, Point)
	(pseudoWitnessSet, RingMap, Ideal, List, Thing)
        Repeats
    	[pseudoWitnessSet, Repeats]
	[numericalImageDegree, Repeats]
        MaxAttempts
    	[pseudoWitnessSet, MaxAttempts]
	[numericalImageDegree, MaxAttempts]
        MaxPoints
    	[pseudoWitnessSet, MaxPoints]
        [numericalImageDegree, MaxPoints]
	DoRefinements
        [pseudoWitnessSet, DoRefinements]
	[numericalImageDegree, DoRefinements]
	DoTraceTest
	[pseudoWitnessSet, DoTraceTest]
        [numericalImageDegree, DoTraceTest]
	TraceThreshold
    	[pseudoWitnessSet, TraceThreshold]
    	[numericalImageDegree, TraceThreshold]
	[pseudoWitnessSet, Threshold]
        [numericalImageDegree, Threshold]
	[isOnImage, Threshold]
    Headline
    	computes a pseudo-witness set for the image of a variety
    Usage
	pseudoWitnessSet(F, I)
	pseudoWitnessSet(F, I, p)
	pseudoWitnessSet(F, I, P, L)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a general point on the source $V(I)$
	P:List
	    of pairs $(p, q)$ with $p$ a general point on the source $V(I)$,
	    and $q = F(p)$. In this case an input slice $L$ must also be 
            provided, and $q$ should additionally lie on $L$.
	L:Matrix
	    representing a linear slice of $F(V(I))$. The format
	    should be a row matrix, whose entries are linear forms
	    in the ambient target space of $F(V(I))$. If this is provided
	    then a nonempty list $P$ of point pairs must also be provided.
    Outputs
    	:PseudoWitnessSet
	    a pseudo-witness set for $F(V(I))$
    Description
	Text
	    This method computes a @TO2{PseudoWitnessSet, "pseudo-witness set"}@
            for the image of a variety, by computing the intersection of the 
	    image with a complementary-dimensional linear slice via tracking 
	    monodromy loops with homotopy continuation, and then applying the 
            trace test. If the trace test fails, only a 
            lower bound for the degree and an incomplete pseudo-witness set 
            is returned. This technique circumvents the calculation of the 
            kernel of the associated ring map.
	    
	    The method also allows the user to provide a particular linear slice $L$ of the 
	    image. In this case a list of point pairs $(p, q)$ such that $p$ is in $V(I)$,
	    $q = F(p)$, and $q$ is in $L$, must be provided (to have an initial input point to 
	    the monodromy - even if it only consists of a single such pair). 
	    The method then applies monodromy to try to compute the entire intersection 
	    $F(V(I))\cap L$. If no linear slice is given, then a random 
	    complementary-dimensional linear slice will be chosen, in which case no 
	    seed is needed, as an initial point pair will be chosen to lie on the slice.

            The following example computes the degree of the Grassmannian 
            $Gr(2,4)$ of $P^1$'s in $P^3$, under its Pl&uuml;cker embedding in $P^5$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)];
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            W = pseudoWitnessSet(F, ideal 0_R)
            W.isCompletePseudoWitnessSet
            W.degree
        Text
        
            This method can also handle cases where the parameterization 
            has positive dimensional fibers. In the example below, we verify that 
            the variety of $3 x 3 x 3$ tensors of border rank $<= 4$, i.e. the $4$th secant 
            variety of $P^2 x P^2 x P^2$, has degree $9$. This is a hypersurface, 
            with defining equation known as Strassen's invariant,
            and it is also a defective secant variety (meaning its dimension is less
            than expected). Here, the parametrization has $10$ dimensional fibers.
	    For more on this example, see V. Strassen, $The asymptotic spectrum of tensors$, 
	    J. Reine Angew. Math. 384 (1988), 102-152.
            
        CannedExample
            i6 : R = CC[a_(0,0)..a_(3,2), b_(0,0)..b_(3,2), c_(0,0)..c_(3,2)];
            
            i7 : F = toList apply((0,0,0)..(2,2,2), (i,j,k) ->
                    a_(0,i)*b_(0,j)*c_(0,k) +
                    a_(1,i)*b_(1,j)*c_(1,k) +
                    a_(2,i)*b_(2,j)*c_(2,k) +
                    a_(3,i)*b_(3,j)*c_(3,k));
                    
            i8 : pseudoWitnessSet(F, ideal 0_R, Repeats => 2)
            Sampling point in source ...
            Tracking monodromy loops ...
            Points found: 1
            Points found: 2
            Points found: 3
            Points found: 5
            Points found: 7
            Points found: 9
            Points found: 9
            Points found: 9
            Running trace test ...
            
            o8 = a pseudo-witness set, indicating
                the degree of the image is 9
            
            o8 : PseudoWitnessSet
        Text
        
            Finally, this method has a large number of optional inputs which may be 
            specified by the user to fit a particular problem instance. 

    	    The option {\tt Repeats} sets the maximum number of consecutive repetitive 
            monodromy loops when computing a pseudo-witness set. A repetitive 
            monodromy loop is one where no new points in the image are discovered. 
            After this many consecutive repetitive monodromy loops occur, the trace 
            test is applied to determine if a complete pseudo-witness set has 
            been found. The default value is $3$.

    	    The option {\tt MaxAttempts} sets the maximum number of times the trace test 
            will be attempted when computing a pseudo-witness set. After a trace test 
            fails, a new slice is chosen, the previous points are tracked to the new 
            slice, and monodromy is performed anew. If the trace test has failed 
            {\tt MaxAttempts} many times, an incomplete pseudo-witness set is returned. 
            The default value is $5$.
            
            Here is an example in which a badly chosen random seed results in a 
            failed trace test on the first attempt.  In later attempts, the trace test 
            passes and the degree of the twisted cubic is correctly computed to be $3$.
            
        Example
            setRandomSeed 10
            R = CC[s,t]
            F = basis(3, R)
            pseudoWitnessSet(F, ideal 0_R)
        Text
        
            We compare this with the native $Macaulay2$ function 
	    @TO2{(degree, Ideal), "degree"}@ (using a symbolic Gr&ouml;bner basis computation).
            
        Example
            degree ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
            
            The option {\tt MaxPoints} sets a number of points such that if more than this 
	    number of points is found following a monodromy loop, then the method gracefully 
	    exits. The option is especially useful in the case that the user specifies a 
	    linear slice $L$ (as discussed above) which is in special position with respect to 
	    $F(V(I))$ (e.g. if $F(V(I))\cap L$ is positive-dimensional). The default value 
	    is @TO infinity@.
            
            The option {\tt DoRefinements} specifies whether or not to refine solution points found 
            via monodromy. Refinement of points may improve their accuracy. If the value of this
            option is true, then refinement occurs after every tracking (which may increase the time 
            for computation). The default value is @TO false@.
	    
	    The option {\tt DoTraceTest} specifies whether or not to run the trace test. This is
	    useful when the user specifies a special linear slice $L$ (as in the discussion on
	    {\tt MaxPoints} above). The default value is @TO true@.
            
    	    The option {\tt TraceThreshold} sets the threshold for a pseudo-witness set to pass 
            the trace test. The trace for a complete exact pseudo-witness set is 
            $0$; large nonzero values indicate failure (the larger the value, the worse 
            the failure). The default value is $1e-5$.

    	    The option {\tt Threshold} sets the threshold for determing point equality. 
            If this option has value $n$, then two points are considered equal iff their 
            first $n$ significant digits agree (equivalently, in scientific notation, the 
            exponents and first $n$ digits of the mantissa agree). The default value is $5$. 
    SeeAlso
    	PseudoWitnessSet
	numericalImageDegree
///

doc ///
    Key
        PseudoWitnessSet
        (net, PseudoWitnessSet)
        isCompletePseudoWitnessSet
	sourceEquations
        sourceSlice
        generalCombinations
        imageSlice
        witnessPointPairs
    Headline
    	the class of all pseudo-witness sets
    Description
	Text
            This is a type of hash table storing the output of a 
            pseudo-witness set computation using monodromy, 
            with the following keys:

        Code
            UL {
                {TEX "\\bf isCompletePseudoWitnessSet: whether the pseudo-witness set has passed the trace test, according to the trace test threshold"},
                TEX "\\bf degree: the number of image points found by monodromy",
                TEX "\\bf map: the map $F$, of which the image is under consideration",
                TEX "\\bf sourceEquations: the defining ideal $I$ of the source variety",
                {TEX "\\bf sourceSlice: additional equations to form a zero-dimensional system (only needed if the map is not finite-to-one)"},
                {TEX "\\bf generalCombinations: additional equations to form a zero-dimensional system (only needed if the source ideal is not a complete intersection)"},
                TEX "\\bf imageSlice: the pullback under F of a general complementary-dimensional linear space to $F(V(I))$",
                {TEX "\\bf witnessPointPairs: a vertical list of 2-point sequences $(p, F(p))$, where $p$ lies on the source $V(I)$ and $F(p)$ lies on imageSlice"},
                TEX "\\bf trace: the result of the trace test applied to witnessPointPairs"
                }
        Text
	    For a discussion of pseudo-witness sets, 
	    see J.D. Hauenstein and A.J. Sommese, $Witness sets of projections$, 
	    Appl. Math. Comput. 217(7) (2010), 3349-3354. 
	    
	    The following example demonstrates the output for the 
            degree $3$ embedding of $P^1$ into $P^3$, whose image is the twisted cubic.
            
	Example
            R = CC[s,t];
            W = pseudoWitnessSet(basis(3,R), ideal 0_R, Verbose => false);
            peek W
    SeeAlso
    	pseudoWitnessSet
	numericalImageDegree
///

doc ///
    Key
    	isOnImage
	(isOnImage, PseudoWitnessSet, Point)
	(isOnImage, Matrix, Ideal, Point)
        (isOnImage, List, Ideal, Point)
        (isOnImage, RingMap, Ideal, Point)
    Headline
    	tests whether a point lies on the image of a variety
    Usage
    	isOnImage(W, p)
	isOnImage(F, I, p)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for $F(V(I))$
	p:Point
	    a point in the ambient space of $F(V(I))$
        F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    Outputs
    	:Boolean
	    whether the point $p$ lies on $F(V(I))$
    Description
	Text
	    This method determines if a point in the ambient target space 
            lies on the image of a variety. This is done via computing a 
            pseudo-witness set for the image.

            If a pseudo-witness set has already been computed, then 
            to avoid repetitive calculation one may run this function with the 
            pseudo-witness set as input.

            The following example determines whether a point lies on the
            Grassmannian $Gr(2,4)$ of $P^1$'s in $P^3$, 
	    under its Pl&uuml;cker embedding in $P^5$.
            
        Example
            R = CC[x_(1,1)..x_(2,4)]; I = ideal 0_R;
            F = (minors(2, genericMatrix(R, 2, 4)))_*;
            W = pseudoWitnessSet(F, I, Repeats => 2, Verbose => false);
            q = first numericalImageSample(F, I)
            isOnImage(W, q)
            isOnImage(W, point random(CC^1, CC^#F))
            isOnImage(W, point{{1_CC,0,0,0,0,0}})
    SeeAlso
    	pseudoWitnessSet
	PseudoWitnessSet
///

doc ///
    Key
        [numericalImageDegree, Verbose]
	[pseudoWitnessSet, Verbose]
        [numericalHilbertFunction, Verbose]
        [numericalNullity, Verbose]
        [isOnImage, Verbose]
    Headline
    	display detailed output
    Usage
        pseudoWitnessSet(..., Verbose => true)
	numericalImageDegree(..., Verbose => true)
	numericalHilbertFunction(..., Verbose => true)
	isOnImage(..., Verbose => true)
        numericalNullity..., Verbose => true)
    Description
	Text
    	    This option determines whether detailed output is displayed 
            during an interpolation or monodromy computation, 
            including timings for various intermediate computations. 
            The default value is @TO true@.
    SeeAlso
        numericalHilbertFunction
	numericalImageDegree
    	pseudoWitnessSet
	isOnImage
///

doc ///
    Key
        ConvertToCone
        [numericalHilbertFunction, ConvertToCone]
    Headline
        specifies whether to convert image to a cone
    Usage
        numericalHilbertFunction(..., ConvertToCone => false)
    Description
        Text
            This option specifies whether to replace the image $F(V(I))$ with 
	    the cone over $F(V(I))$. 
	    If true, then internally the target variety is treated
            as the affine cone over its projective closure - to be precise,
	    the map $F$ is replaced with $t[F, 1]$, where $t$ is a new variable. 
	    The default value is @TO false@.
	    
	    Since @TO numericalHilbertFunction@ works by interpolating monomials
	    (and thus only finds graded relations in the ideal of the image), 
	    this option is necessary when the map is not homogeneous.
	    The following example demonstrates this for an affine rational curve.
	    
    	Example
	    R = CC[t]
	    F = {t, t^4, t^6}
	    I = ideal 0_R
	    (numericalHilbertFunction(F, I, 3, Verbose => false)).hilbertFunctionValue == 0
	    T = numericalHilbertFunction(F, I, 3, ConvertToCone => true)
	    extractImageEquations(T, AttemptZZ => true)
    SeeAlso
        numericalHilbertFunction
///

doc ///
    Key
        MaxThreads
	[numericalImageDegree, MaxThreads]
    	[pseudoWitnessSet, MaxThreads]
        [isOnImage, MaxThreads]
    Headline
    	specifies the maximum number of processor threads
    Usage
        pseudoWitnessSet(..., MaxThreads => allowableThreads)
	numericalImageDegree(..., MaxThreads => allowableThreads)
	isOnImage(..., MaxThreads => allowableThreads)
    Description
	Text
    	    This option sets the maximum number of processor threads that will be used 
            for parallel computation. This distributes the paths to track in each 
            monodromy loop among the processors as evenly as possible. 
            The value of this option should not exceed the value of the variable 
            {\tt allowableThreads}. The default value is $1$.
    Caveat
        Parallel computation in $Macaulay2$ is under development. Unexpected errors 
        may be printed to output while computing a pseudo-witness set - however, the 
        loop will still run, and an answer will still be returned.
        
        If the number of paths to track is too low (i.e. less than or equal to $20$), 
	then parallel computing will not be used.
    SeeAlso
    	numericalImageDegree
	pseudoWitnessSet
	isOnImage
///

doc ///
    Key
        [pseudoWitnessSet, Software]
	[numericalImageDegree, Software]
	[numericalSourceSample, Software]
        [numericalImageSample, Software]
        [numericalImageDim, Software]
        [numericalHilbertFunction, Software]
        [isOnImage, Software]
    Headline
    	specify software for homotopy continuation
    Usage
        pseudoWitnessSet(..., Software => M2engine)
	numericalImageDegree(..., Software => M2engine)
        numericalImageSample(..., Software => M2engine)
        numericalImageDim(..., Software => M2engine)
        numericalHilbertFunction(..., Software => M2engine)
        isOnImage(..., Software => M2engine)
    Description
	Text
    	    This option specifies the software used for polynomial homotopy 
            continuation (used for path tracking) and numerical irreducible 
            decompositions (used for sampling points). The default value is 
            M2engine (native to $Macaulay2$). Other possible values are 
            @TO Bertini@ and @TO PHCpack@ (only if the user has these 
            packages installed).
    SeeAlso
        pseudoWitnessSet
	numericalImageDegree
	numericalSourceSample
        numericalImageSample
        numericalImageDim
        numericalHilbertFunction
        isOnImage
///

undocumented {
    numericalEval,
    (numericalEval, Matrix, List, Boolean),
    (isWellDefined, PseudoWitnessSet),
    (isWellDefined, NumericalInterpolationTable),
    [lineSearch, Initial]
}
