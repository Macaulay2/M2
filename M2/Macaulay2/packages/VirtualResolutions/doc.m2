undocumented {
    (multigradedRegularity, Thing, Ideal),
    (multigradedRegularity, Thing, Thing, Module)
    }

doc ///
  Key
    VirtualResolutions
  Headline
    a package for computing virtual resolutions
  Description
    Text
     While graded minimal free resolutions are useful for studying quasicoherent
     sheaves over projective space, when working over a product of projective spaces or, more generally,
     over smooth projective toric varieties, graded minimal free resolutions over the Cox ring
     seem too restricted by algebraic structure that is in some sense unimportant geometrically. By allowing
     a limited amount of homology, virtual resolutions offer a more flexible alternative for
     studying toric subvarieties when compared to minimal graded free resolutions.

     Introduced by Berkesch, Erman, and Smith in {\em Virtual resolutions for a product of projective spaces}
     (see @{HREF("http://arxiv.org/abs/1703.07631","arXiv:1703.07631")}@) if $X$ is a smooth toric variety, $S$ is the Cox ring of $X$
     graded by the Picard group of $X$, and $B\subset S$ is the irrelevant ideal of $X$, then
     a virtual resolution of a graded $S$-module $M$ is a complex of graded free $S$-modules, which
     sheafifies to a resolution of the associated sheaf of $M$.

     This package provides tools for constructing and studying virtual resolutions for products of projective spaces.
     In particular, it implements a number of the methods for constructing virtual resolutions for products of projective
     spaces as introduced by Berkesch, Erman, and Smith. This package also contains methods for constructing curves in
     $\mathbb{P}^1\times\mathbb{P}^2$, as these are a natural source for interesting virtual resolutions.

     As a running example, consider three points $([1:1],[1:4])$, $([1:2],[1:5])$, and $([1:3],[1:6])$
     in $\mathbb{P}^1 \times \mathbb{P}^1$.

    Example
     X = toricProjectiveSpace(1)**toricProjectiveSpace(1);
     S = ring X;
     B = ideal X;
     J = saturate(intersect(
         ideal(x_1 - x_0, x_3 - 4*x_2),
         ideal(x_1 - 2*x_0, x_3 - 5*x_2),
         ideal(x_1 - 3*x_0, x_3 - 6*x_2)), B);
     minres = res J;
     multigraded betti minres
    Text
     As described in Algorithm 3.4 of Berkesch, Erman, and Smith's
     paper, one may construct a virtual resolution of a module from its graded minimal free resolution and
     an element of the multigraded Castelnuovo-Mumford regularity of the module. (See Maclagan and Smith's paper
     {\em Multigraded Castelnuovo-Mumford Regularity} (@{HREF("http://arxiv.org/abs/math/0305214","arXiv:math/0305214")}@) for the definition of multigraded regularity.)
     Building on the TateOnProducts package, this package contains a function allowing one
     to compute the minimal elements of the multigraded Castelnuovo-Mumford regularity of a $B$-saturated module.

     Continuing the example from above, we see that $(2,0)$ is an element of the multigraded
     regularity of $S/J$. From this we can compute a virtual resolution of $S/J$.
    Example
     multigradedRegularity(X, J)
     vres = virtualOfPair(J, {{3,1}})
     multigraded betti vres
    Text
     Notice that this virtual resolution of $S/J$ is much shorter and thinner than the graded minimal
     free resolution of $S/J$. This is a common theme: virtual resolutions tend to be much
     shorter and less wide than graded minimal free resolutions over the Cox ring, but they still
     preserve geometric information about $S/J$.

     In addition to the functions highlighted above, the VirtualResolutions package contains
     a number of other tools for constructing and studying virtual resolutions. In particular,
     there are functions to construct virtual resolutions for zero dimensionsal subschemes, to
     check whether a complex is a virtual resolution, and to construct curves in $\mathbb{P}^1\times\mathbb{P}^2$.
    Text
       @SUBSECTION "Contributors"@
    Text
       The following people have generously contributed code or worked on
       our code.
    Text
       @UL {
           {HREF("http://www.math.wisc.edu/~derman/","Daniel Erman")},
           {HREF("https://mast.queensu.ca/~ggsmith/","Gregory G. Smith")},
           }@
///

doc ///
    Key
        isVirtual
        (isVirtual,Ideal,ChainComplex)
        (isVirtual,NormalToricVariety,ChainComplex)
    Headline
        checks whether a chain complex is a virtual resolution
    Usage
        isVirtual(irr,C)
        isVirtual(X,C)
    Inputs
        irr:Ideal
            irrelevant ideal of the ring
        X:NormalToricVariety
            normal toric variety
        C:ChainComplex
            chain complex we want to check if is a virtual resolution
    Outputs
        :Boolean
            true if C is a virtual resolution of I
            false if not
    Description
        Text
            Given the irrelevant ideal irr of a NormalToricVariety and a chain complex C, isVirtual returns true if
            C is a virtual resolution of some module. If not, it returns false. This is done by checking that the 
	    higher homology groups of C are supported on the irrelevant ideal.

            If debugLevel is larger than zero, the homological degree where isVirtual fails is printed.
        Example
          R = ZZ/101[s,t];
          isVirtual(ideal(s,t),res ideal(t))
        Text
          Continuing our running example of three points $([1:1],[1:4])$, $([1:2],[1:5])$, and $([1:3],[1:6])$
          in $\mathbb{P}^1 \times \mathbb{P}^1$, we can check whether the virtual complex we compute below and
          in other places is in fact virtual.
        Example
          Y = toricProjectiveSpace(1)**toricProjectiveSpace(1);
          S = ring Y;
          B = ideal Y;
          J = saturate(intersect(
             ideal(x_1 - x_0, x_3 - 4*x_2),
             ideal(x_1 - 2*x_0, x_3 - 5*x_2),
             ideal(x_1 - 3*x_0, x_3 - 6*x_2)), B);
          minres = res J;
          vres = virtualOfPair(J,{{3,1}});
          isVirtual(B,vres)
        Text
          Finally, we can also use the Determinantal strategy, which implements Theorem 1.3 of
          @{HREF("http://arxiv.org/abs/1904.05994","arXiv:1904.05994")}@.
        Example
          isVirtual(B,vres,Strategy=>Determinantal)
///

doc ///
    Key
        [isVirtual, Strategy]
    Headline
        changes strategy from computing homology to computing minors of boundary maps
    Description
        Text
            If Strategy is set to "Determinantal", isVirtual will check whether the given chain complex
            is a virtual resolution by checking the depth of the saturation of the ideals of maximal rank
            from the boundary maps. See Theorem 1.3 of @{HREF("http://arxiv.org/abs/1904.05994","arXiv:1904.05994")}@.
    SeeAlso
        isVirtual
///

doc ///
    Key
        idealSheafGens
        (idealSheafGens,ZZ,Ideal,Ideal)
        (idealSheafGens,ZZ,Ideal,NormalToricVariety)
    Headline
        creates a list of subsets of the minimal generators that generate a given ideal up to saturation
    Usage
        idealSheafGens(n,I,irr)
        idealSheafGens(n,I,X)
    Inputs
        I:Ideal
        n:ZZ
            size of subset of minimal generators of I that may generate I up to saturation with irr
        irr:Ideal
            irrelevant ideal
        X:NormalToricVariety
            normal toric variety whose Cox ring contains I
    Outputs
        :List
            all ideals generated by subsets of size n of generators of I that generate I up to saturation with irr
    Description
        Text
            Given an ideal I, integer n, and irrelevant ideal irr, idealSheafGens searches through
            all n-subsets of the generators of I. If a subset generates the same irr-saturated ideal as the
            irr-saturation of I, then the ideal generated by that subset is added to a list.
            After running through all subsets, the list is returned.
        Example
            R = ZZ/101[x_0,x_1,x_2,x_3,x_4,Degrees=>{2:{1,0},3:{0,1}}];
            B = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
            I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
            idealSheafGens(2,I,B)
///

doc ///
    Key
        GeneralElements
        [idealSheafGens, GeneralElements]
    Headline
        combines generators of same degree into a general linear combination
    Description
        Text
            If GeneralElements is set to true, idealSheafGens will replace all generators of I of the same degree with
            a new generator of the that degree which is a general linear combination of those generators, then run
	     idealSheafGens on the new ideal.
    SeeAlso
        idealSheafGens
///

doc ///
    Key
        randomRationalCurve
        (randomRationalCurve,ZZ,ZZ,Ring)
        (randomRationalCurve,ZZ,ZZ)
    Headline
        creates the ideal of a random rational curve of degree (d,e) in P^1xP^2
    Usage
        randomRationalCurve(d,e,F)
        randomRationalCurve(d,e)
    Inputs
        d:ZZ
            degree of curve on the $\mathbb{P}^1$ factor of $\mathbb{P}^1\times\mathbb{P}^2$
        e:ZZ
            degree of curve on the $\mathbb{P}^2$ factor of $\mathbb{P}^1\times\mathbb{P}^2$
        F:Ring
            base ring
    Outputs
        :Ideal
            defining random rational curve in $\mathbb{P}^1\times\mathbb{P}^2$ of degree (d,e) over F.
    Description
        Text
            Given two positive integers d,e and a ring F, randomRationalCurve returns the ideal
            of a random curve in $\mathbb{P}^1\times\mathbb{P}^2$ of degree (d,e) defined over the base ring F.

            This is done by randomly generating two homogenous polynomials of degree d and three homogenous
            polynomials of degree three in F[s,t] defining maps $\mathbb{P}^1\to\mathbb{P}^1$ and $\mathbb{P}^1\to\mathbb{P}^2$,
            respectively. The graph of the product of these two maps in $\mathbb{P}^1\times(\mathbb{P}^1\times\mathbb{P}^2)$ is computed,
            from which a curve of bi-degree (d,e) in $\mathbb{P}^1\times\mathbb{P}^2$ over F is obtained by
            saturating and then eliminating.

            If no base ring is specified, the computations are performed over ZZ/101.
        Example
            randomRationalCurve(2,3,QQ);
            randomRationalCurve(2,3);
    Caveat
        This creates a ring $F[x_{0,0},x_{0,1},x_{1,0},x_{1,1},x_{1,2}]$ in which the resulting ideal is defined.
///

doc ///
    Key
        randomMonomialCurve
        (randomMonomialCurve,ZZ,ZZ,Ring)
        (randomMonomialCurve,ZZ,ZZ)
    Headline
        creates the ideal of a random monomial curve of degree (d,e) in P^1xP^2
    Usage
        randomMonomialCurve(d,e,F)
        randomMonomialCurve(d,e)
    Inputs
        d:ZZ
            degree of curve on the $\mathbb{P}^1$ factor of $\mathbb{P}^1\times\mathbb{P}^2$
        e:ZZ
            degree of curve on the $\mathbb{P}^2$ factor of $\mathbb{P}^1\times\mathbb{P}^2$
        F:Ring
            base ring
    Outputs
        :Ideal
            defining random monomial curve in $\mathbb{P}^1\times\mathbb{P}^2$ of degree (d,e) over F.
    Description
        Text
            Given two positive integers d,e and a ring F, randomMonomialCurve returns the ideal of a random curve
            in $\mathbb{P}^1\times\mathbb{P}^2$ of degree (d,e) defined over the base ring F.

            This is done by randomly generating a monomial m of degree e in F[s,t], which is not s^e or t^e.
            This allows one to define two maps $\mathbb{P}^1\to\mathbb{P}^1$ and $\mathbb{P}^1\to\mathbb{P}^2$
            given by {s^d,t^d} and {s^e,m,t^e}, respectively. The graph of the product of these two maps
            in $\mathbb{P}^1\times(\mathbb{P}^1\times\mathbb{P}^2)$ is computed, from which a curve
            of bi-degree (d,e) in $\mathbb{P}^1\times\mathbb{P}^2$ over F is obtained by saturating and then eliminating.

            If no base ring is specified, the computations are performed over ZZ/101.
        Example
            randomMonomialCurve(2,3,QQ);
    Caveat
        This creates a ring $F[x_{0,0},x_{0,1},x_{1,0},x_{1,1},x_{1,2}]$ in which the resulting ideal is defined.
///

doc ///
    Key
        curveFromP3toP1P2
        (curveFromP3toP1P2,Ideal)
    Headline
        creates the ideal of a curve in P^1xP^2 from the ideal of a curve in P^3
    Usage
        I = curveFromP3toP1P2(J)
    Inputs
        J:Ideal
            defining a curve in $\mathbb{P}^3$.
    Outputs
        I:Ideal
            defining a curve in $\mathbb{P}^1\times\mathbb{P}^2$.
    Description
        Text
            Given an ideal J defining a curve C in $\mathbb{P}^3$, curveFromP3toP1P2 produces the ideal of the curve
            in $\mathbb{P}^1\times\mathbb{P}^2$ defined as follows:
            consider the projections $\mathbb{P}^3\to\mathbb{P}^2$ and $\mathbb{P}^3\to\mathbb{P}^1$ from the point [0:0:0:1]
            and the line [0:0:s:t], respectively. The product of these defines a map from $\mathbb{P}^3$ to $\mathbb{P}^1\times\mathbb{P}^2$.
            The curve produced by curveFromP3toP1P2 is the image of the input curve under this map.

            This computation is done by first constructing the graph in $\mathbb{P}^3\times(\mathbb{P}^1x\mathbb{P}^2)$ of the product
            of the two projections $\mathbb{P}^3\to\mathbb{P}^2$ and $\mathbb{P}^3\to\mathbb{P}^1$ defined above.
            This graph is then intersected with $C\times(\mathbb{P}^1\times\mathbb{P}^2)$. A curve in $\mathbb{P}^1\times\mathbb{P}^2$ is then
            obtained from this by saturating and then eliminating.

            Note the curve in $\mathbb{P}^1\times\mathbb{P}^2$ will have degree and genus equal to the degree and genus of C as long as C
            does not intersect the base locus of the projection. If the option PreserveDegree is set to true, curveFromP3toP1P2 will check whether C
            intersects the base locus. If it does, the function will return an error. If PreserveDegree is set to false, this check is not
            performed and the output curve in $\mathbb{P}^1\times\mathbb{P}^2$ may have degree and genus different from C.
        Example
            R = ZZ/101[z_0,z_1,z_2,z_3];
            J = ideal(z_0*z_2-z_1^2, z_1*z_3-z_2^2, z_0*z_3-z_1*z_2);
            curveFromP3toP1P2(J)
    Caveat
        This creates a ring $F[x_{0,0},x_{0,1},x_{1,0},x_{1,1},x_{1,2}]$ in which the resulting ideal is defined.
///

doc ///
    Key
        PreserveDegree
        [curveFromP3toP1P2, PreserveDegree]
    Headline
        Determines if curve is disjoint from base loci
    Description
      Text
            When set to true, curveFromP3toP1P2 will check whether or not the given curve
            in $\mathbb{P}^3$ intersects the base locus of the projections maps used in this function.
            If this option is set to true and the given curve does intersect the base locus,
            an error is returned.
    SeeAlso
        curveFromP3toP1P2
///

doc ///
    Key
        randomCurveP1P2
        (randomCurveP1P2,ZZ,ZZ,Ring)
        (randomCurveP1P2,ZZ,ZZ)
    Headline
        creates the ideal of a random curve in P^1xP^2
    Usage
        randomCurveP1P2(d,g,F)
        randomCurveP1P2(d,g)
    Inputs
        d:ZZ
            degree of the curve.
        g:ZZ
            genus of the curve.
        F:Ring
            base ring.
    Outputs
        :Ideal
            defining random curve $\mathbb{P}^1\times\mathbb{P}^2$ from a curve of degree d and genus g in $\mathbb{P}^3$ over F.
    Description
        Text
            Given a positive integer d, a non-negative integer g, and a ring F randomCurveP1P2 produces a random curve
            of bi-degree (d,d) and genus g in $\mathbb{P}^1\times\mathbb{P}^2$.
            This is done by using the curve function from the SpaceCurves package to first generate a random curve
            of degree d and genus g in $\mathbb{P}^1\times\mathbb{P}^2$, and then applying curveFromP3toP1P2 to produce a curve in $\mathbb{P}^1\times\mathbb{P}^2$.

            Since curveFromP3toP1P2 relies on projecting from the point [0:0:0:1] and the line [0:0:s:t], randomCurveP1P2
            attempts to find a curve in $\mathbb{P}^3$, which does not intersect the base locus of these projections.
            If the curve did intersect the base locus the resulting curve in $\mathbb{P}^1\times\mathbb{P}^2$ would not have degree (d,d).
            The number of attempts used to try to find such curves is controlled by the Attempt option, which by default is set to 1000.
        Example
            randomCurveP1P2(3,0);
            randomCurveP1P2(3,0,QQ);
    Caveat
        This creates a ring $F[x_{0,0},x_{0,1},x_{1,0},x_{1,1},x_{1,2}]$ in which the resulting ideal is defined.
///


doc ///
    Key
        Attempt
        [randomCurveP1P2, Attempt]
    Headline
        limit number of attempts for randomCurveP1P2
    Description
      Text
           When randomCurveP1P2 generates a random curve in $\mathbb{P}^3$ using the SpaceCurves package, it is possible the resulting
           curve will intersect the base loci of the projections used to construct the curve in $\mathbb{P}^1\times\mathbb{P}^2$. If the curve
           does intersect the base locusi it will generate a new random curve in $\mathbb{P}^3$. The option Attempt limits the number
           of attempts to find a curve disjoint from the base loci before quitting. By default, Attempt is set to 1000.
    SeeAlso
        randomCurveP1P2
///


doc ///
    Key
        resolveViaFatPoint
        (resolveViaFatPoint, Ideal, Ideal, List)
    Headline
        returns a virtual resolution of a zero-dimensional scheme
    Usage
        resolveViaFatPoint(I, irr, A)
    Inputs
        J:Ideal
            saturated ideal corresponding to a zero-dimensional scheme
        irr:Ideal
            the irrelevant ideal
        A:List
            power you want to take the irrelevant ideal to
    Outputs
        :ChainComplex
            virtual resolution of our ideal
    Description
        Text
            Given a saturated ideal J of a zero-dimensional subscheme, irrelevant ideal irr, and a tuple A,
            resolveViaFatPoint computes a free resolution of J intersected with A-th power of the irrelevant ideal.
            See Theorem 4.1 of [BES, @{HREF("http://arxiv.org/abs/1703.07631","arXiv:1703.07631")}@].

            Below we follow example 4.7 of [BES,@{HREF("http://arxiv.org/abs/1703.07631","arXiv:1703.07631")}@] and
            compute the virtual resolution of 6 points in $\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^2$.
        Example
            N = {1,1,2}
            pts = 6
            (S, E) = productOfProjectiveSpaces N
            irr = intersect for n to #N-1 list (
                ideal select(gens S, i -> (degree i)#n == 1)
                );
            I = saturate intersect for i to pts - 1 list (
                P := sum for n to N#0 - 1 list ideal random({1,0,0}, S);
                Q := sum for n to N#1 - 1 list ideal random({0,1,0}, S);
                R := sum for n to N#2 - 1 list ideal random({0,0,1}, S);
                P + Q + R
                );
            C = resolveViaFatPoint (I, irr, {2,1,0})
            isVirtual(irr, C)
///


doc ///
    Key
        virtualOfPair
        (virtualOfPair, Ideal,        List)
        (virtualOfPair, Module,       List)
        (virtualOfPair, ChainComplex, List)
    Headline
        creates a virtual resolution from a free resolution by keeping only summands of specified degrees
    Usage
        virtualOfPair(I, L)
        virtualOfPair(M, L)
        virtualOfPair(C, L)
    Inputs
        I:Ideal
            ideal over multigraded ring
        M:Module
            module over multigraded ring
        C:ChainComplex
            free resolution of a module
        L:List
            multidegrees of summands to keep
    Outputs
        :ChainComplex
    Description
        Text
          Given an ideal I or module M and a list of multidegrees L, this function produces a chain complex by iteratively
          computing syzygies in degrees in L. In particular, if the list L contains only one element which is in the
          multigraded regularity of M plus the dimension vector, the output will be the virtual resolution of a pair as
          defined in Section 1 of [BES].
          See Algorithm 3.4 of [BES,@{HREF("http://arxiv.org/abs/1703.07631","arXiv:1703.07631")}@] for further details.

          If a resolution for the object exists in the cache or when the input is a chain complex C, virtualOfPair uses
          this information by simply removing the summands in degrees not in L. This option is useful when a minimal free
          resolution of M can be more efficiently computed in the engine or is already known. Otherwise, induced Schreyer
          orders are used to speed up the computation of syzygies. Note that this speedup is often very significant.

          For example, consider the ideal of three points in $\mathbb{P}^1\times\mathbb{P}^1$.
        Example
          X = toricProjectiveSpace(1) ** toricProjectiveSpace(1);
          S = ring X; B = ideal X;
          J = saturate(intersect(
                ideal(x_1 - 1*x_0, x_3 - 4*x_2),
                ideal(x_1 - 2*x_0, x_3 - 5*x_2),
                ideal(x_1 - 3*x_0, x_3 - 6*x_2)),
                B)
        Text
          We can now compute its minimal free resolution and a virtual resolution. One can show that $(2,0)$ is in the multigraded
          regularity of this example. Thus, since we want to compute a virtual resolution we apply virtualOfPair to the element
          $(3,1)$ since $(3,1)=(2,0)+(1,1)$ and $(1,1)$ is the dimension vector for $\mathbb{P}^1\times\mathbb{P}^1$.
        Example
          minres = res J
          vres = virtualOfPair(J, {{3,1}}) --(3,1) = (2,0) + (1,1)
        Text
          Notice that the virtual resolution of the pair $(S^1/J, (2,0))$ is shorter and thinner than the graded minimal free
          resolution of $S^1/J$.

          Finally, we check that the result is indeed virtual.
        Example
          isVirtual(B, vres)
    Caveat
        Given an element of the multigraded regularity, one must add the dimension vector of the product of projective spaces
        for this to return a virtual resolution.
///

doc ///
    Key
        [virtualOfPair, LengthLimit]
    Headline
        stop when the virtual resolution reaches this length
    Description
        Text
          When the optional argument LengthLimit is specified virtualOfPair will stop computing syzygies after the given
          length is reached, otherwise computation continues until the resolution terminates.
    SeeAlso
        virtualOfPair
///

doc ///
    Key
        multigradedRegularity
        (multigradedRegularity, Ring, Module)
        (multigradedRegularity, NormalToricVariety, Module)
    Headline
        computes the minimal elements of the multigraded regularity of a module over a multigraded ring
    Usage
        multigradedRegularity(S,I)
        multigradedRegularity(S,M)
        multigradedRegularity(X,I)
        multigradedRegularity(X,M)
    Inputs
        S:Ring
          a multigraded Cox ring
        X:NormalToricVariety
          a product of normal toric varieties
        I:Ideal
          an ideal over a multigraded ring
        M:Module
          a module over a multigraded ring
    Outputs
        :List
          a list of multidegrees
    Description
        Text
          Given a module M over a multigraded ring S or a product of toric varieties X, this method finds the
          minimal elements of the multigraded Castelnuovo-Mumford regularity of M as defined in Definition 1.1
          of [MS04] (see @{HREF("http://arxiv.org/abs/math/0305214","arXiv:math/0305214")}@). If the input is an ideal, multigraded regularity of S^1/I is computed.

          This is done by calling the @TO cohomologyHashTable@ method from @TO TateOnProducts@ and checking for the
          multidegrees where Hilbert polynomial and Hilbert function match and where the higher sheaf cohomology
          vanishes.

          Note that the module or ideal is assumed to be saturated by the irrelevant ideal of the Cox ring.

          As an example, here we compute the minimal elements of the multigraded regularity for Example 1.4
          of [BES](see @{HREF("http://arxiv.org/abs/1703.07631","arXiv:1703.07631")}@). We consider the example of a hyperelliptic curve of genus 4 in $\mathbb{P}^1\times\mathbb{P}^2$.
        Example
          X = toricProjectiveSpace(1)**toricProjectiveSpace(2)
          S = ring X; B = ideal X;
          I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3))
        Text
          After saturating the defining ideal by the irrelevant ideal we may compute its multigraded regularity.
        Example
          J = saturate(I,B);
          L = multigradedRegularity(X, J)

        Text
          This method also accepts the ring provided by @TO productOfProjectiveSpaces@ from the @TO TateOnProducts@ package.
    Caveat
        The input is assumed to be saturated.
///
