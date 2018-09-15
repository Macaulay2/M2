doc ///
    Key
        HSLGModule
        (HSLGModule, Ring)
        (HSLGModule, Ring, Ideal)
        (HSLGModule, Ideal)
        (HSLGModule, Ring, Ideal, List)
        (HSLGModule, Number, RingElement)
        (HSLGModule, Number, RingElement, Ideal, List)
        (HSLGModule, List, List)
        (HSLGModule, List, List, Ideal, List)
        (HSLGModule, ZZ, List, List, Ideal)
        [HSLGModule, FrobeniusRootStrategy]
    Headline
        computes the submodule of the canonical module stable under the image of the trace of Frobenius
    Usage
        HSLGModule(R)
        HSLGModule(R, canonicalIdeal)
        HSLGModule(canonicalIdeal)
        HSLGModule(R, canonicalIdeal, uList)
        HSLGModule(t, f)
        HSLGModule(t, f, canonicalIdeal, uList)
        HSLGModule(expList, eltList)
        HSLGModule(expList, eltList, canonicalIdeal, uList)
        HSLGModule(e, expList, eltList, canIdeal) --this last command is largely an internal function
    Inputs
        R:Ring
        canonicalIdeal:Ideal
            an ideal isomorphic to the canonical ideal
        f:RingElement
            a ring element, to make a pair
        expList:List
            a list of formal exponents for ring elements, for pairs
        eltList:List
            a list of ring elements, for pairs
        t:Number
            a formal exponent
        expList:List
            a list of formal exponents
        e:ZZ
            an integer, what root of Frobenius to take
        uList:List
            the trace generator in the ambient polynomial ring (a list of elements that generate the trace map)
        FrobeniusRootStrategy=>Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :List
    Description
        Text
            Given a ring $R$ with canonical module $\omega$, this computes the image of $F^e_* \omega \to \omega$ for $e >> 0$.  This image is sometimes called the HSLG-module (named for Hartshorne-Speiser, Lyubeznik and Gabber).  It roughly tells you where a ring is non-F-injective.
        Text
            Specifically, this function returns a list of the following entries.  {\tt HSLGmodule, canonicalModule, u, HSLCount} where {\tt canonicalModule} is the canonical module of the ring (expressed as an ideal), {\tt HSLGmodule} is a submodule of that canonical module, {\tt u} is an element of the ambient polynomial ring representing the trace of Frobenius on the canonical module and {\tt HSLCount} is how many times the trace of Frobenius was computed before the image stabilized.
        Example
            R = ZZ/7[x,y,z]/ideal(x^5+y^5+z^5);
            HSLList = HSLGModule(R);
            HSLList#1 --the ambient canonical module
            HSLList#0 --the HSLGsubmodule
            HSLList#2 --the element representing trace of Frobenius
            HSLList#3 --how many times it took until the image stabilized
        Text
            If you do not want the function to compute the canonical module, you can also pass the canonical module as an ideal.
            You can also pass it something other than the canonical module as well (for example, a submodule of the canonical module).
            In the following example, we compute the non-F-pure ideal of a Q-Gorenstein ring by hijacking this functionality.
        Example
            T = ZZ/7[a,b];
            S = ZZ/7[x,y,z,w];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            I = ker f;
            R = S/I;
            J = ideal(sub(1, R));
            u = QGorensteinGenerator(1, R);
            HSLGModule(R, J, {u})
        Text
            Additionally, you can specify a pair $(R, f^t)$ as long as $t$ is a rational number without $p$ in its denominator.
        Example
            R = ZZ/7[x,y];
            HSLList = HSLGModule(5/6, y^2-x^3);
            HSLList#1 --the canonical module
            HSLList#0
            HSLList = HSLGModule(9/10, y^2-x^3);
            HSLList#0
        Text
            Additionally, we can compute HSLG-modules of things like $(R, f^s g^t)$ even when $R$ is not regular (although we do require that R is Q-Gorenstein with index not divisible by the characteristic).
        Example
            R = ZZ/3[x,y,z]/ideal(x^2-y*z);
            f = y;
            g = z;
            HSLGModule({1/2, 1/2, 1/2}, {y,z,y+z})
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        testModule
        isFInjective
///

doc ///
    Key
        isFInjective
        (isFInjective, Ring)
        [isFInjective, FrobeniusRootStrategy]
        [isFInjective, IsLocal]
        [isFInjective, AssumeCM]
        [isFInjective, AssumeNormal]
        [isFInjective, AssumeReduced]
        [isFInjective, CanonicalStrategy]
    Headline
        whether a ring is F-injective
    Usage
        isFInjective(R)
    Inputs
        R:Ring
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
        IsLocal => Boolean
            specify whether to check F-injectivity only at the origin
        AssumeCM => Boolean
            assume the ring is Cohen-Macaulay
        AssumeNormal => Boolean
            assume the ring is normal
        AssumeReduced => Boolean
            assume the ring is reduced
        CanonicalStrategy => Boolean
            specify what strategy to use when computing the Frobenius action on top local cohomology
    Outputs
        :Boolean
    Description
        Text
            This determines whether a ring of finite type over a prime field is F-injective or not.  Over a more general field this checks the F-injectivity of the relative Frobenius.
            We begin with an example of an F-injective ring that is not F-pure (taken from the work of Anurag Singh).
        Example
             S = ZZ/3[a,b,c,d,t];
             m = 4;
             n = 3;
             M = matrix{ {a^2 + t^m, b, d}, {c, a^2, b^n-d} };
             I = minors(2, M);
             R = S/I;
             isFInjective(R)
             isFPure(R)
        Text
            Next let's form the cone over $P^1 \times E$ where $E$ is an elliptic curve.  We begin with a supersingular elliptic curve.  This should be F-injective and only if it is F-pure.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(x^3+y^2*z-x*z^2); --supersingular elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFInjective(R)
            isFPure(R)
        Text
            Now we do a similar computation this time with an ordinary elliptic curve.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(y^2*z-x^3+x*y*z); --ordinary elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFInjective(R)
            isFPure(R)
        Text
            If {\tt CanonicalStrategy=>Katzman} which is the default behavior, then the Frobenius action on the top local cohomology (bottom $Ext$) is computed via the method of Katzman.  If it is set to anything else, it is simply brute forced in Macaulay2 using the fuctoriality of Ext.  {\tt CanonicalStrategy=>Katzman} typically is much faster.
        Example
            R = ZZ/5[x,y,z]/ideal(y^2*z + x*y*z-x^3)
            time isFInjective(R)
            time isFInjective(R, CanonicalStrategy=>null)
        Text
            If you set the option {\tt IsLocal => true} (default {\tt false}) it will only check F-injectivity at the origin.  Otherwise it will check it everywhere.  Note checking at the origin can be slower than checking it everywhere.  Consider the example of the following non-F-injective ring.
        Example
            R = ZZ/7[x,y,z]/ideal( (x-1)^5 + (y+1)^5 + z^5 );
            isFInjective(R)
            isFInjective(R, IsLocal=>true)
        Text
            If {\tt AssumeCM=>true} then the function only checks the Frobenius action on top cohomology (which is typically much faster).  The default value is {\tt false}.  Note, that it can give an incorrect answer if the non-injective Frobenius occurs in a lower degree.  Consider the example of the cone over a supersingular elliptic curve times $P^1$.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(x^3+y^2*z-x*z^2);
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            time isFInjective(R)
            time isFInjective(R, AssumeCM=>true)
        Text
            If {\tt AssumedReduced=>true} (default {\tt true}) then the bottom local cohomology is avoided (this means the Frobenius action on the top potentially nonzero Ext is not computed).
        Text
            If {\tt AssumeNormal=>true} (default {\tt false}) then we need not compute the bottom two local cohomology modules (or rather their duals).
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        isFPure
        testModule
///



doc ///
    Key
        AssumeCM
        AssumeNormal
        AssumeReduced
    Headline
        make assumptions about your ring
    Description
        Text
            These are options used in various functions to make assumptions about your ring.
///


doc ///
    Key
        CanonicalStrategy
    Headline
        an option for isFInjective
    SeeAlso
        isFInjective
///

doc ///
    Key
        Katzman
    Headline
        a valid value for the option CanonicalStrategy
    SeeAlso
        isFInjective
///
