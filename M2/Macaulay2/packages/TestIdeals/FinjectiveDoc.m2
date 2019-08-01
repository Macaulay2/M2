--***********************************************
--***********************************************
--Documentation for Finjective.m2
--***********************************************
--***********************************************

doc ///
    Key
        FPureModule
        (FPureModule, Ring)
        (FPureModule, Number, RingElement)
        (FPureModule, List, List)
        [FPureModule, FrobeniusRootStrategy]
        [FPureModule, CanonicalIdeal]
        [FPureModule, CurrentRing]
        [FPureModule, GeneratorList]
    Headline
        compute the submodule of the canonical module stable under the image of the trace of Frobenius
    Usage
        FPureModule()
        FPureModule(R)
        FPureModule(t, f)
        FPureModule(tList, fList)
    Inputs
        R:RingMap
        f:RingElement
            to make a pair
        t:Number
            the formal exponent to which {\tt f} is raised
        fList:List
            consisting of ring elements {\tt f_1,\ldots,f_n}, for a pair
        tList:List
            consisting of formal exponents {\tt t_1,\ldots,t_n} for the elements of {\tt fList}
        FrobeniusRootStrategy => Symbol
            selects the strategy for internal {\tt frobeniusRoot} calls
        CanonicalIdeal => Ideal
            specifies the canonical ideal (so the function does not recompute it)
        CurrentRing => Ring
            specifies the ring to work with
        GeneratorList => List
            specifies the element (or elements) of the ambient polynomial ring that determines the Frobenius trace on the canonical module
    Outputs
        :Sequence
	    consisting of four elements: the HSLG module of {\tt R}, {\tt (R,f^t)}, or {\tt (R,f_1^{t_1}\ldots f_n^{t_n})}, represented as a submodule of a canonical module; the canonical module of which it is a submodule (given as an ideal of {\tt R}); the element (or elements) of the ambient polynomial ring that determines the Frobenius trace on the canonical module; the number of times the trace of Frobenius was computed before the image stabilized.
    Description
        Text
            Given a ring $R$ with canonical module $\omega$, this function computes the image of $F^e_* \omega \to \omega$ for $e >> 0$.  
	    This image is sometimes called the {\it HSLG module} (named for Hartshorne-Speiser, Lyubeznik, and Gabber), and it roughly tells us where a ring is non-$F$-injective.
            It can also be used to compute the maximal $F$-pure sub-Cartier module of a given rank-1 Cartier module (represented as an ideal).
            The name of the function is based on this interpretation.

            Specifically, this function returns a sequence {\tt (FPMod, canMod, frobTrace, count)}, where {\tt canMod} is a canonical module of the ring (expressed as an ideal), {\tt FPMod} is the HSLG module, given as a submodule of {\tt canMod}, {\tt frobTrace} is a list of elements of the ambient polynomial ring representing the trace of Frobenius on the canonical module, and {\tt count} is the number of times the trace of Frobenius was computed before the image stabilized, sometimes called the {\it HSLG number} of the canonical module as a Cartier module.
        Example
            R = ZZ/7[x,y,z]/(x^5 + y^5 + z^5);
            (FPMod, canMod, frobTrace, count) = FPureModule(R);
            canMod --the ambient canonical module
            FPMod --the F-pure submodule of the canonical module
            frobTrace --the element representing trace of Frobenius
            count --how many times it took until the image stabilized
        Text
            Sometimes it is convenient to specify the ambient canonical module across multiple calls of {\tt FPureModule}.  
	    This can be done by using the option {\tt CanonicalIdeal}.
            One can also pass it something other than the canonical module as well (for example, a submodule of the canonical module).

            Likewise, one can use the {\tt GeneratorList} option to specify the dual Frobenius action on the canonical module (or ideal playing the role of the canonical module).

            In the following example, the non-$F$-pure ideal of a $\mathbb{Q}$-Gorenstein ring is computed by hijacking this functionality.
        Example
            T = ZZ/7[a,b];
            S = ZZ/7[x,y,z,w];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/(ker f);
            J = ideal 1_R;
            u = QGorensteinGenerator(1, R);
            FPureModule(CanonicalIdeal => J, GeneratorList => {u})
        Text
            Additionally, one can specify a pair ($R$, $f^{ t}$), as long as $t$ is a rational number with a denominator prime to the characteristic of the ring.
        Example
            R = ZZ/7[x,y];
            M = FPureModule(5/6, y^2 - x^3);
            M#1 -- the canonical module
            M#0 -- the F-pure submodule
            N = FPureModule(9/10, y^2 - x^3);
            N#0 -- the F-pure submodule
        Text
            Finally, one can specify a pair ($R$, $f_1^{t_1}\cdots f_n^{t_n}$), even when $R$ is not regular (although $R$ is required to be $\mathbb{Q}$-Gorenstein, with index not divisible by the characteristic).
        Example
            R = ZZ/3[x,y,z]/(x^2 - y*z);
            f = y;
            g = z;
            FPureModule({1/2, 1/2, 1/2}, {y, z, y + z})
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        testModule
        isFInjective
        descendIdeal
///

doc ///
    Key
        descendIdeal
        (descendIdeal, ZZ, List, List, Ideal)
        [descendIdeal, FrobeniusRootStrategy]
    Headline
        finds the maximal F-pure Cartier submodule of an ideal viewed as a Cartier module
    Usage
        descendIdeal(e, tList, fList, J)
    Inputs
        e:ZZ
            the order of the Frobenius root to take
        fList:List
            consisting of ring elements {\tt f_1,\ldots,f_n}, for a pair
        tList:List
            consisting of formal exponents {\tt t_1,\ldots,t_n} for the elements of {\tt fList}
        J:Ideal
            the Cartier module to study
        FrobeniusRootStrategy => Symbol
            selects the strategy for internal {\tt frobeniusRoot} calls
    Outputs
        :Sequence
            whose first entry is the maximal $F$-pure Cartier submodule of {\tt J} under the dual-{\tt e}-iterated Frobenius induced by {\tt f_1^{t_1}\ldots f_n^{t_n}}, and the second entry is the number of times {\tt frobeniusRoot} was applied
    Description
        Text
            This command computes the maximal $F$-pure Cartier submodule of an ideal $J$ under the dual-$e$-iterated Frobenius induced by ${\tt f_1^{t_1}\ldots f_n^{t_n}}$.
        
            The function returns a sequence, where the first entry is the descended ideal, and the second entry is the number of times {\tt frobeniusRoot} was applied (i.e., the HSL number).
        Example
            R = ZZ/7[x,y,z];
            f = y^2 - x^3;
            descendIdeal(1, {5}, {f}, ideal 1_R) --this computes the non-F-pure ideal of (R, f^{5/6})
            descendIdeal(2, {41}, {f}, ideal 1_R) --this computes the non-F-pure ideal of (R, f^{41/48})
        Text
            The same two examples could also be accomplished via calls of {\tt FPureModule}, as illustrated below; however, the {\tt descendIdeal} construction gives the user more direct control.
        Example
            first FPureModule(5/6, f, CanonicalIdeal => ideal 1_R, GeneratorList => {1_R})
            first FPureModule(41/48, f, CanonicalIdeal => ideal 1_R, GeneratorList => {1_R})
        Text	
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        FPureModule
        ascendIdeal
///

doc ///
    Key
        isFInjective
        (isFInjective, Ring)
        [isFInjective, FrobeniusRootStrategy]
        [isFInjective, AtOrigin]
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
            selects the strategy for internal {\tt frobeniusRoot} calls
        AtOrigin => Boolean
            stipulates whether to check $F$-injectivity only at the origin
        AssumeCM => Boolean
            assumes the ring is Cohen-Macaulay
        AssumeNormal => Boolean
            assumes the ring is normal
        AssumeReduced => Boolean
            assumes the ring is reduced
        CanonicalStrategy => Boolean
            specifies what strategy to use when computing the Frobenius action on top local cohomology
    Outputs
        :Boolean
    Description
        Text
            This function determines whether a ring of finite type over a finite prime field is $F$-injective.
	    Over a more general field, it checks the $F$-injectivity of the relative Frobenius.
            We begin with an example of an $F$-injective ring that is not $F$-pure (taken from the work of Anurag Singh on deformation of $F$-regularity).
        Example
             S = ZZ/3[a,b,c,d,t];
             M = matrix{{a^2 + t^4, b, d}, {c, a^2, b^3-d}};
             I = minors(2, M);
             R = S/I;
             isFInjective(R)
             isFPure(R)
        Text
            Next, let us form the cone over $\mathbb{P}^1 \times  E$, where $E$ is an elliptic curve.
	    We begin with a supersingular elliptic curve.
	    This should be $F$-injective if and only if it is $F$-pure.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/(x^3 + y^2*z - x*z^2); --supersingular elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFInjective(R)
            isFPure(R)
        Text
            Now we do a similar computation, this time with an ordinary elliptic curve.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/(y^2*z - x^3 + x*y*z); --ordinary elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFInjective(R)
            isFPure(R)
        Text
            If {\tt CanonicalStrategy} is set to {\tt Katzman} (its default behavior), then the Frobenius action on the top local cohomology (bottom Ext) is computed via the method of Katzman.
	    If it is set to anything else, it is simply brute forced in {\it Macaulay2} using the functoriality of Ext.
	    The {\tt Katzman} strategy is typically much faster.
        Example
            R = ZZ/5[x,y,z]/(y^2*z + x*y*z-x^3)
            time isFInjective(R)
            time isFInjective(R, CanonicalStrategy => null)
        Text
            If the option {\tt AtOrigin} (default value {\tt false}) is set to {\tt true}, {\tt isFInjective} will only check $F$-injectivity at the origin.
	    Otherwise, it will check $F$-injectivity globally.
	    Note that checking $F$-injectivity at the origin can be slower than checking it globally.
	    Consider the following example of a non-$F$-injective ring.
        Example
            R = ZZ/7[x,y,z]/((x-1)^5 + (y+1)^5 + z^5);
            time isFInjective(R)
            time isFInjective(R, AtOrigin => true)
        Text
            If the option {\tt AssumeCM} (default value {\tt false}) is set to {\tt true}, then {\tt isFInjective} only checks the Frobenius action on top cohomology (which is typically much faster). Note that it can give an incorrect answer if the non-injective Frobenius occurs in a lower degree.  Consider the example of the cone over a supersingular elliptic curve times $\mathbb{P}^1$.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/(x^3 + y^2*z - x*z^2);
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            time isFInjective(R)
            time isFInjective(R, AssumeCM => true)
        Text
            If the option {\tt AssumedReduced} is set to {\tt true} (its default behavior), then the bottom local cohomology is avoided (this means the Frobenius action on the top potentially nonzero Ext is not computed).
        Text
            If the option {\tt AssumeNormal} (default value {\tt false}) is set to {\tt true}, then the bottom two local cohomology modules (or, rather, their duals) need not be computed.
        Text
            The value of the option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        isFPure
        testModule
///

doc ///
    Key
        CurrentRing
    Headline
        an option to specify that a certain ring is used
    Description
        Text
            {\tt CurrentRing} is an option used in various functions to specify a ring to work with.
///

doc ///
    Key
        CanonicalIdeal
    Headline
        an option to specify that a certain ideal be used as the canonical ideal
    Description
        Text
            {\tt CanonicalIdeal} is an option used in various functions to specify an ideal to be used as the canonical ideal.  
	    In this way, the canonical ideal does not have to be recomputed, and one can use a single fixed choice across multiple calls of a function.
///

doc ///
    Key
        GeneratorList
    Headline
        an option to specify that a certain list of elements is used to describe a Cartier action
    Description
        Text
            {\tt GeneratorList} is an option used in various functions to specify a Cartier action, particularly on the canonical module.  In particular, if it is not specified, typically the Cartier action on the canonical module will need to be recomputed.
///


doc ///
    Key
        AssumeCM
    Headline
        an option to assume a ring is Cohen-Macaulay
    Description
        Text
            {\tt AssumeCM} is an option used in various functions, to assume that a ring is Cohen-Macaulay.
///

doc ///
    Key
        AssumeNormal
    Headline
        an option to assume a ring is normal
    Description
        Text
            {\tt AssumeNormal} is an option used in various functions, to assume that a ring is normal.
///

doc ///
    Key
        AssumeReduced
    Headline
        an option to assume a ring is reduced
    Description
        Text
            {\tt AssumeReduced} is an option used in various functions, to assume that a ring is reduced.
///


doc ///
    Key
        CanonicalStrategy
    Headline
        an option for isFInjective
///

doc ///
    Key
        Katzman
    Headline
        a valid value for the option CanonicalStrategy
    SeeAlso
        isFInjective
	CanonicalStrategy
///
