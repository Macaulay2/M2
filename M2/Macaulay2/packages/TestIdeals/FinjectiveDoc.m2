doc ///
    Key
        HSLGModule
        (HSLGModule, Ring)
        (HSLGModule, Ring, Ideal)
        (HSLGModule, Ideal)
        (HSLGModule, ZZ, RingElement)
        (HSLGModule, QQ, RingElement)
        (HSLGModule, List, List)
        (HSLGModule, ZZ, List, List, Ideal)
    Headline
        computes the submodule of the canonical module stable under the image of the trace of Frobenius
    Usage
        HSLGModule(R)
        HSLGModule(R, canonicalIdeal)
        HSLGModule(canonicalIdeal)
        HSLGModule(t, f)
        HSLGModule(expList, eltList)
        HSLGModule(e, expList, eltList, canIdeal)
    Inputs 
        R:Ring
        canonicalIdeal:Ideal
        f:RingElement
        expList:List
        eltList:List
        e:ZZ
    Outputs
        :List
    Description
        Text
            Given a ring $R$ with canonical module $\omega$, this computes the image of $F^e_* \omega \to \omega$ for $e >> 0$.  This image is sometimes called the HSLG-module.  It roughly tells you where a ring is non-F-injective.
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
            If you don't want the function to compute the canonicalModule, you can also pass it.  This can be useful if you pass it something other than the canonical module as well (for example, a submodule of the canonical module).
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
            Additionally, we can compute HSLG-modules of things like $\tau(R, f^s g^t)$ even when $R$ is not regular.    
        Example
            R = ZZ/3[x,y,z]/ideal(x^2-y*z);
            f = y;
            g = z;
            HSLGModule({1/2, 1/2, 1/2}, {y,z,y+z})
///

doc ///
    Key
        isFinjective
        (isFinjective, Ring)   
    Headline
        whether a ring is F-injective
    Usage
        isFinjective(R)
    Inputs
        R:Ring
    Outputs
        :Boolean
    Description
        Text
            This verifies if a ring of finite type over a prime field is F-injective or not.  Over a more general field this checks the F-injectivity of the relative Frobenius.            
            We begin with an example of an F-injective ring that is not F-pure (taken from the work of Anurag Singh).
        Example
             S = ZZ/3[a,b,c,d,t];
             m = 4; 
             n = 3;
             M = matrix{ {a^2 + t^m, b, d}, {c, a^2, b^n-d} };
             I = minors(2, M);
             R = S/I;
             isFinjective(R)
             isFpure(R)
        Text
            Next let's form the cone over $P^1 \times E$ where $E$ is an elliptic curve.  We begin with a supersingular elliptic curve.  This should be F-injective and only if it is F-pure.
        Example  
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(x^3+y^2*z-x*z^2); --supersingular elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFinjective(R)
            isFpure(R)
        Text
            Now we do a similar computation this time with an ordinary elliptic curve.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(y^2*z-x^3+x*y*z); --ordinary elliptic curve
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            isFinjective(R)
            isFpure(R)
    SeeAlso
        isFpure            
///

doc ///
    Key
        [isFinjective, IsLocal]
    Headline
        controls whether F-injectivity is checked at the origin or everywhere
    Usage
        isFinjective(..., IsLocal=>b)
    Inputs
        b:Boolean
    Outputs
        :Boolean
    Description
        Text
            If you set the option {\tt IsLocal => true} (default {\tt false}) it will only check F-injectivity at the origin.  Otherwise it will check it everywhere.  Note checking at the origin can be slower than checking it everywhere.  Consider the example of the following non-F-injective ring.
        Example
            R = ZZ/5[x,y,z]/ideal( (x-1)^4 + y^4 + z^4 );
            isFinjective(R)
            isFinjective(R, IsLocal=>true)
///
        
doc ///
    Key
        [isFinjective, AssumeCM]
        [isFinjective, AssumeNormal]
        [isFinjective, AssumeReduced]
    Headline
        assumptions to speed up the computation of isFinjective        
    Usage
        isFinjective(..., AssumeCM=>b1, AssumeNormal=>b2, AssumeReduced=>b3)
    Inputs
        b1:Boolean
        b2:Boolean
        b3:Boolean
    Outputs
        :Boolean
    Description
        Text
            Various options which can (vastly) speed up the computation of whether a ring is F-injective.
        Text
            If {\tt AssumeCM=>true} then it only checks the Frobenius action on top cohomology (which is typically much faster).  The default value is {\tt false}.  Note, that it can give an incorrect answer however if the non-injective Frobenius occurs in a lower degree, as it does in this example of the cone over a supersingular elliptic curve times $P^1$.
        Example
            S = ZZ/3[xs, ys, zs, xt, yt, zt];
            EP1 = ZZ/3[x,y,z,s,t]/ideal(x^3+y^2*z-x*z^2); 
            f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
            R = S/(ker f);
            time isFinjective(R)
            time isFinjective(R, AssumeCM=>true)
        Text
            If {\tt AssumedReduced=>true} (default {\tt true}) then the bottom local cohomology is avoided (this means the Frobenius action on the top potentially nonzero Ext is not computed).
        Text
            If {\tt AssumeNormal=>true} (default {\tt false}) then certain cohomologies of the local cohomology can be avoided.
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
        [isFinjective, CanonicalStrategy]
    Headline
        specify a strategy for isFinjective
    Usage
        isFinjective(..., CanonicalStrategy=>b)
    Inputs
        b:Boolean
    Outputs
        :Boolean
    Description
        Text
            If {\tt CanonicalStrategy=>Katzman} which is the default behavior, then the Frobenius action on the top local cohomology (bottom $Ext$) is computed via the method of Katzman.  If it is set to anything else, it is simply brute forced in Macaulay2 using the fuctoriality of Ext.  {\tt CanonicalStrategy=>Katzman} typically is much faster.
        Example
             R = ZZ/5[x,y,z]/ideal(y^2*z + x*y*z-x^3)
            time isFinjective(R)
            time isFinjective(R, CanonicalStrategy=>null)
///

doc ///
    Key
        CanonicalStrategy
    Headline
        an option for isFinjective
///

doc ///
    Key
        Katzman
    Headline
        a valid value for the option CanonicalStrategy
///        
