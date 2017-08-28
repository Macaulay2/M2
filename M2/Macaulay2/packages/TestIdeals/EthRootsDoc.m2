-------------------------------------------------------
---------- List of functions to document---------------
-----------(as of 2016-07-18 --------------------------
-------------------------------------------------------
-- frobeniusRoot
-- minimalCompatible 
-- Mstar 
-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------



doc ///
    Key
        ascendIdeal
        (ascendIdeal, ZZ, RingElement, Ideal)
        (ascendIdeal, ZZ, ZZ, RingElement, Ideal)
        (ascendIdeal, ZZ, BasicList, BasicList, Ideal)
    Headline
        finds the smallest phi-stable ideal containing a given ideal in a quotient of a polynomial ring.
    Usage
        ascendIdeal(e, h, J)
        ascendIdeal(e, a, h, J)
        ascendIdeal(e, expList, hList, J)
    Inputs
        J:Ideal 
        h:RingElement
        e:ZZ
        a:ZZ
        expList:BasicList
        hList:BasicList
    Outputs
        :Ideal
    Description
        Text
            Let $\phi$ be the $p^{-e}$ linear map obtained by multiplying $e$-th Frobenius trace on a polynomial ring by $h$.  Then this function finds the smallest $\phi$-stable ideal containing $J$.  The idea is to consider the ascending chain $J, J+\phi(J), J+\phi(J)+\phi^2(J), \ldots$.  We return the stable value.  For instance, this can be used to compute the test ideal.  Note if the ideal $J$ is not an ideal in a polynomial ring, the function will do the computation with $e$-th Frobenius trace in the ambient polynomial ring, but will do the comparison inside the quotient ring (to see if we are done).  
        Example
            S = ZZ/5[x,y,z];
            g = x^4+y^4+z^4;
            h = g^4;
            R = S/ideal(g);
            ascendIdeal(1, h, ideal(y^3))
            ascendIdeal(1, h, ideal((sub(y, S))^3))          
        Text
            The alternate ways to call the function allow the function to behave in a more efficient way.  Indeed, frequently the polynomial passed is a power, $h^a$.  If $a$ is large, we don't want to compute $h^a$; instead we try to keep the exponent small by only raising it to the minimal power needed to do computation at that time.
        Example
            S = ZZ/5[x,y,z];
            g = x^4+y^4+z^4;
            R = S/ideal(g);
            ascendIdeal(1, 4, g, ideal(y^3))
            ascendIdeal(1, 4, g, ideal((sub(y, S))^3)) 
        Text
            More generally, if $h$ is a product of powers, $h = h_1^{a_1}\cdots h_n^{a_n}$, then you should pass {\tt ascendIdeal} the lists {\tt expList=\{a_1,\ldots,a_n\}} and {\tt \{h_1,\ldots,h_n\}} of exponents and bases.
        Text
            This method appared first in the work of Mordechai Katzman on star closure.  
///

doc ///
    Key
        [ascendIdeal, AscentCount]
    Headline
        return how many times it took before the ascent of the ideal stabilized
    Usage
        ascendIdeal(..., AscentCount=>b)
    Inputs
        b:Boolean
    Outputs
        :Ideal
        :List
    Description
        Text
            By default (when {\tt AscentCount => true}), {\tt ascendIdeal} just returns the stable (ascended) ideal.  If instead you set {\tt AscentCount=>true} then it returns a list.  The first value is the stable ideal.  The second is how many steps it took to reach that ideal.
        Example
            R = ZZ/5[x,y,z];
            J = ideal(x^12,y^15,z^21);
            f = y^2+x^3-z^5;
            ascendIdeal(1, f^4, J)
            ascendIdeal(1, f^4, J, AscentCount=>true)
///

doc ///
    Key
        AscentCount
    Headline
        an option for ascendIdeal
    SeeAlso
        [ascendIdeal, AscentCount]        
///        


doc ///
    Key
        frobeniusRoot
        (frobeniusRoot, ZZ, Ideal)
        (frobeniusRoot, ZZ, MonomialIdeal)
        (frobeniusRoot, ZZ, List, List)
        (frobeniusRoot, ZZ, ZZ, RingElement, Ideal)
        (frobeniusRoot, ZZ, ZZ, RingElement)
        (frobeniusRoot, ZZ, ZZ, Ideal)
        (frobeniusRoot, ZZ, List, List, Ideal)
        (frobeniusRoot, ZZ, Matrix)
    Headline
        computes I^[1/p^e] in a polynomial ring over a perfect field
    Usage
        frobeniusRoot(e, I) 
        frobeniusRoot(e, exponentList, idealList)
        frobeniusRoot (e, a, f, I)
        frobeniusRoot (e, a, f)
        frobeniusRoot (e, m, I)
        frobeniusRoot(e, exponentList, idealList, I)
        frobeniusRoot(e, A)
    Inputs
        e:ZZ
            the power of the characteristic $p$ to which you are taking the ideal. E.g., to find the $p^2$-th root of an ideal, set {\tt e = 2}
        I:Ideal
            an ideal in a polynomial ring over a perfect field
        idealList:List
            a list of ideals whose product you want to take the root of 
        exponentList:List
            a list of exponents which you are raising idealList to. E.g., to find the root of {\tt I^2J^3}, set {\tt idealList = \{I, J\}} and {\tt exponentList = \{2, 3\}}
        a:ZZ
            the exponent you are raising {\tt f} to
        f:RingElement
            a polynomial 
        m:ZZ
            the exponent you are raising {\tt I} to
        A:Matrix
    Outputs
        :Ideal
    Description
        Text
            In a polynomial ring $k[x_1, \ldots, x_n]$ with cofficients in a field of positive characteristic $p$, the Frobenius root $I^{[1/p^e]}$ is the smallest ideal $J$ such that $I\subseteq J^{[p^e]}$ ({\tt = frobeniusPower(J,e)} ).  This function computes it.  Alternately it can be viewed as the image under the Cartier operator of the ideal $I$.

            There are many ways to call {\tt frobeniusRoot}. The simplest way is to call {\tt frobeniusRoot(e,I)}. For instance, 
        Example
            R = ZZ/5[x,y,z];
            I = ideal(x^50*z^95, y^100+z^27);
            frobeniusRoot(2, I)
        Text
            This computes $I^{[1/p^e]}$, i.e. the $p^e$-th root of $I$. Often, one wants to compute the frobeniusRoot of some product of ideals. This is best accomplished by calling the following version of frobeniusRoot:
        Example 
            R =  ZZ/5[x,y,z];
            I1 = ideal(x^10, y^10, z^10);
            I2 = ideal(x^20*y^100, x + z^100);
            I3 = ideal(x^50*y^50*z^50);
            frobeniusRoot(1, {4,5,6}, {I1, I2, I3})
        Text
            The above example computes the ideal {\tt (I1^4 I2^5 I3^6)^{[1/p]}}. For legacy reasons, you can specify the last ideal in your list using {\tt frobeniusRoot(e,exponentList,idealList,I)}. This last ideal is just raised to the first power. 

            You can also call {\tt frobeniusRoot(e,a,f)}. This computes the $e$th root of the principal ideal $(f^a)$. Calling {\tt frobeniusRoot(e,m,I)} computes the $e$th root of the ideal $I^m$, and calling {\tt frobeniusRoot(e,a,f,I)} computes the eth root of the product $f^a I$. Finally, you can also compute the $p^e$-th root of a matrix $A$ by calling {\tt frobeniusRoot(e,A)}.
    SeeAlso
        frobenius
        frobeniusPower
///

-*
-- not exported
doc ///
    Key
        minimalCompatible
    Headline
        computes minimal compatible ideals and submodules. 
    Usage
        J = minimalCompatible(e, f, I)
        J = minimalCompatible(a, e, f, I)
        M = minimalCompatible(e, A, U)
    Inputs
        e:ZZ
        f:RingElement
        a:ZZ
        I:Ideal
        A:Matrix
        U:Matrix
    Outputs
        J:Ideal
        M:Matrix
    Description
        Text
            minimalCompatible is a method for:
            (1) finding the smallest ideal $J$ which satisfies $uJ\subset J^{[p^e]}$ and $I \subset J$ for a given ideal $I$ and a given ring element $u$, and
            (2) finding the smallest submodule $V$ of a free module which satisfies $UV\subset V^{[p^e]}$ and image$(A)\subset V$ for given matrices $A$ and $U$. 

///
*-

-*
-- not exported
doc ///
    Key
        mEthRoot
    Headline
        computes p^eth roots of matrices
    Usage
        mEthRoot(e, A)
    Inputs
        e: ZZ
        A: Matrix
    Outputs
        :Matrix
///
*-

doc ///
    Key
        [testIdeal, FrobeniusRootStrategy]
        [isFregular, FrobeniusRootStrategy]
        [HSLGModule, FrobeniusRootStrategy]
        [isFpure, FrobeniusRootStrategy]
        [isFinjective, FrobeniusRootStrategy]
        [ascendIdeal, FrobeniusRootStrategy]
        [testModule, FrobeniusRootStrategy]   
        [frobenius, FrobeniusRootStrategy]
        [frobeniusPower, FrobeniusRootStrategy]  
        [parameterTestIdeal, FrobeniusRootStrategy]
        [compatibleIdeals, FrobeniusRootStrategy]
    Headline
        controls the strategy for computing the Frobenius root of an ideal within other call
    Usage
        testIdeal(..., FrobeniusRootStrategy=>S)
        isFregular(..., FrobeniusRootStrategy=>S)
        HSLGModule(..., FrobeniusRootStrategy=>S)        
        isFpure(..., FrobeniusRootStrategy=>S)
        isFinjective(..., FrobeniusRootStrategy=>S)        
        ascendIdeal(..., FrobeniusRootStrategy=>S)
        testModule(..., FrobeniusRootStrategy=>S)
        frobenius(..., FrobeniusRootStrategy=>S)
        frobeniusPower(..., FrobeniusRootStrategy=>S)
        parameterTestIdeal(..., FrobeniusRootStrategy=>S)
        compatibleIdeals(..., FrobeniusRootStrategy=>S)
    Inputs
        S: Symbol
    Description
        Text
            Valid options are {\tt Substitution} and {\tt MonomialBasis}.  These options will be passed in various calls to @TO frobeniusRoot@.
    SeeAlso
        [frobeniusRoot, FrobeniusRootStrategy]
///

doc ///
    Key
        [frobeniusRoot, FrobeniusRootStrategy]
    Headline
        controls what strategy Frobenius root uses.        
    Usage
        frobeniusRoot(..., FrobeniusRootStrategy=>S)
    Inputs
        S: Symbol
    Outputs
        :Ideal
    Description
        Text
            There are two valid inputs for {\tt FrobeniusRootStrategy}, namely {\tt Substitution} and {\tt MonomialBasis}.  In the end, for each generator $f$ of an ideal $I$, we are simply writing $f = \sum a_i^{p^e} m_i$ where $m$ is a monomial all of whose exponents are $< p^e$, then all the possible $a_i$ generate the {\tt frobeniusRoot}. {\tt Substitution} computes this by doing a Grobner basis computation in a ring with twice as many variables. {\tt MonomialBasis} does this more directly and naively.  There does not appear to be a single case where one is much faster than the other.
///    

doc ///
    Key
        FrobeniusRootStrategy
    Headline
        an option for various functions
    Description
        Text
            An option for various functions and in particular for frobeniusRoot.  The valid options are {\tt Substitution} and {\tt MonomialBasis}.
///        

doc ///
    Key
        Substitution
    Headline
        a valid value for the FrobeniusRootStrategy option
///    

doc ///
    Key
        MonomialBasis
    Headline
        a valid value for the FrobeniusRootStrategy option
///    
