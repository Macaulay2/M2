--###################################
-- Types
--###################################

refHubert := "E. Hubert (2003), \"Notes on triangular sets and triangulation-decomposition algorithms I: Polynomial systems\", in \"Springer\" "
refWang := "D. Wang (2001), \"Elimination methods\", in \"Springer Science & Business Media\" "
refALM := "P. Aubry, D. Lazard, M. Moreno Maza (1999), \"On the theories of triangular sets\", in \"Journal of Symbolic Computation\", 28(1):105-–124" 
refLazard := "D. Lazard (1992), \"Solving zero-dimensional algebraic systems\", in \"Journal of Symbolic Computation\", 13(2):117-–131" 
refKalkbrener := "M. Kalkbrener (1993), \"A generalized Euclidean algorithm for computing triangular representations of algebraic varieties\", in \"Journal of Symbolic Computation\", 15(2):143-–167" 

document {
    Key => TriangularSets,
    Headline => "triangular decompositions of polynomial ideals",

    "This package allows to decompose polynomial ideals into ",
    TO2 {TriaSystem,"triangular sets"},
    EXAMPLE lines ///
        R = QQ[a..h, MonomialOrder=>Lex];
        I = ideal {a*d - b*c, c*f - d*e, e*h - f*g};
        triangularize I
    ///,
    BR{},
    "The method ",
    TO triangularize,
    " is implemented in M2 only for monomial and binomial ideals. ",
    "For the general case we interface to Maple.",
    BR{},
    BR{},
    
    "This package also provides methods for manipulating triangular sets: ",
    UL{
        TO (dim, TriaSystem),
        TO (saturate, TriaSystem),
        TO (isRegularChain, TriaSystem),
        TO (isStronglyNormalized, TriaSystem),
        TO (symbol %, RingElement, TriaSystem),
        TO (resultant, RingElement, TriaSystem),
	},

    HEADER2 {"References"},
    UL{
        refALM,
        refHubert,
        refKalkbrener,
        refLazard,
        refWang
    }
}

--###################################
-- Types
--###################################

doc /// --TriaSystem
    Key
        TriaSystem
        (net,TriaSystem)
    Headline
        a triangular system
    Description
      Text
        This type represents a triangular system.

        A polynomial system is a pair $(F,H)$, 
        where $F\subset k[x]$ is a list of @TO2 {(gens,TriaSystem),"equations"}@ and 
        and $H\subset k[x]$ is a list of @TO2 {(ineqs,TriaSystem),"inequations"}@.
        The {\em zero set} of the system is 
        $$Z(F/H) = \{x : f(x)= 0 for f\in F, h(x)\neq 0 for h\in H\}.$$
        A triangular system $(T,U)$ is a polynomial system that satisfies some additional properties [Wang'01].
        The most basic property, is that the set $T = \{t_1,\ldots,t_m\}$ must be {\em triangular}, i.e., the polynomials $\{t_i\}_i$ have distinct @TO2 {mvar,"main variables"}@.
        It is also usually required that $T$ forms a @TO2 {isRegularChain,"regular chain"}@.
        The set of inequations $U$ often consists of the @TO2 {initial,"initials"}@ of $T$.

        Triangular systems have very nice algorithmic properties.
        In particular, the zero set of a triangular system $(F,H)$ is always nonempty, and moreover, has dimension $n-m$.
        In addition, membership to the @TO2 {(saturate,TriaSystem),"saturated ideal"}@ can be efficiently decided.

        Any polynomial system can be decomposed into triangular systems.
        The method @TO triangularize@ can be used to compute such decomposition.

        The constructor of this type is @TO triaSystem@.
    SeeAlso
        triangularize
///

--###################################
-- Methods/Functions
--###################################

doc /// --triaSystem
    Key
        triaSystem
        (triaSystem,Ring,List)
        (triaSystem,Ring,List,List)
    Headline
        a triangular system
    Usage
        triaSystem(R,F)
        triaSystem(R,F,H)
    Inputs
        R:Ring
        F:List
          a list of polynomials
        H:List
          a list of polynomials (optional)
    Outputs
        :TriaSystem
          given by the pair (F,H)
    Consequences
    Description
      Text
        By default $H$ is chosen to be the set of @TO2 {initial,"initials"}@ of $F$.
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        H = {d, f, h};
        triaSystem(R,F,H)
        triaSystem(R,F)
    Caveat
        It is not checked that $(F,H)$ is indeed a triangular system.
    SeeAlso
        TriaSystem
        initial
///

doc /// --gens
    Key
        (gens,TriaSystem)
    Headline
        equations of a triangular system
    Usage
        gens T
    Inputs
        T:TriaSystem
    Outputs
        :List
          of generators of T
    Consequences
    Description
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        T = triaSystem(R,F)
        gens T
      Code
      Pre
    SeeAlso
        TriaSystem
        ineqs
///

doc /// --ineqs
    Key
        ineqs
        (ineqs,TriaSystem)
    Headline
        inequations of a triangular system
    Usage
        ineqs T
    Inputs
        T:TriaSystem
    Outputs
        :List
          of inequations of T
    Consequences
    Description
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        T = triaSystem(R,F)
        ineqs T
      Code
      Pre
    SeeAlso
        TriaSystem
        (gens,TriaSystem)
///

doc /// --RingMap
    Key
        (symbol SPACE,RingMap,TriaSystem)
    Headline
        apply ring map to a triangular system
    Usage
        f T
    Inputs
        f:RingMap
        T:TriaSystem
    Outputs
        :TriaSystem
          image of T under the ring map f
    Consequences
    Description
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        T = triaSystem(R,F)
        S = QQ[x_0..x_7, MonomialOrder=>Lex];
        f = map(S,R,gens S) 
        f T
      Code
      Pre
    SeeAlso
        TriaSystem
        (symbol SPACE,RingMap,RingElement)
///

doc /// --triangularize
    Key
        triangularize
        (triangularize,Ideal)
        (triangularize,MonomialIdeal)
        (triangularize,Ring,List)
        (triangularize,Ring,List,List)
    Headline
        triangular decomposition of polynomial systems
    Usage
        triangularize(I)
        triangularize(R,F)
        triangularize(R,F,H)
    Inputs
        I:Ideal
        R:Ring
        F:List
          a list of polynomials
        H:List
          a list of polynomials
    Outputs
        :List
          a list of triangular systems
    Consequences
    Description
      Text
        Computes a triangular decomposition of a polynomial system.
        The package implements algorithms for monomial and binomial sets.
        For arbitrary systems we interface to Maple.
        
        A polynomial system is a pair $(F,H)$, 
        where $F\subset k[x]$ is a list of @TO2 {(gens,TriaSystem),"equations"}@ and 
        and $H\subset k[x]$ is a list of @TO2 {(ineqs,TriaSystem),"inequations"}@.
        The zero set of the system is 
        $$Z(F/H) = \{x : f(x)= 0 for f\in F, h(x)\neq 0 for h\in H\}.$$
        A triangular decomposition of $(F,H)$ is a collection of "simpler" polynomial systems $(T_1,U_1),\ldots,(T_r,U_r)$ such that
        $$Z(F/H) = Z(T_1/U_1)\cup\cdots\cup Z(T_r/U_r).$$
        These simpler sets, called @TO2 {TriaSystem,"triangular systems"}@, have very nice algorithmic properties.

        As a first example we consider a case without inequations ($H=\emptyset$).
      Example
        R = QQ[a..h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        TT = triangularize(R,F,{})
        first TT
      Text
        We now include some inequations.
      Example
        H = {b,d};
        triangularize(R,F,H)
      Code
      Pre
    SeeAlso
        TriaSystem
///

doc /// --triangularizeBatch
    Key
        triangularizeBatch
    Headline
        triangularize a batch of polynomial systems
    Usage
        triangularizeBatch(alg,R,F,H)
    Inputs
        alg:String
          either "Maple" or "Epsilon"
        R:Ring
        F:HashTable
          whose values are polynmial sets
        H:HashTable
          whose values are polynmial sets
    Outputs
        :HashTable
          containing triangular decompositions of each system
    Consequences
    Description
      Text
        Let $\{F_k: k\in K\}$ and $\{H_k: k\in K\}$ be collections of polynomial sets.
        This function computes a triangular decomposition of each of the pairs $(F_k,H_k)$.

        Although @TO triangularizeBatch@ is equivalent to calling @TO triangularize@ multiple times, the former is usually faster since it avoids the cost of starting multiple Maple sessions.
      Code
      Pre
    SeeAlso
        triangularize
///

doc /// --checkTriangularize
    Key
        checkTriangularize
    Headline
        tests method "triangularize" (for developers)
    Usage
        checkTriangularize alg
    Inputs
        alg:String
          either "Binomial" or "Maple" or "Epsilon"
    Consequences
    Description
      Text
        This function tests that the method triangularize works properly using a given algorithm.
      Code
      Pre
    SeeAlso
        triangularize
        TriangularDecompAlgorithm
///

doc /// --checkInterface
    Key
        checkInterface
    Headline
        whether the Maple interface is working (for developers)
    Usage
        checkInterface i
    Inputs
        i:String
          either "Maple" or "Epsilon"
    Consequences
    Description
      Text
        This function tests whether the interface to Maple (or Epsilon) is working.
      Code
      Pre
    SeeAlso
        MapleInterface
///

doc /// --dimension TriaSystem
    Key
        (dim,TriaSystem)
        (codim,TriaSystem)
    Headline
        dimension of a triangular set
    Usage
        dim T
        codim T
    Inputs
        T:TriaSystem
    Outputs
        :ZZ
          dimension of the zero set of T
    Consequences
    Description
      Text
        This method computes the dimension of the zero set of a triangular system
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        T = triaSystem(R,F)
        dim T
      Code
      Pre
    Caveat
        It is assumed that the triangular set is a regular chain
    SeeAlso
        TriaSystem
        isRegularChain
///

doc /// --degree TriaSystem
    Key
        (degree,TriaSystem)
    Headline
        product of the degrees in a triangular set
    Usage
        degree T
    Inputs
        T:TriaSystem
    Outputs
        :ZZ
          product of the main degrees of the polynomials in T
    Consequences
    Description
      Text
        This method returns the product of degrees of the polynomial in $T$ (the degree in the @TO2 {mvar,"main variables"}@).
        In the zero-dimensional case, this agrees with degree of the @TO2 {(saturate,TriaSystem),"saturated ideal"}@ of $T$.
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a^2*d - b, c*f*h - d*e, e^3*h - f*g};
        T = triaSystem(R,F)
        degree T
        for f in F list degree(mvar f, f)
      Code
      Pre
    Caveat
        It is assumed that the triangular set is a regular chain
    SeeAlso
        TriaSystem
        (saturate,TriaSystem)
///

doc /// --mvar
    Key
        mvar
        (mvar,RingElement)
    Headline
        main variable of a polynomial
    Usage
        mvar f
    Inputs
        f:RingElement
    Outputs
        :RingElement
          the main variable of f
    Consequences
    Description
      Text
        Returns the main variable of a polynomial $f$, i.e., the largest variable that appears in $f$.
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        f = x_1^2*(x_2*x_3+2*x_3) + x_1*(x_2^2-1) + (x_2-x_3)
        mvar f
      Code
      Pre
    SeeAlso
        initial
///

doc /// --initial
    Key
        initial
        (initial,RingElement)
        (initial,TriaSystem)
    Headline
        initial of a polynomial
    Usage
        initial f
        initial(x,f)
    Inputs
        f:RingElement
    Outputs
        h:RingElement
          the initial of f
    Consequences
    Description
      Text
        Returns the initial of a polynomial.
        The initial of a polynomial $f$ is the the leading coefficient of $f$ viewed as a univariate polynomial in its @TO2 {mvar,"main variable"}@.
      Example
        R = QQ[x_0..x_3, MonomialOrder=>Lex];
        f = x_1^2*(x_2*x_3+2*x_3) + x_1*(x_2^2-1) + (x_2-x_3)
        initial f
      Code
      Pre
    SeeAlso
        mvar
        (pseudoRemainder,RingElement,RingElement,RingElement)
///

doc /// --freeVariables
    Key
        freeVariables
        (freeVariables,TriaSystem)
    Headline
        free variables of a triangular set
    Usage
        freeVariables T
    Inputs
        T:TriaSystem
    Outputs
        :List
          of free variables of T
    Consequences
    Description
      Text
        The free variables of $T$ are the ones that do not appear as the @TO2 {mvar,"main variable"}@ of a polynomial in $T$.
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        H = {d, f, h};
        T = triaSystem(R,F,H)
        freeVariables T
      Code
      Pre
    SeeAlso
        initial
///

doc /// --saturate
    Key
        (saturate,TriaSystem)
    Headline
        saturated ideal of a triangular system
    Usage
        saturate T
    Inputs
        T:TriaSystem
    Outputs
        :Ideal
          the saturated ideal of T
    Consequences
    Description
      Text
        Returns the saturated ideal of a triangular system.

        Let $T = (F,H)$ be a triangular system.
        Denoting $J=ideal(F)$, the saturated ideal of $T$ is
        $$sat(T) = J : h^\infty = \{ f: h^k f \in J for some h\in H, k\in \mathbb{N} \}$$
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        H = {d, f, h};
        T = triaSystem(R,F,H)
        saturate T
      Code
      Pre
    SeeAlso
        TriaSystem
        (symbol %,RingElement,TriaSystem)
///

doc /// --prem
    Key
        (pseudoRemainder,RingElement,RingElement,RingElement)
    Headline
        pseudo-remainder
    Usage
        pseudoRemainder(x,f,g)
    Inputs
        x:RingElement
        f:RingElement
        g:RingElement
    Outputs
        :RingElement
          pseudo-remainder of f by g with respect to x
    Consequences
    Description
      Text
        Returns the pseudo-remainder of $f$ by $g$, viewed as univariate polynomials in x.
      Example
        R = QQ[x,y];
        f = x^4
        g = x^2*y + 13*x^2*y^4 +x*y^2-3*x - 1
        pseudoRemainder(x,f,g)
        pseudoRemainder(y,f,g)
      Code
      Pre
    SeeAlso
        pseudoRemainder
        mvar
        initial
        (pseudoRemainder,RingElement,TriaSystem)
///

doc /// --prem TriaSystem
    Key
        (symbol %,RingElement,TriaSystem)
        (pseudoRemainder,RingElement,TriaSystem)
    Headline
        pseudo-remainder by a triangular set
    Usage
        f % T
        pseudoRemainder(f,T)
    Inputs
        f:RingElement
        T:TriaSystem
    Outputs
        :RingElement
          pseudo-remainder of f by T
    Consequences
    Description
      Text
        Returns the pseudo-remainder, $prem(f,T)$, of $f$ by a triangular set $T$.

        Let $T = (t_1,t_2,\cdots,t_k)$ where $mvar(t_1)>\cdots>mvar(t_k)$.
        The pseudo-remainder of $f$ by $T$ is
        $$prem(f,T) = prem(\cdots(prem(prem(f,t_1),t_2)\cdots,t_k)$$

        {\em Remark: } If $T$ is a @TO2 {isRegularChain,"regular chain"}@, then $f$ lies in its @TO2 {(saturate,TriaSystem),"saturated ideal"}@ iff $prem(f,T)=0$.
      Example
        R = QQ[a,b,c,d,e,f,g,h, MonomialOrder=>Lex];
        F = {a*d - b*c, c*f - d*e, e*h - f*g};
        H = {d, f, h};
        T = triaSystem(R,F,H)
        (a*h - b*g) % T
        saturate T
      Code
      Pre
    SeeAlso
        (pseudoRemainder,RingElement,RingElement,RingElement)
        (saturate,TriaSystem)
///

doc /// --resultant TriaSystem
    Key
        (resultant,RingElement,TriaSystem)
    Headline
        iterated resultant by a triangular set
    Usage
        resultant(f,T)
    Inputs
        f:RingElement
        T:TriaSystem
    Outputs
        :RingElement
          iterated resultant of f by T
    Consequences
    Description
      Text
        Returns the iterated resultant of $f$ by a triangular set $T$.
        Let $T = (t_1,t_2,\cdots,t_k)$ where $mvar(t_1)>\cdots>mvar(t_k)$.
        The resultant of $f$ by $T$ is
        $$resultant(f,T) = resultant(\cdots(resultant(resultant(f,t_1),t_2)\cdots,t_k)$$
      Example
        R = QQ[x,y,t,s,MonomialOrder=>Lex];
        F = {x + y^2 - t, t^2 -s};
        T = triaSystem(R,F,{});
        f = x*y*t;
        resultant(f,T)
      Code
      Pre
    SeeAlso
        TriaSystem
        (pseudoRemainder,RingElement,TriaSystem)
///

doc /// --isRegularChain
    Key
        isRegularChain
        (isRegularChain,TriaSystem)
    Headline
        whether a triangular set is a regular chain
    Usage
        isRegularChain(T)
    Inputs
        T:TriaSystem
    Outputs
        :Boolean
          true if T is a regular chain, and false otherwise
    Consequences
    Description
      Text
        Let $T = (t_1,t_2,\cdots,t_k)$ be a triangular set (i.e., their @TO2 {mvar,"main variables"}@ are distinct), and let $h$ be the product of its @TO2 {initial,"initials"}@.
        $T$ is a regular chain if the @TO2 {(resultant,RingElement,TriaSystem),"iterated resultant"}@ is nonzero: $resultant(h,T)\neq 0$.
      Example
        R = QQ[x,y,z,MonomialOrder=>Lex];
        F = {x*y - y*z, y^2 - z^2};
        T = triaSystem(R,F,{y});
        isRegularChain(T)
      Code
      Pre
    SeeAlso
        TriaSystem
        (initial,TriaSystem)
        (resultant,RingElement,TriaSystem)
///

doc /// --isStronglyNormalized
    Key
        isStronglyNormalized
        (isStronglyNormalized,TriaSystem)
    Headline
        whether a triangular set is strongly normalized
    Usage
        isStronglyNormalized(T)
    Inputs
        T:TriaSystem
    Outputs
        :Boolean
          true if T is strongly normalized, and false otherwise
    Consequences
    Description
      Text
        Let $T = (t_1,t_2,\cdots,t_k)$ be a triangular set (i.e., their @TO2 {mvar,"main variables"}@ are distinct).
        $T$ is strongly normalized if the @TO initial@ of each $t_i$ only involves @TO2 {freeVariables,"free variables"}@.
      Example
        R = QQ[x,y,t,s,MonomialOrder=>Lex];
        F = {x + y^2 - t, t^2 -s};
        T = triaSystem(R,F,{});
        isStronglyNormalized(T)
      Example
        R = QQ[x,y,z,MonomialOrder=>Lex];
        F = {x*y - y*z, y^2 - z^2};
        T = triaSystem(R,F,{y});
        isStronglyNormalized(T)
      Code
      Pre
    SeeAlso
        TriaSystem
        (initial,TriaSystem)
        freeVariables
///

doc /// --isPrimeSimple
    Key
        isPrimeSimple
        (isPrimeSimple,TriaSystem)
    Headline
        simple primality test of triangular systems
    Usage
        isPrimeSimple(T)
    Inputs
        T:TriaSystem
    Outputs
        :Boolean
          if the test is positive then sat(T) is prime; otherwise, it may or may not be prime
    Consequences
    Description
      Text
        Let $T = (t_1,t_2,\cdots,t_k)$ be a triangular set (i.e., their @TO2 {mvar,"main variables"}@ are distinct).
        This method verifies if the following properties hold:

        (i) the main degree of $t_i$ is one for $i=1,\dots,k-1$,

        (ii) $t_k$ is an irreducible polynomial.

        If these properties hold then the @TO2 {(saturate,TriaSystem),"saturated ideal"}@ of $T$ is a prime ideal.
      Example
        R = QQ[x,y,z,MonomialOrder=>Lex];
        F = {x*y^2 - y*z, y^3 + z^2};
        T = triaSystem(R,F,{y});
        isPrimeSimple(T)
        I = saturate T
        isPrime I
      Code
      Pre
    SeeAlso
        TriaSystem
        (saturate,TriaSystem)
///

doc /// --minimalObjects
    Key
        minimalObjects
    Headline
        minimal elements in a partial order
    Usage
        minimalObjects(objs, cmp)
    Inputs
        objs:VisibleList
          list of objects
        cmp:Function
          comparison function
    Outputs
        :HashTable
          indicating the minimal elements
    Consequences
    Description
      Text
        The comparison function {\tt cmp} should take values in $\{-1,0,1\}$ as follows:

        if $a>b$ then {\tt cmp(a,b) = 1},

        if $a<b$ then {\tt cmp(a,b) = -1},

        if $a,b$ are incomparable then {\tt cmp(a,b) = 0},

        {\tt cmp(a,a)} can be either -1 or 1.

      Text
        As an example, consider the poset of divisors.
      Example
        L = 2..20
        cmp = (i,j) -> if i%j==0 then 1 else if j%i==0 then -1 else 0;
        prime = minimalObjects(L, cmp);
        select(L, i -> prime#i)
      Code
      Pre
    SeeAlso
///

doc /// --gbList
    Key
        gbList
    Headline
        list of generators of a Groebner basis
    Usage
        gbList I
    Inputs
        I:Ideal
    Outputs
        :List
    Consequences
    Description
      Text
        This is a shortcut for {\tt flatten entries gens gb}.
      Example
        R = QQ[a..d];
        I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
        gbList I
      Code
      Pre
    SeeAlso
        gb
///

--###################################
-- Symbols
--###################################

doc /// --TriaAlgorithm
    Key
        TriangularDecompAlgorithm
        [triangularize,TriangularDecompAlgorithm]
    Headline
        possible values: "Monomial", "Binomial", "Maple", "Epsilon"
    Description
      Text
        The package implements triangular decompositions in two restricted cases:

        (1) "Monomial" =>
        M2 implementation for monomial ideals

        (2) "Binomial" => 
        M2 implementation for binomial ideals

        For arbitrary ideals we interface to Maple:

        (3) "Maple" =>
        uses library RegularChains (included in Maple)

        (4) "Epsilon" =>
        uses Maple's package Epsilon (download from \url{http://www-polsys.lip6.fr/~wang/epsilon/})
    SeeAlso
        triangularize
///

