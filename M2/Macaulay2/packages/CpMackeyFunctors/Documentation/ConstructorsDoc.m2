doc ///
    Key
        "constructing examples of Mackey functors"
    Headline
        info about how to construct explicit Mackey functors
    Description
        Text
            {\bf The general constructor:} New Mackey functors can be built using @TO "makeCpMackeyFunctor"@, which constructs a Mackey functor out of a restriction, transfer, and conjugation @TO "matrix"@, in that order. It takes as input a prime, and three matrices. For example, we can construct $\mathbb{F}_4$ as a $C_2$-Galois Mackey functor as follows:

        Example
            U:=cokernel(matrix({{2,0},{0,2}}));
            C:=inducedMap(U,U,matrix({{1,1},{0,1}}));
            F:=kernel(C - id_U);
            R:=inducedMap(U,F,matrix({{1,0},{0,1}}));
            T:=inducedMap(F,U,matrix({{0,1},{0,0}}));
            M:=makeCpMackeyFunctor(2,R,T,C)

        Text
            The output of the @TO("makeCpMackeyFunctor")@ method is a @TO("CpMackeyFunctor")@, which is a new @TO2(Type,"type")@ implemented in this package. Under the hood it is a @TO2("Macaulay2Doc :: hash tables","hash table")@, encoding all the data of the Mackey functor. The underlying and fixed modules can be recovered with the @TO("Underlying")@ and @TO("Fixed")@ keys, and the homomorphisms in the data can be recovered from @TO("Res")@, @TO("Trans")@, and @TO("Conj")@ keys.

        Text
            {\bf Pruning:} Any time we might need a nicer, more readable form of a Mackey functor, we can use the @TO2((prune,CpMackeyFunctor),"prune")@ method to simplify the presentation of the underlying/fixed modules.

            {\bf Algebraic constructors:} We provide specific methods for constructing common examples of $C_p$-Mackey functors coming from algebra and representation theory. The @TO2(makeBurnsideMackeyFunctor,"Burnside Mackey functor")@ is a prototypical example -- its underlying module is $\ZZ$ with trivial conjugation action, and its fixed module is the {\em Burnside ring} $A(C_p)$, which is the group completion of the monoid of isomorphism classes of finite $C_p$-sets.

            Another example from algebra are the @TO2(makeRealRepresentationMackeyFunctor,"real")@ and @TO2(makeComplexRepresentationMackeyFunctor,"complex representation")@ Mackey functors. Similarly to the Burnside Mackey functor, their underlying module is $\ZZ$, however their fixed module is the representation ring of $C_p$. Any finite $C_p$-set has an associated permutation representation, which induces what are called the @TO2(realLinearizationMap,"real")@ and @TO2(complexLinearizationMap,"complex linearization maps")@ from the Burnside Mackey functor to the representation Mackey functor.

            {\bf Fixed point and orbit Mackey functors:} If $M$ is a $\ZZ[C_p]$-module, the @TO2(makeFixedPointMackeyFunctor,"fixed point Mackey functor")@ of $M$ has $M$ as its underlying module and the fixed points $M^{C_p}$ as its fixed module. The conjugation action on $M$ is the $C_p$-action, and the transfer sums along the orbits of an element.

        Example
            makeFixedPointMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}})

        Text
            Similarly, given a $\ZZ[C_p]$-module $M$, we can form the @TO2(makeOrbitMackeyFunctor,"orbit Mackey functor")@, whose underlying module is again $M$ with $C_p$-action yielding conjugation. The fixed point module is now the {\em quotient module} $M/C_p$, transfer is the quotient map, and now {\em restriction} is defined by summing along the orbits.

        Example
            makeOrbitMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}})

        Text
            {\bf Free constructors:} Analogous to how a free module can be constructed on a generator or set of generators, we can construct free Mackey functors. For $C_p$-Mackey functors, there are two modules, which lead to two different ideas of a "free" Mackey functor, namely a free $C_p$-Mackey functor on a generator in the {\em underlying} module, and a free $C_p$-Mackey functor on a generator in the {\em fixed} module. The @TO2(makeUnderlyingFreeMackeyFunctor,"free Mackey functor on a single underlying generator")@ can be accessed as follows:

        Example
            makeUnderlyingFreeMackeyFunctor(3)

        Text
            We often call this $\underline{B}$. Trying to construct a free Mackey functor on a single generator in the fixed module, we can verify that it recovers a similar construction, namely the @TO2(makeBurnsideMackeyFunctor,"Burnside Mackey functor")@, which we often denote by $\underline{A}$.

        Example
            makeBurnsideMackeyFunctor(5)

        Text
            These two Mackey functors $\underline{A}$ and $\underline{B}$ are very special - they are the projective generators of the category $\text{Mack}_G$, and they play an important role in @TO2((resolution,CpMackeyFunctor,ZZ),"constructing resolutions")@.

            {\bf Random constructor:}
            Furthermore we have a {\bf random constructor} which allow us to build a @TO2(makeRandomCpMackeyFunctor,"random Mackey functor")@ over the group $C_p$. To implement this, we recall that we have projective generators $\underline{A}$ and $\underline{B}$, hence any Mackey functor $M$ can be written as a cokernel of some @TO2((map,CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix),"map")@ of the form

            \[ \underline{A}^k \oplus \underline{B}^\ell \to \underline{A}^n \oplus \underline{B}^m \to M \to 0.\]

            We can specify $k$, $\ell$, $n$, and $m$ if we want, as the following example over $C_7$ shows:

        Example
            makeRandomCpMackeyFunctor(7,{2,2,3,3})

        Text
            If we don't want to specify this data, $n$ and $m$ will be chosen randomly between 1 and 10. To randomly choose between 1 and a higher number, we can use the @TT("GenBound")@ option.

        Example
            makeRandomCpMackeyFunctor(3)
            prune makeRandomCpMackeyFunctor(3,GenBound=>20)
        Text
            {\bf Remark:} What makes this possible is the fact that it is computationally very easy to cook up a random map $\underline{A}^k \oplus \underline{B}^\ell \to \underline{A}^n \oplus \underline{B}^m$ using the universal properties of the functors $\underline{A}$ and $\underline{B}$. Since $\underline{A}$ corepresents the @TO2(Fixed,"fixed module")@ functor and $\underline{B}$ corepresents the @TO2(Underlying,"underlying module")@ functor, we can determine random maps of the form $\underline{A}^k \oplus \underline{B}^\ell \to M$ by picking a @TO2((random,List,Module),"random list of elements")@ from the fixed and underlying modules of any $M$.
    SeeAlso
        "background on Mackey functors"
        "the abelian category of Mackey functors"
        "list of common Mackey functors"
        "explicit applications of the CpMackeyFunctors package"
///

doc ///
    Key
        makeBurnsideMackeyFunctor
        (makeBurnsideMackeyFunctor, ZZ)
    Headline
        constructs the Burnside Mackey functor
    Usage
        makeBurnsideMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the $C_p$-Mackey functor $\underline{A}$
    Description
        Text
            The {\em Burnside Mackey functor} of a group $G$ is defined by sending $G/H$ to the Burnside ring $A(H)$, with transfer and restriction coming from transfer and restriction of finite $G$-sets. When $G$ is a cyclic group of prime order, these maps admit a particularly nice form. The underlying module is given by $\mathbb{Z}$, with trivial conjugation action, while the fixed module is $\mathbb{Z}\{1,t\}$. Restriction is defined as $$\text{res}_e^{C_p} \colon \mathbb{Z}\{1,t\} \to \mathbb{Z}$$ by sending $t\mapsto p$. Transfer is of the form $$\text{tr}_e^{C_p} \colon \mathbb{Z} \to \mathbb{Z}\{1,t\}$$ by sending $x\mapsto xt$.
        Example
            makeBurnsideMackeyFunctor(5)
    SeeAlso
        "constructing examples of Mackey functors"
///

doc ///
    Key
        makeUnderlyingFreeMackeyFunctor
        (makeUnderlyingFreeMackeyFunctor, ZZ)
    Headline
        constructs the free Mackey functor on an underlying generator
    Usage
        makeUnderlyingFreeMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the free Mackey functor on an underlying generator
    Description
        Text
            The free $C_p$-Mackey functor on an underlying generator represents the functor $\text{Mack}_{C_p} \to \text{Ab}$ which sends a Mackey functor $M$ to its @TO2("Underlying","underlying")@ level $M(C_p/e)$. This means there is a natural isomorphism $$\text{Hom}_{\text{Mack}_{C_p}}\left(\underline{B},M \right) \cong M(C_p/e).$$
        Text
            In components, the @TO2("Underlying","underlying")@ module is the free module on the $C_p$-set  $C_p/e=\{1,\gamma,\gamma^2,\ldots,\gamma^{p-1}\}$, with conjugation induced by the $C_p$-action of left multiplication. The @TO2("Fixed","fixed")@ module is the module $\ZZ$. Restriction is the map $$\text{res}_e^{C_p} \colon \ZZ \to \ZZ\{1,\gamma,\ldots,\gamma^{p-1}\}$$ by sending $1\mapsto 1+\gamma+\cdots+\gamma^{p-1}$. The transfer is the map $$\text{tr}_e^{C_p} \colon \ZZ\{1,\gamma,\ldots,\gamma^{p-1}\} \to \ZZ$$ by sending $\gamma^i\mapsto 1$ for all $i$.
        Example
            makeUnderlyingFreeMackeyFunctor(5)
    SeeAlso
        "constructing examples of Mackey functors"
        makeBurnsideMackeyFunctor
///

doc ///
    Key
        makeFixedPointMackeyFunctor
        (makeFixedPointMackeyFunctor, ZZ, Matrix)
    Headline
        constructs the fixed-point Mackey functor of a C_p-module
    Usage
        makeFixedPointMackeyFunctor(p,C)
    Inputs
        p : ZZ
            a prime number $p$
        C : Matrix
            an order $p$ automorphism $c$
    Outputs
        : CpMackeyFunctor
            the fixed-point Mackey functor of the $C_p$-module specified by $\text{c}$.
    Description
        Text
            Given a module $M$ with $C_p$-action specified by an automorphism $\text{c}\colon M\to M$, there is a naturally associated {\em fixed-point Mackey functor} $\text{FP}(M)$. The underlying module is the module $M$ with conjugation given by $\text{c}$. The fixed module is the module $M^{C_p}$ of fixed points of the action, represented as the kernel of $1-\text{c}$. The restriction is the map $$\text{res}_e^{C_p} \colon M^{C_p} \to M$$ induced by the inclusion of fixed-points. The transfer is the map $$\text{tr}_e^{C_p} \colon M\to M^{C_p}$$ given by $1+\text{c}+\text{c}^2+\cdots+\text{c}^{p-1}$.
        Example
            makeFixedPointMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}})

    SeeAlso
        "constructing examples of Mackey functors"
        makeOrbitMackeyFunctor
///

doc ///
    Key
        makeOrbitMackeyFunctor
        (makeOrbitMackeyFunctor, ZZ, Matrix)
    Headline
        constructs the orbit Mackey functor of a C_p-module
    Usage
        makeOrbitMackeyFunctor(p,C)
    Inputs
        p : ZZ
            a prime number $p$
        C : Matrix
            an order $p$ automorphism $\text{c}$
    Outputs
        : CpMackeyFunctor
            the orbit Mackey functor of the $C_p$-module specified by $\text{c}$
    Description
        Text
            Given a module $M$ with $C_p$-action specified by an automorphism $\text{c}\colon M\to M$, there is a naturally associated {\em orbit Mackey functor} $\text{O}(M)$. The underlying module is the module $M$ with conjugation given by $\text{c}$. The fixed module is the quotient module $M_{C_p}$ of the action, represented as the cokernel of $1-\text{c}$. The restriction is the map $$\text{res}_e^{C_p} \colon M_{C_p} \to M$$ given by $1+\text{c}+\text{c}^2+\cdots+\text{c}^{p-1}$. The transfer is the map $$\text{tr}_e^{C_p} \colon M\to M_{C_p}$$ induced by the projection to the quotient.
        Example
            makeOrbitMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}})

    SeeAlso
        "constructing examples of Mackey functors"
        makeFixedPointMackeyFunctor
///

doc ///
    Key
        makeZeroMackeyFunctor
        (makeZeroMackeyFunctor, ZZ)
    Headline
        constructs the zero Mackey functor for the group
    Usage
        makeZeroMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the zero Mackey functor for $C_p$
    Description
        Text
            Perhaps the easiest Mackey functor is the {\em zero Mackey functor} which has the zero-module as both the @TO2("Underlying","underlying")@ and @TO2("Fixed","fixed")@ modules. This is the zero object in @TO("the abelian category of Mackey functors")@, and is therefore important to implement for homological algebra computations.
        Example
            makeZeroMackeyFunctor(2)
    SeeAlso
        "constructing examples of Mackey functors"
        isTrivialMackeyFunctor
///

doc ///
    Key
        makeComplexRepresentationMackeyFunctor
        (makeComplexRepresentationMackeyFunctor, ZZ)
    Headline
        constructs the complex representation Mackey functor
    Usage
        makeComplexRepresentationMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the complex representation Mackey functor for the group $C_p$.
    Description
        Text
            The {\em complex representation} Mackey functor of a group $G$ is defined by sending $G/H$ to the Grothendieck group of complex $G$-representations. The transfer and restriction come from induction and restriction of $G$-representations. When $G$ is a cyclic group of prime order, this admits a nice form. The underlying module is given by $\ZZ$ with trivial conjugation action, while the fixed module is $\mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{p-1}\}$. The element $\lambda_i$ represents the one dimensional complex representation given by multiplication by $e^{2\pi i/p}$. Restriction is defined as",
            \[\text{res}_e^{C_p} \colon \mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{p-1}\} \to \mathbb{Z}\]
            by sending $\lambda_i\mapsto 1$. The transfer
            \[\text{tr}_e^{C_p} \colon \mathbb{Z} \to \mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{p-1}\}\]
            sends $x\mapsto x\cdot \left(\sum^{p-1}_{0} \lambda_i\right)$.
        Example
            makeComplexRepresentationMackeyFunctor(5)
    SeeAlso
        "constructing examples of Mackey functors"
        makeRealRepresentationMackeyFunctor
///

doc ///
    Key
        makeRealRepresentationMackeyFunctor
        (makeRealRepresentationMackeyFunctor, ZZ)
    Headline
        constructs the real representation Mackey functor
    Usage
        makeRealRepresentationMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the real representation Mackey functor for the group $C_p$

    Description
        Text
            The {\em real representation} Mackey functor of a group $G$ is defined by sending $G/H$ to the Grothendieck group of real orthogonal $G$-representations. The transfer and restriction come from induction and restriction of $G$-representations. When $G$ is a cyclic group of prime order p, with p odd, this admits a nice form. The underlying module is given by $\ZZ$ with trivial conjugation action, while the fixed module is $\mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{(p-1)/2}\}$. The element $\lambda_i$ for $i>0$ represents the two dimensional real representation given by rotation by $(2\pi i)/p$ radians. The element $\lambda_0$ represents the trivial one dimensional representation.  Restriction is defined as
            \[\text{res}_e^{C_p} \colon \mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{(p-1)/2}\} \to \mathbb{Z}\]
            by sending
            \[\lambda_i\mapsto \begin{cases} 2 & i>0 \\ 1 & i=0. \end{cases} \]
            Transfer is of the form
            \[\text{tr}_e^{C_p} \colon \mathbb{Z} \to \mathbb{Z}\{\lambda_{0},\lambda_1,\dots,\lambda_{(p-1)/2}\}\]
            by sending $x\mapsto x\cdot \left(\sum^{(p-1)/2}_{0} \lambda_i\right)$.
        Example
            makeRealRepresentationMackeyFunctor(5)
    SeeAlso
        "constructing examples of Mackey functors"
        makeComplexRepresentationMackeyFunctor
///

doc ///
    Key
        makeFixedTrivMackeyFunctor
        (makeFixedTrivMackeyFunctor, ZZ)
    Headline
        constructs the fixed-point Mackey functor for the trivial action of $C_p$ on $\mathbb{Z}/p$
    Usage
        makeFixedTrivMackeyFunctor(p)
    Inputs
        p : ZZ
            a prime number $p$
    Outputs
        : CpMackeyFunctor
            the fixed-point Mackey functor for the trivial action of $C_p$ on $\mathbb{Z}/p$
    Description
        Example
            makeFixedTrivMackeyFunctor(5)
    SeeAlso
        "constructing examples of Mackey functors"
///

doc ///
    Key
        makeFixedSignMackeyFunctor
        (makeFixedSignMackeyFunctor, Sequence)
    Headline
        constructs the fixed-point Mackey functor for the sign action of $C_2$ on $\mathbb{Z}$
    Usage
        makeFixedSignMackeyFunctor()
    Description
        Example
            makeFixedSignMackeyFunctor()
    SeeAlso
        "constructing examples of Mackey functors"
///

doc ///
    Key
        makeZeroOnUnderlyingMackeyFunctor
        (makeZeroOnUnderlyingMackeyFunctor, ZZ, Module)
    Headline
        constructs a Mackey functor which is zero on underlying
    Usage
        makeZeroOnUnderlyingMackeyFunctor(p,M)
    Inputs
        p : ZZ
            a prime number $p$
        M : Module
            An abelian group
    Outputs
        : CpMackeyFunctor
            A Mackey functor with underlying level 0 and fixed level M.

    Description
        Text
            Every abelian group $M$ determines a $C_p$-Mackey functor which is zero on underlying and $M$ on fixed level.
        Example
            makeZeroOnUnderlyingMackeyFunctor(5,ZZ^3)
    SeeAlso
        "constructing examples of Mackey functors"
///

doc ///
    Key
        makeKGroupMackeyFunctor
        (makeKGroupMackeyFunctor, ZZ, ZZ, ZZ)
    Headline
        constructs the $K$-group Mackey functor
    Usage
        makeKGroupMackeyFunctor(p,q,n)
    Inputs
        p : ZZ
            a prime number $p$
        q : ZZ
            a prime power $q$
        n : ZZ
            a positive integer $n$
    Outputs
        : CpMackeyFunctor
            the fixed point $C_p$-Mackey functor of the algebraic $K$-group $K_{2n-1}(\mathbb{F}_{q^p})$.
    Description
        Example
            makeKGroupMackeyFunctor(5,9,4)
    SeeAlso
        "list of common Mackey functors"
///
