--- status: Draft
--- author(s): MES
--- notes: 

doc ///
Node
  Key
    Hom
   (Hom, Ring, Ring)
   (Hom, Ring, Ideal)
   (Hom, Ring, Module)
   (Hom, Ideal, Ring)
   (Hom, Ideal, Ideal)
   (Hom, Ideal, Module)
   (Hom, Module, Ring)
   (Hom, Module, Ideal)
   (Hom, Module, Module)
   [Hom, DegreeLimit]
   [Hom, MinimalGenerators]
   [Hom, Strategy]
  Headline
    module of homomorphisms
  Usage
    Hom(M, N)
  Inputs
    M:{Ring,Ideal,Module}
    N:{Ring,Ideal,Module}
    DegreeLimit => {ZZ,List}
      if set, stop after homomorphisms in this degree have been computed
    MinimalGenerators => Boolean
      whether to @TO trim@ the resulting module
    Strategy => Thing
  Outputs
    :Module
      isomorphic to $\mathrm{Hom}_R(M,N), where $M$ and $N$ are both $R$-modules
  Description
    Text
      If $M$ or $N$ is an ideal or ring, it is regarded as a module in the evident way.
    Example
      R = QQ[x,y]/(y^2 - x^3);
      M = image matrix{{x, y}}
      H = Hom(M, M, MinimalGenerators => true)
    Text
      To recover the modules used to create a Hom-module, use the function @TO formation@.
    Example
      formation H
    Text
      Specific homomorphisms may be obtained using @TO homomorphism@, as follows.
    Example
      f0 = homomorphism H_{0}
      f1 = homomorphism H_{1}
    Text
      In the example above, @TT "f0"@ is the identity map, and @TT "f1"@ maps $x$ to $y$ and $y$ to $x^2$.
    Example
      M_0, M_1
      f0 M_0, f0 M_1
      f1 M_0, f1 M_1
  Contributors
    Devlin Mallory implemented the strategy which accepts a degree limit.
  SeeAlso
    Ext
    formation
  Subnodes
    adjoint
    compose
    homomorphism
    homomorphism'
    (Hom, Module, Matrix)
    "OldChainComplexes :: Hom(Module,ChainComplex)"

Node
  Key
   (Hom, Module, Matrix)
   (Hom, Matrix, Module)
   (Hom, Matrix, Matrix)
  Headline
    induced map on Hom
  Synopsis
    Usage
      Hom(f,g)
    Inputs
      f:Matrix
      g:Matrix
      DegreeLimit => {ZZ,List}
        see @TO [Hom, DegreeLimit]@
      MinimalGenerators => Boolean
        see @TO [Hom, MinimalGenerators]@
      Strategy => Thing
        see @TO [Hom, Strategy]@
    Outputs
       :Matrix
        the map on Hom induced by the maps $f$ and $g$
  Synopsis
    Usage
      Hom(f,M)
      Hom(M,f)
    Inputs
      f:Matrix
      M:Module
      DegreeLimit => {ZZ,List}
        see @TO [Hom, DegreeLimit]@
      MinimalGenerators => Boolean
        see @TO [Hom, MinimalGenerators]@
      Strategy => Thing
        see @TO [Hom, Strategy]@
    Outputs
       :Matrix
        the map on Hom induced by the map $f$
    Description
      Example
        R = QQ[x]
	f = vars R
	M = image f
	g = Hom(f,M)
	target g
	source g
      Example
        R = QQ[x]
	f = vars R
	M = image f
	g = Hom(M,f)
	target g
	source g
  SeeAlso
    inducedMap
///

-----------------------------------------------------------------------------

doc ///
Node
  Key
    End
   [End, DegreeLimit]
   [End, MinimalGenerators]
   [End, Strategy]
  Headline
    module of endomorphisms
  Usage
    End M
  Inputs
    M:{Ring,Ideal,Module}
    DegreeLimit => {ZZ,List}
      see @TO [Hom, DegreeLimit]@
    MinimalGenerators => Boolean
      see @TO [Hom, MinimalGenerators]@
    Strategy => Thing
      see @TO [Hom, Strategy]@
  Description
    Text
      Constructs the module of endomorphisms of $M$.
    Example
      R = QQ[a,b,c];
      M = coker koszul_2 vars R
      End M
  SourceCode
    End
///

-----------------------------------------------------------------------------

doc ///
Node
  Key
    reshape
   (reshape, Module, Module, Matrix)
  Headline
    reshape a matrix
  Usage
    reshape(F, G, f)
  Inputs
    F:Module -- a free module
    G:Module -- a free module
    f:Matrix
  Outputs
     :Matrix
       @TT "F <-- G"@ obtained from @TT "f"@ by taking elements from the first column of @TT "f"@,
       then the second, and so on, filling them into the result column by column.
  Description
    Text
      Currently, it is assumed that @TT "f"@ and the result both have the same number of entries.
      The resulting map has the same degree that @TT "f"@ has, but it is easy to spoil homogeneity
      by giving incorrect free modules.
    Example
      f = matrix{{1,3,5,7,9,11},{2,4,6,8,10,12}}
      reshape(ZZ^3,ZZ^4,f)
///

-----------------------------------------------------------------------------

undocumented {
    [adjoint, Strategy],
    [adjoint, DegreeLimit],
    [adjoint, MinimalGenerators],
}

doc ///
Node
  Key
    adjoint
   (adjoint, Matrix, Module, Module)
    adjoint'
   (adjoint', Matrix, Module, Module)
  Headline
    the tensor-Hom adjunction maps
  Description
    Text
      Recall that @TO (tensor, Module, Module)@ and @TO (Hom, Module, Module)@ form an adjoint pair,
      meaning that there is a natural isomorphism
      $$ \mathrm{Hom}(F\otimes G,H) \cong \mathrm{Hom}(F,\mathrm{Hom}(G,H)). $$
  Synopsis
    Usage
      adjoint(f, F, G)
    Inputs
      f:Matrix
        a homomorphism $F \otimes G \to H$
      F:Module -- a free module
      G:Module -- a free module
    Outputs
       :Matrix
         the adjoint homomorphism $F \to \mathrm{Hom}(G,H)$
    Description
      Example
        R = QQ[x_1 .. x_24];
	f = genericMatrix(R, 2, 4*3)
	isHomogeneous f
	g = adjoint(f, R^4, R^3)
      Text
        If @TT "f"@ is homogeneous, and @TT "source f === F ** G"@ (including the grading),
	then the resulting matrix will be homogeneous.
      Example
        g = adjoint(f, R^4, R^{-1,-1,-1})
	isHomogeneous g
        f === adjoint'(g, R^{-1,-1,-1}, R^2)
  Synopsis
    Usage
      adjoint'(g, G, H)
    Inputs
      g:Matrix
        a homomorphism $F \to \mathrm{Hom}(G,H)$ between modules
      G:Module -- a free module
      H:Module -- a free module
    Outputs
       :Matrix
         the adjoint homomorphism $F \otimes G \to H$
    Description
      Text
        If @TT "g"@ is homogeneous, and @TT "target g === Hom(G,H)"@ (including the grading),
	then the resulting matrix will be homogeneous.
      Example
        R = QQ[x_1 .. x_12];
        g = genericMatrix(R, 6, 2)
        f = adjoint'(g, R^2, R^3)
        isHomogeneous f
        g === adjoint(f, R^{-1,-1}, R^2)
  SeeAlso
    dual
    flip
    compose
    reshape
    (Hom, Module, Module)
    (tensor, Module, Module)
///

-----------------------------------------------------------------------------

undocumented {
    [(dual, Matrix), Strategy],
    [(dual, Matrix), DegreeLimit],
    [(dual, Matrix), MinimalGenerators],
}
document {
    Key => dual,
    Headline => "dual module or map",
}
document {
    Key => (dual, Module),
    Headline => "dual module",
    TT "dual M", " -- the dual of a module."
}
document {
    Key => (dual, Matrix),
    Headline => "dual of a map",
    TT "dual f", " -- the dual (transpose) of a homomorphism."
}

-----------------------------------------------------------------------------

doc ///
Node
  Key
    homomorphism
   (homomorphism, Vector)
   (homomorphism, Matrix)
  Headline
    get the homomorphism from element of Hom
  Usage
    homomorphism f
  Inputs
    f:{Vector,Matrix}
      corresponding to an element in $\mathrm{Hom}(M,N)$ or a map $R^1 \to \mathrm{Hom}(M,N)$
  Outputs
    :Matrix
      the map $M \to N$, corresponding to the element $f \in \mathrm{Hom}(M,N)$
  Description
    Text
      When @TT "H = Hom(M,N)"@ is computed, information about computing the morphisms
      corresponding to its elements is stored in @TT "H"@.
    Example
      R = QQ[x,y,z, Degrees => {2,3,1}]/(y^2 - x^3)
      H = Hom(ideal(x,y), R^1)
      f = H_{1}
      g = homomorphism f
    Text
      The source and target are what they should be.
    Example
      source g === module ideal(x,y)
      target g === R^1
    Text
      Except for a possible redistribution of degrees between the map and modules,
      we can undo the process with @TO "homomorphism'"@.
    Example
      f' = homomorphism' g
      f === f'
      f - f'
      degree f, degree f'
      degrees f, degrees f'
    Text
      After @TO2((minimalPresentation, Module), "pruning")@ a Hom module,
      one cannot use homomorphism directly. Instead, first apply the pruning map:
    Example
      H1 = prune H
      homomorphism(H1.cache.pruningMap * H1_{1})
  SeeAlso
    Hom
    homomorphism'

Node
  Key
    homomorphism'
   (homomorphism', Matrix)
   [homomorphism', DegreeLimit]
   [homomorphism', MinimalGenerators]
   [homomorphism', Strategy]
  Headline
    get the element of Hom from a homomorphism
  Usage
    homomorphism' f
  Inputs
    f:Matrix
      of the form $M \to N$
    DegreeLimit => {ZZ,List}
      see @TO [Hom, DegreeLimit]@
    MinimalGenerators => Boolean
      see @TO [Hom, MinimalGenerators]@
    Strategy => Thing
      see @TO [Hom, Strategy]@
  Outputs
     :Matrix
       the map $R^1 \to \mathrm{Hom}(M,N)$, corresponding to the map $f \in \mathrm{Hom}(M,N)$
  Description
    Example
      R = QQ[x,y,z]
      f = vars R ++ vars R
      g = homomorphism' f
      target g === Hom(source f, target f)
    Text
      We can undo the process with @TO homomorphism@.
    Example
      f' = homomorphism g
      f === f'
  SourceCode
    (homomorphism', Matrix)
  SeeAlso
    Hom
    homomorphism
    adjoint
///

-----------------------------------------------------------------------------

doc ///
Node
  Key
     compose
    (compose, Module, Module, Module)
   [(compose, Module, Module, Module), DegreeLimit]
   [(compose, Module, Module, Module), MinimalGenerators]
   [(compose, Module, Module, Module), Strategy]
  Headline
    composition as a pairing on Hom-modules
  Usage
    compose(M, N, P)
  Inputs
    M:Module
    N:Module
    P:Module
--    DegreeLimit => {ZZ,List}
--      see @TO [Hom, DegreeLimit]@
--    MinimalGenerators => Boolean
--      see @TO [Hom, MinimalGenerators]@
--    Strategy => Thing
--      see @TO [Hom, Strategy]@
  Outputs
    :Matrix
      the composition map of homomorphism modules $\mathrm{Hom}(M,N)$ and $\mathrm{Hom}(N,P)$
  Description
    Text
      In the following example we check that the map does implement the composition map
      $$ \mathrm{Hom}(M,N) \otimes \mathrm{Hom}(N,P) \to \mathrm{Hom}(M,P). $$
    Example
      R = QQ[x,y]
      M = image vars R ++ R^2
      f = compose(M,M,M);
      H = Hom(M,M);
      g = H_{0}
      h = homomorphism g
      f * (g ** g)
      h' = homomorphism oo
      h' === h * h
      assert oo
    Text
      The modules should be defined over the same ring.
  SeeAlso
    Hom
    homomorphism
    homomorphism'
    adjoint
    flip
///
