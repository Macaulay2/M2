newPackage(
	"InverseSystems",
    	Version => "1.1", 
    	Date => "June 27, 2018",
    	Authors => {{Name => "David Eisenbud", 
		  Email => "de@msri.org"
		  },
	            {Name => "Mats Boij",
		     Email => "boij@kth.se"}
			},
    	Headline => "equivariant Macaulay inverse systems",
	Keywords => {"Commutative Algebra"},
    	DebuggingMode => false
    	)

export {"inverseSystem",
        "toDividedPowers",
        "fromDividedPowers",
	"isStandardGradedPolynomialRing",
	--option names (symbols):
	"DividedPowers",
	"toDual", 
	"fromDual",
	"Gorenstein"
	}
///
restart
uninstallPackage "InverseSystems"
installPackage "InverseSystems"
check "InverseSystems"
viewHelp InverseSystems
loadPackage("InverseSystems", Reload=>true)
///
isStandardGradedPolynomialRing = method()
isStandardGradedPolynomialRing Ring := R ->(
    isField coefficientRing R and
       isPolynomialRing R and
       all(gens R, v->(degree v)=={1})
       )

toDividedPowers = method()
toDividedPowers RingElement := p -> (
    --the following routine takes a polynomial and writes in in the divided power basis,
    --where a^(n) is represented as a^n.
    S := ring p;
    sub0 := map(S,S,0_S*vars S);
    (monoms, coeffs) := coefficients p;
    D := sub0 diff(monoms, transpose monoms);
    (flatten entries (monoms*D*coeffs))_0
)

toDividedPowers Matrix := M -> (
    --same for all the entries of a matrix
    map(target M, source M, (i,j) -> toDividedPowers (M_j_i))
)

fromDividedPowers = method()
fromDividedPowers RingElement := p -> (
    --fromDividedPowers takes a polynomial written in the divided power basis,
    --where a^(n) is represented as a^n,
    --and changes it to a polynomial written in the monomial basis.
    S := ring p;
    sub0 := map(S,S,0_S*vars S);
    (monoms, coeffs) := coefficients p;
    D := sub0 diff(monoms, transpose monoms);
    (flatten entries (monoms*(inverse D)*coeffs))_0
)
fromDividedPowers Matrix := M -> (
    --same for all the elements of a matrix
    map(target M, source M, (i,j) -> fromDividedPowers (M_j_i))
)

--fromDual takes a matrix or ring element and returns an ideal or submodule.

fromDual = method(Options=>{DividedPowers => true})						  

fromDual Matrix := o -> M -> (
          R := ring M;
	  if not isPolynomialRing R then error"fromDual requires a polynomial ring";
	  M' := compress M;
 	  --handle the case when the input was 0
 	  if numgens source M' == 0 then return matrix{{1_(ring M)}};
	  if o.DividedPowers === false then M' = toDividedPowers M';
	  g := lcm first entries compress flatten monomials M';
	  M'' := contract(transpose M', transpose matrix{{g}});
    	  e := first exponents g;
	  I := ideal apply(numgens R,i->R_i^(e_i+1));
	  A := sub(syz(R/I**M''),R);
	  A|(gens I)**target A
	  )
fromDual RingElement := o -> f -> fromDual(matrix{{f}}, DividedPowers=> o.DividedPowers)

powers = (d,v) ->
   -- v a list of ring elements; d a natural number. Returns the list of powers.
    apply(v, x->x^d)
    
containsDthPowers = method()
containsDthPowers Ideal := I->(
    --input is an ideal of dimension 0.
    --returns the smallest d such that I contains the d-th 
    --powers of all the variables.
    if dim I != 0 then error"Input ideal must be 0-dimensional";
    S := ring I;
    v := gens S; -- a list
    n := numgens S;
    d := min flatten degrees I;
    while (matrix{powers(d,v)} % I) != 0 do d = d+1;
    d
   ) 


toDual = method(Options => {DividedPowers => true})
toDual (ZZ,Matrix) := Matrix => o-> (d,f) -> (
    --given a submodule of a free module S^n, represented by a matrix f,
    --returns the annihilator, represented as the matrix of a map to D^n,
    --valid up to degree d in each variable
    S := ring f;
    g := matrix{{product powers(d,gens S)}};
    I := ideal powers(d+1,gens S);
    R := S/I;
    f1 := syz transpose sub(f,R);
    m := compress contract(sub(f1,S),g);
    if o.DividedPowers === false then m = fromDividedPowers m;    
    m
    )
toDual(ZZ,Ideal) := Matrix => o->(d,I) -> toDual(d,gens I)



toDualTrunc = method(Options => {DividedPowers => true})
toDualTrunc (ZZ,Matrix) := Matrix => o-> (d,f) -> 
    toDual(d,f | ((target f) ** gens power(ideal vars ring f,d)), 
	DividedPowers => o.DividedPowers)

inverseSystem = method(Options => {DividedPowers => false})
inverseSystem (ZZ,Matrix) := o-> (d,M) -> 
    toDual(d,M,DividedPowers => o.DividedPowers)

inverseSystem (ZZ,Ideal) := o-> (d,I) -> inverseSystem(d,gens I, 
                                         DividedPowers => o.DividedPowers)

inverseSystem Matrix := o-> M -> (
    A := fromDual(M, DividedPowers => o.DividedPowers);
    --A is a matrix; want a submodule or ideal
    B := image A;
    if rank target A == 1 then B = ideal B;
    if isHomogeneous B then trim B else B
    )
inverseSystem Ideal := o-> I -> 
                   inverseSystem(gens I, DividedPowers => o.DividedPowers)
		   
inverseSystem RingElement := o-> M -> (
    A := fromDual(M, DividedPowers => o.DividedPowers);
    --A is a matrix; want a submodule or ideal
    B := ideal A;
    if isHomogeneous B then trim B else B
    )


beginDocumentation()


doc ///
Key
 InverseSystems
Headline
 Macaulay's Inverse Systems
Description
 Text
  Inverse systems are often used to construct artinian Gorenstein
  ideals and modules. For a brief introduction to that application,
  see @TO Gorenstein@. Here we give a general introduction.
  
  The graded Hopf algebra dual of the symmetric algebra
  $S := k[x_1,\dots,x_n]$ is the divided power algebra
  $D$. The dual basis to the monomial basis of $S$
  is the basis consisting of monomials of the form
  $x_1^{(m_1)} \dots x_n^{(m_n)}$. In characteristic zero,
  $S$ and $D$ are isomorphic as algebras, with 
  isomorphism sending 
  $x_i^{a}$ to $a!x_i^{(a)}$.
  In general the multiplication in $D$ is defined
  by the same formulas as in characteristic 0.
  For example,

  $x_1^{(1)}*x_1^{(1)} = 2*x_1^{(2)}$.

  In positive characteristic $D$ and $S$ are not isomorphic; $D$ is not even
  a finitely generated algebra. 
  
  We will be interested
  also in the local versions, where we take power series
  in the divided powers. This is the ordinary linear dual of $S$.
  We denote it by
  $D'$. As an $S$-module, $D'$ is
  the injective hull of the simple module
  $S/(x_1,\dots,x_n)$.  
  
  Since $D$ is the graded dual of $S$, it may also be regarded as an
  $S$-module. Any element of $D$ is annihilated by a power
  of $mm = (x_1,\dots,x_n)$, so the action of $S$ on 
  $D$ or $D'$ extends to an action of the power series ring
  k[[x_1,\dots,x_n]].
  
  F. S. Macaulay defined the inverse system of an $S'$-submodule M of $D'$ to be
  the annihilator I of M in $S'$ (or in $S$).  If $M$ is finitely
  generated then it is annihilated by some power of $mm$,
  and thus I is 0-dimensional.
  
  Inversely, the (local) inverse system of an ideal in $S$
  or $S'$ is
  by definition the submodule of $D'$ that it annihilates.
  
  In the 1880's these ideas were used by Max Noether, in the
  local version, as a substitute for primary decomposition in the 
  case of what he called multiple points in the plane. 
  F. S. Macaualay studied and greatly refined Noether's
  work, and for example identified the ideals I that are
  annihilators of cyclic submodules of $D'$ as the ideals
  such that one could do residuation in $S'/I$ -- 
  that is, $S'/I$ is Gorenstein.
  Though the global version
  has also been studied, we will only
  be concerned with the local version. 
  
  Any 
  finitely generated submodule of D' generated by finite
  polynomials is actually a submodule of D, and its dual
  will have only primary components contained in
  $(x_1,\dots,x_n)$ so the distinction
  will not be important for us on that side. 
  However, it is imporant
  to note that when taking the inverse system of an ideal,
  only the primary components contained in 
  $(x_1,\dots,x_n)$ play a role.

  %%%%%%%%%%%%%%%%%%%%%%%%  

  Going from a submodule of D to an ideal of S 
  (or from a submodule of D^r to a submodule of S^r:
  
  Because D and D' are not finitely generated S-modules
  Macaulay2 cannot deal with them directly.
  These scripts can only deal with a finitely generated 
  submodule M of D or D'. Such a module is represented here
  by a row matrix, of
  ordinary polynomials, whose entries are thought of as
  generators of M. 
  
  More generally, we represent a finitely generated submodule of $D^r$
  as an $r$-rowed matrix M of ordinary polynomials.
  
  In the default behavior
  of the script
  
  inverseSystem M = inverseSystem(M, DividedPowers => false)
  
  a monomial $x^a$ is taken to represent
  $a!x^{(a)} \in D'$, where, if
  $a = (a_1,\dots,a_n)$, then $a! = a_1!*\dots*a_n!$.
  This means
  that the script should not be used in the default
  way unless the characteristic is greater than the highest
  degree to which a variable appears. 
  
  To make $x^a$ represent $x^{(a)}$,
  for example in small characteristics, use
  
  inverseSystem(Matrix, DividedPowers=>true)
  
  (which was and remains the default behavior of the script
  "fromDual"). 

  The reason for the default choice is that the
  general linear group GL_n(k) acts on both S and D, and
  it is reasonable to expect that the operations
  defined by inverseSystem should be equivariant. This is
  the case for the default setting, but with
  DividedPowers=>true it is not the case.
  For example, 
 Example
  S = QQ[x,y]
  J = inverseSystem x^2
 Text
  differs only by a linear change of coordinates from the ideal
 Example
  J' = inverseSystem (x+y)^2
 Text
  But is has very different properties than the ideal
 Example
  J'' = inverseSystem(matrix{{(x+y)^2}}, DividedPowers=>true)  
  betti res J''
 Text
  With the default behavior (DividedPowers=>false),
  inverseSystem is equivariant in a precise sense:
  If G is an $n\times n$ invertible scalar matrix, then G defines an automorphism 
  g: S\to S by change of variables. Also g also acts on 1 x m matrices M, componentwise and
  (transpose g)^{-1} (inverseSystem M) = inverseSystem(g M), as illustrated below:
 Example
  kk = QQ
  n = 3
  S = kk[a,b,c]
  M = matrix{{a^2,b^3+c^3}}
 
  G = random(S^3, S^3)
  g = map(S,S,(vars S)*G)
  g' = map(S,S,(vars S)*(transpose G))
  h =  map(S,S,(vars S)*(transpose G)^(-1))

  inverseSystem M
  g'(inverseSystem g(M))
 Text
  These may look different, but...
 Example
  g'(inverseSystem g(M)) == inverseSystem M
 Text
  Equivalently,
 Example
  h(inverseSystem M) == inverseSystem g(M)
 Text  
  There is a similar equivariance for the dual action of inverseSystem on ideals.

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  Going from an ideal of S to a submodule of D, 
  or from a submodule of S^r to a submodule of D^r.
  
  If $I$ is an ideal of $S$, homogeneous or not,
  we regard $I$ as an ideal of $S'$, and similarly with 
  submodules of S^n. In either case we allow the function
  to accept either a submodule or its matrix of generators
  (a matrix with target S^n, possibly with generators in 
  different degrees.
  
  If I is an ideal or a submodule, then
  M = inverseSystem(d, I)
  M1 = inverseSystem(d, I, DividedPowers => true)
  
  both return 1 x m matrices whose entries are
  the minimal generators of
  the annihilator of $I$ in $D'$, correct up to degree d. 
  In the matrix $M$
  a term $x^a$
  is to be interpreted as 
  $a! x^{(a)}$, while in the matrix $M1$ it is interpreted
  as $x^{(a)}$. Of course the first computation is only
  valid if all the powers of variables appearing in the generators
  of $I$ are < char k.
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%5
  The functor
  inverseSystem ( -- ) and 
  inverseSystem (d ,-- )
  are inverse contravariant isomorphisms between the categories of
  bounded length submodules of D^r and bounded co-length submodules of S^r
  (the bound depends on d in an obvious way.)
 Example
  S = ZZ/101[a,b,c]
  M = random(S^{0,1}, S^{-1,-1,-2,-2})
  d = 6
  N = inverseSystem(d, M)
  image M == inverseSystem N
 Text
  but with a smaller value of d this would not work:
 Example
  d = 5
  N = inverseSystem(d, M)
  image M != inverseSystem N
 Text 
  There is at present no direct way to compare
  inverseSystem(d, inverseSystem M) and M, since the matrices representing these submodules
  are really representing sets of generators in D^r. However, we can
  check equality of containment by applying inverseSystem again:
 Example
  M = random(S^{0,1}, S^{-1,-1,-2,-2})
  inverseSystem(inverseSystem(d,gens inverseSystem M)) == inverseSystem M
 Text
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  Method: To represent finitely generated S-submodule of $D'$ 
  as an S-module we use the map of modules
  S/(x_1^d,\dots, x_n^d) -> D' sending $x^a$ to 
  contract(x^a, product(n, j-> x_i^{d-1})), and its inverse,
  which is of course defined only on divided monomials of 
  small degree.  
Caveat
 The translations used involve multiplying or dividing by scalars; if the polynomials
 involved have maximum degree n, then n! must be invertible for theUsing the tool of translation to make sense.
SeeAlso
 inverseSystem
 fromDual
 toDual
 DividedPowers
 fromDividedPowers
 toDividedPowers
///

doc///
Key
 Gorenstein
Headline
 Constructing Gorenstein Rings and Modules
Description
 Text
  Each artinian graded (or local) Gorenstein ring is the inverse system of a unique
  element of D, and inverse systems are often used to construct such examples.
  (Higher-dimensional Gorenstein rings also correspond to special inverse systems,
  though these are not finitely generated submodules of D. See
  "The structure of the inverse system of Gorenstein k-algebras"
  by Joan Elias and Maria Evelina Rossi, Adv. Math. (2017) 306-327,
  for a recent treatment with computational intent.)
  
  For example,
  studying artinian Gorenstein rings of codimension 4,
  one might consider those corresponding to the sum of n d-th powers of linear
  forms. For example with n= 4,5 and d=3:
 Example
  S = ZZ/101[a,b,c,d]
  nPowers = (S, n,d) ->sum(apply(n, j->(random(1,S))^d))
  minimalBetti inverseSystem nPowers(S,4,3)
  minimalBetti inverseSystem nPowers (S,5,3)
 Text
  One can also construct self-dual modules with more generators by taking the 
  inverseSystem of a submodule that is isomorphic to its dual, for example
  the image of a symmetric or skew-symmetric matrix:
 Example
  Msymm = matrix"0,a,b;a,0,c;b,c,0"
  Mskew = matrix"0,a,b;-a,0,c;-b,-c,0"
  minimalBetti coker gens inverseSystem Msymm
  minimalBetti coker gens inverseSystem Mskew  
 Text
  For an interesting series of examples, consider the d-th Hessian matrices
  obtained by taking the d-th mixed partials of a form of some degree e>2d. The ranks
  of such matrices are connected to the Lefschetz properties of the corresponding
  artinian Gorenstein rings, as explained in "The Lefschetz properties",
  Springer Lecture Notes in Math. 2080, by 
  T. Harima, 
  T. Maeno, 
  H. Morita,
  Y. Numata,
  A. Wachi  and 
  J. Watanabe.
 Example
  Hessian = (d,f) ->(
      S = ring f;
      B = basis(d,S);
      diff(transpose B, diff(B,f))
      )
  S = ZZ/101[x_1..x_4]
  f = nPowers (S,6,4)
  minimalBetti coker gens inverseSystem f
  minimalBetti coker gens inverseSystem Hessian(1, f)
  minimalBetti coker gens inverseSystem Hessian(2, f)
SeeAlso
 inverseSystem
///

doc ///
Key
 inverseSystem
 (inverseSystem, Matrix)
 (inverseSystem, Ideal) 
 (inverseSystem, RingElement) 
 (inverseSystem, ZZ, Ideal)
 (inverseSystem, ZZ, Matrix) 
 [inverseSystem, DividedPowers] 
Headline
 Inverse systems with equivariance
Usage
 I1 = inverseSystem M
 M1 = inverseSystem I
Inputs
 M:Matrix
  if r rows, then represents a submodule of D'^r
 M:RingElement
 I:Ideal
Outputs
 I1:Ideal
  if r=1
 I1:Module
  submodule of S^r
 M1:Matrix
Description
 Text
  Inverse systems are often used to construct artinian Gorenstein
  ideals and modules. For that application
  see @TO Gorenstein@.
 
  Let S = k[x_1..x_n] be a standard graded polynomial ring,
  and let D be its dual, the divided power algebra,
  regarded as an S-module.  Let M be an rxm matrix of polynomials,
  and let I be an ideal of S. 
  
  From a submodule of D^r to a submodule of S^r (or to an ideal, if r=1):
  
  We think of the columns of M as generators of an  S-submodule MM of D^r,
  and 
  inverseSystem M returns the annihilator of MM in S^r = Hom_{graded}(D^r,k).
  In the default behavior
  a monomial $x^a$ in an entry of the matrix M is taken to represent
  $a!x^(a) \in D'$, where,
  $a = (a_1,\dots,a_n)$ then $a! = a_1!*\dots*a_n!$. Use
  
  inverseSystem(M, DividedPowers => false)
  
  to make the monomials of entries of M represent the dual basis of the monomial basis of S,
  that is, the divided powers of the generators of D as an algebra.
  
  From an ideal of S to a submodule of D:
  
  If $I$ is an ideal of $S$, homogeneous or not,
  we regard $I$ as an ideal of the localization $S'$ of $S$ at $(x_1,\dots,x_n)$. If $S'/I$ is of
  finite length then
  
  M = inverseSystem I
  
  and
  
  M1 = inverseSystem(I, DividedPowers => false)
  
  each return a 1 x m matrix whose entries are
  the minimal generators of
  the annihilator of $I$ in $D$. In the matrix $M$
  a term $x^a$
  is to be interpreted as 
  $a! x^(a)$, while in the matrix $M'$ it is interpreted
  as $x^(a)$. Of course the first computation is only
  valid if all the powers of variables appearing in the generators
  of $I$ are < char k.
  
  To make these computations it is necessary to represent some sufficiently
  large finitely generated S-submodule of $D$ (this will automatically be
  an $S'$-submodule. To do this we use the map of modules
  D-> S/(x_1^d,\dots, x_n^d) sending $x^{(a)}$ to 
  contract(x^a, product(n, j-> x_i^{d-1})), defined only when the variables
  in $x^{(a)}$ appear only with powers < d.
  
 Example
  setRandomSeed 0
  kk = QQ
  S = kk[a,b,c]
  map(S,S,0_S*vars S)
  p = (a+b)^2
  q = toDividedPowers p
  p' = fromDividedPowers q
  p'==p
 Text
  Here are some codimension 4 Gorenstein rings with different Betti tables,
  computed by inverseSystem
  from quartic polynomials
 Example
  kk = ZZ/101
  S = kk[a..d]
  f1 = matrix"a2b2+c2d2"; -- gives 1,4,6,4,1
  f2 = matrix"a2b2+b2c2+c2d2"; --gives 1,4,7,4,1;
  f3 = matrix"a2b2+b2c2+c2d2+c2a2"; -- gives 1,4,8,4,1
  f4 = matrix"a2b2+b2c2+c2d2+c2a2+a2d2"; --gives 1,4,8,4,1
  f5 = matrix"a2b2+b2c2+c2d2+c2a2+a2d2+b2d2+b4"; --gives 1,4,9,4,1
  f6 = matrix"a2b2+b2c2+c2d2+c2a2+a2d2+b2d2"; --gives 1,4,10,4,1
  F = {f1,f2,f3,f4,f5,f6};
  netList (F/(f->betti res inverseSystem f))

Caveat
 Because inverseSystem
 involves a conversion between the bases of the dual,
 it should not be used in the default mode
 unless the characteristic is greater than the highest
 degree to which a variable appears. 
 To make $x^a$ represent $x^(a)$,
 for example in small characteristics use
  
 inverseSystem(Matrix, DividedPowers=>false)
  
 (which was the default behavior of the old script
 "fromDual"). 
SeeAlso
 DividedPowers
 fromDividedPowers
 toDividedPowers
 fromDual
 toDual
 isStandardGradedPolynomialRing
///

doc ///
   Key
    DividedPowers
   Headline
    Option for inverseSystem
   Description
    Text
   Caveat
   SeeAlso
    inverseSystem
///

doc ///
   Key 
    fromDividedPowers
    (fromDividedPowers,Matrix)
    (fromDividedPowers,RingElement)
   Headline 
    Translates from divided power monomial basis to ordinary monomial basis
   Usage
    f1 = fromDividedPowers f
   Inputs
    f:RingElement
    f:Matrix
   Outputs
    f1:RingElement
    f1:Matrix
   Description
    Text
     If f is a polynomial, or a matrix of polynomials, written in the divided power monomial basis, then f1 is the corresponding
     object written in the ordinary monomial basis.
    Example
     S = ZZ/101[x,y]
     x^2 == fromDividedPowers (2*x^2)
   SeeAlso
    toDividedPowers
///
doc ///
   Key 
    toDividedPowers
    (toDividedPowers,Matrix)
    (toDividedPowers,RingElement)
   Headline 
    Translates to divided power monomial basis from ordinary monomial basis
   Usage
    f1 = toDividedPowers f
   Inputs
    f:RingElement
    f:Matrix
   Outputs
    f1:RingElement
    f1:Matrix
   Description
    Text
     If f is a polynomial, or a matrix of polynomials, then f1 is the corresponding
     object written in the divided power monomial basis, where for example $x^2$ denotes $x^{(2)}$
    Example
     S = ZZ/101[x,y]
     2*x^2 == toDividedPowers (x^2)
   SeeAlso
    fromDividedPowers
///

doc ///
   Key
    toDual
    (toDual, ZZ,  Ideal)
    (toDual, ZZ, Matrix)
    [toDual,DividedPowers]
   Headline
    finds the inverse system to an ideal up to a given degree
   Usage
    M = toDual (ZZ, I)
   Inputs
    I:Ideal
     in a standard graded polynomial ring
   Outputs
    M:Matrix
     interpreted as list of generators of inverse system to I
   Description
    Text
     If I is an ideal, then toDual(d,I) = inverseSystem(d, I). 
     See the documentation of
     InverseSystems for the basic theory and of inverseSystem for the function.
     
     If I is a matrix, then the code first replaces it with the ideal generated by the entries.
   Caveat
    The Base ring of the input must be a standard graded polynomial ring over a field.
   SeeAlso
    InverseSystems
    inverseSystem    
    fromDual
///
doc ///
   Key
    fromDual
    (fromDual, RingElement)
    (fromDual, Matrix)
    [fromDual,DividedPowers]
   Headline
    Ideal from inverse system
   Usage
    M  = fromDual f
   Inputs
    f:Matrix
    f:RingElement
   Outputs
    M:Matrix
     whose image is the inverse system
   Description
    Text
     The matrix (or ring element) f is interpreted as an element of the linear dual of
     the base polynomial ring, where the monomial in f are interpreted as monomials in the 
     divided power basis (the dual basis of the monomial basis of ring f. Thus
	 
     fromDual f = gens inverseSystem (f, DividedPowers => true).

     See the documentation nodes for InverseSystems for the theory, and inverseSystem for
     the function. Note that the operation of fromDual is not equivariant with respect
     to the general linear group, acting by change of basis. This not a bug, but
     follows because fromDual
     uses the divided power basis. If equivariant behavior is desired, use
     the option DividedPowers=>false (which is the default behavior of 
     inverseSystem):
    Example
     S = ZZ/101[x,y]
     f1 = x^2
     f2 = (x+y)^2
     betti res ideal fromDual f1
     betti res ideal fromDual f2
     betti res (I = ideal fromDual(f2, DividedPowers => false))
     I == inverseSystem (f2, DividedPowers => false)
     inverseSystem (f2, DividedPowers => true)
     ideal fromDual(f2, DividedPowers => false)
     ideal fromDual(f2, DividedPowers => true)
   Caveat
    The Base ring of the input must be a standard graded polynomial ring over a field.

    Logically, the output of fromDual should be an ideal, not a matrix, but the original
    function was converted from the classic Macaulay, where this distinction was not made.
   SeeAlso
    InverseSystems
    inverseSystem    
    fromDual
///
doc ///
   Key
    isStandardGradedPolynomialRing
    (isStandardGradedPolynomialRing, Ring)
   Headline
    Checks whether a ring is a polynomial ring over a field with variables of degree 1
   Usage
    b = isStandardGradedPolynomialRing R
   Inputs
    R:Ring
   Outputs
    b:Boolean
   SeeAlso
    isPolynomialRing
///

--check that the bounds are right
TEST///
S= QQ[a,b,c]
d= 3
f = product(3,i->S_i^(d-1))
I = ideal apply(3, i->S_i^d)
assert(I == inverseSystem f)
assert (I == inverseSystem inverseSystem(3, I))
///

TEST///
R = ZZ/101[a,b]
assert(isStandardGradedPolynomialRing R)
assert(not isStandardGradedPolynomialRing (R[x]))
assert(not isStandardGradedPolynomialRing (ZZ[x]))
assert(not isStandardGradedPolynomialRing (R[x, Degrees =>{{1,1}}]))
///       

--fromDividedPowers and toDividedPowers are inverse to one another
TEST///
setRandomSeed 0
kk = QQ
n = 3
S = kk[a,b,c]
p = (a+b)^2
q = toDividedPowers p
assert(q == 2*a^2+2*a*b+2*b^2)
assert(p ==fromDividedPowers q)

P = (random(S^{0,1},S^{-2,-3}))
Q = fromDividedPowers toDividedPowers P
R = toDividedPowers fromDividedPowers P
assert(P==Q)
assert(P == R)

setRandomSeed 0
kk = QQ
n = 3
S = kk[a,b,c]
g = random(S^3, S^3)
testmap = map(S,S,(vars S)*g)
g == fromDividedPowers toDividedPowers g
g == toDividedPowers fromDividedPowers g
///

--with or without divided powers,
--applying inverseSystem twice should be 
--the identity on ideals AND on submodules of the dual, represented as matrices.
TEST///
setRandomSeed 0
S = QQ[a,b]
G = random(S^2,S^2)
GG = map(S,S,(vars S)*G)
GG' = map(S,S,(vars S)*transpose G^-1)
f = a^2
g = b^3
h = GG matrix{{f,g}}
I = ideal h
--the equality for ideals:
assert(I ==  inverseSystem inverseSystem(3, I))
assert(I ==  inverseSystem(inverseSystem(4, I, DividedPowers=>true), 
			   DividedPowers =>true
			  )
       )
--The equality for matrices 
--since we can't directly compare submodules of the injective hull, we compare them by taking their annihilators:
assert(inverseSystem h == inverseSystem inverseSystem(4,inverseSystem h))

assert(
    inverseSystem(h,DividedPowers=>true)
    == 
    inverseSystem(
	inverseSystem(4,
	    inverseSystem(h, DividedPowers=>true),
	              DividedPowers=>true),DividedPowers =>true)
	      )

///

--inverseSystem is equivariant on matrices
TEST///
setRandomSeed 0
kk = QQ
n = 3
S = kk[a,b,c]
assert(inverseSystem matrix{{0_S}} == ideal(1_S))

g = random(S^3, S^3)
testmap = map(S,S,(vars S)*g)
testmap' = map(S,S,(vars S)*(dual g)^-1)

f = matrix{{a,b^3}}

f = random(S^1, S^{-2,-2,-3});
assert(inverseSystem testmap' inverseSystem(5,f) == 
       inverseSystem inverseSystem(5, (testmap f))
		)


///

--inverseSystem is equivariant on 0-dimensional ideals 
--(and, up to the given degree, on arbitrary ideals)
TEST///
setRandomSeed 0
kk = QQ
n = 3
S = kk[a,b,c]
g = random(S^3, S^3)
testmap = map(S,S,(vars S)*g)
testmap' = map(S,S,(vars S)*(dual g)^-1)

f = random(S^1, S^{-2,-2,-3});
assert(inverseSystem testmap' inverseSystem(5,f) == 
       inverseSystem inverseSystem(5, (testmap f))
		)

f = ideal(a,b^3)+(ideal vars S)^4;
assert( inverseSystem testmap'  inverseSystem (4,f) == 
       inverseSystem inverseSystem (4, testmap f)
		)

mm=ideal vars S
f = ideal(a,b^3)
f = ideal random(S^1, S^{-2,-3});
assert(mm^5+inverseSystem testmap'  inverseSystem (4,f) == 
       mm^5+inverseSystem inverseSystem (4, testmap f)
		)
///

TEST///
--the local, that is, inhomogeneous case:
S = QQ[a,b,c]
G = random(S^3,S^3)
GG = map(S,S,(vars S)*G)
M = matrix{{a^2+b^3}}
I1 = inverseSystem M
I2 = inverseSystem GG M
assert(hilbertSeries ideal leadTerm gens gb I1===hilbertSeries ideal leadTerm gens gb I2)

S = QQ[x,y]
I = ideal"x3,xy+y4+y5"+(ideal vars S)^7

M' = inverseSystem(7, I)
assert(I ==inverseSystem M')

M'' = inverseSystem(7,I,DividedPowers => true)
assert(I ==inverseSystem (M'',DividedPowers => true))

assert(M'!= M'')
---
S = QQ[x,y]
I = ideal"x3,xy+y4+y5"+(ideal vars S)^7

M' = inverseSystem(7, I)
assert(I ==inverseSystem M')

M'' = inverseSystem(7,I, DividedPowers => true)
assert(I ==inverseSystem (M'',DividedPowers => true))

assert(M'!= M'')
///
TEST///
kk = ZZ/32003
S = kk[a,b,c]
f = matrix"a3+b3+c3"
assert(fromDual f == fromDual toDual(10,fromDual f))
f = f++f
assert(fromDual f == fromDual toDual(10,fromDual f))
f = random(S^{1,2,3},S^{0,-1,-2})
assert(fromDual f == fromDual toDual(10,fromDual f))
///


     
TEST ///
setRandomSeed 0
          R = ZZ/32003[x_1..x_3];
	  g = random(R^1, R^{-4})
	  f = fromDual g
	  F = res ideal f
	  assert(apply(4, i->rank F_i) == {1,7,7,1})
          ///
TEST ///
    R = ZZ/101[a..d]
    f = matrix{{a^3 + b^3 + c^3 + d^3 + (a+b+c)^3}}
    fdual = fromDual f
    assert(f == toDual(4, fdual))
///

TEST///
     	  R = ZZ/32003[a..e];
	  f = matrix{{a^2, b^2, c^2, d^2, e^3, a*d-e^2}}
	  betti res coker f
	  g = toDual(1,f)
	  assert((ideal fromDual g == ideal f) ==false)
	  g = toDual(2,f)
	  assert((ideal fromDual g == ideal f) ==true)
	  g = toDual(3,f)
	  assert((ideal fromDual g == ideal f) ==true)	  
	  ///

TEST ///
--a case where the ideal is not 0-dimensional
 R = QQ[a,b,c]
 f= matrix"a-b,c"
toDual(2,f) == matrix {{a^2+a*b+b^2}}
///

end--

document {
     Key => "inverse systems",
     "Suppose that R = k[x1,...,xn], and that
     E = k[y1,...,yn] is the injective envelop of k.  IE, E is given the R-module structure:
     x^A . y^B = y^(B-A), if B-A >= 0 in every component, and x^A . y^B = 0 otherwise.",
     PARA{},
     "If I is an ideal of R, then the submodule I' = Hom_R(R/I,E) of E is called the
     (Macaulay) inverse system of I.  I is zero-dimensional if and only if I' is finitely generated.",
     PARA{},
     "This is a dual operation, since I can be recovered as ann_R(I').",
     PARA{},
     "In Macaulay2, currently the computation of the inverse system I' (toDual) and of the 
     ideal I from I' (fromDual) are restricted to the situation where I and I' are homogeneous.
     As an example, let's compute the ideal corresponding to a cubic.",
     EXAMPLE lines ///
     	 R = QQ[a..e];
	 g = matrix{{a^3+b^3+c^3+d^3+e^3-d^2*e-a*b*c-a*d*e}}
	 f = fromDual g
	 I = ideal f
	 ///,
     "The resulting ideal is always zero dimensional, and its Cohen-Macaulay type is the
     number of generators of the submodule E defined by g.  Therefore, if g is a 1 by 1 matrix,
     then the resulting ideal is Gorenstein.",
     EXAMPLE lines ///
	 res I
	 betti oo
         ///,
     PARA{},
     "The other direction (starting with an ideal I) is more complicated, since the result may not be finitely generated.
     So, we must give an integer d as well as the generators of I:",
     EXAMPLE lines ///
     	 toDual(3,f)
         ///,
     "The integer d has two interpretations.  The most general is that the (finitely generated)
     intersection of I' and the submodule generated by y1^d ... yn^d is returned.
     If the ideal I is zero dimensional, d should be an integer such that x^(d+1) is in I = image f for 
     every variable x.",
     EXAMPLE lines ///
         f = matrix{{a*b,c*d,e^2}}
	 toDual(1,f)
	 toDual(2,f)
	 toDual(3,f)
	 g = toDual(4,f)
	 fromDual g
	 ///,
     SeeAlso => {toDual, fromDual}
     },


restart
loadPackage("InverseSystems", Reload =>true)
uninstallPackage("InverseSystems")
installPackage("InverseSystems")
check "InverseSystems"
viewHelp InverseSystems

cubic = (g,r) -> (
    --make the sum of r cubes in g-2 variables
    x := symbol x;
    S := ZZ/101[x_0..x_(g-3)];
    sum(r, i-> if i<=g-3 then x_i^3 else ((random(S^1,S^{-1}))_0_0)^3)
    )
cubic(6,3)
betti res inverseSystem cubic(6,5)

g= 9
netList apply(toList(g-2..2*g-4), j-> betti res inverseSystem cubic(g,j))


-------code from Mats
modFromDual Matrix := Matrix => (F) -> (
    S := ring F;
    d := max for m in first entries compress flatten monomials F list sum first exponents m;
    L := matrix {{1_S}}; 
    for i from 1 to d+1 do L = L | gens power(ideal vars S,i); 
    pM := (L ** id_(target F)) * syz sub(contract((transpose L) * L,transpose F),vars S-vars S);
    presentation prune trim coker pM
    )

newToDual (ZZ,Matrix) := Matrix => (d,f) -> (
    S := ring f;
    g := product apply(generators S, v -> v^d);
    I := ideal for i to -1 + numgens S list S_i^(d+1);
    R := S / I;
    transpose contract(
	transpose mingens image (sub(gens ker sub(transpose f,R),S)),
	map(S^{degree g},S^1,matrix{{g}})))

newToDualTrunc (ZZ,Matrix) := Matrix => (d,f) -> (
    newToDual (d,f | ((target f) ** gens power(ideal vars ring f,d)))
    )

doc ///
   Key 
      newFromDual
   Headline 
      computes the submodule annihilating an inverse system
   Usage 
      N = newFromDual(F)
   Inputs
      F:Matrix 
         F is a matrix where the columns are the generators of an inverse system in   
	 the inverse system of a free module. 
   Outputs
      N:Matrix
         N is a presentation matrix for the module that has F as its inverse system.
   Description
      Text
         
      Example 
         F = matrix(QQ[x,y,z],{{x^4,y^4},{z^4,x^2*y^2}})
         N = newFromDual(F)
      Text 
         The command {\tt newFromDual} extends the command {\tt fromDual} so that it also 
         works for modules.
///

doc ///
   Key 
      newToDual
   Headline 
      computes the inverse system of a given module
   Usage 
      F = newFromDual(d,N)
   Inputs
      d:ZZ
      N:Matrix 
         d is an integer specifying a particular degree for the truncation of the inverse
	 system to be computed. (Needed since it is in general not finitely generated)
	 N is a presentation matrix for a module over a polynomial ring. 
   Outputs
      F:Matrix
         The columns of F are generators of the intersection of the inverse 
	 system of the module presented by N with the inverse system generated by a the 
	 d:th power of the product of the variables. 
   Description
      Text
         
      Example 
         N = matrix(QQ[x,y,z],{{x^4,y^4},{z^4,x^2*y^2}})
         F = newToDual(5,N)
      Text 
         The command {\tt newToDual} extends the command {\tt toDual} so that it also 
         works for modules.
///
