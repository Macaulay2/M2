-- Data types understood by the engine:
-- ZZ, IntegerArray, String, Boolean, 
-- MonomialOrder, Monoid, Ring, FreeModule, Matrix, MutableMatrix
-- Monomial, RingElement, Vector, 
-- RingMap, MonomialIdeal, TermIdeal,
-- Computation types: ...

-- install(ggindex, cmdIndex, TY_INT);
-- remove gglength: or at least give an error for most types,
--   and set only for data types.
-- Put ggsize in, ggrank, ggnumvars, ggnumrows, ggnumcols?
///
ggindex relies on index_of.  REMOVE these.  ggindex is only used for det/pfaff
computations.

  // viewing information
  install(ggtonet, cmdVToNet);
  
  // information about objects
  install(gglength, cmdLength);
///

///
Routines missing from Ecommands:
  Monoid.nvars
  Monoid.MonomialOrder
  Ring.characteristic
  Ring.coefficientRing
  Ring.Monoid
  FreeModule.Ring
  FreeModule.rank
  FreeModule.degrees
  FreeModule.inducedOrder
  RingElement.Ring
  RingElement.term (uses intarray directly rather than a monomial)
  RingElement.leadterm?
  RingElement.degree => (lo,hi)
  Vector.FreeModule
  Vector.maketerm ??
  Vector.degree => lo,hi
  Matrix.matrixtype (left/right?)
  RingMap.Ring
  -- other features:
  -- non-comm arithmetic
  -- immutable freemodule/matrix
  -- left/right modules?
  
///
  
-------------------------------------------
-- Creation of monoids, monomial orders ---
-------------------------------------------
-- ggmonoid
-- ggzeromonoid
-- ggMO*
document { "gglength(Monoid) : ZZ",
     "The number of variables in the monoid."
     }
///
  // Construction of monomial orders
  install(ggMOgrevlex, cmd_mo_grevlex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOrevlex, cmd_mo_revlex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOglex, cmd_mo_glex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOlex, cmd_mo_lex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOelim, cmd_mo_elim, TY_INTARRAY, TY_INTARRAY, TY_INT);
  install(ggMOproduct, cmd_mo_product1, TY_INTARRAY, TY_INTARRAY, TY_INTARRAY);

  -- Monomials 
  --install(ggisequal, cmd_monoid_isequal, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggiszero, cmd_monoid_isone, TY_MONOMIAL);
  --install(ggdivides, cmd_monoid_divides, TY_MONOMIAL, TY_MONOMIAL);
  install(ggcompare, cmd_monoid_compare, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggdegree, cmd_monoid_degree, TY_MONOMIAL);

  --install(ggvar, cmd_monoid_var, TY_INT, TY_INT);
  install(ggMONOMIAL, cmd_monoid_monomial);

  --install(ggmult, cmd_monoid_mult, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggdiv, cmd_monoid_div, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggpower, cmd_monoid_power, TY_MONOMIAL, TY_INT);
  --install(gglcm, cmd_monoid_lcm, TY_MONOMIAL, TY_MONOMIAL);
  --install(gggcd, cmd_monoid_gcd, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggmonsyz, cmd_monoid_monsyz, TY_MONOMIAL, TY_MONOMIAL);

  --install(ggsat, cmd_monoid_sat, TY_MONOMIAL, TY_MONOMIAL);
  --install(ggradical, cmd_monoid_radical, TY_MONOMIAL);
///
-------------------------------------------
-- Creation of rings ----------------------
-------------------------------------------
document { "ggZ() : Ring",
     "Yields the ring ZZ"
     }

document { "ggcharp(ZZ,Monoid) : Ring",
     "ggcharp(p,D) yields the prime field ZZ/p, with degree monoid D.
     Current restrictions: p must be a prime number less than 2^15."
     }

document { "ggGF(RingElement) : Ring",
     "If f is an irreducible polynomial in a ring ZZ/p[x] of degree n, then
     ggGF(f) yields the ring GF(p^n) = ZZ/p[x]/(f).  Elements of this ring
     are stored as exponents of x.  It is also assumed that x is a primitive
     element of this ring, that is, x has order p^n-1."
     }

document { "ggpolyring(Ring,Monoid) : Ring",
     "ggpolyring(K,M) creates the polynomial ring K[M].  This ring is graded
     with the same degree monoid that M has."
     }

document { "ggweylalgebra(Ring,Monoid,IntegerList,IntegerList,ZZ) : Ring",
     "ggweylalgebra(K,M,d,x,h) yields either a Weyl algebra, or a homogenized
     Weyl algebra.  The underlying polynomial ring is K[M].  Each integer in
     the lists d,x correspond to variables in the ring.  The two lists d,x,
     should have the same length.  The Weyl algebra is defined to be the
     K-algebra with relations $x_i d_i-d_i x_i=H$, where H is 1 if h < 0, or
     is y^2, if y is the h-th variable.",
     PARA,
     "For example, if M is the monoid with variables x,y,dx,dy,h, then
     ggweylalgebra(K,M,{2,3},{0,1},4) is the homogenized Weyl algebra.",
     PARA,
     "There is another, deprecated form of this command, which will be
     removed soon" 
     }

document { "ggschur(Ring,Monoid) : Ring",

     "ggschur(K,M) yields a ring with K-basis K[M], but with multiplication
     given by the Littlewood- Richardson rule.  Each monomial represents an
     irreducible representation of GL(n), where n is the number of variables
     in M.  The monomial [e1,...,en] corresponds to the representation with
     highest weight vector {e1+...+en, e2+...+en, ..., en}.",

     SEEALSO "ggdim"

     }

document { "ggqring(Matrix) : Ring",

     "If m is a matrix over a ring R, having one row, and whose entries are a
     Groebner basis for the ideal I they generate, and if R is a polynomial
     ring, or a quotient of a polynomial ring, then ggqring(m) yields the
     quotient ring R/I.",

     PARA,

     "The ring R may be skew-commutative, or even a Weyl algebra.  In the
     latter case, the elements of the quotient must be in the center of the
     ring (i.e. only involve variables which commute with all other
     variables"

     }

document { "ggfractionfield(Ring) : Ring",

     "If R is a domain, then ggfractionfield(R) yields its field of
     fractions",

     PARA,

     "Current BUGS: display over QQ sometimes places the negative in the
     denominator.  More seriously, fractions are not always reduced to lowest
     terms."

     }

document { "gggetideal(Ring) : Matrix",

     "Given a polynomial ring R, or quotient of one, gggetideal(R) yields a
     one row matrix, whose entries form a minimal Groebner basis of an ideal
     I in a polynomial ring S (not a quotient), such that R = S/I.",

     PARA,
     
     "If A is a polynomial ring, and B = A/I, C=B/J, then gggetideal(C) gives
     a minimal Groebner basis of (I'+J'), where I' and J' are lifts of these
     ideals to A."
     
     }

document { "gglength(Ring) : ZZ",
     "The number of variables in the ring."
     }

-------------------------------------------
-- RingElement ----------------------------
-------------------------------------------

document { "ggisequal(RingElement,RingElement) : Boolean",

     "ggisequal(f,g) is (f == g).  f and g must be elements of the same
     ring."

     }

document { "ggiszero(RingElement) : Boolean",

     "Whether the argument is the zero element of the ring."

     }

document { "ggisunit(RingElement) : Boolean",

     "ggisunit(f) yields true if f is a unit in its ring R",

     PARA,

     "Caveat/BUG: This function works fine if R is a coefficient field, or a
     polynomial ring, or a polynomial ring of the form K[x]/I.  However, if R
     = S/I, then it is possible for f to be a unit, but this routine will
     miss that fact."

     }

document { "ggfromint(Ring,ZZ) : RingElement",

     "ggfromint(R,n) yields (the image of the integer) n in the ring R"

     }

document { "ggvar(ZZ,ZZ,Ring) : RingElement",

     "If x is the v'th variable in the ring R, then ggvar(v,e,R) yields the
     element x^e.",

     PARA,

     "If e is negative, and the element of the monoid x^e is not defined,
     then this routine exists with an error."

     }

document { "ggnegate(RingElement) : RingElement",
     "ggnegate(f) returns -f."
     }

document { "ggadd(RingElement,RingElement) : RingElement",

     "ggadd(f,g) yields f+g.  If f and g are ring elements in different
     rings, then an error is raised."

     }

document { "ggsubtract(RingElement,RingElement) : RingElement",

     "ggsubtract(f,g) yields f-g.  If f and g are ring elements in different
     rings, then an error is raised."

     }

document { "ggmult(RingElement,RingElement) : RingElement",

     "ggmult(f,g) yields f*g.  If f and g are ring elements in different
     rings, then an error is raised.  It is possible for monomial overflow to
     occur, if f and g are elements of a polynomial ring."

     }

document { "ggmult(ZZ,RingElement) : RingElement",
     "ggmult(n,f) yields n*f."
     }

document { "ggpower(RingElement,ZZ) : RingElement",

     "ggpower(f,n) yields f^n, if n is >= 0.  If n < 0, then 1/f^(-n) is
     returned, if this element is invertible in the ring.",

     PARA,
     
     "BUG: If n is negative, and f is invertible, this routine may
     incorrectly say that that f is not invertible."
     
     }

document { "ggdiv(RingElement,RingElement) : RingElement",
     
     "ggdiv(f,g) yields f/g, if g divides f in the common ring R of f, g.  If
     g does not divide f, then the following formula always holds: f =
     ggdiv(f,g) * g + ggmod(f,g).",
     
     PARA,
     
     "If R is a polynomial ring, then division is performed by using long
     division as in Buchberger's algorithm, and so ggdiv(f,g) is chosen so
     that no monomial of ggmod(f,g) is divisible by the lead monomial of g.",
     
     PARA,
     
     "Caveat/possible BUG in documentation: Is multiplication above correct
     in skew polynomial rings or Weyl algebras?",
     
     SEEALSO ("ggmod", "ggdivmod")
     }

document { "ggdivmod(RingElement,RingElement) : {RingElement,RingElement}",

     "ggdivmod(f,g) yields the pair of values {ggdiv(f,g), ggmod(f,g)}.",

     SEEALSO ("ggmod", "ggdiv")
     }

document { "ggmod(RingElement,RingElement) : RingElement",

     "ggmod(f,g) yields the remainder after dividing f by g.  if f and g are
     not elements in the same ring, an error is raised.",
     
     SEEALSO ("ggdiv", "ggdivmod")
     }

document { "gggcd(RingElement,RingElement) : RingElement",

     "If f and g are elements of a ring R which is known to have greatest
     common divisors, then gggcd(f,g) yields this element.  If f and g are
     elements of different rings, an error is raised.",

     PARA,

     "BUG: gcd's exist in multivariate polynomial rings, but this is not
     implemented yet..."

     }

document { 
"gggcdextended(RingElement,RingElement) : {RingElement,RingElement,RingElement}",

     "If f and g are elements of a Euclidean domain R,, then
     gggcdextended(f,g) yields {a,x,y}, where a is the gcd, and x f + y g =
     a.  If f and g are elements of different rings, an error is raised."
 
     }

document { "ggterm(Ring,RingElement,Monomial) : RingElement",

     "If R is a polynomial ring, or quotient of, K[M], and a is an element of
     K, and m is a monomial (thought of as in M), then ggterm(R,a,m) yields
     a*m as an element of R."

     }

document { "gggetcoeff(RingElement,Monomial) : RingElement",

     "If R is a polynomial ring, or quotient of, K[M], then gggetcoeff(f,m)
     yields the (possibly zero) coefficient of m in f, as an element of the
     coefficient ring K."

     }

document { "ggleadcoeff(RingElement) : RingElement",

     "If R is a polynomial ring, or quotient of, K[M], then ggleadcoeff(f)
     yields the coefficient of the lead monomial of f, as an element of the
     coefficient ring K."

     }

document { "ggleadmonom(RingElement) : Monomial",

     "If R is a polynomial ring, or quotient of, K[M], then ggleadmonom(f)
     yields the lead monomial."

     }

-- ggleadterm(f:RingElement) : RingElement

document { "gggetterms(RingElement,ZZ,ZZ) : RingElement",

     "If f is an element of a polynomial ring (or quotient of one), then
     gggetterms(f,lo,hi) returns the polynomial consisting of those monomials
     of f in the range [lo,hi].  If either lo or hi is negative, then they
     refer to the last indices.  For example, gggetterms(f,-3,-1) returns the
     sum of the last three terms of f."

     }

document { "ggishomogeneous(RingElement) : Boolean",

     "Is the element homogeneous, with respect to the grading in the ring.
     Note that if the ring of this element is not graded, this will return
     false."

     }

document { "ggdegree(RingElement) : IntegerArray",

     "Yields the (multi-)degree of the element, as a list of integers."

     }

document { "gghomogenize(RingElement,ZZ,IntegerArray) : RingElement",

     "If f is an element of a polynomial ring R, and wts is a list of
     integers of length = number of variables of R, let D be the maximum
     degree of any monomial of f, under the weight vector wts.  Then
     gghomogenize(f,v,wts) yields the homogenization
     gghomogenize(f,v,D,wts).",

     SEEALSO gghomogenize1
     }

document { "gghomogenize1(RingElement,ZZ,ZZ,IntegerArray) : RingElement",

     "If f is an element of a polynomial ring R, v is the integer index of a
     variable h of this ring, D is an integer, and wts is a list of integers
     of length = number of variables of R, then gghomogenize1(f,v,D,wts)
     yields the homogenization of f: each monomial c*m of f is replaced by
     c*m*h^r, if the weight(m*h^r ) = D, and by 0, if no such r exists.",

     SEEALSO gghomogenize
     }

--ggweights(f:RingElement,wts:IntegerArray) : {ZZ,ZZ} -- lo, hi values obtained.

document { "ggdim(RingElement) : ZZ",

     "If f = sum(c_i * m_i) is a polynomial in a Schur ring having n
     variables, then ggdim(f) yields sum(c_i * dim(m_i)), where dim(m_i) is
     the dimension of the irreducible GL(n)-representation corresponding to
     the monomial m_i.",

     SEEALSO "ggschur"
     }

document { "ggnumerator(RingElement) : RingElement",

     "If f = a/b is an element of the fraction field of R, ggnumerator(f)
     yields the numerator a, as an element of R"

     }

document { "ggdenominator(RingElement) : RingElement",

     "If f = a/b is an element of the fraction field of R, ggdenominator(f)
     yields the denominator b, as an element of R"

     }

document { "ggfraction(Ring,RingElement,RingElement) : RingElement",

     "If f and g are elements of a ring R, and if K is the field of fractions
     of R, then ggfraction(K,f,g) yields the element f/g of K."

     }

document { "gglength(RingElement) : ZZ",
     "Yields the number of monomials in the ring element, if the base ring
     is a polynomial ring."
     }
-------------------------------------------
-- FreeModule -----------------------------
-------------------------------------------
document { "ggfree(Ring,ZZ) : FreeModule",
     "ggfree(R,n) yields the free module R^n."
     }

document { "ggfree(Ring,IntegerArray) : FreeModule",

     "If degs is a list of integers of length n*d, where d is the length of a
     degree vector for the ring R, then ggfree(R,degs) yields the graded free
     module R^n, where the i-th basis element has degree equal to the
     negative of the i-th vector in degs.",

     PARA,

     "For example, if R has a bi-grading, then ggfree(R,{1,1,2,3,0,-1}) gives
     a free module of rank three, and the generators have degrees {-1,-1},
     {-2,-3}, and {0,1}."

     }

document { "ggfree(Matrix) : FreeModule",

     "ggfree(m) returns a free module over the ring of m, of rank = number of
     columns of m, whose monomial order is an induced (Schreyer) order.  The
     monomial order is the following: a*e_i > b*e_j if a * leadmonom(m(e_i))
     > b * leadmonom(m(e_j)), or these two monomials are scalar multiples of
     each other, and i > j.  Here a,b are monomials in the ring of m, and
     e_i, e_j are basis vectors for the source of m."

     }

--document { "ggfree(R:Ring, ...)"
--     }

document { "gglength(FreeModule) : ZZ",
     "gglength(F) yields the rank of F."
     }

document { "ggisequal(FreeModule,FreeModule) : FreeModule",

     "ggisequal(F,G) returns true if F and G have the same rank, the same
     degrees of basis elements, and the same monomial order, otherwise false
     is returned.",
     
     PARA, 
     
     "Bug: If F and G are free modules with an induced order, then currently
     part of that information is not considered, so they may be considered
     equal, even if this monomial order is different."

     }

document { "ggadd(FreeModule,FreeModule) : FreeModule",

     "ggadd(F,G) yields the direct sum of F and G. An error is raised if F
     and G are free modules over different rings."

     }

document { "ggmult(FreeModule,FreeModule) : FreeModule",

     "ggmult(F,G) yields the tensor product of F and G. An error is raised if
     F and G are free modules over different rings.",

     PARA,

     "If F has basis e1, ..., em, and G has basis f1, ..., fn, then the
     (ordered) basis of ggmult(F,G) is e1*f1, e1*f2, ..., e1*fn, e2*f1, ...,
     ..., em*fn.",

     PARA,

     "Caveat: If F or G have an induced monomial order, then that order is
     lost in this product."

     }

document { "ggtranspose(FreeModule) : FreeModule",

     "ggtranspose(F) yields the graded dual of F: the degrees of the
     generators of ggtranspose(F) are the negatives of the degrees of the
     generators of F."

     }

document { "ggshift(FreeModule,IntegerArray) : FreeModule",

     "If F is a free module, and d is a list of integers, of length the
     number of degrees of the base ring R, then ggshift(F,d) yields
     ggmult(F,ggfree(R,d))."

     }

document { "ggsubmodule(FreeModule,IntegerArray) : FreeModule",

     "If s is a list of integers of length n, ggsubmodule(F,s) yields a free
     module of rank n, whose i-th basis vector has degree the same as the s_i
     th basis vector of F.  It is assumed that each integer in s is in the
     range [0, rank(F)-1]."

     }

document { "ggsymm(FreeModule,ZZ) : FreeModule",

     "If F is a free module with basis e1, ..., en, then ggsymm(F,p) is the
     p-th symmetric power of F.",

     PARA,

     "What order are the new basis vectors in?"
     }

document { "ggexterior(FreeModule,ZZ) : FreeModule",

     "If F is a free module with basis e1, ..., en, then ggsymm(F,p) is the
     p-th symmetric power of F.",

     PARA,
     "What order are the new basis vectors in?"
     }

--document { "gglcm(F:FreeModule) : IntegerArray"
--     }

--document { "gggcd(F:FreeModule) : IntegerArray"
--     }

-------------------------------------------
-- Vector ---------------------------------
-------------------------------------------
document { "ggvector(FreeModule,RingElement,...,RingElement) : Vector",

     "If F is a free module of rank n over a ring R, and r1, ..., rn are
     elements of R, then ggvector(F,r1,...,rn) yields the vector r1 e_0 + r2
     e_1 + ... + rn e_(n-1) of F."

     }

document { 
"ggsparsevector(FreeModule,IntegerArray,RingElement,...,RingElement) : Vector",

     "If F is a free module of over a ring R, and r is an integer array of
     length n, and r1, ..., rn are elements of R, then
     ggvector(F,r,r1,...,rn) yields the vector r1 e_(r#0) + r2 e_(r#1) +
     ... + rn e_(r#(n-1)) of F."

     }

document { "ggisequal(Vector,Vector) : Boolean",

     "ggisequal(f,g) is (f == g).  f and g must be elements of the same free
     module.",

     PARA,

     "Caveat: this last statement is important: if f and g appear to be the
     same, but their free modules are not identical, then f and g will not be
     equal."

     }

document { "ggiszero(Vector) : Boolean",
     "Whether the argument is the zero element."
     }

document { "ggzero(FreeModule) : Vector",
     "ggzero(F) yields the zero element of the free module F."
     }

document { "ggfromint(FreeModule,ZZ) : Vector",
     "gg(F,n) yields the n-th basis vector of F."
     }

document { "ggterm(FreeModule,RingElement,ZZ) : Vector",

     "ggterm(F,r,x) yields the vector r*e_x of F, where e_x is the x-th basis
     vector of F."

     }

document { "ggelem(Vector,ZZ) : RingElement",

     "ggelem(v,x) yields the ring element which is the x-th component of the
     vector v."

     }

document { "ggnegate(Vector) : Vector",
     "ggnegate(f) returns -f."
     }

document { "ggadd(Vector,Vector) : Vector",

     "ggadd(f,g) yields f+g.  If f and g are vectors in different free
     modules, then an error is raised.",

     PARA,

     "Caveat: how different can the free modules be before an error is raised?"
     }

document { "ggsubtract(Vector,Vector) : Vector",

     "ggsubtract(f,g) yields f-g.  If f and g are vectors in different free
     modules, then an error is raised.",

     PARA,

     "Caveat: how different can the free modules be before an error is
     raised?"

     }

document { "ggmult(RingElement,Vector) : Vector",

     "ggmult(r,v) yields r*v.  If the base rings of r and v are different,
     then an error is raised.  It is possible for monomial overflow to occur,
     if the base ring is a polynomial ring."

     }

document { "ggmult(Vector,RingElement) : Vector",

     "ggmult(v,r) yields v*r.  If the base rings of r and v are different,
     then an error is raised.  It is possible for monomial overflow to occur,
     if the base ring is a polynomial ring."

     }

document { "ggmult(ZZ,Vector) : Vector",
     "ggmult(n,v) yields n*v."
     }

--document { "gggetcoeff(f:Vector, m:Monomial, x:ZZ) : RingElement"
--     }

document { "ggleadcoeff(Vector) : RingElement",

     "If the base ring R of the vector v is a polynomial ring, or quotient
     of, K[M], then ggleadcoeff(v) yields the coefficient of the lead
     monomial of v, as an element of the coefficient ring K."

     }

document { "ggleadmonom(Vector) : Monomial",

     "If the base ring R of v is a polynomial ring, or quotient of, K[M],
     then ggleadmonom(v) yields the lead monomial of v."

     }

document { "ggleadcomp(Vector) : ZZ",

     "ggleadcomp(v) yields the lead component of v.  If v = c*m*e_i + lower
     terms, then the returned value is i."

     }

-- ggleadterm(f:Vector) : Vector

document { "gglength(Vector) : ZZ",
     "Yields the number of monomials in the vector."
     }
     
document { "gggetterms(Vector,ZZ,ZZ) : RingElement",

     "If v is a vector over a polynomial ring (or quotient of one), then
     gggetterms(v,lo,hi) returns the vector consisting of those monomials of
     v in the range [lo,hi].  If either lo or hi is negative, then they refer
     to the last indices.  For example, gggetterms(v,-3,-1) returns the sum
     of the last three terms of v."

     }

--document { "ggselect(v:Vector,F:FreeModule,s:IntegerArray) : Vector"
--     }  NOT USED IN M2 CODE...

document { "ggishomogeneous(Vector) : Boolean",

     "Is the element homogeneous, with respect to the grading in the ring.
     Note that if the ring of this element is not graded, this will return
     false."

     }

document { "ggdegree(Vector) : IntegerArray",
     "Yields the (multi-)degree of the element, as a list of integers."
     }

document { "gghomogenize(Vector,ZZ,IntegerArray) : Vector",

     "If f is an element of a free module F (over a polynomial ring R), and
     wts is a list of integers of length = number of variables of R, let D be
     the maximum degree of any monomial of f, under the weight vector wts.
     Then gghomogenize(f,v,wts) yields the homogenization
     gghomogenize(f,v,D,wts).",

     SEEALSO gghomogenize1
     }

document { "gghomogenize1(Vector,ZZ,ZZ,IntegerArray) : Vector",

     "If f is an element of a free module F (over a polynomial ring R), v is
     the integer index of a variable h of this ring, D is an integer, and wts
     is a list of integers of length = number of variables of R, then
     gghomogenize1(f,v,D,wts) yields the homogenization of f: each monomial
     c*m*e_i of f is replaced by c*m*h^r*e_i, if the weight(m*h^r*e_i ) = D,
     and by 0, if no such r exists.",

     SEEALSO gghomogenize
     }

--ggweights(f:Vector,wts:IntegerArray) : {ZZ,ZZ} -- lo, hi values obtained.

TEST /// -- of vector interface routines
size Vector := f -> (
     sendgg(ggPush f, gglength); 
     eePopInt())
someTerms(Vector,ZZ,ZZ) := Vector => (f,i,n) -> (
     F := class f;
     if n <= 0
     then 0_F
     else (
	  sendgg(ggPush f, ggPush i, ggPush (i + n - 1), gggetterms);
	  new F))
  R = QQ[a..e]
  m = transpose matrix{{(a+1)*(2*c+1), 3*(b+d)^2}}
  v = m_0
  F = target m
  assert(leadCoefficient v == 3)
  assert(leadComponent v == 1)
  leadMonomial v -- doesn't work
  assert(leadTerm v  == (leadCoefficient v) * b^2 *F_(leadComponent v))
    -- but note that ggleadterm doesn't work here...  
  assert(v == ((a+1)*(2*c+1))*F_0 + 3*(b+d)^2 * F_1)
  assert(v != 0)
  assert(v-v == 0)
  assert(0_F == 0)
  assert(v_0 == (a+1)*(2*c+1))
  assert(v + (-v) == 0)
  assert(2*v + (-v) == v)
  assert(size v == 7)
  assert(someTerms(v,-1,1) == F_0)
  assert(someTerms(v,0,1) == leadTerm v)
  assert(someTerms(v,1,size v - 1) == v - leadTerm v)
  -- test of homog routines
  F = R^{0,1,-2}
  assert(degree (F_0) == {0})
  assert(degree (F_1) == {-1})
  assert(degree (F_2) == {2})
  v = a^2 * F_0 + (b+c)* F_1 - F_2
  assert not isHomogeneous v
  assert(homogenize(v,e) == a^2 * F_0 + e^2*(b+c)* F_1 - F_2)
  w = a^2 * F_0 + (b+c)^3 * F_1 - F_2
  assert isHomogeneous w
    -- note: the second form is NOT used in the front-end (gghomogenize1)
  homogenize(w,e,{-1,0,0,0,1})
  ----
  -- Current problems with vector interface:
  -- - size, someTerms: not in the front end
  -- - leadMonomial not implemented in the engine...
  -- - degreeWeights not implemented
  -- - ggleadmonom not used
  -- - gghomogenize1 not used
  -- - ggterm not used?
  -- - what about right multiplication?
///
-------------------------------------------
-- MonomialIdeal --------------------------
-------------------------------------------
-- accessing the elements
-- over a ring?
-- what other routines? dual...
-- membership

document { "ggmonideal(Matrix,ZZ) : MonomialIdeal",
     }

document { "ggmatrix(MonomialIdeal) : Matrix",
     }

document { "gglength(MonomialIdeal) : ZZ",
     "The number of (minimal) generators for this ideal."
     }

document { "ggisequal(MonomialIdeal,MonomialIdeal) : Boolean",
     }

document { "ggradical(MonomialIdeal) : MonomialIdeal",
     }     

document { "ggadd(MonomialIdeal,MonomialIdeal) : MonomialIdeal",
     }

document { "ggmult(MonomialIdeal,MonomialIdeal) : MonomialIdeal",
     }

document { "ggintersect(MonomialIdeal,MonomialIdeal) : MonomialIdeal",
     }

document { "ggdiv(MonomialIdeal,MonomialIdeal) : MonomialIdeal",
     }

document { "ggdiv(MonomialIdeal,Monomial) : MonomialIdeal",
     }

document { "ggsat(MonomialIdeal,MonomialIdeal) : MonomialIdeal",
     }

document { "ggsat(MonomialIdeal,Monomial) : MonomialIdeal",
     }

document { "ggborel(MonomialIdeal) : MonomialIdeal",
     }

document { "ggisborel(MonomialIdeal) : Boolean",
     }

document { "ggcodim(MonomialIdeal) : ZZ",
     }

document { "ggprimes(MonomialIdeal) : MonomialIdeal",
     }

-- -+ ggcopy(I:MonomialIdeal) : MonomialIdeal  
-- -+ ggremove(I:MonomialIdeal):Monomial
--ggsubtract(I,J:MonomialIdeal) : MonomialIdeal

-------------------------------------------
-- TermIdeal ------------------------------
-------------------------------------------
document { "ggmatrix(TermIdeal) : Matrix",
     }

document { "ggtermideal(Matrix,ZZ) : TermIdeal",
     }

document { "gggetchange(TermIdeal,ZZ) : Matrix",
     }

document { "ggsearch(TermIdeal,Matrix) : Matrix",
     }

document { "gglength(TermIdeal) : ZZ",
     "Yields the number of elements in the term ideal."
     }
     
-------------------------------------------
-- Matrix ---------------------------------
-------------------------------------------
document { "ggmatrix(Vector,...,Vector,FreeModule,ZZ) : Matrix",

     "If v1, ..., vm are vectors in the free module F (or at least in a free
     module over the same ring as F, with the same rank as F), then
     ggmatrix(v1,...,vm,F,m) yields the matrix with columns v1, ..., vm, and
     target F.  The source (which has rank m) is chosen to make the matrix
     homogeneous, if possible."

     }

document { "ggmatrix(Vector,...,Vector,FreeModule,FreeModule) : Matrix",

     "If v1, ..., vm are vectors in the free module F, where m is the rank of
     the free module G, then ggmatrix(v1,...,vm,F,G) is the matrix whose
     columns are the v1, ..., vm, target is F, and the source is G."

     }

document { 
  "ggmatrix(Vector,...,Vector,FreeModule,FreeModule,IntegerArray) : Matrix",

     "ggmatrix(v1,...,vm,F,G,d) is basically the same as
     ggmatrix(v1,...,vm,F,G), except that the degree of the map is set to the
     integer list d (which should be a list of length = the length of degree
     vectors in this ring."

     }

document { "ggmatrix(FreeModule,FreeModule,Matrix) : Matrix",

     "ggmatrix(F,G,m) returns a matrix of the same size as m, but whose
     target is F, and whose source is G.  F and G must have the same rank as
     the target and source of m, respectively."

     }

document { "ggmatrix(FreeModule,FreeModule,Matrix,IntegerArray) : Matrix",

     "ggmatrix(F,G,m,d) returns a matrix of the same size as m, but whose
     target is F, and whose source is G.  F and G must have the same rank as
     the target and source of m, respectively.  Also, the degree of the
     resulting matrix is set to d."

     }

document { "ggisequal(Matrix,Matrix) : Boolean",

     "ggisequal(m,n) returns true if m and n have the same source and target
     and the entries of m and n are the same.  This is a very strict notion
     of equality.  Often ggiszero(ggsubtract(m,n)) is more appropriate."
     
     }

document { "ggiszero(Matrix) : Boolean",
     
     "ggiszero(m) is true if every entry of m is zero."
     
     }

document { "gggetrows(Matrix) : FreeModule",
     "gggetrows(m) yields the target of the map m."
     }

document { "gggetcols(Matrix) : FreeModule",
     "gggetcols(m) yields the source of the map m."
     }

document { "ggelem(Matrix,ZZ) : Vector",
     "ggelem(m,i) yields the i-th column of m, as a vector. Note that ggelem(m,0)
     yields the first column, since all indices are zero-based."
     }

document { "ggelem(Matrix,ZZ,ZZ) : RingElement",
     "ggelem(m,i,j) yields the (i,j) entry of the matrix, where (0,0) refers to the top left
     entry, i is the row number, and j is the column number."
     }

document { "gggetshift(Matrix) : IntegerArray",
     "gggetshift(m) yields the degree of the map m.  Every Matrix has a map degree, which is
     an element of the degree monoid (i.e. an integer list of length the length of a degree
     vector in the ring of m."
     }

document { "ggsetshift(Matrix,IntegerArray) : Void",
     "ggsetshift(m,d) sets the map degree of m to d.",
     SEEALSO "gggetshift(Matrix) : IntegerArray"
     }

document { "ggishomogeneous(Matrix) : Boolean",
     
     "Is the matrix homogeneous, with respect to the grading in the ring.
     Note that if the ring of this element is not graded, this will return
     false."

     }

document { "gghomogenize(Matrix,ZZ,IntegerArray) : Matrix",
     }

document { "ggnegate(Matrix) : Matrix",
     "ggnegate(m) yields -m."
     }

document { "ggadd(Matrix,Matrix) : Matrix",

     "ggadd(m,n) yields m+n.  If the sources are not identical, or the
     targets are not, but the ranks still match up, the addition is
     performed, but the corresponding free module is set to be R^n, where R
     is the base ring, and n is the rank.  Similarly, if the map degrees are
     different, the new map degree is set to zero.  This guarantees that
     addition is symmetric.  "

     }

document { "ggsubtract(Matrix,Matrix) : Matrix",

     "ggsubtract(m,n) yields m-n.  If the sources are not identical, or the
     targets are not, but the ranks still match up, the subtraction is
     performed, but the corresponding free module is set to be R^n, where R
     is the base ring, and n is the rank.  Similarly, if the map degrees are
     different, the new map degree is set to zero."

     }

document { "ggmult(RingElement,Matrix) : Matrix",
     }

document { "ggmult(Matrix,Matrix) : Matrix",
     }

document { "ggmult(Matrix,Vector) : Vector",
     }

document { "ggconcat(ZZ,Matrix,...,Matrix) : Matrix",
     }

document { "ggiden(FreeModule) : Matrix",
     }

document { "ggzeromat(FreeModule,FreeModule) : Matrix",
     }

document { "ggtranspose(Matrix) : Matrix",
     }

document { "ggsubmatrix(Matrix,IntegerArray,IntegerArray) : Matrix",
     }

document { "ggsubmatrix(Matrix,IntegerArray) : Matrix",
     }

document { "ggdirectsum(Matrix,Matrix) : Matrix",
     }

document { "ggdirectsum(ZZ,Matrix,...,Matrix) : Matrix",
     }

document { "ggtensor(Matrix,Matrix) : Matrix",
     }

document { "ggcontract(Matrix,Matrix) : Matrix",
     }

document { "ggdiff(Matrix,Matrix) : Matrix",
     }

document { "ggsymm(Matrix,ZZ) : Matrix",
     }

document { "ggexterior(Matrix,ZZ) : Matrix",
     }

document { "ggkoszul(Matrix,ZZ) : Matrix",
     }

document { "ggkoszul(Matrix,Matrix) : Matrix",
     }

document { "gginitial(Matrix) : Matrix",
     }

document { "gginitial(Matrix,ZZ) : Matrix",
     }

document { "ggcoeffs(Matrix,IntegerArray) : {Matrix,Matrix}",
     }

--  install(ggcoeffs, cmd_Matrix_var_coeffs, TY_MATRIX);  THIS IS DIFFERENT...

document { "ggsortcolumns(Matrix,ZZ,ZZ) : IntegerArray",
     }

document { "ggautoreduce(Matrix) : Matrix",
     }

document { "ggelim(Matrix,ZZ) : IntegerArray",
     }

document { "ggsat(Matrix,ZZ,ZZ) : {Matrix,ZZ}",
     } -- NOT QUITE THIS INTERFACE NOW....?

document { "ggmodtensor(Matrix,Matrix) : Matrix",
     }

document { "ggkbasis(Matrix,Matrix) : Matrix",
     }

document { "ggkbasis(Matrix,Matrix,IntegerArray) : Matrix",
     }

document { "ggtruncate(Matrix,Matrix,IntegerArray) : Matrix",
     }

document { "ggminleadterms(Matrix) : Matrix",
     }

document { "ggsimplify(Matrix,ZZ) : Matrix",
     }

document { "ggreshape(Matrix,FreeModule,FreeModule) : Matrix",
     }

document { "ggflip(FreeModule,FreeModule) : Matrix",
     }

document { "ggexteriorproduct(ZZ,ZZ,FreeModule) : Matrix",
     }

--document { "gglength(Matrix) : ZZ",
--     "Yields the number of columns of the matrix"
--     }
-------------------------------------------
-- Random Numbers -------------------------
-------------------------------------------
document { "ggrandomseed(ZZ) : Void",

     "ggrandomseed(n) sets the random number generator seed to n.  The seed
     is initially set to zero.  The random generator currently used is the
     'minimal' random generator of Park and Miller, described in the book
     'Numerical recipes in C'.  Currently only the low 32 or 64 bits of 'n'
     are used for the seed."

     }

document { "ggrandommax(ZZ) : ZZ",

     "ggrandommax(n) sets the maximum random number value.  The previous
     maximum value is returned",

     PARA,

     "Caveat-bug: n must fit into a machine integer: Larger integers are not
     returned by the random number routines."

     }

document { "ggrandomint() : ZZ",

     "ggrandomint() returns a random integer in the range
     (-randommax,randommax], where randommax is the value set using
     ggrandommax(n).",

     PARA,

     "Possible doc bug: Check that the range is what is given."

     }

document { "ggrandom(Ring) : RingElement",
     "Not completely implemented"
     }

document { "ggrandom(Ring,ZZ,ZZ) : Matrix",

     "ggmatrix(R,m,n) returns an m by n matrix of (images of) integers in R."

     }

document { "ggrandom(FreeModule,FreeModule,ZZ,IntegerArray):Matrix",
     "Not implemented yet"
     }

-------------------------------------------
-- MutableMatrix --------------------------
-------------------------------------------
document { "ggsparsematrix(Ring,ZZ,ZZ) : MutableMatrix",
     "ggsparsematrix(R,r,c) creates a mutable zero r by c matrix over R."
     }

document { "ggsparsematrix(Matrix) : MutableMatrix",

     "ggsparsematrix(m) creates a mutable matrix initially the same matrix as
     m, (although it is stored internally in a different way)."

     }

document { "ggmatrix(MutableMatrix) : Matrix",
     "ggmatrix(m) yields a matrix 'snapshot' of the current state of m."
     }

document { "ggiden(Ring,ZZ) : MutableMatrix",
     "ggiden(R,n) yields an n by n identity (mutable) matrix over R."
     }

document { "ggsetRowChange(MutableMatrix,MutableMatrix) : Void",
     }

document { "ggsetColChange(MutableMatrix,MutableMatrix) : Void",
     }

document { "gggetRowChange(MutableMatrix) : MutableMatrix",
     }

document { "gggetColChange(MutableMatrix) : MutableMatrix",
     }

document { "ggelem(MutableMatrix,ZZ,ZZ) : RingElement",
     }

document { "ggSetEntry(MutableMatrix,ZZ,ZZ,RingElement) : Void",
     }

document { "ggRowInterchange(MutableMatrix,ZZ,ZZ) : Void",
     }

document { "ggColumnInterchange(MutableMatrix,ZZ,ZZ) : Void",
     }

document { "ggRowAddMultiple(MutableMatrix,ZZ,RingElement,ZZ) : Void",
     }

document { "ggColumnAddMultiple(MutableMatrix,ZZ,RingElement,ZZ) : Void",
     }

document { "ggRowScale(MutableMatrix,ZZ,RingElement) : Void",
     }

document { "ggColumnScale(MutableMatrix,ZZ,RingElement) : Void",
     }

document { "ggsortcolumns(MutableMatrix,ZZ,ZZ) : Void",
     }

document { "ggpermute(MutableMatrix,ZZ,ZZ,IntegerArray) : Void",
     }

document { "ggmult(MutableMatrix,ZZ,ZZ) : RingElement",
     }

document { "ggreduce(MutableMatrix,ZZ,ZZ,ZZ) : Void",
     }

document { "ggfindGoodUnitPivot(MutableMatrix,ZZ,ZZ) : {ZZ,ZZ,ZZ}",
     }

document { "ggleadcoeff(MutableMatrix,ZZ) : RingElement",
     }

document { "ggleadcomp(MutableMatrix,ZZ) : ZZ",
     }

document { "ggnumrows(MutableMatrix) : ZZ",
     }

document { "ggnumcols(MutableMatrix) : ZZ",
     }

document { 
"ggRow2by2(RingElement,RingElement,RingElement,RingElement,MutableMatrix,ZZ,ZZ) : Void",
     }

document { 
"ggColumn2by2(RingElement,RingElement,RingElement,RingElement,MutableMatrix,ZZ,ZZ) : Void",
     }

document { "ggreducepivots(MutableMatrix) : Void",
     "Harry's routine."
     }

-- NOT IMPLEMENTED YET:     
--  install(ggRowGCDReduce, cmd_sparse_gcdRowReduce, 
--          TY_SparseMutableMatrix, TY_INT, TY_INT, TY_INT);
--  install(ggColumnGCDReduce, cmd_sparse_gcdColumnReduce, 
--          TY_INT, TY_INT, TY_INT);

-------------------------------------------
-- RingMap --------------------------------
-------------------------------------------
document { "ggringmap(Matrix) : RingMap",

     "Given a one row matrix m over a ring R, ggringmap(m) constructs a ring
     map to R.  No source is given, the same RingMap could in principle be
     used to map different rings to R.  The i-th variable of a ring S is
     mapped to the i-th element of m, if i is in range, and to zero, if i is
     >= rank source m."

     }

document { "ggev(RingMap,RingElement) : RingElement",
     }

document { "ggev(RingMap,FreeModule,Vector) : Vector",
     }

document { "ggev(RingMap,FreeModule,Matrix) : Matrix",
     }

document { "ggpromote(FreeModule,Matrix) : Matrix",
     }

document { "ggpromote(FreeModule,Vector) : Vector",
     }

document { "ggpromote(Ring,RingElement) : RingElement",
     }

document { "gglift(FreeModule,Matrix) : Matrix",
     }

document { "gglift(FreeModule,Vector) : Vector",
     "not implemented yet"
     }

document { "gglift(Ring,RingElement) : RingElement",
     "not implemented yet"
     }

-------------------------------------------
-- Computations ---------------------------
-------------------------------------------
document { "ggtracing(ZZ) : ZZ",
     }

--------------------
-- GBComputation ---
--------------------
document { "gggb(Matrix,Matrix,Matrix) : GBComputation",
     }

document { "gggb(Matrix,Matrix,Matrix,Matrix) : GBComputation",
     }

document { "gggb(Matrix,ZZ,ZZ,RingElement,ZZ) : GBComputation",
     }

document { "ggcalc(GBComputation,IntegerArray,IntegerArray) : ZZ",
     }

document { "ggstats(GBComputation) : ??",
     }

document { "gggetmingens(GBComputation) : Matrix",
     }

document { "gggetgb(GBComputation) : Matrix",
     }

document { "gggetsyz(GBComputation) : Matrix",
     }

document { "gggetchange(GBComputation) : Matrix",
     }

document { "gginitial(GBComputation,ZZ) : Matrix",
     }

document { "ggreduce(GBComputation,Matrix) : Matrix",
     }

document { "ggreduce(GBComputation,Vector) : Vector",
     }

document { "ggissubset(Matrix,GBComputation) : ??",
     }

document { "ggisequal(GBComputation,GBComputation) : Boolean",
     }

document { "gglength(GBComputation) : ZZ",
     "Yields the number of Groebner basis elements."
     }

--------------------------------
-- HilbertFunctionComputation --
--------------------------------
document { "gghilb(Ring,Matrix) : HilbertFunctionComputation",
     }

document { "gghilb(Ring,MonomialIdeal) : HilbertFunctionComputation",
     }

document { "ggcalc(HilbertFunctionComputation,ZZ) : ZZ",
     }

document { "ggstats(HilbertFunctionComputation) : ??",
     }

document { "gggetvalue(HilbertFunctionComputation) : RingElement",
     }

-- gglength is not interesting information.
---------------------------
-- ResolutionComputation --
---------------------------
-- resolutions are in files: gb2.cpp, res_aux.cpp res_aux2.cpp
-- There are 3-4 different algorithms used here.
-- Internally, there are three kinds of ResolutionComputation.

document { "ggres(Matrix,ZZ,ZZ,IntegerArray,ZZ) : ResolutionComputation",
     }

document { "ggcalc(ResolutionComputation,IntegerArray,IntegerArray) : ZZ",
     }

document { "ggstats(ResolutionComputation) : ??",
     }

document { "ggpairs(ResolutionComputation) : IntegerArray",
     }

document { "ggremaining(ResolutionComputation) : IntegerArray",
     }

document { "ggbetti(ResolutionComputation) : IntegerArray",
     }

document { "ggnmonoms(ResolutionComputation) : IntegerArray",
     }

document { "ggresmap(ResolutionComputation,ZZ) : Matrix",
     }

document { "ggresNmap(ResolutionComputation,ZZ) : Matrix",
     }

document { "ggresmodule(ResolutionComputation,ZZ) : FreeModule",
     }

document { "ggresNmodule(ResolutionComputation,ZZ) : FreeModule",
     }

-- gglength is not interesting information.


--  install(gggetmingens, cmd_gbres_mingens, TY_GBRES_COMP, TY_INT);
--  install(gggetgb, cmd_gbres_getgb, TY_GBRES_COMP, TY_INT);
--  install(gggetchange, cmd_gbres_change, TY_GBRES_COMP, TY_INT);
--  install(gginitial, cmd_gbres_initial, TY_GBRES_COMP, TY_INT, TY_INT);
--  install(ggskeleton, cmd_res_skeleton, TY_RES_COMP, TY_INT);
--  install(ggskeleton, cmd_res2_skeleton, TY_RES2_COMP, TY_INT);
----------------------------------------
-- Factorization, Characteristic sets --
----------------------------------------

document { "ggfactor(RingElement) : ??",
     "factor a polynomial"
     }

document { "ggfactor1(RingElement,RingElement) : RingElement",
     "compute the gcd?"
     }

document { "ggfactor2(RingElement,RingElement) : RingElement",
     "compute the pseudo-remainder"
     }

document { "ggfactor1(Matrix) : ??",
     "reorder variables?"
     }

document { "ggfactor2(Matrix) : Matrix",
     "characteristic set"
     }

-------------------------------
-- Kernels of Groebner bases --
-------------------------------

document { "ggker(Matrix) : GBKernelComputation",
     }

document { "ggcalc(GBKernelComputation) : ZZ",
     }

document { "gggetsyz(GBKernelComputation) : Matrix",
     }
-------------------------------
-- Determinants, Pfaffians ----
-------------------------------

document { "ggdets(Matrix,ZZ) : DeterminantComputation",
     }

document { "ggpfaffs(Matrix,ZZ) : DeterminantComputation",
     }

document { "ggcalc(DeterminantComputation, ZZ) : ZZ",
     }

document { 
  "ggcalc(DeterminantComputation,ZZ,ZZ,IntegerArray,IntegerArray) : ZZ",
     }

document { "ggindex(DeterminantComputation,ZZ) : Matrix",
     }
-------------------------------
-- LLL Computations -----------
-------------------------------

document { "ggLLLinit(Matrix,RingElement,ZZ) : {MutableMatrix,MutableMatrix}",
     }

document { "ggLLLinit(MutableMatrix,RingElement) : MutableMatrix",
     }

document { "ggLLLcalc(MutableMatrix,MutableMatrix,ZZ) : ZZ",
     }

-------------------------------------------
-- Not completely function computations ---
-------------------------------------------

---------------------------------------
-- Groebner bases of binomial ideals --
---------------------------------------

  -- binomial GB's
///
  
  install(ggbinomialGB, cmd_binomialGB_make, TY_RING, TY_INTARRAY, TY_INT, TY_INT);
  install(ggbinomialGBaddgens, cmd_binomialGB_addgens, TY_GB_COMP, TY_MATRIX);
  install(ggbinomialGBenlarge, cmd_binomialGB_enlarge, TY_GB_COMP, TY_RING, TY_INTARRAY);
  install(gggetsubring, cmd_binomialGB_subring, TY_GB_COMP);
  install(gggetsubringGB, cmd_binomialGB_subringGB, TY_GB_COMP);
///
---------------------------------------
-- Sagbi bases ------------------------
---------------------------------------
///
  install(ggsubduction, cmd_sagbi_subduction, TY_MATRIX, TY_RING_MAP, TY_GB_COMP);
  install(ggsagbi, cmd_sagbi_make, TY_MATRIX);
///
---------------------------------------
-- Smith normal form ------------------
---------------------------------------
///
  install(ggSmithNormalForm, cmd_smith_make, TY_MATRIX, TY_INT, TY_INT);
  install(ggcalc, cmd_smith_calc, TY_MatrixComputation, TY_INT);
  install(gggetRowChange, cmd_smith_rowchange, TY_MatrixComputation);
  install(gggetColChange, cmd_smith_colchange, TY_MatrixComputation);
  install(gggetgb, cmd_smith_matrix, TY_MatrixComputation);
  install(ggstatus, cmd_smith_status, TY_MatrixComputation);
///
      
-------------------------------------------
-- System commands ------------------------
-------------------------------------------
--  install(ggquit, cmdQuit);

document { "ggmem() : String",
     }

document { "ggstack() : String",
     }

document { "ggheap() : String",
     }

document { "ggsee(Object) : String",
     "But leaves the object on the stack!!"
     }

----------------------
-- Stack operations --
----------------------
document { "ggdup(Object) : {Object,Object}",
     }

document { "ggduplicate(??,ZZ) : ??",
     }

document { "ggpick(...,ZZ) : ...",
     }

document { "ggpop(Object) : Void",
     }

document { "ggpoppem(ZZ) : Void",
     }
----------------------
-- Heap operations ---
----------------------

document { "ggaddress(Object) : ZZ",
     }

document { "ggforget(ZZ) : Void",
     }

document { "ggderef(ZZ) : Object",
     }

----------------------
-- Hash values -------
----------------------
document { "ggisequal0(Object,Object) : Boolean",
     }

document { "gghash(Object) : ZZ",
     }

----------------------------------------
-- Bringing information to the engine --
----------------------------------------

document { "ggINT",
     }

document { "ggSTRING",
     }

document { "ggINTARRAY",
     }


///
-- gggetideal
A = ZZ/101[a..d]
B = A/(a^2,c^2)
C = B/(a*c-d^2)
use A
assert(presentation C == matrix{{d^4, c*d^2, a*d^2, c^2, a*c-d^2, a^2}})


-- ggisequal(FreeModule,FreeModule)
A = ZZ/101[symbol a..symbol d]
B = ZZ/101[symbol a..symbol f]
A^3 == B^3

---------------------------------------------
-- A basic test of the routines in the engine

-- 
R = ZZ/101[a..d]
m = matrix{{a,b}}
assert isHomogeneous m
m * transpose m
(transpose m) * m

m = matrix{{a*b-c^2, a*c}}
gens gb m

R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = flatten(m1*m2-m2*m1)
time res coker J
see R


J = Grassmannian(2,5) -- FAILS

///

TEST ///
-- testing fraction field routines
R = ZZ/101[t]
K = frac R
A = K[x,y]
I = ideal(t*x^2-y, (t+1)*x*y-y^2)
gbTrace 3
gens gb I
ideal oo

R = ZZ/101[a,b,c,d]/(a*d-b*c)
K = frac R
a/b

-- Try working over a number field
K = QQ[a]/(a^2-5)
sendgg(ggPush K, ggdeclarefield);
R = K[x,y,z]
S = K[s,t]
I = matrix{{s^3 - a*s*t^2, s^2*t - a*s*t^2, t^3}}
F = map(S,R,I)
F(ker F)


///