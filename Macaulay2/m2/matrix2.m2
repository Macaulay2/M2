--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

complement = method()

mingens Module := (M,options) -> if M.?mingens then M.mingens else M.mingens = (
     if M.?generators then (
	  if M.?relations then (
	       c := mingens gb (M.generators|M.relations,
		    options,
		    Minimal=>true,Syzygies=>false,ChangeMatrix=>false);
	       c * complement(M.relations // c))
	  else mingens gb (M.generators, 
	       options,
	       Minimal=>true,Syzygies=>false,ChangeMatrix=>false)
	  )
     else (
	  if M.?relations then complement M.relations
	  else id_M
	  )
     )
document { quote mingens,
     TT "mingens M", " -- returns a minimal generating set for the module ", TT "M", ",
     represented as a matrix whose target is the ambient free module of ", TT "M", ".",
     PARA,
     SEEALSO "GroebnerBasis"
     }

TEST "
R = ZZ/101[a..d]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( mingens M == matrix (R, {{1},{0}}))
"

trim Ring := (R,options) -> R
trim QuotientRing := (R,options) -> (
     f := presentation R;
     A := ring f;
     A/(trim(ideal f,options)))
trim Module := (M,options) -> if M.?trim then M.trim else M.trim = (
     if isFreeModule M then M
     else if isHomogeneous M then (
	  g := mingens(M,options);
	  relns := if M.?relations then mingens(image M.relations,options);
	  N := (
	       if not isSubset(target g, image g)
	       then subquotient( g, relns )
	       else if relns === null then ambient M
	       else cokernel relns
	       );
	  N.trim = N;
	  N)
     else M
     )
document { quote trim,
     TT "trim M", " -- produce a module isomorphic to the module M obtained
     by replacing its generators by a minimal set of generators, and doing
     the same for the relations.",
     PARA,
     "Also works for rings and ideals.",
     PARA,
     EXAMPLE "R = ZZ/101[x]",
     EXAMPLE "M = subquotient( matrix {{x,x^2,x^3}}, matrix {{x^3,x^4,x^5}})",
     EXAMPLE "trim M"
     }

TEST "
R = ZZ/101[a..d]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( generators trim M == matrix (R, {{1},{0}}))
"

syz Matrix := (f,options) -> (
     if not isFreeModule target f or not isFreeModule source f
     then error "expected map between free modules";
     if ring f === ZZ or not isHomogeneous f
     then syz gb (f, options, Syzygies=>true)
     else mingens image syz gb (f, options, Syzygies=>true)
     )

document { quote syz,
     TT "syz f", " -- compute minimal generators for the module of syzygies for the 
     ", TO "Matrix", " f.",
     PARA,
     "syz G -- retrieve the ", TO "Matrix", " of syzygies from the Groebner
     basis G.  The result may be empty if syzygies were not to be retained during the
     calculation, or if the computation was not conitnued to a high enough degree.",
     PARA,
     "This function takes the same optional arguments as ", TT "gb", ".",
     PARA,
     SEEALSO "GroebnerBasis"
     }

modulo = method()
modulo(Matrix,Nothing) := (m,null) -> syz m
modulo(Nothing,Matrix) := (null,n) -> n
modulo(Matrix,Matrix) := (m,n) -> (
     P := target m;
     Q := target n;
     if P != Q then error "expected maps with the same target";
     if not isFreeModule P or not isFreeModule Q
     or not isFreeModule source m or not isFreeModule source n
     then error "expected maps between free modules";
     syz(m|n, SyzygyRows => numgens source m)
     )
document { quote modulo,
     "modulo(f,g) - given homomorphisms f and g of free modules with the same target,
     produces a homomorphism of free modules whose target is the source of f, and
     whose image is the pre-image (under f) of the image of g.",
     PARA,
     "If f is null, then it's taken to be the identity.  If g is null, it's
     taken to be zero."
     }

Matrix // Matrix := (f,g) -> (
     -- if ring g =!= ring f then error "expected maps over the same ring";
     M := target f;
     if M != target g then error "expected maps with the same target";
     L := source f;
     N := source g;
     f = matrix f;
     g = matrix g;
     map(N, L, f //
     	  if M.?relations 
     	  then gb(g | presentation M, 
	       ChangeMatrix => true, SyzygyRows => rank source g)
     	  else gb(g,
	       ChangeMatrix => true)))

TEST "
R = ZZ/101[a..d]
A = image matrix {{a}}
B = image matrix {{b}}
f = map((A+B)/A, B/intersect(A,B))
assert isIsomorphism f
g = f^-1
assert( f^-1 === g )			  -- check caching of inverses
assert( f*g == 1 )
assert( g*f == 1 )
assert isWellDefined f
assert isWellDefined g
assert not isWellDefined map(R^1,cokernel matrix {{a}})
"

RingElement // Matrix := (r,f) -> (r * id_(target f)) // f
ZZ           // Matrix := (r,f) -> promote(r,ring f) // f

Matrix // RingElement := (f,r) -> f // (r * id_(target f))
Matrix // ZZ           := (f,r) -> f // promote(r,ring f)

Matrix % Matrix := {
     Matrix,
     (n,m) -> (
	  R := ring n;
	  if R =!= ring m then error "expected matrices over the same ring";
	  if not isFreeModule source n or not isFreeModule source m
	  or not isFreeModule target n or not isFreeModule target m
	  then error "expected maps between free modules";
	  n % gb m),
     TT "f % g", " -- yields the reduction of the columns of the matrix
     ", TT "f", " modulo a Groebner basis of the matrix ", TT "g", "."
     }

Matrix % Module := (f,M) -> f % gb M

RingElement % Matrix := (r,f) -> ((r * id_(target f)) % f)_(0,0)
RingElement % Ideal := (r,I) -> r % gb I

Matrix % RingElement := {
     Matrix,
     (f,r) -> f % (r * id_(target f)),
     TT "f % r", " -- yields the reduction of the columns of the matrix
     ", TT "f", " modulo the ring element ", TT "r", "."
     }

complement Matrix := (m) -> (
     if not isHomogeneous m then error "expected homogeneous matrix";
     n := transpose syz transpose substitute(m,0);
     id_(target n) // n)
document { quote complement,
     TT "complement f", " -- for a matrix f, return a map g with the same 
     target whose columns are minimal generators for the cokernel of f.",
     PARA,
     "The map f must be homogeneous."
     }

-----------------------------------------------------------------------------

TEST "
S = ZZ/107[vars ( 0 .. 5 ) ]

g = matrix {{a*b*c - d*e*f, a*d^2 - e^3, a*e^2 - b*c*e}}
k = syz g
assert( numgens source k === 4 )

t = (a + b + c)^4 
u = (a + b + c) * b^3
v = a * t + b * u
w = c * t - d * u
x = b * t + f * u

h = matrix {{t,u,v,w,x}}
h1 = mingens image h

assert ( h1 == matrix {{
	       a*b^3+b^4+b^3*c,
	       a^4+4*a^3*b+6*a^2*b^2-3*b^4+4*a^3*c+12*a^2*b*c+12*a*b^2*c+6*a^2*c^2
	       +12*a*b*c^2+6*b^2*c^2+4*a*c^3+4*b*c^3+c^4
	       }} )
"

-------------------------------------
-- index number of a ring variable --
-------------------------------------
index = method()

index RingElement := f -> (
    v := try (baseName f) else error("expected a ring variable but received ",name f);
    (monoid ring f).index#v)

document { quote index,
    TT "index v", " -- yields the numeric index of the variable 'v' in its ring.
    Variables are indexed starting at 0, and ending at n-1, where n is the number
    of variables in the ring of 'v'.",
    PARA,
    EXAMPLE "R = ZZ/101[a..d,t]",
    EXAMPLE "index a",
    EXAMPLE "index t",
    "If the ring element 'v' is not a variable, an error is generated.",
    PARA,
    "The symbol ", TT "index", " is also as a key used in 
    ", TO "GeneralOrderedMonoid", "s to store a table which is used to 
    map generator names to the position of the generator in the list of generators."
    }

TEST "
    R = ZZ/101[x_0 .. x_10]
    scan(11, i -> assert(index x_i == i))
    assert( try (index x_11;false) else true )
    R = ZZ/101[w,z,t,e]
    assert( index w == 0 )
    assert( index z == 1 )
    assert( index t == 2 )
    assert( index e == 3 )
"

--------------------
-- homogenization --
--------------------
homogenize = method()

listZ := v -> (
     if not all(v,i -> class i === ZZ) then error "expected list of integers";
     )

homogCheck := (f, v, wts) -> (
    if ring f =!= ring v then error "homogenization requires variable in the same ring";
    listZ wts;
    if # wts != numgens ring f 
       then error "homogenization weight vector has incorrect length";)

homogenize(RingElement, RingElement, List) := (f,v,wts) -> (
    wts = flatten wts;
    homogCheck(f,v,wts);
    sendgg(ggPush f, ggPush index v, ggPush wts, gghomogenize);
    new ring f)

homogenize(Vector, RingElement, List) := (f,v,wts) -> (
    wts = flatten wts;
    homogCheck(f,v,wts);
    sendgg(ggPush f, ggPush index v, ggPush wts, gghomogenize);
    new class f)

homogenize(Matrix, RingElement, List) := (f,v,wts) -> (
    wts = flatten wts;
    homogCheck(f,v,wts);
    sendgg(ggPush f, ggPush index v, ggPush wts, gghomogenize);
    getMatrix ring f)

homogenize(Matrix, RingElement) := (f,n) -> (
    wts := (transpose (monoid ring f).Options.Degrees)#0;
    homogenize(f,n,wts)
    )

homogenize(Module,RingElement) := (M,z) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(M.generators,z),
	  if M.?relations then homogenize(M.relations,z)))

homogenize(Ideal,RingElement) := (I,z) -> ideal homogenize(module I, z)

homogenize(Module,RingElement,List) := (M,z,wts) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(M.generators,z,wts),
	  if M.?relations then homogenize(M.relations,z,wts)))

homogenize(RingElement, RingElement) := (f,n) -> (
    wts := (transpose (monoid ring f).Options.Degrees)#0;
    homogenize(f,n,wts)
    )

homogenize(Vector, RingElement) := (f,n) -> (
    wts := (transpose (monoid ring f).Options.Degrees)#0;
    homogenize(f,n,wts)
    )

document { quote homogenize,
     TT "homogenize(m,v)", " -- homogenize the ring element, vector,
     matrix, or module m using the variable v in the ring of m.",
     PARA,
     TT "homogenize(m,v,w)", " -- homogenize m using the variable v,
     so that the result is homogeneous with respect to the given list w of
     integers provided as weights for the variables.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z,Degrees => {1,2,3}]",
     EXAMPLE "f = 1 + y + z^2",
     EXAMPLE "homogenize(f,x)",
     EXAMPLE "homogenize(f,x,{1,0,-1})",
     PARA,
     "The weights that may be used are limited (roughly) to the range -2^30 .. 2^30.",
     PARA,
     "Caveats and bugs: If the homogenization overflows the monomial, this is not
     reported as an error"
     }

TEST "
R = ZZ/101[a..d,t]
f = a^2-d^3*b-1
assert(homogenize(f,t) == a^2*t^2 - d^3*b - t^4)
assert(homogenize(f,t,{1,2,3,4,2}) == a^2*t^6 - d^3*b - t^7)
assert(homogenize(f,b,{1,1,0,-1,1}) == a^2 - d^3*b^5 - b^2)

m = map(R^{1,-1}, , {{a,b},{c,d-1}})
assert(homogenize(m,t) == map(R^{1,-1}, , {{a*t^2, b*t^2}, {c, d-t}}))
assert(homogenize(m,t,{-1,-1,-1,-1,1}) == map(R^{1,-1}, , {{a*t^2, b*t^3}, {c, d*t-1}}))

v = m_0
F = class v
assert(homogenize(v,t) == a*t^2 * F_0 + c * F_1)
assert(homogenize(v,t,{-1,-1,-1,-1,1}) == a*t^2 * F_0 + c * F_1)

-- now check to make sure that all is ok over quotient rings
R = ZZ/101[a..d]/(a^2-b^2, a*b)
use R
f = c^2 - 1 + b^2 - b
assert(homogenize(f,a) == c^2)
"

coefficients(List, Matrix) := (v,m) -> (
    sendgg(ggPush m, ggPush v, ggcoeffs); 
    m1 := getMatrix ring m; 
    {m1, getMatrix ring m})

coefficients(List, RingElement) := (v,m) -> (
     f := matrix{{m}};
     sendgg(ggPush f, ggPush v, ggcoeffs); 
     m1 := getMatrix ring m; 
     {m1, getMatrix ring m})

coefficients(Matrix) := 
coefficients(RingElement) := (m) -> (
     R := ring m;
     n := numgens R;
     coefficients(splice {0 .. n-1}, m))

-----------------------------
-- Matrix utility routines --
-----------------------------

selectInSubring = method()

selectInSubring(ZZ, Matrix) := (i,m) -> (
     sendgg(ggPush m, ggdup, ggPush i, ggelim, ggsubmatrix);
     getMatrix ring m)

document { quote selectInSubring,
     TT "selectInSubring(i,m)", " -- Form the submatrix of the matrix 'm' consisting of those
     columns which lie in the subring generated by the first 'i' parts of the
     monomial order.",
     PARA,
     "For example, consider the graded lexicographic order",
     EXAMPLE "R = ZZ/101[a..d,MonomialOrder=>Lex]",
     EXAMPLE "m = matrix{{b^2-c^2, a^2 - b^2, c*d}}",
     EXAMPLE "selectInSubring(1,m)",
     EXAMPLE "selectInSubring(2,m)",
     EXAMPLE "selectInSubring(3,m)",
     PARA,
     "Caveats: this routine doesn't do what one would expect for graded orders
     such as 'GLex'.  There, the first part of the monomial order is the degree, 
     which is usually not zero.  This routine should detect and correct this."
     }

divideByVariable = method()

divideByVariable(Matrix, RingElement) := (m,v) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     sendgg(ggPush m, ggPush index v, ggPush (-1), ggsat);
     getMatrix ring m)

divideByVariable(Matrix, RingElement, ZZ) := (m,v,d) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     sendgg(ggPush m, ggPush index v, ggPush d, ggsat);
     getMatrix ring m)

document { quote divideByVariable,
     TT "divideByVariable(m,v)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible.",
     BR,NOINDENT,
     TT "divideByVariable(m,v,d)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible, but divide by no more than v^d.",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "m = matrix{{a*b, a^2*c}, {a*b^2, a^4*d}}",
     EXAMPLE "divideByVariable(m,a)",
     EXAMPLE "divideByVariable(m,a,1)",
     "Caveats and limitations: you can only divide by a variable, not a monomial,
     and you have little control on what power will be divided.  This routine is mostly
     used by the saturation commands as a fast internal way of dividing.",
     PARA,
     "We may eliminate this routine."
     }

compress = method()
--compress Matrix := (m) -> (
--     R := ring m;
--     sendgg( ggPush m, ggcompress );
--     getMatrix R)
compress Matrix := (m) -> (
     R := ring m;
     submatrix(m, select(elements(0..numgens source m-1), i -> m_i != 0)))

document { quote compress,
     TT "compress m", " -- provides the matrix obtained from the matrix ", TT "m", "
     by removing the columns which are zero."
     }

newCoordinateSystem = method()

newCoordinateSystem(PolynomialRing, Matrix) := (S,x) -> (
  -- x should be a one row matrix of linear forms
  -- S should be a ring, with the same number of variables as ring x.
  -- MES will document this and maybe change its name
  R := ring x;
  if numgens R != numgens S 
  then error "newCoordinateSystem requires input rings to have the same number of variables";
     -- probably should also check:
     -- (a) entries of 'x' are linear and independent
     -- (b) what if R,S, are quotient rings
  m := contract(transpose vars R, x);
  n := complement m | m;
  { map(S,R,vars S * substitute(n, S)), map(R,S,vars R * n^(-1))}
  )

document { quote newCoordinateSystem,
     TT "newCoordinateSystem(S,m)", " -- takes a one-rowed matrix ", TT "m", " of
     independent linear forms over a ring ", TT "R", " and returns a list 
     ", TT "{f,g}", ", where ", TT "f", " is a ring map given by some linear change 
     of coordinates from ", TT "R", " to ", TT "S", " which sends the last variables 
     of ", TT"R", " to the forms in ", TT "m", ", and ", TT "g", " is the inverse 
     of ", TT "f", ".",
     PARA,
     "The ring ", TT "S", " should have the same number of variables as 
     ", TT "S", ".",
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "S = ZZ/101[p..s]",
     EXAMPLE "newCoordinateSystem(S,matrix{{a+2*b,3*c-d}})"
     }

lift(Matrix,Ring) := (f,S) -> (
     -- this will be pretty slow and stupid
     if ring target f === S then f
     else if isQuotientOf(ring f,S) and
	     isFreeModule source f and
	     isFreeModule target f then
	 map(S^(-degrees target f), S^(-degrees source f), 
	     applyTable(entries f, r -> lift(r,S)))
     else matrix(S, applyTable(entries f, r -> lift(r,S)))
     )

lift(Ideal,Ring) := (I,S) -> (
     -- provisional, just for quotient rings
     T := ring I;
     if T === S then I
     else ideal lift(I.generators,S) + ideal presentation(S,T));

promote(Matrix,Ring) := (f,S) -> (
     error "this use of 'promote' has been replaced by '**'";
     );

promote(Ideal,Ring) := (I,S) -> (
     error "this use of 'promote' has been replaced by '*'";
     );
