-- Copyright 1998 by Michael E. Stillman

-- Engine documentation

document { quote ggadd,
     TT "ggadd(f:ERingElement,g:ERingElement):ERingElement", "-- yields the ring element sum f+g.",BR,NOINDENT,
     TT "ggadd(f:EVector,g:EVector):EVector", "               -- yields the vector sum f+g.",BR,NOINDENT,
     TT "ggadd(f:EMatrix,g:EMatrix):EMatrix", "               -- yields the matrix sum f+g.",BR,NOINDENT,
     TT "ggadd(F:EFreeModule,G:EFreeModule):EFreeModule", "   -- yields the direct sum F ++ G.",
     PARA,
     "In each case, the base ring of the two arguments must be the same.  
      For vector addition, the two vectors must be in free modules with 
      the same rank.  The result is a vector in the free module of ", TT "f", ".
      In the matrix version, both arguments must have the same number of rows. 
      The resulting matrix has the same target as ", TT "f", ". For free 
      modules, if either ", TO "F", " or ", TO "G", "has an induced monomial 
      order, then the result will as well.",
     PARA,
      "Known problems, restrictions or caveats: The direct sum of free modules 
       should have a different name, e.g. ggdirectsum.",
     SEEALSO "EVector", 
     SEEALSO "EMatrix",
     SEEALSO "EFreeModule"
     }

document { quote ggsubtract,
TT "ggsubtract(f:ERingElement,g:ERingElement):ERingElement", "-- yields the difference f-g.",BR,NOINDENT,
TT "ggsubtract(f:EVector,g:EVector):EVector", "               -- yields the vector difference f-g.",BR,NOINDENT,
TT "ggsubtract(f:EMatrix,g:EMatrix):EMatrix", "               -- yields the matrix difference f-g.",
     PARA,
     "In each case, the base ring of the two arguments must be the same.  
      For vector subtraction, the two vectors must be in free modules with 
      the same rank.  The result is a vector in the free module of ", TT "f", ".
      In the matrix version, both arguments must have the same number of rows. 
      The resulting matrix has the same target as ", TT "f", ". ",
     PARA,
      "Known problems, restrictions or caveats: None.",
     SEEALSO "EVector", 
     SEEALSO "EMatrix",
     SEEALSO "EFreeModule"
     }

document { quote ggiszero,
     TT "ggiszero(f:ERingElement):Boolean", " -- is ", TO "f", " zero?", BR,NOINDENT,
     TT "ggiszero(f:EVector):Boolean", "      -- is ", TO "f", " zero?", BR,NOINDENT,
     TT "ggiszero(f:EMatrix):Boolean", "      -- is ", TO "f", " zero?",
     PARA,
     "In each case, true is returned exactly when ", TO "f", " is zero.",
     PARA,
      "Known problems, restrictions or caveats: None.",
     }

document { quote ggisequal,
     TT "ggisequal(f:ERingElement,g:ERingElement):Boolean", " -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EVector,g:EVector):Boolean", "           -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EFreeModule,g:EFreeModule):Boolean", "   -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EMatrix,g:EVector):Boolean", "           -- is ", TT "f", " = ", TT "g", "?",
     PARA,
     "In each case, true is returned exactly when ", TT "f", " and ", TT "g", " are equal.
      Equality is a very strong notion: for vectors, it means that their respective free 
      modules are equal.  Two matrices are equal only if all entries are the same, 
      and the target and source of each is the same.  For free modules, both arguments 
      must have the same rank, and the degree of each basis element must be the same, 
      and also the monomial ordering must be the same (see ", TO "???", ").",
     PARA,
      "Known problems, restrictions or caveats: Equality checks using ", TO "ggiszero", "
       are often less strict.  For example, if ", TT "m", " and ", TT "n", " are two
       matrices, then it is possible for ", TT "ggiszero(ggsubtract(m,n))", " to yield true,
       but to have ", TT "ggisequal(m,n)", " return false."
     }

document { quote EMonomialOrder,
     TT "EMonomialOrder", " -- An engine type describing a monomial order",
     PARA,
     "This is a hard one to document: Include how to create a monomial order"
     }

document { quote ggEZZ,
     TT "ggEZZ():ERing", " -- yields the ring ZZ of arbitrary precision integers",
     PARA,
      "Known problems, restrictions or caveats: For a VERY SHORT TIME, 
       this ring cannot yet handle arbitrary precision integers."
     }

document { quote ggEcharp,
     TT "ggEZZ(p:int):ERing", " -- yields the quotient ring ZZ/p",
     PARA,
      "Known problems, restrictions or caveats: Currently, the integer ", TT "p", "
       must be a small (< 2^16-1) positive integer.  This restriction 
       will be removed soon."
     }
     
document { quote ggpolyring,
     TT "ggpolyring(K:ERing, M:EMonoid, ZD:ERing, d:IntegerList):ERing", " -- create a polynomial ring",
     PARA,
     "Create the polynomial ring,", TT "R = K[M]", " with the grading 
      given by the last two arguments.  If the monoid is non-commutative, then
      the result is a non-commutative polynomial ring.",
     PARA,
     "The grading is given by a commutative group monoid, ", TT "D", " which is 
      the monoid of the polynomial ring ", TT "ZD", ".  The degree of each
      variable of ", TT "R", " is a list of integers of length the number of
      variables of ", TT "ZD", ".  The argument ", TT "d", " should be the
      integer list obtained by flattening the degrees of the variables.",
     PARA,
     "For example, if the ring has two variables x and y, and ZD has three
      variables, and if degree(x) = {-1,0,1}, degree(y) = {0,0,1}, then
      d should be the list {-1,0,1, 0,0,1}.",
     PARA,
      "If no grading is desired, use ggEZZ() for the degree ring, and {} for d,
       as in ", TT "ggpolyring(K,M,ZZZ,{})", ".",
     PARA,
      "Known problems, restrictions or caveats: Currently, the coefficient ring K
      must be ZZ or ZZ/p.  In particular, polynomial rings over polynomial rings
      are not yet allowed.  This restriction will be gone soon."
      }
 
document { quote ggweylalg,
     TT "ggweylalg(K:ERing, M:Monoid, comm:IntegerList, deriv:IntegerList, homog:ZZ, ",BR,NOINDENT,
     TT "    ZD:ERing, d:IntegerList):ERing)", " -- Create a Weyl algebra",
     PARA,
      "Known problems, restrictions or caveats: Currently, the coefficient ring K
      must be ZZ or ZZ/p.  In particular, polynomial rings over polynomial rings
      are not yet allowed.  This restriction will be gone soon."
     } 

document { "Engine FreeModule API",
     "One of the basic engine data types is", TT "EFreeModule", " .  Each such free module
     is defined over a ring, and is graded according to the (multi)grading of this ring.
     Additionally, the monomial order may be an induced (Schreyer) order.",
     "The following functions are the operations on, or involving free modules.",
     MENU {
	(TT "ggfree(R:ERing,n:int):EFreeModule", " -- yields R^n"),
	(TT "ggfree(R:ERing,n:intarray):EFreeModule", " -- yields R^n, a graded free module"),
	(TT "ggfree(m:EMatrix):EFreeModule", " -- yields a free module with a Schreyer order"),
	(TT "ggrank(F:EFreeModule):int", " -- the number of generators of F."),
	(TT "ggdegree(F:EFreeModule):list", " -- yields a list of degrees of its generators."),
	(TT "gggetcols(F:EFreeModule):EMatrix", " -- yields the matrix encoding the monomial order. RENAME IT"),
	(TT "ggisequal(F,G:EFreeModule):bool", " -- returns true if F and G are equal"),
	(TT "ggadd(F,G:EFreeModule):EFreeModule", " -- the direct sum F ++ G."),
	(TT "ggmult(F,G:EFreeModule):EFreeModule", " -- the tensor product F ** G."),
	(TT "ggtranspose(F:EFreeModule):EFreeModule", " -- the dual of F."),
	(TT "ggsubmodule(F:EFreeModule,a:intarray):EFreeModule", " -- F_a."),
	(TT "ggsymm(F:EFreeModule,n:int):EFreeModule", " -- The symmetric power S^n(F).  NOT DONE YET"),
	(TT "ggexterior(F:EFreeModule,n:int):EFreeModule", " -- The nth exterior power of F."),
	(TT "ggshift(F:EFreeModule,d:intarray):EFreeModule", " -- return F(d).")
	}
   }

document { "Engine RingElement API",
     "A basic data type in the engine.  Each ring element is a member of an engine ring (ERing),
     and each ring element has a hash value, which is unique for all ring elements.",
     "The following functions are the engine operations on ring elements",
     MENU {
	(TT "ggfromint(R:ERing,n:int):ERingElement", " -- return n_R."),
	(TT "ggvar(v:int,d:int,R:ERing):ERingElement", " -- return (R_v)^d."),
	(TT "ggterm(R:ERing,a:ERingElement,m:intarray):ERingElement", " -- return a*m, where 
	     a is in the coefficient ring of R, and m is an intarray of (var,exponent)'s."),
	(TT "ggisequal(a,b:ERingElement):bool", " -- is a == b?"),
	(TT "ggisequal(a:ERingElement,n:int):bool", " -- is a == n?"),
	(TT "ggiszero(a:ERingElement):bool", " -- is a == 0?"),
	(TT "ggnegate(a:ERingElement):ERingElement", " -- return -a."),
	(TT "ggadd(a,b:ERingElement):ERingElement", " -- return a+b, (a,b in same ring)."),
	(TT "ggsubtract(a,b:ERingElement):ERingElement", " -- return a-b, (a,b in same ring)."),
	(TT "ggmult(n:int,b:ERingElement):ERingElement", " -- return n*b."),
	(TT "ggmult(a,b:ERingElement):ERingElement", " -- return a*b, (a,b in same ring)."),
	(TT "ggpower(a:ERingElement,n:int):ERingElement", " -- return a^n."),
	(TT "ggleadcoeff(a:ERingElement):ERingElement", " -- return the lead coefficient of a (in
	     the coefficient ring of (ring a)."),
	(TT "ggleadterm(a:ERingElement):ERingElement", " -- return lead term of a, in same ring"),
	(TT "ggleadmonom(a:ERingElement):intarray", " -- return a list of (variable,exponent)'s."),
	(TT "ggdegree(a:ERingElement):intarray", " -- return the (multi-degree) of a."),
	(TT "ggdegree(a:ERingElement,wts:intarray):(int,int)", " -- return (min,max) values of 'wts'
	     applied to the terms of a."),
	(TT "ggishomogeneous(a:ERingElement):bool", " -- is a homogeneous?"),
	(TT "gghomogenize(a:ERingElement,wts:intarray,var:int):ERingElement", " -- homogenize 'a'. NOT DONE"),
	(TT "gggetterms(a:ERingElement,lo,hi:int):ERingElement", " -- return a subset of terms of a.")
	     }
	
	}
   
	     
	     
	
TEST ///
-------------------------------------
-- ZZZ: ring,vector,matrix operations
-------------------------------------
R = ZZZ
-- ring element operations
assert(31_R == 31)
assert((-31)_R == - (31_R))
assert(5_R - 5_R == 0)
assert(size (1_R) == 1)
assert(size (0_R) == 0)
assert(3 * 2_R == 6_R)
assert(3_R * 2_R == 6_R)
try assert(0 != (2^40)_R) else "ERROR: 2^40 is still 0"
assert(2^40 == (2^40)_R)
assert((3^8)_R == 3 * (3^7)_R)
assert((1_R - 1_R)^50 == 0)
assert try leadCoefficient (1_R) else true
assert try leadTerm (1_R) else true
assert try leadMonomial (1_R) else true
assert try someTerms(1_R, 1, 1) else true
assert(degree(1_R) == {})
degree(0_R) == -infinity  -- is this really what we want?
try degree(1_R, {}) else "ERROR: ggdegree with weights needs front-end interface"
try random R else error "random not working yet" -- ERROR: routine not implemented
-- vectors
F = R^4
v = 3*F_0 + 2*F_1 + 0*F_2
try assert(false) else "POSSIBLE error: allow front end to make vectors/sparse vectors?"
assert(0_F == 0)
assert(leadComponent v == 1)
assert(v_0 == 3)
assert(v_1 == 2)
assert(v_2 == 0)
assert(v != F_1)
assert(v == F_0 + F_0 + F_0 + F_1 + F_1)
assert(try leadCoefficient v else true)
assert(leadTerm v == 2*F_1)  -- FAILS: wrong engine interface
assert(leadTerm(0_F) == 0) -- FAILS: wrong engine interface
assert(v - v == 0)
assert(v + v + v == 3*v)
assert(v + v + v == v*3) -- FAILS: need front end interface
assert(v + v + v == (3_R)*v)
assert(- (-v) == v)
someTerms(v,1,1) -- FAILS: need front end interface
degree v == {}
degree (0_F) == {} -- Inconsistent with degree of a zero ring element
homogenize(v,1_R) -- error message is funny: "key not found in hash table"
-- matrices
random(R^2, R^3) -- incorrect stack arguments for command ggmatrix
                     -- need to IMPLEMENT kbasis first
m = matrix(R, {{1,2,3,4,5},{12,14,16,18,20}})
m1 = matrix {{1_R, 2_R, 3_R, 4_R, 5_R}, {12, 14, 16, 18, 20}}
assert(entries m == entries m1)
assert(entries transpose m == transpose entries m)
assert(m_{0,1} == matrix(R, {{1,2},{12,14}}))
assert(m_(1,1) == 14)
assert(m + (-m) == 0)
assert(m + m == 2*m)
assert(m + m == m*2)
assert(m + m == (2_R)*m)
assert(m + m == m*(2_R)) -- NOT implemented
assert(leadTerm m) -- Incorrect engine interface for gginitial
p = id_(R^5)
p ++ p == id_(R^10)  -- Engine BUG: ggddirectsum needs to check its types.
assert(entries p == table(5,5,(i,j)->if i===j then 1 else 0))
p = map(R^5,R^7,0)
assert(p == 0)
m = matrix{{1,2,3,4}}
assert(koszul(1,m) == m) -- INCORRECT
assert(koszul(2,m) != 0) -- INCORRECT
m = matrix(ZZ, {{1,10},{20,100}})
mm = m ** R 
assert(mm ** mm == matrix(R,{{1, 10, 10, 100}, {20, 100, 200, 1000}, {20, 200, 100, 1000}, {400, 2000, 2000, 10000}})) -- ggmatrix problem
m1 = transpose matrix(R,{{1,1,1}}) -- segmentation fault (after lots of stuff)
m2 = matrix{{3_R}}
entries relations (coker m1 ** coker m2 ) == {{3, 0, 0, 1}, {0, 3, 0, 1}, {0, 0, 3, 1}}
diff(m2,m1) -- Segmentation fault
contract(m2,m1) -- Segmentation fault

m = matrix(R,{{1,0,-1,3,-4}})
try assert false else error "Descending order seems to not be descending."
entries (m _ (sortColumns m)) == {{-4, -1, 1, 3, 0}}-- 0 is at the end: is this what we want?
-- homogenize, elim, sat, coefficients: polynomial ring only
-- reshape,flip,exteriorProduct
assert(flip(R^2,R^3) == matrix(R,{{1, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1}}))
m = map(R^1, R^6, (i,j) -> 6*i + j)
assert(reshape(R^2,R^3,m) == matrix (R,{{0, 2, 4}, {1, 3, 5}}))
assert(wedgeProduct(2,3,ZZ^5) ** R == wedgeProduct(2,3,R^5))  -- FAILS


-----------------------------------------
-- ZZZ/101: ring,vector,matrix operations
-----------------------------------------
R = ZZZ/101
-- ring element operations
assert(100_R == -1)
assert(101_R == 0)
assert(31_R == 31)
assert((-31)_R == - (31_R))
assert(5_R - 5_R == 0)
assert(size (1_R) == 1)
assert(size (0_R) == 0)
assert(3 * 2_R == 6_R)
assert(3_R * 2_R == 6_R)
assert((2_R)^40 == 36)
try assert((2^40)_R == 36) else error "from_int(ZZ) not fully implemented"
assert((3^8)_R == 3 * (3^7)_R)
assert((1_R - 1_R)^50 == 0)
assert try leadCoefficient (1_R) else true
assert try leadTerm (1_R) else true
assert try leadMonomial (1_R) else true
assert try someTerms(1_R, 1, 1) else true
assert(degree(1_R) == {})
degree(0_R) == -infinity  -- is this really what we want?
try degree(1_R, {}) else "ERROR: ggdegree with weights needs front-end interface"
try random R else error "random not working yet" -- ERROR: routine not implemented
-- vectors
F = R^4
v = 3*F_0 + 2*F_1 + 0*F_2
try assert(false) else "POSSIBLE error: allow front end to make vectors/sparse vectors?"
assert(0_F == 0)
assert(leadComponent v == 1)
assert(v_0 == 3)
assert(v_1 == 2)
assert(v_2 == 0)
assert(v != F_1)
assert(v == F_0 + F_0 + F_0 + F_1 + F_1)
assert(try leadCoefficient v else true)
assert(leadTerm v == 2*F_1)  -- FAILS: wrong engine interface
assert(leadTerm(0_F) == 0) -- FAILS: wrong engine interface
assert(v - v == 0)
assert(v + v + v == 3*v)
assert(v + v + v == v*3) -- FAILS: need front end interface
assert(v + v + v == (3_R)*v)
assert(- (-v) == v)
someTerms(v,1,1) -- FAILS: need front end interface
degree v == {}
degree (0_F) == {} -- Inconsistent with degree of a zero ring element
homogenize(v,1_R) -- error message is funny: "key not found in hash table"
-- matrices
random(R^2, R^3) -- incorrect stack arguments for command ggmatrix
                     -- need to IMPLEMENT kbasis first
m = matrix(R, {{1,2,3,4,5},{12,14,16,18,20}})
m1 = matrix {{1_R, 2_R, 3_R, 4_R, 5_R}, {12, 14, 16, 18, 20}}
assert(entries m == entries m1)
assert(entries transpose m == transpose entries m)
assert(m_{0,1} == matrix(R, {{1,2},{12,14}}))
assert(m_(1,1) == 14)
assert(m + (-m) == 0)
assert(m + m == 2*m)
assert(m + m == m*2)
assert(m + m == (2_R)*m)
assert(m + m == m*(2_R)) -- NOT implemented
assert(leadTerm m) -- Incorrect engine interface for gginitial
p = id_(R^5)
p ++ p == id_(R^10)  -- Engine BUG: ggddirectsum needs to check its types.
assert(entries p == table(5,5,(i,j)->if i===j then 1 else 0))
p = map(R^5,R^7,0)
assert(p == 0)
m = matrix{{1,2,3,4}}
assert(koszul(1,m) == m) -- INCORRECT
assert(koszul(2,m) != 0) -- INCORRECT
m = matrix(ZZ, {{1,10},{20,100}})
mm = m ** R 
assert(mm ** mm == matrix(R,{{1, 10, 10, 100}, {20, 100, 200, 1000}, {20, 200, 100, 1000}, {400, 2000, 2000, 10000}})) -- ggmatrix problem
m1 = transpose matrix(R,{{1,1,1}}) -- segmentation fault (after lots of stuff)
m2 = matrix{{3_R}}
entries relations (coker m1 ** coker m2 ) == {{3, 0, 0, 1}, {0, 3, 0, 1}, {0, 0, 3, 1}}
diff(m2,m1) -- Segmentation fault
contract(m2,m1) -- Segmentation fault

m = matrix(R,{{1,0,-1,3,-4}})
try assert false else error "Descending order seems to not be descending."
error "what about sorting columns?"
entries (m _ (sortColumns m)) == {{-4, -1, 1, 3, 0}}-- 0 is at the end: is this what we want?
-- homogenize, elim, sat, coefficients: polynomial ring only
-- reshape,flip,exteriorProduct
assert(flip(R^2,R^3) == matrix(R,{{1, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1}}))
m = map(R^1, R^6, (i,j) -> 6*i + j)
assert(reshape(R^2,R^3,m) == matrix (R,{{0, 2, 4}, {1, 3, 5}}))
assert(wedgeProduct(2,3,ZZ^5) ** R == wedgeProduct(2,3,R^5))  -- FAILS

-------------------------------------
-- ZZZ/101[a..d]
-------------------------------------
R = ZZZ/101[a..d,NewMonomialOrder=>RevLex=>4]
-- ring element operations
assert(31_R == 31)
assert((-31)_R == - (31_R))
assert(5_R - 5_R == 0)
assert(size (1_R) == 1)
assert(size (0_R) == 0)
assert(3 * 2_R == 6_R)
assert(3_R * 2_R == 6_R)
try assert(0 != (2^40)_R) else "ERROR: 2^40 is still 0"
assert(2^40 == (2^40)_R)
assert((3^8)_R == 3 * (3^7)_R)
assert((1_R - 1_R)^50 == 0)
assert(leadCoefficient(1_R) == 1)
assert(leadTerm(1_R) == 1)
assert(leadMonomial(1_R) == {}) -- Returns incorrect type...
f = a+3*b^2 + b*c - 1
assert(size f == 4)
assert(size(f - f) == 0)
assert(f + f == 2*f)
assert(f^3 == f*f*f)
assert(leadCoefficient f == 3)
assert(leadTerm f == 3*b^2)
assert(leadTerm f == someTerms(f,0,1))
assert(leadTerm (f - leadTerm f) == someTerms(f,1,1))
assert(degree f == {2})
assert(degree(1_R) == {0})
degree(0_R) == -infinity  -- is this really what we want?
try degree(1_R, {0,4,0,0}) == 8 else "ERROR: ggdegree with weights needs front-end interface"
try random R else error "random not working yet" -- ERROR: routine not implemented
-- homogenization, isGraded??

-- vectors
F = R^4
v = (3*a*b+1)*F_0 + (a+b+c+d)*F_1 + 0*F_2
try assert(false) else "POSSIBLE error: allow front end to make vectors/sparse vectors?"
assert(0_F == 0)
assert(leadComponent v == 0)
assert(v_0 == 3*a*b+1)
assert(v_1 == a+b+c+d)
assert(v_2 == 0)
assert(v != F_1)
assert(0*v == 0)
leadCoefficient v -- SEGMENTATION FAULT
assert(leadTerm v == 2*F_1)  -- FAILS: wrong engine interface
assert(leadTerm(0_F) == 0) -- FAILS: wrong engine interface
assert(v - v == 0)
assert(v + v + v == 3*v)
assert(v + v + v == v*3) -- FAILS: need front end interface
assert(v + v + v == (3_R)*v)
assert(- (-v) == v)
someTerms(v,1,1) -- FAILS: need front end interface
assert(degree v == {2})
degree (0_F) == {} -- Inconsistent with degree of a zero ring element
homogenize(v,d) -- INFINITE LOOP
-- matrices
random(R^2, R^3) -- incorrect stack arguments for command ggmatrix
                     -- need to IMPLEMENT kbasis first
m = matrix(R, {{1,2,3,4,5},{12,14,16,18,20}})
m1 = matrix {{1_R, 2_R, 3_R, 4_R, 5_R}, {12, 14, 16, 18, 20}}
assert(entries m == entries m1)
assert(entries transpose m == transpose entries m)
assert(m_{0,1} == matrix(R, {{1,2},{12,14}}))
assert(m_(1,1) == 14)
assert(m + (-m) == 0)
assert(m + m == 2*m)
assert(m + m == m*2)
assert(m + m == (2_R)*m)
assert(m + m == m*(2_R)) -- NOT implemented
assert(leadTerm m) -- Incorrect engine interface for gginitial
p = id_(R^5)
p ++ p == id_(R^10)  -- Engine BUG: ggddirectsum needs to check its types.
assert(entries p == table(5,5,(i,j)->if i===j then 1 else 0))
p = map(R^5,R^7,0)
assert(p == 0)
m = matrix{{1,2,3,4}}
assert(koszul(1,m) == m) -- INCORRECT
assert(koszul(2,m) != 0) -- INCORRECT
m = matrix(ZZ, {{1,10},{20,100}})
mm = m ** R 
assert(mm ** mm == matrix(R,{{1, 10, 10, 100}, {20, 100, 200, 1000}, {20, 200, 100, 1000}, {400, 2000, 2000, 10000}})) -- ggmatrix problem
m1 = transpose matrix(R,{{1,1,1}})
m2 = matrix{{3_R}}
entries relations (coker m1 ** coker m2 ) == {{3, 0, 0, 1}, {0, 3, 0, 1}, {0, 0, 3, 1}}
diff(m2,m1)
contract(m2,m1)

m = matrix{{a,b,c,a^2,b*c,b^2,b^3}}
assert(m _ (sortColumns m) == matrix {{c, b, a, b*c, b^2, a^2, b^3}})

-- homogenize, elim, sat, coefficients: polynomial ring only
-- reshape,flip,exteriorProduct
assert(flip(R^2,R^3) == matrix(R,{{1, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1}}))
m = map(R^1, R^6, (i,j) -> 6*i + j)
assert(reshape(R^2,R^3,m) == matrix (R,{{0, 2, 4}, {1, 3, 5}}))
assert(wedgeProduct(2,3,ZZ^5) ** R == wedgeProduct(2,3,R^5))  -- FAILS

-------------------------------------
-- Weyl algebra  NOT DONE
-------------------------------------
R = ZZZ/101[a,b,Da,Db,NewMonomialOrder=>RevLex=>4,Degrees=>{1,1,-1,-1},WeylAlgebra=>{a=>Da,b=>Db}]
-- ring element operations
assert(31_R == 31)
assert((-31)_R == - (31_R))
assert(5_R - 5_R == 0)
assert(size (1_R) == 1)
assert(size (0_R) == 0)
assert(3 * 2_R == 6_R)
assert(3_R * 2_R == 6_R)
try assert(0 != (2^40)_R) else "ERROR: 2^40 is still 0"
assert(2^40 == (2^40)_R)
assert((3^8)_R == 3 * (3^7)_R)
assert((1_R - 1_R)^50 == 0)
assert(leadCoefficient(1_R) == 1)
assert(leadTerm(1_R) == 1)
assert(leadMonomial(1_R) == {}) -- Returns incorrect type...
f = Da*a
assert(f == a*Da + 1)
theta = Da*a
theta^2*a 
Da*theta
theta^2
theta^3
assert(size f == 4)
assert(size(f - f) == 0)
assert(f + f == 2*f)
assert(f^3 == f*f*f)
assert(leadCoefficient f == 3)
assert(leadTerm f == 3*b^2)
assert(leadTerm f == someTerms(f,0,1))
assert(leadTerm (f - leadTerm f) == someTerms(f,1,1))
assert(degree f == {2})
assert(degree(1_R) == {0})
degree(0_R) == -infinity  -- is this really what we want?
try degree(1_R, {0,4,0,0}) == 8 else "ERROR: ggdegree with weights needs front-end interface"
try random R else error "random not working yet" -- ERROR: routine not implemented
-- vectors
F = R^4
v = (3*a*b+1)*F_0 + (a+b+c+d)*F_1 + 0*F_2
try assert(false) else "POSSIBLE error: allow front end to make vectors/sparse vectors?"
assert(0_F == 0)
assert(leadComponent v == 0)
assert(v_0 == 3*a*b+1)
assert(v_1 == a+b+c+d)
assert(v_2 == 0)
assert(v != F_1)
assert(0*v == 0)
leadCoefficient v -- SEGMENTATION FAULT
assert(leadTerm v == 2*F_1)  -- FAILS: wrong engine interface
assert(leadTerm(0_F) == 0) -- FAILS: wrong engine interface
assert(v - v == 0)
assert(v + v + v == 3*v)
assert(v + v + v == v*3) -- FAILS: need front end interface
assert(v + v + v == (3_R)*v)
assert(- (-v) == v)
someTerms(v,1,1) -- FAILS: need front end interface
assert(degree v == {2})
degree (0_F) == {} -- Inconsistent with degree of a zero ring element
homogenize(v,d) -- INFINITE LOOP
-- matrices
m = matrix{{a,b},{Da,Db}}
m^2  -- fails: ggiszero
assert(not(m == 0))
m*m -- fails: ggiszero
entries m == {{a,b},{Da,Db}}
m = matrix(R, {{1,2,3,4,5},{12,14,16,18,20}})
m1 = matrix {{1_R, 2_R, 3_R, 4_R, 5_R}, {12, 14, 16, 18, 20}}
assert(entries m == entries m1)
assert(entries transpose m == transpose entries m)
assert(m_{0,1} == matrix(R, {{1,2},{12,14}}))
assert(m_(1,1) == 14)
assert(m + (-m) == 0)
assert(m + m == 2*m)
assert(m + m == m*2)
assert(m + m == (2_R)*m)
assert(m + m == m*(2_R)) -- NOT implemented
assert(leadTerm m) -- Incorrect engine interface for gginitial
p = id_(R^5)
p ++ p == id_(R^10)  -- Engine BUG: ggddirectsum needs to check its types.
assert(entries p == table(5,5,(i,j)->if i===j then 1 else 0))
p = map(R^5,R^7,0)
assert(p == 0)
m = matrix{{1,2,3,4}}
assert(koszul(1,m) == m) -- INCORRECT
assert(koszul(2,m) != 0) -- INCORRECT
m = matrix(ZZ, {{1,10},{20,100}})
mm = m ** R 
assert(mm ** mm == matrix(R,{{1, 10, 10, 100}, {20, 100, 200, 1000}, {20, 200, 100, 1000}, {400, 2000, 2000, 10000}})) -- ggmatrix problem
m1 = transpose matrix(R,{{1,1,1}})
m2 = matrix{{3_R}}
entries relations (coker m1 ** coker m2 ) == {{3, 0, 0, 1}, {0, 3, 0, 1}, {0, 0, 3, 1}}
diff(m2,m1)
contract(m2,m1)

m = matrix{{a,b,c,a^2,b*c,b^2,b^3}}
assert(m _ (sortColumns m) == matrix {{c, b, a, b*c, b^2, a^2, b^3}})

-- homogenize, elim, sat, coefficients: polynomial ring only
-- reshape,flip,exteriorProduct
assert(flip(R^2,R^3) == matrix(R,{{1, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1}}))
m = map(R^1, R^6, (i,j) -> 6*i + j)
assert(reshape(R^2,R^3,m) == matrix (R,{{0, 2, 4}, {1, 3, 5}}))
assert(wedgeProduct(2,3,ZZ^5) ** R == wedgeProduct(2,3,R^5))  -- FAILS

///
