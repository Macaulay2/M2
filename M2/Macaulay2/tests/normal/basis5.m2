TEST String := teststr -> assert not first capture(teststr, UserMode => false)

TEST ///
 basis (QQ^6)
 basis (ZZ^7)
///

TEST ///
  R = ZZ/101[a..d]
  I = ideal(a*d, b^2, c*d)
  assert(
       basis(2,R) 
       == matrix {{a^2, a*b, a*c, a*d, b^2, b*c, b*d, c^2, c*d, d^2}})
  assert(
       sort(basis(2,3,R), MonomialOrder=>Descending)
       == matrix {{a^3, a^2*b, a*b^2, b^3, a^2*c, a*b*c, b^2*c, a*c^2, b*c^2, c^3, a^2*d, a*b*d, b^2*d, a*c*d,
      b*c*d, c^2*d, a*d^2, b*d^2, c*d^2, d^3, a^2, a*b, b^2, a*c, b*c, c^2, a*d, b*d, c*d, d^2}})
  assert(
       basis(2,R,Variables=>{a,b})
       == matrix {{a^2, a*b, b^2}})
  assert(
       basis(2,R,Variables=>{0,1}) 
       == matrix {{a^2, a*b, b^2}})
  assert(
       matrix basis(2,coker gens I)
        == matrix {{a^2, a*b, a*c, b*c, b*d, c^2, d^2}})
  assert(
       matrix basis(3,coker gens I,Variables=>{a,b})
        == matrix {{a^3, a^2*b}})
  R = ZZ/101[a..d,Degrees=>{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
  assert(
       basis({1,1,1,1},R) 
       == matrix"abcd")
  -- basis({1,1,1,1},R,Heft=>{1,1,1,1})
  R = ZZ/101[a..d,Degrees=>{{1,1},{2,1},{0,3},{4,0}}]
  assert(
       basis({7,5},R)
        == matrix {{a^3*b^2, a*b*c*d}})
  -- basis({7,5},R,Heft=>{1,1})
  R = ZZ/101[a..d]
  assert(
       basis(10,R,Limit=>10) 
       == matrix {{a^10, a^9*b, a^9*c, a^9*d, a^8*b^2, a^8*b*c, a^8*b*d, a^8*c^2, a^8*c*d, a^8*d^2}})
///

TEST ///
  -- skew commuting rings
  R = ZZ/101[a..f,SkewCommutative => {a,b,c}]
  b*a
  a^2
  c^3 
  assert(
    basis(2,R)
    == matrix {{a*b, a*c, a*d, a*e, a*f, b*c, b*d, b*e, b*f, 
	      c*d, c*e, c*f, d^2, d*e, d*f, e^2, e*f, f^2}})
  S = R/(a*b,c*d,e*f, d^3, e^3, f^3)
  assert(
    basis(5,S)
    == matrix {{a*d^2*e^2, a*d^2*f^2, b*d^2*e^2, b*d^2*f^2}})
///

TEST ///
  -- multigraded rings
  R = ZZ/101[a..d,Degrees=>{{1,2,3},{1,-1,0},{1,1,1},{2,1,-3}}]
  degree (a*b*c*d)
  assert(basis({5,3,1},R) == matrix"abcd")
  assert(basis({1,2,3},R) == matrix"a")
///

TEST ///
-- over ZZ
  R = ZZ[a..c]
  M = coker matrix"3a,4b,5c,a3,b3,c3"
  F = basis M
  assert(source F == R^{{0},{-1},{-2},{-1},{-2},{-1},{-2}})
  assert(target F == M)
  assert(matrix F == matrix {{1, a, a^2, b, b^2, c, c^2}})
///

TEST ///
      A = QQ[x];
      R = A/x^6;
      f = basis(R,SourceRing => ambient R)
      assert( ring source f === ambient R)
      assert( isHomogeneous f )
      assert( {{0}, {1}, {2}, {3}, {4}, {5}} == degrees source f )
      use A
      assert( coimage f == 
	   cokernel map(
		A^{{0},{-1},{-2},{-3},{-4},{-5}},
		A^{{-1},{-2},{-3},{-4},{-5},{-6}},
		     {{x,0,0,0,0,0},{-1,x,0,0,0,0},{0,-1,x,0,0,0},{0,0,-1,x,0,0},{0,0,0,-1,x,0},{0,0,0,0,-1,x}}))
      assert( kernel f ==
	   image map(A^{{0},{-1},{-2},{-3},{-4},{-5}},
		    A^{{-1},{-2},{-3},{-4},{-5},{-6}},
		    {{x,0,0,0,0,0},{-1,x,0,0,0,0},{0,-1,x,0,0,0},{0,0,-1,x,0,0},{0,0,0,-1,x,0},{0,0,0,0,-1,x}}))
 ///

 TEST ///
      R = QQ[x]/x^6;
      B = QQ[];
      h = basis(R,SourceRing => B)
      assert( degrees source h == {{0}, {1}, {2}, {3}, {4}, {5}} )
      assert( coimage h == B^{{0}, {-1}, {-2}, {-3}, {-4}, {-5}} )
      assert( kernel h == image map(B^{{0},{-1},{-2},{-3},{-4},{-5}},0,0))
 ///

 TEST ///
      R = QQ[x]/x^6;
      g = basis(R,SourceRing => QQ)
      assert( coimage g === QQ^6 )
      assert( kernel g === image map(QQ^6,0,0) )
 ///

 TEST ///
 -- A simple use of basis: all elements of a given degree in a ring
 -- sometimes just ones involving a set of the variables
 R  = ZZ/101[a..d]
 B2 = basis(2,R)
 assert(B2 == matrix {{a^2, a*b, a*c, a*d, b^2, b*c, b*d, c^2, c*d, d^2}})

 C2 = basis(2,R, Variables=>{a,c})
 assert(C2 == matrix {{a^2, a*c, c^2}})

 R  = ZZ/101[a..d, Degrees=>{1,2,3,4}]
 B2 = basis(2,R)
 assert(B2 == matrix {{a^2, b}})

 C2 = basis(4,R, Variables=>{a,c})
 assert(C2 == matrix {{a^4, a*c}})
 ///

 TEST ///
 -- Similar, but considering variables of degree 0
 A = ZZ/101[a,b]
 B = A[x,y,DegreeMap=>i->0,Join=>false]
 degree x
 degree B_2
 assert(
      basis(2,B)
      == matrix {{x^2, x*y, y^2}})

 C = ZZ/101[x,y,a,b,Degrees=>{1,1,0,0}]
 assert(
      basis(2,C,Variables=>{x,y})
      == matrix"x2,xy,y2")
   -- should only need the heft to be positive on all variables being used!

 ///

TEST ///
-- test of whether finiteness is checked correctly, if we only use some variables:
R = ZZ/101[a..d]
A = R/(a^3, b^4)
assert(
  sort basis(A, Variables=>{a,b})
  == matrix {{1, b, a, b^2, a*b, a^2, b^3, a*b^2, a^2*b, a*b^3, a^2*b^2, a^2*b^3}})
assert try (basis(A, Variables=>{a,c});false) else true
///

 TEST ///
 -- basis of {d,*,*,...,*}
 -- the case done first:
 R = ZZ/101[a..d, Degrees=>toList(4:{1,2})]
 M = coker matrix"a,b;c,d"
 SM = symmetricAlgebra M
 degree SM_0
 degree SM_2
 describe SM
 assert(
      basis(1, SM)
      == matrix {{p_0, p_1}})
 assert(
      basis(2, SM)
      == matrix {{p_0^2, p_0*p_1, p_1^2}})
 assert(
      (B = basis(2, SM, SourceRing=>R, Degree=>{2,0,0}))
      == map(SM^1,R^3,map(SM,R,{a, b, c, d}),{{p_0^2, p_0*p_1, p_1^2}},Degree=>{2, 0, 0})
      )
 assert isHomogeneous B
 ///

TEST ///
  -- had been in bugs/mike/0-basis r12446
  T = QQ[x,Degrees => {-1}]
  assert(basis(-3,-2, T) == matrix"x2,x3")
  assert(basis(-2,-3, T) == 0)
///

 TEST ///
 -- More partial bases:

 A = ZZ/101[a..d, Degrees=>{2:{1,2},2:{0,1}}]
 assert(
      basis({3}, A^1) 
      == matrix"a3,a2b,ab2,b3")

 A = ZZ/101[a..d, Degrees=>{2:{1,1},2:{0,1}}]
 assert(
      basis({3}, A^1) 
      == matrix"a3,a2b,ab2,b3")

 A = ZZ/101[a,b, Degrees=>{{1,2},{0,1}}]
 assert(
      basis({3}, A^1)
      == matrix"a3")

 A = ZZ/101[a..d, Degrees=>{2:{2,1},2:{1,0}}]
 assert(
      basis({3}, A^1) 
      == matrix"ac,ad,bc,bd,c3,c2d,cd2,d3")

 A = ZZ/101[a..d, Degrees=>{2:{2,1,0},2:{1,0,0}}]
 assert(
      basis({3,1}, A^1) 
      == matrix"ac,ad,bc,bd")

 rewriteRing = (R, ndegs) -> (
      zerodeg := toList(ndegs:0);
      basevars := select(gens R, x -> take(degree x, ndegs) == zerodeg);
      B := (coefficientRing R)[basevars, Degrees=>apply(basevars, x -> drop(degree x, ndegs))];
      newvars := select(gens R, x -> take(degree x, ndegs) != zerodeg);
      C := B[newvars, Degrees=>apply(newvars, x -> take(degree x, ndegs))];
      C
      )

 findHeftVars = (R, ndegs) -> (
      zerodeg := toList(ndegs:0);
      goodvars := positions(gens R, x -> take(degree x, ndegs) =!= zerodeg);
      heft := findHeft(apply(goodvars, i -> take(degree R_i, ndegs)), DegreeRank=>ndegs);
      (goodvars, heft))

 A = ZZ/101[a..d, Degrees=>{2:{1,2},2:{0,1}}]
findHeftVars(A,1)
rewriteRing(A,1)
basis({3},oo)
///

TEST ///
  -- basis as functor
  R = ZZ/101[a..d]
  M = matrix"a,b;c,d"
  basis(1,M)  
  
  -- A second example
  -- I need to check to see if this is correct.
  R = ZZ/101[a..e]
  I = ideal"ab,bc,cd,de,ea"
  S = reesAlgebra I
  T = ambient S
  L = ideal S
  describe S
  C = res L
  f = C.dd_2
  basis(1,f)  
  basis(2,f)  
  basis(3,f)
  g = C.dd_3
  basis(2,g)
///

TEST ///
R = QQ[x,y,z]/(x^3,y^2,z^5);                                                                           
basis(-infinity,4,R)                                                                                   
basis(5,infinity,R)                                                                                   
///
