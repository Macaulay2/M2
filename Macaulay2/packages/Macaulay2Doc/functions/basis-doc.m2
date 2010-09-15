document { Key => {basis,
	  (basis,InfiniteNumber,InfiniteNumber,Ring), (basis,List,List,Module), (basis,Ring), (basis,InfiniteNumber,ZZ,Module), (basis,ZZ,InfiniteNumber,Module),
	  (basis,List,InfiniteNumber,Ideal), (basis,InfiniteNumber,List,Ideal), (basis,InfiniteNumber,List,Ring), (basis,List,InfiniteNumber,Ring), (basis,List,List,Ideal),
	  (basis,InfiniteNumber,ZZ,Ideal), (basis,ZZ,InfiniteNumber,Ideal), (basis,List,List,Ring), (basis,ZZ,InfiniteNumber,Ring), (basis,InfiniteNumber,ZZ,Ring), (basis,ZZ,ZZ,Module),
	  (basis,List,ZZ,Ideal), (basis,ZZ,List,Ideal), (basis,ZZ,List,Ring), (basis,List,ZZ,Ring), (basis,ZZ,ZZ,Ideal), (basis,ZZ,ZZ,Ring), (basis,List,Module), (basis,ZZ,Module),
	  (basis,List,Ideal), (basis,List,Ring), (basis,ZZ,Ideal), (basis,ZZ,Ring), (basis,InfiniteNumber,InfiniteNumber,Module), (basis,Module), (basis,List,InfiniteNumber,Module),
	  (basis,InfiniteNumber,List,Module), (basis,InfiniteNumber,InfiniteNumber,Ideal), (basis,Ideal),
	  [basis,Limit], [basis,Truncate], Truncate, [basis,Variables], [basis,SourceRing] },
     Headline => "basis of all or part of a module or ring",
     Usage => "basis(i,M)",
     Inputs => {
	  "i" => "a list of integers to serve as a degree or multidegree",
	  "M" => {ofClass{Module,Ring,Ideal}, ".  If ", TT "M", " is a ring, it is replaced by the free module of rank 1.
	       If ", TT "M", " is an ideal, it is replaced by its underlying module over the ring it is contained in."},
	  Limit => ZZ => {"the maximum number of basis elements to find"},
	  Truncate => Boolean => {"whether to add additional generators to the basis sufficient to generate the submodule of ", TT "M", " generated
	       by all elements of degree at least ", TT "i", ".  If true, the degree rank must be equal to 1.  This option is intended mainly for internal use by
	       ", TO "truncate", "."
	       },
	  Variables => List => {"a list of variables; only basis elements involving only these variables will be reported"},
	  SourceRing => Ring => {"the ring to use as the ring of the source of the map produced; by default, this is the same
	       as the ring of ", TT "M", "."
	       }
	  },
     Outputs => {
	  {
	       "a map from a free module over the ring specified by the ", TO "SourceRing", " option, or over
	       the ring of ", TT "M", " if the option was not used, to ", TT "M", " which sends the
	       basis elements of the free module to a basis (over the coefficient field) of the degree ", TT "i", " part of ", TT "M"
	       }
	  },
     PARA {
	  "The degree ", TT "i", " is a multi-degree, represented as a list of integers.  If the degree rank is 1, then ", TT "i", " may
	  be provided as an integer."
	  },
     PARA {
	  "The algorithm uses the heft vector of the ring, and cannot proceed without one; see ", TO "heft vectors", "."
	  },
     EXAMPLE lines ///
	  R = ZZ/101[a..c];
	  basis(2, R)
	  M = ideal(a,b,c)/ideal(a^2,b^2,c^2)
	  f = basis(2,M)
     ///,
     PARA {
	  "Notice that the matrix of ", TT "f", " above is expressed in terms of the
	  generators of ", TT "M", ".  The reason is that the module ", TT "M", " is the target
	  of the map ", TT "f", ", and matrices of maps such as ", TT "f", " are always expressed 
	  in terms of the generators of the source and target."
	  },
     EXAMPLE lines ///
	  target f
     ///,
     PARA {
     	  "The command ", TO "super", " is useful for rewriting ", TT "f", " in terms of the generators of module of which ", TT "M", " is a submodule."
	  },
     EXAMPLE lines ///
	  super f
     ///,
     PARA { "When a ring is multi-graded, we specify the degree as a list of integers." },
     EXAMPLE lines ///
	  S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];
	  basis({7,24}, S)
     ///,
     PARA {
	  "Here is an example showing the use of the ", TO "SourceRing", " option.  Using a ring
	  of different degree length as the source ring is currently not well supported, because the
	  graded free modules may not lift."
	  },
     EXAMPLE lines ///
     R = QQ[x]/x^6;
     f = basis(R,SourceRing => ambient R)
     coimage f
     kernel f
     g = basis(R,SourceRing => QQ)
     coimage g
     kernel g
     ///,
     PARA {
	  "In some situations it may be desirable to retain the degrees of the generators, so a ring such as
	  ", TT "QQ[]", ", which has degree length 1, can serve the purpose."
	  },
     EXAMPLE lines ///
     degrees source g
     A = QQ[];
     h = basis(R,SourceRing => A)
     degrees source h
     coimage h
     kernel h
     ///,
     SYNOPSIS (
	  Usage => "basis M",
	  Inputs => { "M" => {ofClass{Module,Ring}} },
	  Outputs => { 
	       { "a map from a free module to ", TT "M", " which sends the basis elements to a basis, over the coefficient field, of ", TT "M" }
	       },
	  EXAMPLE lines ///
	       R = QQ[x,y,z]/(x^2,y^3,z^5)
	       basis R
	  ///
	  ),
     SYNOPSIS (
	  Usage => "basis(lo,hi,M)",
	  Inputs => {
	       "M" => {ofClass{Module,Ring,Ideal}},
	       "lo" => {ofClass{ZZ,List,InfiniteNumber}},
	       "hi" => {ofClass{ZZ,List,InfiniteNumber}}
	       },
	  Outputs => {{
		    "a map from a free module to ", TT "M", " which sends the
		    basis elements to a basis, over the ground field, of the part of ", TT "M", " spanned
		    by elements of degrees between ", TT "lo", " and ", TT "hi", ".
		    The degree rank must be 1."
		    }},
	  EXAMPLE lines ///
	       R = QQ[x,y,z]/(x^3,y^2,z^5);
	       basis R
	       basis(-infinity,4,R)
	       basis(5,infinity,R)
	       basis(2,4,R)
	  ///
	  )
     }

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


 -- Tests added MES, Sep 2010
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

{*
 C = ZZ/101[x,y,a,b,Degrees=>{1,1,0,0}]
 assert(
      basis(2,C,Variables=>{x,y}) -- this should be allowed BUG
      == matrix"x2,xy,y2")
   -- should only need the heft to be positive on all variables being used!
   *}

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
 -- More partial bases:
 {*
 A = ZZ/101[a..d, Degrees=>{2:{1,2},2:{0,1}}]
 assert(
      basis({3}, A^1) 
      == matrix"a3,a2b,ab2,b3")
   -- BUG: this last one is wrong, perhaps because of the heft vector has value -1 on the first variable?
*}

{*
 A = ZZ/101[a..d, Degrees=>{2:{1,1},2:{0,1}}]
 assert(
      basis({3}, A^1) 
      == matrix"a3,a2b,ab2,b3")
   -- BUG too: this is coming out ot be "1".  Here the heft vec is picking out the last comp
*}

{*
 A = ZZ/101[a,b, Degrees=>{{1,2},{0,1}}]
 assert(
      basis({3}, A^1)
      == matrix"a3")
      -- BUG: this one is wrong too, as this is a simpler version of one above.
*}
 A = ZZ/101[a..d, Degrees=>{2:{2,1},2:{1,0}}]
 assert(
      basis({3}, A^1) 
      == matrix"ac,ad,bc,bd,c3,c2d,cd2,d3")

 A = ZZ/101[a..d, Degrees=>{2:{2,1,0},2:{1,0,0}}]
 assert(
      basis({3,1}, A^1) 
      == matrix"ac,ad,bc,bd,c3,c2d,cd2,d3")

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
