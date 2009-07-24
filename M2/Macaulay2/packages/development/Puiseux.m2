-- -*- coding: utf-8 -*-
newPackage(
	"Puiseux",
    	Version => "0.1", 
    	Date => "April 24, 2009",
    	Authors => {},
    	Headline => "Computing Puiseux expansions of plane curves",
    	DebuggingMode => true
    	)

-- code is based in large part on the paper of Duval, Compositio Math 1989
-- this code is still buggy, rough, and lacking features!

--     squarefree,
--     factorization,
--     test'puiseux,
--      NewtonEdge
-- NegativeSlopeOnly

export {
     adjoinRoot, 
     newtonEdges,
     puiseux, 
     testPuiseux
     }

DoingRationalPuiseux = true

factorization = (F) -> (
     fs := factor F;
     select(fs//toList/toList/reverse/toSequence, f -> first degree f#1 > 0))

----------------------------------------------
-- Internal routines for the package ---------
----------------------------------------------
slope = (pts,i,j) -> (pts#j#1 - pts#i#1)/(pts#j#0 - pts#i#0)

polynomialGCD = (f,g) -> (gens gb ideal(f,g))_(0,0)

deg = (f) -> if f == 0 then -1 else (degree f)#0

squarefree = (f) -> (
     R := ring f;
     p := char R;
     x := R_0;
     result := {};
     T := polynomialGCD(f,diff(x,f));
     V := f//T;
     k := 0;
     while deg(V) =!= 0 do (
	  k = k+1;
	  W := polynomialGCD(T,V);
	  Ak := V//W;
	  V = W;
	  T = T//V;
	  if deg(Ak) =!= 0 then 
	      result = append(result,(k,Ak));
	  );
     if deg T != 0 then (
	  -- we have a polynomial in x^p
	  result2 := squarefree lowerP(T);
	  result2 = apply(result2, a -> (p*a#0, a#1));
	  result = join(result,result2);
	  );
     result
     );

----------------------------------------------
-- adjoining a root of a polynomial ----------
-- this likely will change interface, and ----
-- leave this file! --------------------------
----------------------------------------------
adjoinRoot = method()
adjoinRoot RingElement := (f) -> (
     K1top := null;
     K1 := null;
     R := ring f;
     t := R_0;
     K := coefficientRing R;
     I := ideal K;
     n := numgens K;
     if deg f == 1 then (
	  -- we need to solve for the root
	  a := coefficient(t, f);
	  b := coefficient(1_R, f);
	  if a == 1 then -b else -b/a
	  )
     else (
	  -- We need to create a new ring
	  -- The default will be to make one non-tower ring out of all of this
	  -- Assuming that f is monic?
	  if numgens K == 0 then (
	       K1top = K[vars 0];
	       K1 = K1top/sub(f, t => K1top_0);
	       toField K1;
	       K1_0
	       )
	  else (
	    K1top = (coefficientRing K)[vars n, gens K, MonomialOrder=>Lex];
	    to1 := map(K1top, ring I, drop(gens K1top,1));
	    to2 := map(K1top, R, gens K1top);
	    J := ideal to2 f + to1 I;
	    K1 = K1top/J;
	    toField K1;
	    K1_0
	  )
     ))

----------------------------------------
-- Puiseux construction ----------------
----------------------------------------
NewtonEdge = new Type of List;  -- {p,q,ell,g(t)}
  -- p and q are positive integers.
  -- ell is too
  -- g(t) is a univariate polynomial over some base ring encoding the poly on the edge.
NewtonTerm = new Type of List -- {p,q,ell, mu, alpha, r} 
  -- r is the power to which the minimal poly of alpha occurs in the lead poly
NewtonBranch = new Type of List -- {{t:NewtonTerm}, F1(x,y)}


polygon1 = method()
polygon1(RingElement,Boolean) := (F,negativeSlopeOnly) -> (
     -- MES: Doesn't handle 0 slope.
     --      Doesn't use option to choose negative slope only...
     -- F should be in a polynomial ring with 2 variables (over something else)
     -- find the lower lines for the Newton polygon of F.
     -- input: F(x,y) in a polynomial ring A[x,y] (names can be different).
     --          assumption: F(0,y) != 0.
     --        negativeSlopeOnly: boolean
     -- output:
     --        a list of lists of the extremal points (j,i) which occur on the (lower) Newton
     --          polygon of F.  (Here (j,i) <--> x^i y^j)
     --          Each list consists of pairs(j,i), with descending j, on the line im + jq = ell
     minpairs = sort apply(pairs partition(x -> x#1, exponents F), (k,v) -> (k, min apply(v, first)));
     lastpoint = 0;     
     ylo = (position(minpairs, x -> x#1 == 0));
     while lastpoint < ylo list (
       slopes = for i from lastpoint+1 to ylo list slope(minpairs,lastpoint,i);
       m := min slopes;
       thisedge = apply(positions(slopes, x -> x == m), y -> y+lastpoint+1);
       result := prepend(minpairs_lastpoint,minpairs_thisedge);
       lastpoint = max thisedge;
       result)
  )

-- attempting to fix the bugs in this function:
edgesToInfo = method()
edgesToInfo(List, RingElement, RingElement) := (elist,F,x) -> (
     -- elist should be the output of 'polygon'
     -- x should be a variable in a ring, this will be the variable used
     --  to create the edge polynomials
     R := ring F;
     S := ring x;
     apply(elist, e -> (
	    y0 := e#1#0 - e#0#0;
	    x0 := e#1#1 - e#0#1;
	    g := gcd(y0,x0);
	    q := -x0//g;
	    p := y0//g;
	    ell := q * e#0#0 + p * e#0#1;
	    j0 := e#0#0;
	    G = sum (L=apply(e, ji -> (
		      d := (ji#0-j0); -- // q; -- dvide by q when we adjoin a q-th root
		      if DoingRationalPuiseux then
		        d = d//p; ---RATIONAL CHANGE
		      coefficient(R_{ji#1,ji#0},F) * x^d
		      )));
	    (p, q, ell, G)
	    ))
  )

polygon = method(Options=>{NegativeSlopeOnly => false})
polygon(RingElement,RingElement) := opts -> (F,t) -> (
     -- input: F(x,y) in a polynomial ring A[x,y] (names can be different).
     --          assumption: F(0,y) != 0.
     --        optional: whether to consider the zero slope too
     -- output: a list of 4-tuples (a,b,m,G(t)), with a,b,m in ZZ, (a,b) = 1, a > 0, b >= 0.
     --        such that the lines ai+bj = m are the extremal lines of the lower Newton polygon (terms x^i y^j)
     --        and G(t) in A[t] consists of the extremal terms of F(x,y), except that
     --        a term x^?? y^?? has been replaced by t^c.
     -- Note: the denominator of the 
     p := polygon1(F, opts.NegativeSlopeOnly);
     edgesToInfo(p, F, t)     
     )

newtonEdges = method()
newtonEdges(RingElement, RingElement) := (F,t) -> apply(polygon(F,t), e -> new NewtonEdge from toList e)
newtonEdges RingElement := (F) -> (
     R1 := (coefficientRing ring F)[symbol t];
     newtonEdges(F,R1_0)
     )

newtonTerms = method()
newtonTerms(NewtonEdge) := (E) -> (
     -- find the squarefree decomp of g(t)
     -- for each irred polynomial, make a new term by 
     -- adding its root to the field.
     -- result: a list of NewtonTerm's
     (p,q,ell,g) := toSequence E;
     gs := factorization g; -- TODO: use UPolynomials.m2
     if DoingRationalPuiseux
     then (
       -- RATIONAL CASE:
       (g0,u0,v0) := toSequence gcdCoefficients(p,q);
       apply(gs, (i,gi) -> (
     	       beta := adjoinRoot gi;
	       mu := beta^(-v0);
	       alpha := beta^u0;
	       new NewtonTerm from {p,q,ell,mu,alpha,i}))
       )
     else
       apply(gs, (i,gi) -> (
     	       alpha := adjoinRoot gi;
	       new NewtonTerm from {p,q,ell,1_(ring alpha),alpha,i}))
     )

applyTerm = method()
applyTerm(NewtonTerm, RingElement) := (tm, F) -> (
     -- return F_1(x,y)
     R := ring F;
     Rnew := R;
     (p,q,ell,mu,alpha,r) := toSequence tm;
     if ring alpha =!= coefficientRing R then
     	  Rnew = (ring alpha)(monoid R);
     coeffvars := drop(gens ring alpha, numgens coefficientRing Rnew - numgens coefficientRing R);
     toRnew := map(Rnew,R,join({mu*Rnew_0^p,Rnew_0^q*(alpha+Rnew_1)},coeffvars));
     (toRnew F) // Rnew_0^ell
     )

seriesClassical = (tms, t, K1) -> (
     -- return the parametrization (t^m, h(t)) corresponding to the list of terms L.
     -- each term is (p,q,mu,ell,alpha,r)  (here r is not used).
     xdegree := product apply(tms, first);
     xt := t^xdegree;
     lastp := xdegree;
     lastq := 0;
     ps := apply(#tms, i -> lastp = lastp / tms#i#0);
     qs := apply(#tms, i -> lastq = lastq + ps#i * tms#i#1);
     alphas := apply(#tms, i -> sub(tms#i#4, K1));
     yt := sum apply(#tms, i -> alphas#i * t^(qs#i));
     (xt,yt)
     )
seriesRational = (tms, t, K1) -> (
     -- each tms element is {p, q, xxx, u, a, xxx}
     -- recall notation here:
     -- y = x0^q0(a0 + y0)   x = u0 x0^p0
     -- y0 = x1^q1(a1+ y1)   x0 = u1 x1^p1
     -- ...
     -- y(n-2) = x(n-1)^q(n-1) (a(n-1) + y(n-1),   x(n-2) = u(n-1) x(n-1)^p(n-1)
     -- and x(n-1) = t,  y(n-1) is set to zero.
     -- the i-th term is ai s0^q0 s1^q1 ... si^qi t^ei
     -- where for 0 <= i <= n-1
     --  ri = exponent of t in xi
     --  si = sub(xi, t=>1)
     --  ei = exponent of the i-th term of y
     -- we compute in order
     --  ri, ei, si, then each term
     n := # tms;
     alphas := apply(#tms, i -> sub(tms#i#4, K1));
     us := apply(#tms, i -> sub(tms#i#3, K1));

     rs := new MutableList from toList(n : 1_K1);
     for i from 1 to n do rs#(n-i-1) = tms#(n-i)#0 * rs#(n-i);

     ss := new MutableList from toList(n : 1_K1);
     for i from 0 to n-2 do ss#(n-i-2) = us#(n-i-1) * (ss#(n-i-1))^(tms#(n-i-1)#0);

     xt := us#0 * ss#0^(tms#0#0) * t^(rs#0);
     
     -- Now let's make the terms
     this'coeff := 1_K1;
     this'exp := 0;
     yt := sum for i from 0 to n-1 list (
     	  this'exp = this'exp + rs#i * tms#i#1;
	  this'coeff = this'coeff * ss#i^(tms#i#1);	  
	  alphas#i * this'coeff * t^this'exp
	  );
     (xt,yt)     
     )

-- Let's try this again!!
seriesRational = (tms, t, K1) -> (
     -- each tms element is {p, q, xxx, u, a, xxx}
     -- recall notation here:
     -- y0 = x1^q0(a0 + y1)   x0 = u0 x1^p0
     -- y1 = x2^q1(a1 + y2)   x1 = u1 x2^p1
     -- ...
     -- y(n-2) = x(n-1)^q(n-2) (a(n-2) + y(n-1)),   x(n-2) = u(n-2) x(n-1)^p(n-2)
     -- y(n-1) = x(n)^q(n-1) (a(n-1) + y(n)),       x(n-1) = u(n-1) x(n)^p(n-2)     
     -- and x(n) = t,  y(n) is set to zero.
     -- the i-th term is ai s0^q0 s1^q1 ... si^qi t^ei
     -- where for 0 <= i <= n-1
     --  ri = exponent of t in xi
     --  si = sub(xi, t=>1)
     --  ei = exponent of the i-th term of y
     -- we compute in order
     --  ri, si, then each term
     n := # tms;
     alphas := apply(#tms, i -> sub(tms#i#4, K1));
     us := apply(#tms, i -> sub(tms#i#3, K1));

     rs := new MutableList from toList((n+1) : 1);
     for i from 1 to n do rs#(n-i) = tms#(n-i)#0 * rs#(n-i+1);

     ss := new MutableList from toList((n+1) : 1_K1);
     for i from 1 to n do ss#(n-i) = us#(n-i) * (ss#(n-i+1))^(tms#(n-i)#0);

     xt := ss#0 * t^(rs#0);
     
     -- Now let's make the terms
     this'coeff := 1_K1;
     this'exp := 0;
     yt := sum for i from 0 to n-1 list (
     	  this'exp = this'exp + rs#(i+1) * tms#i#1;
	  this'coeff = this'coeff * ss#(i+1)^(tms#i#1);	  
	  alphas#i * this'coeff * t^this'exp
	  );
     (xt,yt)     
     )
     
series = method()
series List := (tms) -> (
     K1 := ring(tms#-1#4);
     St := K1[symbol t, MonomialOrder=>RevLex, Global=>false];
     t := St_0;     
     if DoingRationalPuiseux 
       then seriesRational(tms, t, K1)
       else seriesClassical(tms, t, K1)
     )

extend(NewtonBranch,ZZ) := opts -> (B,ord) -> (
     F := B#1;
     ord = ord - sum apply(B#0, tm -> tm#1);
     if ord <= 0 then return B#0;
     R := ring F;
     x := R_0;
     y := R_1;
     c := coefficient(y, F);
     cinv := 1/c;
     F0 := sub(F, {y=>0});
     if F0 == 0 then return B#0;
     m := min apply(terms F0, g -> (first exponents g)_0);
     -- we want to mod out by the ideal 
     ytop := floor(ord / m);
     J = ideal apply(0..ytop, i -> y^i * x^(ord - m*i));
     Rtrunc := R/J;
     G := sub(F,Rtrunc);
     prev'm = 0;
     newtms := for i from 0 to ord list (
	  if G == 0 then break;
	  G0 := sub(G, {Rtrunc_1 => 0});
	  if G0 == 0 then break;
          m := min apply(terms G0, g -> (first exponents g)_0);
	  cm := coefficient(x^m, G);
	  b := -cm * cinv;
	  ans := new NewtonTerm from {1, m-prev'm, m-prev'm, 1_(ring b), b, 1};
	  prev'm = m;
	  G = sub(G, {Rtrunc_1 => x^m*b+y});
	  ans
	  );
     join(B#0, newtms)
     )

branches1 = (F) -> (
     K := coefficientRing ring F;
     Kt := K[symbol t];
     E := newtonEdges(F,Kt_0);
     tms := flatten apply(E, newtonTerms);
     apply(tms, tm -> new NewtonBranch from {{tm}, applyTerm(tm,F)})
     )

branches = method()
branches(RingElement) := (F) -> branches(new NewtonBranch from {{}, F})
branches NewtonBranch := (B) -> (
     -- calls branches1, and branches recursively 
     (tms,F) := toSequence B;
     B1 := branches1 F;
     flatten apply(B1, b -> (
	       (tms1,F1) := toSequence b;
	       r := tms1#0#5; -- if > 1, then we must recurse
	       tmsall := join(tms,tms1);
	       b1 := new NewtonBranch from {tmsall, F1};
	       if r === 1 then 
	         {b1}
	       else
	         branches b1
	       ))
     )

puiseux = method()
puiseux(RingElement, ZZ) := (F, trunclimit) -> (
     P := (branches F)/(b -> extend(b,trunclimit));
     P/series
     )

testPuiseux = method()
testPuiseux(Sequence,RingElement,ZZ) := (parametrization, F, trunclimit) -> (
     R := ring F;
     S1 := ring (parametrization_0);
     Strunc := S1/S1_0^trunclimit;
     sub(F, {R_0=>sub(parametrization_0,Strunc), R_1=>sub(parametrization_1,Strunc)})
     )

test'puiseux = (F,trunclimit) -> (
     P := puiseux(F,trunclimit);
     Q := apply(P, p -> testPuiseux(p,F,trunclimit));
     assert all(Q, q -> q == 0)
     )

--<< "TODO:
--  1. factorization should work over extension fields
--  2. towers of extension fields needs to be set up.
--"

beginDocumentation()

doc ///
  Key
    adjoinRoot
    (adjoinRoot,RingElement)
  Headline
    adjoin an algebraic element to a field
  Usage
    a = adjoinRoot f
  Inputs
    f:RingElement
      An element of a ring A[t] (t can be any name), where A is a field
  Outputs
    a:RingElement
      An element in an extension field B of A, whose minimal polynomial is f(t)
  Description
   Text
     The variables are currently named a,b,c,...  In the future, more flexible names
     might be allowed.  The way this works is to take all of the variables in A, if any,
     and the new variable, and make one quotient ring out of it.  @TO toField@ is called
     to make the ring usable as a field, and for Groebner basis coefficient fields.
   Example
     R = QQ[t];
     a = adjoinRoot(t^2+t+1)
     A = ring a
     a^2+a+1
     S = A[t];
     b = adjoinRoot(t^3-a)
     b^6
  Caveat
    The polynomial does not have to be irreducible, but if it is not,
    then it is possible for some operations to give errors
  SeeAlso
    toField
///

doc ///
  Key
    newtonEdges
    (newtonEdges,RingElement)
    (newtonEdges,RingElement,RingElement)
  Headline
    find the supporting lines and polynomials of the lower Newton polygon
  Usage
    e = newtonEdges F
    e = newtonEdges(F,t)
  Inputs
    F:RingElement
      in a polynomial ring A[x,y] in two variables over a field (the names
      can be different)
    t:RingElement
      usually a variable in A[t].  If not present, then a new ring A[t] is created.
  Outputs
    e:List
      Each element is a NewtonEdge, i.e. a list of four elements: (a,b,m,g(t)).  The first three describe the 
      equation $aj + bi = m$ of one of the edges of the Newton polygon.  g(t) is the polynomial
      in t corresponding to this edge.
  Description
   Text
     The Newton polygon of $F(x,y)$ is the convex hull of the sets $(i,j) + N^2$, where $y^i x^j$
     occurs in $F$.  If $F(0,y)$ is non-zero, then the slopes of all of the edges
     of the Newton polygon are all non-positive.  We ignore the edges of slope infinity and zero.
   Example
     R = QQ[x,y];
     F = 5*x^3*y^2 - 7*x*y^4 + 2*x^2*y^4 - y^7 + 4*x^12*y + 3*x^15
     e = newtonEdges F
     netList e
   Text
     This Newton polygon has three edges.  The first edge lies, in the (i,j) plane (where
     a term $y^i x^j is placed at the $(i,j)$ spot), on the line $j + 6i = 15.  There are two terms
     of F which are non-zero, giving the polynomial $5t^2+3$.
  Caveat
    Currently, only negative slopes are considered, and it is expected that there is a monomial in F
    not involving x.
  SeeAlso
    puiseux
///

doc ///
  Key
    puiseux
    (puiseux,RingElement,ZZ)
  Headline
    compute Puiseux parametrizations of a polynomial F(x,y) monic in y
  Usage
    P = puiseux(F,trunclimit)
  Inputs
    F:RingElement
      F = F(x,y) should be a squarefree polynomial in a ring A[x,y], where A is a field
    trunclimit:ZZ
      power of t at which to truncate power series
  Outputs
    P:List
      a list of parametrizations (x(t), y(t))
  Description 
   Text
     Each parametrization $p = (x(t), y(t)$ satisfies that x(t), y(t) are in B[t], for some
     extension field B of A, and that $F(x(t),y(t)) == 0 mod t^{trunclimit}$.  Currently,
     $x(t) = t^q$, for some q.
   Example
     R = QQ[x,y]
     F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^6
     P = puiseux(F,10);
     netList P
   Text
     How do we determine what extension field is used for each series?
   Example
     ring P_1_0
     ring P_0_1
     ring P_2_1
   Text
     The first two Puiseux series are defined over QQ, and each represents a single series,
     while the third Puiseux series is defined over an extension of QQ of degree 3.  This
     series represents 3 different Puiseux series of F.
     
   Text
     Use @TO testPuiseux@ to see how good each parametrization is.
   Example
     testPuiseux(P_0, F, 14)
     testPuiseux(P_1, F, 14)
     testPuiseux(P_2, F, 14)
  Caveat
    Currently, the polynomial F(x,y) should have no monomial factors.
    The package needs to factor polynomials arising from the edges of the
    Newton polygon.  If it cannot do so in the given field, then a somewhat
    cryptic error message is given.  As factorization over extension fields
    improves, this problem should abate.
  SeeAlso
    testPuiseux
    newtonEdges
///

doc ///
  Key
    testPuiseux
    (testPuiseux,Sequence,RingElement,ZZ)
  Headline
    apply a (truncated) parametrization to a polynomial
  Usage
    testPuiseux(p,F,trunclimit)
  Inputs
    p:Sequence
      $(x(t),y(t))$, in some ring B[t]
    F:RingElement
      F(x,y), in some ring A[x,y]
    trunclimit:ZZ
      power at which to truncate power series
  Outputs
    :RingElement
      $F(x(t),y(t))$ mod $t^{trunclimit}$
  Description
   Text
     Substitute x(t) for x and y(t) for y, truncating the results.  If A is a field
     with variables, then these variables should be the first variables of B (as will
     happen in the parametrizations returned by @TO puiseux@).
   Example
     R = QQ[x,y]
     F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^6
     P = puiseux(F,10);
     netList P
     testPuiseux(P_0, F, 16)
     testPuiseux(P_1, F, 16)
     testPuiseux(P_2, F, 16)
  SeeAlso
    puiseux
///

TEST ///
  R = QQ[x,y]
  F = y^4-y^2+x^3+x^4
  P = puiseux(F,10)
  assert(#P == 2)
  debug Puiseux
  test'puiseux(F,10)
  test'puiseux(F,20)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,10)
  test'puiseux(F,10)
  test'puiseux(F,20)
///

TEST ///
  R = QQ[x,y]
  F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15 + y^9
  P = puiseux(F,10)
  assert(#P == 4)
  debug Puiseux
  test'puiseux(F,10)
  test'puiseux(F,20)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,10)
  test'puiseux(F,10)
  test'puiseux(F,20)
///

TEST ///
  -- Duval example
  R = QQ[x,y]
  F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"
  P = puiseux(F,10)
  assert(#P == 4)
  debug Puiseux
  test'puiseux(F,10)
  test'puiseux(F,20)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,10)
  test'puiseux(F,10)
  test'puiseux(F,20)
///

TEST ///
  -- Leonard example
  R = QQ[x,y]
  F = (y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11
  P = puiseux(F,10)
  debug Puiseux
  test'puiseux(F,10)
  test'puiseux(F,20)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,10)
  test'puiseux(F,10)
  test'puiseux(F,20)
///

TEST ///
  R = QQ[x,y]
  --vanHoeij2
  F = poly"y20+y13x+x4y5+x3(x+1)2"

  P = puiseux(F,10)
  debug Puiseux
  test'puiseux(F,10)
  test'puiseux(F,20)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,10)
  test'puiseux(F,10)
  test'puiseux(F,20)
///

TEST ///
  R = QQ[x,y]
  --vanHoeij3
  F = poly"y30+y13x+x4y5+x3(x+1)2"

  P = puiseux(F,40)
  debug Puiseux
  test'puiseux(F,40)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,40)
  test'puiseux(F,40)
///

TEST ///
  R = QQ[x,y]
  --vanHoeij4
  F = poly"y40+y13x+x4y5+x3(x+1)2"

  P = puiseux(F,40)
  debug Puiseux
  test'puiseux(F,40)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,40)
  test'puiseux(F,40)
///

TEST ///
  R = QQ[x,y]
  --boehm3
  F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5

  P = puiseux(F,40)
  debug Puiseux
  test'puiseux(F,40)

  R = ZZ/101[x,y]
  F = sub(F,R)
  P = puiseux(F,40)
  test'puiseux(F,40)
///

-- also doc: puiseux, possibly: singularParts, regularPart, termsToSeries

-- Some example polynomials to consider
{*

--F = y^4-y^2+x^3+x^4
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
--F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15 + y^9

--duval
F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"
--leonard1
F = (y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11

-- vanHoeij1
F = poly"y10+(-2494x2+474)y8+(84366+2042158x4-660492x2)y6
           +(128361096x4-4790216x2+6697080-761328152x6)y4
	   +(-12024807786x4-506101284x2+15052058268x6+202172841+134266087241x8)y2
	   +34263110700x4-228715574724x6+5431439286x2+201803238-9127158539954x10-3212722859346x8"
--vanHoeij2
F = poly"y20+y13x+x4y5+x3(x+1)2"
--vanHoeij3
F = poly"y30+y13x+x4y5+x3(x+1)2"
--vanHoeij4
F = poly"y40+y13x+x4y5+x3(x+1)2"
--boehm3
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5

*}

end


TEST ///
-- XXXX
restart
load "development/Puiseux.m2"
debug Puiseux
-- This example is from Duval, 1989
R = QQ[x,y]
S = QQ[t]
-- choose one of the F's from above
F = y^2-x^3-x^4
polygon(F,t)
F = y^4-y^2+x^3+x^4
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5

edges = polygon(F,t)

sx = singularPart1(F)
(t,ell,r) = sx_0
G = transformPolynomial(F,t,ell)

sx = singularParts(F,{}) 

px = puiseux(F,20)
-- Now test to see if each one is essentially 
--   a root of F(x,y).
testPuiseux(px_1_0,F,20)

///


TEST ///  --- adjoinRoots
restart
load "development/Puiseux.m2"
R1 = QQ[t]
a = adjoinRoot(t^2+t+1) -- a primitive cube root of 1
R2 = (ring a)[t]
b = adjoinRoot(t^4+1)
A = ring b
R = A[x,y]
F = (x+y)*(a*x+y)*(x+b*y)^2 + x^10
discriminant(F,y) -- it would be nice to be able to factor this!
squarefree oo
///

TEST ///  -- polygon
restart
load "development/Puiseux.m2"
R = QQ[x,y]
S = QQ[t]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
p = polygon(F,t)
netList p

debug Puiseux
e = polygon1(F,true)
assert(e === {{(0, 15), (2, 3)}, {(2, 3), (4, 1)}, {(4, 1), (7, 0)}}) -- check that this is correct!

F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15 + y^9
p = polygon(F,t)  -- WRONG!
assert(p == {(1,6,15,t^2+1),(1,1,5,t^2+1),(3,1,7,t+1)})
netList p

-- Let's make sure that we don't really care about the names of the variables
R = QQ[s,t]
S = QQ[u]
F = poly "s3t2+st4+s2t4+t7+s12t+s15"
p = polygon(F,u)

///

TEST ///  -- singularParts -- NOT DONE
restart
load "development/Puiseux.m2"
R = QQ[x,y]
S = QQ[t]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
p = polygon(F,t)
singularParts(F, {})
px = puiseux(F,20) -- FAILS
netList transpose reverse toList px

netList toList px
tm = termsToSeries px_2
use R
tm_0
S1 = ring tm_0
Strunc = S1/(S1_0^45)
sub(F, {x=>sub(tm_0,Strunc), y=>sub(tm_1,Strunc)})
netList p

debug Puiseux
e = polygon1(F,true)
assert(e === {{(0, 15), (2, 3)}, {(2, 3), (4, 1)}, {(4, 1), (7, 0)}}) -- check that this is correct!

F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15 + y^9
p = polygon(F,t)  -- WRONG!
assert(p == {(1,6,15,t^2+1),(1,1,5,t^2+1),(3,1,7,t+1)})
netList p

e = polygon1(F,true)
assert(p === {{(0, 15), (2, 3)}, {(2, 3), (4, 1)}, {(4, 1), (7, 0)}}) -- check that this is correct!

R = QQ[s,t]
F = poly "s3t2+st4+s2t4+t7+s12t+s15"
p = polygon(F,t)

///



------- BELOW this is probably not worth keeping --

restart
load "development/Puiseux.m2"
path = prepend("/Users/mike/src/M2/Macaulay2/packages/development/",path)
installPackage "Puiseux"
debug Puiseux
kk = ZZ/32003
A1 = kk[a]/(a^2-2)
A2 = A1[b]/(b^2-a)
toField A2
R = A2[y,x]
F = y^3-(a+1)*y^2-a*x*y-b*x^4
discriminant(F,y)
factor oo
value oo
F1 = sub(F, {x=>a*x, y=>x*(1-y)})


R = QQ[x,y]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
polygon F
edgesToInfo(oo,F,x)

K1 = QQ[a,b]/(a^2+1, b^2+b+1)
toField K1
R1 = K1[x,y]

time sub(F, {R_0 => x, R_1 => x^6*(a+y)}) // x^15
time tms = regularPart(oo,6)
y1 = termsToSeries prepend((1,1,6,a),tms)
y1 = sub(y1, t=>t^3)

time sub(F, {R_0 => x, R_1 => x^6*(-a+y)}) // x^15
time tms = regularPart(oo,6)
y2 = termsToSeries prepend((1,1,6,-a),tms)
y2 = sub(y2, t=>t^3)

time sub(F, {R_0 => x, R_1 => x*(a+y)}) // x^5
time tms = regularPart(oo,6)
y3 = termsToSeries prepend((1,1,1,a),tms)
y3 = sub(y3, t=>t^3)

time sub(F, {R_0 => x, R_1 => x*(-a+y)}) // x^5
time tms = regularPart(oo,6)
y4 = termsToSeries prepend((1,1,1,-a),tms)
y4 = sub(y4, t=>t^3)

time sub(F, {R_0 => x^3, R_1 => x*(-1+y)}) // x^7
time tms = regularPart(oo,6)
y5 = termsToSeries prepend((3,1,1,-1),tms)

time sub(F, {R_0 => x^3, R_1 => x*(-b+y)}) // x^7
time tms = regularPart(oo,6)
y6 = termsToSeries prepend((3,1,1,-b),tms)

time sub(F, {R_0 => x^3, R_1 => x*(b+1+y)}) // x^7
time tms = regularPart(oo,6)
y7 = termsToSeries prepend((3,1,1,b+1),tms)

K1 = QQ[a,b]/(a^2+1, b^2+b+1)
toField K1
R1 = K1[x,y]
1/a
1/b
1/(a*b)

S = Rtrunc[y,x, MonomialOrder=>Lex]
(y - sub(y1,S)) * (y-sub(y2,S)) * (y - sub(y3,S)) * (y-sub(y4,S))
(y - sub(y5,S)) * (y-sub(y6,S)) * (y-sub(y7,S))
     
use ring F
sub(F, {x=>t, y=>o14})
R = QQ[x,y]
F = y^4-y^2+x^3+x^4
polygon F
edgesToInfo(oo,F,x)

f1 = sub(F, {x=>x^2, y => x^3*(1+y)}) // x^6

time tms = regularPart(f1,20)
tms = prepend((2,1,3,1),tms)
termsToSeries(tms)
sub(F, {x=>t^2, y => oo})

R43 = QQ[x]/(x^43)
y1 = sum apply(#tms, i -> tms_i#3 * x^(2+2*i))

map(R43, R, {R43_0, y1})
oo f1
f2 = sub(F, {x=>x^2, y => x^3*(-1+y)}) // x^6
f11 = sub(f1, {y=>1/2 * x^2*(1+y)}) // x^2

sub(F, {x=>x^2, y => x^3 + 1/2 * x^5 + x^5*y}) // x^8
sub(F, {x=>x^2, y => x^3 + 1/2 * x^5 - 1/8*x^7 + 9/16*x^9 +  x^9*y}) // x^12

sub(F, {x=>x^2, y => -x^3 + x^3*y})  // x^6
sub(F, {x=>x^2, y => -x^3 - 1/2 * x^5 + x^5*y})  // x^8
sub(F, {x=>x^2, y => -x^3 + 1/2 * x^5 + 1/8*x^7 + x^7 * y})  


minpairs = apply(pairs partition(x -> x#1, polygon F), (k,v) -> (k, min apply(v, first)))



minpairs#0
ylo = (position(minpairs, x -> x#1 == 0))

lastpoint = 0;
slopes = for i from 1 to ylo-1 list slope(minpairs,lastpoint,i)
m = min slopes
thisedge = positions(slopes, x -> x == m)
lastpoint = max thisedge

min for i from 1 to ylo-1 list slope(minpairs,lastpoint,i)
     s := slope(lastpoint, i);
     if s > lastslope then (
	  
	  );
     )


restart
load "development/Puiseux.m2"
debug Puiseux
load "hermite.m2"
S = QQ[y,x,MonomialOrder=>Lex]
F = poly"y4-y2+x3+x4"

singularParts(F,{})

possibleDenominators(F,y)
K1 = QQ[a]/(a^4+2*a^3-4)
toField K1
S1 = K1 (monoid S)
G = sub(F, {S_1 => x - 2*a, S_0 => y})
S2 = K1[x,y]
G = sub(G,S2)
polygon G
edgesToInfo(oo,F,x)

restart
load "development/Puiseux.m2"
debug Puiseux
load "hermite.m2"

R = QQ[x,y]
F = y^4-y^2+x^3+x^4
polygon F
edgesToInfo(oo,F,x)
singularPart1 F
singularParts(F,{})
netList oo
adjoinRoot(t-3)

R = QQ[t]
a = adjoinRoot(t^2-3)

R1 = (ring a)[t]
b = adjoinRoot(t^3-a-3)

R2 = (ring b)[t]
c = adjoinRoot(t^2-a*t-b)

1/c
oo * c

-------------------------------------------
TEST ///
restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
polygon F
edgesToInfo(oo,F,x)
L = singularParts(F, {})
netList oo
regularPart(L_0_1,5)
termsToSeries oo
puiseux(F,10)
///
-------------------------------------------

-- 7/15/09, trying out some things by hand:
restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
F = poly"y4+y3x2+y2x5+yx9+x14"
E = newtonEdges F
apply(E, e -> factorization e#3)
F = poly"y4+y3x2+y2x15+yx19+x24"
sub(F, {y => x^2*(-1+y)})
oo//x^8

TEST ///
-- test applyTerm
restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
F = y^4-y^2+x^3+x^4
tm = new NewtonTerm from append(drop(first newtonEdges F,-1), 1_QQ)
applyTerm(tm, F)
///

TEST ///
-- test applyTerm
restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
S = QQ[t]
R = ZZ/32003[x,y]
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5

polygon(F,t)
F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"
F = y^2-x^3

F = y^4-y^2+x^3+x^4

netList branches F
(branches F)/(b -> extend(b,10))
P = oo/(b -> series(b#0))
P/(p -> testPuiseux(p, F, 40)) -- doesn't look good yet!
P#2


netList branches F



P = apply(branches F, B -> series B#0)

singularPart1 F
(newtonEdges F)/newtonTerms
apply(flatten((newtonEdges F)/newtonTerms), tm -> applyTerm(tm,F))
newtonTerms first newtonEdges F


restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
S = QQ[t]
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^6
polygon1(F,true)
polygon(F,t)
P = branches F
extend(P#0,10)
netList oo
series ooo
time P = puiseux(F,10)
netList oo
testPuiseux(P#0, F, 12)
testPuiseux(P#1, F, 12)
testPuiseux(P#2, F, 12)
time P = puiseux(F,20)
netList P
testPuiseux(P#0, F, 22)
testPuiseux(P#1, F, 22)
testPuiseux(P#2, F, 22)
time P = puiseux(F,30)
testPuiseux(P#0, F, 32)
testPuiseux(P#1, F, 32)
testPuiseux(P#2, F, 32)
///


restart
load "development/Puiseux.m2"
debug Puiseux
DoingRationalPuiseux = true
R = QQ[x,y]
F = poly"y4-2x3y2-4x5y+x6-x7"
branches F

puiseux(F,5) -- gives error msg
puiseux(F,3) -- seems to work

F = y^3-7*x^5
branches F
extend(oo_0, 10)
puiseux(F,5) -- seems to work
puiseux(F,10) -- gives error
restart
load "development/Puiseux.m2"
debug Puiseux
uninstallPackage "Puiseux"
installPackage "Puiseux"
check Puiseux
viewHelp
