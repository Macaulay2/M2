newPackage(
	"Invariants",
    	Version => "0.2", 
    	Date => "January 28, 2007",
    	Authors => {{Name => "Mike Stillman", Email => "mike@math.cornell.edu"}},
    	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "invariants of finite groups",
    	DebuggingMode => true
    	)

export {
     Group,
     newGroup,
     reynolds,
     molien,
     invariants,
     primaryInvariants,
     secondaryInvariants,
     kemper,
     derkson     
     }

needs "invariants2.m2"

-------------------
-- Finite groups --
-------------------

if Group === symbol Group then (
    Group = new Type of MutableHashTable;
    Group.synonym = "group";
)


length Group := (G) -> G.length
generators Group := opts -> (G) -> G.generators
ring Group := (G) -> G.ring
--------------
-- newGroup --
--------------

-- Dan: using the following equality check, instead of ==
-- changes the timing for newGroup of the Heisenberg 5 by 5 group
-- from 79.47 seconds to 19.33 seconds!
quickequals = (m,n) -> (
     sendgg(ggPush m, ggPush n, ggisequal);
     eePopBool())

newGroup = method()
newGroup List := (FF) -> (
     G = new Group;
     R := source FF#0;
     G1 = {map(R,R,vars R)};
     n := 0;  -- elements G1#0 .. G1#(n-1) have been multiplied by the f's.
     member := (f) -> (
	  found := false;
	  i := 0;
	  while i < #G1 and not found do (
	       if G1#i.matrix == f.matrix then found = true;
      	       --if quickequals(G1#i.matrix, f.matrix) then found = true;
	       i = i+1;);
	  found);
     while n < #G1 do (
	  scan(FF, f -> (
	      h := f * G1#n;
	      if not member(h) then (
	          G1 = append(G1, h);
		  << "." << flush;
		  )));
	  n = n+1;
	  );
     G.ring = R;
     G.elements = G1;
     G.generators = FF;
     G.length = #G1;
     G.invariants = new MutableHashTable;
     G)

newGroup List := (FF) -> (
     G = new Group;
     R := source FF#0;
     G1 := {map(R,R,vars R)};
     elems = new MutableHashTable from {first entries vars R => true};
     n := 0;  -- elements G1#0 .. G1#(n-1) have been multiplied by the f's.
     member := (f) -> (
          felems := first entries f.matrix;
	  found := elems#?felems;
	  if not found then elems#felems = true;
	  found
	  );
     while n < #G1 do (
	  scan(FF, f -> (
	      h := f * G1#n;
	      if not member(h) then (
	          G1 = append(G1, h);
		  << "." << flush;
		  )));
	  n = n+1;
	  );
     G.ring = R;
     G.elements = G1;
     G.generators = FF;
     G.length = #G1;
     G.invariants = new MutableHashTable;
     G)

------------------------------------------------
-- Reynolds operator (average over the group) --
------------------------------------------------

reynolds = method()
reynolds (Group, Matrix) := 
reynolds (Group, RingElement) := (G,f) -> (
     g := sum(G.elements, F -> F f);
     g//(#G))
reynolds2 = (G,f) -> (
     result := map(target f, source f, 0);
     scan(G.elements, F -> result = result + F f);
     result//(#G))

-- This version is currently the fastest of these versions, using the least
-- amount of memory.
reynolds (Group, Matrix) := 
reynolds (Group, RingElement) := (G,f) -> (
     n := 1;
     result := G.elements#0 f;
     while n < #G.elements do (
	  result = result + (G.elements#n) f;
	  n = n+1;);
     collectGarbage();
     result//(#G))

-------------------
-- Molien series --
-------------------


molien = method()
molien1 = (G) -> (
     z := symbol z;
     A := (coefficientRing R)[z];
     one = (F) -> (
	  m := substitute(jacobian F.matrix, A);
	  det(1 - (A_0)*m));
     sum(G.elements, F -> 1/(one(F)) // (#G.elements))
     )
molien2 = (G) -> (
     z := symbol z;
     A := (coefficientRing R)[z];
     a := substitute(G.primitive,A);
     makeLogs a;
     B = G.char0Ring;
     one = (F) -> (
	  m := substitute(jacobian F.matrix, A);
	  brauer(det(1 - (A_0)*m),B));
     sum(G.elements, F -> 1/(one(F)) // (#G.elements))
     )

molien3 = (G) -> (
     H := G.charPolynomials;
     sum(pairs H, (f,n) -> n/f))
molien4 = (G) -> (
     if not G.?charPolynomials then
         charPolys G;
     H := G.charPolynomials;
     n := 1;
     p := pairs H;
     F := (p#0#1)/(p#0#0);
     while n < #p do (
	  F = F + (p#n#1)/(p#n#0);
	  n = n+1;
	  );
     F//(#G.elements))
	  
charpoly = (f,A) -> (
     fm := f.matrix;
     nr := numgens source fm - numgens ring fm;
     if nr > 0
       then fm = fm_{nr..numgens source fm-1};
     m := substitute(jacobian fm, A);
     det(1 - (A_0)*m))

charPolys = (G) -> (
     -- Create a hash table with all of the unique polynomials
     -- as keys, and the number of such elements as values.
     z := symbol z;
     A := (coefficientRing ring G)[z];
     H = new MutableHashTable;
     n := 0;
     while n < #G.elements do (
	  f := charpoly(G.elements#n, A);
	  prev := keys H;
	  m := 0;
	  found := false;
	  while m < #prev and not found do (
	       if f == prev#m 
	       then found = true
	       else m = m+1;
	       );
	  if not found
	  then H#f = 1
	  else H#(prev#m) = H#(prev#m) + 1;
	  << "." << flush;
	  n = n+1;
	  );
     G.charPolynomials = H;
     H
     )
	       
	  
molien Group := (G) -> (
     if not G.?molien then (
	  local MS;
	  if not G.?char0Ring or not G.?primitive then
	      G.molien = molien4 G
	  else (
	      MS = molien2 G;
	      C = QQ[symbol t];
	      G.molien = (substitute(numerator MS,C))/
	          (substitute(denominator MS,C));
	      )
          );
     G.molien
     )

makeLogs = (a) -> (
     logtable = new MutableHashTable;
     A := ring a;
     n := 1;
     logtable#1 = 0;
     b := a;
     while b != 1_A do (
	  x := lift(b,ZZ);
	  logtable#x = n;
	  b = b*a;
	  n = n+1;)
     )

brauer = (f,B) -> (
     A := ring f;
     z := A_0;
     a := B_0; --(coefficientRing B)_0;
     t := B_1;
     facs = factor(f);
     expand apply(facs, f -> (
	 if degree f#0 === {0}
	 then lift(f#0,ZZ)
	 else (
	     x := lift(z-f#0,ZZ);
	     (t - a^(logtable#x))^(f#1)))))
     

///
    -- Heisenberg group of order 9
    G.char0Ring = QQ[b,t]/(b^2+b+1);
    G.primitive = 5_(ring G)
    --A = ZZ/31[z]
    --G.primitive = 5_A;
    B = QQ[b,t]/(b^2+b+1)
    f = one(G.elements#10)
    expand brauerLift(f,B)
    
    -- Molien series in char 0
    R = QQ[a]/(a^2+a+1)[z]
///    


-- Expected number of invariants:
expected = (G,d) -> (
     -- Assume that the finite field is large enough to
     -- detect the correct number of invariants.
     F := molien G;
     F1 := expandPowerSeries(F,d+1);
     z := (ring F1)_0;
     p := char (coefficientRing ring G);
     ans := lift(F1_(z^d),ZZ);
     if ans < 0 then ans = ans+p;
     ans)

-- Expected number of primary + secondaries, given a set of primaries
expected = (G,primaries,d) -> (
     F := expandPowerSeries(molien G,d+1);
     z := (ring F)_0;
     if #primaries > 0 then 
         F = F * product(primaries, i -> 1-z^i);
     ans := lift(F_(z^d),ZZ);
     if ans < 0 then (
	  p := char (coefficientRing ring G);
     	  ans = ans+p;
	  );
     ans)
     
     
---------------------------------------------
-- Generating invariants of a given degree --
---------------------------------------------

groupElement := (F,monoms) -> 
     substitute(contract(transpose monoms, F monoms), ZZ/32003)

invariants1 = method()
invariants1(Group,ZZ) := (G, d) -> (
     R := ring G;
     m := basis(d,R);
     p := matrix apply(G.generators, g -> (
		    m1 := groupElement(g, m);
		    {m1 - id_(target m1)}));
     m * substitute (syz p, R))

invariants = method()	
invariants(Group,ZZ) := (G,d) -> (
     m := basis(d,R);
     mingens ideal reynolds(G,m))

invcount = 50
degn = (d,R) -> matrix basis(d,R)
invariants(Group,ZZ,ZZ) := (G,d,expected) -> (
     dn = degn(d,ring G);
     n := numgens source dn;
     m := n // invcount;
     x := 0;
     I = ideal(0_(ring G));
     i := 0;
     while i <= m and numgens I < expected do (
	 y := min(x+invcount,n-1);
	 --m1 := simplify(reynolds(G,dn_{x..y}),2);
	 m1 := reynolds(G,dn_{x..y});
	 x = x+invcount;
	 I = ideal mingens (ideal(m1) + I);
	 << "0.." << y << " = " << numgens I << endl;
	 i = i+1;
	 );
     I)
invariants(Group,ZZ,ZZ) := (G,d,expected) -> (
     dn = degn(d,ring G);
     n := numgens source dn;
     m := n // invcount;
     x := 0;
     I = ideal(0_(ring G));
     i := 0;
     while i <= m and numgens I < expected do (
	 y := min(x+invcount,n-1);
	 -- This removes all zero elements
	 m1 := time simplify(reynolds(G,dn_{x..y}),1);
	 x = x+invcount;
	 I = I + ideal(m1);
	 if numgens I >= expected then
	     I = ideal time mingens I;
	 << "0.." << y << " = " << numgens I << endl;
	 i = i+1;
	 );
     I)

invariants(Group,ZZ) := (G,d) -> (
     e := expected(G,{},d);
     invariants(G,d,e))

invariants(Group,ZZ,ZZ) := (G,d,expected) -> (
     if G.invariants#?d then G.invariants#d else (
     dn = degn(d,ring G);
     n := numgens source dn;
     m := n // invcount;
     x := 0;
     R := ring G;
     I := ideal map(R^1,R^0,0);
     while x < n and numgens I < expected do (
	 y := min(x+invcount,n-1);
	 -- This next removes all zero elements
	 m1 := compress reynolds(G,dn_{x..y});
	 -- OLD: m1 := simplify(reynolds(G,dn_{x..y}),1);
	 I = I + ideal(m1);
	 if numgens I >= expected then
	     I = ideal time mingens I;
	 << "0.." << y << " = " << numgens I << endl;
	 x = y+1;
	 );
     G.invariants#d = I))
     

------------------------
-- Primary invariants --
------------------------

-- Constructing primary invariants, (and secondary ones, as we go...)
-- At each step: have primaries = (p1, ..., pi)
--                    secondaries = (s1, ..., sj)
--                    degree
-- At the next step, degree d, compute a set of elements (random, or not).
-- We need at least n-i of them.

numInvariants = (G,d,P) -> (
     R := ring P;
     H := molien G;
     H = expandPowerSeries(H,d+1);
     z := (ring H)_0;
     H = H * product(numgens P, i -> (
	       e := (degree P_i)#0;
	       1-z^e));
     a := lift(H_(z^d),ZZ);
     if a < 0 then a = a + char R)

-- This next routine is a cheap shot: try to find a subset of 'm'
-- that contributes to the 'expected' codim
-- Grab the next index of m randomly
findInvariants = (I, m, expected) -> (
     i := 0;
     n := numgens source m;
     v := randomPermutation n;
     while i < n and numgens I < expected do (
	  I1 := I + ideal(m_{v#i});
	  -- check whether I1 is a complete intersection:
	  if isCompleteIntersection I1 then
	       I = I1;
	  i = i+1;
	  );
     I)

primaryInvariants = (G) -> (
     -- The idea is easy: start with some degree d0 (possibly 1).
     -- set primaries := invariants in this degree.
     -- for each degree d:
     --    compute a vector space basis of invariants outside the sub algebra 
     --       generated by the primaries.
     --    compute the codimension of this set with the primaries
     --    the number of primaries taken may be set to be this number.
     --    find this set, either randomly, or in a special way.
     --    if we now have the correct number of invariants, stop, else continue.
     R := ring G;
     n := numgens R;
     d := 0;
     --I = ideal invariants(G,d);   -- MES: wrong!! 
                                  -- Also: put in Molien series check...
     I = ideal map(R^1, R^0,0);
     while numgens I < n do (
	  d = d+1;
	  << "degree " << d;
	  -- The next (commented out) line seems to be slower...
          -- m1 = matrix basis(d,coker subAlgebraBasis(gens I,d));
          m1 = matrix basis(d,R);
	  m1 = mingens ideal reynolds(G,m1);
	  << " has " << numgens source m1 << " possible elements" << endl;
	  -- Next, compute the number of new primary invariants:
	  I1 := I + ideal(m1);
	  c = codim I1;
	  expected := c - numgens I;
	  << "expect " << expected << " new primary invariants in degree " << d << endl;
	  -- Special case: if this expected number is the number of elements of m1, take
	  -- them all. Second special case: if the codim of I1 is n, then first try to
	  -- find invariants from the list
	  if expected == numgens source m1 then 
	      I = I1
	  else if expected > 0 then (
	      if c == n+1 then
	          I = findInvariants(I, m1, expected);
	      -- now finish by adding random combinations
	      if c - numgens I > 0 then 
	          I = I + ideal randomColumns(m1, c - numgens I);
     	  ));
     -- Now we check that we have a c.i. of length n
     -- If not, we give a warning message, and try again.
     if not isCompleteIntersection I then (
	  << "warning! random choices we made failed to be generic!  trying again..." << endl;
	  I = primaryInvariants G;
	  );
     I)

primaryInvariants = (G) -> (
     if G.?primaryInvariants then G.primaryInvariants else (
     R := ring G;
     n := numgens R;
     d := 0;
     I = ideal map(R^1, R^0,0); -- primary invariants
     while numgens I < n do (
	  d = d+1;
	  ninvariants := expected(G,{},d);
	  if ninvariants > 0 then (
	      << "degree " << d << " #invariants " << ninvariants <<  endl;
	      m1 := invariants(G,d,ninvariants);
	      I1 := I + m1;
	      c = codim I1;
	      nprimaries := c - numgens I;
	      << "expect " << nprimaries << " new primary invariants in degree " << d << endl;
	      if nprimaries == numgens m1 then 
	          I = I1
	      else (
		   I = findInvariants(I, gens m1, nprimaries);  -- MES: changed 'expected' to 'nprimaries'
	      	   if c - numgens I > 0 then 
	               I = I + ideal randomColumns(gens m1, c - numgens I);
     	           );
	      );
	 );
    -- Now we check that we have a c.i. of length n
    -- If not, we give a warning message, and try again.
    if not isCompleteIntersection I then (
	 << "warning! random choices we made failed to be generic!  trying again..." << endl;
	 I = primaryInvariants G;
	 );
    G.primaryInvariants = I))

newSecondaryInvariants = (G,d) -> (
     -- returns: new secondaries
     -- idea: work mod I, take a min set of generators of m1.
     -- this should be the 'expected' number.
     -- We should not recompute a GB of I, especially since we only need
     -- it up to degree d.
     R := ring I;
     A := R/I;
     m = gens invariants(G,d);
     s1 = syz (substitute(m,A), DegreeLimit=>d);
     s1 = substitute(s1,R);
     sec = leadTerm syz transpose s1;
     sec = substitute(sec,R);
     m*sec
     )

secondaryInvariants = (G) -> (
     if not G.?numSecondaryInvariants then (
	 -- First find the polynomial of secondaries:
	 H := molien G;
	 I := primaryInvariants G;
	 z := (ring H)_0;
	 H = H * product(degrees source gens I, i -> 1-z^(i#0));
	 H = numerator H;
	 SS := new MutableHashTable;
	 while H != 0 do (
	      h := leadTerm H;
	      H = H-h;
	      SS#((degree h)#0) = 
	          lift(leadCoefficient h, ZZ);
	      );
	 G.numSecondaryInvariants = SS;
         );
     if not G.?secondaryInvariants then 
         G.secondaryInvariants = new MutableHashTable;
     degs := keys G.numSecondaryInvariants;
     scan(degs, d -> (
	 if not G.secondaryInvariants#?d then (
	      G.secondaryInvariants#d = 
     	          newSecondaryInvariants(G,d))));
     G.secondaryInvariants)

kemper = (G, P, S) -> (
     -- G is the group
     -- P is the matrix of primaries
     -- S is the matrix of secondaries for a subgroup H.
     -- We should really have a set of coset reps for H in G.
     -- Output: The secondary invariants.
     -- Assumption: order of H is invertible in the base field.
     -- But the representation may be modular.
     --
     -- Step 1. Form the matrix for syzygies
     M1 = matrix apply({F1,F2}, F -> { F S - S });
     -- Step 2. Compute the syzygies
     s1 = syz M1;
     -- Step 3. Compute the elimination
     R = ring P;
     k := coefficientRing R;
     M := monoid [Variables=>numgens source P, Degrees=>degrees source P];
     A = k M;
     phi = map(R,A,P);
     inv = pushforward1(phi,coker s1);
     -- Step 4. Recompute the answer: multiply this by S.
     S * substitute(inv, vars R)
     )

TEST ///
    R = ZZ/31[x_0 .. x_2]
    a = 5_R
    F1 = map(R,R,{x_1, x_2, x_0})
    F2 = map(R,R,{x_0, a*x_1, a^2*x_2})
    G = newGroup {F1,F2}
    P = gens time primaryInvariants G
    S = map(R^1,,matrix basis(coker P))
    kemper(G,P,S)
///

derkson = (G) -> (
     R := ring G;
     n := numgens R;
     k := coefficientRing R;
     M := monoid [Variables=>2*n];
     S := k M;
     v0 := genericMatrix(S,S_0, 1, n);
     v1 := genericMatrix(S,S_(n), 1, n);
     gelem := (F) -> ideal(v1 - substitute(F vars R, v0));
     G1 := apply(G.elements, gelem);
     JJ1 := intersect toSequence G1;
     JJ := substitute(JJ1, vars R | map(R^1, R^n, 0));
     JJ = compress gens JJ;
     reynolds(G,JJ))

beginDocumentation()
document {
     Key => Group,
     TT "Group", " -- the type of finite group representations.",
     PARA{},
     "In Macaulay2, a group acts on a polynomial ring R, and is generated
     by a collection of ring maps f : R <-- R, which sends the variables to
     homogeneous linear polynomials.",
     PARA{},
     "A group is created using the ", TO "newGroup", " routine.  Operations
     involving groups:",
     UL {
	(TO "newGroup", "(FF) -- create a (finite!) group"),
	(TO "ring", "(G)      -- the base ring R mentioned above"),
	(TO "generators", "(G)-- the list of generators (each is a ring map)"),
	(TO "G.elements", "   -- a list of all the elements in the group"),
	(TO "length","(G)     -- the order of the group"),
	},
     "Operations involving computing invariants of groups.  Some of these operations
     store their result, or partial result, into G (under a key of the same name).",
     UL {
	(TO "reynolds","(G,F) -- average the polynomial or matrix over F"),
	(TO "molien","(G)     -- the Molien series of the given representation"),
	(TO "invariants","(G,d) -- a basis of the invariants of degree d"),
	(TO "invariants1","(G,d)-- a basis of the invariants of degree d"),
	(TO "primaryInvariants", "(G) -- a set of primary invariants of G"),
	(TO "secondaryInvariants", "(G) -- a set of secondary invariants of G"),
	(TO "secondaryInvariants", "(G,d) -- a set of secondary invariants of G of degree d")
	},
     "Some supporting routines, which might be of independent use",
     UL {
	(TO "charPolys", "(G) -- compute det(1-zg), for all g in the group")
	},
     "For a series of examples of interesting groups, see the file 'examples/groups.m2'
     in the Macaulay2 directory tree"
   }

document {
     Key => newGroup,
     TT "newGroup(FF)", " -- create the group generated by the
     list of ring maps FF.  Each ring map should be of the form
     R <-- R, and be the matrix of a invertible change of variables.
     The group which is generated by these elements should be finite as well.",
     PARA{},
     Caveat => "Currently, every element of the group is constructed.  So if
     your group is large, this will be very time-consuming and space-consuming.",
     SeeAlso => "Group"     
     }

document {
     Key => reynolds,
     TT "reynolds", "(Group,Matrix)      -- average each entry of matrix over the group",
     BR{},
     TT "reynolds", "(Group,RingElement) -- average the element over the group",
     PARA{},
     "In the case when the order of the group  G is invertible in the ring R of G,
     this produces the average:  sum_(g in G) F(gx), where F(x) is a matrix or ring element.
     In the case when the group order is not invertible, use ", TO "invariants", 
     " to find invariants.",
     PARA{},
     Caveat => "We currently do the stupid thing, and sum up over all elements of
     the group.  One should simply sum over the conjugacy classes, but we don't
     yet compute this information."
     }
     
document {
     Key => molien,
     TT "molien", "(G) -- compute the Molien series of G",
     PARA{},
     "In characteristic 0, the Molien series of G is defined to be the series
     sum_(g in G) 1/det(1-zg), where z is a variable, and g is the matrix
     of g acting on the linear forms in the ring R of G.  This is also the
     rational function in z, whose coefficient of z^d is the dimension of
     invariants of G on degree d forms of R",
     PARA{},
     "For characteristic p, the Molien series is still defined as long
     as the order of the group is invertible in R.  In this case, one must
     do a so-called 'Brauer lift'.",
     PARA{},
     "To obtain numerical information from this series, use ",
     UL {
	  (TO "expandPowerSeries", "(f,n) -- expand the rational function f in k(z)
	       as a polynomial, whose degree >= n terms have been truncated"),
	  (TO "")
     	  },
     Caveat => "The Brauer lift still needs to be written."
     }

document {
     Key => invariants,
     TT "invariants", "(G,d) -- find a basis of the invariants of degree d",
     PARA{},
     "There are two versions of this routine.  The first uses the Reynolds operator
     to find the invariants, while the second version uses linear algebra.  While
     the second version is typically slower, it works for modular representations
     (when the order of the group is not a unit in the ring R of G)."
     }


end
restart
loadPackage "Invariants"

------------------------------------------------------------
-- kemper7: S3 acting on k[V + V + V + V], dim V = 3.
R = ZZ/32003[x_1 .. x_12]  
F1 = map(R,R,{x_2, x_1, x_3, x_5, x_4, x_6, x_8, x_7, x_9, x_11, x_10, x_12})
F2 = map(R,R,{x_3, x_1, x_2, x_6, x_4, x_5, x_9, x_7, x_8, x_12, x_10, x_11})
G = newGroup {F1,F2}
length G
time molien G
primaryInvariants G
------------------------------------------------------------
-- example: 5x5 heisenberg group in a finite characteristic
loadPackage "Invariants"
R = ZZ/71[x_0 .. x_4]
a = 5_R      -- a primitive 5th root of 1
F1 = map(R,R,{x_4,x_0,x_1,x_2,x_3})
F2 = map(R,R,{x_0, a*x_1, a^2*x_2, a^3*x_3, a^4*x_4})
time G = newGroup {F1,F2}
time molien G
time primaryInvariants G
time secondaryInvariants G
------------------------------------------------------------
///     
R = ZZ/32003[x_1 .. x_12]  
f1 = matrix{{x_2, x_1, x_3, x_5, x_4, x_6, x_8, x_7, x_9, x_11, x_10, x_12}}
f2 = matrix{{x_3, x_1, x_2, x_6, x_4, x_5, x_9, x_7, x_8, x_12, x_10, x_11}}
F1 = map(R,R,f1)
F2 = map(R,R,f2)
G = newGroup {F1,F2}
peek G
primaryInvariants G

reynolds(G,x_1)
molien G
expandPowerSeries((molien G)*(1-(ring molien G)_0)^4, 5)


inv1 = invariants(G,1)
I1 = ideal inv1
ret = nextDegree(G,2,I1)
I2 = findInvariants(I1,ret#2,ret#1)
codim I2
ret = nextDegree(G,3,I2)

I2 = I1 + ideal randomColumns(ret#2, ret#1)
ret = nextDegree(G,3,I2)
I3 = I2 + ideal randomColumns(ret#2, ret#1)

S = ZZ/32003[x_1 .. x_12, y_1 .. y_12]
v1 = genericMatrix(S,y_1,1,12)
gelem = (F) -> ideal(v1 - substitute(F vars R, S))
G1 = apply(G, gelem)
gbTrace = 3
JJ = intersect toSequence G1;
JJ1 = substitute(JJ, vars R | map(R^1, R^12, 0));
JJ1 = compress gens JJ1
transpose JJ1

reynolds = (f) -> sum(G, F -> F f)
INV = reynolds JJ1
INV_{select(0..numgens source INV-1, i -> degree INV_i <= {2})}
///

