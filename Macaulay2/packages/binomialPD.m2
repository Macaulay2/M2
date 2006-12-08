-- For computing saturations w.r.t. a single variable:
--
axisSaturate = (m,i) -> (
     -- Returns a list {(m : x_i), s}, where m : x_i^* = m : x_i^s.
     -- First change the ring so that the last variable is x_i:
     -- PROBLEMS: (1) degrees need to be changed as well.
     -- (2) degree needs to be found
     -- (3) use binomial code.
     R := ring m;
     n := numgens R;
     if i < 0 or i >= n then error "variable out of range";
     k := coefficientRing R;
     M := monoid [Variables=>n,MonomialSize=>16];
     R1 = k M;
     if i === n-1 then (
	 perm1 = map(R1,R,vars R1);
	 perm2 = map(R,R1,vars R);)
     else if i === 0 then (
	  p1 = splice {n-1, 1..(n-2), 0};
	  p2 = splice {n-1, 1..(n-2), 0};
	  perm1 = map(R1,R, apply(p1, i -> R1_i));
  	  perm2 = map(R,R1, apply(p2, i -> R_i));
	  )
     else (
	 p1 = splice {0 .. (i-1), n-1, (i+1) .. (n-2), i};
	 p2 = splice {0 .. (i-1), n-1, (i+1) .. (n-2), i};
	 perm1 = map(R1,R, apply(p1, i -> R1_i));
  	 perm2 = map(R,R1, apply(p2, i -> R_i));
	 );
     m1 = perm1 m;
     m1gb := gens gb m1; -- use the binomial code here.
     m2 = perm2 divideByVariable(matrix entries m1gb, R1_(n-1));
     -- now determine the degree, since it is not returned in the above
     inm = leadTerm m1gb;
     degs = matrix entries substitute(inm, matrix{splice{(n-1):1,R1_(n-1)}});
     degs = max degrees source degs;
     {degs#0, mingens ideal m2}
     )
///
-- Testing axisSaturate
R = ZZ/101[a..d]
m = matrix{{a^3*d-b^3*c, a^4-d^4, a*c-b*d}}
binomialPD ideal m
axisSaturate(m,0)
///
----------------------------------------------
-- Primary decomposition of binomial ideals --
----------------------------------------------

binomialPD = (I) -> (
     -- The result is a list of (P,Q), where P is a binomial associated prime,
     -- and Q is the corresponding primary (binomial) ideal.  At the moment, this
     -- might be a redundant list? (I need to think about it).
     --
     -- ToDo list: list of {{vars to invert by}, {vars left to try}, Q}
     -- Answer: list of Q.
     R := ring I;
     n := numgens R;
     Answer = {};
     IntersectAnswer = ideal(1_R);
     ToDo = {{1_R,toList(0..n-1),I}};
     next = () -> (
	 if #ToDo === 0 then false
	 else (
	      L = ToDo#0;
	      ToDo = drop(ToDo,1);
	      if gens IntersectAnswer % L#2 == 0
	      then (<< "redundant component" << endl;)
	      else if #(L#1) === 0 then (
		   -- We have an answer
		   newone := trim L#2;
		   << "component: " << gens newone << endl;
		   Answer = append(Answer,newone);
		   IntersectAnswer = intersect(IntersectAnswer,newone);
		   if IntersectAnswer == I then ToDo = {})
	      else (
	           i := L#1#0;
		   newL1 = drop(L#1,1);
	           result = axisSaturate(gens L#2, i);
		   J := ideal result#1;
		   k := result#0;
		   if k > 0 then (
			J2 = L#2 + ideal(R_i^k);
			-- We need to remove any components supported on the first vars
			J2 = saturate(J2, L#0);
			if J2 != 1 then
			    ToDo = prepend({L#0, newL1, J2},ToDo));
		   if J != ideal(1_R) then ToDo = prepend({R_i * L#0, newL1, J},ToDo);
		   );
	      true));
     while next() do ();
     Answer	      
     )

----------------------------------------------
-- Radical of a binomial ideal ---------------
----------------------------------------------
simplify = (m,n) -> (
     sendgg(ggPush m, ggPush n, ggsimplify);
     getMatrix ring m)

binomialMinprimes = (I) -> (
     -- The result is a list of (P,Q), where P is a binomial associated prime,
     -- and Q is the corresponding primary (binomial) ideal.  At the moment, this
     -- might be a redundant list? (I need to think about it).
     --
     -- ToDo list: list of {{vars to invert by}, {vars left to try}, Q}
     -- Answer: list of Q.
     I = ideal simplify(gens I,3);  -- pare down to squarefree monomial
     R := ring I;
     n := numgens R;
     Answer = {};
     IntersectAnswer = ideal(1_R);
     ToDo = {{1_R,toList(0..n-1),I}};
     next = () -> (
	 if #ToDo === 0 then false
	 else (
	      L = ToDo#0;
	      ToDo = drop(ToDo,1);
	      if gens IntersectAnswer % L#2 == 0
	      then (<< "redundant component" << endl;)
	      else if #(L#1) === 0 then (
		   -- We have an answer
		   newone := trim L#2;
		   << "component: " << gens newone << endl;
		   Answer = append(Answer,newone);
		   IntersectAnswer = intersect(IntersectAnswer,newone);)
	      else (
	           i := L#1#0;
		   newL1 = drop(L#1,1);
	           result = axisSaturate(gens L#2, i);
		   J := ideal result#1;
		   k := result#0;
		   if k > 0 then (
			J2 = L#2 + ideal(R_i);
			J2 = ideal simplify(gens J2, 3);
			-- We need to remove any components supported on the first vars
			J2 = saturate(J2, L#0);
			J2 = ideal simplify(gens J2, 3);
			if J2 != ideal(1_R) then
			    ToDo = prepend({L#0, newL1, J2},ToDo));
		   if J != ideal(1_R) then ToDo = prepend({R_i * L#0, newL1, J},ToDo);
		   );
	      true));
     while next() do ();
     Answer	      
     )

///
-- Is this a gc bug?
--------------
-- Test #1A --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"
R = ZZ/101[a..e,A..E]
I = ideal(a*c*B^2-b^2*A*C, b*d*C^2-c^2*B*D, c*e*D^2-d^2*C*E)
J = I + ideal(
     b*c*A*D-a*d*B*C,
     c*d*B*E-b*e*C*D,
     b*d*A*E-a*e*B*D,
     c^2*A*E-a*e*C^2)

Jsat = saturate(J,a*b*c*d*e*A*B*C*D*E)  -- all 16 generators
J1 = J:Jsat
intersect(J1,Jsat) == J
time binomialPD J1    -- time(with battery, NEC 6200MX): 33.1 seconds

R = ZZ/32003[a..h]
I = ideal(
  a*(a*d-b*c),
  b*(a*c-b^2),
  c*(b*d-c^2))
time binomialPD I     -- time(with battery, NEC 6200MX): 43.17 seconds
--------------
-- Test #1B --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"

R = ZZ/32003[a..h]
I = ideal(
  a*(a*d-b*c),
  b*(a*c-b^2),
  c*(b*d-c^2))
time binomialPD I     -- time(with battery, NEC 6200MX): 5. seconds
--------------
-- Test #1C --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"
R = ZZ/101[a..e,A..E]
I = ideal(a*c*B^2-b^2*A*C, b*d*C^2-c^2*B*D, c*e*D^2-d^2*C*E)
J = I + ideal(
     b*c*A*D-a*d*B*C,
     c*d*B*E-b*e*C*D,
     b*d*A*E-a*e*B*D,
     c^2*A*E-a*e*C^2)

Jsat = saturate(J,a*b*c*d*e*A*B*C*D*E)  -- all 16 generators
J1 = J:Jsat
intersect(J1,Jsat) == J
time binomialPD J1    -- time(with battery, NEC 6200MX): 33.1 seconds (2nd time through: 28.94 sec)

time collectGarbage() -- time 2.47 seconds
R = ZZ/32003[a..h]
I = ideal(
  a*(a*d-b*c),
  b*(a*c-b^2),
  c*(b*d-c^2))
time binomialPD I     -- time(with battery, NEC 6200MX): 27.59 seconds

--------------
-- Test #2A --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"
R = ZZ/101[a..e,A..E]
I = ideal(a*c*B^2-b^2*A*C, b*d*C^2-c^2*B*D, c*e*D^2-d^2*C*E)
J = I + ideal(
     b*c*A*D-a*d*B*C,
     c*d*B*E-b*e*C*D,
     b*d*A*E-a*e*B*D,
     c^2*A*E-a*e*C^2)

Jsat = saturate(J,a*b*c*d*e*A*B*C*D*E)  -- all 16 generators
J1 = J:Jsat
intersect(J1,Jsat) == J
time binomialPD J1    -- time: 32.89 seconds

R = ZZ/32003[a..h]
I = ideal(
  a*d*e*g-b^2*f*h, 
  -a^2*b*f+a^2*c*g, 
  -d*f^3+a^2*c*h)
time binomialPD I     -- time: 298.26 seconds

--------------
-- Test #2B --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"

R = ZZ/32003[a..h]
I = ideal(
  a*d*e*g-b^2*f*h, 
  -a^2*b*f+a^2*c*g, 
  -d*f^3+a^2*c*h)
time binomialPD I     -- time: 109.61 seconds

--------------
-- Test #2C --
--------------
restart
load "/home/mike/src/M2/Macaulay2/mike/binomialPD.m2"
R = ZZ/101[a..e,A..E]
I = ideal(a*c*B^2-b^2*A*C, b*d*C^2-c^2*B*D, c*e*D^2-d^2*C*E)
J = I + ideal(
     b*c*A*D-a*d*B*C,
     c*d*B*E-b*e*C*D,
     b*d*A*E-a*e*B*D,
     c^2*A*E-a*e*C^2)
J = I + ideal(
     b*c*A*D-a*d*B*C,
     c*d*B*E-b*e*C*D,
     b*d*A*E-a*e*B*D)
time binomialPD J
Jsat = saturate(J,a*b*c*d*e*A*B*C*D*E)  -- all 16 generators
J1 = J:Jsat
intersect(J1,Jsat) == J
time binomialPD J1    -- time: 33.92 seconds

time collectGarbage() -- time: 2.2 seconds

R = ZZ/32003[a..h]
I = ideal(
  a*d*e*g-b^2*f*h, 
  -a^2*b*f+a^2*c*g, 
  -d*f^3+a^2*c*h)
time binomialPD I     -- time: 258.4 seconds
///


///
gbTrace = 3
R = ZZ/32003[a..h]
I = ideal(
  a*(a*d-b*c),
  b*(a*c-b^2),
  c*(b*d-c^2))
time binomialPD I

gbTrace = 3
R = ZZ/32003[a..h]
I = ideal(
  a*d*e*g-b^2*f*h, 
  -a^2*b*f+a^2*c*g, 
  -d*f^3+a^2*c*h)
time binomialPD I

-- Answer should be this:
{ideal(b*f-c*g,a*d*e-b*c*h,d*f^2*g-a^2*b*h,d*f^3-a^2*c*h,a^3*e-c*f^2*g,d^2*e*f*g^2-a*b^3*h^2,d^3*e^2*g^3-b^5*h^3), ideal(h,g,f), ideal(h,d,b*f-c*g), ideal(g,f,c), ideal(f,e,c), ideal(f,d,c), ideal(g,b,d*f^3-a^2*c*h), ideal(d,c,b), ideal(f^2,a*f,a^2,a*d*e*g-b^2*f*h), ideal(h^2,a*h,a^2,f^3,a*f^2,a*d*e*g-b^2*f*h), ideal(h^3,g^3,a^3,a*g*h^2,a*f*h^2,f*g^2*h,a^2*g*h,a^2*f*h,f^3*g,a*d*e*g-b^2*f*h,f^4,d*f^3-a^2*c*h,a*f^3,a^2*b*f-a^2*c*g,f^2*g*h^2,f^3*h^2,a*f^2*g*h,a*b*f^2*h-a*c*f*g*h,a*f^2*g^2,a^2*f*g^2,a^2*f^2*g,a^2*d*e*f-a*b*c*f*h,a*d*e*f^2*h-b*c*f^2*h^2), ideal(h,e,a^2,f^3), ideal(h,d,a^3,a^2*b*f-a^2*c*g), ideal(h^2,d^2,a^2,d*f*h,a*f*h,a*d*h,a*d*e*g-b^2*f*h,f^4,d*f^3), ideal(f^3,c*f^2,a*f^2,a*c*f,a^2*f,c^3,a^2*c,a^3,a*d*e*g-b^2*f*h), ideal(a^2,f^3,a*f^2,a*d*e*g-b^2*f*h,a*b^2*f,a*b^4,b^4*f^2,b^6), ideal(g^3,a^3,a^2*g^2,a*d*e*g-b^2*f*h,a^2*b*g,d*f^3-a^2*c*h,a*f^3,a^2*b*f-a^2*c*g,f^3*g^2,a*f^2*g^2,b^2*f*g^2,b*f^3*g,a^2*f^2*g,a*b^2*f*g,b*f^4-c*f^3*g,a*b^3*f,f^5*g,a*b^4*g,f^6,b^4*f^2,b^6,a^2*b^4), ideal(e^2,a^2,f^3,a*d*e*g-b^2*f*h,a*e*f^2,b^2*e*f,a*b^2*f,b^4*f^2,a*b^4*e,b^6), ideal(d,b^2,a^2), ideal(d^2,a^2,a*d*e*g-b^2*f*h,f^4,d*f^3,b^2*d*f,a*b^2*f,b^4*f^2,a*b^4*d,b^6)}
///

///
R = ZZ/101[a..d]
I = ideal(a^2*(a*d-b*c), b^2*(b^2-a*c))
time binomialMinprimes I
time minimalPrimes I   -- This is the fastest at the moment...
                   -- There should be builtin binomial routines for these?
time radical I
///

///
R = ZZ/32003[a..h]
I = ideal(
  a*(a*d-b*c),
  b*(a*c-b^2),
  c*(b*d-c^2))
time minimalPrimes I
time binomialMinprimes I
time radical I
time binomialPD I
///
