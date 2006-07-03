radical0 = (I) -> (
     I = ideal mingens ideal gens gb I;
     --I = trim I;
     III = I;
     << "-----take radical of     = " << transpose gens I << endl;
     comps := minimalPrimes I;
     result := if #comps === 1 then comps#0
               else intersect toSequence comps;
     << "-----radical = " << transpose gens result << endl;
     result)


Matrix \ Ring := (m,A) -> substitute(m,A)
Ideal \ Ring := (I,A) -> substitute(I,A)

--Other things that we need:
-- a. nzd check
-- b. minimal presentation of a (quasi)homogeneous ring.

-- Integral closure of a ring

document {
     Key => isNormal,
     TT "isNormal", "(R:Ring) >>> Boolean -- is R normal, i.e. integrally closed?",
     PARA{},
"If R is an affine ring, determines whether R is integrally closed.
The method used is ..."
}

document {
     Key => normalization,
     TT "normalization", "(R:Ring) >>> Ring -- Yields the integral closure of the
affine ring R"
}

document {
     Key => idealizer,
  TT "idealizer", "(J:Ideal)>>>Ideal -- ring structure on Hom(J,J)",
  PARA{},
  "Yields an ideal L in a ring S such that Hom(J,J) = S/L. If
  the input ideal J is homogeneous, then S is quasi-homogeneous.",
  PARA{},
  "An example: ",
  EXAMPLE lines ///
  R = ZZ/101[x,y,z]/(y^2*z-x^3);
  I = ideal(x,y);
  L = time idealizer I
  isHomogeneous L

  R = ZZ/101[x,y,z]/(y^5*z-x^6);
  I = ideal(x,y);
  L = time idealizer I
  isHomogeneous L
  
  R = ZZ/101[x,y]/(y^2-x^3-x^4-x^2*y^2);
  I = ideal(x,y)
  L = time idealizer I
  ///  
  }

idealizer = method()
Oldidealizer Ideal := (I) -> (
     -- Find a nzd in I.  We assume that R = ring I is a domain
     if I == 0 then error "I must be a nonzero ideal";
     f := (mingens I)_(0,0);
     J := gens trim ideal gens gb (f*I : I);
     JJJ = J;
     << "idealizer: new denominator= " << f << endl;
     << "idealizer: new numerators = "<< transpose J << endl;
     H := compress (J % f);  -- only works if f is an actual element of J.
     if numgens source H === 0 then ring I else (
     Ha := (f // J);
     Hf := H | matrix{{f}};
     -- We will add new elements to the ring, one for each column
     -- of H.
     R := ring I;
     n := numgens source H;
     -- MES: check here whether the arg to Degrees is stricty positive?
     w = symbol w;
     A1 := (coefficientRing R)[w_(numgens R) .. w_(numgens R + n - 1), 
                             Degrees=>degrees source H - toList(n:degree f)];
     A := R ** A1;
     --A = (coefficientRing R)[x_0 .. x_(numgens A - 1), Degrees=>degrees vars A
     --A := tensor(R,A1,Variables=>x);
     mapRtoA := map(A,R,(vars A)_{0..numgens ring f - 1});
     varsA1 := (vars A)_{numgens R .. numgens R - n + 1};
     fA := mapRtoA f;
     XX := varsA1 | matrix{{1_A}};
     lins := XX * mapRtoA syz Hf; -- linear equations(in new variables) in the ideal
     -- now for the quadratic equations
     HA = mapRtoA H;
     HfA := mapRtoA Hf;
     Hf2 := symmetricPower(2,HA);
     quads := matrix entries (symmetricPower(2,varsA1) - XX * ((Hf2 // fA) // HfA));
     B := A/(trim ideal matrix entries gens lift(ideal lins + ideal quads, ring presentation A));
     (ring presentation B)/(ideal presentation B)
     ))

idealizer Ideal := (I) -> (
     -- Find a nzd in I.  We assume that R = ring I is a domain
     if I == 0 then error "I must be a nonzero ideal";
     R := ring I;
     f := (mingens I)_(0,0);
     J := gens trim ideal gens gb (f*I : I);
     << "idealizer: new denominator= " << f << endl;
     << "idealizer: new numerators = "<< transpose J << endl;
     H := compress (J % f);  -- only works if f is an actual element of J.
     n := numgens source H;
     if n === 0 
       then R
       else (
         Ha := (f // J);
         Hf := H | matrix{{f}};
         -- We will add new elements to the ring, one for each column of H.
         -- MES: check here whether the arg to Degrees is stricty positive?
         A1 := (coefficientRing R)[w_(numgens R) .. w_(numgens R + n - 1), 
                             Degrees=>degrees source H - toList(n:degree f)];
	 oldorder = R.monoid.Options.MonomialOrder;
	 neworder := 
	   if class oldorder =!= ProductOrder 
	   then ProductOrder{numgens A1,numgens R}
	   else prepend(numgens A1,oldorder);
         A := tensor(A1,R,MonomialOrder=>neworder);
         RtoA := map(A,R,(vars A)_{n..numgens R + n - 1});
	 varsA1 := (vars A)_{0..n-1};
         fA := RtoA f;
         XX := varsA1 | matrix{{1_A}};
         lins := XX * RtoA syz Hf; -- linear equations(in new variables) in the ideal
         -- now for the quadratic equations
         HA = RtoA H;
         HfA := RtoA Hf;
         Hf2 := symmetricPower(2,HA);
         quads := matrix entries (symmetricPower(2,varsA1) - XX * ((Hf2 // fA) // HfA));
         B := A/(trim ideal matrix entries gens lift(ideal lins + ideal quads, ring presentation A));
         (ring presentation B)/(ideal presentation B)
     ))

normalization = method()
normalization Ring := (R) -> (
     if not isAffineRing R then
         error "expected an affine ring";
     J = jacobian(presentation R);
     J = J\R;
     J = trim minors(codim R, J);

     -- at this point we check whether R is smooth outside of the
     -- origin.

     -- our conductor ideal should be either the radical of this, or
     -- if R is smooth, and homogeneous, then J = vars R.
     << "-----jacobian = " << transpose gens J << endl;
     done := false;
     while not done do (
	  A := ring J;
	  J = J\(ring presentation A) + ideal presentation A;
     	  << "-----take radical of     = " << transpose gens J << endl;
	  J = radical J;
	  J = trim J\A;
	  << "-----radical of jacobian = " << transpose gens J << endl;
	  R1 := idealizer J;
	  <<"------new ring = ";
	  describe R1;
	  << endl;
	  if R1 === R 
	  then done = true 
	  else (
	       R = R1;
	       J = substitute(J,R);
	  ));
     R)

-- A 'todo' list is: { ideal I, ideal J}
-- where J is a lift to S of a minimal set of generators of J in S/I.
-- An ICnode is this list.
--

if ICnode === symbol ICnode then
    ICnode = new Type of MutableHashTable;

newICnode = (R) -> (
     w = symbol w;
     I := ideal presentation R;
     C := new ICnode;
     C#"todo" = {{I,null}};
     C#"pending" = null;
     C#"answer" = {};
     C#"degrees" = degrees source vars ring I;
     C#"blocks" = {numgens ring I};
     C#"basefield" = coefficientRing ring I;
     C#"vars" = toSequence (ring I).generatorSymbols;
     R#"IC" = C;
     C)

next = (C) -> (
     if C#"pending" =!= null then true
     else if #C#"todo" > 0
     then (
	  C#"pending" = C#"todo"#0;
	  C#"todo" = drop(C#"todo",1);
	  true)
     else false)

-- The information about degrees, monomial order of a ring S can be
-- determined via 'toList (monoid S).options.MonomialOrder' and
-- '(monoid S).options.Degrees'.
ICring = (C) -> (
     -- C is an ICnode
     n := #(C#"degrees");
     m := C#"blocks"#0; -- the number in the first block
     newvars := w_(n-m) .. w_(n-1);
     C#"vars" = splice(newvars, C#"vars");
     C#"blocks" = select(C#"blocks", d -> d =!= 0);
     if any(C#"degrees", d -> d <= 0) then (
	  CCC = C;
       (C#"basefield")[C#"vars",
	  MonomialOrder=>ProductOrder (C#"blocks"),
	  MonomialSize=>16])
     else (
       (C#"basefield")[C#"vars",
	  Degrees => C#"degrees", 
	  MonomialOrder=>ProductOrder (C#"blocks"),
	  MonomialSize=>16])
     )

idealizer0 = (C) -> (
     -- Takes {I,J} off the pending list of C,
     -- computes the ring sturcture of Hom_R(J,J).
     -- This is done using the IC structure, since we wish to be able to
     -- handle interrupts, and the creation of the ring is somewhat
     -- easier.  It also faciliates handling the non prime case.
     I = C#"pending"#0;
     J = C#"pending"#1;
     -- Find an element of J, a nzd in S/I.
     f := J_0;  -- MES: check nzd.  If not, split comp into two parts.
     -- Compute Hom_R(J,J), with R = S/I.
     -- From now on, we work in this quotient:
     R := (ring I)/I;
     JR = ideal (gens J ** R);
     fR = substitute(f, R);
     idJ = mingens ideal gens gb (fR * JR : JR);
     --idJ = mingens (fR * JR : JR);
     << "idealizer: new denominator= " << fR << endl;
     << "idealizer: new numerators = "<< transpose idJ << endl;
     if numgens source idJ === 1 then (
	  -- We have the answer!
	  C#"answer" = append(C#"answer", I);
	  C#"pending" = null;)
     else (
     	  H = compress (idJ % fR);
     	  Ha = (fR // gens JR);  -- MES: what is this Ha all about: the problem is that
	                    -- although f is a minimal generator of (fJ:J) mod I,
			    -- it might not be given immediately as one of the elements.
     	  Hf = H | matrix{{fR}};
     	  -- Make the new polynomial ring.
	  n := numgens source H;
	  newdegs = degrees source H - toList(n:degree fR);
     	  C#"degrees" = join(newdegs, C#"degrees");
     	  C#"blocks" = prepend(n, C#"blocks");
     	  A = ICring C;
     	  newvars = (vars A)_{0..n-1};
     	  RtoA = map(A,R,(vars A)_{n..numgens R + n - 1});
	  IA = ideal ((map(A,ring I,RtoA.matrix)) (gens I));
	  XX = newvars | matrix{{1_A}};
     	  -- Linear relations in the new variables
     	  lins = XX * RtoA syz Hf; -- linear equations(in new variables) in the ideal
     	  -- Quadratic relations in the new variables
	  tails = (symmetricPower(2,H) // fR) // Hf;
	  tails = RtoA tails;
	  quads = matrix(A, entries (symmetricPower(2,newvars) - XX * tails));
	  newI = trim ideal matrix entries gens (ideal lins + ideal quads + IA);
	  newJ = newI + RtoA JR;
	  C#"todo" = append(C#"todo", {newI, radical0 newJ});
	  C#"pending" = null;
          )
     )

normal0 = (C) -> (
     -- This handles the first node: finding an ideal that contains the NNL locus.
     I = C#"pending"#0;
     local J;
     SI = jacobian I;
     R := (ring I)/I;
     SIR = substitute(SI,R);
     if isHomogeneous I then (
	  SIdets = minors(codim I, SIR);
	  cs = codim SIdets + codim R;  -- the codimension of the singular locus.
	  if cs === dim ring I -- i.e. the sing locus is empty.
	  then J = ideal vars ring I
	  else (

	       J = radical0(lift(ideal SIdets_0,ring I));
	       )
	  )
     else (
	  det1 = minors(codim I, SI, Limit=>1);
          J = radical0(lift(ideal det1_0,ring I));
	  );
     C#"todo" = append(C#"todo", {I,J});
     C#"pending" = null;
     )

normalization Ring := (R) -> (
     if not R#"IC" then newICnode R;
     C := R#"IC";
     while next C do (
     	  if C#"pending"#1 === null 
     	  then normal0 C
     	  else idealizer0 C
	  );
     C#"answer"#0
     )
///
S = ZZ/101[a..d]
I = monomialCurveIdeal(S,{1,3,4})
R = S/I
normalization R
peek R#"IC"

-- by hand...
C = newICnode R
peek C
next C
time normal0 C
peek C
next C
peek C
idealizer0 C
peek C
next C
peek C
idealizer0 C
peek C

time normalization R
-- times: 6/4/97 first try: 1.91 sec

R = ZZ/101[symbol x..symbol z,Degrees=>{2,2,1}]/(x^2-y*z^2)
time normalization R
-- times: 6/4/97 first try: 2.36 sec
-- 
R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)
time normalization R
-- times: 6/4/97 first try: 5.55 sec

R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,14},MonomialSize=>16]/(z*y^2-x^5*z-x^12)
time normalization R
-- times: 6/4/97 first try: 5.14 sec

R = ZZ/101[symbol x..symbol z,Degrees=>{2,3,10}]/(z*(y^2-x^3)-x^8)
time normalization R
-- times: 6/4/97 first try: 3.69 sec

R = ZZ/101[symbol x, symbol y,Degrees=>{2,5}]/(y^2-x^5)
time normalization R
-- times: 6/4/97 first try: 2.81 sec

R = ZZ/101[symbol x..symbol z,Degrees=>{3,4,12}]/(z*y^3-x^4*z-x^8)
time normalization R
-- times: 6/4/97 first try: 6.22 sec

R = ZZ/101[symbol x..symbol z]/(x^2*y^2+x^2*z^2+y^2*z^2)
time normalization R
-- times: 5/31/97 first try: 4.69 sec

R = ZZ/101[symbol x, symbol y, symbol z, Degrees=>{2,3,6}]/(z*y^2-z*x^3 + x^6)
time normalization R
-- time: 6/18/97 3.19 sec

R = ZZ/101[symbol x, symbol y, symbol z, Degrees=>{3,5,9},MonomialSize=>16]/(z*y^3-z*x^5 + x^8)
time normalization R
-- time: 6/18/97 10.19 sec

S = ZZ/101[a..e]
I = ideal(a*b^3*c + b*c^3*d + a^3*b*e + c*d^3*e + a*d*e^3,
     a^2*b*c^2 + b^2*c*d^2 + a^2*d^2*e + a*b^2*e^2 + c^2*d*e^2,
     a^5 + b^5 + c^5 + d^5 + e^5 - 5*a*b*c*d*e)
I = saturate I
R = S/I
time normalization R

S = ZZ/101[a..h]
I = ideal(b*f-c*g,a*d*e-b*c*h,d*f^2*g-a^2*b*h,d*f^3-a^2*c*h,a^3*e-c*f^2*g,d^2*e*f*g^2-a*b^3*h^2,d^3*e^2*g^3-b^5*h^3)
R = S/I
time normalization R

-- The following is the answer that was obtained:
///
A = ZZ/101[w_16,w_11,w_12,w_13,w_14,w_15,w_8,w_9,w_10,a,b,c,d,e,f,g,h,Degrees => {{1}, {1}, {2}, {2}, {2}, {2}, {2}, {3}, {3}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}},MonomialOrder => ProductOrder{1, 5, 3, 8},MonomialSize => 16]
IN = ideal(b*f-c*g,w_11*f-w_8,w_11*a-c*g,w_11^2-w_15,w_16*f-a*e,w_16*c-w_13,w_16*b-w_15,w_16*a-w_8,w_16*w_11-b*e,w_16^2-w_11*e,a*d*e-b*c*h,w_11*c*h-d*e*f,w_11*b*h-d*e*g,w_11*d*g-w_9,w_14*f-a*b*h,w_14*e-w_15*h,w_14*c-w_8*d,w_14*b-w_9,w_14*a-d*f*g,w_12*g-a*b*h,w_12*f-a*c*h,w_12*e-w_13*h,w_12*c-w_10,w_12*b-w_8*d,w_12*a-d*f^2,w_11*w_14-b^2*h,w_11*w_12-b*c*h,w_16*d*g-b^2*h,w_16*w_14-d*e*g,w_16*w_12-d*e*f,w_14^2-b*d*g*h,w_12*w_14-c*d*g*h,w_12^2-c*d*f*h)

IN1 = substitute(IN, {
     w_8 => w_11 * f, 
     w_9 => w_11 * d * g,
     w_10 => w_12 * c,
     w_13 => w_16 * c,
     w_15 => w_11^2})
mingens IN1
codim IN1
res coker gens IN1
///
-- example that bernd sturmfels gave (this ideal is normal)
x = symbol x;
R = ZZ/101[x_0 .. x_14]
S = ZZ/101[y_0 .. y_19]

m = matrix{{x_0 * x_1,
	    x_0 * x_2,
	    x_1 * x_2,
	    x_3 * x_4,
	    x_3 * x_5,
	    x_4 * x_5,
	    x_6 * x_7,
	    x_6 * x_8,
	    x_7 * x_8,
	    x_9 * x_10,
	    x_9 * x_11,
	    x_10 * x_11,
	    x_12 * x_13,
	    x_12 * x_14,
	    x_13 * x_14,
	    x_2 * x_5,
	    x_5 * x_8,
	    x_8 * x_11,
	    x_11 * x_14,
	    x_14 * x_2}}
F = map(R,S,m)
I = ker F
T = ZZ/101[x_0 .. x_14, y_0 .. y_19, Degrees=>{15:1, 20:2}, MonomialOrder=>Eliminate 15]
m1 = substitute(vars S, T) - substitute(m, T)
gb m1
ee = selectInSubring(1, gens gb m1)
mingens ideal ee
describe oo

I = ideal(y_9*y_15*y_17*y_18-y_10*y_11*y_16*y_19, 
     y_3*y_15*y_16*y_18      - y_4*y_5*y_17*y_19,
     y_1*y_2*y_16*y_18       - y_0*y_15*y_17*y_19, 
     y_7*y_8*y_15*y_18       - y_6*y_16*y_17*y_19, 
     y_13*y_14*y_15*y_17     - y_12*y_16*y_18*y_19, 
     y_10*y_11*y_13*y_14     - y_9*y_12*y_18^2, 
     y_1*y_2*y_13*y_14       - y_0*y_12*y_19^2, 
     y_7*y_8*y_10*y_11       - y_6*y_9*y_17^2, 
     y_4*y_5*y_7*y_8         - y_3*y_6*y_16^2, 
     y_1*y_2*y_4*y_5         - y_0*y_3*y_15^2, 
     y_1*y_2*y_9*y_18^2-y_0*y_10*y_11*y_19^2, 
     y_6*y_13*y_14*y_17^2-y_7*y_8*y_12*y_18^2, 
     y_3*y_10*y_11*y_16^2-y_4*y_5*y_9*y_17^2, 
     y_3*y_13*y_14*y_15^2-y_4*y_5*y_12*y_19^2, 
     y_0*y_7*y_8*y_15^2-y_1*y_2*y_6*y_16^2, 
     y_3*y_9*y_15^2*y_18^2-y_4*y_5*y_10*y_11*y_19^2, 
     y_1*y_2*y_7*y_8*y_18^2-y_0*y_6*y_17^2*y_19^2, 
     y_4*y_5*y_13*y_14*y_17^2-y_3*y_12*y_16^2*y_18^2, 
     y_1*y_2*y_10*y_11*y_16^2-y_0*y_9*y_15^2*y_17^2, 
     y_7*y_8*y_13*y_14*y_15^2-y_6*y_12*y_16^2*y_19^2)
-- Is this ideal smooth?
codim I

bernd = (r) -> (
     R = ZZ/101[x_0 .. x_(r-1),
	        y_0 .. y_(r-1),
		z_0 .. z_(r-1)];
     p = deepSplice apply(0..r-1, i -> apply(0..r-i, j-> (
	  k := r-i-j;
	  if j =!= r and k =!= r then x_i * y_j * z_k else 0)));
     I = ideal compress gens ideal p;
     I
     )
///
