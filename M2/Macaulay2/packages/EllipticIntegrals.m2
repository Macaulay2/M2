-- -*- coding: utf-8 -*-
newPackage(
	"EllipticIntegrals",
    	Version => "1.0", 
    	Date => "February, 2008",
    	Authors => {{Name => "Daniel R. Grayson", 
		  Email => "dan@math.uiuc.edu", 
		  HomePage => "http://www.math.uiuc.edu/~dan/"}},
    	Headline => "uniformization of elliptic curves",
	Keywords => {"Algebraic Number Theory"},
    	DebuggingMode => false
    	)

-* The notation used here comes from my paper
   The arithogeometric mean, Archiv der Mathematik, volume 52, 1989, pages 507-512,
   scanned versions available here: http://www.math.uiuc.edu/~dan/cv.html#agm
*-

export { 
     "EllipticCurve", 
     "Period", 
     "Period'", 
     "periodCoordinates", 
     "checkEquation",
     "distance",
     "PointClass",
     "EllipticCurvePoint",
     "modPeriods",
     "test"					    -- remove later
     }

b := getSymbol "b"
c := getSymbol "c"

see := (label,x) -> (stderr << "--" << label << " = " << x << endl; x)

EllipticCurve = new Type of ProjectiveVariety
EllipticCurve.MethodFunction = new MutableHashTable
Function _ EllipticCurve := (f,E) -> EllipticCurve.MethodFunction#f E
EllipticCurve.MethodFunction#log = E -> z -> E.log z
EllipticCurve.MethodFunction#exp = E -> z -> E.exp z
toString EllipticCurve := toString @@ expression
net EllipticCurve := net @@ expression
ellset					    -- function defined later
new EllipticCurve from List := (EllipticCurve,v) -> ellset toSequence v
precision EllipticCurve := E -> precision E.Period
isReal EllipticCurve := E -> isReal E#b and isReal E#c

log2ten = log_2 10p20

EllipticCurvePoint = new Type of List
net EllipticCurvePoint := 
toString EllipticCurvePoint := P -> (
     s := printingPrecision;
     ac := printingAccuracy;
     if s != 0 and ac == -1 then ac = s - floor (max (size2 \ P) / log2ten);
     concatenate("{",format(s,ac,P#0),",",format(s,ac,P#1),",",format(s,ac,P#2),"}")     
     )

conjugate EllipticCurvePoint := P -> (
     E := (class P).EllipticCurve;
     if not isReal E then error "expected a real elliptic curve";
     apply(P,conjugate))

isReal EllipticCurvePoint := P -> (
     E := (class P).EllipticCurve;
     isReal E and distance(P,conjugate P) < 2p20^(-precision E + 4)
     )

EllipticCurvePoint + EllipticCurvePoint := (P,Q) -> (
     if class P =!= class Q then error "expected points on the same elliptic curve";
     E := (class P).EllipticCurve;
     E.exp (E.log P + E.log Q)
     )

EllipticCurvePoint - EllipticCurvePoint := (P,Q) -> (
     if class P =!= class Q then error "expected points on the same elliptic curve";
     E := (class P).EllipticCurve;
     E.exp (E.log P - E.log Q)
     )

- EllipticCurvePoint := (P) -> (
     E := (class P).EllipticCurve;
     E.exp (- E.log P)
     )

EllipticCurve List := (E,xyz) -> new E.PointClass from xyz

nmr3 = (x,y,z) -> (
     -- normalize a point in the projective plane, so norm is 1 and one of the coordinates is 1
     (ax,ay,az) := (abs x, abs y, abs z);
     m := if az > ay and az > ax then z else if ay > ax then y else x;
     (x/m,y/m,z/m))

t = 1p20e-3; -- use this constant so it is preferred (a bit) to divide by z below
tx = t^2;
ty = t^3;
nm3 = (x,y,z) -> (
     -- normalize a point in the projective plane, so norm is not too far from 1, and one of the coordinates is norm 1
     -- this strategy for normalization does a better job preserving reality of the logarithm
     (ax,ay,az) := (abs(tx*x), abs(ty*y), abs numeric(20,z));
     m := abs if az > ay and az > ax then z else if ay > ax then y else x;
     (x/m,y/m,z/m))

nmb3 = (x,y,z) -> (
     -- normalize a point in the projective plane without changing any signs, so norm is not too far from 1
     -- fast because it changes just exponents
     e := max(size2 x, size2 y, size2 z);
     (x >> e, y >> e, z >> e))

realform = z -> w -> realPart(z*w)
dualform  = (u,v) -> realform(ii * conjugate v / realPart(ii * u * conjugate v))
dualforms = (u,v) -> (dualform(u,v),dualform(v,u))
realizeform = f -> (
     a := f 1;
     b := f ii;
     z -> a * realPart z + b * imaginaryPart z
     )
realizeforms = (f,g) -> (realizeform f, realizeform g)

distance = (P,Q) -> (
     P = toList P;
     Q = toList Q;
     e := exteriorPower_2 matrix{P,Q};
     if precision e === infinity 
     then (max \\ abs \ flatten entries e) / ((max \\ abs \ P) * (max \\ abs \ Q))
     else norm e / (norm P * norm Q)
     )

quadnorm = (a,b,c) -> (
     -- a measure of how far apart the roots of ax^2+bxy+cy^2 are, invariant under scaling
     (a,b,c) = (numeric_20 a,numeric_20 b,numeric_20 c);
     norm (b^2 - 4*a*c) / (norm {a,b,c})^2
     )

quadsolve = (a,b,c) -> (
     -- both roots of ax^2+bx+c
     d := sqrt(b^2 - 4*a*c);
     ((-b+d)/(2*a), (-b-d)/(2*a)))

ellset = (b,c) -> (
     if c == 0 or b^2 == 4*c then error "ellset: discriminant is zero";
     prec := min(precision b,precision c);
     if prec === infinity then prec = defaultPrecision;
     extra := 20;
     newprec := prec + extra;
     (b0,c0) := (numeric_newprec b,numeric_newprec c);
     near := (x,y) -> x == y or numeric_prec x == numeric_prec y;
     makechain := (b,c) -> (
	  stop := false;
	  while not stop list (
	       S := sqrt c;
	       R := (S+(b>>1))>>1;
	       if near(R,S) then stop = true;
	       (b,c) = (R+S,R*S);
	       (R,S)));
     chain := makechain(b0,c0);
     Msq := last last chain;
     M := sqrt Msq;
     makebc := (r,s) -> (-r-s,r*s);	-- (x-r)(x-s) == x^2+bx+c, return (b,c)
     makers := (b,c) -> (
	  disc := sqrt( b^2 - 4*c );
	  (- b + disc)/2, (- b - disc)/2       -- return r,s; larger root first
	  );
     translater := (r,s) -> (-r,s-r);			    -- prefer this one
     -- translates := (r,s) -> (-s,r-s);
     chain' := makechain makebc translater makers (b0,c0);
     period := pi/M;
     period' := pi/(sqrt (last last chain'));
     period' = period' - round (period' / period) * period;
     (fm1,fm2) := realizeforms dualforms(period,period');
     normalize := z -> (
	  a := round fm1 z;
	  b := round fm2 z;
	  if a =!= 0 then z = z - a * period;
	  if b =!= 0 then z = z - b * period';
	  z);
     k := ring (b+c);
     R := k (monoid [getSymbol "x", getSymbol "y", getSymbol "z"]);
     (x,y,z) := (R_0,R_1,R_2);
     S := R / (y^2 * z - (x^3 + b * x^2 * z + c * x * z^2));
     E := Proj S;
     E = newClass(EllipticCurve, E);
     Epointclass := new Type of EllipticCurvePoint;
     Epointclass.EllipticCurve = E;
     toString Epointclass = "Point on the elliptic curve " | toString expression E;
     prs := {
	  global PointClass => Epointclass,
	  global modPeriods => normalize,
	  global Equation => (() -> (
		    x := hold getSymbol "x";
		    y := hold getSymbol "y";
		    y^2 == x^3 + b * x^2 + c * x)) (),
	  getSymbol "b" => b,
	  getSymbol "c" => c,
	  global Period => numeric_prec period,
	  global Period' => numeric_prec period',
	  global periodCoordinates => z -> (
	       if not instance(z,Number) then error "expected a number";
	       numeric_prec { fm1 z, fm2 z }
	       ),
	  global checkEquation => P -> (
	       (x,y,z) := toSequence P;
	       norm ( y^2 * z - x * (x^2 + b*x*z + c*z^2) ) / (norm P)^3
	       ),
	  global exp => w -> (
	       prec' := min(prec, precision w);
	       prec'' := prec' + extra;
	       w = normalize w;
	       wM := - M * numeric_prec'' w;
	       b := M * cos wM;
	       c := sin wM;
	       if c == 0 then return numeric_prec' {0,1,0};
	       if c < 0 then (b,c) = (-b,-c);
	       (x,y,z) := (b^2*c, b*(b^2 + Msq * c^2),c^3);
	       scan(reverse chain, (R,S) -> (
			 r := x+R*z;
			 (x,y,z) = (r*x*(x+S*z),y*(x^2+2*x*R*z+R*S*z^2),r^2*z);
			 (x,y,z) = nmb3 (x,y,z);
			 ));
	       if x==0 and z==0 then return numeric_prec' {0,1,0};
	       (x,y,z) = nm3(x,y,z);
	       (x,y,z) = (numeric_prec' x,numeric_prec' y,numeric_prec' z);
	       new Epointclass from [x,y,z]),
	  global log => P -> (
	       if not instance(P, Epointclass) then error "expected a point on the curve";
	       (x,y,z) := toSequence P;
	       if z == 0 then return z;
	       prec' := min(precision x,precision y,precision z,prec);
	       prec'' := prec' + extra;
	       (x,y,z) = (numeric_prec'' x,numeric_prec'' y,numeric_prec'' z);
	       realP := isReal P;
	       scan(chain, (R,S) -> (
			 local u; local u'; local v;  local v'; local w;
			 local den1; local den2; local den3;
			 local num1; local num2; local num3;
			 eqn := (z, -x+S*z, -x*R);	    -- u2-ux+uS-xR
			 eqn':= (z, -y, x*R*(S-R));	    -- v2-vy-xR2+xRS
			 w = numeric_prec'' 1;
			 if quadnorm eqn' > quadnorm eqn then (
			      (v,v') = quadsolve eqn';
			      -*
			      -- ux2+2uxR-uxS-vy+x2R+xRS
			      num1 = (v) -> - (-v*y*z+w*(x^2*R+x*z*R*S));
			      den1 = (v) -> x^2+x*z*(2*R-S);
			      -- uyx+2uyR-uyS-vx2-4vxR+2vxS-vS2+yxR+yRS
			      num2 = (v) -> - (-v*x^2-4*v*x*R*z+2*v*x*S*z-v*S^2*z^2+w*(y*x*R+y*R*S*z)); -- this one can become tiny if we use it alone
			      den2 = (v) -> y*x+2*y*R*z-y*S*z;
			      *-
			      -- uv+uyd-uy+vxd-vx+vSd+yRd-yR (d==1/2)
			      num3 = (v) -> - (-v*x/2+v*S/2*z-w*y*R/2)*w;
			      den3 = (v) -> v*z-w*y/2;
			      u = num3 v/den3 v;
			      u' = num3 v'/den3 v';
			      if realP and isReal u and isReal u' then ( -- this might be backwards if the two other roots are positive
			      	   if u' > u then (u,v) = (u',v');
				   )
			      else (
			      	   if abs u' > abs u then (u,v) = (u',v');
				   );
			      )
			 else (
			      (u,u') = quadsolve eqn;
			      if realP and isReal u and isReal u' then (
			      	   if u' > u then (u,v) = (u',v');
				   )
			      else (
			      	   if abs u' > abs u then (u,v) = (u',v');
				   );
			      -*
			      -- vy-ux2-2uxR+uxS-x2R-xRS
			      num1 = - (-u*x^2-2*u*x*z*R+u*x*z*S-w*(x^2*R+x*z*R*S));
			      den1 = () -> y*z;
			      -- vu+vxd-vx+vSd+uyd-uy+yRd-yR (d==1/2)
			      num2 = - ((-u-w*R)*y/2*w);
			      den2 = () -> u*z+w*(-x+S*z)/2;
			      *-
			      -- vx2+4vxR-2vxS+vS2-uyx-2uyR+uyS-yxR-yRS
			      num3 = - (-u*y*x-2*u*y*R*z+u*y*S*z-w*y*x*R-w*y*R*S*z);
			      den3 = () -> x^2+4*x*R*z-2*x*S*z+S^2*z^2;
			      v = num3/den3();
			      );
			 (x,y,z) = nmb3(u,v,w);
			 ));
	       numeric_prec' normalize ( atan(M * (x+Msq*z) / y) / -M ))
	  };
     scan(prs, pr -> E#(pr#0) = pr#1);
     E)

-*

eqns = () -> (
     -- run this function to generate the equations need to create the formulas in this algorithm
     eqns0 := uv -> (
	  A = ZZ[uv,y,x,D,R,S,d,e,MonomialOrder=>Lex];
	  b = 4*R-2*S;
	  c = S^2;
	  I = ideal(
	       2 * d - 1,
	       3 * e - 1,
	       D * R * S * (R-S) - 1,
	       v^2-u*(u+R)*(u+S),
	       x*(u+R)-u*(u+S),
	       y*(u+R)^2-v*(u^2+2*R*u+R*S));
	  assert ( (y^2-x*(x^2+b*x+c)) % I == 0 );
	  print gens A;
	  print transpose gens gb I;
	  );
     eqns0 (u,v);
     eqns0 (v,u);
     )     

*-

TEST ///
defaultPrecision = 200;
-- this is a random example
E = new EllipticCurve from {2,3};
assert( instance(E.Period, RR) );
assert( abs E.Period === 2.5308285053958684099070110720312058949823892791088718843952886434735986368365005418805928p200 );
assert( abs imaginaryPart E.Period' === 1.6961196938565959693448565420423855844180032262285705787422735897241846836628622683126757p200 );
assert( abs realPart E.Period' === E.Period / 2 );
w = 2/10;
P = E.exp w;
P' = {24.320324016829770815650582361314829753363183699359305678508046741p200,
     -125.06341511246553122013771748084347529459024606604009244488993779p200,
     .1p200e1};
assert ( toList P === P' );
w' = E.log P;
assert( toRR w === w' );
scan(10, i -> (
	  f := random RR - 1/2;
	  w := f * E.Period;
	  w' := E.log E.exp w;
	  assert( instance(w', RR) );
	  assert( abs((w - w')/w) < 2p20^(-defaultPrecision+1) );
	  ));
assert( {.2p200, .3p200} === E.periodCoordinates (.2p200 * E.Period + .3p200 * E.Period') );
scan(-4 .. 4, i -> scan(-4 .. 4, j -> (
	       assert( distance(E.exp ( .2p200 ), E.exp ( .2p200 + i * E.Period' + j * E.Period )) < 1e-50 );
	       assert( distance(E.exp ( .2p200+.3p200*ii ), E.exp ( .2p200+.3p200*ii + i * E.Period' + j * E.Period )) < 1e-50 );
	       )));
scan(20, i -> (
	  z := random CC * 20 - (10+10*ii);
	  assert( E.checkEquation E.exp z  < 1p20e-40 );
	  ));
scan(20, i -> (
	  x := random CC * 20 - (10+10*ii);
	  y := sqrt ( x^3 + E#b*x^2 + E#c*x );
	  P := E {x,y,1};
	  assert( E.checkEquation P < 1p20e-40 );
	  assert( distance(E.exp E.log P, P) < 1p20e-40 );
	  P = (random CC - (1+ii)/2) * P;
	  assert( E.checkEquation P < 1p20e-40 );
	  assert( distance(E.exp E.log P, P) < 1p20e-40 );
	  ));
scan(20, i -> (
	  w := (random RR - 1/2) * E.Period + (random RR - 1/2) * E.Period';
	  assert( 1p20e-40 > max \\ abs \ E.periodCoordinates ( E.log E.exp w - w ) );
	  ));
assert( 1e-55 > norm (E.periodCoordinates E.log E.exp ( -3.49p200 * E.Period + -4.49p200 * E.Period' ) - { -.49p200,-.49p200 } ));
assert( 1e-55 > norm (E.periodCoordinates E.log E.exp ( 3.49p200 * E.Period + 4.49p200 * E.Period' ) - { .49p200,.49p200 } ));
assert( 1e-55 > norm (abs \ E.periodCoordinates E.log E.exp ( 3.5p200 * E.Period + 4.5p200 * E.Period' ) - { .5p200,.5p200 } ));
assert( E.exp 0 === {0p200,1p200,0p200} );
assert( norm(E.exp E.Period - {0p200,1p200,0p200}) < 1e-55 );
assert( distance(E.exp E.Period', {0p200,1p200,0p200}) < 1e-55 );
assert( distance(E.exp( E.Period + E.Period' ), {0p200,1p200,0p200}) < 1e-55 );
assert( distance(E.exp (.2p200 + 100*E.Period'), E.exp (.2p200)) < 1e-55 );
///

TEST ///
-- this is the example from the paper
defaultPrecision = 200;
D = new EllipticCurve from {49/4,16};
assert( D.log D {4,-18,1} === .369919481948619552895243135959626649608662476782124725192805177111472521060507737885616486p200 );
assert( D.periodCoordinates D.log D {4,-18,1} === {1./4,0.} );
assert( D.periodCoordinates D.log D {-8,12,1} === {1./8,-1./2} );
assert( D.Period === .14796779277944782115809725438385065984346499071284989007712207p200e1 )
assert( D.Period' === toCC(.0p200,-.99348185850601324739329990214047552905027815853831220628647471p200) )
 -* actually, the paper has the complex conjugate of this period ... *-
///

TEST ///
defaultPrecision = 2000;
D = new EllipticCurve from {49/4,16};
result = D.log D {4,-18,1}
assert( result === .36991948194861955289524313595962664960866247678212472519280517711147252106050773788561648590076490501530693046833342157958394618521919564010528994124220080346432218452209356596402438592542481509288551083454520862983924665767390255283306531273974266369599782089674379930950117992432630475867532094677817541346775132258749997575180276140635560096880489359110132094907125889960474705999991693520051782426376858122502435487789180572161826981069764302437583354714571023989856728723371278912399769093858699957284325209191376923558703760523643472581244850282004300109746297354134800300510513509935534967578260651640863571044289561307686736012242633887411501593767514380509940928590948236051405877736p2000 );
scan(-4 .. 4, i -> scan(-4 .. 4, j -> (
	       assert( distance(D.exp ( .2p2000 ), D.exp ( .2p2000 + i * D.Period' + j * D.Period )) < 1p20e-500 );
	       )));
v = D.Period / 4;
L = D.exp v;
assert( norm (L - {4,-18,1})/18 < 2p20^(-defaultPrecision+1) );
assert( D.log D {4,-18,1} === v );
stderr << "--test passed" << endl;
///


TEST ///
defaultPrecision = 200
E = new EllipticCurve from {5,6}

w = .02 + .02001314410965065 * ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

w = .02 + .0200131441096506 * ii
P' = E.exp w
E.checkEquation P'
E.log P'
assert( abs ( E.log P' - w ) < 1e-30 )

w = .02 + .03*ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

w = .02 + .04*ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

///


TEST ///
defaultPrecision = 200
E = new EllipticCurve from {-5,6}

w = .02 + .02001314410965065 * ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

w = .02 + .0200131441096506 * ii
P' = E.exp w
E.checkEquation P'
E.log P'
assert( abs ( E.log P' - w ) < 1e-30 )

w = .02 + .03*ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

w = .02 + .04*ii
P = E.exp w
E.checkEquation P
E.log P
assert( abs ( E.log P - w ) < 1e-30 )

///

beginDocumentation()

multidoc ///
Node
 Key
  EllipticIntegrals
 Description
  Text
   This package provides some functions for computing elliptic integrals and elliptic functions.
   The basis for the computations is the arithmetic geometric mean, and the notation we use
   comes from the paper @ HREF { "http://www.math.uiuc.edu/~dan/cv.html#agm", "The arithogeometric mean"} @,
   Archiv der Mathematik, volume 52, 1989, pages 507-512.
///
