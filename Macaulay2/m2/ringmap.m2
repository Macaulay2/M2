--		Copyright 1995 by Daniel R. Grayson

RingMap = new Type of MutableHashTable
name RingMap := f -> concatenate(
     "map(", name target f, ",", name source f, ",", name f.matrix, ")"
     )
net RingMap := f -> horizontalJoin(
     "map(", net target f, ",", net source f, ",", net f.matrix, ")"
     )
expression RingMap := f -> new FunctionApplication from {
     map, expression (target f, source f, f.matrix)}
document { quote RingMap,
     TT "RingMap", " -- the class of all ring maps.",
     "Operations which produce ring maps:",
     MENU {
	  TO "map",
	  TO "newCoordinateSystem"
	  },
     "Operations on ring maps:",
     MENU {
	  TO "graphIdeal",
	  TO "graphRing",
	  TO "isInjective",
	  TO "kernel"
	  }
     }

map(Ring,Ring,Matrix) := (R,S,m,options) -> (
     if isFreeModule target m
     and isFreeModule source m
     and 1 == numgens target m 
     and ring m === R
     then (
	  n := 0;
	  ultimate( A -> (
		    r := numgens source m;
		    if r === n 
		    then m = vars A ** R | m
		    else if r < n
		    then error ("encountered values for ", string r, " variables");
		    n = n + numgens A;
		    coefficientRing A),
	       S);
	  if n != numgens source m 
	  then error ("encountered values for ", string numgens source m," variables");
	  new RingMap from {
	       quote source => S,
	       quote target => R,
	       quote matrix => m,
	       quote handle => newHandle(ggPush m, ggringmap)
	       }
	  )
     else if coefficientRing R === coefficientRing S
     and ring m === coefficientRing R
     and isFreeModule target m
     and isFreeModule source m
     and numgens source m === numgens target m
     then map(R,S,vars R * (m ** R))
     else error (
	  "expected a 1 by ", numgens S, " matrix over ", name R, 
	  " or a square matrix over the coefficient ring"
	  ))

TEST "
R=ZZ/101[a,b,c]
f = map(R,R,matrix(ZZ/101,{{1,2,3},{4,5,6},{7,8,9}}))
assert( f(a) == a + 4*b + 7*c )
assert( kernel f == ideal ( a-2*b+c ) )
"

map(Ring,Matrix) := (S,m,options) -> map(ring m,S,m)

map(Ring,Ring) := (S,R,options) -> (
     A := R; v := {}; while (
	  if A.?generators then (
	       v = join(apply(A.generators, x -> (
			      x = name x;
			      if S#?x then S#x else 0_S
			      )), v));
	  A.?ring) do A = A.ring;
     map(S,R,matrix (S,{v})))

map(Ring,Ring,List) := (R,S,m,options) -> map(R,S,matrix(R,{m}),options)

document { "map(Ring,Ring,...)",
     TT "map(R,S,m)", " -- sets up a ring homomorphism from ", TT "S", "
     to ", TT "R", " which sends the i-th variable of S to the i-th element
     of the list ", TT "m", ".  Alternatively, ", TT "m", " may be
     a 1 by n matrix over ", TT "R", ", where n is the
     number of variables in the polynomial ring ", TT "S", "; or it may
     be a square matrix over the common coefficient ring of the two rings,
     in which case it is used as the matrix of a linear change of coordinates.",
     EXAMPLE "R = ZZ/101[a,b];",
     EXAMPLE "m = symmetricPower(3, vars R)",
     EXAMPLE "rank source m",
     EXAMPLE "S = ZZ/101[s_1 .. s_oo]",
     EXAMPLE "f = map(R,S,m)",
     EXAMPLE "f s_2",
     EXAMPLE "f vars S",
     EXAMPLE "kernel f",
     EXAMPLE "generators oo",
     EXAMPLE "f oo",
     EXAMPLE "U = ZZ/101[t,u,v]",
     EXAMPLE "g = map(S,U,{s_1+s_2, s_2 + s_3, s_3+s_4})",
     EXAMPLE "f g",
     EXAMPLE "kernel oo",
     EXAMPLE "f g generators oo",
     PARA,
     "The class of all ring maps is ", TO "RingMap", "."
     }

AfterPrint RingMap := AfterNoPrint RingMap := f -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class f;
     << " " << target f << " <--- " << source f << endl;
     )

RingMap RingElement := fff := (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m then (
	  m = promote(m,R);
	  );
     sendgg(ggPush p, ggPush m, ggev);
     S.pop())

RingMap QQ := RingMap ZZ := (p,m) -> fff(p, promote(m,source p))

RingMap Vector := (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m 
     then error "expected source of ring map to be the same as ring of matrix";
     F := if S === R then class m else S^(numgens class m);
     sendgg(ggPush p, ggPush F, ggPush m, ggev);
     new F)

RingMap Matrix := (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m 
     then error "expected source of ring map to be the same as ring of matrix";
     F := if degreeLength R == degreeLength S then 
              S^(- degrees target m) 
          else if class S =!= class R or S =!= R then 
              S^(numgens target m) 
          else 
              target m;
     sendgg(ggPush p, ggPush F, ggPush m, ggev);
     getMatrix S)

kernel RingMap := (f,options) -> if f.?kernel then f.kernel else f.kernel = (
     R := source f;
     n2 := numgens R;
     F := target f;
     n1 := numgens F;
     if class F === FractionField then (
	  C := F.baseRings#-1;
	  if not isHomogeneous f.matrix then error "not implemented yet";
	  images := apply(generators R, x -> (
		    w := f x;
		    new Divide from {numerator w, denominator w} ));
	  -- now make a common denominator for all images
	  images = new MutableList from images;
	  i := 1;
	  while i < #images do (
	       z := syz(
		    matrix{{denominator images#0,denominator images#i}},
		    SyzygyLimit => 1 );
	       a := -z_(0,0);
	       b := z_(1,0);
	       j := 0;
	       while j < i do (
		    images#j = apply(images#j, s -> s*a);
		    j = j+1;
		    );
	       images#i = apply(images#i, s -> s*b);
	       i = i+1;
	       );
	  images = elements images;
	  commonDenominator := images#0#1;
	  k := coefficientRing R;
	  d := quote d;
	  h := quote h;
	  x := quote x;
	  y := quote y;
	  S := k[x_1 .. x_n1, d, y_1 .. y_n2, h,
	       MonomialOrder => Eliminate (n1 + 1),
	       Degrees => join(
		    apply(C.generators, degree), {{1}}, 
		    apply(R.generators, degree), {{1}})];
	  in1 := map(S,C,matrix {take (generators S, n1)});
	  in2 := map(S,R,matrix {take (generators S, {n1 + 1, n1 + n2})});
	  back := map(R,S,map(R^1,R^(n1 + 1),0) | vars R | 1 );
	  ideal back selectInSubring( 1, 
	       generators gb(
		    homogenize (
	       		 in2 vars source in2 - d * in1 matrix {apply(images, first)}
	       		 | d * in1 commonDenominator - 1,
	       		 h),
		    Strategy => LongPolynomial, options)))
     else if (
	  isAffineRing R
	  and isAffineRing F
	  and coefficientRing R === coefficientRing F
	  ) 
     then (
	  JJ := graphIdeal(f,
	       MonomialOrder => Eliminate n1, 
	       VariableBaseName => local X);
	  if isHomogeneous JJ then (
	      hf := poincare (target f)^1;
	      T := (ring hf)_0;
	      hf = hf * product(numgens source JJ, i -> (
			d := (degrees source JJ)#i#0; 
			1 - T^d));
	      (cokernel JJ).poincare = hf;
	      );
	  mapback := map(R, ring JJ, map(R^1, R^n1, 0) | vars R);
	  ideal mapback selectInSubring(1,generators gb(JJ,options))
	  )
     else error "not implemented yet"
     )

TEST "
f = map(frac (QQ[r,s,t]), QQ[x,y,z], {(r-s)/t,(s-t)/r,(t-r)/s})
assert( kernel( f, SubringLimit => 1 ) == ideal(x*y*z+x+y+z) )
"

TEST "
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,0})
assert( kernel f == ideal u )
"

TEST "
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,1})
assert( kernel f == ideal (u-1) )
"

TEST "
R = ZZ/101[a..f]
m = matrix {{a*b*c*d, b^2*c*d, b*c^2*d, b*c*d^2, b*c*d*e, 
	     c*d*e*f, a*d*e*f, a*b*e*f, a*b*c*f, b*c*d*f}}
f = map(R,ZZ/101[x_0..x_9],m)
J = kernel f
"

TEST "
S = ZZ/101[a..j]
m = matrix {{d*g*i-a*g*j, c*h^2-e*h*i, a*b^2*g-a*b*d*h, b*d*f-d*e*j}}
E = Ext^3(cokernel m, S)
annihilator E
"

image RingMap := f -> f.source / kernel f

RingMap RingMap := (g,f) -> (
     if source g != target f then error "ring maps not composable";
     m := g f.matrix;
     new RingMap from {
	  quote source => source f,
	  quote target => target g,
	  quote matrix => m,
	  quote handle => newHandle(ggPush m, ggringmap)
	  }
     )
isHomogeneous RingMap := (f) -> (
     isHomogeneous f.source and 
     isHomogeneous f.target and
     isHomogeneous f.matrix)


substitute(RingElement,Matrix) := (r,f) -> (map(ring f,ring r,f)) r
substitute(Vector,Matrix) := (v,f) -> (map(ring f,ring v,f)) v
substitute(Matrix,Matrix) := (m,f) -> (map(ring f,ring m,f)) m
substitute(Module,Matrix) := (M,f) -> (map(ring f,ring M,f)) M
substitute(Ideal,Matrix) := (I,f) -> (map(ring f,ring I,f)) I

substitute(Matrix,Ring) := (m,S) -> (map(S,ring m)) m
substitute(Module,Ring) := (M,S) -> (map(S,ring M)) M
substitute(Ideal,Ring) := (I,S) -> (map(S,ring I)) I
substitute(Vector,Ring) := (v,S) -> (map(S,ring v)) v
substitute(RingElement,Ring) := (r,S) -> (map(S,ring r)) r

substitute(Matrix,ZZ) := (m,i) -> (
     R := ring m;
     if i === 0 then (
     	  if isPolynomialRing R 
	  or isQuotientRing R and isPolynomialRing ultimate(ambient,R)
	  then substitute(m,map(R^1, R^(numgens R), 0))
     	  else m
	  )
     else error "expected integer to be zero"
     )

sub2 := (R,v) -> (
     m := new MutableList from generators R;
     scan(v, opt -> (
	       x := opt#0;
	       y := opt#1;
	       s := try baseName x else error(
		    "expected ", name x, " to be a generator of ", name R
		    );
	       m#((monoid R).index#s) = y));
     f := matrix{elements m};
     S := ring f;
     map(S,R,f))

substitute(Matrix,List) := (f,v) -> (sub2(ring f,v)) f
substitute(Module,List) := (M,v) -> (sub2(ring M,v)) M
substitute(Ideal,List) := (I,v) -> (sub2(ring I,v)) I
substitute(Vector,List) := (f,v) -> (sub2(ring f,v)) f
substitute(RingElement,List) := (f,v) -> (sub2(ring f,v)) f

substitute(Matrix,Option) := (f,v) -> (sub2(ring f,{v})) f
substitute(Module,Option) := (M,v) -> (sub2(ring M,{v})) M
substitute(Ideal,Option) := (I,v) -> (sub2(ring I,{v})) I
substitute(Vector,Option) := (f,v) -> (sub2(ring f,{v})) f
substitute(RingElement,Option) := (f,v) -> (sub2(ring f,{v})) f

document { quote substitute,
     TT "substitute(f,v)", " -- substitute values for the variables in the matrix,
     module, vector, polynomial, or monomial f as specified by v.", 
     PARA,
     "If f is a matrix over R, and v is a 1 by k matrix over another
     ring S, then the result is obtained by substituting the entries in v 
     for the variables in R.",
     PARA,
     "If f is a module over R, then substitution amounts to substitution
     in the matrices of generators and relations defining the module.  This is
     not the same as tensor product!",
     PARA,
     "If v is a ring, then the result is obtained by substituting the variables of
     v for the variables of R with the same name.  The substitution extends
     to the coefficient ring of R, and so on.",
     PARA,
     "If v is a list of options ", TT "{a => f, b => g, ...}", " then the variable
     ", TT "a", " is replaced by the polynomial ", TT "f", ", etc.",
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "f = x+2*y+3*z",
     EXAMPLE "substitute(f,{x=>x^3, y=>y^3})",
     EXAMPLE "S = ZZ/101[z,y,x]",
     EXAMPLE "substitute(f,S)"
     }

RingMap Ideal := (f,I) -> ideal f module I
RingMap Module := (f,M) -> (
     R := source f;
     S := target f;
     if R =!= ring M then error "no method found";
     if M.?generators or M.?relations 
     then subquotient(
	  if M.?generators then f M.generators, 
	  if M.?relations then f M.relations)
      else if degreesRing R === degreesRing S then
          S^(-degrees M)
      else
          S^(numgens M))
-- replaced this line with the 4 above, MES 5/30/97
--     else S^(numgens M))     

isInjective RingMap := (f) -> kernel f == 0
