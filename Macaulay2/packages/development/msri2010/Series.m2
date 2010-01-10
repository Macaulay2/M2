--================
-- THIS IS NOT A PACKAGE YET!
--
--================
--series(rational function) = series(polynomial)
--series(n,polynomial)
--series(Hilbert)
--series(generating function)
--series(degree, poly)


--arithmetic addion 
--reduction for series not in exact for
--set global default
--handle polys. 
--handle 


Series = new Type of HashTable
expression Series := s -> expression "O(" expression(s#degree+1) expression ")" + expression truncate(s#degree, s#polynomial);
net Series := s -> net expression s;
toString Series := s -> toString expression s;
tex Series := s -> tex expression s;
html Series := s -> html expression s;


truncate(ZZ,RingElement) := RingElement => (n,f) -> sum select(terms f, i -> first degree i <= n);

series = method(Options => {Degree => 5})
series(Function) := Series => opts -> f -> (
     s:=0;
     for i from 0 to opts.Degree do (if f i == 0 then continue else if first degree f i > opts.Degree then break else s=s+f i);
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => opts.Degree, polynomial => s, setDegree => i -> }
     );

ZZ[x]
f = i -> x^i;
s = series f
peek s

setDegree = method()
setDegree(ZZ, Series) := Series => (n,S) -> (if n > S.maxDegree then (<< "--warning: cannot exceed max degree "  << S.maxDegree <<endl;);
     S#setDegree(n);
     S
     );
     


series(RingElement, Function) := Series => opts -> f -> (
     s:=0;
     for i from 0 to opts.Degree do (if f i == 0 then continue else if first degree f i > opts.Degree then break else s=s+f i);
     new Series from {genTerm => f, degree => opts.Degree, series => s}
     );

series RingElement := Series => opts -> f -> (
     f = f/(1_(ring f));
     df := denominator f;
     nf := numerator f;
     degnf := first degree nf;
     degdf := first degree df;
     dC := (coefficients(df,Monomials=>apply(0..opts.Degree,i->x^i)))_1;
     if not isUnit dC_(0,0) then error "lowest degree coefficient not a unit";
     a := i -> if i == 0 then (dC_(0,0))^(-1) else (dC_(0,0))^(-1)*sum(1..i, j -> -dC_(j,0)*a(i-j));    
     s := sum select(terms (nf * sum(0..opts.Degree,i -> a(i) * x^i)), i -> first degree i <= opts.Degree);
     new Series from {rationalFunction => f, degree => if degdf < 1 then infinity else opts.Degree, series => s}
     );


 

seriesOLD = method()
seriesOLD(ZZ, RingElement) := PowerSeries => (n,f) -> (
     df := denominator f;
     nf := numerator f;
     c := coefficient(1_(ring df), df); -- pulls out constant term
     s := sum select(terms lift(nf*(1/c)*sum(n+1, i -> (1-df/c)^i), ring df), i -> first degree i <= n);
     new Series from {rationalFunction => f, degree => n, series => s}
     );



series(ZZ, RingElement) := Series => (n,f) -> (
     sum(0..n, f);
     new Series from {genFunction => f, degree => first degree f n, series => sum(n+1,f)}
     );







Series+Series := (x,y) -> new Series from {generator = decider(x,y), degree => min(x#degree,y#degree), series => x+y}; -- not right
Series*Series := (x,y) -> new Series from {generator = decider(x,y), degree => min(x#degree,y#degree), series => x*y}; -- not right






-----------------------------------------------------------------------------

--=========================================================================--
--=========================================================================--
--=========================================================================--


Puiseux series
sqrt of rational function
nth root of rational function

parent class(1/x)
generatror (ring element) 
instance(ring f,FractionField)

could replace the 1/x with a pair, (1/x,FractionField)


Series == Series := (f,g) -> (
     f.rationalFunction == g.rationalFunction
     );


Series + Series := (f,g) -> (
     u:=s#0+t#0;
     new Series from {part(0,min(s#1,t#1),numgens(class(u)):1,u),min(s#1,t#1)}
     )








-- One way to do sums, do we want to do inheritance with 'growable series'?
seriesSum = method(Options => {Degree => 5})
seriesSum(Series,Series) := Series => opts -> (A,B) -> (
     -- If rational functions, add them
     if member(rationalFunction,keys A) then(
	  if member(rationalFunction,keys B) then(
	       series(A.rationalFunction + B.rationalFunction)
	       )
	  else if member(genTerm,keys B) then(
	       -- Coming Soon
	       )
	  else if member(poly,keys B) then(
	       -- Coming Soon
	       )
	  )
     else if member(genTerm, keys A) then(
	  if member(rationalFunction,keys B) then(
	       --
	       )
	  else if member(genTerm, keys B) then(
	       f := A.genTerm;
	       g := B.genTerm;
	       h := n -> f(n) + g(n);
	       series(h)
	       )
	  else if member(poly, keys B) then(
	       --
	       )
	  )
     )










-- How to do inheritance of functions

Thing1 = new Type of HashTable

thing1 = method()
thing1(ZZ) := Thing1 => n -> (
     new Thing1 from {int => n, up => (i-> i + n)}
     );

up = method()
up(Thing1,ZZ) := Thing1 => (A,i) -> (
     new Thing1 from { int => A#up(i), up => A#up}
     );

add2 = method()
add2(Thing1,Thing1) := Thing1 => (A,B) -> (
     new Thing1 from { int => A.int + B.int, up => i -> A#up(i) + B#up(i)}
     );
member(up,keys A)
A = thing1(5)
A.int
A#up
up(A,1)
B = add2(A,A)
B#up(1)
B
B = thing1(4)
A.up(3)
up(A,3)
f=




R = ZZ/17[x,y]
g = series(1,1/(1+x+y))
f = series(10,1/(1-x))
peek f
toPolynomial(f)

class f


f*g
f+g

benchmark "f*g"
ff*gg
R = QQ[x,y]
I = (ideal vars R)^11
S = R/I
(1+2*x +3*x+5*y^3)^(-1)
gg = (1+x+y)^(-1)
ff = (1-x)^(-1)
benchmark "ff * gg"
--multiplication in the engine with truncation. 
--power series ring


------ Growable Rationals
restart
GrowableRational = new Type of HashTable;
expression GrowableRational := R -> toRR R#value;
net GrowableRational := R -> toRR R#value;
toString GrowableRational := R -> toRR R#value;
tex GrowableRational := R -> toRR R#value;
html GrowableRational := R -> toRR R#value;


growableRational = method();
growableRational(ZZ,ZZ) := GrowableRational => (a,b) -> (
     new GrowableRational from {value=>a//b,growMe => (i -> (a*10^i // b)/10^i)}
     );
     
growMe = method();
growMe(GrowableRational,ZZ) := GrowableRational => (R,i) -> (
     new GrowableRational from {value => R#growMe(i), growMe => (j -> R#growMe(j+i))}
     );

growableRational(7,6)
growMe(oo,1)
growMe(oo,1)
growMe(oo,2)

