newPackage(
        "Series",
        Version => "0.1", 
        Date => "1/2010",
        Authors => {{Name => "", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "computations involving series",
        DebuggingMode => true
        )

export {series, setDegree}


Series = new Type of HashTable
expression Series := s -> expression "O(" expression(s#degree+1) expression ")" + expression truncate(s#degree, s#polynomial);
net Series := s -> net expression s;
toString Series := s -> toString expression s;
tex Series := s -> tex expression s;
html Series := s -> html expression s;


truncate(ZZ,RingElement) := RingElement => (n,f) -> sum select(terms f, i -> first degree i <= n);

series = method(Options => {Degree => 5})
series(ZZ,Function) := Series => opts -> (n,f) -> (
     s := sum(n,f);
     -- now make a new series.
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => first degree s, polynomial => s, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (newPolynomial := oldPolynomial;
		              for i from oldComputedDegree + 1 to newDegree do newPolynomial = newPolynomial + f i;
			      (newPolynomial,max(oldComputedDegree,newDegree))
		             )) });
	 
series(Function) := Series => opts -> f -> series(opts.Degree+1,f);

setDegree = method()
setDegree(ZZ, Series) := Series => (n,S) -> (if n > S.maxDegree then (<< "--warning: cannot exceed max degree "  << S.maxDegree <<endl;);
     (f,c) := S#setDegree (S#polynomial,S#computedDegree,n);
     new Series from {polynomial => f, computedDegree => c, maxDegree => S#maxDegree, degree => (min(n,S#maxDegree)), setDegree=> S#setDegree}
     );
peek s

ZZ[x]
f = i -> x^i;
F = series f
peek F
F = setDegree(3,F)     
peek F
F = setDegree(7,F)
peek F
F = setDegree(3,F)
peek F

f = i -> x^(i+1)
F = series f
peek F
F = setDegree(3,F)
F = setDegree(8,F) 
peek F

series(ZZ, RingElement) := Series => opts -> (n,f) -> (
     new Series from {degree => n, maxDegree => max(first degree f,n), computedDegree => max(first degree f,n), polynomial => f,
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (oldPolynomial,oldComputedDegree))}
     );
S = series(4,x^2 + x)     
peek S
S = setDegree(7,S)
peek S
S = setDegree(3,S)
peek S
S = setDegree(7,S)
peek S

series(RingElement, Function) := Series => opts -> (X,f) -> (
     -- Start with the zero polynomial.
     s:=0;
     -- add opts.Degree terms to s.
     for i from 0 to opts.Degree do s = s + (f i)*X^i;
     
     -- now make a new series.
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => opts.Degree, polynomial => s, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (newPolynomial := oldPolynomial;
		              for i from oldComputedDegree + 1 to newDegree do newPolynomial = newPolynomial + (f i)*X^i;
			      (newPolynomial,max(oldComputedDegree,newDegree))
		        )
		       ) 
		     }
     );
T = series(x,i -> i)
peek S
S = setDegree(7,S)
peek S
S = setDegree(1,S)
peek S
S = setDegree(3,S)
peek S


-- the following code is from modules2.m2
recipN = (n,wts,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form 1 plus terms of positive weight, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     if n <= 0 then error "expected a positive integer";
     if part(,0,wts,f) != 1 then error "expected a polynomial of the form 1 plus terms of positive weight";
     g := 1_(ring f);  -- g always has the form 1 plus terms weight 1,2,...,m-1
     m := 1;			   -- 1-f*g always has terms of wt m and higher
     tr := h -> part(,m-1,wts,h);
     while m < n do (
	  m = 2*m;
	  g = g + tr(g * (1 - tr(g * tr f)));
	  );
     if m === n then g else part(,n-1,wts,g));

rationalSeries = (ord,h) -> ( -- essentially the code used by hilbert series see: modules2.m2
	  num := numerator h;
	  if num == 0 then 0_(ring num) else (
	       wts := (options ring num).Heft;
	       (lo,hi) := weightRange(wts,num);
	       if ord <= lo then 0_(ring num) else (
		    s := part(,ord-1,wts,part(,ord-1,wts,num) * recipN(ord-lo,wts,denominator h)))));

rationalSeries(5,1/(1-x))

series RingElement := Series => opts -> h -> ( 
     	  s := rationalSeries(opts.Degree+1,h);
	   -- now make a new series.
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => opts.Degree, polynomial => s, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> 
	       (rationalSeries(newDegree+1,h),max(oldComputedDegree,newDegree)))})  

peek s 
s = series(1/(1-x))
setDegree(10,s)

methods series 


seriesOLD = method()
seriesOLD(ZZ, RingElement) := PowerSeries => (n,f) -> (
     df := denominator f;
     nf := numerator f;
     c := coefficient(1_(ring df), df); -- pulls out constant term
     s := sum select(terms lift(nf*(1/c)*sum(n+1, i -> (1-df/c)^i), ring df), i -> first degree i <= n);
     new Series from {rationalFunction => f, degree => n, series => s}
     );



series(ZZ, Function) := Series => opts -> (n,f) -> (
     sum(0..n, f);
     new Series from {genFunction => f, degree => first degree f n, series => sum(n+1,f)}
     );





-----------------------------------------------------------------------------

--=========================================================================--
--=========================================================================--
makeSeriesCompatible = method()
makeSeriesCompatible(Series,Series) := Sequence => (A,B) -> (
      if A.computedDegree == B.computedDegree then (A,B)
      else (
      	   newComputedDegree := min(A.maxDegree,B.maxDegree,max(A.computedDegree,B.computedDegree));
           A' := setDegree(newComputedDegree,A);
	   B' := setDegree(newComputedDegree,B);
	   (A',B')
	   )
      )
 
Series + Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     new Series from {degree => min(A'#degree,B'#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => A'.computedDegree, polynomial => A'.polynomial + B'.polynomial, 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree,newA.polynomial + newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     )

Series - Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     new Series from {degree => min(A'#degree,B'#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => A'.computedDegree, polynomial => A'.polynomial - B'.polynomial, 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree,newA.polynomial - newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     )


Series * Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     new Series from {degree => min(A'#degree,B'#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => A'.computedDegree, polynomial => A'.polynomial * B'.polynomial, 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree,newA.polynomial * newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     )


S = series(4,x^2 + x)  
T = series(x,i -> i)

T - T + T
setDegree(9,T - T + T)
setDegree(12,T*T)

end


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


BrokenSeries RingElement := Series => opts -> f -> (
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


 

