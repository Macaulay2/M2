newPackage(
        "PowerSeries",
        Version => "1.0", 
        Date => "1/2010",
        Authors => {{Name => "Chris Cunningham", 
                  Email => "cjc258@cornell.edu"},
	     {Name => "Jason McCullough", 
	     Email => "jmccullo@math.ucr.edu",
	     HomePage => "http://www.math.ucr.edu/~jmccullo/"},
	{Name => "Bart Snapp", 
	     Email => "snapp@math.ohio-state.edu",
	     HomePage => "http://www.math.ohio-state.edu/~snapp/"}
	},
        Headline => "computations involving power series",
        DebuggingMode => true
        )

export {series, setDegree, toPolynomial, dominantTerm, Series}

Series = new Type of HashTable

-- pretty needs to be modified if we work with more general series
pretty Series := s -> net new Sum from apply(apply(select(apply(s#degree+2,i -> part_i( truncate(s#degree, s#polynomial) )), p-> p!=0),expression),e -> if instance(e,Sum) then new Parenthesize from {e} else e)

expression Series := s -> pretty s + expression "O(" expression(s#degree+1) expression ")"
net Series := s -> net pretty s + expression "O(" expression(s#degree+1) expression ")"
toString Series := s -> toString pretty s + expression "O(" expression(s#degree+1) expression ")"
tex Series := s -> tex pretty s + expression "O(" expression(s#degree+1) expression ")"
html Series := s -> html pretty s + expression "O(" expression(s#degree+1) expression ")"

truncate(ZZ,RingElement) := RingElement => (n,f) -> part(,n,f);
-- (sum select(terms f, i -> first degree i <= n) + 0_(ring f));
-- The above is what was there before "part" was discovered.

dominantTerm = method()
dominantTerm(Series) := RingElement => S -> (
     -- This is bad, it depends on the monomial order:last terms toPolynomial S;
     -- This seems slow but at least correct:
     f := toPolynomial S;
     minDegree := min apply(terms f, i -> first degree i);
     part(minDegree,minDegree,f)
     )

isUnit(Series) := Boolean => A -> isUnit(dominantTerm(A));

toPolynomial = method()
toPolynomial(Series) := RingElement => s -> toPolynomial(s#degree,s);
toPolynomial(ZZ,Series) := RingElement => (n,s) -> truncate(n,(setDegree(n,s))#polynomial);

series = method(Options => {Degree => 5})

setDegree = method()
setDegree(ZZ, Series) := Series => (n,S) -> (if n > S.maxDegree then error concatenate("Cannot exceed max degree ",toString S.maxDegree," for this power series.");
     (f,c) := S#setDegree (S#polynomial,S#computedDegree,n);
     new Series from {polynomial => f, computedDegree => c, maxDegree => S#maxDegree, degree => (min(n,S#maxDegree)), setDegree=> S#setDegree}
     );

series(ZZ, RingElement) := Series => opts -> (n,f) -> (
     new Series from {degree => n, maxDegree => max(first degree f,n), computedDegree => max(first degree f,n), polynomial => f,
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (oldPolynomial,oldComputedDegree))}
     );


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
		        ))});

degree(Series) := Series => F -> F#degree;

inverse(Series) := Series => F -> (
     R := (ring F.polynomial);
     denominatorDegree := degree F;
     -- We have a function that computes the inverse of a polynomial to a given degree,
     -- given the inverse of the polynomial already given to some other degree.
     -- That function is called recip(denom,oldDegree,newDegree,oldApprox) and returns a polynomial.
     if not isUnit(dominantTerm F) then error "This series is not invertible: its dominant term is not a unit.";
     G := ((dominantTerm F)^-1) * F;
     
     new Series from {
	  polynomial => (dominantTerm F)^-1 * recip(toPolynomial G,0,denominatorDegree,1_R),
	  computedDegree => 2^(ceiling log(2,denominatorDegree+1))-1,
	  degree => denominatorDegree,
	  maxDegree => F.maxDegree,
	  setDegree => (oldPolynomial,oldComputedDegree,newDegree) -> (
               if newDegree < oldComputedDegree then (
		    (oldPolynomial,oldComputedDegree)
		    )
	       else (
		    newApprox := (dominantTerm F)^-1 * recip(((setDegree(newDegree,G)).polynomial),oldComputedDegree,newDegree,(dominantTerm F) * oldPolynomial);
	            (newApprox,newDegree)
		    )
	       )
	  }
          
     )

recip = (denom,oldDegree,newDegree,oldApprox) -> (
     wts := (options ring oldApprox).Heft;
     if wts === null then wts = {1};

     if ring denom =!= ring oldApprox then error "Rings of denominator and approximation must agree.";
     if oldDegree < (first degree oldApprox) then error "Old approximation had more accuracy than claimed.";
     if newDegree < oldDegree then error "recip does not know how to decrease precision.";
      
     -- Really this algorithm computes terms 0&1, then terms 2&3, then terms 4&5&6&7, etc.
     -- So we need to know which powers of two are relevant.
     if oldDegree == 0 then startingTwo := 0
                       else startingTwo = floor log (2,oldDegree);
     endingTwo := ceiling log (2,newDegree+1);
     oldApprox = truncate(2^startingTwo,oldApprox);
       
     -- denom is a polynomial of the form 1 plus terms of positive weight, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     if part(,0,wts,denom) != 1 then error "expected a polynomial of the form 1 plus terms of positive weight";
     g := oldApprox;  -- g always has the form 1 plus terms weight 1,2,...,m-1
     m := 2^startingTwo;			   -- 1-f*g always has terms of wt m and higher
     tr := h -> part(,m-1,wts,h);
     while m < 2^endingTwo do (
	  m = 2*m;
	  g = g + tr(g * (1 - tr(g * tr denom)));
	  );
     if m === 2^endingTwo then g else part(,2^endingTwo-1,wts,g));


Series == Series := (M,N) -> (
     -- This checks like floating point numbers -- we only check up to he precision of the least-precise one.
     precision:= min(degree M, degree N);
     truncate(precision,M#polynomial) == truncate(precision,N#polynomial))

series(RingElement) := Series => opts -> f -> (
     -- To check whether this thing has a denominator or not, we ask its ring.
     -- If it has a denominator, we need to invert the denominator to form a 
     -- power series, then multiply by the numerator.
     R := ring f;
     if R#?denominator then (
	  if denominator f == 1 then(
	       f = numerator f;
	       )
	  else(
	       num := numerator f;
	       den := denominator f;
	       return num*inverse(series(den,Degree => opts.Degree));
	       );
	  );
     
     -- If we got to this point, f does not have a denominator -- it is
     -- just a polynomial that we need to make an exact series for. 
     
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => opts.Degree, polynomial => f, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (oldPolynomial, max(oldComputedDegree,newDegree))
		    )});

series(Divide) := Series => opts -> f -> (
        num := value numerator f;
     	den := value denominator f;
	num * inverse series den
	)

series(Function) := Series => opts -> f -> ( 
     	  s := f(opts.Degree+1);
	   -- now make a new series.
     new Series from {degree => opts.Degree, maxDegree => infinity, computedDegree => opts.Degree, polynomial => s, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => (
	       (oldPolynomial,oldComputedDegree,newDegree) -> 
	       (
		    if newDegree > oldComputedDegree then (f(newDegree+1),max(oldComputedDegree,newDegree)) else
	       	    (oldPolynomial, oldComputedDegree)
	       )
	  )});  

-- efficientSeries takes the current approximation, the current computed degree, and the function f, which does
-- f(oldApproximation,oldComputedDegree,newDegree) which returns a new approximation to degree newDegree.
-- We promise not to call f if newDegree <= oldComputedDegree.
series (RingElement,ZZ,Function) := Series => opts -> (approximation,approxDegree,f) -> ( 
	   -- now make a new series.
          new Series from {degree => approxDegree, maxDegree => infinity, computedDegree => approxDegree, polynomial => approximation, 
          -- setDegree takes an old polynomial, the old computed degree, and a new degree, and needs
	  -- to know how to tack on the new terms to the old polynomial.
	  setDegree => (
	       (oldPolynomial,oldComputedDegree,newDegree) -> 
	       (
		    if newDegree > oldComputedDegree then (f(oldPolynomial,oldComputedDegree,newDegree),newDegree) else (oldPolynomial, oldComputedDegree)
	       )
	  )});
series (Ring,Function) := Series => opts -> (R,f) -> series(0_R,-1,f);    


makeSeriesCompatible = method()
makeSeriesCompatible(Series,Series) := Sequence => (A,B) -> (
     newComputedDegree := min(degree(A),degree(B));
     (
	  new Series from {degree => newComputedDegree, 
	       	    	   computedDegree => newComputedDegree,
			   maxDegree => A.maxDegree,
			   polynomial => truncate(newComputedDegree,A.polynomial),
			   setDegree => A#setDegree},
	  new Series from {degree => newComputedDegree, 
	       	    	   computedDegree => newComputedDegree,
			   maxDegree => B.maxDegree,
			   polynomial => truncate(newComputedDegree,B.polynomial),
			   setDegree => B#setDegree}
     	  )
     
     
     );
     
     
     


      --if A.computedDegree == B.computedDegree then (A,B)
      --else (
      	--   newComputedDegree := min(degree(A),degree(B));
          -- newA := setDegree(newComputedDegree,A);
	  -- newB := setDegree(newComputedDegree,B);
	  -- (newA,newB)
	  -- )
      --);
 
Series + Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     new Series from {degree => min(A#degree,B#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => A'.computedDegree, polynomial => A'.polynomial + B'.polynomial, 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree,newA.polynomial + newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     );

Series - Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     new Series from {degree => min(A#degree,B#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => A'.computedDegree, polynomial => A'.polynomial - B'.polynomial, 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree,newA.polynomial - newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     );


Series * Series := Series => (A,B) -> (
     (A',B') := makeSeriesCompatible(A,B);
     newComputedDegree = A'.computedDegree;
     -- newComputedDegree should be changed when we do Laurent Series
     new Series from {degree => min(A#degree,B#degree), maxDegree => min(A'.maxDegree,B'.maxDegree), computedDegree => newComputedDegree, polynomial => truncate(newComputedDegree ,toPolynomial(A') * toPolynomial(B')), 
	  setDegree => ((oldPolynomial,oldComputedDegree,newDegree) -> (
		    if newDegree > oldComputedDegree then (
		    	 newA := setDegree(newDegree,A);
		    	 newB := setDegree(newDegree,B);
		    	 (truncate(newDegree, newA.polynomial * newB.polynomial), newDegree)
			 )
		    else (oldPolynomial, oldComputedDegree)
		    )
	       )}
     );

Series / Series := Series => (A,B) -> A * inverse B;     

- Series := Series => A -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => - A.polynomial,
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->(
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (- newA.polynomial, newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });


RingElement * Series := Series => (f,A) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => truncate(A.computedDegree,truncate(A.computedDegree,f) * A.polynomial),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree,truncate(newA.computedDegree,f) * newA.polynomial), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });

Series * RingElement := Series => (A,f) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => truncate(A.computedDegree, A.polynomial*truncate(A.computedDegree,f)),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree, newA.polynomial*truncate(newA.computedDegree,f)), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });


RingElement + Series := Series => (f,A) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => part(,A.computedDegree,part(,A.computedDegree,f) + A.polynomial),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree,truncate(newA.computedDegree,f) + newA.polynomial), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });

Series + RingElement := Series => (A,f) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => truncate(A.computedDegree, A.polynomial+truncate(A.computedDegree,f)),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree, newA.polynomial+truncate(newA.computedDegree,f)), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });


RingElement - Series := Series => (f,A) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => truncate(A.computedDegree,truncate(A.computedDegree,f) - A.polynomial),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree,truncate(newA.computedDegree,f) - newA.polynomial), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });

Series - RingElement := Series => (A,f) -> (new Series from{
     degree => A#degree,
     maxDegree => A.maxDegree,
     computedDegree => A.computedDegree,
     polynomial => truncate(A.computedDegree, A.polynomial-truncate(A.computedDegree,f)),
     setDegree => ((oldPolynomial,oldComputedDegree,newDegree)->( 
	       if newDegree > oldComputedDegree then(
		    newA := setDegree(newDegree,A);
		    (truncate(newA.computedDegree, newA.polynomial-truncate(newA.computedDegree,f)), newA.computedDegree)
		    )
	       else (oldPolynomial, oldComputedDegree)
	       )
	  )
     });


Series * ZZ := Series => (A,f) -> A*f_(ring A.polynomial)
ZZ * Series := Series => (f,A) -> f_(ring A.polynomial) * A

Series + ZZ := Series => (A,f) -> A+f_(ring A.polynomial)
ZZ + Series := Series => (f,A) -> f_(ring A.polynomial) + A

Series - ZZ := Series => (A,f) -> A-f_(ring A.polynomial)
ZZ - Series := Series => (f,A) -> f_(ring A.polynomial) - A

Series / ZZ := Series => (A,f) -> A * (f_(ring A.polynomial))^-1
ZZ / Series := Series => (f,A) -> f_(ring A.polynomial) * inverse A

Series * QQ := Series => (A,f) -> A*(f_(ring A.polynomial))
QQ * Series := Series => (f,A) -> (f_(ring A.polynomial)) * A

Series + QQ := Series => (A,f) -> A+(f_(ring A.polynomial))
QQ + Series := Series => (f,A) -> (f_(ring A.polynomial)) + A

Series - QQ := Series => (A,f) -> A-(f_(ring A.polynomial))
QQ - Series := Series => (f,A) -> (f_(ring A.polynomial)) - A

Series / QQ := Series => (A,f) -> A * (f_(ring A.polynomial))^-1
QQ / Series := Series => (f,A) -> f_(ring A.polynomial) * inverse A

Series / RingElement := Series => (A,f) -> A * inverse series f_(ring A.polynomial)
RingElement / Series := Series => (f,A) -> f_(ring A.polynomial) * inverse A


--=========================================================================--
--Chris's space
beginDocumentation()
needsPackage "SimpleDoc";

doc ///
  Key
    PowerSeries
    Series
  Headline
    A package allowing construction and manipulation of power series in one variable.
  Description
    Text     
      A power series is normally described on paper in the form "a_0 + a_1x + a_2 x^2 + ...", and
      the ellipsis in the above description is implemented to maximize efficiency. By analogy
      with floating point numbers, a power series is only ever calculated to finite precision,
      but power series in this package also contain enough information to generate arbitrarily
      more precision when needed. Combined with some caching abilities, this means that
      power series whose coefficients are very difficult to compute can be manipulated
      without sacrificing the ability to calculate more terms at a later time.
      
      There are large documentation nodes included for @TO "Creating Series"@ and
      @TO"Operations on Series"@. You may also be interested in how @TO2((symbol ==,Series,Series),"equality of series")@ is computed.
///

doc ///
  Key
    "Creating Series"
    series
  Headline
    An overview of the various ways to create power series.
  Description
   Text
     Most series have enough information to extend their polynomial approximation to 
     arbitrary degree when needed. This means that even though sums and products of
     series will return finite-precision results, the precision can be increased at
     any time. The easiest examples of such series are:
  
     1. Creating series by the @TO2 ((series,RingElement),"input of a rational function")@,
     
     2. Creating series @TO2 ((series,Divide),"from a Divide")@, for instance to process the result of a @TO hilbertSeries@ computation,and

     3. Creating series @TO2 ((series,RingElement,Function),"given the coefficients")@, by giving a function that computes the i^{th} coefficient of the power series.

     Series can also be created that have finite precision. If the user only knows the series
     up to a certain degree, series computations can still be used on it. If the precision 
     of such a series is ever increased too far, an error will result:
          
     4. Creating @TO2 ((series,ZZ,RingElement),"series with finite precision")@.

     If the calculation of a coefficient of the power series is difficult, and a closed form is not known for the i^{th} term, a series can be created by
     giving the i^{th} polynomial approximation to the series:

     5. Creating series by @TO2( ((series,Function),"giving a function"))@ that given i, computes the i^{th} polynomial approximation to the series.
     
     The best way to implement a power series whose coefficients are difficult to compute is
     using the following construction: 
     
     6. Creating @TO2 ((series,Ring,Function),"series given the ability to calculate successive approximations")@.
                
     Methods 5 and 6 provide great flexibility and allow almost any power series to be entered, but the latter provides a
     significant computational advantage if the coefficients of the power series are difficult to compute. In the former
     case, computations will be repeated when precision is changed, but in the latter,
     previously computed coefficients will never be computed again.
    
     As an example, we will enter a power series whose coefficients involve (needless) heavy computation:
   Example
     f = i -> value factor(10^i + 1);
   Text
     If a function for the i^{th} coefficient is used to construct the series (method 3), then successive increases in precision do not redo computations:
   Example
     R = ZZ[x];
     A = series(x,f)
     time A = setDegree(24,A); -- increase precision to 24
     time A = setDegree(25,A); -- increase precision one more
   Text
     If a function for the i^{th} polynomial approximation is used instead (method 5), then successive precision increases repeat old computations:
   Example
     B = series(i -> sum(i,(j -> f(j)*x^j)))
     time B = setDegree(24,B); -- increase precision to 24
     time B = setDegree(25,B); -- increase precision one more
   Text
     Even if a closed form for the i^{th} coefficient is not known, and instead the user only knows how to recursively increase the precision, method 6 successfully uses the cache:
   Example
     g = (oldPolynomial,oldDegree,newDegree) -> oldPolynomial + sum(oldDegree+1..newDegree,j -> f(j)*x^j);
     C = series(R,g) -- watch the output: no terms have been computed yet!
     time C = setDegree(24,C); -- increase precision to 24
     time C = setDegree(25,C); -- increase precision one more 
  SeeAlso
    hilbertSeries
///    

doc ///
  Key
    (series,Divide)
  Headline
    Create a series from a Divide. 
  Usage
    F = series D
  Inputs
    D:Divide
      representing a rational function in one variable
  Outputs
    F:Series
  Description
   Text
     This method is meant for accepting the output of the @TO hilbertSeries@ command.
     Creating a Series object allows manipulations to be performed on the Hilbert series while
     reserving the option of calculating more terms later (without calling hilbertSeries).
   Example
     R = ZZ/101[x,Degrees => {2}];
     I = ideal"x2";
     H = hilbertSeries module I
     S = series H
   Text
     By default a new series has low precision, but the degree of the approximating polynomial is increased with setDegree.
   Example
     setDegree(8,S)
  SeeAlso
    hilbertSeries
    "Creating Series"
///


doc ///
  Key
    (series,Function)
  Headline
    Create a series by giving the i^{th} polynomial approximation.
  Usage
    F = series f
  Inputs
    f:Function
      which takes an integer i and returns a polynomial which equals F up to degree i
  Outputs
    F:Series
  Description
   Text
     This method is useful if it is easy to compute an approximation to degree i, but
     difficult to produce a closed formula for the i^{th} coefficient. However,
     it is not very efficient, since very time the precision of the series is changed,
     the entire polynomial approximation will be recomputed using f. To avoid this issue
     for series whose coefficients should be cached for future use, see creating a @TO2((series,Ring,Function),"series from successive approximations")@.
 
     In the following example a series for cos(x) is created: this method of construction
     is appropriate because it is annoying to produce a function for the i^{th} coefficient,
     but it is computationally easy to produce these coefficients.
   Example
     R = QQ[x];
     costerm = j -> ((-1)^j * x^(2*j) / (2*j)!);
     f = i -> sum(0..floor(i/2),(j -> costerm j));    
     cosine = series f
   Text
     By default a new series has low precision, but the degree of the approximating polynomial is increased with setDegree. 
     In this case, the old coefficients are recomputed every time the precision is increased.
   Example
     setDegree(8,cosine)
   Text
     We can do computations with the series, and then check the results up to any precision.
     
     Here is the series for sin x:
   Example 
     sinterm = j -> ((-1)^j * x^(2*j+1) / (2*j+1)!);
     f = i -> sum(0..floor((i-1)/2),(j -> sinterm j));    
     sine = series f
   Text
     And we can check an identity:
   Example
     shouldBeOne = cosine * cosine + sine * sine
   Text
     Even up to degree 100:
   Example
     setDegree(100,shouldBeOne)
  SeeAlso
    "Creating Series"
///

doc ///
  Key
    (series,RingElement)
  Headline
    Create a series from a rational function.
  Usage
    F = series r
  Inputs
    r:RingElement
      a rational function in one variable
  Outputs
    F:Series
  Description
   Text
    This is a straightforward constructor for a series from a rational function.
   Example
     R = ZZ/101[x];
     F = series(1/(1-x))
     G = series(1-x)
     G == inverse F
  Caveat
     If the denominator of the rational function is not invertible even in the power series ring, an error will result.
  SeeAlso
    "Creating Series"
///

doc ///
  Key
    (series,RingElement,Function)
  Headline
    Create a series by giving the i^{th} coefficient.
  Usage
    F = series (v,f)
  Inputs
    v:RingElement
      The variable to use in the power series
    f:Function
      which takes an integer i and returns the i^{th} coefficient (degree zero in the ring, please!)
  Outputs
    F:Series
  Description
   Text
    This is a straightforward constructor and is useful when it is easy to write a closed form
    for the i^{th} coefficient of the power series. If this is not easy, see @TO "Creating Series"@
    for an exposition of other series creation methods.
   Example
     R = ZZ[x];
     F = series(x,i -> i^2)
     setDegree(10,F)
  SeeAlso
    "Creating Series"
///

doc ///
  Key
    (series,ZZ,RingElement)
  Headline
    Create a series of finite precision from a polynomial.
  Usage
    F = series(n,f)
  Inputs
    n:ZZ
      The finite degree to which f is an accurate representation of the series
    f:RingElement
      a polynomial in one variable
  Outputs
    F:Series
  Description
   Text
    All series are only computed up to a finite precision at any time, but if only
    a finite precision is possible for the series, it can be constructed in this way.
    Such a series can interact with normal series, but an error will be returned if
    any resulting expression has its precision increased beyond the limit n.
 
    Below we create the series F = 1+x to precision 3, and compute with the series G = 1/(1+x).
   Example
     R = ZZ/101[x];
     F = series(3,1+x)
     G = series(1/(1+x))
   Text
     Although G is known to possibly infinite precision, F*G is only equal to 1 up to a finite precision.
   Example
     F*G
   Text
     Trying to check whether F*G is 1 to higher precision would return an error.
  SeeAlso
    "Creating Series"
///

doc ///
  Key
    (series,Ring,Function)
    (series,RingElement,ZZ,Function)
  Headline
    Create a series using an inductive definition.
  Usage
    F = series(R,f)
    F = series(approx,n,f)
  Inputs
    R:Ring
      The ring the new power series will live in
    f:Function
      a function that takes arguments (oldPolynomial,oldComputedDegree,newDegree) and returns a new polynomial.
  Outputs
    F:Series
  Description
   Text
    All series are only computed up to a finite precision at any time, and internally
    there is always a function like f involved, which takes an old approximation and
    possibly uses it to find the new approximation.

    This is somewhat complex: if your series has an easy formula for its general term,
    or the terms will not be computationally hard to find, you should use a different
    method from @TO "Creating Series"@.
 
    Below we create the series whose i^{th} coefficient is the i^{th} Fibonacci number.
   Example
     R = ZZ[x];
   Text
     The function f in this case, then, takes an old polynomial and
     adds on terms, starting with oldComputedDegree+1 and going up to newDegree.
   Example  
     f = (oldPolynomial,oldComputedDegree,newDegree) -> (
          newPolynomial := oldPolynomial;
	  for i from oldComputedDegree + 1 to newDegree do 
	       newPolynomial = newPolynomial + x^i * sum flatten entries (coefficients part(i-2,i-1,newPolynomial))_1;
          newPolynomial);
   Text
     We can create a series that doesn't know any of its terms, but only knows how to add
     more terms to itself, using series(Ring,Function):
   Example
     F = series(R,f)     
   Text
     But of course using the Fibonacci series starting with no terms will never create any terms:
   Example
     setDegree(10,F)
   Text
     Instead if you have already computed some of the terms, you can use series(RingElement,ZZ,Function)
     to give a first approximation and its precision. In the case of this Fibonacci series,
     a first approximation is necessary to get the right series.
   Example
     G = series(1+x,1,f)
     setDegree(10,G)     
   Text
     Of course you need to be careful: since old coefficient calculations are not re-checked,
     different starting approximations can give subtly different resulting series. Here
     we start with x^2 + x^3 and use the Fibonacci code for generating new series terms.
   Example
     H = series(x^2+x^3,3,f)
     setDegree(10,H)
  SeeAlso
    "Creating Series"
///

doc ///
  Key
    dominantTerm
    (dominantTerm,Series)
  Headline
    Find the dominant term of a series.
  Usage
    t = dominantTerm F
  Inputs
    F:Series
  Outputs
    t:RingElement
      the lowest-degree nonzero term of the power series.
///
doc ///
  Key
    toPolynomial
    (toPolynomial,Series)
    (toPolynomial,ZZ,Series)
  Headline
    Return a polynomial approximation to a series.
  Usage
    f = toPolynomial f
  Inputs
    F:Series
  Outputs
    f:RingElement
      a polynomial approximation to F
  Description
    Text
      toPolynomial does not return the best approximation currently known: sometimes
      calculations have been done in the past which are not displayed if the series
      has had its precision artificially decreased with setDegree.
      
      Below a series is created, then its precision is reduced.
    Example
      R = ZZ[x];
      F = series(1/(1-x))
      F = setDegree(2,F)
    Text
      The resulting polynomial approximation is of the lower precision, as requested by the user.
    Example
      toPolynomial F
    Text
      Regardless of how much precision is currently being displayed or how much had been calculated,
      the user can request a specific amount of precision using toPolynomial(ZZ,Series).
    Example
      toPolynomial(8,F)
  SeeAlso
    setDegree            
///
doc ///
  Key
    (symbol ==, Series, Series)
  Headline
    Equality testing for series
  Description
   Text
    By analogy with floating-point numbers, equality of two objects is only tested up to the precision of the least precise object.

    The number 1. has finite precision, so it will be "equal" to one of the numbers below but unequal to the other.
   Example
    1.
    1. == 1.0000000000000007
    1. == 1.00000000000000007
   Text

    Series work the same way:
   Example
    R = ZZ[x]
   Text  
    The series 1/(1-x) has finite precision 5 at first, so it will be "equal" to one of the series below but not the other.
   Example
    series(1/(1-x))
    series(1/(1-x)) == series(1 + x +x^2 + x^3 + x^4 + 100*x^5)
    series(1/(1-x)) == series(1 + x +x^2 + x^3 + x^4 + x^5     + 100*x^6)
///
doc ///
  Key
    setDegree
    (setDegree,ZZ,Series)
    [series,Degree]
  Headline
    Set the computed degree of a series
  Description
   Text
    All series are only computed to finite precision, but almost every series remembers
    how to calculate itself to higher precision. To see which types of series can have their
    precision increased, look at @TO "Creating Series"@. This precision change is accomplished
    with either the setDegree function or the Degree option when creating the series. 
   Example
    R = ZZ[x];
   Text
    By default, a power series is computed to degree 5.
   Example
    F = series(1/(1-x))
   Text
    The Degree option to every series constructor allows the initial computed degree to be anything.
    This can be used either to get more computation or to prevent computation if the coefficients are difficult to find.
   Example
    G = series(1/(1-x),Degree => 2)
   Text
    Any series can have its degree increased or decreased using the setDegree command.
   Example
    G = setDegree(10,G)
    G = setDegree(1,G)
   Text
    
    If the precision of a series is artificially decreased, the computations are not discarded:
    instead they are cached, and coefficients are only recomputed if they are new.
   
    Here is a series whose coefficients are difficult to compute since they involve needless factoring:
   Example
    H = series(x,i -> (value factor(10^i+1)))
   Text
    So it takes a long time to increase its precision to 25.
   Example
    time H = setDegree(25,H);
   Text
    If we lower its precision and then raise it again, it takes no time at all: the old calculations are stored.
   Example
    H = setDegree(2,H)
    time H = setDegree(25,H);
  SeeAlso
   "Creating Series"  
///

--=========================================================================--
--Jason's space

doc ///
  Key
    "Operations on Series"
    (symbol +,Series,Series)
    (symbol *,Series,Series)
    (symbol -,Series,Series)
    (symbol /,Series,Series)
    (symbol *,QQ,Series)
    (symbol +,QQ,Series)
    (symbol -,QQ,Series)
    (symbol /,QQ,Series)
    (symbol *,RingElement,Series)
    (symbol +,RingElement,Series)
    (symbol -,RingElement,Series)
    (symbol /,RingElement,Series)
    (symbol *,Series,QQ)
    (symbol *,Series,RingElement)
    (symbol *,Series,ZZ)
    (symbol +,Series,QQ)
    (symbol +,Series,RingElement)
    (symbol +,Series,ZZ)
    (symbol -,Series,QQ)
    (symbol -,Series,RingElement)
    (symbol -,Series,ZZ)
    (symbol /,Series,QQ)
    (symbol /,Series,RingElement)
    (symbol /,Series,ZZ)
    (symbol -,Series)
    (symbol *,ZZ,Series)
    (symbol +,ZZ,Series)
    (symbol -,ZZ,Series)
    (symbol /,ZZ,Series)
  Headline
    Operations on Series
  Description
   Text
        Series may be added, subtracted, and multiplied.  The inverse of a
	series and negative of a series are implemented.  Series may be divided provided that the divisor
	is a unit.  Similar operations involving one series and one integer or rational number are also possible.
	First we show some series operations, nothing that the particular method for constructing a series
	does not matter.  
   Example
        R = QQ[x]
	A = series(x)
	B = series(1/(1 - x)^2)
	C = series(x,i->1/(i!))
	A + B
	A - B
	B * B
	A * A
	A / B
	5 * A
	3 - B
	inverse B
   Text
   	As expected, operations may be composed in any order.
   Example
	A * (B - C) * inverse(B)/7 - 5
   Text
        If we take two series with different precisions, then the sum, difference, product, quotient of them
	has the minimum of the two precisions.
   Example
        R = QQ[x]
	A = series(1/(1-x))
	B = series(x^2 + x)
	C = setDegree(3,A)
	B + C
   Text
        If we want more terms than are currently computed, we can use setDegree to show all terms up to degree n
	even after doing arbitrarily many operations.
   Example
        R = QQ[x]
	A = series(1/(1-x))
	B = series(x^2)
	C = A + B
	setDegree(10,C)
	setDegree(20,C)
  SeeAlso
        setDegree
///

--=========================================================================--
