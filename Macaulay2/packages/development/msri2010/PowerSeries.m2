newPackage(
        "PowerSeries",
        Version => "0.1", 
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

export {series, setDegree, toPolynomial, dominantTerm, seriesPartialSums}

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
	


 


seriesPartialSums = method(Options => {Degree => 5})
seriesPartialSums(Function) := Series => opts -> f -> ( 
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


end


--=========================================================================--
--Chris's space
beginDocumentation()
needsPackage "SimpleDoc";

doc ///
  Key
    PowerSeries
  Headline
    A package allowing construction andmanipulatioon of power series in one variable.
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
      @TO"Operations on Series"@.
///

doc ///
  Key
    "Creating Series"
  Headline
    An overview of the various ways to create power series.
  Description
   Text
     Most series have enough information to extend their polynomial approximation to 
     arbitrary degree when needed. This means that even though sums and products of
     series will return finite-precision results, the precision can be increased at
     any time. The easiest examples of such series are:
     
     1. Creating series by the @TO2 (series,RingElement),"input of a rational function"@.
     
     2. Creating series @TO2 (series,Divide),"from a Divide"@, for instance to process the result of a @TO hilbertSeries@ computation.

     3. Creating series @TO2 (series,RingElement,Function)@ "given the coefficients"@, by giving a function that computes the i^{th} coefficient of the power series.

     4. Creating series by @TO2( (seriesPartialSums,Function) "giving a function") that given i, computes the i^{th} polynomial approximation to the series.

     Series can also be created that have finite precision. If the user only knows the series
     up to a certain degree, series computations can still be used on it. If the precision 
     of such a series is ever increased too far, an error will result.
     
     5. Creating @TO2 (series,ZZ,RingElement),"series with finite precision"@.
     
     The best way to implement a power series whose coefficients are difficult to compute is
     using one of the following two constructions: 
     
     6. Creating @TO2 (series,Ring,Function),"series given the ability to calculate individual terms"@, and
     
     7. Creating @TO2 (series,RingElement,ZZ,Function),"series given the ability to calculate individual terms"@ where some computation is already done.
     
     Methods 4, 6, and 7 provide great flexibility and allow almost any power series to be entered, but the last two provide a
     significant computational advantages if the coefficients of the power series are difficult to compute. In the last two cases,
     previously computed coefficients will never be computed again.
    
     As an example, we will enter a power series whose coefficients involve (needless) heavy computation:
   Example
     f = i -> value factor(10^i + 1);
   Text
     If a function for the i^{th} coefficient is used to construct the series, then successive increases in precision do not redo computations:
   Example
     R = ZZ[x];
     A = series(x,f);
     time A = setDegree(39,A);
     time A = setDegree(40,A);
   Text
     If a function for the i^{th} polynomial approximation is ussed instead, then successive precision increases repeat old computations:
   Example
     B = seriesPartialSums(i -> sum(i,(j -> f(j)*x^j)));
     time B = setDegree(39,B);
     time B = setDegree(40,B);
   Text
     Even if a closed form for the i^{th} coefficient is not known, and instead the user only knows how to recursively increase the precision, one of methods 6 or 7 successfully uses the cache:
   Example
     C = series(R,(oldPolynomial,oldDegree,newDegree) -> oldPolynomial + sum(oldDegree+1:newDegree,j -> f(j)*x^j));
     time C = setDegree(40,C);
     time C = setDegree(41,C); 
///    



--=========================================================================--
--Jason's space

doc ///
  Key
    "Operations on Series",
    (Series + Series),
    (Series * Series),
    (Series - Series),
    (Series / Series)
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
	A * (B - C) * inverse(A)/7 - 5
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
 ///

--=========================================================================--
