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
series (Ring,Function) := Series => opts -> (R,f) -> efficientSeries(0_R,-1,f);    


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


