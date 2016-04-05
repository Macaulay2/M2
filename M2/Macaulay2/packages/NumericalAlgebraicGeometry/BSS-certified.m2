------------------------------------------------------
-- supporting routines for experimental code
-- implementing certified tracking 
-- that emulates Blum-Shub-Smale machine
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------


-- IN: R, polynomial ring in n+1 vars
--     d, list of equation degrees
-- OUT: conjectured (by Shub and Smale) good initial pair (f,z_0), f\in Hd, z\in P^n
--      f, List; z_0, List of one element
goodInitialPair = method(Options=>{GeneralPosition=>false})
goodInitialPair List := o -> T -> (
     R := commonRing T;
     d := T/first@@degree;
     lastVar := last gens R;
     n := numgens R - 1;
     (S, solsS) := ( transpose matrix{apply(n, i->(sqrt d#i)*lastVar^(d#i - 1)*R_i)}, 
     	  transpose matrix{toList (n:0_CC) | {1}} );
      if o.GeneralPosition === true then (
	  coord'transform := randomUnitaryMatrix(n+1);
     	  inv'coord'transform := solve(coord'transform, map CC^(n+1));
     	  coord'change := map(R,R,(vars R) * transpose coord'transform);	       	    
     	  base'S := flatten entries coord'change ((map(R,ring S, vars R)) S);
     	  base'sol := entries transpose (inv'coord'transform * solsS);
	  (base'S, base'sol)
	  ) else (flatten entries S, entries transpose solsS)
     )

-- IN: ds, list of equation degrees                                                                                                         
-- OUT: a random system in unit ball Hd \cap {systems with no two highest terms in x_n}  
randomHd'NoHighXn = method()
randomHd'NoHighXn List := ds -> (
     n := #ds;
     x := symbol x;
     R := CC_53(monoid [vars(53..n+53)]); 
     sqrt'n := 1/sqrt n;
     u := randomInComplexUnitBall (dimHd ds - (n+1)*#ds);     
     i := -1; --counter
     ret := apply(ds,d->sum( 
	       ((ideal gens R)^d)_*, 
	       m->(
	       	    a := first first listForm m; -- exponent vector
	     	    if last a > d-2 then 0 else (
	       	    	 i = i + 1;
     	       	    	 u_(i,0) * sqrt((sum a)! / product(a, d->d!))*m -- ... * sqrt of multinomial coeff
	       	    	 )
		    )
	       ));
     assert(i+1 == dimHd ds - (n+1)*#ds);
     ret          
     );


-- IN: ds, list of equation degrees                                                                                                         
-- OUT: a random system in unit sphere S \subset Hd 
randomSd = method()
randomSd List := ds -> (
     n := #ds;
     x := symbol x;
     R := CC_53(monoid [vars(53..n+53)]); 
     sqrt'n := 1/sqrt n;
     u := randomInComplexUnitSphere dimHd ds;     
     i := -1; --counter
     ret := apply(ds,d->sum( 
	       ((ideal gens R)^d)_*, 
	       m->(
	       	    a := first first listForm m; -- exponent vector
	       	    i = i + 1;
     	       	    u_(i,0) * sqrt((sum a)! / product(a, d->d!))*m -- ... * sqrt of multinomial coeff
	       	    )
	       ));
     assert(i+1 == dimHd ds);
     ret          
     );

TEST ///
debug NumericalAlgebraicGeometry
assert( abs(sum(randomSd {2,3,4,5} / BombieriWeylNormSquared)-1)<1e-8 )
///

randomInitialPair = method()
randomInitialPair List := T -> (
-- for a homogeneous system constructs a start system and one root 
-- IN:  T = list of polynomials 
-- OUT: (S,solsS}, where 
--      S     = list of polynomials, 
--      solsS = list of sequences
     R := commonRing T;
     n := numgens R - 1; 
     Ml := flatten entries randomInComplexUnitBall dimHd(T/first@@degree);
     M := map(CC^n, n+1, (i,j)->Ml#((n+1)*i+j)); -- n by n+1 matrix formed from n^2+n (first) entries of Ml    
     coord'transform := last SVD M;
     --oMl := flatten entries randomInComplexUnitBall (dimHd(T/first@@degree)-n);
     --M := map(CC^n,CC^1,0) | map(CC^n, n, (i,j)->oMl#(n*i+j)); -- 0-column | n by n matrix formed from n^2 (first) entries of Ml    
     --O := flatten entries randomInComplexUnitSphere((n+1)^2);
     --coord'transform := last SVD map(CC^(n+1), n+1, (i,j)->O#((n+1)*i+j));
     inv'coord'transform := solve(coord'transform, map CC^(n+1));
     coord'change := map(R,R,(vars R) * transpose coord'transform);
     --(good'sys, good'sol) := goodInitialPair(R,T/first@@degree);
     good'sol := transpose matrix{toList (n:0_CC)|{1}};
     sol := inv'coord'transform * good'sol; -- root
     good'sys := transpose matrix{randomHd'NoHighXn(T/first@@degree)};  
     h := coord'change ((map(R,ring good'sys, vars R)) good'sys); 
     ret := sqrt(1-(normF M)^2)*h + diagonalMatrix apply(T,f->(
	       d := sum degree f;
	       (sum(numgens R, i->R_i*conjugate sol_(i,0)))^(d-1) * sqrt d 
	       )) * M * transpose vars R;     
     assert (norm2 sub(ret,transpose sol)<0.0001); -- just a check
     (flatten entries ret, {flatten entries sol})
     )     

