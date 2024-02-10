export { "katsura" }

katsura = method()
katsura (ZZ,Ring) := (n,kk) -> (
     n = n-1;
     R := kk[vars(0..n)];
     L := gens R;
     u := i -> (
	  if i < 0 then i = -i;
	  if i <= n then L_i else 0_R
	  );
     f1 := -1 + sum for i from -n to n list u i;
     toList prepend(f1, apply(0..n-1, i -> 
	     - u i + sum(-n..n, j -> (u j) * (u (i-j)))
	     ))
     )

beginDocumentation()

doc /// 
    Key
    	katsura
    	(katsura,ZZ,Ring)
    Headline
    	an example of a 0-dimensional square polynomial system
    Usage
    	katsura(n,kk)
    Inputs
    	n:ZZ
	     the number of variables
        kk:Ring
	     the coefficient ring
    Outputs
    	:List
	    	of polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 512 solutions found in 2.804 seconds with 10 variables.
	    
	    The unknowns stand for the values (in [0,1])
	    of a distribution function of a field created by a mixture of 
	    a ferro-antiferro-magnetic bond at some points.
	Example
	    katsura(10,QQ)
    ///

-* TEST ///
F = katsura(10,QQ)
sols = solveSystem F
assert(#sols==512)
/// *-
