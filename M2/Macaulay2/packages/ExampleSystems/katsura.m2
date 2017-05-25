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

doc /// 
    Key
    	katsura
    	(katsura,ZZ,Ring)
    Headline
    	an example of a 0-dimensional square polynomial system 
    Description
    	Text
	    The unknowns stand for the values (in [0,1])
	    of a distribution function of a field created by a mixture of 
	    a ferro-antiferro-magnetic bond at some some points. 
	Example
	    F = katsura(3,QQ)
	    sols = solveSystem F
	    #sols
    ///

TEST ///
F = katsura(10,QQ)
assert(
    first timing (sols = solveSystem F;) 
    < 10
    )
assert(#sols==512)
///