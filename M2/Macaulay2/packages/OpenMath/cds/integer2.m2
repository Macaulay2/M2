-- todo: ,  ord
-- done: class, euler, divides, eqmod, neqmod, modulo_relation


eulerphi := n -> value(apply(factor n, i-> (i#0-1)*((i#0)^(i#1-1))))
ord := (p,n) -> (
	c := 0;
	while (0 == (n % p)) do (
		c = c + 1;
		n = n // p;
	);
	c
)

OMSEvaluators#"integer2" = new MutableHashTable;
OMSEvaluators#"integer2"#"euler" = (args, attrs) -> ( a := apply(args, fromOpenMath); eulerphi(a#0) )
OMSEvaluators#"integer2"#"divides" = (args, attrs) -> ( a := apply(args, fromOpenMath); 0 == (a#1) % (a#0) )
OMSEvaluators#"integer2"#"eqmod" = (args, attrs) -> ( a := apply(args, fromOpenMath); mod(a#0, a#2) == mod(a#1, a#2) )
OMSEvaluators#"integer2"#"neqmod" = (args, attrs) -> ( a := apply(args, fromOpenMath); mod(a#0, a#2) != mod(a#1, a#2) )
OMSEvaluators#"integer2"#"class" = (args, attrs) -> ( a := apply(args, fromOpenMath); mod(a#0, a#1)  )
OMSEvaluators#"integer2"#"modulo_relation" = (args, attrs) -> ( 
	-- This symbol represents a univariate function, whose argument should be an integer.
	-- When applied to an integer m, it denotes the equivalence relation of being
	-- equal modulo m on Z.
	
	a := apply(args, fromOpenMath); 
	m := a#0;
	(x,y) -> (mod(x, m) == mod(y,m))
)
OMSEvaluators#"integer2"#"ord" = (args, attrs) -> ( 
	-- This symbol denotes a binary function. Its first argument should be a prime
	-- number p, the second an integer n.
	-- When applied to p and n, it represents the highest power of p occurring in a
	-- factorization of n.
	
	a := apply(args, fromOpenMath); 
	p := a#0; n := a#1;
	ord(p,n)
)
     

     

            



