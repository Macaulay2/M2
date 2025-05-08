newPackage(
     "SymmetricPolynomials",
     Version => "1.0",
     Date => "May 20 2009",
    Authors => {{Name => "Alexandra Seceleanu", HomePage => "https://aseceleanu.github.io/"}},
     Headline => "symmetric polynomials",
     Keywords => {"Combinatorics", "Representation Theory"},
     DebuggingMode => false
     )

export {"buildSymmetricGB","elementarySymmetric"}

mons = (X,i)-> (
    n := #X;
    a := unique flatten apply( apply( partitions (i), p-> toList p| for i from #p to n-1 list 0),q-> permutations q);
    return apply(a, r-> product for i to n-1 list X_i^(r_i))
    )


symring= R->(
     X:= flatten entries vars R;
     n := #X;
     w := (for i to n-1 list (1))|toList(1..n);
     e := getSymbol "e";
     S := (coefficientRing R)[X,e_1..e_n,MonomialOrder=>{Weights => w,Lex}];
     return S
      )
 
protect elementarySymmetricRing
elementarySymmetricPolynomialRing =(cacheValue symbol  elementarySymmetricRing)symring
 
buildSymmetricGB =method();
buildSymmetricGB (PolynomialRing) := R -> (
	n := # flatten entries vars R; 
	S := elementarySymmetricPolynomialRing R;
     	xvars := select(n,gens S,i->true);
	svars :=select(2*n,gens S,i->true)-set xvars;
	x := local x;
	A :=coefficientRing(R)[svars,xvars][x];
 	svars =apply(svars,i->(map(A,S))(i));
	xvars =apply(xvars,i->(map(A,S))(i));
	g :=x^n+sum for i to n-1 list svars_i*x^(n-i-1);
	l := {};
	for i to n-1 do (
	     f :=sub(g,x=>-xvars_(n-i-1));
	     l = append(l,f);
	     g=g//(x+xvars_(n-i-1));
		);
	F := map(S,A);
	l= apply(l,i-> F i);
	use R;
	  return l
)


     
elementarySymmetric = method();
elementarySymmetric (RingElement):=  f -> (
     R := ring f;
     n := # flatten entries vars R; 
     if n<2 then return f;
     l := buildSymmetricGB(R);
     I := ideal l;
     S := ring I;
     forceGB matrix {l};
     F := map(S,R);
     answer := F(f)%I;
     xvars := select(n,gens S,i->true);
     svars := gens S -set xvars;
     if isSubset(support(answer),svars) then (use R; return answer)
     else (use R; print "your input is not a symmetric polynomial")
     )
     

elementarySymmetric (PolynomialRing):=  R->(
     forceGB matrix{buildSymmetricGB R};
     return map(elementarySymmetricPolynomialRing R,R)
     )



beginDocumentation()

document {
	Key =>SymmetricPolynomials,
	Headline => "the algebra of symmetric polynomials",
	PARA{"This package uses an explicit description of the Groebner basis of the ideal of obvious relations in this algebra based on:"},
	PARA{"Grayson, Stillmann - Computations in the intersection theory of flag varieties, preprint, 2009"},
	PARA{"Sturmfels - Algorithms in Invariant Theory, Springer Verlag, Vienna, 1993"}
}

document {
	Key =>elementarySymmetric,
	Headline => "expression in terms of elementary symmetric polynomials",
	Inputs => {"f, a symmetric", TO RingElement},
	Outputs =>{"the expression of f in terms of the elementary symmetric functions e_i"},
	Usage => "elementarySymmetric f",
	Caveat => {"if the input is not symmetric the function will announce this"},
EXAMPLE lines ///
	n=5;
	R=QQ[x_1..x_n];
	f=(product gens R)*(sum gens R);
	elementarySymmetric f
///,
	PARA{"This function should work up to a size of 15 variables in the base ring."},
	PARA{"This function is part of the package SymmetricPolynomials."}
}

document {
	Key =>(elementarySymmetric,RingElement),
	Headline => "expression in terms of elementary symmetric polynomials",
	Inputs => {"f"=> {"a symmetric", TO RingElement}},
	Outputs =>{"the expression of f in terms of the elementary symmetric functions e_i"},
	Usage => "elementarySymmetric f",
	Caveat => {"if the input is not symmetric the function will announce this"},
EXAMPLE lines ///
	n=5;
	R=QQ[x_1..x_n];
	f=(product gens R)*(sum gens R);
	elementarySymmetric f
///,
	PARA{"This function should work up to a size of 15 variables in the base ring"},
	PARA{"This function is part of the package SymmetricPolynomials."}
}

document {
	Key =>(elementarySymmetric,PolynomialRing),
	Headline => "elementary symmetric polynomials algebra",
	Inputs => {"R"=>{ "a", TO PolynomialRing}},
	Outputs =>{"a map from R adjoin the elementary symmetric functions e_i to R"},
	Usage => "elementarySymmetric R",
EXAMPLE lines ///
	n=5;
	R=QQ[x_1..x_n];
	elementarySymmetric R
///,
	PARA{"This function should work up to a size of 15 variables in the base ring"},
	PARA{"This function is part of the package SymmetricPolynomials."}
}

document {
	Key =>buildSymmetricGB,
	Headline => "Groebner basis of elementary symmetric polynomials algebra",
	Inputs => {"R"=>{ "a", TO Ring}},
	Outputs =>{"the Groebner basis of the elementary symmetric algebra"},
	Usage => "buildSymmetricGB R",
	EXAMPLE lines ///
	n=5;
	R=QQ[x_1..x_n];
	buildSymmetricGB R
///,
	PARA{"This function should work up to a size of 15 variables in the base ring"},
	PARA{"This function is part of the package SymmetricPolynomials."}
}

document {
	Key =>(buildSymmetricGB,PolynomialRing),
	Headline => "Groebner basis of elementary symmetric polynomials algebra",
	Inputs => {"R"=>{ "a", TO PolynomialRing}},
	Outputs =>{"the Groebner basis of the elementary symmetric algebra"},
	Usage => "buildSymmetricGB R",
	EXAMPLE lines ///
	n=5;
	R=QQ[x_1..x_n];
	buildSymmetricGB R
///,
	PARA{"This function should work up to a size of 15 variables in the base ring"},
	PARA{"This function is part of the package SymmetricPolynomials."}
}
end
