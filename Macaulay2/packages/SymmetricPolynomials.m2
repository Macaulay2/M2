-- -*- coding: utf-8 -*-
newPackage(
     "SymmetricPolynomials",
     Version => "0.1",
     Date => "23 November 2007",
     Authors => {
	  {Name => "Alexandra Seceleanu", HomePage => "http://www.math.uiuc.edu/~asecele2/"},
	  	  },
     Headline => "symmetric polynomials",
     DebuggingMode => true
     )

export {elementarySymmetric}

mons = (X,i)-> (
    n := #X;
    a := unique flatten apply( apply( partitions (i), p-> toList p| for i from #p to n-1 list 0),q-> permutations q);
    return apply(a, r-> product for i to n-1 list X_i^(r_i))
    )




symring= R->(
     X:= flatten entries vars R;
     n := #X;
     w := (for i to n-1 list (1))|toList(1..n);
     S = (coefficientRing R)[X,s_1..s_n,MonomialOrder=>{Weights => w,Lex}];
     return S
      )
 
elementarySymmetricPolynomialRing =(cacheValue symbol  elementarySymmetricRing)symring
 
elementarySymmetric = method();
elementarySymmetric (RingElement):=  f -> (
     R := ring f;
     n := # flatten entries vars R; 
     if n<2 then return f;
     S := elementarySymmetricPolynomialRing R;
     X= select(n,gens S,i->true);
     I :=ideal for i from 1 to n list ( s_i-sum apply(subsets(n,i),l->product(apply(l,u->S_(u)))));
     l := for i from 1 to n list (
	mons(X,i)
	  );
     l1 := for i from 1 to n-1 list (
	  flatten (for j from 1 to i list ((-1)^j*s_j*l_(i-j))|l_i|{(-1)^(i+1)*s_(i+1)})
	  );
     l = {l_0|{-s_1}}|l1;
     grob:= for i from 1 to #l list ({sum select(l_(i-1), j->j<=S_(i-1)^i )});
     m := matrix grob; 
     forceGB m;
     F := map(S,R);
     answer := F(f)%I;
     use ring f;
     return answer;
     )
     

elementarySymmetric (PolynomialRing):=  R->(
     return map(R,elementarySymmetricPolynomialRing R)
     )



beginDocumentation()

document {
	Key => {elementarySymmetric},
	Headline => "decomposition into elementary symmetric polynomials",
	Usage => "",
	Inputs => { "a Ring or a Polynomial" },
	Outputs => {{ "the polynomial in the elementary symmetric polynomials that represents the input" }},
	}

