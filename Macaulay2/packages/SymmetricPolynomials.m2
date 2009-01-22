-- -*- coding: utf-8 -*-
newPackage(
     "SymmetricPolynomials",
     Version => "0.1",
     Date => "June 2008",
     Authors => {
	  {Name => "Alexandra Seceleanu", HomePage => "http://www.math.uiuc.edu/~asecele2/"},
	  	  },
     Headline => "symmetric polynomials",
     DebuggingMode => true
     )

export {elementalSymm}

mons = (X,i)-> (
    n := #X;
    a := unique flatten apply( apply( partitions (i), p-> toList p| for i from #p to n-1 list 0),q-> permutations q);
    return apply(a, r-> product for i to n-1 list X_i^(r_i))
    )


s := local s;


symring= R->(
     X:= flatten entries vars R;
     n := #X;
     w := (for i to n-1 list (1))|toList(1..n);
     S = (coefficientRing R)[X,s_1..s_n,MonomialOrder=>{Weights => w,Lex}];
     return S
      )
 
elementarySymmetricPolynomialRing =(cacheValue symbol  elementarySymmetricRing)symring
 
elementalSymm = method();
elementalSymm (RingElement):=  f -> (
     R := ring f;
     n := # flatten entries vars R; 
     if n<2 then return f;
     S := elementarySymmetricPolynomialRing R;
     X= select(n,gens S,i->true);
     use S;--
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
---------New line: it creates a new ring whose variables are the elementary symmetric polynomials
     F=QQ[s_1..s_n];
     answer=substitute(answer,F);
     E:=QQ[e_1..e_n];
     MAP=map(E,F,vars E);
     answer=MAP(answer);
     return answer;
     )
     

elementalSymm (PolynomialRing):=  R->(
     return map(R,elementarySymmetricPolynomialRing R)
     )

beginDocumentation()

doc ///
  Key
    elementalSymm
    (elementalSymm, RingElement)
  Headline
    Expresses a symmetric polynomial as a sum of elementary symmetric functions
  Usage
    elementalSymm(f)
  Inputs
    f:RingElement
      A symmetric polynomial in n variables  
  Outputs 
    G:RingElement
      The representation of f as a sum of elementary symmetric functions.
  Description
    Text    
      Expresses a symmetric polynomial as a sum of elementary symmetric functions
    
    Example
      R=QQ[x_0,x_1,x_2,x_3]
      q=x_0^2*x_1^2*x_2^2+x_0^2*x_1^2*x_2*x_3+x_0^2*x_1*x_2^2*x_3+x_0*x_1^2*x_2^2*x_3+x_0^2*x_1^2*x_3^2+x_0^2*x_1*x_2*x_3^2+x_0*x_1^2*x_2*x_3^2+x_0^2*x_2^2*x_3^2+x_0*x_1*x_2^2*x_3^2+x_1^2*x_2^2*x_3^2
      elementalSymm(q)
///

