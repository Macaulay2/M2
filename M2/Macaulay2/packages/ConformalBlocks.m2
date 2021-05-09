newPackage(
    "ConformalBlocks",
    Version => "2.4", 
    Date => "June 22, 2018",
    Authors => {
	{Name => "Dave Swinarski", Email => "dswinarski@fordham.edu"}
	},
    PackageExports => { "LieTypes" },
    Headline => "for conformal block divisors",
    Keywords => {"Commutative Algebra"},
    Certification => {
	 -- same article as for package LieTypes
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Software for computing conformal block divisors on bar M_0,n",
	  "acceptance date" => "2 August 2018",
	  "published article URI" => "https://msp.org/jsag/2018/8-1/p08.xhtml",
	  "published article DOI" => "10.2140/jsag.2018.8.81",
	  "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x08-LieTypes.m2",
	  "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/LieTypes.m2",
	  "release at publication" => "923fbcc7c77b23f510bb0d740e00fc1722a2f397",	    -- git commit number in hex
	  "version at publication" => "0.5",
	  "volume number" => "8",
	  "volume URI" => "https://msp.org/jsag/2018/8-1/"
	  }
    )

export {
    "ConformalBlockVectorBundle",
    "conformalBlockVectorBundle",       
    "symmetrizedConformalBlockDivisor",
    "SymmetricDivisorM0nbar",
    "coefficientList",
    "symmetricDivisorM0nbar",    
    "scale",    
    "symmetricCurveDotDivisorM0nbar",
    "basisOfSymmetricCurves", 
    "FdotBjIntMat", 
    "symmetricFCurves",
    "killsCurves", 
    "isSymmetricFDivisor",
    "isExtremalSymmetricFDivisor", 
    "canonicalDivisorM0nbar", 
    "kappaDivisorM0nbar",  
    "psiDivisorM0nbar",
    "conformalBlockRank",
    "conformalBlockDegreeM04bar",
    "FCurveDotConformalBlockDivisor"
     }

-- Access hasAttribute, getAttribute:
debug Core

---------------------------------------------------------
---------------------------------------------------------
-- New types: 
--ConformalBlockVectorBundle and SymmetricDivisorM0nbar
---------------------------------------------------------
---------------------------------------------------------


ConformalBlockVectorBundle = new Type of HashTable;
ConformalBlockVectorBundle.GlobalAssignHook = globalAssignFunction
ConformalBlockVectorBundle.GlobalReleaseHook = globalReleaseFunction
expression ConformalBlockVectorBundle := V -> (
    if hasAttribute(V,ReverseDictionary) then expression toString getAttribute(V,ReverseDictionary) else toString(pairs V)
);
net ConformalBlockVectorBundle := V -> (
    if hasAttribute(V,ReverseDictionary) then return net expression V; 
    if not hasAttribute(V,ReverseDictionary) then return (
	horizontalJoin flatten (
          "{",
          -- the first line prints the parts vertically, second: horizontally    
          stack (horizontalJoin \ apply(pairs V,(k,v) -> (net k, " => ", net v))),                                        
          "}"
          )
      )
);

ConformalBlockVectorBundle#{Standard,AfterPrint} = V -> (
    g:=toString(V#"Genus");
    n:=toString(V#"NumberOfPoints");
    ostring:=concatenate(interpreterDepth:"o");
    << endl;
    << concatenate(ostring,toString lineNumber," : Conformal block vector bundle on M-",g,"-",n,"-bar");
    << endl;
);    





conformalBlockVectorBundle = method(
    TypicalValue => ConformalBlockVectorBundle
)
--Add some consistency checks
--Lift weights from QQ to ZZ
--Check that the weights are in P_l for the Lie algebra
conformalBlockVectorBundle(LieAlgebra,ZZ,List,ZZ):=(lieAlgebra,l,v,ggenus)-> (  
    v=apply(#v, i -> apply(#(v_i), j -> lift(v_i_j,ZZ)));
    Pl:=weylAlcove(l,lieAlgebra);
    for i from 0 to #v-1 do (
        if not member(v_i,Pl) then error concatenate("The weight ",toString(v_i)," is not in the Weyl alcove of this Lie algebra at this level.");	
	);
    return new ConformalBlockVectorBundle from {"LieAlgebra"=>lieAlgebra,"Level"=>l,"Weights"=>v,"Genus"=>ggenus,"NumberOfPoints"=>#v}
)



--declare a new type called SymmetricDivisorM0nbar
SymmetricDivisorM0nbar = new Type of HashTable;

-*Functions and methods available for the type SymmetricDivisorM0nbar:
--look up n
--create from list
--create from polynomial
--list coefficients
--print polynomial
--scale D
--scalar multiplication
--add D, E if n is the same
--negate
--test equality D==E

--Handy examples during debugging phase
--D=new SymmetricDivisorM0nbar from {{numberOfPoints,6},{B_2,2},{B_3,3}};
--E=new SymmetricDivisorM0nbar from {{numberOfPoints,6},{B_2,3},{B_3,5}};
--F=symmetricDivisorM0nbar(6,{2,3});
--G=symmetricDivisorM0nbar(6,2*B_2+3*B_3);
*-

expression SymmetricDivisorM0nbar := D -> (    
    if keys(D) == {"NumberOfPoints"} then return expression 0;
    CL:=coefficientList(D);
    coeff:=0;
    divisorSymbol:=expression "B";
    Sum delete(null,apply(#CL, j -> (if CL_j != 0 then (
	    coeff = expression abs(CL_j);
	    if CL_j === -1 then 
	        Minus Subscript{divisorSymbol, j+2}
	    else if CL_j < 0 then 
	        Minus {coeff * Subscript{divisorSymbol, j+2}}
	    else if CL_j === 1 then 
	        Subscript{divisorSymbol, j+2}
	    else coeff * Subscript{divisorSymbol, j+2} )
    )))
); 

-*
f = D -> (    
    if keys(D) == {"NumberOfPoints"} then return expression 0;
    CL:=coefficientList(D);
    coeff:=0;
    divisorSymbol:=expression "B";
    Sum apply(#CL, j -> (
	    coeff = expression abs(CL_j);
	    if CL_j === -1 then 
	        Minus Subscript{divisorSymbol, j+2}
	    else if CL_j < 0 then 
	        Minus {coeff * Subscript{divisorSymbol, j+2}}
	    else if CL_j === 1 then 
	        Subscript{divisorSymbol, j+2}
	    else coeff * Subscript{divisorSymbol, j+2} )
    )
); 
*-
net SymmetricDivisorM0nbar := D -> net expression D;

SymmetricDivisorM0nbar#{Standard,AfterPrint} = D -> (
    n:=toString(D#"NumberOfPoints");
    ostring:=concatenate(interpreterDepth:"o");
    << endl;
    << concatenate(ostring,toString lineNumber," : S_",n,"-symmetric divisor on M-0-",n,"-bar");
    << endl;
);    



SymmetricDivisorM0nbar==SymmetricDivisorM0nbar :=(D,E) -> ( 
    pairs(D)==pairs(E)
)


SymmetricDivisorM0nbar+SymmetricDivisorM0nbar :=(D,E) -> (
    if D#"NumberOfPoints" != E#"NumberOfPoints" then error ///D and E are not divisors on the same $\bar{M}_{0,n}$ - the numbers of marked points are different///;
    n:=D#"NumberOfPoints";
    answer:={{"NumberOfPoints",n}};
    a:=0;
    b:=0;
    for i from 2 to floor(n/2) do ( 
        a=0;
        b=0;
        if D#?i then a=D#i;
        if E#?i then b=E#i;
        answer = append(answer,{i,a+b})
    );
    return new SymmetricDivisorM0nbar from answer
)

- SymmetricDivisorM0nbar :=(D) -> (
    n:=D#"NumberOfPoints";
    answer:={{"NumberOfPoints",n}};
    a:=0;
    for i from 2 to floor(n/2) do ( 
        a=0;
        if D#?i then a=-(D#i);
        answer = append(answer,{i,a})
    );
    return new SymmetricDivisorM0nbar from answer
)

Number*SymmetricDivisorM0nbar :=(k,D) -> (
    n:=D#"NumberOfPoints";
    answer:={{"NumberOfPoints",n}};
    a:=0;
    for i from 2 to floor(n/2) do ( 
        a=0;
        if D#?i then a=k*(D#i);
        answer = append(answer,{i,a})
    );
    return new SymmetricDivisorM0nbar from answer
)

coefficientList = method(
    TypicalValue => List
)
coefficientList(SymmetricDivisorM0nbar) := (K) -> (
    n:=K#"NumberOfPoints";
    answer:={};
    for i from 2 to floor(n/2) do ( 
        if K#?i then answer=append(answer, K#i) else answer=append(answer,0)
    );
    answer
)


symmetricDivisorM0nbar = method(
    TypicalValue => SymmetricDivisorM0nbar
)
symmetricDivisorM0nbar(ZZ,List) :=(n,L)-> (
    g:=0;
    if even(n) then g=lift((n-2)/2,ZZ) else g=lift((n-3)/2,ZZ);   
    if #L != g then error "expected a list of length floor(n/2)-1";        
    ans:={{"NumberOfPoints",n}};
    for i from 2 to floor(n/2) do (
        ans = append(ans,{i,L_(i-2)})     
    );
    return new SymmetricDivisorM0nbar from ans
)

Number*IndexedVariable :=(k,x) -> (expression(k)*expression(x))

IndexedVariable+IndexedVariable :=(x,y) -> (expression(x) + expression(y))


symmetricDivisorM0nbar(ZZ,IndexedVariable) := (n,f) -> (  
    return new SymmetricDivisorM0nbar from {{"NumberOfPoints",n},{f#1,1}}
)

symmetricDivisorM0nbar(ZZ,Expression) := (n,f) -> (     
    g:=0;
    if even(n) then g=lift((n-2)/2,ZZ) else g=lift((n-3)/2,ZZ);     
    if instance(f,ZeroExpression) then return symmetricDivisorM0nbar(n,apply(g,i->0));
    ans:={{"NumberOfPoints",n}};
    k:=#f;
    mi:=0;
    coeffi:=0;
    monstri:="";
    subi:=0;
    regi:=0;
    if instance(f,Subscript) then (
        ans=append(ans,{f#1,1}));
    if instance(f,Product) then (
        mi=f;
        coeffi=mi#0;
        subi=mi#1#1;
        if not instance(subi,ZZ) then error "the subscripts must be integers";
        if subi<2 then error "the subscripts must be integers greater than 2";
        if subi>g+1 then error "the subscripts must be integers less than or equal to floor(n/2)";
        ans=append(ans,{subi,coeffi})          
    );
    if instance(f,Sum) then (
        return sum apply(k, i -> symmetricDivisorM0nbar(n,f#i))
    );
    return new SymmetricDivisorM0nbar from ans
)

scale = method(
    TypicalValue=>SymmetricDivisorM0nbar     
)
scale(SymmetricDivisorM0nbar) := (D) -> (
    L:=coefficientList(D);
    g:=gcd(L); 
    if g==0 then return D else return (1/g)*D
)


---------------------------------------------------------
---------------------------------------------------------
--General functions for working with F-curves on M0nbar
---------------------------------------------------------
---------------------------------------------------------


CdotBi = (L,i) -> (n:=sum L;
    newL:={L_0,L_1,L_2,L_3,(L_0)+(L_1),(L_0)+(L_2),(L_0)+(L_3)};
    newL = apply(7, k -> if newL_k <= floor(n/2) then newL_k else n-newL_k);
    sum apply({4,5,6}, j -> if newL_j==i then 1 else 0 )-sum apply(4, k -> if newL_k == i then 1 else 0)      
);



symmetricCurveDotDivisorM0nbar = method(
    TypicalValue => QQ
)
symmetricCurveDotDivisorM0nbar(List,SymmetricDivisorM0nbar) := (C,E) -> (
    D:=coefficientList(E);
    sum apply(#D, i ->   (D_i)*CdotBi(C,i+2))
)



basisOfSymmetricCurves = method(
     TypicalValue => List
)
basisOfSymmetricCurves(ZZ) := (n) -> ( f:=floor(n/2);  
    apply(f-1, i -> {n-(i+1)-2,i+1,1,1})
)



FdotBjIntMat = method(
     TypicalValue => Matrix
)
FdotBjIntMat(ZZ) := (n) -> (f:=floor(n/2);
    cu :=basisOfSymmetricCurves(n); 
    matrix apply(f-1, i -> apply(f-1, j -> CdotBi(cu_i,j+2)/1 ))
)

symmetricFCurves= method(
TypicalValue => List
)
symmetricFCurves(ZZ) := (n) -> ( L:={};
    p:=partitions(n);
    delete(null, apply(#p, i -> if #(p_i)==4 then toList(p_i))) 
)



killsCurves= method(
    TypicalValue => List
)
killsCurves(SymmetricDivisorM0nbar) := (E) -> ( 
    n:=E#"NumberOfPoints";
    D:=coefficientList(E);
    f:=floor(n/2);
    killedCurves:={};
    curves:=symmetricFCurves(n);
    delete(null, apply(#curves, i -> if sum(apply(f-1, j -> (D_j*CdotBi(curves_i,j+2)) )) == 0 then curves_i))
);



isSymmetricFDivisor = method(
    TypicalValue => Boolean
)
isSymmetricFDivisor(SymmetricDivisorM0nbar) := (E) -> (
    n:=E#"NumberOfPoints";
    g:=0;
    if even(n) then g=lift(n/2-1,ZZ) else g=lift((n-1)/2-1,ZZ);
    curves:=symmetricFCurves(n);
    for i from 0 to #curves-1 do (
        if symmetricCurveDotDivisorM0nbar(curves_i,E) < 0 then (
             print concatenate("This divisor has negative intersection with the F curve F_",toString(curves_i), " (and maybe others too)") << endl;
	     return false
	)
    );
     return true
)



isExtremalSymmetricFDivisor = method(
    TypicalValue => Boolean
)
isExtremalSymmetricFDivisor(SymmetricDivisorM0nbar) := (E) -> (
    bool:=isSymmetricFDivisor(E);
    if bool==false then return false;
    n:=E#"NumberOfPoints";
    g:=0;
    if even(n) then g=lift(n/2-1,ZZ) else g=lift((n-1)/2-1,ZZ);
    curves:=killsCurves(E);
    if #curves == 0 then return false;
    M := matrix apply(#curves, i -> apply(g, j-> CdotBi(curves_i,j+2) ));
    rank M >= g-1
)


---------------------------------------------------------
---------------------------------------------------------
--Some important divisors on M0nbar:  
--K, kappaDivisorM0nbar, psiDivisorM0nbar
---------------------------------------------------------
---------------------------------------------------------


canonicalDivisorM0nbar = method(
    TypicalValue => SymmetricDivisorM0nbar
)
canonicalDivisorM0nbar(ZZ) := (n) -> (f:=floor(n/2);
    L2:=apply(f-1, i -> i+2);
    symmetricDivisorM0nbar(n,apply(L2, k -> k*(n-k)/(n-1)-2))
);



AlexSwincurves = (n) -> ( 
    f:=floor(n/2);
    if odd(n) == true then return apply(f-1, i -> {f+(i+1)-1,f-(i+1),1,1});
    if even(n) == true then return apply(f-1, i -> {f-2+(i+1),f-(i+1),1,1})  
);





kappaDivisorM0nbar = method(
    TypicalValue => SymmetricDivisorM0nbar
)
kappaDivisorM0nbar(ZZ) := (n) -> (
    canonicalDivisorM0nbar(n) + symmetricDivisorM0nbar(n,apply(floor(n/2)-1, i ->  1))
);


psiDivisorM0nbar = method(
    TypicalValue => SymmetricDivisorM0nbar
)
psiDivisorM0nbar(ZZ) := (n) -> (g:=0;
    if even(n) then g=lift(n/2,ZZ)-1 else  g=lift((n-1)/2,ZZ)-1;
    L:=apply(g, k -> k+2);
    answer:=apply(L, k -> k*(n-k)/(n-1));
    symmetricDivisorM0nbar(n,answer)
);


---------------------------------------------------------
---------------------------------------------------------
--Computing conformal block bundles
---------------------------------------------------------
---------------------------------------------------------


---------------------------------------------------------
---------------------------------------------------------
--Factorization and ranks
---------------------------------------------------------
--------------------------------------------------------- 

-* Conformal block ranks may be computed recursively.  
First, propagation allows you to drop a weight if it is zero.  Next, factorization
allows one to reduce the calculation to computing conformal block ranks on M03bar.
In general, these may be computed using the Kac-Walton algorithm, which is 
implemented as fusionCoefficient in the LieTypes package.  

However, for three special cases, there are faster formulas for conformal
block ranks on M03bar.  These cases are:
sl_2, any level
sl_3, any level
sl_m, level 1
They are implemented here.
*-

sl2threept = (l,L) -> (
    a:=L_0_0;
    b:=L_1_0;
    c:=L_2_0;
    if even(a+b+c) and 0<=a and 0<=b and 0<=c and a<= l and b<=l and c<=l and abs(b-a) <=c and c<= min({a+b,2*l-a-b})  then return 1 else 0
	  )

sl3threept = (l,L) -> (if #L > 3 then error "#L>3" ;
    if #L==3 and L_0 == {0,0} and L_1 == {L_2_1,L_2_0} then return 1;
    if #L==3 and L_0 == {0,0} and L_1 != {L_2_1,L_2_0} then return 0;
    if #L==3 and L_1 == {0,0} and L_0 == {L_2_1,L_2_0} then return 1;
    if #L==3 and L_1 == {0,0} and L_0 != {L_2_1,L_2_0} then return 0;
    if #L==3 and L_2 == {0,0} and L_0 == {L_1_1,L_1_0} then return 1;
    if #L==3 and L_2 == {0,0} and L_0 != {L_1_1,L_1_0} then return 0;
    if #L==1 and L=={0,0} then return 1;
    if #L==1 and L!={0,0} then return 0;
    if #L==2 and L_0 == {L_1_1,L_1_0} then return 1;
    if #L==2 and L_0 != {L_1_1,L_1_0} then return 0;
    if #L!=3 then error "L neq 3";
    a1:=L_0_0; a2:=L_0_1; b1:=L_1_0; b2:=L_1_1; c1:=L_2_0; c2:=L_2_1;
    A:= (1/3)*(2*(a1+b1+c1)+(a2+b2+c2));
    B:= (1/3)*((a1+b1+c1)+2*(a2+b2+c2));
    k0max:=min {A,B};
    k0min:= max { a1+a2,  b1+b2,  c1+c2,  A-min(a1,b1,c1), B-min(a2,b2,c2)   };
    delta:=0;
    if k0max >= k0min and gcd(A,1) == 1 and gcd(B,1) == 1 then delta = 1;
    M:= (k0max-k0min+1)*delta;
    if l < k0min or M==0 then return 0 else return (min {k0max,l} - k0min+1)
);


cc = (v) -> (
    sum apply(#v, j -> (j+1)*(v_j))    
);

slml1threept = (type,m,l,w) -> (
    if ((sum apply(#w, i -> cc(w_i)))%(m+1)) == 0 then 1 else 0 
);

slml1rank = memoize((type, m, l, w) -> (
    if ((sum apply(#w, i -> cc(w_i)))%(m+1)) == 0 then 1 else 0 
));    
     


conformalBlockRankM03bar = memoize((type, m, l, w)  ->  (
    --Zero points
    if #w == 0 then return 1;
    --One point
    if #w == 1 and w_0 != apply(#(w_0), i -> 0) then return 0;
    if #w == 1 and w_0 == apply(#(w_0), i -> 0) then return 1;
    --Two point
    if #w == 2 and w_1 != starInvolution(type, m, w_0) then return 0;
    if #w == 2 and w_1 == starInvolution(type, m, w_0) then return 1;
    --Three point:
    --We use the best option available according to the following preferences:
    ----if g=sl_2, then we compute using the function above
    ----if g=sl_m and l=1, then we compute using the function above
    ----if g=sl_3, then we compute using the function above
    ----otherwise compute it using fusionCoefficient from LieTypes
    if type=="A" and m==1 then (
	return lift(sl2threept(l,w),ZZ));
    if type=="A" and l==1 then (
        return lift(slml1threept(type,m,l,w),ZZ));
    if type=="A" and m==2 then (
        return lift(sl3threept(l,w),ZZ));
    g:=simpleLieAlgebra(type,m);
    U:=irreducibleLieAlgebraModule(w_0,g);
    V:=irreducibleLieAlgebraModule(w_1,g);
    W:=irreducibleLieAlgebraModule(starInvolution(type,m,w_2),g);
    return fusionCoefficient(U,V,W,l)
));


propagation = (m,L) ->  (
    z:=apply(m, i -> 0);
    delete(z, L)
);

 
conformalBlockRank=method(
    TypicalValue=> ZZ
)
conformalBlockRank(ConformalBlockVectorBundle) := memoize( (V) -> (
    if V#"Genus" != 0 then error ///Only implemented for conformal blocks on $\bar{M}_{0,n}$///;   
    g:=V#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    l:=V#"Level";
    L:=V#"Weights";
    if type=="A" and l==1 then return slml1rank(type,m,l,L);
    L = propagation(m,L);
    n:=#L;
    if n <= 3 then return conformalBlockRankM03bar(type,m,l,L);
    --for n >=4 use factorization:
    A:={L_0,L_1};
    B:=drop(L,{0,1});
    rA:=0;
    pl:=weylAlcove(type,m,l);
    r:= sum apply(#pl, i -> (rA=conformalBlockRankM03bar(type,m,l, append(A,pl_i)); 
        if rA == 0 then 0 else rA*conformalBlockRank(conformalBlockVectorBundle(g,l, append(B,starInvolution(type,m,pl_i)),0)) ));
    return lift(r,ZZ)
));
  
 
 
 
    
---------------------------------------------------------
---------------------------------------------------------
--Fakhruddin's formulas for first Chern classes
---------------------------------------------------------
---------------------------------------------------------

symmetrizedConformalBlockDivisor = method(     
    TypicalValue => SymmetricDivisorM0nbar
    )	  
symmetrizedConformalBlockDivisor(ConformalBlockVectorBundle) := (V) -> (  
    g:=V#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    l:=V#"Level";
    wt:=V#"Weights";
    n:=#wt;
    if #tally(wt)==1 then return n!*symmetricConformalBlockDivisor(g,l,n,wt_0);
    Bks:={};
    f:=floor(n/2);
    bi:=0;
    S:={};
    s:={};
    rlambda:=0;
    rlambdaAmu:=0;
    rlambdaAcstarInvolution:=0;
    Acomp:={};
    VlambdaAmu:={};
    VlambdaAcstarInvolution:={};
    pl:=weylAlcove(type,m,l);
    for i from 2 to f do (
        --first term
        bi=0;
        rlambda=conformalBlockRank(V);
        bi = bi+ rlambda*( binomial(n-3,i-1) + binomial(n-3,n-i-1) )*sum(apply(#wt, k -> casimirScalar(type,m,wt_k)));
        --second term
        S = apply(n, z->z);
        s=subsets(S,i);
        for p from 0 to #s-1 do (
            for k from 0 to #pl-1 do (A:=s_p;
                wtA:={};
                for q from 0 to #A-1 do wtA = append(wtA, wt_A_q);
		VlambdaAmu=conformalBlockVectorBundle(g,l,append(wtA,pl_k),0);
                rlambdaAmu=conformalBlockRank(VlambdaAmu);
                if rlambdaAmu !=0 then  (  Acomp := toList(set(S)-set(s_p));
                    wtAcomp:={};
                    for q from 0 to #Acomp-1 do wtAcomp = append(wtAcomp, wt_Acomp_q);
		    VlambdaAcstarInvolution=conformalBlockVectorBundle(g,l, append(wtAcomp, starInvolution(type,m,pl_k)),0);
                    rlambdaAcstarInvolution=conformalBlockRank(VlambdaAcstarInvolution);
                    bi = bi - casimirScalar(type,m,pl_k)*rlambdaAmu*rlambdaAcstarInvolution
	        ) 
            )
        );
    bi = ( ( (i!)*(n-i)!)/(2*(l+dualCoxeterNumber(type,m))))*bi;
    Bks = append(Bks, bi)
    );
    symmetricDivisorM0nbar(n,Bks)
);

--faster function if there is symmetry
symmetricConformalBlockDivisor = (g,l,n,lambda) -> ( 
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank"; 
    Bks:={};
    f:=floor(n/2);
    bi:=0;
    S:={};
    s:={};
    rlambda:=0;
    rlambdaAmu:=0;
    rlambdaAcstarInvolution:=0;
    VlambdaAmu:={};
    VlambdaAcstarInvolution:={};
    pl:=weylAlcove(type,m,l);
    V:={};
    lambdan := apply(n, j -> lambda);
    for i from 2 to f do (
        --first term
        bi=0;
	V=conformalBlockVectorBundle(g,l,lambdan,0);
        rlambda=conformalBlockRank(V);
        bi = bi+ rlambda*( i*(n-i)/(n-1) )*casimirScalar(type,m,lambda) ;
        --second term
        lambdai := apply(i, j -> lambda);
        lambdac:=apply(n-i, j -> lambda);
        for k from 0 to #pl-1 do ( 
	    VlambdaAmu=conformalBlockVectorBundle(g,l,append(lambdai,pl_k),0);
            rlambdaAmu=conformalBlockRank(VlambdaAmu);
            if rlambdaAmu !=0 then  (  
		VlambdaAcstarInvolution=conformalBlockVectorBundle(g,l, append(lambdac, starInvolution(type,m,pl_k)),0);
                rlambdaAcstarInvolution=conformalBlockRank(VlambdaAcstarInvolution); 
                bi = bi - casimirScalar(type,m,pl_k)*rlambdaAmu*rlambdaAcstarInvolution
	    )  
        );
        bi = ( (1 )/(2*(l+dualCoxeterNumber(type,m))))*bi;
        Bks = append(Bks, bi)
    );
    symmetricDivisorM0nbar(n,Bks)
);


conformalBlockDegreeM04bar=method(
    TypicalValue => ZZ
)
conformalBlockDegreeM04bar(ConformalBlockVectorBundle):=memoize((V) -> ( 
    if V#"Genus" != 0 then error ///Only implemented for conformal blocks on $\bar{M}_{0,4}$///;
    g:=V#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    l:=V#"Level";
    w:=V#"Weights";
    n:=#w;
    if n != 4 then error ///This function is for conformal blocks on $\bar{M}_{0,4}$///;
    a:=w_0;
    b:=w_1;
    c:=w_2;
    d:=w_3;
    pl:=weylAlcove(type,m,l);
    answer:=sum apply(4, i -> casimirScalar(type,m,w_i));    
    answer = conformalBlockRank(V)*answer;
    for i from 0 to #pl-1 do ( 
	e:=pl_i;
	f:=starInvolution(type,m,pl_i);
        answer = answer - casimirScalar(type,m,pl_i)*(conformalBlockRankM03bar(type,m,l,{a,b,e})*conformalBlockRankM03bar(type,m,l,{c,d,f})+conformalBlockRankM03bar(type,m,l,{a,c,e})*conformalBlockRankM03bar(type,m,l,{b,d,f}) + conformalBlockRankM03bar(type,m,l,{a,d,e})*conformalBlockRankM03bar(type,m,l,{b,c,f}))
    );    
    return lift(answer/(2*(l+dualCoxeterNumber(type,m))),ZZ);
));

wtindex = (w) -> (
    if w==apply(#w, j-> 0) then return 0;
    i:=0;
    while (w_i)==0 do i =i+1;
    i+1     
)

FCurveDotConformalBlockDivisorslml1 = memoize((curve,m,l,w) -> ( 
    wts:=apply(#w, i -> wtindex(w_i));	  
    if #(flatten curve) != #wts then error "The number of marked points in the F curve does not match the number of weights";
    nu1:=(sum apply(curve_0, h -> wts_(h-1)))%m;
    nu2:=(sum apply(curve_1, h -> wts_(h-1) ))%m;
    nu3:=(sum apply(curve_2, h -> wts_(h-1) ))%m;
    nu4:=(sum apply(curve_3, h -> wts_(h-1) ))%m;
    numax:=max {nu1,nu2,nu3,nu4};
    numin:=min {nu1,nu2,nu3,nu4};
    nusum:= sum {nu1,nu2,nu3,nu4};
    if nusum != 2*m then return lift(0,ZZ);
    if nusum == 2*m and numax + numin <= m then return lift(numin,ZZ); 
    if nusum == 2*m and numax + numin >= m then return lift(m - numax,ZZ)
)); 

FCurveDotConformalBlockDivisor = method(
     TypicalValue=>ZZ)
FCurveDotConformalBlockDivisor(List,ConformalBlockVectorBundle) := (C,V) -> (
    if V#"Genus" != 0 then error ///Only implemented for conformal blocks on $\bar{M}_{0,n}$///;
    g:=V#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    l:=V#"Level";
    w:=V#"Weights";
    if #(flatten C) != #w then error "The number of marked points in the F curve does not match the number of weights";
    if type=="A" and l==1 and m>1 then return FCurveDotConformalBlockDivisorslml1(C,m+1,l,w);
    answer:=0;
    pl:=weylAlcove(type,m,l);
    d:=0;
    r0:=0;
    r1:=0;
    r2:=0;
    r3:=0;
    mu0:=0;
    mu1:=0;
    mu2:=0;
    mu3:=0;
    lambda0:=0;
    lambda1:=0;
    lambda2:=0;
    lambda3:=0;
    W:={};
    for m0 from 0 to #pl-1 do (
        for m1 from 0 to #pl-1 do (
            for m2 from 0 to #pl-1 do (
                for m3 from 0 to #pl-1 do (
	            mu0 = pl_m0;
	            mu1 = pl_m1;
	            mu2 = pl_m2;
	            mu3 = pl_m3; 	  	  
                    W = conformalBlockVectorBundle(g,l,{mu0,mu1,mu2,mu3},0);
		    d = conformalBlockDegreeM04bar(W);	  
		    if d == 0 then continue;
		    lambda0 = append(apply(#(C_0), i -> w_(C_0_i-1)),starInvolution(type,m,mu0));        
	            r0=conformalBlockRank(conformalBlockVectorBundle(g,l,lambda0,0));
		    if r0==0 then continue;
		    lambda1 = append(apply(#(C_1), i -> w_(C_1_i-1)),starInvolution(type,m,mu1)); 
	            r1=conformalBlockRank(conformalBlockVectorBundle(g,l,lambda1,0));
		    if r1==0 then continue;
                    lambda2 = append(apply(#(C_2), i -> w_(C_2_i-1)),starInvolution(type,m,mu2)); 
	            r2=conformalBlockRank(conformalBlockVectorBundle(g,l,lambda2,0));
		    if r2==0 then continue;
                    lambda3 = append(apply(#(C_3), i -> w_(C_3_i-1)),starInvolution(type,m,mu3)); 
	            r3=conformalBlockRank(conformalBlockVectorBundle(g,l,lambda3,0));
                    if r3!=0 then answer = answer + d*r0*r1*r2*r3
		)
	    )
	)
    );
    return answer
);





---------------------------------------------------------
---------------------------------------------------------
beginDocumentation()
---------------------------------------------------------
---------------------------------------------------------


doc ///
    Key
        ConformalBlocks
    Headline
        for vector bundles of conformal blocks on the moduli space of curves
    Description
        Text
	    Vector bundles of conformal blocks are vector bundles on the moduli stack of Deligne-Mumford stable n-pointed genus g curves $\bar{M}_{g,n}$ that arise in conformal field theory.  Each triple $(\mathbf{g},l,(\lambda_1,...,\lambda_n))$ with $\mathbf{g}$ a simple Lie algebra, $l$ a nonnegative integer called the level, and $(\lambda_1,...,\lambda_n)$ an n-tuple of dominant integral weights of $\mathbf{g}$ specifies a conformal block bundle $V=V(\mathbf{g},l,(\lambda_1,...,\lambda_n))$.  This package computes ranks and first Chern classes of conformal block bundles on $\bar{M}_{0,n}$ using formulas from Fakhruddin's paper @TO2{"Bibliography","[Fakh]"}@.
	    
        Text	      
	    Most of the functions are in this package are for $S_n$ symmetric divisors and/or symmetrizations of divisors, but a few functions are included for non-symmetric divisors as well.
	    
	Text
	    Some of the documentation nodes refer to books, papers, and preprints.  Here is a link to the @TO "Bibliography"@. 
	    
	Text
	    Between versions 1.x and 2.0, the package was rewritten in a more object-oriented way, and the basic Lie algebra functions were moved into a separate package called @TO "LieTypes::LieTypes"@.  
///


doc ///
    Key 
        "Bibliography"
    Headline
        Bibliography for the ConformalBlocks package
    Description
        Text	
             [AS] Alexeev and Swinarski.  Nef divisors on $\bar{M}_{0,n}$ from GIT. p. 1–21 in {\it Geometry and arithmetic},  EMS Ser. Congr. Rep., Eur. Math. Soc., Zurich, 2012. 
        
	     [AGSS] Arap, Gibney, Stankewicz, and Swinarski.  $sl_n$ level 1 conformal blocks on $\bar{M}_{0,n}$.  Int. Math. Res. Not. {\bf 7} (2012), 1634-1680.
	
	     [Beauville] Beauville.  Conformal blocks, fusion rules, and the Verlinde formula, (Ramat Gan, 1993),  Israel Math. Conf. Proc., vol. 9, Bar-Ilan Univ., Ramat Gan, 1996, pp. 75-96.
	
             [Bourbaki] Bourbaki.  Lie Groups and Lie Algebras.  Chapters 4-6.
	
	     [DMS] Di Francesco, Mathieu, and Senechal.  {\it Conformal Field Theory.}  Graduate Texts in Contemporary Physics, Springer.
	     
	     [Fakh] Fakhruddin.  Chern classes of conformal blocks.  {\it Compact moduli spaces and vector bundles}, 145-176, Contemp. Math., {\bf 564} Amer. Math. Soc., Providence, RI, 2012.
	     
	     [Humphreys] Humphreys.  {\it Introduction to Lie Algebras and Representation Theory.}  Graduate Texts in Mathematics, Springer.
	     
	     [KM] Keel and McKernan.  Contractible extremal rays. p. 113-128 in {\it Handbook of Moduli, Vol. II.}  Higher Education \& International Press, Beijing-Boston, 2012.	     
	     
	    
	    
     

///

doc ///
    Key 
        "standard basis"
    Headline
        The standard basis of symmetric divisors for the moduli space of stable n-pointed genus zero curves
    Description
        Text	
            The standard basis of the $Q$-vector space of $S_n$ symmetric divisors on $\bar{M}_{0,n}$ is given by the boundary divisors $B_i$, as we now explain.  Let $\Delta_I$ be the closure of the locus of curves with two irreducible components meeting at one node such that the marked points with labels in $I$ lie on the first component, and the marked points with labels in $I^c$ lie on the second component.  Then $B_i= \sum_{\#I=i} \Delta_I$, and the divisors $B_2, ..., B_{[n/2]}$ form a basis of the space of symmetric divisors.  See @TO2{"Bibliography","[KM]"}@. 
///

doc ///
    Key
        "F curve"
    Headline
        F curves in the moduli space of stable n-pointed genus zero curves
    Description
        Text
            Let $P={P_0,P_1,P_2,P_3}$ be a partition of $\{1,...,n\}$ into four nonempty subsets.  Fix four (arithmetic) genus zero at worst nodal curves $C_j$ for  $j=0,1,2,3$, and $\#(P_j)$ marked points on each curve.  We call the curves $C_j$ the tails.  Mark one additional point $x_j$ on each tail. Next, consider $\mathbb{P}^1$ with four marked points, $y_0,...,y_3$; we call this the spine.  Glue the four tails to the spine by identifying $x_j$ and $y_j$.  Then, as the cross ratio of $y_0,...,y_3$ varies, we sweep out a curve $F_{P}$ in $\bar{M}_{0,n}$.
	    
	Text
            The homology class of $F_{P}$ only depends on the partition $P$, and not on the choice of the tails $C_j$ or the choices of marked points.  The classes of the F-curves span $H_2(\bar{M}_{0,n},Q)$.
	    
	Text
            If we only consider F-curves up to $S_n$ symmetry, then it is enough to keep track of the four integers $\#(P_0)$, $\#(P_1)$, $\#(P_2)$, $\#(P_3)$.
///	    

doc ///
    Key
        ConformalBlockVectorBundle
    Headline
        the class of conformal block vector bundles on the moduli space of n-pointed genus g curves
    Description
        Text
	    This type implements conformal block vector bundles on the moduli space of n-pointed genus g curves.
	    
	Text
	    Conformal block vector bundles are implemented as hash tables.  The key "LieAlgebra" records the Lie algebra used to define the conformal block.  The key "Level" records the level.  The key "Weights" records the weights.  The key "Genus" records the $g$ in $\bar{M}_{g,n}$.  The key "NumberOfPoints" records the number of marked points, i.e., the $n$ in $\bar{M}_{g,n}$.
	      
	Text
            An object of the "ConformalBlockVectorBundle" class can be created using the function @TO conformalBlockVectorBundle@.
///	    	
	        
doc ///
    Key
        SymmetricDivisorM0nbar
    Headline
        the class of S_n symmetric divisors on the moduli space of stable n-pointed genus 0 curves
    Description	 
        Text
	    This type implements $S_n$ symmetric divisors on the moduli space of stable n-pointed genus 0 curves $\bar{M}_{0,n}$.
	    
	Text
            The @TO "standard basis"@ of the $Q$-vector space of $S_n$ symmetric divisors on $\bar{M}_{0,n}$ is given by the boundary divisors B_i, as we now explain.   Let $\Delta_I$ be the closure of the locus of curves with two irreducible components meeting at one node such that the marked points with labels in $I$ lie on the first component, and the marked points with labels in $I^c$ lie on the second component.  Then $B_i= \sum_{\#I=i} \Delta_I$, and the divisors $B_2, ..., B_{[n/2]}$ form a basis of the space of symmetric divisors.  See @TO2{"Bibliography","[KM]"}@. 
	    
	Text
	    Symmetric divisors are implemented as hash tables.  The key "NumberOfPoints" records the number of marked points, i.e., the $n$ in $\bar{M}_{0,n}$.  The keys must be integers between 2 and $[n/2]$; the value of the key i is the coefficient of $B_i$ when a divisor $D$ is written in the standard basis.
	    
	Text
            An object of the "SymmetricDivisorM0nbar" class can be created using the function @TO symmetricDivisorM0nbar@ in either one of two ways: by entering $n$ and a linear polynomial in the $B_i$'s, or entering $n$ and a list of coefficients.
	    
	Text
            Methods are included for adding two symmetric divisors, negating a divisor, multiplying a divisor by a scalar, and testing equality of two divisors.  The function @TO coefficientList@ returns the list of the coefficients.
///
 
doc ///
    Key 
        symmetricDivisorM0nbar
	(symmetricDivisorM0nbar,ZZ,List)
	(symmetricDivisorM0nbar,ZZ,Expression)
	(symmetricDivisorM0nbar,ZZ,IndexedVariable)
    Headline
        create a symmetric divisor on the moduli space of stable pointed genus 0 curves
    Usage
        symmetricDivisorM0nbar(n,L), symmetricDivisorM0nbar(n,f)
    Inputs 
        n:ZZ
	L:List
    Outputs 
        D:SymmetricDivisorM0nbar
    Description
        Text
            A symmetric divisor on $\bar{M}_{0,n}$ may be created in either one of two ways.  The user may either enter the number of marked points $n$ and a linear polynomial in the @TO "standard basis"@ classes $B_i$, or  enter $n$ and a list of the coefficients of $D$ in the standard basis.  Both usages are demonstrated in the example below.
	    
        Example
	    D=symmetricDivisorM0nbar(6,{2,3})
	    E=symmetricDivisorM0nbar(6,2*B_2+3*B_3)
	    D==E
///

TEST ///
    assert(symmetricDivisorM0nbar(6,2*B_2+3*B_3) === new SymmetricDivisorM0nbar from {2 => 2, 3 => 3, "NumberOfPoints" => 6})
///

doc ///	   
    Key
	(symbol +,SymmetricDivisorM0nbar,SymmetricDivisorM0nbar)
    Headline 
        add two $S_n$ symmetric divisors
    Usage 
        D+E
    Inputs 
        D:SymmetricDivisorM0nbar
	E:SymmetricDivisorM0nbar
    Outputs
        F:SymmetricDivisorM0nbar
    Description
        Text
            Let $Pic(\bar{M}_{0,n})_Q^{S_n}$ denote the vector space of $S_n$-invariant divisors with rational coefficients.  Here, given two $S_n$ symmetric $Q$-divisors $D$ and $E$ on $\bar{M}_{0,n}$, the function returns $D+E$.

	Example 
	    D=symmetricDivisorM0nbar(6,{1/2,1/3})
	    E=symmetricDivisorM0nbar(6,2*B_2+3*B_3)
	    D+E
///

TEST ///
    D=symmetricDivisorM0nbar(6,{1,0});
    E=symmetricDivisorM0nbar(6,{0,1});
    F=symmetricDivisorM0nbar(6,{1,1});
    assert(D+E == F)
///

doc /// 
    Key
        (symbol -, SymmetricDivisorM0nbar)
    Headline
        negate a symmetric divisor
    Usage
        -D
    Inputs 
        D:SymmetricDivisorM0nbar
    Outputs
        E:SymmetricDivisorM0nbar	
    Description
        Text
            Let $Pic(\bar{M}_{0,n})_Q^{S_n}$ denote the vector space of $S_n$-invariant divisors with rational coefficients.  Here, given an $S_n$ symmetric $Q$-divisor $D$ on $\bar{M}_{0,n}$, the function returns $-D$.

	Example
	    D=symmetricDivisorM0nbar(6,{2,3})
	    E=-D
///

TEST ///
    D=symmetricDivisorM0nbar(6,{1,0});
    E=symmetricDivisorM0nbar(6,{-1,0});
    assert(-D == E)
///
		
doc ///
    Key
        (symbol *, Number, SymmetricDivisorM0nbar)
    Headline
        multiply a symmetric divisor by a number
    Usage 
        c*D		
    Inputs 
        c:Number
	D:SymmetricDivisorM0nbar	     
    Outputs 
        E:SymmetricDivisorM0nbar
    Description
        Text
	    Let $Pic(\bar{M}_{0,n})_R^{S_n}$ denote the vector space of $S_n$-invariant divisors with coefficients in a ring $R$.  Here, given an $S_n$ symmetric $R$-divisor $D$ on $\bar{M}_{0,n}$ and a number $c$, the function returns $cD$.

	Example 
	    D=symmetricDivisorM0nbar(6,{2,3})
	    6*D
///

TEST ///
    D=symmetricDivisorM0nbar(6,{2,3});
    assert(coefficientList(6*D) === {12,18})
///

doc ///
    Key
        (symbol ==, SymmetricDivisorM0nbar, SymmetricDivisorM0nbar)
    Headline
        test equality of two symmetric divisor classes on $\bar{M}_{0,n}$
    Usage 
        D==E		
    Inputs 
        D:SymmetricDivisorM0nbar
	E:SymmetricDivisorM0nbar	     
    Outputs 
        b:Boolean
    Description
        Text
	    Two objects of type SymmetricDivisorM0nbar are equal if their underlying hash tables have the same pairs.
	    
	Example 
	    D=symmetricDivisorM0nbar(6,{2,1})
	    E=scale symmetricDivisorM0nbar(6,288*B_2+144*B_3)
	    D==E
///

TEST ///
    assert(symmetricDivisorM0nbar(6,{2,1}) == scale symmetricDivisorM0nbar(6,288*B_2+144*B_3))
///


doc ///
    Key
        coefficientList
        (coefficientList,SymmetricDivisorM0nbar)
    Headline
        the coefficients of a symmetric divisor D in the standard basis	
    Usage 
         coefficientList(D)
    Inputs 
        D:SymmetricDivisorM0nbar
    Outputs 
        L:List
    SeeAlso 
        SymmetricDivisorM0nbar
    Description
        Text
	    This function returns a list of the coefficients of a symmetric divisor on $\bar{M}_{0,n}$ in the @TO "standard basis"@.

	Example
       	    D=symmetricDivisorM0nbar(6,2*B_2+3*B_3)
      	    coefficientList(D)
///

TEST ///
    D=symmetricDivisorM0nbar(6,2*B_2+3*B_3);
    assert(coefficientList(D) === {2,3})
///

doc ///
    Key
        scale
	(scale,SymmetricDivisorM0nbar)
    Headline
        reduces a list or divisor by the gcd of its coefficients
    Usage 
        scale(D)
    Inputs 
        D:SymmetricDivisorM0nbar 
    Outputs 
        E:SymmetricDivisorM0nbar
    Description
        Text
	    Let $D$ be an $S_n$ symmetric $Q$-divisor on $\bar{M}_{0,n}$.  This function reduces a symmetric divisor $D$ by the gcd of its coefficients in the @TO "standard basis"@.  This gives a canonical representative of each nonzero ray in $Pic(\bar{M}_{0,n})_Q^{S_n}$.   

	Example
	    D=symmetricDivisorM0nbar(6,288*B_2+144*B_3)
      	    scale(D)
///

TEST ///
    D=symmetricDivisorM0nbar(6,288*B_2+144*B_3)
    assert(coefficientList(scale(D)) === {2/1,1/1})
///

doc ///
    Key
        symmetricCurveDotDivisorM0nbar
	(symmetricCurveDotDivisorM0nbar,List,SymmetricDivisorM0nbar)
    Headline 
        the intersection number of a symmetric F-curve C with the symmetric divisor D
    Usage 
        symmetricCurveDotDivisorM0nbar({3,1,1,1},D)
    Inputs
        C:List 
	D:SymmetricDivisorM0nbar
    Outputs 
        k:QQ
    Description
        Text
	    This function implements the basic formula of @TO2{"Bibliography","[KM]"}@ Corollary 4.4 for intersecting an $S_n$-symmetric @TO "F curve"@ with an $S_n$ symmetric divisor on $\bar{M}_{0,n}$.

	Example
	    D=symmetricDivisorM0nbar(6,2*B_2+B_3)
	    symmetricCurveDotDivisorM0nbar({3,1,1,1},D)
	    E=symmetricDivisorM0nbar(6,B_2+3*B_3)
	    symmetricCurveDotDivisorM0nbar({3,1,1,1},E)
///

TEST ///
    assert(symmetricCurveDotDivisorM0nbar({3,1,1,1},symmetricDivisorM0nbar(6,2*B_2+B_3)) === 5)
    assert(symmetricCurveDotDivisorM0nbar({3,1,1,1},symmetricDivisorM0nbar(6,B_2+3*B_3)) === 0)
///

doc ///
    Key
        basisOfSymmetricCurves
	(basisOfSymmetricCurves,ZZ)
    Headline
        produces a basis of symmetric curves
    Usage 
        basisOfSymmetricCurves(8)
    Inputs 
        n:ZZ
    Outputs
        B:List
    Description
        Text
	    This function returns the list of @TO2{"F curve","F curves"}@ $\{F_{1,1,i,n-i-2}: 1 \leq i \leq [n/2]\}$.  This set of curves is a basis for $H_2(\bar{M}_{0,n})_{Q}^{S_n}$; see e.g. @TO2{"Bibliography","[AGSS]"}@.  The symmetric F-curve $F_{1,1,i,n-i-2}$ is represented by the list of integers \{1,1,i,n-i-2\}.

        Example
	    basisOfSymmetricCurves(8)
///

TEST ///
    assert(basisOfSymmetricCurves(8) === {{5, 1, 1, 1}, {4, 2, 1, 1}, {3, 3, 1, 1}})
///
	
doc ///
    Key
        symmetricFCurves
	(symmetricFCurves,ZZ)
    Headline
        a list of all symmetric F-curves given n
    Usage
        symmetricFCurves(8)
    Inputs 
        n:ZZ
    Outputs 
        B:List
    Description
        Text
	    This is the list of @TO2{"F curve","F curves"}@ up to $S_n$ symmetry, i.e., this function generates partitions of the integer $n$ into 4 positive integers, not partitions of the set {1,...,n} into four nonempty subsets. 

	Example
	    symmetricFCurves(8)
///

TEST ///
    assert(symmetricFCurves(8) === {{5, 1, 1, 1}, {4, 2, 1, 1}, {3, 3, 1, 1}, {3, 2, 2, 1}, {2, 2, 2, 2}})
///		
	
doc ///	
    Key
        FdotBjIntMat
	(FdotBjIntMat,ZZ)
    Headline 
        matrix of intersection numbers between F-curves and divisors on $\bar{M}_{0,n}$
    Usage 
        FdotBjIntMat(n)
    Inputs
	n:ZZ
    Outputs 
        M:Matrix
    Description
        Text
	    This function produces the matrix of intersection numbers between the @TO "standard basis"@ of $S_n$ symmetric divisors and the most popular basis of $S_n$ symmetric @TO2{"F curve","F curves"}@.  Specifically, the i,j-th entry of the matrix is $F_{n-i-2,i,1,1} . B_j$. This matrix can be used for instance to write a divisor in the standard basis if its intersection numbers with the F curves are known. See @TO2{"Bibliography","[AGSS]"}@ Section 4 for explicit formulas.
	    
	Text
	    These intersection numbers are integers, but we create the matrix over the rational numbers so that Macaulay2 will invert it correctly if we want to do so later.
	    
	Text
	    In the example below, we use this function to find the divisor class of an $S_{12}$ symmetric divisor $D$ on $\bar{M}_{0,12}$ such that $D . F_{1,1,i,12-i-2} = 1$ if $i=0$, and 0 otherwise.  Then we check that $D$ has the correct intersection numbers.

	Example
	    M=FdotBjIntMat(12)
	    N=M^-1
	    v=N*(matrix{{1},{0},{0},{0},{0}})
	    D=symmetricDivisorM0nbar(12,flatten entries v)
	    symmetricCurveDotDivisorM0nbar({1,1,1,9},D)
	    apply(5, i-> symmetricCurveDotDivisorM0nbar({1,1,i+1,12-i-3},D))
///

TEST ///
    assert(FdotBjIntMat(12) === matrix {{3/1, -1, 0, 0, 0}, {0, 2, -1, 0, 0}, {1, -1, 2, -1, 0}, {1, 0, -1, 2, -1}, {1, 0, 0, -2, 2}})
///	

doc ///	
    Key
        killsCurves
	(killsCurves,SymmetricDivisorM0nbar)
    Headline
        given an S_n symmetric divisor D, produces a list of symmetric F-curves C such that C dot D = 0
    Usage 
        killsCurves(D)
    Inputs 
        D:SymmetricDivisorM0nbar
    Outputs
        L:List
    Description
        Text
	    Given a symmetric divisor D on $\bar{M}_{0,n}$, this function returns the list of symmetric @TO2{"F curve","F curves"}@ $C$ such that $D  .  C=0$.
	    
	Text
            Here is an example from the paper @TO2{"Bibliography","[AGSS]"}@: When n is even, the divisor $D^n_{1,n/2}$ is  zero on even F-curves and 1 on odd F-curves.  (Here the parity of $F_{a,b,c,d}$ is defined to be the parity of the product $abcd$.)  In the calculations below, we check this claim for $n=8$.    

	Example
	    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
	    killsCurves(D)
///

TEST ///
    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
    assert(killsCurves(D) === {{4, 2, 1, 1}, {3, 2, 2, 1}, {2, 2, 2, 2}})
///

doc ///
    Key
        isSymmetricFDivisor
	(isSymmetricFDivisor,SymmetricDivisorM0nbar)
    Headline 
        checks whether a symmetric divisor intersects all the F-curves nonnegatively
    Usage 
        isSymmetricFDivisor(D)
    Inputs 
        D:SymmetricDivisorM0nbar 
    Outputs 
        b:Boolean
    Description
        Text
            We say a symmetric divisor on $\bar{M}_{0,n}$ is a symmetric F-divisor if $D  .  F_{I_1,I_2,I_3,I_4} \geq 0$ for every @TO "F curve"@.
	    
	Text
            In the example below, we see that for $n=8$, the divisor $3B_2+2B_3+4B_4$ is a symmetric F-divisor, while the divisor $B_2$ is not.

	Example
	    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
	    isSymmetricFDivisor(D)
	    D=symmetricDivisorM0nbar(8,B_2)
	    isSymmetricFDivisor(D)
///

TEST ///
    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
    assert(isSymmetricFDivisor(D) === true)
    D=symmetricDivisorM0nbar(8,B_2)
    assert(isSymmetricFDivisor(D) === false)
///	
	
doc ///
    Key
        isExtremalSymmetricFDivisor
	(isExtremalSymmetricFDivisor,SymmetricDivisorM0nbar)
    Headline 
        tests whether an S_n symmetric divisor spans an extremal ray of the cone of symmetric F-divisors
    Usage 
        isExtremalSymmetricFDivisor(D)
    Inputs 
        D:SymmetricDivisorM0nbar
    Outputs 
        b:Boolean 
    Description
        Text
            We say a symmetric divisor on $\bar{M}_{0,n}$ is a symmetric F-divisor if $D  .  F \geq 0$ for every @TO "F curve"@.
	    
	Text   
            Let $SF_{0,n}$ denote the cone of all $S_n$ symmetric divisors on $\bar{M}_{0,n}$ that intersect all the F-curves nonnegatively.  This cone contains the cone of $S_n$ symmetric nef divisors. (Fulton's F Conjecture predicts that the two cones are equal).  See @TO2{"Bibliography","[AGSS]"}@ Section 2 for more details.
	    
        Text	  
            This function first checks to see if $D$ is an F-divisor.  If not, the function returns false.  If so, the function goes on to check  whether $D$ is an extremal ray of the cone $SF_{0,n}$. It does so by finding all the F-curves which $D$ intersects in degree zero (i.e., finding how many facets of the cone $D$ lies on) and then checking to see whether this set contains sufficiently many independent hyperplanes to determine an extremal ray.
	    
	Text
            In the example below, we check that the divisor $3B_2+2B_3+4B_4$ is extremal in the cone $SF_{0,8}$ for $n=8$.  We also check that the divisor kappa (see @TO kappaDivisorM0nbar@), which is known to be very ample, is not an extremal ray of $SF_{0,8}$.

	Example 
	    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
	    isExtremalSymmetricFDivisor(D)
	    D=kappaDivisorM0nbar(8)
	    isExtremalSymmetricFDivisor(D)
///

TEST ///
    D=symmetricDivisorM0nbar(8,3*B_2+2*B_3+4*B_4)
    assert(isExtremalSymmetricFDivisor(D) === true)
    D=kappaDivisorM0nbar(8)
    isExtremalSymmetricFDivisor(D)
///		

doc ///	
    Key 
        canonicalDivisorM0nbar
	(canonicalDivisorM0nbar,ZZ)
    Headline 
        returns the class of the canonical divisor on the moduli space of stable n-pointed genus 0 curves
    Usage
        canonicalDivisorM0nbar(14)
    Inputs 
        n:ZZ 
    Outputs 
        K:SymmetricDivisorM0nbar
    Description
        Text
            This function returns the class of the canonical divisor $K$ on the moduli space $\bar{M}_{0,n}$. See e.g. @TO2{"Bibliography","[KM]"}@ for a formula for $K$ in the @TO "standard basis"@. 

	Example
	    canonicalDivisorM0nbar(14)
///

TEST ///
    assert(coefficientList(canonicalDivisorM0nbar(14)) === {-2/13, 7/13, 14/13, 19/13, 22/13, 23/13})
///	



doc ///
    Key
        kappaDivisorM0nbar
	(kappaDivisorM0nbar,ZZ)	
    Headline 
        the class of the divisor kappa
    Usage 
        kappaDivisorM0nbar(14)
    Inputs 
        n:ZZ
    Outputs 
        D:SymmetricDivisorM0nbar
    Description
        Text
            On $\bar{M}_{0,n}$, the divisor kappa may be defined by $K + \Delta$, where $K$ is the canonical divisor, and $\Delta$ is the sum of the boundary classes $B_i$.  A fun fact is that kappa . $F_{I_1,I_2,I_3,I_4} =1$ for every @TO "F curve"@.   	   

	Example
	    kappaDivisorM0nbar(14)
///

TEST ///
    assert(coefficientList(kappaDivisorM0nbar(14)) === {11/13, 20/13, 27/13, 32/13, 35/13, 36/13})
///		    

doc ///
    Key 
        psiDivisorM0nbar
	(psiDivisorM0nbar,ZZ)
    Headline
        returns the class of the divisor $\Psi$
    Usage 
	psiDivisorM0nbar(14)
    Inputs
        n:ZZ
    Outputs 
        D:SymmetricDivisorM0nbar
    Description
        Text
	    Let $U$ be the universal family over $M=\bar{M}_{0,n}$, let $\omega_{U/M}$ be the relative dualizing sheaf, and let $\sigma_i: M \rightarrow U$ be the sections defining the marked points. The divisors $\psi_i$ are defined by $\psi_i := \sigma_i^*(\omega_{U/M})$.  We define the class $\Psi$ by $\Psi = \psi_1 + ... + \psi_n.$

	Example
	    psiDivisorM0nbar(14)
///
        
TEST ///
    assert(coefficientList(psiDivisorM0nbar(14)) === {24/13, 33/13, 40/13, 45/13, 48/13, 49/13})
///	

doc ///
    Key 
        conformalBlockVectorBundle
	(conformalBlockVectorBundle,LieAlgebra,ZZ,List,ZZ)
    Headline 
        creates an object of class ConformalBlockVectorBundle
    Usage 
        conformalBlockVectorBundle(g,l,w,genus)
    Inputs 
        g:LieAlgebra
	l:ZZ
	w:List
	genus:ZZ
    Outputs 
        V:ConformalBlockVectorBundle
    Description
        Text
            This function creates an object of the type ConformalBlockVectorBundle.
	    
	Text
	    In the example below we create the conformal block bundle $V(sl_3,2,(\omega_1,\omega_1,\omega_1,\omega_2,\omega_2,\omega_2))$ on $\bar{M}_{0,6}$.

	Example
	    sl_3=simpleLieAlgebra("A",2);
	    V=conformalBlockVectorBundle(sl_3,2,{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}},0)
///

TEST ///
    sl_3=simpleLieAlgebra("A",2);
    V=conformalBlockVectorBundle(sl_3,2,{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}},0)
    assert(V#"LieAlgebra" === sl_3)
    assert(V#"Level" === 2)
    assert(V#"Weights" === {{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}})
    assert(V#"NumberOfPoints" === 6)
    --assert(V#"Genus"=== 0)
///




doc ///
    Key 
        conformalBlockRank
	(conformalBlockRank,ConformalBlockVectorBundle)
    Headline 
        computes the rank of the conformal block vector bundle
    Usage 
        conformalBlockRank(V)
    Inputs 
        V:ConformalBlockVectorBundle
    Outputs 
        r:ZZ
    Description
        Text
            This function uses propagation and factorization to recursively compute ranks in terms of the ranks on $\bar{M}_{0,3}$.  These are determined by the so-called fusion rules and are computed via the function @TO "LieTypes::fusionCoefficient"@ in the @TO "LieTypes"@ package.  See @TO2{"Bibliography","[Beauville]"}@ for details on these topics.
	    
	Text
	    In the example below we compute the rank of the conformal block bundle $V(sl_3,2,(\omega_1,\omega_1,\omega_2,\omega_2))$.

	Example
	    sl_3=simpleLieAlgebra("A",2);
	    V=conformalBlockVectorBundle(sl_3,2,{{1,0},{1,0},{0,1},{0,1}},0)
	    conformalBlockRank(V)
///

TEST ///
    sl_3=simpleLieAlgebra("A",2);
    V=conformalBlockVectorBundle(sl_3,2,{{1,0},{1,0},{0,1},{0,1}},0)
    assert(conformalBlockRank(V)=== 2)
///
   
doc ///
    Key
        symmetrizedConformalBlockDivisor
	(symmetrizedConformalBlockDivisor,ConformalBlockVectorBundle)
    Headline 
        computes the symmetrization of the first Chern class of a conformal block vector bundle
    Usage 
        symmetrizedConformalBlockDivisor(V)
    Inputs 
        V:ConformalBlockVectorBundle
    Outputs 
        D:SymmetricDivisorM0nbar
    Description
        Text
            This function implements the formula given in @TO2{"Bibliography","[Fakh]"}@ Corollary 3.6.  It computes the symmetrization of the first Chern class of a conformal block vector bundle: $\sum_{S_n} c_1 V(\mathbf{g},l,(\lambda_{\sigma 1},...\lambda_{\sigma n}))$.
	    
	Text
	    NEW in Version 2.1: Previously there was a separate, faster function to use in the case that $\lambda_1 = ... = \lambda_n$.  However, now this function automatically checks for symmetry and uses the faster formula if applicable, so the user does not need to use two separate functions.
	     
	Text
	    In the example below, we compute the symmetrization of the divisor class of the conformal block bundle $V(sl_4,1,(\omega_1,\omega_1,\omega_2,\omega_2,\omega_3,\omega_3))$.	  

	Example
	    sl_4 =simpleLieAlgebra("A",3);
	    V=conformalBlockVectorBundle(sl_4,1,{{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}},0);
	    D=symmetrizedConformalBlockDivisor(V)
///

TEST ///
    sl_4 =simpleLieAlgebra("A",3);
    V=conformalBlockVectorBundle(sl_4,1,{{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}},0);
    D=symmetrizedConformalBlockDivisor(V)
///	     	
		
doc ///
    Key 
        conformalBlockDegreeM04bar
	(conformalBlockDegreeM04bar,ConformalBlockVectorBundle)
    Headline 
        computes the degree of a conformal block bundle on $\bar{M}_{0,4}$
    Usage 
        conformalBlockDegreeM04bar(V)
    Inputs 
        V:ConformalBlockVectorBundle
    Outputs 
        d:ZZ
    Description
        Text	
            This function implements the formula given in @TO2{"Bibliography","[Fakh]"}@ Corollary 3.5 for computing the degree of a conformal block vector bundle $V$ on $\bar{M}_{0,4}$.
	    
	Text
	    The first line of the example below shows that the conformal block bundle $V(sl_3,1,(\omega_1,\omega_1,\omega_2,\omega_2))$ has degree 1 on $\bar{M}_{0,4} \cong \mathbb{P}^1$. The second line shows that this vector bundle is a line bundle.  Hence, $V(sl_3,1,(\omega_1,\omega_1,\omega_2,\omega_2))$ is isomorphic to $\mathcal{O}(1)$.

	Example
	    sl_3 = simpleLieAlgebra("A",2);
	    V=conformalBlockVectorBundle(sl_3,1,{{1,0},{1,0},{0,1},{0,1}},0);
	    conformalBlockDegreeM04bar(V)
	    conformalBlockRank(V)
///

TEST ///
    sl_3 = simpleLieAlgebra("A",2);
    V=conformalBlockVectorBundle(sl_3,1,{{1,0},{1,0},{0,1},{0,1}},0);
    assert(conformalBlockDegreeM04bar(V) === 1)
///	
	
doc ///	
    Key 
        FCurveDotConformalBlockDivisor
        (FCurveDotConformalBlockDivisor,List,ConformalBlockVectorBundle)	
    Headline 
        intersection of an F-curve with a conformal block divisor 
    Usage 
        FCurveDotConformalBlockDivisor(C,V)
    Inputs 
        C:List
	V:ConformalBlockVectorBundle
    Outputs 
        k:ZZ
    Description
        Text
            This function implements the formulas given in @TO2{"Bibliography","[Fakh]"}@ Prop. 2.7 and Cor. 3.5.  Note: in contrast with most of the other functions in this package, this function is for UNsymmetrized curves and bundles.  The @TO "F curve"@ must be entered as a partition of the set {1,...,n} into four nonempty subsets.
	    
	Text
	    The example below shows that the first Chern class of the conformal block bundle $V(sl_2,1,(1,1,1,1,1,1))$ intersects the F curve $F_{123,4,5,6}$ positively, and intersects $F_{12,34,5,6}$ in degree zero.

	Example
	    sl_2=simpleLieAlgebra("A",1);
	    V=conformalBlockVectorBundle(sl_2,1,{{1},{1},{1},{1},{1},{1}},0);
	    FCurveDotConformalBlockDivisor({{1,2,3},{4},{5},{6}},V)
            FCurveDotConformalBlockDivisor({{1,2},{3,4},{5},{6}},V)
	    sl_3=simpleLieAlgebra("A",2);
	    W=conformalBlockVectorBundle(sl_3,1,{{0,1},{1,0},{1,0},{1,0},{1,0}},0);
	    FCurveDotConformalBlockDivisor({{4,5},{1},{2},{3}},W)
///

TEST ///
    sl_2=simpleLieAlgebra("A",1);
    V=conformalBlockVectorBundle(sl_2,1,{{1},{1},{1},{1},{1},{1}},0);
    assert( FCurveDotConformalBlockDivisor({{1,2,3},{4},{5},{6}},V) === 1 )
    assert( FCurveDotConformalBlockDivisor({{1,2},{3,4},{5},{6}},V) === 0 )
    sl_3=simpleLieAlgebra("A",2);
    W=conformalBlockVectorBundle(sl_3,1,{{0,1},{1,0},{1,0},{1,0},{1,0}},0);
    assert( FCurveDotConformalBlockDivisor({{4,5},{1},{2},{3}},W) === 1 )
///	
	
       	
	
