

evenWedgeBasis = memoize( n -> flatten delete(null,apply(n+1, k -> if even(k) then sort subsets(apply(n, i -> i+1),k))) );   
oddWedgeBasis = memoize( n -> flatten delete(null,apply(n+1, k -> if odd(k) then sort subsets(apply(n, i -> i+1),k))) ); 
wedgeBasis = memoize(n -> join(evenWedgeBasis(n),oddWedgeBasis(n)));


contracteIByemk = (k,I,WedgeBasis,R) -> (    
    if not member(k,I) then return apply(#WedgeBasis, i -> 0_R);
    Imk := select(I, j -> j!=k);
    p:=position(I, j -> j==k);
    apply(WedgeBasis, J -> if J==Imk then (-1_R)^p else 0_R)
);



contractByemk = (k,WedgeBasis,R) ->( 
    transpose matrix apply(WedgeBasis, I -> contracteIByemk(k,I,WedgeBasis,R))    
);



wedgeeIByek = (k,I,WedgeBasis,R) -> (    
    if member(k,I) then return apply(#WedgeBasis, i -> 0_R);
    Icupk := sort prepend(k,I);
    p:=position(Icupk, j -> j==k);
    apply(WedgeBasis, J -> if J==Icupk then (-1_R)^p else 0_R)
);



wedgeByek = (k,WedgeBasis,R) ->( 
    transpose matrix apply(WedgeBasis, I -> wedgeeIByek(k,I,WedgeBasis,R))    
);



-- Now try in the representation
identityMatrix = (n,R) -> (
    matrix apply(n, i -> apply(n, j -> if i==j then 1_R else 0_R))
);



spinRepresentationMatrices = method(
    Options=>{CoefficientRing=>QQ},
    TypicalValue=>List
); 

spinRepresentationMatrices(ZZ) := o -> (n) -> (
    B:={};
    R:=o#CoefficientRing;
    I:=identityMatrix(2^n,R);
    WedgeBasis:=wedgeBasis(n);
    Hbasis := apply(n, i ->  wedgeByek(i+1,WedgeBasis,R)*contractByemk(i+1,WedgeBasis,R) - (1/2)*I);
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then wedgeByek(i+1,WedgeBasis,R)*contractByemk(j+1,WedgeBasis,R))));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then wedgeByek(i+1,WedgeBasis,R)*wedgeByek(j+1,WedgeBasis,R)))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then contractByemk(j+1,WedgeBasis,R)*contractByemk(i+1,WedgeBasis,R)))); 
    L:=flatten {Hbasis, Xbasis, Ybasis, Zbasis};
    sigma := so2nPermutation(n);
    apply(sigma, i -> L_i)
);


halfspinRepresentationMatrices = method(
    Options=>{CoefficientRing=>QQ},
    TypicalValue=>List
); 

halfspinRepresentationMatrices(ZZ,ZZ) := o -> (n,p) -> (
    L:=spinRepresentationMatrices(n,CoefficientRing=>o#CoefficientRing);
    s:=apply(2^(n-1),i -> i);
    if odd(p) then s = apply(2^(n-1),i -> i+2^(n-1));
    apply(#L, i -> (L_i)_s^s)
);

