-- In general, dim (V otimes V*)^G = 1
-- We have a special formula for this invariant when G = SL(n)
-- in terms of the Gelfand-Tsetlin bases for V and V*




gtp = L -> gtPatternFromEntries("A",L)

mirror = (L) -> (
    n:=0;
    while n*(n+1)/2<#L do n=n+1;
    if n*(n+1)/2!=#L then error "#L is not a triangular number";
    x:=gtpA(L);
    flatten apply(n, i -> reverse apply(n-i,j -> x#(n-i,j+1)))
);

dualGTPattern = (L) -> (
    m:=mirror(L);
    n:=first L;
    apply(m, k -> n-k)
);

monomialToPair = (m) -> (
    e:=first exponents m;
    select(#e, i -> e_i!=0)
);


molevCoefficient = (GTP) -> (
    n:=#(GTP#"weight")+1;
    p:=1;
    for k from 2 to n do (
        for i from 1 to k-1 do (
	    for j from i to k-1 do (
		p = p*(((GTP#(k,i)-i+1)-(GTP#(k-1,j)-j+1))!)/(((GTP#(k-1,i)-i+1)-(GTP#(k-1,j)-j+1))!);
	    )
	);
        for i from 1 to k-1 do (
	    for j from i+1 to k do (
		p = p*(((GTP#(k,i)-i+1)-(GTP#(k,j)-j+1)-1)!)/(((GTP#(k-1,i)-i+1)-(GTP#(k,j)-j+1)-1)!);
	    )
	);
    );
    p	
)

scalev = v -> (
    if all(v, i -> i==0) then return v;
    g:=gcd(v);
    1/g*v
);


gtInvariantInVtensorVdual = method(
    Options=>{"SaveAsFunction"=>""},
    TypicalValue=>List
);


gtInvariantInVtensorVdual(List) := o -> (lambda) -> (
    -*
    g := simpleLieAlgebra("A",#lambda);
    Vlambda := irreducibleLieAlgebraModule(lambda,g);
    LAB := lieAlgebraBasis(g);
    Llambda := GTrepresentationMatrices(Vlambda);
    rhoVlambda := lieAlgebraRepresentation(Vlambda,LAB,Llambda);
    lambdastar := starInvolution(lambda,g);
    Vlambdastar := irreducibleLieAlgebraModule(lambdastar,g);
    Llambdastar := GTrepresentationMatrices(Vlambdastar);
    rhoVlambdastar := lieAlgebraRepresentation(Vlambdastar,LAB,Llambdastar);
    zeroVector:=apply(#lambda, i -> 0);
    hwv := first weightNuHighestWeightVectorsInVtensorW(zeroVector,rhoVlambda,rhoVlambdastar);
    R := ring hwv;
    *-
    lambdastar := starInvolution(lambda,simpleLieAlgebra("A",#lambda));
    Blambda := gtPatterns("A",dynkinToPartition("A",lambda));
    Blambdastar := gtPatterns("A",dynkinToPartition("A",lambdastar));
    N := #Blambda;
    A:=getSymbol "A";
    B:=getSymbol "B";
    R := QQ[join(apply(N, i -> A_i),apply(N, i -> B_i)),MonomialOrder=>Lex];
    -- Build my polynomial
    f := 0;
    p0:={};
    p1:={};
    j:=0;
    a:=0;
    b:=0;
    coeff:=0;
    sgn:=0;
    for i from 0 to N-1 do (
        p0 = Blambda_i;
        p1 = dualGTPattern(Blambda_i);
        j = first select(N, k -> Blambdastar_k==p1);
        a = molevCoefficient(gtp(p0));
        b = molevCoefficient(gtp(p1));
        coeff=lift(1/(sqrt(a*b)),QQ);
        sgn = (-1)^(1+level((gtp(p0))#"weight","A",#lambda));
        f = f + sgn*coeff*R_i*R_(N+j)
    );
    if o#"SaveAsFunction"!="" then (
        fn:=openOut concatenate(o#"SaveAsFunction",".m2");
        fn << concatenate(o#"SaveAsFunction", " = B -> ",toString(f)) << endl;
        close fn
    );
    f
);


