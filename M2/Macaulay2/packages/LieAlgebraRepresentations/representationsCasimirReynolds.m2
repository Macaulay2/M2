LieAlgebraRepresentation = new Type of HashTable  
-- Keys:
-- Character
-- Basis
-- RepresentationMatrices

lieAlgebraRepresentation = method(
    TypicalValue=>LieAlgebraRepresentation
);


lieAlgebraRepresentation(LieAlgebraModule,LieAlgebraBasis,List):=(V,LAB,L) -> (
    new LieAlgebraRepresentation from {
        "Module"=>V,
        "Basis"=>LAB,
	"RepresentationMatrices"=>L
    }
);



trivialRepresentation = method(
    TypicalValue => LieAlgebraRepresentation
    )

trivialRepresentation(String,ZZ) := (type,m) -> (
    LAB:=lieAlgebraBasis(type,m);
    trivialRepresentation(LAB)
);

trivialRepresentation(LieAlgebra) := (g) -> (
    LAB:=lieAlgebraBasis(g);
    trivialRepresentation(LAB)
);

trivialRepresentation(LieAlgebraBasis) := LAB -> (   
    L := apply(#(LAB#"BasisElements"), i -> matrix {{0/1}});
    V := trivialModule(LAB#"LieAlgebra");
    lieAlgebraRepresentation(V,LAB,L)
);


standardRepresentation = method(
    TypicalValue => LieAlgebraRepresentation
    )

standardRepresentation(String,ZZ) := (type,m) -> (
    if not member(type,{"A","B","C","D"}) then error "Only implemented for types A,B,C,D";    
    LAB:=lieAlgebraBasis(type,m);
    V := standardModule(LAB#"LieAlgebra");
    lieAlgebraRepresentation(V,LAB,LAB#"BasisElements")

);

standardRepresentation(LieAlgebra) := g -> (
    if not member(g#"RootSystemType",{"A","B","C","D"}) then error "Only implemented for types A,B,C,D";
    LAB:=lieAlgebraBasis(g);
    V := standardModule(LAB#"LieAlgebra");
    lieAlgebraRepresentation(V,LAB,LAB#"BasisElements")
);

-*
standardRepresentation(LieAlgebraBasis) := LAB -> (
    g:=LAB#"LieAlgebra";
    if not member(g#"RootSystemType",{"A","B","C","D"}) then error "Only implemented for types A,B,C,D";
    V := standardModule(LAB#"LieAlgebra");
    lieAlgebraRepresentation(V,LAB,LAB#"BasisElements")
);
*-

adjointRepresentation = method(
    TypicalValue => LieAlgebraRepresentation
    )

adjointRepresentation(String,ZZ) := (type,m) -> (
    LAB:=lieAlgebraBasis(type,m);
    adjointRepresentation(LAB)
);

adjointRepresentation(LieAlgebra) := (g) -> (
    LAB:=lieAlgebraBasis(g);
    adjointRepresentation(LAB)
);

adjointRepresentation(LieAlgebraBasis) := LAB -> (
    br := LAB#"Bracket";
    writeInBasis := LAB#"WriteInBasis";
    B := LAB#"BasisElements";
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    V := adjointModule(LAB#"LieAlgebra");
    lieAlgebraRepresentation(V,LAB,L)
);






representationWeights = method(
    TypicalValue=>List
);

representationWeights(LieAlgebraRepresentation) := memoize((rho) -> (
    W:=rho#"Module";
    LAB:=rho#"Basis";
    L:=rho#"RepresentationMatrices";
    Wweights:={};
    m:=LAB#"LieAlgebra"#"LieAlgebraRank";
    L1:=apply(dim W, i -> apply(m, j -> (L_j)_(i,i)));
    apply(L1, v -> apply(v, i -> lift(i,ZZ)))
));



-- Let V be a LieAlgebraModule with a representation rho installed

-- The Casimir operator is sum_i rho(Bstar_i)*rho(B_i)

casimirOperator = method(
);


casimirOperator(LieAlgebraRepresentation) := (rho) -> (
    W:=rho#"Module";
    LAB:=rho#"Basis";
    rhoB:=rho#"RepresentationMatrices";
    M:={};
    c:={};
    rhoBstar:=for i from 0 to #rhoB-1 list (
        M=(LAB#"DualBasis")_i;
	c=(LAB#"WriteInBasis")(M);
	sum apply(#rhoB, j -> c_j*rhoB_j)
    );
    sum apply(#rhoB, i -> (rhoBstar_i)*(rhoB_i))
);


casimirSpectrum = method(
    TypicalValue=>List
);


casimirSpectrum(LieAlgebraModule) := (W) -> (
    unique sort apply(keys(W#"DecompositionIntoIrreducibles"), w -> casimirScalar(irreducibleLieAlgebraModule(w,W#"LieAlgebra")))
);



casimirProjection = method(
);


casimirProjection(LieAlgebraRepresentation,ZZ) := (rho,z) -> (
    casimirProjection(rho,1/1*z)
);

casimirProjection(LieAlgebraRepresentation,QQ) := (rho,z) -> (
    Cas:=casimirOperator(rho);
    W:=rho#"Module";
    L:=delete(1/1*z,casimirSpectrum(W));
    N:=dim W;
    I:=matrix apply(N, i -> apply(N, j -> if i==j then 1_(ring Cas) else 0));
    product apply(L, x -> (Cas-x*I))
);


reynoldsOperator = method(
);


reynoldsOperator(LieAlgebraRepresentation) := (rho) -> (
    casimirProjection(rho,0)
);

