------------------------------------------
-- Constructors for common Mackey functors
------------------------------------------

makeBurnsideMackeyFunctor = method()
makeBurnsideMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    C := matrix{{1}};
    T := matrix{{0},{1}};
    R := matrix{{1,p}};

    return makeCpMackeyFunctor(p,R,T,C);
)

makeFixedFreeMackeyFunctor = makeBurnsideMackeyFunctor

makeUnderlyingFreeMackeyFunctor = method()
makeUnderlyingFreeMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    I := mutableMatrix id_(ZZ^p);                                    --declare identity matrix
    C := matrix(rowPermute(I,0,({p-1}|(toList (0..p-2)))));
    T := matrix({for i to p-1 list 1});
    R := matrix(for i to p-1 list {1});

    return makeCpMackeyFunctor(p,R,T,C);
)

makeComplexRepresentationMackeyFunctor = method()
makeComplexRepresentationMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    C := matrix {{1}};
    T := matrix (for i to p-1 list {1});
    R := matrix {for i to p-1 list 1};

    return makeCpMackeyFunctor(p,R,T,C);
)

makeRealRepresentationMackeyFunctor = method()
makeRealRepresentationMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    if p < 3 then (
        return makeComplexRepresentationMackeyFunctor p
    )
    else (
        C := matrix {{1}};
        T := matrix (for i to (p-1)//2 list {1});
        R := matrix {{1} | (for i to (p-3)//2 list 2)};

        return makeCpMackeyFunctor(p,R,T,C);
    )
)

makeZeroMackeyFunctor = method()
makeZeroMackeyFunctor (ZZ) := CpMackeyFunctor => (p) -> (
    C:=matrix({});
    R:=C;
    T:=C;
    return makeCpMackeyFunctor(p,R,T,C)
)

makeFixedPointMackeyFunctor = method()
makeFixedPointMackeyFunctor (ZZ,Matrix) := CpMackeyFunctor => (p,C) -> (
    m := C^0 - C; --declare the matrix 1-C
    R := inducedMap(source m, kernel m);
    T := inducedMap(kernel m, source m, sum (for i to p-1 list C^i));
    return makeCpMackeyFunctor(p,R,T,C)
)

makeOrbitMackeyFunctor = method()
makeOrbitMackeyFunctor (ZZ,Matrix) := CpMackeyFunctor => (p,C) -> (
    m := C^0 - C; --declare the matrix 1-C
    R := inducedMap(source m, cokernel m, sum (for i to p-1 list C^i));
    T := inducedMap(cokernel m, source m);
    return makeCpMackeyFunctor(p,R,T,C)
)

makeFixedTrivMackeyFunctor = method()
makeFixedTrivMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    U := coker matrix {{p}};
    makeFixedPointMackeyFunctor(p, id_U)
)

-- p = 2!
makeFixedSignMackeyFunctor = method(Dispatch => Thing)   --takes no argument so write makeFixedSignMackeyFunctor()
makeFixedSignMackeyFunctor Sequence := x -> (
    makeFixedPointMackeyFunctor(2, -id_(ZZ^1))
)

makeZeroOnUnderlyingMackeyFunctor = method()
makeZeroOnUnderlyingMackeyFunctor(ZZ,Module) := CpMackeyFunctor => (p,M) -> (
    U := ZZ^0;
    F := M;
    R := map(U,F,map(U,F,0));
    T := map(F,U,map(F,U,0));
    C := map(U,U,id_U);
    makeCpMackeyFunctor(p,R,T,C)
)

isPrimePower = method()
isPrimePower(ZZ) := Boolean => (n) -> (
    if n < 1 then
        return false;
    return #(factor n) <= 1
)

makeKGroupMackeyFunctor = method()
makeKGroupMackeyFunctor(ZZ,ZZ,ZZ) := CpMackeyFunctor => (p,q,n) -> (
    if not isPrime p then
        error "p must be prime";
    if not isPrimePower q then
        error "q must be a prime power";
    if n < 1 then
        error "n must be a positive integer";
    und := coker matrix {{q^(n*p) - 1}};
    fix := coker matrix {{q^n - 1}};
    tr := inducedMap(fix, und, matrix {{1}});
    re := inducedMap(und, fix, matrix {{sum (for i to p-1 list q^(n*i))}});
    conj := inducedMap(und, und, matrix {{q^n}});
    makeCpMackeyFunctor(p, re, tr, conj)
)