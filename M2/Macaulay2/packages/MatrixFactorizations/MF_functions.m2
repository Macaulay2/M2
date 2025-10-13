
--currently not working, but will put it back: needs "ZZdFactorizationsCODE.m2"
--take this one out when putting above back in:
--needs "ZZdFactorizations.m2"
--needsPackage "TensorComplexes" --Koszul factorization method needs this
--needsPackage "Cyclotomic" --for adjoining roots of unity
--needsPackage "Matrix Factorizations"
--shift function
--shift = method()
--shift(ZZdFactorization) := ZZdFactorization => X -> (
--    ZZdfactorization{-(dd^X)_2, -(dd^X)_1}
--    )

--direct sum function
--directSumMF = method()
--directSumMF(ZZdFactorization, ZZdFactorization) := ZZdFactorization => (X, Y) -> (
    --check they are factorizations of same polynomial
--    x:= dd^X;
--    y:= dd^Y;
--    ZZdfactorization{x_1 ++ y_1, x_2 ++ y_2}
--    )


exponents(ZZ) := d -> (apply(toList factor d, i->i#1))

-*moebius = method();
moebius(ZZ) := d -> (if any(exponents(d,l->l>1) then 0
	 else return (-1)^(length exponents d)))
*-


-*cyclotomicPoly = method();
cyclotomicPoly(ZZ,Ring,Symbol) := (d,R,t) -> (
*-

--isMatrixFactorization
--returns true if matrices in factorization multiply to f*id in all (2) cyclic permutations
-*isMatrixFactorization = method()
isMatrixFactorization(ZZdFactorization) := X -> (
    P:= matrix entries ((dd^X)_1*(dd^X)_2); --product of matrices
    S:= matrix entries ((dd^(shift X))_1*(dd^(shift X))_2); --product of shifted matrices
    P == matrix entries (P_(0,0)*id_(source (dd^X)_1)) and S == matrix entries (S_(0,0)*id_(source(dd^(shift X))_1))
    )*-

-*isdFactorization(ZZdFactorization) := X -> (
    P:= matrix entries product(apply(X.period, i->(dd^X)_i));
    f:= P_(0,0);
    S:= for i to X.period-1 list(
	product(apply(X.period, j->(dd^X)_(i+j)))
	); --S is a list of product of all cyclic shifts of matrix multiplication
    --S)
    if same S then (same S, f) else (same S, "no potential")
    --error "Your ZZdFactorization is not an actual matrix factorization."
    )*-

--Note: previous version had an issue when the differential composed to the zero map
--fixed now, I think
--input is a d-fold matrix factorization, and the output is a sequence
--specifying whether the differentials of the factorization compose to a scalar multiple
--of the identity, then specifying the scalar multiple as the second term of the sequence
isdFactorization = method()
isdFactorization(ZZdFactorization) := (Boolean, RingElement) =>  X -> (
    p := period X;
    S := (X.dd)^p;
    --f := (S_0)_(0,0);
    --if S == f*id_X then (true,f) else (false, "no potential")
    if S == 0 then (true, 0)
    else if S == (S_0)_(0,0) then (true, (S_0)_(0,0))
    else (false, "no potential")
    )



--function for generating matrix factorization from a
--module M over hypersurface ring
tailMF = method()
tailMF(Module) := ZZdFactorization => M -> (
    F := res(M, LengthLimit=>dim ring M + 1);
    C := chainComplex sub(F.dd_(dim ring M + 1), ambient ring M);
    h := (nullhomotopy extend(C, C, ((ring M).relations)_(0,0)*id_(C_0)))_0;
    ZZdfactorization{sub(F.dd_(dim ring M + 1), ambient ring M), h}
    )

tailMF(Ideal) := ZZdFactorization => I -> tailMF(module I)

--cone
--in: map of MFs
--out: new MF
cone(ZZdFactorizationMap) := ZZdFactorization => f -> (
    s := source f;
    t := target f;
    m1 := matrix{{(dd^s)_1, map(s_0, t_0, 0)},
    {f_1, -(dd^t)_2}};
    m2 := matrix{{(dd^s)_2, map(s_1, t_1, 0)}, 
    {f_0, -(dd^t)_1}};
    ZZdfactorization{m1, m2}
    )

--dual
--note: dual is a method with options
-*dual(ZZdFactorization) := ZZdFactorization => {} >> o -> X -> (
    if not(X.period == 2) then error "since period of X is not 2, please input a root of unity";
    ZZdfactorization{-dual((dd^X)_2), dual((dd^X)_1)}
    )

dual(ZZdFactorization, RingElement) := ZZdFactorization => {} >> o -> (X,omega) -> (
    ZZdfactorization for i to X.period-1 list (-omega^i)*dual((dd^X)_(X.period - i))
    )


--hom
Hom(ZZdFactorization, ZZdFactorization) := ZZdFactorization => (X,Y) -> (
    tensorMF(dual X, Y)
    )

--dHom
Hom(ZZdFactorization, ZZdFactorization, RingElement) := ZZdFactorization => (X,Y,omega) -> (
    dTensor(dual(X,omega),Y,omega)
    )*-

-*OLD
--dual
--note: dual is a method with options
dual(ZZdFactorization) := ZZdFactorization => {} >> o -> X -> (
    ZZdfactorization{-dual((dd^X)_2), dual((dd^X)_1)}
    )

--hom
Hom(ZZdFactorization, ZZdFactorization) := ZZdFactorization => (X,Y) -> (
    tensorMF(dual X, Y)
    )*-



------tensorMF

tensorMF = method()
tensorMF(ZZdFactorization, ZZdFactorization) := ZZdFactorization => (F,G) -> (
    Y := youngest(F,G);
    if Y.cache#?(tensor,F,G) then return Y.cache#(tensor,F,G);
    N := F.period;
    modules := hashTable for i to N-1 list i => (
	directSum for j to N-1 list (
	    {j,(i-j)%N} => F_j ** G_(i-j)
	    )
	);
    t := -1;
    dF := dd^F;
    dG := dd^G;
    M := for i from 1 to N list(
	map(modules#((i-1)%N),
            modules#(i%N),
            matrix table(
                indices modules#((i-1)%N),
                indices modules#(i%N),
                (j,k) -> (
                    tar := component(modules#((i-1)%N), j);
                    src := component(modules#(i%N), k);
                    map(tar, src, 
                        if ({(k#0-j#0)%N,(k#1-j#1)%N} == {0,1}) then (t^(k#0)*id_(F_(k#0)))**(dG_(k#1))
                        else if ({(k#0-j#0)%N,(k#1-j#1)%N} == {1,0})  then (dF_(k#0))**(id_(G_(k#1)))
                        else 0)
                    ))));
    result := ZZdfactorization(M);
    result.cache.tensor = (F,G);
    Y.cache#(tensor,F,G) = result;
    result
)



    -*if not(X.period==2 and Y.period==2) then error "Both inputs should have period 2. Use dTensor method instead.";
    yng := youngest(X,Y);
    if yng.cache#?(tensor,X,Y) then return yng.cache#(tensor,X,Y);
    modules := hashTable for i to 1 list i => (
	directSum for j to 1 list (
	    {j,(i-j)%2} => X_i ** Y_(i-j)
	    )
	);
    A1 := id_(X_0) ** (dd^Y)_1;
    B1 := (dd^X)_1 ** id_(Y_0);	   
    C1 := (dd^X)_2 ** id_(Y_1);
    D1 := -id_(X_1)** (dd^Y)_2;
    diff1 := map(modules#0,modules#1,matrix{{A1, B1},{C1,D1}});
    A2  := id_(X_0) ** (dd^Y)_2;
    B2 := (dd^X)_1 ** id_(Y_1);
    C2 := (dd^X)_2 ** id_(Y_0);
    D2 := -id_(X_1)** (dd^Y)_1;
    diff2 := map(modules#1,modules#0,matrix{{A2, B2},{C2,D2}});
    result := ZZdfactorization{diff1, diff2};
    result.cache.tensor = (X,Y);
    yng.cache#(tensor,X,Y) = result;
    result
)*-

tensorMF(ZZdFactorizationMap,ZZdFactorizationMap) := (f,g) -> (
    df := degree f;
    dg := degree g;
    per := period f;
    src := (source f) ** (source g);
    tar := (target f) ** (target g);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    maps := hashTable for i from 1 to per list i => (
        if tar_(i+df+dg) == 0 or src_i == 0 then (
            map(tar_(i+df+dg), src_i, 0)
            )
        else (
            m := for q in indices tar_(i+df+dg) list (
                -- so q == {k,i+df+dg-k}
                for p in indices src_i list (
                    -- so p == {j,i-j}, for various j
                    if p#0 == (q#0 - df)%per
                    then (
                        sgn := 1; -- function of df, dg, i
                        sgn = (-1)^(i * dg);
                        sgn * (f_(p#0) **  g_(p#1))
                        )
                    else map(component(tar_(i+df+dg), q),
                        component(src_i, p),
                        0)
                    ));
            map(tar_(i+df+dg), src_i, matrix m)
            )
        );
    result := map(tar, src, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then
        result.cache.isCommutative = true;
    result    
    )




    -*degf := degree f;
    degg := degree g;
    srcf := source f;
    srcg := source g;
    trgf := target f;
    trgg := target g;
    trgfg := trgf ** trgg;
    srcfg := srcf ** srcg;
    H := new HashTable from {1=>map(trgfg_1, srcfg_1, directSum {{0,1} => f_0**g_1, {1,0} => f_1**g_0}) , 2 =>map(trgfg_1, srcfg_0, directSum {{0,0} => f_0**g_0, {1,0} => f_1**g_1})};
    map(trgf**trgg,srcf**srcg,H,Degree => degf + degg)
    )*-

-----KoszulMF
--koszul matrix factorization
--in: list of even number of elements in ring
--out: matrix factorization of sum products of pairs (L#i)*(L#(i+1))

koszulMFhelper  = method()
koszulMFhelper (List) := L -> (
    X := ZZdfactorization{L#0, L#1};
    for i from 1 to (#L)//2-1 do X = tensorMF(X, ZZdfactorization{ L#(2*i), L#(2*i+1)});
    X
)
-- 2 case is not consistent breaks a list into 2, rather than inputs 2 lists


---inputs a list of d lists each of length n outputs the tensor product of n ZZdfactorizations
koszulMFhelper(List, RingElement) := (L, omega) -> (
    n := #(L#0);
    d := #L;
    Z := for i to n-1 list ZZdfactorization for j to d-1 list (L#j)#i;
    T := Z#0;
    for i from 1 to n-1 do T = dTensor(T,Z#i, omega);
    T
)


---Koszul matrix factorization from ideal and a polynomial
koszulMF = method()
koszulMF(List, RingElement) := (L,f) -> (
    M := f //matrix{L};
    N := transpose (matrix{L}) | M;     
    E  := entries N;
    F := select(E, i -> not(i#1 == 0)); 
    A := flatten F;
    koszulMFhelper(A)
)

koszulMF(Ideal, RingElement) := (I,f) -> (
    koszulMF(flatten entries gens I, f)
)

koszulMF(RingElement) := f -> koszulMF(ideal vars ring f, f)

--constructs Koszul matrix factorization with respect to the Jacobian ideal. Works best in characteristic 0
eulerMF = method();
eulerMF(RingElement) := ZZdFactorization =>  f -> (koszulMF(ideal jacobian f , f))

--i = homological degree
--j = F_j degree
freeMods = method()
freeMods(ZZdFactorization, ZZdFactorization) := (F,G) -> (
    N:= F.period;
for i to N-1 list(
for j to N-1 list(
    {j, (N + i - j)%N} => (F_j)**(G_((N+i-j)%N))
    )))

cycloPoly = (i,v) -> (
            -- returns the i-th cyclotomic polynomial in variable v.
            -- v must be a variable a priori
            v = value v;
            if i <= 0 then error "the input should be > 0.";
            if i==1 then return v-1 ;
            mini := v^i -1;
            -- dividing out the first cyclotomic polynomial
            -- (with result a polynomial)
            mini = mini//(v-1);
            -- i is prime:
            if isPrime i then return mini;
            
            -- i is not prime:
            -- find the divisors:
            for f in toList (2..i//2) do (
                 -- check for factor
                 if i%f == 0 then (
                      fac := cycloPoly (f,v);
                      -- division with result in polynomial ring:
                      mini = mini // fac;
                      )
                 );
            --make sure the leading coefficient is one.
            mini
	    )


--adjoin a root of unity to the underlying object input
adjoinRoot = method();
adjoinRoot(ZZ,Ring,Symbol) := (d,Q,t) -> (S1 := Q[t,Degrees => {0}];
    var := (S1_*)_0;
    cyclo := cycloPoly(d, var);
    S := S1/(cyclo);
    S.rootOfUnity = (S_*)_0;
    S
    )

adjoinRoot(ZZ,Ring,RingElement) := (d,Q,t) -> (adjoinRoot(d,Q,getSymbol "t"))

adjoinRoot(ZZdFactorization,RingElement) := (F,t) -> (
    adjoinRoot(F,getSymbol "t")
    )

adjoinRoot(ZZdFactorization,Symbol) := (F,t) -> (
        d := period F;
	S := adjoinRoot(d,ring F,t);
	P := F**S;
	P.cache.rootOfUnity = (S_*)_0;
	P
	)
    
    adjoinRoot(ZZdFactorizationMap,RingElement) := (F,t) -> (
    adjoinRoot(F,getSymbol "t")
    )

adjoinRoot(ZZdFactorizationMap,Symbol) := (F,t) -> (
        d := period F;
	S := adjoinRoot(d,ring F,t);
	P := F**S;
	P.cache.rootOfUnity = (S_*)_0;
	P
	)
     

--dTensor = method(Options => {RootOfUnity=>false})
-*dTensor = method(Options => {RootOfUnity => true})
dTensor(ZZdFactorization, ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,G,t) -> (
    --put in check for F.period = G.period
    if (period F)==2 then return tensorMF(F,G);
    if not(opts.RootOfUnity) then return dTensor(F,G,getSymbol "t",RootOfUnity => false);
    Y := youngest(F,G);
    if Y.cache#?(tensor,F,G) then return Y.cache#(tensor,C,D);
    N := F.period;
    modules := hashTable for i to N-1 list i => (
	directSum for j to N-1 list (
	    {j,(j+i)%N} => F_i ** G_(j+i)
	    )
	);
    dF := dd^F;
    dG := dd^G;
    M := for k to N-1 list(
	for i to N-1 list(
	    for j to N-1 list(
		if i == j then (t^i*id_(F_i))**(dG_(k-i+1))		
		else if (j == (i+1)%N) then (dF_(i+1))**(id_(G_((k-i)%N)))
		else 0
	    )));
    result = ZZdfactorization(for i to #M-1 list matrix M_i);
    result.cache.tensor = (F,G);
    Y.cache#(tensor,F,G) = result;
    result
)*-



--dTensor = method(Options => {RootOfUnity=>false})
dTensor = method(Options => {RootOfUnity => true})
dTensor(ZZdFactorization, ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,G,t) -> (
    --put in check for F.period = G.period
    --if (period F)==2 then return tensorMF(F,G);
    if not(opts.RootOfUnity) and not(F.period == 2) then return dTensor(F,G,getSymbol "t",RootOfUnity => false);
    Y := youngest(F,G);
    if Y.cache#?(tensor,F,G) then return Y.cache#(tensor,F,G);
    N := F.period;
    modules := hashTable for i to N-1 list i => (
	directSum for j to N-1 list (
	    {j,(i-j)%N} => F_j ** G_(i-j)
	    )
	);
    dF := dd^F;
    dG := dd^G;
    M := for i from 1 to N list(
	map(modules#((i-1)%N),
            modules#(i%N),
            matrix table(
                indices modules#((i-1)%N),
                indices modules#(i%N),
                (j,k) -> (
                    tar := component(modules#((i-1)%N), j);
                    src := component(modules#(i%N), k);
                    map(tar, src, 
                        if ({(k#0-j#0)%N,(k#1-j#1)%N} == {0,1}) then (t^(k#0)*id_(F_(k#0)))**(dG_(k#1))
                        else if ({(k#0-j#0)%N,(k#1-j#1)%N} == {1,0})  then (dF_(k#0))**(id_(G_(k#1)))
                        else 0)
                    ))));
    result := ZZdfactorization(M);
    result.cache.tensor = (F,G);
    Y.cache#(tensor,F,G) = result;
    result
)



dTensor(ZZdFactorization,ZZdFactorization,Symbol) := ZZdFactorization => opts -> (F,G,t) -> (S := adjoinRoot(period F,ring F,t);
    dTensor(F**S,G**S,(S_*)_0)
    )

dTensor(List,RingElement) := ZZdFactorization => opts -> (L,t) -> (
    if #L==2 then return dTensor(L_0,L_1,t,opts);
    if not(opts.RootOfUnity) then return dTensor(L,getSymbol "t");
    dTensor(dTensor(L_{0..#L-2},t),L_(#L-1),t)
    )

dTensor(List,Symbol) := ZZdFactorization => opts -> (L,t) -> (
    if #L==2 then return dTensor(L_0,L_1,t,opts);
    S := adjoinRoot(period L_0,ring L_0,t);
    Ln := apply(L,i->i**S);
    dTensor(dTensor(Ln_{0..#Ln-2},(S_*)_0),Ln_(#Ln-1),(S_*)_0)
    )

dTensor(ZZdFactorizationMap,ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    if not(opts.RootOfUnity) then return dTensor(f,g,getSymbol "t");
    df := degree f;
    dg := degree g;
    per := period f;
    src := (source f) ** (source g);
    tar := (target f) ** (target g);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    maps := hashTable for i from 1 to per list i => (
        if tar_(i+df+dg) == 0 or src_i == 0 then (
            map(tar_(i+df+dg), src_i, 0)
            )
        else (
            m := for q in indices tar_(i+df+dg) list (
                -- so q == {k,i+df+dg-k}
                for p in indices src_i list (
                    -- so p == {j,i-j}, for various j
                    if p#0 == (q#0 - df)%per
                    then (
                        sgn := 1; -- function of df, dg, i
                        sgn = (-1)^(i * dg);
                        sgn * (f_(p#0) **  g_(p#1))
                        )
                    else map(component(tar_(i+df+dg), q),
                        component(src_i, p),
                        0)
                    ));
            map(tar_(i+df+dg), src_i, matrix m)
            )
        );
    result := map(tar, src, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then
        result.cache.isCommutative = true;
    result    
    )

dTensor(ZZdFactorizationMap,ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    S := adjoinRoot(period source f,ring f,t);
    dTensor(f**S,g**S,(S_*)_0)
    )
	    
    
tensor(ZZdFactorization,ZZdFactorization) := ZZdFactorization => {} >> opts -> (F,G) -> (
    if not(F.period == G.period) then error "Expected factorizations with the same period";
    if F.period==2 then return tensorMF(F,G);
    if (ring F).?rootOfUnity then return dTensor(F,G,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

tensor(ZZdFactorization,ZZdFactorization,RingElement) := ZZdFactorization => {Dispatch => {ZZdFactorization,ZZdFactorization,RingElement}} >> opts -> (F,G,t) -> (print("here we");
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dTensor(F,G,t,RootOfUnity=>false)
    )

tensor(ZZdFactorization,ZZdFactorization,Symbol) := ZZdFactorization => {} >> opts -> (F,G,t) -> (
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dTensor(F,G,t)
    )

tensor(ZZdFactorizationMap,ZZdFactorizationMap) := ZZdFactorizationMap => {} >> opts -> (f,g) -> (
    F := source f;
    G := source g;
    if not(F.period == G.period) then error "Expected factorizations with the same period";
    if F.period==2 then return tensorMF(f,g);
    if (ring F).?rootOfUnity then return dTensor(f,g,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

tensor(ZZdFactorizationMap,ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => {} >> opts -> (f,g,t) -> (
    F := source f;
    G := source g;
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dTensor(f,g,t,RootOfUnity=>false)
    )

tensor(ZZdFactorizationMap,ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => {} >> opts -> (f,g,t) -> (
    F := source f;
    G := source g;
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dTensor(f,g,t)
    )


ZZdFactorization ** ZZdFactorization := ZZdFactorization => (F,G) -> (tensor(F,G))
Complex ** ZZdFactorization := ZZdFactorization => (C,F) -> (tensor(Fold(C**(ring F),period F),F))
ZZdFactorization ** Complex := ZZdFactorization => (F,C) -> (tensor(F,Fold(C**(ring F),period F)))
ZZdFactorization ** ZZdFactorizationMap := ZZdFactorizationMap => (F,f) -> (tensor(id_F,f))
ZZdFactorizationMap ** ZZdFactorization := ZZdFactorizationMap => (f,F) -> (tensor(f,id_F))
ComplexMap ** ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> (tensor(Fold(f**(ring g),period source g),g))
ZZdFactorizationMap ** ComplexMap := ZZdFactorizationMap => (f,g) -> (tensor(f,Fold(g**(ring f),period source f)))
ZZdFactorizationMap ** ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> (tensor(f,g))
Complex ** ZZdFactorizationMap := ZZdFactorizationMap => (C,g) -> (tensor(Fold((id_C)**(ring g),period source g),g)) 
ZZdFactorizationMap ** Complex := ZZdFactorizationMap => (f,C) -> (tensor(f,Fold((id_C)**(ring f),period source f)))





dual(ZZdFactorization) := ZZdFactorization => {} >> opts -> F -> (
    if F.period == 2 then return ZZdfactorization {-dual F.dd_2,dual F.dd_1};
    if (ring F).?rootOfUnity then return dDual(F,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

dual(ZZdFactorization,RingElement) := ZZdFactorization => {} >> opts -> (F,t) -> (
    if F.period ==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dDual(F,t,RootOfUnity=>false)
    )

dual(ZZdFactorization,Symbol) := ZZdFactorization => {} >> opts -> (F,t) -> (
    if F.period ==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dDual(F,t)
    )

dual(ZZdFactorizationMap) := ZZdFactorizationMap => {} >> opts -> f -> (
    deg := degree f;
    F := source f;
    G := target f;
    dF := dual F;
    dG := dual G;
    if F.period==2 then return map(dual F,dual G,new HashTable from {1=>map(dF_(1-deg), dG_1, dual f_0),2=>map(dF_(-deg), dG_0, dual f_1)},Degree => -degree f);
    if (ring F).?rootOfUnity then return dDual(f,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

dual(ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => {} >> opts -> (f,t) -> (
    if f.period ==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dDual(f,t,RootOfUnity=>false)
    )

dual(ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => {} >> opts -> (f,t) -> (
    if f.period ==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dDual(f,t)
    )

dDual = method(Options => {RootOfUnity => true});
dDual(ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,t) -> (
    if not(opts.RootOfUnity) then return dDual(F,getSymbol "t");
    diffs := reverse values (F.dd.map);
    ZZdfactorization toList apply(0..#diffs-1,i->-t^i*dual(diffs#i))
    )

dDual(ZZdFactorization,Symbol) := ZZdFactorization => opts -> (F,t) -> (
    S := adjoinRoot(period F,ring F,t);
    dDual(F**S,(S_*)_0)
    )

dDual(ZZdFactorizationMap,RingElement) := ZZdFactorization => opts -> (f,t) -> (
    if not(opts.RootOfUnity) then return dDual(t,getSymbol "t");
    dualMaps := reverse values (f.map);
    d := #dualMaps;
    srcf := source f;
    trgf := target f;
    degf := degree f;
    map(dual srcf, dual trgf, new HashTable from toList apply(1..d,i->i=>dual(dualMaps#(i%d))),Degree => -degf)
    )

dShift =  method(Options => {RootOfUnity => true});
dShift(ZZ, ZZdFactorization, RingElement) := ZZdFactorization => opts -> (s,F,t) -> (
    if not(opts.RootOfUnity) then return dShift(s,F,getSymbol "t");
    d := period F;
    diffs := values (F.dd.map);
    ZZdfactorization toList apply(0..#diffs-1,i->t^s*diffs#((i+s)%d))
    )

dShift(ZZ, ZZdFactorization,Symbol) := ZZdFactorization => opts -> (s,F,t) -> (
    S := adjoinRoot(period F,ring F,t);
    dShift(s,F**S,(S_*)_0)
    )

--shift of a matrix factorization
ZZdFactorization Array := ZZdFactorization => (C, L) -> (
    if #L != 1 or not instance(L#0,ZZ) then error "expected an integer shift";
    if period C == 2 then return ZZdfactorization {(-1)^(L#0)*C.dd_(L#0+1),(-1)^(L#0)*C.dd_(L#0)};
    if (ring C).?rootOfUnity then return dShift(L#0,C,(ring C).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

ZZdFactorizationMap Array := ZZdFactorizationMap => (f, L) -> (
    p := period source f;
    maps := hashTable for k in keys f.map list (if (k - L#0)%p==0 then p else (k-L#0)%p) => f.map#k;
    result := map((target f)[L#0], (source f)[L#0], maps, Degree=> degree f);
    if isCommutativeCached f then result.cache.isCommutative = true;
    result
    )

isCommutative ZZdFactorizationMap := Boolean => f -> (
    if debugLevel == 0 and f.cache.?isCommutative then 
       return f.cache.isCommutative;
    C := source f;
    D := target f;
    deg := degree f;
    p := C.period;
    for i from 0 to p do (
            if not (dd^D_(i+deg) * f_i == (-1)^deg * (f_(i-1) * dd^C_i))
            then (
                if debugLevel > 0 then (
                    << "-- block " << (i,i-1) << " fails to commute" << endl;
                    );
                f.cache.isCommutative = false;
                return false;
                )
        );
    f.cache.isCommutative = true;
    true
    )


ZZdFactorizationMap | ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> (
    if target f =!= target g then error "expected targets to be the same";
    deg := degree f;
    p := period source f;
    if deg =!= degree g then error "expected maps with the same degree";
    result := map(target f, source f ++ source g, hashTable for i from 1 to p list i => f_i|g_i, Degree=>deg);
    if f.cache.?isCommutative and g.cache.?isCommutative then (
      result.cache.isCommutative = f.cache.isCommutative and g.cache.isCommutative;
      );
    result
    )

ZZdFactorizationMap || ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> (
    if source f =!= source g then error "expected sources to be the same";
    p := period source f;
    deg := degree f;
    if deg =!= degree g then error "expected maps with the same degree";
    result := map(target f ++ target g, source f, hashTable for i from 1 to p list i => f_i||g_i , Degree=>deg);
    if f.cache.?isCommutative and g.cache.?isCommutative then (
      result.cache.isCommutative = f.cache.isCommutative and g.cache.isCommutative;
      );
    result
    )

    

--this gives a trivial d-fold factorization of a monomial
monomialMF = method()
monomialMF(RingElement) := f -> (if not(#(terms f)==1) then error "Expected ring element to be monomial";
    Lk := toList factor(f);
    theDiffs := (flatten(Lk/(i->toList(i#1:i#0))))/(j->matrix{{j}});
    if first degree f < #theDiffs then theDiffs = {(last theDiffs)*first(theDiffs)}|(theDiffs_{1..#theDiffs-2});
    ZZdfactorization theDiffs
    )

--constructs a Koszul d-fold factorization of a homogeneous element of degree d
linearMF = method();
linearMF(RingElement) := ZZdFactorization => (f) -> (
    if not isHomogeneous f then error "Expected homogeneous element";
    if not((ring f).?rootOfUnity) and not first degree(f)==2 then error "Must adjoint dth root of unity if degree > 2";
    L := (terms f)/monomialMF;
    if first degree f == 2 then return tensor(L);
    --if not(opts.RootOfUnity) then return dTensor(L,getSymbol "t",opts);
    dTensor(L,(ring f).rootOfUnity)
    )

linearMF(RingElement,RingElement) := ZZdFactorization => (f,t) -> (
    if not isHomogeneous f then error "Expected homogeneous element";
    L := (terms f)/monomialMF;
    dTensor(L,getSymbol "t")
    )

linearMF(RingElement,Symbol) := ZZdFactorization =>  (f,t) -> (
     if not isHomogeneous f then error "Expected homogeneous element";
    L := (terms f)/monomialMF;
    dTensor(L,t)
    )

--generates a linear d-fold factorization of a random degree d polynomial
randomLinearMF = method();
randomLinearMF(ZZ,Ring) := (d,Q) -> (
    f := random(d,Q);
    linearMF(f)
    )

randomLinearMF(ZZ,Ring,RingElement) := (d,Q,t) -> (
    f := random(d,Q);
    linearMF(f,t)
    )

randomLinearMF(ZZ,Ring,Symbol) := (d,Q,t) -> (
    f := random(d,Q);
    linearMF(f,t)
    )

support(ZZdFactorization) := List => C -> (p := period C;
    for i from 0 to p-1 list if not(C_i == 0) then i else continue
    )

basis(ZZ, ZZdFactorization) := ZZdFactorization => opts -> (d, C) -> (
    p := period C;
    L := support C;
    if #L == 0 then return ZZdfactorization((ring C)^0, p);
    if #L == 1 then return ZZdfactorization(source basis(d, C_(L_0), opts), p, Base => L_0);
    ZZdfactorization for i from 1 to p list basis(d, C.dd_i, opts)
    )

basis(List, ZZdFactorization) := ZZdFactorization => opts -> (d, C) -> (
    p := period C;
    L := support C;
    if #L == 0 then return ZZdfactorization((ring C)^0, p);
    if #L == 1 then return ZZdfactorization(source basis(d, C_(L_0), opts), p, Base => L_0);
    ZZdfactorization for i from 1 to p list basis(d, C.dd_i, opts)
    )



truncate(ZZ, ZZdFactorization) := ZZdFactorization => {} >> opts -> (d, C) -> (
    p := period C;
    L := support C;
    if #L == 0 then return ZZdfactorization((ring C)^0, p);
    if #L == 1 then return ZZdfactorization(truncate(d, C_(L_0), opts), p, Base => L_0);
    ZZdfactorization for i from 1 to p list truncate(d, C.dd_i, opts)
    )

truncate(List, ZZdFactorization) := ZZdFactorization => {} >> opts -> (d, C) -> (
    p := period C;
    L := support C;
    if #L == 0 then return ZZdfactorization((ring C)^0, p);
    if #L == 1 then return ZZdfactorization(truncate(d, C_(L_0), opts), p, Base => L_0);
    ZZdfactorization for i from 1 to p list truncate(d, C.dd_i, opts)
    )

--constructs a matrix factorization from a high syzygy of a random module,
--where b is a degree bound on the generators and m and n are bounds controlling the size of the presentation of the module 
randomTailMF = method(Options => {WellDefined => true});
randomTailMF(RingElement, ZZ, ZZ, ZZ) := ZZdFactorization => opts -> (f, m, n, b) -> (
    S:= ring(f)/ideal(f);
    Lm := for i to m-1 list random(1,b);
--    Ln := for i to n-1 list random(-b,b);    
    M := prune trim cokernel random(S^Lm, S^n);
    F := tailMF(M);
    if not(opts.WellDefined) then return F;
    rk := rank F_0;
    if not all(values F.module, i -> rank i == rk) then randomTailMF(f, m, n, b) else F
)

randomTailMF(RingElement, ZZ, ZZ) := ZZdFactorization => opts -> (f, m, n) -> randomTailMF(f, m, n, random(1,10), opts)

randomTailMF(RingElement) := ZZdFactorization => opts -> f -> randomTailMF(f, a := random(1,10), random(a+2, 10+a), random(1,10), opts)

homology(ZZ,ZZdFactorization) := Module => opts -> (i,F) -> (
    if F.period==2 then (
    --We do not check if the differentials compose to 0 since this can be expensive,
    --it is the user's responsibility to check that the input is well-defined
        return (kernel F.dd_i)/(image F.dd_(i+1))
	);
    if not(F.period==2) then (
	return ZZdfactorization for j from 0 to F.period-1 list homology(j,i,F)
	);
    )

homology(ZZdFactorization) := ZZdFactorization => opts -> F -> (
    if not(F.period==2) then error "expected 2-periodic factorization";
    ZZdfactorization for i from 0 to F.period-1 list homology(i,F)
    )

--different types of homology when the period is larger than 2
homology(ZZ,ZZ,ZZdFactorization) := Module => opts -> (i,j,F) -> (p := period F;
    if F.period==2 then error "expected factorization of period at least 3";
    Dn1 := product reverse for l from 0 to j-1 list F.dd_(i-l);
    Dn2 := product for l from 0 to p-j-1 list F.dd_(i+l+1);
    (ker Dn1)/(image Dn1)
    )

hhh = method();
hhh(ZZ,ZZdFactorization) := ZZ => (i,F) -> (p := period F;
    if F.period==2 then return length prune HH_i F;
    if not(F.period==2) then (
	return sum for j from 0 to p-1 list length homology(j,i,F)
	);
    )

hhh(ZZdFactorization) := ZZ => F -> (p := period F;
    if not(p==2) then error "expected 2-periodic factorization";
    hhh(0,F) + hhh(1,F)
    )

--euler characteristic of a ZZdfactorization. Caveat: in order to be well-defined, the input should be a complex
euler(ZZdFactorization) := ZZ => F -> (p := period F;
    if not(p==2) then error "expected 2-periodic factorization";
    hhh(0,F) - hhh(1,F)
    )

--eulerChi(ZZ,ZZdFactorization) := ZZ => (i,F) -> (p := period F;
--    if p==2 then error "expected factorization of period at least 3";
    

forceCx = L -> (n := length L;
    Ln := new MutableList from {};
    for i from 0 to n-2 do Ln#(#Ln) = map(source L_i,source L_(i+1),L_(i+1));
    complex ({L_0}|toList(Ln))
    )

--turn a factorization into a complex, where the list specifies the endpoint homological degrees
unfold = method();
unfold(ZZdFactorization,List) := Complex => (F,L) -> (
    a := L_0;
    b := L_1;
    if not(length L ==2) then error "expected list of length 2 specifying endpoints of homological degrees";
    (forceCx for i from 1 to b-a list F.dd_i)[-a]
    )

unfold(ZZdFactorization,ZZ) := Complex => (F,d) -> unfold(F,{0,d})

unfold(ZZdFactorizationMap, List) := ComplexMap => (f, L) -> (
    map(unfold(target f, L), unfold(source f, L), i -> f_i)
    )

unfold(ZZdFactorizationMap, ZZ) := ComplexMap => (f, d) -> unfold(f, {0,d})
    
--checks if the differentials of a factorization compose to 0
isZZdComplex = method();
isZZdComplex(ZZdFactorization) := F -> (p := period F;
    (F.dd)^p==0
    )


permute = method()
permute(ZZdFactorization,ZZ) := (X,k) -> (
    Y := for i from 1 to X.period list(
    (dd^X)_(i+k)
    );
    ZZdfactorization Y
    )

--converts a d-fold factorization into a d-1-fold factorization by composing
--the kth differential with the k+1 differential
collapseMF = method()
collapseMF(ZZdFactorization , ZZ) := (X,k) -> (
    if X.period == 2 then error "expected factorization of length at least 3";
    s := permute(X,k-1);
    L := for i from 3 to X.period list(
    (dd^s)_i
    );
    x := ZZdfactorization({(dd^s)_1*(dd^s)_2}|L);
    permute(x, X.period - k)
    )

--converts a d-fold factorization into a 2-fold factorization
--by composing the maps in the specified positions
fullCollapse = method()
fullCollapse(ZZdFactorization,ZZ,ZZ) := ZZdFactorization => (X,n,r) -> (
    if X.period == 2 then error "expected factorization of length at least 3";
    x := product(apply(r, i->(dd^X)_(n+i)));
    y := product(apply(X.period-r, i->(dd^X)_(n+i+r)));
    ZZdfactorization{x,y}
    )

--constructs the trivial matrix factorization of an element f, which is just
--the element f with all other differentials being the identity
trivialMF = method();
trivialMF(ZZ,ZZ,Module,RingElement) := ZZdFactorization => (d,i,M,f) -> (
    ZZdfactorization for j from 1 to d list (
	if j==i then f*id_M else id_M
	) 
    )

trivialMF(ZZ, Module, RingElement) := ZZdFactorization => (d, M, f) -> trivialMF(d,1,M,f)

trivialMF(Module, RingElement) := ZZdFactorization => (M,f) -> trivialMF(2,1,M,f)

trivialMF(ZZ,ZZ,Module,ZZ) := ZZdFactorization => (d,i,M,f) -> (
    ZZdfactorization for j from 1 to d list (
	if j==i then f*id_M else id_M
	) 
    )


projectionHelper = method();
projectionHelper(ZZdFactorization) := X -> (
    L := for j to X.period-1 list(
    for i to X.period-1 list(
    product(apply(i, i -> (dd^X)_(i+j+1)))
    )
    );
    cycles := for j to X.period-1 list(
    for i to X.period-1 list((i-j)%X.period)
    );
    projmap := for i to X.period-1 list(
    matrix{(L_i)_(cycles_i)}
    )
)

injectionHelper = method()
injectionHelper(ZZdFactorization) := X -> (
    L := for j to X.period-1 list(
    for i to X.period-1 list(
    product(reverse(apply(i,l -> (dd^X)_(X.period+j-l))))
    )
    );
    cycles := for j to X.period-1 list(
    for i to X.period-1 list((X.period-i+j)%X.period)
    );
    inclmap := for i to X.period-1 list(
    matrix pack(1,(L_i)_(cycles_i))
    )
    )


--given a d-fold factorization, this outputs the polynomial f such that
--the dth power of the differentials is equal to f*id. The element f is typically
--referred to as the potential of the factorization. If there is no such element, an error is returned.
potential = method();
potential(ZZdFactorization) := F -> (
    L := isdFactorization F;
    if L_0 then return L_1 else error "not well-defined factorization";
    )

coverHelper = method();
coverHelper(ZZdFactorization) := F -> (
    f := potential F;
    p := period F;
    directSum for i from 1 to p list trivialMF(p,i,F_(i-1),f)
    )

coverHelper(ZZdFactorizationMap) := phi -> (
    p := period phi;
    L := toList (p:directSum for i to p-1 list phi_i);
    map(coverHelper target phi, coverHelper source phi,L)
    )

--TODO: for factorization map

--constructs the projective cover of a d-fold matrix factorization
projectiveCover = method();
projectiveCover(ZZdFactorization) := ZZdFactorizationMap => F -> (
    P := coverHelper F;
    map(F,P,projectionHelper(F))
    )

--constructs the injective hull of a d-fold matrix factorization
injectiveCover = method();
injectiveCover(ZZdFactorization) := ZZdFactorizationMap =>  F -> (
    P := permute(coverHelper F,1);
    map(P,F,injectionHelper F)
    )

--given a d-fold factorization F, this outputs the ith suspension of F. When the period
--of f is 2, this is the same as the shift functor, but for d > 2 the suspension is
--distinct from the shift (in particular, the ambient ring does not need a root of unity
--for the suspension to be defined)
suspension = method();
suspension(ZZdFactorization) := ZZdFactorization =>  F -> (
    prune coker injectiveCover F
    )

suspension(ZZ,ZZdFactorization) := ZZdFactorization => (i,F) -> (
    P := F;
    for i from 1 to i do P = suspension(P);
    P
    )

suspension(ZZdFactorizationMap) := phi -> (
    F := source phi;
    Fn := ker projectiveCover F;
    G := target phi;
    Gn := ker projectiveCover G;
    inducedMap(Gn,Fn,coverHelper phi)
    )

--TODO: this command is not outputting a well-defined map; need to fix
coneHelper = method();
coneHelper(ZZdFactorizationMap) := phi -> (
    p := period phi;
    F := source phi;
    G := target phi;
    L := for i from 1 to p list (
	matrix{ for j from 0 to p-1 list if j==i-1 then phi_j else map(G_(i-1),F_j,0)}
	);
    L = L_({p-1}|toList(0..p-2));
    map(G,coverHelper F,L, Degree => -1)
    ) 



-*cone(ZZdFactorizationMap) := phi -> (
    p := period phi;
    F := source phi;
    G := target phi;
    *-
    
--KELLER: isCommutative needs an option for "RootOfUnity" 
-*
end--


restart
needs "MF_functions.m2"

S = QQ[x,y,z,u,v,w]
P = ZZdfactorization {x,y,z,u,v,w}
F = ZZdfactorization{matrix{{x,0},{0,x}}, matrix{{y,0},{0,y}}, matrix{{z,0},{0,z}}}
G = ZZdfactorization{matrix{{v,x},{0,v}}, matrix{{v,x},{x,v}}, matrix{{w,0},{0,w}}}

isdFactorization F --true
isdFactorization G --false

--put in check for tensorMF
Q = adjoinRoot(F.period, S)
T = dTensor(F,G,t)
T.period
dd^T
(dd^T)_0*(dd^T)_1*(dd^T)_2
isdFactorization T --false; do we want it false?


---more test
R = QQ[x,y,z,w]
K = koszulMF({x^2*y^2,z^2*w^2, x*y, y^10, x*y, z^100})
KMF = koszulMF({x,x,y,y,z,z})
isdFactorization KMF

----test
R = QQ[a,b,c,d]
K = koszulMFf({a,b,c,d^2}, a^5*d + d^100 + a*b*c)
isMatrixFactorization K
dd^K
I = ideal {a,b,c,d^2}
f = a^5*d + d^100 + a*b*c
koszulMFf(I, f)

M = f //gens(I)
N = transpose (gens I) | M
E = entries N
F = select(E, i -> not(i#1 == 0))
flatten F

K = koszulMFf({a,b,c,d}, a^3 + b^3 +c^3 +d^3)
dd^K

--test with larger tensor
restart
needs "MF_functions.m2"
Q=QQ[x_1..x_3];
F = ZZdfactorization {matrix{{x_1}},matrix{{x_2}},matrix{{x_3}}}; -- defining two trivial factorizations
G = ZZdfactorization {matrix{{x_2}},matrix{{x_2}},matrix{{x_3}}};
S = adjoinRoot(3,Q)
peek S
T = dTensor(F**S,G**S,t)
T.dd
F.dd
(dd^T_1)*(dd^T_2)*(dd^T_3)

R = QQ[x,y,z,u,v,w]
Q = adjoinRoot(4,R)
koszulMF{{x,y,z},{u,v,w},{x^10*y, z,w},{u^2,v,w}}

koszulMF = method()
koszulMF(List, RingElement) := (L, omega) -> (
    n := #(L#0);
    d := #L;
    Z := for i to n-1 list ZZdfactorization for j to d-1 list (L#j)#i;
    T := Z#0;
    for i from 1 to n-1 do T = dTensor(T,Z#i, omega);
    T
)

K = koszulMF({{x,y,z},{u,v,w},{x^10*y, z,w},{u^2,v,w}}, t)

dd^K

isdFactorization K

koszulMFf(List, RingElement, RingElement) := (L,f, omega) -> (
    I := ideal L;
    M := f //gens(I^2);
    N := transpose (gens I) | M;     
    E  := entries N;
    F := select(E, i -> not(i#1 == 0)); 
    A := flatten F;
    koszulMF(A)
)

--In: List L generating an ideal I such that polynomial f lies in I^{d-1}, where d is the desired period of factorization
--In: root of unity omega and integer e=d-1 such that omega^(e+1)=1
--Out: factorization of f with period d
--omega should be an (e+1) root of unity!
koszulMFf(List, RingElement, ZZ, RingElement) := (L,f, e, omega) -> (
    if not(omega^(e+1) == sub(1,ring omega)) then error "ring element must be a d^th root of unity where d is the third input";
    G := matrix {L};
    Gd := multiSubsets(flatten entries G, e);
    prods := for i to #Gd-1 list product(Gd_i);
    M := f //matrix{prods};
    N := for i to #Gd-1 list flatten {Gd#i, (entries M)#i};
    F := select(N, i -> not(last i == 0));
    koszulMF(entries transpose matrix F, omega)
	)
	 
S = QQ[x,y,z,w]
Q = adjoinRoot(5, S)
K = koszulMFf({x,y,z,w}, x^5+y^5+w^5, 4, t)
dd^K
isdFactorization K*-

