toBinomial = (b,R) -> (
     top := 1_R; bottom := 1_R;
     scan(#b, i -> if b_i > 0 then top = top * R_i^(b_i)
          else if b_i < 0 then bottom = bottom * R_i^(-b_i));
     top - bottom); 
toricIdeal = (A) -> (
    n := #(A_0);  
    R = QQ[vars(0..n-1),Degrees=>transpose A,MonomialSize=>16]; 
    B := transpose LLL syz matrix A;
    J := ideal apply(entries B, b -> toBinomial(b,R));
    scan(gens ring J, f -> J = saturate(J,f));
    J
    ); 
localCoherentEquations = (IA,w) -> (
     -- IA is the toric ideal of A living in a ring equipped
     -- with weight order w, if we are computing the local 
     -- equations about the initial ideal of IA w.r.t. w.
     R := ring IA;
     M := ideal leadTerm IA;
     S := first entries ((gens M) % IA);
     << "ideal M" << endl;
     for f in M_* do print toString f;
     << "ideal S" << endl;
     for f in (ideal S)_* do print toString f;
     -- Make the universal family J in a new ring.
     nv := numgens R; n := numgens M;
     T = (coefficientRing R)[generators R, z_1 .. z_n, 
                             Weights => flatten splice{w, n:0},
                             MonomialSize=>16];
     M = substitute(generators M,T);
     S = apply(S, s -> substitute(s,T));
     J = ideal apply(n, i -> 
               M_(0,i) - T_(nv + i) * S_i);
     << "ideal J" << endl;
     for f in J_* do print toString f;
     -- Find the ideal Ihilb of local equations about M:
     spairs := (gens J) * (syz M);
     globM = M;
     globSP = spairs;
     << "ideal spairs" << endl;
     for f in (ideal spairs)_* do print toString f;
     g := forceGB gens J;
     B = (coefficientRing R)[z_1 .. z_n,MonomialSize=>16];
     Fones := map(B,T, matrix(B,{splice {nv:1}}) | vars B);
     Ihilb := ideal Fones (spairs % g);
     Ihilb
     );

testIt = (IA,w) -> (
     -- IA is the toric ideal of A living in a ring equipped
     -- with weight order w, if we are computing the local 
     -- equations about the initial ideal of IA w.r.t. w.
     R := ring IA;
     M := ideal leadTerm IA;
     S := first entries ((gens M) % IA);
     -- Make the universal family J in a new ring.
     n := numgens M;
     T = (coefficientRing R)[generators R, z_1 .. z_n, 
                             Weights => flatten splice{w, n:0},
                             MonomialSize=>16];
     M = substitute(generators M,T);
     Z = syz M; -- this is what changes from machine to machine.
     for i from 0 to numColumns Z - 1 do (
         m := Z_{i};
         L := flatten entries m;
         p := positions(L, f -> f != 0);
         << p << "  " << toString ideal compress transpose m << endl;
         )
     )
testIt2 = (IA,w) -> (
     -- IA is the toric ideal of A living in a ring equipped
     -- with weight order w, if we are computing the local 
     -- equations about the initial ideal of IA w.r.t. w.
     R := ring IA;
     M := ideal leadTerm IA;
     S := first entries ((gens M) % IA);
     -- Make the universal family J in a new ring.
     n := numgens M;
     T = (coefficientRing R)[generators R, z_1 .. z_n, 
                             Weights => flatten splice{w, n:0},
                             MonomialSize=>16];
     M = substitute(generators M,T);
     gbTrace=3;
     syz M;
     )
testIt3 = (IA,w) -> (
     -- IA is the toric ideal of A living in a ring equipped
     -- with weight order w, if we are computing the local 
     -- equations about the initial ideal of IA w.r.t. w.
     R := ring IA;
     M := ideal leadTerm IA;
     T = (coefficientRing R)[generators R, 
                             Weights => w,
                             MonomialSize=>16];
     M = substitute(generators M,T);
     Z = syz M;
     for i from 0 to numColumns Z - 1 do (
         m := Z_{i};
         L := flatten entries m;
         p := positions(L, f -> f != 0);
         << p << "  " << toString ideal compress transpose m << endl;
         )
     )


end

restart
load "test-one-that-changes.m2"

A = {{1,1,1,1,1,1,1},{0,6,7,5,8,4,3},{3,7,2,0,7,6,1},
   {6,5,2,6,5,0,0}};
IA = toricIdeal A
Y = QQ[a..g, MonomialSize => 16,
           Weights => (w = {0,0,276,220,0,0,215}),
           Degrees =>transpose A];
IA = substitute(IA,Y);
testIt3(IA,w)

M = ideal leadTerm IA
testIt(IA,w)

Z = syz gens M;
for i from 0 to numColumns Z - 1 do (
    m := Z_{i};
    L := flatten entries m;
    p := positions(L, f -> f != 0);
    << p << "  " << toString ideal compress transpose m << endl;
    )

JM = localCoherentEquations(IA,w)
position(JM_*, f -> f == z_4*z_6-z_12)
position(JM_*, f -> f == z_3*z_5-z_12)

for f in (ideal globSP)_* do print toString f

