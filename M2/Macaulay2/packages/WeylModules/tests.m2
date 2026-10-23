TEST ///
-- Type A_2 (= cartanA 3 = sl_3) basic structure constants.
A := cartanA 3
N := chevalleyConstants A
-- The pair (alpha_1, alpha_2) should give a non-zero structure constant
-- (alpha_1 + alpha_2 is a root).
assert(N#?({1,0}, {0,1}))
-- Magnitude must be 1 for simply-laced types.
assert(abs(N#({1,0}, {0,1})) == 1)
-- Antisymmetry.
assert(N#({1,0}, {0,1}) == -N#({0,1}, {1,0}))
-- Negative-negative: by Chevalley convention, N(-alpha, -beta) = -N(alpha, beta).
assert(N#({-1,0}, {0,-1}) == -N#({1,0}, {0,1}))
///

TEST ///
-- Type A_1 = sl_2: only one positive root, so the structure-constants
-- table is empty and Jacobi is vacuous.
A := cartanA 2
N := chevalleyConstants A
assert(#(keys N) == 0)
assert(verifyJacobi A)
///

TEST ///
-- Type A_2 Jacobi.
A := cartanA 3
assert(verifyJacobi A)
///

TEST ///
-- Type A_3 has the first non-trivial Jacobi triple (alpha_1, alpha_2, alpha_3).
A := cartanA 4
assert(verifyJacobi A)
-- Sanity: N(alpha_1, alpha_3) = 0 because alpha_1 + alpha_3 is not a root.
assert(chevalleyN(A, {1,0,0}, {0,0,1}) == 0)
-- N(alpha_1, alpha_2 + alpha_3) is well-defined (alpha_1 + alpha_2 + alpha_3 is a root).
assert(chevalleyN(A, {1,0,0}, {0,1,1}) != 0)
-- Two decompositions of (1,1,1) must give compatible values via Jacobi:
--   N(alpha_2, alpha_3) * N(alpha_1, alpha_2+alpha_3)
-- + N(alpha_1, alpha_2) * N(alpha_3, alpha_1+alpha_2) = 0
n23 := chevalleyN(A, {0,1,0}, {0,0,1})
n123 := chevalleyN(A, {1,0,0}, {0,1,1})
n12 := chevalleyN(A, {1,0,0}, {0,1,0})
n312 := chevalleyN(A, {0,0,1}, {1,1,0})
assert(n23 * n123 + n12 * n312 == 0)
///

TEST ///
-- Type A_4 Jacobi.
A := cartanA 5
assert(verifyJacobi A)
///

TEST ///
-- D_4: Jacobi must hold on all 24 roots
A := cartanD 4
assert(verifyJacobi A)
-- Number of pairs (alpha, beta) of roots with alpha+beta a root, in D_4.
N := chevalleyConstants A
-- Just check the table is non-empty and antisymmetric on a sample pair.
posR := flatten entries (cartanPositiveRoots A)^{0}    -- alpha_1
posS := flatten entries (cartanPositiveRoots A)^{1}    -- alpha_2
if N#?(posR, posS) then assert(N#(posR, posS) == -N#(posS, posR))
///

TEST ///
-- D_5: Jacobi on a larger ADE type
A := cartanD 5
assert(verifyJacobi A)
///

TEST ///
-- E_6: Jacobi on the smallest exceptional simply-laced type
A := cartanE 6
assert(verifyJacobi A)
///

TEST ///
-- isSimplyLaced (cartanA n is sl_n = type A_{n-1}, so n >= 2)
assert(isSimplyLaced cartanA 2)
assert(isSimplyLaced cartanA 6)
assert(isSimplyLaced cartanD 4)
assert(isSimplyLaced cartanD 6)
assert(isSimplyLaced cartanE 6)
assert(isSimplyLaced cartanE 7)
assert(isSimplyLaced cartanE 8)
assert(not isSimplyLaced cartanB 2)
assert(not isSimplyLaced cartanB 5)
assert(not isSimplyLaced cartanC 3)
assert(not isSimplyLaced cartanF4())
assert(not isSimplyLaced cartanG2())
///

TEST ///
-- chevalleyEpsilonMatrix is ADE-only; chevalleyConstants now supports
-- both ADE and non-ADE.
A := cartanB 2
errEps := try (chevalleyEpsilonMatrix A; false) else true
assert errEps
-- chevalleyConstants succeeds for non-ADE (dispatches via folding).
N := chevalleyConstants A;
assert(instance(N, HashTable))
assert(not instance(N, MutableHashTable))
assert(verifyJacobi A)
///

TEST ///
-- Cocycle identity on simple roots: eps(alpha_i, alpha_j) is the
-- (i,j) entry of the epsilon matrix.
A := cartanA 4                       -- type A_3, rank 3
M := chevalleyEpsilonMatrix A
N := chevalleyConstants A
-- For simple roots alpha_i, alpha_j with alpha_i + alpha_j a root,
-- N(alpha_i, alpha_j) = M_{i,j}.
assert(N#({1,0,0}, {0,1,0}) == M_(0,1))
assert(N#({0,1,0}, {0,0,1}) == M_(1,2))
///

TEST ///
-- Mixed-sign structure constants for A_2.
-- [e_{alpha_1+alpha_2}, f_{alpha_2}] should give an e_{alpha_1} term.
-- Specifically: N(alpha_1+alpha_2, -alpha_2) is the structure constant.
A := cartanA 3                       -- type A_2
n := chevalleyN(A, {1,1}, {0,-1})
-- alpha_1 + alpha_2 + (-alpha_2) = alpha_1, a positive root.  So n != 0.
assert(n != 0)
-- It should be plus or minus 1.
assert(abs n == 1)
///

----------------------------------------------------------------------
-- Layer 4 tests: WeylModuleZ
----------------------------------------------------------------------

TEST ///
-- pbwTuples basic enumeration
R = {{1,0}, {0,1}, {1,1}}
-- alpha_1 + alpha_2 has two decompositions in R: (0,0,1) [the (1,1) root]
-- and (1,1,0) [alpha_1 + alpha_2 from the simple roots].
T := pbwTuples(R, {1, 1})
assert(#T == 2)
-- 2 alpha_1: only one tuple (2, 0, 0).
assert(#(pbwTuples(R, {2, 0})) == 1)
-- empty drop: only the zero tuple.
assert(#(pbwTuples(R, {0, 0})) == 1)
///

TEST ///
-- A_2 V(omega_1) = sl_3 standard rep, dim 3.
A := cartanA 3
W := weylModuleZ(A, {1, 0})
assert(dim W == 3)
assert(# weights W == 3)
-- Highest weight has multiplicity 1.
assert((character W)#{1, 0} == 1)
-- Verma quotient relation: f_1^{(2)} v_lambda = 0 in V(lambda).
-- We check this indirectly: weight (1, 0) - 2 alpha_1 should not be a V-weight.
twoAlpha1Drop := for k from 0 to 1 list ({1, 0})#k - 2 * A_(0, k);   -- (1, 0) - 2*(2, -1) = (-3, 2)
assert(not member(twoAlpha1Drop, weights W))
///

TEST ///
-- A_2 V(2, 0) = Sym^2 std, dim 6.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
assert(dim W == 6)
-- Six weights, all with multiplicity 1 (Sym^2 of std rep).
ch := character W;
assert(all(values ch, x -> x == 1))
assert(# weights W == 6)
///

TEST ///
-- A_2 V(1, 1) = adjoint of sl_3, dim 8.
A := cartanA 3
W := weylModuleZ(A, {1, 1})
assert(dim W == 8)
-- The zero weight has multiplicity 2 (dim of Cartan in sl_3).
assert((character W)#{0, 0} == 2)
///

TEST ///
-- A_3 V(omega_1) standard rep, dim 4.
A := cartanA 4
W := weylModuleZ(A, {1, 0, 0})
assert(dim W == 4)
-- All four weights have multiplicity 1.
ch := character W;
assert(all(values ch, x -> x == 1))
-- f-actions form the chain v_lambda -> f_1 v_lambda -> f_2 f_1 v_lambda -> f_3 f_2 f_1 v_lambda
F1 := actionF(W, {-1, 1, 0}, 1);
F2 := actionF(W, {0, -1, 1}, 2);
F3 := actionF(W, {0, 0, -1}, 3);
assert(F1 =!= null and F2 =!= null and F3 =!= null)
assert(numrows F1 == 1 and numcols F1 == 1)
assert(abs(F1_(0, 0)) == 1)
///

TEST ///
-- A_3 V(0, 1, 0) = Lambda^2 std, dim 6.
A := cartanA 4
W := weylModuleZ(A, {0, 1, 0})
assert(dim W == 6)
///

TEST ///
-- D_4 V(omega_1) (vector rep), dim 8.
A := cartanD 4
W := weylModuleZ(A, {1, 0, 0, 0})
assert(dim W == 8)
-- All eight weights have multiplicity 1.
ch := character W;
assert(all(values ch, x -> x == 1))
///

TEST ///
-- A_3 V(2, 0, 0) = Sym^2 std for A_3, dim 10.
A := cartanA 4
W := weylModuleZ(A, {2, 0, 0})
assert(dim W == 10)
///

TEST ///
-- D_4 V(0, 0, 1, 0) (a spin rep), dim 8.
A := cartanD 4
W := weylModuleZ(A, {0, 0, 1, 0})
assert(dim W == 8)
///

TEST ///
-- Accessor sanity: highestWeight, ambientCartan, basisVecs, weightSpace.
A := cartanA 3
lambda := {1, 0}
W := weylModuleZ(A, lambda)
assert(highestWeight W == lambda)
assert(ambientCartan W === A)
assert(# basisVecs W == dim W)
-- Highest-weight space is 1-dim, and the unique tuple is the all-zeros vector.
ws := weightSpace(W, lambda)
assert(# ws == 1)
assert(all(ws#0, x -> x == 0))
///

TEST ///
-- actionH: scalar action of h_i, value = i-th Dynkin label.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
assert(actionH(W, 1, {2, 0}) == 2)
assert(actionH(W, 2, {2, 0}) == 0)
assert(actionH(W, 1, {-2, 2}) == -2)
///

TEST ///
-- actionE: at highest weight, e_i v_lambda = 0 (target not a V-weight).
A := cartanA 3
W := weylModuleZ(A, {1, 0})
assert(actionE(W, {1, 0}, 1) === null)
assert(actionE(W, {1, 0}, 2) === null)
-- At lower weights, e_i is non-trivial.
E1 := actionE(W, {-1, 1}, 1);
assert(E1 =!= null)
assert(numrows E1 == 1 and numcols E1 == 1)
assert(E1_(0, 0) == 1)        -- f_1 v_lambda has h_1 image 1.
///

TEST ///
-- [e_i, f_j] v = delta_{ij} h_i v on every basis vector of A_2 V(2, 0).
A := cartanA 3
W := weylModuleZ(A, {2, 0})
rO := numrows A
weightsList := weights W
for mu in weightsList do (
    nMu := (W#"weightMultAt")#mu;
    for i from 1 to rO do (
        for j from 1 to rO do (
            alphaJD := for k from 0 to rO - 1 list A_(k, j - 1);
            alphaID := for k from 0 to rO - 1 list A_(k, i - 1);
            muMA := for k from 0 to rO - 1 list mu#k - alphaJD#k;
            muPA := for k from 0 to rO - 1 list mu#k + alphaID#k;
            resultWeight := for k from 0 to rO - 1 list mu#k + alphaID#k - alphaJD#k;
            if not (W#"weightSet")#?resultWeight or (W#"weightMultAt")#resultWeight == 0 then continue;
            nRes := (W#"weightMultAt")#resultWeight;
            for col from 0 to nMu - 1 do (
                vQ := mutableMatrix(QQ, nMu, 1);
                vQ_(col, 0) = 1_QQ;
                vQ = matrix vQ;
                eIv := if (W#"weightSet")#?muPA and (W#"weightMultAt")#muPA > 0 then (
                    em := actionE(W, mu, i);
                    if em === null then null else (em ** QQ) * vQ
                ) else null;
                fjEIv := if eIv =!= null then (
                    fm := actionF(W, resultWeight, j);
                    if fm === null then null else (fm ** QQ) * eIv
                ) else null;
                fjV := if (W#"weightSet")#?muMA and (W#"weightMultAt")#muMA > 0 then (
                    fm := actionF(W, muMA, j);
                    if fm === null then null else (fm ** QQ) * vQ
                ) else null;
                eiFjV := if fjV =!= null then (
                    em := actionE(W, muMA, i);
                    if em === null then null else (em ** QQ) * fjV
                ) else null;
                lhs := mutableMatrix(QQ, nRes, 1);
                if eiFjV =!= null then for k from 0 to nRes - 1 do
                    lhs_(k, 0) = lhs_(k, 0) + eiFjV_(k, 0);
                if fjEIv =!= null then for k from 0 to nRes - 1 do
                    lhs_(k, 0) = lhs_(k, 0) - fjEIv_(k, 0);
                expected := mutableMatrix(QQ, nRes, 1);
                if i == j then expected_(col, 0) = mu#(i - 1);
                diff := matrix lhs - matrix expected;
                assert all(flatten entries diff, x -> x == 0);
            );
        );
    );
);
///

TEST ///
-- Same [e_i, f_j] = delta h_i check on A_3 V(0, 1, 0) (Lambda^2 std, dim 6).
A := cartanA 4
W := weylModuleZ(A, {0, 1, 0})
rO := numrows A
for mu in weights W do (
    nMu := (W#"weightMultAt")#mu;
    for i from 1 to rO do (
        for j from 1 to rO do (
            alphaJD := for k from 0 to rO - 1 list A_(k, j - 1);
            alphaID := for k from 0 to rO - 1 list A_(k, i - 1);
            muMA := for k from 0 to rO - 1 list mu#k - alphaJD#k;
            muPA := for k from 0 to rO - 1 list mu#k + alphaID#k;
            resultWeight := for k from 0 to rO - 1 list mu#k + alphaID#k - alphaJD#k;
            if not (W#"weightSet")#?resultWeight or (W#"weightMultAt")#resultWeight == 0 then continue;
            nRes := (W#"weightMultAt")#resultWeight;
            for col from 0 to nMu - 1 do (
                vQ := mutableMatrix(QQ, nMu, 1);
                vQ_(col, 0) = 1_QQ;
                vQ = matrix vQ;
                eIv := if (W#"weightSet")#?muPA and (W#"weightMultAt")#muPA > 0 then (
                    em := actionE(W, mu, i);
                    if em === null then null else (em ** QQ) * vQ
                ) else null;
                fjEIv := if eIv =!= null then (
                    fm := actionF(W, resultWeight, j);
                    if fm === null then null else (fm ** QQ) * eIv
                ) else null;
                fjV := if (W#"weightSet")#?muMA and (W#"weightMultAt")#muMA > 0 then (
                    fm := actionF(W, muMA, j);
                    if fm === null then null else (fm ** QQ) * vQ
                ) else null;
                eiFjV := if fjV =!= null then (
                    em := actionE(W, muMA, i);
                    if em === null then null else (em ** QQ) * fjV
                ) else null;
                lhs := mutableMatrix(QQ, nRes, 1);
                if eiFjV =!= null then for k from 0 to nRes - 1 do
                    lhs_(k, 0) = lhs_(k, 0) + eiFjV_(k, 0);
                if fjEIv =!= null then for k from 0 to nRes - 1 do
                    lhs_(k, 0) = lhs_(k, 0) - fjEIv_(k, 0);
                expected := mutableMatrix(QQ, nRes, 1);
                if i == j then expected_(col, 0) = mu#(i - 1);
                diff := matrix lhs - matrix expected;
                assert all(flatten entries diff, x -> x == 0);
            );
        );
    );
);
///

TEST ///
-- weylModuleZ now supports all finite Lie types (A/B/C/D/E/F/G).
-- Both f- and e-actions are built; downstream operations (maximalVectors,
-- simpleQuotient, simpleCharacter, socleWeyl, decompositionNumbers,
-- subWeylModule, quotientWeylModule) work for non-ADE just as for ADE.
A := cartanB 2
W := weylModuleZ(A, {1, 0})
assert(dim W == 5)         -- B_2 V(omega_1) is the 5-dim vector rep
-- Verify [e_i, f_i] = h_i commutator at every weight.
for mu in W#"weightsList" do (
    for i from 1 to numrows A do (
        alphaID := for k from 0 to numrows A - 1 list A_(k, i - 1);
        muMinus := for k from 0 to numrows A - 1 list mu#k - alphaID#k;
        nMu := W#"weightMultAt"#mu;
        if nMu == 0 then continue;
        eifi := if (W#"actF"#?(muMinus, i)) and (W#"actE"#?(muMinus, i))
                then matrix(QQ, applyTable(entries (W#"actE"#(muMinus, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actF"#(muMinus, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        fiei := if (W#"actE"#?(mu, i)) and (W#"actF"#?(mu, i))
                then matrix(QQ, applyTable(entries (W#"actF"#(mu, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actE"#(mu, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        assert(eifi - fiei == mu#(i - 1) * id_(QQ^nMu));
    );
);
///

TEST ///
-- B_2 V(omega_2) is the 4-dim spin rep.
A := cartanB 2
W := weylModuleZ(A, {0, 1})
assert(dim W == 4)
///

TEST ///
-- G_2 V(omega_1) is the 7-dim defining rep (chain-3 corrections).
A := cartanG2()
W := weylModuleZ(A, {1, 0})
assert(dim W == 7)
-- [e_i, f_i] = h_i at every weight.
for mu in W#"weightsList" do (
    for i from 1 to numrows A do (
        alphaID := for k from 0 to numrows A - 1 list A_(k, i - 1);
        muMinus := for k from 0 to numrows A - 1 list mu#k - alphaID#k;
        nMu := W#"weightMultAt"#mu;
        if nMu == 0 then continue;
        eifi := if (W#"actF"#?(muMinus, i)) and (W#"actE"#?(muMinus, i))
                then matrix(QQ, applyTable(entries (W#"actE"#(muMinus, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actF"#(muMinus, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        fiei := if (W#"actE"#?(mu, i)) and (W#"actF"#?(mu, i))
                then matrix(QQ, applyTable(entries (W#"actF"#(mu, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actE"#(mu, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        assert(eifi - fiei == mu#(i - 1) * id_(QQ^nMu));
    );
);
///

TEST ///
-- B_2 V(2 omega_1) (= 14-dim symmetric traceless rep).  Verify [e_i, f_i] = h_i.
A := cartanB 2
W := weylModuleZ(A, {2, 0})
assert(dim W == 14)
for mu in W#"weightsList" do (
    for i from 1 to numrows A do (
        alphaID := for k from 0 to numrows A - 1 list A_(k, i - 1);
        muMinus := for k from 0 to numrows A - 1 list mu#k - alphaID#k;
        nMu := W#"weightMultAt"#mu;
        if nMu == 0 then continue;
        eifi := if (W#"actF"#?(muMinus, i)) and (W#"actE"#?(muMinus, i))
                then matrix(QQ, applyTable(entries (W#"actE"#(muMinus, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actF"#(muMinus, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        fiei := if (W#"actE"#?(mu, i)) and (W#"actF"#?(mu, i))
                then matrix(QQ, applyTable(entries (W#"actF"#(mu, i)), x -> x_QQ)) * matrix(QQ, applyTable(entries (W#"actE"#(mu, i)), x -> x_QQ))
                else matrix(QQ, table(nMu, nMu, (a, b) -> 0_QQ));
        assert(eifi - fiei == mu#(i - 1) * id_(QQ^nMu));
    );
);
///

----------------------------------------------------------------------
-- Layer 2 non-ADE tests: Chevalley structure constants for B/C/F/G
----------------------------------------------------------------------

TEST ///
-- classifyFiniteCartan
assert(classifyFiniteCartan cartanA 3 === ("A", 2))
assert(classifyFiniteCartan cartanB 3 === ("B", 3))
assert(classifyFiniteCartan cartanC 4 === ("C", 4))
assert(classifyFiniteCartan cartanD 5 === ("D", 5))
assert(classifyFiniteCartan cartanE 6 === ("E", 6))
assert(classifyFiniteCartan cartanF4() === ("F", 4))
assert(classifyFiniteCartan cartanG2() === ("G", 2))
///

TEST ///
-- B_2 has 8 roots, 4 positive.  The root-vector Jacobi check must hold.
A := cartanB 2
N := chevalleyConstants A
assert(verifyJacobi A)
-- Chain depth: |N(alpha_2, alpha_1+alpha_2)| should be 2 (long string).
assert(abs(chevalleyN(A, {0,1}, {1,1})) == 2)
-- Antisymmetry on a sample pair.
assert(chevalleyN(A, {1,0}, {0,1}) == -chevalleyN(A, {0,1}, {1,0}))
-- N(-alpha, -beta) = -N(alpha, beta) (Chevalley convention).
assert(chevalleyN(A, {-1,0}, {0,-1}) == -chevalleyN(A, {1,0}, {0,1}))
///

TEST ///
-- B_3 Jacobi.
A := cartanB 3
assert(verifyJacobi A)
///

TEST ///
-- B_4 Jacobi.
A := cartanB 4
assert(verifyJacobi A)
///

TEST ///
-- C_3 Jacobi (= sp_6).
A := cartanC 3
assert(verifyJacobi A)
///

TEST ///
-- C_4 Jacobi.
A := cartanC 4
assert(verifyJacobi A)
///

TEST ///
-- F_4 Jacobi -- tests the non-trivial GAP-to-KacMoody permutation.
A := cartanF4()
assert(verifyJacobi A)
///

TEST ///
-- G_2 Jacobi -- tests the order-3 diagram fold.
A := cartanG2()
assert(verifyJacobi A)
-- G_2 has chain depths up to 3 (long-short ratio 3:1).
assert(abs(chevalleyN(A, {1,0}, {2,1})) == 3)
assert(abs(chevalleyN(A, {1,0}, {1,1})) == 2)
assert(abs(chevalleyN(A, {1,0}, {0,1})) == 1)
///

TEST ///
-- Root-string theorem for every stored structure constant in a
-- representative family of simply- and non-simply-laced types:
-- if beta-r alpha is the negative end of the alpha-string through beta,
-- then |N(alpha,beta)| = r+1.
cartans := {cartanA 4, cartanD 4, cartanB 3, cartanC 3, cartanF4(), cartanG2()};
negRoot := v -> for x in v list -x;
addRoot := (u, v) -> for i from 0 to #u - 1 list u#i + v#i;
for A in cartans do (
    R := cartanPositiveRoots A;
    pos := for i from 0 to numrows R - 1 list flatten entries R^{i};
    roots := pos | apply(pos, negRoot);
    N := chevalleyConstants A;
    for ab in keys N do (
        alpha := ab#0;
        beta := ab#1;
        assert(N#ab == -N#(beta, alpha));
        r := 0;
        probe := addRoot(beta, negRoot alpha);
        while member(probe, roots) do (
            r = r + 1;
            probe = addRoot(probe, negRoot alpha);
        );
        assert(abs(N#ab) == r + 1);
    );
    -- Jacobi relations involving [e_alpha,e_-alpha]=h_alpha, omitted by
    -- verifyJacobi because they contain a Cartan bracket.
    rankA := numrows A;
    for i from 0 to rankA - 1 do (
        alpha := for j from 0 to rankA - 1 list (if i == j then 1 else 0);
        negAlpha := negRoot alpha;
        for beta in roots do (
            if beta == alpha or beta == negAlpha then continue;
            betaMinusAlpha := addRoot(beta, negAlpha);
            betaPlusAlpha := addRoot(beta, alpha);
            pairing := sum for j from 0 to rankA - 1 list beta#j * A_(i, j);
            cartanJacobi :=
                chevalleyN(A, beta, negAlpha) * chevalleyN(A, alpha, betaMinusAlpha)
                + pairing
                + chevalleyN(A, alpha, beta) * chevalleyN(A, negAlpha, betaPlusAlpha);
            assert(cartanJacobi == 0);
        );
    );
);
///

TEST ///
-- For non-ADE, isSimplyLaced returns false; chevalleyEpsilonMatrix errors.
A := cartanB 3
assert(not isSimplyLaced A)
errored := try (chevalleyEpsilonMatrix A; false) else true
assert errored
-- But chevalleyConstants now succeeds (dispatches to nonADE branch).
assert(verifyJacobi A)
///

----------------------------------------------------------------------
-- maximalVectors tests (mod p, all finite types)
----------------------------------------------------------------------

TEST ///
-- A_1 V(omega_1) (= sl_2 standard rep, dim 2): irreducible mod any prime,
-- one maximal vector at the highest weight.
W := weylModuleZ(cartanA 2, {1})
mv := maximalVectors(W, 2)
assert(#mv == 1)
assert(mv#0#0 == {1})       -- weight is the highest
///

TEST ///
-- A_1 V(2) mod 2: composition factors L(0) and L(2), two maximal vectors.
W := weylModuleZ(cartanA 2, {2})
mv := maximalVectors(W, 2)
assert(#mv == 2)
-- weights: should be {2} (highest) and {0}.
weightsFound := for x in mv list x#0
assert(member({2}, weightsFound))
assert(member({0}, weightsFound))
///

TEST ///
-- A_1 V(2) in char 3 (NOT char 2): V(2) is irreducible (since 2 < 3),
-- so only one maximal vector.
W := weylModuleZ(cartanA 2, {2})
mv := maximalVectors(W, 3)
assert(#mv == 1)
assert(mv#0#0 == {2})
///

TEST ///
-- A_1 V(0): trivial rep, one maximal vector.
W := weylModuleZ(cartanA 2, {0})
assert(#(maximalVectors(W, 2)) == 1)
assert(#(maximalVectors(W, 3)) == 1)
assert(#(maximalVectors(W, 5)) == 1)
///

TEST ///
-- A_2 V(omega_1) mod 2 (and 3): standard rep is irreducible, one maximal vector.
W := weylModuleZ(cartanA 3, {1, 0})
assert(#(maximalVectors(W, 2)) == 1)
assert(#(maximalVectors(W, 3)) == 1)
assert(#(maximalVectors(W, 5)) == 1)
///

TEST ///
-- A_2 V(2, 0) mod 2: composition factors L(2, 0) (dim 3) and L(0, 1) (dim 3);
-- two maximal vectors, at weights (2, 0) and (0, 1).
W := weylModuleZ(cartanA 3, {2, 0})
mv := maximalVectors(W, 2)
assert(#mv == 2)
weightsFound := for x in mv list x#0
assert(member({2, 0}, weightsFound))
assert(member({0, 1}, weightsFound))
///

TEST ///
-- A_2 V(2, 0) is irreducible mod 3 (verified here by maximal vectors).
W := weylModuleZ(cartanA 3, {2, 0})
mv := maximalVectors(W, 3)
assert(#mv == 1)
assert(mv#0#0 == {2, 0})
///

TEST ///
-- A_2 V(3, 0) mod 2: composition factors L(3, 0) and L(0, 0).
W := weylModuleZ(cartanA 3, {3, 0})
mv := maximalVectors(W, 2)
assert(#mv == 2)
weightsFound := for x in mv list x#0
assert(member({3, 0}, weightsFound))
assert(member({0, 0}, weightsFound))
///

TEST ///
-- maximalVectors validates input: p must be prime.
W := weylModuleZ(cartanA 2, {1})
errored := try (maximalVectors(W, 4); false) else true
assert errored
///

----------------------------------------------------------------------
-- Phase A: SubWeylModuleFp tests
----------------------------------------------------------------------

TEST ///
-- A_2 V(omega_1) mod 2: irreducible, so SubWeylModule(v_lambda) = full V.
W := weylModuleZ(cartanA 3, {1, 0})
S := subWeylModule(W, 2, {1, 0}, {1})
assert(dim S == dim W)
assert(dim S == 3)
///

TEST ///
-- A_1 V(2) mod 2: the max vec at weight 0 generates a trivial submodule of dim 1.
W := weylModuleZ(cartanA 2, {2})
mvecs := maximalVectors(W, 2)
m0 := first select(mvecs, x -> x#0 == {0})
S := subWeylModule(W, 2, m0#0, m0#1)
assert(dim S == 1)
-- v_lambda generates all of V.
Sfull := subWeylModule(W, 2, {2}, {1})
assert(dim Sfull == 3)
///

TEST ///
-- A_2 V(2, 0) mod 2: max vec at (0, 1) generates the L(0, 1) = std-dual.
W := weylModuleZ(cartanA 3, {2, 0})
mvecs := maximalVectors(W, 2)
m01 := first select(mvecs, x -> x#0 == {0, 1})
S := subWeylModule(W, 2, m01#0, m01#1)
assert(dim S == 3)
-- Weights of S should be (0, 1), (1, -1), (-1, 0) (= weights of L(omega_2)).
ws := weights S
assert(member({0, 1}, ws))
assert(member({1, -1}, ws))
assert(member({-1, 0}, ws))
assert(#ws == 3)
///

TEST ///
-- A_2 V(3, 0) mod 2: max vec at (0, 0) generates the L(0, 0) = trivial.
W := weylModuleZ(cartanA 3, {3, 0})
mvecs := maximalVectors(W, 2)
m00 := first select(mvecs, x -> x#0 == {0, 0})
S := subWeylModule(W, 2, m00#0, m00#1)
assert(dim S == 1)
///

TEST ///
-- isWithin: generators are members; v_lambda is not (when generator is a non-top max vec).
W := weylModuleZ(cartanA 3, {2, 0})
S := subWeylModule(W, 2, {0, 1}, {1})
assert(isWithin(S, {0, 1}, {1}))
assert(not isWithin(S, {2, 0}, {1}))
-- Zero vectors are trivially in any submodule.
assert(isWithin(S, {2, 0}, {0}))
///

TEST ///
-- Multi-generator closure equals direct sum when generators are independent.
W := weylModuleZ(cartanA 2, {2})
mvecs := maximalVectors(W, 2)
mTop := first select(mvecs, x -> x#0 == {2})
mZero := first select(mvecs, x -> x#0 == {0})
STop := subWeylModule(W, 2, mTop#0, mTop#1)
SZero := subWeylModule(W, 2, mZero#0, mZero#1)
SBoth := subWeylModule(W, 2, {(mTop#0, mTop#1), (mZero#0, mZero#1)})
-- v_lambda generates all of V (dim 3); adding (0) generator doesn't add anything.
assert(dim STop == 3)
assert(dim SBoth == 3)
///

TEST ///
-- Extension: subWeylModule(S, mu, vec) should equal multi-gen closure.
W := weylModuleZ(cartanA 3, {2, 0})
mvecs := maximalVectors(W, 2)
m01 := first select(mvecs, x -> x#0 == {0, 1})
S := subWeylModule(W, 2, m01#0, m01#1)
-- Extending S by v_lambda gives all of V.
S2 := subWeylModule(S, {2, 0}, {1})
assert(dim S2 == dim W)
assert(dim S2 == 6)
///

----------------------------------------------------------------------
-- Phase B-D: quotientWeylModule, simpleQuotient, simpleCharacter, socleWeyl tests
----------------------------------------------------------------------

TEST ///
-- quotientWeylModule basic.
W := weylModuleZ(cartanA 3, {2, 0})
S := subWeylModule(W, 2, {0, 1}, {1})
Q := quotientWeylModule S
assert(dim Q == dim W - dim S)
assert(dim Q == 3)
-- Q is irreducible: maximalVectors should have just one entry.
mvecs := maximalVectors Q
assert(#mvecs == 1)
assert(mvecs#0#0 == {2, 0})
///

TEST ///
-- simpleQuotient: L(2, 0) mod 2 has dim 3.
W := weylModuleZ(cartanA 3, {2, 0})
Q := simpleQuotient(W, 2)
assert(dim Q == 3)
-- Irreducible.
W2 := weylModuleZ(cartanA 3, {1, 0})
Q2 := simpleQuotient(W2, 2)
assert(dim Q2 == dim W2)
///

TEST ///
-- simpleCharacter / simpleDimension: Steinberg tensor product.
A := cartanA 3
-- L(2, 0) mod 2: dim 3, weights = (2,0), (-2,2), (0,-2).
ch := simpleCharacter(A, 2, {2, 0})
assert(simpleDimension(A, 2, {2, 0}) == 3)
assert(ch#{2, 0} == 1)
assert(ch#{-2, 2} == 1)
assert(ch#{0, -2} == 1)
-- A_1 L(2) mod 2: dim 2.
A1 := cartanA 2
assert(simpleDimension(A1, 2, {2}) == 2)
-- L(0): trivial, dim 1.
assert(simpleDimension(A, 2, {0, 0}) == 1)
-- L(omega_1) for A_2 mod 2: dim 3 (irreducible).
assert(simpleDimension(A, 2, {1, 0}) == 3)
///

TEST ///
-- socleWeyl: A_2 V(2, 0) mod 2 has socle L(0, 1), dim 3.
W := weylModuleZ(cartanA 3, {2, 0})
soc := socleWeyl(W, 2)
assert(dim soc == 3)
-- The relevant maximal-vector spaces are one-dimensional, so this is the
-- full socle.  Since it is proper, V is not the direct sum of its two
-- composition factors; the characteristic-2 extension is nonsplit.
assert(dim soc < dim W)
-- Weights of socle = weights of L(omega_2) = std-dual.
ws := weights soc
assert(member({0, 1}, ws))
assert(member({1, -1}, ws))
assert(member({-1, 0}, ws))
///

TEST ///
-- The exported low-level PBW helper accepts a completed public WeylModuleZ.
-- It expands f_1 on the highest PBW monomial before quotient projection.
W := weylModuleZ(cartanA 3, {1, 0})
c := first weightSpace(W, highestWeight W)
out := applyChevalley(W, "f", 1, c, highestWeight W)
assert(out#?{0, 1, 0})
assert(out#{0, 1, 0} == 1)
assert(#(keys out) == 1)
///

TEST ///
-- Irreducible Weyl module: socle = whole module.
W := weylModuleZ(cartanA 3, {1, 0})    -- A_2 std rep, irreducible mod any p
soc := socleWeyl(W, 2)
assert(dim soc == dim W)
soc3 := socleWeyl(W, 3)
assert(dim soc3 == dim W)
///

TEST ///
-- decompositionNumbers: A_1 V(2) mod 2 has composition factors L(2) and L(0).
A1 := cartanA 2
W := weylModuleZ(A1, {2})
dn := decompositionNumbers(W, 2)
assert(#(keys dn) == 2)
assert(dn#{2} == 1)
assert(dn#{0} == 1)
-- Total dim check: sum of (mult of L(mu)) * (dim L(mu)) = dim V.
total := sum apply(keys dn, mu -> dn#mu * simpleDimension(A1, 2, mu))
assert(total == dim W)
///

TEST ///
-- decompositionNumbers: A_2 V(2, 0) mod 2 → L(2, 0) + L(0, 1).
A := cartanA 3
W := weylModuleZ(A, {2, 0})
dn := decompositionNumbers(W, 2)
assert(#(keys dn) == 2)
assert(dn#{2, 0} == 1)
assert(dn#{0, 1} == 1)
total := sum apply(keys dn, mu -> dn#mu * simpleDimension(A, 2, mu))
assert(total == dim W)
///

TEST ///
-- decompositionNumbers: A_2 V(2, 0) mod 3 is irreducible.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
dn := decompositionNumbers(W, 3)
assert(#(keys dn) == 1)
assert(dn#{2, 0} == 1)
///

TEST ///
-- decompositionNumbers: A_2 V(3, 0) mod 2 → L(3, 0) + L(0, 0).
A := cartanA 3
W := weylModuleZ(A, {3, 0})
dn := decompositionNumbers(W, 2)
assert(#(keys dn) == 2)
assert(dn#{3, 0} == 1)
assert(dn#{0, 0} == 1)
total := sum apply(keys dn, mu -> dn#mu * simpleDimension(A, 2, mu))
assert(total == dim W)
///

TEST ///
-- A_3 V(0, 1, 0) (Lambda^2 std) is irreducible mod 2.
A := cartanA 4
W := weylModuleZ(A, {0, 1, 0})
dn := decompositionNumbers(W, 2)
assert(#(keys dn) == 1)
assert(dn#{0, 1, 0} == 1)
///

----------------------------------------------------------------------
-- Phase F: socleSeries, gensSocleLayers, maximalSubmodule, ambiguousMaxVecs
----------------------------------------------------------------------

TEST ///
-- A_2 V(2, 0) mod 2: socle series has 2 layers.  Soc_1 = L(0, 1), full at layer 2.
W := weylModuleZ(cartanA 3, {2, 0})
ss := socleSeries(W, 2)
assert(length ss == 2)
assert(dim (ss_0) == 3)         -- L(0, 1) socle
assert(dim (ss_1) == 6)         -- full V
///

TEST ///
-- maximalSubmodule = kernel of simpleQuotient.
-- For A_2 V(2, 0) mod 2, max submod has dim 6 - 3 = 3.
W := weylModuleZ(cartanA 3, {2, 0})
M := maximalSubmodule(W, 2)
assert(dim M == 3)
-- Trivial (irreducible) case: max submod has dim 0.
W2 := weylModuleZ(cartanA 3, {1, 0})       -- A_2 V(omega_1) is irreducible
M2 := maximalSubmodule(W2, 2)
assert(dim M2 == 0)
///

TEST ///
-- isAmbiguous: V(omega_1) is irreducible, only one max vec.
W := weylModuleZ(cartanA 3, {1, 0})
assert(not isAmbiguous(W, 2))
assert(#(ambiguousMaxVecs(W, 2)) == 0)
-- A_2 V(2, 0) mod 2: 2 max vecs at distinct weights, NOT ambiguous.
W2 := weylModuleZ(cartanA 3, {2, 0})
assert(not isAmbiguous(W2, 2))
///

----------------------------------------------------------------------
-- Phase I: Schur algebra
----------------------------------------------------------------------

TEST ///
assert(compositionToWeight {1, 2, 0, 1} == {-1, 2, -1})
assert(weightToComposition(4, {-1, 2, -1}) == {1, 2, 0, 1})
assert(weightToComposition(8, {-1, 2, -1}) == {2, 3, 1, 2})
assert(weightToComposition(6, {-1, 2, -1}) === null)
///

TEST ///
assert(allPartitions 5 == {{5}, {4, 1}, {3, 2}, {3, 1, 1}, {2, 2, 1}, {2, 1, 1, 1}, {1, 1, 1, 1, 1}})
assert(boundedPartitions(5, 3, 2) == {{2, 1, 0, 0, 0}, {1, 1, 1, 0, 0}})
///

TEST ///
assert(conjugatePartition {4} == {1, 1, 1, 1})
assert(conjugatePartition {2, 1, 1, 1} == {4, 1})
assert(pRestrictedQ(2, {2, 1, 1}))
assert(pRestrictedQ(2, {1, 1, 1, 1}))
assert(not pRestrictedQ(2, {2, 0}))
assert(pRestrictedQ(3, {3, 1}))
assert(not pRestrictedQ(3, {4, 1}))
assert(pRegularQ(2, {3, 2}))
assert(pRegularQ(2, {4, 1}))
assert(not pRegularQ(2, {3, 1, 1}))    -- has 1, 1 repeat
///

TEST ///
-- Mullineux examples from Doty's docs.
assert(mullineux(2, {3}) == {3})
assert(mullineux(3, {3}) == {2, 1})
assert(mullineux(3, {2, 1}) == {3})
///

TEST ///
-- Structural checks for the Mullineux map: it preserves size and
-- p-regularity and is an involution.  In the semisimple range p > n it
-- agrees with conjugation.
for p in {2, 3, 5, 7} do (
    for n from 1 to 9 do (
        for mu in pRegularPartitions(p, n) do (
            mmu := mullineux(p, mu);
            assert(sum mmu == n);
            assert(pRegularQ(p, mmu));
            assert(mullineux(p, mmu) == mu);
        );
    );
);
for n from 1 to 9 do (
    for mu in allPartitions n do
        assert(mullineux(11, mu) == conjugatePartition mu);
);
///

TEST ///
-- Partition predicates reject lists outside their documented domain.
assert(try (conjugatePartition {1, 2}; false) else true);
assert(try (pRestrictedQ(2, {2, -1}); false) else true);
assert(try (pRegularQ(3, {1, 2}); false) else true);
///

TEST ///
-- schurAlgebraDecompositionMatrix S(4, 3) char 3 (Doty's example).
M := schurAlgebraDecompositionMatrix(3, 4, 3)
-- Rows and columns are in descending partition order: (3), (2,1), (1,1,1).
assert(M == matrix(ZZ, {{1, 1, 0}, {0, 1, 1}, {0, 0, 1}}))
///

TEST ///
-- symmetricGroupDecompositionMatrix S_4 char 2 (Doty's example).
M := symmetricGroupDecompositionMatrix(2, 4)
-- Rows: (4), (3,1), (2,2), (2,1,1), (1,1,1,1).
-- Columns: the 2-restricted partitions (2,1,1), (1,1,1,1).
assert(M == matrix(ZZ, {{0, 1}, {1, 1}, {1, 0}, {1, 1}, {0, 1}}))
///

TEST ///
-- The degree-one Schur and symmetric groups each have one trivial module.
assert(schurAlgebraDecompositionMatrix(2, 1, 1) == matrix(ZZ, {{1}}))
assert(symmetricGroupDecompositionMatrix(2, 1) == matrix(ZZ, {{1}}))
dn1 := symmetricGroupDecompositionNumbers(2, {1});
assert(#(keys dn1) == 1 and dn1#?{1} and dn1#{1} == 1)
///

----------------------------------------------------------------------
-- Phase J (partial): non-ADE Layer 4
----------------------------------------------------------------------

TEST ///
-- B_2 V(omega_1) and V(omega_2): vector rep dim 5 and spin rep dim 4.
W := weylModuleZ(cartanB 2, {1, 0})
assert(dim W == 5)
W2 := weylModuleZ(cartanB 2, {0, 1})
assert(dim W2 == 4)
///

TEST ///
-- B_3 V(omega_2) is the 21-dimensional adjoint representation.  Its
-- construction reaches the length-two PBW root-string correction.
W := weylModuleZ(cartanB 3, {0, 1, 0});
assert(dim W == 21);
assert(characterDim character W == 21);
///

TEST ///
-- C_3 V(omega_1) (= 6-dim defining rep of sp_6).  Tests C_n.
A := cartanC 3
W := weylModuleZ(A, {1, 0, 0})
assert(dim W == 6)
///

----------------------------------------------------------------------
-- Phase G: socleLengthTwoQuotient + twoFactorQuotientsContaining
----------------------------------------------------------------------

TEST ///
-- A_2 V(2, 0) mod 2 has socle length 2 already.
-- socleLengthTwoQuotient should give the full V (since length is exactly 2).
-- It will have layers L(0,1) (bottom) and L(2,0) (top), 6-dim total.
W := weylModuleZ(cartanA 3, {2, 0})
S0 := subWeylModule(W, 2, {})
Q := socleLengthTwoQuotient S0
assert(dim Q == dim W)         -- length is already 2
gl := gensSocleLayers Q
assert(#gl == 2)
assert(#(gl#0) == 1)           -- one socle generator
assert((gl#0#0)#0 == {0, 1})    -- L(0, 1) bottom
assert((gl#1#0)#0 == {2, 0})    -- L(2, 0) top
///

TEST ///
-- twoFactorQuotientsContaining for A_2 V(2, 0) mod 2.
-- V itself is a 2-factor quotient: L(0, 1) on the bottom and L(2, 0) on the top.
W := weylModuleZ(cartanA 3, {2, 0})
S0 := subWeylModule(W, 2, {})
tfqs := twoFactorQuotientsContaining S0
assert(#tfqs == 1)
assert(dim (tfqs#0) == dim W)
///

TEST ///
-- Doty's G_2 V(3,0), characteristic-2 two-factor example.  This reaches
-- the multi-socle branch of twoFactorQuotientsContaining and guards the
-- List/Matrix normalization of socle-layer generators.
A := cartanG2();
W := weylModuleZ(A, {3, 0});
amb := select(maximalVectors(W, 2), x -> x#0 == {2, 0});
assert(#amb == 2);
vsum := for k from 0 to #(amb#0#1) - 1 list amb#0#1#k + amb#1#1#k;
S := subWeylModule(W, 2, {2, 0}, vsum);
assert(dim S == 27);
Q48 := socleLengthTwoQuotient S;
assert(dim Q48 == 48);
layers48 := apply(gensSocleLayers Q48, L -> apply(L, g -> g#0));
assert(sort(layers48#0) == {{1, 0}, {2, 0}});
assert(layers48#1 == {{3, 0}});
qs := twoFactorQuotientsContaining S;
assert(#qs == 2);
assert(sort apply(qs, dim) == {42, 42});
bottoms := sort apply(qs, q -> (gensSocleLayers q)#0#0#0);
assert(bottoms == {{1, 0}, {2, 0}});
for q in qs do (
    layers := gensSocleLayers q;
    assert(#layers == 2 and #(layers#0) == 1 and #(layers#1) == 1);
    assert((layers#1#0)#0 == {3, 0});
);
///

TEST ///
-- socleSeries(SubWeylModuleFp) overload: empty submodule = same as (W, p).
W := weylModuleZ(cartanA 3, {2, 0})
S0 := subWeylModule(W, 2, {})
ss1 := socleSeries S0
ss2 := socleSeries(W, 2)
assert(#ss1 == #ss2)
for k from 0 to #ss1 - 1 do assert(dim (ss1#k) == dim (ss2#k))
///

TEST ///
-- Phase G works on non-ADE: B_2 V(omega_1) mod 2 has L(1,0) (4-dim) + L(0,0) (1-dim).
A := cartanB 2
W := weylModuleZ(A, {1, 0})
assert(dim W == 5)
Q := simpleQuotient(W, 2)
assert(dim Q == 4)            -- L(omega_1) is 4-dim mod 2
dn := decompositionNumbers(W, 2)
assert(dn#{1, 0} == 1)
assert(dn#{0, 0} == 1)
-- socleLengthTwoQuotient on the empty module: V already has socle length 2.
S0 := subWeylModule(W, 2, {})
QQ := socleLengthTwoQuotient S0
assert(dim QQ == dim W)
gl := gensSocleLayers QQ
assert(#gl == 2)
assert((gl#0#0)#0 == {0, 0})  -- L(0, 0) socle
assert((gl#1#0)#0 == {1, 0})  -- L(omega_1) top
///

TEST ///
-- Phase G on G_2 V(omega_1) mod 2: similar pattern.
A := cartanG2()
W := weylModuleZ(A, {1, 0})
assert(dim W == 7)
dn := decompositionNumbers(W, 2)
-- Decomposition factors must sum to dim 7.
total := sum apply(keys dn, mu -> dn#mu * simpleDimension(A, 2, mu))
assert(total == dim W)
///

----------------------------------------------------------------------
-- Character utilities and weight predicates
----------------------------------------------------------------------

TEST ///
-- characterDim, productCharacter, differenceCharacter, sortedCharacter
ch1 := new HashTable from {{1, 0} => 2, {-1, 0} => 1};
ch2 := new HashTable from {{0, 1} => 1};
assert(characterDim ch1 == 3);
prod := productCharacter(ch1, ch2);
assert(prod#{1, 1} == 2);
assert(prod#{-1, 1} == 1);
diff := differenceCharacter(ch1, new HashTable from {{1, 0} => 1});
assert(diff#{1, 0} == 1);
assert(diff#{-1, 0} == 1);
-- A non-character difference returns null.
assert(differenceCharacter(ch1, new HashTable from {{1, 0} => 5}) === null);
sorted := sortedCharacter ch1;
assert(sorted#0#0 == {1, 0});  -- highest weight first
///

TEST ///
-- isDominantWeight, isRestrictedWeight, highestPrimePower
assert(isDominantWeight {2, 0, 1});
assert(not isDominantWeight {1, -1});
assert(not isDominantWeight {1/2, 0});
assert(isRestrictedWeight(2, {0, 1}));
assert(not isRestrictedWeight(2, {2, 0}));    -- 2 not < 2
assert(not isRestrictedWeight(3, {1/2, 0}));
assert(try (isRestrictedWeight(4, {0, 1}); false) else true);
assert(highestPrimePower(2, 0) == 0);
assert(highestPrimePower(2, 1) == 1);
assert(highestPrimePower(2, 2) == 2);
assert(highestPrimePower(2, 7) == 3);
assert(highestPrimePower(3, 9) == 3);
assert(try (highestPrimePower(4, 7); false) else true);
assert(try (highestPrimePower(2, -1); false) else true);
///

TEST ///
-- dominantWeights / dominantWeightSpaces on a Weyl module.
W := weylModuleZ(cartanA 3, {2, 0});
dw := dominantWeights W;
assert(member({2, 0}, dw));
assert(member({0, 1}, dw));
assert(not member({-2, 2}, dw));
dws := dominantWeightSpaces W;
assert(#(keys dws) == #dw);
///

TEST ///
-- simpleTopFactorCharacter / Dim are aliases for simpleCharacter / Dim.
A := cartanA 3;
assert(simpleTopFactorCharacter(A, 2, {2, 0}) === simpleCharacter(A, 2, {2, 0}));
assert(simpleTopFactorDim(A, 2, {2, 0}) == simpleDimension(A, 2, {2, 0}));
///

TEST ///
-- socleSeriesAndPrims bundles socleSeries with gensSocleLayers.
W := weylModuleZ(cartanA 3, {2, 0});
(ss, gl) := socleSeriesAndPrims(W, 2);
assert(#ss == #gl);
assert(#ss == 2);
///

TEST ///
-- symmetricGroupDecompositionNumbers reproduces a labeled row of the matrix.
dn := symmetricGroupDecompositionNumbers(2, {3, 1});
assert(#(keys dn) == 2);
assert(dn#?{2, 1, 1} and dn#{2, 1, 1} == 1);
assert(dn#?{1, 1, 1, 1} and dn#{1, 1, 1, 1} == 1);
-- Trailing zeros are a presentation choice, not a different partition.
dnPadded := symmetricGroupDecompositionNumbers(2, {3, 1, 0, 0, 0});
assert(#(keys dnPadded) == #(keys dn));
assert(all(keys dn, nu -> dnPadded#?nu and dnPadded#nu == dn#nu));
///

TEST ///
-- highestWeight reports the actual support maximum of a submodule, and
-- returns null for the zero quotient.
W := weylModuleZ(cartanA 3, {2, 0});
mv := first select(maximalVectors(W, 2), x -> x#0 == {0, 1});
S := subWeylModule(W, 2, mv#0, mv#1);
assert(highestWeight S == {0, 1});
whole := subWeylModule(W, 2, {2, 0}, {1});
zeroQ := quotientWeylModule whole;
assert(dim zeroQ == 0 and highestWeight zeroQ === null);
///

TEST ///
-- Public socle-layer generators consistently use coefficient Lists.
W := weylModuleZ(cartanA 3, {2, 0});
layers := gensSocleLayers(W, 2);
assert(all(flatten layers, g -> instance(g#1, List)));
///

TEST ///
-- decomposeCharacter rejects a formal weight table that is not a
-- non-negative sum of simple characters.
A := cartanA 2;
badCharacter := new HashTable from {{1} => 1};
assert(try (decomposeCharacter(A, 2, badCharacter); false) else true);
///

TEST ///
-- Direct sums validate their ambient module, characteristic, and actual
-- independence instead of reinterpreting foreign generators.
W2 := weylModuleZ(cartanA 2, {2});
S2 := subWeylModule(W2, 2, {0}, {1});
W4 := weylModuleZ(cartanA 2, {4});
assert(try (subWeylModuleDirectSum(W4, 2, {S2}); false) else true);
assert(try (subWeylModuleDirectSum(W2, 3, {S2}); false) else true);
assert(try (subWeylModuleDirectSum(W2, 2, {S2, S2}); false) else true);
///

TEST ///
-- Successful direct-sum path: two independent simple submodules of
-- B_2 V(0,4) in characteristic 3.
W := weylModuleZ(cartanB 2, {0, 4})
M := maximalVectors(W, 3)
m20 := first select(M, x -> x#0 == {2, 0})
m10 := first select(M, x -> x#0 == {1, 0})
S20 := subWeylModule(W, 3, m20#0, m20#1)
S10 := subWeylModule(W, 3, m10#0, m10#1)
S := subWeylModuleDirectSum(W, 3, {S20, S10})
assert(dim S20 == 14)
assert(dim S10 == 5)
assert(dim S == dim S20 + dim S10)
///

TEST ///
-- A maximal vector is, by convention, nonzero.
W := weylModuleZ(cartanA 2, {2});
assert(not isMaximalVector(W, 2, {0}, {0}));
///

TEST ///
-- Restricted highest weight does not imply that the Weyl module is simple:
-- Delta_A2(1,1) in characteristic 3 also has the trivial factor.
W := weylModuleZ(cartanA 3, {1, 1});
dn := decompositionNumbers(W, 3);
assert(#(keys dn) == 2);
assert(dn#?{1, 1} and dn#{1, 1} == 1);
assert(dn#?{0, 0} and dn#{0, 0} == 1);
///

----------------------------------------------------------------------
-- WeightFormat I/O round-trips and partition input
----------------------------------------------------------------------

TEST ///
-- Type A: partition input (3, 1, 0, 0) builds the same module as Dynkin (2, 1, 0).
A := cartanA 4
W1 := weylModuleZ(A, {3, 1, 0, 0}, WeightFormat => "Partition")
W2 := weylModuleZ(A, {2, 1, 0})
assert(dim W1 == dim W2)
-- Highest weight reported in either format.
assert(highestWeight W1 == {2, 1, 0})
assert(highestWeight(W1, WeightFormat => "Partition") == {3, 1, 0, 0})
-- Round-trip.
assert(weightAsPartition(W1, {2, 1, 0}) == {3, 1, 0, 0})
assert(weightAsDynkin(W1, {3, 1, 0, 0}) == {2, 1, 0})
///

TEST ///
-- Type B: half-integer (spin) weights.
B := cartanB 2
Wspin := weylModuleZ(B, {1/2, 1/2}, WeightFormat => "Partition")
assert(dim Wspin == 4)
assert(highestWeight Wspin == {0, 1})
assert(highestWeight(Wspin, WeightFormat => "Partition") == {1/2, 1/2})
-- Type C: integer entries only.
C := cartanC 2
Wstd := weylModuleZ(C, {1, 0}, WeightFormat => "Partition")
assert(dim Wstd == 4)        -- standard rep of sp(4) is 4-dimensional
assert(highestWeight Wstd == {1, 0})
///

TEST ///
-- character with WeightFormat: keys come back in user format.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
chD := character W                          -- Dynkin keys
chP := character(W, WeightFormat => "Partition")
-- Both report the same total dimension.
assert(characterDim chD == characterDim chP)
-- The highest weight key is at the top of both.
assert(chD#{2, 0} == chP#{2, 0, 0})
///

TEST ///
-- Partition/epsilon input for a weight-space accessor need not be dominant.
-- In A_2, epsilon weight (0,1,0) is Dynkin weight (-1,1).
A := cartanA 3;
W := weylModuleZ(A, {1, 0});
assert(weightSpace(W, {0, 1, 0}, WeightFormat => "Partition") ==
       weightSpace(W, {-1, 1}));
assert(weightAsDynkin(A, {0, 1, 0}) == {-1, 1});
P := {1, 0, 2};
AP := submatrix(cartanA 4, P, P);
epsWeight := {0, 1, 0, 0};
assert(weightAsPartition(AP, weightAsDynkin(AP, epsWeight)) == epsWeight);
///

TEST ///
-- decompositionNumbers respects WeightFormat both ways.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
dnD := decompositionNumbers(W, 2)                          -- {2,0}, {0,1} keys
dnP := decompositionNumbers(W, 2, WeightFormat => "Partition")
assert(dnD#{2, 0} == dnP#{2, 0, 0})
assert(dnD#{0, 1} == dnP#{1, 1, 0})
///

TEST ///
-- subWeylModule accepts partition mu input.
A := cartanA 3
W := weylModuleZ(A, {2, 0})
-- The trivial submodule generated at partition (1, 1, 0) (= Dynkin (0, 1)).
S := subWeylModule(W, 2, {1, 1, 0}, {1}, WeightFormat => "Partition")
SD := subWeylModule(W, 2, {0, 1}, {1})
assert(dim S == dim SD)
///

TEST ///
-- Standalone converters work for all classical types.
A := cartanA 4
B := cartanB 3
C := cartanC 3
D := cartanD 4
assert(weightAsPartition(A, {1, 1, 1}) == {3, 2, 1, 0})
assert(weightAsDynkin(A, {3, 2, 1, 0}) == {1, 1, 1})
assert(weightAsPartition(B, {0, 0, 1}) == {1/2, 1/2, 1/2})    -- spin rep
assert(weightAsPartition(C, {1, 0, 0}) == {1, 0, 0})          -- standard rep
-- D_4 spin: half-integer.
assert(weightAsPartition(D, {0, 0, 1, 0}) == {1/2, 1/2, 1/2, -1/2})
///

TEST ///
-- Type-A partition weights that differ by a global shift restrict to the
-- same SL_n weight, so their character multiplicities must be combined.
A := cartanA 3
chShifted := new HashTable from {{1, 0, 0} => 2, {2, 1, 1} => 3}
chDynkin := characterAsDynkin(A, chShifted)
assert(#(keys chDynkin) == 1 and chDynkin#?{1, 0} and chDynkin#{1, 0} == 5)
assert(characterDim chDynkin == characterDim chShifted)
///

----------------------------------------------------------------------
-- Top-level wrappers: weylModule(p, lambda) and simpleQuotient(p, lambda)
----------------------------------------------------------------------

TEST ///
-- weylModule defaults to type A.
W := weylModule(2, {2, 1, 0})                  -- sl_3 V(1,1) = adjoint
assert(dim W == 8)
assert(highestWeight W == {1, 1})
assert(highestWeight(W, WeightFormat => "Partition") == {2, 1, 0})
///

TEST ///
-- weylModule with explicit LieType, classical types.
W := weylModule(2, {1, 0}, LieType => "B")     -- B_2 std rep
assert(dim W == 5)
W2 := weylModule(2, {1, 0, 0}, LieType => "C") -- C_3 std rep
assert(dim W2 == 6)
W3 := weylModule(2, {1/2, 1/2}, LieType => "B") -- B_2 spin rep
assert(dim W3 == 4)
///

TEST ///
-- WeightFormat => "Dynkin" overrides the partition default in the
-- high-level constructors.  A Dynkin tuple of length 2 selects A_2.
W := weylModule(2, {2, 0}, WeightFormat => "Dynkin");
assert(dim W == 6);
assert(highestWeight W == {2, 0});
Q := simpleQuotient(2, {2, 0}, WeightFormat => "Dynkin");
assert(dim Q == 3);
///

TEST ///
-- weylModule with exceptional types (Dynkin input).
WG := weylModule(2, {1, 0}, LieType => "G_2")
assert(dim WG == 7)
WG2 := weylModule(2, {0, 1}, LieType => "G_2")
assert(dim WG2 == 14)
///

TEST ///
-- simpleQuotient(p, lambda) with classical type defaults.
Q := simpleQuotient(2, {2, 0, 0})              -- L(2,0) for sl_3 mod 2
assert(dim Q == 3)
QB := simpleQuotient(2, {1, 0}, LieType => "B")
assert(dim QB == 4)                            -- B_2 L(omega_1) mod 2
QG := simpleQuotient(2, {1, 0}, LieType => "G_2")
assert(dim QG == 6)                            -- G_2 L(omega_1) mod 2
///

TEST ///
-- weylModule errors on bad inputs.
assert (try (weylModule(4, {1, 0}); false) else true)        -- 4 is not prime
assert (try (weylModule(2, {1, 0}, LieType => "BAD"); false) else true)
///

----------------------------------------------------------------------
-- Version 0.3 research-facing protocol and certification tests
----------------------------------------------------------------------

TEST ///
-- Canonical weight order is representation-theoretic and independent of
-- hash-table order.  Lex remains available explicitly.
W := weylModuleZ(cartanA 3, {2, 0})
assert(first weights W == {2, 0})
assert(weights W == {{2, 0}, {0, 1}, {1, -1}, {-2, 2}, {-1, 0}, {0, -2}})
assert(weights(W, WeightOrder => "Lex") == rsort weights W)
assert(apply(basisVecs W, first) ==
       flatten for mu in weights W list
           for k from 1 to weightMultiplicity(W, mu) list mu)
///

TEST ///
-- Direct reduction and quotient coordinates.
W := weylModuleZ(cartanA 3, {2, 0})
Q := reduction(W, 2)
assert(dim Q == dim W)
assert(characteristic Q == 2)
assert(coefficientRing Q === ZZ/2)
assert(rootSystemType Q === ("A", 2))
mu := {0, 1}
P := quotientProjection(Q, mu)
J := quotientSection(Q, mu)
assert(P * J == id_((ZZ/2)^(weightMultiplicity(Q, mu))))
v := {1}
assert(projectToQuotient(Q, mu, flatten entries liftFromQuotient(Q, mu, v)) ==
       transpose matrix(ZZ/2, {v}))
assert((constructionData Q)#"sectionIsCanonical" == false)
assert(weylModule(W, 2) =!= null)
///

TEST ///
-- Submodule lattice operations and genuine subquotients.
W := weylModuleZ(cartanA 3, {2, 0})
Z := subWeylModule(W, 2, {})
S := socleWeyl(W, 2)
assert(isSubmoduleOf(Z, S))
assert(containsSubmodule(S, Z))
assert(equalSubmodules(submoduleSum(S, Z), S))
assert(equalSubmodules(submoduleIntersection(S, S), S))
assert(submoduleCodimension S == dim W - dim S)
M := subquotientWeylModule(Z, S)
assert(dim M == dim S)
assert(sort pairs(character M) == sort pairs(character S))
assert(subquotientProjection(M, {0, 1}) * subquotientSection(M, {0, 1}) ==
       id_((ZZ/2)^1))
assert(compositionFactors M == compositionFactors S)
assert(isValid checkWeylModule(M, CheckLevel => "Serre"))
///

TEST ///
-- WeightFormat is accepted uniformly by ordinary Chevalley actions.
A := cartanA 3
W := weylModuleZ(A, {1, 0})
Q := reduction(W, 2)
muDynkin := {-1, 1}
muPartition := {0, 1, 0}
assert(actionE(W, muDynkin, 1) ==
       actionE(W, muPartition, 1, WeightFormat => "Partition"))
assert(actionH(Q, 1, {1, 0, 0}, WeightFormat => "Partition") == 1)
///

TEST ///
-- First-class characters retain Cartan data and support exact arithmetic.
A := cartanA 3
W := weylModuleZ(A, {1, 0})
C := weylCharacter W
assert(ambientCartan C == A)
assert(characteristic C == 0)
assert(dim C == 3)
assert(sort pairs(rawCharacter C) == sort pairs(character W))
assert(first characterTerms C == ({1, 0}, 1))
assert(dim(C * dualCharacter C) == 9)
assert(isVirtualCharacter(C - C) == false)
F := frobeniusTwistCharacter(C, 2)
assert(characteristic F == 2)
assert(weightMultiplicity(F, {2, 0}) == 1)
assert(sum apply(tensorProductFactors(C, dualCharacter C, 2), last) >= 1)
///

TEST ///
-- Labelled decomposition tables carry their indexing data.
T := schurAlgebraDecompositionTable(2, 2, 2)
assert(rowLabels T == {{2, 0}, {1, 1}})
assert(columnLabels T == {{2, 0}, {1, 1}})
assert(matrix T == schurAlgebraDecompositionMatrix(2, 2, 2))
assert(prime T == 2)
assert((tableMetadata T)#"family" == "Schur")
G := symmetricGroupDecompositionTable(2, 3)
assert(#rowLabels G == numrows matrix G)
assert(#columnLabels G == numcols matrix G)
///

TEST ///
-- Certified A2 socle analysis checks every successive quotient and records
-- mathematically consistent layer dimensions and factors.
W := weylModuleZ(cartanA 3, {2, 0})
R := socleAnalysis(W, 2, RequireComplete => true)
assert(isCertified R)
assert(socleAnalysisComplete R)
assert(socleAnalysisPrime R == 2)
assert(socleAnalysisModuleDimension R == 6)
assert(socleLength R == 2)
assert(socleLayerDimensions R == {3, 3})
assert(cumulativeSocleDimensions R == {3, 6})
assert(successiveQuotientDimensions R == {6, 3})
assert((socleLayerFactors(R, 1))#{0, 1} == 1)
assert(ambiguousWeights R == {})
assert(analysisIssues R == {})
///

TEST ///
-- G2 V(3,0) in characteristic two is ambiguous in more than one successive
-- quotient.  Candidate mode records this; certified mode must refuse it.
W := weylModuleZ(cartanG2(), {3, 0})
R := socleAnalysis(W, 2)
assert(not isCertified R)
assert(member({2, 0}, ambiguousWeights R))
assert(#select(ambiguousWeightsByLayer R, L -> member({2, 0}, L)) >= 2)
assert(try (socleAnalysis(W, 2, RequireComplete => true); false) else true)
assert(try (socleWeyl(W, 2, RequireComplete => true); false) else true)
///

TEST ///
-- Multi-prime profiles reuse the integral object and separate inexpensive
-- character columns from opt-in certified socle data.
W := weylModuleZ(cartanA 3, {2, 0})
P := reductionProfile(W, {2, 3}, IncludeSocle => true,
                      RequireComplete => true)
assert(reductionPrimes P == {2, 3})
assert((reductionRow(P, 2))#"simpleDimension" == 3)
assert((reductionRow(P, 3))#"simpleDimension" == 6)
assert((reductionRow(P, 2))#"socleCertified")
assert(reducibleCharacteristics P == {2})
assert(reductionWeylModule P === W)
///

TEST ///
-- Global matrices, Kostant divided powers, and relation certificates.
W := weylModuleZ(cartanA 3, {1, 1})
D := chevalleyActionMatrices W
assert(#raisingMatrices D == 2)
assert(#loweringMatrices D == 2)
assert(#cartanMatrices D == 2)
assert(#globalBasis D == dim W)
assert(numrows dividedPowerActionE(W, 1, 2) == dim W)
assert(numrows dividedPowerActionF(W, 2, 2) == dim W)
assert(numrows binomialCartanAction(W, 1, 2) == dim W)
C := verifyRepresentation W
assert(isValid C)
assert(verificationFailures C == {})
///

TEST ///
-- Accessible graph and socle diagram data are complete textual records.
W := weylModuleZ(cartanA 2, {2})
G := weightGraph W
assert(#graphVertices G == 3)
assert(#graphEdges G == 2)
assert(match("f_1", toDot G) =!= null)
R := socleAnalysis(W, 2, RequireComplete => true)
D := socleDiagram R
assert(isCertified D)
assert(#diagramLayers D == 2)
assert(match("socle layer 1", toDot D) =!= null)
///

TEST ///
-- Maximal-vector/Hom wrappers expose the standard highest-vector
-- correspondence without changing its coordinate convention.
W := weylModuleZ(cartanA 2, {2})
assert(weylHomDimension(W, 2, {0}) == 1)
assert(#weylHomGenerators(W, 2, {0}) == 1)
Q := reduction(W, 2)
assert(weylHomDimension(Q, {0}) == 1)
///

TEST ///
-- The Schur-algebra Weyl module is integral, so its natural constructor is
-- prime-free; the historical prime-bearing overload remains compatible.
W := schurAlgebraWeylModule {2, 1}
Wp := schurAlgebraWeylModule(2, {2, 1})
assert(dim W == dim Wp)
assert(sort pairs(character W) == sort pairs(character Wp))
///

----------------------------------------------------------------------
-- Adversarial character, coordinate, and table contracts
----------------------------------------------------------------------

TEST ///
-- Character output options must be routed all the way through the wrapper.
A := cartanA 3
C := weylCharacter weylModuleZ(A, {1, 1})
lexTerms := apply(characterTerms(C, WeightOrder => "Lex"), first)
assert(weights(C, WeightOrder => "Lex") === lexTerms)
assert(try (weights(C, WeightOrder => "invalid"); false) else true)

CW := weylCharacter weylModuleZ(A, {2, 0})
dDynkin := decomposeCharacter(CW, 2)
dExpected := new HashTable from for mu in keys dDynkin list
    fundamentalToPartition(A, mu) => dDynkin#mu
dPartition := decomposeCharacter(CW, 2, WeightFormat => "Partition")
assert(sort pairs dPartition === sort pairs dExpected)
///

TEST ///
-- Nondominant epsilon coordinates round-trip in the classical types even
-- after a simultaneous permutation of the Cartan rows and columns.
cases := {
    (cartanB 4, {2, 0, 3, 1}),
    (cartanC 4, {1, 3, 0, 2}),
    (cartanD 5, {4, 1, 3, 0, 2})
}
for z in cases do (
    A0 := z#0;
    perm := z#1;
    A := submatrix(A0, perm, perm);
    n := numrows A;
    w1 := for i from 0 to n - 1 list if i % 2 == 0 then 1 else -2;
    w2 := for i from 0 to n - 1 list i - 2;
    C := weylCharacter(A, new HashTable from {w1 => 2, w2 => -1});
    epsTerms := rawCharacter(C, WeightFormat => "Partition");
    roundTrip := weylCharacter(A, epsTerms, WeightFormat => "Partition");
    assert(roundTrip == C);
    for t in pairs epsTerms do
        assert(weightMultiplicity(C, t#0, WeightFormat => "Partition") == t#1);
)

-- Include an actual half-integral spin character and a lattice failure.
B := cartanB 2
spin := weylCharacter weylModuleZ(B, {0, 1})
spinEps := rawCharacter(spin, WeightFormat => "Partition")
assert(weylCharacter(B, spinEps, WeightFormat => "Partition") == spin)
assert(try (weightAsDynkin(B, {1/2, 0}); false) else true)
///

TEST ///
-- WeylCharacter equality is equality in the formal character ring; source
-- format, characteristic, and provenance are metadata rather than terms.
A := cartanA 3
W := weylModuleZ(A, {1, 0})
C := weylCharacter W
D := dualCharacter C
Z := weylCharacter(A, new HashTable)
assert(C + Z == C)
assert(C - C == Z)
assert(not isVirtualCharacter(C - C))
assert(-(-C) == C)
assert(0 * C == Z)
assert(C * D == D * C)
assert((C + D) * C == C * C + D * C)
assert(dim(C * D) == dim C * dim D)

C2 := weylCharacter reduction(W, 2)
assert(C == C2)
assert(characteristic C == 0 and characteristic C2 == 2)
E := weylCharacter(cartanB 2, new HashTable from {{1, 0} => 1})
assert(C != E)
assert(try (C + E; false) else true)
assert(try (C * E; false) else true)
///

TEST ///
-- Duality, tensor products, and Frobenius twists obey their functorial
-- identities and reject incompatible characteristic metadata.
A := cartanA 3
C := weylCharacter weylModuleZ(A, {1, 0})
D := dualCharacter C
assert(dualCharacter C == D)
assert(dualCharacter dualCharacter C == C)
assert(dualCharacter(C * D) == dualCharacter C * dualCharacter D)

F0 := frobeniusTwistCharacter(C, 2, 0)
assert(F0 == C and characteristic F0 == 2)
assert(dim frobeniusTwistCharacter(C, 2, 4) == dim C)
assert(frobeniusTwistCharacter(frobeniusTwistCharacter(C, 2, 2), 2, 3)
       == frobeniusTwistCharacter(C, 2, 5))
assert(dualCharacter frobeniusTwistCharacter(C, 2)
       == frobeniusTwistCharacter(dualCharacter C, 2))
assert(try (frobeniusTwistCharacter(C, 4); false) else true)
assert(try (frobeniusTwistCharacter(C, 2, -1); false) else true)
C3 := frobeniusTwistCharacter(C, 3)
assert(try (frobeniusTwistCharacter(C3, 2); false) else true)

assert(tensorProductFactors(C, D, 2) === {({1, 1}, 1), ({0, 0}, 1)})
assert(tensorProductFactors(C, D, 2, WeightFormat => "Partition")
       === {({2, 1, 0}, 1), ({0, 0, 0}, 1)})
///

TEST ///
-- Decomposition validates the root datum, prime, weights, and coefficients
-- before examining support, and handles incomparable maximal constituents.
A := cartanA 3
emptyCharacter := new HashTable
assert(try (
    decomposeCharacter(matrix(ZZ, {{1}}), 2, emptyCharacter);
    false
) else true)
C0 := weylCharacter(A, emptyCharacter)
assert(#keys decomposeCharacter(C0, 2) == 0)
assert(try (decomposeCharacter(A, 4, emptyCharacter); false) else true)
assert(try (decomposeCharacter(A, 2,
    new HashTable from {{1, 0} => -1}); false) else true)
assert(try (decomposeCharacter(A, 2,
    new HashTable from {{1, 0} => 1/2}); false) else true)
assert(try (decomposeCharacter(A, 2,
    new HashTable from {{1} => 1}); false) else true)
assert(try (decomposeCharacter(A, 2,
    new HashTable from {{1, 0} => 1}); false) else true)

S1 := weylCharacter(A, simpleCharacter(A, 2, {1, 0}))
S2 := weylCharacter(A, simpleCharacter(A, 2, {0, 1}))
X := S1 + 2 * S2
d := decomposeCharacter(X, 2)
assert(d#{1, 0} == 1 and d#{0, 1} == 2)
assert(try (decomposeCharacter(S1 - S2, 2); false) else true)
///

TEST ///
-- A custom decomposition table cannot encode fractional or negative
-- multiplicities, duplicate labels, inconsistent sizes, or a nonprime field.
meta := new HashTable from {"prime" => 2, "algebra" => "test algebra"}
assert(try (
    decompositionTable(matrix(QQ, {{1}}), {"r"}, {"c"}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{-1}}), {"r"}, {"c"}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{1, 0}, {0, 1}}),
        {"r", "r"}, {"c1", "c2"}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{1, 0}, {0, 1}}),
        {"r1", "r2"}, {"c", "c"}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{1}}), {}, {"c"}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{1}}), {"r"}, {}, meta);
    false
) else true)
assert(try (
    decompositionTable(matrix(ZZ, {{1}}), {"r"}, {"c"},
        new HashTable from {"prime" => 4, "algebra" => "X"});
    false
) else true)
///

TEST ///
-- Cross-check every row through degree three against the sparse API,
-- including exact label order, nonnegativity, and the semisimple boundary.
-- The separate known S_4 test above checks nonsemisimple degree four.
for p in {2, 3, 5} do for n from 1 to 3 do (
    T := symmetricGroupDecompositionTable(p, n);
    rows := allPartitions n;
    cols := pRestrictedPartitions(p, n);
    assert(rowLabels T == rows);
    assert(columnLabels T == cols);
    assert(all(flatten entries matrix T, x -> instance(x, ZZ) and x >= 0));
    for i from 0 to #rows - 1 do (
        sparse := symmetricGroupDecompositionNumbers(p, rows#i);
        for j from 0 to #cols - 1 do
            assert((matrix T)_(i, j) ==
                (if sparse#?(cols#j) then sparse#(cols#j) else 0));
    );
    if p > n then assert(matrix T == id_(ZZ^(#rows)));
)
assert(try (schurAlgebraDecompositionMatrix(2, 0, 1); false) else true)
assert(try (schurAlgebraDecompositionMatrix(2, 2, -1); false) else true)
assert(try (symmetricGroupDecompositionMatrix(2, 0); false) else true)
assert(try (symmetricGroupDecompositionNumbers(2, {}); false) else true)
assert(try (symmetricGroupDecompositionNumbers(2, {0, 0}); false) else true)
///

----------------------------------------------------------------------
-- Independent mathematical oracles and cross-family stress tests
----------------------------------------------------------------------

TEST ///
-- Complete A_1 oracle in the divided-power basis v_j=f^(j)v_0.  This checks
-- every entry, including generalized binomial coefficients at negative
-- h-weights and divided powers beyond the length of the string.
for n in {0, 1, 2, 5, 7} do (
    W := weylModuleZ(cartanA 2, {n});
    D := chevalleyActionMatrices W;
    E := (raisingMatrices D)#0;
    F := (loweringMatrices D)#0;
    H := (cartanMatrices D)#0;
    for row from 0 to n do for col from 0 to n do (
        assert(E_(row, col) ==
            if row == col - 1 then n - col + 1 else 0);
        assert(F_(row, col) ==
            if row == col + 1 then col + 1 else 0);
        assert(H_(row, col) ==
            if row == col then n - 2 * col else 0);
    );
    for r from 0 to n + 2 do (
        Er := dividedPowerActionE(W, 1, r);
        Fr := dividedPowerActionF(W, 1, r);
        Br := binomialCartanAction(W, 1, r);
        for row from 0 to n do for col from 0 to n do (
            assert(Er_(row, col) ==
                if row == col - r then binomial(n - col + r, r) else 0);
            assert(Fr_(row, col) ==
                if row == col + r then binomial(col + r, r) else 0);
            assert(Br_(row, col) ==
                if row == col then binomial(n - 2 * col, r) else 0);
        );
    );
)
///

TEST ///
-- The known rank-one structure for 0 <= n <= 2p-1 independently checks
-- irreducibility, two-layer socles, simple factors, and extension metadata.
for p in {2, 3, 5} do (
    for n from 0 to p - 1 do (
        W := weylModuleZ(cartanA 2, {n});
        R := socleAnalysis(W, p, RequireComplete => true);
        assert(isIrreducible(W, p));
        assert(socleLength R == 1);
        assert(socleLayerDimensions R == {n + 1});
        assert((socleLayerFactors(R, 1))#{n} == 1);
        assert(not isTwoFactorExtension extensionData(W, p));
    );
    for n from p to 2 * p - 2 do (
        bottom := 2 * p - 2 - n;
        W = weylModuleZ(cartanA 2, {n});
        dn := decompositionNumbers(W, p);
        assert(#keys dn == 2 and dn#{n} == 1 and dn#{bottom} == 1);
        R = socleAnalysis(W, p, RequireComplete => true);
        assert(isCertified R and socleLength R == 2);
        assert((socleLayerFactors(R, 1))#{bottom} == 1);
        assert((socleLayerFactors(R, 2))#{n} == 1);
        assert(socleLayerDimensions R == {bottom + 1, 2 * (n - p + 1)});
        E := extensionData(W, p);
        assert(isCertified E and isTwoFactorExtension E);
    );
    W = weylModuleZ(cartanA 2, {2 * p - 1});
    assert(isIrreducible(W, p));
    assert(socleLength socleAnalysis(W, p, RequireComplete => true) == 1);
)
///

TEST ///
-- Divided powers satisfy x^(a)x^(b)=binomial(a+b,a)x^(a+b) not only on
-- the integral module, but after all supported submodule/quotient operations.
W := weylModuleZ(cartanA 3, {2, 0})
Z := subWeylModule(W, 2, {})
S := socleWeyl(W, 2, RequireComplete => true)
Q := quotientWeylModule S
M := subquotientWeylModule(Z, S)
for X in {W, Z, S, Q, M} do (
    R := coefficientRing X;
    assert(isValid verifyRepresentation X);
    for i from 1 to lieRank X do for a from 0 to 2 do for b from 0 to 2 do (
        assert(dividedPowerActionE(X, i, a) * dividedPowerActionE(X, i, b)
            == (binomial(a + b, a))_R * dividedPowerActionE(X, i, a + b));
        assert(dividedPowerActionF(X, i, a) * dividedPowerActionF(X, i, b)
            == (binomial(a + b, a))_R * dividedPowerActionF(X, i, a + b));
    );
)
///

TEST ///
-- Exercise bad-characteristic actions and complete relation certificates in
-- every non-simply-laced family, plus D_4 as a branching-type control.
cases := {
    (cartanB 2, {1, 0}, 2),
    (cartanC 2, {1, 0}, 2),
    (cartanD 4, {1, 0, 0, 0}, 2),
    (cartanG2(), {1, 0}, 2),
    (cartanG2(), {0, 1}, 3)
}
for data in cases do (
    W := weylModuleZ(data#0, data#1);
    Q := reduction(W, data#2);
    assert(isValid verifyRepresentation W);
    assert(isValid verifyRepresentation Q);
    assert(sum values character Q == dim Q);
    assert(sum apply(graphVertices weightGraph Q, v -> v#"multiplicity") == dim Q);
)

W := weylModuleZ(cartanB 2, {1, 0})
R := socleAnalysis(W, 2, RequireComplete => true)
assert(socleLayerDimensions R == {1, 4})
assert((socleLayerFactors(R, 1))#{0, 0} == 1)
assert((socleLayerFactors(R, 2))#{1, 0} == 1)

W = weylModuleZ(cartanG2(), {1, 0})
R = socleAnalysis(W, 2, RequireComplete => true)
assert(socleLayerDimensions R == {1, 6})
assert((socleLayerFactors(R, 1))#{0, 0} == 1)
assert((socleLayerFactors(R, 2))#{1, 0} == 1)

W = weylModuleZ(cartanG2(), {0, 1})
R = socleAnalysis(W, 3, RequireComplete => true)
assert(socleLayerDimensions R == {7, 7})
assert((socleLayerFactors(R, 1))#{1, 0} == 1)
assert((socleLayerFactors(R, 2))#{0, 1} == 1)
///

----------------------------------------------------------------------
-- Adversarial modular actions, coordinates, and certificate fault injection
----------------------------------------------------------------------

TEST ///
-- In bad characteristic the ordinary e_i-kernel is not the highest-vector
-- space for the algebraic group: e_i^(2) supplies a genuinely stronger test.
Fp := ZZ/2
Q3 := reduction(weylModuleZ(cartanA 2, {3}), 2)
assert(actionE(Q3, {-1}, 1) == matrix(Fp, {{0}}))
assert((dividedPowerActionE(Q3, 1, 2))_(0, 2) == 1)
assert(maximalVectors(Q3, {-1}) == {})
assert(weylHomDimension(Q3, {-1}) == 0)

-- Graph edges are computed over the actual coefficient ring: one integral
-- edge in V(2) disappears after reduction modulo two.
W2 := weylModuleZ(cartanA 2, {2})
Q2 := reduction(W2, 2)
GZ := weightGraph W2
GFp := weightGraph Q2
assert(#graphVertices GZ == 3 and #graphVertices GFp == 3)
assert(#graphEdges GZ == 2 and #graphEdges GFp == 1)
assert(#select(graphEdges GZ,
    e -> e#"from" == {0} and e#"to" == {-2}) == 1)
assert(#select(graphEdges GFp,
    e -> e#"from" == {0} and e#"to" == {-2}) == 0)
///

TEST ///
-- Quotient coordinates are tested at every weight, including a weight of
-- ambient multiplicity two where the kernel and quotient both have rank one.
A := cartanA 3
W := weylModuleZ(A, {2, 1})
Fp := ZZ/2
S := subWeylModule(W, 2, {1, 0}, {0, 1})
Q := quotientWeylModule S
assert(dim S == 3 and dim Q == dim W - dim S)
assert(weightMultiplicity(W, {1, 0}) == 2)
assert(weightMultiplicity(S, {1, 0}) == 1)
assert(weightMultiplicity(Q, {1, 0}) == 1)
for mu in weights W do (
    dW := weightMultiplicity(W, mu);
    dS := weightMultiplicity(S, mu);
    dQ := weightMultiplicity(Q, mu);
    P := quotientProjection(Q, mu);
    J := quotientSection(Q, mu);
    assert(dW == dS + dQ);
    assert(numrows P == dQ and numcols P == dW);
    assert(numrows J == dW and numcols J == dQ);
    assert(P * J == id_(Fp^dQ));
    if dS > 0 then
        assert(P * weightSpace(S, mu) == matrix mutableMatrix(Fp, dQ, dS));
    assert(projectToQuotient(Q, mu,
               liftFromQuotient(Q, mu, id_(Fp^dQ))) == id_(Fp^dQ));
)
assert(isValid checkWeylModule(Q, CheckLevel => "Serre"))
///

TEST ///
-- A genuine T/S test uses a nonzero lower submodule and a strictly larger
-- upper submodule; all stored sections and projections must split weightwise.
A := cartanA 3
W := weylModuleZ(A, {2, 1})
Fp := ZZ/2
S := subWeylModule(W, 2, {1, 0}, {0, 1})
T := subWeylModule(W, 2, {0, 2}, {1})
assert(dim S > 0 and dim T > dim S and isSubmoduleOf(S, T))
M := subquotientWeylModule(S, T)
assert(dim M == dim T - dim S and dim M > 0)
for mu in weights W do (
    dS := weightMultiplicity(S, mu);
    dT := weightMultiplicity(T, mu);
    dM := weightMultiplicity(M, mu);
    P := subquotientProjection(M, mu);
    J := subquotientSection(M, mu);
    assert(dT == dS + dM);
    assert(P * J == id_(Fp^dM));
    if dS > 0 then
        assert(P * weightSpace(S, mu) == matrix mutableMatrix(Fp, dM, dS));
    assert(projectToSubquotient(M, mu,
               liftFromSubquotient(M, mu, id_(Fp^dM))) == id_(Fp^dM));
)
assert(isValid checkWeylModule(M, CheckLevel => "Serre"))
///

TEST ///
-- The explicit-Cartan constructor has a Dynkin default, and simple-root
-- indices are validated uniformly even when the underlying object is zero.
A := cartanA 3
W := weylModuleZ(A, {2, 1})
Q := weylModule(A, 2, {2, 1})
assert(dim Q == dim W)
assert(sort pairs character Q == sort pairs character W)
Z := subWeylModule(W, 2, {})
Full := subWeylModule(W, 2, highestWeight W, {1})
Q0 := quotientWeylModule Full
M0 := subquotientWeylModule(Z, Z)
for X in {Z, Q0, M0} do (
    assert(dim X == 0);
    assert(weights X == {});
    assert(highestWeight X === null);
    assert(actionH(X, 1, highestWeight W) == 0);
    assert(#graphVertices weightGraph X == 0);
    assert(isValid checkWeylModule(X, CheckLevel => "Basic"));
)
for X in {W, Q, Z, Q0, M0} do (
    assert(try (actionE(X, highestWeight W, 0); false) else true);
    assert(try (actionF(X, highestWeight W, 3); false) else true);
    assert(try (actionH(X, 0, highestWeight W); false) else true);
    assert(try (actionH(X, 3, highestWeight W); false) else true);
)
assert(try (applyChevalley(W, "f", 0,
    first weightSpace(W, highestWeight W), highestWeight W); false) else true)
///

TEST ///
-- Deliberately corrupt private action tables.  The basic audit must detect
-- illegal boundary blocks and altered h_i values before relation checking.
WE := weylModuleZ(cartanA 2, {2})
WE#"actE"#({2}, 1) = matrix(ZZ, {{1}})
CE := checkWeylModule(WE, CheckLevel => "Basic")
assert(not isValid CE)
assert((verificationChecks CE)#"storedActionTables" == false)
assert(member("storedActionBlock",
    apply(verificationFailures CE, F -> F#"check")))

WH := weylModuleZ(cartanA 2, {2})
WH#"actH"#1#{2} = 99
CH := checkWeylModule(WH, CheckLevel => "Basic")
assert(not isValid CH)
assert(member("storedCartanAction",
    apply(verificationFailures CH, F -> F#"check")))

Qbad := reduction(weylModuleZ(cartanA 2, {2}), 2)
Qbad#"actEQ"#({2}, 1) = matrix(ZZ/2, {{1}})
CQ := checkWeylModule(Qbad, CheckLevel => "Basic")
assert(not isValid CQ)
assert(member("storedActionBlock",
    apply(verificationFailures CQ, F -> F#"check")))

CG := checkWeylModule(weylModuleZ(cartanA 3, {1, 0}),
                      CheckLevel => "Relations")
assert(isValid CG)
assert((verificationChecks CG)#?"[h1,h2]")
assert((verificationChecks CG)#"[h1,h2]")
///

----------------------------------------------------------------------
-- Weight graphs and socle diagrams
----------------------------------------------------------------------

TEST ///
-- The SL2 formula f v_j = (j+1) v_(j+1) determines every edge,
-- including all missing edges after reduction modulo p.
for n from 0 to 8 do (
    W := weylModuleZ(cartanA 2, {n});
    expectedWeights := apply(toList(0..n), j -> {n - 2*j});
    expectedIntegral := for j from 0 to n - 1 list
        ({n - 2*j}, {n - 2*j - 2}, 1, 1);
    G := weightGraph W;
    assert(apply(graphVertices G, v -> v#"weight") == expectedWeights);
    assert(all(graphVertices G, v -> v#"multiplicity" == 1));
    assert(apply(graphEdges G,
        e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")) == expectedIntegral);
    for p in {2, 3, 5} do (
        G = weightGraph reduction(W, p);
        expected := for j from 0 to n - 1 list
            if (j+1) % p != 0 then ({n - 2*j}, {n - 2*j - 2}, 1, 1) else null;
        assert(apply(graphVertices G, v -> v#"weight") == expectedWeights);
        assert(all(graphVertices G, v -> v#"multiplicity" == 1));
        assert(apply(graphEdges G,
            e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")) ==
            select(expected, e -> e =!= null));
    );
)
///

TEST ///
-- Nonsymmetric Cartan matrices distinguish columns from rows.
-- These explicit weight strings are independent of the graph construction.
for datum in {
    (cartanB 2, {{1,0}, {-1,2}, {0,0}, {1,-2}, {-1,0}}, {1,2,2,1}, {2}),
    (cartanC 2, {{1,0}, {-1,1}, {1,-1}, {-1,0}}, {1,2,1}, {})
} do (
    W := weylModuleZ(datum#0, {1,0});
    ws := datum#1;
    roots := datum#2;
    expected := for j from 0 to #roots - 1 list
        (ws#j, ws#(j+1), roots#j, 1);
    G := weightGraph W;
    assert(apply(graphVertices G, v -> v#"weight") == ws);
    assert(all(graphVertices G, v -> v#"multiplicity" == 1));
    assert(apply(graphEdges G,
        e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")) == expected);
    G = weightGraph reduction(W, 2);
    assert(apply(graphVertices G, v -> v#"weight") == ws);
    assert(apply(graphEdges G,
        e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")) ==
        apply(select(toList(0..#roots-1), j -> not member(j, datum#3)), j -> expected#j));
)
///

TEST ///
-- For T/S, rank(f_i) = dim(S_nu + f_i(T_mu)) - dim S_nu.
-- This computes every edge without global matrices or quotient sections.
auditGraph := M -> (
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    R := coefficientRing M;
    A := ambientCartan W;
    r := numrows A;
    G := weightGraph M;
    assert(sort apply(graphVertices G,
        v -> (v#"weight", v#"multiplicity")) == sort pairs character M);
    predicted := {};
    for mu in weights M do for i from 1 to r do (
        nu := for j from 0 to r - 1 list mu#j - A_(j, i - 1);
        if weightMultiplicity(M, nu) == 0 then continue;
        B := actionF(W, nu, i);
        if B === null then continue;
        F := B ** R;
        dnu := weightMultiplicity(W, nu);
        low := matrix mutableMatrix(R, dnu, 0);
        upper := id_(R^(weightMultiplicity(W, mu)));
        if instance(M, SubWeylModuleFp) then upper = weightSpace(M, mu);
        if instance(M, QuotientWeylModuleFp) then
            low = weightSpace(definingKernel M, nu);
        if instance(M, SubquotientWeylModuleFp) then (
            low = weightSpace(lowerSubmodule M, nu);
            upper = weightSpace(upperSubmodule M, mu);
        );
        if low === null then low = matrix mutableMatrix(R, dnu, 0);
        rk := rank(low | F * upper) - rank low;
        if rk > 0 then predicted = append(predicted, (mu, nu, i, rk));
    );
    assert(sort predicted == sort apply(graphEdges G,
        e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")));
)
W := weylModuleZ(cartanA 3, {2,1})
S := subWeylModule(W, 2, {1,0}, {0,1})
T := subWeylModule(W, 2, {0,2}, {1})
assert(0 < dim S and dim S < dim T and dim T < dim W)
for M in {W, reduction(W, 2), S, T, quotientWeylModule S,
    quotientWeylModule T, subquotientWeylModule(S,T)} do auditGraph M
for datum in {(cartanB 2, {1,0}, 2), (cartanC 2, {1,0}, 2),
              (cartanG2(), {0,1}, 3)} do (
    W = weylModuleZ(datum#0, datum#1);
    S = socleWeyl(W, datum#2, RequireComplete => true);
    for M in {W, reduction(W, datum#2), S, quotientWeylModule S} do auditGraph M;
)
Z := subWeylModule(W, 3, {})
Full := subWeylModule(W, 3, highestWeight W, {1})
for M in {Z, quotientWeylModule Full, subquotientWeylModule(Z,Z)} do (
    auditGraph M;
    assert(graphVertices weightGraph M == {});
    assert(graphEdges weightGraph M == {});
)
///

TEST ///
-- A rank-two block can drop rank without disappearing.  Isolated vertices
-- can belong to a simple group module and must remain visible in both forms.
W := weylModuleZ(cartanA 3, {2,1})
G := weightGraph W
assert(sort apply(select(graphVertices G, v -> v#"multiplicity" == 2),
    v -> v#"weight") == sort {{1,0}, {-1,1}, {0,-1}})
expected := {({1,0}, {-1,1}, 1, 2), ({-1,1}, {0,-1}, 2, 2)}
assert(sort apply(select(graphEdges G, e -> e#"rank" == 2),
    e -> (e#"from", e#"to", e#"simpleRoot", e#"rank")) == sort expected)
G = weightGraph reduction(W, 2)
for edgeData in expected do (
    es := select(graphEdges G, e -> e#"from" == edgeData#0 and e#"simpleRoot" == edgeData#2);
    assert(#es == 1 and (first es)#"to" == edgeData#1 and (first es)#"rank" == 1);
)
Q := simpleQuotient(weylModuleZ(cartanA 2, {2}), 2)
G = weightGraph Q
assert(isIrreducible Q)
assert(apply(graphVertices G, v -> v#"weight") == {{2}, {-2}})
assert(graphEdges G == {})
assert(rank dividedPowerActionF(Q, 1, 2) == 1)
assert(rank dividedPowerActionE(Q, 1, 2) == 1)
assert(member("  weight {2}: multiplicity 1", lines toString net G))
assert(member("  weight {-2}: multiplicity 1", lines toString net G))
assert(match("v0", toDot G) and match("v1", toDot G))
assert(not match(" -> ", toDot G))
///

TEST ///
-- Socle layer dimensions are increments.  A submodule argument specifies
-- the ambient quotient; in particular a full submodule gives an empty diagram.
W := weylModuleZ(cartanA 2, {2})
R := socleAnalysis(W, 2, RequireComplete => true)
D := socleDiagram R
assert(isCertified D and not D#"incidenceDetermined")
assert(apply(diagramLayers D, L -> L#"dimension") == {1, 2})
assert(cumulativeSocleDimensions R == {1, 3})
assert(pairs(((diagramLayers D)#0)#"factors") == {({0}, 1)})
assert(pairs(((diagramLayers D)#1)#"factors") == {({2}, 1)})
assert(match("socle layer 1 .dim 1.", toDot D))
assert(match("socle layer 2 .dim 2.", toDot D))
assert(match("rank=same", toDot D))
assert(match("rankdir=BT", toDot D))
assert(match("style=invis", toDot D))
S := subWeylModule(W, 2, {0}, {1})
DS := socleDiagram S
DQ := socleDiagram quotientWeylModule S
assert(apply(diagramLayers DS, L -> L#"dimension") ==
    apply(diagramLayers DQ, L -> L#"dimension"))
assert(apply(diagramLayers DS, L -> sort pairs(L#"factors")) ==
    apply(diagramLayers DQ, L -> sort pairs(L#"factors")))
assert(apply(diagramLayers DS, L -> L#"dimension") == {2})
Full := subWeylModule(W, 2, {2}, {1})
assert(diagramLayers socleDiagram Full == {})
assert(isCertified socleDiagram Full)
P := socleDiagram socleAnalysis(W, 2, WeightFormat => "Partition",
    RequireComplete => true)
assert(pairs(((diagramLayers P)#0)#"factors") == {({0,0}, 1)})
assert(pairs(((diagramLayers P)#1)#"factors") == {({2,0}, 1)})
///

TEST ///
-- Reaching the entire module does not remove maximal-vector ambiguity.
R := socleAnalysis(weylModuleZ(cartanG2(), {3,0}), 2)
D := socleDiagram R
assert(not isCertified R and not isCertified D)
assert(D#"complete" == socleAnalysisComplete R)
assert(apply(diagramLayers D, L -> L#"ambiguousWeights") == ambiguousWeightsByLayer R)
assert(match("socle search may be incomplete", toString net D))
assert(match("socle search may be incomplete", toDot D))
assert(match("ambiguous at", toDot D))
assert(not D#"incidenceDetermined")
///

TEST ///
-- SVG drawings contain exactly the mathematical vertices and edges.
for datum in {(cartanA 2, {2}), (cartanA 3, {2,1}),
              (cartanB 2, {1,0}), (cartanC 2, {1,0})} do (
    W := weylModuleZ(datum#0, datum#1);
    for M in {W, reduction(W,2)} do (
        G := weightGraph M;
        svg := toSVG G;
        assert(match("^<svg ", svg));
        assert(match("</svg>", svg));
        assert(html G == svg);
        assert(#select(lines svg,
            s -> match("^<g role=\"group\" aria-label=\"Weight ", s)) == #graphVertices G);
        assert(#select(lines svg,
            s -> match("^<g role=\"group\" aria-label=\"From weight ", s)) == #graphEdges G);
        assert(not match("<script", svg));
        assert(not match("<image", svg));
    );
)
///

TEST ///
-- Isolated weights and the zero module require no edge to be drawn.
W := weylModuleZ(cartanA 2, {2})
Q := simpleQuotient(W, 2)
svg := toSVG weightGraph Q
assert(match("Weight .2., multiplicity 1", svg))
assert(match("Weight .-2., multiplicity 1", svg))
assert(not match("<path ", svg))
Z := subWeylModule(W, 2, {})
assert(match("Zero module: no weights", toSVG weightGraph Z))
Full := subWeylModule(W, 2, {2}, {1})
assert(match("No nonzero layers", toSVG socleDiagram Full))
D := socleDiagram(W, 2)
svg = toSVG D
assert(html D == svg)
assert(match("Complete socle filtration", svg))
assert(match("Layer 1", svg) and match("Layer 2", svg))
assert(match("L.0.", svg) and match("L.2.", svg))
assert(not match("<path ", svg))
///

TEST ///
-- Saving a diagram preserves the exact SVG/DOT, accepts literal filenames,
-- and never silently replaces an existing file.
G := weightGraph weylModuleZ(cartanA 2, {2})
D := socleDiagram(weylModuleZ(cartanA 2, {2}), 2)
for X in {G,D} do (
    svgFile := temporaryFileName() | " graph #&' image.svg";
    dotFile := temporaryFileName() | " graph #&' source.dot";
    assert(toSVG(X, svgFile) == toAbsolutePath svgFile);
    assert(toDot(X, dotFile) == toAbsolutePath dotFile);
    assert(get svgFile == toSVG X);
    assert(get dotFile == toDot X);
    assert(try (toSVG(X, svgFile); false) else true);
    assert(try (toDot(X, dotFile); false) else true);
    assert(get svgFile == toSVG X);
    assert(get dotFile == toDot X);
    removeFile svgFile;
    removeFile dotFile;
)
assert(try (toSVG(G, ""); false) else true)
assert(try (toDot(G, ""); false) else true)
-- These are literal filenames, never M2 process or socket specifications.
oldDirectory := currentDirectory()
exportDirectory := temporaryFileName() | "-graph-export/"
makeDirectory exportDirectory
changeDirectory exportDirectory
for name in {"!literal graph.svg", "$literal graph.svg"} do (
    result := toSVG(G, name);
    assert(first result == "/");
    assert(fileExists("./" | name));
    assert(get("./" | name) == toSVG G);
    assert(try (toSVG(G, name); false) else true);
    removeFile("./" | name);
)
changeDirectory oldDirectory
removeDirectory exportDirectory
///

TEST ///
-- An SVG warning must survive an ambiguous socle computation.
D := socleDiagram(weylModuleZ(cartanG2(), {3,0}), 2)
assert(not isCertified D)
svg := toSVG D
assert(match("Socle search may be incomplete", svg))
assert(not match("Layer 1 is the socle[.]", svg))
assert(not match("<path ", svg))
///
