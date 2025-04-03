-- Basis List
--returns a list of length n with k -> 1 if k == i else 0
basisList := (i,n) -> (
    M := mutableMatrix(ZZ, 1, n); -- of zeros.
    M_(0,i) = 1;
    (entries M)_0
)

-- Differential Variable
-- for Weyl-algebra D and index i get dx_i in D
diffVariable = (D, i) -> D_(basisList((numgens D // 2) + i, numgens D));

-- Integrability Term
-- D Weyl algebra and A system of connection matrices, returns list of (i < j) -> [A_i, A_j] - (d_i(A_j) - d_j(A_i))
integrabilityTerm = (D, A) -> (
    R := ring A#0;
    apply(
        toSequence \ subsets(numgens D // 2, 2), (i,j) -> (
            commutator := A_i * A_j - A_j * A_i;
            dxi := diffVariable(D, i);
            dxj := diffVariable(D, j);
            dA := diffRationalWeyl(dxi, A_j) - diffRationalWeyl(dxj, A_i);
            sub(dA, R) - commutator
        )
    )
)

--------------------------------------------------------------------------
--isIntegrable: checks whether (d - A ∧ ) defines an integrable connection
--------------------------------------------------------------------------
isIntegrable = method()
isIntegrable(PolynomialRing, List) := Boolean => (D, A) -> (
    all(integrabilityTerm(D, A), zero)) -- check that each term is zero
isIntegrable List := Boolean => A -> (
    -- Infers the WeylAlgebra if stored as option.
    D := inferWeylAlgebra(ring A_0);
    if not numgens D // 2 == #A then error "length doesn't match number of differential variables.";
    isIntegrable(D, A)
)
