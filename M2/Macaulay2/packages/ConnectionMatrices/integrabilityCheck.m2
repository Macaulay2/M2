-- Basis List
--returns a list of length n with k -> 1 if k == i else 0
basisList := (i,n) -> (
    M := mutableMatrix(ZZ, 1, n); -- of zeros.
    M_(0,i) = 1;
    (entries M)_0
);

-- Differential Variable
-- for Weyl-algebra D and index i get dx_i in D
diffVariable = (D, i) -> D_(basisList((numgens D // 2) + i, numgens D));

-- Integrability Term
-- D Weyl algebra and A system of connection matrices, returns list of (i < j) -> [A_i, A_j] - (d_i(A_j) - d_j(A_i))
integrabilityTerm = (D, A) -> (
    apply(
        toSequence \ subsets(numgens D // 2, 2), (i,j) -> (
            commutator := A_i * A_j - A_j * A_i;
            dxi := diffVariable(D, i);
            dxj := diffVariable(D, j);
            dA := diffMatrixWeyl(dxi, A_j) - diffMatrixWeyl(dxj, A_i);
            dA - commutator
        )
    )
)

--------------------------------------------------------------------------
--isIntegrable: checks whether (d - A ∧ ) defines an integrable connection
--------------------------------------------------------------------------
isIntegrable = method()
isIntegrable (PolynomialRing, List) := Boolean => (D,A) -> all(integrabilityTerm(D,A), zero); -- Check that each term is zero.

-- Infering the polynomial base ring from A_0:
isIntegrable List := Boolean => A -> (
    assert(class(ring A_0) === FractionField);
    assert(class(baseRing ring A_0) === PolynomialRing);

    -- Recover the WeylAlgebra if it is stored, else construct.
    D := try (ring A_0)#"OriginalWeylAlgebra" else makeWeylAlgebra(baseRing ring A_0);
    assert(numgens D // 2 == #A); -- Length of the list must match the number of generators.
    isIntegrable(D, A)
)
