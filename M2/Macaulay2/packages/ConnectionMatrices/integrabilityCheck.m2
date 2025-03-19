-- Basis List
--
-- Input:
-- i : Index
-- n : Length
--
-- Output:
-- List, k -> 1 if k == i else 0
basisList := (i,n) -> (
    M := mutableMatrix(ZZ, 1, n); -- of zeros.
    M_(0,i) = 1;
    (entries M)_0
);

-- Differential Variable
--
-- Input:
-- D : WeylAlgebra
-- i : Index
--
-- Output:
-- dx_i : Element in D
diffVariable = (D, i) -> D_(basisList((numgens D // 2) + i, numgens D));

-- Integrability Term
--
-- Input:
-- D : WeylAlgebra
-- A : PfaffianSystem
--
-- Output:
-- List, (i < j) -> [A_i, A_j] - (d_i(A_j) - d_j(A_i))
integrabilityTerm = (D, A) ->
    apply(
        toSequence \ subsets(numgens D // 2, 2), (i,j) -> (
            commutator := A_i * A_j - A_j * A_i;
            dxi := diffVariable(D, i);
            dxj := diffVariable(D, j);
            dA := diffMatrixWeyl(dxi, A_j) - diffMatrixWeyl(dxj, A_i);
            dA - commutator
        )
    )

-- Integrability Check
--
-- Input:
-- D : WeylAlgebra
-- A : PfaffianSystem
--
-- Output:
-- Boolean,  whether (d - A ∧ ) defines an integrable connection.
isIntegrable = method()
isIntegrable (PolynomialRing, List) := Boolean => (D,A) -> all(integrabilityTerm(D,A), zero); -- Check that each term is zero.

-- Infering the polynomial base ring from A_0:
isIntegrable List := Boolean => A -> (
    assert(class(ring A_0) === FractionField);
    assert(class(baseRing ring A_0) === PolynomialRing);

    R := baseRing ring A_0;
    assert((numgens R) == #A); -- Length of the list must match the number of generators.

    D := makeWeylAlgebra(R);
    isIntegrable(D, A)
)

end--
restart
needs "./integrabilityCheck.m2"

----------------------------------------------
-- Example Usage --

D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
A = connectionMatrices I;

assert(isIntegrable(D,A))

----------------------------------------------

-- If the Weyl Algebra is to be inferred
M = | 0            1               |, | (-1)/y    (-x)/y        |}
      | (-1)/(x2-xy) (-3x+y)/(x2-xy) |  | 1/(xy-y2) (x+y)/(xy-y2) |
