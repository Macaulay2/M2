----------------------
--INPUT: matrixSchubertRegADI, takes a permutation in 1-line notation
--OUTPUT: returns the Castelnuovo-Mumford reguarity of the matrix 
--        Schubert variety by computing the regularity of the antidiagonal initial ideal
------------------------------------------
-- matrixSchubertRegADI = method()
-- matrixSchubertRegADI List := ZZ => (w) -> (
--     if not (isPerm w) then error ("Expecting a permutation.");
    
--     I := antiDiagInit w;
--     if I == 0 then return 0;
--     return regularity(I) -1;   
-- )

schubertRegularity = method()
schubertRegularity List := ZZ => w -> (
     if not (isPerm w) then error ("The input must be a partial alternating sign matrix or a permutation.");
     rajIndex(w) - permLength(w)
)
schubertRegularity Matrix := ZZ => A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    --Check if Matrix is a partial permutation matrix, and if it is, use rajindex formula.
    w := toOneLineNotation partialASMToASM A;
    if (w != {}) then return rajIndex(w) - permLength(w);
    --Otherwise compute regularity of its antidiagonal initial ideal
    I := antiDiagInit A;
    if I == 0 then return 0;
    regularity(I) -1
);

schubertCodim = method() 
schubertCodim Matrix := ZZ => A -> (
    if not (isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    codim antiDiagInit A
)
schubertCodim List := ZZ => w -> (
    if not (isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    permLength w
)


--------------------------------------------------
--**KPolynomialASM**-
--input: a partial ASM A
--output: the Kpolynomial of ASM variety for A
     --using multidegree where variables are indexed along rows
--TODO: add option for doubly graded
--------------------------------------------------
KPolynomialASM = method()
KPolynomialASM Matrix := ZZ => A -> (
    I := schubertDeterminantalIdeal(A,CoefficientRing=>ZZ/2);
    R := ring I;
    kk := coefficientRing R;
    possibleDegs := apply(numrows A, i-> toList insert(i,1,(numrows(A)-1):0));
    degs := splice apply(possibleDegs, i->(numrows(A):i));
    Q := kk[R_*, Degrees => degs];
    numerator hilbertSeries sub(I,Q)
);

----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation or an ASM ideal
--OUTPUT: whether or not R/I_A is CM
---------------------------------------
isSchubertCM = method()
isSchubertCM Matrix := Boolean => A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    R:=ring(antiDiagInit A);
    codim(antiDiagInit A)==pdim(comodule (antiDiagInit A))
    );

isSchubertCM List := Boolean => w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    print "We know from a theorem of Fulton that the quotient by any Schubert determinantal ideal is actually Cohen--Macaulay!";
    true
    );
