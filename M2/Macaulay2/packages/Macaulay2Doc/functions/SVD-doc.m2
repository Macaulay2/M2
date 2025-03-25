--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {SVD, (SVD,Matrix), (SVD,MutableMatrix)},
     Headline => "singular value decomposition of a matrix",
     Usage => "(S,U,Vt) = SVD M",
     Inputs => {
	  "M" => Matrix => {" over ", TO RR, " or ", TO CC, ", of size ", TT "m", " by ", TT "n"}
	  },
     Outputs => {
	  "S" => VerticalList => "the list of singular values",
	  "U" => Matrix => {"an orthogonal (unitary) matrix of size ", TT "m", " by ", TT "m"},
	  "Vt" => Matrix => {"an orthogonal (unitary) matrix of size ", TT "n", " by ", TT "n"},
	  },
     "If ", TT "Sigma", " is the diagonal ", TT "m", " by ", TT "n", " matrix whose ", TT "(i,i)", " entry is the 
     ", TT "i", "-th element of ", TT "S", ", then ", TT "M = U Sigma Vt", ".  This is the singular value decomposition 
     of ", TT "M", ".  The entries of ", TT "S", " are (up to roundoff error) the eigenvalues
     of the Hermitian matrix ", TT "M * (conjugate transpose M)",
     PARA{},
     "M may also be a ", TO MutableMatrix, " in which case the returned values
     ", TT "U", " and ", TT "Vt", " are also ", TO2(MutableMatrix, "mutable matrices"), ".",
     PARA{},
     "If ", TT "M", " is over ", TO "CC", ", then ", TT "U", " and ", TT "Vt", " are unitary matrices over ", TO "CC", ".
     If ", TT "M", " is over ", TO "RR", ", ", TT "U", " and ", TT "Vt", " are orthogonal over ", TT "RR", ".",
     EXAMPLE lines ///
     printingPrecision=2;
     M = map(RR^3, RR^5, (i,j) -> (i+1)^j * 1.0)
     (S,U,V) = SVD(M)
     M' = (transpose U) * M * (transpose V)
     ///,
     "We can clean the small entries from the result above with ", TO "clean", ".",
     EXAMPLE lines ///
     e = 1e-10;
     clean_e M'
     clean_e norm (1 - U * transpose U)
     ///,
     "Alternatively, if the only issue is display of the matrix, we may set the printing
     accuracy.",
     EXAMPLE lines ///
     printingAccuracy = 2
     M'
     ///,
     "Now let's try the divide and conquer algorithm and compare answers.",
     EXAMPLE lines ///
     (S', U', V') = SVD(M, DivideConquer => true)
     norm \ ({S', U', V'}-{S, U, V})
     ///,
     "The SVD routine calls on the SVD algorithms in the LAPACK and eigen libraries.",
     SeeAlso => {eigenvalues, eigenvectors, norm, clean, "printingAccuracy", "printingPrecision" },
     Subnodes => { TO [SVD, DivideConquer] },
     }
document { 
     Key => [SVD, DivideConquer],
     Headline => "whether to use the LAPACK divide and conquer SVD algorithm",
     Usage => "SVD(M, DivideConquer=>true)",
     "For large matrices, this algorithm is often much faster.",
     EXAMPLE {
	  "M = random(RR^200, RR^200);",
	  "time SVD(M);",
	  "time SVD(M, DivideConquer=>true);"
	  },
     SeeAlso => {}
     }
///
diag = method()
diag(ZZ,ZZ,List) := (a,b,L) -> (
     R := ring L#0;
     M := mutableMatrix(R,a,b);
     scan(#L, i -> M_(i,i) = L#i);
     matrix M)

M = random(RR^4, RR^7)
(S,U,Vt) = SVD M
M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt
assert(transpose U == U^-1)
transpose Vt - Vt^-1

M = random(RR^7, RR^4)
(S,U,Vt) = SVD M
M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt

M = random(CC^4, CC^7)
(S,U,Vt) = SVD M
M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt
transpose U
--U^-1 -- wrong!! BUG
-- U * U^-1

--assert(transpose U == U^-1) WRONG
--transpose Vt - Vt^-1

M = map(RR^3, RR^5, (i,j) -> (i+1)^j * 1.0)
(S,U,V) = SVD(M)
(transpose U) * M * (transpose V)
U^-1 == transpose U
(S1,U1,V1) = SVD(M, DivideConquer => true)
S1 == S
U1 == U
V1 == V

///
