--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {SVD, (SVD,Matrix), (SVD,MutableMatrix)},
     Headline => "singular value decomposition of a rectangular matrix over RR or CC",
     Usage => "(S,U,Vt) = SVD M",
     Inputs => {
	  "M" => Matrix => {" over ", TO RR, " or ", TO CC, ", of size m by n"}
	  },
     Outputs => {
	  "S" => Matrix => "over RR, the singular values, of size t by 1, where t = min(m,n)",
	  "U" => Matrix => "an orthogonal (unitary) matrix of size m by m",
	  "Vt" => Matrix => "an orthogonal (unitary) matrix of size n by n"
	  },
     "If Sigma is the m by n matrix whose (i,i) entry is the i th entry of S, and zero
     elsewhere, then M = U Sigma Vt.  This is the singular value decomposition 
     of M.  Note that the entries of S are (up to roundoff error) the eigenvalues
     of the Hermitian matrix M * (conjugate transpose M)",
     PARA{},
     "M may also be a ", TO MutableMatrix, " in which case the returned values
     S, U, Vt are also ", TO2(MutableMatrix, "mutable matrices"), ".",
     PARA{},
     "If M is over CC, then U and Vt are unitary matrices over CC.  If M is over RR, 
     U and Vt are orthogonal over RR.",
     EXAMPLE {
	  "M = map(RR^3, RR^5, (i,j) -> (i+1)^j * 1.0)",
	  "(S,U,V) = SVD(M)",
	  "(transpose U) * M * (transpose V)",
	  "U^-1 == transpose U",
	  "(S1,U1,V1) = SVD(M, DivideConquer => true)",
	  "S1 == S, U1==U, V1==V",
	  },
     "The SVD routine calls on the SVD algorithms in the lapack library.",
     SeeAlso => {eigenvalues, eigenvectors}
     }
document { 
     Key => [SVD, DivideConquer],
     Headline => "Use the lapack divide and conquer SVD algorithm",
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
