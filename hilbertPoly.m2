hilbertPolynomial ZZ := ProjectiveHilbertPolynomial => o -> (M) -> ( 
    new ProjectiveHilbertPolynomial from {0 => M}
    )
ProjectiveHilbertPolynomial == ZZ := (M,N) -> (M == hilbertPolynomial N)
ZZ == ProjectiveHilbertPolynomial := (M,N) -> (hilbertPolynomial M == N)
ProjectiveHilbertPolynomial + ZZ := (P, N) -> P + hilbertPolynomial N
ZZ + ProjectiveHilbertPolynomial := (P,N) -> hilbertPolynomial P + N
ProjectiveHilbertPolynomial - ZZ := (P, N) -> P - hilbertPolynomial N
ZZ - ProjectiveHilbertPolynomial := (P,N) -> hilbertPolynomial P - N
ZZ * ProjectiveHilbertPolynomial := (P,N) -> hilbertPolynomial P * N
ProjectiveHilbertPolynomial * ZZ := (P,N) -> P * hilbertPolynomial N
