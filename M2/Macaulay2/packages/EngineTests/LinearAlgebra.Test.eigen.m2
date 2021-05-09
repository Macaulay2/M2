-- Test functionality of Eigen linear algebra

conjugate Matrix := (M) -> (
    L := entries M;
    map(target M, source M, L/(L1 -> L1/conjugate))
    )

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     (S,U,Vt) := SVD M;
     errM := norm(M - U * diagonalMatrix(numcols U, numrows Vt, S) * Vt);
     errU := norm((conjugate transpose U) * U - 1);
     errV := norm(Vt*(conjugate transpose Vt) - 1);
     prec := precision ring M;
     safety := 10; -- safety bits (heuristic)
     assert( 
	 errM < 2^(-prec+safety) 
	 and errU < 2^(-prec+safety) 
	 and errV < 2^(-prec+safety) 
	 )
     )

TEST ///
  debug needsPackage "EngineTests"
  KK = RR_53
  M = random(KK^20,KK^10) * random(KK^10,KK^30);
  checkSVD M
  KK = RR_2000
  M = random(KK^20,KK^10) * random(KK^10,KK^30);
  checkSVD M
  KK = CC_1000
  M = random(KK^10,KK^5) * random(KK^5,KK^12);
  checkSVD M
///
