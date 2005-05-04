-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

1) There should exist a command like faceRing SimplicialCoomplex
 whcih returns the Stanley Reisner ring. Currently
 this works with StanleyReisnerRing.facering but
 it is not exported. Fix also documentation for dim
 accordingly

 2) codim AffineVariety should exist!!
 
 3) Why does there exist (codim,PolynomialRing) ??


 4) M2 yields zero and not expected result     
 EXAMPLE {
	  	  "projplane = Proj(ZZ/101[a,b,c])",
		  	  "R = ring projplane",
			   "FF = sheaf(kernel(matrix{{1,1,1}}*koszul(2,vars R)))",
			   "rank FF",
			    "nonfreelocus = sheafExt^2(FF,OO_projplane^1)"	  
									  }

