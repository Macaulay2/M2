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

5) If there is genera of an ideal there should exist also
genus of an ideal!!!


																											  
FIXED 6) The following code

          R = QQ[x_0..x_4]
          a = {1,0,0,0,0}
          b = {0,1,0,0,1}
          c = {0,0,1,1,0}
          M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))
          M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))
          M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))
          M = M1 | M2 | M3
          betti (C=res coker M)
          dim coker M  -- it is a finite length module

          hilbertFunction(5, coker M) -- this is past the length

yields stdio:11:1:(1): unknown engine error

