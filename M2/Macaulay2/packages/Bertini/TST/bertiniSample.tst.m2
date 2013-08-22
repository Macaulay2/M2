needsPackage "Bertini"


---we will do a sample homotopy
     R = CC[x,y,z]
--F is the defining system of equations
     F = {(y^2+x^2+z^2-1)*x,
	  (y^2+x^2+z^2-1)*y}
--NV is a numerical variety  consisting of 2 components
     NV = bertiniPosDimSolve(F)
--W1 is the 1 dimensional component
--W2 is the 2 dimensional component
     W1 = NV#1_0 --z-axis
     W2 = NV#2_0      
     sample1=bertiniSample(W,10)
     sample2=bertiniSample(W,20)     
     assert(#sample1==10)
     assert(#sample2==20)
     

