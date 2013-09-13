needsPackage "Bertini"

---we will do a sample homotopy
     R = CC[x,y,z]
--F is the defining system of equations
     F = {(y^2+x^2+z^2-1)*x*z,
	  (y^2+x^2+z^2-1)*y*z}
--NV is a numerical variety  consisting of 2 components
     NV = bertiniPosDimSolve(F,RANDOMSEED=>1)
--W1 is the 1 dimensional component
--W2 is the 2 dimensional component
     W1 = NV#1_0 --z-axis
     W2 = NV#2_0      
     W3 = NV#2_1
     sample1=bertiniSample(W1,10)
     sample2=bertiniSample(W3,20--,SHARPENDIGITS =>1e-100
	 )     
     assert(#sample1==10)
     assert(#sample2==20)

     assert( unique flatten bertiniComponentMemberTest(NV,sample1) === {W1} )
     assert(  bertiniComponentMemberTest(NV,{(1,1,1*ii)}) === {{W3}}
     	 --bertiniComponentMemberTest(NV,sample2) -- a problem with precision of bertiniSample ???
	 )
     assert( 
	 set flatten bertiniComponentMemberTest(NV,{(0,0,1)}) 
	 === set {W3,W1}
	 )

