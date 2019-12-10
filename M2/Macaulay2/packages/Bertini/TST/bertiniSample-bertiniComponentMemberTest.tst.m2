
restart
loadPackage("Bertini",Reload=>true)
peek Bertini
---we will do a sample homotopy
     R = CC[x,y,z]
--F is the defining system of equations
     F = {(y^2+x^2+z^2-1)*x*z,
	  (y^2+x^2+z^2-1)*y*z}
--NV is a numerical variety  consisting of 2 components
     BIC = {RandomSeed=>1}
     NV = bertiniPosDimSolve(F,BertiniInputConfiguration=>BIC)
--W1 is the 1 dimensional component
--W2 is the 2 dimensional component
     W1 = NV#1_0 --z-axis
     W2 = NV#2_0      
     W3 = NV#2_1
     sample1=bertiniSample(10, W1)
     sample2=bertiniSample(20, W3--,SHARPENDIGITS =>1e-100
	 )     
     assert(#sample1==10)
     assert(#sample2==20)

     assert( unique flatten bertiniComponentMemberTest(sample1, NV) === {W1} )
     assert(  bertiniComponentMemberTest({(1,1,1*ii)}, NV) === {{W3}}
     	 --bertiniComponentMemberTest(NV,sample2) -- a problem with precision of bertiniSample ???
	 )
     assert( 
	 set flatten bertiniComponentMemberTest({(0,0,1)}, NV) 
	 === set {W3,W1}
	 )

