--From: 	Jenna Rajchgot <jrajchgot@math.cornell.edu>
--Subject: 	Re: M2 integralClosure question
--Date: 	March 13, 2010 10:26:31 PM EST
--To: 	Michael Stillman <mike@math.cornell.edu>

-- uses oneStep3.m2
restart
loadPackage"oneStep3"
R = ZZ/32003[a,b,c,d]
J = ideal "a(d2a+c2-bdc)"
L=decompose J
iteration(R,L,L_0)

(oldI,div,icR, icId,f) = iteration(R,L,L_0) 
iteration(oo_2,oo_1,(oo_1)_0)
