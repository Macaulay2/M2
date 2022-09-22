-- version 0.9.5 doesn't keep track of memory any longer, so we can't check for memory leaks
end

 -- From: Fabio Tonoli <Fabio.Tonoli@uni-bayreuth.de>
 -- To: Daniel R Grayson <dan@math.uiuc.edu>
 -- Subject: M2 bugs..
 -- Date: Wed, 6 Feb 2002 16:03:57 +0100
 -- Cc: Frank Schreyer <schreyer@btm8x5.mat.uni-bayreuth.de>
 -- 
 -- Dear Daniel,
 -- I have the following problems with M2 to report. 
 -- Let me know,
 -- Fabio.
 -- 
 -- 
 -- 1. memory gets lost somewhere in this program:
 -- it should run random searches for 5^7 times, instead of the 1000 written 
 -- below. 5^7 trials would require 10-20 hours on a P4 1.5G (with 1000 just 10 
 -- minutes).
 -- The problem is then that after 20 hours the memory use reaches 800MB....and 
 -- then M2 exits (we have 1GRAM).
 -- Theoretically it should run stably at around 22MB or less:
 -- it should just create a list of "desired" matrices, which should not appear 
 -- more than 10-20 times in 5^7 attempts.
 -- Instead, it happens that the RAM usage increases linearly in time.
 -- The function "collectGarbage" does not seem to help in this.


kk=ZZ/5
R=kk[a_1..a_(27),b_1..b_(45)]
A=genericMatrix(R,a_1,3,9)
B=genericSymmetricMatrix(R,b_1,9)
b=genericMatrix(R,b_1,1,45);
C=transpose diff(transpose b,flatten( A*B));
RA=kk[a_1..a_(27)]
C=substitute(C,RA);
A=substitute(A,RA);
b=symbol b
RB=kk[b_1..b_(45)]
B=substitute(B,RB)
S=kk[t_0..t_3]
Ap=random(S^1,S^{27:-1});
As=substitute(A,Ap);
expectedSyzygies=betti  res coker As
Cs=substitute(C,Ap);
expectedSymmetricSections=betti syz( Cs,DegreeLimit=>1)

Ap=random(S^1,S^{27:-1});
As=substitute(A,Ap);
betti res coker As
example=Ap

	collectGarbage()
	collectGarbage()
	collectGarbage()
	k = # netRows engineHeap()
	k'= # netRows engineMemory()
        n = 40

f = i->(
     if i%20==0 then collectGarbage();
     Ap=random(S^1,S^{27:-1});
     As=substitute(A,Ap);
     if betti res coker As == expectedSyzygies
     then (
  	  Cs=substitute(C,Ap);
	  bet=betti syz(Cs,DegreeLimit=>1);
  	  if expectedSymmetricSections == bet 
	  then ( "expected Sections" )
	  else (
	       example=example||Ap;
	       (bet,i)
	       )
	  )
     else "wrong syzygies"
     )

scan(n,f)

	collectGarbage()
	collectGarbage()
	collectGarbage()
	K = # netRows engineHeap()
	K'= # netRows engineMemory()
	stderr << "total number of handles wasted : " << K-k << endl
	stderr << "total number of memory types wasted : " << K'-k' << endl
	w = floor( 1/2 + (K-k)/n )
	w' = floor( 1/2 + (K'-k')/n )
	stderr << "average number of handles wasted per iteration : " << w << endl
	stderr << "average number of memory types wasted per iteration : " << w' << endl
	-- engineHeap
	-- engineMemory
	assert( w < 1 )
	assert( w' < 1 )
