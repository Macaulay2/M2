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
 -- Theorically it should run stably at around 22MB or less:
 -- it should just create a list of "desired" matrices, which should not appear 
 -- more than 10-20 times in 5^7 attempts.
 -- Instead, it happens that the RAM usage increases linarly in time.
 -- The function "collectGarbage" does not seem to help in this.


R=ZZ/5[a_1..a_(27),b_1..b_(45)]

	collectGarbage()
	collectGarbage()
	collectGarbage()
	k = # netRows engineHeap()
	k'= # netRows engineMemory()
        n = 10

scan(n,i -> rrr = random(R^1,R^{3:-1}))

	collectGarbage()
	collectGarbage()
	collectGarbage()
	K = # netRows engineHeap()
	K'= # netRows engineMemory()
	w = floor( 1/2 + (K-k)/n )
	w' = floor( 1/2 + (K'-k')/n )
	stderr << "average number of handles wasted per iteration : " << w << endl
	stderr << "average number of memory types wasted per iteration : " << w' << endl
	-- engineHeap
	-- engineMemory
	assert( w < 1 )
	assert( w' < 1 )
