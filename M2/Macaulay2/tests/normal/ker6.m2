--Date: Mon, 02 Oct 2006 15:02:32 -0400
--From: Sorin Popescu <sorin@math.sunysb.edu>
--To: dan@math.uiuc.edu, Michael Stillman <mike@math.cornell.edu>
--Subject: M2 kernel multi degrees
--Content-Type: text/plain; charset=ISO-8859-1; format=flowed
--Content-Transfer-Encoding: 7bit
--
--
--Dear Dan and Mike,
--
--  I have a problem with the M2 "kernel" command
--in a multi-graded  setting
--
--For example

R=ZZ/101[x,y, A]
P=ideal(R_0, R_1)
S=R/P
f = map(S, R)
ker f
use R
assert( ker f == ideal(x,y) )

---- produces the expect result
--
--o32 = ideal (x, y)
--
--o32 : Ideal of R
--
--
--however, once multi-degrees are in place, like in

clearAll()

R=ZZ/101[x,y, A, Degrees => {{1,0}, {1,0}, {2,1}}]
P=ideal(R_0, R_1)
S=R/P
f = map(S, R)
ker f
use R
assert( ker f == ideal(x,y) )

--
--ker yields
--
----error message bumped: Incorrect Hilbert function given
--stdio:35:1:(1):[0]: Incorrect Hilbert function given
--
--
--Could you please help me (and David) get around this?
--Thanks a lot!
--
--See you soon (in Banff and/or Minneapolis)!
--
--Best,
--
--                Sorin
--
--
--
