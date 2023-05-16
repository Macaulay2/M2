-- From an email of Dan 2 Oct 2006
M = monoid [x,y,MonomialOrder => { GRevLex => {}, GRevLex => {1,1} }, Global => false ]
R = (ZZ/101) M
assert(x_R == R_0)
debug Core

-- These are correct, as they should be
R = ZZ/101 [x,y,MonomialOrder => { Weights => {1}}] -- this one is OK
R = ZZ/101 [a,b,c,d,MonomialOrder => { Weights => 3:1}] -- this one is allowed
R = ZZ/101 [x,y,MonomialOrder => { GRevLex => {}, GRevLex => {1,1}}, Weights => {1,2}] -- OK
R = ZZ/101 [x,y,MonomialOrder => { GRevLex => {}, GRevLex => {1,1}, Weights => {} }, Global => false ] -- OK

-- These should be fixed (note: sometimes 'Weights' is in MonomialOrder, sometimes outside, in these examples)
R = ZZ/101 [x,y,MonomialOrder => { Weights => {{1,3},{-4,-1}}}] -- perhaps this should be allowed?  The front end could expand it to two weights
R = ZZ/101 [a,b,c,d,MonomialOrder => { Weights => {3:1}}] -- perhaps this should be allowed?
R = ZZ/101 [x,y,MonomialOrder => { Weights => {1,2,3}}] -- perhaps this should be an error message, if the length of weights is > nvars
R = ZZ/101 [x,y,MonomialOrder => { GRevLex => {}, GRevLex => {1,1}}, Weights => {1,2,3,5}] -- error message wrong
R = ZZ/101 [x,y,MonomialOrder => { GRevLex => {}, GRevLex => {1,1}, Weights => {1,2,3} }] -- allowed, but perhaps should not be.  Also, weight vector comes out wrong

raw R
