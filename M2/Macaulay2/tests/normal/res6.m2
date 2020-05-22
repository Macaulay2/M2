-- Date: Thu, 27 Nov 1997 04:11:50 +0100 (MET)
-- To: dan@math.uiuc.edu
-- Subject: Crash
-- 
-- Dear Dan, The Macaulay2 code in the next letter crashes
-- on the alpha machine sophie but works well on other platforms.
-- Best regards
-- Jan-Erik
-- 

-- 
-- 
-- R=ZZ/101[a..t];
-- N=genericMatrix(R,a,4,5);
-- J=minors(3,N);
-- Q=R/J;
-- M=coker vars Q;
-- time betti res (M,Strategy=>2,LengthLimit=>3)
-- time betti res (M,Strategy=>2,LengthLimit=>4)
-- time betti res (M,Strategy=>2,LengthLimit=>5)
-- time betti res (M,Strategy=>2,LengthLimit=>6)
-- 

-- hmm, these use a LOT of memory and time, so I've commented them out

R=ZZ/101[vars(0..5)]
N=genericMatrix(R,a,2,3);
J=minors(2,N);
Q=R/J;
M=coker vars Q;
time betti res (M,Strategy=>3,LengthLimit=>4) === new BettiTally from {
     (2,{2}) => 18, (3,{3}) => 40, (0,{0}) => 1, (4,{4}) => 81, (1,{1}) => 6}

R=QQ[vars(0..5)]
N=genericMatrix(R,a,2,3);
J=minors(2,N);
Q=R/J;
M=coker vars Q;
time betti res (M,Strategy=>3,LengthLimit=>4) === new BettiTally from {
     (2,{2}) => 18, (3,{3}) => 40, (0,{0}) => 1, (4,{4}) => 81, (1,{1}) => 6}


end

R=ZZ/101[a..t, MonomialSize=>8];
N=genericMatrix(R,a,4,5);
J=minors(3,N);
Q=R/J;
M=coker vars Q;
time betti res (M,Strategy=>2,LengthLimit=>3)
time betti res (M,Strategy=>3,LengthLimit=>4)
time betti res (M,Strategy=>2,LengthLimit=>5)
time betti res (M,Strategy=>2,LengthLimit=>6)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test res6.out"
-- End:
