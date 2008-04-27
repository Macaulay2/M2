--status: Mike has been working again to improve the choice of algorithm to get this to work (it uses too much memory)

-- To: noreply@sourceforge.net
-- From: "SourceForge.net" <noreply@sourceforge.net>
-- Subject: [ macaulay2-Bugs-1571662 ] Memory leak in 0.9.20
-- Date: Thu, 05 Oct 2006 12:17:10 -0700
-- 
-- Bugs item #1571662, was opened at 2006-10-05 15:17
-- Message generated for change (Tracker Item Submitted) made by Item Submitter
-- You can respond by visiting: 
-- https://sourceforge.net/tracker/?func=detail&atid=740753&aid=1571662&group_id=138417
-- 
-- Please note that this message will contain a full copy of the comment thread,
-- including the initial issue submission, for this request,
-- not just the latest update.
-- Category: None
-- Group: None
-- Status: Open
-- Resolution: None
-- Priority: 5
-- Submitted By: Anders Buch (abuch)
-- Assigned to: Nobody/Anonymous (nobody)
-- Summary: Memory leak in 0.9.20
-- 
-- Initial Comment:
-- The following code runs ok with Macaulay-0.9.2, but
-- eats all the memory with version 0.9.20.  (I'm using
-- the precompiled binaries for linux.)
-- 
-- 

-- This actually runs in 0.9.8 on my intel mac MBP under rosetta in
-- about 19 minutes, and using virtual memory 1.43 G
-- Answer: three cubics, a CI.

R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33];

R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33, MonomialSize=>8];

f = map(R,R,{x11*y11+x12*y21, x11*y12+x12*y22, 
      x11*y13+x12*y23, x21*y11+x22*y21, x21*y12+x22*y22, 
      x21*y13+x22*y23, x31*y11+x32*y21,
      x31*y12+x32*y22, x31*y13+x32*y23,
      x13*y11+x12*y21+x13*y21+x11*y31+x12*y31+x13*y31,
      x13*y12+x12*y22+x13*y22+x11*y32+x12*y32+x13*y32,
      x13*y13+x12*y23+x13*y23+x11*y33+x12*y33+x13*y33,
      x23*y11+x22*y21+x23*y21+x21*y31+x22*y31+x23*y31,
      x23*y12+x22*y22+x23*y22+x21*y32+x22*y32+x23*y32,
      x23*y13+x22*y23+x23*y23+x21*y33+x22*y33+x23*y33,
      x33*y11+x32*y21+x33*y21+x31*y31+x32*y31+x33*y31,
      x33*y12+x32*y22+x33*y22+x31*y32+x32*y32+x33*y32,
      x33*y13+x32*y23+x33*y23+x31*y33+x32*y33+x33*y33});

time kernel(f);

-- 
-- 
-- 
-- 
-- ----------------------------------------------------------------------
-- 
-- You can respond by visiting: 
-- https://sourceforge.net/tracker/?func=detail&atid=740753&aid=1571662&group_id=138417

end
R = ZZ/32003[X_0..X_35, Degrees=>{18:1, 18:2}, MonomialOrder=>Eliminate 18, MonomialSize=>8]
I = ideal (-X_0*X_9-X_1*X_12+X_18,
     -X_0*X_10-X_1*X_13+X_19,
     -X_0*X_11-X_1*X_14+X_20,
     -X_3*X_9-X_4*X_12+X_21,
     -X_3*X_10-X_4*X_13+X_22,
     -X_3*X_11-X_4*X_14+X_23,
     -X_6*X_9-X_7*X_12+X_24,
     -X_6*X_10-X_7*X_13+X_25,
     -X_6*X_11-X_7*X_14+X_26,
     -X_2*X_9-X_1*X_12-X_2*X_12-X_0*X_15-X_1*X_15-X_2*X_15+X_27,
     -X_2*X_10-X_1*X_13-X_2*X_13-X_0*X_16-X_1*X_16-X_2*X_16+X_28,
     -X_2*X_11-X_1*X_14-X_2*X_14-X_0*X_17-X_1*X_17-X_2*X_17+X_29,
     -X_5*X_9-X_4*X_12-X_5*X_12-X_3*X_15-X_4*X_15-X_5*X_15+X_30,
     -X_5*X_10-X_4*X_13-X_5*X_13-X_3*X_16-X_4*X_16-X_5*X_16+X_31,
     -X_5*X_11-X_4*X_14-X_5*X_14-X_3*X_17-X_4*X_17-X_5*X_17+X_32,
     -X_8*X_9-X_7*X_12-X_8*X_12-X_6*X_15-X_7*X_15-X_8*X_15+X_33,
     -X_8*X_10-X_7*X_13-X_8*X_13-X_6*X_16-X_7*X_16-X_8*X_16+X_34,
     -X_8*X_11-X_7*X_14-X_8*X_14-X_6*X_17-X_7*X_17-X_8*X_17+X_35)
gbTrace=3
hf = poincare ideal(X_18..X_35)
time gb(I, Hilbert=>hf, DegreeLimit=>10);
time gb(I, Hilbert=>hf);

end
