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
R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33];

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

kernel(f)
-- 
-- 
-- 
-- 
-- ----------------------------------------------------------------------
-- 
-- You can respond by visiting: 
-- https://sourceforge.net/tracker/?func=detail&atid=740753&aid=1571662&group_id=138417
