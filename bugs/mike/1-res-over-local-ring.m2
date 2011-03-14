- resolutions over local rings could be minimal:
- we might want to distinguish local rings from polynomial rings that happen
  to have a local ordering:

Date: Mon, 23 Jun 2008 10:26:47 -0700 (PDT)
Subject: [Macaulay2] minimal resolutions over local rings
From: Edward Carter <edward.carter@gmail.com>
To: Macaulay2 <macaulay2@googlegroups.com>
Reply-To: macaulay2@googlegroups.com


Is there currently a way to have Macaulay2 produce a minimal
resolution over a local ring?  For example, given this code,

Q = newRing(ZZ/19[x,y], Local => true)
M = coker matrix{{x^2+y},{y^3-x*y}}
res(M ** Q^1/(x^5, y^5))

I would like the output of the last statement to be killed by
tensoring with Q^1/(x,y), but it isn't.
