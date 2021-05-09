-- moved from Macaulay2/tests/normal/ann.m2 and ann2.m2
-- Date: Sat, 22 Apr 2000 14:09:05 -0700
-- From: David Eisenbud <de@msri.org>
-- To: Dan Grayson <dan@math.uiuc.edu>, Mike Stillman <mike@math.cornell.edu>, Craig Huneke <huneke@math.ukans.edu>
-- Subject: bug in Tor?
-- Reply-To: de@msri.org
--
-- Dear Mike and Dan,
--
--         The bugs below have been giving me grief in some
-- work that Craig Huneke and I are trying to do.
--         Help, please!

TEST ///
  kk=QQ
  S=kk[x_0..x_4]
  R=S/(ideal(x_0,x_1)*ideal(x_2,x_3))
  J=ideal vars R
  M=R^1/J
  d=3
  N=(R^1)/(J^d)
  assert( annihilator Tor_1(M,N) == annihilator Tor_1(N,M) )
///

TEST ///
  kk=QQ
  S=kk[x_0..x_3]
  R=S/ideal(x_0*x_1-x_2*x_3)
  J=ideal vars R
  M=R^1/J
  d=3
  N=(R^1)/(J^d)
  assert( annihilator Tor_1(M,N)==  annihilator Tor_1(N,M) )
///
