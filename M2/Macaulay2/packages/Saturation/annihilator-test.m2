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

-- Tests added in the 2026 test-audit pass: annihilator stress.
-- The existing tests focus on the Tor-symmetry identity; these add direct
-- coverage on cyclic quotient modules, principal ideals, the unit module,
-- the cokernel of a row of variables, and the direct-sum identity.
TEST ///
  R = QQ[a,b,c]
  -- annihilator of a cyclic quotient module recovers the defining ideal
  I = ideal(a^2, b*c)
  assert(annihilator (R^1/I) == I)
  assert(annihilator (R^1/ideal(a^2, b^2, c^2)) == ideal(a^2, b^2, c^2))
  -- principal ideal
  assert(annihilator (R^1/ideal(a^2 - b^2)) == ideal(a^2 - b^2))
  -- the unit module has zero annihilator
  assert(annihilator R^1 == ideal 0_R)
  -- cokernel of a row of variables: annihilator is the variable ideal
  assert(annihilator coker matrix{{a,b,c}} == ideal(a,b,c))
  -- direct-sum annihilator equals the intersection of summand annihilators
  S = QQ[s,t]
  M = S^1/ideal s
  N = S^1/ideal t
  assert(annihilator(M ++ N) == intersect(annihilator M, annihilator N))
///
