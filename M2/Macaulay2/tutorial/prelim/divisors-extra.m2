---------------------------------
-- Extensions to the non S2 case
---------------------------------

-- Not too many changes are necessary for this case.
-- 
-- The main difficulty is that sheaf homomorphisms
-- O_X(-E) --> O_X(-F) correspond not to 
-- Hom(I,J), but to Hom(I_(>=d), J) for d 'sufficiently
-- large.
-- 
-- The rational quartic curve.
-- This is a smooth curve, but is not S2.

RC = Z/32003[a..d]
IC = monomialCurveIdeal({1,3,4},RC)
SC = RC/IC

-- Def. Define e(X) to be the degree of non-saturation
-- of S_X/f, where f is a non-zero linear form in S_X:
--    e(X) = min{d, (I_X,f) is equal to its saturation 
--                 in degrees >= d}


-- Prop. Suppose X is a smooth projective variety, and
-- L = O_X(D-E) is the sheaf corresponding to the
-- pair of ideals (I,J).  If we write Ie to be the
-- ideal I truncated in degrees >= e(X),
-- and choose f to be a non-zero element of Ie, then
-- H^0(L) is the degree zero part of
--
--   sat((f*J) : Ie)
--   ---------------
--          f
--
-- Prop. If f/g is a homomorphism as above, then
-- the zero scheme of the section is the ideal
--     sat(f*I : g) : J
--
