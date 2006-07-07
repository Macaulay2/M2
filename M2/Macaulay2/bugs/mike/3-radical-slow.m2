-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8),MonomialSize=>8]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
I1 = ideal(I_0,I_1,I_2)
codim I1
time radical(I, CompleteIntersection=>I1)

-- Local Variables:
-- M2-send-to-buffer: "*gud*"
-- End:
