-- * Usage:adjoint'(f,G,H)
-- * Inputs:
--     * f, a matrix, a homomorphism F --> Hom(G,H) between free modules
--     * G, a module, a free module
--     * H, a module, a free module
-- * Outputs:
--     * a matrix, the adjoint homomorphism F ** G --> H

R = QQ[x]
F = G = H = R^1
f = map(G**H,F,{{x}},Degree=>1)
assert isHomogeneous f
f' = adjoint'(f,dual G,H)
assert isHomogeneous f'
assert (source f' === F ** dual G)
assert (target f' === H)

F = R^{2}
G = R^{3}
H = R^{7}
f = map(G**H,F,{{x}},Degree=>-7)
assert isHomogeneous f
f' = adjoint'(f,dual G,H)
assert isHomogeneous f'
assert (source f' === F ** dual G)
assert (target f' === H)

-- * Usage:adjoint(f,F,G)
-- * Inputs:
--     * f, a matrix, a homomorphism F ** G --> H between free modules
--     * F, a module, a free module
--     * G, a module, a free module
-- * Outputs:
--     * a matrix, the adjoint homomorphism F --> (dual G) ** H

F = G = H = R^1
f = map(H,F**G,{{x}},Degree => 1)
assert isHomogeneous f
assert isHomogeneous adjoint(f,F,G)

F = R^{2}
G = R^{3}
H = R^{7}
f = map(H,F**G,{{x}},Degree => -1)
assert isHomogeneous f
f' = adjoint(f,F,G)
assert isHomogeneous f'
assert (source f' === F)
assert (target f' === dual G ** H)
