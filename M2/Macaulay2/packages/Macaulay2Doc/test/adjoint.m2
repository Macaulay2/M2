-- * Usage:adjoint1(f,G,H)
-- * Inputs:
--     * f, a matrix, a homomorphism F --> G ** H between free modules
--     * G, a module, a free module
--     * H, a module, a free module
-- * Outputs:
--     * a matrix, the adjoint homomorphism F ** (dual G) --> H

R = QQ[x]
F = G = H = R^1
f = map(G**H,F,{{x}},Degree=>1)
assert isHomogeneous f
assert isHomogeneous adjoint1(f,G,H)

-- * Usage:adjoint(f,F,G)
-- * Inputs:
--     * f, a matrix, a homomorphism F ** G --> H between free modules
--     * F, a module, a free module
--     * G, a module, a free module
-- * Outputs:
--     * a matrix, the adjoint homomorphism F --> (dual G) ** H

f = map(H,F**G,{{x}},Degree => 1)
assert isHomogeneous f
assert isHomogeneous adjoint(f,F,G)
