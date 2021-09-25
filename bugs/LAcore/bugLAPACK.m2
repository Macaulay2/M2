testRoutine = method(Options => true)
testRoutine (ZZ, ZZ, Ring, Function) := List => true >> opts -> (m, n, k, f) -> (
    apply(m, i -> (
	    A := random(k^n, k^n);
	    if opts.?Hermitian and opts.Hermitian then A = A + matrix table(n, n, (i,j) -> conjugate A_(j,i));
	    elapsedTime (A, f(A, opts))
	    ))
    )
end
restart
load "bugLAPACK.m2"
setRandomSeed 1
L = testRoutine(10, 200, CC_53, eigenvectors); -- this is likely to crash in 1.17 ...
residuals = apply(L, p -> (A = p#0; (E, V) = p#1; elapsedTime norm(A*V - V*diagonalMatrix E))) -- ... or give incorrect result
assert all(residuals, r -> r < 1e-12)
