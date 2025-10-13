TEST ///
needsPackage "CpMackeyFunctors"

-- Check that the identity has trivial kernel and cokernel.
A = makeBurnsideMackeyFunctor(5);
K = (ker id_A, coker id_A);
for i to 1 do assert( all( {(K_i).Trans, (K_i).Res, (K_i).Fixed, (K_i).Underlying,
	                    (K_i).Conj}, zero))

-- A non-trivial map between Mackey functors.
B = makeBurnsideMackeyFunctor 2;
U = makeUnderlyingFreeMackeyFunctor 2;
f = map(U, B, matrix {{2},{2}}, matrix {{2,4}});
K' = ker f;
assert( all({(K').Underlying, (K').Conj, (K').Res, (K').Trans}, zero) )
assert( gens (K').Fixed  == matrix {{2},{-1}} )
assert( rank (K').Fixed == 1 )
C = coker f;
assert( presentation C.Fixed      == matrix {{2,4}} )
assert( presentation C.Underlying == matrix {{2},{2}} )

-- Checking that direct sums are well-defined.
assert( class(B ++ U)  === CpMackeyFunctor)
assert( class(directSum {B,B,B,B}) === CpMackeyFunctor )
///