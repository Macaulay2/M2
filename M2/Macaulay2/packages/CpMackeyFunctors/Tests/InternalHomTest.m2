TEST ///

p := 5;
A := makeBurnsideMackeyFunctor p;
B := makeUnderlyingFreeMackeyFunctor p;
RO := makeRealRepresentationMackeyFunctor p;

assert(rank (internalHom(A,B)).Underlying == rank Hom(A.Underlying, B.Underlying))
assert(rank (internalHom(A,B)).Fixed == rank Hom(A, B))

assert(rank (internalHom(B,RO)).Underlying == rank (B ** RO).Underlying)
assert(rank (internalHom(B,RO)).Fixed == rank (B ** RO).Fixed)
///