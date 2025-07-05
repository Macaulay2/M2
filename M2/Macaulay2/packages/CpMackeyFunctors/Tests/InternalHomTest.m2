TEST ///

p := 5;
A := makeBurnsideMackeyFunctor p;
B := makeUnderlyingFreeMackeyFunctor p;
RO := makeRealRepresentationMackeyFunctor p;

assert(rank (InternalHom(A,B)).Underlying == rank Hom(A.Underlying, B.Underlying))
assert(rank (InternalHom(A,B)).Fixed == rank Hom(A, B))

assert(rank (InternalHom(B,RO)).Underlying == rank (B ** RO).Underlying)
assert(rank (InternalHom(B,RO)).Fixed == rank (B ** RO).Fixed)

///