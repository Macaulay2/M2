k = ZZ/3
Q = k[x]
R = Q/(x^3-1)
k' = R^1/(x-1)
f = map(R,Q)

try (
     M = pushForward(f,k');
     assert( M != 0 );
     ) else (
     stderr << "pushForward not implemented yet for inhomogeous maps" << endl;
     )

-- Local Variables:
-- compile-command: "make pushforward.okay"
-- End:
