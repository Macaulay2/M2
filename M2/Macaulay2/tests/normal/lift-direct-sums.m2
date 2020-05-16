-- test added handling of direct summands to lift and promote
-- and added handling of lifting and promotion of (free) modules

-*

generateAssertions ///
R = QQ[x]
S = R[y,Join=>false]
T = S[z,Join=>false]
M = R^{-1,-1} ++ R^{-2}
N = T^{-1,-1} ++ T^{-2}
f = map(R^1,M,{{x,x,x^2}})
g = map(T^1,N,{{x,x,x^2}})
M.cache.components
N.cache.components
(source f).cache.components
(target f).cache.?components
(source g).cache.components
(target g).cache.?components
previous = promote(M,R)
previous.cache.components
previous = promote(M,S)
previous.cache.components
previous = promote(M,T)
previous.cache.components
previous = lift(N,T)
previous.cache.components
previous = lift(N,S)
previous.cache.components
previous = lift(N,R)
previous.cache.components
previous = promote(f,R)
(source previous).cache.components,(target previous).cache.?components
previous = promote(f,S)
(source previous).cache.components,(target previous).cache.?components
previous = promote(f,T)
(source previous).cache.components,(target previous).cache.?components
previous = lift(g,T)
(source previous).cache.components,(target previous).cache.?components
previous = lift(g,S)
(source previous).cache.components,(target previous).cache.?components
previous = lift(g,R)
(source previous).cache.components,(target previous).cache.?components
///

*-

     R = QQ[x]
     S = R[y,Join=>false]
     T = S[z,Join=>false]
     assert( (M = R^{-1,-1} ++ R^{-2}) === R^{{-1},{-1},{-2}} );
     assert( (N = T^{-1,-1} ++ T^{-2}) === T^{{-1},{-1},{-2}} );
     assert( (f = map(R^1,M,{{x,x,x^2}})) === map(R^1,R^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( (g = map(T^1,N,{{x,x,x^2}})) === map(T^1,T^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( (M.cache.components) === {R^{{-1},{-1}},R^{{-2}}} );
     assert( (N.cache.components) === {T^{{-1},{-1}},T^{{-2}}} );
     assert( ((source f).cache.components) === {R^{{-1},{-1}},R^{{-2}}} );
     assert( ((target f).cache.?components) === false );
     assert( ((source g).cache.components) === {T^{{-1},{-1}},T^{{-2}}} );
     assert( ((target g).cache.?components) === false );
     assert( (previous = promote(M,R)) === R^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {R^{{-1},{-1}},R^{{-2}}} );
     assert( (previous = promote(M,S)) === S^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {S^{{-1},{-1}},S^{{-2}}} );
     assert( (previous = promote(M,T)) === R^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {R^{{-1},{-1}},R^{{-2}}} );
     assert( (previous = lift(N,T)) === T^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {T^{{-1},{-1}},T^{{-2}}} );
     assert( (previous = lift(N,S)) === S^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {S^{{-1},{-1}},S^{{-2}}} );
     assert( (previous = lift(N,R)) === R^{{-1},{-1},{-2}} );
     assert( (previous.cache.components) === {R^{{-1},{-1}},R^{{-2}}} );
     assert( (previous = promote(f,R)) === map(R^1,R^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({R^{{-1},{-1}},R^{{-2}}},false) );
     assert( (previous = promote(f,S)) === map(S^1,S^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({S^{{-1},{-1}},S^{{-2}}},false) );
     assert( (previous = promote(f,T)) === map(T^1,T^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({T^{{-1},{-1}},T^{{-2}}},false) );
     assert( (previous = lift(g,T)) === map(T^1,T^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({T^{{-1},{-1}},T^{{-2}}},false) );
     assert( (previous = lift(g,S)) === map(S^1,S^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({S^{{-1},{-1}},S^{{-2}}},false) );
     assert( (previous = lift(g,R)) === map(R^1,R^{{-1},{-1},{-2}},{{x, x, x^2}}) );
     assert( ((source previous).cache.components,(target previous).cache.?components) === ({R^{{-1},{-1}},R^{{-2}}},false) );
