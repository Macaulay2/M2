implement this more generally, so it can handle denominators 

import = s -> getGlobalSymbol(User#"private dictionary",s) <- value Core#"private dictionary"#s;
import "rawGetPart";
import "raw";
S = QQ[a..d, Degrees => {{2,-1},{1,0},{0,1},{-1,2}}]
M = S^1/(b^2,b*c,c^2)
f = poincare M
A = ring f
f = (map(A,A,apply(gens A, x -> 1-x))) f
new A from rawGetPart({1,1},raw f,codim M,codim M)
