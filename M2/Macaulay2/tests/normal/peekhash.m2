debug PrimaryDecomposition -- to get private function "flatt"
R = ZZ/3[x,y,u,s,t]
I = ideal( x^27, y^27, u^27, u^5-x*y*(x-y)*(s*x-t*y))
C = flatt(I,s*t)
keys C
(values C)/print;
peek C
