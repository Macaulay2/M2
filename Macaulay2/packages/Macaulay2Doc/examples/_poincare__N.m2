R = ZZ/101[a,b,c, Degrees=>{1,1,2}];
C = res cokernel vars R
betti C
p = poincareN C
use ring p
substitute(p, {S=>-1})
