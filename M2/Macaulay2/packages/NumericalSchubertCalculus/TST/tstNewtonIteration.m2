debug needsPackage "NumericalSchubertCalculus"
n = 4;
k = 2;

Pblm = {
    ({1},id_(FFF^4)),
   ({1},rsort id_(FFF^4)), 
   ({1},sub(transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}},FFF)),
   ({1},sub(transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}},FFF))
   };

Sols = {matrix {{1, 0}, { 1.57735026918965, 0}, {0,.788675134594813}, {0, 1}}, 
    matrix {{1, 0}, {.422649730810372,0}, {0,  .211324865405188}, {0, 1}}};

checkNewtonIteration(Sols, Pblm,(2,4))

end

restart
load "tstNewtonIteration.m2"
