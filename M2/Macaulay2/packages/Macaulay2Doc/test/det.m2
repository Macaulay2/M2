RINGS = {GF 8,ZZ/101,QQ,QQ[x],RR_53,RR_100,CC_53,CC_100}
for K in RINGS do (
    M = matrix {{0,1_K},{1,0}};
    assert(det M == -1)
    )
end

restart
load "det.m2"
