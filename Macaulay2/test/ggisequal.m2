-- with CL under Windows this will break, but can be fixed by compiling
-- e/x_relem.cpp without optimization
R = ZZ/5[t]/(t^2+t+1);
f = (x,i) -> (sendgg(ggPush x, ggPush i, ggisequal); eePopBool());
assert( not f(2_R,3) )
assert( not f(2,3_R) )
