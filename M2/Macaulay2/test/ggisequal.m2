-- with CL under Windows this broke, but was fixed by
-- rearranging the code in cmd_Ring_isequal in e/x_relem.cpp
R = ZZ/5[t]/(t^2+t+1);
equal = (x,i) -> (sendgg(ggPush x, ggPush i, ggisequal); eePopBool());
assert not equal(2_R,3)
assert not equal(2,3_R)
assert not equal(3,2_R)
assert not equal(3_R,2)
assert equal(2_R,2)
assert equal(2,2_R)
assert equal(3,3_R)
assert equal(3_R,3)
-- Local Variables:
-- compile-command: "make ggisequal.okay"
-- End:
