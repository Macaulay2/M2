-- See https://github.com/Macaulay2/M2/issues/1689

-- printing nets
(err, out) = capture " << id_(ZZ^0) << id_(ZZ^3) << id_(ZZ^2) << id_(ZZ^1);"
assert(not err and out == "
i1 :  << id_(ZZ^0) << id_(ZZ^3) << id_(ZZ^2) << id_(ZZ^1);
0| 1 0 0 || 1 0 || 1 |
 | 0 1 0 || 0 1 |
 | 0 0 1 |
i2 : \n")

-- printing errors
(err, out) = capture "1/0"
assert(err and match("\\A\ni1 : 1/0\ncurrentString:1:2:\\(3\\):\\[.\\]: error: division by zero\n\\Z", out))

-- FIXME:
assert(instance(K, Symbol))
capture("K = ZZ/3", UserMode => false);
--assert(instance(K, Symbol))

-- FIXME
last capture("K = ZZ/101\nA = matrix\"1,2,3,4;1,3,6,10;19,7,11,13\" ** oo", UserMode => false)

