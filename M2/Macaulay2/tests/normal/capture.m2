-- See https://github.com/Macaulay2/M2/issues/1689

-- printing nets
(err, out) = capture " << id_(ZZ^0) << id_(ZZ^3) << id_(ZZ^2) << id_(ZZ^1);"
assert(not err and out == "
i1 :  << id_(ZZ^0) << id_(ZZ^3) << id_(ZZ^2) << id_(ZZ^1);
0| 1 0 0 || 1 0 || 1 |
 | 0 1 0 || 0 1 |
 | 0 0 1 |
i2 : \n")

(err, out) = capture("K = ZZ/101\nA = matrix\"1,2,3,4;1,3,6,10;19,7,11,13\" ** oo", UserMode => false);
assert(not err and match(regexQuote "
i1 : K = ZZ/101\n\no1 = K\n\no1 : QuotientRing\n
i2 : A = matrix\"1,2,3,4;1,3,6,10;19,7,11,13\" ** oo\n
o2 = | 1  2 3  4  |
     | 1  3 6  10 |
     | 19 7 11 13 |\n
             3       4
o2 : Matrix K  <--- K\n" | ".*", out))

-- printing errors
(err, out) = capture "1/0"
assert(err and match("\\A\ni1 : 1/0\ncurrentString:1:2:\\(3\\):\\[.\\]: error: division by zero\n\\Z", out))

-- make sure runaway existing private symbols are not changed
-- TODO: how can we prevent new methods or hooks from escaping capture?
assert(instance(K, Symbol))
capture("K = ZZ/3", UserMode => false);
assert(instance(K, Symbol))
