-- 10/3/2010 this resulting matrix is auto-reduced.
--M = random(QQ^3,QQ^4)

M = random(QQ^5,QQ^4)
assert((gens gb M)^{1..4} == 1)
