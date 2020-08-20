assert ( gens gb matrix {{1/1,2},{2,1}} == gens gb matrix {{1/1,0},{0,1}} )


gens gb matrix {{1/1,2},{2,1}}
random(QQ^10, QQ^10)
gens gb oo

random(QQ^10, QQ^10)
assert(gens gb oo == id_(QQ^10))

random(QQ^20, QQ^10)
gens gb oo

random(QQ^10, QQ^20)
assert(gens gb oo == id_(QQ^10))
