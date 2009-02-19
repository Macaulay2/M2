-- kernel(RingMap) depends on the generators of the graph ideal all being "good"
assert( numgens graphIdeal map(QQ[x],QQ) == 0 )
assert( numgens graphIdeal map(QQ[x],QQ[]) == 0 )
