R = QQ[a..f]
M = genericSymmetricMatrix(R,a,3)
I = minors(2,M)
G = gb(I, PairLimit=>5)
mingens G
mingens I
