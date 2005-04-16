R = ZZ/101[x,y,z,w]
I = ideal(x*y-z^2,y^2-w^2,w^4)
gb(I,SyzygyLimit => 1, Syzygies => true)
syz oo
gb(I,SyzygyLimit => 2, Syzygies => true)
syz oo
gb(I,SyzygyLimit => infinity, Syzygies => true)
syz oo
