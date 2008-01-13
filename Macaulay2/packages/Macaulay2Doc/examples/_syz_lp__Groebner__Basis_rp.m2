R = QQ[a..g];
I = ideal"ab2-c3,abc-def,ade-bfg"
G = gb(I, Syzygies=>true);
syz G
syz gens I
mingens image syz G
