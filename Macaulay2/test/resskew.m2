n=6
kk=ZZ/101;
E=kk[y_0..y_n,SkewCommutative=>true]
A=matrix{{y_1*y_6},{y_2*y_5},{y_3*y_4}}
n=matrix{{y_1*y_2*y_4,y_2*y_3*y_5,y_3*y_4*y_6,y_4*y_5*y_0,y_5*y_6*y_1,y_6*y_0*y_2,y_0*y_1*y_3},
         {y_3*y_5*y_6,y_4*y_6*y_0,y_5*y_0*y_1,y_6*y_1*y_2,y_0*y_2*y_3,y_1*y_3*y_4,y_2*y_4*y_5}}
betti(fn=res(image n,LengthLimit=>4))
betti(fnt=res(image transpose n,LengthLimit=>4))
bb = betti(fc=res(image fnt.dd_2,LengthLimit=>2))
cc = betti(fc=res(ker fnt.dd_1,LengthLimit=>2))
assert ( bb == cc )
