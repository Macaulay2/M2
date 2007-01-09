R = ZZ/101[a..d,Degrees=>{2:{1,0},2:{0,1}},Heft=>{1,1}];
I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
t = betti res I
peek t
betti(t,Weights=>{1,0})
betti(t,Weights=>{0,1})
t1 = betti(t,Weights=>{1,1})
peek t1
