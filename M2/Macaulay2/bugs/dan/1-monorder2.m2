vardegs = {8, 5, 5, 16, 8, 11, 6, 6, 11, 6, 13, 5, 8, 3, 3, 8, 3, 13, 5, 8, 3, 3, 8, 3, 15, 7, 10, 5, 5, 10, 5, 15, 7, 10, 5, 5, 10, 5, 15, 7, 10, 5, 5, 10, 5, 15, 7, 10, 5, 5, 10, 5}
w = {8, 5, 5}
wts = {15, 7, 10, 5, 5, 10, 5, 12, 4, 7, 2, 2, 7, 2, 12, 4, 7, 2, 2, 7, 2, 14, 6, 9, 4, 4, 9, 4, 14, 6, 9, 4, 4, 9, 4, 14, 6, 9, 4, 4, 9, 4, 14, 6, 9, 4, 4, 9, 4}

S = monoid [x,y,z, t_1 .. t_49, Degrees => vardegs, 
     MonomialOrder=>{Weights=>w, 3, Weights=>join(w,wts), 49},
     MonomialSize=>32];

-- The following is a simpler version
w = {1,1}
wts = {1,1}

S = monoid [x,y, a,b, 
     MonomialOrder=>{Weights=>w, 2, Weights=>join({0,0,0},wts), 2}]

-- The following is a simpler version
S = monoid [x,y, a,b, MonomialOrder=>{Weights=>{1,1}, 2, Weights=>{0,0,1,1}, 2}]
