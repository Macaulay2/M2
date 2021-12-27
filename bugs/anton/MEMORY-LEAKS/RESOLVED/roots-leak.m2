-- this leaked memory
-- RESOLVED on 2021-12-13
R := CC[x,y];
S := CC[R_0];
toS = map(S,R,matrix{{S_0,1}})
for i from 1 to 10000 do (
    if i%100==0 then << "(" << i << ")" << flush; 
    if i%1000==0 then << endl;
    roots toS random(10,R); 
    )
