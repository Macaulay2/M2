--R = Z/32003[s,t,a..d,Degrees=>{1,1,4,4,4,4},MonomialOrder => Eliminate 2]
--m = matrix{{a-s^4, b-s^3*t, c-s*t^3, d-t^4}}

test1 = () -> (
    S = Z/5[s,t];
    m = matrix{{s^4}};
    RS = Z/5[s,t,a..d,Degrees=>{1,1,4,4,4,4},MonomialOrder => Eliminate 2];
    f = ringmap(RS,S,matrix{{s,t}});
    );

test2 = () -> (
    S = Z/5[s,t];
    m = matrix{{s^4}};
    RS = Z/5[s,t,a..d];
    f = ringmap(RS,S,matrix{{s,t}});
    );

testold = () -> (
    R = Z/5[a..d];
    S = Z/5[s,t];
    f = matrix{{s^4, s^3*t, s*t^3, t^4}};
    RS = Z/5[s,t,a..d,Degrees=>{1,1,4,4,4,4},MonomialOrder => Eliminate 2];
    yvars = matrix{{a,b,c,d}};
    m = (vars RS)_(map(2,identity));
    --subst(f, m);
    );

R = ZZ/101[a..d]
I = monomialCurve(R,{1,3,4})
m = matrix entries gens gb I
gbTrace 3
C = res(coker m, SortStrategy=>2)
sendgg(ggPush C.Resolution, ggstats)
sendgg(ggPush C.Resolution, ggPush 1, ggresNmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 2, ggresNmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 3, ggresNmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 4, ggresNmap); getMatrix R

sendgg(ggPush C.Resolution, ggPush 1, ggresmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 2, ggresmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 3, ggresmap); getMatrix R
sendgg(ggPush C.Resolution, ggPush 4, ggresmap); getMatrix R
