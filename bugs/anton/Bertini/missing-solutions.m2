-- missing solutions in bertini 1.3.1
-- works in 1.4
restart
QQ[s,t]
I = ideal(588*s^3*t^2-588*t^5-588*s^3*t-882*s^2*t^2+1470*t^4+126*s^3+1170*s^2*t+378*s*t^2-1674*t^3-333*s^2-666*s*t+999*t^2+225*s-225*t,588*s^5-588*s^2*t^3-1470*s^4+882*s^2*t^2+588*s*t^3+1674*s^3-378*s^2*t-1170*s*t^2-126*t^3-999*s^2+666*s*t+333*t^2+225*s-225*t)
needsPackage "Bertini"

for i to 100 do (
    print i;
    V := bertiniPosDimSolve(I_*,RANDOMSEED=>i,FINALTOL=>1e-12);
    assert(# (realPoints flatten (components V / (W->W.Points))) == 4)
    )

     
     