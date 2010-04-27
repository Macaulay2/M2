needsPackage "Puiseux"
makeExample = (R, n, L, K, coeffs) ->(
     --R is the base ring QQ[x,y] (or any polynomial ring in 2 vars)
     --n is an integer, the lcm of the a,b,c
     --L is the list of "puiseux exponents" m0,m1,..,ms, positive integers.
     --K is an extension field of QQ
     --coeffs is a list of elements of K
     --The routine constructs the generator of the kernel of
     --QQ[x,y] --> K[t]
     --sending x to t^n and y to a1 t^m1(1+a2 t^m2(1+...))
     S = K[t];
     p :=0_S;
     s := #L-1;
     for j from 0 to s do p = coeffs_(s-j)*t^(L_(s-j))*(1+p);     
     print {t^n, p};
     I := ker map(S,R, {t^n, p});
     (entries gens I)#0#0
     )

end


uninstallPackage "Puiseux"
installPackage "Puiseux"
viewHelp Puiseux
--loadPackage "Puiseux"

restart
load "puiseux-test.m2"

R = QQ[x,y]
L = {1,1,2,3,4,5,8}
coeffs = {1,1,1,1,1,1,1}
F = makeExample(R,8,L,QQ,coeffs)
pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))

K = toField QQ[z,w]/ideal(z^3-1)
F = makeExample(R,4,{1,2},K,{z,z^2+1})
pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))


K = toField QQ[z,w]/ideal(z^3-1,w^2-2)
F = makeExample(R,4,{1,2},K,{z,w})
pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))


--this one is harder!
K = toField QQ[z,w]/ideal(z^3-1,w^2-2)
time F = makeExample(R,4,{1,2,1},K,{z,w,1})
pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))


--this one is harder!
K = toField QQ[z,w]/ideal(z^3-1,w^2-2)
time F = makeExample(R,4,{1,2,1},K,{z,w,z+w}) -- this takes 97 seconds!
time pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))


--this one is hard:
K = toField QQ[z,w]/ideal(z^3-1,w^2-2)
time F = makeExample(R,4,{1,2,1,3},K,{z,w,w+z,w^2*z})
pF = puiseux(F, 30)
m = #pF
for i from 0 to m-1 do print (testPuiseux(pF_i,F,30))

