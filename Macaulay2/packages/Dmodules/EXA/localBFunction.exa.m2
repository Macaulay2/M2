loadPackage("Dmodules", FileName=>"../../Dmodules.m2");
--load "../localBFunction.m2"

QQ[x,y,z];
f = x^2 + y^2 + z^2;
f = x^3 + x*y^3 + z^2;
f = x^3 + z^2*y^2; P = ideal(x+1,y-1,z-1);
f = (x^3 + z^2*y^2)^2; P = ideal(x+1,y-1,z-1);
f = (x^3 + z^2*y^2)^3; P = ideal(x+1,y-1,z-1);

-- Nakayama examples (from Oaku 1997)
f = x*(x+y+1); P = ideal(x,y,z);
f = x^6 + y^4 + z^3; P = ideal(x,y,z);
--f = y*((y + 1)*x^3 - y^2*z^2); P = ideal(x,y,z);

factorBFunction globalBFunction f
b = localBFunction(f,P)
factorBFunction b

end
restart
load "localBFunction.exa.m2"
debug Dmodules
strata = localBFunctionStrata f  
print apply(keys strata, k->k=>strata#k)   
Dtrace 10

