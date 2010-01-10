
--------input----------------------------------------------------

----------------------------------------------------------------
q=23;
degr=7;
Rq=ZZ/q[y,x,
          MonomialOrder=>{
	  Weights=>{11,7},
          Weights=>{1,0}}];

depq={y};
indq={x};
deps=#depq;
inds=#indq;
footq=apply(degr,i->y^i);     

Iq = ideal(
y^7+y^6*(3*x+1)+y^5*(3*x^3+6*x^2)+y^4*9*x^4+y^3*(4*x^6-x^5)-3*y^2*x^7-3*y*x^9-x^11
);
-----------------------------------------------------------------

end
restart
load "example_3_2.m2"
needsPackage "QthPower"
gbTrace=3
qthPower(Iq, deps, footq)
