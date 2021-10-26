-- Kaie Kubjas, 2017
FF[p_(0,0,0), p_(0,0,1), p_(0,1,0), p_(0,1,1), p_(1,0,0), p_(1,0,1), p_(1,1,0), p_(1,1,1), l_1, l_2, l_3];
I = ideal(p_(0,1,0)*p_(1,0,0)-p_(0,1,1)*p_(1,0,1)-p_(0,0,0)*p_(1,1,0)+p_(0,0,1)*p_(1,1,1),p_(0,0,1)*p_(1,0,0)-p_(0,0,0)*p_(1,0,1)-p_(0,1,1)*p_(1,1,0)+p_(0,1,0)*p_(1,1,1),p_(0,0,1)*p_(0,1,0)-p_(0,0,0)*p_(0,1,1)-p_(1,0,1)*p_(1,1,0)+p_(1,0,0)*p_(1,1,1),-p_(0,0,0)*p_(1,1,0)*l_1-p_(0,0,0)*p_(1,0,1)*l_2-p_(0,0,0)*p_(0,1,1)*l_3-413*p_(0,0,0)+64,p_(0,0,1)*p_(1,1,1)*l_1+p_(0,0,1)*p_(1,0,0)*l_2+p_(0,0,1)*p_(0,1,0)*l_3-413*p_(0,0,1)+10,p_(0,1,0)*p_(1,0,0)*l_1+p_(0,1,0)*p_(1,1,1)*l_2+p_(0,0,1)*p_(0,1,0)*l_3-413*p_(0,1,0)+36,-p_(0,1,1)*p_(1,0,1)*l_1-p_(0,1,1)*p_(1,1,0)*l_2-p_(0,0,0)*p_(0,1,1)*l_3-413*p_(0,1,1)+81,p_(0,1,0)*p_(1,0,0)*l_1+p_(0,0,1)*p_(1,0,0)*l_2+p_(1,0,0)*p_(1,1,1)*l_3-413*p_(1,0,0)+23,-p_(0,1,1)*p_(1,0,1)*l_1-p_(0,0,0)*p_(1,0,1)*l_2-p_(1,0,1)*p_(1,1,0)*l_3-413*p_(1,0,1)+44,-p_(0,0,0)*p_(1,1,0)*l_1-p_(0,1,1)*p_(1,1,0)*l_2-p_(1,0,1)*p_(1,1,0)*l_3-413*p_(1,1,0)+86,p_(0,0,1)*p_(1,1,1)*l_1+p_(0,1,0)*p_(1,1,1)*l_2+p_(1,0,0)*p_(1,1,1)*l_3-413*p_(1,1,1)+69);
end
restart
FF = ZZ/32003
FF = CC_53
load "likelihood-ideal-deg92.m2"

needsPackage "MonodromySolver"
MonodromySolver#"exported symbols"
setRandomSeed 0;
{CorrectorTolerance,Precision}/getDefault
setDefault(CorrectorTolerance=>0.1*getDefault CorrectorTolerance)
setDefault(Precision=>2*getDefault Precision)
elapsedTime S = sparseMonodromySolve(polySystem I,Verbose=>true);
#S -- gets 90
#select(S,s->status s == Regular)
P = solveSystem(I_*,Software=>PHCPACK);
#P -- gets 89-90
