needsPackage "NumericalAlgebraicGeometry"

S=CC[v_(0,0)..v_(2,2),x_0,x_1 ]
xx=matrix{{x_0,x_1}}
FF=diff(symmetricPower(8,xx),sum(3,i->v_(i,0)*(matrix{{1,v_(i,1),v_(i,2)}}*transpose symmetricPower(2,xx))^4))

R=CC[v_(0,0)..v_(2,2)]
theF=sub(FF,R)
varList=basis(1,R)
numcols varList, numcols theF -- square system!
F = transpose theF

setRandomSeed 0
needsPackage "NumericalAlgebraicGeometry"

-- create a generic pair (s0,T0) = (decomposition,tensor)
random1 = (m,n) -> matrix apply(m,i->apply(n,j->exp(2*pi*ii*random RR)));
s0 = random1 (1,9)
--s0 = random(CC^1,CC^9)
T0 = sub(F,s0)

-- we know one decomposition at the moment:
sols0 = {point s0};

end --------------------------------------------------------------------------

-- START pushing F11 here ****************************************************
restart
load "tensor-binary-8ic-three-4powers-decompositions.m2"

-* outsource to an external solver:
setDefault(Software=>BERTINI)
setDefault(Software=>PHCPACK)
*-

setDefault(tStepMin=>1e-10,CorrectorTolerance=>1e-8,InfinityThreshold=>1e9,SingularConditionNumber=>1e15)
stop = false; n = #sols0;
while not stop -* 6*76=456 *- do (
    T1 = sub(F,random1(1,9));
    T2 = sub(F,random1(1,9));
--    T1 = random(CC^9,CC^1);
--    T2 = random(CC^9,CC^1);    
    elapsedTime sols1 = track(polySystem(F-T0),polySystem(F-T1),sols0);
    elapsedTime sols2 = track(polySystem(F-T1),polySystem(F-T2),sols1);
    elapsedTime sols0' = track(polySystem(F-T2),polySystem(F-T0),sols2);
    print "  -- refining...";
    elapsedTime sols0'' = refine(polySystem(F-T0),select(sols0', s->status s === Regular), Bits=>100, Software=>M2engine);
    sols0''' = select(sols0'',s->status s === Regular);
    sols0 = solutionsWithMultiplicity(sols0 | sols0''',Tolerance=>1e-5); -- take the union
    if #sols0 > n then n = #sols0 else stop = true;
    << "found " << #sols0 << " decompositions so far" << endl;
    )

-- ignore stuff below... ---------------------------------------------------

max (sols0 / (s->s.ConditionNumber)) -- max condition number
max (sols0 / (s->norm(2,s))) -- max norm

sols0' / status
peek first select(sols0', s->status s===MinStepFailure)

sols0'' / status
#select(sols0'', s->status s===RefinementFailure)
peek first sols0''

select(sols0, s->norm(T0 - sub(F,matrix s)) > 1e-6)

oo / (s->norm(T0 - sub(F,matrix s)))
peek first select(sols0',s->status s === MinStepFailure)
#select(sols0, s->norm(2,s) < 5)










