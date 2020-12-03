randomPlanePoints = (delta,R) -> (
     k:=ceiling((-3+sqrt(9.0+8*delta))/2);
     eps:=delta-binomial(k+1,2);
     if k-2*eps>=0 
     then minors(k-eps,
          random(R^(k+1-eps),R^{k-2*eps:-1,eps:-2}))
     else minors(eps,
          random(R^{k+1-eps:0,2*eps-k:-1},R^{eps:-2})));
distinctPoints = (J) -> (
     singJ:=minors(2,jacobian J)+J;
     codim singJ == 3);
randomNodalCurve = method();
randomNodalCurve (ZZ,ZZ,Ring) := (d,g,R) -> (
     delta:=binomial(d-1,2)-g;
     K:=coefficientRing R;
     if (delta==0) 
     then (     --no double points
          ideal random(R^1,R^{-d}))
     else (      --delta double points            
          Ip:=randomPlanePoints(delta,R);
          --choose the curve
          Ip2:=saturate Ip^2;
          ideal (gens Ip2 * random(source gens Ip2, R^{-d}))));
isNodalCurve = (I) -> (
     singI:=ideal jacobian I +I;delta:=degree singI;
     d:=degree I;g:=binomial(d-1,2)-delta;
     {distinctPoints(singI),delta,g});
randomGenus11Curve = (R) -> (
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
          Mt=coker random(R^{3:8},R^{6:7,2:6});
          M=coker (transpose (res Mt).dd_4);
          Gt:=transpose (res M).dd_3;
          I:=ideal syz (Gt*random(source Gt,R^{6:5}));
          correctCodimAndDegree=(codim I==2 and degree I==12););
     I);
isSmoothSpaceCurve = (I) -> (
     --I generates the ideal sheaf of a pure codim 2 scheme in P3
     singI:=I+minors(2,jacobian I);
     codim singI==4);
K=ZZ/101;
R=K[x_0..x_3];
C=randomGenus11Curve(R);
isSmoothSpaceCurve(C)
Omega=prune Ext^2(coker gens C,R^{-4});
betti Omega
randomGenus12Curve = (R) -> (
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
          M:=coker random(R^{3:-2},R^{7:-3});
          Gt:=transpose (res M).dd_3;
          I:=ideal syz (Gt*random(source Gt,R^{7:5}));
          correctCodimAndDegree=(codim I==2 and degree I==12););
     I);
randomGenus13Curve = (R) -> (
     kappa:=koszul(3,vars R);
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
          test:=false;while test==false do ( 
               alpha:=random(R^{4:-2},R^{6:-2});
               beta:=random(R^{4:-2},R^{5:-3});
               M:=coker(alpha*kappa|beta);
               test=(codim M==4 and degree M==16););
          Gt:=transpose (res M).dd_3;
          --up to change of basis we can reduce phi to this form
          phi:=random(R^6,R^3)++id_(R^6);
          I:=ideal syz(Gt_{1..12}*phi);
          correctCodimAndDegree=(codim I==2 and degree I==13););
     I);
testModulesForGenus14Curves = (N,p) ->(
     x := local x;
     R := ZZ/p[x_0..x_3];
     i:=0;j:=0;
     kappa:=koszul(3,vars R);
     kappakappa:=kappa++kappa;
     utime:=timing while (i<N) do (
          test:=false;
          alpha:=random(R^{5:-2},R^{12:-2});
          beta:=random(R^{5:-2},R^{3:-3});
          M:=coker (alpha*kappakappa|beta);
          fM:=res (M,DegreeLimit =>3);
          if (tally degrees fM_2)_{5}==3 then (
               --further checks to pick up the right module
               test=(tally degrees fM_2)_{4}==2 and
               codim M==4 and degree M==23;);
          i=i+1;if test==true then (j=j+1;););
     timeForNModules:=utime#0; numberOfGoodModules:=j;
     {timeForNModules,numberOfGoodModules});
testModulesForGenus14Curves(1000,5)
randomGenus14Curve = (R) -> (
     kappa:=koszul(3,vars R);
     kappakappa:=kappa++kappa;
     correctCodimAndDegree:=false;
     count:=0;while not correctCodimAndDegree do (
          test:=false;
          t:=timing while test==false do (
               alpha=random(R^{5:-2},R^{12:-2});
               beta=random(R^{5:-2},R^{3:-3});
               M:=coker (alpha*kappakappa|beta);
               fM:=res (M,DegreeLimit =>3);
               if (tally degrees fM_2)_{5}==3 then (
                    --further checks to pick up the right module
                    test=(tally degrees fM_2)_{4}==2 and
                    codim M==4 and degree M==23;);
               count=count+1;);
          Gt:=transpose (res M).dd_3;
          I:=ideal syz (Gt_{5..17});
          correctCodimAndDegree=(codim I==2 and degree I==14););
     <<"     -- "<<t#0<<" seconds used for ";
     <<count<<" modules"<<endl;
     I);
testModulesForDeg17CY = (N,k,p) -> (
     x:=symbol x;R:=(ZZ/p)[x_0..x_6];
     numberOfGoodModules:=0;i:=0;
     usedTime:=timing while (i<N) do (
          b:=random(R^3,R^{16:-1});
          --we put SyzygyLimit=>60 because we expect 
          --k<16 syzygies, so 16+28+k<=60
          fb:=res(coker b, 
               DegreeLimit =>0,SyzygyLimit=>60,LengthLimit =>3);
          if rank fb_3>=k and (dim coker b)==0 then (
               fb=res(coker b, DegreeLimit =>0,LengthLimit =>4);
               if rank fb_4==0 
               then numberOfGoodModules=numberOfGoodModules+1;);
          i=i+1;);
     collectGarbage();
     timeForNModules:=usedTime#0;
     {timeForNModules,numberOfGoodModules});
randomModuleForDeg17CY = (k,R) -> (
     isGoodModule:=false;i:=0;
     while not isGoodModule do (
          b:=random(R^3,R^{16:-1});
          --we put SyzygyLimit=>60 because we expect 
          --k<16 syzygies, so 16+28+k<=60
          fb:=res(coker b, 
               DegreeLimit =>0,SyzygyLimit=>60,LengthLimit =>3);
          if rank fb_3>=k and (dim coker b)==0 then (
               fb=res(coker b, DegreeLimit =>0,LengthLimit =>4);
               if rank fb_4==0 then isGoodModule=true;);
          i=i+1;);
     <<"     -- Trial n. " << i <<", k="<< rank fb_3 <<endl;
     b);
getDeltaForDeg17CY = (b,f1) -> (
     fb:=res(coker b, LengthLimit =>3);
     k:=numgens target fb.dd_3-28; --# of linear syzygies
     b1:=fb.dd_1;b2:=fb.dd_2_{0..27};b2':=fb.dd_2_{28..28+k-1};
     b3:=fb.dd_3_{0..k-1}^{0..27};
     --the equation for f2 is b1*f2+f1*b2=0, 
     --so f2 is a lift of (-f1*b2) through b1 
     f2:=-(f1*b2)//b1;
     --the equation for A=(f3||Delta) is -f2*b3 = (b2|b2') * A
     A:=(-f2*b3)//(b2l|b2');
     Delta:=A^{28..28+k-1});
codimInfDefModuleForDeg17CY = (b) -> (
     --we create a parameter ring for the matrices f1's
     R:=ring b;K:=coefficientRing R;
     u:=symbol u;U:=K[u_0..u_(3*16*7-1)];
     i:=0;while i<3 do (
          <<endl<< " " << i+1 <<":" <<endl;
          j:=0;while j<16 do(
               << "    " << j+1 <<". "<<endl;
               k:=0;while k<7 do (
                  l=16*7*i+7*j+k; --index parametrizing the f1's
                  f1:=matrix(R,apply(3,m->apply(16,n->
                       if m==i and n==j then x_k else 0)));
                  Delta:=substitute(getDeltaForDeg17CY(b,f1),U);
                  if l==0 then (equations=u_l*Delta;) else (
                       equations=equations+u_l*Delta;);
                  k=k+1;);
               collectGarbage(); --frees up memory in the stack
               j=j+1;);
          i=i+1;);
     codim ideal equations);
skewSymMorphismsForDeg17CY = (b) -> (
     --we create a parameter ring for the morphisms: 
     K:=coefficientRing ring b;
     u:=symbol u;U:=K[u_0..u_(binomial(16,2)-1)];
     --now we compute the equations for the u_i's:
     UU:=U**ring b;
     equationsInUU:=flatten (substitute(b,UU)*
          substitute(genericSkewMatrix(U,u_0,16),UU));
     uu:=substitute(vars U,UU);
     equations:=substitute(
          diff(uu,transpose equationsInUU),ring b);
     syz(equations,DegreeLimit =>0));
getMorphismForDeg17CY = (SkewSymMorphism) -> (
     u:=symbol u;U:=K[u_0..u_(binomial(16,2)-1)];
     f=map(ring SkewSymMorphism,U,transpose SkewSymMorphism);
     f genericSkewMatrix(U,u_0,16));
checkBasePtsForDeg17CY = b -> (
     --firstly the number of linear syzygies
     fb:=res(coker b, DegreeLimit=>0, LengthLimit =>4);
     k:=#select(degrees source fb.dd_3,i->i=={3});
     --then the check
     a=symbol a;A=K[a_0..a_2];
     mult:=(id_(A^7)**vars A)*substitute(
          syz transpose jacobian b,A);
     basePts=ideal mingens minors(5,mult);
     codim basePts==2 and degree basePts==k and distinctPoints(
          basePts));
checkMorphismsForDeg17CY = (b,skewSymMorphisms) -> (
     --first the number of linear syzygies
     fb:=res(coker b, DegreeLimit=>0, LengthLimit =>4);
     k:=#select(degrees source fb.dd_3,i->i=={3});
     if (numgens source skewSymMorphisms)!=k then (
          error "the number of skew-sym morphisms is wrong";);
     --we parametrize the morphisms:
     R:=ring b;K:=coefficientRing R;
     w:=symbol w;W:=K[w_0..w_(k-1)];
     WW:=R**W;ww:=substitute(vars W,WW);
     genericMorphism:=getMorphismForDeg17CY(
          substitute(skewSymMorphisms,WW)*transpose ww);
     --we compute the scheme of the 3x3 morphisms:
     equations:=mingens pfaffians(4,genericMorphism);
     equations=diff(
          substitute(symmetricPower(2,vars R),WW),equations);
     equations=saturate ideal flatten substitute(equations,W);
     CorrectDimensionAndDegree:=(
          dim equations==1 and degree equations==k);
     isNonDegenerate:=#select(
          (flatten degrees source gens equations),i->i==1)==0;
     collectGarbage();
     isOK:=CorrectDimensionAndDegree and isNonDegenerate;
     if isOK then (
          --in this case we also look for a skew-morphism f 
          --which is a linear combination of the special 
          --morphisms with all coefficients nonzero.
          isGoodMorphism:=false;while isGoodMorphism==false do (
               evRandomMorphism:=random(K^1,K^k);
               itsIdeal:=ideal(
                    vars W*substitute(syz evRandomMorphism,W));
               isGoodMorphism=isGorenstein(
                    intersect(itsIdeal,equations));
               collectGarbage());
          f=map(R,WW,vars R|substitute(evRandomMorphism,R));
          randomMorphism:=f(genericMorphism);
          {isOK,randomMorphism}) else {isOK});
isGorenstein = (I) -> (
     codim I==length res I and rank (res I)_(length res I)==1);
randomModule2ForDeg17CY = (k,R) -> (
     isGoodModule:=false;i:=0;
     while not isGoodModule do (
          b:=(random(R^1,R^{3:-1})++
               random(R^1,R^{3:-1})++
               random(R^1,R^{3:-1})|
               matrix(R,{{1},{1},{1}})**random(R^1,R^{3:-1})|
               random(R^3,R^1)**random(R^1,R^{3:-1})|
               random(R^3,R^{1:-1}));
          --we put SyzygyLimit=>60 because we expect 
          --k<16 syzygies, so 16+28+k<=60
          fb:=res(coker b, 
               DegreeLimit =>0,SyzygyLimit=>60,LengthLimit =>3);
          if rank fb_3>=k and dim coker b==0 then (
               fb=res(coker b, DegreeLimit =>0,LengthLimit =>4);
               if rank fb_4==0 then isGoodModule=true;);
          i=i+1;);
     <<"     -- Trial n. " << i <<", k="<< rank fb_3 <<endl;
     b);
K=ZZ/13;
R=K[x_0..x_6];
time b=randomModule2ForDeg17CY(8,R);
betti res coker b
betti (skewSymMorphisms=skewSymMorphismsForDeg17CY b)
checkBasePtsForDeg17CY b
finalTest=checkMorphismsForDeg17CY(b,skewSymMorphisms);
finalTest#0
n=finalTest#1;
betti (nn=syz n)
n2t=transpose submatrix(nn,{0..15},{3});
b2:=syz b;
j:=ideal mingens ideal flatten(n2t*b2);
degree j
codim j
betti res j
