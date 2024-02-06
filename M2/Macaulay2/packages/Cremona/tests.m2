TEST ///  --- quadro-quadric Cremona transformations 
    ringP5=ZZ/33331[x_0..x_5]; ringP8=GF(331^2)[x_0..x_8]; ringP14=QQ[x_0..x_14];
    phi1=toMap trim minors(2,genericSymmetricMatrix(ringP5,3)) 
    phi2=toMap minors(2,genericMatrix(ringP8,3,3)) 
    phi3=toMap pfaffians(4,genericSkewMatrix(ringP14,6)) 
    phi4=map quadroQuadricCremonaTransformation(20,1,ZZ/3331)
    time psi1=inverseMap(phi1)
    time psi2=inverseMap(phi2,Certify=>true)
    time psi3=inverseMap(phi3,Certify=>false)
    time psi4=inverseMap phi4
    time assert (isInverseMap(phi1,psi1) and isInverseMap(phi2,psi2) and isInverseMap(phi3,psi3) and isInverseMap(phi4,psi4))
    time assert (degreeMap(phi1,Certify=>true) == 1 and degreeMap phi2 == 1 and degreeMap phi3 == 1 and degreeMap phi4 == 1)
///

TEST ///
    for i from 1 to 11 do assert(degrees specialQuadraticTransformation i === projectiveDegrees rationalMap map specialQuadraticTransformation(i,ZZ/33331))
    for i from 1 to 11 do assert isBirational rationalMap map specialQuadraticTransformation(i,ZZ/3331)
    for i from 1 to 11 do assert isDominant rationalMap map specialQuadraticTransformation(i,ZZ/3331)
///
    
TEST /// -- Hankel matrices
    phi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r list for j to r list x_(i+j)); toMap ideal jacobian ideal det M);
    psi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r-1 list for j to r+1 list x_(i+j)); toMap minors(r,M));
    psi'= (r,K) -> toMap(psi(r,K),Dominant=>2);
    psi0= (r,K) -> (f:=psi'(r,K); map((target f)/(ideal f(sub(random(1,ambient source f),source f))),target f) * f);
    assert(projectiveDegrees phi(2,frac(ZZ/331[i]/(i^2+1)))  == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi'(2,ZZ/331) == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees(psi'(2,ZZ/331),Certify=>true) == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi0(2,ZZ/331) ==    {2, 4, 4, 2})
    assert(projectiveDegrees(psi0(2,ZZ/331),Certify=>true) ==    {2, 4, 4, 2})
    assert(degreeMap phi(2,QQ) == 2)
    assert(projectiveDegrees inverseMap psi'(2,ZZ/101) == reverse {1, 2, 4, 4, 2})
    assert(projectiveDegrees(inverseMap psi'(2,ZZ/5),Certify=>true) == reverse {1, 2, 4, 4, 2})
    assert(projectiveDegrees phi(3,ZZ/3331)  == {1, 3, 9, 17, 21, 15, 5})   
    assert(projectiveDegrees psi'(3,ZZ/3331) == {1, 3, 9, 17, 21, 15, 5})   
    assert(projectiveDegrees psi0(3,ZZ/3331) ==    {3, 9, 17, 21, 15, 5})
    assert(degreeMap phi(3,ZZ/3331) == 5)
    assert(projectiveDegrees inverseMap psi'(3,ZZ/3331) == reverse {1, 3, 9, 17, 21, 15, 5})
    assert(degreeMap phi(4,ZZ/431) == 14)
///    

TEST ///  -- special map P^8 ---> P^11
    Phi = specialQuadraticTransformation(8,ZZ/3331)
    phi = map rationalMap Phi;
    Z = ideal target Phi;
    time assert(kernel(phi,2) == Z)
    time assert(projectiveDegrees phi == {1, 2, 4, 8, 16, 23, 23, 16, 8})
    time assert(degreeMap phi == 1)
    H=ideal random(1,source phi)
    phi'=map((target phi)/phi(H),target phi) * phi;
    time assert ( kernel(phi',1) == H )
    Q=ideal random(2,source phi)
    phi'=map((target phi)/phi(Q),target phi) * phi;
    time assert ( kernel(phi',2) == Q+Z )
    phi=toMap(phi,Dominant=>Z)
    ideal matrix approximateInverseMap(approximateInverseMap(phi,Certify=>true)) == ideal matrix phi
/// 

TEST /// -- a quadric surface bundle in P^2 x P^5
K := ZZ/3331; P2xP5 := K[t_0..t_2,x_2..x_7,Degrees=>{3:{1,0},6:{0,1}}]; P5 := K[y_0..y_5];
J = ideal(t_1*x_5-t_2*x_5-t_0*x_7-2*t_1*x_7-2*t_2*x_7,t_2*x_4-t_0*x_6-2*t_1*x_6-2*t_2*x_6,t_2*x_2^2+t_1*x_3^2+t_0*x_5^2+t_1*x_4*x_6+t_0*x_7^2+2*t_1*x_7^2+2*t_2*x_7^2);
p1 = (rationalMap {t_0,t_1,t_2})|J
p2 = rationalMap((rationalMap(P2xP5,P5,{x_7,x_6,x_5,x_4,x_3,x_2}))|source(p1),Dominant=>4)
phi = rationalMap {8*y_2^2*y_3-4*y_1*y_3^2-4*y_3*y_4^2,-16*y_2^3+4*y_1*y_2*y_3+4*y_2*y_4^2+4*y_2*y_5^2,-4*y_2*y_3*y_5,-4*y_2*y_3*y_4,-4*y_2*y_3^2,-4*y_2^2*y_3,-4*y_1*y_2*y_3,-4*y_0*y_2*y_3}
phi = rationalMap(phi|(target p2),Dominant=>2)
assert(projectiveDegrees phi == {4, 7, 8, 8, 8})
assert(isBirational(phi,Certify=>true))
assert(degrees phi == {4, 7, 8, 8, 8})
assert(projectiveDegrees p2 == {36, 25, 16, 9, 4})
assert(degrees p2 == {36, 25, 16, 9, 4})
assert(isDominant(p2,Certify=>true))
assert(degree p2 == 1)
assert(projectiveDegrees p1 == {36, 11, 2, 0, 0})
assert(degrees p1 == {36, 11, 2, 0, 0})
f = p2 * phi
assert( projectiveDegrees(f,0) == 8 and projectiveDegrees(f,4) == 36)
assert(projectiveDegrees(f,Certify=>true) == {36, 64, 48, 24, 8})
assert(dim ideal f -2 == 2 and dim ideal matrix f -2 == 3 and # f#"maps" == 3)
(A,B,C) = graph f
assert(degrees A == {684, 174, 22, 0, 0} and multidegree B == {684, 222, 64, 16, 4} and projectiveDegrees(C,Certify=>true) == {684, 288, 104, 32, 8})
assert(isDominant A and (not isDominant B) and (isDominant C))
assert(isDominant(A,Certify=>true) and (not isDominant(B,Certify=>true)) and isDominant(C,Certify=>true))
///
    
TEST ///
    K=ZZ/761; P4=K[x_0..x_4];
    S4=P4/ideal(x_0^3+x_1^3+x_2^3); u=gens S4;
    time phi=toMap(minors(2,matrix{{u_0,u_1,u_2,u_3},{u_1,u_2,u_3,u_4}}),Dominant=>infinity)
    time assert (projectiveDegrees phi == {3, 6, 12, 12})
    time assert (projectiveDegrees(phi,Certify=>true) == {3, 6, 12, 12})
    time assert(isDominant(phi,Certify=>true) and isBirational phi)
    time psi=inverseMap(phi,Certify=>true)
    time assert (projectiveDegrees psi == reverse {3, 6, 12, 12})
    time assert (projectiveDegrees(psi,Certify=>true) == reverse {3, 6, 12, 12})
///

TEST ///
    K=ZZ/101; R=K[t_0..t_7];
    R=R/ideal(t_3^2*t_5^2-4*t_1*t_3*t_4*t_6+2*t_2*t_3*t_5*t_6+t_2^2*t_6^2-4*t_0*t_4*t_6^2-4*t_1*t_3^2*t_7-4*t_0*t_3*t_6*t_7);
    t=gens R;
    S=K[x_0..x_8];
    phi=map(R,S,{t_4*t_6+t_3*t_7,t_5^2-t_1*t_7,t_4*t_5-t_2*t_7,t_3*t_5+t_2*t_6,t_2*t_5-t_0*t_7,t_1*t_4-t_0*t_7,t_1*t_3+t_0*t_6,t_2^2-t_0*t_4,t_1*t_2-t_0*t_5});
    Out=ideal(x_3*x_5-x_2*x_6-x_0*x_8,x_4^2-x_4*x_5-x_1*x_7+x_2*x_8,x_3^2-4*x_0*x_6);
    assert( kernel(phi,2) == Out )
    assert( ideal image basis(2,kernel phi) == Out )
    phi'=toMap(phi,Dominant=>ideal(Out_0,Out_1))
    assert( ideal image basis(2,kernel phi') == kernel(phi',2) )
///

TEST /// 
    ringP5=(ZZ/7)[x_0..x_5];
    phi=toMap(minors(2,matrix {{x_0, x_1, x_2, x_3, x_4}, {x_1, x_2, x_3, x_4, x_5}}),Dominant=>infinity);
    time psi=inverseMap(phi)
    assert(isInverseMap(phi,psi))
    phi'=inverseMap(psi);
    m={1, 2, 4, 8, 11, 10};
    time assert(isInverseMap(phi',psi) and isInverseMap(psi,phi'))
    time assert(projectiveDegrees(psi,Certify=>true) == reverse {1, 2, 4, 8, 11, 10})
///

TEST ///
    R=ZZ/331[t_0,t_1];
    phi=toMap(kernel toMap((ideal vars R)^4),Dominant=>2)
    psi=inverseMap phi
    assert( projectiveDegrees(phi,Certify=>true) == {1, 2, 4, 4, 2})
    assert( projectiveDegrees(psi,Certify=>true) == reverse({1, 2, 4, 4, 2}))
///

TEST ///
    P3 = ZZ/41[z_0..z_3];
    phi = toMap minors(3,matrix{{-z_1,z_0,-z_1^2+z_0*z_3},{z_0,z_1,z_0^2-z_1*z_2},{0,z_2,z_0*z_1-z_1*z_3},{0,z_3,-z_0*z_1+z_0*z_2}})
    assert isInverseMap(phi,inverseMap phi)
///

TEST ///
  phi = rationalMap map specialCremonaTransformation(3,ZZ/33331);
  phi' = abstractRationalMap phi;
  psi' = inverseMap phi'
  psi = rationalMap psi';
  assert isInverseMap(phi,psi)
  Phi' = abstractRationalMap(source phi',target phi',q -> phi' q)
  assert(phi == rationalMap Phi')
///

TEST ///
K = ZZ/33331;
-- twisted cubic curve
P3 = K[x_0..x_3];
C = minors(2,matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}); 
f = abstractRationalMap(C,"OADP")
assert(3 == projectiveDegrees(f,2))
B = ideal rationalMap f;
assert(B:C == C)
-- Scroll surface S(2,2)
P5 = K[x_0..x_5];
S = minors(2,matrix{{x_0,x_1,x_3,x_4},{x_1,x_2,x_4,x_5}}); 
g = abstractRationalMap(S,"OADP")
assert(3 == projectiveDegrees(g,4))
-- quintic del Pezzo surface
D = ideal(x_2*x_4-x_1*x_5,x_0*x_4-x_1*x_5-x_3*x_5+x_4*x_5,x_2*x_3-x_0*x_5,x_1*x_3-x_1*x_5-x_3*x_5+x_4*x_5,x_0*x_1-x_1*x_2-x_0*x_5+x_1*x_5);
h = abstractRationalMap(D,"OADP")
assert(5 == projectiveDegrees(h,4))
///

TEST /// -- bug fixed in inverseMap (25/01/2019)
   x := local x; P2 := (ZZ/65521)[x_0..x_2];
   f = rationalMap(P2,{6,1,7});
   S = image f;
   p = for i to 2 list random(ZZ/65521);
   J = sub(transpose jacobian matrix f,{x_0=>p_0,x_1=>p_1,x_2=>p_2});
   T = image rationalMap(P2,target f,transpose(J*(transpose vars P2)));
   h = (rationalMap T)|S;
   assert isDominant(h,Certify=>true)
   assert(degreeMap h == 2)
   assert(try inverseMap h else true)
   assert(try inverseMap(h,Certify=>true) else true)
   assert(degree h == 2)
///

TEST /// -- graph of inverse map
P5 := ZZ/100003[x_0..x_5];
phi = rationalMap(minors(2,matrix{{x_0,x_1,x_2,x_3,x_4},{x_1,x_2,x_3,x_4,x_5}}),Dominant=>2);
(p1,p2) = graph phi;
(q1,q2) = graph inverse phi;
assert(q1 * inverse phi == q2 and q2 * phi == q1 and p1 * phi == p2 and p2 * inverse phi == p1)
Bl = ideal source q1;
Bl' = ideal source first graph rationalMap map inverse phi;
assert(sub(Bl,vars ring Bl') == Bl' and sub(Bl',vars ring Bl) == Bl)
--
phi = specialCubicTransformation(2,ZZ/100003);
(p1,p2) = graph phi;
(q1,q2) = graph inverse phi;
assert(q1 * inverse phi == q2 and q2 * phi == q1 and p1 * phi == p2 and p2 * inverse phi == p1)
Bl = ideal source q1;
Bl' = ideal source first graph rationalMap map inverse phi;
assert(sub(Bl,vars ring Bl') == Bl' and sub(Bl',vars ring Bl) == Bl)
///

