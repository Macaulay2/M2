needsPackage "MonodromySolver"
setRandomSeed 100;
declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3};
paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}};
varMatrix = gateMatrix{{t_1,t_2}};
phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}};
loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2;
dLoss = diff(varMatrix, gateMatrix{{loss}});
G = gateSystem(paramMatrix,varMatrix,transpose dLoss);
# monodromyGroup(G,FileName=>"eddGG","msOptions" => {NumberOfEdges=>10,Verbose=>true})


setRandomSeed 0
needsPackage "MonodromySolver"
m = 4
n = 2
declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3}
paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}}
varMatrix = gateMatrix{{t_1,t_2}}

phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}}
phiEval = gateSystem(varMatrix, phi)
assert(m==numrows phi)
--distance = sum for i from 0 to 2 list (u_i-phi_(0,i))^2
loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2
dLoss = diff(varMatrix, gateMatrix{{loss}})
G = gateSystem(paramMatrix,varMatrix,transpose dLoss)
end
restart
needs "ed-gal.m2"
monodromyGroup(G,FileName=>"eddGG","msOptions" => {Verbose=>true,NumberOfEdges=>10})

-*
--eddGG file contents
p0:= PermList([5, 3, 7, 8, 6, 1, 2, 9, 4, 16, 11, 17, 13, 18, 15, 10, 12, 14, 19, 20, 21]);
p1:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p2:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p3:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p4:= PermList([5, 3, 7, 8, 6, 1, 2, 9, 4, 16, 11, 17, 13, 18, 15, 10, 12, 14, 19, 20, 21]);
p5:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p6:= PermList([10, 5, 19, 12, 8, 20, 14, 2, 21, 4, 11, 7, 13, 1, 15, 16, 17, 18, 6, 9, 3]);
p7:= PermList([10, 5, 11, 12, 8, 13, 14, 2, 15, 4, 3, 7, 6, 1, 9, 16, 17, 18, 20, 21, 19]);
p8:= PermList([6, 2, 7, 9, 5, 1, 3, 8, 4, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
G:=Group(p0, p1, p2, p3, p4, p5, p6, p7, p8);
-- GAP session with eddGG shows we get full wreath product S3 wr S7
gap> p0:= PermList([5, 3, 7, 8, 6, 1, 2, 9, 4, 16, 11, 17, 13, 18, 15, 10, 12, 14, 19, 20, 21]);
p1:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p2:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p3:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p4:= PermList([5, 3, 7, 8, 6, 1, 2, 9, 4, 16, 11, 17, 13, 18, 15, 10, 12, 14, 19, 20, 21]);
p5:= PermList([4, 5, 3, 7, 8, 6, 1, 2, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
p6:= PermList([10, 5, 19, 12, 8, 20, 14, 2, 21, 4, 11, 7, 13, 1, 15, 16, 17, 18, 6, 9, 3]);
p7:= PermList([10, 5, 11, 12, 8, 13, 14, 2, 15, 4, 3, 7, 6, 1, 9, 16, 17, 18, 20, 21, 19]);
p8:= PermList([6, 2, 7, 9, 5, 1, 3, 8, 4, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]);
G:=Group(p0, p1, p2, p3, p4, p5, p6, p7, p8);
(1,5,6)(2,3,7)(4,8,9)(10,16)(12,17)(14,18)
gap> (1,4,7)(2,5,8)
gap> (1,4,7)(2,5,8)
gap> (1,4,7)(2,5,8)
gap> (1,5,6)(2,3,7)(4,8,9)(10,16)(12,17)(14,18)
gap> (1,4,7)(2,5,8)
gap> (1,10,4,12,7,14)(2,5,8)(3,19,6,20,9,21)
gap> (1,10,4,12,7,14)(2,5,8)(3,11)(6,13)(9,15)(19,20,21)
gap> (1,6)(3,7)(4,9)
gap> <permutation group with 9 generators>
gap> H:=Image(ActionHomomorphism(G,Blocks(G,[1..21]),OnSets));
Group([ (1,2,3)(4,6), (), (), (), (1,2,3)(4,6), (), (1,4)(3,7), (1,4)(3,5), (1,3) ])
gap> IsSymmetricGroup(H);
true
gap> Order(G)/(3^7*Factorial(7));
1
*-
