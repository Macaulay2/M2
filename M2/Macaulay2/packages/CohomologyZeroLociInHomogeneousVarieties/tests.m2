-- Test 0
TEST ///
R = rootSystemA(5);
X = homogeneousVariety(R,set{1,2,4,5});
assert(dim X == 9)
R = rootSystemD(5);
X = homogeneousVariety(R,set{3,4,5});
assert(dim X == 14)
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
assert(dim X == 16)
///

-- Test 1
TEST ///
X = Gr{2,5};
assert(dim X == 6)
X = Gr{3,8};
assert(dim X == 15)
X = Gr{1,6};
assert(dim X == 5)
///

-- Test 2
TEST ///
X = Fl{1,3,5};
assert(dim X == 8)
X = Fl{2,4,7};
assert(dim X == 16)
X = Fl{1,2,3,6};
assert(dim X == 12)
///

-- Test 3
TEST ///
X = OGr{1,7};
assert(dim X == 5)
X = OGr{2,8};
assert(dim X == 9)
X = OGr{3,10};
assert(dim X == 15)
///

-- Test 4
TEST ///
X = SGr{1,4};
assert(dim X == 3)
X = SGr{2,6};
assert(dim X == 7)
X = SGr{3,8};
assert(dim X == 12)
///

-- Test 5
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{1,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
assert(rank E == 2)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{0,1,0});
E = homogeneousVectorBundle({l},{1},X);
assert(rank E == 5)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,1,0});
E = homogeneousVectorBundle({l1,l2},{1,2},X);
assert(rank E == 3)
///

-- Test 6
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,0,1});
F = filtrationBundle({l1,l2},X);
assert(rank F == 3)
R = rootSystemC(3);
X = homogeneousVariety(R,set{1,3});
l1 = weight(R,{0,1,0});
E1 = homogeneousVectorBundle({l1},{1},X);
l2 = weight(R,{0,0,1});
E2 = homogeneousVectorBundle({l2},{1},X);
F = filtrationBundle({E1,E2},X);
assert(rank F == 3)
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
l1 = weight(R,{1,0,0,0,0,0});
l2 = weight(R,{0,0,0,0,0,1});
l3 = weight(R,{0,1,0,0,0,0});
F = filtrationBundle({l1,l2,l3},X);
assert(rank F == 27)
///

-- Test 7
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{0,0,1,0});
F = homogeneousVectorBundle({l},{1},X);
Y = embeddedVariety F;
assert(dim Y == 3)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{0,1,0});
F = homogeneousVectorBundle({l},{1},X);
Y = embeddedVariety F;
assert(dim Y == 0)
R = rootSystemD(5);
X = homogeneousVariety(R,set{2,3,4,5});
l = weight(R,{0,0,0,0,1});
F = homogeneousVectorBundle({l},{1},X);
Y = embeddedVariety F;
assert(dim Y == 0)
///

-- Test 8
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{1,3});
T = homogeneousTangentBundle X;
assert(rank T == 4)
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
T = homogeneousTangentBundle X;
assert(rank T == 8)
R = rootSystemA(5);
X = homogeneousVariety(R,set{1,2,4,5});
T = homogeneousTangentBundle X;
assert(rank T == 9)
///

-- Test 9
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
Cot = homogeneousCotangentBundle X;
assert(rank Cot == dim X)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
Cot = homogeneousCotangentBundle X;
assert(rank Cot == dim X)
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
Cot = homogeneousCotangentBundle X;
assert(rank Cot == dim X)
///

-- Test 10
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
S = structureSheaf X;
assert(rank S == 1)
R = rootSystemD(5);
X = homogeneousVariety(R,set{1,3,4,5});
S = structureSheaf X;
assert(rank S == 1)
R = rootSystemF4;
X = homogeneousVariety(R,set{2,3});
S = structureSheaf X;
assert(rank S == 1)
///

-- Test 11
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,0,1});
E = homogeneousVectorBundle({l1,l2},{2,1},X);
S = summands E;
assert(#S == 3)
R = rootSystemB(3);
X = homogeneousVariety(R,set{3});
l = weight(R,{0,0,1});
E = homogeneousVectorBundle({l},{3},X);
S = summands E;
assert(#S == 3)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,1,0,0});
l3 = weight(R,{0,0,1,0});
E = homogeneousVectorBundle({l1,l2,l3},{1,1,1},X);
S = summands E;
assert(#S == 3)
///

-- Test 12
TEST ///
R = rootSystemA(4);
l = weight(R,{-1,2,-3,0});
(v,k) = dominantConjugate(l,R);
assert(entries v == {0,1,1,1})
assert(k == 7)
R = rootSystemD(4);
l = weight(R,{2,-1,0,1});
(v,k) = dominantConjugate(l,R);
assert(entries v == {1,0,1,0})
assert(k == 2)
R = rootSystemG2;
l = weight(R,{-1,2});
(v,k) = dominantConjugate(l,R);
assert(entries v == {1,1})
assert(k == 1)
///

-- Test 13
TEST ///
R = rootSystemA(4);
l = weight(R,{1,0,2,1});
assert(isDominant(l,R))
R = rootSystemB(3);
l = weight(R,{-1,0,1});
assert(not isDominant(l,R))
R = rootSystemF4;
l = weight(R,{1,1,0,0});
assert(isDominant(l,R))
///

-- Test 14
TEST ///
R = rootSystemA(4);
l = weight(R,{2,1,0,1});
assert(isSingular(l,R))
R = rootSystemC(3);
l = weight(R,{1,0,0});
assert(isSingular(l,R))
R = rootSystemG2;
l = weight(R,{1,1});
assert(not isSingular(l,R))
///

-- Test 15
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{1,3});
l = weight(R,{0,1,0});
L = homogeneousVectorBundle({l},{1},X);
assert(isAmple L)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{1,0,0});
L = homogeneousVectorBundle({l},{1},X);
assert(isAmple L)
R = rootSystemD(4);
X = homogeneousVariety(R,set{1,2,3});
l = weight(R,{0,0,0,1});
L = homogeneousVectorBundle({l},{1},X);
assert(isAmple L)
///

-- Test 16
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{0,1,0,0});
E = homogeneousVectorBundle({l},{1},X);
assert(isGloballyGenerated(E,X))
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{1,0,0});
E = homogeneousVectorBundle({l},{1},X);
assert(isGloballyGenerated(E,X))
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
l = weight(R,{0,1,0,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
assert(isGloballyGenerated(E,X))
///

-- Test 17
TEST ///
R = rootSystemA(4);
l = weight(R,{1,0,0,0});
assert(weylFormula(l,R) == 5)
R = rootSystemB(3);
l = weight(R,{0,0,1});
assert(weylFormula(l,R) == 8)
R = rootSystemG2;
l = weight(R,{1,0});
assert(weylFormula(l,R) == 7)
///

-- Test 18
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{1,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
assert(eulerCharacteristic E == 5)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
G = filtrationBundle({structureSheaf X, F},X);
assert(eulerCharacteristic(X,F,G) == 5)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
G = filtrationBundle({structureSheaf X, F},X);
assert(eulerCharacteristic(Y,G) == 5)
///

-- Test 19
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicCotangent(1,Y) == -1)
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicCotangent(2,Y) == 0)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicCotangent(1,Y) == -1)
///

-- Test 20
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
assert(chiCotangent(1,Y) == -1)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
Y = embeddedVariety F;
assert(chiCotangent(1,Y) == -3)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
assert(chiCotangent(2,Y) == 0)
///

-- Test 21
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicTangent(1,Y) == 3)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicTangent(1,Y) == 10)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
assert(eulerCharacteristicTangent(2,Y) == 0)
///

-- Test 22
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
Y = embeddedVariety F;
assert(chiTangent(1,Y) == 10)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
Y = embeddedVariety F;
assert(chiTangent(1,Y) == 9)
R = rootSystemF4;
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
assert(chiTangent(2,Y) == 378)
///

-- Test 23
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
assert(eulerCharacteristicTangentTwisted(1,Y,E) == 36)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,0,1})},{1},X);
assert(eulerCharacteristicTangentTwisted(1,Y,E) == 112)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
assert(eulerCharacteristicTangentTwisted(2,Y,E) == 0)
///

-- Test 24
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
assert(eulerCharacteristicStructure Y == 1)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
Y = embeddedVariety F;
assert(eulerCharacteristicStructure Y == 3)
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
F = homogeneousVectorBundle({weight(R,{1,0,0,0,0,0})},{2},X);
Y = embeddedVariety F;
assert(eulerCharacteristicStructure Y == 1)
///

-- Test 25
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
Y = embeddedVariety F;
assert(chiStructure Y == 1)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
assert(chiStructure Y == 0)
R = rootSystemG2;
X = homogeneousVariety(R,set{2});
F = homogeneousVectorBundle({weight(R,{0,1})},{1},X);
Y = embeddedVariety F;
assert(chiStructure Y == 1)
///

-- Test 26
TEST ///
X = Gr{1,4};
assert(volumeFano X == 64)
X = Gr{1,3};
assert(volumeFano X == 9)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
assert(volumeFano Y == 9)
///

-- Test 27
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
G = filtrationBundle({structureSheaf X,F},X);
assert(cohomologyRestriction(X,F,G) == {4,0,0,0})
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
G = filtrationBundle({structureSheaf X,F},X);
assert(cohomologyRestriction(Y,G) == {4,0,0,0})
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
G = filtrationBundle({structureSheaf X,F},X);
assert(cohomologyRestriction(X,F,G) == {7,0,0,0,0,0})
///

-- Test 28
TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
E1 = homogeneousVectorBundle({weight(R,{1,1,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
E = tensorProduct(E1,E2);
assert(rank E == 4)
R = rootSystemB(3);
X = homogeneousVariety(R,set{3});
E1 = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
F = filtrationBundle({E1,E1},X);
E = tensorProduct(F,E2);
assert(rank E == 2)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,4});
E1 = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);
E3 = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
E = tensorProduct{E1,E2,E3};
assert(rank E == 3)
///

-- Test 29
TEST ///
X = Gr{1,3};
hn = hodgeNumbers X;
assert(hn#0 == {1,0,0} and hn#1 == {0,1,0})
X = Gr{2,4};
hn2 = hodgeNumbers(2,X);
assert(hn2#0 == {1,0,0,0,0} and hn2#1 == {0,1,0,0,0})
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
(hn3, bounds3) = hodgeNumbers Y;
assert(hn3#0 == {1,0,0,0} and hn3#1 == {0,1,0,0} and hn3#2 == {0,0,1})
assert(#keys bounds3 == 0)
///

-- Test 30
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
hn = hochschildNumbers Y;
(h, b) = hn;
assert(apply(sort keys h, k -> toString h#k) == {"{1, 0, 0, 0}", "{x_0+8, x_0, 0, 0}", "{-x_2+x_1+10, x_1, x_2, 0}"})
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
hn2 = hochschildNumbers Y;
(h, b) = hn2;
assert(apply(sort keys h, k -> toString h#k) == {"{1, 0, 0, 0, 0, 0}", "{x_0+24, x_0, 0, 0, 0, 0}", "{-x_2+x_1+126, x_1, x_2, 0, 0, 0}", "{x_5-x_4+x_3+224, x_3, x_4, x_5, 0, 0}", "{-x_9+x_8-x_7+x_6+126, x_6, x_7, x_8, x_9, 0}"})
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
Y = embeddedVariety F;
hn3 = hochschildNumbers Y;
(h, b) = hn3;
assert(apply(sort keys h, k -> toString h#k) == {"{1, 0, 0, 0, 0, 0, 0}", "{x_0+21, x_0, 0, 0, 0, 0, 0}", "{-x_2+x_1+189, x_1, x_2, 0, 0, 0, 0}", "{x_5-x_4+x_3+616, x_3, x_4, x_5, 0, 0, 0}", "{-x_9+x_8-x_7+x_6+819, x_6, x_7, x_8, x_9, 0, 0}", "{x_14-x_13+x_12-x_11+x_10+378, x_10, x_11, x_12, x_13, x_14, 0}"})
///

-- Test 31
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
(h, b) = hochschildNumbersTwisted(Y,E);
assert(apply(sort keys h, k -> toString h#k) == {"{6, 0, 0, 0}", "{x_0+36, x_0, 0, 0}", "{-x_2+x_1+39, x_1, x_2, 0}"})
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,0,1})},{1},X);
(h, b) = hochschildNumbersTwisted(Y,E);
assert(apply(sort keys h, k -> toString h#k) == {"{8, 0, 0, 0, 0, 0}", "{x_0+112, x_0, 0, 0, 0, 0}", "{-x_2+x_1+568, x_1, x_2, 0, 0, 0}", "{x_5-x_4+x_3+1000, x_3, x_4, x_5, 0, 0}", "{-x_9+x_8-x_7+x_6+560, x_6, x_7, x_8, x_9, 0}"})
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
(h, b) = hochschildNumbersTwisted(Y,E);
assert(apply(sort keys h, k -> toString h#k) == {"{7, 0, 0, 0, 0, 0, 0}", "{x_0+105, x_0, 0, 0, 0, 0, 0}", "{-x_2+x_1+616, x_1, x_2, 0, 0, 0, 0}", "{x_5-x_4+x_3+1560, x_3, x_4, x_5, 0, 0, 0}", "{-x_9+x_8-x_7+x_6+1750, x_6, x_7, x_8, x_9, 0, 0}", "{x_14-x_13+x_12-x_11+x_10+714, x_10, x_11, x_12, x_13, x_14, 0}"})
///

-- Test 32
TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
R = rootSystemG2;
X = homogeneousVariety(R,set{2});
F = homogeneousVectorBundle({weight(R,{0,1})},{1},X);
Y = embeddedVariety F;
(h, b) = hochschildNumbers Y;
assert(apply(sort keys h, k -> toString h#k) == {"{1, 0, 0, 0, 0, 0}", "{x_0+8, x_0, 0, 0, 0, 0}", "{-x_2+x_1+28, x_1, x_2, 0, 0, 0}", "{x_5-x_4+x_3+27, x_3, x_4, x_5, 0, 0}"})
///

-- Test 33
TEST ///
X = Gr{1,3};
(vol,chiT,chiCot,hodge) = invariants X;
assert(vol == 9)
assert(chiT == {8})
assert(chiCot == {1,-1})
X = Gr{1,4};
(vol2,chiT2,chiCot2,hodge2) = invariants(X,doHodge=>false,maxChiT=>2);
assert(vol2 == 64)
assert(chiT2 == {15,45})
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
(vol3,chiT3,chiCot3,hodge3) = invariants(Y,maxChiT=>1);
assert(vol3 == 9)
assert(chiT3 == {8})
///
