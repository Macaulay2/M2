TEST ///
R = rootSystemA(4);
P = newParabolic(R,set{1,2,4});
P#"parabolic"
R = rootSystemB(3);
P = newParabolic(R,set{1,3});
P#"parabolic"
R = rootSystemE(6);
P = newParabolic(R,set{2,3,4,5,6});
P#"parabolic"
///

TEST ///
R = rootSystemA(5);
X = homogeneousVariety(R,set{1,2,4,5});
dim X
R = rootSystemD(5);
X = homogeneousVariety(R,set{3,4,5});
dim X
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
dim X
///

TEST ///
X = Gr{2,5};
dim X
X = Gr{3,8};
dim X
X = Gr{1,6};
dim X
///

TEST ///
X = Fl{1,3,5};
dim X
X = Fl{2,4,7};
dim X
X = Fl{1,2,3,6};
dim X
///

TEST ///
X = OGr{1,7};
dim X
X = OGr{2,8};
dim X
X = OGr{3,10};
dim X
///

TEST ///
X = SGr{1,4};
dim X
X = SGr{2,6};
dim X
X = SGr{3,8};
dim X
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{1,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
rank E
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{0,1,0});
E = homogeneousVectorBundle({l},{1},X);
rank E
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,1,0});
E = homogeneousVectorBundle({l1,l2},{1,2},X);
rank E
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,0,1});
F = filtrationBundle({l1,l2},X);
rank F
R = rootSystemC(3);
X = homogeneousVariety(R,set{1,3});
l1 = weight(R,{0,1,0});
E1 = homogeneousVectorBundle({l1},{1},X);
l2 = weight(R,{0,0,1});
E2 = homogeneousVectorBundle({l2},{1},X);
F = filtrationBundle({E1,E2},X);
rank F
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
l1 = weight(R,{1,0,0,0,0,0});
l2 = weight(R,{0,0,0,0,0,1});
l3 = weight(R,{0,1,0,0,0,0});
F = filtrationBundle({l1,l2,l3},X);
rank F
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{0,0,1,0});
F = homogeneousVectorBundle({l},{3},X);
Y = embeddedVariety F;
dim Y
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{0,1,0});
F = homogeneousVectorBundle({l},{2},X);
Y = embeddedVariety F;
dim Y
R = rootSystemD(5);
X = homogeneousVariety(R,set{2,3,4,5});
l = weight(R,{0,0,0,0,1});
F = homogeneousVectorBundle({l},{2},X);
Y = embeddedVariety F;
dim Y
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{1,3});
T = homogeneousTangentBundle X;
rank T
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
T = homogeneousTangentBundle X;
rank T
R = rootSystemA(5);
X = homogeneousVariety(R,set{1,2,4,5});
T = homogeneousTangentBundle X;
rank T
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
Cot = homogeneousCotangentBundle X;
rank Cot
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
Cot = homogeneousCotangentBundle X;
rank Cot
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
Cot = homogeneousCotangentBundle X;
rank Cot
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
S = structureSheaf X;
rank S
R = rootSystemD(5);
X = homogeneousVariety(R,set{1,3,4,5});
S = structureSheaf X;
rank S
R = rootSystemF4;
X = homogeneousVariety(R,set{2,3});
S = structureSheaf X;
rank S
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,0,0,1});
E = homogeneousVectorBundle({l1,l2},{2,1},X);
S = summands E;
#S
R = rootSystemB(3);
X = homogeneousVariety(R,set{3});
l = weight(R,{0,0,1});
E = homogeneousVectorBundle({l},{3},X);
S = summands E;
#S
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
l1 = weight(R,{1,0,0,0});
l2 = weight(R,{0,1,0,0});
l3 = weight(R,{0,0,1,0});
E = homogeneousVectorBundle({l1,l2,l3},{1,1,1},X);
S = summands E;
#S
///

TEST ///
R = rootSystemA(4);
l = weight(R,{-1,2,-3,0});
(v,k) = dominantConjugate(l,R);
v
k
R = rootSystemD(4);
l = weight(R,{2,-1,0,1});
(v,k) = dominantConjugate(l,R);
v
k
R = rootSystemG2;
l = weight(R,{-1,2});
(v,k) = dominantConjugate(l,R);
v
k
///

TEST ///
R = rootSystemA(4);
l = weight(R,{1,0,2,1});
isDominant(l,R)
R = rootSystemB(3);
l = weight(R,{-1,0,1});
isDominant(l,R)
R = rootSystemF4;
l = weight(R,{1,1,0,0});
isDominant(l,R)
///

TEST ///
R = rootSystemA(4);
l = weight(R,{2,1,0,1});
isSingular(l,R)
R = rootSystemC(3);
l = weight(R,{1,0,0});
isSingular(l,R)
R = rootSystemG2;
l = weight(R,{1,1});
isSingular(l,R)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{1,3});
l = weight(R,{0,1,0});
L = homogeneousVectorBundle({l},{1},X);
isAmple L
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{1,0,0});
L = homogeneousVectorBundle({l},{1},X);
isAmple L
R = rootSystemD(4);
X = homogeneousVariety(R,set{1,2,3});
l = weight(R,{0,0,0,1});
L = homogeneousVectorBundle({l},{1},X);
isAmple L
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{0,1,0,0});
E = homogeneousVectorBundle({l},{1},X);
isGloballyGenerated(E,X)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
l = weight(R,{1,0,0});
E = homogeneousVectorBundle({l},{1},X);
isGloballyGenerated(E,X)
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
l = weight(R,{0,1,0,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
isGloballyGenerated(E,X)
///

TEST ///
R = rootSystemA(3);
adjointRepresentation R
R = rootSystemD(5);
adjointRepresentation R
R = rootSystemF4;
adjointRepresentation R
///

TEST ///
R = rootSystemA(4);
l = weight(R,{1,0,0,0});
weylFormula(l,R)
R = rootSystemB(3);
l = weight(R,{0,0,1});
weylFormula(l,R)
R = rootSystemG2;
l = weight(R,{1,0});
weylFormula(l,R)
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
l = weight(R,{1,0,0,0});
E = homogeneousVectorBundle({l},{1},X);
eulerCharacteristic E
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
G = filtrationBundle({structureSheaf X, F},X);
eulerCharacteristic(X,F,G)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
G = filtrationBundle({structureSheaf X, F},X);
eulerCharacteristic(Y,G)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicCotangent(1,Y)
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicCotangent(2,Y)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicCotangent(1,Y)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
chiCotangent(1,Y)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{2},X);
Y = embeddedVariety F;
chiCotangent(1,Y)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{2},X);
Y = embeddedVariety F;
chiCotangent(2,Y)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicTangent(1,Y)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicTangent(1,Y)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
eulerCharacteristicTangent(2,Y)
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
Y = embeddedVariety F;
chiTangent(1,Y)
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
Y = embeddedVariety F;
chiTangent(1,Y)
R = rootSystemF4;
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
chiTangent(2,Y)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
eulerCharacteristicTangentTwisted(1,Y,E)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,0,1})},{1},X);
eulerCharacteristicTangentTwisted(1,Y,E)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
eulerCharacteristicTangentTwisted(2,Y,E)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
eulerCharacteristicStructure Y
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
Y = embeddedVariety F;
eulerCharacteristicStructure Y
R = rootSystemE(6);
X = homogeneousVariety(R,set{2,3,4,5,6});
F = homogeneousVectorBundle({weight(R,{0,1,0,0,0,0})},{2},X);
Y = embeddedVariety F;
eulerCharacteristicStructure Y
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{1,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
Y = embeddedVariety F;
chiStructure Y
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
Y = embeddedVariety F;
chiStructure Y
R = rootSystemG2;
X = homogeneousVariety(R,set{2});
F = homogeneousVectorBundle({weight(R,{0,1})},{1},X);
Y = embeddedVariety F;
chiStructure Y
///

TEST ///
X = Gr{1,4};
volumeFano X
X = Gr{1,3};
volumeFano X
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
volumeFano Y
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
G = filtrationBundle({structureSheaf X,F},X);
cohomologyRestriction(X,F,G)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
G = filtrationBundle({structureSheaf X,F},X);
cohomologyRestriction(Y,G)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
G = filtrationBundle({structureSheaf X,F},X);
cohomologyRestriction(X,F,G)
///

TEST ///
R = rootSystemA(4);
X = homogeneousVariety(R,set{2,4});
E1 = homogeneousVectorBundle({weight(R,{1,1,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);
E = tensorProduct(E1,E2);
rank E
R = rootSystemB(3);
X = homogeneousVariety(R,set{3});
E1 = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
F = filtrationBundle({E1,E1},X);
E = tensorProduct(F,E2);
rank E
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,4});
E1 = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
E2 = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);
E3 = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);
E = tensorProduct{E1,E2,E3};
rank E
///

TEST ///
X = Gr{1,3};
hn = hodgeNumbers X;
peek hn
X = Gr{2,4};
hn2 = hodgeNumbers(2,X);
peek hn2
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
hn3 = hodgeNumbers Y;
peek hn3
///

TEST ///
X = Gr{1,3};
displayHN X
X = Gr{2,4};
displayHN(1,X)
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
displayHN Y
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
hn = hochschildNumbers Y;
peek hn
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
hn2 = hochschildNumbers Y;
peek hn2
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
Y = embeddedVariety F;
hn3 = hochschildNumbers Y;
peek hn3
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);
hochschildNumbersTwisted(Y,E)
R = rootSystemB(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{0,0,1})},{1},X);
hochschildNumbersTwisted(Y,E)
R = rootSystemD(4);
X = homogeneousVariety(R,set{2,3,4});
F = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
Y = embeddedVariety F;
E = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);
hochschildNumbersTwisted(Y,E)
///

TEST ///
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
displayHochN Y
R = rootSystemC(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
displayHochN Y
R = rootSystemG2;
X = homogeneousVariety(R,set{2});
F = homogeneousVectorBundle({weight(R,{0,1})},{1},X);
Y = embeddedVariety F;
displayHochN Y
///

TEST ///
X = Gr{1,3};
(vol,chiT,chiCot,hodge) = invariants X;
vol
chiT
chiCot
X = Gr{1,4};
(vol2,chiT2,chiCot2,hodge2) = invariants(X,doHodge=>false,maxChiT=>2);
vol2
chiT2
R = rootSystemA(3);
X = homogeneousVariety(R,set{2,3});
F = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);
Y = embeddedVariety F;
(vol3,chiT3,chiCot3,hodge3) = invariants(Y,maxChiT=>1);
vol3
chiT3
///
