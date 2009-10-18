setRandomSeed();
R = ZZ/101[y11, y12, y21, y22];
PolynomialSystem = apply(1..4, i -> 
                random(0, R) + random(1, R) + random(2, R));
I = ideal PolynomialSystem;
dim I, degree I
J = ideal (random(R^4, R^7) *  transpose(
        matrix{{1, y11, y12, y21, y22, y11*y22, y12*y21}}));
dim J, degree J
K = ideal (random(R^4, R^6) * transpose( 
        matrix{{1, y11, y12, y21, y22, y11*y22 - y12*y21}}));
dim K, degree K
R = ZZ/7[y, x, MonomialOrder=>Lex];
I = ideal (y^3*x^2 + 2*y^2*x + 3*x*y,  3*y^2 + x*y - 3*y);
J = saturate(I, ideal(y))
factor(J_1)
load "realroots.m2"
code eliminant
code regularRep
code charPoly
code SturmSequence
code numRealSturm
code numPosRoots
code variations
code traceForm
code traceFormSignature
code numRealTrace
R = QQ[x, y];
I = ideal (1 - x^2*y + 2*x*y^2,  y - 2*x - x*y + x^2);
dim I, degree I
A = R/I;
time g = eliminant(x, QQ[Z])
time g = charPoly(x, Z)
numRealSturm(g), numRealTrace(A)
traceFormSignature(x*y);
traceFormSignature(x - 2);
traceFormSignature(x + y - 3);
Points = {{2, 2,  0 }, {1, -2,  0}, {-3, 0, 0}, 
          {0, 0, 5/2}, {0,  0, -3}};
R = QQ[r, y11, y12, y21, y22];
P = matrix{{0, y11, y12}};
V = matrix{{1, y21, y22}};
Points = matrix Points ** R;
I = ideal apply(0..4, i -> (
          X := Points^{i};
          r * (V * transpose V)  +
           ((X - P) * transpose V)^2) -
           ((X - P) * transpose(X - P)) * (V * transpose V)
          );
dim I, degree I
A = R/I; numPosRoots(charPoly(r, Z))
Sphere = (a, b, c, r) -> (
        matrix{{a^2 + b^2 + c^2 - r ,-a ,-b ,-c },
               {         -a         , 1 , 0 , 0 },
               {         -b         , 0 , 1 , 0 },
               {         -c         , 0 , 0 , 1 }}
        );
R = QQ[y11, y12, y21, y22];
tangentTo = (M) -> (
     P := matrix{{1, 0, y11, y12}};
     V := matrix{{0, 1, y21, y22}};
     (P * M * transpose V)^2 - 
       (P * M * transpose P) * (V * M * transpose V)
     );
I = ideal (tangentTo(Sphere(0,0,0,5)), 
           tangentTo(Sphere(4,1,1,5)), 
           tangentTo(Sphere(1,4,1,5)), 
           tangentTo(Sphere(1,1,4,5)));
dim I, degree I
A = R/I;
numRealSturm(eliminant(y11 - y12 + y21 + y22, QQ[Z]))
R = ZZ/101[apply(subsets(5,2), i -> p_i )];
I = Grassmannian(1, 4, R)
dim(Proj(R/I)), degree(I)
oscPlane = (i, n, s) -> (
     gamma := matrix {toList apply(1..n, i -> s^(i-1))};
     L := gamma;
     j := 0;
     while j < i-1 do (gamma = diff(s, gamma); 
          L = L || gamma;
          j = j+1);
     L);
QQ[s]; oscPlane(3, 6, s)
spSchub = (r, L, P) -> (
     I := ideal apply(subsets(numgens source L, 
                      r + numgens target L), S -> 
          fold((sum, U) -> sum +
           fold((term,i) -> term*(-1)^i, P_(S_U) * det(
            submatrix(L, sort toList(set(S) - set(S_U)))), U), 
               0, subsets(#S, r))));
R = QQ[apply(subsets(6,3), i -> p_i )];
I = fold((J, i) -> J +
      spSchub(3, substitute(oscPlane(3, 6, s), {s=> 1+i}), p) +
      spSchub(3, substitute(oscPlane(2, 6, s), {s=> 4+i}), p), 
      Grassmannian(2, 5, R), {0,1,2}) + 
     ideal (p_{0,1,5} - 1);
dim I, degree I
A = R/I; numRealSturm(eliminant(p_{2,3,4}, QQ[Z]))
randL = (R, n, r, l) -> 
          matrix table(n-r+1-l, n, (i, j) -> random(0, R));
testTransverse = F -> (
      R := F[apply(subsets(6, 3), i -> q_i )];
      Continue := true;
      j := 0;  
      limit := 5;
      while Continue and (j < limit) do (
           j = j + 1;
           I := fold((J, i) -> J + 
                     spSchub(3, randL(R, 6, 3, 1), q) +
                     spSchub(3, randL(R, 6, 3, 2), q),
                     Grassmannian(2, 5, R) + 
                     ideal (1 + random(1, R)),
                     {0, 1, 2});
           if (dim I == 0) and (degree I == 6) then (
           lin := promote(random(1, R), (R/I));
           g := charPoly(lin, Z);
           Continue = not(1 == gcd(g, diff(Z, g)));
           ));
      if Continue then << "Failed for the prime " << char F << 
         " with " << j << " iterations" << endl;
      if not Continue then << "Succeeded for the prime " <<
          char F << " in " << j << " iteration(s)" << endl;
      );
testTransverse(ZZ/2);
(random 10;for i to 7 do random 2;testTransverse(GF 4);)
testTransverse(ZZ/7);
randomSymmetricMatrix = (R, n) -> (
    entries := new MutableHashTable;
    scan(0..n-1, i -> scan(i..n-1, j -> 
                 entries#(i, j) = random(0, R)));
    matrix table(n, n, (i, j) -> if i > j then 
                 entries#(j, i) else entries#(i, j))
    );
tangentEquation = (r, R, M) -> (
     g := matrix {gens(R)};
     (entries(g * exteriorPower(r, M) * transpose g))_0_0
     );
R = QQ[apply(subsets(4, 2), i -> p_i )];
I = Grassmannian(1, 3, R) + ideal apply(0..3, i -> 
     tangentEquation(2, R, randomSymmetricMatrix(R, 4)));
dim Proj(R/I), degree I
I = Grassmannian(1, 3, R) + 
        ideal (tangentEquation(2, R, Sphere(0,0,0,5)),
               tangentEquation(2, R, Sphere(4,1,1,5)),
               tangentEquation(2, R, Sphere(1,4,1,5)),
               tangentEquation(2, R, Sphere(1,1,4,5)));
dim Proj(R/I), degree I
Lines = saturate(I, ideal (p_{0,1}));
dim Proj(R/Lines), degree(Lines)
Junk = I : Lines;
dim Proj(R/Junk), degree Junk
radical(Junk)
Two = (a, b, c, r) -> (
     matrix{{a^2 + b^2 - c^2 + r ,-a ,-b , c },
            {         -a         , 1 , 0 , 0 },
            {         -b         , 0 , 1 , 0 },
            {          c         , 0 , 0 ,-1 }}
     );
One = (a, b, c, r) -> (
     matrix{{a^2 + b^2 - c^2 - r ,-a ,-b , c },
            {         -a         , 1 , 0 , 0 },
            {         -b         , 0 , 1 , 0 },
            {          c         , 0 , 0 ,-1 }}
     );
R = QQ[y11, y12, y21, y22];
I = ideal (tangentTo(One( 5, 3, 3,16)), 
           tangentTo(One( 5,-4, 2, 1)),  
           tangentTo(One(-3,-1, 1, 1)), 
           tangentTo(One( 2,-7, 0, 1)));
numRealSturm(charPoly(promote(y22, R/I), Z))
I = ideal (tangentTo(One( 3,-2,-3, 6)), 
           tangentTo(One(-3,-7,-6, 7)),  
           tangentTo(One(-6, 3,-5, 2)), 
           tangentTo(Two( 1, 6,-2, 5)));
numRealSturm(charPoly(promote(y22, R/I), Z))
I = ideal (tangentTo(One( 6, 4, 6, 4)),  
           tangentTo(One(-1, 3, 3, 6)), 
           tangentTo(Two(-7,-2, 3, 3)), 
           tangentTo(Two(-6, 7,-2, 5)));
numRealSturm(charPoly(promote(y22, R/I), Z))
I = ideal (tangentTo(One(-1,-4,-1, 1)),
           tangentTo(Two(-3, 3,-1, 1)),  
           tangentTo(Two(-7, 6, 2, 9)), 
           tangentTo(Two( 5, 6,-1,12)));
numRealSturm(charPoly(promote(y22, R/I), Z))
I = ideal (tangentTo(Two( 5, 2,-1,25)), 
           tangentTo(Two( 6,-6, 2,25)), 
           tangentTo(Two(-7, 1, 6, 1)), 
           tangentTo(Two( 3, 1, 0, 1)));
numRealSturm(charPoly(promote(y22, R/I), Z))
tanQuad = (M, X) -> (
     u := X^{0};
     v := X^{1};
     (u * M * transpose v)^2 - 
     (u * M * transpose u) * (v * M * transpose v)
     );
nSphere = (V, r) -> 
         (matrix {{r + V * transpose V}} || transpose V ) |
         ( V || id_((ring r)^n)
         );
V = () -> matrix table(1, n, (i,j) -> random(0, R));
r = () -> random(0, R);
n = 4;
R = ZZ/1009[flatten(table(2, n-1, (i,j) -> z_(i,j)))];
X = 1 | matrix table(2, n-1, (i,j) -> z_(i,j))
I = ideal (apply(1..(2*n-2), 
               i -> tanQuad(nSphere(V(), r()), X)));
dim I, degree I
