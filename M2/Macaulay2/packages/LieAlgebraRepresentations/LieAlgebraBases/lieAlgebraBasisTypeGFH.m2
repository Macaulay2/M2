-- We implement the basis of g_2 described by Fulton and Harris in Section 22
-- The brackets are given in Table 22.1



-*
What are the roots in Dynkin coordinates?
a1 = {2,-1}
a2 = {-3,2}
a3=a1+a2 = {-1,1}
a4=a1+a3 = {1,0}
a5=a1+a4 = {3,-1}
a6=a5+a2 = {0,1}

This matches the lex-level order used in positiveRoots("G",2)
*-


-*

           H_1,    H_2,    X_1,    Y_1,    X_2,    Y_2,    X_3,    Y_3,    X_4,    Y_4,    X_5,    Y_5,    X_6,    Y_6
------------------------------------------------------------------------------------------------------------------------	   
H_1   {              0,  2*X_1, -2*Y_1, -3*X_2,  3*Y_2,   -X_3,    Y_3,    X_4,   -Y_4,  3*X_5, -3*Y_5,      0,      0},
H_2   {               ,   -X_1,    Y_1,  2*X_2, -2*Y_2,    X_3,   -Y_3,      0,      0,   -X_5,    Y_5,    X_6,   -Y_6},
X_1   {               ,       ,    H_1,    X_3,      0,  2*X_4, -3*Y_2, -3*X_5, -2*Y_3,      0,    Y_4,      0,      0},  
Y_1   {               ,       ,       ,      0,   -Y_3,  3*X_2, -2*Y_4,  2*X_3,  3*Y_5,   -X_4,      0,      0,      0},
X_2   {               ,       ,       ,       ,    H_2,      0,    Y_1,      0,      0,   -X_6,      0,      0,    Y_5},
Y_2   {               ,       ,       ,       ,       ,   -X_1,      0,      0,      0,      0,    Y_6,   -X_5,      0},
X_3   {               ,       ,       ,       ,       ,     ,H_1+3*H_2, -3*X_6,  2*Y_1,      0,      0,      0,    Y_4},
Y_3   {               ,       ,       ,       ,       ,     ,         , -2*X_1,  3*Y_6,      0,      0,   -X_4,      0},
X_4   {               ,       ,       ,       ,       ,       ,       ,   ,2*H_1+3*H_2,      0,   -Y_1,      0,   -Y_3},
Y_4   {               ,       ,       ,       ,       ,       ,       ,       ,       ,    X_1,      0,    X_3,      0},
X_5   {               ,       ,       ,       ,       ,       ,       ,       ,       ,       ,H_1+H_2,      0,   -Y_2},
Y_5   {               ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,    X_2,      0},
X_6   {               ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,H_1+2*H_2} 
*-
H=getSymbol "H";
X=getSymbol "X";
Y=getSymbol "Y";
G2ring = QQ[H_1,H_2,X_1,X_2,X_3,X_4,X_5,X_6,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6];
-- FH  = QQ[H_1,H_2,X_1,Y_1,X_2,Y_2,X_3,Y_3,X_4,Y_4,X_5,Y_5,X_6,Y_6];
--           0   1   2   3   4   5   6   7   8   9   10  11  12  13

preT= {
   {        ,      0,  2*G2ring_2, -2*G2ring_8, -3*G2ring_3,  3*G2ring_9,   -G2ring_4,    G2ring_10,    G2ring_5,   -G2ring_11,  3*G2ring_6, -3*G2ring_12,      0,      0},
   {        ,       ,   -G2ring_2,    G2ring_8,  2*G2ring_3, -2*G2ring_9,    G2ring_4,   -G2ring_10,      0,      0,   -G2ring_6,    G2ring_12,    G2ring_7,   -G2ring_13},
   {        ,       ,       ,    G2ring_0,    G2ring_4,      0,  2*G2ring_5, -3*G2ring_9, -3*G2ring_6, -2*G2ring_10,      0,    G2ring_11,      0,      0},  
   {        ,       ,       ,       ,      0,   -G2ring_10,  3*G2ring_3, -2*G2ring_11,  2*G2ring_4,  3*G2ring_12,   -G2ring_5,      0,      0,      0},
   {        ,       ,       ,       ,       ,    G2ring_1,      0,    G2ring_8,      0,      0,   -G2ring_7,      0,      0,    G2ring_12},
   {        ,       ,       ,       ,       ,       ,   -G2ring_2,      0,      0,      0,      0,    G2ring_13,   -G2ring_6,      0},
   {        ,       ,       ,       ,       ,       ,     ,G2ring_0+3*G2ring_1, -3*G2ring_7,  2*G2ring_8,      0,      0,      0,    G2ring_11},
   {        ,       ,       ,       ,       ,       ,     ,         , -2*G2ring_2,  3*G2ring_13,      0,      0,   -G2ring_5,      0},
   {        ,       ,       ,       ,       ,       ,       ,       ,   ,2*G2ring_0+3*G2ring_1,      0,   -G2ring_8,      0,   -G2ring_10},
   {        ,       ,       ,       ,       ,       ,       ,       ,       ,       ,    G2ring_2,      0,    G2ring_4,      0},
   {        ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,G2ring_0+G2ring_1,      0,   -G2ring_9},
   {        ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,    G2ring_3,      0},
   {        ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,G2ring_0+2*G2ring_1},
   {        ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,       ,         }
};


T = apply(14, i -> apply(14, j -> if i==j then 0_G2ring else if i>j then 1_G2ring*-preT_j_i else 1_G2ring*preT_i_j));

-- Match variables to the table row/column indices
sigma  = {0,1,2,4,6,8,10,12,3,5,7,9,11,13};
RtoZ = new HashTable from apply(14, i -> {G2ring_i,sigma_i});



g2brMonomial = (x,y) -> (
    R:=ring(x);
    if x==0 then return 0_R;
    if y==0 then return 0_R;
    T_(RtoZ#x)_(RtoZ#y)
);


g2br = (A,B) -> (
    R:=ring(A);
    if A==0 then return 0_R;
    if B==0 then return 0_R;
    1_R*(sum flatten apply(terms A, f -> apply(terms B, g -> leadCoefficient(f)*leadCoefficient(g)*g2brMonomial(leadMonomial(f),leadMonomial(g)))))
);


writeIng2Basis = (x) -> (
    apply(gens ring x, i -> coefficient(i,x))
);



G2DualBasis = {2/3*G2ring_0+G2ring_1,G2ring_0+2*G2ring_1,1/3*G2ring_8,G2ring_9,1/3*G2ring_10,1/3*G2ring_11,G2ring_12,G2ring_13,1/3*G2ring_2,G2ring_3,1/3*G2ring_4,1/3*G2ring_5,G2ring_6,G2ring_7};  


-- Lie algebra
-- Basis
-- Dual basis
-- Weights
-- Labels
-- RaisingOperatorIndices
-- LoweringOperatorIndices
-- WriteInBasis

g2BasisFH = () -> (
    --if m!=2 then error "Only implemented for m=2";
    new LieAlgebraBasis from {
	"LieAlgebra"=>simpleLieAlgebra("G",2),
        "BasisElements"=>gens G2ring,
	"Bracket"=> g2br,
	"DualBasis"=> G2DualBasis,
        "Weights"=>{{0, 0}, {0, 0}, {2, -1}, {-3, 2}, {-1, 1}, {1, 0}, {3, -1}, {0, 1}, {-2, 1}, {3, -2}, {1, -1}, {-1, 0}, {-3, 1}, {0, -1}},
	"Labels"=>{"H_1","H_2","X_1","X_2","X_3","X_4","X_5","X_6","Y_1","Y_2","Y_3","Y_4","Y_5","Y_6"},
	"RaisingOperatorIndices"=>toList(2..7),
	"LoweringOperatorIndices"=>toList(8..13),
	"WriteInBasis"=>writeIng2Basis
    }
)



