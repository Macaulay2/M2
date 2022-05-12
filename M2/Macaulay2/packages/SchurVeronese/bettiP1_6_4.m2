--This file computes Betti tables for P^1 for d = 6 and b = 4
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 5, (0,1) => 0, (1,0) => 24, (1,1) => 0, (2,0) => 45, (2,1) => 0, (3,0) => 40, (3,1) => 0, (4,0) => 15, (4,1) => 0, (5,0) => 0, (5,1) => 1},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0^4+A_0^3*A_1+A_0^2*A_1^2+A_0*A_1^3+A_1^4, (1,0) => A_0^9*A_1+2*A_0^8*A_1^2+3*A_0^7*A_1^3+4*A_0^6*A_1^4+4*A_0^5*A_1^5+4*A_0^4*A_1^6+3*A_0^3*A_1^7+2*A_0^2*A_1^8+A_0*A_1^9, (0,1) => 0, (1,1) => 0, (2,0) => A_0^13*A_1^3+2*A_0^12*A_1^4+4*A_0^11*A_1^5+5*A_0^10*A_1^6+7*A_0^9*A_1^7+7*A_0^8*A_1^8+7*A_0^7*A_1^9+5*A_0^6*A_1^10+4*A_0^5*A_1^11+2*A_0^4*A_1^12+A_0^3*A_1^13, (3,0) => A_0^16*A_1^6+2*A_0^15*A_1^7+3*A_0^14*A_1^8+5*A_0^13*A_1^9+6*A_0^12*A_1^10+6*A_0^11*A_1^11+6*A_0^10*A_1^12+5*A_0^9*A_1^13+3*A_0^8*A_1^14+2*A_0^7*A_1^15+A_0^6*A_1^16, (2,1) => 0, (4,0) => A_0^18*A_1^10+A_0^17*A_1^11+2*A_0^16*A_1^12+2*A_0^15*A_1^13+3*A_0^14*A_1^14+2*A_0^13*A_1^15+2*A_0^12*A_1^16+A_0^11*A_1^17+A_0^10*A_1^18, (3,1) => 0, (5,0) => 0, (4,1) => 0, (5,1) => A_0^20*A_1^20},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({4,0},1)}, (0,1) => {}, (1,0) => {({9,1},1)}, (1,1) => {}, (2,0) => {({13,3},1)}, (2,1) => {}, (3,0) => {({16,6},1)}, (3,1) => {}, (4,0) => {({18,10},1)}, (4,1) => {}, (5,0) => {}, (5,1) => {({20,20},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{4,0}}, (0,1) => {}, (1,0) => {{9,1}}, (1,1) => {}, (2,0) => {{13,3}}, (2,1) => {}, (3,0) => {{16,6}}, (3,1) => {}, (4,0) => {{18,10}}, (4,1) => {}, (5,0) => {}, (5,1) => {{20,20}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {4,0}, (0,1) => {}, (1,0) => {9,1}, (1,1) => {}, (2,0) => {13,3}, (2,1) => {}, (3,0) => {16,6}, (3,1) => {}, (4,0) => {18,10}, (4,1) => {}, (5,0) => {}, (5,1) => {20,20}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 1, (3,1) => 0, (4,0) => 1, (4,1) => 0, (5,0) => 0, (5,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 1, (3,1) => 0, (4,0) => 1, (4,1) => 0, (5,0) => 0, (5,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 5, (0,1) => 0, (1,0) => 24, (1,1) => 0, (2,0) => 45, (2,1) => 0, (3,0) => 40, (3,1) => 0, (4,0) => 15, (4,1) => 0, (5,0) => 0, (5,1) => 1},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{720/1},
}
