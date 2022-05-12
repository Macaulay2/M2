--This file computes Betti tables for P^1 for d = 5 and b = 2
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 10, (1,1) => 0, (2,0) => 10, (2,1) => 0, (3,0) => 0, (3,1) => 5, (4,0) => 0, (4,1) => 2},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0^2+A_0*A_1+A_1^2, (1,0) => A_0^6*A_1+2*A_0^5*A_1^2+2*A_0^4*A_1^3+2*A_0^3*A_1^4+2*A_0^2*A_1^5+A_0*A_1^6, (0,1) => 0, (1,1) => 0, (2,0) => A_0^9*A_1^3+A_0^8*A_1^4+2*A_0^7*A_1^5+2*A_0^6*A_1^6+2*A_0^5*A_1^7+A_0^4*A_1^8+A_0^3*A_1^9, (3,0) => 0, (2,1) => 0, (4,0) => 0, (3,1) => A_0^13*A_1^9+A_0^12*A_1^10+A_0^11*A_1^11+A_0^10*A_1^12+A_0^9*A_1^13, (4,1) => A_0^14*A_1^13+A_0^13*A_1^14},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({2,0},1)}, (0,1) => {}, (1,0) => {({6,1},1)}, (1,1) => {}, (2,0) => {({9,3},1)}, (2,1) => {}, (3,0) => {}, (3,1) => {({13,9},1)}, (4,0) => {}, (4,1) => {({14,13},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{2,0}}, (0,1) => {}, (1,0) => {{6,1}}, (1,1) => {}, (2,0) => {{9,3}}, (2,1) => {}, (3,0) => {}, (3,1) => {{13,9}}, (4,0) => {}, (4,1) => {{14,13}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {2,0}, (0,1) => {}, (1,0) => {6,1}, (1,1) => {}, (2,0) => {9,3}, (2,1) => {}, (3,0) => {}, (3,1) => {13,9}, (4,0) => {}, (4,1) => {14,13}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 10, (1,1) => 0, (2,0) => 10, (2,1) => 0, (3,0) => 0, (3,1) => 5, (4,0) => 0, (4,1) => 2},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{120/1},
}
