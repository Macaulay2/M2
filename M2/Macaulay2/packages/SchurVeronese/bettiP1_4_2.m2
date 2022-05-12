--This file computes Betti tables for P^1 for d = 4 and b = 2
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 8, (1,1) => 0, (2,0) => 6, (2,1) => 0, (3,0) => 0, (3,1) => 1},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0^2+A_0*A_1+A_1^2, (1,0) => A_0^5*A_1+2*A_0^4*A_1^2+2*A_0^3*A_1^3+2*A_0^2*A_1^4+A_0*A_1^5, (0,1) => 0, (1,1) => 0, (2,0) => A_0^7*A_1^3+A_0^6*A_1^4+2*A_0^5*A_1^5+A_0^4*A_1^6+A_0^3*A_1^7, (3,0) => 0, (2,1) => 0, (3,1) => A_0^9*A_1^9},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({2,0},1)}, (0,1) => {}, (1,0) => {({5,1},1)}, (1,1) => {}, (2,0) => {({7,3},1)}, (2,1) => {}, (3,0) => {}, (3,1) => {({9,9},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{2,0}}, (0,1) => {}, (1,0) => {{5,1}}, (1,1) => {}, (2,0) => {{7,3}}, (2,1) => {}, (3,0) => {}, (3,1) => {{9,9}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {2,0}, (0,1) => {}, (1,0) => {5,1}, (1,1) => {}, (2,0) => {7,3}, (2,1) => {}, (3,0) => {}, (3,1) => {9,9}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 8, (1,1) => 0, (2,0) => 6, (2,1) => 0, (3,0) => 0, (3,1) => 1},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{24/1},
}
