--This file computes Betti tables for P^1 for d = 4 and b = 0
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 6, (2,0) => 0, (2,1) => 8, (3,0) => 0, (3,1) => 3},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => 1, (1,0) => 0, (0,1) => 0, (1,1) => A_0^6*A_1^2+A_0^5*A_1^3+2*A_0^4*A_1^4+A_0^3*A_1^5+A_0^2*A_1^6, (2,0) => 0, (3,0) => 0, (2,1) => A_0^8*A_1^4+2*A_0^7*A_1^5+2*A_0^6*A_1^6+2*A_0^5*A_1^7+A_0^4*A_1^8, (3,1) => A_0^9*A_1^7+A_0^8*A_1^8+A_0^7*A_1^9},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({0,0},1)}, (0,1) => {}, (1,0) => {}, (1,1) => {({6,2},1)}, (2,0) => {}, (2,1) => {({8,4},1)}, (3,0) => {}, (3,1) => {({9,7},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{0,0}}, (0,1) => {}, (1,0) => {}, (1,1) => {{6,2}}, (2,0) => {}, (2,1) => {{8,4}}, (3,0) => {}, (3,1) => {{9,7}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {0,0}, (0,1) => {}, (1,0) => {}, (1,1) => {6,2}, (2,0) => {}, (2,1) => {8,4}, (3,0) => {}, (3,1) => {9,7}},
--nr encodes the number of distinct representations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1, (2,0) => 0, (2,1) => 1, (3,0) => 0, (3,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1, (2,0) => 0, (2,1) => 1, (3,0) => 0, (3,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 6, (2,0) => 0, (2,1) => 8, (3,0) => 0, (3,1) => 3},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{24/1},
}
