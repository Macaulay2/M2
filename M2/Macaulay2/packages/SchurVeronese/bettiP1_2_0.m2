--This file computes Betti tables for P^1 for d = 2 and b = 0
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => A_0^2*A_1^2},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({0,0},1)}, (0,1) => {}, (1,0) => {}, (1,1) => {({2,2},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{0,0}}, (0,1) => {}, (1,0) => {}, (1,1) => {{2,2}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {0,0}, (0,1) => {}, (1,0) => {}, (1,1) => {2,2}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 0, (1,1) => 1},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{2/1},
}
