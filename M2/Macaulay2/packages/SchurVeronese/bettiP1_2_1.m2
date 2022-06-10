--This file computes Betti tables for P^1 for d = 2 and b = 1
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 2, (0,1) => 0, (1,0) => 2, (1,1) => 0},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0+A_1, (0,1) => 0, (1,0) => A_0^2*A_1+A_0*A_1^2, (1,1) => 0},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({1,0},1)}, (0,1) => {}, (1,0) => {({2,1},1)}, (1,1) => {}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{1,0}}, (0,1) => {}, (1,0) => {{2,1}}, (1,1) => {}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {1,0}, (0,1) => {}, (1,0) => {2,1}, (1,1) => {}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 2, (0,1) => 0, (1,0) => 2, (1,1) => 0},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{2/1},
}
