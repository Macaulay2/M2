--This file computes Betti tables for P^1 for d = 6 and b = 2
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 12, (1,1) => 0, (2,0) => 15, (2,1) => 0, (3,0) => 0, (3,1) => 15, (4,0) => 0, (4,1) => 12, (5,0) => 0, (5,1) => 3},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0^2+A_0*A_1+A_1^2, (1,0) => A_0^7*A_1+2*A_0^6*A_1^2+2*A_0^5*A_1^3+2*A_0^4*A_1^4+2*A_0^3*A_1^5+2*A_0^2*A_1^6+A_0*A_1^7, (0,1) => 0, (1,1) => 0, (2,0) => A_0^11*A_1^3+A_0^10*A_1^4+2*A_0^9*A_1^5+2*A_0^8*A_1^6+3*A_0^7*A_1^7+2*A_0^6*A_1^8+2*A_0^5*A_1^9+A_0^4*A_1^10+A_0^3*A_1^11, (3,0) => 0, (2,1) => 0, (4,0) => 0, (3,1) => A_0^17*A_1^9+A_0^16*A_1^10+2*A_0^15*A_1^11+2*A_0^14*A_1^12+3*A_0^13*A_1^13+2*A_0^12*A_1^14+2*A_0^11*A_1^15+A_0^10*A_1^16+A_0^9*A_1^17, (5,0) => 0, (4,1) => A_0^19*A_1^13+2*A_0^18*A_1^14+2*A_0^17*A_1^15+2*A_0^16*A_1^16+2*A_0^15*A_1^17+2*A_0^14*A_1^18+A_0^13*A_1^19, (5,1) => A_0^20*A_1^18+A_0^19*A_1^19+A_0^18*A_1^20},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({2,0},1)}, (0,1) => {}, (1,0) => {({7,1},1)}, (1,1) => {}, (2,0) => {({11,3},1)}, (2,1) => {}, (3,0) => {}, (3,1) => {({17,9},1)}, (4,0) => {}, (4,1) => {({19,13},1)}, (5,0) => {}, (5,1) => {({20,18},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{2,0}}, (0,1) => {}, (1,0) => {{7,1}}, (1,1) => {}, (2,0) => {{11,3}}, (2,1) => {}, (3,0) => {}, (3,1) => {{17,9}}, (4,0) => {}, (4,1) => {{19,13}}, (5,0) => {}, (5,1) => {{20,18}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {2,0}, (0,1) => {}, (1,0) => {7,1}, (1,1) => {}, (2,0) => {11,3}, (2,1) => {}, (3,0) => {}, (3,1) => {17,9}, (4,0) => {}, (4,1) => {19,13}, (5,0) => {}, (5,1) => {20,18}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1, (5,0) => 0, (5,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 1, (2,1) => 0, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1, (5,0) => 0, (5,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 3, (0,1) => 0, (1,0) => 12, (1,1) => 0, (2,0) => 15, (2,1) => 0, (3,0) => 0, (3,1) => 15, (4,0) => 0, (4,1) => 12, (5,0) => 0, (5,1) => 3},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{720/1},
}
