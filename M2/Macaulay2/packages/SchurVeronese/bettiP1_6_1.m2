--This file computes Betti tables for P^1 for d = 6 and b = 1
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(0,0) => 2, (0,1) => 0, (1,0) => 6, (1,1) => 0, (2,0) => 0, (2,1) => 20, (3,0) => 0, (3,1) => 30, (4,0) => 0, (4,1) => 18, (5,0) => 0, (5,1) => 4},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(0,0) => A_0+A_1, (1,0) => A_0^6*A_1+A_0^5*A_1^2+A_0^4*A_1^3+A_0^3*A_1^4+A_0^2*A_1^5+A_0*A_1^6, (0,1) => 0, (1,1) => 0, (2,0) => 0, (3,0) => 0, (2,1) => A_0^14*A_1^5+A_0^13*A_1^6+2*A_0^12*A_1^7+3*A_0^11*A_1^8+3*A_0^10*A_1^9+3*A_0^9*A_1^10+3*A_0^8*A_1^11+2*A_0^7*A_1^12+A_0^6*A_1^13+A_0^5*A_1^14, (4,0) => 0, (3,1) => A_0^17*A_1^8+2*A_0^16*A_1^9+3*A_0^15*A_1^10+4*A_0^14*A_1^11+5*A_0^13*A_1^12+5*A_0^12*A_1^13+4*A_0^11*A_1^14+3*A_0^10*A_1^15+2*A_0^9*A_1^16+A_0^8*A_1^17, (5,0) => 0, (4,1) => A_0^19*A_1^12+2*A_0^18*A_1^13+3*A_0^17*A_1^14+3*A_0^16*A_1^15+3*A_0^15*A_1^16+3*A_0^14*A_1^17+2*A_0^13*A_1^18+A_0^12*A_1^19, (5,1) => A_0^20*A_1^17+A_0^19*A_1^18+A_0^18*A_1^19+A_0^17*A_1^20},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(0,0) => {({1,0},1)}, (0,1) => {}, (1,0) => {({6,1},1)}, (1,1) => {}, (2,0) => {}, (2,1) => {({14,5},1)}, (3,0) => {}, (3,1) => {({17,8},1)}, (4,0) => {}, (4,1) => {({19,12},1)}, (5,0) => {}, (5,1) => {({20,17},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(0,0) => {{1,0}}, (0,1) => {}, (1,0) => {{6,1}}, (1,1) => {}, (2,0) => {}, (2,1) => {{14,5}}, (3,0) => {}, (3,1) => {{17,8}}, (4,0) => {}, (4,1) => {{19,12}}, (5,0) => {}, (5,1) => {{20,17}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(0,0) => {1,0}, (0,1) => {}, (1,0) => {6,1}, (1,1) => {}, (2,0) => {}, (2,1) => {14,5}, (3,0) => {}, (3,1) => {17,8}, (4,0) => {}, (4,1) => {19,12}, (5,0) => {}, (5,1) => {20,17}},
--nr encodes the number of distinct representations in each entry
"nr"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 0, (2,1) => 1, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1, (5,0) => 0, (5,1) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(0,0) => 1, (0,1) => 0, (1,0) => 1, (1,1) => 0, (2,0) => 0, (2,1) => 1, (3,0) => 0, (3,1) => 1, (4,0) => 0, (4,1) => 1, (5,0) => 0, (5,1) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(0,0) => 2, (0,1) => 0, (1,0) => 6, (1,1) => 0, (2,0) => 0, (2,1) => 20, (3,0) => 0, (3,1) => 30, (4,0) => 0, (4,1) => 18, (5,0) => 0, (5,1) => 4},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{720/1},
}
