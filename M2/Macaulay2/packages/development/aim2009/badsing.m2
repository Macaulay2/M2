-- Compute quiver coefficients for Zwara's orbit closure with non-rational 
-- singularities, for the Kronecker quiver

loadPackage "QuiverCycles";
Q = quiver(2, {0,0}, {1,1});
dv = {3, 3};
rep = {{{0,0,0},{1,0,0},{0,1,0}}, {{1,0,0},{0,0,0},{0,0,1}}};
cc = cycleClass(Q, dv, rep);
qc = quiverCoefficients(Q, dv, cc)

