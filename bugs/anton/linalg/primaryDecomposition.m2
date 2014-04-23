R=QQ[x11,x22,x21,x12,x23,x13,x14,x24];
system={x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14};
PD=primaryDecomposition(ideal(system))
