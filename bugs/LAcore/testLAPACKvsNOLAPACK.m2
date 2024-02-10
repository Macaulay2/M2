-*
M2 --script testLAPACKvsNOLAPACK.m2 &> NOLAPACK.output
*-
setRandomSeed 0
load "testEigenvectors.m2"
<< "version: " << version#"VERSION" << endl
<< "machine: " << version#"machine" << endl  
<< "n-by-n matrix with entries in RR_53" <<endl
<< "n=100: " << testEigenvectors(100,RR_53, false) << endl
<< "n=100, symmetric: " << testEigenvectors(100,RR_53, true) << endl
<< "n=500: " << testEigenvectors(500,RR_53, false) << endl
<< "n=500, symmetric: " << testEigenvectors(500,RR_53, true) << endl
<< "n-by-n matrix with entries in CC_53" <<endl
<< "n=100: " << testEigenvectors(100,CC_53, false) << endl
<< "n=100, Hermitian: " << testEigenvectors(100,CC_53, true) << endl
<< "n=500: " << testEigenvectors(500,CC_53, false) << endl
<< "n=500, Hermitian: " << testEigenvectors(500,CC_53, true) << endl
