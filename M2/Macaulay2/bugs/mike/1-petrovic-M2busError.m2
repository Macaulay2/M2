--------------------
-- Creating the simple phylogenetic poly. ring R (we'll define a function that does it):
-- the variables are the joint probabilities.
--------------------
getPolyRing = numLeaves->(
     workingL={"A","G","C","T"}; dna=workingL; k=2; 
     while k<= numLeaves do {
     	  tempL =  flatten apply(0..#workingL-1, j->
     	       toList flatten apply(0..#dna-1, i -> concatenate(workingL_j,dna_i) )
     	       );
     	  workingL = tempL;
     	  k=k+1;
	  };
     L = toList apply(0..#workingL-1, i -> concatenate("q", workingL_i) );
     R=ZZ/101[apply(0..#L-1, i-> value(L_i))];
     --R=QQ[apply(0..#L-1, i-> value(L_i))];
     return R
)
-- Change the following to any desired number of leaves to get another phylogenetic polynomial ring.
R=getPolyRing(4);
vars R; -- there are 4^4 variables for 4 leaves.


--------------------
-- Creating the poly. ring S
-- to use the selectInSubring command with a degree bound instead of "eliminate": 
-- to avoid degree bound problem in selectInSubring command, we will set t to have degree zero.
--------------------
getPolyRingWithT = numLeaves->(
     workingL={"A","G","C","T"}; dna=workingL; k=2; 
     while k<= numLeaves do {
     	  tempL =  flatten apply(0..#workingL-1, j->
     	       toList flatten apply(0..#dna-1, i -> concatenate(workingL_j,dna_i) )
     	       );
     	  workingL = tempL;
     	  k=k+1;
	  };
     L = toList apply(0..#workingL-1, i -> concatenate("q", workingL_i) );
     --   S=ZZ/101[t,apply(0..#L-1, i-> value(L_i)),MonomialOrder=>Eliminate 1];
     degs="0";
     apply(1..#L,i->degs=concatenate{degs,",1"});
     S=ZZ/101[t,apply(0..#L-1, i-> value(L_i)),Degrees => toList value(degs),MonomialOrder=>Eliminate 1];
     return S
);
-- Change the following to any desired number of leaves to get another phylogenetic polynomial ring.
S=getPolyRingWithT(4);
vars S; -- there are 1 + 4^4 variables for 4 leaves.


--------------------
-- the two trees for the mixture model:
--------------------

-- 4-leaf tree:  {1,2}, {3,4}
--
-- There are 4 matrices that we need:
M1=matrix{
     {qAAAA,qAACC,qAAGG,qAATT},
     {qCCAA,qCCCC,qCCGG,qCCTT},
     {qGGAA,qGGCC,qGGGG,qGGTT},
     {qTTAA,qTTCC,qTTGG,qTTTT}};
--describe M1
M2=matrix{
     {qACAC,qACCA,qACGT,qACTG},
     {qCAAC,qCACA,qCAGT,qCATG},
     {qGTAC,qGTCA,qGTGT,qGTTG},
     {qTGAC,qTGCA,qTGGT,qTGTG}};
-- AC vs GT was M2
-- AG vs CT:
M3=matrix{
     {qAGAG,qAGGA,qAGCT,qAGTC},
     {qGAAG,qGAGA,qGACT,qGATC},
     {qCTAG,qCTGA,qCTCT,qCTTC},
     {qTCAG,qTCGA,qTCCT,qTCTC}};
-- AT vs CG:
M4=matrix{
     {qATAT,qATTA,qATCG,qATGC},
     {qTAAT,qTATA,qTACG,qTAGC},
     {qCGAT,qCGTA,qCGCG,qCGGC},
     {qGCAT,qGCTA,qGCCG,qGCGC}};
-- The ideal [I_T*I_T]_3 is gen. by 3-minors of the 4 matrices:
Itree1deg3 = minors(3,M1) +minors(3,M2) +minors(3,M3) +minors(3,M4) ;

-- 4-leaf tree:  {1,3},{2,4}
-- There are 4 matrices that we need:
-- same:
N1=matrix{
     {qAAAA,qACAC,qAGAG,qATAT},
     {qCACA,qCCCC,qCGCG,qCTCT},
     {qGAGA,qGCGC,qGGGG,qGTGT},
     {qTATA,qTCTC,qTGTG,qTTTT}};
-- AC vs GT:
N2=matrix{
     {qAACC,qACCA,qAGCT,qATCG},
     {qCAAC,qCCAA,qCGAT,qCTAG},
     {qGATC,qGCTA,qGGTT,qGTTG},
     {qTAGC,qTCGA,qTGGT,qTTGG}};
-- AG vs CT:
N3=matrix{
     {qAAGG,qAGGA,qACGT,qATGC},
     {qGAAG,qGGAA,qGCAT,qGTAC},
     {qCATG,qCGTA,qCCTT,qCTTC},
     {qTACG,qTGCA,qTCCT,qTTCC}};
-- AT vs CG:
N4=matrix{
     {qAATT,qATTA,qACTG,qAGTC},
     {qTAAT,qTTAA,qTCAG,qTGAC},
     {qCAGT,qCTGA,qCCGG,qCGGC},
     {qGACT,qGTCA,qGCCG,qGGCC}};
Itree2deg3 = minors(3,N1) +minors(3,N2) +minors(3,N3) +minors(3,N4) ;


-- intersect as ideals:
-- here i DO NOT need the variable T at all:
--       IjoinDeg3 = intersect(Itree1deg3,Itree2deg3) -- ran out of memory!!
---in Macaulay2.1.1, Dec.08, I get a bus error.

time      IjoinDeg3 = intersect(Itree1deg3,Itree2deg3)