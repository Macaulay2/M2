-- -*- coding: utf-8 -*-
-- solvePackage.m2
-- Copyright (C) 2017  Laura Menini, Corrado Possieri, Antonio Tornambe

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 3 of the License, or (at
--  your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, see <http://www.gnu.org/licenses/>.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newPackage (
 "NumericSolutions",
 Version => "1.0",
 Date => "Apr 3, 2017",
 Headline => "solving systems of polynomial equalities",
 Keywords => {"Numerical Algebraic Geometry"},
 Authors => {
  {Name => "Laura Menini", Email => "menini@disp.uniroma2.it"},
  {Name => "Corrado Possieri", Email => "possieri@ing.uniroma2.it"},
  {Name => "Antonio Tornambe", Email => "tornambe@disp.uniroma2.it"}
 }
)
     
export {
 "compMatr",
 "traceForm",
 "solveSystem",
 "jordanForm",
 -- options
 "Tolerance"
}

compMatr = method(TypicalValue => Matrix)
compMatr(Ideal,RingElement) := (I,f) -> (
 -- computes the companion matrix of the ideal I associated to the 
 -- polynomial f over the ring R. The ideal I needs to be zero dimensional
 -- over the ring R, otherwise the function returns an error.
 
 -- inputs: the ring R where the ideal is defined,
 --         the ideal I,
 --         the function f,
 
 -- output: the companion matrix associated to the 
 --         function f, of the ideal I over the ring R.
 
 -- find the ring
 R := ring I;
 
 -- check that the ideal I is zero dimensional.
 if dim I != 0 then 
  error "the ideal is not zero dimensional";
 
 -- compute the quotient ring R/I and a monomial basis of this quotient ring.
 quotRing := R/I;
 B := basis(quotRing);
 
 -- m denotes the number of elements in the basis (i.e., the number of different complex
 -- point in the affine variety V(I)).
 m := numgens source B;
 
 -- compute the linear map M: R/I -> R/I, (f*g mod I) = (M*g mod I).
 M := new MutableMatrix from id_(quotRing^m);
 for jj from 0 to m-1 do (
  colVal := f*B_(0,jj);
  for ii from 0 to m-1 do (
   v := coefficient(B_(0,ii),colVal);
   vv := promote(v,quotRing);
   M_(ii,jj) = vv;
  );
 );
 out := new Matrix from M;
 
 -- the function returns the linear map M.
 out
);

traceForm = method(TypicalValue => Matrix)
traceForm(Ideal) := (I) -> (
 -- this function computes the trace matrix of the zero dimensional
 -- ideal I to characterize the number of real points in V(I)
 
 -- find the ring
 R := ring I;
 
 -- check that the ideal I is zero dimensional.
 if dim I != 0 then 
  error "the ideal is not zero dimensional";

 -- compute the quotient ring and its basis
 quotRing := R/I;
 B := basis(quotRing);
 m := numColumns B-1;
 
 -- compute the matrices M_{b_i}^A
 L := new MutableList;
 for ii from 0 to m do (
  L#ii = compMatr(I,B_(0,ii));
 );
 
 -- initialize the trace matrix
 T := new MutableMatrix from id_(quotRing^(m+1));
 
 -- compute the entries of the trace matrix
 for ii from 0 to m do (
  for jj from  ii to m do (
   tempTr := trace(L#ii * L#jj);
   T_(ii,jj) = tempTr;
   T_(jj,ii) = tempTr;
  );
 );
 
 -- return the trace matrix
 new Matrix from T
);

solveSystem = method(TypicalValue => Tally,Options => {Tolerance => 10.^-3})
solveSystem(Ideal) := opts -> (I) -> (
 -- compute the coordinates of the points in the affine variety V(I).
 -- the ideal I needs to be zero dimensional, otherwise the function returns an error.
 
 -- inputs: the ring R where the ideal is defined,
 --         the ideal I.
 
 -- output: the solutions
 toll := opts.Tolerance;
 R := ring I;
 
 -- check if the ideal is zero dimensional.
 if dim I != 0 then 
  error "the ideal is not zero dimensional";
 
 -- define the variables vector.
 var := gens R;
 
 -- compute the quotient ring R/I and a monomial basis.
 quotRing := R/I;
 B := basis(quotRing);
 
 -- m denotes the number of elements in the basis (i.e., the number of different complex
 -- point in the affine variety V(I)).
 m := numgens source B;
 
 -- define the coefficient ring. 
 coeffRing := coefficientRing(R);
 
 -- n denotes the number of variables to be considered
 n := length var;
 
 -- solMat denotes the list of the companion matrices associated to the
 -- ring R variables of the ideal I.
 solMat := {};
 
 -- if there is a single variable return the eigenvalues of the companion matrix
 -- associated to such a variable,
 if n == 1 then (
  solMat = substitute(compMatr(I,var_(ii-1)),coeffRing);
  eigenvalues(solMat)
 ) 
 
 -- otherwise compute the eigenvectors of the first companion matrix and use them
 -- to find all the coordinates of the points in V(I) (the method used here to compute 
 -- such points is based on the Stickelberger’s Theorem).
 else (
 
  -- each row of the matrix solutions is a point in the affine variety V(I),
  -- the columns are arranged according to the order of the variables in the ring R.
  solutions := new MutableMatrix from random(CC^m,CC^n);
  
  -- computation of the companion matrices of the ideal I.
  for kk from 1 to n do (
   solMat = append(solMat,substitute(compMatr(I,var_(kk-1)),coeffRing));
  );
  
  -- computation of the eigenvectors of the companion matrices (they have 
  -- the same set of eigenvectors).
  eigVec := (eigenvectors(solMat_(0)))_(1);
  
  -- computation of the associated eigenvalues, which constitute a point in V(I).
  flag := 0;
  for ii from 0 to m-1 do (
   vecs := eigVec_{ii};
   for jj from 0 to n-1 do (
    MMM := solMat_(jj);
    index := -1;
    parvec := MMM*vecs;
    for ll from 0 to numrows eigVec - 1 do (
     if vecs_(ll,0) != 0+0*ii then (
      index = ll;
      break;
     );
    );
    pareig := (parvec_(index,0))/(vecs_(index,0));
    for ll from 0 to numrows eigVec - 1 do(
     if abs(pareig*vecs_(ll,0) - parvec_(ll,0)) > toll and flag == 0 then (
      flag = 1;
      print("-- eigenvectors not matching, attempting computations of Jordan forms");
      break;
     );
     if flag == 0 then (
      solutions_(ii,jj) = pareig;
     );
    );
   );
  
   if flag == 1 then (
    break;
   );
  );
  
  -- the function returns the vector of the solutions
  out := 0;
  if flag == 0 then (
   out = new Matrix from solutions;
   outList := new MutableList;
   for rr1 from 0 to numRows out - 1 do (
    tsol := new MutableList;
    for cc1 from 0 to numColumns out - 1 do (
     tsol#cc1 = out_(rr1,cc1);
    );
    outList#rr1 = new List from tsol;
   );
   out = new List from outList;
   out = tally out;
  ) else (
   out = NSolve(R,I,solMat,toll);
   solList := out#0;
   voutList := new MutableList;
   count := 0;
   for jj from 0 to length solList - 1 do (
    for ll from 0 to (out#1)#jj - 1 do (
     voutList#count = solList#jj;
     count = count + 1;
    );
   );
   out = new List from voutList;
   out = tally out;
  );
  out
 )
)

NSolve = (R,I,M,toll) -> (
 -- compute numerically a solution to a system of equalities
 
 -- inputs: the ring R where the ideal is defined,
 --          the ideal I,
 --          the companion matrices M of the ideal I
 --          the tolerance toll.
 
 -- output: the list vecSol containing the solutions,
 --         the list multSol containing the multiplicity of each solution.
 
 -- variables employed
 varR :=  gens R;
 n := length varR;
 m := numrows M_(0);
 toll = toRR toll;
 
 -- new coefficient ring
 R2 := CC[varR];
 
 -- error when computing Jordan Forms
 errJf := 0;
 
 -- compute the jordan forms of the various matrices
 genEigenvecs := new MutableList;
 genEigenvals := new MutableList;
 for ll from 0 to n-1 do (
  (pCh,sepEigs,peigs,palgm,JF) := jFor(M_(ll), toll);
  if length sepEigs == 0 then (
   errJf = 1;
   break;
  );
  ppCh := substitute(pCh,R2);
  pMM := substitute(M_(ll),R2);
  -- print JF;
  psepEigs := new MutableList;
  for jj from 0 to length sepEigs - 1 do (
   psepEigs#jj = substitute(sepEigs#jj,R2);
  );
  psepEigs = new List from psepEigs;
  -- print (pCh, psepEigs);
  genEigenvecs#ll = psepEigs;
  genEigenvals#ll = peigs;
 );
 
 -- initialize the vector of solutions
 indMatr := {};
 for jj from 0 to n-1 do (
  indMatr = append(indMatr,substitute(0,ZZ));
 );
 indMatr = new MutableList from indMatr;
 vecSol := new MutableList;
 multSol := new MutableList;
 count := 0;
 
 -- if no numerical error occurred while computing jordan forms
 if errJf == 0 then (
  -- by intersecting the generalized eigenspaces determine 
  -- the solutions and their multiplicity
  totDim := 0;
  while totDim < m do (
   -- consider one element of the array
   -- print new List from indMatr;
   parSol := new MutableList;
   insSpace := image id_(R2^m);
   for jj from 0 to n-1 do (
    parSol#jj = (genEigenvals#jj)#(indMatr#jj);
    -- intersect the generalized eigenspaces
    insSpace = intersect(insSpace, image (genEigenvecs#jj)#(indMatr#jj));
   );
   parSol = new List from parSol;
   -- print parSol;
   dimSpace := numColumns gens gb insSpace;
   -- print dimSpace;
   -- if the intersection is not zero add the solution
   if dimSpace > 0 then (
    vecSol#count = parSol;
    multSol#count = dimSpace;
    count = count + 1;
    totDim = totDim + dimSpace;
   );
   -- continue exploring the space of solutions
   indMatr#(n-1) = indMatr#(n-1) + 1;
   -- print new List from indMatr;
   for ll from 1 to n-1 do (
    if indMatr#(n-ll) == length(genEigenvals#(n-ll)) then (
     indMatr#(n-ll) = 0;
     indMatr#(n-ll-1) = indMatr#(n-ll-1) + 1;
    );
   );
   if (indMatr#0) == length(genEigenvals#0) then (
    break;
   );
  );
  vecSol = new List from vecSol;
  multSol = new List from multSol;
 
  -- print (totDim,m);
  -- print vecSol;
  if totDim < m then (
   print "-- multiplicities not computed due to";
   print "-- numerical errors in computing intersections of subspaces";
   vecSol = enumSolve(R,I,M,toll);
   multSol = new MutableList;
   for kk from 0 to length vecSol - 1 do (
    multSol#kk = 1;
   );
   multSol = new List from multSol;
  );
 ) else (
  print "-- multiplicities not computed due to";
  print "-- numerical errors in computing matrices kernels";
  vecSol = enumSolve(R,I,M,toll);
  -- print vecSol;
  multSol = new MutableList;
  for kk from 0 to length vecSol - 1 do (
   multSol#kk = 1;
  );
  multSol = new List from multSol;
 );
 
 -- print vecSol;
 -- print multSol;
 
 -- return the list of solutions together with their multiplicity
 (vecSol, multSol)
)


jFor = (A,toll) -> (
 -- compute the Jordan canonical form of a given matrix
 
 -- inputs: the matrix A,
 --         the tolerance toll.
 
 -- output: the matrix chBasMatr that transforms A in Jordan form,
 --         the generalized eigenvectors sepEigen, 
 --         the eigenvalues eigNrep of A
 --         the algebraic multiplicity algMult of each eigenvalues, 
 --         the Jordan form of the matrix A.
 
 R := ring A; 
 -- variables and new coefficient ring
 varR :=  gens R;
 m := numRows A;
 R2 := CC[varR];

-- compute the eigenvalues of the matrix
 eigs := eigenvalues(A);
 -- print eigs;
 -- and the list of different eigenvalues
 eigNrep := new MutableList;
 count := 0;
 for jj from 0 to length eigs - 1 do (
  flag := 0;
  for kk from 0 to count - 1 do (
   if norm(eigs#jj - eigNrep#kk) < toll then (
    flag = 1;
    break;
   );
  );
  if flag == 0 then (
   tempEig := clean(toRR(toll),eigs#jj);
   tempEig = rationalize(realPart(tempEig),toll) 
             + ii*rationalize(imaginaryPart(tempEig),toll);
   eigNrep#count = tempEig;
   count = count + 1;
  );
 );
 eigNrep = new List from eigNrep;
 -- print eigNrep;
 
 -- find the algebraic multiplicity of each eigenvalue
 algMult := new MutableList;
 for jj from 0 to length eigNrep - 1 do (
  algMult#jj = substitute(0,QQ);
 );
 for jj from 0 to length eigs - 1 do (
  for mk from 0 to length eigNrep - 1 do (
   if norm(eigs#jj - eigNrep#mk) < toll then (
    vtemp := (algMult#mk);
    ntemp := vtemp + 1;
    algMult#mk = ntemp;
   );
  );
 );
 algMult = new List from algMult;
 -- print algMult;
 
 -- find the generalized eigenspace of the first matrix
 errkercomp := 0;
 MM0 := substitute(A,R2);
 GenEigenvectors := new MutableList;
 GenAdjEigenvectors := new MutableList;
 chBasMatr := new MutableMatrix from 0*id_(R2^m);
 sepEigen := new MutableList;
 jorFor := matrix{{}};
 -- if the matrix is diagonalizable
 if length eigNrep == numRows(A) then (
  chBasMatr = (eigenvectors(A))#1;
  for zk from 0 to numRows(A) - 1 do (
   rowList := new List from 0..(m-1);
   sepEigen#zk = submatrix(chBasMatr,rowList,{zk});
  );
  jorFor = clean(toRR toll,inverse(chBasMatr)*A*chBasMatr);
 ) else (
  for jj from 0 to length algMult - 1 do (
   -- print eigNrep#jj;
   dimKer := 0;
   perMatr := clean(toll,MM0 - substitute(eigNrep#jj,R2)*id_(R2^m));
   -- print rank(perMatr);
   -- print MM0;
   -- print (perMatr);
   -- print toString perMatr;
   genEig := new MutableList;
   adjEig := new MutableList;
   pdimKer := 0;
   count = 1;
   while dimKer != algMult#jj do (
    -- print count;
    -- print perMatr^count;
    genEig#(count-1) = apprKer((perMatr)^count,toll);
    genEig#(count-1) = substitute(genEig#(count-1),R2);
    -- print genEig#(count-1);
    -- print genEig#(count-1);
    if count - 1 > 0 then ( 
     perpEig := substitute(gens gb kernel(transpose genEig#(count-2)),R2);
     -- print perpEig;
     -- print genEig#(count-1);
     adjEig#(count-1) = gens gb intersect(image perpEig, image genEig#(count-1));
     -- print (((MM0 - substitute(eigNrep#jj,R2)*id_(R2^m))^(count-1))*(adjEig#(count-1)));
     -- print (((MM0 - substitute(eigNrep#jj,R2)*id_(R2^m))^(count))*(adjEig#(count-1)));
    ) else (
     adjEig#(count-1) = genEig#(count-1);
    );
    dimKer = numColumns genEig#(count-1);
    -- print (dimKer, algMult#jj);
    count = count + 1;
    -- print (pdimKer,dimKer,algMult#jj);
    if pdimKer >= dimKer then (
     errkercomp = 1;
     jj = length algMult;
     break;
    );
    pdimKer = dimKer;
   );
   -- print jj;
   genEig = new List from genEig;
   GenEigenvectors#jj = genEig;
   adjEig = new List from adjEig;
   GenAdjEigenvectors#jj = adjEig;
  );
  
  -- construct the change of basis
  if errkercomp == 0 then (
   count = 0;
   for jj from 0 to length algMult - 1 do (
    lenEigL :=  length GenAdjEigenvectors#jj;
    perMatr := MM0 - substitute(eigNrep#jj,R2)*id_(R2^m);
    partList := GenAdjEigenvectors#jj;
    tailEigVec := partList#(lenEigL-1);
    sepEigjj := new MutableMatrix from 0*id_(R2^m);
    rlist := new List from 0..(m-1);
    clist := new List from 0..(substitute(algMult#jj,ZZ) -1);
    sepEigjj = submatrix(new Matrix from sepEigjj,rlist,clist);
    sepEigjj = new MutableMatrix from sepEigjj;
    pcount := 0;
    fflag := 0;
    while fflag == 0 do (
     for kk from 0 to numColumns tailEigVec - 1 do (
      for ss from 0 to lenEigL - 1 do (
       -- print (jj,count,numColumns tailEigVec, lenEigL);
       rowList := new List from 0..(m-1);
       pma := (perMatr^(lenEigL - 1 - ss))*submatrix(tailEigVec,rowList,{kk});
       -- print npma;
       for ll from 0 to m - 1 do (
        chBasMatr_(ll,count) = pma_(ll,0);
        sepEigjj_(ll,pcount) = pma_(ll,0);
       );
       count = count + 1;
       pcount = pcount + 1;
      );
     );
     -- print (pcount,algMult#jj);
     if pcount != algMult#jj then (
      lenEigL = lenEigL - 1;
      -- print partList#(lenEigL-1);
      perpVec := substitute(gens kernel transpose gens gb image (perMatr*tailEigVec),R2);
      tailEigVec = gens gb intersect(image perpVec, image partList#(lenEigL-1));
      -- print tailEigVec;
     ) else (
      fflag = 1;
      break;
     );
    );
    sepEigen#jj = new Matrix from sepEigjj;
   );
   chBasMatr = new Matrix from chBasMatr;
   -- printt rank chBasMatr;
   jorFor = clean(toll,inverse(chBasMatr)*MM0*chBasMatr);
  );
 ); 
 
 sepEigen = new List from sepEigen;
 
 
 (chBasMatr, sepEigen, eigNrep, algMult, jorFor)
);

jordanForm = method(Options => {Tolerance => 10.^-3})
jordanForm(Matrix) := opts -> A -> (
 -- compute the Jordan canonical form of a given matrix
 
 -- inputs: the matrix A.
 
 -- output: the matrix chBasMatr that transforms A in Jordan form,
 --         the generalized eigenvectors sepEigen, 
 --         the eigenvalues eigNrep of A
 --         the algebraic multiplicity algMult of each eigenvalues, 
 --         the Jordan form of the matrix A.
 
 toll := opts.Tolerance;
 
 (chBasMatr, sepEigen, eigNrep, algMult, jorFor) := jFor(A,toll);
 
 if length sepEigen == 0 then (
  error "numerical errors occurred";
 );
 
 (chBasMatr, sepEigen, eigNrep, algMult, jorFor)
)

rationalize = (num,toll) -> (
 -- compute the nearest rational with fixed precision
 
 -- inputs: a real number num,
 --         the precision prec.
 
 -- output: a rational approximation of num
 
 prec := 1/toll;
 nnum := round(num*prec);
 out := nnum / prec;
 out
)

apprKer = (M,tol) -> (
 -- computes the nullSpace of the matrix M
 
 VVV := gens gb kernel M;
 
 M = substitute(M,CC);
 m := numRows M;
 (S,U,Vt) := SVD M;
 Vt = transpose Vt;
 
 indx := new MutableList;
 count := 0;
 for ss from 0 to m-1 do (
  if norm(S#ss) < tol then (
   indx#count = ss;
   count = count + 1;
  );
 );
 indx = new List from indx;
 
 VV := submatrix(Vt, new List from 0..(m-1), indx);
 
 VV = gens gb image clean(tol,VV);
 
 -- print clean(tol,substitute(VV,ring VVV)-VVV);
 
 if numColumns VVV == length indx then
  VV = VVV;
  
 VV
)

enumSolve = (R,I,M,toll) -> (
 -- compute by enumeration a solution to a system of equalities
 
  -- inputs: the ring R where the ideal is defined,
 --          the ideal I,
 --          the companion matrices M of the ideal I
 --          the tolerance toll.
 
 -- output: a list containing the solutions
 
 -- variables employed
 varR :=  gens R;
 n := length varR;
 m := numrows M_(0);
 toll = toRR toll;
 
 -- new coefficient ring
 R2 := CC[varR];
 
  -- extract a set of generators from the ideal
 f := transpose gens I;
 
 -- compute the jordan forms of the various matrices
 genEigenvals := new MutableList;
 for ll from 0 to n-1 do (
  (pCh,sepEigs,peigs,palgm,JF) := jFor(M_(ll), toll);
  genEigenvals#ll = peigs;
 );
 
 genEigenvals = new List from genEigenvals;
 -- print genEigenvals;
 
 -- initialize the vector of solutions
 indMatr := {};
 for jj from 0 to n-1 do (
  indMatr = append(indMatr,substitute(0,ZZ));
 );
 indMatr = new MutableList from indMatr;
 vecSol := new MutableList;
 count := 0;
 
 -- use the eigenvalues of the companion matrices to 
 -- determine the solutions by enumerations
 totDim := 0;
 while totDim < m do (
  -- consider one element of the array
  -- print new List from indMatr;
  parSol := new MutableList;
  for jj from 0 to n-1 do (
   parSol#jj = (genEigenvals#jj)#(indMatr#jj);
  );
  tval := substitute(f,R2);
  for ii from 0 to n-1 do (
   -- evaluate the polynomials at the given points 
   Svar := substitute(varR#(ii),R2);
   Sval :=  parSol#ii;
   tval = substitute(tval,{Svar => Sval});
  ); 
  if norm(tval) < toll then (
   vecSol#count = new List from parSol;
   count = count + 1;
   -- print parSol;
  );
  -- continue exploring the space of solutions
  indMatr#(n-1) = indMatr#(n-1) + 1;
  -- print new List from indMatr;
  for ll from 1 to n-1 do (
   if indMatr#(n-ll) == length(genEigenvals#(n-ll)) then (
    indMatr#(n-ll) = 0;
    indMatr#(n-ll-1) = indMatr#(n-ll-1) + 1;
   );
  );
  if (indMatr#0) == length(genEigenvals#0) then (
   break;
  );
 );
 
 vecSol = new List from vecSol;
 
 vecSol
)

-- End of source code --

beginDocumentation()

document { 
 Key => NumericSolutions,
 Headline => "solving systems of polynomial equalities",
 EM "NumericSolutions", " is a package for solving systems of polynomial equalities.
  It is used to compute the points in the affine variety V(I) of a zero 
  dimensional ideal I. Note that this package can be used only to compute the
  solution to a system of polynomial equalities having a finite number of 
  solutions (i.e., it can be used only if the ideal I is zero dimensional)."
} 
    
document { 
 Key => {compMatr,(compMatr,Ideal,RingElement)},
 Headline => "companion matrix",
 Usage => "compMatr(I,f)",
 Inputs => {
  "I" => { "a zero dimensional ideal."},
  "f" => { "a polynomial in R."} 
 },
 Outputs => {
  "M" => {"the companion matrix of the ideal I
            associated to the polynomial f."}
 },
 EXAMPLE lines ///
  R = QQ[x,y,z]
  f = x^2+3*y-2*z
  I = ideal(x+3*y^2-2*z, x^2-2*y-z, 3*x-4*y+5*z^2)
  M = compMatr(I,f)
 ///,
 SeeAlso => {solveSystem,traceForm}
}   
    
document { 
 Key => {solveSystem,(solveSystem,Ideal),[solveSystem, Tolerance]},
 Headline => "solutions to a system of equalities",
 Usage => "solveSystem(I)",
 Inputs => {
  "I" => { "a zero dimensional ideal."},
  Tolerance => {"numerical tolerance."}
 },
 Outputs => {
  "S" => {"containing the solutions 
           with their multiplicities" }
 },
 Caveat => {"the procedure involves computation over inexact fields. If 
             numerical errors occur, then the multiplicities of the solutions
             may be not reliable."},
 EXAMPLE lines ///
  R = QQ[x,y,z]
  I = ideal(x+3*y^2-2*z, x^2-2*y-z, 3*x-4*y+5*z^2)
  M = solveSystem(I)
 ///,
 SeeAlso => {compMatr}
}  

document { 
 Key => {traceForm,(traceForm,Ideal)},
 Headline => "trace matrix of an ideal",
 Usage => "traceForm(I)",
 Inputs => {
  "I" => { "a zero dimensional ideal."}
 },
 Outputs => {
  "S" => {"the trace form of the ideal I. The signature of such a form
           equals the number of real points in the variety V(I)." }
 },
 EXAMPLE lines ///
  R = QQ[x,y,z]
  I = ideal(x+3*y^2-2*z, x^2-2*y-z, 3*x-4*y+5*z^2)
  T = traceForm(I)
 ///,
 SeeAlso => {compMatr}
}   

document { 
 Key => {jordanForm,(jordanForm,Matrix),[jordanForm,Tolerance]},
 Headline => "Jordan canonical form",
 Usage => "jordanForm(A)",
 Inputs => {
  "A"   => { "a square matrix."},
  Tolerance => {"numerical tolerance."}
 },
 Caveat => {"the procedure involves computation over inexact field and hence the
             results may be not reliable."},
 Outputs => {
  "chBasMatr" => {"the matrix  that transforms A in Jordan form."},
  "sepEigen"  => {"the generalized eigenvectors."},
  "eigNrep"   => {"the eigenvalues of the matrix A."},
  "algMult"   => {"the algebraic multiplicity of each eigenvalue."},
  "jorFor"    => {"the Jordan form of A."}
 },
 EXAMPLE lines ///
  R = QQ[x,y,z]
  A = matrix{{1,2},{-1,3}}
  (pCh,sepEigs,peigs,palgm,JF) = jordanForm(A)
 ///,
 SeeAlso => {compMatr,solveSystem}
}   
    
TEST ///
R = QQ[x,y,z];
eq1 = x^2+y^2+z^2-2;
eq2 = x+y+z;
eq3 = z-1;
I = ideal(eq1,eq2,eq3);
St = solveSystem(I);
S = elements(St);
normSol = 0;
for ii from 0 to length S -1 do (
 vec = substitute(substitute(gens I,CC[x,y,z]),{x=>(S#ii)#0,y=>(S#ii)#1,z=>(S#ii)#2});
 normSol = normSol + sqrt((vec*transpose(vec))_(0,0));
);
assert(normSol < 10^(-10));
///

end

when using this package please refer to 
L. Menini, C. Possieri, A. Tornambe, "Dead–beat regulation of mechanical 
juggling systems", Asian Journal of Control, 2017
