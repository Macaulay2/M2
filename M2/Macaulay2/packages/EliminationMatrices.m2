-- -*- coding: utf-8-unix -*-
--######################################################################
-- PROGRAMMER : Nicolás Botbol, Laurent Busé, Manuel Dubinsky
-- UPDATE HISTORY : 3 March 2011 -> 16 June 2011 -> 7 July 2011 -> 6 September 2011 -> 17 October 2011 -> 24 October 2011 -> 2 November 2011 -> 12 January 2012 -> 13 February 2012.
---------------------------------------------------------------------------

newPackage("EliminationMatrices",
   Version => "1.4",
   Date => "13 February 2012",
   Authors => {
         {Name => "Nicolás Botbol", Email => "nbotbol@dm.uba.ar", HomePage => "http://mate.dm.uba.ar/~nbotbol/"},
	     {Name => "Laurent Busé", Email => "Laurent.Buse@inria.fr", HomePage => "http://www-sop.inria.fr/members/Laurent.Buse/"},
	     {Name => "Manuel Dubinsky", Email => "manudubinsky@gmail.com" }
	     },
   Headline => "resultants",
   Keywords => {"Commutative Algebra"},
   PackageImports => { "Elimination" },
   DebuggingMode => false
   )

export {
--	"matrixToListByColumns",
--	"matrixToListByRows",
--	"rankNum",
	"maxCol",
	"maxMinor",
-- MAIN FUNCTIONS
	"degHomPolMap",
	"listDetComplex",
	"detComplex",
	"mapsComplex",
	"minorsComplex",
--	"degMap",
--	"macRes",
	"macaulayFormula",
	"bezoutianMatrix",
--	"ciRes",
	"ciResDeg",
	"ciResDegGH",
--	"cm2Res",
--	"detRes",
	"detResDeg",
--	"rankNE",
	"Numeric",
	"Exact",
	"Sylvester",
	"Macaulay",
	"CM2Residual",
	"ciResidual",
	"determinantal",
	"byResolution",
	"eliminationMatrix",
	"regularityVar"
}


--######################################################################
--## Basic functions ###################################################
--######################################################################

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- PURPOSE : Methods that converts a matrices into lists and vice-versa (by rows and columns)

matrixToListByColumns = method();

--   INPUT : 'm' a matrix
--  OUTPUT : 'l' a list which has the elements of 'm' listed from the up-left corner to the down-right corner
matrixToListByColumns(Matrix) := (m) -> (
	l := {};
	for i from 0 to (rank source m) - 1 do (
		col := {};
		for j from 0 to (rank target m) - 1 do (
			col = col | {m_i_j};
		);
		l = l | {col};
	);
	return l;
)

matrixToListByRows = method();

matrixToListByRows(Matrix) := (m) -> (
	return matrixToListByColumns transpose m;
)


---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : returns the degree of a 'RingElement' in the variables 'var'

pdeg = method ();

pdeg(RingElement, List) := (f, var) -> (
	return max((degrees (coefficients(f,Variables=>var))_0)_1)_0;
);

--   INPUT : m is a line matrix |f1,..,fn| of polynomials
--  OUTPUT : var is a list of variables of the ring of f
pdeg(Matrix, List) := (m,var) -> (
     return apply((matrixToListByRows m)_0,i -> if i == 0 then 0 else pdeg(i, var))
);

---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : returns the coefficients of the RingElement/Matrix of RingElements in the monomials 'base'. It is a shortcut to the built-in function 'coefficients'

coeffs = method ();

coeffs(RingElement, List, List) := (f,var,base) -> (
	return coefficients(f, Variables=>var, Monomials=>base);
);

coeffs(Matrix, List, List) := (m,var,base) -> (
	return coefficients(m, Variables=>var, Monomials=>base);
);

coeffs(Matrix, List, Matrix) := (m,var,base) -> (
	return coefficients(m, Variables=>var, Monomials=>base);
);

---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : This function calculates the sourceShifts of the morphism R(d1)+...+R(dn)->R(e1)+...+R(ek) based on the target shifts

sourceShifts = method ();

sourceShifts(Matrix,List,List) := (m, targetShifts, var) -> (	
	nonZero := apply(matrixToListByColumns(m), i -> position(i, j -> j != 0));
	sourceShiftsList := {};
	for i from 0 to (length nonZero)-1 do (
		sourceShiftsList = sourceShiftsList | { pdeg(m_(nonZero_i,i),var) + targetShifts_(nonZero_i) };
	);

	return sourceShiftsList;
);

---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : it returns the complement of 2 lists

listComplement = method ();
listComplement(List, List) := (l1, l2) -> (
	return (sort toList(set l1 - set l2))
);

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- Functions that end with 'num' (rankNum, maxColNum, maxMinorNum, listDetComplexNum, minorsComplexNum and detComplexNum) are numerical functions, meaning that it does the same computation than the non-num function but after a random evaluation.
-- It is perhaps recommendable to run them at least twice.

---------------------------------------------------------------------------
---------------------------------------------------------------------------

getComputationStrategy := (options) -> (
     exactStr := 0;
     numericStr := 1;
     strat := if options.?Strategy then options.Strategy else null;
     if strat === global Exact then exactStr
     else if strat === global Numeric then numericStr 
     else if strat =!= null then (
	  error "'Strategy' keyword must be 'Numeric' or 'Exact'";
	  )
     else exactStr
);
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- PURPOSE : it returns the rank of a matrix obtained after specialization 
-- of the variables. With high probability, this is the rank of the input matrix.

rankNE = method(Options => { Strategy => null })
rankNE(Matrix) := ZZ => options -> (M) -> (
	if getComputationStrategy(options) == 0 then return(rank M)
	else return(rankNum M)
);

rankNum = method();
rankNum(Matrix) := (M) -> (
     Aring:=ring M;
     l:={};
     i:=0;
     
     while i<numgens(Aring) do (l=l|{random({0},Aring)}; i=i+1;);
     
     s:=map(Aring,Aring,l);
     N:=s(M); 
     rm:=rank(N);
     return(rm)
 );    


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- PURPOSE : Auxiliary method that takes from a given m x n - Matrix of rank r, a new full rank m x r - Matrix
-- It returns a list of which has in the first position a maximal matrix of linearly independent columns of M and in the
-- second the indices of those columns
--   INPUT : 'M' an m x n - Matrix of rank r
--  OUTPUT : 'N' an m x r - Matrix of rank r

maxCol = method(Options => { Strategy => null })
maxCol(Matrix) := (Matrix,List) => options -> (M) -> (

        rm:=rankNE(M,options);
        i:=0;
        c:={};
        while #c < rm do (
                if rankNE(M_(c|{i}),options) == #(c|{i}) then c=c|{i};
                i=i+1;
        );
        
        return {M_c,c}
);


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- PURPOSE : Method that takes from a given m x n - Matrix of rank r, a new r x r full rank Matrix"
--   INPUT : 'M' an m x n - Matrix of rank r
--  OUTPUT : 'N' an r x r - Matrix of rank r

maxMinor  = method(Options => { Strategy => null })
maxMinor(Matrix) := (Matrix,List) => options -> (M) -> (
        --Apply two times maxCol to obtain a maximal minor
        --This is not very efficient...  
        return (transpose (maxCol(transpose (maxCol(M,options))_0,options ))_0 )
);

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- PURPOSE : Method that takes a list of variables in a polynomial ring R and returns the corresponding list of integer for vars(R)
varpos = method();
--   INPUT : a list of variables
--  OUTPUT : a list of integers

varpos(List) := (l) -> (
     --l is a list (non empty) of variables of the same ring.
     --It returns the corresponding list of number of these var.
     i:=0;ln:={};lvar:={};mvars:=vars(ring l#0);
     for i from 0 to (rank source mvars)-1 do lvar=lvar|{mvars_(0,i)}; 
     for i from 0 to #l-1 do ln=ln|{position(lvar, j -> j == l#i)};
     return(ln)
);

--######################################################################
--######################################################################
--## Main functions ####################################################
--######################################################################
--######################################################################

---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : Let R be a polynomial ring in two groups of variables R=S[X1,...,Xr] and S=k[a1,...,as]. Here, X1,...,Xr ar called 'var' and a1,...,as are called 'coefficients'. Let m be a line matrix [f1,...,fn], where fi is an element of R which is homogeneous as a polynomial in the variables 'var', of degree di for all i in 'var'. The matrix 'm' defines a graded map of R-modules (of degree 0 in 'var') from R(-d1)+...+R(-dn) to R. In particular, looking on each strand d, we have a map of free S-modules of finite rank f_d: R_{d-d1}+...+R_{d-dn} -> R_d, where R_d is the homogeneous part of degree d in 'var' of R.
--This function returns a sequence with two elements: first the list of monomials of degree d in 'var'; Second, the matrix f_d with entries in S in the base of monomials.
--For computing the base of monomials, it needs as a second argument the list (d1,...,dn) of the degrees of the fi's in 'var'. There is an auxiliary function computing this automatically from the list of elements fi's and the list of variables 'var' called 'sourceShifts'.
-- INPUT: (m, sourceShifts, var, d): 'm' a row matrix of homogeneous polynomials in the variables 'var',  a list 'sourceShifts' of integers representing the degrees of 'm', a list 'var' with ring variables to be eliminated, a positive integer number 'd' which represents the strand of the map.
-- OUTPUT: a sequence with two elements: the list of monomials of degree d in 'var' and, the matrix f_d with entries in S in the base of monomials. (Same arity as 'coeffs')

degHomPolMap = method ();

degHomPolMap(Matrix,List,List,ZZ) := (m, sourceShifts, var,d) -> (
	i := position((matrixToListByRows m)_0, i -> i != 0);
	targetShift := sourceShifts_i - pdeg(m_(0,i), var);

	mat := matrix{{}};
	for i from 0 to (rank source m)-1 do (
		mat = mat | (m_i_0 * gens((ideal var)^(d-sourceShifts#i)));
	);

	return coeffs(mat, var, gens((ideal var)^(d-targetShift)));
);

-- default method with no source shifts...
degHomPolMap(Matrix,List,ZZ) := (m, var,d) -> (
	degHomPolMap(m, apply((matrixToListByRows m)_0, i -> ((degrees(monomials(i, Variables=>var)))_1)_0_0), var, d)
);

---------------------------------------------------------------
---------------------------------------------------------------
-- PURPOSE : this functions must be thought in the context of Elimination Theory as an intermediate step in processing the morphisms of a chain complex. Given a morphism of a chain complex of free modules f: R^n->R^m (R a polynomial ring), one may be interested in computing the strand of the complex of a certain degree 'd' in a subset ('var') of the variables of R. The new expression of the morphism has two parts: a set of monomials in the variables 'var' and a set of coefficients. This function returns the matrix of coefficients of the morphism in the variables 'var'. (see degHomPolMap)
-- INPUT: (m, targetShifts, var, d): 'm' a row matrix of polynomials representing an element of the free module R^n (the source of the morphism), 'targetShifts' is a list integers (d1,...,dm) corresponding to the shifts of the summands in the target in the case R^m is written as R(-d1)+...+R(-dm), 'var' is the list of variables of the polynomial ring R to take into account to 'eliminate or hide', for constructing the base, 'd' the degree of the strand of the complex or map.
-- OUTPUT: the matrix representing the morphism f.

degMap = method ();
degMap(Matrix,List,List,ZZ) := (m, targetShifts, var, d) -> (
	M := matrix{{}};
	rows := rank target m;
	sourceShiftsList := sourceShifts(m,targetShifts,var);
	for i from 0 to rows - 1 do (
		row := transpose(matrix((transpose m)_i));
		MM:=degHomPolMap(row,sourceShiftsList,var,d);
		if M == 0 then (
			M = MM_1;
		) else (
			M = transpose(transpose(M) | transpose(MM_1));
		);		
	);
	return M
);

---------------------------------------------------------------
---------------------------------------------------------------


--#######################################################################
--## Determinant of a complex ###########################################
--#######################################################################


-- PURPOSE : this function calculates the morphisms of a ChainComplex with respect to a subset of the variables of the polynomial ring.
-- INPUT: (complex, var, d): 'complex' is a ChainComplex, 'var' is the list of ring variables to take into account in the morphism, 'd' integer corresponding to the degree of the strand of the chain complex.
-- OUTPUT: a list of matrices representing the morphisms of the chain complex


mapsComplex = method();

mapsComplex(ZZ, List, ChainComplex) := (d, var, complex) -> (
	targetShifts := {0,0,0};
	mapList := {};
	columnList := {};
	complList := {};
	for i from 1 to length complex do (
		m := degMap(complex.dd_i, targetShifts, var, d);
		mapList = mapList | {m};
		targetShifts = sourceShifts(complex.dd_i,targetShifts,var);
	);
	return mapList
);

-- PURPOSE : this function calculates the sub-morphisms of a ChainComplex with respect to a subset of the variables of the polynomial ring.
-- INPUT: (complex, var, d): 'complex' is a ChainComplex, 'var' is the list of ring variables to take into account in the morphism, 'd' integer corresponding to the degree of the strand of the chain complex.
-- OUTPUT: a list of matrices representing a restriction and corestriction of the morphisms of the chain complex (the choice of the rows and columns is done according to the computation of a determinant of a complex).

minorsComplex = method(Options => { Strategy => null });
minorsComplex(ZZ, List, ChainComplex) := List => options -> (d, var, complex) -> (
	minList := {};
	columnList := {};
	complList := {};
	mapComp := mapsComplex (d, var, complex);
	for i from 0 to (length mapComp) -1 do (
		m := mapComp_i;
		complList = listComplement(toList(0..(rank target m) - 1),  columnList);
		m = (transpose(m))_complList;
		m = transpose(m);
		columnList = (maxCol (m,options))_1;
		minList = minList | {(m_columnList)};
	);
	return minList
);


-- PURPOSE : this function calculates the determinants of the submatrices of a ChainComplex with respect to a subset of the variables of the polynomial ring.
-- INPUT: (complex, var, d): 'complex' is a ChainComplex, 'var' is the list of ring variables to take into account in the morphism, 'd' integer corresponding to the degree of the strand of the chain complex.
-- OUTPUT: a list of polynomials corresponding to the determinants of the maps given by 'minorsComplex'.

listDetComplex = method (Options => { Strategy => null });
listDetComplex(ZZ, List, ChainComplex) := List => options -> (d, var, complex) -> (
	targetShifts := {0,0,0};
	detList := {};
	columnList := {};
	complList := {};
	for i from 1 to length complex do (
		m := degMap(complex.dd_i, targetShifts, var, d);
		complList = listComplement(toList(0..(rank target m) - 1),  columnList);
		m = (transpose(m))_complList;
		m = transpose(m);
		columnList = (maxCol (m,options))_1;
		detList = detList | {determinant(m_columnList)};
		targetShifts = sourceShifts(complex.dd_i,targetShifts,var);
	);
	return detList
);



-- PURPOSE : this function calculates the determinant of a ChainComplex with respect to a subset of the variables of the polynomial ring.
-- INPUT: (complex, var, d): 'complex' is a ChainComplex, 'var' is the list of ring variables to take into account in the morphism, 'd' integer corresponding to the degree of the strand of the chain complex.
-- OUTPUT: a polynomial corresponding to the determinant of the given complex. This is obtained by taking alternate product of the elements in the list 'listDetComplex'.

detComplex = method (Options => { Strategy => null });
detComplex(ZZ, List, ChainComplex) := List => options -> (d, var, complex) -> (
	listDetCmp := listDetComplex(d, var, complex, options);
	detC := 1_(ring var_0);
	for i from 0 to length(listDetCmp)-1 do (
		if even(i) then (detC = detC* (listDetCmp_i)) else (
		detC = detC / (listDetCmp_i);
		);
	);
	return detC;
);


---------------------------------------------------------------
---------------------------------------------------------------

--#######################################################################
--## Macaulay resultant #################################################
--#######################################################################

-- PURPOSE : this function calculates the Macaulay resultants 
-- INPUT: (p,var): 'p' is a one line matrix of homogeneous polynomial in variables 'var'
-- OUTPUT: It returns a list. The first argument is the matrix of the first Koszul map in degree the well-known critical degree. The second and third are basis of source and target (see degHomPolMap)

macRes = method ();

macRes(List,Matrix) := (var,p) -> (  
  return((degHomPolMap(p,var,sum(pdeg(p,var))-#var+1))_1)
  --remove the _1 at the end of the line above if the monomial basis indexing the rows are wanted	
);

---------------------------------------------------------------
---------------------------------------------------------------

--#######################################################################
--## Macaulay Formula ###################################################
--#######################################################################

-- PURPOSE : this function calculates the Macaulay formula 
-- INPUT: (p,var): 'p' is a one line matrix of homogeneous polynomial in variables 'var'
-- OUTPUT: It returns a list of two matrices such that the ratio of these two matrices is equal, if it is defined, to the Macaulay resultant of p w.r.t. the variables var.

macaulayFormula = method ();

macaulayFormula(List,Matrix) := (var,p) -> (  
     ld:=pdeg(p,var); 
     nu:=sum(ld)-#var+1;
     listmon:=gens((ideal var)^nu);
     sizelistmon:=numgens source listmon;
     f:=matrix{{}};
     dodu:={};
     for i to (sizelistmon-1) do (
     	  m:=listmon_(0,i);
     	  indexm:={};
     	  for j to #var-1 do (
	       if pdeg(m,{var_j})>=ld_j then indexm=append(indexm,j);
	       if #indexm>=2 then (dodu=append(dodu,i);  break;);
	  );
     ind:=indexm_0;
     f=f | matrix{{sub( m/(var_ind)^(ld_ind)*p_(0,ind),ring(p))}};
     );
     (M,C):=coefficients(f,Variables=>var,Monomials=>listmon);
     D:=submatrix(C,dodu,dodu);
     return((C,D));       
);

---------------------------------------------------------------
---------------------------------------------------------------

--#######################################################################
--## Bezoutian Matrix ###################################################
--#######################################################################

-- PURPOSE : this function calculates the bezoutian matrix associated to a system of n+1 (affine) polynomials in n variables  
-- INPUT: (p,var): 'p' is a one line matrix of (affine) polynomials in the n variables 'var'
-- OUTPUT: It returns the bezoutian matrix.

bezoutianMatrix = method ();

bezoutianMatrix(List, Matrix) := (var, p) -> (  
     --p is a matrix of m (affine) polynomials in R
     --It returns the Bezoutian matrix w.r.t. var
     R:=ring p;
     n:=#var;
     np:=varpos(var);--position of the n var in the list vars(R)
     m:=rank source p; --need m-1 == n
     tt:=getSymbol "tt"; 
     zz:=getSymbol "zz";
     Rb:=R(monoid[tt_1 ..  tt_n,  zz_1 ..  zz_n]); --ring for Bezoutian
     i:=0;  k:=0;
     --first column:
     lv:=gens R; nlv:=#lv; --list of generators of R
     for i from 0 to n-1 do lv=lv_{0..np#(i)-1}|{Rb_i}|lv_{np#(i)+1..nlv-1};
     rb0:=map(Rb,R,lv);--Send var on tt_
     rbp:=rb0 p;--p in Rb
     --rb1:=map(Rb,R); --initialization of rb1
     Theta:=transpose rbp; l:={};
     --second columns:
     pp1:=rbp; --introduced for late use
     pp2:=substitute(pp1,{Rb_0 => Rb_n}); -- tt_1=>zz_1
     Theta=Theta | transpose matrix { for k from 0 to m-1 list
          (pp1_(0,k) - pp2_(0,k))//(Rb_0 - Rb_n) };
     --the other columns:
     for i from 2 to n do ( 
          pp1=pp2;
	  pp2=substitute(pp2,{Rb_(i-1) => Rb_(n+i-1)}); -- tt_i => zz_i
          Theta=Theta | transpose matrix { for k from 0 to m-1 list
                (pp1_(0,k) - pp2_(0,k)) // (Rb_(i-1) - Rb_(n+i-1)) };
     );
     --The Bezoutian of p is the determinant of Theta:
     bez:=det Theta; 
     --Construction of the Bezoutian matrix:
     lvar:=gens Rb;
     ttvar:=lvar_{0..(n-1)};
     zzvar:=lvar_{n..(2*n-1)};
     zzmon:=(coefficients(bez,Variables=>zzvar))#0; --target basis
     mct:=(coefficients(bez,Variables=>ttvar))#1; -- idem#0 = source basis
     mbez:=(coefficients(mct_(0,0),Variables=>zzvar,Monomials=>zzmon))#1;
     for i from 1 to rank target mct - 1 do
     	  mbez=mbez|(coefficients(mct_(i,0),Variables=>zzvar,Monomials=>zzmon))#1;
     return(substitute(mbez,R))
);

---------------------------------------------------------------
---------------------------------------------------------------

--#######################################################################
--## Residual resultant of a complete intersection ######################
--#######################################################################

-- PURPOSE : this function calculates the residual resultant over a complete intersection 
-- INPUT: (g,H,var): 'g' is a one line matrix of homogeneous polynomial in variables 'var' 
-- forming a complete intersection. H is a matrix such that F=gH and F is the system of 
-- polynomials for which the residual resultant is computed
-- OUTPUT: It returns a matrix.

ciRes= (var, g, H) -> (
  --g is a one line matrix in R
  --H is a matrix in R
  --it returns the residual resultants of g*H over g w.r.t to var
  ng:=rank source g; m:=rank source H; --we must have rank target M == ng
  dg:=pdeg(g,var);
  --dg:={}; i:=0; for i from 0 to ng-1 do (dg=dg|{(degree(g_(0,i)))_0});
  f:=g*H; --polynomials f1,..,fm
  df:=pdeg(f,var);
  --df:={}; for i from 0 to m-1 do (df=df|{pdeg(var,f_(0,i))_0});
  bm:=subsets(0..m-1,ng); nbm:=#bm; --base of minors
  for i from 0 to nbm-1 do (f=f|det(H_(bm_i)));--add minors to f
  return((degHomPolMap(f,var,(sum df) - m + 1 - (m - ng + 1)*(min dg)))_1)
);

-- PURPOSE: similar than ciRes but return the critical degree in which the resultant is computed

ciResDegGH= (var, g, H) -> (
  --g is a one line matrix in R
  --H is a matrix in R
  --it returns the critical degree of the residual resultant of g*H over g w.r.t to var
  ng:=rank source g; m:=rank source H; --we must have rank target M == ng
  dg:=pdeg(g,var);
  f:=g*H; --polynomials f1,..,fm
  df:=pdeg(f,var);
  return((sum df) - m + 1 - (m - ng + 1)*(min dg))
);

-- PURPOSE: compute the partial degrees of the residual resultant over a complete intersection
-- INPUT: two lists of integers: the first one is the list of degrees of the polynomials for 
-- which one wants to compute the residual resultant, the second one is the list of degrees 
-- of the complete intersection associated to this residual resultant.
-- OUTPUT: the list of partial degrees

ciResDeg = (ld,lk) -> (
  --ld is a list of integers {d_0..d_m}
  --lk is a list of integers {k_1..k_n}
  --It returns the regularity and the degrees of the CiRes associated to these degrees
  i:=0; nd:=#ld; nk:=#lk;
  s:=reverse subsets(0..nd-1,nd-1);
  {(sum ld) - nd + 1 - (nd - nk + 1)*(min lk),
    for i from 0 to nd-1 list cideg(ld_(s_i),lk)
    }
);

cideg = (ld,lk) -> (
  --ld and lk are lists of integer.
  --Calling with ld:=[d1,...,dm] and lk:=[k1,...,kn], it returns the degree
  --of CiRes in the coefficient of the polynomial f_0.
  m:=#ld; n:=#lk;i:=0;j:=0;u:={};v:={};w:={};
  if m >= 2*n+1 then (
       H:=for i from 0 to n list (
          u=for j from 0 to i list sympol(lk,i-j);
          v=for j from i+1 to m-n list 0;
         (u|v));
       H=H|for i from n+1 to m-n list (
          u=for j from 0 to i-n-1 list 0;
          v=u|for j from i-n to i list sympol(lk,i-j);
          w=v|for j from i+1 to m-n list 0))
  else (H=for i from 0 to m-n list (
          u=for j from 0 to i list sympol(lk,i-j);
          v=u|for j from i+1 to m-n list 0)
       );
  c:=for i from 1 to m-n list i;
  H=(matrix(H))^c;
  N:=sympol(ld,m);s:=subsets(0..m-n,m-n);
  if m==n then N=sympol(ld,m)-sympol(lk,n)
         else for i from 0 to m-n do
  N=N+sympol(ld,i)*(-1)^(m-n-i+1)*sympol(lk,n)*det(H_(s_i))*(-1)^(n*(m-n+ 1));
  return(N);
);

sympol = (l,n) -> (
  --l is a list of integer with #l>=n
  --n is an integer
  --it returns the symmetric polynomial (-1)^(n-l)sum l_i1..l_in
  (-1)^(#l-n)*sum(apply(subsets(l,n),product))
);  

--########################################################################
--## Residual resultant of a Cohen-Macaulay codim 2 ideal ################
--########################################################################

-- PURPOSE : this function calculates the residual resultant over a Cohen-Macaulay codim 2 base locus 
-- INPUT: (g,H,var): 'g' is a one line matrix of homogeneous polynomial in variables 'var' 
-- forming a CM codim 2 ideal. H is a matrix such that F=gH and F is the system of 
-- polynomials for which the residual resultant is computed
-- OUTPUT: It returns a matrix.

cm2Res= (var, g, H) -> (
  --g is a one line matrix in R
  --H is a matrix in R such that f=gH, 
  --It returns the residual resultants of f over g
  n:=rank source g; --rank source H=m, and we must have rank target M==n
  dg:=pdeg(g,var);
  f:=g*H; --the polynomials f1,..,fm
  df:=pdeg(f,var);
  S:= syz  g; --first syzygy of G with Hilbert-Burch theorem
  return((degHomPolMap(gens minors(n,S|H),var,(sum df) - 2*(min dg + 1) ))_1)
);
 
--######################################################################
--## Determinantal resultant ###########################################
--######################################################################

-- PURPOSE : this function calculates the determinantal resultant a matrix
-- INPUT: (p,r,var): 'p' is a matrix of homogeneous polynomial in variables 'var'
-- and r is an integer 
-- OUTPUT: the determinantal resultant of p associated to the condition that 
-- the rank of p is lower or equal to r.

detRes=(r,var,p) -> (
   --p is a matrix of homogeneous polynomials in var
   --r is an integer
   m:=rank source p; n:=rank target p;
   dp:=pdeg(p^{n-1},var);--degrees of last row
   dc:=pdeg(transpose(p_{0}),var); --degree of first column
   dk:=sort(apply(dc, i->dp#0-i));
   return((degHomPolMap(gens(minors(r+1,p)),var,
   (n-r)*(sum dp - sum dk)-(m-n)*(sum dk_{0..n-r-1})-(m-r)*(n-r)+1)))_1
);

-- PURPOSE : this function calculates the critical degree and that partial degrees of 
-- the determinantal resultant
-- INPUT: 
-- dp and dk are list of integers corresponding to DetRes with r.
-- dp=d1,..,dm and dk=k1,..,kn
-- r is an integer
-- R is the ring where computations take place
-- OUTPUT: a list of integers containing the critical degree and then the partial degrees


detResDeg=(r,dp,dk,R) -> (

   m:=#dp;n:=#dk;
   h:=getSymbol "h";
   t:=getSymbol "t";
   nR:=R(monoid[h,t]); 
   sdp:=0_nR;sdk:=0_nR;
   for i from 1 to m do sdp=sdp+dp#(i-1)_nR;
   for i from 1 to n do sdk=sdk+dk#(i-1)_nR;
   subsdk:=0_nR;dk=sort(dk);
   for i from 1 to n-r do subsdk=subsdk+dk#(i-1)_nR;
   reg:=(n-r)*(sdp - sdk)-(m-n)*subsdk-(m-r)*(n-r)+1;
   vmax:=m+n-2*r-1;
   vmin:=m-n+1;
   F:={};i:=0;j:=0;
   for i from 1 to n do 
       F=F|{sum for j from 0 to vmax list ((nR_1)*(dk#(i-1)_nR))^j};
   F=product(F);
   degs:={};E:=0;S:=0;coefS:=0;nc:=0;mTP:=0;
   for j from 0 to m-1 do (
   	 E=(1-((dp#j)_nR+(nR_0))*(nR_1))*product(0..j-1,i->1-((dp#i)_nR)*(nR_1))*product(j+1..m-1,i->1-((dp#i)_nR)*(nR_1)); 
    	S=coefficients(matrix{{E*F}},Variables=>{nR_1});--coeff en t
		coefS=transpose(S#1);nc=rank source coefS;--decreasing --All monomials in t appears 1,t,t^2,....
		mTP=coefS_{(nc-1-m+r)..(nc-1-m+n-1)};
		for i from 1 to n-r-1 do (
	    	mTP=mTP||coefS_{(nc-1-m+r-i)..(nc-1-m+n-1-i)};
		);
		degs=degs|{substitute(((coefficients(det(mTP),Variables=>{nR_0},Monomials=>{matrix{{nR_0}}}))_1)_(0,0),R)};
   );
   return({substitute(reg,R),degs})
);

---------------------------------------------------------------
---------------------------------------------------------------
regularityVar = method ();

-- regularityVar computes the CM-reg of an ideal 'I' with respect to the variables 'var' 
regularityVar(List, Ideal) := (var, I) -> (
  --var is a list of variables 
  -- I is an homogeneous ideal wrt var.
 
 -- we assign weight 1 to the vars in 'var' and 0 otherwise
 	R:= ring I;
 	varWeights := {};
	ringVars := (entries vars R)_0;
	scan (ringVars , i -> if isSubset ({i},var) then { varWeights = varWeights | {1}; }  else {varWeights = varWeights | {0};});
	S:= (coefficientRing R)[(entries vars R)_0, Degrees => varWeights];

 -- we map I to S and we compute the resolution

	ff:= map (S, R);
	IS:= ff(I);
	resIS:= resolution IS;
	
 -- we compute the list of b_i's and the regularity 
	b:= {};
	for i from 0 to length resIS-1 do (
	b = b | {(max max degrees mingens resIS_i)_0-i}; -- one could use (max flatten degrees resIS_i) ?
	);
 use R; -- return to the ambient ring, otherwise some problems may appear.

return ((max b)+1)

);


getFamilyStrategy := (options) -> (

	 isNull := 77;
     resoluStr := 0;
     sylResStr := 1;
     macResStr := 2;
     cm2ResStr := 3;
     ciResStr := 4;
     detResStr := 5;
     
     strat := if options.?Strategy then options.Strategy else null;
     if strat === global Sylvester then sylResStr 
     else if strat === global Macaulay then macResStr 
     else if strat === global CM2Residual then cm2ResStr 
     else if strat === global ciResidual then ciResStr
     else if strat === global determinantal then detResStr
     else if strat === global byResolution then resoluStr
     else if strat =!= null then (
	  error "'Strategy' keyword must be 'Sylvester', 'Macaulay', 'CM2Residual', 'ciResidual',  or 'byResolution'";
	  )
     else isNull
);

---------------------------------------------------------------
---------------------------------------------------------------



eliminationMatrix = method (Options => { Strategy => null });

eliminationMatrix(List, Matrix) := Matrix => options -> (var, g) -> (

	if (rank source g == length var +1  and rank source g == 2) then return sylvesterMatrix (g_0_0,g_1_0,var_0) 
	else if rank source g  == length var then return macRes (var,g)
	else print "The arguments do not satisfy the required hypothesis.";
);

eliminationMatrix((ZZ, List, Matrix)) := Matrix => options -> (r, var, g) -> (
	if r >= rank target g  or r<0 then error "the rank 'r' must be a non-negative integer smaller strictly than the size of the target of the matrix."
	else if getFamilyStrategy(options) == 2 then return macRes (var,g)
	else if getFamilyStrategy(options) == 5 then return detRes (r, var, g)
	else if getFamilyStrategy(options) == 0 then (
		I := minors (r+1,g);
		return (mapsComplex(regularityVar (var,I), var, res I ))_0;
		)
	else if (getFamilyStrategy(options) == 1 or getFamilyStrategy(options) == 3 or getFamilyStrategy(options) == 4 ) then error "with this input, 'Strategy' keyword must be 'Macaulay', 'determinantal' or 'byResolution'"
	-- at this point Strategy is null
	else if rank target g ==1 and r == 0 then return eliminationMatrix(var, g)
	else if  (((rank source g) - r)*((rank target g)-r)+1 == length(var))  then return detRes (r, var, g)
	else (print "The arguments do not satisfy the hypothesis for the strategies 'Macaulay' and 'determinantal'. You might want to use the strategy 'byResolution'.");
);

eliminationMatrix(List, Matrix, Matrix) := Matrix => options -> (var, g, H) -> (

	if getFamilyStrategy(options) == 3 then return cm2Res (var, g, H)
	else if getFamilyStrategy(options) == 4 then return ciRes (var, g, H)
	else if getFamilyStrategy(options) == 0 then (
		I := (ideal (g*H): ideal g);
		return (mapsComplex(regularityVar (var,I), var, res I))_0;
		)
	else if (getFamilyStrategy(options) == 1 or getFamilyStrategy(options) == 3 or getFamilyStrategy(options) == 4 ) then error "with this input, 'Strategy' keyword must be 'CM2Residual', 'ciResidual' or 'byResolution'"
	-- at this point Strategy is null
--	else if (((rank target g) == 1) and ((rank source g) <= (length(var)))) then return ciRes (var, g, H) 
--	else if ((rank target g) == (rank target H)) and (((rank source g)+1) == (rank target g)) then return cm2Res (var, g, H)
	else (
		print "A strategy is required; please choose 'CM2Residual', 'ciResidual' or 'byResolution' depending on your inputs.";
--		J := (ideal (g*H): ideal g);
--		return (mapsComplex(regularityVar (var,J), var, res J))_0;
		);
);

---------------------------------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------------- DOCUMENTATION ---------------------------
---------------------------------------------------------------
beginDocumentation()
---------------------------------------------------------------
---------------------------------------------------------------
-- Simple Doc information
---------------------------------------------------------------
---------------------------------------------------------------
document {
	Key => {EliminationMatrices},
	Headline => "resultants",
	
	TT "EliminationMatrices", " is a package for elimination theory, emphasizing universal formulas, in particular, resultant computations.",
	
	PARA {}, "The package contains an implementation for computing determinant of free graded complexes, called ", TO2((detComplex), "detComplex"), ", with several derived methods:  ", TO2((listDetComplex), "listDetComplex"),", ", TO2((mapsComplex), "mapsComplex"),", and  ", TO2((minorsComplex), "minorsComplex"), ". This provides a method for producing universal formulas for any family of schemes, just by combining the ", TO2 {(resolution,Ideal),"resolution(Ideal)"}, " method with  ", TO2((detComplex), "detComplex"), ". In Section 2 determinants of free resolutions are treated, as well as a few examples. We recommend to see [Dem84, Jou95, GKZ94, Bus06] for more details on determinants of complexes in elimination theory.",

	PARA {}, "The package also provides a method ", TO2((eliminationMatrix), "eliminationMatrix")," for computing matrices and formulas for different resultants applicable on different families of polynomials, such as the Macaulay resultant (Macaulay) for generic homogeneous polynomials; the residual resultant ('ciResidual' and 'CM2Residual') for generic polynomials having a non empty base locus scheme; the determinantal resultant ('determinantal') for generic polynomial matrices of a given generic rank. For the theory behind those resultants, the reader can refer to [Mac02, Jou91, Cha93, GKZ94, Jou97, CLO98, BEM00, BEM01, Bus01b, Bus06, Bus04].",

	PARA {}, "The goal of this package is to provide universal formulas for elimination. The main advantage of this approach consists in the fact that one can provide formulas for some families of polynomials just by taking determinant to a free resolution. A direct consequence of a universal formula is that it is preserved by base change, in particular it commutes with specialization. A deep study of universal formulas for the image of a map of schemes can be seen in [EH00].",

	PARA {}, BOLD "Bibliography:", 

	PARA {}, "[BBD12] ", ITALIC "Nicolás Botbol, Laurent Busé and Manuel Dubinsky. ", HREF("http://mate.dm.uba.ar/~nbotbol/pdfs/ResultantsM2.pdf", " PDF"), ". Package for elimination theory in Macaulay2 (2012).",

    PARA {}, "[BEM00] ", ITALIC "Laurent Busé, Mohamed Elkadi and Bernard Mourrain",", Generalized resultants over unirational algebraic varieties, J. Symbolic Comput. 29 (2000), no. 4-5, 515–526.", 

    PARA {}, "[BEM01] ", ITALIC "Laurent Busé, Mohamed Elkadi and Bernard Mourrain",", Resultant over the residual of a complete intersection, Journal of Pure and Applied Algebra 164 (2001), no. 1-2, 35–57.",

    PARA {}, "[Bus01a] ", ITALIC "Laurent Busé",", Residual resultant over the projective plane and the implicitization problem, International Symposium on Symbolic and Algebraic Computing (ISSAC), ACM, (2001). Please, see the errata.pdf attached file., pp. 48–55.", 

--    PARA {}, "[Bus01b] ", ITALIC "Laurent Busé",", Étude du résultant sur une variété algébrique. phd thesis. (2001) ", 

    PARA {}, "[Bus04] ", ITALIC "Laurent Busé",", Resultants of determinantal varieties, J.Pure Appl. Algebra193 (2004), no.1-3, 71–97.", 

    PARA {}, "[Bus06] ", ITALIC "Laurent Busé",", Elimination theory in codimension one and applications, (2006).", 

    PARA {}, "[Cha93] ", ITALIC "Marc Chardin",", The resultant via a Koszul complex, Computational algebraic geometry (1992), Progr. Math, vol. 109, Birkhäuser Boston, Boston, MA, pp. 29–39. ", 

    PARA {}, "[CLO98] ", ITALIC "David Cox, John Little and Donal O’Shea",", Using algebraic geometry, Graduate Texts in Mathematics, vol. 185, Springer-Verlag, New York, (1998).", 

    PARA {}, "[Dem94] ", ITALIC "Michel Demazure",", Une définition constructive du resultant, Centre de Mathématiques de l’Ecole Polytechnique 2 (1984), no. Notes informelles du calcul formel 1984-1994, 0–23. ", 

    PARA {}, "[EH00] ", ITALIC "David Eisenbud and Joe Harris",", The geometry of schemes., Graduate Texts in Mathematics. 197. New York, NY: Springer. x, 294 p., (2000). ", 

--    PARA {}, "[Eis95] ", ITALIC "David Eisenbud",", Commutative algebra, With a view toward algebraic geometry. Graduate Texts in Mathematics, vol. 150, Springer-Verlag, New York, (1995).",
    
    PARA {}, "[GKZ94] ", ITALIC "Israel M. Gel′fand, Mikhail M. Kapranov and Andrei V. Zelevinsky",", Discriminants, resultants, and multidimensional determinants, (1994). Mathematics: Theory & Applications, Birkh ̈auser Boston Inc, Boston, MA. ", 

    PARA {}, "[Jou91] ", ITALIC "Jean-Pierre Jouanolou",", Le formalisme du résultant, Adv. Math 90 (1991), no. 2, 117–263.", 

--    PARA {}, "[Jou95] ", ITALIC "Jean-Pierre Jouanolou",", Aspects invariants de l’élimination, vol. 114, 1995, pp. 1–174. ", 

    PARA {}, "[Jou97] ", ITALIC "Jean-Pierre Jouanolou",", Formes d’inertie et résultant: un formulaire, Adv. Math. 126 (1997), no. 2, 119–250.", 

    PARA {}, "[Mac02] ", ITALIC "Francis S. Macaulay",", Some formulae in elimination, Proc. London Math. Soc. 33 (1902), no. 1, 3–27.", 
    
--   PARA {}, "[Mou02] ", ITALIC "Bernard Mourrain",", Enumeration problems in Geometry, Robotics and Vision, Prog. in Math. 143 (1996), 285–306.",
}

------------------------ \ degHomPolMap / ------------------------
document {
     	Key => {degHomPolMap, (degHomPolMap, Matrix, List, List, ZZ), (degHomPolMap, Matrix, List, ZZ)},
	Headline => "return the base of monomials in a subset of variables, and the matrix of coefficients of a morphism of free modules f:R(d1)+...+R(dn)->R_d with respect to these variables",
	Usage => " coefAndMonomials = degHomPolMap(r, l, v, d)",

	Inputs => {
		"r" => Matrix => {"single row matrix with polynomials ", TEX "$f_1,...,f_n$"},
		"l" => List => {"list {d1,...,dn} of degrees corresponding to the degrees of ", TEX "$f_1,...,f_n$"},
		"v" => List => {"list of variables of the polynomial ring R with respect to which the polynomials fi's are homogeneous of degree 'di' (to take into account for elimination)"},
		"d" => ZZ => {"the degree in 'var' of the homogeneous strand of the map f (i.e.: R_d)"}
	},
	Outputs => {
		"List" => List => {"a list {monomials, coefficients} of the coefficients and monomials of the morphism f "}
	},

	PARA {}, "Let R be a polynomial ring in two groups of variables ", TEX "$R=S[X_1,...,X_r]$", " and ", TEX "$S=k[a_1,...,a_s]$", ". Here, ", TEX "$X_1,...,X_r$", " are called ", TT "v" ," and  ", TEX "$a_1,...,a_s$", " are called 'coefficients'. Let m be a line matrix ", TEX "$f_1,...,f_n$", ", where fi is an element of R which is homogeneous as a polynomial in the variables 'var', of degree ", TEX "$di$", " for all i in 'var'. The matrix 'm' defines a graded map of R-modules (of degree 0 in 'var') from ", TEX "$R(-d_1)+...+R(-d_n)$", " to R. In particular, looking on each strand d, we have a map of free S-modules of finite rank ", TEX "$f_d: R_{d-d_1}+...+R_{d-d_n} -> R_d$", " where ", TEX "$R_d$", " is the homogeneous part of degree d in 'var' of R.",

	PARA {}, "This function returns a sequence with two elements: first the list of monomials of degree d in 'var'; Second, the matrix f_d with entries in S in the base of monomials.",

	PARA {}, "For computing the base of monomials, it needs as a second argument the list ", TEX "$d_1,...,d_n$", "  of the degrees of the fi's in ", TT "v", ". There is an auxiliary function computing this automatically from the list of elements fi's and the list of variables ", TT "v", " called ", TT "l", ".",

	EXAMPLE {" R=QQ[a,b,c,x,y] ",
		" f1 = a*x^2+b*x*y+c*y^2 ",
		" f2 = 2*a*x+b*y ",
		" M = matrix{{f1,f2}} ",
		" l = {x,y} ",
		" dHPM = degHomPolMap (M,l,2)",
		" dHPM = degHomPolMap (M,{2,1},l,2)"
		},
		
	EXAMPLE {" R=QQ[a,b,c,d,e,f,g,h,i,x,y,z] ",
		" f1 = a*x+b*y+c*z ",
		" f2 = d*x+e*y+f*z ",
		" f3 = g*x+h*y+i*z ",
		" M = matrix{{f1,f2,f3}} ",
		" l = {x,y,z} ",
		" dHPM = degHomPolMap (M,l,1)",
		" dHPM = degHomPolMap (M,{1,1,1},l,1)"
		},
     
	SeeAlso => {detComplex, listDetComplex, minorsComplex, mapsComplex, coefficients}
     
}

------------------------ \ mapsComplex / ------------------------

document {
     	Key => {mapsComplex, (mapsComplex, ZZ, List, ChainComplex)},
	Headline => "This function calculates the maps of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree.",
	Usage => " mapsOfTheComplex = mapsComplex(d, v, C)",

	Inputs => {
		"d" => ZZ => {"integer corresponding to the degree of the strand of the chain complex."},
		"v" => List => {"list of variables of the polynomial ring R to take into account for elimination"},
		"C" => ChainComplex => {"a chain complex of free modules over a polynomial ring"}
	},
	Outputs => {
		List => {"a list of matrices"}
	},

	PARA {}, "This function calculates the maps of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree.",
	PARA {}, "The input ChainComplex needs to be an exact complex of free modules over a polynomial ring. The polynomial ring must contain the list ", TT "v", " as variables.",
	PARA {}, "It is recommended not to define rings as R=QQ[x,y][a,b,c] when the variables to eliminate are '{x,y}'. In this case, see ", TO "flattenRing", " for passing from ", TEX "$R=QQ[x,y][a,b,c]$", " to ", TEX "QQ[x,y,a,b,c].",

	EXAMPLE {" R=QQ[a,b,c,x,y] ",
		" f1 = a*x^2+b*x*y+c*y^2 ",
		" f2 = 2*a*x+b*y ",
		" M = matrix{{f1,f2}} ",
		" l = {x,y} ",
		" dHPM = mapsComplex (2,l,koszul M)",
		" dHPM = mapsComplex (3,l,koszul M)"
		},
		
	EXAMPLE {" R=QQ[a,b,c,d,e,f,g,h,i,x,y,z] ",
		" f1 = a*x+b*y+c*z ",
		" f2 = d*x+e*y+f*z ",
		" f3 = g*x+h*y+i*z ",
		" M = matrix{{f1,f2,f3}} ",
		" l = {x,y,z} ",
		" dHPM = mapsComplex (1,l,koszul M)",
		" dHPM = mapsComplex (2,l,koszul M)"
		},
     
	SeeAlso => {degHomPolMap, listDetComplex, minorsComplex, mapsComplex }
}


------------------------ \ minorsComplex / ------------------------

document {
     	Key => {minorsComplex, (minorsComplex, ZZ, List, ChainComplex)},
	Headline => "calculate some minors of the maps of a graded ChainComplex in a subset of variables and fixed degree",
	Usage => " minorsOfTheComplex = minorsComplex(d,v,C)",

	Inputs => {
		"d" => ZZ => {"integer corresponding to the degree of the strand of the chain complex."},
		"v" => List => {"list of variables of the polynomial ring R to take into account for elimination"},
		"C" => ChainComplex => {"a chain complex of free modules over a polynomial ring"}
	},
	Outputs => {
		List => {"a list of square (full-rank) matrices"}
	},

	PARA {}, "This function calculates some minors of the maps of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree. The choice of the minors is according to the construction of the determinant of a complex",
	PARA {}, "The input ChainComplex needs to be an exact complex of free modules over a polynomial ring. The polynomial ring must contain the list ", TT "v", " as variables.",
	PARA {}, "It is recommended not to defines rings as R=QQ[x,y][a,b,c] when the variables to eliminate are '{x,y}'. In this case, see ", TO "flattenRing", " for passing from ", TEX "$R=QQ[x,y][a,b,c]$", " to ", TEX "QQ[x,y,a,b,c].",
	
	EXAMPLE {" R=QQ[a,b,c,x,y] ",
		" f1 = a*x^2+b*x*y+c*y^2 ",
		" f2 = 2*a*x+b*y ",
		" M = matrix{{f1,f2}} ",
		" l = {x,y} ",
		" dHPM = minorsComplex (2,l,koszul M)",
		" dHPM = minorsComplex (3,l,koszul M)"
		},
		
	EXAMPLE {" R=QQ[a,b,c,d,e,f,g,h,i,x,y,z] ",
		" f1 = a*x+b*y+c*z ",
		" f2 = d*x+e*y+f*z ",
		" f3 = g*x+h*y+i*z ",
		" M = matrix{{f1,f2,f3}} ",
		" l = {x,y,z} ",
		" dHPM = minorsComplex (1,l,koszul M, Strategy => Exact)",
		" setRandomSeed 0",			    -- set seed to one known to work
		" dHPM = minorsComplex (2,l,koszul M, Strategy => Numeric)"
		},
     
	SeeAlso => {degHomPolMap, listDetComplex, detComplex, mapsComplex }
}

------------------------ \ listDetComplex / ------------------------

document {
     	Key => {listDetComplex, (listDetComplex, ZZ, List, ChainComplex)},
	Headline => "This function calculates the list with the determinants of some minors of the maps of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree.",
	Usage => "listOfTheDetOfTheComplex = listDetComplex(d,v,C)",

	Inputs => {
		"d" => ZZ => {"integer corresponding to the degree of the strand of the chain complex."},
		"v" => List => {"list of variables of the polynomial ring R to take into account for elimination"},
		"C" => ChainComplex => {"a chain complex of free modules over a polynomial ring"}
	},
	Outputs => {
		List => {"a list with the determinant polynomials of the maps computed by 'minorsComplex'"}
	},

	PARA {}, "This function calculates the list with the determinants of some minors of the maps of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree. Precisely, this list corresponds to the list with the determinant polynomials of the maps computed by ", TO "minorsComplex", ".",
	PARA {}, "The input ChainComplex needs to be an exact complex of free modules over a polynomial ring. The polynomial ring must contain the list ", TT "v", " as variables.",
	PARA {}, "It is recommended not to defines rings as R=QQ[x,y][a,b,c] when the variables to eliminate are '{x,y}'. In this case, see ", TO "flattenRing", " for passing from ", TEX "$R=QQ[x,y][a,b,c]$", " to ", TEX "QQ[x,y,a,b,c].",
	
	EXAMPLE {" R=QQ[a,b,c,x,y] ",
		" f1 = a*x^2+b*x*y+c*y^2 ",
		" f2 = 2*a*x+b*y ",
		" M = matrix{{f1,f2}} ",
		" l = {x,y} ",
		" dHPM = listDetComplex (2,l,koszul M)",
		" dHPM = listDetComplex (3,l,koszul M)"
		},
		
	EXAMPLE {" R=QQ[a,b,c,d,e,f,g,h,i,x,y,z] ",
		" f1 = a*x+b*y+c*z ",
		" f2 = d*x+e*y+f*z ",
		" f3 = g*x+h*y+i*z ",
		" M = matrix{{f1,f2,f3}} ",
		" l = {x,y,z} ",
		" dHPM = listDetComplex (1,l,koszul M, Strategy => Exact)",
		" setRandomSeed 0",			    -- set seed to one known to work
		" dHPM = listDetComplex (2,l,koszul M, Strategy => Numeric)"
		},
     
	SeeAlso => {degHomPolMap, detComplex, minorsComplex, mapsComplex }
}


------------------------ \ detComplex / ------------------------

document {
     	Key => {detComplex, (detComplex, ZZ, List, ChainComplex)},
	Headline => "This function calculates the determinant of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree.",
	Usage => " deteriminantComplex = detComplex(d,v,C)",

	Inputs => {
		"c" => ZZ => {"integer corresponding to the degree of the strand of the chain complex."},
		"v" => List => {"list of variables of the polynomial ring R to take into account for elimination"},
		"C" => ChainComplex => {"a chain complex of free modules over a polynomial ring"}
	},
	Outputs => {
		"aPolynomialList" => List => {"an element in frac(R) that is the alternate product of the elements in the list 'listDetComplex'"}
	},

	PARA {}, "This function calculates the determinant of a graded ChainComplex with respect to a subset of the variables of the polynomial ring in a fixed degree. It corresponds to the alternate product of the elements in the list ", TO "listDetComplex",
	PARA {}, "The input ChainComplex needs to be an exact complex of free modules over a polynomial ring. The polynomial ring must contain the list ", TT "v", " as variables.",
	PARA {}, "It is recommended not to defines rings as R=QQ[x,y][a,b,c] when the variables to eliminate are '{x,y}'. In this case, see ", TO "flattenRing", " for passing from ", TEX "$R=QQ[x,y][a,b,c]$", " to ", TEX "QQ[x,y,a,b,c].",
	
	EXAMPLE {" R=QQ[a,b,c,x,y] ",
		" f1 = a*x^2+b*x*y+c*y^2 ",
		" f2 = 2*a*x+b*y ",
		" M = matrix{{f1,f2}} ",
		" l = {x,y} ",
		" dHPM = detComplex (2,l,koszul M)",
		" dHPM = detComplex (3,l,koszul M)"
		},
		
	EXAMPLE {" R=QQ[a,b,c,d,e,f,g,h,i,x,y,z] ",
		" f1 = a*x+b*y+c*z ",
		" f2 = d*x+e*y+f*z ",
		" f3 = g*x+h*y+i*z ",
		" M = matrix{{f1,f2,f3}} ",
		" l = {x,y,z} ",
		" dHPM = detComplex (1,l,koszul M)",
		" dHPM = detComplex (2,l,koszul M)",
		" dHPM = detComplex (1,l,koszul M, Strategy => Exact)",
		" dHPM = detComplex (1,l,koszul M, Strategy => Numeric)"
		},
     
	SeeAlso => {degHomPolMap, listDetComplex, minorsComplex, mapsComplex }
}


------------------------ \ Strategy for functions on Complex / ------------------------

scan({detComplex, minorsComplex, listDetComplex}, fn -> document { 
    Key => [fn, Strategy],
    Headline => "choose between Exact and Numeric algorithms",     
    Usage => toString fn | "(d,v,C, Strategy => s)",     
    
    Inputs => {
	"d" => ZZ => {"integer corresponding to the degree of the strand of the chain complex."},
	"v" => List => {"list of variables of the polynomial ring R to take into account for elimination"},
	"C" => ChainComplex => {"a chain complex of free modules over a polynomial ring"},
	"s" => Symbol => {"either ", TT "Exact", " or ", TT "Numeric"}	  
	},
    
    Consequences => {{ "If ", TT "s", " is ", TO "Exact", ", then the", TO "rank", " algorithms is used computing minors; if ", TT "s", " is ", TO "Numeric", ", then numerical rank computation is used, this is, all coefficients are evaluated in the ground field before computing ranks."}},
    
    PARA{}, TO "Exact", "is the default Strategy.",     
    
		
    SeeAlso => select({detComplex, minorsComplex, listDetComplex}, g -> g =!= fn)
     })

------------------------ \ documentation maxCol / ------------------------

document {
	Key => {maxCol, (maxCol, Matrix)},
	Headline => "Returns a submatrix form by a maximal set of linear independent columns.",
	Usage => " MM = maxCol(m)",
	Inputs => {
		"m" => Matrix
	},
	Outputs => {
		"M" => Matrix,
		"c" => List
	},
	PARA {}, " From a given m x n - Matrix of rank r, ", TO maxCol, " returns a submatrix ", TT "M", " form by a maximal set of linear independent columns, and the list of columns ", TT "c", " chosen.",

	EXAMPLE { 
		" M = matrix {{1,2,3},{1,2,3},{4,5,6},{4,5,6}}",
		" maxCol M;"},

	PARA {}, "NOTE: because of the necessity of ", TO rank," the base field need to be QQ for doing generic evaluation. If not, one gets the message: expected an affine ring (consider Generic=>true to work over QQ).",

	EXAMPLE {
		" R=QQ[a..g]",
		" M = matrix {{a,a,b},{c,c,d},{e,e,f},{g,g,g}}",
		" maxCol M"},

	SeeAlso => {maxMinor, rank}

}

------------------------ \ documentation maxMinor / ------------------------

document {
	Key => {maxMinor, (maxMinor, Matrix)},
	Headline => "Returns a maximal minor of the matrix of full rank.",
	Usage => " MM = maxMinor(Mat)",
	Inputs => {
		"m" => Matrix
	},
	Outputs => {
		Matrix
	},
	PARA {}, " From a given m x n - Matrix of rank r, ", TO maxMinor, " returns an r x r full rank Matrix. This method uses twice the method ", TO maxCol," by transposing twice.",
	
	EXAMPLE { 
		" M=matrix {{1,2,3},{1,2,3},{4,5,6},{4,5,6}}",
		" maxMinor M"},

	PARA {}, "NOTE: because of the necessity of ", TO rank," the base field need to be QQ for doing generic evaluation. If not, one gets the message: expected an affine ring (consider Generic=>true to work over QQ).",

	EXAMPLE {
		" R=QQ[a..g]",
		" M=matrix {{a,a,b},{c,c,d},{e,e,f},{g,g,g}}",
		" maxMinor M"},

	SeeAlso => {maxCol, rank}
}



------------------------ \ Strategy for functions on Minors / ------------------------

scan({maxCol, maxMinor}, fn -> document { 
    Key => [fn, Strategy],
    Headline => "choose between Exact and Numeric algorithms",     
    Usage => toString fn | "(M, Strategy => s)",     
    
    Inputs => {
	"m" => Matrix => {"a matrix (usually with coefficients in a polynomial ring)"},
	"s" => Symbol => {"either ", TT "Exact", " or ", TT "Numeric"}	  
	},
    
    Consequences => {{ "If ", TT "s", " is ", TO "Exact", ", then the", TO "rank", " algorithms is used computing minors; if ", TT "s", " is ", TO "Numeric", ", then numerical rank computation is used, this is, all coefficients are evaluated in the ground field before computing ranks."}},
    
    PARA{}, TO "Exact", "is the default Strategy.",     
    
		
    SeeAlso => select({maxCol, maxMinor}, g -> g =!= fn)
     })
------------------------ \ macaulayFormula / ------------------------
document {
     	Key => {macaulayFormula, (macaulayFormula, List, Matrix)},
	Headline => "returns two matrices such that the ratio of their determinants is the Macaulay resultant",
	Usage => " macaulayFormula(v,m)",

	Inputs => {
		"v" => List => {"a list of n variables such that the polynomials ", TEX "$f_1,...,f_n$", " are homogeneous with respect to these variables"},
		"m" => Matrix => {"a single row matrix with polynomials ", TEX "$f_1,...,f_n$"},

	},
	Outputs => {
		List => {"a list of two matrices such that the ratio of their determinants is the Macaulay resultant of f_1,...,f_n with respect to the variables ", TT "v"}
	},

	PARA {}, "Let ", TEX "$f_1,...,f_n$", " be a polynomials two groups of variables ", TEX "$X_1,...,X_n$", " and ", TEX "$a_1,...,a_s$", " and such that ", TEX "$f_1,...,f_n$", " are homogeneous polynomials with respect to the variables ", TEX "$X_1,...,X_n$", ". This function returns two matrices ", TT "M1", " and ", TT "M2", " such that ", TEX "$det(D_1)/det(D_2)$", " is the Macaulay resultant of ", TEX "$f_1,...,f_n$", " providing ", TEX "det(D_2)", " is nonzero.",
	
	PARA {}, "Remark: if D2 is the empty matrix, its determinant has to be understood as 1 (and not zero, which is the case in Macaulay2 since the empty matrix is identified to the zero.",


	EXAMPLE {" R=QQ[a..i,x,y,z]",
	"f1 = a*x+b*y+c*z",
	"f2 = d*x+e*y+f*z",
	"f3 = g*x+h*y+i*z",
	"M = matrix{{f1,f2,f3}}",
	"l = {x,y,z}",
	"MR = macaulayFormula (l,M)"
		},
     
	SeeAlso => {eliminationMatrix, detComplex, listDetComplex, minorsComplex, mapsComplex }
     
}


------------------------ \ bezoutianMatrix / ------------------------
document {
     	Key => {bezoutianMatrix, (bezoutianMatrix, List, Matrix)},
	Headline => "returns a matrix associated to generalized resultants",
	Usage => " bezoutianMatrix(v, m)",

	Inputs => {
		"v" => List => {" a list of n-1 variables to be eliminated form the fi's"},
		"m" => Matrix => {" a single row matrix with (affine) polynomials ", TEX "$f_1,...,f_n$"},
	},
	Outputs => {
		 Matrix => {"an elimination matrix"}
	},

	PARA {}, "Let R be a polynomial ring in two groups of variables ", TEX "$X_1,...,X_{n-1}$"," and ", TEX "$a_1,...,a_s$", ". The variables ", TEX "$a_1,...,a_s$", " are seen as parameters and the variables ", TEX "$X_1,...,X_{n-1}$", " are to be eliminated. Being given a row matrix ", TEX "$f_1,...,f_n$", " where each  ", TEX "$f_i$", " is a polynomial in ", TEX "$X_1,...,X_{n-1}$", " and ", TEX "$a_1,...,a_s$", ", this function returns an elimination matrix that only depends on the parameters ", TEX "$a_1,...,a_s$", " and whose maximal nonzero minor yields a multiple of the generalized resultant associated to ", TEX "$f_1,...,f_n$",


	EXAMPLE {" R=QQ[a..i,x,y]",
	"f1 = a*x+b*y+c",
	"f2 = d*x+e*y+f",
	"f3 = g*x+h*y+i",
	"M = matrix{{f1,f2,f3}}",
	"l = {x,y}",
	"MR = bezoutianMatrix (l,M)"
		},		
     
	SeeAlso => {eliminationMatrix, macaulayFormula}
     
}


------------------------ \ eliminationMatrix / ------------------------

------------------------ \ macRes / ------------------------

document {
     	Key => {(eliminationMatrix, List, Matrix)},
	Headline => "returns a matrix associated to the Macaulay resultant",
	Usage => " eliminationMatrix(v,m)",

	Inputs => {
		"v" => List => {" a list of n variables such that the polynomials ", TEX "$f_1,...,f_n$", " are homogeneous with respect to these variables"},
		"m" => Matrix => {" a single row matrix with polynomials ", TEX "$f_1,...,f_n$",},
	},
	Outputs => {
		Matrix => {"a generically surjective matrix such that the gcd of its maximal minors if the Macaulay resultant of f1,...,fn with respect to the variables 'varList'"}
	},



	PARA {}, "Let ", TEX "$f_1,...,f_n$", " be a polynomials two groups of variables ", TEX "$X_1,...,X_n$", " and ", TEX "$a_1,...,a_s$", " and such that ", TEX "$f_1,...,f_n$", " are homogeneous polynomials with respect to the variables ", TEX "$X_1,...,X_n$", ". This function returns a matrix which is generically (in terms of the parameters ", TEX "$a_1,...,a_s$", ") surjective such that the gcd of its maximal minors is the Macaulay resultant of ", TEX "$f_1,...,f_n$",


	EXAMPLE {" R=QQ[a..i,x,y,z]",
	"f1 = a*x+b*y+c*z",
	"f2 = d*x+e*y+f*z",
	"f3 = g*x+h*y+i*z",
	"M = matrix{{f1,f2,f3}}",
	"l = {x,y,z}",
	"MR = eliminationMatrix (l,M)"
	},
}


------------------------ \ detRes / ------------------------
document {
    Key => {(eliminationMatrix, ZZ, List, Matrix)},
	Headline => "returns a matrix corresponding to the determinantal resultant, in particular the Macaulay resultant",
	Usage => " eliminationMatrix(r, v, m)",

	Inputs => {
		"r" => ZZ => {"corresponding to the regularity index used to form the determinantal resultant"},
		"v" => List => {" a list of homogeneous variables that are to be eliminated"},
		"m" => Matrix => {"a polynomial matrix"},
	},
	Outputs => {
		"M" => Matrix => {"a matrix corresponding to the determinantal resultant"}
	},

	PARA {}, " Compute the determinantal resultant of an (n,m)-matrix (n<m) of homogeneous polynomials over the projective space of dimension (m-r)(n-r), i.e. a condition on the parameters of these polynomials to have rank(M)<r+1.", TT "M",


	EXAMPLE {"R=QQ[a_0..a_5,b_0..b_5,x,y]",
	"M:=matrix{{a_0*x+a_1*y,a_2*x+a_3*y,a_4*x+a_5*y},{b_0*x+b_1*y,b_2*x+b_3*y,b_4*x+b_5*y}}",
	"eliminationMatrix(1,{x,y},M, Strategy => determinantal)"
		},
     
	SeeAlso => {macaulayFormula, bezoutianMatrix}
     
}



------------------------ \ cm2Res / ------------------------
document {
     	Key => {(eliminationMatrix, List, Matrix, Matrix)},
	Headline => "returns a matrix corresponding to a residual resultant",
	Usage => " eliminationMatrix(v, r, m)",

	Inputs => {
		"v" => List => {"list ", TT "v", " of variables with respect to which the polynomials are homogeneous and from which one wants to remove these variables"},
		"r" => Matrix => {"a single row matrix describing the base locus"},
		"m" => Matrix => {"a matrix corresponding to the decomposition of a polynomial system over the base locus"},
	},
	Outputs => {
		Matrix => {"a matrix corresponding to the residual resultant"}
	},


	PARA {}, "If the strategy is 'CM2Residual':",
	
	PARA {}, " Suppose given a homogeneous ideal locally complete intersection Cohen-Macaulay codimension 2 ", TEX "$J=(g_1,..,g_n)$", ", such that ", TEX "$I=(f_1,..,f_m)$", " is included in J and (I:J) is a residual intersection. Let H be the matrix that I=J.H. Let R be the matrix of the first syzygies of J. This function computes an elimination matrix corresponding to the residual resultant over V(I) over V(J).",


	EXAMPLE {"R = QQ[X,Y,Z,x,y,z]",
	"F = matrix{{x*y^2,y^3,x*z^2,y^3+z^3}}",
	"G = matrix{{y^2,z^2}}",
	"M = matrix{{1,0,0},{0,1,0},{0,0,1},{-X,-Y,-Z}}",
	"H = (F//G)*M",
	"l = {x,y,z}",
	"L=eliminationMatrix (l,G,H, Strategy => CM2Residual)",
	"maxCol L",
		},

	PARA {}, "If the strategy is 'ciResidual':",
	
	PARA {}, "This function basically computes the matrix of the first application in the resolution of (I:J) given in the article of Bruns, Kustin and Miller: 'The resolution of the generic residual intersection of a complete intersection', Journal of Algebra 128.",
	
	PARA {}, "The first argument is a list of homogeneous polynomials ", TEX "$J=(g_1,..,g_n)$", "forming a complete intersection with respect to the variables 'varList'. Given a system of homogeneous ", TEX "$I=(f_1,..,f_m)$", ", such that I is included in J and (I:J) is a residual intersection, one wants to to compute a sort of resultant of (I:J). The second argument is the matrix M such that I=J.M. The output is a generically (with respect to the other variables than ", TT "v", ") surjective matrix such that the determinant of a maximal minor is a multiple of the resultant of I on the closure of the complementary of V(J) in V(I). Such a minor can be obtain with ", TO2((maxMinor),"maxMinor"),".",


	EXAMPLE {" R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]", 
	"G=matrix{{z,x^2+y^2}}", 
	"H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}",
	"L=eliminationMatrix ({x,y,z},G,H, Strategy => ciResidual)",
		},
    
    
	PARA {}, "If the strategy is 'byResolution':",
	
	PARA {}, "This function computes the matrix of the first application in the resolution of (I:J) given by ", TO "resolution", "in degree ", TO "regularity",

	EXAMPLE {" R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]", 
	"G=matrix{{z,x^2+y^2}}", 
	"H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}",
	"L=eliminationMatrix ({x,y,z},G,H, Strategy => byResolution)",
		},
		
	SeeAlso => {macaulayFormula, bezoutianMatrix}
     
}



------------------------ \ Strategy for eliminationMatrix / ------------------------
document {
     	Key => {eliminationMatrix, [eliminationMatrix, Strategy]},
	Headline => "returns a matrix that represents the image of the map",
	Usage => " eliminationMatrix(..., Strategy => s)",


	PARA {}, "If the strategy 's' is 'Sylvester':",

	PARA {}, "Refer to ", TO "sylvesterMatrix",

	PARA {}, "If the strategy 's' is 'Macaulay':",

	PARA {}, "Let ", TEX "$f_1,..,f_n$", " be a polynomials two groups of variables ", TEX "$X_1,...,X_n$", " and  ", TEX "$a_1,...,a_s$", " and such that ", TEX "$f_1,...,f_n$", " are homogeneous polynomials with respect to the variables ", TEX "$X_1,...,X_n$", ". This function returns a matrix which is generically (in terms of the parameters ", TEX "$a_1,...,a_s$", ") surjective such that the gcd of its maximal minors is the Macaulay resultant of ", TEX "$f_1,...,f_n$", ".",


	EXAMPLE {" R=QQ[a_0..a_8,x,y,z]",
	"f1 = a_0*x+a_1*y+a_2*z",
	"f2 = a_3*x+a_4*y+a_5*z",
	"f3 = a_6*x+a_7*y+a_8*z",
	"M = matrix{{f1,f2,f3}}",
	"l = {x,y,z}",
	"MR = eliminationMatrix (l,M, Strategy => Macaulay)"
	},


	PARA {}, "If the strategy 's' is 'determinantal':",

	PARA {}, " Compute the determinantal resultant of an (n,m)-matrix (n<m) of homogeneous polynomials over the projective space of dimension (m-r)(n-r), i.e. a condition on the parameters of these polynomials to have rank(M)<r+1.",


	EXAMPLE {"R=QQ[a_0..a_5,b_0..b_5,x,y]",
	"M:=matrix{{a_0*x+a_1*y,a_2*x+a_3*y,a_4*x+a_5*y},{b_0*x+b_1*y,b_2*x+b_3*y,b_4*x+b_5*y}}",
	"eliminationMatrix(1,{x,y},M, Strategy => determinantal)"
		},

	PARA {}, "If the strategy 's' is 'CM2Residual':",
	
	PARA {}, " Suppose given a homogeneous ideal locally complete intersection Cohen-Macaulay of codimension 2,", TEX "$J=(g_1,..,g_n)$",", such that ", TEX"$I=(f1,..,fm)$", " is included in J and (I:J) is a residual intersection. Let H be the matrix that I=J.H. Let R be the matrix of the first syzygies of J. This function computes an elimination matrix corresponding to the residual resultant over V(I) over V(J).",


	EXAMPLE {"R = QQ[X,Y,Z,x,y,z]",
	"F = matrix{{x*y^2,y^3,x*z^2,y^3+z^3}}",
	"G = matrix{{y^2,z^2}}",
	"M = matrix{{1,0,0},{0,1,0},{0,0,1},{-X,-Y,-Z}}",
	"H = (F//G)*M",
	"l = {x,y,z}",
	"L=eliminationMatrix (l,G,H, Strategy => CM2Residual)",
	"maxCol L",
		},

	PARA {}, "If the strategy 's' is 'ciResidual':",
	
	PARA {}, "This function basically computes the matrix of the first application in the resolution of (I:J) given in the article of Bruns, Kustin and Miller: 'The resolution of the generic residual intersection of a complete intersection', Journal of Algebra 128.",
	
	PARA {}, "The first argument is a list of homogeneous polynomials ", TEX "$J=(g_1,..,g_m)$"," forming a complete intersection with respect to the variables 'varList'. Given a system of homogeneous ", TEX "$I=(f_1,..,f_n)$"," such that I is included in J and (I:J) is a residual intersection, one wants to to compute a sort of resultant of (I:J). The second argument is the matrix M such that I=J.M. The output is a generically (with respect to the other variables than 'varList') surjective matrix such that the determinant of a maximal minor is a multiple of the resultant of I on the closure of the complementary of V(J) in V(I). Such a minor can be obtain with ", TO2((maxMinor),"maxMinor"),".",


	EXAMPLE {" R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]", 
	"G=matrix{{z,x^2+y^2}}", 
	"H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}",
	"L=eliminationMatrix ({x,y,z},G,H, Strategy => ciResidual)",
		},
    
    
	PARA {}, "If the strategy is 'byResolution':",
	
	PARA {}, "This function computes the matrix of the first application in the resolution of (I:J) given by ", TO "resolution", "in degree ", TO "regularity",

	EXAMPLE {" R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]", 
	"G=matrix{{z,x^2+y^2}}", 
	"H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}",
	"L=eliminationMatrix ({x,y,z},G,H, Strategy => byResolution)",
		},
		
	SeeAlso => {macaulayFormula, bezoutianMatrix}
     
}


------------------------ \ Degrees / ------------------------

------------------------ \ detResDeg / ------------------------
document {
     	Key => {detResDeg},
	Headline => "compute a regularity index and partial degrees of the determinantal resultant",
	Usage => " detResDeg(n, c, r, R)",

	Inputs => {
		"n" => ZZ => {" corresponding to the drop of rank used to form the determinantal resultant"},
		"c" => List => {"a list of degrees indexing the columns of a polynomial matrix "},
		"r" => List => {"a list of degrees indexing the rows of the same polynomial"},
		"R" => Ring => {" an ambient ring where the computations take place"},
	},
	Outputs => {
		List => {"a list of element consisting of a regularity index and partial degree of homogeneity of the  determinantal resultant"}
	},

	PARA {}, " Compute the regularity index and the multi-degree of the determinantal resultant associated to the matrix M",


	EXAMPLE {"R = ZZ[d1,d2,d3,k1,k2]",
	"detResDeg(1,{d1,d2,d3},{k1,k2},R)",
		},
     
	SeeAlso => {macaulayFormula, eliminationMatrix, ciResDeg, bezoutianMatrix}
     
}



------------------------ \ ciResDeg / ------------------------
document {
     	Key => {ciResDeg}, --, (ciResDeg, List, List)
	Headline => "compute a regularity index and partial degrees of the residual resultant over a complete intersection",
	Usage => " ciResDeg(List, List)",

	Inputs => {
		"d" => List => {"a list of element in the ambient ring corresponding to the degrees of a system of polynomials ", TEX "$d_0,...,d_n$"},
		"k" => List => {"a list of element in the ambient ring corresponding to the degrees of a complete intersection ", TEX "$k_1,...,k_m$", ", where m=<n"},
	},
	Outputs => {
		"List" => List => {"a list of element consisting of a regularity index and partial degree of homogeneity"}
	},

	PARA {}, " Given a system of polynomials ", TEX "$f_0,...,f_n$", " of degree ", TEX "$d_0,...,d_n$", " that are contained in a complete intersection ", TEX "$g_1,...,g_m$", " of degree ", TEX "$k_1,...,k_m$", ", this function returns the regularity index used to form the matrix associated to the residual resultant over a complete intersection and then all the partial degrees of this resultant with respect to the coefficients of ", TEX "$f_0,f_1,..,f_n$", ".",


	EXAMPLE {" R=ZZ[d_0..d_3,k_1,k_2]",
	"L=ciResDeg({d_0,d_1,d_2,d_3},{k_1,k_2})", 
		},
     
	SeeAlso => {eliminationMatrix,ciResDegGH}
     
}

------------------------ \ ciResDegGH / ------------------------
document {
     	Key => {ciResDegGH}, --, (ciResDeg, List, Matrix, Matrix)
	Headline => "compute a regularity index used for the residual resultant over a complete intersection",
	Usage => " ciResDegGH(r, m, v)",

	Inputs => {
		"v" => List => {"list 'var' of variables with respect to which the polynomials are homogeneous and from which one wants to remove these variables"},
		"r" => Matrix => {"a single row matrix describing the base locus"},
		"m" => Matrix => {"a matrix corresponding to the decomposition of a polynomial system over the base locus"},
	},
	Outputs => {
		 ZZ => {"a regularity index to form the residual resultant-"}
	},

	PARA {}, " This function is similar to the first element in the list returned by the function ", TO2(ciResDeg,"ciResDeg")," but with arguments that are identical to the ones used with the function ", TO2(eliminationMatrix,"eliminationMatrix")," using the Strategy ", TO2(CM2Residual,"CM2Residual") ,".",

	EXAMPLE {" R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]", 
	"G=matrix{{z,x^2+y^2}}", 
	"H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}",
	"ciResDegGH({x,y,z},G,H)",
		},
     
	SeeAlso => {eliminationMatrix, ciResDeg}
     
}


------------------------ \ Doc of Symbols / ------------------------
------------------------ \ Numeric / ------------------------

document { 
	Key => {Numeric},
	Headline => "Strategy for functions that uses rank computation.",
	Usage => " Strategy => Numeric",
	    
    Consequences => { "If 'Numeric' Strategy is used, then numerical rank computation is used instead of ", TO "Exact", " rank computation. Precisely, all coefficients are evaluated in the ground field before computing ranks."},
    
}


------------------------ \ Exact / ------------------------

document { 
	Key => {Exact},
	Headline => "Strategy for functions that uses rank computation.",
	Usage => " Strategy => Exact",
	    
    Consequences => { "If 'Exact' Strategy is used, then exact rank computation is used. This could be notably slower that ", TO "Numeric", " rank computation when many generic elements are manipulated."},

}

------------------------ \ Macaulay / ------------------------

document { 
	Key => {Macaulay},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => Macaulay)",
	    
    Consequences => { "Let ", TEX "$f_1,...,f_n$", " be a polynomials two groups of variables ", TEX "$X_1,...,X_n$", " and ", TEX "$a_1,...,a_s$", " and such that ", TEX "$f_1,...,f_n$", " are homogeneous polynomials with respect to the variables ", TEX "$X_1,...,X_n$", ". This function returns a matrix which is generically (in terms of the parameters ", TEX "$a_1,...,a_s$", ") surjective such that the gcd of its maximal minors is the Macaulay resultant of  ", TEX "$f_1,...,f_n$", "."},

}

------------------------ \ Sylvester / ------------------------

document {
	Key => {Sylvester},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => Sylvester)",
	    
    Consequences => {"Invokes the method ", TO "sylvesterMatrix"},

}


------------------------ \ determinantal / ------------------------

document {
	Key => {determinantal},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => determinantal)",
	    
    Consequences => {"Compute the determinantal resultant of an (n,m)-matrix (n<m) of homogeneous polynomials over the projective space of dimension m-n, i.e. a condition on the parameters of these polynomials to have rank(M)<n."},

}

------------------------ \ ciResidual / ------------------------

document {
	Key => {ciResidual},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => ciResidual)",
	    
    Consequences => {"This function basically computes the matrix of the first application in the resolution of (I:J) given in the article of Bruns, Kustin and Miller: 'The resolution of the generic residual intersection of a complete intersection', Journal of Algebra 128."},
	
	PARA {"The first argument is a list of homogeneous polynomials ", TEX "$J=(g_1,..,g_m)$", " forming a complete intersection with respect to the variables 'varList'. Given a system of homogeneous  ", TEX "$I=(f_1,..,f_n)$", ", such that I is included in J and (I:J) is a residual intersection, one wants to to compute a sort of resultant of (I:J). The second argument is the matrix M such that I=J.M. The output is a generically (with respect to the other variables than 'varList') surjective matrix such that the determinant of a maximal minor is a multiple of the resultant of I on the closure of the complementary of V(J) in V(I). Such a minor can be obtain with", TO "maxMinor"}, 
	
}

------------------------ \ CM2Residual / ------------------------

document {
	Key => {CM2Residual},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => CM2Residual)",
	    
    Consequences => {"Suppose given a homogeneous ideal locally complete intersection Cohen-Macaulay of codimension 2,  ", TEX "$J=(g_1,..,g_n)$", ", such that  ", TEX "$I=(f_1,..,f_m)$", " is included in J and (I:J) is a residual intersection. Let H be the matrix that I=J.H. Let R be the matrix of the first syzygies of J. This function computes an elimination matrix corresponding to the residual resultant over V(I) over V(J)."},

}

------------------------ \ byResolution / ------------------------

document {
	Key => {byResolution},
	Headline => "Strategy for eliminationMatrix.",
	Usage => " eliminationMatrix(..., Strategy => byResolution)",
	    
    Consequences => {"This function computes the matrix of the first application in the resolution of (I:J) given by ", TO "resolution", "in degree ", TO "regularity"},

}

------------------------ \ regularityVar / ------------------------

document { 
    Key => {regularityVar, (regularityVar, List, Ideal)},
    Headline => "computes the Castelnuovo-Mumford regularity of homogeneous ideals in terms of Betti numbers, with respect to some of the variables of the ring",     
    Usage => "regularityVar(l,I)",     
    
    Inputs => {
	"l" => List => {"list of variables of the polynomial ring R to take into account for computing the Castelnuovo-Mumford regularity"},
	"I" => Ideal => {"ideal of a polynomial ring"}
       	},
       
    PARA{}, TT "regularityVar", " computes the Castelnuovo-Mumford regularity of homogeneous ideals in a polynomial ring by computing the shifts and degrees of generators in a minimal free resolution of the homogeneous ideal.",
    PARA{}, "The list of variables ", TT "l", " contains the variables of the ring having degree 1. Those variables on the ring not in ", TT "l", " have automatically degree 0, as well as the the elements on the coefficient ring",
             
    	EXAMPLE {" R=QQ[a..i,x,y,z]",
	"f1 = a*x+b*y+c*z",
	"f2 = d*x+e*y+f*z",
	"f3 = g*x+h*y+i*z",
	"I = ideal(f1,f2,f3)",
	"l = {x,y,z}",
	"regularityVar (l,I)"
      	},
    
    SeeAlso => {coefficientRing, resolution, res}
     }

---------------------------------------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
------------------------- TESTS -------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
---------------------------------------------------------------
--	degMap,
--	macRes	

-- Test 0
-- Checking the function degHomPolMap
TEST ///
R=QQ[a,b,c,x,y];
f1 = a*x^2+b*x*y+c*y^2;
f2 = 2*a*x+b*y;
M = matrix{{f1,f2}};
l = {x,y};

dHPM = degHomPolMap (M,l,2)
MR = eliminationMatrix (l,M)
assert(MR == eliminationMatrix (l,M, Strategy => Macaulay))
Syl = matrix {{a, 2*a, 0}, {b, b, 2*a}, {c, 0, b}}

assert(dHPM_1 == MR)
assert(dHPM_0 == gens ((ideal {x,y})^2))
assert(toString dHPM_1 == toString Syl)

mapC = mapsComplex (2, l, koszul M)

assert(dHPM_1 == mapC_0)

///

-- Test 1
-- Checking the function degHomPolMap and macRes
TEST ///
R=QQ[a..i,x,y,z];
f1 = a*x+b*y+c*z;
f2 = d*x+e*y+f*z;
f3 = g*x+h*y+i*z;
M = matrix{{f1,f2,f3}};
l = {x,y,z}
dHPM = degHomPolMap (M,l,1)
MR = eliminationMatrix (l,M)
Jac = transpose matrix{{a,b,c},{d,e,f},{g,h,i}};

assert(dHPM_1 == MR)
assert(dHPM_0 == gens (ideal {x,y,z}))
assert(toString dHPM_1 == toString Jac)

mapC = mapsComplex (1, l, koszul M)

assert(dHPM_1 == mapC_0)

///

-- Test 2
-- Checking the function mapsComplex and minorsComplex

TEST ///

R=QQ[a,b,c,x,y];
f1 = a*x^2+b*x*y+c*y^2;
f2 = 2*a*x+b*y;
M = matrix{{f1,f2}};
l = {x,y};

mapC2 = mapsComplex (2, l, koszul M)
minC2 = minorsComplex (2, l, koszul M)

assert(mapC2_0 == minC2_0)

mapC3 = mapsComplex (3, l, koszul M)
minC3 = minorsComplex (3, l, koszul M)

assert(mapC3_0_{0,1,2,3} == minC3_0)

assert((degHomPolMap (M,l,3))_1 == mapC3_0)

///

-- Test 3
-- Checking the function mapsComplex and minorsComplex

TEST ///

R=QQ[a..i,x,y,z];
f1 = a*x+b*y+c*z;
f2 = d*x+e*y+f*z;
f3 = g*x+h*y+i*z;
M = matrix{{f1,f2,f3}};
l = {x,y,z}
mapC1 = mapsComplex (1, l, koszul M)
minC1 = minorsComplex (1, l, koszul M)

assert(mapC1_0 == minC1_0)

mapC2 = mapsComplex (2,l,koszul M)
minC2 = minorsComplex (2,l,koszul M)

assert(mapC2_0_{0,1,2,3,4,6} == minC2_0)

assert((degHomPolMap (M,l,2))_1 == mapC2_0)

///


-- Test 4
-- Checking the function detComplex

TEST ///

R=QQ[a,b,c,x,y];
f1 = a*x^2+b*x*y+c*y^2;
f2 = 2*a*x+b*y;
M = matrix{{f1,f2}};
l = {x,y};

detC2 = detComplex (2,l,koszul M)
detC3 = detComplex (3,l,koszul M)
detC4 = detComplex (4,l,koszul M)

assert(detC2 == detC3)
assert(detC2 == detC4)

///


-- Test 5
-- Checking the function detComplex

TEST ///

R=QQ[a..i,x,y,z];
f1 = a*x+b*y+c*z;
f2 = d*x+e*y+f*z;
f3 = g*x+h*y+i*z;
M = matrix{{f1,f2,f3}};
l = {x,y,z}

detC1 = detComplex (1,l,koszul M)
detC2 = detComplex (2,l,koszul M)
detC3 = detComplex (3,l,koszul M)
detC4 = detComplex (4,l,koszul M)

assert(detC1 == detC2)
assert(detC3 == detC4)
assert(detC1 == -detC3)

///

-- Test 6
-- Checking the function ciRes
TEST ///

R=QQ[a_0,a_1,a_2,a_3,a_4,b_0,b_1,b_2,b_3,b_4,c_0,c_1,c_2,c_3,c_4,x,y,z]; 
G=matrix{{z,x^2+y^2}}; 
H=matrix{{a_0*z+a_1*x+a_2*y,b_0*z+b_1*x+b_2*y,c_0*z+c_1*x+c_2*y},{a_3,b_3,c_3}}; 
L=eliminationMatrix({x,y,z},G,H, Strategy => ciResidual)
maxCol L


assert(toString L == "matrix {{a_3, b_3, c_3, -a_3*b_1+a_1*b_3, 0, 0, -a_3*c_1+a_1*c_3, 0, 0, -b_3*c_1+b_1*c_3, 0, 0}, {0, 0, 0, -a_3*b_2+a_2*b_3, -a_3*b_1+a_1*b_3, 0, -a_3*c_2+a_2*c_3, -a_3*c_1+a_1*c_3, 0, -b_3*c_2+b_2*c_3, -b_3*c_1+b_1*c_3, 0}, {a_1, b_1, c_1, -a_3*b_0+a_0*b_3, 0, -a_3*b_1+a_1*b_3, -a_3*c_0+a_0*c_3, 0, -a_3*c_1+a_1*c_3, -b_3*c_0+b_0*c_3, 0, -b_3*c_1+b_1*c_3}, {a_3, b_3, c_3, 0, -a_3*b_2+a_2*b_3, 0, 0, -a_3*c_2+a_2*c_3, 0, 0, -b_3*c_2+b_2*c_3, 0}, {a_2, b_2, c_2, 0, -a_3*b_0+a_0*b_3, -a_3*b_2+a_2*b_3, 0, -a_3*c_0+a_0*c_3, -a_3*c_2+a_2*c_3, 0, -b_3*c_0+b_0*c_3, -b_3*c_2+b_2*c_3}, {a_0, b_0, c_0, 0, 0, -a_3*b_0+a_0*b_3, 0, 0, -a_3*c_0+a_0*c_3, 0, 0, -b_3*c_0+b_0*c_3}}")
///


-- Test 7
-- Checking the function ciResDeg
TEST ///

R=ZZ[d_0..d_3,k_1,k_2]
L=ciResDeg({d_0,d_1,d_2,d_3},{k_1,k_2})

assert(toString L == "{d_0+d_1+d_2+d_3-3*k_2-3, {d_1*d_2*d_3-d_1*k_1*k_2-d_2*k_1*k_2-d_3*k_1*k_2+k_1^2*k_2+k_1*k_2^2, d_0*d_2*d_3-d_0*k_1*k_2-d_2*k_1*k_2-d_3*k_1*k_2+k_1^2*k_2+k_1*k_2^2, d_0*d_1*d_3-d_0*k_1*k_2-d_1*k_1*k_2-d_3*k_1*k_2+k_1^2*k_2+k_1*k_2^2, d_0*d_1*d_2-d_0*k_1*k_2-d_1*k_1*k_2-d_2*k_1*k_2+k_1^2*k_2+k_1*k_2^2}}")

///

-- Test 8
-- Checking the function cm2Res
TEST ///

R = QQ[X,Y,Z,x,y,z];
F = matrix{{x*y^2,y^3,x*z^2,y^3+z^3}}
G = matrix{{y^2,z^2}};
M = matrix{{1,0,0},{0,1,0},{0,0,1},{-X,-Y,-Z}};
H = (F//G)*M;
l = {x,y,z};
CmR = (eliminationMatrix (l,G,H, Strategy => CM2Residual))

I := (ideal (G*H): ideal G);
Mat= (mapsComplex(regularity I -1, l, res I))_0

fittI = minors(10,Mat)
fittCmR = minors(10,CmR)

assert(toString CmR == "matrix {{0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, -X, 1, 0, -Y+1, 0, 0}, {0, 0, -Y, 0, 0, 0, -Z, 0, 1, 0, 0, 0}, {-1, 0, 0, 0, 0, 0, 0, -X, 0, 0, -Y+1, 0}, {0, 0, X, -Y, 0, 0, 0, -Z, -X, -Z, 0, -Y+1}, {0, 0, 0, 0, -Y, -1, 0, 0, -Z, 0, 0, 0}, {X, Y-1, 0, 0, 0, Z, 0, 0, 0, 0, 0, 0}, {0, 0, 0, X, 0, 0, 0, 0, 0, 0, -Z, 0}, {0, 0, 0, 0, X, 0, 0, 0, 0, 0, 0, -Z}, {X, Y, 0, 0, 0, Z, 0, 0, 0, 0, 0, 0}}")

assert(fittI == fittCmR)

///

-- Test 9
-- Checking the function detResDeg
TEST ///

R = ZZ[d1,d2,d3,k1,k2]
DRD = detResDeg(1, {d1,d2,d3},{k1,k2},R)

assert(toString DRD == "{d1+d2+d3-k1-2*k2-1, {d2+d3-k1-k2, d1+d3-k1-k2, d1+d2-k1-k2}}")
///



-- Test 10
-- Checking the function detRes
TEST ///

R = QQ[a_0..a_5,b_0..b_5,x,y]
M = matrix{{a_0*x+a_1*y,a_2*x+a_3*y,a_4*x+a_5*y},{b_0*x+b_1*y,b_2*x+b_3*y,b_4*x+b_5*y}}
Res = eliminationMatrix(1,{x,y},M, Strategy => determinantal)

assert(toString Res == "matrix {{-a_2*b_0+a_0*b_2, -a_4*b_0+a_0*b_4, -a_4*b_2+a_2*b_4}, {-a_3*b_0-a_2*b_1+a_1*b_2+a_0*b_3, -a_5*b_0-a_4*b_1+a_1*b_4+a_0*b_5, -a_5*b_2-a_4*b_3+a_3*b_4+a_2*b_5}, {-a_3*b_1+a_1*b_3, -a_5*b_1+a_1*b_5, -a_5*b_3+a_3*b_5}}")

///

-- Test 11
-- Checking the function regularityVar
TEST ///

R=QQ[a..i,x,y,z]
f1 = a*x+b*y+c*z
f2 = d*x+e*y+f*z
f3 = g*x+h*y+i*z
I = ideal(f1,f2,f3)
l = {x,y,z}

assert(regularityVar (l,I)==1)

///
