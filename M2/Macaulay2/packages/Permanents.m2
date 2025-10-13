---------------------------------------------------------------------------
--PURPOSE : compute the permanent of matrices.
---------------------------------------------------------------------------
newPackage(
        "Permanents",
        Version => "0.9", 
        Date => "July 11, 2014",
        Authors => {{Name => "Tair Akhmejanov", 
                  Email => "ta328@cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~takhmejanov"}},
	Keywords => {"Commutative Algebra"},
        Headline => "permanents of a matrix"
        )

export {"ryser", 
        "glynn", 
        "grenet",
	"pminors"}

---------------------------------------------------------------------------
--ryser
---------------------------------------------------------------------------
ryser = method()
ryser Matrix := (M) -> (

      n := numgens target M;
      R := ring(M);
      
      E := entries M;
      
       --these are the sums of the rows
      s := new MutableList from {};
      for i from 0 to n-1 do(
	  s=append(s,sum(E#i));
	  );

      cube := set splice {0..n-1};

      permanent := 0;
            
      --Recall that we can list the binary strings of length n in a Gray code order by
      --defining defining a vector delta to be all 0's and counting as usual in binary.
      --Then the ith entry of the current word in the Gray code is delta_i+delta_{i+1},
      --so at the point increment delta below we are searching for the unique index
      --in which the current word in the Gray code changed.    
	    
      --initialize del vector to all 0's
      del := new MutableList from {0};
      --Is there any better way to create a MutableList of length n or is this the only way??
      for i from 1 to n-1 do(del = append(del,0););--delta is of length n
      
      --take the initial product corresponding to delta all +1's
      sgn:=1;
      prod:=1;
      for i from 0 to n-1 do(
	  prod=prod*s#i;
	  );
      permanent=prod;
      
      curIndex:=0;--which position in the gray code changed
      curVal:=0;--what is the new value in the changed position of the gray code
      for count from 2 to 2^n do(--start at 2 because we already took the initial product corresponding to delta consisting of all 1's
	  sgn=(-1)*sgn;
	  --increment delta
	  flag:=1;
	  for k from 0 to n-1 when flag==1 do(
	      if del#k==0 then (del#k=1; flag=0; curIndex=k;) else (del#k=0);
	      if curIndex==n-1 then curVal=del#(n-1) else curVal=(del#curIndex)+(del#(curIndex+1));
	      if curVal==2 then curVal=0;
	      );
	  
	  prod = 1;
	  --for every row update the values of s
	  for i from 0 to n-1 do (
	      if curVal==0 then s#i=s#i+(E#i#curIndex) else s#i=s#i-(E#i#curIndex); 
	  );
          --take the product of all of the row sums
          for i from 0 to n-1 do(
	  	  prod=prod*s#i;
	      );
	      
	  permanent = permanent+sgn*prod;
	  );
      
      permanent
)

---------------------------------------------------------------------------
--glynn
---------------------------------------------------------------------------
glynn = method()
glynn Matrix := (M) -> (
      
      n := numgens target M;
      R :=ring(M);
      
      if char R==2 then print("ring has characteristic 2");
      
      E :=entries M;
      --these are the sums of the rows
      s := new MutableList from {};
      for i from 0 to n-1 do(
	  s=append(s,sum(E#i));
	  );
            
      perm := 0;
      
      --Recall that we can list the binary strings of length n in a Gray code order by
      --defining defining a vector delta to be all 0's and counting as usual in binary.
      --Then the ith entry of the current word in the Gray code is delta_i+delta_{i+1},
      --so at the point increment delta below we are searching for the unique index
      --in which the current word in the Gray code changed.
      
      --initialize delta vector to all 1's
      del := new MutableList from {0};
      --Is there any better way to create a MutableList of length n or is this the only way??
      for i from 1 to n-2 do(del = append(del,0););--delta is of length n-1 because first position is always positive, so we don't consider it in the gray code
      
      --take the initial product corresponding to delta all +1's
      sgn:=1;
      prod:=1;
      for i from 0 to n-1 do(
	  prod=prod*s#i;
	  );
      perm=prod;
      
      curIndex:=0;--which position in the gray code changed
      curVal:=0;--what is the new value in the changed position of the gray code
      for count from 2 to 2^(n-1) do(--start at 2 because we already took the initial product corresponding to delta consisting of all 1's
	  sgn=(-1)*sgn;
	  --increment delta
	  flag:=1;
	  for k from 0 to n-2 when flag==1 do(
	      if del#k==0 then (del#k=1; flag=0; curIndex=k;) else (del#k=0);
	      if curIndex==n-2 then curVal=del#(n-2) else curVal=(del#curIndex)+(del#(curIndex+1));
	      if curVal==2 then curVal=0;
	      );
	  
	  prod = 1;
	  --for every row update the values of s
	  for i from 0 to n-1 do (
	      if curVal==0 then s#i=s#i+2*(E#i#curIndex) else s#i=s#i-2*(E#i#curIndex); 
	  );
          --take the product of all of the row sums
          for i from 0 to n-1 do(
	  	  prod=prod*s#i;
	      );
	      
	  perm = perm+sgn*prod;
	  );

      --need to divide in Glynn's formula
      --so can't be characteristic 2
      perm=perm//2^(n-1);
      perm
)

---------------------------------------------------------------------------
-- grenet
-- On input matrix M, returns a corresponding matrix with determinant
-- equal to the permanent of M.
---------------------------------------------------------------------------
grenet = method()
grenet Matrix := (M) -> ( 

     --get size of the matrix
     --n = length M_rows; --this is really the length of the list consisting of all of the rows of M
     n := numgens target M;
     R := ring(M);
     
     E:=entries M;
     
     m := 2^n-1; -- Size of the new matrix
     N := mutableMatrix(R,m,m); 
     --The last row and column will correspond to the merged source/sink vertex t in Grenet's construction.
     --Entry N_{i,j} will correspond to the weight on the edge from vertex i to vertex j.
     --The rows and columns are ordered by the binary order on the power set of [n] with the combined source/sink vertex being the last position.

     --Make the discrete cube and its powerset
     cube := set splice {0..n-1};
     powerSet := subsets(cube);
     
     emptySet:=set{};
     
     powerSet = subsets(cube); --This is a list given in binary ordering
     powerSet = delete(emptySet,powerSet);
     
     --Start filling the matrix
     
     --the diagonal entries are -1 when n is even and +1 when n is odd
     diag:=1;
     if n%2==0 then diag=-1 else diag=1; 
     --fill the diagonal entries (last diagonal entry is 0 corresponding to source/sink)
     for i from 0 to m-2 do N_(i,i)=diag;
     
     --fill the row labelled by t, i.e. vertices the vertex t points to in the construction
     --get all subsets of the cube of size 1
     currentSets := subsets(cube,1);
     
     for el in currentSets do(
	 place:=((elements(el))#0);
	 --the indices of the columns and rows of N correspond to the binary order of the powerset
	 N_(m-1,2^place-1)=E#0#place;
         );
     
     --fill the column labelled by t, i.e. the vertices that point to t in the construction
     --get all subsets of the cube of size n-1
     currentSets = subsets(cube,n-1);
     
     for el in currentSets do(
	 place:=((elements(cube-el))#0);
	 N_((2^n)-(2^place)-2,m-1)=E#(n-1)#place;
	 );
          
     --i is the row index  
     for i from 0 to m-2 do(
	  current := powerSet#i;
	  index1 := #current;
	  
	  --technically, we could just call subsets(cube,index1+1), 
	  --but then we would have to work out the value of index2 from a given subset in this list
	  --j is the row index
	  for j from i+1 to m-2 do(
	       superSet:=powerSet#j;
	       if isSubset(current,superSet)and#(superSet-current)==1 then (
		    index2:=((elements (superSet-current))#0);
		    N_(i,j)=E#index1#index2;
		    );
	       );
	  );
     
     --fills in 0's in all of the other spaces that were not explicitly defined
     N = matrix N;
     
     --we want to return the matrix resulting from this construction
     N
)

---------------------------------------------------------------------------
-- pminors
-- On input integer k and matrix M, returns ideal generated by k by k permanent
-- minors of M.
---------------------------------------------------------------------------
pminors = method(Options => {Strategy=>null})
pminors (ZZ,Matrix) := opts -> (i,M) -> (
   
    -- M is nxm
    n := numgens target M;
    m := numgens source M;
    R := ring M;
    E := entries M;
     
    if (i>m or i>n) then (I:=ideal(1);) else(
    
    rows := subsets(n,i);
    cols := subsets(m,i);
    
    --List from which the ideal will be created
    L:= {};
    
    for a in rows do(
	for b in cols do(
	    N := submatrix(M,a,b);
	    if (opts.Strategy === glynn or opts.Strategy === null) then (L=append(L,glynn(N));) else (L=append(L,ryser(N));); 
	    );
	);
    
    I=ideal(L);
    );
    I
)

beginDocumentation()

TEST ///
  M=matrix{{1,2,3},{4,5,6},{7,8,9}}
  assert((permanents(3,M))_0 == ryser M)
///

TEST ///
  R = QQ[vars(0..8)]
  M = genericMatrix(R,a,3,3)
  assert((permanents(3,M))_0 == ryser M)
///

TEST ///
   R = QQ[vars(0..15)]
   M = genericMatrix(R,a,4,4)
   assert((permanents(4,M))_0 == ryser M)
///

TEST ///
  M=random(QQ^5,QQ^5)
  assert((permanents(5,M))_0 == ryser M)
///

TEST ///
  R=QQ[x_1..x_3]
  M=random(R^5,R^{5:-1})
  assert((permanents(5,M))_0 == ryser M)
///

TEST ///
  R=ZZ[x_1..x_3]
  M=random(R^5,R^{5:-1})
  assert((permanents(5,M))_0 == ryser M)
///

TEST ///
  M=matrix{{1,2,3},{4,5,6},{7,8,9}}
  assert((permanents(3,M))_0 == glynn M)
///

TEST ///
  R = QQ[vars(0..8)]
  M = genericMatrix(R,a,3,3)
  assert((permanents(3,M))_0 == glynn M)
///

TEST ///
  R = QQ[vars(0..15)]
  M = genericMatrix(R,a,4,4)
  assert((permanents(4,M))_0 == glynn M)
///

TEST ///
  M=random(QQ^5,QQ^5)
  assert((permanents(5,M))_0 == glynn M)
///

TEST ///
  R=QQ[x_1..x_3]
  M=random(R^5,R^{5:-1})
  assert((permanents(5,M))_0 == glynn M)
///

TEST ///
  R=ZZ[x_1..x_3]
  M=random(R^5,R^{5:-1})
  assert((permanents(5,M))_0 == glynn M)
///

TEST ///
  M=matrix{{1,2,3},{4,5,6},{7,8,9}}
  N = grenet M
  assert((permanents(3,M))_0==det N)
///

TEST ///
  R = QQ[vars(0..8)]
  M = genericMatrix(R,a,3,3)
  N = grenet M
  assert((permanents(3,M))_0 == det N)
///

TEST ///
  R=ZZ/32003[a..p]
  M=genericMatrix(R,4,4)
  N = grenet M
  assert((permanents(4,M))_0 == det N)
///

TEST ///
  L=random(QQ^5,QQ^5)
  N=grenet L;
  assert((permanents(5,L))_0 == det N)
///

doc ///
Key
  Permanents
Headline
  Computes the permanent of a square matrix.
Description
  Text
    {\em Permanents} is a package of functions for computing the permanent of a square matrix. 
    
    Computing the permanent is believed to be computationally intractable. In Valiant's theory of algebraic complexity
    the permanent polynomial is complete for the class VNP. Even computing the permanent of 0-1 matrices is #P-complete.
    See Valiant, Leslie G. (1979), "The Complexity of Computing the Permanent," {\em Theoretical Computer Science (Elsevier)} 8 (2): 189-201.
  
    The permanent of a $n\times n$ matrix $(x_{i,j})$ is defined in analogy to the determinant as $\sum_{\sigma \in S_n} \prod x_{i,\sigma(i)}$. 
    There are two other formulas for the permanent polynomial, Ryser's formula and Glynn's formula, both of which have the asymptotically smaller formula size of $O(2^n n^2)$. 
    This can be improved further to {$O(2^n n)$} arithmetic operations with the use of Gray codes, and we do so in this package.
    The connection between the two formulae and the possibility of others is discussed in Glynn, David G. (2013), "Permanent Formulae from the Veronesean." {\em Designs Codes and Cryptography}, 68(1-3) pp. 39-47.
    
    It is conjectured that the permanent polynomial does not have a polynomial size formula. By Valiant's theory, a possible strategy for proving this is to show
    that the permanent of the $n\times n$ generic matrix $N$ cannot be the determinant of a $p(n) \times p(n)$ matrix $M$ with entries affine linear entries of the variables of $M$ where $p(n)$ is a polynomial.
    The best lower bound is quadratic, i.e. the permanent of the $nxn$ generic matrix is not the affine projection of the determinant of a $n^2/2xn^2/2$ matrix.
    See T. Mignon, N. Ressayre. "A Quadratic Bound for the Determinant and Permanent Problem." (2004).
    
    The best known upper bound is $2^n-1$ due to Grenet. More specifically, Grenet constructs a $2^n-1x2^n-1$ matrix $M$ with entries $0,1,-1$ and individual 
    variables of the $nxn$ generic matrix $N$, such that the determinant of $M$ is equal to the permanent of $N$. See B. Grenet, "An Upper Bound for the Permanent versus Determinant Problem" (2012).   
Caveat
  Computationally intensive
SeeAlso
  permanents
  det
///

doc ///
Key
  ryser
  (ryser,Matrix)
Headline
  compute permanent using Ryser's formula
Usage
  F = ryser M
Inputs
  M:Matrix
    a square matrix in any commutative ring
Outputs
  F:RingElement
    the permanent of {\tt M}
Description
  Text
    Uses Ryser's inclusion-exclusion formula (see page 27 of Ryser, Herbert John (1963), Combinatorial Mathematics, The Carus mathematical monographs, The Mathematical Association of America).
    
    Let $M=(m_{i,j})$ be an nxn matrix. Then $perm(M)=\sum_{S\subseteq [n]} (-1)^{\mid S\mid} \prod_{i=1}^n \sum_{j\in S^c}m_{i,j}$.
        
    For example, for the 3x3 generic matrix Ryser’s formula gives $perm(M) = (a + b + c)(d + e + f)(g + h + i) − (a + b)(d + e)(g + h)−(a + c)(d + f)(g + i) − (b + c)(e + f)(h + i) + adg + beh + cfi$.
    
    Here is the permanent of a 3x3 generic matrix of variables.
  Example
    R = QQ[vars(0..8)]
    M = genericMatrix(R,a,3,3)
    ryser M
  Text
    Here is the permanent of a 4x4 generic matrix of variables.
  Example
    R = QQ[vars(0..15)]
    M = genericMatrix(R,a,4,4)
    ryser M
  Text
    Here is the permanent of a 3x3 matrix of integers.
  Example
    M=matrix{{1,2,3},{4,5,6},{7,8,9}}
    ryser M
Caveat
  Computationally intensive for moderate size matrices.
SeeAlso
  permanents
///

doc ///
Key
  glynn
  (glynn,Matrix)
Headline
  compute permanent using Glynn's formula
Usage
  F = glynn M
Inputs
  M:Matrix
    a square matrix in any commutative ring
Outputs
  F:RingElement
    the permanent of {\tt M}
Description
  Text
    Uses Glynn's formula (see Glynn, David G. (2010), "The permanent of a square matrix", European Journal of Combinatorics 31 (7): 1887–1891, doi:10.1016/j.ejc.2010.01.010).
    
    Let $M=(m_{i,j})$ be an nxn matrix. Then $perm(M)=\sum_{\delta}\prod_{k=1}^n(\delta_k) \prod_{i=1}^n\sum_{j=1}^n \delta_j m_{i,j}$ where the outer summation is over $\delta\in \{ -1,+1\}^n$ with $\delta_1=1$.
            
    For example, for the 3x3 generic matrix Glynn’s formula gives $2^2 perm(M) =(a + b + c)(d + e + f)(g + h + i) − (a − b + c)(d − e + f)(g − h + i) −(a + b − c)(d + e − f)(g + h − i) + (a − b − c)(d − e − f)(g − h − i)$.
    
    Here is the permanent of a 3x3 generic matrix of variables.
  Example
    R = QQ[vars(0..8)]
    M = genericMatrix(R,a,3,3)
    glynn M
  Text
    Here is the permanent of a 4x4 generic matrix of variables.
  Example
    R = QQ[vars(0..15)]
    M = genericMatrix(R,a,4,4)
    glynn M
  Text
    Here is the permanent of a 3x3 matrix of integers.
  Example
    M=matrix{{1,2,3},{4,5,6},{7,8,9}}
    glynn M
Caveat
  Works in characteristic not equal to 2 because need to divide by $2^{n-1}$.
  Computationally intensive for moderate size matrices.
SeeAlso
  permanents
///

doc ///
Key
  grenet
  (grenet,Matrix)
Headline
  Construct 2^n-1 by 2^n-1 matrix with determinant equal to the permanent of the input matrix
Usage
  F = grenet M
Inputs
  M:Matrix
    a square matrix in any commutative ring
Outputs
  N:Matrix
    a $2^n-1\times 2^n-1$ matrix {\tt N} with determinant equal to the permanent of {\tt M}
Description
  Text
    Uses Grenet's combinatorial construction (see B. Grenet: An Upper Bound for the Permanent versus Determinant Problem (2012)).
    
    Here is the 7x7 matrix constructed from the 3x3 generic matrix of variables.
  Example
    R = QQ[vars(0..8)]
    M = genericMatrix(R,a,3,3)
    N = grenet M
    det N
  Text  
    Here is the 15x15 matrix constructed from a 4x4 generic matrix of variable (note that the even case has -1 on the diagonal).
  Example
    R=QQ[a..p]
    M=genericMatrix(R,4,4)
    N = grenet M
    det N
  Text
    Here is the construction for a matrix of integers.
  Example
    M = matrix{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}}
    permanents(4,M)
    N = grenet M
    det N
Caveat
  
SeeAlso
  permanents
///

doc ///
Key
  pminors
  (pminors,ZZ,Matrix)
Headline
  Return ideal generated by pminors of a specified size 
Usage
  I = pminors(k,M)
Inputs
  M:Matrix
    a square matrix in any commutative ring
  k:ZZ
    a positive integer
Outputs
  I:Ideal
    the ideal generated by the $k\times k$ permanent minors of $M$
Description
  Text
    Computes the ideal generated by $k\times k$ permanent minors of $M$ with the specified strategy. If no strategy is input then the default is Glynn's formula.
  
    Here is the ideal generated by the $2\times 2$ permanent minors of the 3x3 generic matrix of variables.
  Example
    R = QQ[vars(0..8)]
    M = genericMatrix(R,a,3,3)
    I = pminors(2,M)
  Text
    Here is the ideal generated by the $2\times 2$ permanent minors of a $2\times 3$ matrix using the two different strategies.
  Example
    M=matrix{{1,2,3},{4,5,6}}
    pminors(2,M,Strategy=>glynn)
    pminors(2,M,Strategy=>ryser)
Caveat
  
SeeAlso
  permanents
///

end

restart
loadPackage "Permanents"
installPackage "Permanents"
check oo
viewHelp 

R=ZZ/31[x_1..x_3]
M=random(R^10,R^{10:-1})
time ryser M
time glynn M

M=matrix{{1,2,3},{4,5,6}}
pminors(2,M,Strategy=>glynn)
pminors(2,M,Strategy=>ryser)

R = ZZ/32003[vars(0..400)]
M = genericMatrix(R,a,8,8)
time ryser M;
time glynn M;
assert((permanents(3,M))_0 == glynn M)

for i from 3 to 20 do(print(i);M = genericMatrix(R,a,i,i); time grenet M;)

R=ZZ/32003
M=random(R^6,R^6)
A=pminors(2,M)
B=permanents(2,M)
A==B
A=time ryser M
B=time glynn M
print(A==B)

for i from 21 to 25 do(print(i);M=random(QQ^i,QQ^i); A=time ryser M; B=time glynn M;print(A==B);)

for i from 17 to 20 do(M=random(R^i,R^i); time ryser M; time glynn M;)

kk=ZZ
R=kk[vars(0..8)]
M=random(R^3,R^{3:-1})
time ryser M;
time glynn M;
time det grenet M;

R=ZZ[x_1..x_5];
for i from 2 to 10 do(print(i); M=random(R^i,R^{i:-1}); A=time ryser M;);

kk=ZZ/32003
R=QQ[vars(0..2)]
M=random(R^12,R^{12:-2})
time ryser M;
time glynn M;
time det grenet M;

for i from 7 to 12 do(print(i);M=random(R^i,R^{i:-2}); A=time ryser M; B=time glynn M; print(A==B););

f = method(Options => {Strategy=>null})
f Matrix := opts -> (M) -> (if opts.Strategy === null then det M else ryser M)
f M
f(M, Strategy=>ryser)

---------------------------------------------------------------------------
--Perhaps in the future one could implement the fully-polynomial randomized 
-- approximation scheme of Jerrum, Sinclair, and Vigoda to approximate in
--randomized polynomial time the permanent of a matrix with nonnegative entries to 
--within epsilon*Perm(M) for any desired epsilon.
---------------------------------------------------------------------------
