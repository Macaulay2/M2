--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

getIndex := (R,x) -> (
     M := try monoid R else error "expected a polynomial ring or quotient of one";
     if class x =!= R then error "expected an element of the ring";
     x = try baseName x else error "expected a variable of the ring";
     M.index#x)

genericMatrix = (R,first,nrows,ncols) -> (
     first = getIndex(R,first);
     if not instance(nrows,ZZ) or not instance(ncols,ZZ) or nrows < 0 or ncols < 0
     then error "expected nonnegative integers";
     if first + nrows * ncols > numgens R
     then error "not enough variables in this ring";
     matrix table(nrows, ncols, (i,j)->R_(first + i + nrows*j)))
document { quote genericMatrix,
     TT "genericMatrix(R,x,m,n)", " -- produce an m by n matrix of variables drawn
     from the ring R, starting with variable x.",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "genericMatrix(R,a,2,2)"
     }

genericSkewMatrix = (R,first,n) -> (
     first = getIndex(R,first);
     vars := new MutableHashTable;
     nextvar := first;
     scan(0..n-1, 
	  i -> scan(i+1..n-1, 
	       j -> (vars#(i,j) = R_nextvar; nextvar = nextvar+1)));
     matrix table(n,n,(i,j) -> 
	  if i>j then - vars#(j,i) 
	  else if i<j then vars#(i,j)
	  else 0_R))
document { quote genericSkewMatrix,
     TT "genericSkewMatrix(R,x,n)", " -- make a skew symmetric n by n 
     matrix whose entries above the diagonal are the variables of R, starting 
     with the variable x."
     }
genericSymmetricMatrix = (R,first,n) -> (
     first = getIndex(R,first);
     vars := new MutableHashTable;
     nextvar := first;
     scan(0..n-1, i -> scan(i..n-1, j -> (
		    vars#(i,j) = R_nextvar; 
		    nextvar = nextvar+1)));
     matrix table(n,n, (i,j) -> if i>j then vars#(j,i) else vars#(i,j)))
document { quote genericSymmetricMatrix,
     TT "genericSymmetricMatrix(R,x,n)", " -- make a symmetric n by n matrix 
     whose entries on and above the diagonal are the variables of R, starting 
     with the variable x."
     }

random(List,Ring) := random(ZZ,Ring) := (deg,R) -> (
     m := basis(deg, R) ** R;
     p := char R;
     n := matrix table(numgens source m,1, x -> promote(random p,R));
     (m*n)_(0,0))

random(Module, Module) := (F,G) -> (
     R := ring F;
     p := char R;
     if p === 0 then error "not implemented yet for characteristic 0";
     if R =!= ring G then error "modules over different rings";
     degreesTable := table(degrees F, degrees G, 
	  (i,j) -> toList apply(j,i,difference));
     degreesTally := tally flatten degreesTable;
     if #degreesTally === 0 then map(F,G,0)
     else if #degreesTally === 1 then (
	  deg := first keys degreesTally;
	  if all(deg,i->i===0) 
	  then map(F,G,table(numgens F, numgens G, x -> random p))
	  else (
	       m := basis(deg,R) ** R;
	       s := degreesTally#deg;
	       reshape(F,G, 
		    m * map(source m, R^s, 
			 table(numgens source m, s, x -> random p)))))
     else (
	  randomElement := memoize(
	       deg -> (
		    numused := 0;
		    if deg === 0 then (
			 n := apply(degreesTally#deg, x -> random p);
			 () -> (
			      r := n#numused;
			      numused = numused + 1;
			      r))
		    else (
			 m := basis(deg,R) ** R;
			 k := numgens source m;
			 if k === 0
			 then () -> 0_R
			 else (
			      n = first entries (
				   m * matrix (R, table(
					     k, degreesTally#deg, 
					     (i,j)->random p)));
			      () -> (
				   r := n#numused;
				   numused = numused + 1;
				   r)))));
	  map(F, G, applyTable(degreesTable, k -> (randomElement k)()))))

TEST "
k = ZZ/101
f = random(k^3,k^9)
R = k[a,b,c]
g = random(R^4,R^{-2,-2})
"
