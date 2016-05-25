-- PURPOSE : Computing the Hilbert basis of a Cone 
--   INPUT : 'C',  a Cone
--  OUTPUT : 'L',  a list containing the Hilbert basis as one column matrices 
rules#((set {computedHyperplanes, computedFacets}, set {computedHilbertBasis})) = method(TypicalValue => List)
rules#((set {computedHyperplanes, computedFacets}, set {computedHilbertBasis})) Cone := C -> (
     -- Computing the row echolon form of the matrix M
     ref := M -> (
        n := numColumns M;
        s := numRows M;
        BC := map(ZZ^n,ZZ^n,1);
        m := min(n,s);
        -- Scan through the first square part of 'M'
        i := 0;
        stopper := 0;
        while i < m and stopper < n do (
             -- Selecting the first non-zero entry after the i-th row in the i-th column
             j := select(1,toList(i..s-1),k -> M_i_k != 0);
             -- if there is a non-zero entry, scan the remaining entries and compute the reduced form for this column
             if j != {} then (
             j = j#0;
             scan((j+1)..(s-1), k -> (
                  if M_i_k != 0 then (
                  a := M_i_j;
                  b := M_i_k;
                  L := gcdCoefficients(a,b);
                  a = substitute(a/(L#0),ZZ);
                  b = substitute(b/(L#0),ZZ);
                  M = M^{0..j-1} || (L#1)*M^{j} + (L#2)*M^{k} || M^{j+1..k-1} || (-b)*M^{j} + a*M^{k} || M^{k+1..s-1})));
             if i != j then (
                  M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1});
             if M_i_i < 0 then M = M^{0..i-1} || -M^{i} || M^{i+1..s-1})
             else (
             M = M_{0..i-1} | M_{i+1..n-1} | M_{i};
             BC = BC_{0..i-1} | BC_{i+1..n-1} | BC_{i};
             i = i-1);
             i = i+1;
             stopper = stopper + 1);
        (M,BC)
     );
     -- Function to compute the/one preimage of h under A
     preim := (h,A) -> (
        -- Take the generators of the kernel of '-h|A' and find an element with 1 as first entry -> the other entrys are a preimage
        -- vector
        N := gens ker(-h|A);
        N = transpose (ref transpose N)#0;
        N_{0}^{1..(numRows N)-1}
     );
     A := halfspaces(C);
     if hyperplanes(C) != 0 then A = A || hyperplanes(C) || -(hyperplanes(C));
     A = substitute(A,ZZ);
     -- Use the project and lift algorithm to compute a basis of the space of vectors positive on 'A' whose preimages are the HilbertBasis
     (B,BC) := ref transpose A; 
     H := constructHilbertBasis B;
     BC = inverse transpose BC;
     C.cache#computedHilbertBasis = apply(H,h -> preim(BC*h,A))
)


-- PURPOSE : Computing the Hilbert basis of a standardised cone (project and lift algorithm)
--   INPUT : 'A' a matrix, the row echolon form of the defining half-spaces of the cone
--  OUTPUT : a list of one column matrices, the generators of the cone over A intersected with 
--     	     the positive orthant
constructHilbertBasis = A -> (
    -- Defining the function to determine if u is lower v
    lowvec := (u,v) -> (
	 n := (numRows u)-1;
	 diffvec := flatten entries(u-v);
	 if all(diffvec, i -> i <= 0) then abs(u_(n,0)) <= abs(v_(n,0)) and (u_(n,0))*(v_(n,0)) >= 0
	 else false);
    -- Collecting data
    A = substitute(A,ZZ);
    H := {A^{0}_{0}};
    s := numRows A;
    n := numColumns A;
    --doing the project and lift algorithm step by step with increasing dimensions
    scan(n-1, i -> (
	      -- the set 'F' will contain the lifted basis vectors, 'B' are the first i+2 columns of 'A' as a rowmatrix,
	      -- the set 'H' contains the vectors from the last loop that are one dimension smaller
	      F := {};
	      B := transpose A_{0..(i+1)};
	      -- Decide between lifting the existing vectors (i > s-1) or also adding the next column of 'B'
	      if i < s-1 then (
		   -- Lifting the existing vectors from 'H'
		   F = apply(H, h -> (
			     j := 0;
			     while numRows h == i+1 do (
				  if isSubset(image(h || matrix{{j}}), image B) then h = (h || matrix{{j}});
				  j = j+1);
			     h));
		   -- Adding +- times the next column of 'B'
		   F = join(F,{B_{i+1}^{0..(i+1)},-B_{i+1}^{0..(i+1)}}))
	      else (
		   -- Lifting the existing vectors from 'H'
		   nullmap := map(ZZ^1,ZZ^s,0);
		   nullvec := map(ZZ^1,ZZ^1,0);
		   F = apply(H, h -> B*substitute(vertices intersection(nullmap,nullvec,B^{0..i},h),ZZ)));
	      -- Computing the S-pairs from the elements of 'F' and saving them in 'C'
	      C := select(subsets(#F,2), j -> (
			f := F#(j#0);
			g := F#(j#1);
			(f_(i+1,0))*(g_(i+1,0)) < 0 and f+g != 0*(f+g)));
	      C = apply(C, j -> F#(j#0)+F#(j#1));
	      -- The elements of 'F' are saved in 'G'
	      G := F;
	      j := 0;
	      -- Adding those elements of 'C' to 'G' that satisfy the "normalform" condition by increasing last entry
	      while C != {} do (
		   Cnow := partition(e -> sum drop(flatten entries e,-1) == j,C);
		   C = if Cnow#?false then Cnow#false else {};
		   Cnow = if Cnow#?true then select(Cnow#true, f -> all(G, g -> not lowvec(g,f))) else {};
		   Cnew := flatten apply(Cnow, f -> apply(select(G, g -> f_(i+1,0)*g_(i+1,0) < 0 and f+g != 0*(f+g)), g -> f+g));
		   if all(Cnew, e -> sum drop(flatten entries e,-1) != j) then j = j+1;
		   C = unique (C | Cnew);
		   G = unique (G | Cnow));
	      -- saving those elements of 'G' with positive last entry into 'H'
	      H = select(G, g -> g_(i+1,0) >= 0)));
    H)



