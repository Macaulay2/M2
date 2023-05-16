

-- PURPOSE : Computing the affine image of a polyhedron
affineImage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,v)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space and 'v' is a matrix
--     	    	      	 defining a vector in the target space of 'A'
--  OUTPUT : a polyhedron, the affine image of 'P':
--                       A*P+v={A*p+v | p in P}
affineImage(Matrix,Polyhedron,Matrix) := (A,P,v) -> (
   -- Checking for input errors
   A = chkZZQQ(A,"linear map");
   v = chkZZQQ(v,"translation vector");
   if ambDim(P) =!= numColumns A then error("Matrix source must be ambient space");
   if numRows A =!= numRows v then error("Vector must lie in target space of matrix");
   if numColumns v =!= 1 then error("Second argument must be a vector");
   -- Generating nr of vertices many copies of v
   v = v * (matrix {toList((numColumns vertices P):1_QQ)});
   Mv := A*(vertices P) + v;
   Mr := A*(rays P);
   Ml := A*(linealitySpace P);
   convexHull(Mv, Mr, Ml)
)


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space
--  OUTPUT : A Polyhedron, the image of 'P' under 'A'
affineImage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     A = chkZZQQ(A,"map");
     v := map(target A,QQ^1,0);
     affineImage(A,P,v))


--   INPUT : '(P,v)',  where 'v' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : A Polyhedron, the translation of 'P' by 'v', i.e. {p+v | p in P} 
affineImage(Polyhedron,Matrix) := (P,v) -> (
     -- Generating the identity matrix
     A := map(QQ^(ambDim(P)),QQ^(ambDim(P)),1);
     affineImage(A,P,v))


--   INPUT : '(M,C,v)',  where 'M' is a ZZ or QQ matrix from the ambient space of 
--     	    	      	 the cone 'C' to some target space and 'v' is a matrix
--     	    	      	 defining a vector in that target space
--  OUTPUT : A polyhedron, the affine image of 'C':
--                       (M*C)+v={(M*c)+v | c in C}
affineImage(Matrix,Cone,Matrix) := (M,C,v) -> if v == 0 then affineImage(M,C) else affineImage(M, polyhedron C,v)


--   INPUT : '(M,C)',  where 'M' is a ZZ or QQ matrix from the 
--     	    	      	 ambient space of the cone 'C' to some target space
--  OUTPUT : A cone, the affine image of 'C':
--                       M*C={M*c | c in C}
affineImage(Matrix,Cone) := (M,C) -> coneFromVData affineImage(M, polyhedron C)


--   INPUT : '(C,v)',  where 'C' is a cone and 'v' is a matrix
--     	    	      	 defining a vector in the ambient space of 'C'
--  OUTPUT : A polyhedron, the affine image of 'C':
--                       C+v={c+v | c in C}
affineImage(Cone,Matrix) := (C,v) -> affineImage(polyhedron C,v)


-- PURPOSE : Computing the affine preimage of a cone or polyhedron
affinePreimage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the polyhedron 'P' and 'b' is a matrix
--     	    	      	 defining a vector in the ambient space of 'P'
--  OUTPUT : A polyhedron, the affine preimage of 'P':
--                       {q | (A*q)+b in P}
affinePreimage(Matrix,Polyhedron,Matrix) := (A,P,b) -> (
     -- Checking for input errors
     A = chkZZQQ(A,"linear map");
     b = chkZZQQ(b,"translation vector");
     if ambDim(P) =!= numRows A then error("Matrix source must be ambient space");
     if numRows A =!= numRows b then error("Vector must lie in target space of matrix");
     if numColumns b =!= 1 then error("Second argument must be a vector");
     -- Constructing the new half-spaces and hyperplanes
     (M,v) := halfspaces P;
     (N,w) := hyperplanes P;
     v = v - (M * b);
     w = w - (N * b);
     M = M * A;
     N = N * A;
     polyhedronFromHData(M,v,N,w))


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	       ambient space of the polyhedron 'P' 
affinePreimage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     A = chkZZQQ(A,"map");
     affinePreimage(A,P,map(target A,QQ^1,0)))


--   INPUT : '(P,b)',  where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : A Polyhedron, the negative translation of 'P' by 'b', i.e. {q | q+b in P} 
affinePreimage(Polyhedron,Matrix) := (P,b) -> affinePreimage(map(QQ^(ambDim(P)),QQ^(ambDim(P)),1),P,b)


--   INPUT : '(A,C,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C' and 'b' is a matrix
--     	    	      	 defining a vector in the ambient space of 'C'
--  OUTPUT : A polyhedron, the affine preimage of 'C':
--                       {q | (A*q)+b in C}
--     	     or a cone, the affine preimage of 'C' if 'b' is 0:
--     	    	         {q | (A*q) in C}
affinePreimage(Matrix,Cone,Matrix) := (A,C,b) -> if b == 0 then affinePreimage(A,C) else affinePreimage(A,polyhedron C,b)


--   INPUT : '(A,C)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C'
--  OUTPUT : A cone, the affine preimage of 'C':
--                       {q | (A*q) in C}
affinePreimage(Matrix,Cone) := (A,C) -> coneFromVData affinePreimage(A,polyhedron C)


--   INPUT : '(C,b)',   where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the cone 'C'
--  OUTPUT : A polyhedron, the affine preimage of 'C':
--                       {q | q+b in C}
affinePreimage(Cone,Matrix) := (C,b) -> affinePreimage(polyhedron C,b)

linearTransform = method()
linearTransform(Fan, Matrix) := (F, A) -> (
   newRays := rays F;
   newRays = A * newRays;
   newLineality := linealitySpace F;
   check := kernel A;
   check = newLineality | (gens check);
   if(rank check != rank newLineality) then << "Warning: Output fan may not be well defined. Check with 'isWellDefined'" << endl;
   newLineality = A * newLineality;
   newLineality = mingens image newLineality;
   goodNewRays := makeRaysUniqueAndPrimitive(newRays, newLineality);
   result := new HashTable from {
      rays => newRays,
      computedLinealityBasis => newLineality,
      generatingObjects => maxCones F
   };
   internalFanConstructor result
)
