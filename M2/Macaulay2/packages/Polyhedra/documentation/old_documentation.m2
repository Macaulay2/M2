document {
     Key => "Working with fans",
     
     "We start by constructing a fan, which consists of a single cone and all of its 
     faces:",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " F = fan C"
	  },
     
     PARA{}, "By this, we have already constructed the fan consisting of the 
     positive orthant and all of its faces. The package saves the generating cones 
     of the fan, which can be accessed by:",
     
     EXAMPLE {
	  "maxCones F"
	  },
     
     PARA{}, "Now we could expand the fan by adding more cones, for example the following:",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0,0},{1,1,0},{0,0,-1}}"
	  },
     
     PARA{}, "But in this case we can not, because the two cones are not compatible, 
     i.e. their intersection is not a face of each. So, when one tries to add a cone 
     to a fan that is not compatible with one of the generating cones of the fan, the 
     function ",TO addCone," gives an error. For two cones one can 
     check if their intersection is a common face by using ",TO commonFace,":",
     
     EXAMPLE {
	  " commonFace(C,C1)"
	  },
     
     PARA{}, "Since the intersection of both is already computed in this function 
     there is a different function, which also returns the intersection, to save 
     computation time when one needs the intersection afterward anyway:",
     
     EXAMPLE {
	  " (b,C2) = areCompatible(C,C1)",
	  " rays C2"
	  },
     
     PARA{}, "So we can make the cone compatible and add it to the fan.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,-1}}",
	  " F = addCone(C1,F)"
	  },
     
     PARA{}, "Instead of creating a fan with one cone and then adding more cones, we 
     can also make a fan out of a list of cones:",
     
     EXAMPLE {
	  " C2 = coneFromVData matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " C3 = coneFromVData matrix {{-1,0,0},{0,1,0},{0,0,-1}};",
	  " C4 = coneFromVData matrix {{-1,0,0},{0,-1,0},{0,0,1}};",
	  " C5 = coneFromVData matrix {{-1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = fan {C2,C3,C4,C5}"
	  },
     
     PARA{}, "Furthermore, we could add a list of cones to an existing fan:",
     
     EXAMPLE {
	  " C6 = coneFromVData matrix {{1,0,0},{0,-1,0},{0,0,1}};",
	  " C7 = coneFromVData matrix {{1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = addCone( {C6,C7}, F1)",
	  },
     
     PARA{}, "So, ",TO fan," and ",TO addCone," are the methods to construct 
     fans ''from scratch'', but there are also methods to get fans directly, for example ",TO normalFan,", 
     which constructs the inner normal fan of a polytope.",
     
     EXAMPLE {
	  " P = hypercube 4",
	  " F2 = normalFan P"
	  },
     
     PARA{}, "Now we have seen how to construct fans, so we turn to functions on fans, 
     for example the direct product (",TO (directProduct, Cone, Cone), "):",
     
     EXAMPLE {
	  " F3 = fan {coneFromVData matrix {{1}},coneFromVData matrix {{-1}}}",
	  " F1 = F3 * F1"},
     
     PARA{}, "The result is in the direct product of the ambient spaces.",
     
     EXAMPLE {
	  " ambDim F1"
	  },
     
     PARA{}, "Of course, we can check if two fans are the same:",
     
     EXAMPLE {
	  " F1 == F2"
	  },
     
     PARA{}, "A bit more on fans can be found in part 2: ",TO "Working with fans - Part 2","."
     
     }

document {
     Key => "Working with fans - Part 2",
     
     "Now we construct a new fan to show some other functions.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};",
	  " C2 = coneFromVData matrix {{1,1,1},{0,1,-1},{-1,1,1}};",
	  " C3 = coneFromVData matrix {{-1,-1,-1},{0,1,-1},{-1,1,1}};",
	  " C4 = coneFromVData matrix {{1,-1},{0,0},{-1,-1}};",
	  " F = fan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "This is not a ''very nice'' fan, as it is neither complete nor 
     of pure dimension:",
     
     EXAMPLE {
	  " isComplete F",
	  " isPure F"
	  },
     
     PARA{}, "If we add two more cones the fan becomes complete.",
     
     EXAMPLE {
	  " C5 = coneFromVData matrix {{1,-1,1,-1},{-1,-1,0,0},{1,1,-1,-1}};",
	  " C6 = coneFromVData matrix {{1,-1,1,-1},{1,1,0,0},{1,1,-1,-1}};",
	  " F = addCone({C5,C6},F)",
	  " isComplete F"
	  },
     
     PARA{}, "For a complete fan we can check if it is projective:",
     
     EXAMPLE {
	  " isPolytopal F"
	  },
     
     PARA{}, "If the fan is projective, the function returns a polyhedron such that  
     the fan is its  normal fan, otherwise it returns the empty polyhedron. This means 
     our fan is projective."
     
     }

document {
     Key => PolyhedralObject,
     Headline => "the class of all polyhedral objects in Polyhedra",
          
     TT "PolyhedralObject"," is the parent class of the four polyhedral objects in Polyhedra:",
     
     UL {
	  {TO "Cone"},
	  {TO "Polyhedron"},
	  {TO "Fan"},
	  {TO "PolyhedralComplex"}
	},
   
     EXAMPLE {
	  " coneFromVData matrix {{1,2},{2,1}}",
	  " convexHull matrix {{1,1,0,0},{1,0,1,0}}",
	  " hirzebruch 3",
	  " polyhedralComplex crossPolytope 3",
	  }
        
     }
        
document {     
     Key => Polyhedron,
     Headline => "the class of all convex polyhedra",
          
     "A Polyhedron represents a rational polyhedron. It can be bounded or unbounded, 
     need not be full dimensional or may contain a proper affine subspace. It can 
     be empty or zero dimensional. It is saved as a hash table which contains 
     the vertices, generating rays, and the basis of the lineality space of the 
     Polyhedron as well as the defining affine half-spaces and hyperplanes. The 
     output of a Polyhedron looks like this:",
     
     EXAMPLE {
	  " convexHull(matrix {{0,0,-1,-1},{2,-2,1,-1},{0,0,0,0}},matrix {{1},{0},{0}})",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Polyhedron. 
     Note that the number of rays and vertices are modulo the lineality space. So 
     for example a line in QQ^2 has one vertex and no rays. However, one can not
     access the above information directly, because this is just a virtual hash table 
     generated for the output. The data defining a Polyhedron is extracted 
     by the functions included in this package. A Polyhedron can be constructed as 
     the convex hull (",TO convexHull,") of a set of points and a set of rays or as the 
     intersection (",TO polyhedronFromHData,") of a set of affine half-spaces and affine hyperplanes.",
     
     PARA{}, "For example, consider the square and the square with an emerging ray 
     for the convex hull:",
     
     EXAMPLE {
	  " V = matrix {{1,1,-1,-1},{1,-1,1,-1}}",
	  " convexHull V",
	  " R = matrix {{1},{1}}",
	  " convexHull(V,R)"
	  },
     
     PARA{}, "If we take the intersection of the half-spaces defined by the directions of the 
     vertices and 1 we get the crosspolytope:",
     
     EXAMPLE {
	  " HS = transpose V",
	  " v = R || R",
	  " P = polyhedronFromHData(HS,v)",
	  " vertices P"
	  },
     
     PARA{}, "This can for example be embedded in 3-space on height 1:",
     
     EXAMPLE {
	  " HS = HS | matrix {{0},{0},{0},{0}}",
	  " hyperplanesTmp = matrix {{0,0,1}}",
	  " w = matrix {{1}}",
	  " P = polyhedronFromHData(HS,v,hyperplanesTmp,w)",
	  " vertices P"
	  },
     
     PARA{}, "See also ",TO "Working with polyhedra","."
     
     }

document {     
     Key => Cone,
     Headline => "the class of all rational convex polyhedral cones",
     
     "A Cone represents a rational convex polyhedral cone. It need not be full 
     dimensional or may contain a proper linear subspace. It can be zero 
     dimensional, i.e. the origin. It is saved as a hash table which 
     contains the generating rays and the basis of the lineality space of the 
     cone as well as the defining half-spaces and hyperplanes. The output of a 
     Cone looks like this:",
     
     EXAMPLE {
	  " coneFromVData matrix {{0,0,-1,-1,1},{2,-2,1,-1,0},{1,1,1,1,0}}",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Cone. The number 
     of rays is modulo the lineality space. However, one can not access the above information 
     directly, because this is just a virtual hash table generated for the output. The data 
     describing a Cone is extracted by the functions included in this package. A Cone 
     can be constructed as the positive hull (",TO coneFromVData,")of a set of rays or as the intersection 
     (",TO coneFromHData,") of a set of linear half-spaces and linear hyperplanes.",
     
     PARA{}, "As examples for the positive hull consider the following cones:",
     
     EXAMPLE {
	  " R = matrix{{1,2,3,1},{2,3,1,1},{3,1,2,1}}",
	  " C = coneFromVData R",
	  " rays C",
	  " LS = matrix{{1},{1},{-2}}",
	  " C = coneFromVData(R,LS)",
	  " rays C"
	  },
     
     PARA{}, "On the other hand, we can use these matrices as defining half-spaces and hyperplanes 
     for the intersection:",
     
     EXAMPLE {
	  " HS = transpose R",
	  " C = coneFromHData HS",
	  " rays C",
	  " hyperplanesTmp = transpose LS",
	  " C = coneFromHData(HS,hyperplanesTmp)",
	  " rays C"
	  },
     
     PARA{}, "See also",TO "Working with cones","."
     
     }

document {     
     Key => Fan,
     Headline => "the class of all fans",
     
     "A Fan represents a fan of rational convex polyhedral cones, i.e. a collection of cones, 
     such that for every cone in the fan all faces are in the fan and for every two cones in 
     the fan their intersection is a face of each (intersection condition). 
     It need not be full dimensional or pure, and the cones need not be pointed. It is saved 
     as a hash table which contains a list of the generating cones of the fan starting 
     with those of maximal dimension. So for every cone in this list all faces are considered 
     to be in the fan. The output of a Fan looks like this:",
     
     EXAMPLE {
	  " normalFan crossPolytope 3",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Fan. 
     However, one can not access the above information directly, because this 
     is just a virtual hash table generated for the output. The data defining a Fan 
     is extracted by the functions included in this package. A Fan can be constructed by 
     collecting Cones that satisfy the intersection condition. Every cone that is added to 
     a Fan is always considered as the collection of the Cone and all of its faces.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{2,2},{1,-1}};",
	  " C2 = coneFromVData matrix {{2,-2},{1,1}};",
	  " C3 = coneFromVData matrix {{-2,-2},{1,-1}};",
	  " C4 = coneFromVData matrix {{-2,2},{-1,-1}};",
	  " F = fan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "This fan is for example the normal fan of a ''flattened'' crosspolytope in 2-space.",
     
     PARA{}, " See also ",TO "Working with fans","."
     
     }

document {     
     Key => PolyhedralComplex,
     Headline => "the class of all polyhedral complexes",
     
     "A PolyhedralComplex represents a complex of rational convex polyhedra, i.e. a collection of polyhedra, 
     such that for every polyhedron in the complex all faces are in the complex and for every two polyhedra in 
     the complex their intersection is a face of each (intersection condition). 
     It need not be full dimensional or pure, and the polyhedra need not be compact. It is saved 
     as a hash table which contains a list of the generating polyhedra of the complex starting 
     with those of maximal dimension. So for every polyhedron in this list all faces are considered 
     to be in the complex. The output of a PolyhedralComplex looks like this:",
     
     EXAMPLE {
	  " polyhedralComplex crossPolytope 3",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the PolyhedralComplex. 
     However, one can not access the above information directly, because this 
     is just a virtual hash table generated for the output. The data defining a PolyhedralComplex 
     is extracted by the functions included in this package. A PolyhedralComplex can be constructed by 
     collecting Polyhedra that satisfy the intersection condition. Every polyhedron that is added to 
     a PolyhedralComplex is always considered as the collection of the Polyhedron and all of its faces.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{2,2,0},{1,-1,0}};",
	  " P2 = convexHull matrix {{2,-2,0},{1,1,0}};",
	  " P3 = convexHull matrix {{-2,-2,0},{1,-1,0}};",
	  " P4 = convexHull matrix {{-2,2,0},{-1,-1,0}};",
	  " F = polyhedralComplex {P1,P2,P3,P4}"
	  }
     
     }

document {
     Key => {convexHull, (convexHull,Matrix), (convexHull,Matrix,Matrix), 
	  (convexHull,Polyhedron,Polyhedron), (convexHull,List), (convexHull,Matrix,Matrix,Matrix)},
     Headline => "computing the convex hull of points, rays and polyhedra",
     Usage => " P = convexHull M \nP = convexHull(M,N) \nP = convexHull(P1,P2) \nP = convexHull L \nP = convexHull(M,N,W)",
     Inputs => {
	  "M" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "N" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "W" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "P1" => Polyhedron,
	  "P2" => Polyhedron,
	  "L" => List
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     PARA{}, TT "convexHull", " computes the convex hull of the input. 
     In the first two cases it considers the columns of ", TT "M", " 
     as a set of points and the columns of ", TT "N", " (if given) as 
     a set of rays and computes the polyhedron that is the convex hull 
     of the points plus the rays. The two matrices must have the same 
     number of rows, i.e. the columns must lie in the same ambient space. 
     If ", TT "N", " is not given or equal to 0, then the resulting 
     polyhedron is compact and hence a polytope. The points need not 
     be the vertices of the polyhedron. In the third case it computes 
     the convex hull of ", TT "P1", " and ", TT "P2", " if they lie 
     in the same ambient space. Finally, it computes the convex hull 
     of a list ", TT "L"," where the list may contain a combination 
     of the following in any order.",
     
     UL {
	  {"Points, given by a matrix ", TT "M", " over ", TO ZZ, " 
	       or ", TO QQ},
	  {"Points and ray generators, given by a sequence ", TT "(V,R)", "of two 
	       matrices over ", TO ZZ, " or ", TO QQ},
	  {"Points, ray generators and lineality generators, given by a sequence ", TT "(M,N,W)", "of three 
	       matrices over ", TO ZZ, " or ", TO QQ},
	  {TO Cone},
	  {TO Polyhedron}
	},
     
     PARA{}, "Then ", TT "convexHull", " computes the convex hull of all 
     inserted objects, if they are in the same ambient space, i.e. all matrices 
     must have the same number of rows, which must equal the ambient dimension 
     of all cones and polyhedra.",
     
     PARA{}, "For example, consider the square in ",TO QQ,"^2:",
     
     EXAMPLE {
	  " M = matrix {{1,1,-1,-1},{1,-1,1,-1}}",
	  " P = convexHull M"
	  },
     
     PARA{}, "If we add a ray, then it is not compact anymore:",
     
     EXAMPLE {
	  " r = matrix {{1},{2}}",
	  " P =convexHull(M,r)"
	  },
     
     PARA{}, "If we add some more vertices to ",TT "M"," then we get a hexagon:",
     
     EXAMPLE {
	  " N = matrix {{-2,-2,0},{0,-2,-2}}",
	  " Q = convexHull(M|N)"
	  },
     
     PARA{}, "Again if we add the ray ",TT "r"," then the polyhedron is not compact:",
     
     EXAMPLE {
	  " Q1 = convexHull(M|N,r)"
	  },
     
     PARA{}, "To get this polyhedron we could also have used the application of ",TT "convexHull"," 
     to lists or pairs of polyhedra:",
     
     EXAMPLE {
	  " P1 = convexHull {P,N}",
	  " P1 == Q1",
	  " P1 = convexHull(P,Q)",
	  " P1 == Q1"
	  }
     }

document {
     Key => {coneFromVData, (coneFromVData,Cone,Cone), (coneFromVData,Matrix), (coneFromVData,Matrix,Matrix), (coneFromVData,Polyhedron), 
	  (coneFromVData,List)},
     Headline => "computes the positive hull of rays, cones, and the cone over a polyhedron",
     Usage => " C = coneFromVData M \nC = coneFromVData(M,N) \nC = coneFromVData(C1,C2) \nC = coneFromVData P \nC = coneFromVData L",
     Inputs => {
	  "M" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "N" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "C1" => Cone,
	  "C2" => Cone,
	  "P" => Polyhedron,
	  "L" => List 
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     PARA{}, TT "coneFromVData", " computes the positive hull of the input. In the 
     first two cases it considers the columns of ", TT "M", " as a set of rays 
     and the columns of ", TT "N", " (if given) as generators of the lineality 
     space and computes the cone that is the positive hull of the rays plus 
     the lineality space. The two matrices must have the same number of rows, 
     i.e. the columns must lie in the same ambient space. If ", TT "N", " is 
     not given or equal to 0 then the resulting cone is pointed. The rays need 
     not be a minimal generating set of the cone. If two cones are inserted it 
     computes their positive hull if they lie in the same ambient space. In the 
     case of a polyhedron it computes the cone given by all positive multiples 
     of points of the polyhedron. If applied to a list, it may contain a 
     combination of the following in any order.",
     
     UL {
	  {"Rays, given by a matrix ", TT "R", " over ", TO ZZ, " 
	       or ", TO QQ},
	  {"Rays and a lineality space, given by a sequence ", TT "(R,LS)", " of two 
	       matrices over ", TO ZZ, " or ", TO QQ},
	  {TO Cone},
	  {TO Polyhedron}
	},
     
     PARA{}, "Then ", TT "coneFromVData", " computes the positive hull of all 
     inserted objects, if they are in the same ambient space, i.e. all matrices 
     must have the same number of rows, which must equal the ambient dimension 
     of all cones and polyhedra.",
     
     PARA{}, "As a first example consider the following 2 dimensional cone in 3 space:",
     
     EXAMPLE {
	  " R = matrix {{1,2},{2,1},{0,0}}",
	  " C = coneFromVData R"
	  },
     
     PARA{}, "We can construct a full dimensional cone out of this one by adding a lineality 
     space for example:",
     
     EXAMPLE {
	  " LS = matrix {{0},{0},{1}}",
	  " C1 = coneFromVData (R,LS)"
	  },
     
     PARA{}, "The resulting cone is not pointed anymore, because it contains the subspace spanned 
     by (0,0,1). To get a full dimensional pointed cone we have to add another ray to C. For 
     this we can apply ",TT "coneFromVData"," to a list containing ",TT "C"," and the new ray:",
     
     EXAMPLE {
	  " r = matrix {{0},{1},{2}}",
	  " C2 = coneFromVData {C,r}"
	  },
     
     PARA{}, "Another way would be, if we would have ",TT "r"," not as a ray but already as 
     a cone:",
     
     EXAMPLE {
	  " r = coneFromVData r"
	  },
     
     PARA{}, "In this case we can just take the positive hull of the two cones:",
     
     EXAMPLE {
	  " C3 = coneFromVData(C,r)",
	  " C3 == C2"
	  }
     
     }

document {
     Key => {intersection, (intersection,Cone,Cone), (intersection,List), 
	  (intersection,Polyhedron,Polyhedron),
	  (intersection,Cone,Polyhedron), (intersection,Polyhedron,Cone)},
     Headline => "computes the intersection of cones, and polyhedra",
     Usage => " P = intersection L\nC = intersection(C1,C2) \nP = intersection(P1,P2)",
     Inputs => {
	  "L" => List => {"containing any of the inputs below"},
	  "C1" => Cone,
	  "C2" => Cone,
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     
     
     PARA{}, "If two polyhedra or two cones are inserted, then the 
     intersection of both arguments is computed if both arguments lie in 
     the same ambient space. If both arguments are cones then the output 
     is again a cone. Otherwise intersection returns a polyhedron.",
     
     PARA{}, "If ", TT "intersection", " is called for a list ", TT "L", ", 
     then the list may contain a combination of the following in any order.",
     UL {
	  {TO Cone},
	  {TO Polyhedron}
	},
   
     
     EXAMPLE {
     " C = hypercube 2",
     " S = simplex 2",
     " CS = intersection(C,S)"
	  }     

     }

document {
     Key => {fan, (fan,Cone), (fan,List)},
     Headline => "generates a Fan",
     Usage => " F = fan C \nF = fan L",
     Inputs => {
	  "C" => Cone,
	  "L" => List => {"with elements of class ", TO Cone, " or ", TO Fan}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, " If ",TT "fan", " is applied to a ", TO Cone, " it generates 
     the ", TO Fan, " given by the Cone and all of its faces. If applied to 
     a ", TO List, " the list must only contain Cones and Fans in the same 
     ambient space. Then it adds the Cones in the List and the generating Cones 
     of the Fans in the List one by one to the Fan, checking each time if the 
     new Cone is compatible with the cones that have already been added, i.e. 
     that the intersection with each of them is a face of both Cones 
     (intersection condition).",
     
     PARA{}, "If one of the cones is in the wrong ambient space, there will be an 
     error and no fan will be returned. If the intersection condition fails, there 
     will also be an error. The pairs of incompatible cones can be accessed with the 
     function ",TO incompCones,".",
          
     EXAMPLE {
	  " C = coneFromVData matrix {{1,-1},{0,-1}}",
	  " F = fan C",
	  " C1 = coneFromVData matrix {{1,0},{0,1}};",
	  " C2 = coneFromVData matrix {{0,-1},{1,-1}};",
	  " F = fan {C,C1,C2}"
	  }
     
     }

document {
     Key => {polyhedralComplex, (polyhedralComplex,Polyhedron), (polyhedralComplex,List)},
     Headline => "generates a PolyhedralComplex",
     Usage => " PC = polyhedralComplex P \nPC = polyhedralComplex L",
     Inputs => {
	  "P" => Polyhedron,
	  "L" => List => {"with elements of class ", TO Polyhedron, " or ", TO PolyhedralComplex}
	  },
     Outputs => {
	  "F" => PolyhedralComplex
	  },
     
     PARA{}, " If ",TT "polyhedralComplex", " is applied to a ", TO Polyhedron, " it generates 
     the ", TO PolyhedralComplex, " given by the Polyhedron and all of its faces. If applied to 
     a ", TO List, " the list must only contain Polyhedra and PolyhedralComplexes in the same 
     ambient space. Then it adds the Polyhedra in the List and the generating Polyhedra 
     of the PolyhedralComplexes in the List one by one to the new PolyhedralComplex, checking each time if the 
     new Polyhedron is compatible with the polyhedra that have already been added, i.e. 
     that the intersection with each of them is a face of both Polyhedra 
     (intersection condition).",
     
     PARA{}, "If one of the polyhedra is in the wrong ambient space (i.e. not the ambient space of the first object in 
     the list), then there will be an error and no PolyhedralComplex will be returned. If the intersection condition fails, there 
     will also be an error. The pairs of incompatible polyhedra can be accessed with the 
     function ",TO incompPolyhedra,".",
          
     EXAMPLE {
	  " P = convexHull matrix {{1,-1,0},{0,-1,0}}",
	  " PC = polyhedralComplex P",
	  " P1 = convexHull matrix {{1,0,0},{0,1,0}};",
	  " P2 = convexHull matrix {{0,-1,0},{1,-1,0}};",
	  " PC = polyhedralComplex {PC,P1,P2}"
	  }
     
     }

document {
     Key => {addCone, (addCone,Cone,Fan), (addCone,List,Fan)},
     Headline => "adds cones to a Fan",
     Usage => " F1 = addCone(C,F) \nF1 = addCone(L,F)",
     Inputs => {
	  "C" => Cone,
	  "L" => List => {"with elements of class ", TO Cone, " or ", TO Fan},
	  "F" => Fan
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, "If ",TT "addCone", " is applied to a ", TO Cone, " and a ",TO Fan, " 
     it adds the Cone to the Fan if they are in the same ambient space, if the Cone is 
     compatible with every generating Cone of ",TT "F", ", but is not a face of one 
     of them. If one of the first two conditions fails, there will be an error and no fan 
     will be returned. The pairs of incompatible cones can be accessed with the 
     function ",TO incompCones,". If the last condition fails, then the cone is already in 
     the fan as a face of one of the cones, so it does not have to be added. The conditions 
     are checked in this order.",
     
     PARA{}, "If ",TT "addCone"," is applied to a ",TO List," and a ",TO Fan,", then 
     the function adds the list cone by cone and stops if one of the three conditions 
     fails for one of the cones. There is again an error for the first two conditions. The 
     pairs of incompatible cones can again be retrieved using ",TO incompCones,".",
     
     PARA{}, "If applied to a pair of fans it adds the generating cones of the first 
     fan to the second fan, again checking for the same conditions as above.",
     
     PARA{}, " As an example, we make a fan consisting of the following cone and 
     try to add an adjacent orthant.",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,0,0},{0,1,1},{0,0,1}};",
	  " F = fan C",
	  " C = coneFromVData matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " incompCones(C,F)"
	  },
     
     PARA{}, "This shows that the two cones do not intersect in a common face, but 
     if we divide C into two parts, we get a fan.",
     
     EXAMPLE {
	  " C1 = intersection {C, coneFromHData(matrix {{0,1,-1}})};",
	  " C2 = intersection {C, coneFromHData(matrix {{0,-1,1}})};",
	  " F = addCone({C1,C2},F)"
	  }
     
     }

document {
     Key => {addPolyhedron, (addPolyhedron,Polyhedron,PolyhedralComplex), (addPolyhedron,List,PolyhedralComplex)},
     Headline => "adds Polyhedra to a PolyhedralComplex",
     Usage => " PC1 = addPolyhedron(P,PC) \nPC1 = addPolyhedron(L,PC)",
     Inputs => {
	  "P" => Polyhedron,
	  "L" => List => {"with elements of class ", TO Polyhedron, " or ", TO PolyhedralComplex},
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "PC1" => PolyhedralComplex
	  },
     
     PARA{}, "If ",TT "addPolyhedron", " is applied to a ", TO Polyhedron, " and a ",TO PolyhedralComplex, " 
     it adds the Polyhedron to the PolyhedralComplex if they are in the same ambient space, if the Polyhedron is 
     compatible with every generating Polyhedron of ",TT "PC", ", but is not a face of one 
     of them. If one of the first two conditions fails, there will be an error and no PolyhedralComplex 
     will be returned. The pairs of incompatible polyhedra can be accessed with the 
     function ",TO incompPolyhedra,". If the last condition fails, then the Polyhedron is already in 
     the PolyhedralComplex as a face of one of the polyhedra, so it does not have to be added. The conditions 
     are checked in this order.",
     
     PARA{}, "If ",TT "addPolyhedron"," is applied to a ",TO List," and a ",TO PolyhedralComplex,", then 
     the function adds the list Polyhedron by Polyhedron and stops if one of the three conditions 
     fails for one of the polyhedra. There is again an error for the first two conditions. The 
     pairs of incompatible polyhedra can again be retrieved using ",TO incompPolyhedra,". Note that the may also 
     contain PolyhedralComplexes. Then the function replaces it by its list of generating polyhedra.",
     
     PARA{}, "If applied to a pair of PolyhedralComplexes it adds the generating polyhedra of the first 
     PolyhedralComplex to the second PolyhedralComplex, again checking for the same conditions as above.",
     
     PARA{}, " As an example, we make a PolyhedralComplex consisting of the following Polyhedron and 
     try to add an adjacent cube.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,1,1,1,2,2,2,2},{0,0,1,1,0,0,1,1},{0,1,0,1,0,1,0,1}};",
	  " PC = polyhedralComplex P",
	  " P = hypercube 3;",
	  " incompPolyhedra(P,PC)"
	  },
     
     PARA{}, "This shows that the two polyhedra do not intersect in a common face, but 
     if we divide P into three parts, we get a PolyhedralComplex.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,0,1,1,0,0,1,1},{0,1,0,1,0,1,0,1}};",
	  " P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,1,1,-1,0,1,1,-1},{0,0,-1,-1,0,0,-1,-1}};",
	  " P3 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,0,-1,-1,0,0,-1,-1},{0,1,1,-1,0,1,1,-1}};",
	  " P == convexHull {P1,P2,P3}",
	  " PC = addPolyhedron({P1,P2,P3},PC)"
	  }
     
     }

document {
     Key => {ambDim, (ambDim,PolyhedralObject)},
     Headline => "ambient dimension of a Polyhedron, Cone or Fan",
     Usage => "d = ambDim P \nd = ambDim C \nd = ambDim F",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone,
	  "F" => Fan
	  },
     Outputs => {
	  "d" => ZZ => {", the dimension of the ambient space"}
	  },
     
     PARA{}, TT "ambDim", " returns the dimension of the ambient space 
     either of the ", TO Polyhedron," ",TT "P", ", of the ", TO Cone," ",TT "C", " 
     or the ", TO Fan," ",TT "F", ".",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0},{0,1}}",
	  " ambDim P",
	  }
     }

document {
     Key => {cones, (cones,ZZ,Fan)},
     Headline => "computes all cones of a fan of a certain dimension",
     Usage => " L = cones(d,F)",
     Inputs => {
	  "d" => ZZ => {" between 0 and the dimension of the fan"},
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "cones", " computes the ", TO List, " of all Cones in ",
     TT "F", " of dimension ", TT "d", ".",
     
     EXAMPLE {
	  " F = normalFan hypercube 3",
	  " L = cones(2,F)",
	  },
     
     PARA{}, "To actually see the cones of the fan we can look at their 
     rays, for example:",
     
     EXAMPLE {
     " raysF = rays F",
	  " apply(L, c -> raysF_c)"
	  }
     
     }

document {
     Key => {polyhedra, (polyhedra,ZZ,PolyhedralComplex)},
     Headline => "computes all polyhedra of a polyhedral complex of a certain dimension",
     Usage => " L = polyhedra(d,PC)",
     Inputs => {
	  "d" => ZZ => {" between 0 and the dimension of the polyhedral complex"},
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "polyhedra", " computes the ", TO List, " of all Polyhedra in ",
     TT "PC", " of dimension ", TT "d", ".",
     
     EXAMPLE {
	  " PC = polyhedralComplex hypercube 3",
	  " L = polyhedra(2,PC)",
	  },
     
     PARA{}, "To actually see the polyhedra of the complex we can look at their 
     vertices, for example:",
     
     EXAMPLE {
     " vertPC = vertices PC",
	  " apply(L, l -> vertPC_(l#0))"
	  }
     
     }

document {
     Key => {maxCones, (maxCones,Fan)},
     Headline => "displays the generating Cones of a Fan",
     Usage => " L = maxCones F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "maxCones", " displays the ", TO List, " of generating cones 
     of the ", TO Fan, ", i.e. all Cones that are not a face of one 
     of the other cones. These are all of the same dimension if and only if 
     the Fan is pure (see: ", TO isPure,").",
     
     EXAMPLE {
	  " F = normalFan crossPolytope 3",
	  " L = maxCones F",
     " raysF = rays F",
	  " apply(L, mc -> raysF_mc)"
	  }
     
     }

document {
     Key => {maxPolyhedra, (maxPolyhedra,PolyhedralComplex)},
     Headline => "displays the generating Polyhedra of a PolyhedralComplex",
     Usage => " L = maxPolyhedra PC",
     Inputs => {
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "maxPolyhedra", " displays the ", TO List, " of generating polyhedra 
     of the ", TO PolyhedralComplex, ", i.e. all Polyhedra that are not a face of one 
     of the other Polyhedra. These are all of the same dimension if and only if 
     the PolyhedralComplex is pure (see: ", TO isPure,").",
     
     EXAMPLE {
	  " PC = skeleton(1,polyhedralComplex hypercube 2)",
	  " L = maxPolyhedra PC",
     " vertPC = vertices PC",
	  " apply(L, mp -> vertPC_(mp#0))"
	  }
     
     }

document {
     Key => {halfspaces, (halfspaces, Cone), (halfspaces, Polyhedron)},
     Headline => "computes the defining half-spaces of a Cone or a Polyhedron",
     Usage => " M = halfspaces C \n(M,v) = halfspaces P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix => {"with entries over ", TO QQ},
	  "v" => Matrix => {"with entries over ", TO QQ, " and only one column"}
	  },
     
     PARA{}, TT "halfspaces", " returns the defining affine half-spaces. For a 
     polyhedron ", TT "P", " the output is ", TT "(M,v)", ", where the source 
     of ", TT "M", " has the dimension of the ambient space of ", TT "P", " 
     and ", TT "v", " is a one column matrix in the target space 
     of ", TT "M", " such that ",TT "P = {p in H | M*p =< v}", " where 
     ", TT "H", " is the intersection of the defining affine hyperplanes.",
     
     PARA{}, " For a cone ", TT "C", " the output is the matrix", TT "M"," that is the 
     same matrix as before but ", TT "v", " is omitted since it is 0, 
     so ", TT "C = {c in H | M*c => 0}", " and ", TT "H", " is the intersection 
     of the defining linear hyperplanes.",
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     EXAMPLE {
	  " R = matrix {{1,1,2,2},{2,3,1,3},{3,2,3,1}};",
	  " V = matrix {{1,-1},{0,0},{0,0}};",
	  " C = coneFromVData R",
	  " halfspaces C"
	  },
     
     PARA{}, "Now we take this cone over a line and get a polyhedron.",
     
     EXAMPLE {
	  " P = convexHull(V,R)",
	  " halfspaces P"
	  }
     
     }

document {
     Key => {hyperplanes, (hyperplanes, Cone), (hyperplanes, Polyhedron)},
     Headline => "computes the defining hyperplanes of a Cone or a Polyhedron",
     Usage => " N = hyperplanes C \n(N,w) = hyperplanes P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "N" => Matrix => {"with entries over ", TO QQ},
	  "w" => Matrix => {"with entries over ", TO QQ, " and only one column"}
	  },
     
     PARA{}, TT "hyperplanes", " returns the defining affine hyperplanes for a 
     polyhedron ", TT "P", ". The output is ", TT "(N,w)", ", where the source 
     of ", TT "N", " has the dimension of the ambient space of ", TT "P", " 
     and ", TT "w", " is a one column matrix in the target space 
     of ", TT "N", " such that ",TT "P = {p in H | N*p = w}", " where 
     ", TT "H", " is the intersection of the defining affine half-spaces.",
     
     PARA{}, " For a cone ", TT "C", " the output is the matrix ", TT "N", ", that 
     is the same matrix as before but ", TT "w", " is omitted since it is 0, 
     so ", TT "C = {c in H | N*c = 0}", " and ", TT "H", " is the intersection 
     of the defining linear half-spaces.",
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     EXAMPLE {
	  " P = stdSimplex 2",
	  " hyperplanes P",
	  " C = coneFromVData matrix {{1,2,4},{2,3,5},{3,4,6}}",
	  " hyperplanes C"
	  }
     
     }

document {
     Key => {linealitySpace, (linealitySpace, PolyhedralObject)},
     Headline => "computes a basis of the lineality space",
     Usage => " LS = linealitySpace C \nLS = linealitySpace F \nLS = linealitySpace P",
     Inputs => {
	  "PO" => PolyhedralObject
	  },
     Outputs => {
	  "LS" => Matrix
	  },
     
     PARA{}, TT "linealitySpace", " returns a basis of the lineality space of the 
     input as the columns of the matrix ", TT "LS", ". The lineality space of a 
     Fan is the lineality space of any Cone of the Fan, since they all have the 
     same lineality space.",
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     EXAMPLE {
	  " M = matrix {{1,1,1},{0,1,0},{-1,1,-1},{-1,-1,-1},{0,-1,0},{1,-1,1}};",
	  " v = matrix {{2},{1},{2},{2},{1},{2}};",
	  " P = polyhedronFromHData(M,v)",
	  " linealitySpace P",
	  " C = dualCone coneFromHData M",
	  " linealitySpace C"
	  }
     
     }

document {
     Key => {(rays,PolyhedralObject)},
     Headline => "displays all rays of a Cone, a Fan, or a Polyhedron",
     Usage => " R = rays F \nR = rays P",
     Inputs => {
	  "PO" => PolyhedralObject
	  },
     Outputs => {
	  "R" => Matrix
	  },
     
     PARA{}, TT "rays", " returns the rays of the input as the columns of the 
     matrix ", TT "R", ".",
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     EXAMPLE {
	  " P = convexHull(matrix {{1,-1,2,-2},{1,1,2,2}}, matrix {{0},{1}})",
	  " rays P",
	  " C = coneFromVData P",
	  " rays C",
	  " F = normalFan P",
	  " rays F"
	  }
     
     }

document {
     Key => {vertices, (vertices,Polyhedron), (vertices,PolyhedralComplex)},
     Headline => "displays the vertices of a Polyhedron or a PolyhedralComplex",
     Usage => " V = vertices P",
     Inputs => {
	  "P" => Polyhedron => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "V" => Matrix
	  },
     
     PARA{}, TT "vertices", " returns the vertices of the Polyhedron or PolyhedralComplex ", TT "P", " 
     as the columns of the Matrix ", TT "V",".",
     
     PARA{},"Please see ",TO "V- and H-representation"," on the conventions we
     use for cones and polyhedra.",
     
     EXAMPLE {
	  " P = polyhedronFromHData(matrix{{1,-1},{0,-1},{-1,-1},{0,1}}, matrix{{0},{-1},{0},{1}})",
	  " vertices P",
	  " PC = skeleton(2,polyhedralComplex hypercube 3)",
	  " vertices PC"
	  }
     
     }

document {
     Key => {areCompatible, (areCompatible,Cone,Cone), (areCompatible,Polyhedron,Polyhedron)},
     Headline => "checks if the intersection of two cones/polyhedra is a face of each",
     Usage => " (b,X) = areCompatible(X1,X2)",
     Inputs => {
	  "X1" => Cone => {"or ",ofClass Polyhedron},
	  "X2" => Cone => {"or ",ofClass Polyhedron}
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face of each cone, 
	       and ", TO false, " otherwise."},
	  "C" => Cone => {"or ",ofClass Polyhedron,", the intersection of both if they are of the 
	                  same type and compatible, otherwise the empty polyhedron."}
	  },
     
     PARA{}, TT "areCompatible", " is an extension of ", TO commonFace, " for two Cones and for two 
     Polyhedra. It also checks if the intersection ", TT "X", " of ", TT "X1", " and ", TT "X2", " is a 
     face of each and the answer is given by ", TT "b", ". Furthermore, the intersection 
     is given for further calculations if the two cones/polyhedra lie in the same ambient space. Otherwise,  
     the empty polyhedron in the ambient space of ",TT "X1"," is given. Note that the input arguments 
     must either both be polyhedra or both be cones.",
     
     PARA{}, "For example, consider the following three cones",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0},{0,1}};",
	  " C2 = coneFromVData matrix {{1,-1},{0,-1}};",
	  " C3 = coneFromVData matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "These might form a fan, but if we check if they are compatible, we see they 
     are not:",
     
     EXAMPLE {
	  " areCompatible(C1,C2)",
	  " areCompatible(C2,C3)",
	  " areCompatible(C3,C1)",
	  }
     
     }

document {
     Key => {commonFace, (commonFace,Cone,Cone), (commonFace,Polyhedron,Polyhedron), 
	  (commonFace,Cone,Fan), (commonFace,Fan,Cone), (commonFace,Fan,Fan), (commonFace,List)},
     Headline => "checks if the intersection is a face of both Cones or Polyhedra, or of cones with fans",
     Usage => " b = commonFace(C1,C2) \nb = commonFace(P1,P2) \nb = commonFace(X,F) \nb = commonFace(F,X) \nb = commonFace L",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone,
	  "P1" => Polyhedron,
	  "P2" => Polyhedron,
	  "F" => Fan,
	  "X" => {TO Cone," or ",TO Fan},
	  "L" => List
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face both, 
	       and ", TO false, " otherwise."}
	  },
     
     PARA{}, TT "commonFace", " checks if the intersection of ", TT "C1", " 
     and ", TT "C2", " or the intersection of ", TT "P1", " and ", TT "P2", " is 
     a face of both. If it is applied to a pair of a cone ",TT "C"," and a fan ",TT "F"," then 
     it checks if the intersection of ",TT "C"," with every generating cone of ",TT "F"," is 
     a face of each. For two fans it checks this condition for every pair of generating cones. 
     If applied to a list then the list must contain Fans and Cones and it checks pairwise for 
     a common face.",
     
     PARA{}, "For example, consider the following three cones:",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0},{0,1}};",
	  " C2 = coneFromVData matrix {{1,-1},{0,-1}};",
	  " C3 = coneFromVData matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "for each pair of two of them we can check if their intersection is a common face:",
     
     EXAMPLE {
	  " commonFace(C1,C2)",
	  " commonFace(C2,C3)",
	  " commonFace(C3,C1)",
	  }
     
     }

document {
     Key => {contains, (contains,Cone,Cone), (contains,Cone,Matrix), (contains,Cone,Polyhedron), 
	  (contains,Fan,Cone), (contains,List,Cone), (contains,List,Polyhedron), (contains,Polyhedron,Cone), 
	  (contains,Polyhedron,Matrix), (contains,Polyhedron,Polyhedron)},
     Headline => "checks if the first argument contains the second argument",
     Usage => " b = contains(C,X) \nb = contains(P,X) \nb = contains(F,C) \nb = contains(L,C) \nb = contains(L,P)",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron,
	  "F" => Fan,
	  "L" => List,
	  "X" => {"either a ", TO Cone,", a ", TO Polyhedron,", or a ", TO Matrix," with only one 
	       column giving a point."}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the first argument contains the second argument, ", 
	       TO false, " otherwise."}
	 },
    
    PARA{}, TT "contains", " determines if the first argument contains the second argument. 
    Both arguments have to lie in the same ambient space. When the first argument is a Cone or 
    Polyhedron, it tests if the equations of the first argument are satisfied by the generating 
    points/rays of the second argument.",
    
    PARA{}, "For example, we can check if the 3 dimensional crosspolytope contains the hypercube 
    or the other way around:",
    
    EXAMPLE {
	" P = hypercube 3",
	" Q = crossPolytope 3",
	" contains(Q,P)",
	" contains(P,Q)"
	},
    
    PARA{}, "We can also check if the hypercube lies in the positive orthant.",
    
    EXAMPLE {
	 " C = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}};",
	 " contains(C,P)",
	 " P = affineImage(P,matrix{{1},{1},{1}})",
	 " contains(C,P)"
	 }
        
    }

document {
     Key => {isCompact, (isCompact,Polyhedron)},
     Headline => "checks compactness of a Polyhedron",
     Usage => " b = isCompact P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Polyhedron, " is compact, ", TO false, " otherwise"}
	  },
     
     PARA{}, TT "isCompact", " tests whether ", TT "P"," is compact, i.e. a polytope, by checking if the 
     rays and lineality space matrices are 0.",
     
     EXAMPLE {
	  " P = polyhedronFromHData(matrix{{1,1,1},{0,1,0},{-1,-1,-1},{-1,-1,-1},{0,-1,0},{1,-1,1}},matrix{{2},{1},{2},{2},{1},{2}})",
	  " isCompact P"
	  }
     
     }

document {
     Key => {isComplete, (isComplete, Fan), (isComplete, PolyhedralComplex)},
     Headline => "checks completeness of a Fan or PolyhedralComplex",
     Usage => " b = isComplete X",
     Inputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Fan, "/",TO PolyhedralComplex," is complete, ", TO false, " otherwise"}
	  },
     
     PARA{}, TT "isComplete"," just calls an entry in the hash table of the Fan. The check for completeness 
     is done while generating the fan. Whenever a full dimensional Cone is added (see ", TO fan," 
     or ", TO addCone,") the set of faces of codimension 1 that appear only in one full dimensional Cone 
     is updated. The Fan is then complete if and only if this set is empty and there is at least one 
     full dimensional Cone.",
     
     PARA{}," For a ",TO PolyhedralComplex," the function does the same. Just note, that a complete polyhedral 
     complex must contain non-compact polyhedra.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0},{0,1}};",
	  " C2 = coneFromVData matrix {{1,-1},{0,-2}};",
	  " C3 = coneFromVData matrix {{0,-2},{1,-1}};",
	  " F = fan {C1,C2,C3}",
	  " isComplete F"
	  },
     
     PARA{}, "Hence the fan above is not complete, but we can add the missing cone:",
     
     EXAMPLE {
	  " C4 = coneFromVData matrix {{-1,-2},{-2,-1}};",
	  " F = addCone(C4,F)",
	  " isComplete F"
	  }
     
     }

document {
     Key => {isEmpty, (isEmpty,Polyhedron)},
     Headline => "checks if a Polyhedron is empty",
     Usage => " b = isEmpty P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Polyhedron, " is empty, ", TO false, " otherwise"}
	  },
     
     PARA{}, "The polyhedron is empty if the dimension is -1.",
     
     EXAMPLE {
	  " P = polyhedronFromHData(matrix{{1,0},{0,1},{-1,-1}},matrix{{-1},{1},{-1}})",
	  " isEmpty P"
	  }
     
     }

document {
     Key => {isFace, (isFace,Cone,Cone), (isFace,Polyhedron,Polyhedron)},
     Headline => "tests if the first argument is a face of the second",
     Usage => "b = isFace(X,Y)",
     Inputs => {
	  "X" => Cone => {" or ", TO Polyhedron},
	  "Y" => {"an element of the same class as ", TT "X"}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if ", TT "X", " is a face of ",TT "Y",", false otherwise"}
	  },
     
     PARA{}, "Both arguments must lie in the same ambient space. Then ", TT "isFace", " computes all 
     faces of ",TT "Y"," with the dimension of ",TT "X"," and checks if one of them is ",TT "X",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " Q = convexHull matrix{{1,1,1},{1,1,-1},{1,-1,1}}",
	  " isFace(Q,P)"
	  },
     
     PARA{}, "Thus, ",TT "Q"," is not a face of ",TT "P",", but we can extend it to a face.",
     
     EXAMPLE {
	  " v = matrix{{1},{-1},{-1}};",
	  " Q = convexHull{Q,v}",
	  " isFace(Q,P)"
	  }
     
     }

document {
     Key => {isLatticePolytope, (isLatticePolytope,Polyhedron)},
     Headline => "checks if a polyhedron is a lattice polytope",
     Usage => "b = isLatticePolytope P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "b" => Boolean => {"true if P is a lattice polytope"}
	  },

     PARA{}, TT "isLatticePolytope"," can only be applied to polytopes, i.e. compact polyhedra. It
     simply checks if it is compact and all vertices are lattice points.",

     EXAMPLE {
	  " P = polyhedronFromHData(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{1},{1},{1},{1}})",
	  " isLatticePolytope P",
	  " P = polyhedronFromHData(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{4},{6},{3},{6}})",
	  " isLatticePolytope P"
	    }

     }

document {
     Key => (isNormal,Polyhedron),
     Headline => "checks if a polytope is normal in the ambient lattice",
     Usage => "b = isNormal P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "b" => Boolean => {"true if P is normal in the ambient lattice"}
	  },

     PARA{}, TT "isNormal"," can only be applied to polytopes, i.e. compact polyhedra. It
     embeds the polytope on height 1 in a space of dimension plus 1 and takes the Cone over
     this polytope. Then it checks if all elements of the Hilbert basis lie in height 1.",

     EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " isNormal P"
	    }

     }

document {
     Key => {isPointed, (isPointed,Cone), (isPointed,Fan)},
     Headline => "checks if a Cone or Fan is pointed",
     Usage => "b = isPointed C \nb = isPointed F",
     Inputs => {
	  "C" => Cone,
	  "F" => Fan
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the ",TO Cone," or the ",TO Fan," is pointed, false otherwise"}
	  },
     
     PARA{}, "Tests if a Cone is pointed, i.e. the lineality space is 0. A Fan is pointed if one of its 
     Cones is pointed. This is equivalent to all Cones being pointed.",
     
     EXAMPLE {
	  " C = coneFromHData(matrix{{1,1,-1},{-1,-1,-1}})",
	  " isPointed C",
	  " C = intersection{C, coneFromHData(matrix{{1,-1,-1}})}",
	  " isPointed C"
	  }
     
     }

document {
     Key => {isPolytopal, (isPolytopal,Fan)},
     Headline => "checks if a Fan is polytopal",
     Usage => "b = isPolytopal F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the ",TO Fan," is polytopal, ",TO false," otherwise"}
	  },
     
     PARA{}, "If ",TT "F"," is projective, then there exists a polyhedron ",TT "P"," such that ",TT "F"," 
     is the normalFan of ",TT "P",". This means every codimension 1 cone of the Fan corresponds exactly to 
     an edge of the polytope. So consider ", TO QQ," to the number of all edges. This can be considered as the 
     space of all edge lengths. If we take arbitrary lengths now for every edge we do not get a polytope. But 
     every codimension 2 cone of the fan corresponds to a 2 dimensional face of the polytope and if the edges 
     belonging to this face add up to 0 zero, they form in fact a 2 dimensional face. This gives linear 
     equations on the space of edge lengths and if we intersect these equations with the positive orthant in 
     the space of edge lengths we get a Cone. Thus, there exists such a polytope if and only if there is a 
     vector in this cone with strictly positive entries, since every edge has to appear in the polytope.",
     
     PARA{}, "IF ",TT "F"," is polytopal, the function ",TO polytope," returns a polytope of which ",TT "F"," is 
     the normalFan.",
     
     PARA{}, "Note that the function first checks if the fan is complete.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0},{0,1}};",
	  " C2 = coneFromVData matrix {{1,-1},{0,-2}};",
	  " C3 = coneFromVData matrix {{0,-2},{1,-1}};",
	  " C4 = coneFromVData matrix {{-1,-2},{-2,-1}};",
	  " F = fan{C1,C2,C3,C4}",
	  " isPolytopal F"
	  }
     
     }

document {
     Key => {isPure,(isPure, Fan), (isPure, PolyhedralComplex)},
     Headline => "checks if a Fan or PolyhedralComplex is of pure dimension",
     Usage => " b = isPure X",
     Inputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Fan,"/",TO PolyhedralComplex," is of pure dimension, ",TO false," otherwise"}
	  },
     
     PARA{}, TT "isPure", " tests if the ", TO Fan,"/",TO PolyhedralComplex," is pure by checking if the first 
     and the last entry in the list of generating Cones/Polyhedra are of the same dimension.",
     
     PARA{}, "Let us construct a fan consisting of the positive orthant and the ray ",TT "v"," that is the 
     negative sum of the canonical basis, which is obviously not pure:",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " v = coneFromVData matrix {{-1},{-1},{-1}}",
	  " F = fan {C,v}",
	  " isPure F",
	  },
     
     PARA{}, "But we can make a pure fan if we choose any two dimensional face of the positive 
     orthant and take the cone generated by this face and ",TT "v"," and add it to the cone:",
     
     EXAMPLE {
	  " C1 = coneFromVData (rays C)_((faces(1,C))#0)",
     " C1 = coneFromVData(C1, v)",
	  " F = addCone(C1,F)",
	  " isPure F"
	  }
     
     }

document {
     Key => {isReflexive, (isReflexive,Polyhedron)},
     Headline => " checks if a Polytope is reflexive",
     Usage => " b = isReflexive P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Polyhedron," is reflexive, ",TO false," otherwise"}
	  },
     
     PARA{},"A lattice polytope ",TT "P"," in the ",TO QQ," space of a lattice ",TEX ///$M$///," 
     is reflexive if its polar polytope is also a lattice polytope. The function checks if ",TT "P"," 
     is compact, a lattice polytope and if the dual is a lattice polytope.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,0,-1},{0,1,-1}}",
	  " isReflexive P"
	  },
     
     SeeAlso => {isCompact,
	  isLatticePolytope,
	  polar}
     
     }

document {
     Key => {isSimplicial, (isSimplicial,PolyhedralObject)},
     Headline => " checks if a polyhedral object is simplicial",
     Usage => " b = isSimplicial X",
     Inputs => {
	  "X" => PolyhedralObject
	  },
     Outputs => {
	  "b" => {TO true," if the ",TO PolyhedralObject," is simplicial, ",TO false," otherwise"}
	  },
     
     PARA{},"A ",TO Polyhedron," of dimension ",TEX///$d$///," is simplicial if it is compact and 
     every facet is (isomorphic to) a simplex.",
     
     EXAMPLE {
	  " P = convexHull matrix {{3,0,0,0,1},{0,3,0,0,1},{0,0,3,0,1}}",
	  " isSimplicial P",
	  " P = hypercube 2",
	  " isSimplicial P"
	  },
     
     PARA{},"A pointed ",TO Cone," of dimension ",TEX///$d$///," is simplicial if it has ",TEX///$d$///," rays.",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,0,0,1},{0,1,0,1},{0,0,1,1}}",
	  " isSimplicial C",
	  " C = coneFromVData matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " isSimplicial C"
	  },
     
     PARA{},"A ",TO Fan,"/",TO PolyhedralComplex," is simplicial if  every ",TO Cone,"/",TO Polyhedron," 
     of it is simplicial.",
     
     EXAMPLE {
	  " F = normalFan hypercube 3",
	  " isSimplicial F",
	  " PC = skeleton(2,polyhedralComplex crossPolytope 3)",
	  " isSimplicial PC"
	  },
     
     SeeAlso => {isCompact,
	  isPointed,
	  dim,
	  vertices,
	  rays}
     
     }

document {
     Key => {(isSmooth,Cone), (isSmooth,Fan)},
     Headline => "checks if a Cone or Fan is smooth",
     Usage => " b = isSmooth C \nb = isSmooth F",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Cone,"/",TO Fan," is smooth, ",TO false," otherwise"}
	  },
     
     PARA{}, TT "isSmooth"," checks for a ",TO Cone," if the rays are a subset of a basis of the 
     lattice. For a ",TO Fan," it checks smoothness for every ",TO Cone,".",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  " isSmooth C",
	  " F = hirzebruch 3",
	  " isSmooth F"
	  }
     
     }

document {
     Key => {(isVeryAmple, Polyhedron)},
     Headline => "checks if the Polyhedron is very ample",
     Usage => " b = isVeryAmple P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Polyhedron," is very ample, ",TO false," otherwise"}
	  },
     
     PARA{}, "A lattice polytope ",TT "P"," in the ",TO QQ," space of a lattice ",TEX ///$M$///," 
     is very ample if for every vertex ",TEX ///$v\in P$///," the semigroup ",TEX ///$\mathbb{N}(P\cap M - v)$///," 
     generated by ",TEX ///$P\cap M - v = \{v'-v|v'\in P\cap M\}$///," is saturated in ",TEX ///$M$///,". 
     For example, normal lattice polytopes are very ample.",
     
     PARA{}, "Note that therefore ",TT "P"," must be compact and a lattice polytope.",
     
     EXAMPLE {
	  " P = convexHull matrix {{0,1,0,0,1,0,1,2,0,0},{0,0,1,0,1,0,2,2,0,-1},{0,0,0,1,2,0,1,2,0,-1},{0,0,0,0,-1,1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,1,1}}",
	  " isVeryAmple P"
	  }
     }	  


document {
     Key => {faces, (faces,ZZ,PolyhedralObject)},
     Headline => "computes all faces of a certain codimension of a Cone or Polyhedron",
     Usage => " L = faces(k,C) \nL = faces(k,P) \nL = faces(F)",
     Inputs => {
	  "k" => ZZ,
	  "PO" => PolyhedralObject
	  },
     Outputs => {
	  "L" => HashTable => {"containing the indices of the vertices/rays used in the codim  ",TT "k", "faces of ", TT "P"}
	  },
     
     PARA{}, TT "faces"," computes the faces of codimension ",TT "k"," of the given ",TO Cone," 
     or ",TO Polyhedron,", where ",TT "k"," must be between 0 and the dimension of the second 
     argument. The faces will be of the same class as the original convex object. If the parameter",
     TT "k","is omitted,", TT "faces"," will return a ",TT "HashTable"," containing the faces
     with the codimensions as keys.",
     
     PARA{}, "For example, we can look at the edges of the cyclicPolytope with 5 vertices in 3 space",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " L = faces(2,P)"
	  },
     
     PARA{}, "Since this is only a list of polyhedra we look at their vertices:",
     
     EXAMPLE {
     " vertP = vertices P",
	  " apply(L, f -> vertP_(f#0))"
	  }
     }

document {
     Key => {fVector, (fVector, PolyhedralObject)},
     Headline => "computes the f-vector of a Cone, Polyhedron, Fan or PolyhedralComplex",
     Usage => " f = fVector C \nf = fVector P",
     Inputs => {
	  "P" => PolyhedralObject
	  },
     Outputs => {
	  "L" => List => {"containing the number of faces for each codimension"}
	  },
     
     PARA{}, "The ",TT "i","-th entry of the f-vector of ",TT "P"," is the number of dimension ",
     TT "i","-1 faces of ",TT "P",", so it starts with the number vertices, has 
     dim(",TT "P",")+1 entries, and the last entry is 1 for ",TT "P"," itself. It is the same for 
     a Cone ",TT "C",".",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " fVector P"
	  }
     
     }

document {
     Key => {hilbertBasis, (hilbertBasis,Cone)},
     Headline => "computes the Hilbert basis of a Cone",
     Usage => " HB = hilbertBasis C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "L" => List => {"containing the elements of the Hilbert basis"}
	  },
     
     PARA{}, "The Hilbert basis of the cone ",TT "C"," is computed by the 
     Project-and-Lift-algorithm by Raymond Hemmecke (see below). It computes a Hilbert basis of 
     the cone modulo the lineality space, so it returns the list of one column matrices that give 
     the Hilbert basis of the Cone if one adds the basis of the lineality space and its negative. 
     For the Project-and-Lift-algorithm see: ",
     
     PARA{}, HREF("http://www.hemmecke.de/raymond/", "Raymond Hemmecke's"), " ", EM "On the 
     computation of Hilbert bases of cones", ", in A. M. Cohen, X.-S. Gao, and N. Takayama, 
     editors, Mathematical Software, ICMS 2002, pages 307317. World Scientific, 2002.",

     EXAMPLE {
	  " C = coneFromVData matrix {{1,2},{2,1}}",
	  " hilbertBasis C"
	  },

     PARA{}, "Beginning with Macaulay2 version 1.24.11, this method calls the internal function ",
     TT "rawHilbertBasis", " which calls the C++ library ", TO2 {"normaliz", "libnormaliz"}, ".",
     
     SeeAlso => {"FourTiTwo::hilbertBasis(Matrix)"}
     }

document {
     Key => {incompCones, (incompCones,List), (incompCones,Cone,Fan), (incompCones,Fan,Cone), (incompCones,Fan,Fan)},
     Headline => "returns the pairs of incompatible cones",
     Usage => " Lpairs = incompCones L \nLpairs = incompCones(X,F) \nLpairs = incompCones(F,X)",
     Inputs => {
	  "L" => List,
	  "F" => Fan,
	  "X" => {TO Cone," or ",TO Fan,}
	  },
     Outputs => {
	  "Lpairs" => List
	  },
     
     PARA{}, "If ",TT "incompCones"," is applied to a list of cones and fans, then it returns the pairs of elements 
     whose intersection is not a face of each. For a cone ",TT "C"," and a fan ",TT "F"," in the list this means there 
     is at least one generating cone of ",TT "F"," whose intersection with ",TT "C"," is not a face of each. For two 
     fans in the list this means there is at least one generating cone each such that their intersection is not a face
     of each. If applied to a pair consisting of a cone and a fan or two fans, then it returns the pairs of cones that 
     do not share a common face.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix{{1,0},{1,1}};",
	  " C2 = coneFromVData matrix{{1,0},{0,-1}};",
	  " C3 = coneFromVData matrix{{-1,0},{0,1}};",
	  " C4 = coneFromVData matrix{{1,1},{0,1}};",
	  " C5 = coneFromVData matrix {{1,2},{2,1}};",
	  " L = {C1,C2,C3,C4,C5};",
	  " Lpairs = incompCones L",
	  " Lpairs == {(C1,C4),(C1,C5)}",
	  }
     
     }

document {
     Key => {incompPolyhedra, (incompPolyhedra,List), (incompPolyhedra,Polyhedron,PolyhedralComplex), (incompPolyhedra,PolyhedralComplex,Polyhedron), (incompPolyhedra,PolyhedralComplex,PolyhedralComplex)},
     Headline => "returns the pairs of incompatible polyhedra",
     Usage => " Lpairs = incompPolyhedra L \nLpairs = incompPolyhedra(X,PC) \nLpairs = incompPolyhedra(PC,X)",
     Inputs => {
	  "L" => List,
	  "PC" => PolyhedralComplex,
	  "X" => {TO Polyhedron," or ",TO PolyhedralComplex,}
	  },
     Outputs => {
	  "Lpairs" => List
	  },
     
     PARA{}, "If ",TT "incompPolyhedra"," is applied to a list of polyhedra and polyhedral complexes, then it returns the pairs of elements 
     whose intersection is not a face of each. For a Polyhedron ",TT "P"," and a PolyhedralComplex ",TT "PC"," in the list this means there 
     is at least one generating Polyhedron of ",TT "PC"," whose intersection with ",TT "P"," is not a face of each. For two 
     polyhedral complexes in the list this means there is at least one generating polyhedron each such that their intersection is not a face
     of each. If applied to a pair consisting of a polyhedron and a polyhedral complex or two polyhedral complexes, then it returns the pairs of polyhedra that 
     do not share a common face.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,0,0},{1,1,0}};",
	  " P2 = convexHull matrix {{1,0,0},{0,-1,0}};",
	  " P3 = convexHull matrix {{-1,0,0},{0,1,0}};",
	  " P4 = convexHull matrix {{1,1,0},{0,1,0}};",
	  " P5 = convexHull matrix {{1,2,0},{2,1,0}};",
	  " L = {P1,P2,P3,P4,P5};",
	  " Lpairs = incompPolyhedra L",
	  " Lpairs == {(P1,P4),(P1,P5)}",
	  }
     
     }
 
document {
     Key => {inInterior, (inInterior,Matrix,Cone), (inInterior,Matrix,Polyhedron)},
     Headline => "checks if a point lies in the relative interior of a Cone/Polyhedron",
     Usage => " b = inInterior(p,C) \nb = inInterior(p,P)",
     Inputs => {
	  "p" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if ",TT "p"," lies in the relative interior of the 
	       Cone/Polyhedron, ", TO false," otherwise"}
	  },
     
     PARA{}, TT "inInterior", " checks if the smallest face of the ",TO Cone," or 
     the ",TO Polyhedron," containing ",TT "p"," is the ",TO Cone," or 
     the ",TO Polyhedron," itself. For this the number of rows of ",TT "p"," must 
     equal the ambient dimension of the second argument.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " p = matrix{{2},{4},{8}}",
	  " q = matrix{{2},{6},{20}}",
	  " inInterior(p,P)",
	  " inInterior(q,P)"
	  }
     
     }

document {
     Key => {interiorPoint, (interiorPoint,Polyhedron)},
     Headline => "computes a point in the relative interior of the Polyhedron",
     Usage => " p = interiorPoint P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "p" => Matrix => {" over ",TO QQ," with only one column representing a point"}
	  },
     
     PARA{}, TT "interiorPoint", " takes the vertices of the ",TO Polyhedron," and computes the sum 
     multiplied by ",TT "1/n",", where ",TT "n"," is the number of vertices.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " interiorPoint P"
	  }
     
     }

document {
     Key => {interiorVector, (interiorVector,Cone)},
     Headline => "computes a vector in the relative interior of a Cone",
     Usage => " p = interiorVector C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "p" => Matrix => {" over ",TO QQ," with only one column representing a vector"}
	  },
     
     PARA{}, TT "interiorVector", " takes the rays of the ",TO Cone,", computes the sum and 
     divides by the gcd to get a primitive vector.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,4)",
	  " C = coneFromVData P",
	  " interiorVector C"
	  }
     
     }

document {
     Key => {interiorLatticePoints, (interiorLatticePoints,Polyhedron)},
     Headline => "computes the lattice points in the relative interior of a polytope",
     Usage => " L = interiorLatticePoints P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "L" => List => {"containing the interior lattice points as matrices over ",TO ZZ," with only 
	       one column"}
	  },
     
     PARA{}, TT "latticePoints"," can only be applied to polytopes, i.e. compact polyhedra. It 
     returns all lattice points in the relative interior of the polytope.",
     
     EXAMPLE {
	  " P = crossPolytope(3,2)",
	  " interiorLatticePoints P",
	  " Q = cyclicPolytope(2,4)",
	  " interiorLatticePoints Q"
	  }
     
     }

document {
     Key => {latticePoints, (latticePoints,Polyhedron)},
     Headline => "computes the lattice points of a polytope",
     Usage => " L = latticePoints P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "L" => List => {"containing the lattice points as matrices over ",TO ZZ," with only 
	       one column"}
	  },
     
     PARA{}, TT "latticePoints"," can only be applied to polytopes, i.e. compact polyhedra. It 
     embeds the polytope on height 1 in a space of dimension plus 1 and takes the Cone over 
     this polytope. Then it projects the elements of height 1 of the Hilbert basis back again.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " latticePoints P",
	  " Q = cyclicPolytope(2,4)",
	  " latticePoints Q"
	  }
     
     }

document {
     Key => {maxFace, (maxFace,Matrix,Polyhedron), (maxFace,Matrix,Cone)},
     Headline => "computes the face of a Polyhedron or Cone where a weight attains its maximum",
     Usage => " F = maxFace(w,P) \nF = maxFace(w,C)",
     Inputs => {
	  "w" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a 
	       weight vector"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => {"Depending on the input, a Cone or a Polyhedron, the face where ",TT "w"," attains 
	       its maximum"}
	  },
     
     PARA{}, TT "maxFace"," computes the face of the given Polyhedron ",TT "P"," or Cone ",TT "C","  
     where ",TT "w"," attains its maximum.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " w = matrix {{1},{-1},{0}}",
	  " F = maxFace(w,P)",
	  " vertices F"
	  }
     }
	  

document {
     Key => {minFace, (minFace,Matrix,Polyhedron), (minFace,Matrix,Cone)},
     Headline => "computes the face of a Polyhedron or Cone where a weight attains its minimum",
     Usage => " F = minFace(w,P) \nF = minFace(w,C)",
     Inputs => {
	  "w" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a 
	       weight vector"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => {"Depending on the input, a Cone or a Polyhedron, the face where ",TT "w"," attains 
	       its minimum"}
	  },
     
     PARA{}, TT "minFace"," computes the face of the given Polyhedron ",TT "P"," or Cone ",TT "C"," 
     where ",TT "w"," attains its minimum.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " w = matrix {{1},{2},{0}}",
	  " F = minFace(w,P)",
	  " vertices F"
	  }
     }
	  

document {
     Key => {minkSummandCone, (minkSummandCone,Polyhedron)},
     Headline => "computes the Cone of all Minkowski summands and the minimal decompositions",
     Usage => " (C,L,M) = minkSummandCone P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C" => Cone,
	  "L" => List => {" containing Polyhedra"},
	  "M" => Matrix
	  },
     
     PARA{}, "For the Minkowski summand cone one takes ",TO QQ,"^d where d is the number 
     of edges of the input polyhedron ",TT "P",". Every Minkowski summand of ",TT "P"," has 
     only edges that are edges of ",TT "P",", so it can be constructed by rescaling every 
     edge of ",TT "P",", i.e. is represented by a point in ",TO QQ,"^d. But not every point 
     in ",TO QQ,"^d gives a polyhedron via this method. This is the case if on the one 
     hand the point lies in the positive orthant and on the other hand for every compact two 
     dimensional face of ",TT "P"," the rescaled edges of such a face give a two dimensional 
     polytope, i.e. the sum of the ordered rescaled edge directions is zero. Therefore, every 
     compact two dimensional face of ",TT "P"," gives a set of linear equalities on a part of 
     the variables in ",TO QQ,"^d. The Minkowski Summand Cone ",TT "C"," is the intersection 
     of the positive orthant with these equations. Thus, every point in ",TT "C"," corresponds 
     to a Minkowski summand (probably after rescaling). The corresponding polyhedron to every 
     minimal generator of ",TT "C"," is saved in the hash table ",TT "L",". Finally, all possible 
     minimal decompositions of ",TT "P"," are saved as columns in the matrix ",TT "M",".",
     
     PARA{}, "For example, consider the Minkowski summand cone of the hexagon.",
     
     EXAMPLE {
	  " P = convexHull matrix{{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}}",
	  " (C,L,M) = minkSummandCone P"
	  },
     
     PARA{}, "Thus, we see that the minimal Minkowski summands of the hexagon are two triangles 
     and three lines and that there are two minimal decompositions, i.e. the hexagon is the sum 
     of the two triangles or the three lines:",
     
     EXAMPLE {
	  " rays C",
	  " apply(values L,vertices)",
	  " M"
	  }
	  
     }

document {
     Key => {mixedVolume, (mixedVolume,List)},
     Headline => "computes the mixed volume of a list of polytope",
     Usage => " v = mixedVolume L",
     Inputs => {
	  "L" => List => {"containing n polytopes in n-space"}
	  },
     Outputs => {
	  "v" => ZZ => {"the mixed volume"}
	  },
     
     PARA{},"Let ",TEX ///$P_1,...,P_n$///," be polytopes in ",TEX ///$n$///,"-space. Then the volume 
     of the Minkowski sum ",TEX ///$\lambda_1 P_1 + ... + \lambda_n P_n$///," is a homogeneous polynomial of degree ",
     TEX ///$n$///," in nonnegative variables ",TEX ///$\lambda_1,...,\lambda_n$///,". The coefficient Vol",
     TEX ///$(P_1,...,P_n)$///," of ",TEX ///$\lambda_1\lambda_2 ... \lambda_n$///," is called 
     the mixed volume of ",TEX ///$P_1,...,P_n$///,". For example, the number of toric solutions 
     to a generic system of ",TEX ////$n$///," polynomial equations on ",TEX ///$n$///,"-space amounts to 
     the mixed volume of the corresponding Newton polytopes.",
     
     PARA{},"The function ",TT "mixedVolume"," takes the ",TO List," ",TT "L"," with ",TEX ///$n$///," polytopes 
     in ",TEX ///$n$///,"-space and computes their mixed Volume by using the algorithm by Ioannis Z. Emiris in his paper ",
     HREF("http://www.nag.co.uk/projects/frisco/frisco/reports/d33421.ps", "Mixed Volume Implementation"),". Note that this function 
     computes an upper bound by using a random lifting. To reassure the result run the function until it returns the same result.",
     
     PARA{},"CAVEAT: So far the input is not checked so use the function with care!",
     
     EXAMPLE {
	  " P = crossPolytope 2",
	  " Q = hypercube 2",
	  " mixedVolume {P,Q}"
	  }
     
     }
     

document {
     Key => {objectiveVector, (objectiveVector,Polyhedron,Polyhedron)},
     Headline => "computes an objective vector of a face of a polyhedron",
     Usage => " v = objectiveVector(P,Q)",
     Inputs => {
	  "P" => Polyhedron,
	  "Q" => Polyhedron => {"which must be a face of ",TT "P"}
	  },
     Outputs => {
	  "v" => Matrix => {"one column vector over ",TO QQ," representing a vector"}
	  },
     
     PARA{}, "An objective vector ",TT "v"," of a face ",TT "Q"," of a polyhedron ",TT "P"," is vector 
     such that ",TT "Q = {p in P | v*p = max over P}"," i.e. it is the face on which ",TT "v"," attains 
     its maximum.",
     
     EXAMPLE{
	  " P = hypercube 3",
	  " Q = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " v = objectiveVector(P,Q)"
	  },
     
     PARA{}, "Since it is the face on which ",TT "v"," attains its maximum it can be recovered with ",TO maxFace,":",
     
     EXAMPLE{
	  " Q == maxFace(v,P)"
	  }
     }

document {
     Key => (normalCone,Polyhedron,Polyhedron),
     Headline => "computes the normal cone of a face of a polyhedron",
     Usage => " C = normalCone(P,Q)",
     Inputs => {
	  "P" => Polyhedron,
	  "Q" => Polyhedron => {"which must be a face of ",TT "P"}
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "The normal cone of a face ",TT "Q"," of a polyhedron ",TT "P"," is the cone in the normal fan (see ",TO normalFan,")
     that corresponds to this face. This is the cone of all vectors attaining their maximum on this face.",
     
     EXAMPLE{
	  " P = hypercube 3",
	  " Q = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " C = normalCone(P,Q)",
	  " rays C"
	  }
     }

document {
     Key => {polytope, (polytope,Fan)},
     Headline => "returns a polytope of which the fan is the normal fan if it is polytopal",
     Usage => " P = polytope F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "If the fan ",TT "F"," is polytopal then ",TT "polytope"," returns a polytope ",TT "P",". ",TT "F"," is the 
     normal fan of this polytope. Note that such a polytope is not unique.",
     
     EXAMPLE {
	  " F = fan {coneFromVData matrix {{1,0},{0,1}},coneFromVData matrix {{0,-1},{1,1}},coneFromVData matrix {{-1,-1},{0,1}},coneFromVData matrix {{-1,1},{0,-1}},coneFromVData matrix {{1,1},{0,-1}}}",
	  " P = polytope F"
	  }
     
     }

document {
     Key => {proximum, (proximum,Matrix,Polyhedron), (proximum,Matrix,Cone)},
     Headline => "computes the proximum of the Polyhedron/Cone to a point in euclidean metric",
     Usage => " q = proximum(p,P) \nq = proximum(p,C)",
     Inputs => {
	  "p" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "q" => Matrix => {" over ",TO QQ," with only one column representing the closest point"}
	  },
     
     PARA{}, "For a point ",TT "p"," and a Polyhedron ",TT "P"," or a Cone ",TT "C",", ",TT "proximum"," 
     computes the point in ",TT "P"," or ",TT "C"," with minimal euclidean distance to ",TT "p",".",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " p = matrix {{1},{2},{3}}",
	  " q = proximum(p,P)"
	  }
     
     }

document {
     Key => {skeleton, (skeleton,ZZ,Fan), (skeleton,ZZ,PolyhedralComplex)},
     Headline => "computes the k-skeleton of a Fan or PolyhedralComplex",
     Usage => " X = skeleton(k,F) \nX = skeleton(k,PC)",
     Inputs => {
	  "k" => ZZ,
	  "F" => Fan,
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     
     PARA{}, "For a ",TO Fan,TT " F"," and an integer ",TT "k"," between 0 and the dimension of ",TT "F",", 
     ",TT "skeleton"," computes the ",TT "k","-skeleton of the ",TO Fan," ",TT "F",", 
     i.e. the ",TO Fan," ",TT "F1"," generated by all cones of dimension 
     ",TT "k"," in ",TT "F",".",
     
     PARA{}, "For example, we can look at the 2-skeleton of the fan of projective 
     3-space:",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0,0,0},{0,1,0,0},{0,0,1,0}}",
	  " F = normalFan P",
	  " F1 = skeleton(2,F)",
     " raysF = rays F",
	  " apply(maxCones F1, mc -> raysF_mc)"
	  },
     
     PARA{}, "For a ",TO PolyhedralComplex,TT " PC"," and an integer ",TT "k"," between 0 and the dimension of ",TT "PC",", 
     ",TT "skeleton"," computes the ",TT "k","-skeleton of the ",TO PolyhedralComplex," ",TT "PC",", 
     i.e. the ",TO PolyhedralComplex," ",TT "PC1"," generated by all polyhedra of dimension 
     ",TT "k"," in ",TT "PC",".",
     
     EXAMPLE {
	  " PC = polyhedralComplex hypercube 3",
	  " PC1 = skeleton(2,PC)",
     " vertPC1 = vertices PC1",
	  " apply(maxPolyhedra PC1, mp -> vertPC1_(mp#0))"
	  }
     
     }

document {
     Key => {smallestFace, (smallestFace,Matrix,Cone), (smallestFace,Matrix,Polyhedron)},
     Headline => "determines the smallest face of the Cone/Polyhedron containing a point",
     Usage => " C1 = smallestFace(p,C) \nP1 = smallestFace(p,P)",
     Inputs => {
	  "p" => Matrix => {"over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C1" => Cone => {" or"},
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "p"," is considered to be a point in the ambient space of the second argument, so 
     the number of rows of ",TT "p"," must equal the dimension of the ambient space of the 
     second argument. The function computes the smallest face of the second argument that 
     contains ",TT "p",". If the second argument is a ",TO Polyhedron," the output is a 
     ",TO Polyhedron," and if it is a ",TO Cone," the output is a ",TO Cone,". In both cases, 
     if the point is not contained in the second argument then the output is the empty 
     polyhedron.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " p = matrix {{1},{0},{0}}",
	  " smallestFace(p,P)"
	  }
     
     }

document {
     Key => {smoothSubfan, (smoothSubfan,Fan)},
     Headline => "computes the subfan of all smooth cones",
     Usage => " F1 = smoothSubfan F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, " For a given ",TO Fan," ",TT "F"," the function computes the subfan ",TT "F1"," of 
     all smooth cones.",
     
     PARA{}, "Let's consider the fan consisting of the following three dimensional cone and all 
     of its faces:",
     
     EXAMPLE {
	  " C = coneFromVData  matrix {{1,-1,0},{1,1,0},{1,1,1}}",
	  " F = fan C"
	  },
     
     PARA{}, "This cone is not smooth, therefore also the fan is not. But if we remove the interior and one 
     of the two dimensional faces the resulting subfan is smooth.",
     
     EXAMPLE {
	  " F1 = smoothSubfan F",
     " raysF1 = rays F1",
	  " apply(maxCones F1, mc -> raysF1_mc)"
	  }
     
     }

document {
     Key => {stellarSubdivision, (stellarSubdivision,Fan,Matrix)},
     Headline => "computes the stellar subdivision of the fan by a ray",
     Usage => "F1 = stellarSubdivision(F,r)",
     Inputs => {
	  "F" => Fan,
	  "r" => Matrix => {"with one column in the ambient space of the fan"}
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, "This function computes the stellar subdivision of ",TT "F"," by inserting the 
     ray given by ",TT "r",".",
     
     EXAMPLE {
	  " F = normalFan hypercube 2",
	  " r = matrix {{1},{1}}",
	  " F1 = stellarSubdivision(F,r)"
	  }
     }

document {
     Key => {tailCone, (tailCone,Polyhedron)},
     Headline => "computes the tail/recession cone of a polyhedron",
     Usage => " C = tailCone P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Every polyhedron ",TT "P"," can be uniquely decomposed into the sum of a 
     polytope and a cone, the tail or recession cone of ",TT "P",". Thus, it is the cone 
     generated by the non-compact part, i.e. the rays and the lineality space 
     of ",TT "P",". If ",TT "P"," is a polytope then the tail cone is the origin in the 
     ambient space of ",TT "P",".",
     
     EXAMPLE {
	  " P = polyhedronFromHData(matrix{{-1,0},{1,0},{0,-1},{-1,-1},{1,-1}},matrix{{2},{2},{-1},{0},{0}}) ",
	  " C = tailCone P",
	  " rays C"
	  }
     
     }

document {
     Key => {barycentricTriangulation, (barycentricTriangulation,Polyhedron)},
     Headline => "computes a triangulation of a polytope",
     Usage => " L = barycentricTriangulation P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "L" => List => {" containing the simplices of the triangulation"}
	  },
     
     PARA{}, TT "barycentricTriangulation","  computes the triangulation of the polyhedron ",TT "P",", if it is compact, 
     i.e. a polytope, recursively. For this, it takes all facets and checks if they are simplices. If so, then 
     it takes the convex hull of these with the weighted centre of the polytope (the sum of the vertices divided 
     by the number of vertices). For those that are not simplices it takes all their facets and does the same 
     for these.",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " barycentricTriangulation P"
	  }
     
     }

document {
     Key => {volume, (volume,Polyhedron)},
     Headline => "computes the volume of a polytope",
     Usage => " v = volume P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "v" => QQ
	  },
     
     PARA{}, TT "volume"," computes the volume of a polytope. To do this, it triangulates the polytope first. The volume 
     of a simplex is |det(v_1-v_0,..,v_n-v_0)|/n!, where v_0,..,v_n are the vertices of the simplex.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " volume P"
	  }
     
     }

document {
     Key => {vertexEdgeMatrix, (vertexEdgeMatrix,Polyhedron)},
     Headline => "computes the vertex-edge-relations matrix",
     Usage => " M = vertexEdgeMatrix P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix
	  },
     
     PARA{}, TT "vertexEdgeMatrix"," computes the matrix ",TT "M"," where the columns are indexed 
     by the edges and the rows are indexed by the vertices of ",TT "P"," and has 1 as an entry 
     if the corresponding edge contains this vertex and 0 otherwise.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertexEdgeMatrix P"
	  }
     
     }

document {
     Key => {vertexFacetMatrix, (vertexFacetMatrix,Polyhedron)},
     Headline => "computes the vertex-facet-relations matrix",
     Usage => " M = vertexFacetMatrix P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix
	  },
     
     PARA{}, TT "vertexFacetMatrix"," computes the matrix ",TT "M"," where the columns are indexed 
     by the facets and the rows are indexed by the vertices of ",TT "P"," and has 1 as an entry 
     if the corresponding facet contains this vertex and 0 otherwise.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertexFacetMatrix P"
	  }
     
     }

document {
     Key => {affineHull, (affineHull,Polyhedron)},
     Headline => "computes the affine hull of a polyhedron",
     Usage => " Q = affineHull P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the affine hull of a polyhedron. This is the affine subspace with the same 
     dimension as the polyhedron, containing the polyhedron.",
     
     EXAMPLE {
	  " P = stdSimplex 3",
	  " Q = affineHull P",
	  " linealitySpace Q"
	  }
     
     }

document {
     Key => affineImage,
     Headline => "computes the affine image of a cone or polyhedron"
     }

document {
     Key => {(affineImage,Matrix,Cone,Matrix), (affineImage,Matrix,Cone),
	  (affineImage,Cone,Matrix)},
     Headline => "computes the affine image of a cone",
     Usage => " C1 = affineImage(A,C,b) \nC1 = affineImage(A,C) \nC1 = affineImage(C,b)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "C" => Cone,
	  "b" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "C1" => {" of class ",TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, TT "A"," must be a matrix from the ambient space of the cone ",TT "C"," to some 
     other target space and ",TT "b"," must be a vector in that target space, i.e. the number of 
     columns of ",TT "A"," must equal the ambient dimension of ",TT "C"," and ",TT "A"," and ",TT "b"," 
     must have the same number of rows. Then ",TT "affineImage"," computes the 
     polyhedron ",TT "{(A*c)+b | c in C}"," and the cone ",TT "{A*c | c in C}"," if ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone.",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "This Cone can be mapped to the positive orthant:",
     
     EXAMPLE {
	  " A = matrix  {{-5,7,1},{1,-5,7},{7,1,-5}}", 
	  " C1 = affineImage(A,C)",
	  " rays C1",
	  }
     
     }

document {
     Key => {(affineImage,Matrix,Polyhedron,Matrix), (affineImage,Matrix,Polyhedron), 
	  (affineImage,Polyhedron,Matrix)},
     Headline => "computes the affine image of a polyhedron",
     Usage => " P1 = affineImage(A,P,v) \nP1 = affineImage(A,P) \nP1 = affineImage(P,v)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "P" => Polyhedron,
	  "v" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "A"," must be a matrix from the ambient space of the polyhedron ",TT "P"," to some 
     other target space and ",TT "v"," must be a vector in that target space, i.e. the number of 
     columns of ",TT "A"," must equal the ambient dimension of ",TT "P"," and ",TT "A"," and ",TT "v"," 
     must have the same number of rows. Then ",TT "affineImage"," computes the 
     polyhedron ",TT "{(A*p)+v | p in P}"," where ",TT "v"," is set to 0 if omitted and ",TT "A"," is the 
     identity if omitted.",
     
     PARA{}, "For example, consider the following two dimensional polytope:",
     
     EXAMPLE {
	  " P = convexHull matrix {{-2,0,2,4},{-8,-2,2,8}}",
	  },
     
     PARA{}, "This polytope is the affine image of the square:",
     
     EXAMPLE {
	  " A = matrix {{-5,2},{3,-1}}",
	  " v = matrix {{5},{-3}}",
	  " Q = affineImage(A,P,v)",
	  " vertices Q",
	  }
          
     }

document {
     Key => affinePreimage,
     Headline => "computes the affine preimage of a cone or polyhedron"
     }

document {
     Key => {(affinePreimage,Matrix,Cone,Matrix), (affinePreimage,Matrix,Cone), 
	  (affinePreimage,Cone,Matrix)},
     Headline => "computes the affine preimage of a cone",
     Usage => " C1 = affinePreimage(A,C,b) \nC1 = affinePreimage(A,C) \nC1 = affinePreimage(C,b)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "C" => Cone,
	  "b" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "C1" => {" of class ",TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, TT "A"," must be a matrix from some source space to the ambient space of ",TT "C"," and ",TT "b"," must be 
     a vector in that ambient space, i.e. the number of rows of ",TT "A"," must equal the ambient dimension of ",TT "C"," 
     and the number of rows of ",TT "b",". ",TT "affinePreimage"," then computes the 
     polyhedron ",TT "{q | (A*q)+b in C}"," or the cone ",TT "{q | (A*q) in C}"," if ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone:",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "We can look at its preimage under the following map:",
     
     EXAMPLE {
	  " A = matrix  {{-5,7,1},{1,-5,7},{7,1,-5}}", 
	  " C1 = affinePreimage(A,C)",
	  " rays C1",
	  }
     
     }

document {
     Key => {(affinePreimage,Matrix,Polyhedron,Matrix), (affinePreimage,Matrix,Polyhedron),  
	  (affinePreimage,Polyhedron,Matrix)},
     Headline => "computes the affine preimage of a polyhedron",
     Usage => " P1 = affinePreimage(A,P,v) \nP1 = affinePreimage(A,P) \nP1 = affinePreimage(P,v)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "P" => Polyhedron,
	  "v" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "A"," must be a matrix from some source space to the ambient space of the polyhedron ",TT "P"," 
     and ",TT "v"," must be a vector in that ambient space, i.e. the number of 
     rows of ",TT "A"," must equal the ambient dimension of ",TT "P"," and the number of rows 
     of ",TT "v",". ",TT "affinePreimage"," then computes the polyhedron ",TT "{q | (A*q)+v in P}"," 
     where ",TT "v"," is set to 0 if omitted and ",TT "A"," is the identity if omitted.",
     
     PARA{}, "For example, consider the following two dimensional polytope",
     
     EXAMPLE {
	  " P = convexHull matrix {{-2,0,2,4},{-8,-2,2,8}}",
	  },
     
     PARA{}, "and its affine preimage under the following map:",
     
     EXAMPLE {
	  " A = matrix {{-5,2},{3,-1}}",
	  " v = matrix {{5},{-3}}",
	  " Q = affinePreimage(A,P,v)",
	  " vertices Q",
	  }
     
     }

document {
     Key => {bipyramid, (bipyramid,Polyhedron)},
     Headline => "computes the bipyramid over a polyhedron",
     Usage => " Q = bipyramid P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "The ",TT "bipyramid"," over a ",TO Polyhedron," in n-space is constructed by 
     embedding the Polyhedron into (n+1)-space, computing the barycentre of the vertices, 
     which is a point in the relative interior, and taking the convex hull of the embedded 
     Polyhedron and the barycentre ",TT "x {+/- 1}",".",
     
     PARA{}, "As an example, we construct the octahedron as the bipyramid over the square 
     (see ",TO hypercube,").",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " Q = bipyramid P",
	  " vertices Q",
	  }
     
     }

document {
     Key => {ccRefinement, (ccRefinement,Matrix)},
     Headline => "computes the coarsest common refinement of a set of rays",
     Usage => " F = ccRefinement R",
     Inputs => {
	  "R" => Matrix
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The coarsest common refinement of a set of rays ",TT "R"," is the common refinement 
     of all possible triangulations  of the rays.",
     
     PARA{}, "For example, consider a three dimensional cone with four rays:",
     
     EXAMPLE {
	  " R = matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}"
	  },
     
     PARA{}, "The coarsest common refinement has a fifth ray and consists of four cones.",
     
     EXAMPLE {
	  " F = ccRefinement R",
	  " rays F"
	  }
     
     }

-- document {
--      Key => {(polyhedron,Cone)},
--      Headline => "converts a cone to class Polyhedron",
--      Usage => " P = polyhedron C",
--      Inputs => {
-- 	  "C" => Cone
-- 	  },
--      Outputs => {
-- 	  "P" => Polyhedron
-- 	  },
     
--      PARA{}, "Every ",TO Cone," is in particular a ",TO Polyhedron,". ",TT "polyhedron"," 
--      converts the cone into the same cone but of class ",TO Polyhedron,".",
     
--      PARA{}, "Consider the positive orthant in ",TO QQ,"^3:",
     
--      EXAMPLE {
-- 	  " C = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}}"
-- 	  },
     
--      PARA{}, "If we want to consider the positive orthant not as cone but as a polyhedron we 
--      apply ",TT "polyhedron",":",
     
--      EXAMPLE {
-- 	  " P = polyhedron C"
-- 	  },
     
--      PARA{}, "Although, they are the same geometric object but of different classes, Polyhedra 
--      considers them not as equal:",
     
--      EXAMPLE {
-- 	  " P === C"
-- 	  }
--      }

document {
     Key => {(directProduct,Cone,Cone), (directProduct,Cone,Polyhedron), 
	  (directProduct,Polyhedron,Cone), (directProduct,Polyhedron,Polyhedron)},
     Headline => "computes the direct product of polyhedra and cones",
     Usage => " P = directProduct(X,Y)",
     Inputs => {
	  "X" => {TO Cone," or ",TO Polyhedron},
	  "Y" => {TO Cone," or ",TO Polyhedron}
	  },
     Outputs => {
	  "P" => {TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, "The ", TT "directProduct"," of ",TT "X"," and ",TT "Y"," is the polyhedron 
     ",TT "{(x,y) | x in X, y in Y}"," in the direct product of the ambient spaces. If 
     ",TT "X"," and ",TT "Y"," are both cones, then the direct product is again a cone 
     and the output is then also given as a ",TO Cone,", otherwise as a ",TO Polyhedron,".",
     
     EXAMPLE {
	  " P = hypercube 1",
	  " Q = hypercube 2",
	  " directProduct(P,Q) == hypercube 3"
	  },
     
     PARA{}, "See also ",TO (symbol *,Cone,Cone),", ",TO (symbol *,Cone,Polyhedron),", ",
              TO (symbol *,Polyhedron,Cone),", and ",TO (symbol *,Polyhedron,Polyhedron),"."
     
     }

document {
     Key => (directProduct,Fan,Fan),
     Headline => "computes the direct product of two fans",
     Usage => " F = directProduct(F1,F2)",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "directProduct"," of two fans is the fan given by ",TT "C = C1 x C2"," 
     for all cones ",TT "C1 in F1"," and ",TT "C2 in F2"," in the direct product of the 
     ambient spaces.",
     
     EXAMPLE {
	  " F1 = normalFan hypercube 1",
	  " F2 = normalFan hypercube 2",
	  " F = directProduct(F1,F2)",
	  " F == normalFan hypercube 3"
	  },
     
     PARA{}, "See also ", TO (symbol *,Fan,Fan),"."
     
     }

document {
     Key => {dualCone, (dualCone,Cone)},
     Headline => " computes the dual Cone",
     Usage => " Cv = dualCone C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "Cv" => Cone
	  },
     
     PARA{}, "The dual cone of ",TT "C"," in ",TO QQ,"^n is the cone in the dual ambient 
     space (",TO QQ,"^n)^*, given 
     by ",TT "{p in (",TO QQ,TT "^n)^* | p*c >= 0 for all c in C}",".",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,2},{2,1}}",
	  " Cv = dualCone C",
	  " rays Cv"
	  }
     
     }

document {
     Key => { faceFan, (faceFan,Polyhedron)},
     Headline => " computes the fan generated by the cones over the faces",
     Usage => " F = faceFan P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "For a polyhedron with the origin in its relative interior, the face fan is the fan 
     generated by the cones over the faces of the polytope. Hence the origin must be in the relative interior.",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " F = faceFan P",
     " raysF = rays F",
	  "apply(maxCones F, mc -> raysF_mc)"
	  }
     }

document {
     Key => { imageFan, (imageFan,Matrix,Cone)},
     Headline => " computes the fan of the image",
     Usage => " F = imageFan(M,C)",
     Inputs => {
	  "M" => Matrix,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, TT "M"," must be a matrix from the ambient space of the ",TO Cone," ",TT "C"," to some 
     target space. The ",TT "imageFan"," is the common refinement of the images of all faces of ",TT "C",".",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{2,1,-1,-3},{1,1,1,1},{0,1,-1,0}}",
	  " M = matrix {{1,0,0},{0,1,0}}",
	  " F = imageFan(M,C)",
	  " rays F"
	  }
     
     }

document {
     Key => { minkowskiSum, (minkowskiSum,Cone,Cone), (minkowskiSum,Cone,Polyhedron), 
	  (minkowskiSum,Polyhedron,Cone), (minkowskiSum,Polyhedron,Polyhedron)},
     Headline => " computes the Minkowski sum of two convex objects",
     Usage => " Q = minkowskiSum(X,Y)",
     Inputs => {
	  "X" => {TO Cone," or ",TO Polyhedron},
	  "Y" => {TO Cone," or ",TO Polyhedron}
	  },
     Outputs => {
	  "Q" => {TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, "The Minkowski sum of ",TT "X"," and ",TT "Y"," is the polyhedron 
     ",TT "X + Y = {x + y | x in X, y in Y}",". If ",TT "X"," and ",TT "Y"," are both 
     cones, then their Minkowski sum is their positive hull, which is a cone, so the 
     output is a ",TO Cone,". Otherwise the output is a ",TO Polyhedron,". ",TT "X"," 
     and ",TT "Y"," have to lie in the same ambient space.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{0,1,-1},{0,-1,-1}}",
	  " P2 = convexHull matrix {{0,1,-1},{0,1,1}}",
	  " Q = minkowskiSum(P1,P2)",
	  " vertices Q"
	  },
     
     PARA{}, "See also ",TO (symbol +,Cone,Cone),", ",TO (symbol +,Cone,Polyhedron),", ",
              TO (symbol +,Polyhedron,Cone),", and ",TO (symbol +,Polyhedron,Cone),"."
     
     }

document {
     Key => {normalFan, (normalFan,Polyhedron)},
     Headline => "computes the normalFan of a polyhedron",
     Usage => " F = normalFan P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "normalFan"," of a ",TO Polyhedron," is the fan generated by the 
     cones ",TT "C_v"," for all vertices ",TT "v"," of the ",TO Polyhedron,", 
     where ",TT "C_v"," is the dual Cone of the positive Hull of ",TT "P-v",". 
     If ",TT "P"," is compact, i.e. a polytope, then the normalFan is complete.",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0,0},{0,1,0}}",
	  " F = normalFan P",
     " raysF = rays F",
	  " apply(maxCones F, mc -> raysF_mc)"
	  }
     
     }

document {
     Key => {polar, (polar,Polyhedron)},
     Headline => " computes the polar of a polyhedron",
     Usage => " Pv = polar P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Pv" => Polyhedron
	  },
     
     PARA{}, "The polar polyhedron of ",TT "P"," in n-space is the polyhedron in the dual 
     space given by ",TT "{v in (QQ^n)^* | v*p >= -1 for all p in P}",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " Q = polar P",
	  " Q == crossPolytope 3"
	  }
     
     }

document {
     Key => {polarFace, (polarFace,Polyhedron,Polyhedron)},
     Headline => " computes the dual face of the polar polyhedron",
     Usage => " fv = polarFace f",
     Inputs => {
	  "f" => Polyhedron,
     "P" => Polyhedron
	  },
     Outputs => {
	  "fv" => Polyhedron
	  },
     
     PARA{}, "Given a polyhedron ",TT "f"," which is a face of a polyhedron ",TT "P"," the function ",TT "polarFace"," 
     computes the ",TO polar," ",TT "P'"," of ",TT "P"," and the corresponding face of ",TT "P'"," on which 
     all points of ",TT "f"," attain their minimum. Note that this function only works correctly for polyhedra 
     with the origin in its relative interior.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " f = first faces(1,P)",
     " f = convexHull (vertices P)_(f#0)",
	  " fv = polarFace(f, P)",
	  " vertices fv"
	  },
     
     PARA{}, "If ",TT "f"," is not a face of another polytope, then it considers ",TT "f"," as a face of itself. 
     Thus, it computes the polar of ",TT "f",", and returns the empty polyhedron as a face of the polar of ",
     TT "f",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " polarFace(P, P)"
	  }
     
     }
	  

document {
     Key => {pyramid, (pyramid,Polyhedron)},
     Headline => "computes the pyramid over a polyhedron",
     Usage => " Q = pyramid P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, TT "pyramid"," takes the polyhedron ",TT "P"," with ambient dimension n 
     and embeds it into ",TO QQ,"^(n+1) on height 0 with respect to the new last variable. 
     Then it computes the convex hull of the embedded ",TT "P"," and the point (0,...,0,1).", 
     
     EXAMPLE {
	  " P = hypercube 2",
	  " Q = pyramid P",
	  " vertices Q"
	  }
     
     }

document {
     Key => {sublatticeBasis, (sublatticeBasis,Matrix), (sublatticeBasis,Polyhedron)},
     Headline => "computes a basis for the sublattice generated by integral vectors or lattice points of a polytope",
     Usage => " B = sublatticeBasis M \nB = sublatticeBasis P",
     Inputs => {
	  "M" => Matrix => {" over ",TO ZZ," with each column representing a sublattice generator"},
	  "P" => Polyhedron,
	  },
     Outputs => {
	  "B" => {"A matrix over ", TO ZZ," containing a sublattice basis"}
	  },

     PARA{}, TT "sublatticeBasis"," computes a basis for the sublattice generated by the columns of",TT "M"," or 
     by the lattice points of",TT "P",".",

      EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " sublatticeBasis P"
	    }
     }

document {
     Key => {toSublattice, (toSublattice,Polyhedron)},
     Headline => "calculates the preimage of a polytope in the sublattice generated by its lattice points",
     Usage => "Q = toSublattice P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "Q" => Polyhedron => {"preimage of P in the sublattice generated by its lattice points"}
	  },

     PARA{}, TT "toSublattice"," can only be applied to polytopes, i.e. compact polyhedra. It
     calculates a basis of the sublattice generated by its lattice points, and then takes the affine 
     preimage under the corresponding map.",

     EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " toSublattice P"
	    }

     }

document {
     Key =>  {crossPolytope, (crossPolytope,ZZ), (crossPolytope,ZZ,QQ), (crossPolytope,ZZ,ZZ)},
     Headline => "computes the d-dimensional crosspolytope with diameter 2s",
     Usage => " P = crossPolytope(d,s)",
     Inputs => {
	  "d" => ZZ => {" strictly positive"},
	  "s" => {TO ZZ," or ",TO QQ,", positive (optional)"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional ",TT "crossPolytope"," with diameter ",TT "s"," is the 
     convex hull of ",TT "+/- s"," times the standard basis in ",TO QQ,"^d. If ",TT "s"," is omitted 
     it is set to 1.",
     
     EXAMPLE {
	  " P = crossPolytope(3,3/2)",
	  " vertices P"
	  }
     
     }

document {
     Key => {cyclicPolytope, (cyclicPolytope,ZZ,ZZ)},
     Headline => "computes the d dimensional cyclic polytope with n vertices",
     Usage => " P = cyclicPolytope(d,n)",
     Inputs => {
	  "d" => ZZ => {"strictly positive"},
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional ",TT "cyclicPolytope"," with ",TT "n"," vertices 
     is the convex hull of ",TT "n"," points on the moment curve in ",TO QQ,"^",TT "d",". The 
     moment curve is defined by ",TT "t -> (t,t^2,...,t^d)"," and the function takes the 
     points ",TT "{0,...,n-1}",".",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertices P"
	  }
     
     }

document {
    Key => {ehrhart, (ehrhart,Polyhedron)},
    Headline => "calculates the Ehrhart polynomial of a polytope",
    Usage => "f = ehrhart P",
    Inputs => {
         "P" => Polyhedron => {"which must be compact"}
         },
    Outputs => {
         "f" => RingElement => {"Ehrhart polynomial as element of QQ[x]"}
         },

    PARA{}, TT "ehrhart"," can only be applied to polytopes, i.e. compact polyhedra. 
    To calculate the Ehrhart polynomial, the number of lattice points in the first n 
    dilations of the polytope are calculated, where n is the dimension of the polytope. 
    A system of linear equations is then solved to find the polynomial.",

    EXAMPLE {
         " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
         " ehrhart P"
           }

    }	

document {
     Key => {emptyPolyhedron, (emptyPolyhedron,ZZ)},
     Headline => "generates the empty polyhedron in n-space",
     Usage => " P = emptyPolyhedron n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "Generates the empty polyhedron in ",TT "n","-space.",
     
     EXAMPLE {
	  " P = emptyPolyhedron 3"
	  }
     
     }

document {
     Key => {hirzebruch, (hirzebruch,ZZ)},
     Headline => "computes the fan of the r-th Hirzebruch surface",
     Usage => " F = hirzebruch r",
     Inputs => {
	  "r" => ZZ => {"positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "r","-th Hirzebruch surface is the ",TO Fan," in ",TO QQ,"^2 generated 
     by the cones <e_1,e_2>, <e_1,-e_2>, <-e_1+r*e_2,-e_2> and <-e_1+r*e_2,e_2>.",
     
     EXAMPLE {
	  " F = hirzebruch 3",
     " raysF = rays F",
	  " apply(maxCones F, mc -> raysF_mc)"
	  }
     
     }


document {
     Key => {newtonPolytope, (newtonPolytope,RingElement)},
     Headline => "computes the Newton polytope of a polynomial",
     Usage => "P = newtonPolytope f",
     Inputs => {
	  "f" => RingElement
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "newtonPolytope"," of ",TT "f"," is the convex hull of its 
     exponent vectors in n-space, where n is the number of variables in the ring.",
     
     PARA{}, "Consider the Vandermond determinant in 3 variables:",
     
     EXAMPLE {
	  " R = QQ[a,b,c]",
	  " f = (a-b)*(a-c)*(b-c)"
	  },
     
     PARA{}, "If we compute the Newton polytope we get a hexagon in ",TT "QQ","^3.",
     
     EXAMPLE {
	  " P = newtonPolytope f"
	  }
     
     }

document {
     Key => {posOrthant, (posOrthant,ZZ)},
     Headline => "generates the positive orthant in n-space",
     Usage => " C = posOrthant n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Generates the positive orthant in the n dimensional space as a cone.",
     
     EXAMPLE {
	  " C = posOrthant 3",
	  " rays C"
	  }
     }

document {
     Key => {secondaryPolytope, (secondaryPolytope, Polyhedron)},
     Headline => "computes the secondary polytope of a compact polyhedron",
     Usage => " Q = secondaryPolytope P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "The secondary polytope parametrises the regular subdivisions of a polytope. See ...
     ",TT "to be added",".",
     
     EXAMPLE {
	  " P = crossPolytope 2",
	  " Q = secondaryPolytope P",
	  " vertices Q"
	  }
     }

document {
     Key => {statePolytope, (statePolytope,Ideal)},
     Headline => "computes the state polytope of a homogeneous ideal",
     Usage => " P = statePolytope I",
     Inputs => {
	  "I" => Ideal => {"which must be homogeneous"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "A ",TT "statePolytope"," of an Ideal ",TT "I"," has as normalFan 
     the Groebner fan of the ideal. We use the construction by Sturmfels, see Algorithm 3.2 in ", 
     HREF("http://math.berkeley.edu/~bernd/index.html", "Bernd Sturmfels'"), " ", EM "Groebner Bases and 
     Convex Polytopes", ", volume 8 of University Lecture Series. American Mathematical Society, 
     first edition, 1995.",
     
     PARA{}, "Consider the following ideal in a ring with 3 variables:",
     
     EXAMPLE {
	  " R = QQ[a,b,c]",
	  " I = ideal (a-b,a-c,b-c)"
	  },
     
     PARA{}, "The state polytope of this ideal is a triangle in 3 space, because the ideal has three 
     initial ideals:",
     
     EXAMPLE {
	  " statePolytope I"
	  },
     
     PARA{}, "The generators of the three initial ideals are given in the first part of the result."
     
     }

document {
     Key => {stdSimplex, (stdSimplex,ZZ)},
     Headline =>  "generates the d-dimensional standard simplex",
     Usage => " P = stdSimplex d",
     Inputs => {
	  "d" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional standard simplex is the convex hull of the 
     standard basis in ",TO QQ,"^(d+1).",
     
     EXAMPLE {
	  " P = stdSimplex 2",
	  " vertices P"
	  }
     
     }

document {
     Key => (symbol ?,Cone,Cone),
     Headline => "compares the Cones",
     Usage => " b = C1 ? C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "b" => {TT ">"," or ",TT "<"," or ",TT "="}
	  },
     
     PARA{}, "This induces an order on Cones. ",TT "C1"," is greater then ",TT "C2"," if 
     its ambient dimension is greater, if this is equal then if its dimension is higher and 
     if this is equal if it has the higher ordered rays and lineality space.",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,0},{0,1},{1,1}}",
	  " C2 = coneFromVData matrix {{1,0,1},{0,1,0},{1,1,0}}",
	  " C1 ? C2"
	  }
     }

document {
     Key => (symbol *,Cone,Cone),
     Headline => "computes the direct product of two cones",
     Usage => "  C = C1 * C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Computes the direct product of ",TT "C1"," and ",TT "C2",". This is the cone 
     ",TT "{(x,y) | x in C1, y in C2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO (directProduct, Cone, Cone),".",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,2},{2,1}}",
	  " C2 = coneFromVData matrix {{1}}",
	  " C = C1 * C2",
	  " rays C"
	  }
     
     }

document {
     Key => (symbol *,Cone,Polyhedron),
     Headline => "computes the direct product of a cone and a polyhedron",
     Usage => "  Q = C * P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "C"," and ",TT "P",". This is the 
     polyhedron ",TT "{(c,p) | c in C, p in P}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO (directProduct, Cone, Cone),".",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1,2},{2,1}}",
	  " P =convexHull matrix {{1},{-1}}",
	  " Q = C * P",
	  " (vertices Q,rays Q)"
	  }
     
     }

document {
     Key => (symbol *,Polyhedron,Cone),
     Headline => "computes the direct product of a polyhedron and a cone",
     Usage => "  Q = P * C",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "P"," and ",TT "C",". This is the polyhedron 
     ",TT "{(p,c) | p in P, x in C}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO (directProduct, Cone, Cone),".",
     
     EXAMPLE {
	  " P =convexHull matrix {{1},{-1}}",
	  " C = coneFromVData matrix {{1,2},{2,1}}",
	  " Q = P * C",
	  " (vertices Q,rays Q)"
	  }
     
     }

document {
     Key => (symbol *,Polyhedron,Polyhedron),
     Headline => "computes the direct product of two polyhedra",
     Usage => "  Q = P1 * P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "P1"," and ",TT "P2",".This is the polyhedron 
     ",TT "{(x,y) | x in P1, y in P2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO (directProduct, Cone, Cone),".",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,-1,0,0},{0,0,1,-1}}",
	  " P2 = convexHull matrix {{1},{-1}}",
	  " P = P1 * P2",
	  " vertices P"
	  }
     
     }

document {
     Key => (symbol *,Fan,Fan),
     Headline => "computes the direct product",
     Usage => "  F = F1 * F2",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Computes the direct product of two fans. This is the fan given by ",TT "C=C1 x C2"," 
     for all cones ",TT "C1 in F1"," and ",TT "C2 in F2",", in the direct product of the 
     ambient spaces.",
     
     PARA{}, "See also ",TO (directProduct,Fan,Fan),".",
     
     EXAMPLE {
	  " F1 = normalFan hypercube 1",
	  " F2 = normalFan hypercube 2",
	  " F = F1 * F2",
	  " F == normalFan hypercube 3"
	  }
          
     }

document {
     Key => {(symbol *,QQ,Polyhedron), (symbol *,ZZ,Polyhedron)},
     Headline => "rescales a polyhedron by a given positive factor",
     Usage => " Q = k * P",
     Inputs => {
	  "k" => {TO ZZ," or ",TO QQ,", strictly positive"},
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Rescales the ",TO Polyhedron," by the strictly positive factor
     ",TT "k",".",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " k = 3",
	  " Q = k * P",
	  " vertices Q"
	  }
     }

document {
     Key => (symbol +,Cone,Cone),
     Headline => "computes the Minkowski sum of two cones",
     Usage => "  C = C1 + C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "C1"," and ",TT "C2",". This is the cone 
     ",TT "C1 + C2 = {x + y | x in C1, y in C2}",". Note that ",TT "C1"," and ",TT "C2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " C1 = coneFromVData matrix {{1,2,3},{2,3,1},{3,1,2}}",
	  " C2 = coneFromVData matrix {{1},{0},{0}}",
	  " C = C1 + C2",
	  " rays C"
	  }
     
     }

document {
     Key => (symbol +,Cone,Polyhedron),
     Headline => "computes the Minkowski sum of a cone and a polyhedron",
     Usage => "  Q = C + P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "C"," and ",TT "P",".This is the polyhedron 
     ",TT "C + P = {c + p | c in C, p in P}",". Note that ",TT "C"," and ",TT "P"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " C = coneFromVData matrix {{1},{2},{0}}",
	  " P = hypercube 3",
	  " Q = C + P",
	  " (vertices Q,rays Q)"
	  }
     
     }

document {
     Key => (symbol +,Polyhedron,Cone),
     Headline => "computes the Minkowski sum of a polyhedron and a cone",
     Usage => "  Q = P + C",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "P"," and ",TT "C",". This is the polyhedron 
     ",TT "P + C = {p + c | p in P, c in C}",". Note that ",TT "P"," and ",TT "C"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " C = coneFromVData matrix {{1},{2}}",
	  " Q = P + C",
	  " (vertices Q,rays Q)"
	  }
     
     }

document {
     Key => (symbol +,Polyhedron,Polyhedron),
     Headline => "computes the Minkowski sum of two polyhedra",
     Usage => "  Q = P1 + P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "P1"," and ",TT "P2",".This is the polyhedron 
     ",TT "P1 + P2 = {x + y | x in P1, y in P2}",". Note that ",TT "P1"," and ",TT "P2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,0,0},{0,1,0}}",
	  " P2 = convexHull matrix {{-1,0,0},{0,-1,0}}",
	  " P = P1 + P2",
	  " vertices P"
	  }
          
     }

document {
     Key => (symbol ==,Cone,Cone),
     Headline => "equality",
     Usage => " C1 == C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  }
     
     }

document {
     Key => (symbol ==,Fan,Fan),
     Headline => "equality",
     Usage => " F1 == F2",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  }
     
     }

document {
     Key => (symbol ==,Polyhedron,Polyhedron),
     Headline => "equality",
     Usage => " P1 == P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  }
     
     }

document {
     Key => {(dim,PolyhedralObject)},
     Headline => "computes the dimension of a cone, polyhedron, fan or polyhedral complex",
     Usage => " d = dim PO",
     Inputs => {
	  "PO" => PolyhedralObject
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     
     EXAMPLE {
	  " C = coneFromVData matrix {{2,3},{3,2}}",
	  " dim C"
	  },
     
     PARA{}, "Returns the dimension of a cone.",
     
     EXAMPLE {
	  " F = hirzebruch 3",
	  " dim F"
	  },
     
     PARA{}, "Returns the dimension of a fan. This 
     is the maximal dimension of all cones of 
     the fan.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}}",
	  " dim P"
	  },
     
     PARA{}, "Returns the dimension of a polyhedron.",
     
     EXAMPLE {
	  " PC = polyhedralComplex crossPolytope 3",
	  " dim PC"
	  },

     PARA{}, "Returns the dimension of a polyhedral complex. This 
     is the maximal dimension of all polyhedra of the complex."
     
}



document {
     Key => {saveSession,(saveSession,String)},
     Headline => "save the actual Polyhedra session to a file",
     Usage => " saveSession F",
     Inputs => {
	  "F" => String
	  },
     
     PARA{}, "All convex polyhedral objects (",TO Cone,",",TO Fan,",",TO Polyhedron,") that have been assigned 
     to a ",TO Symbol," will be saved into the file ",TT "F",". If the package ",TT "PPDivisor"," is loaded, then 
     also all ",TT "PolyhedralDivisors"," are saved into ",TT "F",". Also every ",TO List," or ",TO Sequence," 
     containing any of the above types or lists and sequences of them in arbitrary nested depth of lists is saved.",
     
     PARA{}, "To recover the session simply call ",TT "load F",". It is not necessary that ",TT "Polyhedra"," is already 
     loaded (if not, it will be) and also ",TT "PPDivisor"," is loaded if it was loaded when the session had been saved."
     
     }


