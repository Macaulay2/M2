---------------------------------------
-- DOCUMENTATION
---------------------------------------

document {
     	Key => Polyhedra,
	Headline => "for computations with convex polyhedra, cones, and fans",
	
	"A rational convex ", TO Polyhedron, " is the intersection of finitely many affine half-spaces 
	over ", TO QQ, " or equivalently, the convex hull of a finite set of vertices and rays. 
	A rational convex polyhedral ", TO Cone, " is the intersection of finitely many linear half-spaces 
	over ", TO QQ, " or equivalently, the positive hull of a finite set of rays. A ", TO Fan, " is 
	a finite collection of cones such that for each cone all its faces are in the fan and for two cones 
	in the fan the intersection is a face of each.",
	
	PARA{}, TT "Polyhedra", " uses the ", TO FourierMotzkin, " package by ", 
	HREF("http://www.mast.queensu.ca/~ggsmith", "Gregory G. Smith"), ". Each polyhedron or cone is 
	saved in both descriptions and a fan is saved as the list of its generating cones.",
	
	PARA{}, "Here are some examples illustrating the main uses of this package.",
	UL {
	     {TO "Working with polyhedra"},
	     {TO "Working with cones"},
	     {TO "Working with fans"}
	     },
	
	PARA{}, "For an introduction to polyhedra and cones, we recommend ",
	HREF("http://www.mi.fu-berlin.de/math/groups/discgeom/ziegler/", "Gunter
	M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
	Texts in Mathematics 152, Springer-Verlag, New York, 1995.",
	
	PARA{}, "The authors would like to thank ",
   UL {
         {HREF("https://faculty.math.illinois.edu/~dan/", "Daniel R. Grayson")},
         {HREF("http://people.cs.uchicago.edu/~nilten/", "Nathan Ilten")},
         {HREF("http://page.math.tu-berlin.de/~panizzut/", "Marta Panizzut")},
         {HREF("http://www.mast.queensu.ca/~ggsmith/", "Gregory G. Smith")},
         {HREF("http://astaal.be/", "Andrew P. Staal")},
         {HREF("http://pi.math.cornell.edu/~mike/", "Michael Stillman")},
         {HREF("http://www.mi.fu-berlin.de/math/groups/ag-algebra/members/mitarbeiter/Winz.html", "Anna-Lena Winz")}
      },
   "for code contributions, bug reports, and helpful discussions concerning this package."
	
	}

