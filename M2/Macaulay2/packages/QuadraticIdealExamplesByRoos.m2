newPackage(
	"QuadraticIdealExamplesByRoos",
	Version => "0.1",
	Date => "June, 2023",
	AuxiliaryFiles => false,
	Authors => {{Name => "David Eisenbud", Email => "de@msri.org"},
	    {Name => "Michael Perlman", Email => "mperlman@umn.edu"}, 
	    {Name => "Ritvik Ramkumar", Email => "ritvikr@cornell.edu"},
	    {Name => "Deepak Sireeshan", Email => "dsbx7@umsystem.edu"},
	    {Name => "Aleksandra Sobieska", Email => "asobieska@wisc.edu"},
	    {Name => "Teresa Yu", Email => "twyu@umich.edu"},
	    {Name => "Jacob Zoromski", Email => "jzoromsk@nd.edu"} },
	Headline => "Examples of Quadratic Ideals with Embedding Dimension Four by Jan-Erik Roos",
	PackageExports => {"Depth"},
	PackageImports => {"Classic"},
	Keywords => {"Examples and Random Objects"})
export {
 "roosTable", 
 "higherDepthTable", 
 "depthZeroTable",
 "almostKoszul",
 "roosIsotopes",
 "onedimToricIrrationalPoincare",
 "twodimToricIrrationalPoincare"
}


---TO DO: make this a method to allow user to pick coefficient field
roosTable = (S = QQ[x,y,z,u];
L= {
ideal(0_S),
ideal "x2",
ideal "x2, y2",
ideal "x2, xy",
ideal "x2, y2, z2",
ideal "x2, y2 + xz, yz",
ideal "x2 + y2, z2 + u2, xz + yu",
ideal "x2, y2, xz",
ideal "x2, xy, y2",
ideal "x2, xy, xz",
ideal "x2, y2, z2, u2",
ideal "x2 + xy, y2 + xu, z2 + xu, u2 + zu",
ideal "x2 + z2 + u2, y2, xz, yu + zu",
ideal "xz, y2, z2 + u2, yu + zu",
ideal "xz, y2, yz + u2, yu + zu",
ideal "xy + z2 + yu, y2, yu + zu, xz",
ideal "xz, yz + xu, y2, yu + zu",
ideal "x2, y2, z2, yu",
ideal "xz, y2, yu + zu, u2",
ideal "xz,y2,yu+z2,yu+zu",
ideal "xz,y2,z2,yu+zu",
ideal "x2+xy,xu,xz+yu,y2",
ideal "xz,xu,y2,z2",
ideal "xz,y2,yz+z2,yu+zu",
ideal "x2,xy,xz,u2",
ideal "xz,y2,yu,zu",
ideal "xy,xz,y2,yz",
ideal "x2,xy,xz,xu",
ideal "x2+xy,y2+xu,z2+xu,zu+u2,yz",
ideal "xy+u2,xz,x2+z2+u2,y2,yu+zu",
ideal "x2-y2,y2-z2,z2-u2,xz+yu,-x2+xy-yz+xu",
ideal "x2+z2,xz,y2,yu+zu,u2",
ideal "x2+xy,y2+yz,y2+xu,z2+xu,zu+u2",
ideal "x2+xy+yu+u2,y2,xz,x2+z2+u2,yu+zu",
ideal "x2+z2+u2,y2,xz,xy+yz+yu,yu+zu",
ideal "x2+y2,z2,u2,yz-yu,xz+zu",
ideal "x2,y2,xy-zu,yz-xu,(x-y)(z-u)",
ideal "x2,y2,z2,zu,u2",
ideal "x2+yz+u2,xz+z2 +yu,xy, xu, zu",
ideal "x2-xu,xu-y2,y2-z2,z2-u2,xz+yu",
ideal "xy,y2,z2,zu,u2",
ideal "x2 +xy,zu,y2,xu,xz+yu",
ideal "x2,y2,yz,zu,u2",
ideal "xz,yz,y2,yu+zu,z2+u2",
ideal "xy+yz,xy+z2 +yu,yu + zu, y2, xz",
ideal "x2,xy,yz,zu,u2",
ideal "x2+xy,y2,xu,xz+yu,-x2+xz-yz",
ideal "xy,z2+yu,yu+zu,y2,xz",
ideal "xz, y2, z2, yu, zu",
ideal "x2, xy, xz, y2, z2",
ideal "xy,xz,yz+xu,z2,zu",
ideal "x2, xy, xz, y2, yz",
ideal "y2 -u2, xz, yz, z2, zu",
ideal "x2,xz,y2,z2,yu+zu,u2",
ideal "x2 +xy,xz+yu,xu,y2,z2,zu+u2",
ideal "x2+xz+u2,xy,xu,x2 -y2,z2,zu",
ideal "x2+yz+u2,xu,x2 +xy,xz+yu, zu+u2, y2+z2",
ideal "x2 + xy, x2 + zu, y2, z2, xz+yu, xu",
ideal "x2 - y2, xy, xu, z2, zu,xz + yu",
ideal "x2 + yz + u2, xz+yu, zu, xy, z2, xu",
ideal "x2-y2, xy, z2, xu, zu, u2",
ideal "x2 - y2, xy, xu, yz+yu, z2, zu",
ideal "x2, xy, xu, y2, z2, zu",
ideal "x2 - y2, xy, z2, xu, yu, zu",
ideal "x2, xy, xz, y2, yu+z2, yu+zu",
ideal "xz, y2, yu, z2, zu, u2",
ideal "xy, xz, y2, yu, z2, zu",
ideal "x2, xy, xz,y2, yz, z2",
ideal "x2, xz, xu, xy-zu, yz, z2",
ideal "x2, xy, xz xu, y2, yz",
ideal "x2, y2, z2, u2, xy, zu, yz+xu",
ideal "x2-y2, xy, yz, zu, z2, xz+yu, xu",
ideal "x2, y2, z2, u2, zu, yu, xu",
ideal "x2, xy+z2, yz, xu, yu, zu, u2",
ideal "x2,xy,xz,xu,y2,yz,u2",
ideal "x2, xy, xz, xu, z2, zu, yu",
ideal "x2, xy, xz, xu, y2, yz, yu",
ideal "x2,xy,y2,z2,zu,u2,xz+yu,yz-xu",
ideal "x2,xy,xz,xu,y2,yu,z2,zu",
ideal "x2,xy,xz,y2,yz,yu,z2,zu",
ideal "x2,y2,z2,u2,xy,xz,yz-xu,yu,zu",
ideal "x2,xy,xz,xu,y2,zu,u2,yz,yu",
ideal "x2,y2,z2,u2,xy,xz,xu,yz,yu,zu"
};
new HashTable from for i from 1 to 83 list i => L#(i-1)
)



higherDepthIndices = splice {1..6, 8..10, 23, 26, 27, 50, 52, 68};


depthZeroIndices = (lst = toList(1..83);
     for i in higherDepthIndices do lst=delete(i,lst);
     lst
     )

higherDepthTable = new HashTable from for i in higherDepthIndices list i=> (roosTable#i); 

depthZeroTable = new HashTable from for i in depthZeroIndices list i=> (roosTable#i);




roosIsotopes = (
    S = QQ[x,y,z,u];
    L = {
	"46va" => ideal "xz + u2, xy,xu,x2,zu + y2 + z2", ---46va
        "57v2" => ideal "x2+y2+z2,xy,xu,yz,zu,xz+u2", ----57v2
        "59va" => ideal "x2-y2,xy,yz,zu,xz+u2,xu", ----59va
        "62va" => ideal "x2+yz+u2,yu,zu,xy,z2,xu", ---62va
        "63v4" => ideal "y2,xz+yu,zu,xy,z2,xu", ---63v4
        "63v8" => ideal "x2,xy,xu,yu,z2,xz+u2,y2+z2+zu",------63v8
        "63ne" => ideal "x2,xy,xz+u2,xu,y2+z2,zu", ----63ne
        "66v5" => ideal "xy,xz+u2,xu,yu,zu,z2",----66v5
        "68v" => ideal "x2,xy,xz,xu,u2,y2+z2+zu",----68v
        "71v16" => ideal "x2,y2+z2,xy,yz,zu,xz+u2,xu", ----71v16
        "71v4" => ideal "x2+u2,xy,xu,y2,yz,z2,zu",----71v4
        "71v7" => ideal "x2,y2,z2,xz+u2,xu,yz,zu",----71v7
        "71v5" => ideal "x2+xy,x2+yz,xy+y2,z2,z2,xu,zu,xz+u2",---71v5
        "72v1" => ideal "xu+u2,x2+xy,y2+xu,y2+yz,y2+yz,yu+zu,z2+xu,zu+u2",---72v1
        "72v2e" => ideal "yz,x2+xy,xz+yu,xu,z2,zu,x2+u2", ---72v2e
        "75v1" => ideal "y2,xz,yz+xu,z2,yu,zu,u2",----75v1
        "75v2" => ideal "xy,xz+u2,xu,yz+u2,yu,zu,z2",---75v2
        "78v1" => ideal "x2,y2,z2,u2,xy,xz,xu,yu",----78v1
        "78v2e" => ideal "xu,yu+xz,yz,x2,y2,z2+xy,u2,zu", ---78v2e
        "78v3v" => ideal "xu,yu+xz,yz,x2,y2,z2+xz,u2,zu",----78v3v
        "81va" => ideal "x2,y2,z2,u2,xy,xz,xu,yu,zu"---81va
        };
    new HashTable from L
    )


-----need to export and document the following two functions

onedimToricIrrationalPoincare = (degs1 := {18,24,25,26,28,30,33}; --this example is from froberg-roos 2000, lofwall-lundqvist-roos
    ker map(QQ[t], QQ[w_1 .. w_7, Degrees => degs1], apply(degs1, a -> t^a))
)

twodimToricIrrationalPoincare = (degs2 := {{36,0}, {33,3}, {30,6}, {28,8}, {26,10}, {25,11}, {24,12}, {18,18}, {0,36}};
    ker map(QQ[t,s], QQ[w_1 .. w_9, Degrees => degs2], apply(degs2, a -> t^(a#0)*s^(a#1)))
    )


 almostKoszul = method()
 almostKoszul (Ring, ZZ) := Ring => (kk, a)-> (
    --A series of examples discovered by Jan-Erik Roos:
    --
    --If kk = QQ then the resolution of kk over the output
    --ring has linear resolution for a steps but 1 quadratic syzygy
    --at the a+1-st step. 
    --It also seems to have the first socle summand
    --at the a+1-st step!
    --These phenomena are also visible with kk = ZZ/32003, at
    --least for moderate size a.
    x := symbol x; 
    y := symbol y;
    z := symbol z; 
    u := symbol u; 
    v := symbol v; 
    w := symbol w;
    S := kk[x,y,z,u,v,w];
    I := ideal (x^2,x*y,y*z,z^2,z*u,u^2,u*v,v*w,w^2,
           x*z+a*z*w-u*w,z*w+x*u+(a-2)*u*w);
    S/I
       )


---TO DO: create a function to identify non-Koszul examples (see Table 8)

      -* Documentation section *-
      
beginDocumentation()

doc ///
Key
 "QuadraticIdealExamplesByRoos"
Headline
 Examples of Quadratic Ideals with Embedding Dimension Four by Jan-Erik Roos
Description
  Text
    Quadratic ideals based on Main Theorem and Tables in "Homological properties of the homology algebra 
    of the Koszul complex of a local ring: Examples and questions" by Jan-Erik Roos, 
    Journal of Algebra 465 (2016) 399-436.
Subnodes
 "roosTable"
 "higherDepthTable"
 "depthZeroTable"
 "roosIsotopes"
 "almostKoszul"
 "onedimToricIrrationalPoincare"
 "twodimToricIrrationalPoincare"
///

doc ///
Key
 "roosTable"
Headline
 Creates hashtable of Jan-Erik Roos' examples of quadratic ideals
Usage
 H = roosTable ()
Outputs
 H: HashTable
Description
  Text
    This is based on Main Theorem and Tables 3-7 in "Homological properties of the homology
    algebra of the Koszul complex of a local ring: Examples and questions" by Jan-Erik Roos, Journal of Algebra 
    465 (2016) 399-436. The ideals in this table exemplify 83 known cases of bi-graded Poincar\'e series of 
    quadratic ideals of embedding dimension four in characteristic zero. The coefficient field is QQ.
  Example
    roosTable
///

doc ///
Key
 "higherDepthTable"
Headline
 Creates hashtable of Jan-Erik Roos' examples of quadratic ideals with positive depth
Usage
 H = higherDepthTable ()
Outputs
 H: HashTable
Description
  Text
    This outputs the examples in Tables 3-7 of positive depth. These are those in the tables
    with non-bold row index. 
  Example
    higherDepthTable
///

doc ///
Key
 "depthZeroTable"
Headline
 Creates hashtable of Jan-Erik Roos' examples of quadratic ideals with depth zero
Usage
 H = depthZeroTable ()
Outputs
 H: HashTable
Description
  Text
    This outputs the examples in Tables 3-7 of depth zero. These are those in the tables
    with bold row index. 
  Example
    depthZeroTable
///


doc ///
Key
 "almostKoszul"
 (almostKoszul, Ring, ZZ)
Headline
 Examples discovered by Jan-Erik Roos
Usage
 R = almostKoszul(kk,a)
Inputs
 kk:Ring
  the field over which R will be defined
 a: ZZ
  length of the linear part of the resolution of kk over R
Outputs
 R:Ring
Description
  Text
   A standard graded ring R is Koszul if the
   minimal R-free resolution F of its residue field kk
   is linear. Roos' examples, which are 2-dimensional rings of
   depth 0 in 6 variables, show that it is not enough to require
   that F be linear for a steps, no matter how large a is.
   
   The examples are also remarkable in that (as far as we could check)
   the (a+1)-st syzygy, and all subsequent syzygies
   of kk have socle summands, but none before the (a+1)-st do.
   This shows that the the socle summands do NOT all come from the
   Koszul complex, but leaves open the conjecture that 
   (with the one exception) the socle summand persist once they start.
   
   It's also striking that (in this case) the first socle
   summands come from the linear strand of the resolution,
   though they begin to appear exactly where the resolution
   ceases to be linear.
   
  Example
   R = almostKoszul(ZZ/32003, 4)
   F = res (coker vars R, LengthLimit =>6)
   betti F
References
 J.E.Roos, Commutative non Koszul algebras
  having a linear resolution of arbitrarily high order.
  Applications to torsion in loop space homology,
  C. R. Acad. Sci. Paris 316 (1993),1123-1128.
///


doc ///
Key
 "roosIsotopes"
Headline
 Creates hashtable of Jan-Erik Roos' quadratic "isotopes" 
Usage
 H = depthZeroTable ()
Outputs
 H: HashTable
Description
  Text
    This outputs the examples in Tables B,C,D of Roos' quadratic "isotopes" (see Main Theorem in  "Homological properties of the homology
    algebra of the Koszul complex of a local ring: Examples and questions", Journal of Algebra 
    465 (2016) 399-436). The key is the alphanumeric
    name provided by Roos.  While these have the same bigraded Poincar\'e series as their corresponding example in
    Tables 3-7, they have different homology algebra (called "HKR" in Roos). These all have depth zero. The coefficient field is QQ.
  Example
    roosIsotopes
///

doc ///
Key
 "onedimToricIrrationalPoincare"
Headline
 Produces the example of a one-dimensional toric ideal whose Poincar\'e series is irrational.
Usage 
 I = onedimToricIrrationalPoincare ()
Outputs
 I: Ideal
Description
 Text
  This returns the example from Fr\:oberg and Roos' "An affine monomial curve with irrational with irrational Poincar\'e-Betti series" 
  of a toric ideal whose quotient ring has an irrational Poincar\'e series. 
  It is the toric ideal of the numerical subsemigroup of $\mathbb{N}$ generated by $\{18, 24, 25, 26, 28, 30, 33\}.$
 Example
  I = onedimToricIrrationalPoincare
///

doc ///
Key
 "twodimToricIrrationalPoincare"
Headline
 Produces the example of a two-dimensional toric ideal whose Poincar\'e series is irrational.
Usage 
 I = twodimToricIrrationalPoincare ()
Outputs
 I: Ideal
Description
 Text
  Returns the example from Roos and Sturmfels' "A toric ring with irrational Poincar\'e-Betti series" (1998) 
  of a toric ideal whose quotient ring has an irrational Poincar\'e series.
  It is the toric ideal of the numerical subsemigroup of $\mathbb{N}^2$ 
  generated by ${(36,0), (33,3), (30,6), (28,8), (26,10), (25,11), (24,12), (18,18), (0,36)}.$
 Example
  I = onedimToricIrrationalPoincare
///


-* Test section *-
TEST///
I=roosTable#7
S=ring I
assert(depth (S/I) == 0)
///

TEST///
I=roosTable#68
S=ring I
assert(depth (S/I) == 1)
///

TEST///
I=roosIsotopes#"59va"
F=res (ideal vars ((ring I)/I), LengthLimit => 6)
assert(betti F == new BettiTally from {(0,{0},0) => 1, (1,{1},1) => 4, (2,{2},2) => 12, (3,{3},3) => 33, (4,{4},4) => 87, (5,{5},5) => 225, (6,{6},6) => 576, (6,{7},7) => 1})
///


TEST///
   R = almostKoszul(ZZ/32003, 2)
   F = res (coker vars R, LengthLimit =>4)
   assert( betti F === new BettiTally from {(0,{0},0) => 1, (1,{1},1) => 6, (2,{2},2) => 26, (3,{3},3) => 104, (3,{4},4) => 1,
      (4,{4},4) => 404, (4,{5},5) => 10})
///

end--

uninstallPackage "QuadraticIdealExamplesByRoos"
restart
installPackage "QuadraticIdealExamplesByRoos"


check QuadraticIdealExamplesByRoos
