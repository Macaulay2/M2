-- -*- coding: utf-8 -*-
newPackage(
	"SubalgebraBases",
	AuxiliaryFiles => true,
    	Version => "1.3",
    	Date => "September 2022",
    	Authors => {
	    {Name => "Michael Burr",
	     Email => "burr2@clemson.edu",
	     HomePage => "https://cecas.clemson.edu/~burr2/"},
	    {Name => "Oliver Clarke",
	     Email => "oliver.clarke.crgs@gmail.com",
	     HomePage => "https://sites.google.com/view/oclarke-homepage/"},
	    {Name => "Timothy Duff",
	     Email => "timduff@uw.edu",
	     HomePage => "https://timduff35.github.io/timduff35/"},
	    {Name => "Jackson Leaman",
		 Email => "jacksonleaman@gmail.com"},
	    {Name => "Nathan Nichols", 
	     Email => "nathannichols454@gmail.com"},
	    {Name => "Elise Walker",
	     Email => "elise.walker@tamu.edu",
	     HomePage => "https://sites.google.com/view/elise-walker/home"},
	    {Name => "Legacy author: Mike Stillman",
	     Email => "mike@math.cornell.edu",
	     HomePage => "http://www.math.cornell.edu/~mike/"},
	    {Name => "Legacy author: Harrison Tsai"},
            {Name => "We thank the following contributors from the 2020 Macaulay2 workshops at Cleveland State University and University of Warwick: Aaron Dall, Alessio D'Alì, Alicia Tocino, Ayah Almousa, Dharm Veer, Dipak Kumar Pradhan, Emil Sköldberg, Eriola Hoxhaj, Kriti Goel, Liđan Edin, Peter Phelan, Ritika Nair, Ilir Dema"}
	    },
    	Headline => "Canonical subalgebra bases (aka SAGBI/Khovanskii bases)",
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
  	DebuggingMode => true -- set to true only during development
        )
exportMutable {}

needs "./SubalgebraBases/exports.m2"
needs "./SubalgebraBases/classes.m2"
needs "./SubalgebraBases/sagbi-functions.m2"
needs "./SubalgebraBases/compTable-functions.m2"
--needs "./SubalgebraBases/toric_syz.m2"
--needs "./SubalgebraBases/subring_modules.m2"
needs "./SubalgebraBases/tests.m2"
--needs "./SubalgebraBases/print_util.m2"
needs "./SubalgebraBases/sagbi-main.m2"

beginDocumentation()
needs "./SubalgebraBases/documentation.m2"
--needs "./SubalgebraBases/tests.m2"

end--

uninstallAllPackages()
restart
path = prepend("./", path)
installPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2",InstallPrefix=>"./")
check "SubalgebraBases"

-- Bruns/Conca 2
FF = ZZ/2
R = FF[x,y,z]
T={ x^6, x^5*y, y^5*z, z^5*x,    y^6+ y^3*z^3 }
elapsedTime G = gens sagbi(subring T, Limit=>96);
assert(G == matrix {{x*z^5, y^5*z, y^6+y^3*z^3, x^5*y, x^6, x^6*y^13*z^17, x^30*y^3*z^3, x^30*y^9*z^9, x^12*y^25*z^29, x^12*y^17*z^43, x^12*y^18*z^42+x^12*y^15*z^45, x^6*y^37*z^41, x^12*y^24*z^54, x^11*y^39*z^40+x^11*y^30*z^49+x^11*y^27*z^52, x^12*y^25*z^59, x^11*y^34*z^51+x^11*y^31*z^54+x^11*y^22*z^63+x^11*y^19*z^66}}
G=gens oo
degree \ (flatten entries G)

-- Bruns/Conca 1
R = QQ[x,y,z]
M = matrix{{x^3+y^3+z^3,   x^4+y^4+z^4,   x^5+y^5+z^5}}
partialSBSeventeen = gens sagbi(M, Limit => 17);
assert(partialSBSeventeen == matrix {{x^3+y^3+z^3, x^4+y^4+z^4, x^5+y^5+z^5, x^5*y^3-2*x^4*y^4+x^3*y^5+x^5*z^3+y^5*z^3-2*x^4*z^4-2*y^4*z^4+x^3*z^5+y^3*z^5,
      3*x^6*y^3-x^5*y^4-x^4*y^5+3*x^3*y^6+3*x^6*z^3+6*x^3*y^3*z^3+3*y^6*z^3-x^5*z^4-y^5*z^4-x^4*z^5-y^4*z^5+3*x^3*z^6+3*y^3*z^6,
      2*x^7*y^3+x^6*y^4-2*x^5*y^5+x^4*y^6+2*x^3*y^7+2*x^7*z^3+2*x^4*y^3*z^3+2*x^3*y^4*z^3+2*y^7*z^3+x^6*z^4+2*x^3*y^3*z^4+y^6*z^4-2*x^5*z^5-2*y^5
      *z^5+x^4*z^6+y^4*z^6+2*x^3*z^7+2*y^3*z^7, 5*x^8*y^4-4*x^7*y^5+6*x^6*y^6-4*x^5*y^7+5*x^4*y^8+12*x^6*y^3*z^3-4*x^5*y^4*z^3-4*x^4*y^5*z^3+12*x
      ^3*y^6*z^3+5*x^8*z^4-4*x^5*y^3*z^4+18*x^4*y^4*z^4-4*x^3*y^5*z^4+5*y^8*z^4-4*x^7*z^5-4*x^4*y^3*z^5-4*x^3*y^4*z^5-4*y^7*z^5+6*x^6*z^6+12*x^3*
      y^3*z^6+6*y^6*z^6-4*x^5*z^7-4*y^5*z^7+5*x^4*z^8+5*y^4*z^8,
      5*x^9*y^4-4*x^8*y^5+3*x^7*y^6+3*x^6*y^7-4*x^5*y^8+5*x^4*y^9+6*x^7*y^3*z^3+3*x^6*y^4*z^3-6*x^5*y^5*z^3+3*x^4*y^6*z^3+6*x^3*y^7*z^3+5*x^9*z^4
      +3*x^6*y^3*z^4+4*x^5*y^4*z^4+4*x^4*y^5*z^4+3*x^3*y^6*z^4+5*y^9*z^4-4*x^8*z^5-6*x^5*y^3*z^5+4*x^4*y^4*z^5-6*x^3*y^5*z^5-4*y^8*z^5+3*x^7*z^6+
      3*x^4*y^3*z^6+3*x^3*y^4*z^6+3*y^7*z^6+3*x^6*z^7+6*x^3*y^3*z^7+3*y^6*z^7-4*x^5*z^8-4*y^5*z^8+5*x^4*z^9+5*y^4*z^9,
      5*x^10*y^4-4*x^9*y^5-3*x^8*y^6+12*x^7*y^7-3*x^6*y^8-4*x^5*y^9+5*x^4*y^10-6*x^8*y^3*z^3+12*x^7*y^4*z^3-6*x^6*y^5*z^3-6*x^5*y^6*z^3+12*x^4*y^
      7*z^3-6*x^3*y^8*z^3+5*x^10*z^4+12*x^7*y^3*z^4+6*x^6*y^4*z^4-2*x^5*y^5*z^4+6*x^4*y^6*z^4+12*x^3*y^7*z^4+5*y^10*z^4-4*x^9*z^5-6*x^6*y^3*z^5-2
      *x^5*y^4*z^5-2*x^4*y^5*z^5-6*x^3*y^6*z^5-4*y^9*z^5-3*x^8*z^6-6*x^5*y^3*z^6+6*x^4*y^4*z^6-6*x^3*y^5*z^6-3*y^8*z^6+12*x^7*z^7+12*x^4*y^3*z^7+
      12*x^3*y^4*z^7+12*y^7*z^7-3*x^6*z^8-6*x^3*y^3*z^8-3*y^6*z^8-4*x^5*z^9-4*y^5*z^9+5*x^4*z^10+5*y^4*z^10,
      7*x^9*y^6-3*x^8*y^7-3*x^7*y^8+7*x^6*y^9+14*x^9*y^3*z^3-3*x^8*y^4*z^3-6*x^7*y^5*z^3+30*x^6*y^6*z^3-6*x^5*y^7*z^3-3*x^4*y^8*z^3+14*x^3*y^9*z^
      3-3*x^8*y^3*z^4+6*x^7*y^4*z^4-3*x^6*y^5*z^4-3*x^5*y^6*z^4+6*x^4*y^7*z^4-3*x^3*y^8*z^4-6*x^7*y^3*z^5-3*x^6*y^4*z^5+6*x^5*y^5*z^5-3*x^4*y^6*z
      ^5-6*x^3*y^7*z^5+7*x^9*z^6+30*x^6*y^3*z^6-3*x^5*y^4*z^6-3*x^4*y^5*z^6+30*x^3*y^6*z^6+7*y^9*z^6-3*x^8*z^7-6*x^5*y^3*z^7+6*x^4*y^4*z^7-6*x^3*
      y^5*z^7-3*y^8*z^7-3*x^7*z^8-3*x^4*y^3*z^8-3*x^3*y^4*z^8-3*y^7*z^8+7*x^6*z^9+14*x^3*y^3*z^9+7*y^6*z^9}})

loadPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2")
check(1, "SubalgebraBases")

path = prepend("./", path)
installPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2",InstallPrefix=>"./")


check SubalgebraBases
