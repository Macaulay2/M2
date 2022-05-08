-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2021 Gregory G. Smith
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
newPackage(
    "SimplicialComplexes",
    Version => "2.0", 
    Date => "7 May 2022",
    Authors => {
	{Name     => "Gregory G. Smith", 
	 Email    => "ggsmith@mast.queensu.ca", 
	 HomePage => "http://www.mast.queensu.ca/~ggsmith"},
        {Name => "Ben Hersey", 
	 Email => "b.hersey@queensu.ca"},
        {Name => "Alexandre Zotine", 
	 Email => "18az45@queensu.ca"}
     },
    Headline => "exploring abstract simplicial complexes within commutative algebra",
    Keywords => {"Combinatorial Commutative Algebra"},
    PackageExports => { "Polyhedra" },
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

export {
    -- types
    "SimplicialComplex",
    "SimplicialMap",
    -- methods
    "algebraicShifting",    
    "barycentricSubdivision",
    "boundaryMap",    
    "buchbergerSimplicialComplex",
    "buchbergerResolution",
    "connectedComponents",
    "elementaryCollapse",
    "flagfVector",
    "inducedSubcomplex",
    "isProper",
    "link",
    "lyubeznikSimplicialComplex", 
    "lyubeznikResolution",
    "scarfSimplicialComplex",
    "scarfChainComplex",
    "star",	
    "simplexComplex",
    "simplicialComplex",
    "taylorResolution",
    "wedge",
    -- special constructors
    "bartnetteSphereComplex",
    "bjornerComplex",
    "dunceHatComplex",
    "grunbaumBallComplex",
    "kleinBottleComplex",
    "nonPiecewiseLinearSphereComplex",
    "poincareSphereComplex",
    "realProjectiveSpaceComplex", 
    "rudinBallComplex",
    "smallManifold",
    "zieglerBallComplex",
    -- symbol
    "Multigrading",
    "Labels"
    }

importFrom("GenericInitialIdeal", {"gin", "Multigraded"})

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
load "SimplicialComplexes/Code.m2"

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation()
load "SimplicialComplexes/Documentation.m2"

------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------
load "SimplicialComplexes/Tests.m2"

end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
-- SCRATCH SPACE
------------------------------------------------------------------------------

-- XXX
uninstallPackage "SimplicialComplexes";
restart
installPackage "SimplicialComplexes"
check SimplicialComplexes

needsPackage "SimplicialComplexes";

