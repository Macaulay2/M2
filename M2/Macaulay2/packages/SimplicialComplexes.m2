-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2022 Ben Hersey, Gregory G. Smith, and Alexandre Zotine
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
        {Name     => "Sasha Zotine",
	 Email    => "zotinea@mcmaster.ca",
	 HomePage => "https://sites.google.com/view/szotine/home" }
     },
    Headline => "exploring abstract simplicial complexes within commutative algebra",
    Keywords => {"Combinatorial Commutative Algebra"},
    PackageExports => { "Polyhedra", "Complexes" },
    AuxiliaryFiles => true,
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Simplicial complexes in Macaulay2",
	"acceptance date" => "2023-03-21",
	"published article URI" => "https://msp.org/jsag/2023/13-1/p05.xhtml",
	"published article DOI" => "10.2140/jsag.2023.13.53",
	"published code URI" => "https://msp.org/jsag/2023/13-1/jsag-v13-n1-x05-SimplicialComplexes.m2",
	"release at publication" => "41377e58a3e8f6289ff06d70d06be0e84876fc1a",
	"version at publication" => "2.0",
	"volume number" => "13",
	"volume URI" => "https://msp.org/jsag/2023/13-1/"
	}
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

