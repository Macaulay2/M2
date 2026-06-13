-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2015-2025 Alvise Trevisan and Alexander I. Suciu
-- Copyright 2025-2026 Kumar Sannidhya Shukla
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
------------------------------------------------------------------------------

newPackage(
	"ToricTopology",
	Version => "1.2",
	Date => "May 20, 2026",
	Authors => {
		{Name => "Alvise Trevisan", Email => "a.trevisan@enpicom.com", HomePage => "http://www.enpicom.com"},
		{Name => "Alexander I. Suciu", Email => "a.suciu@neu.edu"},
		{Name => "Kumar Sannidhya Shukla", Email => "kshukla5@uwo.ca"}
	},
	Keywords => {"Toric Geometry"},
	PackageImports => { "Complexes", "SimplicialComplexes" },
	PackageExports => { "NormalToricVarieties" },
	Headline => "toric topology",
	AuxiliaryFiles => true
)

protect QTMSimplicialComplex
protect QTMCharacteristicMatrix
protect QTMDimension
protect MACSimplicialComplex

export {
	"SmallCover", "QuasiToricManifold", "MomentAngleComplex", "isValidChar",
	"smallCover", "quasiToricManifold", "momentAngleComplex",
	"cohomologyRing", "equivariantCohomology",
	"stiefelWhitney",
	"bettiSmallCover", "bettiQTM", "bettiMAC", "eulerMAC",
	"realProjectiveSpace", "hessenbergVariety", "complexProjectiveSpace",
	"QTMSimplicialComplex", "QTMCharacteristicMatrix", "QTMDimension",
	"MACSimplicialComplex",
	"AFPVariety"
}

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
load "ToricTopology/Code.m2"
load "ToricTopology/Helper.m2"

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation ()

load "ToricTopology/Documentation.m2"

------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------

load "ToricTopology/Tests.m2"

end
------------------------------------------------------------------------------
