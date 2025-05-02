--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : April 2008, December 2008, March 2009, July 2009,
--     	    	    September 2009, October 2009, January 2010
---------------------------------------------------------------------------
newPackage("Polyhedra",
    Headline => "convex polyhedra",
    Version => "1.10",
    Date => "November 12, 2018",
    AuxiliaryFiles => true,
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "Polyhedra: a package for computations with convex polyhedral objects",
	 "acceptance date" => "2009-09-07",
	 "published article URI" => "https://msp.org/jsag/2009/1-1/p03.xhtml",
	 "published article DOI" => "10.2140/jsag.2009.1.11",
	 "published code URI" => "https://msp.org/jsag/2009/1-1/jsag-v1-n1-x03-code.zip",
 	 "release at publication" => "c065ec7651789907627333018dc7d675968639e4", -- git commit number in hex
	 "version at publication" => "1.0.5",
	 "volume number" => "1",
	 "volume URI" => "https://msp.org/jsag/2009/1-1/"
	 },
    Keywords => {"Convex Geometry"},
    Authors => {
      {
         Name => "René Birkner",
        HomePage => "http://page.mi.fu-berlin.de/rbirkner/index.htm",
        Email => "rbirkner@mi.fu-berlin.de"
        },
     {
        Name => "Lars Kastner", -- Maintaining author
        HomePage => "http://page.mi.fu-berlin.de/lkastner/",
        Email => "k.l@fu-berlin.de"
     }
     },
    PackageImports=>{"LLLBases"}
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2010 René Birkner
--
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
---------------------------------------------------------------------------
load "./Polyhedra/loadFile.m2"
