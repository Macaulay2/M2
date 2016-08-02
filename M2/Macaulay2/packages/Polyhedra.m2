--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : April 2008, December 2008, March 2009, Juli 2009,
--     	    	    September 2009, October 2009, January 2010
---------------------------------------------------------------------------
newPackage("Polyhedra",
    Headline => "A package for computations with convex polyhedra",
    Version => "1.9",
    Date => "August 21, 2014",
    AuxiliaryFiles => true,
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Polyhedra: a package for computations with convex polyhedral objects",
	 "acceptance date" => "2009-09-07",
	 "published article URI" => "http://j-sag.org/Volume1/jsag-3-2009.pdf",
	 "published code URI" => "http://j-sag.org/Volume1/Polyhedra.m2",
	 "repository code URI" => "svn://svn.macaulay2.com/Macaulay2/trunk/M2/Macaulay2/packages/Polyhedra.m2",
 	 "release at publication" => 9344,
	 "version at publication" => "1.0.5",
	 "volume number" => "1",
	 "volume URI" => "http://j-sag.org/Volume1/"
	 },
    Authors => {
         {Name => "René Birkner",
	  HomePage => "http://page.mi.fu-berlin.de/rbirkner/index.htm",
	  Email => "rbirkner@mi.fu-berlin.de"}},
    DebuggingMode => false,
    PackageExports=>{"FourTiTwo"}
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
