--------------------------------------------------------------------------------
-- Copyright 2014  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

newPackage(
     "HighestWeights",
     Version => "0.6.5",
     Date => "July 03, 2015",
     AuxiliaryFiles => true,
     Authors => {{Name => "Federico Galetto",
     	       Email => "galetto.federico@gmail.com",
	       HomePage => "http://math.galetto.org"}},
     Headline => "decompose free resolutions and graded modules with a semisimple Lie group action",
     Keywords => {"Lie Groups and Lie Algebras", "Homological Algebra"},
     PackageImports => {"OldChainComplexes"},
     PackageExports => {"WeylGroups", "Complexes"},
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "https://msp.org/jsag/",
	  "article title" => "Free resolutions and modules with a semisimple Lie group action",
	  "acceptance date" => "5 June 2015",
	  "published article URI" => "https://msp.org/jsag/2015/7-1/p03.xhtml",
	  "published article DOI" => "10.2140/jsag.2015.7.9",
	  "published code URI" => "https://msp.org/jsag/2015/7-1/jsag-v7-n1-x03-HW.zip",
	  "release at publication" => "a434adb94f76f9be38131f87745867b0d7925405",	    -- git commit number in hex
	  "version at publication" => "0.6.5",
	  "volume number" => "7",
	  "volume URI" => "https://msp.org/jsag/2015/7-1/"
	  }
     )

load "./HighestWeights/freudenthal.m2"
load "./HighestWeights/main.m2"

beginDocumentation()
load "./HighestWeights/doc.m2"
load "./HighestWeights/examples.m2"

load "./HighestWeights/tests.m2"

end

--A little version history:
--0.1: wrote a basic working version of the package! :-)
--0.2: completed documentation and tests
--0.2.1: suppressed minimality test for propagate
--0.2.2: now with auxiliary files and freudenthal decomposition
--0.2.3: new propagateWeights function
--0.2.4: preliminary HW decomposition for resolutions (no multigrading)
--0.2.5: HW decomposition for (multi)-graded rings
--0.2.6: multigraded HW decomposition for resolutions
--0.2.7: HW decomposition for modules (and improved? for quotients)
--0.2.8: HW decomposition for ideals
--0.3: main restructuring (incompatible with previous versions)
--0.3.1: added documentation for propagateWeights
--0.3.2: added range option for hwd for resolutions
--0.3.3: added range input for hwd of modules, rings and ideals
--0.3.4: added documentation for hwd of complexes, rings and ideals
--0.4: complete documentation with examples
--0.4.1: added simplified decomposition for res of quotient rings
--0.4.2: switched to VirtulTally for characters
--0.4.3: added tutorial examples
--0.4.4: added tests and references
--0.5: ready for distribution
--0.5.1: can now handle position down (just in case!)
--0.5.2: included some references in the documentation
--0.6: updated documentation with more/better examples
--0.6.1: decomposeWeightsList returns error if multiplicities are negative
--0.6.2: included changes from technical comments
--0.6.3: updated one reference
--0.6.4: updated website link
--0.6.5: updated reference to theory article
