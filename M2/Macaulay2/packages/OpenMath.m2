-- -*- coding: utf-8 -*-
newPackage(
	"OpenMath",
    	Version => "0.1", 
    	Date => "July 14, 2009",
    	Authors => {
			{Name => "Dan Roozemond", Email => "dan@banaan.org", HomePage => "http://www.danroozemond.nl/"}
		},
    	Headline => "OpenMath for Macaulay2",
    	DebuggingMode => true,
		AuxiliaryFiles => true
    	)

needsPackage "XML"
load "./OpenMath/OMelts.m2"
load "./OpenMath/base.m2"
load "./OpenMath/rings.m2"

export { "toOpenMath" }


-- beginDocumentation()
-- document { 
-- 	Key => FirstPackage,
-- 	Headline => "an example Macaulay 2 package",
-- 	EM "FirstPackage", " is a basic package to be used as an example."
-- 	}
-- document {
-- 	Key => {(firstFunction,ZZ),firstFunction},
-- 	Headline => "a silly first function",
-- 	Usage => "firstFunction n",
-- 	Inputs => { "n" },
-- 	Outputs => {{ "a silly string, depending on the value of ", TT "n" }},
--         SourceCode => {(firstFunction,ZZ)},
-- 	EXAMPLE lines ///
-- 	   firstFunction 1
-- 	   firstFunction 0
--      	///
-- 	}

-- TEST ///
--     assert ( firstFunction 2 == "D'oh!" )
-- ///
-- 
