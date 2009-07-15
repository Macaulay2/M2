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
load "./OpenMath/cds/arith1.m2"
load "./OpenMath/cds/fns1.m2"
load "./OpenMath/cds/integer1.m2"
load "./OpenMath/cds/integer2.m2"
load "./OpenMath/cds/list1.m2"
load "./OpenMath/cds/logic1.m2"
load "./OpenMath/cds/nums1.m2"
load "./OpenMath/cds/polygb1.m2"
load "./OpenMath/cds/polygb2.m2"
load "./OpenMath/cds/polyd1.m2"
load "./OpenMath/cds/polyd2.m2"
load "./OpenMath/cds/polynomial4.m2"
load "./OpenMath/cds/relation1.m2"
load "./OpenMath/cds/setname1.m2"

export { "toOpenMath", "fromOpenMath", 
	"OMI", "OMSTR", "OMA", "OMF", "OME", "OMOBJ", "OMV", "OMR", "OMS", "OMBIND"
}

renderXML = (x,t) -> (
	for i in 1..t do << " ";
	
	<< x.tag;
	if (x.tag === "OMS") then
		<< "(" << x#"cd" << "." << x#"name" << ")";
	if (x.tag === "OMV") then
		<< "(" << x#"name" << ")";
	if (x.tag === "OMI") then
		<< "(" << x.children << ")";
	<< endl;

	if x.?children then
		if class(x.children) === List then 
			for c in x.children do
				renderXML(c, t+2);
			
)

------------------------
--------TESTS-----------
------------------------
-- R = QQ[x];
-- p = x^2 - 1;
-- s = toOpenMath(p);
-- renderXML(s, 0);
-- 
-- s = OMA("polynomial4", "factorise", {s} );
-- fs = fromOpenMath s;
-- print fs;
-- tfs = toOpenMath fs;
-- renderXML(tfs, 0);

-- (lambda.x.(x^2)) ((lambda.x.x+3)(1)) = 16 (I think)
x = symbol x;
b1 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "power", { OMV("x"), OMI(2) }) });
b2 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "plus", { OMV("x"), OMI(3) }) });
s = OMA(b1, {OMA(b2, {OMI(1)})});
renderXML(s, 0);

s2 = fromOpenMath s;
print "s2 = \n"; print s2;


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
