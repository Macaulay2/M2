-- -*- coding: utf-8 -*-
needsPackage "XML"
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

theOMerror = null;

load "./OpenMath/OMattr.m2"
load "./OpenMath/OMelts.m2"
load "./OpenMath/OMrefs.m2"
load "./OpenMath/base.m2"
load "./OpenMath/cds/arith1.m2"
load "./OpenMath/cds/field3.m2"
load "./OpenMath/cds/field4.m2"
load "./OpenMath/cds/finfield1.m2"
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
load "./OpenMath/cds/scscp1.m2"
load "./OpenMath/cds/scscp2.m2"
load "./OpenMath/cds/set1.m2"
load "./OpenMath/cds/setname1.m2"

--Should actually need to export little else than openMathValue
export { "openMathValue" }

renderXML = (x,t) -> (
	for i in 1..t do << " ";
	
	if class(x) =!= XMLnode then (
		<< "Whoops! Not an XMLnode: '" << x << endl;
		return;
	);
	
	<< x.tag;
	if (x.tag === "OMS") then
		<< "(cd = " << x#"cd" << ", name = " << x#"name" << ")";
	if (x.tag === "OMV") then << "(" << x#"name" << ")";
	if (x.tag === "OMI") then << "(" << (x.children)#0 << ")";
	if (x.tag === "OMSTR") then << "(" << (x.children)#0 << ")";
	if (x.tag === "OMR") then << "(" << x#"href" << ")";
	<< endl;

	if x.?children and x.tag =!= "OMI" and x.tag =!= "OMSTR" then
		if class(x.children) === List then 
			for c in x.children do
				renderXML(c, t+2);
			
)

------------------------
--------TESTS-----------
------------------------

-- t = parse ///<OMA><OMS cd="arith1" name="plus"/><OMI>1</OMI><OMI>17</OMI></OMA>///
-- << "t = " << t << endl;
-- << "fromOpenMath t = " << fromOpenMath t << endl;

t = parse ///<OMA id="d1"><OMS cd="polyd1" name="poly_ring_d"/><OMS cd="setname1" name="Q"/><OMI>2</OMI></OMA>/// ;
vt := value t;
<< "value t = " << vt << endl;
ovt := openMathValue vt;
<< "openMathValue value t = " << ovt << endl;

------------------------
----(NO) DOCUMENTATION--
------------------------
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

------------------------
----For Debugging-------
------------------------
endPackage("OpenMath");
debug OpenMath
