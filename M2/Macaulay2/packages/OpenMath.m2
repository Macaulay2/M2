-- -*- coding: utf-8 -*-
needsPackage "XML"
newPackage(
	"OpenMath",
    	Version => "0.1", 
    	Date => "July 14, 2009",
    	Authors => {
			{Name => "Dan Roozemond", Email => "d.a.roozemond@tue.nl", HomePage => "http://www.win.tue.nl/~droozemo/"}
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
load "./OpenMath/expr.m2"
load "./OpenMath/cds/arith1.m2"
load "./OpenMath/cds/field3.m2"
load "./OpenMath/cds/field4.m2"
load "./OpenMath/cds/finfield1.m2"
load "./OpenMath/cds/fns1.m2"
load "./OpenMath/cds/integer1.m2"
load "./OpenMath/cds/integer2.m2"
load "./OpenMath/cds/linalg1.m2"
load "./OpenMath/cds/linalg2.m2"
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

------------------------
--------TESTS-----------
------------------------

-- t = parse ///<OMA><OMS cd="arith1" name="plus"/><OMI>1</OMI><OMI>17</OMI></OMA>///
-- << "t = " << t << endl;
-- << "fromOpenMath t = " << fromOpenMath t << endl;

-- t = parse ///<OMA><OMS cd="polyd1" name="poly_ring_d"/><OMS cd="setname1" name="Q"/><OMI>2</OMI></OMA>/// ;
-- vt := value t;
-- << "value t = " << vt << endl;
-- ovt := openMath vt;
-- << "openMath value t = " << ovt << endl;
-- ovt = openMath vt;
-- << "openMath value t = " << ovt << endl;
-- resetDeclaredIDs()
-- ovt = openMath vt;
-- << "openMath value t = " << ovt << endl;
-- ovt = openMath vt;
-- << "openMath value t = " << ovt << endl;

-------------------------------
----(LITTLE) DOCUMENTATION-----
-------------------------------

--Should actually need to export little else than openMath
-- (and value XMLnode, obviously)
export { "openMath" }
beginDocumentation()
document { 
	Key => OpenMath,
	Headline => "OpenMath support",
	SeeAlso => {(value, XMLnode)}
	}

document {
	Key => {(value,XMLnode)},
	Headline => "Evaluate an XMLnode containing OpenMath",
	Usage => "value x",
	Inputs => { "x" },
	Outputs => {{ "the value of the OpenMath object described by x" }},
	EXAMPLE lines ///
		t = parse ////<OMA><OMS cd="arith1" name="plus"/><OMI>1</OMI><OMI>2</OMI></OMA>////
		value t
     	///,
	SeeAlso => {openMath}
	}

document {
	Key => {openMath},
	Headline => "Turn an arbitrary Macaulay2 object into OpenMath (if possible)",
	Usage => "value x",
	Inputs => { "x" },
	Outputs => {{ "an XMLnode describing x" }},
	"We show how to convert a random integer to OpenMath and print it as XML",
	EXAMPLE lines ///
		v = openMath 42
		toLibxmlNode v
     	///,
	"We show how to represent 2*3 in OpenMath",
	EXAMPLE lines ///
		v = openMath (hold 2*3)
		toLibxmlNode v
     	///,
	SeeAlso => {(value, XMLnode)}
	}
-------------------------------
----- (NO) ACTUAL TESTS -------
-------------------------------

-- TEST ///
--     assert ( firstFunction 2 == "D'oh!" )
-- ///
-- 

------------------------
----For Debugging-------
------------------------
endPackage("OpenMath");
debug OpenMath
