-- -*- coding: utf-8 -*-
needsPackage "XML"
newPackage(
	"OpenMath",
    	Version => "0.1", 
    	Date => "October 12, 2009",
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
load "./OpenMath/cds/fieldname1.m2"
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
load "./OpenMath/cds/setname2.m2"

load "./OpenMath/cds/scscp_transient_1.m2"


-------------------------------
----(LITTLE) DOCUMENTATION-----
-------------------------------

export { "openMath", "val", "matrixGroup" }

beginDocumentation()
document { 
	Key => OpenMath,
	Headline => "OpenMath support",
	SeeAlso => {(val, XMLnode)}
	}

document {
	Key => {val, (val, XMLnode), (value,XMLnode)},
	Headline => "Evaluate an XMLnode containing OpenMath",
	Usage => "val x",
	Inputs => { "x" },
	Outputs => {{ "the value of the OpenMath object described by x" }},
	EXAMPLE lines ///
		t = parse ////<OMA><OMS cd="arith1" name="plus"/><OMI>1</OMI><OMI>2</OMI></OMA>////
		val t
     	///,
	TEST ///
		s = toString toLibxmlNode openMath (hold 2*3)
		assert (val parse s == 6)
	///,
	TEST ///
		GF(25)[x,y]
		s1 = toString toLibxmlNode (openMath (x+y))^2
		s2 = toString toLibxmlNode (openMath (x^2 + 2*y*x + y^2))
		assert ( s1 != s2 )
		assert ( val parse s1 == val parse s2 )
	///,
	SeeAlso => {openMath}
	}

document {
	Key => {openMath, (openMath, Thing)},
	Headline => "Turn an arbitrary Macaulay2 object into OpenMath (if possible)",
	Usage => "openMath x",
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
	TEST ///
		s = toString toLibxmlNode openMath (hold 2*3)
		s = replace("(\r|\n| )", "", s)
		assert (s == "<OMA><OMScd=\"arith1\"name=\"times\"/><OMI>2</OMI><OMI>3</OMI></OMA>")
	///,
	SeeAlso => {(val, XMLnode)}
	}
	
undocumented{ matrixGroup }


endPackage("OpenMath");
