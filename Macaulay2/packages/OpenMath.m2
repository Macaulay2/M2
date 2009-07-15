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

export { "toOpenMath", "fromOpenMath", 
	"OMattributes",
	"setOMAttr", "clearOMAttr",
	"OMI", "OMSTR", "OMA", "OMF", "OME", "OMOBJ", "OMV", "OMR", "OMS", "OMBIND", "OMATTR",
	"renderXML",
	"existsOMref", "getOMref", "setOMref", "removeOMref", "hasOMid", "getOMid"
}

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
	if (x.tag === "OMI") then << "(" << x.children << ")";
	if (x.tag === "OMSTR") then << "(" << x.children << ")";
	if (x.tag === "OMR") then << "(" << x#"href" << ")";
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

-- -- (lambda.x.(x^2)) ((lambda.x.x+3)(1)) = 16 (I think)
-- x = symbol x;
-- b1 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "power", { OMV("x"), OMI(2) }) });
-- --b2 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "plus", { OMV("x"), OMI(3) }) });
-- --s = OMA(b1, {OMA(b2, {OMI(1)})});
-- s = OMA(b1, {OMI(2)});
-- renderXML(s, 0);
-- 
-- s2 = fromOpenMath s;
-- print "s2 = \n"; print s2;


-- s = OMA("scscp1", "procedure_call", { OMA("arith1", "plus", {OMI(1), OMI(5) }) });
-- pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz"), OMS("scscp1", "option_return_cookie") => OMSTR("") } );
-- --pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz") } );
-- renderXML(pc, 0);
-- 
-- << "pc becomes " ;
-- fpc = fromOpenMath pc;
-- print fpc;
-- renderXML(toOpenMath fpc, 0);
-- 
-- << "trying to resolve OMR: " << endl;
-- s = OMA("scscp1", "procedure_call", { OMA("scscp2", "retrieve", {OMR("#r0")})});
-- pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz"), OMS("scscp1", "option_return_object") => OMSTR("") } );
-- print fromOpenMath pc;


s = OMA("scscp1", "procedure_call", { OMA("scscp2", "store_session", {OMI(42)})});
pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz"), OMS("scscp1", "option_return_object") => OMSTR("") } );
print fromOpenMath pc;

s = OMA("scscp1", "procedure_call", { OMA("scscp2", "unbind", {OMR("#r0")})});
pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz"), OMS("scscp1", "option_return_object") => OMSTR("") } );
print fromOpenMath pc;
print fromOpenMath pc;


-- l = toOpenMath(set{1,3,7});
-- b = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "power", { OMV("x"), OMI(2) }) });
-- m = OMA("set1", "map", {b, l});
-- print fromOpenMath m;

-- R = GF(2,5);
-- t = random(R);
-- << "t = " << t << endl;
-- renderXML(toOpenMath t, 0)
-- << "from to t = " << fromOpenMath toOpenMath t << endl;

-- R = GF(2);
-- R[x]; p = x^2+x+1;
-- s = OMA("field3", "field_by_poly", {
-- 	toOpenMath R,
-- 	toOpenMath p
-- } )
-- renderXML(s, 0);
-- << "from(s) = " << fromOpenMath s << endl;

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
