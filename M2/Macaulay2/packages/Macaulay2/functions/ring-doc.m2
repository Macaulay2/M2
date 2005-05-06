--- status: DRAFT
--- author(s): 
--- notes: 

document { 
     Key => {ring, (ring,Vector), (ring,SheafOfRings), 
	  (ring,Variety), (ring,ChainComplexMap),
	  (ring,Thing),(ring,Matrix),
	  (ring,MutableMatrix),(ring,Ideal), 
	  (ring,CoherentSheaf),(ring,MonomialIdeal)},
     Headline => "Get the associated ring of an object",
     Usage => "ring M",
     Inputs => {"M" => "an object with a ring associated to it"},
     Outputs => {
	  Ring => "associated to the input object"
	  },
     -- Consequences => {},
     "For example, matrices, ideals, modules, chain complexes,
     varieties, coherent sheaves, etc., all have a base ring naturally associated
     to them.  ",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "ring x",
	  "M = matrix {{2*x, x+y},{y^3, z*y}};",
	  "ring M",
	  "S = QQ[x,y,z];",
	  "ring x",
	  "I = ideal (x*y, y*z);",
	  "ring I"
	  },
    -- Caveat => {},
     SeeAlso => {baseRings, coefficientRing}
     }

document{Key=> (ring,Type), Headline => "associated ring of a user defined type",
     {"If a user defined type has a ring as its data, the function ", 
     TT "ring", " can be used to extract the ring. The command", TT "ring T", " will return ", TT "T.ring", 
     " if there is an associated ring and ", TT "no ring", " otherwise."}}


 -- doc.m2:241:     Key => toString symbol commandLine,
 -- doc.m2:248:     Key => toString symbol environment,
 -- doc.m2:775:     Key => (ascii, String),
 -- doc.m2:904:     Key => substring,
 -- doc.m2:943:     Key => (read,String),
 -- doc1.m2:991:     Key => (symbol #, String),
 -- doc1.m2:1015:     Key => (symbol #, Database, String),
 -- doc1.m2:1022:     Key => (symbol #, String, ZZ),
 -- doc1.m2:1058:     Key => (symbol #?, Database, String),
 -- doc1.m2:1065:     Key => (symbol #?, String, ZZ),
 -- doc10.m2:844:     Key => (ring, CoherentSheaf),
 -- doc12.m2:579:     Key => toString,
 -- doc12.m2:588:     Key => toExternalString,
 -- doc2.m2:400:     Key => (symbol <<, String, Thing),
 -- doc2.m2:770:     Key => (value,String),
 -- doc3.m2:164:     Key => String,
 -- doc3.m2:449:     Key => printString,
 -- doc5.m2:423:     Key => (setRandomSeed, String),
 -- doc6.m2:38:     Key => ring,
 -- doc6.m2:41:     Key => (ring, Matrix),
 -- doc6.m2:52:     Key => (ring, Ideal),
 -- doc6.m2:63:     Key => (ring, MonomialIdeal),
 -- doc6.m2:439:     Key => (symbol _, Ring, String),
 -- doc7.m2:1405:     Key => SubringLimit,
 -- doc7.m2:1413:     Key => [kernel,SubringLimit],
 -- doc8.m2:1199:     Key => selectInSubring,
 -- doc8.m2:1309:     Key => (GF,Ring), Headline => "make a finite field from a ring",
 -- doc8.m2:1413:     Key => isAffineRing, Headline => "whether something is an affine ring",
 -- doc_packages.m2:11:     Key => {loadPackage,(loadPackage,String)},
 -- doc_packages.m2:82:     Key => {needsPackage,(needsPackage,String),(needsPackage,Package)},
 -- doc_packages.m2:214:     Key => {TEST, (TEST,String)},
 -- doc_packages.m2:238:     Key => {installPackage,(installPackage,String),(installPackage,Package)},
 -- doc_packages.m2:334:     Key => {newPackage, (newPackage,String)}, 
 -- normal_doc.m2:5:     Key => isNormal, Headline => "determine whether a reduced ring is normal" }
 -- overview2.m2:1016:     Key => "strings",
 -- overviewA.m2:3:     Key => "rings",
 -- overviewA.m2:233:     Key => "substitution and maps between rings",
 -- overviewA.m2:326:     Key => "rings that are available for Groebner basis computations",
 -- overviewB.m2:542:     Key => "working with multiple rings",   -- DOUBLE CHECK BEING DONE WITH THIS ONE!
 -- overviewB.m2:683:     Key => "basic construction, source and target of a ring map",
 -- overviewB.m2:735:     Key => "evaluation and composition of ring maps",
 -- overviewB.m2:765:     Key => "kernel and image of a ring map",
 -- overviewC.m2:4:     Key => "basic rings of numbers",
 -- overviewC.m2:179:     Key => "polynomial rings",
 -- overviewC.m2:294:     Key => "monomial orderings", 
 -- overviewC.m2:410:     Key => "obtaining the monomial order of a ring",
 -- overviewC.m2:422:     Key => "monomial orderings1", 
 -- overviewC.m2:481:     Key => "monomial orderingsOLD",
 -- overviewC.m2:576:     Key => "graded and multigraded polynomial rings",
 -- overviewC.m2:610:     Key => "quotient rings",
 -- overviewC.m2:851:     Key => "factoring polynomials",
 -- overviewC.m2:993:     Key => "tensor products of rings",
 -- overviewC.m2:1407:     Key => "monomial orderings v1.0",
