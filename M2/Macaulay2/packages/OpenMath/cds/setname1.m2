-- done: Z,Q,C,R
-- not done: N, P

--- To OpenMath ---
toOpenMath Ring := R -> (
	if 		R === ZZ then OMS("setname1", "Z")
	else if R === QQ then OMS("setname1", "Q")
	else 
		(theOMerror = concatenate("Cannot handle ring :", toString(R)); error("whoops"))
)

toOpenMath InexactFieldFamily := R -> (
	if 		R === CC then OMS("setname1", "C")
	else if R === RR then OMS("setname1", "R")
	else 
		(theOMerror = concatenate("Cannot handle ring :", toString(R)); error("whoops"))
)

--- From OpenMath ---
OMSEvaluators#"setname1" = new MutableHashTable;
OMSEvaluators#"setname1"#"Z" = ZZ;
OMSEvaluators#"setname1"#"Q" = QQ;
OMSEvaluators#"setname1"#"C" = CC;
OMSEvaluators#"setname1"#"R" = RR;
