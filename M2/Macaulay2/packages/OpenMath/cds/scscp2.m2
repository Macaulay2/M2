-- See -- See http://www.win.tue.nl/SCIEnce/cds/scscp2.html
-- done: retrieve, store_session, unbind, get_service_description, service_description,
--       get_allowed_heads, symbol_set
-- always errors: get_signature, get_transient_cd, is_allowed_head, store_persistent
-- not needed: signature, no_such_transient_cd, symbol_set_all

OMSEvaluators#"scscp2" = new MutableHashTable;
OMSEvaluators#"scscp2"#"retrieve" = (args, attrs) -> ( 
	a := args#0;
	if a.tag =!= "OMR" then
		(theOMerror = "1st argument to scscp2.retrieve should be an OMR"; error("whoops"));

	s := a#"href";
	if not existsOMref(s) then
		(theOMerror = concatenate("Unknown reference: '", s, "'"); error("whoops"));
	
	o := (getOMref(s))#1;
	if o === null then (
		--Apparently, we had stored an object, but had not yet openMath-ed it
		o = openMath (getOMref(s))#0;
	);
	o
)
OMSEvaluators#"scscp2"#"store_session" = (args, attrs) -> ( 
	a := fromOpenMath args#0;
	OMR(addOMref(getNewForRemoteRef(), a, args#0))
)
OMSEvaluators#"scscp2"#"unbind" = (args, attrs) -> ( 
	a := args#0;
	if a.tag =!= "OMR" then
		(theOMerror = "1st argument to scscp2.unbind should be an OMR"; error("whoops"));

	s := a#"href";
	if not existsOMref(s) then
		(theOMerror = concatenate("Unknown reference: '", s, "'"); error("whoops"));

	removeOMref(s)
)
OMSEvaluators#"scscp2"#"get_service_description" = (args, attrs) -> ( 
	OMA("scscp2", "service_description", {
		OMSTR("Macaulay2"),
		OMSTR(toString OpenMath.Options.Version),
		OMSTR("SCSCP interface to Macaulay2")
	})
)
OMSEvaluators#"scscp2"#"get_allowed_heads" = (args, attrs) -> ( 
	supportedSymbols := flatten apply(keys OMSEvaluators, cdnm->(apply(keys OMSEvaluators#cdnm, symbnm -> 
		OMS(cdnm, symbnm)
	)));
	
	OMA("scscp2", "symbol_set", supportedSymbols)
)

OMDeclareUnhandledSymbol("scscp2", "get_signature");
OMDeclareUnhandledSymbol("scscp2", "get_transient_cd");
OMDeclareUnhandledSymbol("scscp2", "is_allowed_head");
OMDeclareUnhandledSymbol("scscp2", "store_persistent");
