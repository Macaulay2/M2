-- See -- See http://www.win.tue.nl/SCIEnce/cds/scscp2.html
-- done: retrieve, store_session, unbind, get_service_description, service_description
-- todo/not done: 
-- * get_allowed_heads, get_signature, get_transient_cd, is_allowed_head, no_such_transient_cd, 
-- * signature, store_persistent, symbol_set, symbol_set_all

OMSEvaluators#"scscp2" = new MutableHashTable;
OMSEvaluators#"scscp2"#"retrieve" = (args, attrs) -> ( 
	a := args#0;
	if a.tag =!= "OMR" then
		(theOMerror = "1st argument to scscp2.retrieve should be an OMR"; error("whoops"));

	s := a#"href";
	if not existsOMref(s) then
		(theOMerror = concatenate("Unknown reference: '", s, "'"); error("whoops"));
	
	getOMref(s)
)
OMSEvaluators#"scscp2"#"store_session" = (args, attrs) -> ( 
	a := fromOpenMath args#0;
	OMR(addOMref(a))
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