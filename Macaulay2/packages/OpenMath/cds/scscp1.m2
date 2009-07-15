-- done (at least sort of)
--  * call_id, 
--  * error_system_specific, 
--  * option_return_cookie, option_return_nothing
--  * procedure_call, procedure_completed, procedure_terminated 
-- todo / not done:
--  * option_return_object
--  * error_memory, error_runtime,
--  * info_memory, info_message, info_runtime, 
--  * option_debuglevel, option_max_memory, option_min_memory, option_runtime,

constructProcTerm = method()
constructProcTerm (XMLnode, XMLnode) := (omenode, callid) -> (
	e := OMA("scscp1", "procedure_terminated", {omenode});
	enw := new MutableHashTable from e;
	enw.attributes = { OMS("scscp1", "call_id") => callid };
	enw = new XMLnode from enw;
	enw
);
constructProcTerm (String, XMLnode) := (errmsg, callid) -> (
	constructProcTerm(
		OME("scscp1", "error_system_specific", {OMSTR(errmsg)}),
		callid
	)
);

constructProcCompl = method()
constructProcCompl (XMLnode, XMLnode) := (x, callid) -> (
	e := OMA("scscp1", "procedure_completed", {x});
	enw := new MutableHashTable from e;
	enw.attributes = { OMS("scscp1", "call_id") => callid };
	enw = new XMLnode from enw;
	enw
);


OMSEvaluators#"scscp1" = new MutableHashTable;
OMSEvaluators#"scscp1"#"procedure_call" = (args, attrs) -> ( 
	-- Get call_id 
	if not attrs#?(OMS("scscp1", "call_id")) then
		return constructProcTerm("scscp1.procedure_call should have a call_id", OMSTR("0"));
	callid := attrs#(OMS("scscp1", "call_id"));
	
	-- Get option_return_ ..
	if attrs#?(OMS("scscp1", "option_return_object")) then (
		ret := "object";
	) else if attrs#?(OMS("scscp1", "option_return_nothing")) then (
		ret := "nothing";
	) else if attrs#?(OMS("scscp1", "option_return_cookie")) then (
		ret := "cookie";
		return constructProcTerm("scscp1.procedure_call: No support for option_return_cookie (yet)", callid);
	) else (
		return constructProcTerm("scscp1.procedure_call: No suitable option_return_ found", callid);
	);
	
	-- Get the object, and evaluate
	if (#args =!= 1) or ((args#0).tag =!= "OMA") then
		return constructProcTerm("scscp1.procedure_call: 1st and only argument of pc should be an OMA.", callid);
	-- Try to convert back
	try (
		e = toOpenMath(fromOpenMath(args#0))
	) else (
		return constructProcTerm("Something unspecified went wrong.", callid);
	);

	-- If the result is wrong...
	if (class(e) === XMLnode) and (e.tag == "OME") then (
		--Something went wrong :(
		return constructProcTerm(e, callid);
	);
	
	
	-- If the result is right...
	return constructProcCompl(e, callid);
)
