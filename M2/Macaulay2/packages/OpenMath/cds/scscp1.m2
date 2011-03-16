-- See http://www.win.tue.nl/SCIEnce/cds/scscp1.html
-- done (at least sort of)
--  * call_id, 
--  * error_system_specific, 
--  * option_return_cookie, option_return_nothing, option_return_object
--  * procedure_call, procedure_completed, procedure_terminated 
--  * info_runtime, info_message
-- not necessary:
--  * error_memory, error_runtime,
-- todo / not done:
--  * info_memory, 
--  * option_debuglevel, option_max_memory, option_min_memory, option_runtime,

protect callid
protect tstart

doAttrs := (e, retopts) -> (
	setOMAttr(e, OMS("scscp1", "call_id"), retopts.callid);
	if (retopts.?tstart) then
		setOMAttr(e, OMS("scscp1", "info_runtime"), OMI floor(1000*(cpuTime() - retopts.tstart)));
	if (recentOMProblem =!= null) then
		setOMAttr(e, OMS("scscp1", "info_message"), OMSTR concatenate("Possible problem: ", recentOMProblem));
)

constructProcTerm = method()
constructProcTerm (XMLnode, XMLnode) := (omenode, retopts) -> (
	e := OMA("scscp1", "procedure_terminated", {omenode});
	doAttrs(e, retopts);
	e
);
constructProcTerm (XMLnode, MutableHashTable) := (omenode, retopts) -> constructProcTerm(omenode, new XMLnode from retopts);
constructProcTerm (String, XMLnode) := (errmsg, retopts) -> constructProcTerm( OME(errmsg), retopts )
constructProcComplObj = (x, retopts) -> (
	e := OMA("scscp1", "procedure_completed", {x});
	doAttrs(e, retopts);
	e
);
constructProcComplNothing = (retopts) -> (
	e := OMA("scscp1", "procedure_completed", {});
	doAttrs(e, retopts);
	e
);
constructProcComplCookie = (x, retopts) -> (
	r := OMR(addOMref(getNewForRemoteRef(), x, null));
	e := OMA("scscp1", "procedure_completed", {r});
	doAttrs(e, retopts);
	e
);


OMSEvaluators#"scscp1" = new MutableHashTable;
OMSEvaluators#"scscp1"#"procedure_call" = (args, attrs) -> ( 
	retopts := new MutableHashTable;
	retopts.callid = OMSTR("0");
	retopts.tstart = cpuTime();

	-- Get call_id 
	if not attrs#?("scscp1.call_id") then
		return constructProcTerm("scscp1.procedure_call should have a call_id", retopts);
	retopts.callid = attrs#("scscp1.call_id")#1;
	
	-- Get option_return_ ..
	ret := null;
	if attrs#?("scscp1.option_return_object") then (
		ret = "object";
	) else if attrs#?("scscp1.option_return_nothing") then (
		ret = "nothing";
	) else if attrs#?("scscp1.option_return_cookie") then (
		ret = "cookie";
	) else (
		return constructProcTerm("scscp1.procedure_call: No suitable option_return_ found", retopts);
	);
	
	-- Get the object, and evaluate
	if (#args =!= 1) or ((args#0).tag =!= "OMA") then
		return constructProcTerm("scscp1.procedure_call: 1st and only argument of pc should be an OMA.", retopts);

	-- Try to evaluate: value will always exit, albeit sometimes with an OME
	recentOMProblem = null;
	evld := value(args#0);
	if (class(evld) === XMLnode) and (evld.tag == "OME") then 
		return constructProcTerm(evld, retopts);
		
	-- If we want to return nothing or a cookie, we are pretty much done!
	if ret === "nothing" then
		return constructProcComplNothing(callid, retopts)
	else if ret === "cookie" then
		return constructProcComplCookie(evld, retopts);

	-- try to convert back to OpenMath otherwise
	e := toOpenMath(evld);

	-- If the result is wrong...
	if (class(e) === XMLnode) and (e.tag == "OME") then 
		--Something went wrong :(
		return constructProcTerm(e, retopts);
	
	-- If the result is right...
	if ret === "object" then
		constructProcComplObj(e, retopts)
	else
		null
)
OMSEvaluators#"scscp1"#"procedure_completed" = (args, attrs) -> fromOpenMath(args#0)
OMSEvaluators#"scscp1"#"procedure_terminated" = (args, attrs) -> fromOpenMath(args#0)

