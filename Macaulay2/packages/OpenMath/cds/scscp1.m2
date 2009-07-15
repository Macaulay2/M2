-- done: 
-- todo / not done:
--  * call_id, 
--  * error_memory, error_runtime, error_system_specific, 
--  * info_memory, info_message, info_runtime, 
--  * option_debuglevel, option_max_memory, option_min_memory, option_runtime,
--  * option_return_cookie, option_return_nothing, option_return_object,  
--  * procedure_call, procedure_completed, procedure_terminated 

OMSEvaluators#"scscp1" = new MutableHashTable;
OMSEvaluators#"scscp1"#"procedure_call" = (args, attrs) -> ( 
	<< "attrs = " << attrs << endl;
	true
)
