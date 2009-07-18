--- From OpenMath: Maybe add some things? ---
OMSEvaluators#"scscp_transient_1" = new MutableHashTable;
OMSEvaluators#"scscp_transient_1" = new MutableHashTable;
OMSEvaluators#"scscp_transient_1"#"factorint" = (args, attrs) -> factor fromOpenMath args#0
OMSEvaluators#"scscp_transient_1"#"factorint_lst" = (args, attrs) -> apply(toList factor fromOpenMath args#0, toList)

--- To OpenMath: For cool demo purposes with GAP. ---
matrixGroup = l -> (
	OMA( "scscp_transient_1", "MatrixGroup", l )
)
