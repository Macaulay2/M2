in the kernel we need:
* CompiledSLEvaluator in SLP-...{hpp,cpp}
* rawCompiledSLEvaluator in engine.dd...
* modify rawSLEvaluatorEvaluate (to call a fn from dll)

in the front:
  * store cCode in CompiledSLProgram
  * modify rawSLEvaluatorK (bad name?) to involve CompiledSLEvaluator
  * create dll (use temporaryFileName)  
    
  
