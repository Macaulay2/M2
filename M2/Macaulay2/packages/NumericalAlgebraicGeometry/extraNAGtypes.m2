------------------------------------------------------
-- additional NumericalAG types  
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
-- (common types are in ../NAGtypes.m2 
------------------------------------------------------
export { "HomotopySystem", "ParameterHomotopySystem", "SpecializedParameterHomotopySystem", 
    "evaluateH", "evaluateHt", "evaluateHx", "Parameters" }

HomotopySystem = new Type of MutableHashTable -- abstract type
evaluateH = method()
evaluateH (HomotopySystem,Matrix,Number) := (H,x,t) -> error "not implemented"
evaluateHt = method()
evaluateHt (HomotopySystem,Matrix,Number) := (H,x,t) -> error "not implemented"
evaluateHx = method()
evaluateHx (HomotopySystem,Matrix,Number) := (H,x,t) -> error "not implemented"

ParameterHomotopySystem = new Type of MutableHashTable -- abstract type
evaluateH (ParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"
evaluateHt (ParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"
evaluateHx (ParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"

SpecializedParameterHomotopySystem = new Type of HomotopySystem
specialize = method()
specialize (ParameterHomotopySystem,Matrix) := (PH, M) -> (
    SPH := new SpecializedParameterHomotopySystem;
    SPH.ParameterHomotopySystem = PH;
    SPH.Parameters = M;
    SPH
    ) 
evaluateH (SpecializedParameterHomotopySystem,Matrix,Number) := (H,x,t) -> evaluateH(H.ParameterHomotopySystem,H.Parameters,x,t) 
evaluateHt (SpecializedParameterHomotopySystem,Matrix,Number) := (H,x,t) -> evaluateHt(H.ParameterHomotopySystem,H.Parameters,x,t) 
evaluateHx (SpecializedParameterHomotopySystem,Matrix,Number) := (H,x,t) -> evaluateHx(H.ParameterHomotopySystem,H.Parameters,x,t) 
    
