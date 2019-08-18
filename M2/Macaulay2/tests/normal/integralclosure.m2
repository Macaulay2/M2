    setRandomSeed 2342351
    S = QQ[a..d]
    I = monomialCurveIdeal(S,{1,3,4})
    R = S/I
    R' = integralClosure R				    -- division by zero
