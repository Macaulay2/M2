
R = frac(QQ[a])[x]
F = leadCoefficient (a*x)
leadTerm F  -- blue smoke

-- The problem is that 'leadTerm F' (really IM2_RingElement_some_terms) is
-- returning an error (the ring is not a poly ring), but that the front end is
-- ignoring this error.

