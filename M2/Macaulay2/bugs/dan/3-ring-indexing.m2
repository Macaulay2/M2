--Switch these two around.  Maybe just deprecate or give errors for these.

(symbol _, IndexedVariable, Ring), -- ring variable
(symbol _, Symbol, Ring),         -- ring variable

-- make this obsolete
(symbol _, RingElement, Ring),    -- ring variable
(symbol _, RingElement, RingElement), --coeff of monomials in polynomial