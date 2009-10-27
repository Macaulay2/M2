restart
SchurVariable = new Type of BasicList
SchurVariable | SchurVariable := join			    -- stupid
SchurVariable ? SchurVariable := (x,y) -> toSequence x ? toSequence y
SchurPolynomial = new Type of HashTable			    -- keys are of type SchurVariable, values are coefficients
SubscriptingSymbol = new Type of Symbol
s = new SubscriptingSymbol from symbol s
SubscriptingSymbol _ ZZ := (s,n) -> s _ (1:n);
SubscriptingSymbol _ Symbol := SubscriptingSymbol _ Sequence := (s,x) -> new Subscript from {s,x}
s_(3,2,1)
s_a


end


expression SchurVariable := x -> new Subscript from {s,unsequence toSequence x}
net SchurVariable := net @@ expression
s_(3,2,1)
s_a
SchurVariable * SchurVariable := (x,y) -> new SchurPolynomial from { x|y => 1, y|x => -1 }; -- fill in later with the right rule
s_(4,2,1) * s_2
net SchurPolynomial := p -> net new Sum from apply(sort pairs p, (x,a) -> a * expression x)
s_(4,2,1) * s_2
nonzero = p -> select(p,a -> a != 0)
- SchurPolynomial := p -> applyValues(p,minus)
SchurPolynomial + SchurPolynomial := (p,q) -> nonzero merge(p,q,plus)
SchurPolynomial - SchurPolynomial := (p,q) -> p + -q
s_(4,2,1) * s_2
- s_(4,2,1) * s_2
s_(4,2,1) * s_2 + s_(4,2) * s_(1,2)
s_(4,2,1) * s_2 - s_(4,2,1) * s_2
RingElement * SchurVariable := Number * SchurVariable := (a,x) -> nonzero new SchurPolynomial from {(x,a)}
SchurVariable * RingElement := SchurVariable * Number := (x,a) -> nonzero new SchurPolynomial from {(x,a)}
RingElement * SchurPolynomial := Number * SchurPolynomial := (a,p) -> nonzero applyValues(p, b -> a*b)
SchurPolynomial * RingElement := SchurPolynomial * Number := (p,a) -> nonzero applyValues(p, b -> b*a)
SchurVariable * SchurPolynomial := (x,p) -> (new SchurPolynomial from {(x,1)}) * p
SchurPolynomial * SchurVariable := (p,x) -> p * (new SchurPolynomial from {(x,1)})
SchurPolynomial * SchurPolynomial := (p,q) -> sum flatten apply(pairs p,(x,a) -> apply(pairs q,(y,b) -> (x*y)*(a*b)))
2 * s_5 * s_7 * s_(1,2,3)
