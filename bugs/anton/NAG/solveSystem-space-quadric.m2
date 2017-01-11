restart
debug needsPackage "NumericalAlgebraicGeometry"
CC[x,y,z]
H = segmentHomotopy(polySystem {x^2-1,y-1,z-1},polySystem {x^2-y,z,(-.81649472472903173p53-.12123515862590568p53*ii)*x+(.14521886999185563p53+.30214660693116907p53*ii)*y+(.33916065374716547p53-.30203867214427921p53*ii)*z+.63927434582551312p53+.43596305931403112p53*ii},gamma=>toCC(-.98321030583925062p53,-.18247601072877281p53))
trackHomotopy(H,{point { {toCC(-.1p53e1,.12246467991473532p53e-15),toCC(.1p53e1,.0p53),toCC(.1p53e1,.0p53)} },point { {toCC(.1p53e1,.0p53),toCC(.1p53e1,.0p53),toCC(.1p53e1,.0p53)} }})


