document {
    Key => identity,
    Headline => "the identity function",
    TT "identity x", " -- returns x.",
    PARA{},
    "This is the identity function."
}

document {
    Key => {
	 id,
	(id, Ring),
	(id, Module),
	(id, ChainComplex)
    },
    Headline => "identity map",
    Usage => "id_F",
    Inputs => {
	"F" => {ofClass Ring, ", ", ofClass Module, ", or ", ofClass ChainComplex}
    },
    Outputs => {
	{ofClass RingMap, ", ", ofClass Matrix, ", or ", ofClass ChainComplexMap, " the identity map on ", TT "F"}
    },
    EXAMPLE lines ///
        R = QQ[a..d];
	id_R
	id_(R^3)
	C = res coker vars R
	id_C
    ///
}
