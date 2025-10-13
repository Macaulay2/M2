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
    },
    Headline => "identity map",
    Usage => "id_F",
    Inputs => {
	"F" => {ofClass Ring, " or ", ofClass Module}
    },
    Outputs => {
	{ofClass RingMap, " or ", ofClass Matrix, " of the identity map on ", TT "F"}
    },
    EXAMPLE lines ///
        R = QQ[a..d];
	id_R
	id_(R^3)
	C = res coker vars R
	id_C
    ///
}
