document {
    Key => length,
    Headline => "length of an object"
    }

document {
    Key => (length, Dictionary),
    Headline => "length of a dictionary",
    Usage => "n = length d",
    Inputs => { "d" },
    Outputs => { "n" => { "the number of entries in ", TT "d" } }
    }

document {
    Key => (length, VisibleList),
    Headline => "length of a visible list",
    Usage => "n = length x",
    Inputs => { "x" },
    Outputs => { "n" => { "the number of entries in ", TT "x" } }
    }

document {
    Key => (length, GradedModule),
    Headline => "length of a graded module",
    "The length of a graded module is the difference between the largest and
    smallest indices of occupied spots.  Chain complexes are graded modules,
    so this function applies to them, too.",
    EXAMPLE {
	"R = QQ[x..z];",
	"C = res coker vars R",
	"length C"
	}
    }
