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
