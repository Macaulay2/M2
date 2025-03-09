document {
    Key => {
	 netList,
	(netList, VisibleList),
	[netList, Boxes],
	[netList, BaseRow],
	[netList, HorizontalSpace],
	[netList, VerticalSpace],
	[netList, Alignment]},
    Headline => "a table of boxes",
    Usage => "netList v",
    Inputs => {
	"v" => {"a list of lists of things to be converted to nets and displayed as a table in a net"},
	Boxes => {"whether to draw boxes around the individual nets.
	    Can be a Boolean, or a pair controlling separately the horizontal and vertical lines of the boxes.
	    Each element of the pair is either a Boolean (draw all or none) or a list of rows/columns where lines are to inserted."},
	BaseRow => ZZ => {"the index of the base row, for the purpose of setting the baseline of the net produced.  The value
	    is allowed to be as large as the length of ", TT "v", ", larger by 1 than one might expect."},
	HorizontalSpace => ZZ => {"the amount of space horizontally between entries or between entries and their enclosing boxes"},
	VerticalSpace => ZZ => "the amount of space vertically between entries or between entries and their enclosing boxes",
	Alignment => {TT "Center", ", ", TT "Left", ", ", TT "Right", ", or a list of those symbols indicating horizontal adjustment; if it's a list, the ", TT "i", "-th
	    entry specifies the adjustment in the ", TT "i", "-th column; if not, the symbol applies to all columns."}
	},
    Outputs => {{"a net obtained by converting the elements of each list in the list of lists ", TT "v",
	    " to nets and arranging them in a table, as specified by the options"}},
    EXAMPLE lines ///
	  f = {{"hi there","foo"},{-3, 2^40}}
	  netList f
	  netList(f,Boxes=>false)
	  netList(f,Boxes=>true,HorizontalSpace=>1,VerticalSpace=>1)
	  netList(f,Boxes=>true,Alignment=>Center)
	  netList(f,Boxes=>true,BaseRow=>1)
	  netList(f,Boxes=>{{1},{1}})
	  netList apply(5,i->apply(i+1,j->(i,j)))
	  netList(apply(5,i->apply(i+1,j->(i,j))),Boxes=>{true,false})
    ///,
    SeeAlso => {
	net,
    }
}
