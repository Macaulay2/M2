undocumented methods hypertext
document {
     Key => hypertext,
     Headline => "prepare hypertext for display",
     Usage => "hypertext x",
     Inputs => {
	  "x" => List => {"a list of strings and hypertext mark-up lists"}
	  },
     Outputs => {
	  Hypertext => {"a new list of mark-up lists obtained from the old by making the format more suitable for
	       ultimate display"}
	  },
     "Here is a list of some of the transformations that are performed.",
     UL {
	  {"occurrences of ", TT "null", ", such as those that might have been produced by the insertion of an extra comma, are removed"},
	  {TO2 {Sequence,"sequences"}, ", such as those produced by extra sets of parentheses, are ", TO2{ "splice","spliced"}, " into the lists containing them"},
	  {"the contents of lists and sequences are merged into the mark-up lists containing them" },
	  {"mark-up lists of type ", TO "TO", " occurring a mark-up list of type ", TO "UL", " are converted to lists of type ", TO "TOH", ", so the headlines of
	       the items will appear in the resulting menu"},
	  {"the targets of the links in mark-up lists of type TO, TOH, and TO2, are converted into ", TO2 {"DocumentTag", "document tags"}},
	  {"mark-up types, such as ", TO "PARA", ", are converted into lists of length 0 of that type"},
	  {"strings spanning multiple lines are wrapped into one long line after appropriately trimming the spaces at the beginning and end of each line"},
	  {"an error message is produced if something not recognizable as documentation is encountered"},
	  },
     "We may phase out this function in favor of performing the listed transformations automatically when hypertext elements are created.",
     SeeAlso => {"Hypertext"}
     }
