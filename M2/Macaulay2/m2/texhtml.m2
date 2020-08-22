-- tex to html conversion -*- coding: utf-8 -*-

html TEX := str -> (
     local oldstr;
     str = concatenate str;
     origstr := str;
     abbrev := () -> format if #origstr > 20 then (substring(0,20,origstr) | "...") else origstr;
     f := (p,r) -> (
	  n := replace(p,r,str);
	  if n != str and debugLevel == 120 then (
	       stderr << "html TEX: ///" << str << "/// matches ///" << p << "/// and becomes ///" << n << "///" << endl;
	       );
	  str = n);
     f("<","--TEMPORARY AMPERSAND--lt;");					   -- as in htmlLiteral
     f("]]>","]]--TEMPORARY AMPERSAND--gt;");					   -- as in htmlLiteral
     -- we could try replacing \$ by \dollar and then bring it back later...
     -- but watch out for \\$ and \\\\$ ...
     -- but replace \\\$ and \\\\\$ ...
     while match("\\$.*\\$", str) do (
     	  f(///(^|[^\$])\$\$([^$]*[^\$])?\$\$([^$]|$)///,///\1</p><div style="text-align:center"><i>\2</i></div><p>\3///);
     	  f(///(^|[^\$])\$([^$]*[^\$])\$([^$]|$)///,///\1<i>\2</i>\3///);
	  );
     if match(///(^|[^\\])\$///,str) then error("unmatched dollar signs in TeX string ",abbrev());
     f(///\\\{///,///\\lbrace ///);
     f(///\\\}///,///\\rbrace ///);

     --	    \begin{pmatrix}	    <table><tr><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    \\			    </td></tr><tr><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    \\			    </td></tr><tr><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    &			    </td><td>
     --				    bb
     --	    \end{pmatrix}	    </td></tr></table>
     while (
	  -- this will not quite work if there are two matrices in the string!
	  oldstr = str;
     	  f(///(\\begin\{pmatrix\}.*)(&)(.*\\end\{pmatrix\})///, ///\1</td><td>\3///);
     	  f(///(\\begin\{pmatrix\}.*)(\\\\)(.*\\end\{pmatrix\})///, ///\1</td></tr><tr><td>\3///);
	  oldstr != str
	  ) do null;
     f(///\\begin\{pmatrix\}(.*)\\end\{pmatrix\}///, ///
</i><table class="matrix" border="1"><tr><td><table><tr><td>\1</td></tr></table></td></tr></table><i>
///);							 -- <table> can't be inside <i>; we currently aren't putting <i> inside each entry
     while (
	  oldstr = str;
	  f(///\{ *\\bf +([^{}]*)\}///,///{<b>\1</b>}///);
	  f(///\{ *\\mathbf +([^{}]*)\}///,///{<b>\1</b>}///);
	  f(///\{ *\\rm +([^{}]*)\}///,///{\1}///);
	  f(///\{ *\\it +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\{ *\\tt +([^{}]*)\}///,///{<tt>\1</tt>}///);
	  f(///\{ *\\em +([^{}]*)\}///,///{<em>\1</em>}///);
	  f(///\{ *\\cal +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\{ *\\mathcal +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\\url *\{([^{}]*)\}///,///<a href="\1">\1</a>///);
	  f(///\\frac *\{([^{}]*)\}\{([^{}]*)\}///,///{(\1)/(\2)}///);
	  f(///\{([^{}]*)\\over *([^{}]*)\}///,///{\1/\2}///);
	  f(///\^ *\{([^{}]*)\}///,///<sup>\1</sup>///);
	  f(///_ *\{([^{}]*)\}///,///<sub>\1</sub>///);
	  oldstr != str) do null;
     f(///\\mathbb +N///,///&#x2115;///);
     f(///\\mathbb +Q///,///&#x211A;///);
     f(///\\mathbb +R///,///&#x211D;///);
     f(///\\mathbb +Z///,///&#x2124;///);
     f(///\\mathbb +P///,///&#x2119;///);
     f(///\\mathbb *\{ *N *\}///,///&#x2115;///);
     f(///\\mathbb *\{ *Q *\}///,///&#x211A;///);
     f(///\\mathbb *\{ *R *\}///,///&#x211D;///);
     f(///\\mathbb *\{ *Z *\}///,///&#x2124;///);
     f(///\\mathbb *\{ *P *\}///,///&#x2119;///);
     f(///\\mathbb +(.)///,///<b>\1</b>///);
     f(///\\bar *\{ *([A-Za-z]) *\}///,///<span style="text-decoration: overline">\1</span>///);
     f(///\\bar +([A-Za-z])///,///<span style="text-decoration: overline">\1</span>///);
     f(///\\Cal +([A-Za-z])///,///<i>\1</i>///);
     f(///\\\^a///,///&acirc;///);
     f(///\\\^e///,///&ecirc;///);
     f(///\\\^(.)///,///\1///);
     f(///\^(\\[a-zA-Z]*)///,///<sup>\1</sup>///);
     f(///_(\\[a-zA-Z]*)///,///<sub>\1</sub>///);
     f(///\^ *(.)///,///<sup>\1</sup>///);
     f(///_ *(.)///,///<sub>\1</sub>///);
     if match(///\\\\///,str) then error(///in conversion to html, unknown TeX control sequence \\ in string ///,abbrev());
     f(///\\frac *(.) *(.)///,///{\1/\2}///);
     f(///\\"a///,///&auml;///);			    -- "
     f(///\\"o///,///&ouml;///);			    -- "
     f(///\\"u///,///&uuml;///);			    -- "
     f(///\\#///,///#///);
     f(///\\&///,///&amp;///);
     f(///\\,///,///&nbsp;///);
     f(///\\' *e///,///&eacute;///);
     f(///\\` *e///,///&egrave;///);
     f(///\\[`'](.)///,///\1///);
     f(///``///,///&ldquo;///);
     f(///`///,///&lsquo;///);
     f(///''///,///&rdquo;///);
     f(///'///,///&rsquo;///); -- This is for text.  But an apostrophe in math mode should be a prime!
     f(///\\NN\> *///,///&#x2115;///);			    -- these unicode characters are experimental
     f(///\\QQ\> *///,///&#x211A;///);			    -- on at least some machines they are represented by bitmaps, not by truetype fonts!
     f(///\\RR\> *///,///&#x211D;///);
     f(///\\R\> *///,///&#x211D;///);			   -- used by arxiv.org
     f(///\\C\> *///,///&#x2102;///);			   -- used by arxiv.org
     f(///\\CC\> *///,///&#x2102;///);
     f(///\\ZZ\> *///,///&#x2124;///);
     f(///\\PP\> *///,///&#x2119;///);
     f(///\\Delta\> *///,///&Delta;///);
     f(///\\Gamma\> *///,///&Gamma;///);
     f(///\\Lambda\> *///,///&Lambda;///);
     f(///\\Omega\> *///,///&Omega;///);
     f(///\\Phi\> *///,///&Phi;///);
     f(///\\Pi\> *///,///&Pi;///);
     f(///\\Psi\> *///,///&Psi;///);
     f(///\\Sigma\> *///,///&Sigma;///);
     f(///\\Theta\> *///,///&Theta;///);
     f(///\\Upsilon\> *///,///&Upsilon;///);
     f(///\\Xi\> *///,///&Xi;///);
     f(///\\aleph\> *///,///&aleph;///);
     f(///\\alpha\> *///,///&alpha;///);
     f(///\\beta\> *///,///&beta;///);
     f(///\\beth\> *///,///&beth;///);
     f(///\\bf\> *///,//////);
     f(///\\break\> *///,///<br/>///);
     f(///\\bullet\> *///,///&bull;///);
     f(///\\cap\> *///,///&cap;///);
     f(///\\cdot\> *///,///&#8901;///);
     f(///\\cdots\> *///,///&hellip;///);
     -- f(///\\centerline\> *///,"");
     f(///\\cong\> *///,///&#8773;///);
     f(///\\cos\> *///,///cos///);
     f(///\\cup\> *///,///&cup;///);
     f(///\\daleth\> *///,///&daleth;///);
     -- f(///\\datefont\> *///,//////);
     f(///\\delta\> *///,///&delta;///);
     f(///\\dots\> *///,///&hellip;///);
     f(///\\ell\> *///,///<em>l</em>///);
     f(///\\emptyset\> *///,///&Oslash;///);
     f(///\\epsilon\> *///,///&epsilon;///);
     f(///\\equiv\> *///,///&equiv;///);
     f(///\\exists\> *///,///&exist;///);
     f(///\\forall\> *///,///&forall;///);
     f(///\\gamma\> *///,///&gamma;///);
     f(///\\geq?\> *///,///&ge;///);
     f(///\\gimel\> *///,///&gimel;///);
     f(///\\in\> *///,///&isin;///);
     f(///\\infty\> *///,///&infin;///);
     f(///\\int\> *///,///&int;///);
     -- f(///\\it\> *///,//////);
     f(///\\lambda\> *///,///&lambda;///);
     f(///\\ldots\> *///,///...///);
     f(///\\leftarrow\> *///,///&larr;///);
     f(///\\leq?\> *///,///&le;///);
     f(///\\mapsto\> *///,///↦///);
     f(///\\mathbb\> *///,//////);
     f(///\\mathbf\> *///,//////);
     f(///\\mathcal\> *///,//////);
     f(///\\mid\> *///,///&nbsp;|&nbsp;///);
     f(///\\mod\> *///,///mod///);
     f(///\\mu\> *///,///&mu;///);
     f(///\\neq?\> *///,///&ne;///);
     f(///\\nu\> *///,///&nu;///);
     f(///\\omega\> *///,///&omega;///);
     f(///\\oplus\> *///,///&oplus;///);
     f(///\\otimes\> *///,///&otimes;///);
     f(///\\par\> *///,///<p/>///);
     f(///\\partial\> *///,///&part;///);
     f(///\\phi\> *///,///&phi;///);
     f(///\\pi\> *///,///&pi;///);
     f(///\\prime\> *///,///&prime;///);
     f(///\\prod\> *///,///&prod;///);
     f(///\\psi\> *///,///&psi;///);
     f(///\\rho\> *///,///&rho;///);
     f(///\\rightarrow\> *///,///&rarr;///);
     f(///\\rm\> *///,//////);
     f(///\\setminus\> *///,///&#92;///);
     f(///\\sigma\> *///,///&sigma;///);
     f(///\\sin\> *///,///sin///);
     f(///\\subset\> *///,///&sub;///);
     f(///\\subseteq\> *///,///&sube;///);
     f(///\\supset\> *///,///&sup;///);
     f(///\\supseteq\> *///,///&supe;///);
     f(///\\sum\> *///,///&sum;///);
     f(///\\tau\> *///,///&tau;///);
     -- f(///\\textrm\> *///,//////);
     f(///\\\$ *///,///$///);
     f(///\\theta\> *///,///&theta;///);
     f(///\\times\> *///,///&times;///);
     f(///\\to\> *///,///&rarr;///);
     f(///\\ +///,///&nbsp;///);
     f(///\\wedge\> *///,///&and;///);
     f(///\\wp\> *///,///&weierp;///);
     f(///\\xi\> *///,///&xi;///);
     f(///\\zeta\> *///,///&zeta;///);
     -- f(///Macaulay2///,///<i>Macaulay2</i>///); -- this is a bad idea because it interferes with URLs and filenames
     f(///Macaulay 2///,///<i>Macaulay2</i>///);
     while (
	  oldstr = str;
     	  f(///\{([^{}]*)\}///,///\1///);
	  oldstr != str) do null;
     f(///\\lbrace\> *///,///{///);
     f(///\\rbrace\> *///,///}///);
     f(///\\backslash\> *///,"--TEMPORARY BACKSLASH--");
     r := unique sort select("\\\\(.|[a-zA-Z]+)?",str);
     if #r > 0 then error("in conversion to html, unknown TeX control sequence(s): ",concatenate between(", ",r)," in string ",abbrev());
     f("--TEMPORARY AMPERSAND--","&");
     f("--TEMPORARY BACKSLASH--",///\///);
     str)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
