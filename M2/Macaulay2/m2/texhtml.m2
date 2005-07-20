-- tex to html conversion

 --	a_b
 --	a^b
 --	{}		for grouping
 --	\frac{...}{...}
 --	\mathbb		bold
 --	\mathcal	bold
 --	\mathfrak	bold
 --	\it
 --	\textrm{...}
 --	\alpha ...	&alpha;
 --	\Gamma ...	&Gamma;
 --
 --    //////////////////////////////////////////////////////////
 --    // function remove_tex ($string) replaces tex symbols for
 --    // clean html view
 --    // somewhat improved using regular expressions -- YNM
 --    // cleaned up/customized for SB -- SEP
 --    function remove_tex ($subject) {
 --      $retval = $subject;
 --      $retval = ereg_replace("\\$\\$([^$]*)\\$\\$","<p align=center><i>\\1</i></p>",$retval);
 --      $retval = ereg_replace("\\$([^$]*)\\$","<i>\\1</i>",$retval);
 --    for ($i=0; $i<8; $i++) {
 --    // { font command ...} without nested braces.Leaves braces incase
 --    // they are used by ^ or _.
 --      $retval = ereg_replace("{ *\\\\bf ([^{}]*)}","{<b>\\1</b>}",$retval);
 --      $retval = ereg_replace("{ *\\\\mathbf ([^{}]*)}","{<b>\\1</b>}",$retval);
 --      $retval = ereg_replace("{ *\\\\mathbb ([^{}]*)}","{<b>\\1</b>}",$retval);
 --      $retval = ereg_replace("{ *\\\\rm ([^{}]*)}","{\\1}",$retval);  // how ?
 --      $retval = ereg_replace("{ *\\\\it ([^{}]*)}","{<i>\\1</i>}",$retval);
 --      $retval = ereg_replace("{ *\\\\em ([^{}]*)}","{<em>\\1</em>}",$retval);
 --      $retval = ereg_replace("{ *\\\\cal ([^{}]*)}","{<i>\\1</i>}",$retval);
 --      $retval = ereg_replace("{ *\\\\mathcal ([^{}]*)}","{<i>\\1</i>}",$retval);
 --    // \url:
 --      $retval = ereg_replace("\\\\url{([^{}]*)}",
 --			"<a href=\"\\1\" target=blank>\\1</a>",$retval);
 --    // \frac:
 --      $retval = ereg_replace("\\\\frac{([^{}]*)}{([^{}]*)}","{(\\1)/(\\2)}",$retval);
 --      $retval = ereg_replace("\\\\frac([0-9])([0-9])","{\\1/\\2}",$retval);
 --    // \over:
 --      $retval = ereg_replace("{([^{}]*)\\\\over([^{}]*)}","{(\\1)/(\\2)}",$retval);
 --    // ^\macro and _\macro:
 --      $retval = ereg_replace("\^(\\\\[a-zA-Z]*)","<sup>\\1</sup>",$retval);
 --      $retval = ereg_replace("\_(\\\\[a-zA-Z]*)","<sub>\\1</sub>",$retval);
 --    // ^x and _x where x is any letter or number
 --      $retval = ereg_replace("\^([0-9b-zA-Z])","<sup>\\1</sup>",$retval);
 --      $retval = ereg_replace("\_([0-9a-zA-Z])","<sub>\\1</sub>",$retval);
 --    // ^{...} and _{...}  with no nested braces:
 --      $retval = ereg_replace("\^{([^{}]*)}","<sup>\\1</sup>",$retval);
 --      $retval = ereg_replace("\_{([^{}]*)}","<sub>\\1</sub>",$retval);
 --      // get rid of dummy braces
 --      $retval = ereg_replace("([^^_]|^){ *([^\\\\][^{}]*)}","\\1\\2",$retval);
 --    }
 --    //  $retval = str_replace('$','', $retval);
 --      $retval = str_replace('\mathbb','', $retval);
 --      $retval = str_replace('\mathcal','',$retval);
 --      $retval = str_replace('\mathbf','',$retval);
 --      $retval = str_replace('\textrm','',$retval);
 --      $retval = str_replace('\rm','',$retval);
 --      $retval = str_replace('\it','',$retval);
 --      $retval = str_replace('\bf','',$retval);
 --      $retval = str_replace('\epsilon','&epsilon;',$retval);
 --      $retval = str_replace('\delta','&delta;',$retval);
 --      $retval = str_replace('\Omega','&Omega;',$retval);
 --      $retval = str_replace('\omega','&omega;',$retval);
 --      $retval = str_replace('\Psi','&Psi;',$retval);
 --      $retval = str_replace('\psi','&psi;',$retval);
 --      $retval = str_replace('\alpha','&alpha;',$retval);
 --      $retval = str_replace('\beta','&beta;',$retval);
 --      $retval = str_replace('\gamma','&gamma;',$retval);
 --      $retval = str_replace('\Gamma','&Gamma;',$retval);
 --      $retval = str_replace('\zeta','&zeta;',$retval);
 --      $retval = str_replace('\theta','&theta;',$retval);
 --      $retval = str_replace('\Theta','&Theta;',$retval);
 --      $retval = str_replace('\lambda','&lambda;',$retval);
 --      $retval = str_replace('\Lambda','&Lambda;',$retval);
 --      $retval = str_replace('\mu','&mu;',$retval);
 --      $retval = str_replace('\nu','&nu;',$retval);
 --      $retval = str_replace('\pi','&pi;',$retval);
 --      $retval = str_replace('\rho','&rho;',$retval);
 --      $retval = str_replace('\sigma','&sigma;',$retval);
 --      $retval = str_replace('\tau','&tau;',$retval);
 --      $retval = str_replace('\xi','&xi;',$retval);
 --      $retval = str_replace('\phi','&phi;',$retval);
 --      $retval = str_replace('\to','&rarr;',$retval);
 --      $retval = str_replace('\mapsto','&rarr;',$retval);
 --      $retval = str_replace('\partial','&part;',$retval);
 --      $retval = str_replace('\rightarrow','&rarr;',$retval);
 --     $retval = str_replace('\leftarrow','&larr;',$retval);
 --      $retval = str_replace('\equiv','&equiv;',$retval);
 --      $retval = str_replace('\cong','&#8773;',$retval);
 --      $retval = str_replace('\geq','&ge;',$retval);
 --      $retval = str_replace('\leq','&le;',$retval);
 --      $retval = str_replace('\infty','&infin;',$retval);
 --      $retval = str_replace('\int','&int;',$retval);
 --      $retval = str_replace('\in','&isin;',$retval);
 --      $retval = str_replace('\subset','&sub;',$retval);
 --      $retval = str_replace('\sin','sin',$retval);
 --      $retval = str_replace('\cos','cos',$retval);
 --
 --      $retval = str_replace('\aleph','&aleph;',$retval);
 --      $retval = str_replace('\beth','&beth;',$retval);
 --      $retval = str_replace('\gimel','&gimel;',$retval);
 --      $retval = str_replace('\daleth','&daleth;',$retval);
 --
 --      $retval = str_replace('\"a','&auml;',$retval);
 --      $retval = str_replace('\"u','&uuml;',$retval);
 --      $retval = str_replace('\"o','&ouml;',$retval);
 --      $retval = str_replace('\`e','&egrave;',$retval);
 --      $retval = str_replace('\\\'e','&eacute;',$retval);
 --      $retval = str_replace('\^e','&ecirc;',$retval);
 --      $retval = str_replace('\^a','&acirc;',$retval);
 --
 --      $retval = str_replace('\ell','<em>l</em>',$retval);
 --      $retval = str_replace('\H','<b>H</b>',$retval);
 --
 --      $retval = str_replace('\,','&nbsp;',$retval);
 --      $retval = str_replace('\ldots,','...',$retval);
 --      $retval = str_replace('\cdots,','...',$retval);
 --      $retval = str_replace('\bullet','&bull;',$retval);
 --      $retval = str_replace('\&','&amp;',$retval);
 --
 --      $retval = str_replace('\infty','&infin;',$retval);
 --      $retval = str_replace('\forall','&forall;',$retval);
 --      $retval = str_replace('\exists','&exist;',$retval);
 --      $retval = str_replace('\prime','&prime;',$retval);
 --      $retval = str_replace('\times','x',$retval);
 --      $retval = str_replace('\cup','&cup;',$retval);
 --      $retval = str_replace('\cap','&cap;',$retval);
 --      $retval = str_replace('\sum','&sum;',$retval);
 --      $retval = str_replace('\prod','&prod;',$retval);
 --      $retval = str_replace('\otimes','&otimes;',$retval);
 --      $retval = str_replace('\oplus','&oplus;',$retval);
 --      $retval = str_replace('\wedge','&and;',$retval);
 --      $retval = str_replace('\emptyset','&Oslash',$retval);
 --      $retval = str_replace('\setminus','&#92;',$retval);
 --      $retval = str_replace('\mod','mod',$retval);
 --      $retval = str_replace('\wp','&weierp;',$retval);
 --      $retval = str_replace('%%%','',$retval);
 --      $retval = str_replace('\centerline','',$retval);
 --      $retval = str_replace('\datefont','',$retval);
 --

html TEX := str -> (
     str = str#0;
     f := (p,r) -> (
	  n := replace(p,r,str);
	  if n != str and debugLevel > 0 then (
	       stderr << "html TEX: ///" << str << "/// matches ///" << p << "/// and becomes ///" << n << "///" << endl;
	       );
	  str = n);
     f(///\$\$([^$]*)\$\$///,///<p align=center><i>\1</i></p>///);
     f(///\$([^$]*)\$///,///<i>\1</i>///);
     f(///''///,///&rdquo;///);
     f(///'///,///&rsquo;///); -- This is for text.  An apostrophe in math mode would be a prime, but we can't tell the difference, too bad.
     f(///``///,///&ldquo;///);
     f(///`///,///&lsquo;///);
     f(///\\\\///,///\backslash///);
     f(///\\\{///,///\lbrace///);
     f(///\\\}///,///\rbrace///);
     while (
	  oldstr := str;
	  f(///\{ *\\bf +([^{}]*)\}///,///{<b>\1</b>}///);
	  f(///\{ *\\mathbf +([^{}]*)\}///,///{<b>\1</b>}///);
	  f(///\{ *\\mathbb +([^{}]*)\}///,///{<b>\1</b>}///);
	  f(///\{ *\\rm +([^{}]*)\}///,///{\1}///);
	  f(///\{ *\\it +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\{ *\\tt +([^{}]*)\}///,///{<tt>\1</tt>}///);
	  f(///\{ *\\em +([^{}]*)\}///,///{<em>\1</em>}///);
	  f(///\{ *\\cal +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\{ *\\mathcal +([^{}]*)\}///,///{<i>\1</i>}///);
	  f(///\\url\{([^{}]*)\}///,///<a href="\1" target=blank>\1</a>///);
	  f(///\\frac\{([^{}]*)\}\{([^{}]*)\}///,///(\1)/(\2)///);
	  f(///\{([^{}]*)\\over([^{}]*)\}///,///{(\1)/(\2)}///);
	  f(///\^ *\{([^{}]*)\}///,///<sup>\1</sup>///);
	  f(///_ *\{([^{}]*)\}///,///<sub>\1</sub>///);
	  oldstr != str) do null;
     f(///\{([^{}]*)\}///,///\1///);
     f(///\^(\\[a-zA-Z]*)///,///<sup>\1</sup>///);
     f(///_(\\[a-zA-Z]*)///,///<sub>\1</sub>///);
     f(///\^ *(.)///,///<sup>\1</sup>///);
     f(///_ *(.)///,///<sub>\1</sub>///);
     f(///\\frac([0-9])([0-9])///,///{\1/\2}///);
     f(///\\"a///,///&auml;///);
     f(///\\"o///,///&ouml;///);
     f(///\\"u///,///&uuml;///);
     f(///\\#///,///#///);
     f(///\\&///,///&amp;///);
     f(///\\'e///,///&eacute;///);
     f(///\\,///,///&nbsp;///);
     f(///\\\^a///,///&acirc;///);
     f(///\\\^e///,///&ecirc;///);
     f(///\\`e///,///&egrave;///);
     f(///\\NN\> *///,///&#x2115;///);
     f(///\\QQ\> *///,///&#x211A;///);
     f(///\\RR\> *///,///&#x211D;///);
     f(///\\ZZ\> *///,///&#x2124;///);
     f(///\\PP\> *///,///&#x2119;///);
     f(///\\Gamma\> *///,///&Gamma;///);
     f(///\\Lambda\> *///,///&Lambda;///);
     f(///\\Omega\> *///,///&Omega;///);
     f(///\\Psi\> *///,///&Psi;///);
     f(///\\Theta\> *///,///&Theta;///);
     f(///\\aleph\> *///,///&aleph;///);
     f(///\\alpha\> *///,///&alpha;///);
     f(///\\backslash\> *///,///\///);
     f(///\\beta\> *///,///&beta;///);
     f(///\\beth\> *///,///&beth;///);
     f(///\\bf\> *///,//////);
     f(///\\bullet\> *///,///&bull;///);
     f(///\\cap\> *///,///&cap;///);
     f(///\\cdots,\> *///,///&hellip;///);
     f(///\\centerline\> *///,//////);
     f(///\\cong\> *///,///&#8773;///);
     f(///\\cos\> *///,///cos///);
     f(///\\cup\> *///,///&cup;///);
     f(///\\daleth\> *///,///&daleth;///);
     f(///\\datefont\> *///,//////);
     f(///\\delta\> *///,///&delta;///);
     f(///\\dots,\> *///,///&hellip;///);
     f(///\\ell\> *///,///<em>l</em>///);
     f(///\\emptyset\> *///,///&Oslash///);
     f(///\\epsilon\> *///,///&epsilon;///);
     f(///\\equiv\> *///,///&equiv;///);
     f(///\\exists\> *///,///&exist;///);
     f(///\\forall\> *///,///&forall;///);
     f(///\\gamma\> *///,///&gamma;///);
     f(///\\geq\> *///,///&ge;///);
     f(///\\gimel\> *///,///&gimel;///);
     f(///\\in\> *///,///&isin;///);
     f(///\\infty\> *///,///&infin;///);
     f(///\\infty\> *///,///&infin;///);
     f(///\\int\> *///,///&int;///);
     f(///\\it\> *///,//////);
     f(///\\lambda\> *///,///&lambda;///);
     f(///\\lbrace\> *///,///{///);
     f(///\\ldots,\> *///,///...///);
     f(///\\leftarrow\> *///,///&larr;///);
     f(///\\leq\> *///,///&le;///);
     f(///\\mapsto\> *///,///&rarr;///);
     f(///\\mathbb\> *///,//////);
     f(///\\mathbf\> *///,//////);
     f(///\\mathcal\> *///,//////);
     f(///\\mid\> *///,///&nbsp;|&nbsp;///);
     f(///\\mod\> *///,///mod///);
     f(///\\mu\> *///,///&mu;///);
     f(///\\nu\> *///,///&nu;///);
     f(///\\omega\> *///,///&omega;///);
     f(///\\oplus\> *///,///&oplus;///);
     f(///\\otimes\> *///,///&otimes;///);
     f(///\\par\> *///,///<p>///);
     f(///\\partial\> *///,///&part;///);
     f(///\\phi\> *///,///&phi;///);
     f(///\\pi\> *///,///&pi;///);
     f(///\\prime\> *///,///&prime;///);
     f(///\\prod\> *///,///&prod;///);
     f(///\\psi\> *///,///&psi;///);
     f(///\\rbrace\> *///,///}///);
     f(///\\rho\> *///,///&rho;///);
     f(///\\rightarrow\> *///,///&rarr;///);
     f(///\\rm\> *///,//////);
     f(///\\setminus\> *///,///&#92;///);
     f(///\\sigma\> *///,///&sigma;///);
     f(///\\sin\> *///,///sin///);
     f(///\\subset\> *///,///&sub;///);
     f(///\\sum\> *///,///&sum;///);
     f(///\\tau\> *///,///&tau;///);
     f(///\\textrm\> *///,//////);
     f(///\\theta\> *///,///&theta;///);
     f(///\\times\> *///,///x///);
     f(///\\to\> *///,///&rarr;///);
     f(///\\wedge\> *///,///&and;///);
     f(///\\wp\> *///,///&weierp;///);
     f(///\\xi\> *///,///&xi;///);
     f(///\\zeta\> *///,///&zeta;///);
     f(///Macaulay2///,///<i>Macaulay2</i>///);
     str)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
