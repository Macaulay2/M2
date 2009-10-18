--		Copyright 1995 by Daniel R. Grayson

load "booktex.m2"

--------------------------------------------------- make the tex file
bookFile = openOut "M2book.tmp"
--------------------------------------------
bookFile << ///
%% Macaulay2 manual
%% latex
%% Copyright 1996-1999, by Daniel R. Grayson and Michael E. Stillman

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% some macros

\documentclass{amsbook}

% we are using hyperref.sty version: 1999/10/14 v6.66m Hypertext links for LaTeX]
% available from /ftp@sunsite.unc.edu:/pub/packages/TeX/macros/latex/contrib/supported/hyperref
\usepackage[
	bookmarksnumbered=true,
        pdftitle={Macaulay2},
        pdfsubject={symbolic algebra},
        pdfkeywords={syzygy Groebner resolution polynomials},
        pdfauthor={Daniel R. Grayson and Michael E. Stillman},
    	colorlinks=true
        ]{hyperref}

\setcounter{secnumdepth}{10}
\catcode`\@=11
\def\subsection{\@startsection{subsection}{2}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubsection{\@startsection{subsubsection}{3}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\paragraph{\@startsection{paragraph}{4}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subparagraph{\@startsection{subparagraph}{5}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubparagraph{\@startsection{subparagraph}{6}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubsubparagraph{\@startsection{subparagraph}{7}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubsubsubparagraph{\@startsection{subparagraph}{8}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubsubsubsubparagraph{\@startsection{subparagraph}{9}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\def\subsubsubsubsubsubparagraph{\@startsection{subparagraph}{10}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\centering}}
\catcode`\@=12

\renewcommand{\thepart}{\Roman{part}}
\renewcommand{\thechapter}{\arabic{chapter}}
\renewcommand{\thesection}{\thechapter.\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}
\renewcommand{\thesubsubsection}{\thesubsection.\arabic{subsubsection}}

\makeindex

% \parindent=10pt
% \parskip=4pt

\overfullrule=0pt

{\obeyspaces\global\let =\ \tt}
\def\beginverbatim{%
     \begingroup
     % \parindent=24pt
     \baselineskip=9.5pt
     \tt
     \parskip=0pt
     \obeyspaces\def\par{\leavevmode\hss\endgraf}\obeylines}
\def\endverbatim{\endgroup}

\def\beginsection#1{{\bf #1}}

% \font\headerFontOne=cmbx12 scaled \magstep 1
% \font\headerFontTwo=cmbx12 scaled \magstep 1
% \font\headerFontThree=cmbx12
% \font\headerFontFour=cmbx12
% \font\headerFontFive=cmbx10
% \font\headerFontSix=cmbx10

\begin{document}
        \title{Macaulay2 \\ A system for computations in \\ algebraic geometry and commutative algebra}
        % \thanks{Supported by the NSF}
        \author[Grayson]{Daniel R. Grayson}
        \address{University of Illinois at Urbana-Champaign}
        \email{dan\char`\@math.uiuc.edu}
        \urladdr{\href{http://www.math.uiuc.edu/~dan}{http://www.math.uiuc.edu/\char`\~dan}}
        \author[Stillman]{Michael E. Stillman}
        \address{Cornell University}
        \email{mike\char`\@math.cornell.edu}
        \urladdr{\href{http://www.math.cornell.edu/~mike/}{http://www.math.cornell.edu/\char`\~mike/}}
        \date{/// << version#"compile time" << ///}
        \maketitle
	\tableofcontents
///
--------------------------------------------
    -- this loop depends on the feature of hash tables that when the keys
    -- are consecutive integers starting at 0, the keys are scanned
    -- in the natural order, which in turn depends on the hash number of
    -- a small integer being the integer itself

    levelLimit := 10;

    sectionType := sectionNumber -> (
	 level := # select(characters sectionNumber, i -> i === ".");
	 if level > levelLimit then level = levelLimit;
	 if level === 0 then "\\part" else
	 if level === 1 then "\\chapter" else
	 if level === 2 then "\\section" else
	 if level === 3 then "\\subsection" else
	 if level === 4 then "\\subsubsection" else
	 if level === 5 then "\\paragraph" else
	 if level === 6 then "\\subparagraph" else
	 if level === 7 then "\\subsubparagraph" else
	 if level === 8 then "\\subsubsubparagraph" else
	 if level === 9 then "\\subsubsubsubparagraph" else
	 "\\subsubsubsubsubparagraph"
	 )

    scan(pairs getNameFromNumber, (i,node) -> (
	      n := sectionNumberTable#i;
	      d := documentationMemo node;
	      -- oops: what was this for?
	      -- if class d#0 === CENTER then d = drop(d,1);
	      bookFile << endl << endl
	      << "\\hypertarget{" << n << "}{}" << endl
	      << sectionType n << "{" << tex formatDocumentTag node << "}"
	      << "\\label{" << n << "}" << endl;
	      if getenv "DEBUG" != "" then bookFile << " (label: " << n << ") " << endl;
	      bookFile << concatenate booktex d << endl;
	      )
	 )
-----------------------------------------------------------------------------
-- all done

bookFile << ///

We should cite at least one paper \cite{MR47:3318}.

\bibliographystyle{plain}
\bibliography{papers}
\printindex
\end{document}
/// << close
