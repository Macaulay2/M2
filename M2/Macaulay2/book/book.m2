--		Copyright 1995 by Daniel R. Grayson

load "booktex.m2"

--------------------------------------------------- make the tex file
bk = openOut (if hypertex then "M2hbook.tex" else "M2book.tex")
--------------------------------------------
bk << ///
%% Macaulay 2 manual
%% plain tex
%% Copyright 1996, by Daniel R. Grayson and Michael E. Stillman

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% some macros

\hsize=420pt

\def\cite#1#2{{\bf #1} [#2]}
{\obeyspaces\global\let =\ \tt}
\def\beginverbatim{%
     \begingroup
     % \parindent=24pt
     \baselineskip=9.5pt
     \tt
     \obeyspaces\def\par{\leavevmode\hss\endgraf}\obeylines}
\def\endverbatim{\endgroup}

%% double columns, compare TeX Book, page 417

\catcode`@=11
\newdimen\ohsize
\newdimen\ovsize
\newbox\partialpage
\def\begintwocolumn{%
    \begingroup
    \output={\global\setbox\partialpage=\vbox{\unvbox255\medskip}}\eject
    \output={\twocolumnoutput}
    \ohsize=\hsize
    \ovsize=\vsize
    \multiply\vsize by 2    	\advance\vsize by 1pc
    \divide\hsize by 2  	\advance\hsize by -20pt
    \def\fullline{\hbox to \ohsize}
    \def\makeheadline{%
	    \vbox to 0pt {%
		    \vskip -22.5pt
		    \fullline{\vbox to 8.5pt{}\the\headline}%
		    \vss}%
	    \nointerlineskip
	    }
    \def\makefootline{%
	    \baselineskip 24pt
	    \fullline{\the\footline}}
    }
\def\pagesofar{%
  \unvbox\partialpage
  \wd0=\hsize 
  \wd2=\hsize 
  \hbox to \ohsize {\box0\hfil\box2}}
\def\twocolumnoutput{%
  \dimen@=\ovsize
  \advance\dimen@ by -\ht\partialpage
  \splittopskip=\topskip
  \splitmaxdepth=\maxdepth
  \setbox0=\vsplit 255 to \dimen@
  \setbox2=\vsplit 255 to \dimen@
  \shipout\vbox{%
      \makeheadline
      \pagesofar
      \makefootline
      }%
  \advancepageno
  \unvbox255
  \penalty\outputpenalty
  }

\def\endtwocolumn{%
    \output={\balancecolumns}\eject
    \endgroup
    }

\def\balancecolumns{%
  \setbox0=\vbox{\unvbox255}%
  \dimen@=\ht0
  \advance \dimen@ by \topskip
  \advance \dimen@ by -\baselineskip
  \divide  \dimen@ by 2
  \splittopskip=\topskip
  {\vbadness=10000 
    \loop 
      \global\setbox3=\copy0
      \global\setbox1=\vsplit3 to \dimen@
      \ifdim \ht3 > \dimen@ \global\advance\dimen@ by 1pt 
    \repeat
  }%
  \setbox0=\vbox to\dimen@{\unvbox1}
  \setbox2=\vbox to\dimen@{\unvbox3}
  \pagesofar
  }

\catcode`@=12

///;
--------------------------------------------
bk << if hypertex then ///
\def\sectionhdr#1#2{%
	\bigskip\goodbreak
	\special{html:<A name="#2">}
	\line{\bf#2\ \ \leaders\hrule\hfill\ \ #1\ \ \leaders\hrule\hfill\ \ #2}%
	\special{html:</A>}
	\medskip\noindent\ignorespaces
	}
/// else ///
\def\sectionhdr#1#2{%
	\bigskip\goodbreak
	\line{\bf#2\ \ \leaders\hrule\hfill\ \ #1\ \ \leaders\hrule\hfill\ \ #2}%
	\medskip\noindent\ignorespaces
	}
///
--------------------------------------------
bk << ///

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% title page

\begingroup
\font\tf=cmr10 scaled \magstep 4
\null
\vskip 2 in
\centerline{\tf Macaulay 2}
\vskip .35 in
\centerline{Daniel R. Grayson and Michael E. Stillman}
\vskip .5 in
\centerline{version /// << version#"VERSION" << ///}
\vskip .5 in
\centerline{tutorial sections written with David Eisenbud}
\endgroup
\vfill\eject

///
--------------------------------------------
-- this depends on the feature of hash tables that when the keys
-- are consecutive integers starting at 0, the keys are scanned
-- in the natural order, which in turn depends on the hash number of
-- a small integer being the integer itself

scan(pairs nodeTable, (i,node) -> (
	  bk << ///

\sectionhdr{///
	  << concatenate cmrLiteral node << "}{" << sectionNumberTable#i 
	  << "}" << endl
	  << concatenate booktex doc node << endl;
	  )
     )

---------------------------------
-- all done

bk << ///

\end
/// << close
