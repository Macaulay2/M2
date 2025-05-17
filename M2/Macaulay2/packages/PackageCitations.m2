-- -*- coding: utf-8 -*-
--  PackageCitations.m2
--
--  Copyright (C) 2017 Aaron Dall <aaronmdall@gmail.com>
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  This program is free software; you can redistribute it
--  and/or modify  it under the terms of the GNU General
--  Public License as   published by  the Free Software Foundation;
--  either version 2 of the License, or (at  your
--  option) any later version.
--
--  This program is distributed in the hope that it will be
--  useful, but  WITHOUT ANY WARRANTY; without even the
--  implied warranty of  MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.  See the GNU  General Public License
--  for more details.
--
--  You should have received a copy of the GNU General
--  Public License along with this program; if not, write
--  to the Free Software Foundation, Inc.,  51 Franklin
--  Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  Release 0.1 (2017 03)
--      NEW:
--          A method for obtaining a bibtex citation for a Macaulay2 package.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newPackage (
    "PackageCitations",
    Version => "0.1",
    Date => "2017 03 28",
    Authors => {{
        Name => "Aaron Dall",
        Email => "aaronmdall -at- gmail.com",
        HomePage => "https://www.aarondall.com"}},
    Headline => "citation of Macaulay2 packages",
    Keywords => {"Miscellaneous"},
    HomePage => "https://github.com/aarondall/PackageCitations-M2"
    )

export {
    "cite"
    --"hasGoodHeadline",  -- internal method
    --"quotesToTex",  -- internal method
    --"wrapTexStrings",  -- internal method
    --"headlineToTex" -- internal method
    }

importFrom_Core { "citePackage" }

-- store for the TeX equivalent of needed diacritics and symbols
-- add symbols as needed
-- note that the length of a diacritic (as a string in M2) is 2
texStore = hashTable {
    ///Macaulay2///     => ///\\emph{Macaulay2}///,
    ///Macaulay 2///     => ///\\emph{Macaulay2}///,
    ///Mbar_\{g,n\}///    => ///$Mbar_{g,n}$///,
    ///á///             => ///{\\'a}///,
    ///å///             => ///{\\aa}///,
    ///æ///             => ///{\\ae}///,
    ///è///             => ///{\\`e}///,
    ///é///             => ///{\\'e}///,
    ///ò///             => ///{\\`o}///,
    ///ø///             => ///{\\o}///,
}

-- PREPARE THE HEADLINE FOR USE IN THE BIBTEX TITLE

-- An internal method for checking if a package headline is a good
-- candidate for use in the citation title.
-- A good package headline satisfies the following conditions
--  (1) is n words with 0 < n <= 10,
--  (2) is not a repeat of the title, and
--  (3) does not contain a colon
hasGoodHeadline = method (TypicalValue => Boolean)
hasGoodHeadline Package := P -> (
    T := P#"pkgname";
    H := P#Options#Headline;
    -- check for a colon in the headline
    if regex(":", H) =!= null then return false else
    -- check length of headline
    L := separate (" ", H);
    if #L == 0 or #L > 10 then return false else
    -- check for nontrivial headline content
    --first check if title and headline are identical
    if T === H then return false else
    -- then list each word of headline with first letter removed
    reducedHeadline := apply (L, w -> substring (w, 1, #w));
    -- list each word (starting with an upper case letter) of title
    -- with first letter removed
    reducedTitle := delete("" ,separate(" ", replace ("[[:upper:]]", " ", T)));
    -- compare reducedTitle and reducedHeadline
    if #reducedHeadline == #reducedTitle and all (
        #reducedTitle,
        i -> isSubset (
                characters reducedHeadline#i,
                characters reducedTitle#i)
            or isSubset (
                characters reducedTitle#i,
                characters reducedHeadline#i)
            )
        then return false
    else return true
    )

-- method for converting pairs " " of quotes to latex style `` "
quotesToTex = method (TypicalValue => String)
quotesToTex String := S -> (
    quoteLocations := select (#characters S, i -> (characters S)#i == "\""); -- another " for emacs
    charS := characters S;
    i := 0;
    while i < #quoteLocations do (
        if even i
            then (
                charS = replace (quoteLocations#i, ///``///, charS),
                i = i+1;)
        else charS=charS, i = i+1);
    concatenate charS
    )

wrapTexStrings = method (TypicalValue => String)
wrapTexStrings String := S -> (
    k := # texStore;
    i := 0;
    while i < k do (
        S = replace ((keys texStore)#i,texStore#((keys texStore)#i), S);
        i =i+1;
        );
    S
)

headlineToTex = method (TypicalValue => String)
headlineToTex Package := P -> (
    if not hasGoodHeadline P then return ///A \emph{Macaulay2} package/// else
    rawH := P#Options#Headline; -- package headline unprocessed
    wrapTexStringsH := wrapTexStrings rawH;
    removeEndStopH :=
        if wrapTexStringsH#-1 == "."
            then concatenate apply (#wrapTexStringsH-1, i-> wrapTexStringsH#i )
        else wrapTexStringsH;
    repairQuotesH :=  quotesToTex removeEndStopH;
    repairQuotesH
    )

-- the cite method
iCite = method (TypicalValue => String)
iCite Package := P -> (
    T := P#"pkgname"; -- package title
    V := concatenate("Version~", P#Options#Version); -- package version
    isInternalPackage := member(T, separate (" ", version#"packages"));
    isInternalSource := P#"source directory" === prefixDirectory | currentLayout#"packages";
    certificationInfo := if P#Options#Certification =!= null then hashTable P#Options#Certification else null;
    -- bibtex author content
    if not P#Options#?Authors
        then  print concatenate ("Warning: The \"", T, "\" package provides insufficient citation data: author.")
    else
    packageAuthorsWithContributors := apply(P#Options#Authors, a -> a#0#1);
    packageAuthors := select (packageAuthorsWithContributors, a -> not member (":", characters a));
    bibPackageAuthors := wrapTexStrings (demark (" and ", packageAuthors));
    if packageAuthors === {}
        then  print concatenate ("Warning: The \"", T, "\" package provides insufficient citation data: author.")
    else null;
    -- set up bibtex string for certified packages
    bibtexCert := if certificationInfo === null then null else
        (bibYear := substring ((regex ("[[:digit:]]{4}", certificationInfo#"acceptance date"))#0, certificationInfo#"acceptance date");
        certTitle := quotesToTex (wrapTexStrings (certificationInfo#"article title"));
        concatenate (
            "\n",
            "@article{", T, "Article,\n",
                concatenate ("  title = {{", certTitle, "}},\n"),
                concatenate ("  author = {", bibPackageAuthors, "},\n"), -- authors
                concatenate ("  journal = {", certificationInfo#"journal name", "},\n"),
                concatenate ("  volume = {", certificationInfo#"volume number", "},\n"),
                concatenate ("  year = {",  bibYear, "},\n"),
            "}\n")
        );
    -- set up bibtex string for package source
    -- title
    -- bibtex title content: name of package followed either by a good headline or "A Macaulay2 package"
    bibPackageTitle :=  if hasGoodHeadline P
        then concatenate ("{", T, ": ", headlineToTex P, ". ", V, "}")
        else concatenate ("{", T, ": A \\emph{Macaulay2} package. ", V, "}");
    -- bibtex howpublished content
    bibPackageSource :=
        if isInternalPackage and isInternalSource
            then "\\url{https://github.com/Macaulay2/M2/tree/stable/M2/Macaulay2/packages}"
        else if isInternalPackage and not isInternalSource
            then if P#Options#HomePage =!= null
                then concatenate("\"", toString (P#Options#HomePage), "\"")
            else "\\url{https://github.com/Macaulay2/M2/tree/stable/M2/Macaulay2/packages}"
        else if P#Options#HomePage === null
            then (print concatenate ("Warning: The \"", T, "\" package provides insufficient citation data: howpublished."))
        else concatenate("\\url{" ,toString (P#Options#HomePage), "}");
    bibtexString :=
        concatenate (
            "@misc{", T, "Source,\n",
                concatenate ("  title = {", bibPackageTitle, "},\n"),
                concatenate ("  author = {", bibPackageAuthors, "},\n"),
                concatenate (///  howpublished = {A \emph{Macaulay2} package available at///, newline, 4, bibPackageSource, "}\n"),
            "}\n",
            bibtexCert);
    bibtexString
    )

iCite String := S -> (
    if S === "M2" then return (
        concatenate (
            "@misc{M2,\n",
            "  author = {Grayson, Daniel R. and Stillman, Michael E.},\n",
            "  title = {Macaulay2, a software system for research in algebraic geometry},\n",
            "  howpublished = {Available at ", ///\///, "url{https://macaulay2.com/}}\n",
            "}\n",
            ));
    iCite needsPackage S)

-- The cite command
cite = new Command from (T -> if T === () or T === "M2" then iCite "M2" else citePackage T)

------------------------
-- End of source code --
------------------------

-------------------------
-- Begin documentation --
-------------------------
beginDocumentation()
doc ///
    Key
        PackageCitations
    Headline
        a package facilitating citation of Macaulay2 packages
    Description
        Text
          This is a modest package with lofty goals. It is modest because it
          is a package for a powerful open-source mathematical software suite
          but it contains only one method and adds exactly zero computational
          ability to the platform. The one method, called @TO cite@, can be
          called on any @HREF {"https://macaulay2.com/",
          "Macaulay2"}@ package and will return a bibtex citation for
          inclusion in a @HREF {"https://www.latex-project.org", "LaTeX"}@
          document. For example, a citation for this package can be obtained
          as follows.
        Example
            cite "PackageCitations"
        Text
            The inner workings of @TO cite@ are explained on the @TO2 {cite,
            "documentation page"}@ so we won't give any details here except to
            point out that the preferred citation for Macaulay2 can also be
            obtained with ease.
        Example
            cite
        Text
            The initial benefit of having a fast and facile mechanism for
            citing packages should be that more users of the software will
            include citations in their work. This, of course, will benefit the
            community in a number of ways. First it will recognize the hard
            work of the coders in the Macaulay2 community and second it will
            serve as valuable promotion for the platform and encourage new
            users and coders to join the community.
    SeeAlso
        cite
///

doc ///
    Key
        cite
    Headline
        obtain bibtex citations for Macaulay2 packages
    Usage
        cite
        cite P
        cite S
    Inputs
        P:Package
        S:String
    Outputs
        T:String
            bibtex entry or entries
    Description
        Text
            When called without an argument, @TO cite@ produces the desired
            reference to Macaulay2.
        Example
            cite
        Text
            When applied to a loaded package @TO cite@ returns a bibtex
            citation for inclusion in a LaTeX document, assuming there is
            enough information included in the package to build it. Compare the
            following.
        Example
            cite PackageCitations
            cite Text
        Text
            If @TO cite@ is given a string, then it will load the package
            if necessary and issue the corresponding citation. Note that if
            the package is @TO2 {Certification, "certified"}@ then two bibtex
            entries will be produced: one for the article witnessing the
            certification and one for the source code. Moreover, if the
            headline of a package does not meet a certain set of criteria then
            a more generic title containing  "A Macaulay2 package" is
            produced. For example, the package PieriMaps is a certified
            Macaulay2 package whose headline is deemed too long by the @TO
            cite@ method.
        Example
            cite "PieriMaps"
        Text
            No effort is made to correct apparent typos in the package data. The
            user is urged to check for correct spelling and grammar.
      Example
          cite "Bruns"
    SeeAlso
        PackageCitations
///
-- End exported documentation --
-- Begin Tests --
-- End Tests --
end

restart
uninstallPackage (PackageCitations)
installPackage (PackageCitations)
viewHelp cite
