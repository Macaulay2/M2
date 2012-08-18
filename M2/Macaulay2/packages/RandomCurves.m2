needsPackage"RandomObjects"
needsPackage"RandomSpaceCurves"
needsPackage"RandomPlaneCurves"
needsPackage"RandomGenus14Curves"
needsPackage"RandomCanonicalCurves"

newPackage(
	"RandomCurves",
    	Version => "0.6",
    	Date => "Juli 5, 2011",
    	Authors => {{Name => "Frank-Olaf Schreyer",
		     Email => "schreyer@math.uni-sb.de",
		     HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},

	            {Name => "Hans-Christian Graf v. Bothmer",
	             Email => "bothmer@uni-math.gwdg.de",
		     HomePage => "http://www.crcg.de/wiki/User:Bothmer"},

	     	    {Name=> "Florian Geiss",
	             Email=> "fg@math.uni-sb.de",
	             HomePage=> "http://www.math.uni-sb.de/ag/schreyer/"}


                   },
    	Headline => "Construction of random curves",
    	DebuggingMode => true
        )
end

restart;
uninstallPackage"RandomCurves"
uninstallPackage"RandomObjects"
uninstallPackage"RandomPlaneCurves"
uninstallPackage"RandomSpaceCurves"
uninstallPackage"RandomGenus14Curves"
uninstallPackage"RandomCanonicalCurves"
--installing all packages takes about 90 seconds:
installPackage("RandomObjects",RerunExamples=>true,RemakeAllDocumentation=>true);
installPackage("RandomPlaneCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
installPackage("RandomSpaceCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
installPackage("RandomGenus14Curves",RerunExamples=>true,RemakeAllDocumentation=>true);
installPackage("RandomCanonicalCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
installPackage("RandomCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
restart
loadPackage"RandomCurves"
viewHelp
g=7;
kk=ZZ/101;
R=kk[x_0..x_(g-1)];
C=(random canonicalCurve)(g,R);
betti res C
(genus C, degree C)

