-- -*- coding: utf-8 -*-
newPackage(
    "SubalgebraBases",
    AuxiliaryFiles => true,
    Version => "1.4",
    Date => "January 2024",
    Authors => {{
        Name => "Michael Burr",
        Email => "burr2@clemson.edu",
        HomePage => "https://cecas.clemson.edu/~burr2/"
        }, {
        Name => "Oliver Clarke",
        Email => "oliver.clarke.crgs@gmail.com",
        HomePage => "https://www.oliverclarkemath.com/"
        }, {
        Name => "Timothy Duff",
        Email => "timduff@uw.edu",
        HomePage => "https://timduff35.github.io/timduff35/"
        }, {
        Name => "Jackson Leaman",
        Email => "jacksonleaman@gmail.com"
        }, {
        Name => "Nathan Nichols",
        Email => "nathannichols454@gmail.com"
        }, {
        Name => "Elise Walker",
        Email => "elise.walker@tamu.edu",
        HomePage => "https://sites.google.com/view/elise-walker/home"
        }},
    Headline => "Canonical subalgebra bases (aka SAGBI/Khovanskii bases)",
    AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    DebuggingMode => false, -- set to true only during development
    Keywords => {"Commutative Algebra"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "SubalgebraBases in Macaulay2",
	"acceptance date" => "2024-03-18",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p11.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.97",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x11-SAGBIBases.zip",
	"release at publication" => "97d711d9b7232b6fcf64c0395bd9cb07b8b968c0",
	"version at publication" => "1.3",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
)

needs "./SubalgebraBases/exports.m2"
needs "./SubalgebraBases/classes.m2"
needs "./SubalgebraBases/sagbi-functions.m2"
needs "./SubalgebraBases/sagbiComp-functions.m2"
needs "./SubalgebraBases/tests.m2"
needs "./SubalgebraBases/sagbi-main.m2"

beginDocumentation()
needs "./SubalgebraBases/documentation.m2"

end--

uninstallAllPackages()
restart
path = prepend("./", path)
installPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2",InstallPrefix=>"./",RerunExamples=>true)
check "SubalgebraBases"
viewHelp SubalgebraBases
loadPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2")
check(1, "SubalgebraBases")

path = prepend("./", path)
installPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2",InstallPrefix=>"./")

check SubalgebraBases
