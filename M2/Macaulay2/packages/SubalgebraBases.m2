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
        }, {
        Name => "Legacy author: Mike Stillman",
        Email => "mike@math.cornell.edu",
        HomePage => "http://www.math.cornell.edu/~mike/"
        }, {
        Name => "Legacy author: Harrison Tsai"
        }},
    Headline => "Canonical subalgebra bases (aka SAGBI/Khovanskii bases)",
    AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    DebuggingMode => false, -- set to true only during development
    Keywords => {"Commutative Algebra"}
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
