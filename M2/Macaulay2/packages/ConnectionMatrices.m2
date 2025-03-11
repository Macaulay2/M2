newPackage(
    "ConnectionMatrices",
    Version => "0.1",
    Date => "March 2025",
    Authors => {
	{ Name => "Paul Goerlach",           Email => "paul.goerlach@ovgu.de",              HomePage => "" },
	{ Name => "Joris Koefler",           Email => "joris.koefler@mis.mpg.de",           HomePage => "" },
	{ Name => "Mahrud Sayrafi",          Email => "mahrud@fields.utoronto.ca",          HomePage => "" },
	{ Name => "Anna-Laura Sattelberger", Email => "anna-laura.sattelberger@mis.mpg.de", HomePage => "" },
	{ Name => "Carlos Rodriguez",        Email => "carlos.rodriguez@mis.mpg.de",        HomePage => "" },
	{ Name => "Hendrik Schroeder",       Email => "h.schroeder@tu-berlin.de",           HomePage => "" },
	{ Name => "Nicolas Weiss",           Email => "nicolas.weiss@mis.mpg.de",           HomePage => "" },
	{ Name => "Franzesca Zaffalon",      Email => "francesca.zaffalon@mis.mpg.de",      HomePage => "" }
    },
    Headline => "Pfaffian systems and integrable systems",
    Keywords => { "D-modules" },
    PackageExports => { "Dmodules" },
    AuxiliaryFiles => true,
    DebuggingMode => true,
)

export {
    -- "pfaffians" already exported by Core
    "normalForm",
    "gaugeMatrix",
    "gaugeTransform",
    "diffConnectionMatrix",
    "stdMon"
}

-- to access private methods from Core
importFrom_Core { "concatRows", "concatCols" }

-- to access private methods from Dmodules
debug Dmodules

--------------------------------------------------------------------
-- contains changes to Dmodules method holonomicRank
--------------------------------------------------------------------

load "./ConnectionMatrices/holonomic.m2"

--------------------------------------------------------------------
-- Normal forms over the rational Weyl algebra
--------------------------------------------------------------------

load "./ConnectionMatrices/reduce.m2"

--------------------------------------------------------------------
-- Pfaffian systems
--------------------------------------------------------------------

load "./ConnectionMatrices/pfaffians.m2"

--------------------------------------------------------------------
-- Gauge matrix
--------------------------------------------------------------------

load "./ConnectionMatrices/gaugeMatrix.m2"

--------------------------------------------------------------------
-- change of basis
--------------------------------------------------------------------

load "./ConnectionMatrices/gaugeTransform.m2"

--------------------------------------------------------------------
-- Tools to check the integrability of a Pfaffian system
--------------------------------------------------------------------

load "./ConnectionMatrices/integrabilityCheck.m2"

--------------------------------------------------------------------
-- Tests section
--------------------------------------------------------------------

load "./ConnectionMatrices/tests.m2"

--------------------------------------------------------------------
-- Documentation section
--------------------------------------------------------------------

beginDocumentation()
load "./ConnectionMatrices/docs.m2"

end--

--------------------------------------------------------------------
-- Development section
--------------------------------------------------------------------

restart
debug needsPackage "ConnectionMatrices"
check "ConnectionMatrices"

uninstallPackage "ConnectionMatrices"
restart
installPackage "ConnectionMatrices"
viewHelp "ConnectionMatrices"
