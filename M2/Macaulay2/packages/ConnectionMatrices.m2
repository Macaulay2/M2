newPackage(
    "ConnectionMatrices",
    Version => "1.0",
    Date => "March 2025",
    Authors => {
	{ Name => "Paul Goerlach",           Email => "paul.goerlach@ovgu.de",              HomePage => "" },
	{ Name => "Joris Koefler",           Email => "joris.koefler@mis.mpg.de",           HomePage => "" },
	{ Name => "Mahrud Sayrafi",          Email => "mahrud@fields.utoronto.ca",          HomePage => "https://math.umn.edu/~mahrud/" },
	{ Name => "Anna-Laura Sattelberger", Email => "anna-laura.sattelberger@mis.mpg.de", HomePage => "" },
	{ Name => "Hendrik Schroeder",       Email => "h.schroeder@tu-berlin.de",           HomePage => "" },
	{ Name => "Nicolas Weiss",           Email => "nicolas.weiss@mis.mpg.de",           HomePage => "" },
	{ Name => "Francesca Zaffalon",      Email => "francesca.zaffalon@mis.mpg.de",      HomePage => "" }
    },
    Headline => "connection matrices and integrable systems from D-ideals",
    Keywords => { "D-modules" },
    PackageExports => { "Dmodules" },
    AuxiliaryFiles => true,
    DebuggingMode => false,
)

export {
    -- see ConnectionMatrices/reduce.m2
    "normalForm",
    "baseFractionField",
    -- see ConnectionMatrices/pfaffians.m2
    "standardMonomials",
    "connectionMatrices",
    "connectionMatrix",
    -- see ConnectionMatrices/integrabilityCheck.m2
    "isIntegrable",
    -- see ConnectionMatrices/gaugeMatrix.m2
    "gaugeMatrix",
    -- see ConnectionMatrices/gaugeTransform.m2
    "gaugeTransform",
    "isEpsilonFactorized",
}

-- to access private methods from Dmodules
--debug Dmodules

--------------------------------------------------------------------
-- contains changes to Dmodules method holonomicRank
--------------------------------------------------------------------

load "./ConnectionMatrices/holonomic.m2"

--------------------------------------------------------------------
-- Normal forms over the rational Weyl algebra
--------------------------------------------------------------------

load "./ConnectionMatrices/reduce.m2"

--------------------------------------------------------------------
-- Connection Matrices
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
-- Tools to check the integrability of a system of connection matrices
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
