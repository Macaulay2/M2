-- Probability package for Macaulay2
-- Copyright (C) 2022-2024 Doug Torrance

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

newPackage("Probability",
    Headline => "basic probability functions",
    Version => "0.5",
    Date => "September 13, 2024",
    Authors => {{
	    Name     => "Doug Torrance",
	    Email    => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Algebraic Statistics"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "The Probability package for Macaulay2",
	"acceptance date" => "2024-01-23",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p07.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.51",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x07-Probability.m2",
	"release at publication" => "fe3f536ed3c90d114452e31a24fc3a935a3b9ca3",
	"version at publication" => "0.3",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
    )

---------------
-- ChangeLog --
---------------

-*

0.5 (2024-09-13, M2 1.24.11)
* add JSAG info
* remove Constant methods now that we can use inheritance
* adjust tests now that we use flint instead of boost for some special functions
* use mpfr's built-in function for generating normally distributed variates
* add subnodes to improve docs

0.4 (2024-01-23, M2 1.23)
* release under GPL
* add keyword

0.3 (2023-10-31, version submitted to JSAG)
* add Caveats to docs warning user to ensure that pdf's are well-defined
* use ASCII characters for chi-squared distribution
* clarify in docs that the support of a discrete distribution will be a subset
  of the integers
* add Caveat to docs mentioning limitations of floating-point arithmetic

0.2 (2022-10-31, M2 1.21)
* fix typos
* adjust some tests after MPFR support added for Boost special functions

0.1 (2022-05-04, M2 1.20)
* initial release

*-

export {
-- classes
    "ProbabilityDistribution",
    "DiscreteProbabilityDistribution",
    "ContinuousProbabilityDistribution",

-- generic constructor methods
    "discreteProbabilityDistribution",
    "continuousProbabilityDistribution",

-- discrete distributions
    "binomialDistribution",
    "bernoulliDistribution",
    "poissonDistribution",
    "geometricDistribution",
    "negativeBinomialDistribution",
    "hypergeometricDistribution",

-- continuous distributions
    "uniformDistribution",
    "exponentialDistribution",
    "normalDistribution",
    "gammaDistribution",
    "chiSquaredDistribution",
    "tDistribution",
    "fDistribution",
    "betaDistribution",

-- functions
    "density",
    "probability",
    "quantile",

-- symbols
    "DensityFunction",
    "DistributionFunction",
    "QuantileFunction",
    "RandomGeneration",
    "Support",
    "LowerTail"
    }

---------------------------------------------
-- abstract probability distribution class --
---------------------------------------------

ProbabilityDistribution = new Type of HashTable
ProbabilityDistribution.synonym = "probability distribution"

density' := (X, x) -> (
    if x < first X.Support or x > last X.Support then 0
    else X.DensityFunction x)

density = method()
density(ProbabilityDistribution, Number) := density'

probability' := true >> o -> (X, x) -> (
    p := if x < first X.Support then 0
    else if x > last X.Support then 1
    else X.DistributionFunction x;
    if o.LowerTail then p else 1 - p)

probability = method(Options => {LowerTail => true})
probability(ProbabilityDistribution, Number) := o -> (X, x) ->
     probability'(X, x, o)

quantile' = true >> o -> (X, p) -> (
    if p < 0 or p > 1 then error "expected number between 0 and 1"
    else if p == 0 and first X.Support == -infinity then -infinity
    else if p == 1 and last X.Support == infinity then infinity
    else X.QuantileFunction if o.LowerTail then p else 1 - p)

quantile = method(Options => {LowerTail => true})
quantile(ProbabilityDistribution, Number) := o -> (X, p) -> quantile'(X, p, o)

random ProbabilityDistribution := o -> X -> X.RandomGeneration()
net ProbabilityDistribution := X -> X.Description
texMath ProbabilityDistribution := texMath @@ net

-- helper functions for checking parameters
checkReal := n -> if not isReal n then error(
    "expected real parameter: ", n)
checkPositive := n -> if n <= 0 or not isReal n then error(
    "expected positive parameter: ", n)
checkNonnegative := n -> if n < 0 or not isReal n then error(
    "expected nonnegative parameter: ", n)
checkProbability := p -> if p < 0 or p > 1 or not isReal p then error(
    "expected parameter to be between 0 and 1: ", p)
checkSupport := A -> if not (instance(A, Sequence) and length A == 2 and
    (isReal first A or isInfinite first A) and
    (isReal last A or isInfinite last A) and first A < last A) then error(
    "expected an increasing pair of real or infinite numbers: ", A)

----------------------------------------
-- discrete probability distributions --
----------------------------------------

DiscreteProbabilityDistribution = new SelfInitializingType of
	ProbabilityDistribution
DiscreteProbabilityDistribution.synonym = "discrete probability distribution"

discreteProbabilityDistribution = method(Options => {
	DistributionFunction => null,
	QuantileFunction     => null,
	RandomGeneration     => null,
	Support              => (0, infinity),
	Description          => "a discrete probability distribution"})

discreteProbabilityDistribution Function := o -> f -> (
    checkSupport o.Support;
    a := first o.Support;
    cdf := if o.DistributionFunction =!= null
	then o.DistributionFunction
	else x -> sum(a..floor x, f);
    quant := if o.QuantileFunction =!= null
	then o.QuantileFunction
	else p -> (
	    x := a;
	    q := f x;
	    while q < p do (
		x = x + 1;
		q = q + f x);
	    x);
    rand := if o.RandomGeneration =!= null
	then o.RandomGeneration
	else () -> quant random 1.;
    DiscreteProbabilityDistribution hashTable {
	DensityFunction      => f,
	DistributionFunction => cdf,
	QuantileFunction     => quant,
	RandomGeneration     => rand,
	Support              => o.Support,
	Description          => o.Description})

density(DiscreteProbabilityDistribution, Number) := (X, x) ->
    if x != floor x then 0 else density'(X, x)

probability(DiscreteProbabilityDistribution, Number) := o -> (X, x) ->
    probability'(X, floor x, o)

quantile(DiscreteProbabilityDistribution, Number) := o -> (X, p) -> (
    maybefloor := x -> if isInfinite x then x else floor x;
    maybefloor quantile'(X, p, o))

binomialDistribution = method()
binomialDistribution(ZZ, Number) := (n, p) -> (
    checkPositive n;
    checkProbability p;
    discreteProbabilityDistribution(
	x -> binomial(n, x) * p^x * (1 - p)^(n - x),
	DistributionFunction => x -> regularizedBeta(1 - p, n - x, x + 1),
	Support => (0, n),
	Description => "B" | toString (n, p)))

bernoulliDistribution = method()
bernoulliDistribution Number := p -> binomialDistribution(1, p)

poissonDistribution = method()
poissonDistribution Number := lambda -> (
    checkPositive lambda;
    discreteProbabilityDistribution(x -> lambda^x / x! * exp(-lambda),
	DistributionFunction => x -> regularizedGamma(floor(x + 1), lambda),
	Description => "Pois(" | toString lambda | ")"))

geometricDistribution = method()
geometricDistribution Number := p -> (
    checkProbability p;
    discreteProbabilityDistribution(x -> p * (1 - p)^x,
	DistributionFunction => x -> 1 - (1 - p)^(x + 1),
	QuantileFunction => q -> ceiling(log((1 - q)/(1 - p)) / log(1 - p)),
	Description => "Geo(" | toString p | ")"))

negativeBinomialDistribution = method()
negativeBinomialDistribution(Number, Number) := (r, p) -> (
    checkPositive r;
    checkProbability p;
    discreteProbabilityDistribution(
	x -> Gamma(x + r) / (Gamma r * x!) * p^r * (1 - p)^x,
	DistributionFunction => x -> regularizedBeta(p, r, x + 1),
	Description => "NB" | toString(r, p)))

hypergeometricDistribution = method()
hypergeometricDistribution(ZZ, ZZ, ZZ) := (m, n, k) -> (
    checkNonnegative m;
    checkNonnegative n;
    checkNonnegative k;
    if k > m + n then error(
	"expected parameter to be at most ", m + n, ": ", k);
    discreteProbabilityDistribution(
	x -> binomial(m, x) * binomial(n, k - x) / binomial(m + n, k),
	Support => (0, m),
	Description => "HG" | toString(m, n, k)))

------------------------------------------
-- continuous probability distributions --
------------------------------------------

ContinuousProbabilityDistribution = new SelfInitializingType of
	ProbabilityDistribution
ContinuousProbabilityDistribution.synonym =
    "continuous probability distribution"

continuousProbabilityDistribution = method(Options => {
	DistributionFunction => null,
	QuantileFunction     => null,
	RandomGeneration     => null,
	Support              => (0, infinity),
	Description          => "a continuous probability distribution"})

bisectionMethod = (f, a, b, epsilon) -> (
    while b - a > epsilon do (
	mid := 0.5 * (a + b);
	if f(mid) == 0 then break
	else if f(a) * f(mid) > 0 then a = mid
	else b = mid);
    0.5 * (a + b))

continuousProbabilityDistribution Function := o -> f -> (
    checkSupport o.Support;
    (a, b) := o.Support;
    cdf := if o.DistributionFunction =!= null
    	then o.DistributionFunction
	else x -> integrate(f, a, x);
    quant := if o.QuantileFunction =!= null
    	then o.QuantileFunction
	else p -> (
	    c := if a > -infinity then a else 0;
	    while cdf c > p do c = c - 1;
	    d := if b < infinity then b else 0;
	    while cdf d < p do d = d + 1;
	    bisectionMethod(x -> cdf x - p, c, d, 1e-14));
    rand := if o.RandomGeneration =!= null
	then o.RandomGeneration
	else () -> quant random 1.;
    ContinuousProbabilityDistribution hashTable {
	DensityFunction      => f,
	DistributionFunction => cdf,
	QuantileFunction     => quant,
	RandomGeneration     => rand,
	Support              => o.Support,
	Description          => o.Description})

uniformDistribution = method()
uniformDistribution(Number, Number) := (a, b) -> (
    checkReal a;
    checkReal b;
    if a >= b then error("expected parameters to be in increasing order: ",
	a, ", ", b);
    continuousProbabilityDistribution(
	x -> 1/(b - a),
	DistributionFunction => x -> (x - a) / (b - a),
	QuantileFunction => p -> a + p * (b - a),
	Support => (a, b),
	Description => "U" | toString (a, b)))
installMethod(uniformDistribution, () -> uniformDistribution(0, 1))

exponentialDistribution = method()
exponentialDistribution Number := lambda -> (
    checkPositive lambda;
    continuousProbabilityDistribution(
	x -> lambda * exp(-lambda * x),
	DistributionFunction => x -> 1 - exp(-lambda * x),
	QuantileFunction => p -> -log(1 - p) / lambda,
	Description => "Exp(" | toString lambda | ")"))

importFrom(Core, "rawRandomRRNormal")
normalDistribution = method()
normalDistribution(Number, Number) := (mu, sigma) -> (
    checkReal mu;
    checkPositive sigma;
    continuousProbabilityDistribution(
	x -> 1 / (sigma * sqrt(2 * pi)) * exp(-1/2 * ((x - mu) / sigma)^2),
	DistributionFunction => x ->
	    1/2 * (1 + erf((x - mu) / (sigma * sqrt 2))),
	QuantileFunction => p ->
	    mu + sigma * sqrt 2 * inverseErf(2 * p - 1),
	RandomGeneration => (
	    p := min(precision numeric mu, precision numeric sigma);
	    () -> mu + sigma * rawRandomRRNormal p),
	Support => (-infinity, infinity),
	Description => "N" | toString (mu, sigma)))

-- standard normal distribution
installMethod(normalDistribution, () -> normalDistribution(0, 1))

gammaDistribution = method()
gammaDistribution(Number, Number) := (alpha, lambda) -> (
    checkPositive alpha;
    checkPositive lambda;
    continuousProbabilityDistribution(
	x -> lambda^alpha / Gamma(alpha) * x^(alpha - 1) * exp(-lambda * x),
	DistributionFunction => x -> 1 - regularizedGamma(alpha, lambda * x),
	QuantileFunction => p -> inverseRegularizedGamma(alpha, 1 - p) / lambda,
	Description => "Gamma" | toString (alpha, lambda)))

chiSquaredDistribution = method()
chiSquaredDistribution Number := n -> (
    checkPositive n;
    continuousProbabilityDistribution(
	x -> 1/(2^(n/2) * Gamma(n/2)) * x^(n/2 - 1) * exp(-x / 2),
	DistributionFunction => x -> 1 - regularizedGamma(n / 2, x / 2),
	QuantileFunction => p -> 2 * inverseRegularizedGamma(n / 2, 1 - p),
	Description => "chi2(" | toString n | ")"))

tDistribution = method()
tDistribution Number := df -> (
    checkPositive df;
    continuousProbabilityDistribution(
	x -> Gamma((df + 1)/2) / (sqrt(df * pi) * Gamma(df / 2)) *
	    (1 + x^2/df)^(-(df + 1) / 2),
	DistributionFunction => x -> (
	    p := 1 - 1/2*regularizedBeta(df/(x^2 + df), df/2, 1/2);
	    if x >= 0 then p else 1 - p),
	QuantileFunction => p -> (if p >= 0.5
	    then sqrt(df / inverseRegularizedBeta(2 - 2 * p, df/2, 1/2) - df)
	    else -sqrt(df / inverseRegularizedBeta(2 * p, df/2, 1/2) - df)),
	Support => (-infinity, infinity),
	Description => "t(" | toString df | ")"))

fDistribution = method()
fDistribution(Number, Number) := (d1, d2) -> (
    checkPositive d1;
    checkPositive d2;
    continuousProbabilityDistribution(
	x -> sqrt(
	    (d1 * x)^d1 * d2^d2 / (d1*x + d2)^(d1 + d2)) /
	    (x * Beta(d1 / 2, d2 / 2)),
	DistributionFunction => x ->
	    regularizedBeta(d1 * x / (d1 * x + d2), d1 / 2, d2 / 2),
	QuantileFunction => p ->
	    d2 / d1 * (1 / (1 - inverseRegularizedBeta(p, d1 / 2, d2 / 2)) - 1),
	Description => "F" | toString (d1, d2)))

betaDistribution = method()
betaDistribution(Number, Number) := (alpha, beta) -> (
    checkPositive alpha;
    checkPositive beta;
    continuousProbabilityDistribution(
	x -> x^(alpha - 1) * (1 - x)^(beta - 1) / Beta(alpha, beta),
	DistributionFunction => x -> regularizedBeta(x, alpha, beta),
	QuantileFunction => p -> inverseRegularizedBeta(p, alpha, beta),
	Support => (0, 1),
	Description => "Beta" | toString(alpha, beta)))

beginDocumentation()

doc ///
  Key
    Probability
  Headline
    basic probability functions
  Description
    Text
      This package provides a number of basic probability functions.  In
      particular, for both discrete and continuous probability distributions,
      there are the following four functions:

      @UL {
	  LI {TO density, ":  probability density (or mass) function"},
	  LI {TO probability, ":  cumulative distribution function"},
	  LI {TO quantile, ":  quantile function"},
	  LI {TO (random, ProbabilityDistribution),
	      ":  generate random samples"}}@

      A variety of common probability distributions are supported.

      @HEADER3 "Discrete distributions"@

      @UL {
	  LI {TO2 {binomialDistribution, "binomial"}},
	  LI {TO2 {poissonDistribution, "Poisson"}},
	  LI {TO2 {geometricDistribution, "geometric"}},
	  LI {TO2 {negativeBinomialDistribution, "negative binomial"}},
	  LI {TO2 {hypergeometricDistribution, "hypergeometric"}}}@

      @HEADER3 "Continuous distributions"@

      @UL {
	  LI {TO2 {uniformDistribution, "uniform"}},
	  LI {TO2 {exponentialDistribution, "exponential"}},
	  LI {TO2 {normalDistribution, "normal"}},
	  LI {TO2 {gammaDistribution, "gamma"}},
	  LI {TO2 {chiSquaredDistribution, "chi-squared"}},
	  LI {TO2 {tDistribution, "Student's t"}},
	  LI {TO2 {fDistribution, "F"}},
	  LI {TO2 {betaDistribution, "beta"}}}@

      You may also define your own probability distributions using
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@.
  Caveat
    As is always the case when working with real numbers in Macaulay2,
    unexpected results may occur due to the limitations of floating
    point arithmetic.
  Subnodes
    ProbabilityDistribution
///

doc ///
  Key
    ProbabilityDistribution
    DiscreteProbabilityDistribution
    ContinuousProbabilityDistribution
    (net, ProbabilityDistribution)
  Headline
    probability distribution class
  Description
    Text
      This is the class of which all probability distribution objects
      belong.  @TT "ProbabilityDistribution"@ is an abstract class
      defining the interface and should not be used directly.
      Instead, its subclasses @TT "DiscreteProbabilityDistribution"@ and
      @TT "ContinuousProbabilityDistribution"@ should be used.

      @TT "ProbabilityDistribution"@ objects are hash tables containing six
      key-value pairs:

      @DL{
	  DT {TT "DensityFunction"},
	  DD {"The probability mass or density function (for discrete or ",
	      "continuous distributions, respectively).  Do not use this ",
	      "directly.  Instead, use ", TO density, "."},
	  DT {TT "DistributionFunction"},
	  DD {"The cumulative distribution function.  Do not use this ",
	      "directly.  Instead, use ", TO probability, "."},
	  DT {TT "QuantileFunction"},
	  DD {"The quantile function.  Do not use this directly.  Instead, ",
	      "use ", TO quantile, "."},
	  DT {TT "RandomGeneration"},
	  DD {"A function to generate random samples of the distribution.  ",
	      "Do not use this directly.  Instead, use ",
	      TO (random, ProbabilityDistribution), "."},
	  DT {TT "Support"},
	  DD {"A sequence of two numbers, the lower and upper bound of the ",
	      "support of the distribution."},
	  DT {TT "Description"},
	  DD {"A string containing a description of the distribution.  This ",
	      "is the return value when a ", TT "ProbabilityDistribution",
	      " object is passed to ", TO net, "."}
	  }@
    Example
      Z = normalDistribution()
      ancestors class Z
      peek Z
    Text
      To create a @TT "ProbablityDistribution"@ object, use one of the
      constructor methods, @TO discreteProbabilityDistribution@,
      @TO continuousProbabilityDistribution@, or any of the various built-in
      methods for common distributions.
  Subnodes
    :Keys
    DensityFunction
    DistributionFunction
    QuantileFunction
    RandomGeneration
    Support
    :Constructor methods
    discreteProbabilityDistribution
    continuousProbabilityDistribution
///

doc ///
  Key
    DensityFunction
  Headline
    probability density function
  Description
    Text
      A key in @TO ProbabilityDistribution@ objects and an option for
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@ for setting the probability
      density/mass function to be used by @TO density@.
  Subnodes
    density
///

doc ///
  Key
    DistributionFunction
  Headline
    cumulative distribution function
  Description
    Text
      A key in @TO ProbabilityDistribution@ objects and an option for
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@ for setting the cumulative
      distribution function to be used by @TO probability@.
  Subnodes
    probability
///

doc ///
  Key
    QuantileFunction
  Headline
    quantile function
  Description
    Text
      A key in @TO ProbabilityDistribution@ objects and an option for
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@ for setting the quantile function
      to be used by @TO quantile@.
  Subnodes
    quantile
///

doc ///
  Key
    RandomGeneration
  Headline
    random generation function
  Description
    Text
      A key in @TO ProbabilityDistribution@ objects and an option for
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@ for setting the random generation
      function to be used by @TO (random, ProbabilityDistribution)@.
  Subnodes
    (random, ProbabilityDistribution)
///

doc ///
  Key
    Support
  Headline
    support for probability distribution
  Description
    Text
      A key in @TO ProbabilityDistribution@ objects and an option for
      @TO discreteProbabilityDistribution@ and
      @TO continuousProbabilityDistribution@ for setting the support of
      the probability distribution.
///

doc ///
  Key
    density
    (density, ProbabilityDistribution, Number)
    (density, DiscreteProbabilityDistribution, Number)
  Headline
    probability density (or mass) function
  Usage
    density_X x
  Inputs
    X:ProbabilityDistribution
    x:RR
  Outputs
    :RR
  Description
    Text
      For a discrete probability distribution, this returns values of
      of the @wikipedia "probability mass function"@ of the distribution, i.e.,
      \(f_X(x) = P(X = x)\).
    Example
      X = binomialDistribution(5, 0.25)
      density_X 2
      binomial(5, 2) * 0.25^2 * 0.75^3
    Text
      For a continuous probability distribution, this returns values of
      the @wikipedia "probability density function"@ of the distribution, i.e.,
      the integrand in \(\int_a^b f_X(x)\,dx = P(a\leq X \leq b)\).
    Example
      Z = normalDistribution()
      density_Z 0
      1/sqrt(2 * pi)
      integrate(density_Z, -1, 1)
      integrate(density_Z, -2, 2)
      integrate(density_Z, -3, 3)
///

doc ///
  Key
    LowerTail
  Headline
    whether to computer lower tail probabilities
  Description
    Text
      This is an option for @TO probability@ and @TO quantile@.
///

doc ///
  Key
    probability
    (probability, ProbabilityDistribution, Number)
    (probability, DiscreteProbabilityDistribution, Number)
    [probability, LowerTail]
  Headline
    cumulative distribution function
  Usage
    probability_X x
  Inputs
    X:ProbabilityDistribution
    x:RR
    LowerTail => Boolean
  Outputs
    :RR
  Description
    Text
      The @wikipedia "cumulative distribution function"@ of the probability
      distribution, i.e., the lower tail probability \(F_X(x) = P(X \leq x)\).
    Example
      Z = normalDistribution()
      probability_Z 1.96
    Text
      If the @TT "LowerTail"@ option is @TT "false"@, then it instead computes
      the value of the @wikipedia "survival function"@, i.e., the upper tail
      probability \(S_X(x) = P(X > x)\).
    Example
      probability_Z(1.96, LowerTail => false)
  Subnodes
    LowerTail
///

doc ///
  Key
    quantile
    (quantile, ProbabilityDistribution, Number)
    (quantile, DiscreteProbabilityDistribution, Number)
    [quantile, LowerTail]
  Headline
    quantile function
  Usage
    quantile_X p
  Inputs
    X:ProbabilityDistribution
    p:RR
    LowerTail => Boolean
  Outputs
    :RR
  Description
    Text
      For continuous probability distributions, the @wikipedia
      "quantile function"@ is the inverse of the cumulative
      distribution function, i.e., \(x\) for which \(P(X \leq x) = p\).
    Example
      Z = normalDistribution()
      quantile_Z 0.95
      probability_Z oo
    Text
      For discrete probability distributions, it returns the smallest \(x\)
      for which \(P(X \leq x) \geq p\).
    Example
      X = binomialDistribution(10, 0.25)
      quantile_X 0.75
      probability_X 2
      probability_X 3
    Text
      If the @TT "LowerTail"@ option is @TT "false"@, then it instead finds
      \(x\) for which \(P(X > x) = p\) in the continuous case.
    Example
      quantile_Z(0.95, LowerTail => false)
      probability_Z(oo, LowerTail => false)
    Text
      In the discrete case, it finds the smallest \(x\) for which
      \(P(X > x) \leq p\).
    Example
      quantile_X(0.75, LowerTail => false)
      probability_X(2, LowerTail => false)
      probability_X(1, LowerTail => false)
///

doc ///
  Key
    (random, ProbabilityDistribution)
  Headline
    randomly generate samples from probability distribution
  Usage
    random X
  Inputs
    X:ProbabilityDistribution
  Outputs
    :RR
  Description
    Text
      Randomly generate samples from the given probability distribution.
    Example
      Z = normalDistribution()
      for i to 10 list random Z
///

doc ///
  Key
    discreteProbabilityDistribution
    (discreteProbabilityDistribution, Function)
    [discreteProbabilityDistribution, DistributionFunction]
    [discreteProbabilityDistribution, QuantileFunction]
    [discreteProbabilityDistribution, RandomGeneration]
    [discreteProbabilityDistribution, Support]
    [discreteProbabilityDistribution, Description]
  Headline
    construct a discrete probability distribution
  Usage
    discreteProbabilityDistribution f
  Inputs
    f:Function
      the probability mass function of @TT "X"@, to be used by
      @TO density@.
    DistributionFunction => Function
      the cumulative distribution function of @TT "X"@, to be used by
      @TO probability@.  If @TT "null"@, then obtained by adding values of
      @TT "f"@.
    QuantileFunction => Function
      the quantile function of @TT "X"@, to be used by @TO quantile@.
      If @TT "null"@, then obtained by adding values of @TT "f"@.
    RandomGeneration => Function
      a function for generating random samples from @TT "X"@, to be used
      by @TO (random, ProbabilityDistribution)@.  If @TT "null"@, then obtained
      using @wikipedia "inverse transform sampling"@.
    Support => Sequence
      containing the lower and upper bounds, respectively, of the
      @wikipedia("Support (mathematics)", "support")@ of @TT "X"@.
      Elements of the support are assumed to be integers.
    Description => String
      describing the probability distribution.
  Outputs
    X:DiscreteProbabilityDistribution
  Description
    Text
      To construct a discrete probability distribution, provide the probability
      mass function and, if different than the default of \([0, \infty\]), the
      support.
    Example
      X = discreteProbabilityDistribution(x -> 1/6, Support => (1, 6))
      density_X 3
    Text
      Non-integers and values outside the support are automatically sent to 0.
    Example
      density_X 3.5
      density_X 7
    Text
      The cumulative distribution, quantile, and random generation functions
      are set to defaults based on the probability mass function.
    Example
      probability_X 3
      quantile_X 0.2
      random X
    Text
      However, if possible, it is good to provide these directly to
      improve performance.  A description may also be provided.
    Example
      X = discreteProbabilityDistribution(x -> 1/6, Support => (1, 6),
	  DistributionFunction => x -> x / 6,
	  QuantileFunction => p -> 6 * p,
	  Description => "six-sided die")
  Caveat
    When defining a probability mass function, the user must be careful that
    it satisfies the definition, i.e., it must be nonnegative and its values
    must sum to 1 on its support.
  Subnodes
    binomialDistribution
    poissonDistribution
    geometricDistribution
    negativeBinomialDistribution
    hypergeometricDistribution
///

doc ///
  Key
    continuousProbabilityDistribution
    (continuousProbabilityDistribution, Function)
    [continuousProbabilityDistribution, DistributionFunction]
    [continuousProbabilityDistribution, QuantileFunction]
    [continuousProbabilityDistribution, RandomGeneration]
    [continuousProbabilityDistribution, Support]
    [continuousProbabilityDistribution, Description]
  Headline
    construct a continuous probability distribution
  Usage
    continuousProbabilityDistribution f
  Inputs
    f:Function
      the probability density function of @TT "X"@, to be used by
      @TO density@.
    DistributionFunction => Function
      the cumulative distribution function of @TT "X"@, to be used by
      @TO probability@.  If @TT "null"@, then obtained by numerically
      integrating @TT "f"@.
    QuantileFunction => Function
      the quantile function of @TT "X"@, to be used by @TO quantile@.
      If @TT "null"@, then obtained by using the @wikipedia "bisection method"@.
    RandomGeneration => Function
      a function for generating random samples from @TT "X"@, to be used
      by @TO (random, ProbabilityDistribution)@.  If @TT "null"@, then obtained
      using @wikipedia "inverse transform sampling"@.
    Support => Sequence
      containing the lower and upper bounds, respectively, of the
      @wikipedia("Support (mathematics)", "support")@ of @TT "X"@.
    Description => String
      describing the probability distribution.
  Outputs
    X:ContinuousProbabilityDistribution
  Description
    Text
      To construct a continuous probability distribution, provide the
      probability density function and, if different than the default
      of \([0, \infty\]), the support.
    Example
      X = continuousProbabilityDistribution(x -> 2 * x, Support => (0, 1))
      density_X 0.75
    Text
      Values outside the support are automatically sent to 0.
    Example
      density_X 2
    Text
      The cumulative distribution, quantile, and random generation functions
      are set to defaults based on the probability density function.
    Example
      probability_X 0.75
      quantile_X 0.5625
      random X
    Text
      However, if possible, it is good to provide these directly to
      improve performance.  A description may also be provided.
    Example
      X = continuousProbabilityDistribution(x -> 2 * x, Support => (0, 1),
	  DistributionFunction => x -> x^2,
	  QuantileFunction => p -> sqrt p,
	  Description => "triangular distribution")
  Caveat
    When defining a probability density function, the user must be careful that
    it satisfies the definition, i.e., it must be nonnegative and it must
    integrate to 1 on its support.
  Subnodes
    uniformDistribution
    exponentialDistribution
    normalDistribution
    gammaDistribution
    chiSquaredDistribution
    tDistribution
    fDistribution
    betaDistribution
///

doc ///
  Key
    binomialDistribution
    (binomialDistribution, ZZ, Number)
    bernoulliDistribution
    (bernoulliDistribution, Number)
  Headline
    binomial distribution
  Usage
    binomialDistribution(n, p)
    bernoulliDistribution p
  Inputs
    n:ZZ
    p:Number -- between 0 and 1
  Outputs
    :DiscreteProbabilityDistribution
  Description
    Text
      The @wikipedia "binomial distribution"@, the distribution of the number
      of successes in a sequence of @TT "n"@ Bernoulli trials, where the
      probability of success is @TT "p"@.
    Example
      X = binomialDistribution(10, 0.1)
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
    Text
      A special case is the @wikipedia "Bernoulli distribution"@, where
      @TT "n"@ is 1.
    Example
      Y = bernoulliDistribution 0.1
///

doc ///
  Key
    poissonDistribution
    (poissonDistribution, Number)
  Headline
    Poisson distribution
  Usage
    poissonDistribution lambda
  Inputs
    lambda:Number -- the rate parameter
  Outputs
    :DiscreteProbabilityDistribution
  Description
    Text
      The @wikipedia "Poisson distribution"@, the distribution of the number
      of events to occur during some interval of time when the expected number
      of events is @TT "lambda"@.
    Example
      X = poissonDistribution 10
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
///

doc ///
  Key
    geometricDistribution
    (geometricDistribution, Number)
  Headline
    geometric distribution
  Usage
    geometricDistribution p
  Inputs
    p:Number -- between 0 and 1
  Outputs
    :DiscreteProbabilityDistribution
  Description
    Text
      The @wikipedia "geometric distribution"@, the distribution of the number
      of a failures in a sequence of Bernoulli trials before the first success,
      where @TT "p"@ is the probability of a success.
    Example
      X = geometricDistribution 0.1
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
  Caveat
    Some probability texts define the geometric distribution as the number
    of Bernoulli trials until the first success, and so the values will be
    one greater than ours.  Our definition is consistent with R.
///

doc ///
  Key
    negativeBinomialDistribution
    (negativeBinomialDistribution, Number, Number)
  Headline
    negative binomial distribution
  Usage
    negativeBinomialDistribution(r, p)
  Inputs
    r:Number -- positive
    p:Number -- between 0 and 1
  Outputs
    :DiscreteProbabilityDistribution
  Description
    Text
      The @wikipedia "negative binomial distribution"@, the distribution of
      the number of failures in a sequence of Bernoulli trials (with probability
      of success @TT "p"@) until the @TT "r"@th success.
    Example
      X = negativeBinomialDistribution(5, 0.1)
      density_X 20
      probability_X 30
      quantile_X 0.4
      random X
  Caveat
    Probability texts define the negative binomial distribution in a variety
    of different ways.  Our definition is consistent with R.
///

doc ///
  Key
    hypergeometricDistribution
    (hypergeometricDistribution, ZZ, ZZ, ZZ)
  Headline
    hypergeometric distribution
  Usage
    hypergeometricDistribution(m, n, k)
  Inputs
    m:ZZ -- nonnegative
    n:ZZ -- nonnegative
    k:ZZ -- between 0 and @TT "m + n"@
  Outputs
    :DiscreteProbabilityDistribution
  Description
    Text
      The @wikipedia "hypergeometric distribution"@, the number of white balls
      drawn after drawing @TT "k"@ balls from an urn containing @TT "m"@ white
      and @TT "n"@ black balls.
    Example
      X = hypergeometricDistribution(5, 6, 4)
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
 Caveat
   Probability texts often use the total number of balls (our @TT "m + n"@)
   as one of the parameters of this distribution.  Our definition is
   consistent with R.
///

doc ///
  Key
    uniformDistribution
    1:uniformDistribution
    (uniformDistribution, Number, Number)
  Headline
    continuous uniform distribution
  Usage
    uniformDistribution(a, b)
  Inputs
    a:Number
    b:Number -- greater than @TT "a"@
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @TT "continuous uniform distribution"@.
    Example
      X = uniformDistribution(3, 7)
      density_X 4
      probability_X 5
      quantile_X 0.6
      random X
    Text
      With no arguments, the standard uniform distribution on [0, 1] is
      returned.
    Example
      uniformDistribution()
///

doc ///
  Key
    exponentialDistribution
    (exponentialDistribution, Number)
  Headline
    exponential distribution
  Usage
    exponentialDistribution lambda
  Inputs
    lambda:Number -- the rate parameter (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "exponential distribution"@, the waiting time between
      events in a Poisson process, where @TT "lambda"@ is the expected number
      of events in one unit of time.
    Example
      X = exponentialDistribution 0.25
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
///

doc ///
  Key
    normalDistribution
    1:normalDistribution
    (normalDistribution, Number, Number)
  Headline
    normal distribution
  Usage
    normalDistribution(mu, sigma)
  Inputs
    mu:Number -- the mean
    sigma:Number -- the standard deviation (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "normal distribution"@ with mean @TT "mu"@ and standard
      deviation @TT "sigma"@.
    Example
      X = normalDistribution(30, 5)
      density_X 25
      probability_X 35
      quantile_X 0.75
      random X
    Text
      With no arguments, the standard normal distribution with mean 0 and
      standard deviation 1 is returned.
    Example
      Z = normalDistribution()
      quantile_Z 0.975
///

doc ///
  Key
    gammaDistribution
    (gammaDistribution, Number, Number)
  Headline
    gamma distribution
  Usage
    gammaDistribution(alpha, lambda)
  Inputs
    alpha:Number -- the shape parameter (positive)
    lambda:Number -- the rate parameter (positive)
  Description
    Text
      The @wikipedia "gamma distribution"@.  When @TT "alpha"@ is an integer
      (also known as the @wikipedia "Erlang distribution"@), this is waiting
      time until the @TT "alpha"@th event in a Poisson process where
      @TT "lambda"@ is the expected number of events in one unit of time.  In
      particular, when @TT "alpha"@ is 1, this is the
      @TO2 {exponentialDistribution, "exponential distribution"}@.
    Example
      X = gammaDistribution(5, 0.6)
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
  Caveat
    Some probability texts define the gamma distribution using a @EM "scale"@
    parameter instead of a rate parameter.  They are reciprocals of one another.
///

doc ///
  Key
    chiSquaredDistribution
    (chiSquaredDistribution, Number)
  Headline
    chi-squared distribution
  Usage
    chiSquaredDistribution n
  Inputs
    n:Number -- the degrees of freedom (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "chi-squared distribution"@, the distribution of the sum
      of @TT "n"@ squares of independent
      @TO2 {normalDistribution, "normally distributed"}@ random variables.
    Example
      X = chiSquaredDistribution 5
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
///

doc ///
  Key
    tDistribution
    (tDistribution, Number)
  Headline
    Student's t-distribution
  Usage
    tDistribution df
  Inputs
    df:Number -- the degrees of freedom (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "Student's t-distribution"@.
    Example
      X = tDistribution 5
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
///

doc ///
  Key
    fDistribution
    (fDistribution, Number, Number)
  Headline
    F-distribution
  Usage
    fDistribution(d1, d2)
  Inputs
    d1:Number -- the numerator degrees of freedom (positive)
    d2:Number -- the denominator degrees of freedom (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "F-distribution"@, widely used in
      @wikipedia "ANOVA"@.
    Example
      X = fDistribution(5, 6)
      density_X 2
      probability_X 3
      quantile_X 0.4
      random X
///

doc ///
  Key
    betaDistribution
    (betaDistribution, Number, Number)
  Headline
    beta distribution
  Usage
    betaDistribution(alpha, beta)
  Inputs
    alpha:Number -- shape parameter (positive)
    beta:Number -- shape parameter (positive)
  Outputs
    :ContinuousProbabilityDistribution
  Description
    Text
      The @wikipedia "beta distribution"@.
    Example
      X = betaDistribution(5, 6)
      density_X 0.2
      probability_X 0.3
      quantile_X 0.4
      random X
///

undocumented {(texMath, ProbabilityDistribution)}

TEST ///
X = binomialDistribution(10, 0.25)
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, binomial(10, 3) * 0.25^3 * 0.75^7)
assert Equation(density_X 3.5, 0)
assert Equation(density_X 11, 0)

assert Equation(probability_X(-1), 0)
assert Equation(probability_X 3,
    sum(0..3, x -> binomial(10, x) * 0.25^x * 0.75^(10 - x)))
assert Equation(probability_X 3.5,
    sum(0..3, x -> binomial(10, x) * 0.25^x * 0.75^(10 - x)))
assert Equation(probability_X 11, 1)

assert Equation(quantile_X 0, 0)
assert Equation(quantile_X 0.3, 2)
assert Equation(quantile_X 1, 10)
///

TEST ///
X = poissonDistribution 3
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 3^3/3! * exp(-3))
assert Equation(density_X 3.5, 0)

assert Equation(probability_X(-1), 0)
assert (abs(probability_X 3 - sum(0..3, x -> 3^x / x! * exp(-3))) < 1e-15)
assert (abs(probability_X 3.5 - sum(0..3, x -> 3^x / x! * exp(-3))) < 1e-15)

assert Equation(quantile_X 0, 0)
assert Equation(quantile_X 0.3, 2)
assert Equation(quantile_X 1, infinity)
///

TEST ///
X = geometricDistribution 0.25
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 0.25 * 0.75^3)
assert Equation(density_X 3.5, 0)

assert Equation(probability_X(-1), 0)
assert Equation(probability_X 3, 1 - 0.75^4)
assert Equation(probability_X 3.5, 1 - 0.75^4)

assert Equation(quantile_X 0, 0)
assert Equation(quantile_X 0.3, 1)
assert Equation(quantile_X 1, infinity)
///

TEST ///
X = negativeBinomialDistribution(3, 0.25)
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, binomial(5, 3) * 0.25^3 * 0.75^3)
assert Equation(density_X 3.5, 0)

assert Equation(probability_X(-1), 0)
assert(abs(probability_X 3 - 0.1694336) < 1e-7) -- R: pnbinom(3, 3, 0.25)
assert(abs(probability_X 3.5 - 0.1694336) < 1e-7)

assert Equation(quantile_X 0, 0)
assert Equation(quantile_X 0.3, 5)
assert Equation(quantile_X 1, infinity)
///

TEST ///
X = hypergeometricDistribution(5, 6, 7)
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 5/11)
assert Equation(density_X 3.5, 0)
assert Equation(density_X 6, 0)

assert Equation(probability_X(-1), 0)
assert Equation(probability_X 3, 43/66)
assert Equation(probability_X 3.5, 43/66)
assert Equation(probability_X 6, 1)

assert Equation(quantile_X 0, 0)
assert Equation(quantile_X 0.3, 3)
assert Equation(quantile_X 1, 5)
///

TEST ///
X = uniformDistribution(1, 7)
assert Equation(density_X 0, 0)
assert Equation(density_X 3, 1/6)
assert Equation(density_X 8, 0)

assert Equation(probability_X 0, 0)
assert Equation(probability_X 3, 1/3)
assert Equation(probability_X 8, 1)

assert Equation(quantile_X 0, 1)
assert Equation(quantile_X(1/3), 3)
assert Equation(quantile_X 1, 7)
///

TEST ///
X = exponentialDistribution 3
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 3 * exp(-9))

assert Equation(probability_X(-1), 0)
assert Equation(probability_X 3, 1 - exp(-9))

assert Equation(quantile_X 0, 0)
assert(abs(quantile_X(1 - exp(-9)) - 3) < 1e-13)
///

TEST ///
X = normalDistribution(3, 2)
assert Equation(density_X 0, 1/(sqrt(8 * pi)) *  exp(-9/8))
assert Equation(density_X 3, 1/(sqrt(8 * pi)))

assert(abs(probability_X 0 - 0.0668072) < 1e-7) -- R: pnorm(0, 3, 2)
assert Equation(probability_X 3, 0.5)

assert(abs quantile_X 0.0668072 < 1e-7)
assert Equation(quantile_X 0.5, 3)
///

TEST ///
X = gammaDistribution(3, 2)
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 36 * exp(-6))

assert Equation(probability_X(-1), 0)
assert(abs(probability_X 3 - 0.9380312) < 1e-7) -- R: pgamma(3, 3, 2)

assert Equation(quantile_X 0, 0)
assert(abs(quantile_X 0.9380312 - 3) < 1e-7)
///

TEST ///
X = chiSquaredDistribution 3
assert Equation(density_X(-1), 0)
assert Equation(density_X 3, 1/(2^1.5 * Gamma(1.5)) * sqrt 3 * exp(-1.5))

assert Equation(probability_X(-1), 0)
assert(abs(probability_X 3 - 0.6083748) < 1e-7) -- R: pchisq(3, 3)

assert Equation(quantile_X 0, 0)
assert(abs(quantile_X 0.6083748 - 3) < 1e-6)
///

TEST ///
X = tDistribution 3
assert(abs(density_X 0 - 0.3675526) < 1e-7)  -- R: dt(0, 3)
assert(abs(density_X 3 - 0.02297204) < 1e-7) -- R: dt(3, 3)

assert(abs(probability_X(-3) - 0.02883444) < 1e-7) -- R: pt(-3, 3)
assert(abs(probability_X 0 -  0.5) < 1e-7)
assert(abs(probability_X 3 - 0.9711656) < 1e-7) -- R: pt(3, 3)

assert(abs(quantile_X 0.02883444 + 3) < 1e-6)
assert(abs quantile_X 0.5 < 1e-7)
assert(abs(quantile_X 0.9711656 - 3) < 1e-5)
///

TEST ///
X = fDistribution(3, 2)
assert Equation(density_X(-1), 0)
assert(abs(density_X 3 - 0.06727939) < 1e-7) -- R: df(3, 3, 2)

assert Equation(probability_X(-1), 0)
assert(abs(probability_X 3 - 0.7400733) < 1e-7) -- R: pdf(3, 3, 2)

assert Equation(quantile_X 0, 0)
assert(abs(quantile_X 0.7400733 - 3) < 1e-7)
///

TEST ///
X = betaDistribution(3, 2)
assert Equation(density_X(-1), 0)
assert Equation(density_X 0.3, 0.756) -- R: dbeta(0.3, 3, 2)
assert Equation(density_X 2 , 0)

assert Equation(probability_X(-1), 0)
assert (abs(probability_X 0.3 - 0.0837) < 1e-15) -- R: pbeta(0.3., 3, 2)
assert Equation(probability_X 2, 1)

assert Equation(quantile_X 0, 0)
assert (abs(quantile_X 0.0837 - 0.3) < 1e-15)
assert Equation(quantile_X 1, 1)
///

TEST ///
-- sum of two dice
X = discreteProbabilityDistribution(
    x -> 1/36 * (if x < 8 then x - 1 else 13 - x),
    Support => (2, 12))
assert Equation(density_X 1, 0)
assert Equation(density_X 3, 1/18)
assert Equation(density_X 3.5, 0)
assert Equation(density_X 13, 0)

assert Equation(probability_X 1, 0)
assert Equation(probability_X 3, 1/12)
assert Equation(probability_X 3.5, 1/12)
assert Equation(probability_X 13, 1)

assert Equation(quantile_X 0, 2)
assert Equation(quantile_X 0.3, 6)
assert Equation(quantile_X 1, 12)
///

TEST ///
-- Cauchy distribution w/ location = 3, scale = 2
X = continuousProbabilityDistribution(
    x -> 1 / (pi * 2 * (1 + (x - 3)^2/4)),
    Support => (-infinity, infinity))

assert(abs(density_X 0 - 0.04897075) < 1e-7)   -- R: dcauchy(0, 3, 2)
assert(abs(probability_X 0 - 0.187167) < 0.01) -- R: pcauchy(0, 3, 2)
assert(abs(quantile_X 0.3 - 1.546915) < 0.1)   -- R: qcauchy(0.3, 3, 2)

-- try specifying cdf to improve things
X = continuousProbabilityDistribution(
    x -> 1 / (pi * 2 * (1 + (x - 3)^2/4)),
    DistributionFunction => x -> 1/pi * atan((x - 3)/2) + 1/2,
    Support => (-infinity, infinity))
assert(abs(probability_X 0 - 0.187167) < 1e-7)
assert(abs(quantile_X 0.3 - 1.546915) < 1e-7) 
///
