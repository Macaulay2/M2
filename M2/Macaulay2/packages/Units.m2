-- newPackage "Units"
-- partially taken from units.dat in the GNU unit program
k = RR -- change RR to CC later after the bug is fixed
common := (
     symbol s,
     symbol A,
     symbol cd,
     symbol mol,
     symbol K,
     symbol bit,
     symbol wholenote,
     symbol US$
     )
CGS = k[symbol cm,symbol g,common, Inverses=>true, MonomialOrder => Lex]
MKS = k[symbol m,symbol kg,common, Inverses=>true, MonomialOrder => Lex]
Number Number :=
MKS Constant := CGS Constant := Constant MKS := Constant CGS := Constant Number := Number Constant := 
MKS MKS := CGS CGS := MKS Number := Number MKS := CGS Number := Number CGS := times

square = x -> x^2
cubic = x -> x^3
reciprocal = x -> x^-1

MKS/MKS := CGS/CGS := (x,y) -> (
     if x == 0 then return x;
     if y == 0 then error "division by 0";
     if size y =!= 1 then error "division not possible";
     x//y)

units := identity

CGS.use = CGS -> (
     m = 100 cm;
     kg = 1000 g;
     units();
     )

MKS.use = MKS -> (
     cm = .01 m;
     g = .001 kg;
     units();
     )

use CGS ; toCGS = map(CGS,MKS,{m,kg}|drop(gens CGS,2))
use MKS ; toMKS = map(MKS,CGS,{cm,g}|drop(gens MKS,2))

exa = 1e18
peta = 1e15
tera = 1e12
giga = 1e9
mega = 1000000
kilo = 1000
hecto = 100
deka = 10
deci = .1
centi = .01
milli = .001
micro = .000001
nano = 1e-9
pico = 1e-12
femto = 1e-15
atto = 1e-18
hundred = 100
thousand = 1000
million = 1e6
billion = 1e9
trillion = 1e12
quadrillion = 1e15
quintillion = 1e18
sextillion = 1e21
septillion = 1e24
octillion = 1e27
noventillion = nonillion = 1e30
decillion = 1e33
undecillion = 1e36
duodecillion = 1e39
tredecillion = 1e42
quattuordecillion = 1e45
quindecillion = 1e48
sexdecillion = 1e51
septendecillion = 1e54
octodecillion = 1e57
novemdecillion = 1e60
vigintillion = 1e63
centillion = 1e303
googol = 1e100

units = x -> (
     kelvin = K;
     one = 1_(ring K);
     mole = mol;
     amp = ampere = A;
     second = s;
     minute = 60 s;
     hour = 60 minute;
     day = 24 hour;
     wk = week = 7 day;
     fortnight = 14 day;
     yr = year = tropicalyear = 365.242198781 day;
     mo = month = yr/12;
     decade = 10 yr;
     century = 100 yr;
     millennia = millennium = 1000 year;
     meter = m;
     gm = gram = .001 kg;
     t = tonne = 1000 kg;
     inch = 254/100 * cm;
     ft = foot = 12 inch;
     chain = 66 ft;
     yd = yard = 3 foot;
     rod = (5+1/2) yard;
     furlong = 40 rod;
     mile = 5280 foot;
     acre = mile^2/640;
     league = 3 mile;
     marathon = 26 mile + 385 yard;
     N = newton = kg m / s^2;
     mm = milli m;
     Pa = pascal = N/m^2;
     bar = 1e5 Pa;
     micron = micro meter;
     cc = cm^3;
     are = 100 m^2;
     hectare = hecto are;
     l = liter = 1000 cc;
     J = joule = m N;
     W = watt = J/s;
     C = coulomb = A s;
     V = volt = W/A;
     ohm = V/A;
     mho = 1/ohm;
     angstrom = 1e-10 m;
     fermi = 1e-15 m;
     F = farad = C/V;
     Wb = weber = V s;
     H = henry = Wb/A;
     T = tesla = Wb/m^2;
     Hz = hertz = 1/s;
     barn = 1e-28 m^2;
     diopter = 1/m;
     radian = m/m;
     circle = pi radian 2;
     sr = steradian = m^2/m^2;
     arcdeg = deg = {* degree = *} 1/360 * circle;
     arcmin = arcdeg/60;
     arcsec = arcmin/60;
     degC = degcelsius = K;
     degF = degfahrenheit = 5/9 * degC;
     light = c = 2.99792458e8 m/s;
     au = astronomicalunit = 499.004783806 light second;
     solarmass = 1.9891e30 kg;
     mu0 = 4 pi 1e-7 H/m;
     epsilon0 = 1/mu0 c^2;
     e = 1.602176462e-19 C;
     h = 6.62606876e-34 J s;
     hbar = h / 2 pi;
     G = 6.6743e-11 N m^2 / kg^2;
     amu = u = atomicmassunit = 1.66053873e-27 kg;
     N$A = avogadro = gram/amu mol;
     R = gasconstant = 8.314472 J / mol K;
     k = boltzmann = R / N$A;
     stdtemp = standardtemp = 273.15 K;
     atm = 101325 Pa;
     force = gravity = 9.80665 m/s^2;
     water = gram force/cm^3;
     waterdensity = gram / cm^3;
     molarvolume = mol R stdtemp / atm;
     alpha = 7.297352533e-3;
     Hg = 13.5951 gram force / cm^3;
     mmHg = mm Hg;
     inHg = inch Hg;
     dyne = cm gram / s^2;
     erg = cm dyne;
     P = poise = gram / cm s;
     candela = cd;
     lm = lumen = cd sr;
     lx = lux = lm/m^2;
     footcandle = lumen/ft^2;
     lb = pound = 0.45359237 kg;
     hp = horsepower = 550 foot pound force / s;
     grain = 1/7000 * pound;
     oz = ounce = 1/16 * pound;
     brgallon = 4.54609 l;
     gal = gallon = usgallon = 231 inch^3;
     qt = quart = 1/4 * gallon;
     pt = pint = 1/2 * quart;
     cup = 1/2 * pint;
     tbsp = tablespoon = 1/16 * cup;
     teaspoon = 1/3 tablespoon;
     floz = fluidounce = 1/16 * pint;
     bbl = barrel = 42 gallon;
     bu = bushel = 2150.42 inch^3;
     peck = 1/4 * bushel;
     )

survey = method()
apply({CGS,MKS},
     UUU -> (
	  use UUU;
	  surveyMap := map(UUU,UUU,{ leadMonomial foot => 1200/3937 * m/foot * leadMonomial foot });
	  survey UUU := x -> surveyMap x;
	  type := new MutableHashTable;
	  scan({
		    1_UUU => "dimensionless quantity",
		    s => "time",
	       	    m => "length",
		    m^2 => "area",
		    m^3 => "volume",
	       	    volt => "voltage",
		    newton => "force",
		    watt => "power",
		    gm => "mass",
		    ohm => "resistance",
		    mho => "conductance",
		    pascal => "pressure",
		    joule => "energy, work",
		    newton/m^2 => "stress",
		    ampere => "current",
		    mole => "amount",
		    radian => "angle",
		    steradian => "solid angle",
		    coulomb => "charge",
		    farad => "capacitance",
		    henry => "inductance",
		    hertz => "frequency",
		    m/s => "velocity",
		    m/s^2 => "acceleration",
		    kg/m^3 => "density",
		    poise => "viscosity",
		    kelvin => "temperature",
		    candela => "luminous intensity",
		    lumen => "luminous flux",
		    lux => "illuminance, exitance",
		    cd/m^2 => "luminance",
		    J/K => "entropy",
		    W/K => "entropy flow"
	       	    },
	       x -> ( m := leadMonomial x#0; type#m = if type#?m then append(type#m,x#1) else {x#1}));
	  UUU#{Standard,AfterPrint} = x -> (
     	       << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class x;
     	       if size x == 1 then (
		    m := leadMonomial x;
		    if type#?m then << concatenate(" (",between(", ",type#m),")"));
     	       << endl;
     	       )
	  ))

use MKS
