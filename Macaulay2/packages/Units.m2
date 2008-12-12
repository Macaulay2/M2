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

units = x -> (
     one = 1_(ring gm);
     mole = mol;
     amp = ampere = A;
     second = s;
     minute = 60 s;
     hour = 60 minute;
     day = 24 hour;
     wk = week = 7 day;
     fortnight = 14 day;
     year = tropicalyear = 365.242198781 day;
     meter = m;
     gm = gram  = .001 kg;
     t = tonne = 1000 kg;
     exa   = 1e18;
     peta  = 1e15;
     tera  = 1e12;
     giga  = 1e9;
     mega  = 1000000;
     kilo  = 1000;
     hecto = 100;
     deka  = 10;
     deci  = .1;
     centi = .01;
     milli = .001;
     micro = .000001;
     nano  = 1e-9;
     pico  = 1e-12;
     femto = 1e-15;
     atto  = 1e-18;
     inch = 2.54 centi meter;
     foot = 12 inch;
     yard = 3 foot;
     mile = 5280 foot;
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
     circle = 2. pi radian;
     steradian = m^2/m^2;
     arcdeg = deg = {* degree = *} 1/360 * circle;
     arcmin = arcdeg/60;
     arcsec = arcmin/60;
     degC = degcelsius = K;
     degF = degfahrenheit = 5/9 * degC;
     c = 2.99792458e8 m/s;
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
     )

apply({CGS,MKS},
     UUU -> (
	  use UUU;
	  type := new MutableHashTable;
	  scan({
	       	    m => "length",
		    m^2 => "area",
		    m^3 => "volume",
	       	    volt => "voltage",
		    newton => "force",
		    gm => "mass",
		    ohm => "resistance",
		    mho => "conductance",
		    pascal => "pressure",
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
		    newton s/m^2 => "viscosity"
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
