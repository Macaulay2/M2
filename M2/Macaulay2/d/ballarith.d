------------------------------------------
-- FLINT ball arithmetic (formerly Arb) --
------------------------------------------

use gmp;

declarations "
	#ifdef HAVE_ARB_H
	# include <arb.h>
	# include <acb.h>
	#endif
	#ifdef HAVE_FLINT_ARB_H
	# include <flint/arb.h>
	# include <flint/acb.h>
	#endif
	";

header "
	#ifdef HAVE_ARB_H
	# include <arb_hypgeom.h>
	# include <acb_hypgeom.h>
	#endif
	#ifdef HAVE_FLINT_ARB_H
	# include <flint/arb_hypgeom.h>
	# include <flint/acb_hypgeom.h>
	#endif
	";

-- TODO: RRb and CCb at top level?

------------
-- RRb --
------------

export RRb := Pointer "arb_ptr";
init(x:RRb) ::= (
    Ccode(void, "arb_init(", x, ")");
    x);
newRRb():RRb := init(GCmalloc(RRb));
clear(x:RRb) ::= Ccode(void, "arb_clear(", x, ")");

-- clear after using
export toRRb(x:RR, y:RR, prec:ulong):RRb := (
    z := newRRb();
    Ccode(void, "arb_set_interval_mpfr(", z, ", ", x, ", ", y, ", ", prec, ")");
    z);
export toRRb(x:RR):RRb := toRRb(x, x, precision(x));
export toRRb(x:RRi):RRb := toRRb(leftRR(x), rightRR(x), precision(x));

toRR(x:RRb, prec:ulong):RR := (
    y := newRRmutable(prec);
    Ccode(int, "arf_get_mpfr(", y, ", arb_midref(", x, "), MPFR_RNDN)");
    moveToRRandclear(y));

moveToRRandclear(x:RRb, prec:ulong):RR := (
    r := toRR(x, prec);
    clear(x);
    r);

toRRi(x:RRb, prec:ulong):RRi := (
    y := newRRimutable(prec);
    Ccode(void, "arb_get_interval_mpfr((mpfr_ptr)&", y,
	"->left, (mpfr_ptr)&", y, "->right, ", x, ")");
    moveToRRiandclear(y));

moveToRRiandclear(x:RRb, prec:ulong):RRi := (
    r := toRRi(x, prec);
    clear(x);
    r);

-- special functions
export eint(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_hypgeom_ei(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export Gamma(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_gamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export Gamma(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 0, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export regularizedGamma(z:RR,w:RR):RR := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToRRandclear(r, prec));

export regularizedGamma(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export Digamma(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_digamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export lgamma(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_lgamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export zeta(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_zeta(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export erf(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_hypgeom_erf(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export erfc(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_hypgeom_erfc(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export inverseErf(x:RR):RR := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_hypgeom_erfinv(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRandclear(r, precision(x)));

export inverseErf(x:RRi):RRi := (
    y := toRRb(x);
    r := newRRb();
    Ccode(void, "arb_hypgeom_erfinv(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export BesselJ(z:RR,w:RR):RR := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRandclear(r, prec));

export BesselJ(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export BesselY(z:RR,w:RR):RR := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRandclear(r, prec));

export BesselY(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export Beta(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    v := toRRb(toRRi(1, prec));
    r := newRRb();
    Ccode(void, "arb_hypgeom_beta_lower(", r, ", ", x, ", ", y, ", ", v,
	 ", 0, ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export regularizedBeta(u:RR,v:RR,w:RR):RR := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toRRb(u);
    y := toRRb(v);
    z := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToRRandclear(r, prec));

export regularizedBeta(u:RRi,v:RRi,w:RRi):RRi := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toRRb(u);
    y := toRRb(v);
    z := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToRRiandclear(r, prec));

export agm(z:RRi, w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_agm(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export sign(z:RRi):RRi := (
    w := toRRb(z);
    r := newRRb();
    Ccode(void, "arb_sgn(", r, ", ", w, ")");
    clear(w);
    moveToRRiandclear(r, precision(z)));

export polylog(z:RR, w:RR):RR := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_polylog(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRandclear(r, prec));

export polylog(z:RRi, w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRb(z);
    y := toRRb(w);
    r := newRRb();
    Ccode(void, "arb_polylog(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);

    moveToRRiandclear(r, prec));

------------
-- CCBall --
------------

export CCb := Pointer "acb_ptr";
init(z:CCb) ::= (
    Ccode(void, "acb_init(", z, ")");
    z);
newCCb():CCb := init(GCmalloc(CCb));
clear(z:CCb) ::= Ccode(void, "acb_clear(", z, ")");

-- clear after using
export toCCb(x:RRb, y:RRb):CCb := (
    z := newCCb();
    Ccode(void, "acb_set_arb_arb(", z, ", ", x, ", ", y, ")");
    z);

export toCCb(z:CC):CCb := (
    x := toRRb(realPart(z));
    y := toRRb(imaginaryPart(z));
    w := toCCb(x, y);
    clear(x);
    clear(y);
    w);

toCCb(z:CCi):CCb := (
    x := toRRb(realPart(z));
    y := toRRb(imaginaryPart(z));
    w := toCCb(x, y);
    clear(x);
    clear(y);
    w);

toCC(z:CCb, prec:ulong):CC := (
    x := Ccode(RRb, "acb_realref(", z, ")");
    y := Ccode(RRb, "acb_imagref(", z, ")");
    toCC(toRR(x, prec), toRR(y, prec)));

moveToCCandclear(z:CCb, prec:ulong):CC := (
    r := toCC(z, prec);
    clear(z);
    r);

toCCi(z:CCb, prec:ulong):CCi := (
    x := Ccode(RRb, "acb_realref(", z, ")");
    y := Ccode(RRb, "acb_imagref(", z, ")");
    toCCi(toRRi(x, prec), toRRi(y, prec)));

moveToCCiandclear(z:CCb, prec:ulong):CCi := (
    r := toCCi(z, prec);
    clear(z);
    r);

export sin(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sin(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export cos(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_cos(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export tan(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_tan(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export acos(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_acos(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export sec(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sec(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export csc(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_csc(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export cot(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_cot(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export sech(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sech(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export csch(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_csch(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export coth(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_coth(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export asin(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_asin(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export log1p(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_log1p(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export log1p(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_log1p(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export expm1(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_expm1(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export expm1(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_expm1(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export eint(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_ei(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export eint(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_ei(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export Gamma(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_gamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export Gamma(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_gamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export Gamma(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 0, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export Gamma(z:CCi,w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 0, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export regularizedGamma(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export regularizedGamma(z:CCi,w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export Digamma(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_digamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export Digamma(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_digamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export lgamma(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_lgamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export lgamma(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_lgamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export zeta(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_zeta(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export zeta(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_zeta(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export erf(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_erf(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export erf(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_erf(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export erfc(z:CC):CC := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_erfc(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export erfc(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_hypgeom_erfc(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export BesselJ(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export BesselJ(z:CCi,w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export BesselY(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export BesselY(z:CCi,w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export atan(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_atan(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export Beta(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    v := toCCb(toCC(1, prec));
    r := newCCb();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", x, ", ", y, ", ", v,
	 ", 0, ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export Beta(z:CCi,w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    v := toCCb(toCC(1, prec));
    r := newCCb();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", x, ", ", y, ", ", v,
	 ", 0, ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export regularizedBeta(u:CC,v:CC,w:CC):CC := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toCCb(u);
    y := toCCb(v);
    z := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToCCandclear(r, prec));

export regularizedBeta(u:CCi,v:CCi,w:CCi):CCi := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toCCb(u);
    y := toCCb(v);
    z := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToCCiandclear(r, prec));

export cosh(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_cosh(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export sinh(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sinh(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export tanh(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_tanh(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export exp(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_exp(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export log(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_log(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export log(z:CCi, w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_log(", x, ", ", x, ", ", prec, ")");
    Ccode(void, "acb_log(", y, ", ", y, ", ", prec, ")");
    Ccode(void, "acb_div(", r, ", ", y, ", ", x, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export (z:CCi) ^ (w:CCi):CCi := exp(log(z)*w);

export agm(z:CCi, w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_agm(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));

export sqrt(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sqrt(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export abs(z:CCi):RRi := (
    w := toCCb(z);
    r := newRRb();
    Ccode(void, "acb_abs(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToRRiandclear(r, precision(z)));

export sign(z:CCi):CCi := (
    w := toCCb(z);
    r := newCCb();
    Ccode(void, "acb_sgn(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCiandclear(r, precision(z)));

export polylog(z:CC, w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_polylog(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export polylog(z:CCi, w:CCi):CCi := (
    prec := min(precision(z), precision(w));
    x := toCCb(z);
    y := toCCb(w);
    r := newCCb();
    Ccode(void, "acb_polylog(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCiandclear(r, prec));
