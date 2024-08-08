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

-- TODO: RRball and CCball at top level?

------------
-- RRball --
------------

RRball := Pointer "arb_ptr";
init(x:RRball) ::= (
    Ccode(void, "arb_init(", x, ")");
    x);
newRRball():RRball := init(GCmalloc(RRball));
clear(x:RRball) ::= Ccode(void, "arb_clear(", x, ")");

-- clear after using
toRRball(x:RR, y:RR, prec:ulong):RRball := (
    z := newRRball();
    Ccode(void, "arb_set_interval_mpfr(", z, ", ", x, ", ", y, ", ", prec, ")");
    z);
toRRball(x:RR):RRball := toRRball(x, x, precision(x));
toRRball(x:RRi):RRball := toRRball(leftRR(x), rightRR(x), precision(x));

toRR(x:RRball, prec:ulong):RR := (
    y := newRRmutable(prec);
    Ccode(int, "arf_get_mpfr(", y, ", arb_midref(", x, "), MPFR_RNDN)");
    moveToRRandclear(y));

toRRi(x:RRball, prec:ulong):RRi := (
    y := newRRimutable(prec);
    Ccode(void, "arb_get_interval_mpfr((mpfr_ptr)&", y,
	"->left, (mpfr_ptr)&", y, "->right, ", x, ")");
    moveToRRiandclear(y));

moveToRRiandclear(x:RRball, prec:ulong):RRi := (
    r := toRRi(x, prec);
    clear(x);
    r);

-- special functions
export eint(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_hypgeom_ei(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export Gamma(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_gamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export Gamma(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRball(z);
    y := toRRball(w);
    r := newRRball();
    Ccode(void, "arb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 0, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export regularizedGamma(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRball(z);
    y := toRRball(w);
    r := newRRball();
    Ccode(void, "arb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export Digamma(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_digamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export lgamma(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_lgamma(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export zeta(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_zeta(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export erf(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_hypgeom_erf(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export erfc(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_hypgeom_erfc(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export inverseErf(x:RRi):RRi := (
    y := toRRball(x);
    r := newRRball();
    Ccode(void, "arb_hypgeom_erfinv(", r, ", ", y, ", ", precision(x), ")");
    clear(y);
    moveToRRiandclear(r, precision(x)));

export BesselJ(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRball(z);
    y := toRRball(w);
    r := newRRball();
    Ccode(void, "arb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export BesselY(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRball(z);
    y := toRRball(w);
    r := newRRball();
    Ccode(void, "arb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export Beta(z:RRi,w:RRi):RRi := (
    prec := min(precision(z), precision(w));
    x := toRRball(z);
    y := toRRball(w);
    v := toRRball(toRRi(1, prec));
    r := newRRball();
    Ccode(void, "arb_hypgeom_beta_lower(", r, ", ", x, ", ", y, ", ", v,
	 ", 0, ", prec, ")");
    clear(x);
    clear(y);
    moveToRRiandclear(r, prec));

export regularizedBeta(u:RRi,v:RRi,w:RRi):RRi := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toRRball(u);
    y := toRRball(v);
    z := toRRball(w);
    r := newRRball();
    Ccode(void, "arb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToRRiandclear(r, prec));

------------
-- CCBall --
------------

CCball := Pointer "acb_ptr";
init(z:CCball) ::= (
    Ccode(void, "acb_init(", z, ")");
    z);
newCCball():CCball := init(GCmalloc(CCball));
clear(z:CCball) ::= Ccode(void, "acb_clear(", z, ")");

-- clear after using
toCCball(z:CC):CCball := (
    x := toRRball(realPart(z));
    y := toRRball(imaginaryPart(z));
    w := newCCball();
    Ccode(void, "acb_set_arb_arb(", w, ", ", x, ", ", y, ")");
    clear(x);
    clear(y);
    w);

toCC(z:CCball, prec:ulong):CC := (
    x := Ccode(RRball, "acb_realref(", z, ")");
    y := Ccode(RRball, "acb_imagref(", z, ")");
    toCC(toRR(x, prec), toRR(y, prec)));

moveToCCandclear(z:CCball, prec:ulong):CC := (
    r := toCC(z, prec);
    clear(z);
    r);

export eint(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_hypgeom_ei(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export Gamma(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_gamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export Gamma(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCball(z);
    y := toCCball(w);
    r := newCCball();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 0, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export regularizedGamma(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCball(z);
    y := toCCball(w);
    r := newCCball();
    Ccode(void, "acb_hypgeom_gamma_upper(", r, ", ", x, ", ", y, ", 1, ",
	prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export Digamma(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_digamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export lgamma(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_lgamma(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export zeta(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_zeta(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export erf(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_hypgeom_erf(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export erfc(z:CC):CC := (
    w := toCCball(z);
    r := newCCball();
    Ccode(void, "acb_hypgeom_erfc(", r, ", ", w, ", ", precision(z), ")");
    clear(w);
    moveToCCandclear(r, precision(z)));

export BesselJ(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCball(z);
    y := toCCball(w);
    r := newCCball();
    Ccode(void, "acb_hypgeom_bessel_j(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export BesselY(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCball(z);
    y := toCCball(w);
    r := newCCball();
    Ccode(void, "acb_hypgeom_bessel_y(", r, ", ", x, ", ", y, ", ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export Beta(z:CC,w:CC):CC := (
    prec := min(precision(z), precision(w));
    x := toCCball(z);
    y := toCCball(w);
    v := toCCball(toCC(1, prec));
    r := newCCball();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", x, ", ", y, ", ", v,
	 ", 0, ", prec, ")");
    clear(x);
    clear(y);
    moveToCCandclear(r, prec));

export regularizedBeta(u:CC,v:CC,w:CC):CC := (
    prec := min(min(precision(u), precision(v)), precision(w));
    x := toCCball(u);
    y := toCCball(v);
    z := toCCball(w);
    r := newCCball();
    Ccode(void, "acb_hypgeom_beta_lower(", r, ", ", y, ", ", z, ", ", x,
	 ", 1, ", prec, ")");
    clear(x);
    clear(y);
    clear(z);
    moveToCCandclear(r, prec));
