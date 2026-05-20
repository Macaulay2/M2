/**
 * @file debug.hpp
 * @brief Debugger-callable `d*` helpers that pretty-print engine values to stderr.
 *
 * Declares short-named global functions --- `dmatrix`,
 * `drelem`, `dfree`, `dringelem`, `dNterm`, `dvec`, `dgbvec`,
 * `drespoly` / `drespoly2`, `dmutablemat`, `dmonideal`,
 * `dintarray`, `dvector`, `dstash`, `dRRR`, `pring`, `showint`
 * --- that print engine objects in a form readable from gdb or
 * lldb. The implementations typically format into a local
 * `buffer` and flush it via `emit(o.str())` (defined in
 * `text-io.hpp`), which writes to stderr. Most of the
 * pointer-taking helpers are declared `extern "C"` so the
 * debugger can call them by an unmangled name from a stopped
 * frame.
 *
 * The functions exist alongside each class's own
 * `text_out(buffer&)` because they take only opaque pointers
 * (so any stack frame can use them, even one that has no
 * `buffer` available) and they flush immediately rather than
 * leaving the formatted text inside a caller-owned buffer. The
 * breadth of forward declarations is intentional --- every
 * printable engine type is forward-declared rather than
 * included so the header stays cheap to pull in.
 *
 * @see buffer.hpp
 * @see text-io.hpp
 */

#include "ringelem.hpp"

class Matrix;
class RingElement;
class FreeModule;
class Ring;
class GBRing;
class gbvector;
class res_poly;
class res2_poly;
struct resterm;
struct res2term;
class MutableMatrix;
class MonomialIdeal;

void showint(mpz_srcptr a);

void dmatrix(const Matrix *M);

void drelem(const RingElement *f);

void dfree(const FreeModule *F);

extern "C" void dringelem(const Ring *R, const ring_elem f);

extern "C" void dNterm(const Ring *R, const Nterm *f);

extern "C" void dvec(const Ring *R, const vec v);

extern "C" void dgbvec(const GBRing *R, gbvector *v);

void drespoly(const res_poly *R, const resterm *f);

void drespoly2(const res2_poly *R, const res2term *f);

extern "C" void dmutablemat(MutableMatrix *m);

extern "C" void dmonideal(MonomialIdeal *m);

extern "C" void dintarray(M2_arrayint a);

template <typename T>
void dvector(gc_vector<T>& a);

template <typename T>
void dvector(std::vector<T>& a);

extern "C" void dstash();

extern "C" void dRRR(gmp_RR a);

extern "C" void pring(const Ring *R);

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
