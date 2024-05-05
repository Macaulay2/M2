use arithmetic;

declarations "
  #include \"M2/atomic-field.h\"
  #ifdef __cplusplus
    #include <atomic>
    using std::atomic_fetch_add;
    using std::atomic_signal_fence;
    using std::memory_order_seq_cst;
  #else
    #include <stdatomic.h>
  #endif
  ";

export atomicField := Type "struct atomic_field";
export load(x:atomicField) ::= Ccode(int, "load_Field(",x,")");
export test(x:atomicField) ::= (load(x) != 0);
export store(x:atomicField,y:int) ::= Ccode(void, "store_Field(",x,",",y,")");
export store(x:atomicField,y:bool) ::= store(x, Ccode(int, y));
export fetch_add(x:atomicField,y:int) ::=
    Ccode(int, "atomic_fetch_add(&(",lvalue(x),").field,",y,")");

export compilerBarrier() ::= Ccode(void,"atomic_signal_fence(memory_order_seq_cst)");

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d atomic.o DEPENDS=no "
-- End:
