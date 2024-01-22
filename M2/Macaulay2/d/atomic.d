use arithmetic;
declarations "
  #ifdef __cplusplus
    #include <atomic>
    using std::atomic_fetch_add;
    using std::atomic_int;
    using std::atomic_load;
    using std::atomic_signal_fence;
    using std::atomic_store;
    using std::memory_order_seq_cst;
  #else
    #include <stdatomic.h>
  #endif

  #ifndef atomic_field_decl
  #define atomic_field_decl
  /* if you change this, change the copy in ../system/supervisor.hpp, too; better yet, remove it. */
  struct atomic_field {
       atomic_int field;
       };

  #define load_Field(x) atomic_load(&(x).field)
  #define test_Field(x) (load_Field(x) != 0)
  #define store_Field(x,val) atomic_store(&(x).field,val)
  #endif
  ";
export atomicField := Type "struct atomic_field";
export atomicFieldPointer := Pointer "struct atomic_field *";
export atomicFieldPointerOrNull := atomicFieldPointer or null;
export address(x:atomicField) ::= Ccode(atomicFieldPointer,"(&(",x,"))");
export contents(x:atomicFieldPointer) ::= Ccode(atomicField,"(*(",x,"))");
export atomicInt := integerType "atomic_int";
export load(x:atomicField) ::= Ccode(atomicInt, "load_Field(",x,")");
export test(x:atomicField) ::= (load(x) != atomicInt(0));
export store(x:atomicField,y:atomicInt) ::= Ccode(void, "store_Field(",x,",",y,")");
export store(x:atomicFieldPointer,y:atomicInt) ::= store(contents(x),y);
export store(x:atomicFieldPointer,y:bool) ::= store(contents(x),y);
export store(x:atomicFieldPointer,y:int) ::= store(contents(x),y);
export toAtomicInt(x:bool) ::= Ccode(atomicInt,"((atomic_int)",x,")");
export store(x:atomicField,y:bool) ::= store(x,toAtomicInt(y));
export store(x:atomicField,y:int) ::= store(x,atomicInt(y));
export increment(x:atomicInt) ::= Ccode(atomicInt,"atomic_fetch_add(&(",x,", 1))");
export compilerBarrier() ::= Ccode(void,"atomic_signal_fence(memory_order_seq_cst)");

-- here is the way to declare one of these:
-- header "struct atomic_field x;";
-- import x:atomicField;
-- y := load(x);
-- store(x,1);

-- j := atomicInt(0);
-- increment(j);
-- increment(j);
-- k := int(j);
-- ar := iar.j;


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d atomic.o DEPENDS=no "
-- End:
