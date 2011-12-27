use arithmetic;
declarations "
  #include <atomic_ops.h>
  #ifndef atomic_field_decl
  #define atomic_field_decl
  /* if you change this, change the copy in ../system/supervisor.hpp, too; better yet, remove it. */
  struct atomic_field {
       AO_t field;
       };

  #define load_Field(x) AO_load(&(x).field)
  #define test_Field(x) (load_Field(x) != 0)
  #define store_Field(x,val) AO_store(&(x).field,val)
  #endif
  ";
export LockValue := atomicType "AO_TS_VAL_t";
export LockField := atomicType "AO_TS_t";
export atomicField := Type "struct atomic_field";
export atomicFieldPointer := Pointer "struct atomic_field *";
export atomicFieldPointerOrNull := atomicFieldPointer or null;
export address(x:atomicField) ::= Ccode(atomicFieldPointer,"(&(",x,"))");
export contents(x:atomicFieldPointer) ::= Ccode(atomicField,"(*(",x,"))");
export atomicInt := integerType "AO_t";
export load(x:atomicField) ::= Ccode(atomicInt, "load_Field(",x,")");
export test(x:atomicField) ::= (load(x) != atomicInt(0));
export store(x:atomicField,y:atomicInt) ::= Ccode(void, "store_Field(",x,",",y,")");
export store(x:atomicFieldPointer,y:atomicInt) ::= store(contents(x),y);
export store(x:atomicFieldPointer,y:bool) ::= store(contents(x),y);
export store(x:atomicFieldPointer,y:int) ::= store(contents(x),y);
export toAtomicInt(x:bool) ::= Ccode(atomicInt,"((AO_t)",x,")");
export store(x:atomicField,y:bool) ::= store(x,toAtomicInt(y));
export store(x:atomicField,y:int) ::= store(x,atomicInt(y));
export testAndSet(x:LockField) ::= Ccode(LockValue,"AO_test_and_set(&",x,")");
export testSet() ::= Ccode(LockValue,"AO_TS_SET");
export testClear() ::= Ccode(LockValue,"AO_TS_CLEAR");
export newLockField() ::= Ccode(LockField,"AO_TS_INITIALIZER");
export acquire(t:LockField) ::= while testAndSet(t) == testSet() do nothing;
export release(t:LockField) ::= Ccode(void,"AO_CLEAR(&",t,")");
export increment(x:atomicInt) ::= Ccode(atomicInt,"AO_fetch_and_add1(&(",x,"))");
export compilerBarrier() ::= Ccode(void,"AO_compiler_barrier()");

-- here is the way to declare one of these:
-- header "struct atomic_field x;";
-- import x:atomicField;
-- y := load(x);
-- store(x,1);

-- s := newLockField();
-- acquire(s);
-- release(s);

-- j := atomicInt(0);
-- increment(j);
-- increment(j);
-- k := int(j);
-- ar := iar.j;


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d atomic.o DEPENDS=no "
-- End:
