// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_ATOMIC_GUARD
#define MATHICGB_ATOMIC_GUARD

// We need this include for std::memory_order even if we are not
// using std::atomic.
#include <atomic>

MATHICGB_NAMESPACE_BEGIN

namespace AtomicInternal {
  /// Tells the compiler (not the CPU) to not reorder reads across this line.
  inline void compilerReadMemoryBarrier();

  /// Tells the compiler (not the CPU) to not reorder writes across this line.
  inline void compilerWriteMemoryBarrier();

  /// Tells the compiler (not the CPU) to not reorder reads and writes across
  /// this line.
  inline void compilerReadWriteMemoryBarrier();

  /// Tells the CPU and also the compiler to not reorder reads and writes
  /// across this line.
  inline void cpuReadWriteMemoryBarrier();

  /// Stores a variable with sequentially consistent ordering. The variable
  /// must be aligned and have a size such that aligned reads of that size
  /// are atomic. This requies a locked instruction like XHCG according to
  /// http://goo.gl/U8xTK .
  template<class T>
  void seqCstStore(const T value, T& ref);
}

#if defined(_MSC_VER) && defined(MATHICGB_USE_CUSTOM_ATOMIC_X86_X64)
MATHICGB_NAMESPACE_END
#include <Windows.h>
MATHICGB_NAMESPACE_BEGIN
namespace AtomicInternal {
  template<class T, size_t size = sizeof(T)>
  struct SeqCst {};
#ifdef MATHICGB_USE_CUSTOM_ATOMIC_4BYTE
  template<class T> struct SeqCst<T, 4> {
    static void store(const T value, T& ref) {
      _InterlockedExchange((volatile LONG*)&ref, (LONG)value);
    }
  };
#endif
#ifdef MATHICGB_USE_CUSTOM_ATOMIC_8BYTE
  template<class T> struct SeqCst<T, 8> {
    static void store(const T value, T& ref) {
      _InterlockedExchange64((volatile _LONGLONG*)&ref, (_LONGLONG)value);
    }
  };
#endif

  inline void compilerReadMemoryBarrier() {_ReadBarrier();}
  inline void compilerWriteMemoryBarrier() {_WriteBarrier();}
  inline void compilerReadWriteMemoryBarrier() {_ReadWriteBarrier();}
  inline void cpuReadWriteMemoryBarrier() {MemoryBarrier();}
  template<class T>
  void seqCstStore(const T value, T& ref) {SeqCst<T>::store(value, ref);}
}
#endif

#if defined(__GNUC__) && defined(MATHICGB_USE_CUSTOM_ATOMIC_X86_X64)
namespace AtomicInternal {
  inline void compilerReadMemoryBarrier() {
    // As far as I can tell there is no way to do a partial optimization
    // barrier on GCC, so we have to do the full barrier every time.
    compilerReadWriteMemoryBarrier();
  }

  inline void compilerWriteMemoryBarrier() {compilerReadWriteMemoryBarrier();}

  inline void compilerReadWriteMemoryBarrier() {
    // As far as I can tell this is not documented to work, but it is the
    // only way to do this on GCC and it is what the Linux kernel does, so
    // that will have to be good enough.
    __asm__ __volatile__ ("" ::: "memory");
  }

  inline void cpuReadWriteMemoryBarrier() {__sync_synchronize();}

  template<class T>
  void seqCstStore(const T value, T& ref) {
    const auto ptr = static_cast<volatile T*>(&ref);
    while (!__sync_bool_compare_and_swap(ptr, *ptr, value)) {}
  }    
}
#endif

namespace AtomicInternal {
#ifdef MATHICGB_USE_FAKE_ATOMIC
  // This class has the same interface as the actual custom atomic
  // class but it does absolutely no synchronization and it does not
  // constrain compiler optimizations in any way. The purpose of this class
  // is to enable it while running only a single thread to determine the
  // overhead imposed by the atomic operations.
  template<class T>
  class FakeAtomic {
  public:
    FakeAtomic(): mValue() {}
    FakeAtomic(T value): mValue(value) {}
    T load(const std::memory_order) const {return mValue;}
    void store(const T value, const std::memory_order order) {mValue = value;}

  private:
    T mValue;
  };

  template<class T, size_t size>
  struct ChooseAtomic {
    typedef FakeAtomic<T> type;
  };

#else
  /// Class for deciding which implementation of atomic to use. The default is
  /// to use std::atomic which is a fine choice if std::atomic is implemented
  /// in a reasonable way by the standard library implementation you are using.
  template<class T, size_t size>
  struct ChooseAtomic {
    typedef std::atomic<T> type;
  };
#endif
}

#ifdef MATHICGB_USE_CUSTOM_ATOMIC_X86_X64
namespace AtomicInternal {
  /// Custom Atomic class for x86 and x64. Uses special compiler directives
  /// for barriers. Only instantiate this for sizes where aligned reads and
  /// writes are guaranteed to be atomic - this class only takes care of the
  /// ordering constraints using CPU and compiler fences. Since the directives
  /// to achieve this are coming from the compiler it is surprising that
  /// any compiler ships with a std::atomic that is worse than this - but
  /// that is very much the case. Though implementing atomic load and store
  /// is very little code, as you can see, it is quite tricky and took me
  /// a long time to understand everything well enough to actually know what
  /// those few lines of code should say. Perhaps compiler vendors have
  /// postponed fast atomics just because they want to focus on other things
  /// since it's not that easy to figure out how to do right (on that topic,
  /// bug reports on this code are welcome!).
  ///
  /// There are 5 kinds of reorderings that we are concerned with here. Let
  /// S,S' be stores and let L,L' be stores. Note that these short-hands may
  /// be idiosyncratic - feel free to find some standard terminology from
  /// some prominent source and fix this to reflect that.
  ///
  ///   SS: Store-after-store: Reorder S,S' to S',S
  ///   SL: Store-after-load: Reorder S,L to L,S
  ///   LS: Load-after-store: Reorder L,S to S,L
  ///   LL: Load-after-load: Reorder L,L' to L',L
  ///   DLL: Dependent-load-after-load: As LL but L' depends on L. For example
  ///     reordering the load of p->a to before the load of p is a DLL.
  ///
  /// The DEC Alpha processor will perform all of these reorderings in the
  /// absense of memory barriers telling it not to do that, including DLL.
  /// DLL can happen on DEC Alpha if p->a is cached locally while p is not.
  /// Then p will be loaded from memory while p->a is loaded from the cache,
  /// which is functionally identical to loading p->a before p since we may
  /// see a value of p->a from the past while we get an up to date value of p.
  /// This can happen even if the processor that stored p did a full memory
  /// barrier between storing p->a and storing p.
  ///
  /// Compilers will also perform all of these reorderings to optimize the
  /// code - even including DLL. DLL happens if the compiler guesses what
  /// the value of p is, loads p->a and then checks that the guess for p
  /// was correct. This directly causes p->a to be actually loaded before p.
  /// These kinds of optimizations turn up in profile-driven optimization.
  ///
  /// You can check this out here:
  ///   http://en.wikipedia.org/wiki/Memory_ordering
  ///
  /// On x86 and x64 only SL is done by the CPU, so we need a CPU barrier to
  /// prevent that and nothing else. The compiler is free to perform all of
  /// these reorderings, so we need lots of compiler optimization barriers
  /// to deal with all of these cases.
  ///
  /// Some of the quotes below are from:
  ///   http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1525.htm
  ///
  /// Also interesting:
  ///http://bartoszmilewski.com/2008/11/05/who-ordered-memory-fences-on-an-x86/
  template<class T>
  class CustomAtomicX86X64 {
  public:
    CustomAtomicX86X64(): mValue() {}
    CustomAtomicX86X64(T value): mValue(value) {}

    MATHICGB_INLINE
    T load(const std::memory_order order) const {
      switch (order) {
      case std::memory_order_relaxed:
        // There are two constraints for memory_order_relaxed. The first
        // constraint is that if you read *p, then you will never
        // after that read a value of *p that was stored before the value
        // you just read, where "before" is in terms of either the same thread
        // that did the writing or external synchronization of another thread
        // with this thread. This is automaticaly guaranteed on this platform
        // and the compiler cannot break this guarantee.
        //
        // The second constraint is that writes performed by other threads will
        // appear "in a timely manner" when reading the written-to memory
        // location. It is not clear how much time is allowed to elapse before
        // this constraint is broken, but clearly the written value must turn
        // up eventually. This constraint could be broken in cases like this:
        //
        //   while (x.load(std::memory_order_relaxed) == 0) {
        //     // do something that does not write to x
        //   }
        //
        // The problem is that the compiler might observe that x is not being
        // written to inside the loop, so the expression x.load(...) should be
        // the value for every iteration, so the code can safely be transformed
        // to:
        //
        //   if (x.load(std::memory_order_relaxed) == 0) {
        //     while (true) {
        //       // ...
        //     }
        //   }
        //
        // So we need to tell the compiler that the value can change due to
        // code not in view of the current thread of execution. There are two
        // solutions that I have considered:
        //
        // * A compiler read memory barrier. This is overkill because it stops
        //   the compiler from moving all reads across this line.
        //
        // * A volatile read. This is pretty much exactly what we need on gcc,
        //   but on MSVC it provides extra guarantees that we do not need here:
        //   http://msdn.microsoft.com/en-us/library/12a04hfd%28v=vs.80%29.aspx
        //
        // I do not think there is any best choice on MSVC, so I went for a
        // volatile read since it is the best choice on GCC.
        return const_cast<volatile T&>(mValue);

      case std::memory_order_consume: {
        // Loads in this thread that depend on the loaded value must not be
        // reordered to before this load. So no DLL reorderings past this
        // load from after to before (up). So we need a read barrier AFTER the
        // load. It is a compiler only barrier since x86 and x64 CPUs do not do
        // DLL reorderings. 
        const auto value = mValue;
        compilerReadMemoryBarrier();
        return value;
      }

      case std::memory_order_acquire: {
        // Loads in this thread must not be reordered to before this load.
        // So no LL reorderings past this load from after to before (up).
        // So we need a read barrier AFTER the load. It is a compiler only
        // barrier since x86 and x64 CPUs do not do LL reorderings.
        const auto value = mValue;
        compilerReadMemoryBarrier();
        return value;
      }

      case std::memory_order_seq_cst: {
        // There must be some global order in which all sequentially consistent
        // atomic operations are considered to have happened in. On x86 and x64
        // this is guaranteed by just a normal read as long as all writes use
        // locked instructions like XHCG. See: http://goo.gl/U8xTK
        //
        // We still need to prevent the compiler from reordering the reads,
        // which is the same constraint as for std::memory_order_acquire.
        const auto value = mValue;
        compilerReadMemoryBarrier();
        return value;
      }

      case std::memory_order_release: // not available for load
      case std::memory_order_acq_rel: // not available for load
      default: // specified value is not a known std::memory_order
        MATHICGB_UNREACHABLE;
      }
    }

    MATHICGB_INLINE
    void store(const T value, const std::memory_order order) {
      switch (order) {
      case std::memory_order_relaxed:
        // There are no reordering constraints here but we need to tell the
        // compiler that it must actually write out the value to memory in
        // a scenario like this:
        //
        //   x.store(1, std::memory_order_relaxed);
        //   while (true) {}
        //
        // So as for relaxed store we need either a volatile access or a memory
        // barrier. I chose a volatile access for the same reason as for the
        // store: MSVC has no best choice and for GCC the volatile is perfect.
        const_cast<volatile T&>(mValue) = value;
        break;

      case std::memory_order_release:
        // Stores in this thread must not be reordered to after this store.
        // So no SS reorderings past this load from before to after (down).
        // So we need a barrier BEFORE the load. It is a compiler only barrier
        // since x86 and x64 CPUs do not do SS reorderings.
        compilerWriteMemoryBarrier();
        mValue = value;
        break;

      case std::memory_order_acq_rel:
        // Combine the guarantees for std::memory_order_acquire and
        // std::memory_order_release. So no loads moved up past here (SL) and
        // no stores moved down past here (LL). We need a compiler barrier
        // BEFORE the load to avoid LL and a CPU (+compiler) barrier AFTER the
        // load to avoid SL, since x86 and x64 CPUs can in fact do SL
        // reordering.
        compilerWriteMemoryBarrier();
        mValue = value;
        cpuReadWriteMemoryBarrier();
        break;

      case std::memory_order_seq_cst:
        // All operations happen in a globally consistent total order.
        seqCstStore(value, mValue);

        break;

      case std::memory_order_consume: // not available for store
      case std::memory_order_acquire: // not available for store
      default: // specified value is not a known std::memory_order
        MATHICGB_UNREACHABLE;
      }
    }

  private:
    T mValue;
  };

#ifdef MATHICGB_USE_CUSTOM_ATOMIC_4BYTE
  template<class T>
  struct ChooseAtomic<T, 4> {
    typedef CustomAtomicX86X64<T> type;
  };
#endif

#ifdef MATHICGB_USE_CUSTOM_ATOMIC_8BYTE
  template<class T>
  struct ChooseAtomic<T, 8> {
    typedef CustomAtomicX86X64<T> type;
  };
#endif
}
#endif

/// This class is equivalent to std::atomic<T>. Some functions from the
/// interface of std::atomic are missing - add them as necessary. Do not add
/// operator= and operator T() --- it is better to make the code explicit
/// about when and how loading and storing of atomic variables occurs.
///
/// The purpose of the class is that it performs far better than
/// std::atomic for some implementations. For example the std::atomic in MSVC
/// 2012 performs a compare-and-swap operation on a load even with the
/// paramter std::memory_order_relaxed.
///
/// We force all the functions to be inline because they can contain switches
/// on the value of std::memory_order. This will usually be a compile-time
/// constant parameter so that after inlining the switch will disappear. Yet
/// the code size of the switch may make some compilers avoid the inline.
template<class T>
class Atomic {
public:
  Atomic(): mValue() {}
  Atomic(T value): mValue(value) {}

  MATHICGB_INLINE
  T load(const std::memory_order order = std::memory_order_seq_cst) const {
    MATHICGB_ASSERT(debugAligned());
    return mValue.load(order);
  }

  MATHICGB_INLINE
  void store(
    const T value,
    const std::memory_order order = std::memory_order_seq_cst
  ) {
    MATHICGB_ASSERT(debugAligned());
    mValue.store(value, order);
  }

private:
  Atomic(const Atomic<T>&); // not available
  void operator=(const Atomic<T>&); // not available

  bool debugAligned() const {
    return reinterpret_cast<size_t>(&mValue) % sizeof(T) == 0;
  }

  typename AtomicInternal::ChooseAtomic<T, sizeof(T)>::type mValue;
};

MATHICGB_NAMESPACE_END

#endif
