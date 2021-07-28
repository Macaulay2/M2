// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_M_TBB_GUARD
#define MATHICGB_M_TBB_GUARD

#ifndef MATHICGB_NO_TBB
#define TBB_SUPPRESS_DEPRECATED_MESSAGES 1
#include <tbb/tbb.h>

MATHICGB_NAMESPACE_BEGIN

/// A compatibility layer for tbb. If we are compiling with tbb present, then
/// these classes will simply be the same classes as in tbb. However, if we
/// are compiling without tbb (so without parallelism), then these classes will
/// be trivial non-parallel implementations that allows MathicGB to work
/// without tbb being present. TBB doesn't work on Cygwin, so that is at least
/// one good reason to have this compatibility layer. This only works if all
/// uses of tbb go through mtbb, so make sure to do that.

namespace mtbb {
  using ::tbb::task_scheduler_init;
  using ::tbb::mutex;
  using ::tbb::parallel_do_feeder;
  using ::tbb::enumerable_thread_specific;
  using ::tbb::parallel_do;
  using ::tbb::parallel_for;
  using ::tbb::parallel_sort;
  using ::tbb::blocked_range;
  using ::tbb::tick_count;
}

MATHICGB_NAMESPACE_END
#else

#include <functional>
#include <vector>
#include <ctime>
#include <algorithm>
#include <chrono>

MATHICGB_NAMESPACE_BEGIN

namespace mtbb {
  class task_scheduler_init {
  public:
    task_scheduler_init(int) {}
    static const int automatic = 1;
  };

  class mutex {
  public:
    mutex(): mLocked(false) {}

    class scoped_lock {
    public:
      scoped_lock(): mMutex(0) {}
      scoped_lock(mutex& m): mMutex(&m) {mMutex->lock();}
      ~scoped_lock() {
        if (mMutex != 0)
          release();
      }

      void acquire(mutex& m) {
        MATHICGB_ASSERT(mMutex == 0);
        mMutex = &m;
      }

      bool try_acquire(mutex& m) {
        MATHICGB_ASSERT(mMutex == 0);
        if (!m.try_lock())
          return false;
        mMutex = &m;
        return true;
      }

      void release() {
        MATHICGB_ASSERT(mMutex != 0);
        mMutex->unlock();
        mMutex = 0;
      }

    private:
      mutex* mMutex;
    };

    void lock() {
      MATHICGB_ASSERT(!mLocked); // deadlock
      mLocked = true;
    }

    bool try_lock() {
      if (mLocked)
        return false;
      lock();
      return true;
    }

    void unlock() {
      MATHICGB_ASSERT(mLocked);
      mLocked = false;
    }

  private:
    bool mLocked;
  };

  template<class T>
  class enumerable_thread_specific {
  public:
    template<class Op>
    enumerable_thread_specific(Op&& creater): mCreater(creater) {}

    bool empty() const {return mObj.get() == 0;}

    T& local() {
      if (empty())
        mObj = make_unique<T>(mCreater());
      MATHICGB_ASSERT(!empty());
      return *mObj;
    }

    T* begin() {
      if (empty())
        return 0;
      else
        return mObj.get();
    }

    T* end() {
      if (empty())
        return  0;
      else
        return begin() + 1;
    }

    void clear() {
      mObj.reset(0);
    }

  private:
    std::function<T()> mCreater;
    std::unique_ptr<T> mObj;
  };

  template<class Value>
  class blocked_range {
  public:
    typedef size_t size_type;
    typedef Value const_iterator;

    blocked_range(Value begin, Value end, size_t grainSize = 1):
      mBegin(begin), mEnd(end), mGrainSize(grainSize) {}

    size_type size() const {return end() - begin();}
    bool empty() const {return mBegin == mEnd;}
    size_type grainsize() const {return mGrainSize;}
    bool is_divisible() const {return false;}

    const_iterator begin() const {return mBegin;}
    const_iterator end() const {return mEnd;}

  private:
    const_iterator mBegin;
    const_iterator mEnd;
    size_type mGrainSize;
  };
    
  template<class Range, class Func>
  void parallel_for(Range&& range, Func&& f) {
    f(range);
  }

  template<class Index, class Func>
  void parallel_for(Index begin, Index end, Index step, Func&& f) {
    for (auto i = begin; i < end; i += step)
      f(i);
  }

  template<class T>
  class parallel_do_feeder {
  public:
    parallel_do_feeder(std::vector<T>& tasks): mTasks(tasks) {}

    template<class TT>
    void add(TT&& t) {mTasks.push_back(std::forward<TT>(t));}

  private:
    std::vector<T>& mTasks;
  };

  template<class InputIterator, class Body>
  void parallel_do(InputIterator begin, InputIterator end, Body body) {
    typedef typename std::remove_reference<decltype(*begin)>::type Task;
    std::vector<Task> tasks;
    parallel_do_feeder<Task> feeder(tasks);
    for (; begin != end; ++begin) {
      tasks.push_back(*begin);
      while (!tasks.empty()) {
        auto task = std::move(tasks.back());
        tasks.pop_back();
        body(task, feeder);
      }
    }
  }

  template<class It, class Pred>
  void parallel_sort(It begin, It end, Pred&& pred) {
    std::sort(begin, end, pred);
  }

  class tick_count {
  private:
    // This really should be std::chrono::steady_clock, but GCC 4.5.3 doesn't
    // have that.
    typedef std::chrono::system_clock clock;

  public:
    tick_count(): mTime() {}

    static tick_count now() {
      tick_count t;
      t.mTime = clock::now();
      return t;
    }

    class interval_t {
    public:
      interval_t(double seconds): mSeconds(seconds) {}

      double seconds() const {return mSeconds;}

    private:
      const double mSeconds;
    };

    interval_t operator-(const tick_count t) const {
      typedef std::chrono::duration<double> SecondDuration;
      const auto duration =
        std::chrono::duration_cast<SecondDuration>(mTime - t.mTime);
      return duration.count();
    }

  private:
    clock::time_point mTime;
  };
}


MATHICGB_NAMESPACE_END
#endif

#endif
