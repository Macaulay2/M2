// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef M2TBB_HPP
#define M2TBB_HPP

#define TBB_PRESENT




/// A compatibility layer for tbb. If we are compiling with tbb present, then
/// these classes will simply be the same classes as in tbb. However, if we
/// are compiling without tbb (so without parallelism), then these classes will
/// be trivial non-parallel implementations that allows MathicGB to work
/// without tbb being present. TBB doesn't work on Cygwin, so that is at least
/// one good reason to have this compatibility layer. This only works if all
/// uses of tbb go through m2tbb, so make sure to do that.

#ifdef TBB_PRESENT
#include <tbb/version.h>

#define XSTR(x) STR(x)
#define STR(x) #x

//#define TBB_VERSION_MAJOR 2020
#pragma message "TBB_VERSION_MAJOR = " XSTR(TBB_VERSION_MAJOR)

#if 1   //TBB_VERSION_MAJOR >= 2021

#include <tbb/enumerable_thread_specific.h>
#include <tbb/concurrent_unordered_map.h>     
#include <tbb/queuing_mutex.h>                // for queuing_mutex
#include <tbb/null_mutex.h>                   // for null_mutex
#include <tbb/parallel_for_each.h>                  // for parallel_do_feeder
#include <tbb/tick_count.h>                   // for tick_count
#include <tbb/parallel_sort.h>                // for parallel_sort
#include <tbb/parallel_for.h>                 // for parallel_for
#include <mutex>

namespace m2tbb {
  //using task_scheduler_init        = ::tbb::task_scheduler_init;
  using ::std::mutex;
  using ::tbb::queuing_mutex;
  using ::tbb::null_mutex;
  using ::tbb::parallel_for_each;
  using ::tbb::parallel_for;
  using ::tbb::parallel_sort;
  using ::tbb::blocked_range;
  using ::tbb::tick_count;
  using ::tbb::concurrent_unordered_map;
  using ::tbb::feeder;
  using ::tbb::enumerable_thread_specific;
}

// as new as 2021 section

#else // tbb present, but 2020 or older

// include those tbb files that we are using here.  Don't do a blanket tbb include.
// TODO
#include <tbb/enumerable_thread_specific.h>
#include <tbb/concurrent_unordered_map.h>     
#include <tbb/queuing_mutex.h>                // for queuing_mutex
#include <tbb/null_mutex.h>                   // for null_mutex
#include <tbb/parallel_do.h>                  // for parallel_do_feeder
#include <tbb/tick_count.h>                   // for tick_count
#include <tbb/parallel_sort.h>                // for parallel_sort
#include <tbb/parallel_for.h>                 // for parallel_for
#include <mutex>

namespace m2tbb {
  //using task_scheduler_init        = ::tbb::task_scheduler_init;
  using ::std::mutex;
  using ::tbb::queuing_mutex;
  using ::tbb::null_mutex;
  using ::tbb::parallel_do;
  using ::tbb::parallel_for;
  using ::tbb::parallel_sort;
  using ::tbb::blocked_range;
  using ::tbb::tick_count;
  using ::tbb::concurrent_unordered_map;
  using ::tbb::parallel_do_feeder;
  using ::tbb::enumerable_thread_specific;
}

// #define parallel_do parallel_for_each
// #define parallel_do_feeder feeder
// parallel_do -> parallel_for_each
// parallel_do_feeder -> feeder

#endif // TBB_VERSION_MAJOR

#else // TBB not present
// below is an interface to serial versions of the above code.

#include <functional>
#include <vector>
#include <ctime>
#include <algorithm>
#include <chrono>

namespace m2tbb {
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

#endif

#endif
