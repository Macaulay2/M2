#ifndef MATHIC_COMPARER_GUARD
#define MATHIC_COMPARER_GUARD

namespace mathic {
  template<class C>
    class Comparer {
  public:
  Comparer(const C& conf): _conf(conf) {}
  template<class A, class B>
  bool operator()(const A& a, const B& b) const {
    return _conf.isLessThan(a.get(), b.get());
  }
  private:
    const C& _conf;
  };
}

#endif
