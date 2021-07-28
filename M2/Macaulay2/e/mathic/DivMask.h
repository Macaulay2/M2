#ifndef MATHIC_BIT_MASK_GUARD
#define MATHIC_BIT_MASK_GUARD

#include "stdinc.h"
#include <vector>
#include <utility>
#include <algorithm>

// Value x means do 2^x mask checks before printing stats
//#define MATHIC_TRACK_DIV_MASK_HIT_RATIO 25
//#define MATHIC_TRACK_DIV_MASK_HIT_RATIO 30
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
#include <iostream>
#include "ColumnPrinter.h"
#endif

namespace mathic {
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
  namespace DivMaskStats {
    extern unsigned long maskComputes; // div masks computed
    extern unsigned long maskChecks; // how many times mask is checked
    extern unsigned long maskHits; // times canDivide returns false
    extern unsigned long divChecks; // times divisibility is checked with mask
    extern unsigned long divDivides; // mask can't hit as there is divisibility
    extern unsigned long divHits; // times mask rules out divisibility
  }
#endif

  /** Class representing a div mask. This is a set of bits that can
      be used to determine that one monomial cannot divide another
      monomial. */
  class DivMask {
  public:
    /** Calculates div masks. Don't change NullCalculator
        from its default value. The actual code are in partial specializations
        selecting the right version based on NullCalculator. */
    template<class Configuration,
      bool NullCalculator = Configuration::UseDivMask>
      class Calculator;

  DivMask(): _mask(0) {}
    static DivMask getMaxMask() {return ~static_cast<MaskType>(0);}

    template<class T, class Configuration>
      DivMask(const T& t,
              const Calculator<Configuration>& calc, const Configuration& conf):
    _mask(calc.compute(t, conf)) {}

    template<class T, class Configuration>
    void recalculate(const T& t,
                     const Calculator<Configuration>& calc,
                     const Configuration& conf) {
      _mask = calc.compute(t, conf);
    }

    bool canDivide(const DivMask& mask) const {
      const bool canDiv = ((_mask & ~mask._mask) == 0);
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
      ++DivMaskStats::maskChecks;
      if (!canDiv)
        ++DivMaskStats::maskHits;

      // print stats every 2^MATHIC_TRACK_DIV_MASK_HIT_RATIO time
      const unsigned long mod = (1 << MATHIC_TRACK_DIV_MASK_HIT_RATIO) - 1;
      if ((DivMaskStats::maskChecks & mod) == 0) {
        std::cerr << "**** DivMask stats (turn off by not defining macro) ****\n";
        ColumnPrinter pr;
        pr.addColumn(true, "* ");
        pr.addColumn(false, " ");
        pr.addColumn(false, "  ");
        pr.addColumn(true, " ");
        pr[0] << "masks computed:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::maskComputes) << '\n';
        pr[2] << ColumnPrinter::percent
          (DivMaskStats::maskComputes, DivMaskStats::maskChecks) << '\n';
        pr[3] << "of mask checks\n";

        pr[0] << "mask checks:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::maskChecks) << '\n';
        pr[2] << '\n';
        pr[3] << '\n';

        pr[0] << "mask hits:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::maskHits) << '\n';
        pr[2] << ColumnPrinter::percent
          (DivMaskStats::maskHits, DivMaskStats::maskChecks) << '\n';
        pr[3] << "of mask checks\n";

        pr[0] << "mask div checks:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::divChecks) << '\n';
        pr[2] << ColumnPrinter::percent
          (DivMaskStats::divChecks, DivMaskStats::maskChecks) << '\n';
        pr[3] << "of mask checks\n";

        pr[0] << "actually divide:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::divDivides) << '\n';
        pr[2] << ColumnPrinter::percent
          (DivMaskStats::divDivides, DivMaskStats::divChecks) << '\n';
        pr[3] << "of div checks\n";

        pr[0] << "mask div hits:\n";
        pr[1] << ColumnPrinter::commafy(DivMaskStats::divHits) << '\n';
        pr[2] << ColumnPrinter::percent
          (DivMaskStats::divHits, DivMaskStats::divChecks) << '\n';
        pr[3] << "of div checks, " << ColumnPrinter::percent
          (DivMaskStats::divHits,
           DivMaskStats::divChecks - DivMaskStats::divDivides)
           << " adjusted.\n";          
        std::cerr << pr << "****\n";
      }
#endif

      return canDiv;
    }

    void combineAnd(const DivMask& mask) {_mask &= mask._mask;}

    bool operator==(DivMask& mask) const {return _mask == mask._mask;}
    bool operator!=(DivMask& mask) const {return _mask != mask._mask;}

    /** Extender extends T with a div mask if UseDivMask is true. It is
        allowed for T to be a reference type or const. */
    template<class T, bool UseDivMask>
    class Extender;

    /** Base class to include a DivMask into a class at compile time
        based on the template parameter UseDivMask. The class offers
        the same methods either way, but they are replaced by do-nothing
        or asserting versions if UseDivMask is false. */
    template<bool UseDivMask>
    class HasDivMask;

  protected:
    template<class C>
    class ExponentComparer {
    public:
      ExponentComparer(size_t var, const C& conf): _var(var), _conf(conf) {}
      template<class E>
      bool operator()(const E& a, const E& b) const {
        return _conf.getExponent(a, _var) < _conf.getExponent(b, _var);
      }
    private:
      size_t _var;
      const C& _conf;
    };

    struct VarData {
      size_t var; // the variable in question

      // let e be the average of the minimum and maximum exponent of var.
      // then split is the number of elements whose exponent of var is
      // <= average, or it is the number of elements whose exponent of var
      // is > average, whichever is those two numbers is smaller.
      size_t split;

      // How many bits of the DivMask to dedicate to this variable.
      size_t bitsForVar;

      bool operator<(const VarData& data) const {
        return split > data.split; // larger split first in order
      }
    };

    typedef unsigned int MaskType;
    DivMask(MaskType mask): _mask(mask) {}

  private:
    /** To eliminate warnings about T& if T is already a reference type. */
    template<class T> struct Ref {typedef T& RefType;};
    template<class T> struct Ref<T&> {typedef T& RefType;};
    MaskType _mask;
  };

  template<class C>
    class DivMask::Calculator<C, true> {
  public:
    Calculator(const C& conf) {rebuildDefault(conf);}

    /** Change the meaning of the bits in the div masks produced by this
        object to something that will likely work well for entries
        in [begin, end). All div masks will have to be recomputed
        after this. Mixing div masks computed before a call to
        rebuild() with ones after has unpredictable results. */
    template<class Iter>
    void rebuild(Iter begin, Iter end, const C& conf);

    /** Rebuilds without the benefit of knowing a range of entries
        that the div masks are supposed to work well for. */
    void rebuildDefault(const C& conf);

    /** Computes a div mask for t. */
    template<class T>
    DivMask::MaskType compute(const T& t, const C& conf) const;

  private:
    typedef typename C::Exponent Exponent;
    /** If entry at index i is the pair (var,exp) then the bit at
        index var in a bit mask is 1 if the exponent of var in the
        monomial is strictly greater than exp. */
    typedef std::vector<std::pair<size_t, Exponent> > BitContainer;
    BitContainer _bits;
  };

  template<class C>
  template<class Iter>
  void DivMask::Calculator<C, true>::
  rebuild(Iter begin, Iter end, const C& conf) {
    const size_t size = std::distance(begin, end);
    if (size == 0) {
      rebuildDefault(conf);
      return;
    }

    _bits.clear();
    const size_t varCount = conf.getVarCount();
    const size_t TotalBits = sizeof(MaskType) * BitsPerByte;

    // ** Determine information about each variable
    std::vector<VarData> datas;
    for (size_t var = 0; var < varCount; ++var) {
      Exponent min = conf.getExponent(*begin, 0);
      Exponent max = min;
      for (Iter it = begin; it != end; ++it) {
        Exponent exp = conf.getExponent(*it, var);
        if (max < exp)
          max = exp;
        if (exp < min)
          min = exp;
      }
      Exponent average = min + (max - min) / 2; // this formula avoids overflow
      size_t split = 0;
      for (Iter it = begin; it != end; ++it)
        if (conf.getExponent(*it, var) < average)
          ++split;
      if (split > size / 2)
        split = size - split;
      VarData data;
      data.var = var;
      data.split = split;
      datas.push_back(data);
    }
    std::sort(datas.begin(), datas.end());
    MATHIC_ASSERT(datas.size() == varCount);

    // distribute bits to variables according to the data collected
    std::vector<size_t> bitsForVars(varCount);
    for (size_t i = 0; i < varCount; ++i) {
      const size_t var = datas[i].var;
      bitsForVars[var] = TotalBits / varCount;
      if (i < TotalBits % varCount)
        ++bitsForVars[var];
    }

    // calculate the meaning of each bit and put it in _bits
    for (size_t var = 0; var < varCount; ++var) {
      const size_t bitsForVar = bitsForVars[var];
      if (bitsForVar == 0)
        continue;

      const bool useRank = false;
      if (useRank) {
        std::sort(begin, end, ExponentComparer<C>(var, conf));
        size_t size = std::distance(begin, end);
        size_t offset = size / (bitsForVar + 1);
        if (offset== 0)
          offset = 1;
        Exponent lastExp;
        size_t lastJ = 0;
        for (size_t i = 1; i <= bitsForVar; ++i) {
          size_t j = i * offset;
          if (i > 1 && j < lastJ)
            j = lastJ + 1;
          if (j >= size)
            j = size - 1;
          if (i > 1) {
            while (j < size && conf.getExponent(begin[j], var) == lastExp)
              ++j;
            if (j == size)
              break;
          }
          _bits.push_back(
            std::make_pair(var, conf.getExponent(begin[j], var)));
          lastJ = j;
          lastExp = conf.getExponent(begin[j], var);
          if (j == size - 1)
            break;
        }
      } else {
        Exponent min = conf.getExponent(*begin, 0);
        Exponent max = min;
        for (Iter it = begin; it != end; ++it) {
          Exponent exp = conf.getExponent(*it, var);
          if (max < exp)
            max = exp;
          if (exp < min)
            min = exp;
        }

        // divide the range [a,b] into bitsForVar equal pieces
        // and use the left end points of those ranges
        // as the points for the bits.
        Exponent increment = (max - min) / static_cast<Exponent>(bitsForVar); // todo: can avoid cast?
        if (increment == 0)
          increment = 1;
        for (size_t i = 0; i < bitsForVar; ++i)
          _bits.push_back(std::make_pair(var, min + increment * static_cast<Exponent>(i)));
           // todo: can avoid cast?
      }
    }
  }

  template<class C>
    void DivMask::Calculator<C, true>::
    rebuildDefault(const C& conf) {
    _bits.clear();
    const size_t varCount = conf.getVarCount();
    const size_t TotalBits = sizeof(MaskType) * BitsPerByte;
    for (size_t var = 0; var < varCount; ++var) {
      const size_t bitsForVar =
        TotalBits / varCount + (var < TotalBits % varCount);
      Exponent exp = 0;
      for (size_t i = 0; i < bitsForVar; ++i) {
        _bits.push_back(std::make_pair(var, exp));
        exp = (i == 0 ? 1 : exp * 2);
      }
    }
  }

  template<class C>
  template<class T>
  DivMask::MaskType DivMask::Calculator<C, true>::
    compute(const T& t, const C& conf) const {
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
      ++DivMaskStats::maskComputes;
#endif
    typedef typename BitContainer::const_iterator const_iterator;
    const const_iterator end = _bits.end();
    DivMask::MaskType mask = 0;
    for (const_iterator it = _bits.begin(); it != end; ++it)
      mask = (mask << 1) | (conf.getExponent(t, it->first) > it->second);   
    return mask;
  }

  template<class C>
    class DivMask::Calculator<C, false> {
  public:
    Calculator(const C& conf) {}

    template<class Iter>
    void rebuild(Iter begin, Iter end, const C& conf) {}
    void rebuildDefault(const C& conf) {}
  };

  template<>
  class DivMask::HasDivMask<true> {
  public:
    template<class T, class C>
      HasDivMask(const T& t, const Calculator<C>& calc, const C& conf):
    _mask(t, calc, conf) {}
    HasDivMask() {resetDivMask();}

    DivMask& getDivMask() {return _mask;}
    const DivMask& getDivMask() const {return _mask;}
    void resetDivMask() {_mask = DivMask::getMaxMask();}
    bool canDivide(const HasDivMask<true>& t) const {
      return getDivMask().canDivide(t.getDivMask());
    }

    void updateToLowerBound(const HasDivMask<true>& t) {
      _mask.combineAnd(t.getDivMask());
    }

  protected:
    template<class T, class C>
      void recalculateDivMask
      (const T& t, const Calculator<C>& calc, const C& conf) {
      _mask.recalculate(t, calc, conf);
    }

  private:
    DivMask _mask;
  };

  template<>
  class DivMask::HasDivMask<false> {
  public:
    void resetDivMask() {MATHIC_ASSERT(false);}
    DivMask getDivMask() const {MATHIC_ASSERT(false); return DivMask();}
    bool canDivide(const HasDivMask<false>& t) const {return true;}
    template<bool B>
    void updateToLowerBound(const HasDivMask<B>& entry) {}
  };

  template<class T>
  class DivMask::Extender<T, true> : public HasDivMask<true> {
  private:
    typedef typename Ref<T>::RefType Reference;
    typedef typename Ref<const T>::RefType ConstReference;
  public:
  Extender(): HasDivMask<true>(), _t() {}
    template<class C>
    Extender(ConstReference t, const Calculator<C>& calc, const C& conf):
    HasDivMask<true>(t, calc, conf), _t(t) {}

    template<class S, class C>
    bool divides(const Extender<S, true>& t, const C& conf) const {
      bool canDiv = this->canDivide(t);
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
      ++DivMaskStats::divChecks;
      if (!canDiv)
        ++DivMaskStats::divHits;
#endif
      if (!canDiv)
        return false;

      bool actuallyDivides = conf.divides(get(), t.get());
#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
      if (actuallyDivides)
        ++DivMaskStats::divDivides;
#endif
      return actuallyDivides;
    }

    template<class C>
      void recalculateDivMask(const Calculator<C>& calc, const C& conf) {
      this->HasDivMask<true>::recalculateDivMask(get(), calc, conf);
    }

    Reference get() {return _t;}
    ConstReference get() const {return _t;}

  private:
    T _t;
  };

  template<class T>
    class DivMask::Extender<T, false> : public HasDivMask<false> {
  private:
    typedef typename Ref<T>::RefType Reference;
    typedef typename Ref<const T>::RefType ConstReference;
  public:
  Extender(): HasDivMask<false>(), _t() {}
    template<class C>
      Extender(ConstReference t, const Calculator<C>& calc, const C& conf):
    HasDivMask<false>(), _t(t) {}

    template<class S, class C>
      bool divides(const Extender<S, false>& t, const C& conf) const {
      return conf.divides(get(), t.get());
    }

    template<class C>
      void recalculateDivMask(const Calculator<C>& calc, const C& conf) {}

    Reference get() {return _t;}
    ConstReference get() const {return _t;}

  private:
    T _t;
  };
}

#endif
