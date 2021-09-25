#ifndef MATHIC_NAME_FACTORY_GUARD
#define MATHIC_NAME_FACTORY_GUARD

#include "stdinc.h"
#include "error.h"
#include <vector>
#include <string>
#include <algorithm>
#include <memory>

namespace mathic {
  /** A NameFactory takes a name and then creates an instance of a class
   that has been previously registered under that name. This is done
   in a general way using templates.

   None of this is very efficient currently. However, the interface can be
   implemented much more efficiently if that becomes necessary.
  */
  template<class AbstractProduct>
  class NameFactory {
   public:
    /** @param abstractName The name for those things that are being
     generated in general. Used for error messages. */
    NameFactory(const char* abstractName): _abstractName(abstractName) {}

    typedef std::unique_ptr<AbstractProduct> (*FactoryFunction)();
    void registerProduct(const std::string& name, FactoryFunction function);

    /** Calls the function registered to the parameter name and returns
     the result. Returns null if name has not been registered. */
    std::unique_ptr<AbstractProduct>
      createNullOnUnknown(const std::string& name) const;

    /** Calls the function registered to the parameter name and returns
     the result. Throws an exception if name has not been registered. */
    std::unique_ptr<AbstractProduct> create(const std::string& name) const;

    /** Inserts into names all registered names that have the indicated
     prefix in lexicographic increasing order. */
    void namesWithPrefix
      (const std::string& prefix, std::vector<std::string>& names) const;

    /** Returns the name of the kinds of things being created. */
    std::string abstractProductName() const;

    /** Returns true if no names have been registered. */
    bool empty() const;

   private:
    typedef std::pair<std::string, FactoryFunction> Pair;
    typedef typename std::vector<Pair>::const_iterator const_iterator;
    std::vector<Pair> _pairs;
    const std::string _abstractName;
  };

  /** Registers the given name to a function that
    default-constructs a ConcreteProduct. */
  template<class ConcreteProduct, class AbstractProduct>
  void nameFactoryRegister
    (NameFactory<AbstractProduct>& factory, const std::string& name);

  /** Registers the name ConcreteProduct::staticName() to a function that
    default-constructs a ConcreteProduct. */
  template<class ConcreteProduct, class AbstractProduct>
  void nameFactoryRegister
    (NameFactory<AbstractProduct>& factory);

  /** Registers the string returned by ConcreteProduct::staticName()
   to a function that default-constructs a ConcreteProduct. */
  template<class ConcreteProduct, class AbstractProduct>
  void nameFactoryRegister(NameFactory<AbstractProduct>& factory);

  /** Creates the unique product that has the indicated prefix, or
   creates the actual product that has name equal to the indicated
   prefix. Exceptions thrown are as for uniqueNamesWithPrefix(). */
  template<class AbstractProduct>
  std::unique_ptr<AbstractProduct> createWithPrefix
  (const NameFactory<AbstractProduct>& factory, const std::string& prefix);

  /** Returns the unique product name that has the indicated prefix, or
   return prefix itself if it is the actual name of a product.

   @exception UnknownNameException If no product has the indicated
   prefix.

   @exception AmbiguousNameException If more than one product has the
   indicated prefix and the prefix is not the actual name of any
   product. */
  template<class AbstractProduct>
  std::string uniqueNameWithPrefix
  (const NameFactory<AbstractProduct>& factory, const std::string& prefix);


  // **************************************************************
  // These implementations have to be included here due
  // to being templates.

  template<class AbstractProduct>
  std::unique_ptr<AbstractProduct> NameFactory<AbstractProduct>::
  createNullOnUnknown(const std::string& name) const {
    for (const_iterator it = _pairs.begin(); it != _pairs.end(); ++it)
      if (it->first == name)
        return it->second();
    return std::unique_ptr<AbstractProduct>();
  }

  template<class AbstractProduct>
  std::unique_ptr<AbstractProduct> NameFactory<AbstractProduct>::
  create(const std::string& name) const {
    std::unique_ptr<AbstractProduct> product = createNullOnUnknown(name);
    if (product.get() == 0)
      throwError<UnknownNameException>(
        "Unknown " + abstractProductName() + " \"" + name + "\".");
    return product;
  }

  template<class AbstractProduct>
  void NameFactory<AbstractProduct>::
  registerProduct(const std::string& name, FactoryFunction function) {
    MATHIC_ASSERT(createNullOnUnknown(name).get() == 0); // no duplicate names
    _pairs.push_back(Pair(name, function));
  }

  template<class AbstractProduct>
  void NameFactory<AbstractProduct>::namesWithPrefix(
    const std::string& prefix,
    std::vector<std::string>& names
  ) const {
    for (const_iterator it = _pairs.begin(); it != _pairs.end(); ++it)
      if (it->first.compare(0, prefix.size(), prefix) == 0)
        names.push_back(it->first);
    std::sort(names.begin(), names.end());
  }

  template<class AbstractProduct>
  bool NameFactory<AbstractProduct>::empty() const {
    return _pairs.empty();
  }

  template<class AbstractProduct>
  std::string NameFactory<AbstractProduct>::abstractProductName() const {
    return _abstractName;
  }

  template<class ConcreteProduct, class AbstractProduct>
  void nameFactoryRegister(NameFactory<AbstractProduct>& factory) {
    nameFactoryRegister<ConcreteProduct, AbstractProduct>
      (factory, ConcreteProduct::staticName());
  }

  template<class ConcreteProduct, class AbstractProduct>
  void nameFactoryRegister
    (NameFactory<AbstractProduct>& factory, const std::string& name) {
    // work-around for no local functions in old C++
    struct HoldsFunction {
      static std::unique_ptr<AbstractProduct> createConcreteProduct() {
        return std::unique_ptr<AbstractProduct>(new ConcreteProduct());
      }
    };
    factory.registerProduct(name, HoldsFunction::createConcreteProduct);
  }

  template<class AbstractProduct>
  std::unique_ptr<AbstractProduct> createWithPrefix
  (const NameFactory<AbstractProduct>& factory, const std::string& prefix) {
    return factory.createNullOnUnknown(uniqueNameWithPrefix(factory, prefix));
  }

  template<class AbstractProduct>
  std::string uniqueNameWithPrefix
  (const NameFactory<AbstractProduct>& factory, const std::string& prefix) {
    std::vector<std::string> names;
    factory.namesWithPrefix(prefix, names);

    // if exact string found, then use that one even if there are other
    // prefix matches.
    if (std::find(names.begin(), names.end(), prefix) != names.end()) {
      names.clear();
      names.push_back(prefix);
    }

    if (names.empty()) {
      throwError<UnknownNameException>
        ("No " + factory.abstractProductName() +
         " has the prefix \"" + prefix + "\".");
    }

    if (names.size() >= 2) {
      std::string errorMsg = "More than one " + factory.abstractProductName() +
        " has prefix \"" + prefix + "\":\n ";
      for (size_t name = 0; name < names.size(); ++name)
        errorMsg += ' ' + names[name];
      throwError<AmbiguousNameException>(errorMsg);
    }

    MATHIC_ASSERT(names.size() == 1);
    return names.back();
  }
}

#endif
