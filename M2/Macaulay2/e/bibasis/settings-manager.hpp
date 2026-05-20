/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_SETTINGS_MANAGER_HPP
#define BIBASIS_SETTINGS_MANAGER_HPP

/**
 * @file bibasis/settings-manager.hpp
 * @brief `BIBasis::SettingsManager` --- singleton holding the per-run monomial order and variable count.
 *
 * Declares the lazy singleton (accessed via the free function
 * `GetSettingsManager()`) that carries the subsystem-wide
 * parameters every BIBasis class needs to consult: the active
 * monomial order (`Monom::Lex` / `Monom::DegLex` /
 * `Monom::DegRevLex`) and, via `SetNumberOfVariables`, the
 * static `Monom::DimIndepend` that fixes the size of every
 * monomial's variable list. `Launcher` populates these once at
 * the start of a run; the algorithm code reads them through the
 * singleton instead of threading them through every constructor.
 *
 * Constructors, copy-constructor, and assignment are private and
 * `GetSettingsManager` is the only friend, enforcing the single-
 * instance contract --- BIBasis runs are single-threaded per
 * `rawBIBasis` call, so the shared mutable state is safe.
 *
 * @see monom.hpp
 * @see launcher.hpp
 */

#include "monom.hpp"

namespace BIBasis
{
    /**
     * @brief Process-wide singleton holding the BIBasis monomial order and
     * variable count.
     *
     * @details Hidden constructor / copy disabled --- the only way to reach
     * an instance is through the free function `GetSettingsManager()`,
     * which is declared a `friend`. Stores `MonomialOrder`
     * (`Lex` / `DegLex` / `DegRevLex`) so the BIBasis monom
     * subclasses can consult it before doing comparisons, and
     * forwards `SetNumberOfVariables` to `Monom`'s static
     * `DimIndepend` slot.
     */
    class SettingsManager
    {
    private:
        Monom::Order MonomialOrder;

    public:
        ~SettingsManager();

        Monom::Order GetMonomialOrder() const;
        void SetMonomialOrder(Monom::Order order);
        void SetNumberOfVariables(Monom::Integer numberOfVariables);

    private:
        SettingsManager();
        SettingsManager(const SettingsManager&);
        SettingsManager& operator=(const SettingsManager&);

        friend SettingsManager& GetSettingsManager();
    };

    SettingsManager& GetSettingsManager();

    inline Monom::Order SettingsManager::GetMonomialOrder() const
    {
        return MonomialOrder;
    }
}

#endif // BIBASIS_SETTINGS_MANAGER_HPP
