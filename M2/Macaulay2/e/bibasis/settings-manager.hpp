/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef M2_BIBASIS_SETTINGS_MANAGER_HPP
#define M2_BIBASIS_SETTINGS_MANAGER_HPP

#include "bibasis/monom.hpp"

namespace BIBasis
{
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
