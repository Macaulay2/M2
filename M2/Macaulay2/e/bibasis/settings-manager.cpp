/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include "settings-manager.hpp"

namespace BIBasis
{
    SettingsManager::SettingsManager()
        : MonomialOrder(Monom::DegRevLex)
    {
    }

    SettingsManager::~SettingsManager()
    {
    }

    void SettingsManager::SetMonomialOrder(Monom::Order order)
    {
        MonomialOrder = order;
    }
    
    void SettingsManager::SetNumberOfVariables(Monom::Integer numberOfVariables)
    {
        Monom::SetDimIndepend(numberOfVariables);
    }

    SettingsManager& GetSettingsManager()
    {
        static SettingsManager manager = SettingsManager();
        return manager;
    }
}
