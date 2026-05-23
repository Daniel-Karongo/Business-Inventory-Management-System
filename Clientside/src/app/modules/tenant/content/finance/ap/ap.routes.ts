import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../../core/security/role-at-least.guard';

export const AP_ROUTES: Routes = [

    {
        path: '',
        canMatch: [roleAtLeast('MANAGER')],
        children: [

            {
                path: '',
                pathMatch: 'full',
                redirectTo: 'debts'
            },

            {
                path: 'debts',
                loadComponent: () =>
                    import('./debts/pages/supplier-debt-list/supplier-debt-list.component')
                        .then(m => m.SupplierDebtListComponent)
            },

            {
                path: 'debts/:supplierId',
                loadComponent: () =>
                    import('./debts/pages/supplier-workspace/supplier-workspace.component')
                        .then(m => m.SupplierWorkspaceComponent)
            }
        ]
    }
];