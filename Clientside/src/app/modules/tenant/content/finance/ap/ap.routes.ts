import { Routes } from '@angular/router';

export const AP_ROUTES: Routes = [
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
];