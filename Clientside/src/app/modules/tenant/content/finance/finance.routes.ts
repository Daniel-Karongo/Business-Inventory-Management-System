import { Routes } from '@angular/router';
import { roleAtLeast } from '../../../../core/security/role-at-least.guard';

export const FINANCE_ROUTES: Routes = [
  {
    path: '',
    canMatch: [roleAtLeast('MANAGER')],
    children: [
      {
        path: '',
        pathMatch: 'full',
        redirectTo: 'accounting'
      },
      {
        path: 'accounting',
        loadChildren: () =>
          import('./accounts/accounts.routes')
            .then(m => m.ACCOUNTS_ROUTES)
      },
      {
        path: 'tax',
        loadChildren: () =>
          import('./tax/tax.routes')
            .then(m => m.TAX_ROUTES)
      },
      {
        path: 'ap',
        loadChildren: () =>
          import('./ap/ap.routes')
            .then(m => m.AP_ROUTES)
      },
      {
        path: 'expenses',
        loadChildren: () =>
          import('./expenses/expense.routes')
            .then(m => m.EXPENSE_ROUTES)
      }
    ]
  }
];