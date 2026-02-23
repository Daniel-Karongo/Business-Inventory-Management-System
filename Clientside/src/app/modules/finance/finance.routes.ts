import { Routes } from '@angular/router';
import { financeRoleGuard } from '../auth/guards/finance-role.guard';

export const FINANCE_ROUTES: Routes = [
  {
    path: '',
    canActivate: [financeRoleGuard],
    children: [
      {
        path: 'accounting',
        loadChildren: () =>
          import('../accounts/accounts.routes')
            .then(m => m.ACCOUNTS_ROUTES)
      },
      {
        path: 'budgeting',
        loadChildren: () =>
          import('./budgeting/budgeting.routes')
            .then(m => m.BUDGETING_ROUTES)
      },
      {
        path: 'tax',
        loadChildren: () =>
          import('./tax/tax.routes')
            .then(m => m.TAX_ROUTES)
      },
      {
        path: 'payments',   // ✅ NEW
        loadChildren: () =>
          import('./payments/payments.routes')
            .then(m => m.PAYMENTS_ROUTES)
      }
    ]
  }
];