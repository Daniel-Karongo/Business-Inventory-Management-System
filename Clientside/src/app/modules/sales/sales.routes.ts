import { Routes } from '@angular/router';

export const SALES_ROUTES: Routes = [
  {
    path: '',
    loadComponent: () =>
      import('./pages/sales-list/sales-list.component')
        .then(m => m.SalesListComponent)
  },
  {
    path: 'new',
    loadComponent: () =>
      import('./pages/sale-create/sale-create.component')
        .then(m => m.SaleCreateComponent)
  },
  {
    path: ':id',
    loadComponent: () =>
      import('./pages/sale-details/sale-details.component')
        .then(m => m.SaleDetailsComponent)
  },
  {
    path: ':id/receipt',
    loadComponent: () =>
      import('./pages/sale-receipt/sale-receipt.component')
        .then(m => m.SaleReceiptComponent)
  }
];