import { Routes } from '@angular/router';

import { roleAtLeast }
  from '../../../../../core/security/role-at-least.guard';

import { InventoryTransactionsComponent }
  from './pages/inventory-transactions/inventory-transactions.component';

export const INVENTORY_ROUTES: Routes = [

  {
    path: '',
    redirectTo: '/app/stock',
    pathMatch: 'full'
  },

  {
    path: ':variantId',

    canMatch: [
      roleAtLeast('EMPLOYEE')
    ],

    component:
      InventoryTransactionsComponent
  }
];