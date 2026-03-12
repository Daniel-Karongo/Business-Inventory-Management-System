import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { InventoryListComponent } from './pages/inventory-list/inventory-list.component';
import { InventoryTransactionsComponent } from './pages/inventory-transactions/inventory-transactions.component';

export const INVENTORY_ROUTES: Routes = [
{
  path: '',
  canMatch: [roleAtLeast('EMPLOYEE')],
  children: [
    { path: '', component: InventoryListComponent },
    { path: ':variantId', component: InventoryTransactionsComponent }
  ]
}
];