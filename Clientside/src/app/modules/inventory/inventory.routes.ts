import { Routes } from '@angular/router';
import { InventoryTransactionsComponent } from './pages/inventory-transactions/inventory-transactions.component';
import { InventoryListComponent } from './pages/inventory-list/inventory-list.component';

export const INVENTORY_ROUTES: Routes = [
  {
    path: '',
    component: InventoryListComponent
  },
  {
    path: ':variantId',
    component: InventoryTransactionsComponent
  }
];