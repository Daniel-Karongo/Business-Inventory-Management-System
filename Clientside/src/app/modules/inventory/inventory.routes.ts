import { Routes } from '@angular/router';
import { InventoryValuationComponent } from './pages/inventory-valuation/inventory-valuation.component';
import { InventoryTransactionsComponent } from './pages/inventory-transactions/inventory-transactions.component';
import { InventoryListComponent } from './pages/inventory-list/inventory-list.component';

export const INVENTORY_ROUTES: Routes = [
  {
    path: '',
    component: InventoryListComponent
  },
  {
    path: 'transactions/:variantId',
    component: InventoryTransactionsComponent
  },
  {
    path: 'valuation',
    component: InventoryValuationComponent
  }
];