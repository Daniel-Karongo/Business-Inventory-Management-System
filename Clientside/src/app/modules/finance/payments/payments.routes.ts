import { Routes } from '@angular/router';
import { SupplierPaymentsComponent } from './pages/supplier-payments/supplier-payments.component';

export const PAYMENTS_ROUTES: Routes = [
  { path: '', redirectTo: 'supplier', pathMatch: 'full' },
  { path: 'supplier', component: SupplierPaymentsComponent }
];