import { Routes } from '@angular/router';
import { CustomerListComponent } from './pages/customer-list/customer-list.component';
import { CustomerCreateComponent } from './pages/customer-create/customer-create.component';
import { CustomerEditComponent } from './pages/customer-edit/customer-edit.component';
import { CustomerDetailsComponent } from './pages/customer-details/customer-details.component';

export const CUSTOMER_ROUTES: Routes = [
  { path: '', component: CustomerListComponent },
  { path: 'create', component: CustomerCreateComponent },
  { path: ':id', component: CustomerDetailsComponent },
  { path: ':id/edit', component: CustomerEditComponent }
];