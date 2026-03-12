import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { CustomerCreateComponent } from './pages/customer-create/customer-create.component';
import { CustomerDetailsComponent } from './pages/customer-details/customer-details.component';
import { CustomerEditComponent } from './pages/customer-edit/customer-edit.component';
import { CustomerListComponent } from './pages/customer-list/customer-list.component';

export const CUSTOMER_ROUTES: Routes = [
{
  path: '',
  canMatch: [roleAtLeast('MANAGER')],
  children: [
    { path: '', component: CustomerListComponent },
    { path: 'create', component: CustomerCreateComponent },
    { path: ':id', component: CustomerDetailsComponent },
    { path: ':id/edit', component: CustomerEditComponent }
  ]
}
];