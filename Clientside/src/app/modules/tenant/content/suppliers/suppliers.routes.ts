import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { SupplierCreateComponent } from './pages/supplier-create/supplier-create.component';
import { SupplierDetailsComponent } from './pages/supplier-details/supplier-details.component';
import { SupplierEditComponent } from './pages/supplier-edit/supplier-edit.component';
import { SupplierListComponent } from './pages/supplier-list/supplier-list.component';

export const SUPPLIER_ROUTES: Routes = [
{
  path: '',
  canMatch: [roleAtLeast('SUPERVISOR')],
  children: [
    { path: '', component: SupplierListComponent },
    { path: 'create', component: SupplierCreateComponent },
    { path: ':identifier/edit', component: SupplierEditComponent },
    { path: ':identifier', component: SupplierDetailsComponent }
  ]
}
];