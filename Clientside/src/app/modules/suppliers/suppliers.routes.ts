import { Routes } from '@angular/router';
import { authGuard } from '../auth/guards/auth.guard';

import { SupplierListComponent } from './pages/supplier-list/supplier-list.component';
import { SupplierCreateComponent } from './pages/supplier-create/supplier-create.component';
import { SupplierDetailsComponent } from './pages/supplier-details/supplier-details.component';
import { SupplierEditComponent } from './pages/supplier-edit/supplier-edit.component';

export const SUPPLIER_ROUTES: Routes = [
  {
    path: '',
    canActivate: [authGuard],
    children: [
      { path: '', component: SupplierListComponent },
      { path: 'create', component: SupplierCreateComponent },
      { path: ':identifier/edit', component: SupplierEditComponent },
      { path: ':identifier', component: SupplierDetailsComponent }
    ]
  }
];