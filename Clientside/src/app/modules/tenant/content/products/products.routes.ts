import { Routes } from '@angular/router';
import { roleAtLeast } from '../../../../core/security/role-at-least.guard';

export const PRODUCT_ROUTES: Routes = [

{
  path: '',
  canMatch: [roleAtLeast('EMPLOYEE')],
  loadComponent: () =>
    import('./parent/pages/product-list/product-list.component')
      .then(m => m.ProductListComponent)
},

{
  path: 'create',
  canMatch: [roleAtLeast('EMPLOYEE')],
  loadComponent: () =>
    import('./parent/pages/product-create/product-create.component')
      .then(m => m.ProductCreateComponent)
},

{
  path: ':id',
  canMatch: [roleAtLeast('EMPLOYEE')],
  loadComponent: () =>
    import('./parent/pages/product-details/product-details.component')
      .then(m => m.ProductDetailsComponent)
},

{
  path: ':id/edit',
  canMatch: [roleAtLeast('EMPLOYEE')],
  loadComponent: () =>
    import('./parent/pages/product-edit/product-edit.component')
      .then(m => m.ProductEditComponent)
}

];