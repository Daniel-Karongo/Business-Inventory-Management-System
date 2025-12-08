import { Routes } from '@angular/router';
import { MainLayoutComponent } from './core/layout/main-layout/main-layout.component';
import { authGuard } from './modules/auth/guards/auth.guard';

export const routes: Routes = [

  {
    path: '',
    component: MainLayoutComponent,
    canActivate: [authGuard],

    children: [
      { path: '', redirectTo: 'dashboard', pathMatch: 'full' },

      { path: 'dashboard', loadChildren: () => import('./modules/dashboard/dashboard.routes').then(m => m.DASHBOARD_ROUTES) },
      { path: 'inventory', loadChildren: () => import('./modules/inventory/inventory.routes').then(m => m.INVENTORY_ROUTES) },
      { path: 'products', loadChildren: () => import('./modules/products/products.routes').then(m => m.PRODUCT_ROUTES) },
      { path: 'suppliers', loadChildren: () => import('./modules/suppliers/suppliers.routes').then(m => m.SUPPLIER_ROUTES) },
      { path: 'sales', loadChildren: () => import('./modules/sales/sales.routes').then(m => m.SALES_ROUTES) },
      { path: 'payments', loadChildren: () => import('./modules/payments/payments.routes').then(m => m.PAYMENTS_ROUTES) },
      { path: 'customers', loadChildren: () => import('./modules/customers/customers.routes').then(m => m.CUSTOMER_ROUTES) },
      { path: 'accounts', loadChildren: () => import('./modules/accounts/accounts.routes').then(m => m.ACCOUNTS_ROUTES) },
      { path: 'users', loadChildren: () => import('./modules/users/users.routes').then(m => m.USER_ROUTES) },
      { path: 'branches', loadChildren: () => import('./modules/branches/branches.routes').then(m => m.BRANCH_ROUTES) },
      { path: 'departments', loadChildren: () => import('./modules/departments/departments.routes').then(m => m.DEPARTMENT_ROUTES) },
    ]
  },

  // AUTH OUTSIDE LAYOUT
  { path: 'login', loadComponent: () => import('./modules/auth/pages/login/login.component').then(m => m.LoginComponent) },
  { path: 'auth', loadChildren: () => import('./modules/auth/auth.routes').then(m => m.AUTH_ROUTES) }
];
