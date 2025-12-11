import { Routes } from '@angular/router';
import { MainLayoutComponent } from './core/layout/main-layout/main-layout.component';
import { authGuard } from './modules/auth/guards/auth.guard';

export const routes: Routes = [

  // 🔓 PUBLIC ROUTES FIRST (unguarded)
  { path: 'login', loadComponent: () => import('./modules/auth/pages/login/login.component').then(m => m.LoginComponent) },
  { path: 'auth', loadChildren: () => import('./modules/auth/auth.routes').then(m => m.AUTH_ROUTES) },

  // 🔐 PROTECTED AREA
  {
    path: '',
    component: MainLayoutComponent,
    children: [
      { path: '', redirectTo: 'dashboard', pathMatch: 'full' },

      { path: 'dashboard', canMatch: [authGuard], loadChildren: () => import('./modules/dashboard/dashboard.routes').then(m => m.DASHBOARD_ROUTES) },
      { path: 'inventory', canMatch: [authGuard], loadChildren: () => import('./modules/inventory/inventory.routes').then(m => m.INVENTORY_ROUTES) },
      { path: 'products', canMatch: [authGuard], loadChildren: () => import('./modules/products/products.routes').then(m => m.PRODUCT_ROUTES) },
      { path: 'suppliers', canMatch: [authGuard], loadChildren: () => import('./modules/suppliers/suppliers.routes').then(m => m.SUPPLIER_ROUTES) },
      { path: 'sales', canMatch: [authGuard], loadChildren: () => import('./modules/sales/sales.routes').then(m => m.SALES_ROUTES) },
      { path: 'payments', canMatch: [authGuard], loadChildren: () => import('./modules/payments/payments.routes').then(m => m.PAYMENTS_ROUTES) },
      { path: 'customers', canMatch: [authGuard], loadChildren: () => import('./modules/customers/customers.routes').then(m => m.CUSTOMER_ROUTES) },
      { path: 'accounts', canMatch: [authGuard], loadChildren: () => import('./modules/accounts/accounts.routes').then(m => m.ACCOUNTS_ROUTES) },
      { path: 'users', canMatch: [authGuard], loadChildren: () => import('./modules/users/users.routes').then(m => m.USER_ROUTES) },
      { path: 'branches', canMatch: [authGuard], loadChildren: () => import('./modules/branches/branches.routes').then(m => m.BRANCH_ROUTES) },
      { path: 'departments', canMatch: [authGuard], loadChildren: () => import('./modules/departments/departments.routes').then(m => m.DEPARTMENT_ROUTES) },
    ]
  },

  { path: '**', redirectTo: 'login' }
];