import { Routes } from '@angular/router';
import { MainLayoutComponent } from './layout/main-layout/main-layout.component';
import { authGuard } from '../auth/guards/auth.guard';

export const TENANT_ROUTES: Routes = [

  /* ============================================================
     TENANT APP SHELL
  ============================================================ */

  {
    path: '',
    component: MainLayoutComponent,
    canActivate: [authGuard],

    children: [

      /* ============================================================
         DASHBOARD
      ============================================================ */

      {
        path: '',
        redirectTo: 'dashboard',
        pathMatch: 'full'
      },

      {
        path: 'dashboard',
        loadChildren: () =>
          import('./content/dashboard/dashboard.routes')
            .then(m => m.DASHBOARD_ROUTES)
      },

      /* ============================================================
         SALES
      ============================================================ */

      {
        path: 'sales',
        loadChildren: () =>
          import('./content/sales/sales.routes')
            .then(m => m.SALES_ROUTES)
      },

      /* ============================================================
         INVENTORY
      ============================================================ */

      {
        path: 'inventory',
        loadChildren: () =>
          import('./content/inventory/inventory.routes')
            .then(m => m.INVENTORY_ROUTES)
      },

      /* ============================================================
         PRODUCTS
      ============================================================ */

      {
        path: 'products',
        loadChildren: () =>
          import('./content/products/products.routes')
            .then(m => m.PRODUCT_ROUTES)
      },

      /* ============================================================
         CATEGORIES
      ============================================================ */

      {
        path: 'categories',
        loadChildren: () =>
          import('./content/categories/categories.routes')
            .then(m => m.CATEGORY_ROUTES)
      },

      /* ============================================================
         SUPPLIERS
      ============================================================ */

      {
        path: 'suppliers',
        loadChildren: () =>
          import('./content/suppliers/suppliers.routes')
            .then(m => m.SUPPLIER_ROUTES)
      },

      /* ============================================================
         CUSTOMERS
      ============================================================ */

      {
        path: 'customers',
        loadChildren: () =>
          import('./content/customers/customers.routes')
            .then(m => m.CUSTOMER_ROUTES)
      },

      /* ============================================================
         USERS
      ============================================================ */

      {
        path: 'users',
        loadChildren: () =>
          import('./content/users/users.routes')
            .then(m => m.USER_ROUTES)
      },

      /* ============================================================
         BRANCHES
      ============================================================ */

      {
        path: 'branches',
        loadChildren: () =>
          import('./content/branches/branches.routes')
            .then(m => m.BRANCH_ROUTES)
      },

      /* ============================================================
         DEPARTMENTS
      ============================================================ */

      {
        path: 'departments',
        loadChildren: () =>
          import('./content/departments/departments.routes')
            .then(m => m.DEPARTMENT_ROUTES)
      },

      /* ============================================================
         REPORTS
      ============================================================ */

      {
        path: 'reports',
        loadChildren: () =>
          import('./content/reports/reports.routes')
            .then(m => m.REPORTS_ROUTES)
      },

      /* ============================================================
         FINANCE
      ============================================================ */

      {
        path: 'finance',
        loadChildren: () =>
          import('./content/finance/finance.routes')
            .then(m => m.FINANCE_ROUTES)
      }

    ]

  }

];