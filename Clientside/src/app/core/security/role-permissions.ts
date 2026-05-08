export type Feature =
  | 'dashboard'
  | 'reports'
  | 'stock'
  | 'categories'
  | 'products'
  | 'inventory'
  | 'stock-onboarding'
  | 'sales'
  | 'suppliers'
  | 'customers'
  | 'users'
  | 'branches'
  | 'departments'
  | 'accounts'
  | 'payments'
  | 'finance'
  | 'devices'
  | '*';

export const ROLE_PERMISSIONS: Record<string, Feature[]> = {

  EMPLOYEE: [
    'stock',
    'categories',
    'products',
    'inventory',
    'stock-onboarding',
    'sales'
  ],

  SUPERVISOR: [
    'stock',
    'categories',
    'products',
    'inventory',
    'stock-onboarding',
    'sales',
    'suppliers',
    'users',
    'departments'
  ],

  MANAGER: [
    'dashboard',
    'reports',
    'stock',
    'categories',
    'products',
    'inventory',
    'stock-onboarding',
    'sales',
    'suppliers',
    'customers',
    'users',
    'branches',
    'departments',
    'accounts',
    'payments'
  ],

  ADMIN: [
    'dashboard',
    'reports',
    'stock',
    'categories',
    'products',
    'inventory',
    'stock-onboarding',
    'sales',
    'suppliers',
    'customers',
    'users',
    'branches',
    'departments',
    'finance'
  ],

  SUPERUSER: [
    '*'
  ]

};