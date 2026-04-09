export type Feature =
  | 'dashboard'
  | 'reports'
  | 'categories'
  | 'products'
  | 'inventory'
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
    'categories',
    'products',
    'inventory',
    'sales'
  ],

  SUPERVISOR: [
    'categories',
    'products',
    'inventory',
    'sales',
    'suppliers',
    'users',
    'departments'
  ],

  MANAGER: [
    'dashboard',
    'reports',
    'categories',
    'products',
    'inventory',
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
    'categories',
    'products',
    'inventory',
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