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
  | 'accounts'
  | 'ap'
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
    'accounts',
    'ap',
    'finance'
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
    'finance'
  ],

  SUPERUSER: [
    '*'
  ]

};