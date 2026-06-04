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
  | 'security'
  | 'sessions'
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
    'security',
    'sessions',
    'devices',
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
    'finance',
    'security',
    'sessions',
    'devices',
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
    'finance',
    'security',
    'sessions',
    'devices',
  ],

  SUPERUSER: [
    '*'
  ]

};