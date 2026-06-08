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
  | 'expenses'
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
    'expenses',
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
    'expenses',
    'finance',
    'security',
    'sessions',
    'devices',
  ],

  SUPERUSER: [
    '*'
  ]

};