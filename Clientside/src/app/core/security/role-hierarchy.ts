export const ROLE_HIERARCHY = [
  'EMPLOYEE',
  'SUPERVISOR',
  'MANAGER',
  'ADMIN',
  'SUPERUSER'
] as const;

export type Role = typeof ROLE_HIERARCHY[number];