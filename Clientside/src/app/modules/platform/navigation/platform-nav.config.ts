export interface PlatformNavItem {
  label: string;
  icon: string;
  route: string;
}

export const PLATFORM_NAV_ITEMS: PlatformNavItem[] = [
  {
    label: 'Dashboard',
    icon: 'dashboard',
    route: '/platform'
  },
  {
    label: 'Devices',
    icon: 'devices',
    route: '/platform/security/devices'
  },
  {
    label: 'Tenants',
    icon: 'apartment',
    route: '/platform/tenants'
  },
  {
    label: 'Users',
    icon: 'manage_accounts',
    route: '/platform/users'
  },
  {
    label: 'Plans',
    icon: 'workspace_premium',
    route: '/platform/plans'
  },
  {
    label: 'Analytics',
    icon: 'insights',
    route: '/platform/analytics'
  },
  {
    label: 'Audit Logs',
    icon: 'history',
    route: '/platform/audit'
  }
];