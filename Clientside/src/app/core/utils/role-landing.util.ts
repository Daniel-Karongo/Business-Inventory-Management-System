export function resolveTenantLanding(role: string | undefined | null): string {

  switch (role) {

    case 'EMPLOYEE':
      return '/app/sales';

    case 'SUPERVISOR':
      return '/app/inventory';

    case 'MANAGER':
    case 'ADMIN':
    case 'SUPERUSER':
    default:
      return '/app/dashboard';

  }

}