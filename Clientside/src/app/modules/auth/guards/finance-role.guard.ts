import { inject } from '@angular/core';
import { CanActivateFn, Router } from '@angular/router';
import { AuthService } from '../services/auth.service';

export const financeRoleGuard: CanActivateFn = () => {

  const auth = inject(AuthService);
  const router = inject(Router);

  const role = auth.getSnapshot()?.role;

  const allowed = ['SUPERUSER', 'ADMIN', 'MANAGER'];

  if (role && allowed.includes(role)) {
    return true;
  }

  router.navigate(['/dashboard']);
  return false;
};