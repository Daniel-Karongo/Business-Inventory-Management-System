import { inject } from '@angular/core';
import { CanActivateFn, Router } from '@angular/router';
import { AuthService } from '../../auth/services/auth.service';

export const platformAdminGuard: CanActivateFn = () => {

  const auth = inject(AuthService);
  const router = inject(Router);

  const user = auth.getSnapshot();

  if (!user) {

    router.navigate(['/auth']);

    return false;

  }

  if (user.role !== 'SUPERUSER') {

    router.navigate(['/dashboard']);

    return false;

  }

  return true;

};