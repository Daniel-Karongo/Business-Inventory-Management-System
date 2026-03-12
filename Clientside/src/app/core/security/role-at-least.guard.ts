import { inject } from '@angular/core';
import { CanMatchFn, Router } from '@angular/router';
import { AuthService } from '../../modules/auth/services/auth.service';
import { ROLE_HIERARCHY, Role } from '../security/role-hierarchy';

export function roleAtLeast(required: Role): CanMatchFn {

  return () => {

    const auth = inject(AuthService);
    const router = inject(Router);

    const userRole = auth.getSnapshot()?.role as Role | undefined;

    if (!userRole) {
      return router.parseUrl('/auth');
    }

    const userLevel = ROLE_HIERARCHY.indexOf(userRole);
    const requiredLevel = ROLE_HIERARCHY.indexOf(required);

    if (userLevel >= requiredLevel) {
      return true;
    }

    return router.parseUrl('/app');

  };

}