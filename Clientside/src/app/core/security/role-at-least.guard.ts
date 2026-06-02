import { inject } from '@angular/core';
import { CanMatchFn, Router } from '@angular/router';
import { AuthService } from '../../modules/auth/services/auth.service';
import { ROLE_HIERARCHY, Role } from './role-hierarchy';
import { map, catchError, of } from 'rxjs';

export function roleAtLeast(
  required: Role
): CanMatchFn {

  return () => {

    const auth = inject(AuthService);
    const router = inject(Router);

    return auth.getCurrentUser().pipe(

      map(user => {

        if (!user) {
          return router.parseUrl('/auth');
        }

        const userLevel =
          ROLE_HIERARCHY.indexOf(
            user.role as Role
          );

        const requiredLevel =
          ROLE_HIERARCHY.indexOf(required);

        if (userLevel >= requiredLevel) {
          return true;
        }

        return router.parseUrl('/app');

      }),

      catchError(() =>
        of(router.parseUrl('/auth'))
      )

    );

  };

}