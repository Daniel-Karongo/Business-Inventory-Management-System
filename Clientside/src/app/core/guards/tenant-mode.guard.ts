import { inject } from '@angular/core';
import { CanMatchFn, Router } from '@angular/router';
import { AuthService } from '../../modules/auth/services/auth.service';
import { DomainContextService } from '../services/domain-context.service';
import { map, catchError, of } from 'rxjs';

export const tenantModeGuard: CanMatchFn = () => {
  const auth = inject(AuthService);
  const router = inject(Router);
  const domain = inject(DomainContextService);

  return auth.init().pipe(
    map(user => {
      if (!user) return router.parseUrl('/auth');

      if (!domain.isTenant) {
        return router.parseUrl('/auth');
      }

      if (user.userType !== 'TENANT') {
        return router.parseUrl('/auth');
      }

      return true;
    }),
    catchError(() => of(router.parseUrl('/auth')))
  );
};