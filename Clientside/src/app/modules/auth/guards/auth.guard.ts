import { inject } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../services/auth.service';
import { catchError, map, of } from 'rxjs';

export const authGuard = () => {
  const auth = inject(AuthService);
  const router = inject(Router);

  return auth.init().pipe(
    map(user => user ? true : router.parseUrl('/auth')),
    catchError(() => of(router.parseUrl('/auth')))
  );
};