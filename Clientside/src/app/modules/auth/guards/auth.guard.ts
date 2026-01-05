import { inject } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../services/auth.service';
import { catchError, map, of } from 'rxjs';

export const authGuard = () => {

  const auth = inject(AuthService);
  const router = inject(Router);

  // Already loaded
  if (auth.getSnapshot()) {
    return true;
  }

  return auth.loadMe().pipe(
    map(() => true),
    catchError(() => of(router.parseUrl('/login')))
  );
};