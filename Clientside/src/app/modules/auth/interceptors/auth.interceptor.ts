import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { AuthService } from '../services/auth.service';
import { catchError } from 'rxjs';
import { Router } from '@angular/router';

export const authInterceptor: HttpInterceptorFn = (req, next) => {
  const auth = inject(AuthService);
  const token = auth.getToken();
  const router = inject(Router);

  if (token) {
    req = req.clone({
      setHeaders: {
        Authorization: `Bearer ${token}`
      }
    });
  }

  return next(req).pipe(
    catchError(err => {

      // 🚫 Ignore auth/logout failures
      if (req.url.includes('/auth/logout')) {
        throw err;
      }

      if (err.status === 401) {
        auth.logoutLocal();
        router.navigate(['/login']);
      }

      throw err;
    })
  );
};