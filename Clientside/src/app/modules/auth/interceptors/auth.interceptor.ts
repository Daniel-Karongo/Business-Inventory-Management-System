import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { Router } from '@angular/router';
import { catchError, throwError } from 'rxjs';

export const authInterceptor: HttpInterceptorFn = (req, next) => {
  const router = inject(Router);

  req = req.clone({ withCredentials: true });

  return next(req).pipe(
    catchError(err => {

      // ✅ IGNORE unauthenticated /auth/me on startup
      if (
        err.status === 401 &&
        req.url.endsWith('/api/auth/me')
      ) {
        return throwError(() => err);
      }

      // 🔐 REAL auth failure (after login)
      if (err.status === 401) {
        router.navigate(['/login']);
      }

      return throwError(() => err);
    })
  );
};