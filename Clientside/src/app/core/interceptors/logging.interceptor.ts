import { HttpInterceptorFn } from '@angular/common/http';
import { tap } from 'rxjs';
import { environment } from '../../../environments/environment';

export const loggingInterceptor: HttpInterceptorFn = (req, next) => {

  // In production → just forward the request
  if (environment.production) {
    return next(req);
  }

  // In development → log requests
  console.log('%cHTTP REQUEST', 'color: #10b981; font-weight: bold;', {
    method: req.method,
    url: req.urlWithParams,
    body: req.body
  });

  return next(req).pipe(
    tap({
      next: event => {
        console.log(
          '%cHTTP RESPONSE',
          'color: #3b82f6; font-weight: bold;',
          event
        );
      },
      error: err => {

        // ✅ EXPECTED: unauthenticated app bootstrap
        if (
          err?.status === 401 &&
          req.url.endsWith('/api/auth/me')
        ) {
          console.log(
            '%cHTTP INFO',
            'color: #f59e0b; font-weight: bold;',
            'Unauthenticated (expected): /api/auth/me'
          );
          return;
        }

        // ❌ REAL errors
        console.error(
          '%cHTTP ERROR',
          'color: #ef4444; font-weight: bold;',
          err
        );
      }
    })
  );
};