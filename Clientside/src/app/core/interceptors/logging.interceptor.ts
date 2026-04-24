import { HttpInterceptorFn } from '@angular/common/http';
import { tap } from 'rxjs';
import { environment } from '../../../environments/environment';

export const loggingInterceptor: HttpInterceptorFn = (req, next) => {

  // In production → no logging at all
  if (environment.production) {
    return next(req);
  }

  // 🔕 SILENTLY IGNORE auth bootstrap check
  if (req.url.endsWith('/api/auth/me')) {
    return next(req);
  }

  /* =========================
     REQUEST LOGGING
     ========================= */
  console.log(
    '%cHTTP REQUEST',
    'color: #10b981; font-weight: bold;',
    {
      method: req.method,
      url: req.urlWithParams,
      body: req.body
    }
  );

  return next(req).pipe(
    tap({
      /* =========================
         RESPONSE LOGGING
         ========================= */
      next: event => {
        console.log(
          '%cHTTP RESPONSE',
          'color: #3b82f6; font-weight: bold;',
          event
        );
      },

      /* =========================
         ERROR HANDLING
         ========================= */
      error: err => {

        // ✅ EXPECTED: password reset initiation (anti-enumeration)
        if (
          err?.status === 500 &&
          req.url.includes('/api/auth/password-reset/initiate')
        ) {
          console.log(
            '%cHTTP INFO',
            'color: #f59e0b; font-weight: bold;',
            'Password reset initiated (expected)'
          );
          return;
        }

        // ✅ EXPECTED: wrong / expired OTP during verification
        if (
          err?.status === 400 &&
          req.url.includes('/api/auth/password-reset/verify')
        ) {
          console.log(
            '%cHTTP INFO',
            'color: #f59e0b; font-weight: bold;',
            'Invalid or expired verification code (user input)'
          );
          return;
        }

        // ❌ REAL, UNEXPECTED ERRORS
        console.error(
          '%cHTTP ERROR',
          'color: #ef4444; font-weight: bold;',
          err
        );
      }
    })
  );
};
