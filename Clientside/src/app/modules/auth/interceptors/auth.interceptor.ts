import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { Router } from '@angular/router';
import { catchError, throwError } from 'rxjs';
import { DeviceService } from '../../../core/services/device.service';

export const authInterceptor: HttpInterceptorFn = (req, next) => {
  const router = inject(Router);

  req = req.clone({ withCredentials: true });

  const deviceService = inject(DeviceService);

  req = req.clone({
    setHeaders: {
      'X-Device-Id': deviceService.getDeviceId()
    }
  });

  console.log("X-Device-Id HEADER:", deviceService.getDeviceId());

  return next(req).pipe(
    catchError(err => {

      // 1. Ignore bootstrap check (/auth/me)
      if (
        err.status === 401 &&
        req.url.includes('/auth/me')
      ) {
        return throwError(() => err);
      }

      // 2. Ignore auth endpoints themselves (login, reset, biometric)
      if (req.url.includes('/auth/')) {
        return throwError(() => err);
      }

      // 3. Only redirect for real session expiry
      if (err.status === 401) {
        router.navigate(['/auth'], {
          queryParams: { reason: 'expired' }
        });
      }

      return throwError(() => err);
    })
  );
};