import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { catchError, throwError } from 'rxjs';
import { MatSnackBar } from '@angular/material/snack-bar';

let isShowing = false; // 🔥 shared across all requests

export const networkErrorInterceptor: HttpInterceptorFn = (req, next) => {

  const snack = inject(MatSnackBar);

  return next(req).pipe(
    catchError((err: HttpErrorResponse) => {

      if (err.status === 0) {

        if (!isShowing) {
          isShowing = true;

          const ref = snack.open(
            'Server is unreachable. Please check your connection.',
            'Close'
          );

          ref.afterDismissed().subscribe(() => {
            isShowing = false; // 🔥 allow future alerts
          });
        }

        return throwError(() => err);
      }

      return throwError(() => err);
    })
  );
};