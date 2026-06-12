import { HttpErrorResponse, HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { catchError, throwError } from 'rxjs';

let isShowing = false;

export const networkErrorInterceptor: HttpInterceptorFn = (req, next) => {
  const snack = inject(MatSnackBar);

  return next(req).pipe(
    catchError((err: HttpErrorResponse) => {

      console.error('HTTP ERROR', {
        url: err.url,
        status: err.status,
        statusText: err.statusText,
        message: err.message,
        error: err.error
      });

      const backendUnavailable =
        err.status === 0 ||
        (
          err.status === 500 &&
          (
            err.error == null ||
            err.error === ''
          )
        );

      if (backendUnavailable && !isShowing) {

        isShowing = true;

        const ref = snack.open(
          'Server is unreachable. Please check your connection.',
          'Close'
        );

        ref.afterDismissed().subscribe(() => {
          isShowing = false;
        });
      }

      return throwError(() => err);
    })
  );
};