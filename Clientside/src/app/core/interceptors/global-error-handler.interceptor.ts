import { ErrorHandler, Injectable } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';

@Injectable()
export class GlobalErrorHandler
    implements ErrorHandler {

    constructor(
        private snackBar: MatSnackBar
    ) { }

    handleError(error: any): void {

        const message =
            String(error);

        if (
            message.includes(
                'Failed to fetch dynamically imported module'
            )
        ) {

            this.snackBar.open(
                'Connection lost. Please refresh the page.',
                'Close'
            );

            return;
        }

        console.error(error);
    }
}