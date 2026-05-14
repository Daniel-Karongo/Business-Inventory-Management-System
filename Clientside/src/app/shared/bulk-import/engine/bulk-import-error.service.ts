import { Injectable, inject } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';

@Injectable({
    providedIn: 'root'
})
export class BulkImportErrorService {

    private snack =
        inject(MatSnackBar);

    handle(err: any) {

        console.error(
            '[Bulk Import Error]',
            err
        );

        const status =
            err?.status;

        const body =
            err?.error;

        const code =
            body?.code;

        const message =
            this.extractMessage(err);

        /* =========================
           NETWORK
        ========================= */

        if (status === 0) {
            return this.toast(
                'Network error. Please check your connection.'
            );
        }

        /* =========================
           AUTH
        ========================= */

        if (status === 401) {
            return this.toast(
                'Session expired. Please log in again.'
            );
        }

        if (status === 403) {
            return this.toast(
                'You are not authorized to perform bulk imports.'
            );
        }

        /* =========================
           CODE-BASED
        ========================= */

        switch (code) {

            case 'SUBSCRIPTION_LIMIT_EXCEEDED':
                return this.toast(
                    message ||
                    'Subscription limit exceeded.'
                );

            case 'INVALID_REQUEST':
                return this.toast(
                    message ||
                    'Invalid bulk import request.'
                );

            case 'VALIDATION_FAILED':
                return this.toast(
                    'Some rows contain validation errors.'
                );

            case 'DUPLICATE_RESOURCE':
                return this.toast(
                    message ||
                    'Duplicate records detected.'
                );
        }

        /* =========================
           MESSAGE FALLBACKS
        ========================= */

        if (
            message.includes(
                'User limit exceeded'
            )
        ) {
            return this.toast(
                'Import exceeds your subscription user limit.'
            );
        }

        if (
            message.includes(
                'Branch limit exceeded'
            )
        ) {
            return this.toast(
                'Import exceeds your subscription branch limit.'
            );
        }

        if (
            message.includes(
                'Duplicate username'
            )
        ) {
            return this.toast(
                'Duplicate usernames detected in import.'
            );
        }

        if (
            message.includes(
                'Duplicate email'
            )
        ) {
            return this.toast(
                'Duplicate email addresses detected.'
            );
        }

        if (
            message.includes(
                'Duplicate phone'
            )
        ) {
            return this.toast(
                'Duplicate phone numbers detected.'
            );
        }

        if (
            message.includes(
                'Unknown branchCode'
            )
        ) {
            return this.toast(
                'One or more branch codes do not exist.'
            );
        }

        if (
            message.includes(
                'Unknown department'
            )
        ) {
            return this.toast(
                'One or more departments do not exist.'
            );
        }

        if (
            message.includes(
                'Invalid role'
            )
        ) {
            return this.toast(
                'One or more roles are invalid.'
            );
        }

        if (
            message.includes(
                'Insufficient privilege'
            )
        ) {
            return this.toast(
                'You are not allowed to assign one or more selected roles.'
            );
        }

        if (
            message.includes(
                'rollback-only'
            )
        ) {
            return this.toast(
                'Import transaction failed and was rolled back.'
            );
        }

        if (
            message.includes(
                'DataIntegrityViolationException'
            )
        ) {
            return this.toast(
                'Database integrity violation detected.'
            );
        }

        if (
            message.includes(
                'Maximum upload size exceeded'
            )
        ) {
            return this.toast(
                'Uploaded files are too large.'
            );
        }

        if (
            message.includes(
                'Current request is not a multipart request'
            )
        ) {
            return this.toast(
                'Invalid upload request.'
            );
        }

        if (
            message.includes(
                'Failed to fetch'
            )
        ) {
            return this.toast(
                'Unable to reach the server.'
            );
        }

        /* =========================
           DEFAULT
        ========================= */

        return this.toast(
            message ||
            'Bulk import failed.'
        );
    }

    /* =====================================
       MESSAGE EXTRACTION
    ===================================== */

    private extractMessage(
        err: any
    ): string {

        const body =
            err?.error;

        if (
            typeof body === 'string'
        ) {
            return body;
        }

        if (
            typeof body?.message === 'string'
        ) {
            return body.message;
        }

        if (
            typeof body?.detail === 'string'
        ) {
            return body.detail;
        }

        if (
            Array.isArray(
                body?.errors
            )
        ) {
            return body.errors
                .map(
                    (e: any) =>
                        e?.message
                )
                .filter(Boolean)
                .join(', ');
        }

        if (
            typeof err?.message === 'string'
        ) {
            return err.message;
        }

        return '';
    }

    private toast(
        message: string
    ) {

        this.snack.open(
            message,
            'Close',
            {
                duration: 8000
            }
        );
    }
}