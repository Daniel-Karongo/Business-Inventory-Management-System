import { Injectable, inject } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Router } from '@angular/router';

@Injectable({ providedIn: 'root' })
export class AuthErrorService {

    private snack = inject(MatSnackBar);
    private router = inject(Router);

    handle(err: any, context: 'login' | 'biometric' = 'login') {

        const code: string | null = err?.error?.code ?? null;
        const msg: string = err?.error?.message ?? '';

        /* ======================
           🔥 FALLBACK (NO CODE)
           ====================== */
        if (!code) {

            if (msg.includes('Invalid username') || msg.includes('Invalid password')) {
                return this.toast('Invalid login credentials');
            }

            if (msg.includes('JWT token has expired')) {
                return this.toast('Session expired. Please log in again.');
            }

            if (msg.includes('Authentication failed')) {
                return this.toast('Authentication failed. Try again.');
            }

            if (err.status === 401) {
                return this.toast('Session expired. Please log in again.');
            }

            if (err.status === 403) {
                return this.toast('Access denied.');
            }

            return this.toast(msg || 'Something went wrong.');
        }

        /* ======================
           🔥 CODE-BASED HANDLING
           ====================== */
        switch (code) {

            case 'DEVICE_PENDING_APPROVAL':
                return this.toast('Device pending approval. Please wait.');

            case 'DEVICE_NOT_APPROVED':
                return this.toast('This device is not approved.');

            case 'DEVICE_LIMIT_REACHED':
                return this.toast(msg || 'Device or session limit reached.');

            case 'SESSION_EXPIRED':
                return this.toast('Session expired. Please log in again.');

            case 'PASSWORD_RESET_INVALID':
                return this.toast('Invalid or expired reset code.');

            case 'DEVICE_NOT_FOUND':
                return this.toast('Device not found.');

            case 'DEVICE_NOT_REGISTERED':
                return this.toast('Device not registered.');

            case 'LOCATION_NOT_CONFIGURED':
                return this.toast('Location security is not configured.');

            case 'BIOMETRIC_TENANT_MISMATCH':
                return this.toast('Biometric does not belong to this tenant.');

            case 'DEVICE_ID_REQUIRED':
                return this.toast('Device ID is required.');

            case 'LOCATION_REQUIRED':
                return this.toast('Location access is required.');

            case 'LOCATION_OUTSIDE_BOUNDARY':
                return this.toast('You are outside the allowed location.');

            case 'LOCATION_ACCURACY_LOW':
                return this.toast('Location accuracy too low.');

            case 'INVALID_CREDENTIALS':
            case 'USER_NOT_FOUND':
                return this.toast('Invalid login credentials');

            case 'ACCOUNT_DISABLED':
                return this.toast('Your account is disabled.');

            case 'ACCOUNT_DELETED':
                return this.toast('Account has been deleted.');

            case 'USER_NOT_IN_BRANCH':
                return this.toast('You are not assigned to this branch.');

            case 'BRANCH_NOT_FOUND':
                return this.toast('Branch not found.');

            case 'PASSWORD_CHANGE_REQUIRED':
                this.toast('You must change your password before continuing.');
                this.router.navigate(['/auth/reset-password'], {
                    state: { forced: true }
                });
                return;

            /* ===== BIOMETRIC ===== */

            case 'BIOMETRIC_VERIFICATION_FAILED':
                return this.toast('Biometric verification failed.');

            case 'BIOMETRIC_CHALLENGE_EXPIRED':
                return this.toast('Biometric session expired.');

            case 'BIOMETRIC_DEVICE_MISMATCH':
                return this.toast('Biometric belongs to another device.');

            case 'BIOMETRIC_USER_MISMATCH':
                return this.toast('Biometric belongs to another user.');

            case 'BIOMETRIC_CREDENTIAL_NOT_FOUND':
                return this.toast('No biometrics registered for this device.');

            case 'BIOMETRIC_INVALID_PAYLOAD':
                return this.toast('Invalid biometric data.');

            case 'INVALID_REQUEST':
                return this.toast(
                    msg || 'Invalid request.'
                );

            case 'UNKNOWN':
            default:
                return this.toast(msg || 'Something went wrong.');
        }
    }

    private toast(message: string) {
        this.snack.open(message, 'Close', { duration: 4000 });
    }
}