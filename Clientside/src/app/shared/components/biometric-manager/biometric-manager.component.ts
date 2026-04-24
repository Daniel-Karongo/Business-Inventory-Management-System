import {
    Component,
    Input,
    OnInit,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    MatCardModule
} from '@angular/material/card';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatSnackBar
} from '@angular/material/snack-bar';

import {
    MatDialog,
    MatDialogModule
} from '@angular/material/dialog';

import {
    MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
    BiometricApiService,
    UserBiometricDTO
} from '../../../core/services/biometric-api.service';

import {
    BiometricRegistrationService
} from '../../../core/services/biometric-registration.service';


@Component({
    standalone: true,
    selector: 'app-biometric-manager',

    imports: [
        CommonModule,
        MatCardModule,
        MatButtonModule,
        MatIconModule,
        MatDialogModule,
        MatProgressSpinnerModule
    ],

    templateUrl:
        './biometric-manager.component.html',

    styleUrls: [
        './biometric-manager.component.scss'
    ]
})
export class BiometricManagerComponent implements OnInit {

    @Input()
    adminMode = false;

    @Input()
    targetUserId?: string;

    private api =
        inject(BiometricApiService);

    private registerSvc =
        inject(BiometricRegistrationService);

    private snack =
        inject(MatSnackBar);

    private dialog =
        inject(MatDialog);

    loading = true;

    registerBusy = false;

    rowBusy:
        Record<string, boolean> = {};

    biometrics:
        UserBiometricDTO[] = [];

    ngOnInit() {
        this.load();
    }

    load() {

        this.loading = true;

        const req =
            this.adminMode
                && this.targetUserId

                ? this.api.adminListForUser(
                    this.targetUserId
                )

                : this.api.list();

        req.subscribe({

            next: rows => {

                this.biometrics = rows || [];
                this.loading = false;

            },

            error: () => {

                this.loading = false;

                this.snack.open(
                    'Failed to load credentials',
                    'Close',
                    { duration: 4000 }
                );

            }

        });

    }
    registerCredential() {

        if (this.registerBusy) {
            return;
        }

        this.registerBusy = true;

        this.registerSvc
            .register()
            .finally(() => {

                this.registerBusy = false;

                setTimeout(
                    () => this.load(),
                    1200
                );

            });
    }

    removeCredential(
        row: UserBiometricDTO
    ) {

        if (this.rowBusy[row.id]) {
            return;
        }

        if (
            !window.confirm(
                `Remove credential "${row.deviceName}"?`
            )
        ) {
            return;
        }

        this.rowBusy[row.id] = true;

        const req =
            this.adminMode

                ? this.api.adminDelete(
                    row.id
                )

                : this.api.delete(
                    row.id
                );

        req.subscribe({

            next: () => {

                delete this.rowBusy[row.id];

                this.snack.open(
                    'Credential removed',
                    'Close',
                    { duration: 3000 }
                );

                this.load();

            },

            error: (err) => {

                delete this.rowBusy[row.id];

                const msg =
                    err?.error?.message;

                if (
                    msg === 'Cannot delete last credential'
                ) {

                    this.snack.open(
                        'At least one credential must remain.',
                        'Close',
                        { duration: 4500 }
                    );

                    return;
                }

                this.snack.open(
                    'Credential removal failed',
                    'Close',
                    { duration: 4500 }
                );

            }

        });

    }
}