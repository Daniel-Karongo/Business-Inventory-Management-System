import { Component, Input, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatMenuModule } from '@angular/material/menu';
import { MatDividerModule } from '@angular/material/divider';
import { BiometricApiService, UserBiometricDTO, BiometricStatsDTO } from '../../../core/services/biometric-api.service';
import { BiometricRegistrationService } from '../../../core/services/biometric-registration.service';
import { RenameDeviceDialogComponent } from '../rename-device-dialog/rename-device-dialog.component';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';

@Component({
    standalone: true, selector: 'app-biometric-manager', imports: [
        CommonModule, MatCardModule, MatButtonModule, MatIconModule, MatDialogModule, MatProgressSpinnerModule, MatTooltipModule, MatMenuModule, MatDividerModule
    ], templateUrl: './biometric-manager.component.html', styleUrls: [
        './biometric-manager.component.scss'
    ]
})
export class BiometricManagerComponent
    implements OnInit {

    @Input()
    adminMode = false;
    @Input()
    targetUserId?: string;

    private api = inject(BiometricApiService);
    private registerSvc = inject(BiometricRegistrationService);
    private snack = inject(MatSnackBar);
    private dialog = inject(MatDialog);

    loading = true;
    registerBusy = false;
    rowBusy: Record<string, boolean> = {};
    biometrics: UserBiometricDTO[] = [];
    stats: BiometricStatsDTO = {
        activeCredentials: 0, uniqueUsers: 0, uniqueDevices: 0
    };
    credentialCount = 0;
    ngOnInit() {
        this.load();
    }

    load(): Promise<void> {

        return new Promise(resolve => {

            this.loading = true;

            const req =
                this.adminMode &&
                    this.targetUserId
                    ? this.api.adminListForUser(
                        this.targetUserId
                    )
                    : this.api.list();

            req.subscribe({

                next: rows => {

                    this.biometrics = rows || [];
                    this.credentialCount =
                        this.biometrics.length;

                    this.loading = false;

                    resolve();

                },

                error: () => {

                    this.loading = false;

                    this.snack.open(
                        'Failed to load credentials',
                        'Close',
                        { duration: 4000 }
                    );

                    resolve();

                }

            });

            if (this.adminMode) {

                this.api.stats()
                    .subscribe({
                        next: s => {
                            this.stats = s;
                        }
                    });

            }

        });

    }

    async registerCredential() {

        if (this.registerBusy) {
            return;
        }

        this.registerBusy = true;

        try {

            const success =
                await this.registerSvc.register();

            if (success) {

                await this.load();

                this.snack.open(
                    'Credential list refreshed',
                    'Close',
                    { duration: 2500 }
                );

            }

        }
        finally {

            this.registerBusy = false;

        }

    }

    renameCredential(
        row: UserBiometricDTO
    ) {

        if (this.rowBusy[row.id]) {
            return;
        }

        const ref = this.dialog.open(
            RenameDeviceDialogComponent, {
            width: '520px',
            maxWidth: '95vw',
            data: {
                currentName: row.deviceName,
                title: 'Rename Credential',
                subtitle: 'Update the display name used to identify this biometric credential.',
                label: 'Credential Name'
            }
        }
        );
        ref.afterClosed()
            .subscribe(name => {

                if (
                    !name?.trim()
                ) {
                    return;
                }

                this.rowBusy[row.id] = true;
                this.api.rename(
                    row.id, name.trim()
                ).subscribe({

                    next: () => {

                        delete this.rowBusy[row.id];
                        this.snack.open(
                            'Credential renamed', 'Close', { duration: 3000 }
                        );
                        this.load();
                    },

                    error: () => {

                        delete this.rowBusy[row.id];
                        this.snack.open(
                            'Rename failed', 'Close', { duration: 4000 }
                        );
                    }

                });
            });
    }

    removeCredential(row: UserBiometricDTO) {

        if (this.rowBusy[row.id]) {
            return;
        }

        const ref = this.dialog.open(
            ConfirmDialogComponent,
            {
                width: '420px',
                data: {
                    title: 'Remove Credential',
                    message: `Remove "${row.deviceName}"?`,
                    confirmText: 'Remove',
                    cancelText: 'Cancel'
                }
            }
        );

        ref.afterClosed().subscribe(confirmed => {

            if (!confirmed) {
                return;
            }

            this.rowBusy[row.id] = true;

            const req = this.adminMode
                ? this.api.adminDelete(row.id, false)
                : this.api.delete(row.id, false);

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

                    const message =
                        err?.error?.message ||
                        err?.error ||
                        'Credential removal failed';

                    this.snack.open(
                        message,
                        'Close',
                        { duration: 5000 }
                    );
                }

            });

        });

    }

    hardDeleteCredential(row: UserBiometricDTO) {

        if (!this.adminMode || this.rowBusy[row.id]) {
            return;
        }

        const ref = this.dialog.open(
            ConfirmDialogComponent,
            {
                width: '420px',
                data: {
                    title: 'Hard Delete Credential',
                    message:
                        `Permanently delete "${row.deviceName}"? This cannot be undone.`,
                    confirmText: 'Delete',
                    cancelText: 'Cancel'
                }
            }
        );

        ref.afterClosed().subscribe(confirmed => {

            if (!confirmed) {
                return;
            }

            this.rowBusy[row.id] = true;

            this.api.adminDelete(
                row.id,
                true
            ).subscribe({

                next: () => {

                    delete this.rowBusy[row.id];

                    this.snack.open(
                        'Credential hard deleted',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                },

                error: (err) => {

                    delete this.rowBusy[row.id];

                    const message =
                        err?.error?.message ||
                        err?.error ||
                        'Hard delete failed';

                    this.snack.open(
                        message,
                        'Close',
                        { duration: 5000 }
                    );
                }

            });

        });

    }

    trackById(
        _: number, row: UserBiometricDTO
    ) {
        return row.id;
    }
}