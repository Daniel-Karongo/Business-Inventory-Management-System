import {
    Component,
    Inject,
    inject
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    MAT_DIALOG_DATA,
    MatDialogModule,
    MatDialogRef
} from '@angular/material/dialog';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    UserSessionDto
} from '../../../../core/models/session.models';

import {
    SessionService
} from '../../../../core/services/session.service';

@Component({
    selector: 'app-session-recovery-dialog',
    standalone: true,
    imports: [
        CommonModule,
        MatDialogModule,
        MatButtonModule,
        MatIconModule
    ],
    templateUrl: './session-recovery-dialog.component.html',
    styleUrls: ['./session-recovery-dialog.component.scss']
})
export class SessionRecoveryDialogComponent {

    private sessionService =
        inject(SessionService);

    busy = false;

    constructor(
        private dialogRef:
            MatDialogRef<SessionRecoveryDialogComponent>,

        @Inject(MAT_DIALOG_DATA)
        public data: {
            identifier: string;
            password: string;
            branchId: string | null;
            sessions: UserSessionDto[];
        }
    ) {
    }

    terminate(
        session: UserSessionDto
    ) {

        if (this.busy) {
            return;
        }

        this.busy = true;

        this.sessionService
            .logoutRecoverySession({
                identifier: this.data.identifier,
                password: this.data.password,
                branchId: this.data.branchId,
                tokenId: session.tokenId
            })
            .subscribe({
                next: () => {

                    this.dialogRef.close(true);

                },
                error: () => {

                    this.busy = false;

                }
            });
    }

    cancel() {

        this.dialogRef.close(false);

    }
}