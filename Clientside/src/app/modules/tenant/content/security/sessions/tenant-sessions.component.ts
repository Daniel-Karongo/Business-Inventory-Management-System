import {
    CommonModule
} from '@angular/common';

import {
    Component,
    OnInit,
    inject
} from '@angular/core';

import {
    FormsModule
} from '@angular/forms';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    finalize
} from 'rxjs/operators';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatSnackBar
} from '@angular/material/snack-bar';

import {
    MatTooltipModule
} from '@angular/material/tooltip';

import {
    PageShellComponent
} from '../../../../../shared/layout/page-shell/page-shell.component';

import {
    SessionService
} from '../../../../../core/services/session.service';

import {
    UserSessionDto
} from '../../../../../core/models/session.models';

@Component({
    selector: 'app-tenant-sessions',
    standalone: true,
    imports: [
        CommonModule,
        FormsModule,
        PageShellComponent,
        MatButtonModule,
        MatIconModule,
        MatTableModule,
        MatPaginatorModule,
        MatTooltipModule
    ],
    templateUrl: './tenant-sessions.component.html',
    styleUrls: ['./tenant-sessions.component.scss']
})
export class TenantSessionsComponent
    implements OnInit {

    private sessionService =
        inject(SessionService);

    private snack =
        inject(MatSnackBar);

    loading = true;

    actionBusy = false;

    busyTokenId: string | null = null;

    sessions: UserSessionDto[] = [];

    paged: UserSessionDto[] = [];

    total = 0;

    page = 0;

    size = 10;

    displayedColumns = [
        'device',
        'branch',
        'browser',
        'os',
        'platform',
        'ip',
        'login',
        'actions'
    ];

    ngOnInit() {

        this.load();

    }

    load() {

        this.loading = true;

        this.sessionService
            .getActiveSessions()
            .subscribe({

                next: sessions => {

                    this.sessions = sessions;

                    this.total =
                        sessions.length;

                    this.applyPagination();

                    this.loading = false;

                },

                error: () => {

                    this.loading = false;

                }
            });
    }

    applyPagination() {

        const start =
            this.page * this.size;

        this.paged =
            this.sessions.slice(
                start,
                start + this.size
            );
    }

    changePage(
        event: PageEvent
    ) {

        this.page =
            event.pageIndex;

        this.size =
            event.pageSize;

        this.applyPagination();
    }

    logoutSession(
        session: UserSessionDto
    ) {

        if (
            this.actionBusy ||
            session.current
        ) {
            return;
        }

        this.busyTokenId =
            session.tokenId;

        this.actionBusy = true;

        this.sessionService
            .logoutSession(
                session.tokenId
            )
            .pipe(
                finalize(() => {

                    this.actionBusy = false;

                    this.busyTokenId = null;

                })
            )
            .subscribe({

                next: () => {

                    this.snack.open(
                        'Session terminated',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.load();
                }

            });
    }

    logoutOthers() {

        if (
            this.actionBusy ||
            this.sessions.length <= 1
        ) {
            return;
        }

        this.actionBusy = true;

        this.sessionService
            .logoutOtherSessions()
            .pipe(
                finalize(() => {

                    this.actionBusy = false;

                })
            )
            .subscribe({

                next: () => {

                    this.snack.open(
                        'Other sessions logged out',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.load();
                }

            });
    }
}