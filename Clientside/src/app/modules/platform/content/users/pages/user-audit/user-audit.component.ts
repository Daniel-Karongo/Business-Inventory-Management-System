import {
    Component,
    OnInit,
    inject
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    ActivatedRoute,
    Router
} from '@angular/router';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatSnackBar,
    MatSnackBarModule
} from '@angular/material/snack-bar';

import {
    PlatformUserApiService
}
    from '../../services/platform-user-api.service';

import {
    PageShellComponent
}
    from '../../../../../../shared/layout/page-shell/page-shell.component';
import { PlatformUserAudit, PlatformUserAuditPage } from '../../models/user.model';

@Component({
    standalone: true,
    selector: 'app-user-audit',
    templateUrl: './user-audit.component.html',
    styleUrls: ['./user-audit.component.scss'],
    imports: [
        CommonModule,
        PageShellComponent,
        MatTableModule,
        MatPaginatorModule,
        MatButtonModule,
        MatSnackBarModule
    ]
})
export class UserAuditComponent
    implements OnInit {

    private api =
        inject(
            PlatformUserApiService
        );

    private route =
        inject(
            ActivatedRoute
        );

    private router =
        inject(
            Router
        );

    private snack =
        inject(
            MatSnackBar
        );

    rows:
        PlatformUserAudit[] = [];

    page = 0;

    size = 20;

    total = 0;

    displayedColumns = [
        'timestamp',
        'action',
        'reason',
        'performedBy'
    ];

    ngOnInit() {
        this.load();
    }

    load() {

        this.api.audit(
            this.page,
            this.size
        )
            .subscribe({

                next: (res: PlatformUserAuditPage) => {

                    const id =
                        this.route.snapshot
                            .paramMap
                            .get('id');

                    if (id) {

                        this.rows =
                            (res.content || [])
                                .filter(
                                    x => x.userId === id
                                );

                    }
                    else {

                        this.rows =
                            res.content || [];

                    }

                    this.total =
                        res.totalElements || 0;

                },

                error: () => {

                    this.snack.open(
                        'Failed loading audit',
                        'Close',
                        { duration: 4000 }
                    );

                }

            });

    }

    changePage(
        e: PageEvent
    ) {

        this.page =
            e.pageIndex;

        this.size =
            e.pageSize;

        this.load();

    }

    back() {

        const id =
            this.route.snapshot
                .paramMap
                .get('id');

        if (id) {

            this.router.navigate([
                '/platform/users',
                id
            ]);

        }

    }

    actionClass(
        action: string
    ) {

        if (
            action === 'DELETE'
        ) {
            return 'danger';
        }

        if (
            action === 'LOCK'
        ) {
            return 'warn';
        }

        return 'ok';

    }

}