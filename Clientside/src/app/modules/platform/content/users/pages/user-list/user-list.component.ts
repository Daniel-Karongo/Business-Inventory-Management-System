import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';

import { PlatformUserApiService } from '../../services/platform-user-api.service';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { PlatformUser } from '../../models/user.model';

@Component({
    selector: 'app-user-list',
    standalone: true,
    templateUrl: './user-list.component.html',
    styleUrls: ['./user-list.component.scss'],
    imports: [
        CommonModule,
        PageShellComponent,
        MatTableModule,
        MatPaginatorModule,
        MatButtonModule,
        MatButtonToggleModule,
        MatIconModule,
        MatSnackBarModule,
        MatTooltipModule
    ]
})
export class UserListComponent implements OnInit {

    private api = inject(PlatformUserApiService);
    private router = inject(Router);
    private snack = inject(MatSnackBar);

    users: PlatformUser[] = [];
    paged: PlatformUser[] = [];

    page = 0;
    size = 10;
    total = 0;

    compactView = false;

    viewMode: 'table' | 'cards' = 'table';

    displayedColumns = [
        'username',
        'role',
        'status',
        'createdAt',
        'actions'
    ];

    stats = {
        active: 0,
        locked: 0,
        superAdmins: 0,
        total: 0
    };

    ngOnInit() {

        const savedView =
            localStorage.getItem(
                'platform.users.view'
            );

        if (
            savedView === 'table' ||
            savedView === 'cards'
        ) {
            this.viewMode = savedView;
        }

        this.load();

    }

    load() {

        this.api.listAll().subscribe({
            next: users => {
                this.users = users;
                this.computeStats();
                this.applyPagination();
            },
            error: () => {

                this.snack.open(
                    'Failed loading users',
                    'Close',
                    { duration: 4000 }
                );

            }
        });
    }

    computeStats() {

        this.stats.total =
            this.users.length;

        this.stats.locked =
            this.users.filter(
                x => x.locked
            ).length;

        this.stats.active =
            this.users.filter(
                x => x.active && !x.locked
            ).length;

        this.stats.superAdmins =
            this.users.filter(
                x => x.role === 'PLATFORM_SUPER_ADMIN'
            ).length;

    }

    applyPagination() {

        this.total =
            this.users.length;

        const start =
            this.page * this.size;

        this.paged =
            this.users.slice(
                start,
                start + this.size
            );

    }

    changePage(
        e: PageEvent
    ) {

        this.page = e.pageIndex;
        this.size = e.pageSize;

        this.applyPagination();

    }

    setView(
        mode: 'table' | 'cards'
    ) {

        this.viewMode = mode;

        localStorage.setItem(
            'platform.users.view',
            mode
        );

    }

    toggleDensity() {

        this.compactView =
            !this.compactView;

        this.size =
            this.compactView
                ? 25
                : 10;

        this.applyPagination();

    }

    openCreate() {

        this.router.navigate(
            ['/platform/users/create']
        );

    }

    openUser(id: string) {

        this.router.navigate(
            ['/platform/users', id]
        );

    }

    lock(
        u: PlatformUser
    ) {

        if (
            u.role === 'PLATFORM_SUPER_ADMIN'
        ) {
            return;
        }

        this.api.lock(
            u.id
        ).subscribe(
            () => this.load()
        );

    }

    unlock(
        u: PlatformUser
    ) {

        this.api.unlock(
            u.id
        ).subscribe(
            () => this.load()
        );

    }

    status(
        u: PlatformUser
    ) {

        if (u.locked) {
            return 'LOCKED';
        }

        if (!u.active) {
            return 'DISABLED';
        }

        return 'ACTIVE';

    }

}