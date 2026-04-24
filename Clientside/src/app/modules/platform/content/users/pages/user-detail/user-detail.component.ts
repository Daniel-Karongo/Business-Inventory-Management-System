import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { PlatformUserApiService } from '../../services/platform-user-api.service';

import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { PlatformUser } from '../../models/user.model';

@Component({
    selector: 'app-user-detail',
    standalone: true,
    imports: [
        CommonModule,
        PageShellComponent,
        MatButtonModule,
        MatSnackBarModule,
        RouterModule
    ],
    templateUrl: './user-detail.component.html',
    styleUrls: ['./user-detail.component.scss']
})
export class UserDetailComponent implements OnInit {

    private api = inject(PlatformUserApiService);
    private route = inject(ActivatedRoute);
    private router = inject(Router);
    private snack = inject(MatSnackBar);

    user?: PlatformUser;

    loading = true;
    actionBusy = false;

    private userId = '';

    ngOnInit() {
        const id =
            this.route.snapshot.paramMap.get('id');
        if (!id) {
            this.router.navigate(['/platform/users']);
            return;
        }
        this.userId = id;
        this.load();

    }

    load() {
        this.loading = true;
        this.api.getById(
            this.userId
        ).subscribe({
            next: u => {
                this.user = u;
                this.loading = false;
            },
            error: () => {
                this.loading = false;
                this.snack.open(
                    'User load failed',
                    'Close',
                    { duration: 4000 }
                );
            }
        });

    }

    canModify(): boolean {
        return !!this.user &&
            this.user.role !== 'PLATFORM_SUPER_ADMIN';

    }

    lock() {
        if (
            !this.user ||
            !this.canModify() ||
            this.actionBusy
        ) {
            return;
        }
        this.actionBusy = true;
        this.api.lock(
            this.user.id
        ).subscribe({
            next: () => {
                this.actionBusy = false;
                this.snack.open(
                    'User locked',
                    'Close',
                    { duration: 3000 }
                );
                this.load();
            },
            error: () => {
                this.actionBusy = false;
                this.snack.open(
                    'Lock failed',
                    'Close',
                    { duration: 4000 }
                );
            }
        });

    }

    unlock() {
        if (
            !this.user ||
            this.actionBusy
        ) {
            return;
        }
        this.actionBusy = true;
        this.api.unlock(
            this.user.id
        ).subscribe({
            next: () => {
                this.actionBusy = false;
                this.snack.open(
                    'User unlocked',
                    'Close',
                    { duration: 3000 }
                );
                this.load();
            },
            error: () => {
                this.actionBusy = false;
                this.snack.open(
                    'Unlock failed',
                    'Close',
                    { duration: 4000 }
                );
            }
        });

    }

    back() {
        this.router.navigate(
            ['/platform/users']
        );

    }

    status(): string {
        if (!this.user) {
            return '';
        }
        if (this.user.locked) {
            return 'LOCKED';
        }
        if (!this.user.active) {
            return 'DISABLED';
        }
        return 'ACTIVE';

    }
}