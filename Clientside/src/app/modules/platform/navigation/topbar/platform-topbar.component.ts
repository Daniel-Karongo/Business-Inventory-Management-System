import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';

import {
    Router,
    NavigationEnd
} from '@angular/router';

import { filter } from 'rxjs';

import { MatToolbarModule } from '@angular/material/toolbar';
import { MatMenuModule } from '@angular/material/menu';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';
import { SidebarService } from '../../../../core/services/sidebar.service';
import { ThemeService } from '../../../../core/services/theme.service';
import { AuthService } from '../../../auth/services/auth.service';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';
import { LogoutChoiceDialogComponent } from '../../../tenant/navigation/topbar/logout-choice-dialog/logout-choice-dialog.component';

@Component({
    selector: 'app-platform-topbar',
    standalone: true,
    imports: [
        CommonModule,
        MatToolbarModule,
        MatMenuModule,
        MatIconModule,
        MatButtonModule
    ],
    templateUrl: './platform-topbar.component.html',
    styleUrls: ['./platform-topbar.component.scss']
})
export class PlatformTopbarComponent implements OnInit {

    currentPage = 'Dashboard';

    currentUser?: {
        username: string;
        role: string;
    };

    constructor(
        public sidebar: SidebarService,
        public theme: ThemeService,
        private auth: AuthService,
        private idle: IdleLogoutService,
        private router: Router,
        private dialog: MatDialog
    ) { }

    ngOnInit() {

        this.auth.getCurrentUser()
            .subscribe(user => {

                if (!user) return;

                this.currentUser = {
                    username: user.username,
                    role: user.role
                };
            });

        this.updateTitle();

        this.router.events
            .pipe(
                filter(e => e instanceof NavigationEnd)
            )
            .subscribe(() => this.updateTitle());
    }

    private updateTitle() {

        let route =
            this.router.routerState.root;

        while (
            route.firstChild
        ) {
            route =
                route.firstChild;
        }

        const title =
            route.snapshot.data?.['title'];

        if (title) {

            this.currentPage =
                title;

            return;
        }

        const parts =
            this.router.url
                .split('/')
                .filter(Boolean);

        this.currentPage =
            parts.length
                ? this.format(
                    parts[
                    parts.length - 1
                    ]
                )
                : 'Dashboard';
    }

    private format(v: string) {
        return v
            .replace(/-/g, ' ')
            .replace(/\b\w/g, c => c.toUpperCase());
    }

    toggleSidebar() {
        this.sidebar.toggle();
    }

    setTheme(v: 'light' | 'dark' | 'system') {
        this.theme.setPreference(v);
    }

    goProfile() {
        const me =
            this.auth.getSnapshot();
        if (!me || me.userType !== 'PLATFORM') {
            return;
        }

        this.router.navigate(['/platform/users', me.userId]);
    }

    logout() {

        this.idle.stop();

        this.auth.getSessions().subscribe({

            next: sessions => {

                if (sessions.length <= 1) {
                    this.finalizeCurrent();
                    return;
                }

                const ref = this.dialog.open(
                    LogoutChoiceDialogComponent, {
                    width: '440px',
                    data: { count: sessions.length }
                }
                );

                ref.afterClosed()
                    .subscribe(choice => {

                        if (choice === 'all') {
                            this.finalizeAll();
                        }

                        if (choice === 'current') {
                            this.finalizeCurrent();
                        }

                    });

            },

            error: () => this.finalizeCurrent()

        });

    }

    private finalizeCurrent() {

        this.auth.logout()
            .subscribe(() => {

                this.auth.clearLocalState();

                this.router.navigate(
                    ['/auth'],
                    { replaceUrl: true }
                );

            });

    }

    private finalizeAll() {

        this.auth.logoutAll()
            .subscribe(() => {

                this.auth.clearLocalState();

                this.router.navigate(
                    ['/auth'],
                    { replaceUrl: true }
                );

            });

    }

}