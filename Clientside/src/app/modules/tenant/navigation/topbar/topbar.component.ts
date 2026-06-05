import { CommonModule } from '@angular/common';
import { Component, HostListener, OnInit } from '@angular/core';
import { NavigationEnd, Router } from '@angular/router';
import { filter } from 'rxjs';

import { MatBadgeModule } from '@angular/material/badge';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';
import { MatToolbarModule } from '@angular/material/toolbar';

import { SidebarService } from '../../../../core/services/sidebar.service';
import { ThemeService } from '../../../../core/services/theme.service';
// import { NotificationButtonComponent } from './notification-button/notification-button.component';
import { BreakpointObserver } from '@angular/cdk/layout';
import { MatDialog } from '@angular/material/dialog';
import { MatTooltipModule } from '@angular/material/tooltip';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';
import { NavigationHistoryService } from '../../../../core/services/navigation-history.service';
import { AuthService } from '../../../auth/services/auth.service';
import { BranchDTO } from '../../content/branches/models/branch.model';
import { BranchService } from '../../content/branches/services/branch.service';
import { LogoutChoiceDialogComponent } from './logout-choice-dialog/logout-choice-dialog.component';

@Component({
  selector: 'app-topbar',
  standalone: true,
  imports: [
    CommonModule,
    MatToolbarModule,
    MatMenuModule,
    MatButtonModule,
    MatIconModule,
    MatBadgeModule,
    MatTooltipModule
    // NotificationButtonComponent
  ],
  templateUrl: './topbar.component.html',
  styleUrls: ['./topbar.component.scss']
})
export class TopbarComponent implements OnInit {

  currentPage = '';

  branch = 'Main Branch';
  branchMenu = false;
  notificationsCount = 3;
  currentUser?: {
    username: string;
    role: string;
    branch: { id: string; name: string };
  };
  branches: BranchDTO[] = [];
  selectedBranchId?: string;
  selectedBranchName = 'Select branch';
  isSmallScreen = false;
  isMobileLayout = false;

  constructor(
    public sidebar: SidebarService,
    public theme: ThemeService,
    private branchService: BranchService,
    private router: Router,
    private auth: AuthService,
    private idle: IdleLogoutService,
    private dialog: MatDialog,
    private navHistory: NavigationHistoryService,
    private breakpoint: BreakpointObserver
  ) { }

  ngOnInit(): void {
    this.loadUser();
    this.loadBranches();

    this.updatePageTitle(); // ✅ handle refresh

    this.router.events
      .pipe(filter(e => e instanceof NavigationEnd))
      .subscribe(() => this.updatePageTitle());

    this.breakpoint
      .observe('(max-width: 768px)')
      .subscribe(res => {

        this.isMobileLayout =
          res.matches;

        this.isSmallScreen =
          res.matches;

      });
  }

  private loadBranches() {
    this.branchService.getAllLegacy().subscribe(list => {
      this.branches = list;

      // 🔑 recompute AFTER branches load
      this.updateSelectedBranchName();
    });
  }

  private loadUser() {
    this.auth.getCurrentUser().subscribe(user => {
      if (!user) return;

      this.currentUser = {
        username: user.username,
        role: user.role,
        branch: { id: user.branchId, name: '' }
      };

      this.selectedBranchId = user.branchId;

      // 🔑 recompute AFTER user arrives
      this.updateSelectedBranchName();
    });
  }

  private updateSelectedBranchName() {
    if (!this.selectedBranchId || !this.branches.length) {
      this.selectedBranchName = 'Select branch';
      return;
    }

    const found = this.branches.find(b => b.id === this.selectedBranchId);
    this.selectedBranchName = found?.name ?? 'Select branch';
  }


  get activeBranchName(): string {
    if (!this.currentUser || !this.branches.length) {
      return '—';
    }

    return (
      this.branches.find(b => b.id === this.currentUser?.branch.id)?.name
      ?? '—'
    );
  }

  private updatePageTitle(): void {
    const cleanUrl =
      this.router.url.split('?')[0];

    const segments =
      cleanUrl.split('/').filter(Boolean);

    if (!segments.length) {
      this.currentPage = 'Dashboard';
      return;
    }

    const last =
      segments[segments.length - 1];

    this.currentPage =
      this.formatSegment(last);
  }

  private formatSegment(seg: string): string {
    return decodeURIComponent(seg)
      .replace(/-/g, ' ')
      .replace(/\b\w/g, char => char.toUpperCase());
  }

  onNotificationsClicked() {
    console.log('Notifications clicked');
  }

  toggleSidebar() {

    if (this.isMobileLayout) {

      this.sidebar.toggleMobile();

      return;
    }

    this.sidebar.toggleDesktopCollapsed();
  }

  goBack() {
    const prev = this.navHistory.back();

    if (prev) {
      this.router.navigateByUrl(prev);
    } else {
      this.router.navigate(['/app/dashboard']);
    }
  }

  toggleBranchMenu(event?: MouseEvent) {
    if (event) event.stopPropagation();
    this.branchMenu = !this.branchMenu;
  }

  @HostListener('document:click', ['$event'])
  closeDropdown(event: MouseEvent) {

    const target =
      event.target as HTMLElement;

    if (
      !target.closest('.branch-menu-trigger') &&
      !target.closest('.branch-menu-panel')
    ) {
      this.branchMenu = false;
    }
  }

  setTheme(pref: 'light' | 'dark' | 'system') {
    this.theme.setPreference(pref);
  }

  goToProfile() {
    if (!this.currentUser?.username) return;

    this.router.navigate([
      '/app/users',
      this.currentUser.username
    ]);
  }

  logout() {

    this.idle.stop();

    const ref =
      this.dialog.open(
        LogoutChoiceDialogComponent,
        {
          width: '1000px',
          maxWidth: '95vw',
          panelClass:
            'enterprise-dialog'
        }
      );

    ref.afterClosed()
      .subscribe(choice => {

        if (!choice) {
          return;
        }

        switch (choice) {

          case 'current':
            this.finalizeLogoutCurrent();
            break;

          case 'others':
            this.finalizeLogoutOthers();
            break;

          case 'all':
            this.finalizeLogoutAll();
            break;
        }
      });
  }

  private finalizeLogoutCurrent() {
    this.auth.logout().subscribe(() => {
      this.auth.clearLocalState();

      this.router.navigate(
        ['/auth'],
        { replaceUrl: true }
      );
    });
  }

  private finalizeLogoutOthers() {

    this.auth
      .logoutOtherSessions()
      .subscribe({

        next: () => {

          this.router.navigate(
            ['/app/security/sessions']
          );

        },

        error: () => {

          this.router.navigate(
            ['/app/security/sessions']
          );

        }
      });
  }

  private finalizeLogoutAll() {
    this.auth.logoutAll().subscribe(() => {
      this.auth.clearLocalState();

      this.router.navigate(
        ['/auth'],
        { replaceUrl: true }
      );
    });
  }
}
