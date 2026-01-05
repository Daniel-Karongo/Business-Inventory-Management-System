import { Component, HostListener, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, NavigationEnd } from '@angular/router';
import { filter } from 'rxjs';

import { MatToolbarModule } from '@angular/material/toolbar';
import { MatMenuModule } from '@angular/material/menu';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatBadgeModule } from '@angular/material/badge';

import { SidebarService } from '../../services/sidebar.service';
import { ThemeService } from '../../services/theme.service';
// import { NotificationButtonComponent } from './notification-button/notification-button.component';
import { AuthService } from '../../../modules/auth/services/auth.service';
import { IdleLogoutService } from '../../services/IdleLogoutService';
import { MatDialog } from '@angular/material/dialog';
import { LogoutChoiceDialogComponent } from './logout-choice-dialog/logout-choice-dialog.component';
import { BranchDTO } from '../../../modules/branches/models/branch.model';
import { BranchService } from '../../../modules/branches/services/branch.service';

@Component({
  selector: 'app-topbar',
  standalone: true,
  imports: [
    CommonModule,
    MatToolbarModule,
    MatMenuModule,
    MatButtonModule,
    MatIconModule,
    MatBadgeModule
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

  constructor(
    public sidebar: SidebarService,
    public theme: ThemeService,
    private branchService: BranchService,
    private router: Router,
    private auth: AuthService,
    private idle: IdleLogoutService,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.loadUser();
    this.loadBranches();

    this.updatePageTitle(); // ✅ handle refresh

    this.router.events
      .pipe(filter(e => e instanceof NavigationEnd))
      .subscribe(() => this.updatePageTitle());
  }

  private loadBranches() {
    this.branchService.getAll(false).subscribe(list => {
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
    const segments = this.router.url.split('/').filter(Boolean);

    if (segments.length === 0) {
      this.currentPage = 'Dashboard';
      return;
    }

    const last = segments[segments.length - 1];
    this.currentPage = this.formatSegment(last);
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
    this.sidebar.toggle();
  }

  toggleBranchMenu(event?: MouseEvent) {
    if (event) event.stopPropagation();
    this.branchMenu = !this.branchMenu;
  }

  @HostListener('document:click')
  closeDropdown() {
    this.branchMenu = false;
  }

  setTheme(pref: 'light' | 'dark' | 'system') {
    this.theme.setPreference(pref);
  }

  goToProfile() {
    if (!this.currentUser?.username) return;
    this.router.navigate(['/users', this.currentUser.username]);
  }

  logout() {
    this.idle.stop();

    this.auth.getSessions().subscribe({
      next: sessions => {
        if (sessions.length <= 1) {
          this.finalizeLogoutCurrent();
          return;
        }

        const ref = this.dialog.open(LogoutChoiceDialogComponent, {
          width: '440px',
          disableClose: false, // ✅ allow outside click / ESC
          data: { count: sessions.length }
        });

        ref.afterClosed().subscribe(choice => {
          if (choice === 'all') {
            this.finalizeLogoutAll();
          } else if (choice === 'current') {
            this.finalizeLogoutCurrent();
          }
          // ✅ undefined (backdrop / ESC) → DO NOTHING
        });
      },
      error: () => this.finalizeLogoutCurrent()
    });
  }

  private finalizeLogoutCurrent() {
    this.auth.logout().subscribe(() => {
      this.auth.clearLocalState();
      this.router.navigate(['/login']);
    });
  }

  private finalizeLogoutAll() {
    this.auth.logoutAll().subscribe(() => {
      this.auth.clearLocalState();
      this.router.navigate(['/login']);
    });
  }
}