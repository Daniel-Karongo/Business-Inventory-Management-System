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
import { NotificationButtonComponent } from './notification-button/notification-button.component';

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
    NotificationButtonComponent
  ],
  templateUrl: './topbar.component.html',
  styleUrls: ['./topbar.component.scss']
})
export class TopbarComponent implements OnInit {

  currentPage = '';

  branch = 'Main Branch';
  branches = ['Main Branch', 'Warehouse', 'Remote'];
  branchMenu = false;
  notificationsCount = 3;

  constructor(
    public sidebar: SidebarService,
    public theme: ThemeService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.router.events
      .pipe(filter(e => e instanceof NavigationEnd))
      .subscribe(() => this.updatePageTitle());

    this.updatePageTitle(); // initialize on load
  }

  private updatePageTitle(): void {
    const segments = this.router.url.split('/').filter(s => s);

    if (segments.length === 0) {
      this.currentPage = 'Dashboard';
      return;
    }

    const last = segments[segments.length - 1];
    this.currentPage = this.formatSegment(last);
  }

  private formatSegment(seg: string): string {
    // /users/123/edit → "Edit"
    // /departments/department-details → "Department Details"
    return seg
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

  selectBranch(b: string) {
    this.branch = b;
    this.branchMenu = false;
  }

  @HostListener('document:click')
  closeDropdown() {
    this.branchMenu = false;
  }

  setTheme(pref: 'light' | 'dark' | 'system') {
    this.theme.setPreference(pref);
  }

  logout() {
    console.log('logout');
  }
}