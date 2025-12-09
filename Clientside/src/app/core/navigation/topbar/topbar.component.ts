import { Component, HostListener } from '@angular/core';
import { CommonModule } from '@angular/common';
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
export class TopbarComponent {
  branch = 'Main Branch';
  branches = ['Main Branch', 'Warehouse', 'Remote'];
  branchMenu = false;
  notificationsCount = 3;

  constructor(public sidebar: SidebarService, public theme: ThemeService) {}

  onNotificationsClicked() {
    // open your notifications panel / navigate / mark-read / etc.
    console.log('Notifications clicked');
    // example: this.router.navigate(['/notifications']);
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

  setTheme(pref: 'light'|'dark'|'system') {
    this.theme.setPreference(pref);
  }

  logout() {
    console.log('logout');
  }
}