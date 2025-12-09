import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd, RouterOutlet } from '@angular/router';
import { AsyncPipe, CommonModule } from '@angular/common';
import { filter } from 'rxjs';

import { SidebarComponent } from '../../navigation/sidebar/sidebar.component';
import { TopbarComponent } from '../../navigation/topbar/topbar.component';
import { SidebarService } from '../../services/sidebar.service';
import { BreadcrumbComponent } from '../../../shared/components/breadcrumb/breadcrumb.component';

@Component({
  selector: 'app-main-layout',
  standalone: true,
  imports: [
    CommonModule,
    RouterOutlet,
    SidebarComponent,
    TopbarComponent,
    BreadcrumbComponent,
    AsyncPipe
  ],
  templateUrl: './main-layout.component.html',
  styleUrls: ['./main-layout.component.scss']
})
export class MainLayoutComponent implements OnInit {

  currentPage = '';

  constructor(
    public sidebar: SidebarService,
    private router: Router
  ) {}

  ngOnInit(): void {
    // Update currentPage when route changes
    this.router.events
      .pipe(filter(event => event instanceof NavigationEnd))
      .subscribe(() => this.updateCurrentPage());
  }

  private updateCurrentPage(): void {
    const urlSegments = this.router.url.split('/').filter(seg => seg);

    if (urlSegments.length === 0) {
      this.currentPage = 'Dashboard';
      return;
    }

    // Use the last segment for the page title (e.g. /users/create → Create)
    const last = urlSegments[urlSegments.length - 1];

    this.currentPage = this.formatSegment(last);
  }

  private formatSegment(seg: string): string {
    // Convert route segment -> readable text
    // "user-details" → "User Details"
    // "edit" → "Edit"
    return seg
      .replace(/-/g, ' ')
      .replace(/\b\w/g, c => c.toUpperCase());
  }
}