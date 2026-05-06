import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd, RouterOutlet } from '@angular/router';
import { AsyncPipe, CommonModule } from '@angular/common';
import {
  Observable,
  combineLatest,
  distinctUntilChanged,
  filter,
  map
} from 'rxjs';

import { SidebarComponent } from '../../navigation/sidebar/sidebar.component';
import { SidebarService } from '../../../../core/services/sidebar.service';
import { BreadcrumbComponent } from '../../../../shared/components/breadcrumb/breadcrumb.component';
import { TopbarComponent } from '../../navigation/topbar/topbar.component';
import { NavigationHistoryService } from '../../../../core/services/navigation-history.service';
import { BreakpointObserver } from '@angular/cdk/layout';

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
  vm$!: Observable<{
    mobileOpen: boolean;
    collapsed: boolean;
    railExpanded: boolean;
  }>;

  constructor(
    public sidebar: SidebarService,
    private router: Router,
    private navHistory: NavigationHistoryService,
    private breakpoint: BreakpointObserver
  ) {
    this.vm$ = combineLatest([
      this.sidebar.mobileOpen$,
      this.sidebar.isCollapsed$,
      this.sidebar.railExpanded$
    ]).pipe(
      map(([mobileOpen, collapsed, railExpanded]) => ({
        mobileOpen,
        collapsed,
        railExpanded
      }))
    );
  }

  ngOnInit(): void {

    this.breakpoint
      .observe('(max-width: 768px)')
      .pipe(
        map(res => res.matches),
        distinctUntilChanged()
      )
      .subscribe(isMobile => {

        /* ================================
           MOBILE
        ================================= */

        if (isMobile) {

          if (!this.sidebar.mobileModeSnapshot) {

            this.sidebar.setMobileMode(true);

            /* OPEN ONLY ON MODE ENTRY */

            this.sidebar.openMobile();
          }

          return;
        }

        /* ================================
           DESKTOP
        ================================= */

        if (this.sidebar.mobileModeSnapshot) {

          this.sidebar.setMobileMode(false);

          this.sidebar.closeMobile();
        }
      });

    this.router.events
      .pipe(filter(e => e instanceof NavigationEnd))
      .subscribe((e: NavigationEnd) => {

        this.navHistory.push(
          e.urlAfterRedirects
        );

        this.updateCurrentPage();
      });
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
