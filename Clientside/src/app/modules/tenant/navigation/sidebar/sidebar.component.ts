import {
  Component,
  OnInit,
  inject
} from '@angular/core';
import { CommonModule, NgFor } from '@angular/common';
import {
  NavigationEnd,
  Router,
  RouterModule
} from '@angular/router';
import { filter } from 'rxjs';
import { MatListModule } from '@angular/material/list';
import { MatIconModule } from '@angular/material/icon';
import { SidebarService } from '../../../../core/services/sidebar.service';
import { MatTooltipModule } from '@angular/material/tooltip';
import { AuthService } from '../../../auth/services/auth.service';
import { TenantBrandingService } from '../../../../core/services/tenant-branding.service';
import { PermissionService } from '../../../../core/security/permission.service';

interface NavItem {
  title: string;
  route?: string;
  icon: string;
  feature?: string;

  children?: NavItem[];

  expanded?: boolean;

  collapsedExpanded?: boolean;
}

@Component({
  selector: 'app-sidebar',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    MatListModule,
    MatIconModule,
    NgFor,
    MatTooltipModule
  ],
  templateUrl: './sidebar.component.html',
  styleUrls: ['./sidebar.component.scss']
})
export class SidebarComponent implements OnInit {

  private branding = inject(TenantBrandingService);
  private perm = inject(PermissionService);

  nav: NavItem[] = [];
  logo$ = this.branding.logo$;

  constructor(
    public sidebar: SidebarService,
    private auth: AuthService,
    private router: Router
  ) {
    this.buildNav();
  }

  ngOnInit(): void {

    this.syncSidebarStateToRoute();

    this.router.events
      .pipe(
        filter(e => e instanceof NavigationEnd)
      )
      .subscribe(() => {

        this.syncSidebarStateToRoute();
      });

    this.sidebar.isCollapsed$
      .subscribe(() => {

        this.syncSidebarStateToRoute();
      });
  }

  private buildNav() {

    const items: NavItem[] = [

      { title: 'Dashboard', route: '/app/dashboard', icon: 'dashboard', feature: 'dashboard' },
      { title: 'Reports', route: '/app/reports', icon: 'reports', feature: 'reports' },

      {
        title: 'Stock',
        icon: 'stock',
        feature: 'stock',
        children: [
          {
            title: 'Products',
            route: '/app/stock',
            icon: 'inventory'
          },
          {
            title: 'Categories',
            route: '/app/categories',
            icon: 'categories'
          },
          {
            title: 'Stock Onboarding',
            route: '/app/stock-onboarding',
            icon: 'onboarding'
          }
        ]
      },

      { title: 'Sales', route: '/app/sales', icon: 'sales', feature: 'sales' },

      { title: 'Suppliers', route: '/app/suppliers', icon: 'suppliers', feature: 'suppliers' },
      { title: 'Customers', route: '/app/customers', icon: 'customers', feature: 'customers' },

      { title: 'Users', route: '/app/users', icon: 'users', feature: 'users' },
      { title: 'Branches', route: '/app/branches', icon: 'branches', feature: 'branches' },

      {
        title: 'Finance',
        icon: 'finance',
        feature: 'finance',
        children: [
          { title: 'Accounting', route: '/app/finance/accounting', icon: 'accounts' },
          { title: 'Tax', route: '/app/finance/tax', icon: 'tax' },
          { title: 'Payments', route: '/app/finance/ap/debts', icon: 'payments' }
        ]
      },

      {
        title: 'Devices',
        route: '/app/security/devices',
        icon: 'devices',
        feature: 'devices'
      }

    ];

    this.nav = items.filter(item => this.perm.can(item.feature ?? ''));

  }

  isSuperuser(): boolean {
    return this.auth.getSnapshot()?.role === 'SUPERUSER';
  }

  private collapseAllGroups() {

    this.nav.forEach(item => {
      item.expanded = false;
      item.collapsedExpanded = false;
    });

    this.sidebar.collapseRail();
  }

  syncSidebarStateToRoute() {

    const url =
      this.router.url;

    const activeGroup =
      this.nav.find(item =>
        item.children?.some(
          child => child.route === url
        )
      );

    /*
      RESET
    */

    this.nav.forEach(item => {

      item.expanded = false;
      item.collapsedExpanded = false;
    });

    /*
      NO ACTIVE GROUP
    */

    if (!activeGroup) {

      this.sidebar.collapseRail();

      return;
    }

    /*
      COLLAPSED DESKTOP
    */

    if (this.sidebar.collapsedSnapshot) {

      activeGroup.collapsedExpanded = true;

      this.sidebar.expandRail();

      return;
    }

    /*
      EXPANDED DESKTOP
    */

    activeGroup.expanded = true;

    this.sidebar.collapseRail();
  }

  toggleGroup(item: NavItem) {

    const collapsed =
      this.sidebar.collapsedSnapshot;

    /* =========================================
       COLLAPSED DESKTOP RAIL
    ========================================= */

    if (collapsed) {

      const opening =
        !item.collapsedExpanded;

      this.collapseAllGroups();

      item.collapsedExpanded =
        opening;

      if (opening) {
        this.sidebar.expandRail();
      } else {
        this.sidebar.collapseRail();
      }

      return;
    }

    /* =========================================
       EXPANDED DESKTOP
    ========================================= */

    const opening =
      !item.expanded;

    this.nav.forEach(x => {
      x.expanded = false;
    });

    item.expanded =
      opening;
  }

  onStandaloneNavigate() {
    this.collapseAllGroups();
  }

  onChildNavigate() {
  }


}