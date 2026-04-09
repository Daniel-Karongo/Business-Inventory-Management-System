import { Component, inject } from '@angular/core';
import { CommonModule, NgFor } from '@angular/common';
import { RouterModule } from '@angular/router';
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
export class SidebarComponent {

  private branding = inject(TenantBrandingService);
  private perm = inject(PermissionService);

  nav: NavItem[] = [];
  logo$ = this.branding.logo$;

  constructor(
    public sidebar: SidebarService,
    private auth: AuthService,
  ) {
    this.buildNav();
  }

  private buildNav() {

    const items: NavItem[] = [

      { title: 'Dashboard', route: '/app/dashboard', icon: 'dashboard', feature: 'dashboard' },
      { title: 'Reports', route: '/app/reports', icon: 'reports', feature: 'reports' },

      { title: 'Categories', route: '/app/categories', icon: 'categories', feature: 'categories' },
      { title: 'Products', route: '/app/products', icon: 'products', feature: 'products' },
      { title: 'Inventory', route: '/app/inventory', icon: 'inventory', feature: 'inventory' },
      { title: 'Sales', route: '/app/sales', icon: 'sales', feature: 'sales' },

      { title: 'Suppliers', route: '/app/suppliers', icon: 'suppliers', feature: 'suppliers' },
      { title: 'Customers', route: '/app/customers', icon: 'customers', feature: 'customers' },

      { title: 'Users', route: '/app/users', icon: 'users', feature: 'users' },
      { title: 'Branches', route: '/app/branches', icon: 'branches', feature: 'branches' },
      { title: 'Departments', route: '/app/departments', icon: 'departments', feature: 'departments' },

      { title: 'Accounts', route: '/app/finance/accounting', icon: 'accounts', feature: 'accounts' },
      { title: 'Payments', route: '/app/finance/payments', icon: 'payments', feature: 'payments' },

      {
        title: 'Finance',
        icon: 'finance',
        feature: 'finance',
        children: [
          { title: 'Accounting', route: '/app/finance/accounting', icon: 'accounts' },
          { title: 'Budgeting', route: '/app/finance/budgeting', icon: 'budget' },
          { title: 'Tax', route: '/app/finance/tax', icon: 'tax' },
          { title: 'Payments', route: '/app/finance/payments', icon: 'payments' }
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

  toggleGroup(item: NavItem) {
    item.expanded = !item.expanded;
  }
}