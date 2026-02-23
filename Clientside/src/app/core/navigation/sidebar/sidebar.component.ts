import { Component } from '@angular/core';
import { CommonModule, NgFor } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatListModule } from '@angular/material/list';
import { MatIconModule } from '@angular/material/icon';
import { SidebarService } from '../../services/sidebar.service';
import { MatTooltipModule } from '@angular/material/tooltip';
import { AuthService } from '../../../modules/auth/services/auth.service';

interface NavItem {
  title: string;
  route?: string;
  icon: string;
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

  nav: NavItem[] = [];

  constructor(
    public sidebar: SidebarService,
    private auth: AuthService
  ) {
    this.buildNav();
  }

  private buildNav() {

    const role = this.auth.getSnapshot()?.role;

    const baseNav: NavItem[] = [
      { title: 'Dashboard', route: '/dashboard', icon: 'dashboard' },
      { title: 'Reports', route: '/reports', icon: 'reports' }
    ];

    const financeNav: NavItem = {
      title: 'Finance',
      icon: 'finance',
      expanded: false,
      children: [
        {
          title: 'Accounting',
          route: '/finance/accounting',
          icon: 'accounts'
        },
        {
          title: 'Budgeting',
          route: '/finance/budgeting',
          icon: 'budget'
        },
        {
          title: 'Tax',
          route: '/finance/tax',
          icon: 'tax'
        },
        {
          title: 'Payments',
          route: '/finance/payments',
          icon: 'payments'
        }
      ]
    };

    const otherNav: NavItem[] = [
      { title: 'Inventory', route: '/inventory', icon: 'inventory' },
      { title: 'Products', route: '/products', icon: 'products' },
      { title: 'Suppliers', route: '/suppliers', icon: 'suppliers' },
      { title: 'Sales', route: '/sales', icon: 'sales' },
      { title: 'Customers', route: '/customers', icon: 'customers' },
      { title: 'Users', route: '/users', icon: 'users' },
      { title: 'Branches', route: '/branches', icon: 'branches' },
      { title: 'Departments', route: '/departments', icon: 'departments' }
    ];

    const allowedFinanceRoles = ['SUPERUSER', 'ADMIN', 'MANAGER'];

    this.nav = [
      ...baseNav,
      ...(allowedFinanceRoles.includes(role ?? '') ? [financeNav] : []),
      ...otherNav
    ];
  }

  isSuperuser(): boolean {
    return this.auth.getSnapshot()?.role === 'SUPERUSER';
  }

  toggleGroup(item: NavItem) {
    item.expanded = !item.expanded;
  }
}