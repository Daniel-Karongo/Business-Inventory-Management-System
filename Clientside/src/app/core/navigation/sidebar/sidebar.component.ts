import { Component } from '@angular/core';
import { CommonModule, NgFor } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatListModule } from '@angular/material/list';
import { MatIconModule } from '@angular/material/icon';
import { SidebarService } from '../../services/sidebar.service';
import { MatTooltipModule } from '@angular/material/tooltip';

interface NavItem {
  title: string;
  route: string;
  icon: string; // svg name without extension
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

  nav: NavItem[] = [
    { title: 'Dashboard',  route: '/dashboard',  icon: 'dashboard' },
    { title: 'Inventory',  route: '/inventory',  icon: 'inventory' },
    { title: 'Products',   route: '/products',   icon: 'products' },
    { title: 'Suppliers',  route: '/suppliers',  icon: 'suppliers' },
    { title: 'Sales',      route: '/sales',      icon: 'sales' },
    { title: 'Payments',   route: '/payments',   icon: 'payments' },
    { title: 'Customers',  route: '/customers',  icon: 'customers' },
    { title: 'Accounts',   route: '/accounts',   icon: 'accounts' },
    { title: 'Users',      route: '/users',      icon: 'users' },
    { title: 'Branches',   route: '/branches',   icon: 'branches' },
    { title: 'Departments',   route: '/departments',   icon: 'departments' }
  ];

  constructor(public sidebar: SidebarService) {}
}