import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, RouterModule } from '@angular/router';

import { TenantService } from '../../services/tenant.service';
import { TenantResponse } from '../../models/tenant.model';

import { MatButtonModule } from '@angular/material/button';
import { MatTableModule } from '@angular/material/table';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';

@Component({
  standalone: true,
  selector: 'app-tenant-list',
  templateUrl: './tenant-list.component.html',
  styleUrls: ['./tenant-list.component.scss'],
  imports: [
    CommonModule,
    RouterModule,
    MatButtonModule,
    MatTableModule,
    MatSnackBarModule,
    MatPaginatorModule,
    MatFormFieldModule,
    MatInputModule
  ]
})
export class TenantListComponent implements OnInit {

  private tenantService = inject(TenantService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  tenants: TenantResponse[] = [];

  total = 0;
  page = 0;
  size = 20;

  search = '';

  displayedColumns = [
    'name',
    'code',
    'status',
    'createdAt',
    'actions'
  ];

  ngOnInit() {

    this.loadTenants();

  }

  loadTenants() {

    this.tenantService
      .getTenants(this.page, this.size, this.search)
      .subscribe({

        next: (page: any) => {

          this.tenants = page.content ?? [];

          this.total = page.totalElements ?? 0;

        },

        error: () => {

          this.snack.open(
            'Failed to load tenants',
            'Close',
            { duration: 4000 }
          );

        }

      });

  }

  changePage(event: PageEvent) {

    this.page = event.pageIndex;

    this.size = event.pageSize;

    this.loadTenants();

  }

  searchChanged(value: string) {

    this.search = value;

    this.page = 0;

    this.loadTenants();

  }

  createTenant() {

    this.router.navigate(['/platform/tenants/create']);

  }

  subscription(id: string) {

    this.router.navigate([
      '/platform/tenants',
      id,
      'subscription'
    ]);

  }

  suspend(id: string) {

    this.tenantService.suspendTenant(id)
      .subscribe({

        next: () => {

          this.snack.open(
            'Tenant suspended',
            'Close',
            { duration: 2500 }
          );

          this.loadTenants();

        }

      });

  }

  activate(id: string) {

    this.tenantService.activateTenant(id)
      .subscribe({

        next: () => {

          this.snack.open(
            'Tenant activated',
            'Close',
            { duration: 2500 }
          );

          this.loadTenants();

        }

      });

  }

}