import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, RouterModule } from '@angular/router';
import { TenantPage, TenantResponse } from '../../models/tenant.model';
import { TenantService } from '../../services/tenant.service';
import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSnackBar } from '@angular/material/snack-bar';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';

@Component({
  standalone: true,
  selector: 'app-tenant-list',
  imports: [
    CommonModule,
    FormsModule,
    RouterModule,
    PageShellComponent,
    MatTableModule,
    MatPaginatorModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule,
    MatIconModule,
    MatButtonToggleModule,
    MatTooltipModule
  ],
  templateUrl: './tenant-list.component.html',
  styleUrls: [
    './tenant-list.component.scss'
  ]
})
export class TenantListComponent implements OnInit {
  
  private tenantService = inject(TenantService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  loading = true;
  actionBusy = false;
  tenants: TenantResponse[] = [];
  filtered: TenantResponse[] = [];
  paged: TenantResponse[] = [];
  
  searchTerm = '';
  statusFilter = 'ALL';
  viewMode:
    'table' | 'cards'
    = 'table';
  compactView = false;
  page = 0;
  size = 10;
  total = 0;
  
  displayedColumns = [
    'tenant',
    'status',
    'plan',
    'created',
    'actions'
  ];
  
  stats = {
    active: 0,
    trial: 0,
    suspended: 0,
    expired: 0
  };

  ngOnInit() {
    const savedView =
      localStorage.getItem(
        'platform.tenants.view'
      );
    if (
      savedView === 'table'
      ||
      savedView === 'cards'
    ) {
      this.viewMode =
        savedView;
    }

    this.loadTenants();
  }

  loadTenants() {
    this.loading = true;
    this.tenantService
      .getTenants(
        0,
        1000,
        this.searchTerm
      )
      .subscribe({
        next: (
          page: TenantPage
        ) => {
          this.tenants =
            page.content || [];
          this.computeStats();
          this.applyFilters();
          this.loading =
            false;
        },
        error: () => {
          this.loading =
            false;
          this.snack.open(
            'Failed to load tenants',
            'Close',
            {
              duration: 4000
            }

          );
        }

      });
  }

  computeStats() {
    this.stats.active =
      this.tenants.filter(
        x =>
          x.status === 'ACTIVE'
      ).length;
    this.stats.trial =
      this.tenants.filter(
        x =>
          x.status === 'TRIAL'
      ).length;
    this.stats.suspended =
      this.tenants.filter(
        x =>
          x.status === 'SUSPENDED'
      ).length;
    this.stats.expired =
      this.tenants.filter(
        x =>
          x.status === 'EXPIRED'
      ).length;
  }

  applyFilters() {
    const term =
      this.searchTerm
        .trim()
        .toLowerCase();
    this.filtered =
      this.tenants.filter(
        t => {
          const textMatch =
            !term
            ||
            t.name
              .toLowerCase()
              .includes(term)
            ||
            t.code
              .toLowerCase()
              .includes(term);
          const statusMatch =
            this.statusFilter
            === 'ALL'
            ||
            t.status
            === this.statusFilter;
          return (
            textMatch
            &&
            statusMatch
          );
        }

      );
    this.page = 0;
    this.applyPagination();
  }

  applyPagination() {
    this.total =
      this.filtered.length;
    const start =
      this.page * this.size;
    this.paged =
      this.filtered.slice(
        start,
        start + this.size
      );
  }

  changePage(
    e: PageEvent
  ) {
    this.page =
      e.pageIndex;
    this.size =
      e.pageSize;
    this.applyPagination();
  }

  toggleDensity() {
    this.compactView =
      !this.compactView;
    this.size =
      this.compactView
        ? 25
        : 10;
    this.applyPagination();
  }

  setView(
    mode:
      'table'
      |
      'cards'
  ) {
    this.viewMode =
      mode;
    localStorage.setItem(
      'platform.tenants.view',
      mode
    );
  }

  createTenant() {
    this.router.navigate([
      '/platform/tenants/create'
    ]);
  }

  subscription(
    id: string
  ) {
    this.router.navigate([
      '/platform/tenants',
      id,
      'subscription'
    ]);
  }

  suspend(
    id: string
  ) {
    if (
      this.actionBusy
    ) {
      return;
    }

    this.actionBusy = true;
    this.tenantService
      .suspendTenant(id)
      .subscribe({
        next: () => {
          this.actionBusy =
            false;
          this.snack.open(
            'Tenant suspended',
            'Close',
            {
              duration: 3000
            }

          );
          this.loadTenants();
        },
        error: () => {
          this.actionBusy =
            false;
          this.snack.open(
            'Suspend failed',
            'Close',
            {
              duration: 3500
            }

          );
        }

      });
  }

  activate(
    id: string
  ) {
    if (
      this.actionBusy
    ) {
      return;
    }

    this.actionBusy = true;
    this.tenantService
      .activateTenant(id)
      .subscribe({
        next: () => {
          this.actionBusy =
            false;
          this.snack.open(
            'Tenant activated',
            'Close',
            {
              duration: 3000
            }

          );
          this.loadTenants();
        },
        error: () => {
          this.actionBusy =
            false;
          this.snack.open(
            'Activation failed',
            'Close',
            {
              duration: 3500
            }

          );
        }

      });
  }
}