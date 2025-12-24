import { Component, OnInit, ViewChild } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { MatTableModule } from '@angular/material/table';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSelectModule } from '@angular/material/select';
import { MatDialog } from '@angular/material/dialog';
import { MatCheckboxModule } from '@angular/material/checkbox';

import { SelectionModel } from '@angular/cdk/collections';
import { Router } from '@angular/router';

import { finalize } from 'rxjs/operators';

import { SupplierService } from '../../services/supplier.service';
import { Supplier } from '../../models/supplier.model';
import { ReasonDialogComponent } from '../../../../shared/components/reason-dialog/reason-dialog.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';

@Component({
  selector: 'app-supplier-list',
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatTableModule,
    MatPaginatorModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule,
    MatSelectModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatInputModule
  ],
  templateUrl: './supplier-list.component.html',
  styleUrls: ['./supplier-list.component.scss']
})
export class SupplierListComponent implements OnInit {

  displayedColumns = [
    'select',
    'name',
    'emails',
    'phones',
    'region',
    'rating',
    'createdAt',
    'status',
    'actions'
  ];

  allSuppliers: Supplier[] = [];
  filteredSuppliers: Supplier[] = [];
  suppliers: Supplier[] = [];

  regions: string[] = [];

  q = '';
  filterRegion = '';

  sortField: string | null = null;
  sortDir: 'asc' | 'desc' = 'asc';
  statusFilter: 'all' | 'active' | 'deleted' = 'active';



  total = 0;
  page = 0;
  size = 10;

  loading = false;

  selection = new SelectionModel<Supplier>(true, []);

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  constructor(
    private supplierService: SupplierService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    public router: Router
  ) { }

  ngOnInit(): void {
    this.loadSuppliers();
  }

  /* ============================================================
     LOAD
  ============================================================ */

  loadSuppliers(): void {
    this.loading = true;

    this.supplierService.getAll()
      .pipe(finalize(() => this.loading = false))
      .subscribe({
        next: suppliers => {
          this.allSuppliers = suppliers || [];

          this.regions = Array.from(
            new Set(
              this.allSuppliers
                .map(s => s.region)
                .filter((r): r is string => !!r)
            )
          ).sort();

          // ✅ THIS WAS MISSING
          this.page = 0;
          this.applyFilters();
        },
        error: () =>
          this.snackbar.open('Failed to load suppliers', 'Close', { duration: 3000 })
      });
  }

  /* ============================================================
     FILTER + SORT
  ============================================================ */

  applyFilters(): void {
    let data = [...this.allSuppliers];

    /* 🔍 SEARCH */
    const q = this.q.trim().toLowerCase();
    if (q) {
      data = data.filter(s =>
        s.name.toLowerCase().includes(q) ||
        (s.email || []).some(e => e.toLowerCase().includes(q)) ||
        (s.phoneNumber || []).some(p => p.includes(q))
      );
    }

    /* 🌍 REGION */
    if (this.filterRegion) {
      data = data.filter(s => s.region === this.filterRegion);
    }

    /* 🟢 STATUS */
    switch (this.statusFilter) {
      case 'active':
        data = data.filter(s => !s.deleted);
        break;
      case 'deleted':
        data = data.filter(s => s.deleted);
        break;
      case 'all':
      default:
        break;
    }

    /* ↕ SORT */
    if (this.sortField) {
      data.sort((a: any, b: any) => {
        const av = a[this.sortField!];
        const bv = b[this.sortField!];

        if (av == null) return 1;
        if (bv == null) return -1;

        return this.sortDir === 'asc'
          ? av > bv ? 1 : -1
          : av < bv ? 1 : -1;
      });
    }

    /* 🔁 RESET PAGINATION */
    this.page = 0;
    this.selection.clear();

    this.filteredSuppliers = data;
    this.total = data.length;

    this.applyPagination();
  }

  sortBy(field: string): void {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }
    this.applyFilters();
  }

  /* ============================================================
     PAGINATION
  ============================================================ */

  applyPagination(): void {
    const start = this.page * this.size;
    this.suppliers = this.filteredSuppliers.slice(start, start + this.size);
  }

  changePage(event: PageEvent): void {
    this.page = event.pageIndex;
    this.size = event.pageSize;
    this.applyPagination();
  }

  /* ============================================================
     SELECTION
  ============================================================ */

  toggleRow(row: Supplier): void {
    this.selection.toggle(row);
  }

  isAllSelected(): boolean {
    return this.selection.selected.length === this.suppliers.length && this.suppliers.length > 0;
  }

  masterToggle(): void {
    this.isAllSelected()
      ? this.selection.clear()
      : this.suppliers.forEach(s => this.selection.select(s));
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    const selected = this.selection.selected;
    if (!selected.length) return null;
    const states = new Set(selected.map(s => s.deleted));
    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }

  /* ============================================================
     BULK ACTIONS
  ============================================================ */

  bulkDisable(): void {
    if (this.bulkState !== 'active') {
      this.snackbar.open('Selected suppliers have mixed status', 'Close', { duration: 2500 });
      return;
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '420px',
      data: {
        title: 'Disable Suppliers?',
        message: `Provide a reason for disabling ${this.selection.selected.length} suppliers.`,
        action: 'DISABLE',
        confirmText: 'Disable'
      }
    });

    ref.afterClosed().subscribe(r => {
      if (!r?.confirmed) return;

      Promise.all(
        this.selection.selected.map(s =>
          this.supplierService.softDelete(s.id, r.reason).toPromise()
        )
      ).then(() => {
        this.snackbar.open('Suppliers disabled', 'Close', { duration: 2000 });
        this.loadSuppliers();
      });
    });
  }

  bulkRestore(): void {
    if (this.bulkState !== 'deleted') {
      this.snackbar.open('Only disabled suppliers can be restored', 'Close', { duration: 2500 });
      return;
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '420px',
      data: {
        title: 'Restore Suppliers?',
        message: `Provide a reason for restoring ${this.selection.selected.length} suppliers.`,
        action: 'RESTORE',
        confirmText: 'Restore'
      }
    });

    ref.afterClosed().subscribe(r => {
      if (!r?.confirmed) return;

      Promise.all(
        this.selection.selected.map(s =>
          this.supplierService.restore(s.id, r.reason).toPromise()
        )
      ).then(() => {
        this.snackbar.open('Suppliers restored', 'Close', { duration: 2000 });
        this.loadSuppliers();
      });
    });
  }

  /* ============================================================
     ROW ACTIONS
  ============================================================ */

  view(s: Supplier) {
    this.router.navigate(['/suppliers', s.id], { state: { deleted: s.deleted } });
  }

  edit(s: Supplier) {
    this.router.navigate(['/suppliers', s.id, 'edit'], { state: { deleted: s.deleted } });
  }

  goCreate() {
    this.router.navigate(['/suppliers/create']);
  }
}