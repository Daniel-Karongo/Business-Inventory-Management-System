import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, RouterModule } from '@angular/router';
import { FormControl, ReactiveFormsModule } from '@angular/forms';
import { debounceTime } from 'rxjs/operators';

import { SelectionModel } from '@angular/cdk/collections';

import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';

import { CustomerService } from '../../services/customer.service';
import { CustomerResponse } from '../../models/customer.model';
import { SmsDialogComponent } from '../../../../shared/components/sms-dialog/sms-dialog.component';
import { SmsService } from '../../../communication/services/sms.service';
import { MatSnackBar } from '@angular/material/snack-bar';
import { CustomerBulkImportDialogComponent } from '../../components/customer-bulk-import-dialog/customer-bulk-import-dialog.component';

@Component({
  standalone: true,
  selector: 'app-customer-list',
  imports: [
    CommonModule,
    RouterModule,
    ReactiveFormsModule,

    MatTableModule,
    MatPaginatorModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatTooltipModule,
    MatDialogModule
  ],
  templateUrl: './customer-list.component.html',
  styleUrls: ['./customer-list.component.scss']
})
export class CustomerListComponent implements OnInit {

  /** MASTER DATA (loaded once) */
  allCustomers: CustomerResponse[] = [];

  /** FILTERED + PAGED DATA */
  customers: CustomerResponse[] = [];

  /** SELECTION */
  selection = new SelectionModel<CustomerResponse>(true, []);

  /** COLUMNS (dynamic) */
  displayedColumns: string[] = [];

  /** PAGINATION */
  page = 0;
  size = 20;
  total = 0;

  /** SORTING */
  sortField: 'name' | 'phones' | 'emails' | 'createdAt' | 'status' | 'gender' | null = 'createdAt';
  sortDir: 'asc' | 'desc' = 'desc';

  /** FILTERS */
  searchCtrl = new FormControl('');
  filterTypeCtrl = new FormControl('');
  filterGenderCtrl = new FormControl('');
  filterStatusCtrl = new FormControl<'active' | 'deleted' | ''>('');

  /** SHOW DELETED TOGGLE */
  showDeleted = false;

  constructor(
    private service: CustomerService,
    private smsService: SmsService,
    private dialog: MatDialog,
    private router: Router,
    private snackbar: MatSnackBar
  ) { }

  /* =====================================================
     INIT
  ===================================================== */
  ngOnInit(): void {
    this.loadAllCustomersOnce();

    this.searchCtrl.valueChanges
      .pipe(debounceTime(200))
      .subscribe(() => this.applyAll());

    this.filterTypeCtrl.valueChanges.subscribe(type => {
      if (type !== 'INDIVIDUAL') {
        this.filterGenderCtrl.reset('');
      }
      this.applyAll();
    });

    this.filterGenderCtrl.valueChanges.subscribe(() => this.applyAll());
    this.filterStatusCtrl.valueChanges.subscribe(() => this.applyAll());
  }

  /* =====================================================
     LOAD ONCE (IMPORTANT)
  ===================================================== */
  loadAllCustomersOnce(): void {
    this.service.list(0, 999999).subscribe(res => {
      this.allCustomers = res.content || [];
      this.applyAll();
    });
  }

  /* =====================================================
     FILTER + SORT + PAGINATE (CLIENT SIDE)
  ===================================================== */
  applyAll(): void {
    let data = [...this.allCustomers];

    /* SEARCH */
    const q = (this.searchCtrl.value || '').toLowerCase();
    if (q) {
      data = data.filter(c =>
        (c.name || '').toLowerCase().includes(q) ||
        (c.phoneNumbers || []).some(p => p.includes(q)) ||
        (c.email || []).some(e => e.toLowerCase().includes(q))
      );
    }

    /* TYPE */
    if (this.filterTypeCtrl.value) {
      data = data.filter(c => c.type === this.filterTypeCtrl.value);
    }

    /* GENDER */
    if (this.filterGenderCtrl.value) {
      data = data.filter(c => c.gender === this.filterGenderCtrl.value);
    }

    /* STATUS FILTER */
    if (this.filterStatusCtrl.value === 'active') {
      data = data.filter(c => !c.deleted);
    }
    if (this.filterStatusCtrl.value === 'deleted') {
      data = data.filter(c => c.deleted);
    }

    /* SHOW DELETED TOGGLE (INCLUSIVE FILTER) */
    if (!this.showDeleted) {
      data = data.filter(c => !c.deleted);
    }

    /* SORT */
    data.sort((a, b) => this.compare(a, b));

    /* PAGINATION */
    this.total = data.length;
    const start = this.page * this.size;
    const end = start + this.size;
    this.customers = data.slice(start, end);

    this.selection.clear();
    this.computeDisplayedColumns();
  }

  compare(a: CustomerResponse, b: CustomerResponse): number {
    const dir = this.sortDir === 'asc' ? 1 : -1;

    let v1: any = '';
    let v2: any = '';

    switch (this.sortField) {
      case 'phones':
        v1 = a.phoneNumbers?.[0] || '';
        v2 = b.phoneNumbers?.[0] || '';
        break;
      case 'emails':
        v1 = a.email?.[0] || '';
        v2 = b.email?.[0] || '';
        break;
      case 'createdAt':
        v1 = new Date(a.createdAt || 0).getTime();
        v2 = new Date(b.createdAt || 0).getTime();
        break;
      case 'status':
        v1 = a.deleted ? 1 : 0;
        v2 = b.deleted ? 1 : 0;
        break;
      case 'gender':
        v1 = a.gender || '';
        v2 = b.gender || '';
        break;
      default:
        v1 = a.name || '';
        v2 = b.name || '';
    }

    return v1 > v2 ? dir : v1 < v2 ? -dir : 0;
  }

  sortBy(field: any): void {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }
    this.applyAll();
  }

  /* =====================================================
     COLUMNS
  ===================================================== */
  computeDisplayedColumns(): void {
    const cols = ['select', 'name'];

    if (this.filterTypeCtrl.value === 'INDIVIDUAL') {
      cols.push('gender');
    }

    cols.push('phones', 'emails', 'createdAt');

    if (this.showDeleted) {
      cols.push('status');
    }

    cols.push('actions');
    this.displayedColumns = cols;
  }

  /* =====================================================
     SELECTION
  ===================================================== */
  toggleRow(row: CustomerResponse) {
    this.selection.toggle(row);
  }

  toggleAll() {
    this.isAllSelected()
      ? this.selection.clear()
      : this.customers.forEach(c => this.selection.select(c));
  }

  isAllSelected() {
    return this.customers.length &&
      this.selection.selected.length === this.customers.length;
  }

  isIndeterminate() {
    return this.selection.selected.length > 0 &&
      this.selection.selected.length < this.customers.length;
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    if (!this.selection.selected.length) return null;
    const states = new Set(this.selection.selected.map(c => c.deleted));
    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }

  /* =====================================================
     PAGINATION
  ===================================================== */
  onPageChange(e: PageEvent) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.applyAll();
  }

  /* =====================================================
     BULK ACTIONS (PRESERVED)
  ===================================================== */
  bulkDisable() {
    if (this.bulkState !== 'active') return;
    if (!confirm('Disable selected customers?')) return;

    this.selection.selected.forEach(c =>
      this.service.softDelete(c.id).subscribe({
        next: () => {
          this.snackbar.open('Customers disabled successfully', 'Close', { duration: 2000 });
          this.loadAllCustomersOnce();
        },
        error: () =>
          this.snackbar.open('Failed to disable customers', 'Close', { duration: 2000 })
      })
    );

  }

  bulkRestore() {
    if (this.bulkState !== 'deleted') return;

    this.selection.selected.forEach(c =>
      this.service.restore(c.id).subscribe({
        next: () => {
          this.snackbar.open('Customers restored successfully', 'Close', { duration: 2000 });
          this.loadAllCustomersOnce();
        },
        error: () =>
          this.snackbar.open('Failed to restore customers', 'Close', { duration: 2000 })
      })
    );
  }

  bulkHardDelete() {
    if (this.bulkState !== 'deleted') return;
    if (!confirm('Permanently delete selected customers?')) return;

    this.selection.selected.forEach(c =>
      this.service.hardDelete(c.id).subscribe({
        next: () => {
          this.snackbar.open('Customers deleted successfully', 'Close', { duration: 2000 });
          this.loadAllCustomersOnce();
        },
        error: () =>
          this.snackbar.open('Failed to delete customers', 'Close', { duration: 2000 })
      })
    );
  }

  /* =====================================================
     ROW ACTIONS
  ===================================================== */
  toggleDeleted() {
    this.showDeleted = !this.showDeleted;
    this.applyAll();
  }

  exportCsv() {
    this.service.exportCsv(
      this.searchCtrl.value || '',
      this.filterTypeCtrl.value || undefined,
      this.filterGenderCtrl.value || undefined,
      this.showDeleted
    ).subscribe(blob => {
      const a = document.createElement('a');
      a.href = URL.createObjectURL(blob);
      a.download = 'customers.csv';
      a.click();
    });
  }

  openSmsDialog() {
    const eligible = this.allCustomers.filter(c =>
      !c.deleted && c.phoneNumbers?.length
    );

    const ref = this.dialog.open(SmsDialogComponent, {
      // width: '720px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      data: { customers: eligible }
    });

    ref.afterClosed().subscribe(res => {
      if (!res) return;
      this.smsService.sendToCustomers(res.customerIds, res.message).subscribe();
    });
  }

  view(id: string) { this.router.navigate(['/customers', id]); }
  edit(id: string) { this.router.navigate(['/customers', id, 'edit']); }

  openBulkImport() {
    this.dialog.open(CustomerBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    }).afterClosed().subscribe(imported => {
      if (imported === true) {
        this.loadAllCustomersOnce();
      }
    });
  }
}