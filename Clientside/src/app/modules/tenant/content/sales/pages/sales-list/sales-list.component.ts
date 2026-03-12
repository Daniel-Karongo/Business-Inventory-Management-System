import { Component, OnInit } from '@angular/core';
import { CommonModule, CurrencyPipe, DatePipe } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSelectModule } from '@angular/material/select';

import { SalesService } from '../../services/sales.service';
import { MatDialog } from '@angular/material/dialog';
import { SaleBulkImportDialogComponent } from '../../components/sale-bulk-import-dialog/sale-bulk-import-dialog.component';

@Component({
  standalone: true,
  selector: 'app-sales-list',
  imports: [
    CommonModule,
    FormsModule,
    MatTableModule,
    MatPaginatorModule,
    MatChipsModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule,
    MatSelectModule,
    CurrencyPipe,
    DatePipe
  ],
  templateUrl: './sales-list.component.html',
  styleUrls: ['./sales-list.component.scss']
})
export class SalesListComponent implements OnInit {

  displayedColumns = [
    'createdAt',
    'receiptNo',
    'status',
    'total',
    'paid',
    'balance',
    'actions'
  ];

  /** Load once */
  allSales: any[] = [];

  /** After filters + search + sort */
  filteredSales: any[] = [];

  /** Current page */
  sales: any[] = [];

  total = 0;
  page = 0;
  size = 20;
  loading = false;

  /** Search & filters */
  q = '';
  filterStatus = '';
  filterPayment: '' | 'paid' | 'partial' | 'unpaid' = '';

  /** Sorting */
  sortField: string | null = null;
  sortDir: 'asc' | 'desc' = 'asc';

  constructor(
    private salesService: SalesService,
    private router: Router,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.loadAll();
  }

  /** 🔥 LOAD EVERYTHING ONCE */
  loadAll() {
    this.loading = true;

    this.salesService.list({ page: 0, size: 999999 }).subscribe({
      next: res => {
        this.allSales = res.content ?? [];
        this.applyFilters();
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  /* ======================================================
     FILTER / SEARCH / SORT
     ====================================================== */

  applyFilters() {
    let data = [...this.allSales];

    /** SEARCH (includes DATE) */
    const q = (this.q || '').trim().toLowerCase();
    if (q) {
      data = data.filter(s => {
        const date = s.createdAt
          ? new Date(s.createdAt)
          : null;

        const dateStr = date
          ? `${date.toLocaleDateString()} ${date.toLocaleTimeString()}`
          : '';

        return (
          (s.receiptNo || '').toLowerCase().includes(q) ||
          (s.status || '').toLowerCase().includes(q) ||
          String(s.totalAmount || '').includes(q) ||
          dateStr.toLowerCase().includes(q)
        );
      });
    }

    /** STATUS */
    if (this.filterStatus) {
      data = data.filter(s => s.status === this.filterStatus);
    }

    /** PAYMENT STATE */
    if (this.filterPayment) {
      data = data.filter(s => {
        const paid = this.paidAmount(s);
        const total = Number(s.totalAmount);

        if (this.filterPayment === 'paid') {
          return paid >= total && total > 0;
        }

        if (this.filterPayment === 'partial') {
          return paid > 0 && paid < total;
        }

        if (this.filterPayment === 'unpaid') {
          return paid === 0;
        }

        return true;
      });
    }

    /** SORT */
    if (this.sortField) {
      const dir = this.sortDir === 'asc' ? 1 : -1;
      data.sort((a, b) => {
        const av = this.sortValue(a);
        const bv = this.sortValue(b);
        if (av < bv) return -1 * dir;
        if (av > bv) return 1 * dir;
        return 0;
      });
    }

    this.filteredSales = data;
    this.total = data.length;
    this.page = 0; // reset page on filter
    this.applyPagination();
  }

  applyPagination() {
    const start = this.page * this.size;
    const end = start + this.size;
    this.sales = this.filteredSales.slice(start, end);
  }

  changePage(e: PageEvent) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.applyPagination();
  }

  sortBy(field: string) {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }
    this.applyFilters();
  }

  sortValue(s: any) {
    switch (this.sortField) {
      case 'createdAt': return new Date(s.createdAt).getTime();
      case 'receiptNo': return s.receiptNo;
      case 'status': return s.status;
      case 'total': return Number(s.totalAmount);
      case 'paid': return this.paidAmount(s);
      case 'balance': return this.balance(s);
      default: return '';
    }
  }

  /* ======================================================
     HELPERS
     ====================================================== */

  paidAmount(sale: any): number {
    return (sale.payments || [])
      .filter((p: any) => p.status === 'SUCCESS')
      .reduce((sum: number, p: any) => sum + Number(p.amount), 0);
  }

  balance(sale: any): number {
    return Number(sale.totalAmount) - this.paidAmount(sale);
  }

  view(sale: any) {
    this.router.navigate(['/sales', sale.id]);
  }

  create() {
    this.router.navigate(['/sales/new']);
  }

  openBulkImport() {
    this.dialog.open(SaleBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    }).afterClosed().subscribe(done => {
      if (done === true) {
        this.loadAll();
      }
    });
  }
}