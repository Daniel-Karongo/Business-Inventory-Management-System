import {
  CommonModule,
  CurrencyPipe,
  DatePipe
} from '@angular/common';

import {
  Component,
  DestroyRef,
  OnInit,
  inject
} from '@angular/core';

import {
  FormsModule
} from '@angular/forms';

import {
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatChipsModule
} from '@angular/material/chips';

import {
  MatDialog
} from '@angular/material/dialog';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatPaginatorModule,
  PageEvent
} from '@angular/material/paginator';

import {
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
  MatSelectModule
} from '@angular/material/select';

import {
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  MatTableModule
} from '@angular/material/table';

import {
  debounceTime,
  distinctUntilChanged,
  Subject
} from 'rxjs';

import {
  SaleBulkImportDialogComponent
} from '../../components/sale-bulk-import-dialog/sale-bulk-import-dialog.component';

import {
  SalesService
} from '../../services/sales.service';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  standalone: true,
  selector: 'app-sales-list',
  imports: [
    CommonModule,
    FormsModule,
    CurrencyPipe,
    DatePipe,
    MatTableModule,
    MatPaginatorModule,
    MatChipsModule,
    MatIconModule,
    MatButtonModule,
    MatSelectModule,
    MatTooltipModule,
    MatInputModule,
    MatFormFieldModule,
    MatProgressSpinnerModule
  ],
  templateUrl: './sales-list.component.html',
  styleUrls: ['./sales-list.component.scss']
})
export class SalesListComponent
  implements OnInit {

  private readonly destroyRef =
    inject(DestroyRef);

  private readonly router =
    inject(Router);

  private readonly route =
    inject(ActivatedRoute);

  private readonly dialog =
    inject(MatDialog);

  private readonly snackBar =
    inject(MatSnackBar);

  private readonly salesService =
    inject(SalesService);

  private readonly search$ =
    new Subject<void>();

  readonly displayedColumns = [
    'createdAt',
    'receiptNo',
    'status',
    'total',
    'paid',
    'balance',
    'actions'
  ];

  sales: any[] = [];

  total = 0;

  page = 0;

  size = 20;

  loading = false;

  q = '';

  filterStatus = '';

  filterPayment:
    '' |
    'paid' |
    'partial' |
    'unpaid' = '';

  sortField = 'createdAt';

  sortDirection:
    'asc' |
    'desc' = 'desc';

  ngOnInit(): void {

    this.setupSearch();

    this.load();
  }

  private setupSearch(): void {

    this.search$
      .pipe(
        debounceTime(300),
        distinctUntilChanged(),
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe(() => {

        this.page = 0;

        this.load();
      });
  }

  triggerSearch(): void {

    this.search$.next();
  }

  load(): void {

    this.loading = true;

    this.salesService.list({
      page: this.page,
      size: this.size,
      status:
        this.filterStatus || undefined
    })
      .pipe(
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({
        next: result => {

          let content =
            result.content ?? [];

          if (this.q.trim()) {

            const query =
              this.q
                .trim()
                .toLowerCase();

            content =
              content.filter((sale: any) => {

                return (
                  sale.receiptNo
                    ?.toLowerCase()
                    .includes(query) ||

                  sale.status
                    ?.toLowerCase()
                    .includes(query) ||

                  String(
                    sale.totalAmount
                  ).includes(query)
                );
              });
          }

          if (this.filterPayment) {

            content =
              content.filter((sale: any) => {

                const paid =
                  this.paidAmount(sale);

                const total =
                  Number(
                    sale.totalAmount ?? 0
                  );

                switch (
                this.filterPayment
                ) {

                  case 'paid':
                    return (
                      paid >= total &&
                      total > 0
                    );

                  case 'partial':
                    return (
                      paid > 0 &&
                      paid < total
                    );

                  case 'unpaid':
                    return paid <= 0;

                  default:
                    return true;
                }
              });
          }

          content =
            this.sortData(content);

          this.sales = content;

          this.total =
            result.totalElements ??
            content.length;

          this.loading = false;
        },
        error: error => {

          this.loading = false;

          this.snackBar.open(
            error?.error?.message ??
            'Failed to load sales.',
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }

  sortBy(
    field: string
  ): void {

    if (
      this.sortField === field
    ) {

      this.sortDirection =
        this.sortDirection === 'asc'
          ? 'desc'
          : 'asc';

    } else {

      this.sortField = field;
      this.sortDirection = 'asc';
    }

    this.load();
  }

  private sortData(
    data: any[]
  ): any[] {

    const direction =
      this.sortDirection === 'asc'
        ? 1
        : -1;

    return [...data]
      .sort((a, b) => {

        const av =
          this.sortValue(a);

        const bv =
          this.sortValue(b);

        if (av < bv) {
          return -1 * direction;
        }

        if (av > bv) {
          return 1 * direction;
        }

        return 0;
      });
  }

  private sortValue(
    sale: any
  ): any {

    switch (
    this.sortField
    ) {

      case 'createdAt':
        return new Date(
          sale.createdAt
        ).getTime();

      case 'receiptNo':
        return sale.receiptNo;

      case 'status':
        return sale.status;

      case 'total':
        return Number(
          sale.totalAmount ?? 0
        );

      case 'paid':
        return this.paidAmount(sale);

      case 'balance':
        return this.balance(sale);

      default:
        return '';
    }
  }

  changePage(
    event: PageEvent
  ): void {

    this.page =
      event.pageIndex;

    this.size =
      event.pageSize;

    this.load();
  }

  paidAmount(
    sale: any
  ): number {

    return (
      sale.payments ?? []
    )
      .filter((payment: any) =>
        payment.status === 'SUCCESS'
      )
      .reduce(
        (
          sum: number,
          payment: any
        ) =>
          sum +
          Number(
            payment.amount ?? 0
          ),
        0
      );
  }

  refundedAmount(
    sale: any
  ): number {

    return (
      sale.payments ?? []
    )
      .filter((payment: any) =>
        payment.status === 'REFUNDED'
      )
      .reduce(
        (
          sum: number,
          payment: any
        ) =>
          sum +
          Number(
            payment.amount ?? 0
          ),
        0
      );
  }

  balance(
    sale: any
  ): number {

    return Number(
      sale.totalAmount ?? 0
    ) - this.paidAmount(sale);
  }

  paymentState(
    sale: any
  ): string {

    const total =
      Number(
        sale.totalAmount ?? 0
      );

    const paid =
      this.paidAmount(sale);

    if (
      paid <= 0
    ) {
      return 'UNPAID';
    }

    if (
      paid < total
    ) {
      return 'PARTIAL';
    }

    return 'PAID';
  }

  paymentStateClass(
    sale: any
  ): string {

    switch (
    this.paymentState(sale)
    ) {

      case 'PAID':
        return 'payment-paid';

      case 'PARTIAL':
        return 'payment-partial';

      default:
        return 'payment-unpaid';
    }
  }

  saleStatusClass(
    status: string
  ): string {

    switch (status) {

      case 'COMPLETED':
        return 'status-completed';

      case 'REFUNDED':
        return 'status-refunded';

      case 'CANCELLED':
        return 'status-cancelled';

      default:
        return 'status-created';
    }
  }

  view(
    sale: any
  ): void {

    this.router.navigate(
      [sale.id],
      {
        relativeTo: this.route
      }
    );
  }

  create(): void {

    this.router.navigate(
      ['new'],
      {
        relativeTo: this.route
      }
    );
  }

  openBulkImport(): void {

    this.dialog.open(
      SaleBulkImportDialogComponent,
      {
        width: '1200px',
        maxWidth: '95vw',
        maxHeight: '90vh',
        autoFocus: false
      }
    )
      .afterClosed()
      .subscribe(done => {

        if (done === true) {
          this.load();
        }
      });
  }
}