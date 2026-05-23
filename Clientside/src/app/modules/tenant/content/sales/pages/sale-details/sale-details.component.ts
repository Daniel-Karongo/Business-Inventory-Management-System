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
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';

import {
  ActivatedRoute,
  Router,
  RouterModule
} from '@angular/router';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatChipsModule
} from '@angular/material/chips';

import {
  MatDialog,
  MatDialogModule
} from '@angular/material/dialog';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  MatTableModule
} from '@angular/material/table';

import {
  MatTooltipModule
} from '@angular/material/tooltip';

import {
  finalize
} from 'rxjs';

import {
  CashPaymentDialogComponent
} from '../../dialogs/cash-payment-dialog/cash-payment-dialog.component';

import {
  MpesaPaymentDialogComponent
} from '../../dialogs/mpesa-payment-dialog/mpesa-payment-dialog.component';

import {
  PaymentsService
} from '../../services/payments.service';

import {
  SalesService
} from '../../services/sales.service';
import { ConfirmDialogComponent } from '../../../../../../shared/components/confirm-dialog/confirm-dialog.component';

@Component({
  standalone: true,
  selector: 'app-sale-details',
  imports: [
    CommonModule,
    CurrencyPipe,
    DatePipe,
    RouterModule,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    MatChipsModule,
    MatDialogModule,
    MatTooltipModule
  ],
  templateUrl: './sale-details.component.html',
  styleUrls: ['./sale-details.component.scss']
})
export class SaleDetailsComponent
  implements OnInit {

  private readonly route =
    inject(ActivatedRoute);

  private readonly router =
    inject(Router);

  private readonly destroyRef =
    inject(DestroyRef);

  private readonly dialog =
    inject(MatDialog);

  private readonly snackBar =
    inject(MatSnackBar);

  private readonly salesService =
    inject(SalesService);

  private readonly paymentsService =
    inject(PaymentsService);

  sale: any = null;

  loading = false;

  actionLoading = false;

  readonly lineColumns = [
    'product',
    'variant',
    'packaging',
    'branch',
    'qty',
    'price',
    'total'
  ];

  readonly paymentColumns = [
    'date',
    'method',
    'amount',
    'status',
    'actions'
  ];

  ngOnInit(): void {

    this.route.paramMap
      .pipe(
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe(params => {

        const id =
          params.get('id');

        if (!id) {
          return;
        }

        this.load(id);
      });
  }

  load(
    id: string
  ): void {

    this.loading = true;

    this.salesService.get(id)
      .pipe(
        finalize(() => {
          this.loading = false;
        }),
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({
        next: sale => {
          this.sale = sale;
        },
        error: error => {

          this.snackBar.open(
            error?.error?.message ??
            'Failed to load sale.',
            'Close',
            {
              duration: 4000
            }
          );

          this.router.navigate([
            '/app/sales'
          ]);
        }
      });
  }

  reload(): void {

    if (!this.sale?.id) {
      return;
    }

    this.load(this.sale.id);
  }

  paid(): number {

    return (
      this.sale?.payments ?? []
    )
      .filter((payment: any) =>
        payment.status === 'SUCCESS'
      )
      .reduce(
        (acc: number, payment: any) =>
          acc + Number(payment.amount ?? 0),
        0
      );
  }

  refunded(): number {

    return (
      this.sale?.payments ?? []
    )
      .filter((payment: any) =>
        payment.status === 'REFUNDED'
      )
      .reduce(
        (acc: number, payment: any) =>
          acc + Number(payment.amount ?? 0),
        0
      );
  }

  balance(): number {

    return Number(
      this.sale?.totalAmount ?? 0
    ) - this.paid();
  }

  isCreated(): boolean {

    return this.sale?.status === 'CREATED';
  }

  isCompleted(): boolean {

    return this.sale?.status === 'COMPLETED';
  }

  isCancelled(): boolean {

    return this.sale?.status === 'CANCELLED';
  }

  isRefunded(): boolean {

    return this.sale?.status === 'REFUNDED';
  }

  canPay(): boolean {

    return (
      this.isCreated() &&
      this.balance() > 0
    );
  }

  canDeliver(): boolean {

    return (
      this.isCreated() &&
      this.paid() >=
      Number(this.sale?.totalAmount ?? 0)
    );
  }

  canCancel(): boolean {

    return (
      this.isCreated() &&
      !this.actionLoading
    );
  }

  canRefund(): boolean {

    return (
      this.isCompleted() &&
      !this.actionLoading
    );
  }

  canCancelAndRefund(): boolean {

    return (
      this.isCreated() &&
      this.paid() > 0 &&
      !this.actionLoading
    );
  }

  addCashPayment(): void {

    this.dialog.open(
      CashPaymentDialogComponent,
      {
        width: '420px',
        data: {
          sale: this.sale
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result) {
          return;
        }

        this.reload();
      });
  }

  addMpesaPayment(): void {

    this.dialog.open(
      MpesaPaymentDialogComponent,
      {
        width: '420px',
        data: {
          sale: this.sale
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result) {
          return;
        }

        this.reload();
      });
  }

  private confirmAction(
    title: string,
    message: string,
    confirmText = 'Confirm'
  ) {

    return this.dialog.open(
      ConfirmDialogComponent,
      {
        width: '420px',
        data: {
          title,
          message,
          confirmText,
          cancelText: 'Cancel'
        }
      }
    ).afterClosed();
  }

  deliverSale(): void {

    if (!this.sale?.id) {
      return;
    }

    this.confirmAction(
      'Deliver Sale',
      'Deliver this sale and consume inventory?',
      'Deliver'
    )
      .subscribe(confirmed => {

        if (!confirmed) {
          return;
        }

        this.actionLoading = true;

        this.salesService
          .deliver(this.sale.id)
          .pipe(
            finalize(() => {
              this.actionLoading = false;
            }),
            takeUntilDestroyed(this.destroyRef)
          )
          .subscribe({
            next: sale => {

              this.sale = sale;

              this.snackBar.open(
                'Sale delivered successfully.',
                'Close',
                {
                  duration: 3000
                }
              );
            },

            error: error => {

              this.snackBar.open(
                error?.error?.message ??
                'Failed to deliver sale.',
                'Close',
                {
                  duration: 4000
                }
              );
            }
          });
      });
  }

  cancelSale(): void {

    if (!this.sale?.id) {
      return;
    }

    this.confirmAction(
      'Cancel Sale',
      'Cancel this sale and release reserved stock?',
      'Cancel Sale'
    )
      .subscribe(confirmed => {

        if (!confirmed) {
          return;
        }

        this.actionLoading = true;

        this.salesService
          .cancel(this.sale.id)
          .pipe(
            finalize(() => {
              this.actionLoading = false;
            }),
            takeUntilDestroyed(this.destroyRef)
          )
          .subscribe({
            next: sale => {

              this.sale = sale;

              this.snackBar.open(
                'Sale cancelled successfully.',
                'Close',
                {
                  duration: 3000
                }
              );
            },

            error: error => {

              this.snackBar.open(
                error?.error?.message ??
                'Failed to cancel sale.',
                'Close',
                {
                  duration: 4000
                }
              );
            }
          });
      });
  }

  refundSale(): void {

    if (!this.sale?.id) {
      return;
    }

    this.confirmAction(
      'Refund Sale',
      'Refund this completed sale?',
      'Refund'
    )
      .subscribe(confirmed => {

        if (!confirmed) {
          return;
        }

        this.actionLoading = true;

        this.salesService
          .refund(this.sale.id)
          .pipe(
            finalize(() => {
              this.actionLoading = false;
            }),
            takeUntilDestroyed(this.destroyRef)
          )
          .subscribe({
            next: sale => {

              this.sale = sale;

              this.snackBar.open(
                'Sale refunded successfully.',
                'Close',
                {
                  duration: 3000
                }
              );
            },

            error: error => {

              this.snackBar.open(
                error?.error?.message ??
                'Failed to refund sale.',
                'Close',
                {
                  duration: 4000
                }
              );
            }
          });
      });
  }

  cancelAndRefund(): void {

    if (!this.sale?.id) {
      return;
    }

    this.confirmAction(
      'Cancel And Refund',
      'Cancel sale and refund all successful payments?',
      'Cancel + Refund'
    )
      .subscribe(confirmed => {

        if (!confirmed) {
          return;
        }

        this.actionLoading = true;

        this.salesService
          .cancelAndRefund(this.sale.id)
          .pipe(
            finalize(() => {
              this.actionLoading = false;
            }),
            takeUntilDestroyed(this.destroyRef)
          )
          .subscribe({
            next: sale => {

              this.sale = sale;

              this.snackBar.open(
                'Sale cancelled and refunded.',
                'Close',
                {
                  duration: 4000
                }
              );
            },

            error: error => {

              this.snackBar.open(
                error?.error?.message ??
                'Failed to cancel and refund sale.',
                'Close',
                {
                  duration: 4000
                }
              );
            }
          });
      });
  }

  refundPayment(
    paymentId: string
  ): void {

    if (!paymentId) {
      return;
    }

    this.confirmAction(
      'Refund Payment',
      'Refund this payment?',
      'Refund'
    )
      .subscribe(confirmed => {

        if (!confirmed) {
          return;
        }

        this.actionLoading = true;

        this.paymentsService
          .refund(paymentId)
          .pipe(
            finalize(() => {
              this.actionLoading = false;
            }),
            takeUntilDestroyed(this.destroyRef)
          )
          .subscribe({
            next: () => {

              this.snackBar.open(
                'Payment refunded successfully.',
                'Close',
                {
                  duration: 3000
                }
              );

              this.reload();
            },

            error: error => {

              this.snackBar.open(
                error?.error?.message ??
                'Failed to refund payment.',
                'Close',
                {
                  duration: 4000
                }
              );
            }
          });
      });
  }

  paymentStatusClass(
    status: string
  ): string {

    switch (status) {

      case 'SUCCESS':
        return 'payment-success';

      case 'REFUNDED':
        return 'payment-refunded';

      case 'FAILED':
        return 'payment-failed';

      default:
        return 'payment-pending';
    }
  }

  saleStatusClass(): string {

    switch (this.sale?.status) {

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
}