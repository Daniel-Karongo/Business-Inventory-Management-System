import {
  CommonModule,
  CurrencyPipe,
  DatePipe
} from '@angular/common';

import {
  Component,
  DestroyRef,
  Input,
  OnInit,
  inject
} from '@angular/core';

import {
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';

import {
  ActivatedRoute,
  RouterModule
} from '@angular/router';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  finalize
} from 'rxjs';

import {
  SalesService
} from '../../services/sales.service';

@Component({
  standalone: true,
  selector: 'app-sale-receipt',
  imports: [
    CommonModule,
    CurrencyPipe,
    DatePipe,
    RouterModule,
    MatButtonModule
  ],
  templateUrl: './sale-receipt.component.html',
  styleUrls: ['./sale-receipt.component.scss']
})
export class SaleReceiptComponent
  implements OnInit {

  private readonly route =
    inject(ActivatedRoute);

  private readonly destroyRef =
    inject(DestroyRef);

  private readonly salesService =
    inject(SalesService);

  @Input()
  sale: any = null;

  loading = true;

  autoPrintTriggered = false;

  ngOnInit(): void {

    const id =
      this.route.snapshot
        .paramMap
        .get('id');

    if (!id) {
      return;
    }

    this.load(id);
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

          if (
            !this.autoPrintTriggered
          ) {

            this.autoPrintTriggered = true;

            setTimeout(() => {
              window.print();
            }, 350);
          }
        }
      });
  }

  print(): void {

    window.print();
  }

  paidAmount(): number {

    return (
      this.sale?.payments ?? []
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

  refundedAmount(): number {

    return (
      this.sale?.payments ?? []
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

  balance(): number {

    return Number(
      this.sale?.totalAmount ?? 0
    ) - this.paidAmount();
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

    switch (
    this.sale?.status
    ) {

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