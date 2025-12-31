import { Component, OnInit } from '@angular/core';
import { CommonModule, CurrencyPipe, DatePipe } from '@angular/common';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatChipsModule } from '@angular/material/chips';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatTooltipModule } from '@angular/material/tooltip';

import { SalesService } from '../../services/sales.service';
import { PaymentsService } from '../../services/payments.service';

import { CashPaymentDialogComponent } from '../../dialogs/cash-payment-dialog/cash-payment-dialog.component';
import { MpesaPaymentDialogComponent } from '../../dialogs/mpesa-payment-dialog/mpesa-payment-dialog.component';

@Component({
  standalone: true,
  selector: 'app-sale-details',
  imports: [
    CommonModule,
    CurrencyPipe,
    DatePipe,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    MatChipsModule,
    MatDialogModule,
    MatTooltipModule,
    RouterModule
  ],
  templateUrl: './sale-details.component.html',
  styleUrls: ['./sale-details.component.scss']
})
export class SaleDetailsComponent implements OnInit {

  sale: any;
  loading = false;

  lineColumns = ['product', 'branch', 'qty', 'price', 'total'];
  paymentColumns = ['date', 'method', 'amount', 'status', 'actions'];

  constructor(
    private route: ActivatedRoute,
    private salesService: SalesService,
    private paymentsService: PaymentsService,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.route.paramMap.subscribe(params => {
      const id = params.get('id')!;
      this.salesService.get(id).subscribe(s => this.sale = s);

      this.load(id);
    });
  }

  load(id: string) {
    this.loading = true;
    this.salesService.get(id).subscribe({
      next: s => {
        this.sale = s;
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  paid(): number {
    return (this.sale?.payments || [])
      .filter((p: any) => p.status === 'SUCCESS')
      .reduce((a: number, b: any) => a + Number(b.amount), 0);
  }

  balance(): number {
    return Number(this.sale?.totalAmount || 0) - this.paid();
  }

  canPay(): boolean {
    return this.sale?.status === 'CREATED' && this.balance() > 0;
  }

  addCashPayment() {
    this.dialog.open(CashPaymentDialogComponent, {
      width: '420px',
      data: { sale: this.sale }
    }).afterClosed().subscribe(ok => ok && this.reload());
  }

  addMpesaPayment() {
    this.dialog.open(MpesaPaymentDialogComponent, {
      width: '420px',
      data: { sale: this.sale }
    }).afterClosed().subscribe(ok => ok && this.reload());
  }

  refundSale() {
    if (!confirm('Refund this sale?')) return;
    this.salesService.refund(this.sale.id).subscribe(() => this.reload());
  }

  cancelSale() {
    if (!confirm('Cancel this sale?')) return;
    this.salesService.cancel(this.sale.id).subscribe(() => this.reload());
  }

  refundPayment(paymentId: string) {
    if (!confirm('Refund this payment?')) return;

    this.paymentsService.refund(paymentId).subscribe(() => {
      this.reload();
    });
  }

  private reload() {
    this.load(this.sale.id);
  }
}