import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { PaymentsService } from '../../services/payments.service';

@Component({
  standalone: true,
  selector: 'app-supplier-payments',
  imports: [
    CommonModule,
    FormsModule,
    MatFormFieldModule,
    MatSelectModule,
    MatInputModule,
    MatButtonModule,
    MatSnackBarModule
  ],
  templateUrl: './supplier-payments.component.html',
  styleUrls: ['./supplier-payments.component.scss']
})
export class SupplierPaymentsComponent implements OnInit {

  supplierId = '';
  amount!: number;
  method = 'CASH';
  reference = '';

  payments: any[] = [];

  constructor(
    private paymentsService: PaymentsService,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit(): void {
    this.load();
  }

  load() {
    this.paymentsService.listSupplierPayments()
      .subscribe(res => this.payments = res.content);
  }

  pay() {

    if (!this.supplierId || !this.amount) return;

    this.paymentsService.paySupplier(
      this.supplierId,
      this.amount,
      this.method,
      this.reference
    ).subscribe({
      next: () => {
        this.snackbar.open('Supplier paid successfully', 'Close', { duration: 2000 });
        this.load();
      },
      error: () =>
        this.snackbar.open('Payment failed', 'Close', { duration: 3000 })
    });
  }
}