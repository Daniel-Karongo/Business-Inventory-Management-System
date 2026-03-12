import { Component, Inject } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';

import { PaymentsService } from '../../services/payments.service';

@Component({
  standalone: true,
  selector: 'app-cash-payment-dialog',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    CurrencyPipe,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule
  ],
  templateUrl: './cash-payment-dialog.component.html',
  styleUrls: ['./cash-payment-dialog.component.scss']
})
export class CashPaymentDialogComponent {

  form: FormGroup;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: { sale: any },
    private dialogRef: MatDialogRef<CashPaymentDialogComponent>,
    private fb: FormBuilder,
    private paymentsService: PaymentsService
  ) {
    this.form = this.fb.group({
      amount: [
        data.sale.totalAmount,
        [Validators.required, Validators.min(1)]
      ],
      note: ['Paid in cash']
    });
  }

  balanceDue(): number {
    const paid = (this.data.sale.payments || [])
      .filter((p: any) => p.status === 'SUCCESS')
      .reduce((sum: number, p: any) => sum + Number(p.amount), 0);

    return Number(this.data.sale.totalAmount) - paid;
  }

  submit() {
    if (this.form.invalid) return;

    this.paymentsService.create({
      saleId: this.data.sale.id,
      amount: this.form.value.amount,
      method: 'CASH',
      note: this.form.value.note
    }).subscribe(() => this.dialogRef.close(true));
  }

  cancel() {
    this.dialogRef.close(false);
  }
}