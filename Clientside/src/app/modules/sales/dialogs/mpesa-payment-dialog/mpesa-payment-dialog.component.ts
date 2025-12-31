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
  selector: 'app-mpesa-payment-dialog',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    CurrencyPipe,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule
  ],
  templateUrl: './mpesa-payment-dialog.component.html',
  styleUrls: ['./mpesa-payment-dialog.component.scss']
})
export class MpesaPaymentDialogComponent {

  form: FormGroup;
  loading = false;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: { sale: any },
    private dialogRef: MatDialogRef<MpesaPaymentDialogComponent>,
    private fb: FormBuilder,
    private paymentsService: PaymentsService
  ) {
    this.form = this.fb.group({
      phone: ['', [Validators.required]],
      amount: [
        data.sale.totalAmount,
        [Validators.required, Validators.min(1)]
      ]
    });
  }

  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    this.paymentsService.initiateMpesa(
      this.data.sale.id,
      this.form.value.phone,
      this.form.value.amount
    ).subscribe({
      next: () => this.dialogRef.close(true),
      error: () => this.loading = false
    });
  }

  cancel() {
    this.dialogRef.close(false);
  }
}