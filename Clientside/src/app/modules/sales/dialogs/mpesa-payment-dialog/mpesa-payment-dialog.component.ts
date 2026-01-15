import { Component, Inject } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialog, MatDialogRef } from '@angular/material/dialog';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';

import { PaymentsService } from '../../services/payments.service';
import { MpesaPaymentSuccessDialogComponent } from '../mpesa-payment-success-dialog/mpesa-payment-success-dialog.component';

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
  private pollHandle?: number;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: { sale: any },
    private dialogRef: MatDialogRef<MpesaPaymentDialogComponent>,
    private dialog: MatDialog,
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
      next: () => this.startPollingForSuccess(),
      error: () => this.loading = false
    });
  }

  private startPollingForSuccess() {
    const saleId = this.data.sale.id;

    this.pollHandle = window.setInterval(() => {
      this.paymentsService.getBySale(saleId).subscribe(payments => {

        const mpesa = payments.find(
          (p: any) => p.method === 'MPESA' && p.status === 'SUCCESS'
        );

        if (!mpesa) return;

        clearInterval(this.pollHandle);
        this.loading = false;

        // 🔥 DO NOT close parent dialog yet

        this.dialog.open(MpesaPaymentSuccessDialogComponent, {
          width: '380px',
          disableClose: true,
          data: {
            amount: mpesa.amount,
            phone: mpesa.providerReference,
            receipt: mpesa.transactionCode
          }
        }).afterClosed().subscribe(() => {
          // ✅ close ONCE, with true
          this.dialogRef.close(true);
        });
      });
    }, 2000);
  }

  cancel() {
    if (this.pollHandle) {
      clearInterval(this.pollHandle);
    }
    this.dialogRef.close(false);
  }

  ngOnDestroy() {
    if (this.pollHandle) {
      clearInterval(this.pollHandle);
    }
  }
}