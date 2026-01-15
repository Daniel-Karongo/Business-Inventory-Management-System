import { Component, Inject } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

import { MatButtonModule } from '@angular/material/button';

@Component({
  standalone: true,
  selector: 'app-mpesa-payment-success-dialog',
  imports: [
    CommonModule,
    CurrencyPipe,
    MatButtonModule
  ],
  templateUrl: './mpesa-payment-success-dialog.component.html',
  styleUrls: ['./mpesa-payment-success-dialog.component.scss']
})
export class MpesaPaymentSuccessDialogComponent {

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: {
      amount: number;
      phone: string;
      receipt: string;
    },
    private dialogRef: MatDialogRef<MpesaPaymentSuccessDialogComponent>
  ) {}

  close() {
    this.dialogRef.close(true);
  }
}